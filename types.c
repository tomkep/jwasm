/****************************************************************************
*
*                            Open Watcom Project
*
*    Portions Copyright (c) 1983-2002 Sybase, Inc. All Rights Reserved.
*
*  ========================================================================
*
*    This file contains Original Code and/or Modifications of Original
*    Code as defined in and that are subject to the Sybase Open Watcom
*    Public License version 1.0 (the 'License'). You may not use this file
*    except in compliance with the License. BY USING THIS FILE YOU AGREE TO
*    ALL TERMS AND CONDITIONS OF THE LICENSE. A copy of the License is
*    provided with the Original Code and Modifications, and is also
*    available at www.sybase.com/developer/opensource.
*
*    The Original Code and all software distributed under the License are
*    distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
*    EXPRESS OR IMPLIED, AND SYBASE AND ALL CONTRIBUTORS HEREBY DISCLAIM
*    ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
*    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
*    NON-INFRINGEMENT. Please see the License for the specific language
*    governing rights and limitations under the License.
*
*  ========================================================================
*
* Description:  STRUCT, UNION, RECORD and TYPEDEF directives
* virtually rewritten for JWasm.
*
****************************************************************************/


#include "globals.h"
#include <ctype.h>

#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "proc.h"
#include "input.h"
#include "types.h"
#include "expreval.h"
#include "labels.h"
#include "symbols.h"
#include "listing.h"

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

struct_state StructDef;
static dir_node * redef_struct;
static int level;  /* current nesting level if a field is searched */

void TypesInit()
{
    StructDef.struct_depth = 0;
    StructDef.struct_stack = NULL;
    StructDef.curr_struct = NULL;
    redef_struct = NULL;
    level = 0;
}

// search a name in a struct's namespace

struct asm_sym *SearchNameInStruct( asm_sym *tstruct, const char *name, unsigned int * poffset)
{
    int len = strlen(name);
    field_list * fl = ((dir_node *)tstruct)->e.structinfo->head;
    asm_sym *sym = NULL;

    if (ModuleInfo.oldstructs == TRUE) {
        return(SymSearch(name));
    }
    if (level >= 32) {
        AsmError(NESTING_LEVEL_TOO_DEEP);
        return( NULL );
    }
    level++;
    for (;fl;fl = fl->next) {
        /* recursion: if member has no name, check if it is a structure
         and scan this structure's namespace then */
        if (*(fl->sym->name) == 0 ) {
            /* there are 2 cases: an anonymous inline struct ... */
            if (fl->sym->state == SYM_TYPE) {
                if (sym = SearchNameInStruct(fl->sym, name, poffset)) {
                    *poffset += fl->sym->offset;
                    break;
                }
            /* or an anonymous structured field */
            } else if ((fl->sym->mem_type == MT_TYPE)) {
                if (sym = SearchNameInStruct(fl->sym->type, name, poffset)) {
                    *poffset += fl->sym->offset;
                    break;
                }
            }
        } else if (len == fl->sym->name_size && SymCmpFunc( name, fl->sym->name ) == 0 ) {
            DebugMsg(("SearchNameInStruct: '%s' found in struct namespace\n", name));
            sym = fl->sym;
            break;
        }
    }
    level--;
    return (sym);
}

// check if a struct has changed

static bool AreStructsEqual(dir_node *oldstr, dir_node *newstr)
{
    field_list  *fold = oldstr->e.structinfo->head;
    field_list  *fnew = newstr->e.structinfo->head;

    DebugMsg(("AreStructsEqual(%s) enter\n", oldstr->sym.name ));

    /* kind of structs must be identical */
    if (oldstr->e.structinfo->typekind != newstr->e.structinfo->typekind)
        return( FALSE );

    for (;fold; fold = fold->next, fnew = fnew->next) {
        if (!fnew) {
            DebugMsg(("AreStructsEqual: fields don't match\n"));
            return( FALSE );
        }
        if (0 != strcmp(fold->sym->name, fnew->sym->name)) {
            DebugMsg(("AreStructsEqual: type name of field changed\n"));
            return( FALSE );
        }
        if (fold->sym->offset != fnew->sym->offset) {
            DebugMsg(("AreStructsEqual: offset of field %s changed: %u - %u\n", fold->sym->name, fold->sym->offset, fnew->sym->offset));
            return( FALSE );
        }
        if (fold->sym->total_size != fnew->sym->total_size) {
            DebugMsg(("AreStructsEqual: total_size of field changed\n"));
            return( FALSE );
        }
    }
    if (fnew)
        return( FALSE );
    return( TRUE );
}

// called on pass one only
// i is the token which contains STRUCT, STRUC, UNION or ENDS

int StructDirective( int i )
/********************/
{
    char *name;
    unsigned alignment;
    unsigned int offset;
    int name_loc;
    struct asm_sym *sym;
    memtype mem_type;
    dir_node *dir;
    dir_node *parent;

    DebugMsg(("StructDirective enter, i=%u\n", i));

    /* structs/unions must have a name if at level 0 */
    /* inside other structs/unions they can be anonymous */
    if (( StructDef.struct_depth == 0) && (i == 0 )) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (i) {
        name_loc = i-1;
        name = AsmBuffer[i-1]->string_ptr;
    } else if (AsmBuffer[i+1]->token != T_FINAL &&
               is_valid_id_char(*(AsmBuffer[i+1]->string_ptr))) {
        /* the section name might be a reserved word! */
        name_loc = i+1;
        name = AsmBuffer[i+1]->string_ptr;
    } else {
        name_loc = -1;
        name = "";
    }

    switch( AsmBuffer[i]->value ) {
    case T_STRUC:
    case T_STRUCT:
    case T_UNION:
        if (Parse_Pass > PASS_1) {
            StructDef.struct_depth++;
            break;
        }

        if ( ( StructDef.struct_depth > 0 ) && ( i > 0 ) ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        alignment = Options.alignment_default;

        /* get an optional alignment argument: 1,2,4,8,16 or 32 */
        if ((StructDef.struct_depth == 0) && (AsmBuffer[i+1]->token != T_FINAL)) {
            int j = i+1;
            unsigned int power;
            expr_list opndx;
            /* get the optional alignment parameter */
            if ( EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR ) {
                ;
            } else if (opndx.type != EXPR_CONST || opndx.string != NULL) {
                AsmError( CONSTANT_EXPECTED );
            } else if( opndx.value > MAX_STRUCT_ALIGN ) {
                AsmError( STRUCT_ALIGN_TOO_HIGH );
            } else {
                for( power = 1; power < opndx.value; power <<= 1 );
                if( power != opndx.value ) {
                    AsmError( POWER_OF_2 );
                } else
                    alignment = opndx.value;

                DebugMsg(("StructDirective(%s) alignment=%u\n", name, alignment));
                /* there might also be the NONUNIQUE keyword */
                if (AsmBuffer[j]->token == T_COMMA &&
                    AsmBuffer[j+1]->token == T_ID &&
                    (stricmp(AsmBuffer[j+1]->string_ptr, "NONUNIQUE") == 0)) {
                    /* currently NONUNIQUE is just skipped */
                    j += 2;
                }
                if (AsmBuffer[j]->token != T_FINAL) {
                    AsmError(SYNTAX_ERROR);
                }
            }
        }
        /* does struct have a name? */
        if (*name) {
            if (StructDef.struct_depth == 0) {
                /* the "top-level" struct is part of the global namespace */
                sym = SymSearch( name );
                DebugMsg(("StructDirective: SymSearch (%s)=%X (curr struct=%X)\n", name, sym, StructDef.curr_struct));
            } else {
                sym = SearchNameInStruct((asm_sym *)StructDef.curr_struct, name, &offset);
                DebugMsg(("StructDirective(%s): SearchNameInStruc()=%X\n", name, sym));
            }
        } else {
            sym = NULL;   /* no, it's anonymous */
        }

        if( sym == NULL ) {
            /* is it a global STRUCT? */
            if ( StructDef.struct_depth == 0 )
                dir = dir_insert( name, TAB_TYPE );
            else {
                /* a nested structure is split in an anonymous STRUCT type
                 and a struct field with/without name
                 */
                field_list *f;
                dir = dir_insert_ex( "", TAB_TYPE);
                sym = AddFieldToStruct(name_loc, -1, MT_TYPE, (asm_sym *)dir, 0);
                alignment = StructDef.curr_struct->e.structinfo->alignment;
            }
        } else {
            /* the symbol exists already */
            dir = (dir_node *)sym;
            if( sym->state == SYM_UNDEFINED ) {
                dir_change( dir, TAB_TYPE );
            } else if( sym->state == SYM_TYPE && (StructDef.struct_depth == 0)) {
                /* structure redefinition */
                redef_struct = dir;
                dir = dir_insert_ex( name, TAB_TYPE );
            } else {
                AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
                return( ERROR );
            }
        }

        dir->e.structinfo->alignment = alignment;
        dir->sym.offset = 0;
        dir->e.structinfo->isOpen = TRUE;
        if (AsmBuffer[i]->value == T_UNION)
            dir->e.structinfo->typekind = TYPE_UNION;
        else
            dir->e.structinfo->typekind = TYPE_STRUCT;

        if (AsmFiles.file[LST]) {
            WriteLstFile(LSTTYPE_STRUCT, 0, dir->sym.name);
            directive_listed = TRUE;
        }

        if (StructDef.struct_depth)
            dir->e.structinfo->isInline = TRUE;

        i++;
        DebugMsg(("StructDirective(%s): token following: %X\n", name, AsmBuffer[i]->token));
        push( &( StructDef.struct_stack ), StructDef.curr_struct );
#ifdef DEBUG_OUT
        {
            int x;
            asm_sym *sym;
            for (x=0;;x++) {
                sym = peek(StructDef.struct_stack, x);
                if (sym == NULL) break;
                DebugMsg(("StructDirective stack(%u): %X, name=>%s<\n", x, sym, sym->name));
            }
        }
#endif
        StructDef.curr_struct = dir;
        StructDef.struct_depth++;
        break;
    case T_ENDS:
        if (Parse_Pass > PASS_1) {
            StructDef.struct_depth--;
            break;
        }
        if( StructDef.struct_depth == 0) {
            /* ENDS found, but the struct stack is empty */
            DebugMsg(("StructDirective(T_ENDS): struct stack is empty, i=%u\n", i));
            AsmError( UNMATCHED_BLOCK_NESTING );
            return( ERROR );
        }

        /* an inline struct will end with a simple ENDS without name */
        /* an global struct will end with a <name ENDS>  */
#ifdef DEBUG_OUT
        if (StructDef.curr_struct == NULL) {
            DebugMsg(("StructDirective(T_ENDS), current struct is NULL, but struct depth is > 0!!!\n"));
            AsmError( BLOCK_NESTING_ERROR );
            return( ERROR );
        }
#endif
        DebugMsg(("StructDirective(T_ENDS), level=%u, ofs=%u, struct size=%u, alignmnt=%u\n",
                  StructDef.struct_depth,
                  StructDef.curr_struct->sym.offset,
                  StructDef.curr_struct->sym.total_size,
                  StructDef.curr_struct->e.structinfo->alignment));
        dir = StructDef.curr_struct;
        /* inline "struct"/"union" must be first identifier in a line */
        if (((dir->e.structinfo->isInline) && (i == 0)) ||
            (name && strcmp( name, dir->sym.name ) == 0)) {

            unsigned int size;

            if (dir->e.structinfo->alignment) {
                dir->sym.total_size = (dir->sym.total_size + dir->e.structinfo->alignment - 1) & (-dir->e.structinfo->alignment);
                DebugMsg(("StructDirective(T_ENDS):, struct size after final alignment=%u\n", dir->sym.total_size));
            }
            dir->e.structinfo->isOpen = FALSE;

            dir->sym.defined = TRUE;

            /* if there's a negative offset, size will be wrong! */
            size = dir->sym.total_size;

            /* reset offset, it's just used during the definition */
            dir->sym.offset = 0;

            StructDef.curr_struct = pop( &( StructDef.struct_stack ) );
            StructDef.struct_depth--;

            if (AsmFiles.file[LST]) {
                WriteLstFile(LSTTYPE_STRUCT, size, dir->sym.name);
                directive_listed = TRUE;
            }
#if 1
            /* to allow direct structure access */
            switch (dir->sym.total_size) {
            case 1:
                dir->sym.mem_type = MT_BYTE;
                break;
            case 2:
                dir->sym.mem_type = MT_WORD;
                break;
            case 4:
                dir->sym.mem_type = MT_DWORD;
                break;
            case 6:
                dir->sym.mem_type = MT_FWORD;
                break;
            case 8:
                dir->sym.mem_type = MT_QWORD;
                break;
            default:
                /* set something which cannot be accessed by a reg */
                /* there might exist a better solution, once the mess
                 in the parser has been removed */
                // dir->sym.mem_type = MT_OWORD;
                dir->sym.mem_type = MT_EMPTY;
            }
#endif

            /* reset redefine */
            if (StructDef.struct_depth == 0) {
                if (redef_struct) {
                    if (AreStructsEqual(dir, redef_struct) == FALSE) {
                        AsmError( NON_BENIGN_STRUCT_REDEFINITION );
                    }
                    DebugMsg(("delete the redefinition of %s\n", dir->sym.name));
                    dir_free( dir, FALSE );
                    redef_struct = NULL;
                }
            } else {
                UpdateStructSize( size );
                DebugMsg(("StructDirective: new size of restored structure=%u\n", StructDef.curr_struct->sym.total_size));
            }
        } else {
            /* ENDS found, but names don't match */
            DebugMsg(("StructDirective(T_ENDS): names don't match, i=%u, name=%s\n", i, name));
            AsmError( UNMATCHED_BLOCK_NESTING );
            return( ERROR );
        }
    } /* end switch(AsmBuffer[i]->value */
    return( NOT_ERROR );
}

/* this function is called in pass 1 only */
/* name_loc: index of field name or -1  */
/* loc: initializer location, may be -1 */
/* vartype: type of item if memtype is MT_TYPE */
/* size: size of type - used for alignment only */

struct asm_sym * AddFieldToStruct( int name_loc, int loc, memtype mem_type, struct asm_sym * vartype, int size)
/*****************************/
{
    int offset;
    int count;
    int i;
    char * name;
    struct_info *si;
    field_list  *f;
    asm_sym     *sym;

    si = StructDef.curr_struct->e.structinfo;
    offset = StructDef.curr_struct->sym.offset;

    DebugMsg(("AddFieldToStruct: curr struct=%s, curr ofs=%u\n", StructDef.curr_struct->sym.name, offset));

    if (name_loc >= 0) {
        /* the field has a name */
        name = AsmBuffer[name_loc]->string_ptr;
        /* check if field name is already used */
        /* RECORD fields names, which are global, aren't handled here */
        sym = SearchNameInStruct((asm_sym *)StructDef.curr_struct, name, (unsigned int *)&i);
        if (sym) {
            AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
            name = "";
        }
    } else
        name = "";

    /* alloc the items needed */

    if (ModuleInfo.oldstructs == TRUE && *name != NULLC)
        sym = SymCreate(name, TRUE);
    else
        sym = SymCreate(name, FALSE);

    sym->state = SYM_STRUCT_FIELD;
    sym->defined = TRUE;
    sym->mem_type = mem_type;
    sym->type = vartype;
    // ok to do?
    // sym->total_size = SizeFromMemtype(mem_type, Use32);

    f = AsmAlloc( sizeof( field_list ) );

    f->next = NULL;
    f->sym = sym;

    if (loc != -1) {
        f->initializer = AsmAlloc( strlen( AsmBuffer[loc]->string_ptr ) + 1 );
        strcpy( f->initializer, AsmBuffer[ loc ]->string_ptr );

        /* now add the value to initialize the struct to */

        count = 0;
        for( i = loc + 1; AsmBuffer[i]->token != T_FINAL; i++ ) {
            if( AsmBuffer[i]->string_ptr != NULL ) {
                count += strlen( AsmBuffer[i]->string_ptr ) + 1;
            }
            if( AsmBuffer[i]->token == T_STRING ) count += 2;
        }

        f->value = AsmAlloc( count + 1 );
        f->value[0] = '\0';

        for( i = loc + 1; AsmBuffer[i]->token != T_FINAL; i++ ) {
            if( AsmBuffer[i]->token == T_STRING && mem_type == MT_TYPE) {
                //        if( AsmBuffer[i]->token == T_STRING) {
                strcat( f->value, "<" );
            }
            if( AsmBuffer[i]->string_ptr != NULL ) {
                strcat( f->value, AsmBuffer[i]->string_ptr );
            }
            if( AsmBuffer[i]->token == T_STRING && mem_type == MT_TYPE) {
                //        if( AsmBuffer[i]->token == T_STRING) {
                strcat( f->value, ">" );
            }
            if (AsmBuffer[i+1]->token != T_FINAL)
                strcat( f->value, " " );
        }
    } else {
        f->initializer = NULL;
        f->value = NULL;
    }

    if( si->head == NULL ) {
        si->head = si->tail = f;
    } else {
        si->tail->next = f;
        si->tail = f;
    }

    /* align the field if an alignment argument was given */
    if (si->alignment) {
        DebugMsg(("AddFieldToStruct: align=%u, size=%u, ofs=%u\n", si->alignment, size, offset));
        /* if it's the first field to add, use offset of the parent's current field */
        if (offset == 0 && StructDef.struct_depth > 1) {
            dir_node *parent = peek(StructDef.struct_stack, 0 );
            if (si->alignment < size)
                parent->e.structinfo->tail->sym->offset =
                    (parent->e.structinfo->tail->sym->offset + (si->alignment - 1)) & ( - si->alignment);
            else if (size)
                parent->e.structinfo->tail->sym->offset =
                    (parent->e.structinfo->tail->sym->offset + (size - 1)) & (-size);
        } else {
            if (si->alignment < size)
                offset = (offset + (si->alignment - 1)) & ( - si->alignment);
            else if (size)
                offset = (offset + (size - 1)) & (-size);
        }
        /* adjust the struct's current offset + size.
         The field's size is added in  UpdateStructSize()
         */
        if (StructDef.curr_struct->e.structinfo->typekind != TYPE_UNION ) {
            StructDef.curr_struct->sym.offset = offset;
            if (offset > StructDef.curr_struct->sym.total_size)
                StructDef.curr_struct->sym.total_size = offset;
        }
    }

    sym->offset = offset;

    return( sym );
}

// called by AlignDirective() if ALIGN/EVEN has been found inside
// a struct. It's already verified that <value> is a power of 2.

int AlignInStruct( int value )
{
    if (Parse_Pass == PASS_1 && StructDef.curr_struct->e.structinfo->typekind != TYPE_UNION ) {
        int offset;
        offset = StructDef.curr_struct->sym.offset;
        offset = (offset + (value - 1)) & (-value);
        StructDef.curr_struct->sym.offset = offset;
        if (offset > StructDef.curr_struct->sym.total_size)
            StructDef.curr_struct->sym.total_size = offset;
    }
    return( NOT_ERROR );
}

// called by data_init() when a structure field has been created
// now called on pass 1 only.

void UpdateStructSize(int no_of_bytes)
{
    if( Parse_Pass == PASS_1 ) {
        if ( StructDef.curr_struct->e.structinfo->typekind == TYPE_UNION ) {
            if (no_of_bytes > StructDef.curr_struct->sym.total_size)
                StructDef.curr_struct->sym.total_size = no_of_bytes;
        } else {
            StructDef.curr_struct->sym.offset += no_of_bytes;
            if (StructDef.curr_struct->sym.offset > StructDef.curr_struct->sym.total_size)
                StructDef.curr_struct->sym.total_size = StructDef.curr_struct->sym.offset;
        }
    }
    return;
}

/* called if ORG occurs inside STRUCT/UNION definition */

int SetStructCurrentOffset(int offset)
{
    if ( StructDef.curr_struct->e.structinfo->typekind == TYPE_UNION ) {
        AsmError( ORG_NOT_ALLOWED_IN_UNIONS );
        return( ERROR );
    }
    if( Parse_Pass == PASS_1 ) {
        StructDef.curr_struct->sym.offset = offset;
        /* if an ORG is inside the struct, it cannot be instanced anymore */
        StructDef.curr_struct->e.structinfo->OrgInside = TRUE;
        if (offset > StructDef.curr_struct->sym.total_size)
            StructDef.curr_struct->sym.total_size = offset;
    }
    return( NOT_ERROR );
}

// initialize an array inside a structure
// if there are no brackets, the next comma, '>' or '}' will terminate

static int InitializeArray(field_list *f, char * ptr, char delim )
{
    int  count;
    int  savedToken_Count = Token_Count;
    int  i;
    char buffer[MAX_LINE_LEN];

    DebugMsg(("InitializeArray(%s, init=%s ) enter, items=%u, type=%s\n", f->sym->name, ptr, f->sym->total_length, f->initializer));

    /* a string can be enclosed in <>, {}, "" or '' */
    if (delim == '{' ) {
        ptr++;
        i = strlen(ptr);
        *(ptr+i-1) = NULLC;
    }

    i = Token_Count+1;

    /* if there's nothing, use the default initializer */
    if ( *ptr == NULLC )
        ptr = f->value;

    Token_Count = Tokenize( ptr, i );

    strcpy( buffer, f->initializer );
    strcat( buffer, " ");

    for ( count = f->sym->total_length; count ; count-- ) {
        int lvl = 0;
        int start = i;
        if ( AsmBuffer[i]->token == T_FINAL ) {
            break;
        }
        while ( AsmBuffer[i]->token != T_FINAL ) {

            if ( AsmBuffer[i]->token == T_OP_BRACKET )
                lvl++;
            else if ( AsmBuffer[i]->token == T_CL_BRACKET )
                lvl--;
            else if ( lvl == 0 && AsmBuffer[i]->token == T_COMMA )
                break;
            else if ( AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_DUP ) {
                expr_list opndx;
                /* this is not fool-proved, but works pretty good */
                if (EvalOperand( &start, i, &opndx, FALSE ) != ERROR)
                    if ( opndx.type == EXPR_CONST && opndx.string == NULL )
                        if ( opndx.value > count )
                            AsmErr( TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE, AsmBuffer[i]->string_ptr );
                        else {
                            count -= opndx.value;
                            count++; /* adjust */
                        }
            }

            if ( AsmBuffer[i]->token == T_STRING &&
                 (f->sym->mem_type == MT_BYTE || f->sym->mem_type == MT_SBYTE) &&
                 (AsmBuffer[i]->string_delim == '"' || AsmBuffer[i]->string_delim == '\'')) {
                DebugMsg(("InitializeArray: string init, size=%u\n", AsmBuffer[i]->value ));
                if ( AsmBuffer[i]->value > count ) {
                    AsmError(STRING_OR_TEXT_LITERAL_TOO_LONG);
                    while (AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA)
                        i++;
                    break;
                } else {
                    count -= AsmBuffer[i]->value;
                    count++; /* adjust */
                }
            }

            strcat( buffer, AsmBuffer[i]->string_ptr );
            strcat( buffer, " " );
            i++;
        }
        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA ) {
                strcat( buffer, AsmBuffer[i]->string_ptr );
                i++;
            } else {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                while (AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA)
                    i++;
            }
    }
    InputQueueLine( buffer );

    if ( AsmBuffer[i]->token != T_FINAL )
        AsmErr( TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE, AsmBuffer[i]->string_ptr );

    if (count) {
        DebugMsg(("InitializeArray: remaining items=%u\n", count));
        if (count == f->sym->total_length)
            strcat(buffer, f->value);
        else {
            asm_sym * sym = f->sym;
            while (sym->mem_type == MT_TYPE)
                sym = sym->type;
            if (sym->state == SYM_TYPE && ((dir_node *)sym)->e.structinfo->typekind != TYPE_TYPEDEF )
                sprintf(buffer, "%s %u dup (<>)", f->initializer, count);
            else {
                if (CurrSeg && CurrSeg->seg->e.seginfo->segtype != SEGTYPE_BSS)
                    sprintf(buffer, "%s %u dup (0)", f->initializer, count);
                else
                    sprintf(buffer, "%s %u dup (?)", f->initializer, count);
            }
        }
        InputQueueLine( buffer );
    }

    Token_Count = savedToken_Count;
    DebugMsg(("InitializeArray(%s) exit\n", f->sym->name ));
    return( NOT_ERROR );
}

// initialize a STRUCT/UNION/RECORD data item
// sym = label of data item (might be NULL!)
// struct_symbol = type of data item
// init_string = initializer string
// delim = string start delimiter

// currently this proc emits ASM lines with simple types
// to actually "fill" the structure.

int InitializeStructure( asm_sym *sym, asm_sym *struct_symbol, char *init_string, char delim )
/********************************************************************/
{
    char            *ptr;
    dir_node        *dir;
    field_list      *f;
    int             tmpstate;
    int             nextofs;
    int             i;
    int             savedToken_Count = Token_Count;
    unsigned int    dwRecInit;
    bool            is_record_set;
    field_list      fl;
    expr_list       opndx;
    char            buffer[MAX_LINE_LEN];

    /* skip TYPEDEF aliases */
    while (struct_symbol->mem_type == MT_TYPE) {
        struct_symbol = struct_symbol->type;
    }

    dir = (dir_node *)struct_symbol;
#ifdef DEBUG_OUT
    if (sym)
        DebugMsg(("InitializeStructure(%s:%s) enter, total=%u/%u, init_string=>%s<\n",
                  sym->name, struct_symbol->name,
                  struct_symbol->total_size, struct_symbol->total_length, init_string ));
    else
        DebugMsg(("InitializeStructure(void:%s) enter, total=%u/%u, init_string=>%s<\n",
                  struct_symbol->name, struct_symbol->total_size, struct_symbol->total_length, init_string ));
#endif

    ptr = init_string;

    if (delim != '<' ) {
        if (delim != '{')
            AsmError( MISSING_ANGLE_BRACKET_OR_BRACE_IN_LITERAL );
        ptr++;
        i = strlen(ptr);
        *(ptr+i-1) = NULLC;
    }

    i = Token_Count+1;
    Token_Count = Tokenize( ptr, i );

    if ( dir->e.structinfo->typekind == TYPE_RECORD ) {
        dwRecInit = 0;
        is_record_set = FALSE;
//        sprintf(buffer, "db %u dup (?)", dir->sym.total_size);
//        InputQueueLine( buffer );
//        return( ptr );
    } else if ( dir->e.structinfo->typekind == TYPE_TYPEDEF ) {
        /* there's no field list for typedefs */
        /* create a temporary one for init */
        fl.next = NULL;
        fl.sym = struct_symbol;
        fl.value = "?";
        switch (dir->sym.total_size) {
        case 2:
            fl.initializer = "dw ";
            break;
        case 4:
            fl.initializer = "dd ";
            break;
        case 8:
            fl.initializer = "dq ";
            break;
        default:
            fl.initializer = "db ";
            break;
        }
        dir->e.structinfo->head = &fl;
        tmpstate = struct_symbol->state;
        fl.sym->state = SYM_INTERNAL;
    }

    for( f = dir->e.structinfo->head; f != NULL; f = f->next ) {
        /* put the lines to define the fields of the structure in,
         * using the values specified ( if any ) or the default ones otherwise
         */
        DebugMsg(("InitializeStructure: init %s, default=>%s<\n", f->sym->name, f->value));

        if (f->sym->mem_type == MT_TYPE && ((dir_node *)f->sym->type)->e.structinfo->typekind != TYPE_TYPEDEF ) {
            if ( f->initializer == NULL )  { /* embedded struct? */
                if (AsmBuffer[i]->token == T_STRING) {
                    InitializeStructure( sym, f->sym, AsmBuffer[i]->string_ptr, AsmBuffer[i]->string_delim );
                    i++;
                } else {
                    if ( AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA ) {
                        AsmError(INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM);
                    }
                    InitializeStructure( sym, f->sym, "", '<' );
                }
            } else { /* or a structured field? */
                if (AsmBuffer[i]->token == T_STRING) {
                    InitializeStructure( sym, f->sym->type, AsmBuffer[i]->string_ptr, AsmBuffer[i]->string_delim );
                    i++;
                } else {
                    if ( AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA ) {
                        AsmError(INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM);
                    }
                    InitializeStructure( sym, f->sym->type, "", '<' );
                }
            }
        } else if (f->sym->mem_type == MT_BITS) {
            opndx.type = EXPR_CONST;
            opndx.string = NULL;
            if (AsmBuffer[i]->token == T_COMMA || AsmBuffer[i]->token == T_FINAL) {
                if (f->value) {
                    int j = Token_Count + 1;
                    int max_item = Tokenize(f->value, j);
                    EvalOperand(&j, max_item, &opndx, TRUE);
                    is_record_set = TRUE;
                } else {
                    opndx.value = 0;
                }
            } else {
                EvalOperand(&i, Token_Count, &opndx, TRUE);
                is_record_set = TRUE;
            }
            if (opndx.type != EXPR_CONST || opndx.string != NULL)
                AsmError(SYNTAX_ERROR);
            if (f->sym->total_size < 32) {
                unsigned long dwMax = (1 << f->sym->total_size);
                if (opndx.value >= dwMax)
                    AsmError(INITIALIZER_MAGNITUDE_TOO_LARGE);
            }
            dwRecInit |= opndx.value << f->sym->offset;
#if 0
            if (AsmBuffer[i]->token == T_COMMA) {
                ptr = AsmBuffer[i]->pos;
                i++;
            } else {
                if (AsmBuffer[i]->token == T_FINAL)
                    ptr = "";
                else if (AsmBuffer[i]->token != T_NUM)
                    ptr = AsmBuffer[i]->pos;
                break;
            }
#endif
        } else if (f->sym->total_length > 1) {
            if (AsmBuffer[i]->token == T_STRING) {
                InitializeArray( f, AsmBuffer[i]->string_ptr, AsmBuffer[i]->string_delim );
                i++;
            } else {
                if ( AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA ) {
                    AsmError(INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM);
                }
                InitializeArray( f, "", '<' );
            }
        } else {
            strcpy( buffer, f->initializer );
            strcat( buffer, " " );
            if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_COMMA ) {
                strcat( buffer, f->value );
            } else {
                int lvl = 0; /* ignore commas enclosed in () */
                while ( AsmBuffer[i]->token != T_FINAL) {
                    if ( AsmBuffer[i]->token == T_OP_BRACKET )
                        lvl++;
                    else if ( AsmBuffer[i]->token == T_CL_BRACKET )
                        lvl--;
                    else if ( lvl == 0 && AsmBuffer[i]->token == T_COMMA )
                        break;
                    if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<') {
                        char *src = AsmBuffer[i]->string_ptr;
                        ptr = buffer + strlen(buffer);
                        *ptr++ = '<';
                        while (*src) {
                            if (*src == '<' || *src == '>' || *src == '!')
                                *ptr++ = '!';
                            *ptr++ = *src++;
                        }
                        *ptr++ = '>';
                        *ptr = NULLC;
                    } else
                        strcat( buffer, AsmBuffer[i]->string_ptr );
                    strcat( buffer, " " );
                    i++;
                }
            }
            InputQueueLine( buffer );

            /* add padding bytes */

            if ( f->next == NULL || dir->e.structinfo->typekind == TYPE_UNION )
                nextofs = struct_symbol->total_size;
            else
                nextofs = f->next->sym->offset;

            if (f->sym->offset + f->sym->total_size < nextofs) {
                sprintf(buffer,"db %u dup (?) ;padding",
                        nextofs - (f->sym->offset + f->sym->total_size));
                InputQueueLine( buffer );
            }
        }
        /* for a union, just the first field is initialized */
        if ( dir->e.structinfo->typekind == TYPE_UNION )
            break;

        if (f->next != NULL) {

            if ( AsmBuffer[i]->token != T_FINAL )
                if ( AsmBuffer[i]->token == T_COMMA)
                    i++;
                else {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                    while (AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA)
                        i++;
                }
        }
    }  /* end for */

    if ( dir->e.structinfo->typekind == TYPE_RECORD ) {
        char * p;
        if (dir->sym.mem_type == MT_BYTE)
            p = "db";
        else if (dir->sym.mem_type == MT_WORD)
            p = "dw";
        else
            p = "dd";
        if (is_record_set)
            sprintf(buffer,"%s 0%Xh", p, dwRecInit);
        else
            sprintf(buffer,"%s ?", p);
        InputQueueLine( buffer );
    }

    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE, AsmBuffer[i]->string_ptr );
    }

    /* restore the temporarily modified TYPEDEF */
    if (dir->e.structinfo->head == &fl) {
        dir->e.structinfo->head = NULL;
        struct_symbol->state = tmpstate;
    }

    DebugMsg(("InitializeStructure exit, ptr=>%s<\n", ptr ));

    Token_Count = savedToken_Count;

    return( NOT_ERROR );
}

// TYPEDEF

asm_sym * CreateTypeDef(char * name, int * pi)
{
    char        *token;
    int         i = *pi;
    struct asm_sym      *sym = NULL;
    struct asm_sym      *symtype;
    dir_node            *dir;
    int                 type;
    int                 size;
    int                 oldsize;
    int                 indirection = 0;
    int                 ptrqual;

    if (*name) {
        sym = SymSearch( name );
    }
    if( sym == NULL ) {
        dir = dir_insert( name, TAB_TYPE );
        sym = &dir->sym;
    } else {
        dir = (dir_node *)sym;
        if (sym->state == SYM_UNDEFINED) {
            dir_change( dir, TAB_TYPE );
        } else {
            if ((sym->state != SYM_TYPE) || (dir->e.structinfo->typekind != TYPE_TYPEDEF )) {
                AsmErr( SYMBOL_PREVIOUSLY_DEFINED, sym->name );
                return( NULL );
            }
            /* MASM allows to have the TYPEDEF included several times */
            /* but the types must be identical! */
        }
    }

#if 0
    /* "name TYPEDEF" is a valid syntax */
    if (i >= Token_Count) {
        AsmError( SYNTAX_ERROR );
        return( NULL );
    }
#endif
    dir->e.structinfo->typekind = TYPE_TYPEDEF;
    dir->sym.defined = TRUE;

    token = AsmBuffer[i]->string_ptr;
    type = ERROR;
    if (AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_COMMA)
        type = 0; /* for a VOID type, use first index in simple type */
    else if (AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE)
        type = FindSimpleType( AsmBuffer[i]->value );

    if( type == ERROR ) {
        /* it's not a simple type, check for PROTO qualifier */
        if (AsmBuffer[i]->value == T_PROTO) {
            dir_node * dir2;  /* create an anonymous PROC entry */
            dir2 = dir_insert_ex( "", TAB_PROC );
            DebugMsg(("TypeDef PROTO, call ExamineProc, i=%d\n", i));
            if( ExamineProc( dir2, i+1, FALSE ) == ERROR )
                return (NULL);
            DebugMsg(("TypeDef PROTO, returned from ExamineProc\n"));
            sym->mem_type = MT_PROC;
            dir->e.structinfo->target = (asm_sym *)dir2;
            *pi = Token_Count;
            return( sym );
        }
        /* besides PROTO typedef, an arbitrary type is ok */
        if( !(symtype = IsLabelType( token ) ) ) {
            AsmError( INVALID_QUALIFIED_TYPE );
            return( NULL );
        }
        /* there are many type redefinitions in Win32.
         don't be too rigid, best is just to compare sizes.
         here the source type is an arbitrary type (MT_TYPE)
         */
#if 0
        oldsize = symtype->total_size;

        if (sym->type)
            size = sym->type->total_size;
        else if (sym->mem_type == MT_EMPTY)
            size = oldsize;
        else
            size = SizeFromMemtype(sym->mem_type, Use32);

        if (size != oldsize) {
            DebugMsg(("TypeDef error, newsize=%u, oldsize=%u\n", size, oldsize));
            AsmErr( SYMBOL_TYPE_CONFLICT, sym->name);
            return( NULL );
        }
#else
        if (((sym->mem_type != MT_TYPE) && (sym->mem_type != MT_EMPTY)) ||
            ((sym->total_size != 0) && (sym->total_size != symtype->total_size)) ||
            ((sym->type != NULL) && (sym->type != symtype))) {
            AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( NULL );
        }
#endif
        i++;
        sym->mem_type = MT_TYPE;
        sym->total_size = symtype->total_size;
        sym->type = symtype;
    } else {
        /* it's a simple or void type */
        size = SimpleType[type].size;
        if (size == -1) /* if it's a pointer, get true size */
            size = SizeFromMemtype(SimpleType[type].mem_type, Use32);
#if 1
        /* just check size, not mem_type */
        oldsize = sym->total_size;
        if (oldsize == 0)
            oldsize = size;
        if (size != oldsize) {
            DebugMsg(("TypeDef error, new type/size=%u/%u, old type/size=%u/%u\n", SimpleType[type].mem_type, size, sym->mem_type, sym->total_size));
            AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( NULL );
        }
#else
        if (((sym->mem_type != MT_EMPTY) && (sym->mem_type != SimpleType[type].mem_type)) ||
            ((sym->total_size != 0) && (sym->total_size != size)) ||
            (sym->type != NULL) ) {
            DebugMsg(("TypeDef error, new type/size=%u/%u, old type/size=%u/%u\n", SimpleType[type].mem_type, size, sym->mem_type, sym->total_size));
            AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( NULL );
        }
#endif
        /* set memtype. Can be either a simple type alias or a
         pointer to anything.
         */
        sym->mem_type = SimpleType[type].mem_type;

        ptrqual = EMPTY;
        while (AsmBuffer[i]->token == T_RES_ID) {
            switch (AsmBuffer[i]->value) {
            case T_FAR:
            case T_FAR16:
            case T_FAR32:
            case T_NEAR:
            case T_NEAR16:
            case T_NEAR32:
                if (ptrqual == EMPTY) {
                    ptrqual = AsmBuffer[i]->value;
                    type = FindSimpleType(ptrqual);
                    size = SimpleType[type].size;
                    if (size == -1)
                        size = SizeFromMemtype(SimpleType[type].mem_type, Use32);
                }
                i++;
                if (indirection == 0 &&
                    (AsmBuffer[i]->token != T_RES_ID ||
                     AsmBuffer[i]->value != T_PTR)) {
                    AsmError( INVALID_QUALIFIED_TYPE );
                    return( NULL );
                }
                continue;
            case T_PTR:
                sym->mem_type = MT_PTR;
                indirection++;
                i++;
                continue;
            }
            DebugMsg(("TypeDef: pointer type\n"));
            break;
        }

        dir->e.structinfo->indirection = indirection;

        DebugMsg(("TypeDef: token=%u\n", AsmBuffer[i]->token));
        if ((AsmBuffer[i]->token == T_ID) || (AsmBuffer[i]->token == T_RES_ID)) {
            symtype = SymSearch( AsmBuffer[i]->string_ptr );
#ifdef DEBUGOUT
            if (symtype)
                DebugMsg(("TypeDef: ptr dest=%X, name=%s\n", symtype, symtype->name));
            else
                DebugMsg(("TypeDef: ptr dest=%X\n", symtype));
#endif
            if (symtype)
                /* accept pointers to PROTOs and arbitrary types */
                if (symtype->mem_type == MT_PROC) {
                    DebugMsg(("TypeDef: type is function pointer\n"));
                    dir->e.structinfo->target = symtype;
#if 1
                } else if (symtype->state == SYM_TYPE) {
                    DebugMsg(("TypeDef: type is arbitrary type\n"));
                    dir->e.structinfo->target = symtype;
#endif
                } else {
                    DebugMsg(("TypeDef: invalid type, symbol state=%u\n", symtype->state));
                    AsmErr( SYMBOL_TYPE_CONFLICT, symtype->name);
                    return( NULL );
                }
            i++;
        }
        sym->total_size = size;
    }
    *pi = i;
    return((asm_sym *)dir);
}

// generate a TYPEDEF. Called on pass 1 only.
// usual syntax is:
// <type name> TYPEDEF [[far|near [ptr]]<ref type name>]
// but this function might also be called by EXTERN(def)
// and create a pointer type then:
// EXTERNDEF: ptr <type>

int TypeDef( int i )
{
    char *name;

    DebugMsg(("TypeDef enter, i=%d\n", i));
    if( i < 0 ) {
        AsmError( TYPE_MUST_HAVE_A_NAME );
        return( ERROR );
    }
    name = AsmBuffer[i]->string_ptr;

    i += 2;

    if (CreateTypeDef( name, &i) == NULL)
        return(ERROR);
    if (AsmBuffer[i]->token != T_FINAL) {
        DebugMsg(("TypeDef: unexpected token %u, idx=%u\n", AsmBuffer[i]->token, i));
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    return( NOT_ERROR );
}

// generate a RECORD. Called on pass 1 only

int RecordDef( int i )
/********************/
{
    char *name;
    struct asm_sym *sym;
    dir_node *dir;
    field_list  *f;
    int num;
    int value;
    int name_loc;
    expr_list opndx;

    DebugMsg(("RecordDef enter, i=%u\n", i));
    if (i < 1) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
#if 0
    /* it's strange, but a record definition can be placed
     inside a STRUCT, although it will still be global */
    if (StructDef.struct_depth > 0) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
#endif
    name = AsmBuffer[i-1]->string_ptr;
    if (SymSearch( name )) {
        AsmErr( SYMBOL_ALREADY_DEFINED, name );
        return( ERROR );
    }
    dir = dir_insert( name, TAB_TYPE );
    dir->e.structinfo->typekind = TYPE_RECORD;
    dir->sym.defined = TRUE;
    i++;
    for (num = 0;;) {
        if (AsmBuffer[i]->token != T_ID) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        name_loc = i;
        i++;
        if (AsmBuffer[i]->token != T_COLON) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;
        /* get width */
        if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
            return( ERROR );
        if (opndx.type != EXPR_CONST || opndx.string != NULL) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if (opndx.value + num > 32) {
            AsmError( TOO_MANY_BITS_IN_RECORD );
            return( ERROR );
        }
        /* record field names are global! (Masm design flaw) */
        sym = SymLookup(AsmBuffer[name_loc]->string_ptr);
        if (!sym)
            return( ERROR);
        if (sym->state != SYM_UNDEFINED) {
            AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
            return (ERROR);
        }
        sym->state = SYM_STRUCT_FIELD;
        sym->mem_type = MT_BITS;
        sym->total_size = opndx.value;
        num = num + opndx.value;
        f = AsmAlloc( sizeof( field_list ) );
        f->next = NULL;
        f->sym = sym;
        f->value = NULL;
        f->initializer = NULL;
        if( dir->e.structinfo->head == NULL ) {
            dir->e.structinfo->head = dir->e.structinfo->tail = f;
        } else {
            dir->e.structinfo->tail->next = f;
            dir->e.structinfo->tail = f;
        }
        /* optional initializer? */
        if (*AsmBuffer[i]->string_ptr == '=') {
            int count = 0;
            int j;
            i++;
            for( j = i;
                 AsmBuffer[j]->token != T_FINAL && AsmBuffer[j]->token != T_COMMA;
                 j++ ) {
                if( AsmBuffer[j]->string_ptr != NULL ) {
                    count += strlen( AsmBuffer[j]->string_ptr ) + 1;
                }
            }
            f->value = AsmAlloc(count );
            f->value[0] = NULLC;
            for (;i < j;i++) {
                strcat(f->value, AsmBuffer[i]->string_ptr);
                if (i != j - 1)
                    strcat(f->value," ");
            }
        }
        if (AsmBuffer[i]->token == T_FINAL)
            break;
        if (AsmBuffer[i]->token != T_COMMA) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
    } /* end for () */

    /* now calc size in bytes and set the bit positions */

    if (num > 16) {
        dir->sym.total_size = 4;
        dir->sym.mem_type = MT_DWORD;
    } else if (num > 8) {
        dir->sym.total_size = 2;
        dir->sym.mem_type = MT_WORD;
    } else {
        dir->sym.total_size = 1;
        dir->sym.mem_type = MT_BYTE;
    }
    /* if the BYTE/WORD/DWORD isn't used fully, shift bits to the right! */
    // num = dir->sym.total_size * 8;

    /* set the bit position */
    for (f=dir->e.structinfo->head;f;f = f->next) {
        num = num - f->sym->total_size;
        f->sym->offset = num;
    }
    DebugMsg(("RecordDef exit, no error\n"));
    return( NOT_ERROR );
}

