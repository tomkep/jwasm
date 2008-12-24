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
#include "segment.h"
#include "proc.h"
#include "input.h"
#include "tokenize.h"
#include "types.h"
#include "expreval.h"
#include "labels.h"
#include "symbols.h"
#include "listing.h"
#include "fastpass.h"

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
        return( SymSearch( name ) );
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

static bool AreStructsEqual(dir_node *newstr, dir_node *oldstr)
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
        /* for global member names, don't check the name if it's "" */
        if ( ModuleInfo.oldstructs && *fnew->sym->name == NULLC )
            ;
        else if (0 != strcmp(fold->sym->name, fnew->sym->name)) {
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

ret_code StructDirective( int i )
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

    /* top level structs/unions must have a name */
    /* if embedded in other structs/unions they can be anonymous */
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
        alignment = 1 << ModuleInfo.fieldalign;

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
                dir = dir_insert( name, SYM_TYPE );
            else {
                /* a nested structure is split in an anonymous STRUCT type
                 and a struct field with/without name
                 */
                field_list *f;
                dir = dir_insert_ex( "", SYM_TYPE);
                sym = AddFieldToStruct(name_loc, -1, MT_TYPE, dir, 0);
                alignment = StructDef.curr_struct->e.structinfo->alignment;
            }
        } else {
            /* the symbol exists already */
            dir = (dir_node *)sym;
            if( sym->state == SYM_UNDEFINED ) {
                dir_change( dir, SYM_TYPE );
            } else if( sym->state == SYM_TYPE && (StructDef.struct_depth == 0)) {
                /* structure redefinition */
                redef_struct = dir;
                dir = dir_insert_ex( name, SYM_TYPE );
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

        if ( ModuleInfo.list ) {
            if ( StructDef.struct_depth )
                LstWrite( LSTTYPE_STRUCT, StructDef.curr_struct->sym.total_size, NULL );
            else
                LstWrite( LSTTYPE_STRUCT, 0, NULL );
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

            if (dir->e.structinfo->alignment > 1) {
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

            if ( FileInfo.file[LST] ) {
                LstWrite(LSTTYPE_STRUCT, size, dir->sym.name);
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

struct asm_sym * AddFieldToStruct( int name_loc, int loc, memtype mem_type, dir_node * vartype, int size)
/*****************************/
{
    int offset;
    int count;
    int i;
    char * name;
    char * init;
    struct_info *si;
    field_list  *f;
    asm_sym     *sym;

    si = StructDef.curr_struct->e.structinfo;
    offset = StructDef.curr_struct->sym.offset;

#ifdef DEBUG_OUT
    if (name_loc < 0)
        DebugMsg(("AddFieldToStruct(%s): anonymous, curr ofs=%u\n", StructDef.curr_struct->sym.name, offset ));
    else
        DebugMsg(("AddFieldToStruct(%s): name=%s curr ofs=%u\n", StructDef.curr_struct->sym.name, AsmBuffer[name_loc]->string_ptr, offset ));
#endif

    if (name_loc >= 0) {
        /* the field has a name */
        name = AsmBuffer[name_loc]->string_ptr;
        /* check if field name is already used */
        /* RECORD fields names, which are global, aren't handled here */
        sym = SearchNameInStruct((asm_sym *)StructDef.curr_struct, name, (unsigned int *)&i);
        if (sym) {
            if ( ModuleInfo.oldstructs &&
                 sym->state == SYM_STRUCT_FIELD &&
                 redef_struct != NULL ) {
            } else
                AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
            name = "";
        }
    } else
        name = "";

    /* alloc the items needed */

    if ( ModuleInfo.oldstructs == TRUE && *name != NULLC )
        sym = SymCreate(name, TRUE);
    else
        sym = SymCreate(name, FALSE);

    sym->state = SYM_STRUCT_FIELD;
    sym->defined = TRUE;
    sym->mem_type = mem_type;
    sym->type = &vartype->sym;
    // ok to do?
    // sym->total_size = SizeFromMemtype(mem_type, Use32);

    f = AsmAlloc( sizeof( field_list ) );

    f->next = NULL;
    f->sym = sym;

    if (loc != -1) {

        i = strlen( AsmBuffer[loc]->string_ptr );
        DebugMsg(("AddFieldToStruct(%s): type=>%s<\n", StructDef.curr_struct->sym.name, AsmBuffer[loc]->string_ptr ));
        f->initializer = AsmAlloc( i + 1 );
        strcpy( f->initializer, AsmBuffer[loc]->string_ptr );

        /* now add the value to initialize the struct to */

        //if ( AsmBuffer[loc+1]->token == T_STRING &&
        //     AsmBuffer[loc+1]->string_delim == '<') {
        //    f->value = AsmAlloc( AsmBuffer[loc+1]->value + 1 );
        //    strcpy( f->value, AsmBuffer[loc+1]->string_ptr );
        //} else {
            init = AsmBuffer[loc]->pos + i;
            while ( isspace( *init)) init++;
            f->value = AsmAlloc( strlen( init ) + 1 );
            strcpy( f->value, init );
        //}
        DebugMsg(("AddFieldToStruct(%s): initializer=>%s<\n", StructDef.curr_struct->sym.name, f->value ));

    } else {
        DebugMsg(("AddFieldToStruct(%s): no initializer<\n", StructDef.curr_struct->sym.name ));
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
    if ( si->alignment > 1 ) {
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

ret_code AlignInStruct( int value )
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

ret_code SetStructCurrentOffset(int offset)
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

// TYPEDEF worker

asm_sym *CreateTypeDef(char * name, int * pi)
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
        dir = dir_insert( name, SYM_TYPE );
        sym = &dir->sym;
        sym->use32 = Use32;
    } else {
        dir = (dir_node *)sym;
        if (sym->state == SYM_UNDEFINED) {
            dir_change( dir, SYM_TYPE );
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
    type = -1;
    if (AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_COMMA)
        type = ST_NULL;
    else if ( AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE )
        type = FindSimpleType( AsmBuffer[i]->value );

    if( type == -1 ) {
        /* it's not a simple type, check for PROTO qualifier */
        if (AsmBuffer[i]->value == T_PROTO) {
            dir_node * dir2;  /* create a PROTOtype item without name */
            dir2 = (dir_node *)CreateProc( NULL, "" );
            DebugMsg(("CreateTypeDef PROTO, call ExamineProc, i=%d\n", i));
            if( ExamineProc( dir2, i+1, FALSE ) == ERROR )
                return (NULL);
            DebugMsg(("CreateTypeDef PROTO, returned from ExamineProc\n"));
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
            DebugMsg(("CreateTypeDef error, newsize=%u, oldsize=%u\n", size, oldsize));
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
        DebugMsg(("CreateTypeDef ok, size=%u, mem_type=MT_TYPE, type=%X, i=%u\n", sym->total_size, sym->type, i ));
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
            DebugMsg(("CreateTypeDef error, new type/size=%u/%u, old type/size=%u/%u\n", SimpleType[type].mem_type, size, sym->mem_type, sym->total_size));
            AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( NULL );
        }
#else
        if (((sym->mem_type != MT_EMPTY) && (sym->mem_type != SimpleType[type].mem_type)) ||
            ((sym->total_size != 0) && (sym->total_size != size)) ||
            (sym->type != NULL) ) {
            DebugMsg(("CreateTypeDef error, new type/size=%u/%u, old type/size=%u/%u\n", SimpleType[type].mem_type, size, sym->mem_type, sym->total_size));
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
                    type = FindSimpleType( ptrqual );
                    size = SimpleType[type].size;

                    if ( ptrqual == T_NEAR16 || ptrqual == T_FAR16 )
                        dir->sym.use32 = FALSE;
                    else if ( ptrqual == T_NEAR32 || ptrqual == T_FAR32 )
                        dir->sym.use32 = TRUE;
                    if ( ptrqual == T_FAR || ptrqual == T_FAR16 || ptrqual == T_FAR32 )
                        dir->sym.isfar = TRUE;
                    else
                        dir->sym.isfar = FALSE;
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
            DebugMsg(("CreateTypeDef: pointer type\n"));
            break;
        }

        dir->e.structinfo->indirection = indirection;

        DebugMsg(("CreateTypeDef: i=%u, token=%u\n", i, AsmBuffer[i]->token));
        if ((AsmBuffer[i]->token == T_ID) || (AsmBuffer[i]->token == T_RES_ID)) {
            symtype = SymSearch( AsmBuffer[i]->string_ptr );
#ifdef DEBUGOUT
            if (symtype)
                DebugMsg(("CreateTypeDef: ptr dest=%X, name=%s\n", symtype, symtype->name));
            else
                DebugMsg(("CreateTypeDef: ptr dest=%X\n", symtype));
#endif
            if (symtype)
                /* accept pointers to PROTOs and arbitrary types */
                if (symtype->mem_type == MT_PROC) {
                    DebugMsg(("TypeDef: type is function pointer\n"));
                    dir->e.structinfo->target = symtype;
#if 1
                } else if (symtype->state == SYM_TYPE) {
                    DebugMsg(("CreateTypeDef: type is arbitrary type\n"));
                    dir->e.structinfo->target = symtype;
#endif
                } else {
                    DebugMsg(("CreateTypeDef: invalid type, symbol state=%u\n", symtype->state));
                    AsmErr( SYMBOL_TYPE_CONFLICT, symtype->name);
                    return( NULL );
                }
            i++;
        }
        sym->total_size = size;
        DebugMsg(("CreateTypeDef ok, size=%u, mem_type=%u, indirection=%u\n", size, sym->mem_type, indirection ));
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

ret_code TypeDef( int i )
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

ret_code RecordDef( int i )
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
    dir = dir_insert( name, SYM_TYPE );
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

