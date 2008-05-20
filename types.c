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
* Description:  STRUCT, UNION, RECORD and TYPEDEF derectives
* virtually rewritten for JWasm.
*
****************************************************************************/


#include "asmglob.h"
#include <ctype.h>

#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "asmdefs.h"
#include "asminput.h"
#include "types.h"
#include "expreval.h"
#include "labels.h"
#include "symbols.h"
#include "fatal.h"

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

a_definition_struct Definition = { 0, NULL, NULL };
static dir_node * redef_struct = 0;

// search a name in a struct's namespace

struct asm_sym *SearchNameInStruct( asm_sym *tstruct, const char *name, unsigned int * poffset)
{
    field_list * fl = ((dir_node *)tstruct)->e.structinfo->head;

    for (;fl;fl = fl->next)
        /* recursion: if member has no name, check if it is a structure
         and scan this structure's namespace then */
        if (*(fl->fsym->name) == 0 ) {
            asm_sym *sym;
            /* there are 2 cases: an anonymous inline struct ... */
            if (fl->fsym->state == SYM_TYPE) {
                if (sym = SearchNameInStruct(fl->fsym, name, poffset)) {
                    *poffset += fl->fsym->offset;
                    return( sym );
                }
            /* or an anonymous structured field */
            } else if ((fl->fsym->mem_type == MT_TYPE)) {
                if (sym = SearchNameInStruct(fl->fsym->type, name, poffset)) {
                    *poffset += fl->fsym->offset;
                    return( sym );
                }
            }
        } else if (SymCmpFunc( name, fl->fsym->name ) == 0 ) {
            DebugMsg(("SearchNameInStruct: '%s' found in struct namespace\n", name));
            return( fl->fsym );
        }
    return (NULL);
}

// check if a struct has changed

static bool AreStructsEqual(dir_node *oldstr, dir_node *newstr)
{
    field_list  *fold = oldstr->e.structinfo->head;
    field_list  *fnew = newstr->e.structinfo->head;

    DebugMsg(("AreStructsEqual enter\n"));
    for (;fold; fold = fold->next, fnew = fnew->next) {
        if (!fnew) {
            DebugMsg(("AreStructsEqual: fields don't match\n"));
            return FALSE;
        }
        if (0 != strcmp(fold->fsym->name, fnew->fsym->name)) {
            DebugMsg(("AreStructsEqual: type name of field changed\n"));
            return FALSE;
        }
        if (fold->fsym->offset != fnew->fsym->offset) {
            DebugMsg(("AreStructsEqual: offset of field %s changed: %u - %u\n", fold->fsym->name, fold->fsym->offset, fnew->fsym->offset));
            return FALSE;
        }
        if (fold->fsym->total_size != fnew->fsym->total_size) {
            DebugMsg(("AreStructsEqual: total_size of field changed\n"));
            return FALSE;
        }
    }
    if (fnew)
        return FALSE;
    return TRUE;
}

// i is the token which contains STRUCT, STRUC, UNION or ENDS

int StructDef( int i )
/********************/
{
    char *name;
    unsigned alignment = 0;
    unsigned int offset;
    struct asm_sym *sym;
    memtype mem_type;
    dir_node *dir;
    dir_node *parent;

    DebugMsg(("StructDef enter, i=%u\n", i));

    /* structs/unions must have a name if at level 0 */
    /* inside other structs/unions they can be anonymous */
    if (( Definition.struct_depth == 0) && (i == 0 )) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (i) {
        name = AsmBuffer[i-1]->string_ptr;
    } else if (AsmBuffer[i+1]->token != T_FINAL &&
               is_valid_id_char(*(AsmBuffer[i+1]->string_ptr)))
               /* the section name might be a reserved word! */
        name = AsmBuffer[i+1]->string_ptr;
    else
        name = "";

    switch( AsmBuffer[i]->value ) {
    case T_STRUC:
    case T_STRUCT:
    case T_UNION:
        if (Parse_Pass > PASS_1) {
            Definition.struct_depth++;
            break;
        }

        if ((Definition.struct_depth > 0) && (i > 0)) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        /* get an optional alignment argument */
        if ((Definition.struct_depth == 0) && (AsmBuffer[i+1]->token != T_FINAL)) {
            int j = i+1;
            int power;
            expr_list opndx;
            /* get the optional alignment parameter */
            if ((EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR) ||
                (opndx.type != EXPR_CONST || opndx.string != NULL)) {
                AsmError( SYNTAX_ERROR );
                return( ERROR );
            }
            alignment = opndx.value;
            for( power = 1; power < alignment; power <<= 1 );
            if( power != alignment ) {
                AsmError( POWER_OF_2 );
                return( ERROR );
            }
            DebugMsg(("StructDef(%s) alignment=%u\n", name, alignment));
            /* there might also be the NONUNIQUE keyword */
            if (AsmBuffer[j]->token == T_COMMA) {
                j++;
                if (AsmBuffer[j]->token == T_ID &&
                    stricmp(AsmBuffer[j]->string_ptr, "NONUNIQUE") == 0) {
                    j++;
                    /* currently NONUNIQUE is just skipped */
                }
            }
            if (AsmBuffer[j]->token != T_FINAL) {
                AsmError(SYNTAX_ERROR);
            }
        }
        /* does struct have a name? */
        if (*name) {
            /* the "top-level" struct is part of the global namespace */
            if (Definition.struct_depth == 0) {
                sym = SymSearch( name );
                DebugMsg(("StructDef: SymSearch (%s)=%X (curr struct=%X)\n", name, sym, Definition.curr_struct));
            } else {
                /* a local structure name is to be searched in the
                 struct's namespace
                 */
                sym = SearchNameInStruct((asm_sym *)Definition.curr_struct, name, &offset);
                DebugMsg(("StructDef(%s): SearchNameInStruc()=%X\n", name, sym));
            }
        } else {
            /* anonymous struct/union */
            if (Parse_Pass == PASS_1) {
                sym = NULL;
            } else
                sym = (asm_sym *)Definition.curr_struct;
        }
        if( Parse_Pass == PASS_1 ) {
            if( sym == NULL ) {
                /* is it a global STRUCT? */
                if (Definition.struct_depth == 0)
                    dir = dir_insert( name, TAB_STRUCT );
                else {
                    /* this could possibly be replaced by a call of
                     AddFieldToStruct? */
                    field_list *f;
                    dir = dir_insert_ex( name, TAB_STRUCT);
                    alignment = Definition.curr_struct->e.structinfo->alignment;
                    f = AsmAlloc( sizeof( field_list ) );
                    f->next = NULL;
                    f->value = NULL;
                    f->initializer = NULL;
                    f->fsym = (asm_sym *)dir;
                    if( Definition.curr_struct->e.structinfo->head == NULL ) {
                        Definition.curr_struct->e.structinfo->head = f;
                        Definition.curr_struct->e.structinfo->tail = f;
                    } else {
                        Definition.curr_struct->e.structinfo->tail->next = f;
                        Definition.curr_struct->e.structinfo->tail = f;
                    }
                }
            } else {
                dir = (dir_node *)sym;
                if( sym->state == SYM_UNDEFINED ) {
                    dir_change( dir, TAB_STRUCT );
                } else if( sym->state == SYM_TYPE && (Definition.struct_depth == 0)) {
                    /* structure redefinition */
                    if (SymChangeName( name, ".$$$$$$$" ) == ERROR) {
                        AsmErr( SYMBOL_ALREADY_DEFINED, name );
                        return( ERROR );
                    }
                    redef_struct = dir;
                    dir = dir_insert( name, TAB_STRUCT );
                } else {
                    AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
                    return( ERROR );
                }
            }
        } else {
            /* pass > 1 */
            dir = (dir_node *)sym;
        }

        if (Parse_Pass == PASS_1) {
            dir->e.structinfo->alignment = alignment;

            /* set the struct/union's offset. this value will not
             change, offset of members are calculated from the "total_size"
             field value */

            if (Definition.struct_depth) {
                /* set the offset (which is ALWAYS a local offset */
                if (Definition.curr_struct->e.structinfo->isUnion)
                    dir->sym.offset = 0;
                else
                    dir->sym.offset = Definition.curr_struct->sym.total_size;
                DebugMsg(("StructDef: nested structure, offset=%X\n", dir->sym.offset));
            }
            dir->e.structinfo->isOpen = TRUE;

            if (AsmBuffer[i]->value == T_UNION)
                dir->e.structinfo->isUnion = TRUE;

            if (Definition.struct_depth)
                dir->e.structinfo->isInline = TRUE;

        }
        i++;
        DebugMsg(("StructDef(%s): token following: %X\n", name, AsmBuffer[i]->token));
        /* even if the current is null */
        push( &( Definition.struct_stack ), Definition.curr_struct );
        Definition.curr_struct = dir;
        Definition.struct_depth++;
        break;
    case T_ENDS:
        if (Parse_Pass > PASS_1) {
            Definition.struct_depth--;
            break;
        }
        if( Definition.struct_depth) {
            /* an inline struct will end with a simple ENDS without name */
            /* an global struct will end with a <name ENDS>  */
#ifdef DEBUG_OUT
            if (Definition.curr_struct == NULL) {
                DebugMsg(("StructDef(T_ENDS), current struct is NULL, but struct depth is > 0!!!\n"));
                AsmError( NO_SEGMENT_OPENED );
                AsmShutDown();
                exit(1);
            }
#endif
            DebugMsg(("StructDef(T_ENDS), level=%u, struct size=%u, alignmnt=%u\n",
                      Definition.struct_depth,
                      Definition.curr_struct->sym.total_size,
                      Definition.curr_struct->e.structinfo->alignment));
            dir = Definition.curr_struct;
            /* inline "struct"/"union" must be first identifier in a line */
            if (((dir->e.structinfo->isInline) && (i == 0)) ||
                (Parse_Pass != PASS_1) ||
                (name && strcmp( name, dir->sym.name ) == 0)) {

                unsigned int size;

                if (dir->e.structinfo->alignment) {
                    dir->sym.total_size = (dir->sym.total_size + dir->e.structinfo->alignment - 1) & (-dir->e.structinfo->alignment);
                    DebugMsg(("StructDef(T_ENDS):, struct size after final alignment=%u\n", dir->sym.total_size));
                }
                dir->e.structinfo->isOpen = FALSE;

                dir->sym.defined = TRUE;

                size = dir->sym.total_size;
                Definition.curr_struct = pop( &( Definition.struct_stack ) );
                Definition.struct_depth--;

                if (Parse_Pass == PASS_1) {

#if 1
                    /* to allow a direct structure access */
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
                        dir->sym.mem_type = MT_OWORD;
                    }
#endif

                    /* reset redefine */
                    if (Definition.struct_depth == 0) {
                        if (redef_struct) {
                            if (AreStructsEqual(dir, redef_struct)) {
                                ;
                            } else
                                AsmError( NON_BENIGN_STRUCT_REDEFINITION );
                            /* delete the "redefinition" */
                            DebugMsg(("delete the new definition\n"));
                            SymTakeOut(name);
                            DebugMsg(("rename the old definition\n"));
                            SymChangeName( ".$$$$$$$", name );
                            redef_struct = NULL;
                        }
                    } else {

                        if (Definition.curr_struct->e.structinfo->isUnion) {
                            if (size > Definition.curr_struct->sym.total_size) {
                                Definition.curr_struct->sym.total_size = size;
                            }
                        } else {
                            Definition.curr_struct->sym.total_size += size;
                        }
                        DebugMsg(("StructDef: size of restored structure=%u\n", Definition.curr_struct->sym.total_size));
                    }
                }
            } else {
                DebugMsg(("StructDef(T_ENDS) error 1: i=%u, name=%s\n", i, name));
                AsmError( UNMATCHED_BLOCK_NESTING );
                return( ERROR );
            }
        } else {
            DebugMsg(("StructDef(T_ENDS) error 2, i=%u\n", i));
            AsmError( UNMATCHED_BLOCK_NESTING );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}

asm_sym * CreateTypeDef(char * name, int i)
{
    char        *token;
    struct asm_sym      *sym = NULL;
    struct asm_sym      *symtype;
    dir_node            *dir;
    int                 type;
    int                 size;
    int                 oldsize;
    int                 indirection = 0;

    if (*name) {
        sym = SymSearch( name );
    }
    if( sym == NULL ) {
        dir = dir_insert( name, TAB_STRUCT );
        sym = &dir->sym;
    } else {
        dir = (dir_node *)sym;
        if (sym->state == SYM_UNDEFINED) {
            dir_change( dir, TAB_STRUCT );
        } else {
            if ((sym->state != SYM_TYPE) || (dir->e.structinfo->isTypedef == FALSE)) {
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
    dir->e.structinfo->isTypedef = TRUE;
    dir->sym.defined = TRUE;

    token = AsmBuffer[i]->string_ptr;
    type = ERROR;
    if (AsmBuffer[i]->token == T_FINAL)
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
            return( sym );
        }
        /* besides PROTO typedef, an arbitrary type is ok */
        if( !(symtype = IsLabelStruct( token ) ) ) {
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

        /* accect pointer qualifiers "near", "far", ... */
        /* todo: store the pointer qualifier in the dir_node */
        if ((AsmBuffer[i]->value == T_NEAR) ||
            (AsmBuffer[i]->value == T_NEAR16) ||
            (AsmBuffer[i]->value == T_NEAR32) ||
            (AsmBuffer[i]->value == T_FAR) ||
            (AsmBuffer[i]->value == T_FAR16) ||
            (AsmBuffer[i]->value == T_FAR32)) {
            if ((AsmBuffer[i+1]->token != T_RES_ID) ||
                (AsmBuffer[i+1]->value != T_PTR)) {
                AsmError( INVALID_QUALIFIED_TYPE );
                return( NULL );
            }
            i++;
        }
        /* if it's a pointer, don't care about level of indirection */
        /* or to what type if finally points */
        while ((i < Token_Count) && (AsmBuffer[i]->value == T_PTR) ) {
            indirection++;
            i++;
            DebugMsg(("TypeDef: pointer type\n"));
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
    if (AsmBuffer[i]->token != T_FINAL) {
        DebugMsg(("TypeDef: unexpected token %u, idx=%u\n", AsmBuffer[i]->token, i));
        AsmError( SYNTAX_ERROR );
        return( NULL );
    }
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

    DebugMsg(("TypeDef enter, i=%d\n", i));
    if( i < 0 ) {
        AsmError( TYPE_MUST_HAVE_A_NAME );
        return( ERROR );
    }

    if (CreateTypeDef(AsmBuffer[i]->string_ptr, i+2) == NULL)
        return(ERROR);

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
    if (Definition.struct_depth > 0) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
#endif
    name = AsmBuffer[i-1]->string_ptr;
    if (SymSearch( name )) {
        AsmErr( SYMBOL_ALREADY_DEFINED, name );
        return( ERROR );
    }
    dir = dir_insert( name, TAB_STRUCT );
    dir->e.structinfo->isRecord = TRUE;
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
        /* record field names are global! (design flaw) */
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
        f->fsym = sym;
        f->value = NULL;
        f->initializer = NULL;
        if( dir->e.structinfo->head == NULL ) {
            dir->e.structinfo->head = dir->e.structinfo->tail = f;
        } else {
            dir->e.structinfo->tail->next = f;
            dir->e.structinfo->tail = f;
        }
        if (*AsmBuffer[i]->string_ptr == '=') {
            i++;
            if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
                return( ERROR );
            if (opndx.type != EXPR_CONST || opndx.string != NULL) {
                AsmError( CONSTANT_EXPECTED );
                return( ERROR );
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
    num = dir->sym.total_size * 8;
    /* set the bit position */
    for (f=dir->e.structinfo->head;f;f = f->next) {
        num = num - f->fsym->total_size;
        f->fsym->offset = num;
    }
    DebugMsg(("RecordDef exit, no error\n"));
    return( NOT_ERROR );
}

// initialize an array inside a structure
// if there are no brackets, the next comma, '>' or '}' will terminate

static char * InitializeArray(field_list *f, char * ptr, char * buffer)
{
    char delim;
    char *ptr2;
    int  count;

    while (isspace(*ptr)) ptr++;
    delim = *ptr;
    DebugMsg(("InitializeArray enter: init=%s, items=%u, initializer=%s\n", ptr, f->fsym->total_length, f->initializer));
    if (delim == '<') {
        delim = '>';
        ptr++;
    } else if (delim == '{') {
        delim = '}';
        ptr++;
    } else
        delim = ',';
#if 0
        if (*ptr != 0) {
            AsmError(INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM);
            return (ptr);
        }
#endif
    strcpy( buffer, f->initializer );
    strcat( buffer, " ");
    ptr2 = buffer + strlen(buffer);
    for (count = f->fsym->total_length; count; count--) {
        char *ptr3 = ptr2;
        while (isspace(*ptr)) ptr++;
        if ((*ptr == '\0') || (*ptr == delim))
            break;
        while ((*ptr != ',') && (*ptr != '\0') && (*ptr != delim)) {
            if (*ptr == '\'' || *ptr == '"') {
                char c = *ptr;
                *ptr2++ = *ptr++;
                while (*ptr != c && *ptr != '0')
                    *ptr2++ = *ptr++;
                if (*ptr == 0) {
                    AsmError(MISSING_QUOTE_IN_STRING);
                    return(ptr);
                }
            }
            *ptr2++ = *ptr++;
        }
        if (ptr2 == ptr3) {
            strcpy( ptr2, f->value );
            ptr2 = ptr2+strlen(ptr2);
        }
        if (delim != ',' && *ptr == ',')
            *ptr2++ = *ptr++;
        *ptr2 = '\0';
    }
    if ((*ptr != '\0') && (*ptr != delim)) {
        AsmError(TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE);
        *buffer = 0;
        return(ptr);
    }
    if (count != f->fsym->total_length) {
        InputQueueLine( buffer );
    }
    if (count) {
        if (count == f->fsym->total_length)
            strcat(buffer, f->value);
        else {
            asm_sym * sym = f->fsym;
            while (sym->mem_type == MT_TYPE)
                sym = sym->type;
            if (sym->state == SYM_TYPE && ((dir_node *)sym)->e.structinfo->isTypedef == FALSE)
                sprintf(buffer, "%s %u dup (<>)", f->initializer, count);
            else
                sprintf(buffer, "%s %u dup (?)", f->initializer, count);
        }
        InputQueueLine( buffer );
    }

    if (delim && (*ptr == delim))
        ptr++;

    return(ptr);

}

// sym = symbol of variable to initialize
// struct_symbol = structure to initialize
// init_string = init string
// first = first call (might be called recursively!)

// currently this proc emits ASM lines with simple types
// to actually "fill" the structure.

char * InitializeStructure( asm_sym *sym, asm_sym *struct_symbol, char * init_string, bool first )
/********************************************************************/
{
    /* input: a line that looks like : sym_name struct_name { init. values }
     * where i marks the struct_name
     */

    char            buffer[MAX_LINE_LEN];
    char            *ptr;
    dir_node        *dir;
    field_list      *f;
    int             tmpstate;
    char            start_delim;
    char            end_delim;
    int             delim_lvl;
    int             bracket_lvl;
    int             nextofs;
    field_list      fl;

    /* skip TYPEDEF aliases */
    while (struct_symbol->mem_type == MT_TYPE) {
        struct_symbol = struct_symbol->type;
    }

    dir = (dir_node *)struct_symbol;
    DebugMsg(("InitializeStructure enter, sym=%s, total=%u/%u, init_string=>%s<\n", struct_symbol->name, struct_symbol->total_size, struct_symbol->total_length, init_string ));

#if 0
    /* the TYPE item is NOT to be used to init
     these values. This is done in array_elem()!
     */
    if( sym != NULL ) {
        sym->total_size   = struct_symbol->total_size;
        sym->total_length = struct_symbol->total_length;
        sym->first_size   = struct_symbol->first_size;
        sym->first_length = struct_symbol->first_length;
    }
#endif

    ptr = init_string;
    bracket_lvl = 0;

    start_delim = 0;
    end_delim = 0;
    delim_lvl = 0;

    if (*ptr == '{') {
        ptr++;
        end_delim = '}';
        start_delim = '{';
//        delim_lvl++;
    } else if (*ptr == '<') {
        if (first) {
            /* check if a matching '>' exists */
            /* if no, don't regard the '<' as a delim */
#if 0
            int i = strlen(ptr);
            if (*(ptr+i-1) != '>')
#endif
                goto nodelim;
        }
        ptr++;
        end_delim = '>';
        start_delim = '<';
//        delim_lvl++;
    }
nodelim:

#if 0
    /* that's too late, must be done in data_init() */
    if (first) {
        PushLineQueue();
    }
#endif

    /* for RECORDS, just reserve space */
    if (dir->e.structinfo->isRecord == TRUE) {
        sprintf(buffer, "db %u dup (?)", dir->sym.total_size);
        InputQueueLine( buffer );
        return( ptr );
    } else if (dir->e.structinfo->isTypedef == TRUE) {
        /* there's no field list for typedefs */
        /* create a temporary one for init */
        fl.next = NULL;
        fl.fsym = struct_symbol;
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
        fl.fsym->state = SYM_INTERNAL;
    }

    for( f = dir->e.structinfo->head; f != NULL; f = f->next ) {
        /* put the lines to define the fields of the structure in,
         * using the values specified ( if any ) or the default ones otherwise
         */
        DebugMsg(("InitializeStructure: init %s, default=>%s<\n", f->fsym->name, f->value));

        if (f->fsym->state == SYM_TYPE)  { /* embedded struct? */
            if (*ptr == end_delim)
                InitializeStructure(sym, f->fsym, "", FALSE);
            else
                ptr = InitializeStructure(sym, f->fsym, ptr, FALSE);
#if 0
        } else if (f->fsym->mem_type == MT_TYPE) {
            /* a structured field? no need for a recursive call! */
            if (*ptr == end_delim)
                InitializeStructure(sym, f->fsym->type, "", FALSE);
            else
                ptr = InitializeStructure(sym, f->fsym->type, ptr, FALSE);
#endif
        } else if (f->fsym->mem_type == MT_BITS) {
            /* todo: how to init bit fields? */
            AsmError( SYNTAX_ERROR );
        } else if (f->fsym->total_length > 1) {
            if (*ptr == end_delim) {
                InitializeArray(f, "", buffer);
            } else
                ptr = InitializeArray(f, ptr, buffer);
        } else {
            while (isspace(*ptr)) ptr++;
            strcpy( buffer, f->initializer );
            strcat( buffer, " " );
            if((*ptr == '\0') || (*ptr == end_delim) || (*ptr == ',') ) {
                strcat( buffer, f->value );
            } else {
                char *ptr1;
                ptr1 = buffer + strlen(buffer);
                while (*ptr != '\0') {
                    if (*ptr == start_delim)
                        delim_lvl++;
                    else if (*ptr == end_delim) {
                        delim_lvl--;
                        if (delim_lvl < 0)
                            break;
                    } else if (*ptr == '<') {
                        bracket_lvl++;
                    } else if (*ptr == '>') {
                        bracket_lvl--;
                    } else if (*ptr == '"') {
                        char startc = *ptr;
                        *ptr1++ = *ptr++;
                        while (*ptr) {
                            *ptr1++ = *ptr++;
                            if (*(ptr-1) == startc)
                            if (*ptr == startc)
                                *ptr1++ = *ptr++;
                            else
                                break;
                        }
                        continue;
                    } else if (bracket_lvl == 0 && delim_lvl == 0 && (*ptr == ',' || *ptr == '}'))
                        break;

                    *ptr1++ = *ptr++;
                }
                *ptr1 = '\0';
            }
            InputQueueLine( buffer );

            /* add padding bytes */

            if (f->next == NULL || dir->e.structinfo->isUnion)
                nextofs = struct_symbol->total_size;
            else
                nextofs = f->next->fsym->offset;

            if (f->fsym->offset + f->fsym->total_size < nextofs) {
                sprintf(buffer,"db %u dup (?) ;padding",
                        nextofs - (f->fsym->offset + f->fsym->total_size));
                InputQueueLine( buffer );
            }

        }
        /* for a union, just the first field is initialized */
        if (dir->e.structinfo->isUnion)
            break;

        if (f->next != NULL) {
            if (*ptr == ',')
                ptr++;
        }
    }  /* end for */
    if ((end_delim) && (*ptr == end_delim))
        ptr++;
    else if (*ptr != '\0') {
        AsmError(TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE);
    }

    /* restore the temporarily modified TYPEDEF */
    if (dir->e.structinfo->head == &fl) {
        dir->e.structinfo->head = NULL;
        struct_symbol->state = tmpstate;
    }

    DebugMsg(("InitializeStructure exit, ptr=>%s<\n", ptr ));

    return( ptr );
}

/* this function is called only during pass 1 */
/* sym_loc: index in AsmBuffer of field/struct name (optional!) */
/* loc: */
/* vartype: type of item if memtype is MT_TYPE */
/* size: size of type */

struct asm_sym * AddFieldToStruct( int sym_loc, int loc, memtype mem_type, struct asm_sym * vartype, int size)
/*****************************/
{
    int offset;
    int count;
    int i;
    char * name;
    dir_node    *dir;
    struct_info *si;
    field_list  *f;
    asm_sym     *sym;

    si = Definition.curr_struct->e.structinfo;

    if (si->isUnion)
        offset = 0;
    else
        offset = Definition.curr_struct->sym.total_size;

    DebugMsg(("AddFieldToStruct: %s, ofs=%u\n", Definition.curr_struct->sym.name, offset));

    if( loc == -1 ) {
        /* this is a label declaration inside a structure */
        for( loc = 0; AsmBuffer[loc]->token != T_FINAL; ++loc ) {
            /* nothing to do */
        }
    }
    if (sym_loc >= 0) {
        /* the field has a name */
        name = AsmBuffer[sym_loc]->string_ptr;
        /* check if field name is already used */
        /* RECORD fields names, which are global, aren't handled here */
        sym = SearchNameInStruct((asm_sym *)Definition.curr_struct, name, (unsigned int *)&i);
        if (sym) {
            AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
            name = "";
        }
    } else
        name = "";

    /* alloc the items needed */

    sym = SymCreate(name, FALSE);
    f = AsmAlloc( sizeof( field_list ) );
    f->initializer = AsmAlloc( strlen( AsmBuffer[loc]->string_ptr ) + 1 );

    sym->state = SYM_STRUCT_FIELD;
    sym->defined = TRUE;
    sym->type = vartype;
//    ok to do?
//    sym->total_size = SizeFromMemtype(mem_type, Use32);
    f->fsym = sym;
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

    f->next = NULL;

    if( si->head == NULL ) {
        si->head = si->tail = f;
    } else {
        si->tail->next = f;
        si->tail = f;
    }
    sym->mem_type = mem_type;

    /* align the field if an alignment argument was given */
    if (si->alignment) {
        DebugMsg(("AddFieldToStruct: align=%u, size=%u, ofs=%u\n",
                  si->alignment, size, offset));
        if (si->alignment < size)
            offset = (offset + (si->alignment - 1)) & ( - si->alignment);
        else if (size)
            offset = (offset + (size - 1)) & (-size);
    }

    sym->offset = offset;

    return( sym );
}

// called by data_init() when a structure field has been created
// now called on pass 1 only.

void UpdateStructSize(int no_of_bytes)
{
    if( Parse_Pass == PASS_1 ) {
        if (Definition.curr_struct->e.structinfo->isUnion) {
            if (no_of_bytes > Definition.curr_struct->sym.total_size)
                Definition.curr_struct->sym.total_size = no_of_bytes;
        } else
            Definition.curr_struct->sym.total_size += no_of_bytes;
    }
    return;
}
