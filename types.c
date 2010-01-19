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
* Description:  STRUCT, UNION, RECORD and TYPEDEF directives,
*               virtually rewritten for JWasm.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
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
#include "myassert.h"

dir_node *CurrStruct;
static dir_node *redef_struct;

void TypesInit()
/**************/
{
    CurrStruct   = NULL;
    redef_struct = NULL;
}

// search a name in a struct's fieldlist

struct asm_sym *SearchNameInStruct( asm_sym *tstruct, const char *name, unsigned int * poffset, int level )
/*********************************************************************************************************/
{
    int len = strlen( name );
    field_list * fl = ((dir_node *)tstruct)->e.structinfo->head;
    asm_sym *sym = NULL;

    //if (ModuleInfo.oldstructs == TRUE) {
    //    return( SymSearch( name ) );
    //}
    if ( level >= MAX_STRUCT_NESTING ) {
        AsmError( NESTING_LEVEL_TOO_DEEP );
        return( NULL );
    }
    level++;
    for ( ; fl; fl = fl->next ) {
        /* recursion: if member has no name, check if it is a structure
         and scan this structure's fieldlist then */
        if ( *( fl->sym->name ) == 0 ) {
            /* there are 2 cases: an anonymous inline struct ... */
            if (fl->sym->state == SYM_TYPE) {
                if ( sym = SearchNameInStruct(fl->sym, name, poffset, level ) ) {
                    *poffset += fl->sym->offset;
                    break;
                }
            /* or an anonymous structured field */
            } else if ( fl->sym->mem_type == MT_TYPE ) {
                if (sym = SearchNameInStruct( fl->sym->type, name, poffset, level ) ) {
                    *poffset += fl->sym->offset;
                    break;
                }
            }
        } else if ( len == fl->sym->name_size && SymCmpFunc( name, fl->sym->name ) == 0 ) {
            DebugMsg(("SearchNameInStruct: '%s' found in struct %s\n", name, tstruct->name ));
            sym = fl->sym;
            break;
        }
    }
    return( sym );
}

// check if a struct has changed

static bool AreStructsEqual(dir_node *newstr, dir_node *oldstr)
/*************************************************************/
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

// handle STRUCT, STRUC, UNION directives
// i = index of directive token

ret_code StructDirective( int i )
/*******************************/
{
    char *name;
    unsigned alignment;
    unsigned int offset;
    int name_loc;
    int cmd = AsmBuffer[i]->value;
    //unsigned int size;
    asm_sym *sym;
    dir_node *dir;

    DebugMsg(("StructDirective(%s) enter, i=%u, CurrStruct=%s\n", AsmBuffer[i]->string_ptr, i, CurrStruct ? CurrStruct->sym.name : "NULL" ));

    /* top level structs/unions must have a name at pos 0 */
    if (( i == 1 && CurrStruct == NULL ) ||
        ( i == 0 && CurrStruct != NULL )) {
        ;
    } else {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    alignment = ( 1 << ModuleInfo.fieldalign );

    i++; /* go past STRUCT/UNION */

    if ( i == 1 ) {
        /* scan for optional name of embedded struct */
        if (AsmBuffer[i]->token != T_FINAL &&
            is_valid_id_first_char(*(AsmBuffer[i]->string_ptr))) {
            /* the name might be a reserved word! */
            name_loc = i;
            name = AsmBuffer[i]->string_ptr;
            i++;
        } else {
            name_loc = -1;
            name = "";
        }
    } else {
        name_loc = 0;
        name = AsmBuffer[0]->string_ptr;
    }

    /* get an optional alignment argument: 1,2,4,8,16 or 32 */
    if ( CurrStruct == NULL && AsmBuffer[i]->token != T_FINAL ) {
        unsigned int power;
        expr_list opndx;
        /* get the optional alignment parameter */
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) != ERROR ) {
            if (opndx.kind == EXPR_EMPTY ) {
                ;
            } else if (opndx.kind != EXPR_CONST ) {
                AsmError( CONSTANT_EXPECTED );
            } else if( opndx.value > MAX_STRUCT_ALIGN ) {
                AsmError( STRUCT_ALIGN_TOO_HIGH );
            } else {
                for( power = 1; power < opndx.value; power <<= 1 );
                if( power != opndx.value ) {
                    AsmError( POWER_OF_2 );
                } else
                    alignment = opndx.value;
            }
            DebugMsg(("StructDirective(%s) alignment=%u\n", name, alignment));
        }
        /* there might also be the NONUNIQUE parameter */
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( AsmBuffer[i]->token == T_ID &&
                (_stricmp(AsmBuffer[i]->string_ptr, "NONUNIQUE") == 0)) {
                /* currently NONUNIQUE is ingored */
                _strupr( AsmBuffer[i]->string_ptr );
                AsmWarn( 2, TOKEN_IGNORED, AsmBuffer[i]->string_ptr );
                i++;
            }
        }
    }
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    /* does struct have a name? */
    if ( *name ) {
        if ( CurrStruct == NULL ) {
            /* the "top-level" struct is part of the global namespace */
            sym = SymSearch( name );
            DebugMsg(("StructDirective: SymSearch (%s)=%X (curr struct=%X)\n", name, sym, CurrStruct ));
        } else {
            sym = SearchNameInStruct( (asm_sym *)CurrStruct, name, &offset, 0 );
            DebugMsg(("StructDirective(%s): SearchNameInStruc()=%X\n", name, sym));
            if ( Parse_Pass > PASS_1 && sym && sym->mem_type == MT_TYPE )
                sym = sym->type;
        }
    } else {
        sym = NULL;   /* anonymous struct */
        /* if pass is > 1, the embedded struct must be found in the current
         * struct. This isn't implemented fool-proved yet!!!
         */
        if ( Parse_Pass > PASS_1 ) {
            field_list *f;
            for ( f = CurrStruct->e.structinfo->head; f; f = f->next ) {
                if ( f->sym->mem_type == MT_TYPE && f->sym->offset >= CurrStruct->sym.offset ) {
                    sym = f->sym->type;
                    break;
                }
            }
#ifdef DEBUG_OUT
            DebugMsg(("StructDirective: %s type found at CurrStruct->sym.offset=%X\n", sym ? sym->name : "No", CurrStruct->sym.offset ));
#endif
        }
    }

    /* if pass is > 1, update struct stack + CurrStruct.offset and exit */
    if ( Parse_Pass > PASS_1 ) {
        /**/myassert( sym != NULL );
        sym->offset = 0;
        ((dir_node *)sym)->next = CurrStruct;
        CurrStruct = (dir_node *)sym;
        return( NOT_ERROR );
    }

    if( sym == NULL ) {
        /* is it a global STRUCT? */
        if ( CurrStruct == NULL )
            dir = dir_insert( name, SYM_TYPE );
        else {
            /* a nested structure is split in an anonymous STRUCT type
             * and a struct field with/without name
             */
            //field_list *f;
            dir = dir_insert_ex( name, SYM_TYPE );
            /* v2: don't call AddFieldToStruct() here. First the
             * structure must be read in ( because of alignment issues
             */
            // sym = AddFieldToStruct( name_loc, -1, MT_TYPE, dir, 0 );

            alignment = CurrStruct->e.structinfo->alignment;
        }
    } else {
        /* the symbol exists already */
        dir = (dir_node *)sym;
        if( sym->state == SYM_UNDEFINED ) {
            dir_settype( dir, SYM_TYPE );
        } else if( sym->state == SYM_TYPE && CurrStruct == NULL ) {
            /* was symbol forward referenced? */
            if ( dir->e.structinfo->typekind != TYPE_NONE ) {
                /* structure redefinition! */
                redef_struct = dir;
                dir = dir_insert_ex( name, SYM_TYPE );
            }
        } else {
            AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
            return( ERROR );
        }
    }

    dir->e.structinfo->alignment = alignment;
    dir->sym.offset = 0;
    dir->e.structinfo->isOpen = TRUE;
    if ( cmd == T_UNION )
        dir->e.structinfo->typekind = TYPE_UNION;
    else
        dir->e.structinfo->typekind = TYPE_STRUCT;

    if ( ModuleInfo.list ) {
        if ( CurrStruct )
            LstWrite( LSTTYPE_STRUCT, CurrStruct->sym.total_size, NULL );
        else
            LstWrite( LSTTYPE_STRUCT, 0, NULL );
    }

    if ( CurrStruct )
        dir->e.structinfo->isInline = TRUE;

    dir->next = CurrStruct;
    CurrStruct = dir;

#if 0 //def DEBUG_OUT
    {
        dir_node *struc;
        for ( struc = CurrStruct; struc; struc = struc->next ) {
            DebugMsg(("StructDirective stack: %X, name=>%s<\n", struc, struc->sym.name ));
        }
    }
#endif

    return( NOT_ERROR );
}

// handle ENDS directive if CurrStruct != NULL

ret_code EndstructDirective( int i )
/**********************************/
{
    //char *name;
    //unsigned int offset;
    unsigned int size;
    //struct asm_sym *sym;
    //memtype mem_type;
    dir_node *dir;

    DebugMsg(("EndstructDirective enter, i=%u\n", i));

    /* if pass is > 1 just do minimal work */
    if ( Parse_Pass > PASS_1 ) {
        CurrStruct->sym.offset = 0;
        size = CurrStruct->sym.total_size;
        CurrStruct = CurrStruct->next;
        if ( CurrStruct )
            UpdateStructSize( size );
        return( NOT_ERROR );
    }

    dir = CurrStruct;

    DebugMsg(("EndstructDirective(%s), ofs=%u, struct size=%u, max_mbr=%u, alignment=%u\n",
              dir->sym.name,
              dir->sym.offset,
              dir->sym.total_size,
              dir->sym.max_mbr_size,
              dir->e.structinfo->alignment));

    if ( ( i == 1 && dir->next == NULL ) ||
        ( i == 0 && dir->next != NULL ) ) {
        ;
    } else {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if ( i == 1 ) { /* an global struct ends with <name ENDS>  */
        if ( SymCmpFunc( AsmBuffer[0]->string_ptr, dir->sym.name ) != 0 ) {
            /* names don't match */
            DebugMsg(("EndstructDirective: names don't match, i=%u, name=%s - %s\n", i, AsmBuffer[0]->string_ptr, dir->sym.name));
            AsmErr( UNMATCHED_BLOCK_NESTING, AsmBuffer[0]->string_ptr );
            return( ERROR );
        }
    }

    i++; /* go past ENDS */

    /* Pad bytes at the end of the structure. */
#if 1
    /* v2.02: this is to be done in any case, whether -Zg is set or not */
    //if ( dir->e.structinfo->alignment > 1 && Options.masm_compat_gencode == FALSE ) {
    if ( dir->e.structinfo->alignment > 1 ) {
        size = dir->sym.max_mbr_size;
        if ( size == 0 )
            size++;
        if ( size > dir->e.structinfo->alignment )
            size = dir->e.structinfo->alignment;
        dir->sym.total_size = (dir->sym.total_size + size - 1) & (-size);
        DebugMsg(("EndstructDirective:, struct size after final alignment=%u\n", dir->sym.total_size));
    }
#endif
    dir->e.structinfo->isOpen = FALSE;
    dir->sym.defined = TRUE;

    /* if there's a negative offset, size will be wrong! */
    size = dir->sym.total_size;

    /* reset offset, it's just used during the definition */
    dir->sym.offset = 0;

    CurrStruct = dir->next;
    /* v2.0: add the embedded struct AFTER it has been parsed! */
    if ( i == 1 ) {
        asm_sym *sym;
        sym = AddFieldToStruct( -1, -1, MT_TYPE, dir, dir->sym.total_size );
        /* the member name was stored in the type name */
        sym->name = dir->sym.name;
        sym->name_size = strlen( dir->sym.name );
        sym->total_size = dir->sym.total_size;
        dir->sym.name = ""; /* the type becomes anonymous */
        dir->sym.name_size = 0;
    }

    if ( FileInfo.file[LST] ) {
        LstWrite( LSTTYPE_STRUCT, size, dir->sym.name );
    }
#if 1
    /* to allow direct structure access */
    switch ( dir->sym.total_size ) {
    case 1:  dir->sym.mem_type = MT_BYTE;   break;
    case 2:  dir->sym.mem_type = MT_WORD;   break;
    case 4:  dir->sym.mem_type = MT_DWORD;  break;
    case 6:  dir->sym.mem_type = MT_FWORD;  break;
    case 8:  dir->sym.mem_type = MT_QWORD;  break;
    default:
        /* set something which cannot be accessed by a reg */
        /* there might exist a better solution, once the mess
         in the parser has been removed */
        // dir->sym.mem_type = MT_OWORD;
        dir->sym.mem_type = MT_EMPTY;
    }
#endif
    /* reset redefine */
    if ( CurrStruct == NULL ) {
        if ( redef_struct ) {
            if ( AreStructsEqual( dir, redef_struct) == FALSE ) {
                AsmError( NON_BENIGN_STRUCT_REDEFINITION );
            }
            DebugMsg(("EndstructDirective: delete the redefinition of %s\n", dir->sym.name ));
            dir_free( dir, FALSE );
            redef_struct = NULL;
        }
    } else {

        if ( dir->sym.max_mbr_size > CurrStruct->sym.max_mbr_size )
            CurrStruct->sym.max_mbr_size = dir->sym.max_mbr_size;

        UpdateStructSize( size );
        DebugMsg(("EndstructDirective: new size of restored structure=%u\n", CurrStruct->sym.total_size));
    }
    //dir->sym.max_mbr_size = 0;
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

/* this function is called in pass 1 only */
/* name_loc: index of field name or -1  */
/* loc: initializer location, may be -1 */
/* mem_type: mem_type of item */
/* vartype: arbitrary type of item if memtype is MT_TYPE */
/* size: size of type - used for alignment only */

struct asm_sym * AddFieldToStruct( int name_loc, int loc, memtype mem_type, dir_node * vartype, int size )
/********************************************************************************************************/
{
    int offset;
    //int count;
    int i;
    char * name;
    char * init;
    struct_info *si;
    field_list  *f;
    asm_sym     *sym;
    asm_sym     *gsym;

    si = CurrStruct->e.structinfo;
    offset = CurrStruct->sym.offset;

#ifdef DEBUG_OUT
    if (name_loc < 0)
        DebugMsg(("AddFieldToStruct(%s): anonymous, curr ofs=%u, vartype=%s, size=%u\n", CurrStruct->sym.name, offset, vartype ? vartype->sym.name : "NULL", size ));
    else
        DebugMsg(("AddFieldToStruct(%s): name=%s, curr ofs=%u, vartype=%s, size=%u\n", CurrStruct->sym.name, AsmBuffer[name_loc]->string_ptr, offset, vartype ? vartype->sym.name : "NULL", size ));
#endif

    if (name_loc >= 0) {
        /* the field has a name */
        name = AsmBuffer[name_loc]->string_ptr;
        /* check if field name is already used */
        /* RECORD fields names, which are global, aren't handled here */
        sym = SearchNameInStruct((asm_sym *)CurrStruct, name, (unsigned int *)&i, 0 );
        if (sym) {
            //if ( ModuleInfo.oldstructs &&
            //     sym->state == SYM_STRUCT_FIELD &&
            //     redef_struct != NULL ) {
            //} else
                AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
            name = "";
        }
    } else
        name = "";

    /* alloc the items needed */

    sym = SymCreate( name, FALSE );
    sym->state = SYM_STRUCT_FIELD;
    sym->defined = TRUE;
    sym->mem_type = mem_type;
    sym->type = &vartype->sym;
    // ok to do?
    // sym->total_size = SizeFromMemtype( mem_type, ModuleInfo.Ofssize );

    f = AsmAlloc( sizeof( field_list ) );

    f->next = NULL;
    f->sym = sym;

    if ( loc != -1 ) {

        i = strlen( AsmBuffer[loc]->string_ptr );
        DebugMsg(("AddFieldToStruct(%s): type=>%s<\n", CurrStruct->sym.name, AsmBuffer[loc]->string_ptr ));
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
        DebugMsg(("AddFieldToStruct(%s): initializer=>%s<\n", CurrStruct->sym.name, f->value ));

    } else {
        DebugMsg(("AddFieldToStruct(%s): no initializer<\n", CurrStruct->sym.name ));
        f->initializer = NULL;
        f->value = NULL;
    }

    if( si->head == NULL ) {
        si->head = si->tail = f;
    } else {
        si->tail->next = f;
        si->tail = f;
    }

#if 1
    /* v2.0: for STRUCTs, don't use the struct's size for alignment calculations,
     * instead use the size of the "max" member!
     */
    if ( mem_type == MT_TYPE &&
        ( vartype->e.structinfo->typekind == TYPE_STRUCT ||
         vartype->e.structinfo->typekind == TYPE_UNION ) ) {
        size = vartype->sym.max_mbr_size;
    }
#endif
    /* align the field if an alignment argument was given */
    if ( si->alignment > 1 ) {
        //memtype mt;
        //dir_node *tdir;
        DebugMsg(("AddFieldToStruct(%s): align=%u, size=%u, ofs=%u\n", CurrStruct->sym.name, si->alignment, size, offset ));
        /* if it's the first field to add, use offset of the parent's current field */
#if 0
        /* v2: removed. An embedded struct is now added AFTER it has
         * been parsed. */
        if ( offset == 0 && CurrStruct->next ) {
            dir_node *parent = CurrStruct->next;
            if ( si->alignment < size )
                parent->e.structinfo->tail->sym->offset =
                    (parent->e.structinfo->tail->sym->offset + (si->alignment - 1)) & ( - si->alignment);
            else if ( size )
                parent->e.structinfo->tail->sym->offset =
                    (parent->e.structinfo->tail->sym->offset + (size - 1)) & (-size);
        } else
#endif
        {
            if ( si->alignment < size )
                offset = (offset + (si->alignment - 1)) & ( - si->alignment);
            else if ( size )
                offset = (offset + (size - 1)) & (-size);
        }
        /* adjust the struct's current offset + size.
         The field's size is added in  UpdateStructSize()
         */
        if ( CurrStruct->e.structinfo->typekind != TYPE_UNION ) {
            CurrStruct->sym.offset = offset;
            if ( offset > CurrStruct->sym.total_size )
                CurrStruct->sym.total_size = offset;
        }
    }
    /* v2.0: for padding, save the max member size */
    if ( size > CurrStruct->sym.max_mbr_size ) {
        DebugMsg(("AddFieldToStruct(%s): max_mbr_size set to %u\n", CurrStruct->sym.name, size ));
        CurrStruct->sym.max_mbr_size = size;
    }
    sym->offset = offset;

    /* if -Zm is on, create a global symbol */
    if ( ModuleInfo.oldstructs == TRUE && *name != NULLC ) {
        DebugMsg(("AddFieldToStruct(%s): Masm51 compat on, lookup %s in global symbol table\n", CurrStruct->sym.name, name ));
        gsym  = SymLookup( name );
        if ( gsym ) {
            if ( gsym->state == SYM_UNDEFINED )
                gsym->state = SYM_STRUCT_FIELD;
            if ( gsym->state == SYM_STRUCT_FIELD ) {
                dir_node *dir;
                gsym->mem_type = mem_type;
                gsym->type = &vartype->sym;
                gsym->offset = offset; /* added v2.0 */
                /* v2.01: must be the full offset.
                 * (there's still a problem if alignment is > 1!)
                 */
                for ( dir = CurrStruct->next; dir; dir = dir->next )
                    gsym->offset += dir->sym.offset;
                gsym->defined = TRUE;
            }
        }
    }

    return( sym );
}

// called by AlignDirective() if ALIGN/EVEN has been found inside
// a struct. It's already verified that <value> is a power of 2.

ret_code AlignInStruct( int value )
/*********************************/
{
    if ( CurrStruct->e.structinfo->typekind != TYPE_UNION ) {
        int offset;
        offset = CurrStruct->sym.offset;
        offset = (offset + (value - 1)) & (-value);
        CurrStruct->sym.offset = offset;
        if (offset > CurrStruct->sym.total_size)
            CurrStruct->sym.total_size = offset;
    }
    return( NOT_ERROR );
}

// called by data_init() when a structure field has been created
// now called on pass 1 only.

void UpdateStructSize(int no_of_bytes)
/************************************/
{
    if ( CurrStruct->e.structinfo->typekind == TYPE_UNION ) {
        if ( no_of_bytes > CurrStruct->sym.total_size )
            CurrStruct->sym.total_size = no_of_bytes;
    } else {
        CurrStruct->sym.offset += no_of_bytes;
        if ( CurrStruct->sym.offset > CurrStruct->sym.total_size )
            CurrStruct->sym.total_size = CurrStruct->sym.offset;
    }
    return;
}

/* called if ORG occurs inside STRUCT/UNION definition */

ret_code SetStructCurrentOffset(int offset)
/*****************************************/
{
    if ( CurrStruct->e.structinfo->typekind == TYPE_UNION ) {
        AsmError( ORG_NOT_ALLOWED_IN_UNIONS );
        return( ERROR );
    }
    CurrStruct->sym.offset = offset;
    /* if an ORG is inside the struct, it cannot be instanced anymore */
    CurrStruct->e.structinfo->OrgInside = TRUE;
    if ( offset > CurrStruct->sym.total_size )
        CurrStruct->sym.total_size = offset;

    return( NOT_ERROR );
}

// TYPEDEF worker

asm_sym *CreateTypeDef(char * name, int * pi)
/*******************************************/
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
        sym->Ofssize = ModuleInfo.Ofssize;
    } else {
        dir = (dir_node *)sym;
        if (sym->state == SYM_UNDEFINED) {
            dir_settype( dir, SYM_TYPE );
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
        type = FindStdType( AsmBuffer[i]->value );

    if( type == -1 ) {
        DebugMsg(("CreateTypeDef(%s): arbitrary type\n", name ));
        /* it's not a simple type, check for PROTO qualifier */
        if (AsmBuffer[i]->value == T_PROTO) {
            dir_node * dir2;  /* create a PROTOtype item without name */
            dir2 = (dir_node *)CreateProc( NULL, "", FALSE );
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
        if( !(symtype = SymIsType( token ) ) ) {
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
            size = SizeFromMemtype( sym->mem_type, ModuleInfo.Ofssize );

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
        DebugMsg(("CreateTypeDef(%s) ok, size=%u, mem_type=MT_TYPE, type=%X, i=%u\n", sym->name, sym->total_size, sym->type, i ));
    } else {
        /* it's a simple or void type */
#if 0 /* v1.96: removed */
        if ( ( SimpleType[type].mem_type & MT_SPECIAL ) == 0 )
            size = (SimpleType[type].mem_type & MT_SIZE_MASK) + 1;
        else  /* if it's a more special type, get true size */
#endif
             if ( SimpleType[type].Ofssize == USE_EMPTY )
                 size = SizeFromMemtype( SimpleType[type].mem_type, ModuleInfo.Ofssize );
             else
                 size = SizeFromMemtype( SimpleType[type].mem_type, SimpleType[type].Ofssize );
        DebugMsg(("CreateTypeDef(%s): size %u\n", sym->name, size ));
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
                    type = FindStdType( ptrqual );
                    if ( SimpleType[type].Ofssize != USE_EMPTY )
                        size = SizeFromMemtype( SimpleType[type].mem_type, SimpleType[type].Ofssize );
                    else
                        size = SizeFromMemtype( SimpleType[type].mem_type, ModuleInfo.Ofssize );

                    if ( ptrqual == T_NEAR16 || ptrqual == T_FAR16 )
                        dir->sym.Ofssize = USE16;
                    else if ( ptrqual == T_NEAR32 || ptrqual == T_FAR32 )
                        dir->sym.Ofssize = USE32;

                    if ( ptrqual == T_FAR || ptrqual == T_FAR16 || ptrqual == T_FAR32 )
                        dir->sym.isfar = TRUE;
                    else
                        dir->sym.isfar = FALSE;
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

        DebugMsg(("CreateTypeDef(%s): memtype=%Xh, size=%u i=%u, token=%u\n", sym->name, sym->mem_type, size, i, AsmBuffer[i]->token));
        if ((AsmBuffer[i]->token == T_ID) || (AsmBuffer[i]->token == T_RES_ID)) {
            symtype = SymSearch( AsmBuffer[i]->string_ptr );
#ifdef DEBUGOUT
            if (symtype)
                DebugMsg(("CreateTypeDef: ptr dest=%X, name=%s\n", symtype, symtype->name));
            else
                DebugMsg(("CreateTypeDef: ptr dest=%X\n", symtype));
#endif
            if (symtype) {
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
            }
            i++;
        }
        sym->total_size = size;
        DebugMsg(("CreateTypeDef(%s) ok, size=%u, mem_type=%Xh, indirection=%u\n", sym->name, size, sym->mem_type, indirection ));
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

ret_code TypeDirective( int i )
/*****************************/
{
    char *name;

    DebugMsg(("TypeDef enter, i=%d\n", i));

    if( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    name = AsmBuffer[0]->string_ptr;

    i++; /* go past TYPEDEF */

    if ( CreateTypeDef( name, &i) == NULL )
        return( ERROR );
    if ( AsmBuffer[i]->token != T_FINAL ) {
        DebugMsg(("TypeDef: unexpected token %u, idx=%u\n", AsmBuffer[i]->token, i));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    return( NOT_ERROR );
}

// generate a RECORD. Called on pass 1 only

ret_code RecordDef( int i )
/*************************/
{
    char *name;
    struct asm_sym *sym;
    dir_node *dir;
    field_list  *f;
    int num;
    //int value;
    int name_loc;
    expr_list opndx;

    DebugMsg(("RecordDef enter, i=%u\n", i));
    if (i != 1) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
#if 0
    /* a record definition can be placed
     inside a STRUCT, but it will still be global */
    if (StructDef.struct_depth > 0) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
#endif

    name = AsmBuffer[0]->string_ptr;
    sym = SymSearch( name );
    if ( sym == NULL ) {
        dir = dir_insert( name, SYM_TYPE );
    } else {
        if ( sym->state != SYM_UNDEFINED ) {
            AsmErr( SYMBOL_ALREADY_DEFINED, name );
            return( ERROR );
        }
        dir = (dir_node *)sym;
        dir_settype( dir, SYM_TYPE );
    }
    dir->e.structinfo->typekind = TYPE_RECORD;
    dir->sym.defined = TRUE;

    i++; /* go past RECORD */

    num = 0; /* counter for total of bits in record */
    do {
        if ( AsmBuffer[i]->token != T_ID ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
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
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            AsmError( CONSTANT_EXPECTED );
            opndx.value = 1;
        }
        if (opndx.value + num > 32) {
            AsmError( TOO_MANY_BITS_IN_RECORD );
            return( ERROR );
        }
        /* record field names are global! (Masm design flaw) */
        sym = SymLookup( AsmBuffer[name_loc]->string_ptr );
        if ( !sym )
            return( ERROR);
        if ( sym->state != SYM_UNDEFINED ) {
            AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
            break;
        }
        sym->state = SYM_STRUCT_FIELD;
        sym->mem_type = MT_BITS;
        sym->total_size = opndx.value;
        num += opndx.value;
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
            f->value = AsmAlloc( count );
            f->value[0] = NULLC;
            for (; i < j; i++) {
                strcat(f->value, AsmBuffer[i]->string_ptr);
                if (i != j - 1)
                    strcat(f->value," ");
            }
        }

        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA ) {
                if ( (i + 1) < Token_Count )
                    i++;
            } else {
                AsmError( EXPECTING_COMMA );
                break;
            }

    } while ( i < Token_Count);

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
    for ( f = dir->e.structinfo->head; f; f = f->next ) {
        num = num - f->sym->total_size;
        f->sym->offset = num;
    }
    DebugMsg(("RecordDef exit, no error\n"));
    return( NOT_ERROR );
}

