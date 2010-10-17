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

/* v2.04: changed to 0 */
//#define ANYNAME 1 /* fixme: this probably should be changed to 0 */
#define ANYNAME 0

dir_node *CurrStruct;
static dir_node *redef_struct;
static const char szRecord[] = "RECORD";

void TypesInit()
/**************/
{
    CurrStruct   = NULL;
    redef_struct = NULL;
}

/* create a SYM_TYPE symbol.
 * <sym> must be NULL or of state SYM_UNDEFINED.
 * <name> and <global> are only used if <sym> is NULL.
 */

asm_sym *CreateTypeSymbol( asm_sym *sym, const char *name, bool global )
/***********************************************************************/
{
    struct_info *si;

    if ( sym )
        dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );
    else
        sym = SymCreate( name, global );

    if ( sym ) {
        sym->state = SYM_TYPE;
        ((dir_node *)sym)->e.structinfo = si = AsmAlloc( sizeof( struct_info ) );
        si->head = NULL;
        si->tail = NULL;
        si->typekind = TYPE_NONE;
        si->alignment = 0;
        si->flags = 0;
    }
    return( sym );
}

/* search a name in a struct's fieldlist */

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
            if ( fl->sym->state == SYM_TYPE ) {
                if ( sym = SearchNameInStruct( fl->sym, name, poffset, level ) ) {
                    *poffset += fl->sym->offset;
                    break;
                }
            /* or an anonymous structured field */
            } else if ( fl->sym->mem_type == MT_TYPE ) {
                if ( sym = SearchNameInStruct( fl->sym->type, name, poffset, level ) ) {
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

/* check if a struct has changed */

static bool AreStructsEqual(dir_node *newstr, dir_node *oldstr)
/*************************************************************/
{
    field_list  *fold = oldstr->e.structinfo->head;
    field_list  *fnew = newstr->e.structinfo->head;

    DebugMsg(("AreStructsEqual(%s) enter\n", oldstr->sym.name ));

    /* kind of structs must be identical */
    if ( oldstr->e.structinfo->typekind != newstr->e.structinfo->typekind )
        return( FALSE );

    for ( ; fold; fold = fold->next, fnew = fnew->next ) {
        if ( !fnew ) {
            DebugMsg(("AreStructsEqual: fields don't match\n"));
            return( FALSE );
        }
        /* for global member names, don't check the name if it's "" */
        if ( ModuleInfo.oldstructs && *fnew->sym->name == NULLC )
            ;
        else if ( 0 != strcmp( fold->sym->name, fnew->sym->name ) ) {
            DebugMsg(("AreStructsEqual: type name of field changed\n"));
            return( FALSE );
        }
        if ( fold->sym->offset != fnew->sym->offset ) {
            DebugMsg(("AreStructsEqual: offset of field %s changed: %u - %u\n", fold->sym->name, fold->sym->offset, fnew->sym->offset));
            return( FALSE );
        }
        if ( fold->sym->total_size != fnew->sym->total_size ) {
            DebugMsg(("AreStructsEqual: total_size of field changed\n"));
            return( FALSE );
        }
    }
    if ( fnew )
        return( FALSE );
    return( TRUE );
}

/* handle STRUCT, STRUC, UNION directives
 * i = index of directive token
 */
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

    DebugMsg1(("StructDirective(%s) enter, i=%u, CurrStruct=%s\n", AsmBuffer[i]->string_ptr, i, CurrStruct ? CurrStruct->sym.name : "NULL" ));

    /* top level structs/unions must have a name at pos 0.
     * for embedded structs, the directive must be at pos 0.
     */
    if (( i == 1 && CurrStruct == NULL ) ||
        ( i == 0 && CurrStruct != NULL )) {
        ;
    } else {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    alignment = ( 1 << ModuleInfo.fieldalign );

    i++; /* go past STRUCT/UNION */

    if ( i == 1 ) { /* embedded struct? */
        /* scan for the optional name */
#if ANYNAME
        /* the name might be a reserved word!
         * Masm won't allow those.
         */
        if ( AsmBuffer[i]->token != T_FINAL &&
            is_valid_id_first_char(*(AsmBuffer[i]->string_ptr) ) ) {
#else
        if ( AsmBuffer[i]->token == T_ID ) {
#endif
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
            DebugMsg1(("StructDirective(%s) alignment=%u\n", name, alignment));
        }
        /* there might also be the NONUNIQUE parameter */
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( AsmBuffer[i]->token == T_ID &&
                (_stricmp(AsmBuffer[i]->string_ptr, "NONUNIQUE") == 0)) {
                /* currently NONUNIQUE is ignored */
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
            DebugMsg1(("StructDirective: SymSearch (%s)=%X (curr struct=%X)\n", name, sym, CurrStruct ));
        } else {
            sym = SearchNameInStruct( (asm_sym *)CurrStruct, name, &offset, 0 );
            DebugMsg1(("StructDirective(%s): SearchNameInStruc()=%X\n", name, sym));
        }
    } else {
        sym = NULL;   /* anonymous struct */
    }

    if ( ModuleInfo.list ) {
        if ( CurrStruct )
            LstWrite( LSTTYPE_STRUCT, CurrStruct->sym.total_size, NULL );
        else
            LstWrite( LSTTYPE_STRUCT, 0, NULL );
    }

    /* if pass is > 1, update struct stack + CurrStruct.offset and exit */
    if ( Parse_Pass > PASS_1 ) {
        /* v2.04 changed. the previous implementation was insecure.
         * See also change in data.c, behind AddFieldToStruct().
         */
        if ( CurrStruct ) {
            sym = CurrStruct->e.structinfo->tail->sym->type;
            CurrStruct->e.structinfo->tail = CurrStruct->e.structinfo->tail->next;
        }
        dir = (dir_node *)sym;
        dir->e.structinfo->tail = dir->e.structinfo->head;
        /**/myassert( sym != NULL );
        sym->offset = 0;
        ((dir_node *)sym)->next = CurrStruct;
        CurrStruct = (dir_node *)sym;
        return( NOT_ERROR );
    }

    if( sym == NULL ) {
        /* is it a global STRUCT? */
        if ( CurrStruct == NULL )
            dir = (dir_node *)CreateTypeSymbol( NULL, name, TRUE );
        else {
            /* a nested structure is split in an anonymous STRUCT type
             * and a struct field with/without name
             */
            //field_list *f;
            dir = (dir_node *)CreateTypeSymbol( NULL, name, FALSE );
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
            CreateTypeSymbol( sym, NULL, CurrStruct != NULL );
        } else if( sym->state == SYM_TYPE && CurrStruct == NULL ) {
            /* was symbol forward referenced? */
            if ( dir->e.structinfo->typekind != TYPE_NONE ) {
                /* structure redefinition! */
                redef_struct = dir;
                dir = (dir_node *)CreateTypeSymbol( NULL, name, FALSE );
            }
        } else {
            AsmErr( SYMBOL_REDEFINITION, sym->name );
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

/* handle ENDS directive when a struct definition is active */

ret_code EndstructDirective( int i )
/**********************************/
{
    //char *name;
    //unsigned int offset;
    unsigned int size;
    //struct asm_sym *sym;
    //memtype mem_type;
    dir_node *dir;

    dir = CurrStruct;

    DebugMsg1(("EndstructDirective(%s), ofs=%u, struct size=%u, max_mbr=%u, alignment=%u\n",
              dir->sym.name,
              dir->sym.offset,
              dir->sym.total_size,
              dir->sym.max_mbr_size,
              dir->e.structinfo->alignment));

    /* if pass is > 1 just do minimal work */
    if ( Parse_Pass > PASS_1 ) {
        CurrStruct->sym.offset = 0;
        size = CurrStruct->sym.total_size;
        CurrStruct = CurrStruct->next;
        if ( CurrStruct )
            UpdateStructSize( size );
        if ( FileInfo.file[LST] )
            LstWrite( LSTTYPE_STRUCT, size, dir->sym.name );
        return( NOT_ERROR );
    }

    if ( ( i == 1 && dir->next == NULL ) ||
        ( i == 0 && dir->next != NULL ) ) {
        ;
    } else {
        /* v2.04: error msg improved */
        //AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        if ( i == 1)
            AsmErr( UNMATCHED_BLOCK_NESTING, AsmBuffer[0]->string_ptr );
        else
            AsmErr( UNMATCHED_BLOCK_NESTING, "" );
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
        DebugMsg1(("EndstructDirective:, struct size after final alignment=%u\n", dir->sym.total_size));
    }
#endif
    dir->e.structinfo->isOpen = FALSE;
    dir->sym.isdefined = TRUE;

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
                AsmErr( NON_BENIGN_XXX_REDEFINITION, "structure", dir->sym.name );
            }
            DebugMsg(("EndstructDirective: delete the redefinition of %s\n", dir->sym.name ));
            SymFree( (asm_sym *)dir );
            redef_struct = NULL;
        }
    } else {

        if ( dir->sym.max_mbr_size > CurrStruct->sym.max_mbr_size )
            CurrStruct->sym.max_mbr_size = dir->sym.max_mbr_size;

        UpdateStructSize( size );
        DebugMsg1(("EndstructDirective: new size of restored structure=%u\n", CurrStruct->sym.total_size));
    }
    //dir->sym.max_mbr_size = 0;
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

/* this function is called in pass 1 only.
 * name_loc: index of field name or -1
 * loc: initializer location, may be -1
 * mem_type: mem_type of item
 * vartype: arbitrary type of item if memtype is MT_TYPE
 * size: size of type - used for alignment only
 */
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
    char buffer[MAX_LINE_LEN];

    si = CurrStruct->e.structinfo;
    offset = CurrStruct->sym.offset;

#ifdef DEBUG_OUT
    if ( name_loc < 0 )
        DebugMsg1(("AddFieldToStruct(%s): anonymous, curr ofs=%u, vartype=%s, size=%u\n", CurrStruct->sym.name, offset, vartype ? vartype->sym.name : "NULL", size ));
    else
        DebugMsg1(("AddFieldToStruct(%s): name=%s, curr ofs=%u, vartype=%s, size=%u\n", CurrStruct->sym.name, AsmBuffer[name_loc]->string_ptr, offset, vartype ? vartype->sym.name : "NULL", size ));
#endif

    if ( name_loc >= 0 ) {
        /* the field has a name */
        name = AsmBuffer[name_loc]->string_ptr;
        /* check if field name is already used */
        /* RECORD fields names, which are global, aren't handled here */
        sym = SearchNameInStruct((asm_sym *)CurrStruct, name, (unsigned int *)&i, 0 );
        if ( sym ) {
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
    sym->isdefined = TRUE;
    sym->mem_type = mem_type;
    sym->type = &vartype->sym;
    /* ok to do? */
    // sym->total_size = SizeFromMemtype( mem_type, ModuleInfo.Ofssize );

    f = AsmAlloc( sizeof( field_list ) );

    f->next = NULL;
    f->sym = sym;

    if ( loc != -1 ) {

        i = strlen( AsmBuffer[loc]->string_ptr ) + 1;
        DebugMsg1(("AddFieldToStruct(%s): type=>%s<\n", CurrStruct->sym.name, AsmBuffer[loc]->string_ptr ));
        f->initializer = AsmAlloc( i );
        memcpy( f->initializer, AsmBuffer[loc]->string_ptr, i );

        /* now add the value to initialize the struct to */

#if 1
        /* v2.03: the initializer value may contain assembly time
         * variables ( $ inside structs is also one ). It's crucial that
         * the variable's CURRENT value is used then.
         */
        init = buffer;
        for ( i = loc+1; AsmBuffer[i]->token != T_FINAL; i++ ) {
            if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
                *init++ = '<';
                strcpy( init, AsmBuffer[i]->string_ptr );
                init += strlen( init );
                *init++ = '>';
            } else {
                if ( AsmBuffer[i]->token == T_ID ) {
                    asm_sym *sym2 = SymSearch( AsmBuffer[i]->string_ptr );
                    if ( sym2 && sym2->variable ) {
                        myltoa( sym2->uvalue, init, ModuleInfo.radix, sym2->value3264 < 0, TRUE );
                    } else {
                        strcpy( init, AsmBuffer[i]->string_ptr );
                    }
                } else {
                    strcpy( init, AsmBuffer[i]->string_ptr );
                }
                init += strlen( init );
            }
            if ( AsmBuffer[i+1]->token != T_FINAL )
                *init++ = ' ';
        }
        *init = NULLC;
        f->value = AsmAlloc( init - buffer + 1 );
        strcpy( f->value, buffer );
#else
        init = AsmBuffer[loc]->pos + i;
        while ( isspace( *init)) init++;
        f->value = AsmAlloc( strlen( init ) + 1 );
        strcpy( f->value, init );
#endif
        DebugMsg1(("AddFieldToStruct(%s): initializer=>%s<\n", CurrStruct->sym.name, f->value ));

    } else {
        DebugMsg1(("AddFieldToStruct(%s): no initializer<\n", CurrStruct->sym.name ));
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
        DebugMsg1(("AddFieldToStruct(%s): align=%u, size=%u, ofs=%u\n", CurrStruct->sym.name, si->alignment, size, offset ));
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
        DebugMsg1(("AddFieldToStruct(%s): max_mbr_size set to %u\n", CurrStruct->sym.name, size ));
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
                gsym->isdefined = TRUE;
            }
        }
    }

    return( sym );
}

/* called by AlignDirective() if ALIGN/EVEN has been found inside
 * a struct. It's already verified that <value> is a power of 2.
 */
ret_code AlignInStruct( int value )
/*********************************/
{
    if ( CurrStruct->e.structinfo->typekind != TYPE_UNION ) {
        int offset;
        offset = CurrStruct->sym.offset;
        offset = (offset + (value - 1)) & (-value);
        CurrStruct->sym.offset = offset;
        if ( offset > CurrStruct->sym.total_size )
            CurrStruct->sym.total_size = offset;
    }
    return( NOT_ERROR );
}

/* called by data_init() when a structure field has been created
 * now called on pass 1 only.
 */
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

/* TYPEDEF worker
 * this function is called by TypedefDirective()
 * and might also be called by EXTERN[DEF]/PROC/LOCAL
 * and create an anonymous pointer type then:
 * EXTERNDEF: ptr <type>
 * In those cases, name is NULL.
 */
asm_sym *CreateTypeDef( char *name, int *pi )
/*******************************************/
{
    char        *token;
    int         i = *pi;
    struct asm_sym      *sym;
    struct asm_sym      *symtype;
    memtype             mem_type;
    memtype             ptr_memtype;
    dir_node            *dir;
    dir_node            *dir2;
    int                 type;
    int                 size;
    int                 indirection;
    int                 ptrqual;

    if ( name ) {
        sym = SymSearch( name );
        if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = CreateTypeSymbol( sym, name, TRUE );
            if ( sym == NULL )
                return( NULL );
        } else {
            /* MASM allows to have the TYPEDEF included multiple times */
            /* but the types must be identical! */
            if ( ( sym->state != SYM_TYPE ) ||
                ( ((dir_node *)sym)->e.structinfo->typekind != TYPE_TYPEDEF &&
                 ((dir_node *)sym)->e.structinfo->typekind != TYPE_NONE ) ) {
                AsmErr( SYMBOL_REDEFINITION, sym->name );
                return( NULL );
            }
        }
    } else {
        sym = CreateTypeSymbol( NULL, "", FALSE );
    }
    dir = (dir_node *)sym;
    sym->Ofssize = ModuleInfo.Ofssize;

#if 0
    /* "name TYPEDEF" is a valid syntax */
    if ( i >= Token_Count ) {
        AsmError( SYNTAX_ERROR );
        return( NULL );
    }
#endif
    dir->e.structinfo->typekind = TYPE_TYPEDEF;
    sym->isdefined = TRUE;
    ptr_memtype = MT_EMPTY;

    token = AsmBuffer[i]->string_ptr;
    type = -1;
    if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_COMMA )
        type = ST_NULL;
    else if ( AsmBuffer[i]->token == T_RES_ID )
        type = FindStdType( AsmBuffer[i]->value );
    else if ( AsmBuffer[i]->token == T_DIRECTIVE ) {
        /* PROC or PROTO may occur */
        if ( AsmBuffer[i]->value == T_PROTO ) {
            dir_node * dir2;  /* create a PROTOtype item without name */
            /* v2.04: added check if prototype is set already */
            if ( dir->e.structinfo->target == NULL && sym->mem_type == MT_EMPTY ) {
                dir2 = (dir_node *)CreateProc( NULL, "", FALSE );
                DebugMsg1(("CreateTypeDef PROTO, created new unnamed prototype %p\n", dir2 ));
            } else if ( sym->mem_type == MT_PROC ) {
                dir2 = (dir_node *)dir->e.structinfo->target;
            } else {
                AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
                return( NULL );
            }
            i++;
            DebugMsg1(("CreateTypeDef PROTO, call ExamineProc(sym=%p i=%d, 0)\n", dir2, i));
            if( ExamineProc( dir2, i, FALSE ) == ERROR )
                return ( NULL );
            DebugMsg1(("CreateTypeDef PROTO, ExamineProc() returned status ok\n"));
            sym->mem_type = MT_PROC;
            /* v2.03: set value of field total_size (previously was 0) */
             if( dir2->sym.mem_type == MT_NEAR ) {
                 sym->total_size = (2 << dir2->sym.Ofssize);
             } else {
                 sym->isfar = TRUE; /* v2.04: added */
                 sym->total_size = (2 + (2 << dir2->sym.Ofssize));
             }
             dir->e.structinfo->target = (asm_sym *)dir2;
            *pi = Token_Count;
            DebugMsg1(("CreateTypeDef(%s) ok, mem_type=%Xh\n", sym->name, sym->mem_type ));
            return( sym );
        }
        if ( AsmBuffer[i]->value == T_PROC )
            type = ST_PROC;
    }

    if( type == -1 ) {
        /* must be an ID */
        if ( AsmBuffer[i]->token != T_ID ) {
            AsmErr( SYNTAX_ERROR_EX, token );
            return( NULL );
        }
        DebugMsg1(("CreateTypeDef(%s): arbitrary type\n", sym->name ));
        /* besides PROTO typedef, an arbitrary type is ok */
        symtype = SymSearch( token );
        if( symtype == NULL || symtype->state == SYM_UNDEFINED ) {
            AsmErr( SYMBOL_NOT_DEFINED, token );
            return( NULL );
        }
        if( symtype->state != SYM_TYPE ) {
            AsmError( INVALID_QUALIFIED_TYPE );
            return( NULL );
        }
        /* v2.04a: simplify the type if it is scalar */
        dir2 = (dir_node *)symtype;
        if ( dir2->e.structinfo->typekind == TYPE_TYPEDEF &&
            symtype->mem_type != MT_TYPE &&
            dir2->e.structinfo->target == NULL ) {
            mem_type = symtype->mem_type;
            size = symtype->total_size;
            indirection = dir2->e.structinfo->indirection;
            symtype = NULL;
            i++;
            goto simple_type;
        }
#if 1
        /* v2.04: compare the basic types if symbol was already defined */
        if ( sym->mem_type != MT_EMPTY ) {
            asm_sym *sym2;
            asm_sym *sym3;
            for ( sym2 = sym; sym2->type; sym2 = sym2->type );
            for ( sym3 = symtype; sym3->type; sym3 = sym3->type );
            if ( ( ((dir_node *)sym2)->e.structinfo->typekind != TYPE_TYPEDEF && sym2 != sym3 ) ||
                ( sym2->mem_type != sym3->mem_type  )) {
                DebugMsg(("CreateTypeDef(%s): error, memtypes=%X/%X\n", sym->name, sym2->mem_type, sym3->mem_type ));
                AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
                return( NULL );
            }
        }
#else
        if ( ( ( sym->mem_type != MT_TYPE ) && ( sym->mem_type != MT_EMPTY ) ) ||
            ( ( sym->total_size != 0 ) && ( sym->total_size != symtype->total_size ) ) ||
            ( ( sym->type != NULL ) && ( sym->type != symtype ) ) ) {
            DebugMsg(("CreateTypeDef(%s): error, memtype=%X size=%" FU32 "/%" FU32 " type=%p/%p type.memtype=%X\n", sym->name, sym->mem_type, sym->total_size, symtype->total_size, sym->type, symtype, symtype->mem_type ));
            AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( NULL );
        }
#endif
        i++;
        sym->mem_type = MT_TYPE;
        sym->total_size = symtype->total_size;
        sym->type = symtype;
        DebugMsg1(("CreateTypeDef(%s) ok, mem_type=MT_TYPE, size=%" FU32 ", type=%p type.memtype=%X\n",
                   sym->name, sym->total_size, sym->type, symtype->mem_type ));
    } else {
        /* it's a simple or void type */
        mem_type = SimpleType[type].mem_type;
        ptrqual = EMPTY;
        indirection = 0;
        symtype = NULL;

        size = SizeFromMemtype( SimpleType[type].mem_type, SimpleType[type].Ofssize, NULL );

        while ( AsmBuffer[i]->token == T_RES_ID ) {
            switch ( AsmBuffer[i]->value ) {
            case T_FAR:
            case T_FAR16:
            case T_FAR32:
            case T_NEAR:
            case T_NEAR16:
            case T_NEAR32:
                if ( ptrqual == EMPTY ) {
                    ptrqual = AsmBuffer[i]->value;
                    type = FindStdType( ptrqual );
                    size = SizeFromMemtype( SimpleType[type].mem_type, SimpleType[type].Ofssize, NULL );

                    if ( ptrqual == T_NEAR16 || ptrqual == T_FAR16 )
                        sym->Ofssize = USE16;
                    else if ( ptrqual == T_NEAR32 || ptrqual == T_FAR32 )
                        sym->Ofssize = USE32;

                    if ( ptrqual == T_FAR || ptrqual == T_FAR16 || ptrqual == T_FAR32 )
                        sym->isfar = TRUE;
                    else
                        sym->isfar = FALSE;
                }
                i++;
                if ( indirection == 0 &&
                    ( AsmBuffer[i]->token != T_RES_ID ||
                     AsmBuffer[i]->value != T_PTR ) ) {
                    AsmError( INVALID_QUALIFIED_TYPE );
                    return( NULL );
                }
                DebugMsg1(("CreateTypeDef(%s): pointer type, indirection=%u, qual=%d\n", sym->name, indirection, ptrqual ));
                continue;
            case T_PTR:
                mem_type = MT_PTR;
                indirection++;
                /* v2,94: added */
                if ( indirection == 1 && ptrqual == EMPTY )
                    if ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) )
                        sym->isfar = TRUE;
                i++;
                DebugMsg1(("CreateTypeDef(%s): pointer type, indirection=%u, qual=%d\n", sym->name, indirection, ptrqual ));
                continue;
            }
            break; /* other T_RES_IDs ( scalar types, FLAT, VARARG,... ) break */
        }

        DebugMsg1(("CreateTypeDef(%s): typeindex=%u memtype=%Xh size=%u i=%u token=%u\n",
                   sym->name, type, sym->mem_type, size, i, AsmBuffer[i]->token));

        if ( AsmBuffer[i]->token == T_ID  ) {
            symtype = SymSearch( AsmBuffer[i]->string_ptr );
            DebugMsg1(("CreateTypeDef: ptr dest=%X (name=%s)\n", symtype, symtype ? symtype->name : "" ));
            /* v2.04: added */
            if ( symtype == NULL )
                symtype = CreateTypeSymbol( dir->e.structinfo->target, AsmBuffer[i]->string_ptr, TRUE );
            if ( symtype ) {
                /* accept pointers to PROTOs and arbitrary types */
                if ( symtype->state == SYM_TYPE ) {
                    DebugMsg1(("CreateTypeDef: >%s< is arbitrary type\n", symtype->name ));
                    if ( symtype->mem_type == MT_PROC ) {
                        DebugMsg(("CreateTypeDef: >%s< is function pointer\n", symtype->name ));
                        /* v2.03: ptr to PROTO has size of the proc */
                        if ( type == ST_PTR && indirection == 1 )
                            size = symtype->total_size;
                        /* v2.04: added */
                        if ( indirection && ( ptrqual == EMPTY ))
                            sym->isfar = symtype->isfar;
                    }
                } else {
                    DebugMsg(("CreateTypeDef: invalid type, symbol state=%u\n", symtype->state));
                    if ( sym->state == SYM_UNDEFINED )
                        AsmErr( SYMBOL_NOT_DEFINED, symtype->name );
                    else
                        AsmErr( SYMBOL_TYPE_CONFLICT, symtype->name );
                    return( NULL );
                }
            } /* if symbol isn't found, don't emit an error! */
            i++;
        } else if ( AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->type == RWT_TYPE ) {
            /* v2.04: store mem_type of pointer */
            if ( indirection ) {
                ptr_memtype = SimpleType[SpecialTable[ AsmBuffer[i]->value ].value8].mem_type;
            }
            i++;
        }
        /* v2.04:check if the arbitrary type is just an alias of a simple type.
         * If yes, use the simple type instead.
         */
        if ( symtype ) {
            for ( ; symtype->type; symtype = symtype->type );
            if ( ((dir_node *)symtype)->e.structinfo->typekind == TYPE_TYPEDEF &&
                ((dir_node *)symtype)->e.structinfo->indirection == 0 &&
                ((dir_node *)symtype)->e.structinfo->target == NULL ) {
                ptr_memtype = symtype->mem_type;
                symtype = NULL;
            }
        }
    simple_type:
        /* if type did exist already, check for type conflicts
         * v2.04: this code has been rewritten */
        if ( sym->mem_type != MT_EMPTY ) {
            asm_sym *sym2;
            for ( sym2 = sym; sym2->type; sym2 = sym2->type );
            if ( sym2->mem_type != mem_type ||
                dir->e.structinfo->indirection != indirection ||
                dir->e.structinfo->target != symtype ||
                dir->e.structinfo->ptr_memtype != ptr_memtype ) {
                DebugMsg(("CreateTypeDef(%s): error, memtype=%X/%X indirection=%u/%u types=%p/%p ptypes=%X/%X\n",
                          sym->name, sym->mem_type, mem_type,
                          dir->e.structinfo->indirection, indirection,
                          dir->e.structinfo->target, symtype,
                          dir->e.structinfo->ptr_memtype, ptr_memtype ));
                AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
                return( NULL );
            }
        }
        sym->mem_type = mem_type;
        sym->total_size = size;
        dir->e.structinfo->target = symtype;
        dir->e.structinfo->indirection = indirection;
        dir->e.structinfo->ptr_memtype = ptr_memtype;
        DebugMsg1(("CreateTypeDef(%s) ok, mem_type=%Xh, size=%u, indirection=%u target=%p\n", sym->name, sym->mem_type, size, indirection, symtype ));
    }
    *pi = i;
    return( (asm_sym *)dir );
}

/* Handle TYPEDEF directive. Called on pass 1 only.
 * usual syntax is:
 * <type name> TYPEDEF [[far|near [ptr]]<ref type name>]
 */
ret_code TypedefDirective( int i )
/********************************/
{
    char *name;

    DebugMsg1(("TypeDef enter, i=%d\n", i));

    if( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    name = AsmBuffer[0]->string_ptr;

    i++; /* go past TYPEDEF */

    if ( CreateTypeDef( name, &i ) == NULL )
        return( ERROR );
    if ( AsmBuffer[i]->token != T_FINAL ) {
        DebugMsg(("TypeDef: unexpected token %u, idx=%u\n", AsmBuffer[i]->token, i));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    return( NOT_ERROR );
}

/* generate a RECORD. Called on pass 1 only */

ret_code RecordDef( int i )
/*************************/
{
    char *name;
    struct asm_sym *sym;
    dir_node *dir;
    dir_node *oldr = NULL;
    field_list  *f;
    int num;
    int redef_err;
    //int value;
    int name_loc;
    expr_list opndx;

    DebugMsg(("RecordDef enter, i=%u\n", i));
    if ( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
#if 0
    /* a record definition can be placed
     * inside a STRUCT, but it will still be global
     */
    if ( StructDef.struct_depth > 0 ) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
#endif

    name = AsmBuffer[0]->string_ptr;
    sym = SymSearch( name );
    if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
        sym = CreateTypeSymbol( sym, name, TRUE );
    } else if ( sym->state == SYM_TYPE &&
               ( ((dir_node *)sym)->e.structinfo->typekind == TYPE_RECORD ||
               ((dir_node *)sym)->e.structinfo->typekind == TYPE_NONE ) ) {
        /* v2.04: allow redefinition of record and forward references */
        if ( ((dir_node *)sym)->e.structinfo->typekind == TYPE_RECORD ) {
            oldr = (dir_node *)sym;
            sym = CreateTypeSymbol( NULL, name, FALSE );
            redef_err = 0;
        }
    } else {
        AsmErr( SYMBOL_REDEFINITION, name );
        return( ERROR );
    }
    dir = (dir_node *)sym;
    dir->e.structinfo->typekind = TYPE_RECORD;
    dir->sym.isdefined = TRUE;

    i++; /* go past RECORD */

    num = 0; /* counter for total of bits in record */
    do {
        if ( AsmBuffer[i]->token != T_ID ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            break;
        }
        name_loc = i;
        i++;
        if ( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            break;
        }
        i++;
        /* get width */
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            break;
        if ( opndx.kind != EXPR_CONST ) {
            AsmError( CONSTANT_EXPECTED );
            opndx.value = 1;
        }
        if ( opndx.value + num > 32 ) {
            AsmError( TOO_MANY_BITS_IN_RECORD );
            break;
        }
        /* record field names are global! (Masm design flaw) */
        if ( oldr ) {
            sym = SymSearch( AsmBuffer[name_loc]->string_ptr );
            if ( sym == NULL ||
                sym->state != SYM_STRUCT_FIELD ||
                sym->mem_type != MT_BITS ||
                sym->total_size != opndx.value ) {
                AsmErr( NON_BENIGN_XXX_REDEFINITION, szRecord, AsmBuffer[name_loc]->string_ptr );
                redef_err++;
            }
        } else {
            sym = SymLookup( AsmBuffer[name_loc]->string_ptr );
            if ( !sym )
                break;
            if ( sym->state != SYM_UNDEFINED ) {
                AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
                break;
            }
            sym->state = SYM_STRUCT_FIELD;
            sym->mem_type = MT_BITS;
            sym->total_size = opndx.value;
        }
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
        if ( *AsmBuffer[i]->string_ptr == '=' ) {
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
                strcat( f->value, AsmBuffer[i]->string_ptr );
                if (i != j - 1)
                    strcat( f->value," " );
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

    } while ( i < Token_Count );

    /* now calc size in bytes and set the bit positions */

    if ( num > 16 ) {
        dir->sym.total_size = 4;
        dir->sym.mem_type = MT_DWORD;
    } else if ( num > 8 ) {
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
    if ( oldr ) {
        if ( redef_err > 0 ||
            AreStructsEqual( dir, oldr ) == FALSE )
            AsmErr( NON_BENIGN_XXX_REDEFINITION, szRecord, dir->sym.name );
        /* record can be freed, because the record's fields are global items */
        SymFree( (asm_sym *)dir );
    }
    DebugMsg(("RecordDef exit, no error\n"));
    return( NOT_ERROR );
}

void DeleteType( dir_node *dir )
/******************************/
{
    field_list      *curr;
    field_list      *next;

    DebugMsg(("DeleteType(%s) enter\n", dir->sym.name ));
    for( curr = dir->e.structinfo->head; curr != NULL; curr = next ) {
        /* bitfields field names are global, don't free them here! */
        if ( dir->e.structinfo->typekind != TYPE_RECORD )
            SymFree( curr->sym );
        AsmFree( curr->initializer );
        AsmFree( curr->value );
        next = curr->next;
        AsmFree( curr );
    }
    AsmFree( dir->e.structinfo );
    return;
}

