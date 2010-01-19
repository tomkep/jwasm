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
* Description:  Processing of PUBLIC, EXT[E]RN, EXTERNDEF, COMM directives.
*
****************************************************************************/

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"
#include "segment.h"
#include "fastpass.h"
#include "listing.h"
#include "queues.h"
#include "equate.h"
#include "fixup.h"
#include "mangle.h"
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "types.h"
#include "condasm.h"
#include "proc.h"
#include "extern.h"

// the "mangler" has been inherited from Wasm.
// it allows some fine tuning of the external's name in the object module,
// but is relevant for mixing with OW code only.
// syntax:
// EXTERN|EXTERNDEF [ [ mangle_type, ] lang_type ] name : type
// PUBLIC [ [ mangle_type, ] lang_type ] name
// COMM [ [ mangle_type, ] langtype] [NEAR|FAR] name: ...
// mangle_type must be a string.

#if MANGLERSUPP
static char *Check4Mangler( int *i )
/**********************************/
{
    char *mangle_type = NULL;
    if( AsmBuffer[*i]->token == T_STRING ) {
        mangle_type = AsmBuffer[*i]->string_ptr;
        (*i)++;
        if( AsmBuffer[*i]->token != T_COMMA ) {
            AsmWarn( 2, EXPECTING_COMMA );
        } else {
            (*i)++;
        }
    }
    return( mangle_type );
}
#endif

static asm_sym *CreateGlobal( asm_sym *sym, const char * name )
/*************************************************************/
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    if ( sym ) {
        sym->state = SYM_EXTERNAL;
        sym->Ofssize = ModuleInfo.Ofssize;
        sym->comm = 0;
        sym->weak = 1;
        dir_add_table( (dir_node *)sym );
    }
    return( sym );
}

static asm_sym *CreateExternal( asm_sym *sym, const char *name )
/**************************************************************/
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    if ( sym ) {
        sym->state = SYM_EXTERNAL;
        sym->Ofssize = ModuleInfo.Ofssize;
        sym->comm = 0;
        sym->weak = 0;
        dir_add_table( (dir_node *)sym );
    }
    return( sym );
}

static asm_sym *CreateComm( asm_sym *sym, const char *name)
/*********************************************************/
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    if ( sym ) {
        sym->state = SYM_EXTERNAL;
        sym->Ofssize = ModuleInfo.Ofssize;
        sym->comm = 1;
        sym->weak = 0;
        sym->isfar = 0;
        dir_add_table( (dir_node *)sym );
    }
    return( sym );
}

// externdef [ attr ] symbol:type
// called during Pass 1 only

ret_code ExterndefDirective( int i )
/**********************************/
{
    char                *token;
    char                *mangle_type = NULL;
    //char                *typetoken;
    memtype             mem_type;
    unsigned char       Ofssize;
    int                 size;
    struct asm_sym      *sym;
    struct asm_sym      *symtype;
    lang_type           langtype;

    DebugMsg(("ExterndefDirective entry\n"));

#if MANGLERSUPP
    mangle_type = Check4Mangler( &i );
#endif
    do {

        symtype = NULL;
        mem_type = MT_EMPTY;
         /* set default offset size */
        if ( CurrSeg )
            Ofssize = ModuleInfo.Ofssize;
        else
            Ofssize = ModuleInfo.defOfssize;

        /* get the symbol language type if present */
        langtype = ModuleInfo.langtype;
        GetLangType( &i, &langtype );

        /* get the symbol name */
        if( AsmBuffer[i]->token != T_ID ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        token = AsmBuffer[i++]->string_ptr;

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        //typetoken = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token == T_ID ) {
            if (0 == _stricmp(AsmBuffer[i]->string_ptr, "ABS")) {
                mem_type = MT_ABS;
            } else if( symtype = SymIsType( AsmBuffer[i]->string_ptr ) ) {
                mem_type = MT_TYPE;
            }
        } else if ( AsmBuffer[i]->token == T_DIRECTIVE ) {
            if ( AsmBuffer[i]->value == T_PROTO ) {
                /* dont scan this line further */
                /* ProtoDef() will define a SYM_EXTERNAL */
                return ( ProtoDef(i, token) );
            } else if ( AsmBuffer[i]->value == T_PROC ) {
                mem_type = SimpleType[ST_PROC].mem_type;
            }
        } else if ( AsmBuffer[i]->token == T_RES_ID ) {
            if ( AsmBuffer[i]->rm_byte == RWT_TYPE ) {
                mem_type = SimpleType[AsmBuffer[i]->opcode].mem_type;
                if ( SimpleType[AsmBuffer[i]->opcode].Ofssize != USE_EMPTY )
                    Ofssize = SimpleType[AsmBuffer[i]->opcode].Ofssize;
            } else if ( AsmBuffer[i]->value == T_PTR ) {
                mem_type = MT_PTR;
            }
        }
        if ( mem_type == MT_EMPTY ) {
            /* v2.01: "EXTERNDEF <name>:" is accepted by Masm!
             * No type is associated with the external then.
             */
            if ( AsmBuffer[i]->token != T_FINAL ) {
                AsmError( INVALID_QUALIFIED_TYPE );
                return( ERROR );
            }
        } else if ( mem_type == MT_PTR && AsmBuffer[i+1]->token != T_FINAL ) {
            /* if it is a pointer to something (not VOID),
             an anonymous TYPEDEF has to be created
             */
            if (( symtype = CreateTypeDef("", &i )) == NULL )
                return (ERROR);
            DebugMsg(("ExterndefDirective(%s): CreateTypeDef()=%X\n", token, symtype));
            mem_type = MT_TYPE;
        } else {
            i++;
        }

        sym = SymSearch( token );
        if (!sym) {
            sym = CreateGlobal( NULL, token );
            DebugMsg(("ExterndefDirective(%s): new symbol\n", token));
        } else {
            if ( sym->state == SYM_UNDEFINED ) {
                dir_remove_table( (dir_node *)sym );
                CreateGlobal( sym, NULL );
            }
            DebugMsg(("ExterndefDirective(%s): symbol exists, state=%u\n", token, sym->state));
        }

        sym->defined = TRUE;

        /* ensure that the type of the symbol won't change */

        if ( sym->state == SYM_EXTERNAL && sym->mem_type == MT_EMPTY ) {
            DebugMsg(("ExterndefDirective: type set for >%s<\n", sym->name));
            if (mem_type != MT_ABS)
                SetSymSegOfs(sym);
            else if (sym->weak == TRUE)
                sym->equate = TRUE; /* allow redefinition by EQU, = */
            sym->offset = 0;
            sym->Ofssize = Ofssize;

            if ( sym->segment && ((dir_node *)sym->segment)->e.seginfo->Ofssize != sym->Ofssize )
                sym->segment = NULL;

            sym->mem_type = mem_type;
            if (mem_type == MT_TYPE) {
                sym->type = symtype;
                sym->total_size = symtype->total_size;
            } else {
                size = SizeFromMemtype(mem_type, Ofssize );
                if ( size != 0 )
                    sym->total_size = size;
            }
        } else if ( sym->mem_type != mem_type ) {
            /* if the symbol is already defined (as SYM_INTERNAL), Masm
             won't display an error. The other way, first externdef and
             then the definition, will make Masm complain, however */
            DebugMsg(("ExterndefDirective: type conflict for %s. mem_types: %u - %u ; %u - %u\n", sym->name, sym->mem_type, mem_type));
            AsmWarn( 1, SYMBOL_TYPE_CONFLICT, sym->name );
        } else if ( sym->mem_type == MT_TYPE && sym->type != symtype ) {
            asm_sym *sym2 = sym;
            /* skip alias types and compare the base types */
            DebugMsg(("ExterndefDirective(%s): types differ: %X (%s) - %X (%s)\n", sym->name, sym->type, sym->type->name, symtype, symtype->name));
            while (sym2->type)
                sym2 = sym2->type;
            while (symtype->type)
                symtype = symtype->type;
            if (sym2 != symtype) {
                DebugMsg(("ExterndefDirective(%s): type conflict: %X (%s) - %X (%s)\n", sym->name, sym2, sym2->name, symtype, symtype->name));
                AsmWarn( 1, SYMBOL_TYPE_CONFLICT, sym->name );
            }
        }

        // FIXME !! symbol can have different language type
        SetMangler( sym, mangle_type, langtype );

        /* write a global entry if none has been written yet */
        if ( sym->state == SYM_EXTERNAL && sym->weak == FALSE )
            ;// skip EXTERNDEF if a real EXTERN/COMM was done
        else if ( sym->global == FALSE ) {
            sym->global = TRUE;
            DebugMsg(("writing a global entry for %s\n", sym->name));
            AddGlobalData( (dir_node *)sym );
        }

        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA ) {
                if ( (i + 1) < Token_Count )
                    i++;
            } else {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }

    } while ( i < Token_Count );

    return( NOT_ERROR );
}

// helper for EXTERN directive

asm_sym *MakeExtern( const char *name, memtype mem_type, struct asm_sym * vartype, asm_sym * sym, uint_8 Ofssize )
/********************************************************************/
{
    sym = CreateExternal( sym, name );
    if (sym == NULL)
        return( NULL );

    if (mem_type != MT_ABS)
        SetSymSegOfs( sym );

    sym->offset = 0;
    sym->defined = TRUE;
    sym->mem_type = mem_type;
    if ( mem_type != MT_TYPE ) {
        int size = SizeFromMemtype( mem_type, Ofssize );
        sym->Ofssize = Ofssize;
        if ( size != 0 )
            sym->total_size = size;
    } else
        sym->total_size = vartype->total_size;
    sym->type = vartype;
    return( sym );
}

// syntax: EXTERN [lang_type] name (altname) :type [, ...]

ret_code ExternDirective( int i )
/*******************************/
{
    char                *token;
    char                *mangle_type = NULL;
    char                *typetoken;
    char                *altname;
    memtype             mem_type;
    unsigned char       Ofssize;
    struct asm_sym      *sym;
    struct asm_sym      *symtype;
    lang_type           langtype;

#if MANGLERSUPP
    mangle_type = Check4Mangler( &i );
#endif
    do {

        symtype = NULL;
        mem_type = MT_EMPTY;
        Ofssize = ModuleInfo.Ofssize;

        /* get the symbol language type if present */
        langtype = ModuleInfo.langtype;
        GetLangType( &i, &langtype );

        /* get the symbol name */
        if( AsmBuffer[i]->token != T_ID ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        token = AsmBuffer[i++]->string_ptr;

        /* go past the optional alternative name (weak ext, default resolution) */
        if( AsmBuffer[i]->token == T_OP_BRACKET ) {
            i++;
            if ( AsmBuffer[i]->token != T_ID ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
            altname = AsmBuffer[i]->string_ptr;
            i++;
            if( AsmBuffer[i]->token != T_CL_BRACKET ) {
                AsmErr( EXPECTED, ")" );
                return( ERROR );
            }
            i++;
        } else
            altname = NULL;

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        typetoken = AsmBuffer[i]->string_ptr;
        if (AsmBuffer[i]->token == T_ID) {
            if ( 0 == _stricmp( AsmBuffer[i]->string_ptr, "ABS" ) ) {
                mem_type = MT_ABS;
            } else if ( symtype = SymIsType( typetoken ) ) {
                mem_type = MT_TYPE;
            }
        } else if ( AsmBuffer[i]->token == T_DIRECTIVE ) {
            if ( AsmBuffer[i]->value == T_PROC ) {
                mem_type = SimpleType[ST_PROC].mem_type;
            }
        } else if ( AsmBuffer[i]->token == T_RES_ID ) {
            if ( AsmBuffer[i]->rm_byte == RWT_TYPE ) {
                mem_type = SimpleType[AsmBuffer[i]->opcode].mem_type;
                if (SimpleType[AsmBuffer[i]->opcode].Ofssize != USE_EMPTY)
                    Ofssize = SimpleType[AsmBuffer[i]->opcode].Ofssize;
            } else if ( AsmBuffer[i]->value == T_PTR ) {
                mem_type = MT_PTR;
            }
        }

        if ( mem_type == MT_EMPTY ) {
            /* v2.01: "EXTERN <name>:" is accepted by Masm!
             * No type is associated with the external then.
             */
            if ( AsmBuffer[i]->token != T_FINAL ) {
                AsmError( INVALID_QUALIFIED_TYPE );
                return( ERROR );
            }
        }

        for( ; i < Token_Count && AsmBuffer[i]->token != T_COMMA; i++ );

        sym = SymSearch( token );

        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            if ( sym && sym->public == TRUE ) {
                AsmErr(CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name);
                return( ERROR );
            }
            if ( sym )
                dir_remove_table( (dir_node *)sym );
            if(( sym = MakeExtern( token, mem_type, symtype, sym, Ofssize )) == NULL )
                return( ERROR );

        } else {
            if( sym->mem_type != mem_type ||
                ( langtype != LANG_NONE && sym->langtype != LANG_NONE && sym->langtype != langtype )) {
                AsmError( EXT_DEF_DIFF );
                return( ERROR );
            }
            /* if EXTERN is placed BEHIND the symbol definition, it's
             ignored by Masm! */
            if ( sym->state != SYM_EXTERNAL ) {
                DebugMsg(("ExternDirective: symbol %s redefinition\n", token ));
                AsmWarn( 3, SYMBOL_REDEFINITION, token );
            }
        }
        if ( altname && sym->state == SYM_EXTERNAL ) {
            if ( sym->altname ) {
                if( strcmp( sym->altname->name, altname ) ) {
                    AsmError( EXT_DEF_DIFF );
                    return( ERROR );
                }
            } else {
                if ( sym->altname = SymSearch( altname ) ) {
                    if ( sym->altname->state != SYM_INTERNAL ) {
                        sym->altname = NULL;
                        AsmErr( SYMBOL_TYPE_CONFLICT, altname );
                        return( ERROR );
                    }
                } else {
                    sym->altname = CreateGlobal( NULL, altname );
                }
            }
        }
        SetMangler( sym, mangle_type, langtype );

        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA ) {
                if ( (i + 1) < Token_Count )
                    i++;
            } else {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }
    }  while ( i < Token_Count );

    return( NOT_ERROR );
}

/* scan EXTERNS with alternative names */

ret_code ExternDirective2( int i )
/********************************/
{
    char                *token;
    //char                *mangle_type;
    lang_type           langtype;
    struct asm_sym      *sym;
    struct asm_sym      *symalt;

#if MANGLERSUPP
    //mangle_type = Check4Mangler( &i );
    Check4Mangler( &i );
#endif
    for( ; i < Token_Count; i++ ) {

        GetLangType( &i, &langtype );

        /* get the symbol name */
        token = AsmBuffer[i++]->string_ptr;

        /* is there the optional alternative name? */
        if( AsmBuffer[i]->token == T_OP_BRACKET ) {
            i++;
            symalt = SymSearch( AsmBuffer[i]->string_ptr );
            if ( symalt == NULL || ( symalt->state == SYM_EXTERNAL && symalt->weak == TRUE ) ) {
                AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
            } else if (symalt->state != SYM_INTERNAL ) {
                AsmErr( SYMBOL_TYPE_CONFLICT, AsmBuffer[i]->string_ptr );
            } else {
                sym = SymSearch( token );
                if ( sym == NULL || sym->mem_type != symalt->mem_type )
                    AsmErr( SYMBOL_TYPE_CONFLICT, AsmBuffer[i]->string_ptr );
            }
        }
        for( ; i < Token_Count && AsmBuffer[i]->token != T_COMMA; i++ );
    }
    return( NOT_ERROR );
}

// syntax: PUBLIC [lang_type] name [, ...]

ret_code PublicDirective( int i )
/*******************************/
{
    char                *mangle_type = NULL;
    char                *token;
    struct asm_sym      *sym;
    //dir_node            *dir;
    lang_type           langtype;

#if MANGLERSUPP
    mangle_type = Check4Mangler( &i );
#endif
    do {

        /* read the optional language type */
        langtype = ModuleInfo.langtype;
        GetLangType( &i, &langtype );

        if ( AsmBuffer[i]->token != T_ID ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        /* get the symbol name */
        token = AsmBuffer[i++]->string_ptr;

        DebugMsg(("PublicDirective: %s\n", token ));

        /* Add the public name */
        sym = SymSearch( token );
        if (Parse_Pass != PASS_1 && (sym == NULL || sym->state == SYM_UNDEFINED)) {
            AsmErr( SYMBOL_NOT_DEFINED, token );
            return( ERROR );
        }
        if( sym != NULL ) {
            if (sym->state != SYM_UNDEFINED &&
                sym->state != SYM_INTERNAL &&
                sym->state != SYM_EXTERNAL ) {
                AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
                return( ERROR );
            }
            if ( sym->scoped == TRUE && ( sym->state == SYM_INTERNAL ) ) {
                AsmErr( CANNOT_DECLARE_SCOPED_CODE_LABEL_AS_PUBLIC, sym->name );
                return( ERROR );
            }
            if ( sym->state == SYM_EXTERNAL && sym->weak != TRUE ) {
                DebugMsg(("PublicDirective: symbol redefinition\n"));
                AsmErr( SYMBOL_REDEFINITION, sym->name );
                return( ERROR );
            }
            if( Parse_Pass == PASS_1 && sym->public == FALSE ) {
                /* put it into the public table */
                sym->public = TRUE;
                AddPublicData( sym );
            }
        } else if (Parse_Pass == PASS_1) {
            sym = SymCreate( token, TRUE );
            dir_add_table( (dir_node *)sym );
            sym->public = TRUE;
            AddPublicData( sym );
            DebugMsg(("PublicDirective: new symbol, state=%u\n", sym->state ));
        }
        if (Parse_Pass == PASS_1)
            SetMangler( sym, mangle_type, langtype );

        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA ) {
                if ( (i + 1) < Token_Count )
                    i++;
            } else {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }

    } while ( i < Token_Count );

    return( NOT_ERROR );
}

// helper for COMM directive

static asm_sym *MakeComm( char *name, asm_sym *sym, int size, int count, bool isfar )
/***********************************************************************************/
{
    sym = CreateComm( sym, name );
    if( sym == NULL )
        return( NULL );

    sym->total_length = count;
    sym->isfar = isfar;

    SetSymSegOfs( sym );
    sym->offset = 0;

    MemtypeFromSize( size, &sym->mem_type );

    sym->total_size = count * size;

    return( sym );
}

/* define "communal" items
 syntax:
 COMM [langtype] [NEAR|FAR] label:type[:count] [, ... ]
 */

ret_code CommDirective( int i )
/*****************************/
{
    char            *token;
    char            *mangle_type = NULL;
    bool            isfar;
    //int             distance;
    int             tmp;
    int             size;
    int             count;
    struct asm_sym  *sym;
    expr_list       opndx;
    lang_type       langtype;

    for( ; i < Token_Count; i++ ) {
        count = 1;
#if MANGLERSUPP
        mangle_type = Check4Mangler( &i );
#endif
        /* get the symbol language type if present */
        langtype = ModuleInfo.langtype;
        GetLangType( &i, &langtype );

        /* get the distance ( near or far ) */
        isfar = FALSE;
        if (AsmBuffer[i]->token == T_RES_ID)
            switch (AsmBuffer[i]->value) {
            case T_FAR:
            case T_FAR16:
            case T_FAR32:
                isfar = TRUE;
                if (ModuleInfo.model == MOD_FLAT) {
                    AsmError(FAR_NOT_ALLOWED_IN_FLAT_MODEL_COMM_VARIABLES);
                    return( ERROR );
                }
            case T_NEAR:
            case T_NEAR16:
            case T_NEAR32:
                i++;
            }

        /* get the symbol name */
        token = AsmBuffer[i++]->string_ptr;

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;
        /* the evaluator cannot handle a ':' so scan for one first */
        for (tmp = i; tmp < Token_Count;tmp++)
            if (AsmBuffer[tmp]->token == T_COLON)
                break;
        if ( EvalOperand( &i, tmp, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if (opndx.kind != EXPR_CONST || opndx.string != NULL) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if (opndx.value == 0) {
            AsmError( POSITIVE_VALUE_EXPECTED );
            return( ERROR );
        }
        size = opndx.value;

        if( AsmBuffer[i]->token == T_COLON ) {
            i++;
            /* count */
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                return( ERROR );
            if (opndx.kind != EXPR_CONST || opndx.string != NULL) {
                AsmError( CONSTANT_EXPECTED );
                return( ERROR );
            }
            if (opndx.value == 0) {
                AsmError( POSITIVE_VALUE_EXPECTED );
                return( ERROR );
            }
            count = opndx.value;
        }

        sym = SymSearch( token );
        if( sym != NULL ) {
            if( sym->state == SYM_UNDEFINED ) {
                if ( sym->public ) {
                    AsmErr(CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name);
                    return( ERROR );
                }
                dir_remove_table( (dir_node *)sym );
                if( MakeComm( token, sym, size, count, isfar) == NULL )
                    return( ERROR );
            } else {
                if (sym->total_length == 0)
                    tmp = sym->total_size;
                else
                    tmp = sym->total_size / sym->total_length;
                if( size != tmp) {
                    AsmError( EXT_DEF_DIFF );
                    return( ERROR );
                }
            }
        } else {
            sym = MakeComm( token, FALSE, size, count, isfar );
            if ( sym == NULL )
                return( ERROR );
        }
        SetMangler( sym, mangle_type, langtype );

        if ( AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}
