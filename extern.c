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
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "fastpass.h"
#include "listing.h"
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

static const char szCOMM[] = "COMM";

#if MANGLERSUPP

/* The "mangler" has been inherited from Wasm.
 * By default it's not active in JWasm (see MANGLERSUPP in globals.h)
 * It allows some fine tuning of the external's name in the object module,
 * but is relevant for mixing with OW code only.
 * Syntax:
 * EXTERN|EXTERNDEF [ [ mangle_type, ] lang_type ] name : type
 * PUBLIC [ [ mangle_type, ] lang_type ] name
 * COMM [ [ mangle_type, ] langtype] [NEAR|FAR] name: ...
 * mangle_type must be a 'string'.
 */
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

/* create external.
 * sym must be NULL or of state SYM_UNDEFINED!
 */
static asm_sym *CreateExternal( asm_sym *sym, const char *name, char weak )
/*************************************************************************/
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    else
        dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );

    if ( sym ) {
        sym->state = SYM_EXTERNAL;
        sym->Ofssize = ModuleInfo.Ofssize;
        sym->comm = FALSE;
        sym->weak = weak;
        dir_add_table( &Tables[TAB_EXT], (dir_node *)sym ); /* add EXTERNAL */
    }
    return( sym );
}

/* create communal.
 * sym must be NULL or of state SYM_UNDEFINED!
 */
static asm_sym *CreateComm( asm_sym *sym, const char *name )
/**********************************************************/
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    else
        dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );

    if ( sym ) {
        sym->state = SYM_EXTERNAL;
        sym->Ofssize = ModuleInfo.Ofssize;
        sym->comm = TRUE;
        sym->weak = FALSE;
        sym->isfar = FALSE;
        dir_add_table( &Tables[TAB_EXT], (dir_node *)sym ); /* add EXTERNAL */
    }
    return( sym );
}

/* externdef [ attr ] symbol:type [, symbol:type,...] */

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

    DebugMsg1(("ExterndefDirective(%u) enter\n", i));

    i++; /* skip EXTERNDEF token */
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

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;
        sym = SymSearch( token );

        //typetoken = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token == T_ID ) {
            if (0 == _stricmp(AsmBuffer[i]->string_ptr, "ABS")) {
                mem_type = MT_ABS;
            } else if( symtype = SymIsType( AsmBuffer[i]->string_ptr ) ) {
                mem_type = MT_TYPE;
            }
        } else if ( AsmBuffer[i]->token == T_DIRECTIVE ) {
            if ( AsmBuffer[i]->value == T_PROTO ) {
                /* dont scan this line further!
                 * CreateProto() will either define a SYM_EXTERNAL or fail
                 * if there's a syntax error of symbol redefinition.
                 */
                sym = CreateProto( i + 1, token );
                if ( sym && sym->global == FALSE ) {
                    sym->global = TRUE;
                    QAddItem( &ModuleInfo.g.GlobalQueue, sym );
                }
                return( sym ? NOT_ERROR : ERROR );
            } else if ( AsmBuffer[i]->value == T_PROC ) {
                mem_type = SimpleType[ST_PROC].mem_type;
            }
        } else if ( AsmBuffer[i]->token == T_RES_ID ) {
            if ( AsmBuffer[i]->type == RWT_TYPE ) {
                mem_type = SimpleType[AsmBuffer[i]->value8].mem_type;
                if ( SimpleType[AsmBuffer[i]->value8].Ofssize != USE_EMPTY )
                    Ofssize = SimpleType[AsmBuffer[i]->value8].Ofssize;
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
             * an anonymous TYPEDEF has to be created
             */
            if ( sym && sym->mem_type == MT_TYPE ) {
                symtype = sym->type;
                for( ; i < Token_Count && AsmBuffer[i]->token != T_COMMA; i++ );
            } else
                if ( ( symtype = CreateTypeDef( NULL, &i ) ) == NULL )
                    return( ERROR );
            DebugMsg(("ExterndefDirective(%s): CreateTypeDef()=%X\n", token, symtype));
            mem_type = MT_TYPE;
        } else {
            i++;
        }

        if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = CreateExternal( sym, token, TRUE );
        }

        /* new symbol? */

        if ( sym->state == SYM_EXTERNAL && sym->mem_type == MT_EMPTY ) {
            DebugMsg1(("ExterndefDirective(%s): memtype=%X set, ofssize=%X\n", token, mem_type, Ofssize ));
            switch ( mem_type ) {
            case MT_ABS:
                /* v2.04: hack no longer necessary */
                //if ( sym->weak == TRUE )
                //    sym->equate = TRUE; /* allow redefinition by EQU, = */
                break;
            case MT_FAR:
                /* v2.04: don't inherit current segment for FAR externals
                 * if -Zg is set.
                 */
                if ( Options.masm_compat_gencode )
                    break; 
                /* fall through */
            default:
                //SetSymSegOfs( sym );
                sym->segment = &CurrSeg->sym;
            }
            sym->Ofssize = Ofssize;

            if ( sym->segment && ((dir_node *)sym->segment)->e.seginfo->Ofssize != sym->Ofssize )
                sym->segment = NULL;

            sym->mem_type = mem_type;
            if ( mem_type == MT_TYPE ) {
                sym->type = symtype;
                sym->total_size = symtype->total_size;
            } else {
                size = SizeFromMemtype( mem_type, Ofssize, symtype );
                if ( size != 0 )
                    sym->total_size = size;
            }
            /* v2.04: only set language if there was no previous definition */
            SetMangler( sym, mangle_type, langtype );

        } else {

            /* ensure that the type of the symbol won't change */

            if ( sym->mem_type != mem_type ) {
                /* if the symbol is already defined (as SYM_INTERNAL), Masm
                 won't display an error. The other way, first externdef and
                 then the definition, will make Masm complain, however */
                DebugMsg(("ExterndefDirective: type conflict for %s. mem_types: %u - %u ; %u - %u\n", sym->name, sym->mem_type, mem_type));
                AsmWarn( 1, SYMBOL_TYPE_CONFLICT, sym->name );
            } else if ( sym->mem_type == MT_TYPE && sym->type != symtype ) {
                asm_sym *sym2 = sym;
                /* skip alias types and compare the base types */
                DebugMsg(("ExterndefDirective(%s): types differ: %X (%s) - %X (%s)\n", sym->name, sym->type, sym->type->name, symtype, symtype->name));
                while ( sym2->type )
                    sym2 = sym2->type;
                while ( symtype->type )
                    symtype = symtype->type;
                if ( sym2 != symtype ) {
                    DebugMsg(("ExterndefDirective(%s): type conflict: %X (%s) - %X (%s)\n", sym->name, sym2, sym2->name, symtype, symtype->name));
                    AsmWarn( 1, SYMBOL_TYPE_CONFLICT, sym->name );
                }
            }

            /* v2.04: emit a - weak - warning if language differs.
             * Masm doesn't warn.
             */
            if ( langtype != LANG_NONE && sym->langtype != langtype )
                AsmWarn( 3, SYMBOL_REDEFINITION, sym->name );
        }
        sym->isdefined = TRUE;

        /* write a global entry if none has been written yet */
        if ( sym->state == SYM_EXTERNAL && sym->weak == FALSE )
            ;/* skip EXTERNDEF if a real EXTERN/COMM was done */
        else if ( sym->global == FALSE ) {
            sym->global = TRUE;
            DebugMsg1(("ExterndefDirective(%s): writing a global entry\n", sym->name));
            QAddItem( &ModuleInfo.g.GlobalQueue, sym );
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

/* helper for EXTERN directive.
 * also used to create 16-bit floating-point fixups.
 * sym must be NULL or of state SYM_UNDEFINED!
 */

asm_sym *MakeExtern( const char *name, memtype mem_type, struct asm_sym * vartype, asm_sym * sym, uint_8 Ofssize )
/****************************************************************************************************************/
{
    sym = CreateExternal( sym, name, FALSE );
    if ( sym == NULL )
        return( NULL );

    if ( mem_type == MT_ABS )
        ;
    else if ( Options.masm_compat_gencode == FALSE || mem_type != MT_FAR )
        sym->segment = &CurrSeg->sym;

    sym->isdefined = TRUE;
    sym->mem_type = mem_type;
    if ( mem_type != MT_TYPE ) {
        int size = SizeFromMemtype( mem_type, Ofssize, vartype );
        sym->Ofssize = Ofssize;
        if ( size != 0 )
            sym->total_size = size;
    } else
        sym->total_size = vartype->total_size;
    sym->type = vartype;
    return( sym );
}

/* handle optional alternate names in EXTERN directive
 */

static ret_code HandleAltname( char *altname, struct asm_sym *sym )
/*****************************************************************/
{
    struct asm_sym *symalt;

    if ( altname && sym->state == SYM_EXTERNAL ) {

        symalt = SymSearch( altname );

        /* altname symbol changed? */
        if ( sym->altname && sym->altname != symalt ) {
            AsmErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }

        if ( Parse_Pass > PASS_1 ) {
            if ( symalt->state == SYM_UNDEFINED ) {
                AsmErr( SYMBOL_NOT_DEFINED, altname );
            } else if (symalt->state != SYM_INTERNAL && symalt->state != SYM_EXTERNAL ) {
                AsmErr( SYMBOL_TYPE_CONFLICT, altname );
            } else {
#if COFF_SUPPORT || ELF_SUPPORT
                if ( symalt->state == SYM_INTERNAL && symalt->public == FALSE )
                    if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
                        || Options.output_format == OFORMAT_ELF
#endif
                       ) {
                        AsmErr( MUST_BE_PUBLIC_OR_EXTERNAL, altname );
                    }
#endif
                if ( sym->mem_type != symalt->mem_type )
                    AsmErr( SYMBOL_TYPE_CONFLICT, altname );
            }
        } else {

            if ( symalt ) {
                DebugMsg(("HandleAltname: %s found\n", altname ));
                if ( symalt->state != SYM_INTERNAL &&
                    symalt->state != SYM_EXTERNAL &&
                    symalt->state != SYM_UNDEFINED ) {
                    AsmErr( SYMBOL_TYPE_CONFLICT, altname );
                    return( ERROR );
                }
            } else {
                symalt = SymCreate( altname, TRUE );
                dir_add_table( &Tables[TAB_UNDEF], (dir_node *)symalt );
            }
            /* make sure the alt symbol becomes strong if it is an external */
            symalt->used = TRUE;
            /* symbol inserted in the "weak external" queue?
             * currently needed for OMF only.
             */
            if ( sym->altname == NULL ) {
                sym->altname = symalt;
                ((dir_node *)sym)->nextext = NULL;
                if ( ModuleInfo.g.AltQueue.head == NULL )
                    ModuleInfo.g.AltQueue.head = ModuleInfo.g.AltQueue.tail = (dir_node *)sym;
                else {
                    ModuleInfo.g.AltQueue.tail->nextext = (dir_node *)sym;
                    ModuleInfo.g.AltQueue.tail = (dir_node *)sym;
                }
            }
        }
    }
    return( NOT_ERROR );
}

/* syntax: EXT[E]RN [lang_type] name (altname) :type [, ...] */

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

    DebugMsg1(("ExternDirective(%u) enter\n", i));
    i++; /* skip EXT[E]RN token */
#if MANGLERSUPP
    mangle_type = Check4Mangler( &i );
#endif
    do {

        symtype = NULL;
        mem_type = MT_EMPTY;
        Ofssize = ModuleInfo.Ofssize;
        altname = NULL;

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
        }

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;
        sym = SymSearch( token );

        typetoken = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token == T_ID ) {
            if ( 0 == _stricmp( AsmBuffer[i]->string_ptr, "ABS" ) ) {
                mem_type = MT_ABS;
            } else if ( symtype = SymIsType( typetoken ) ) {
                mem_type = MT_TYPE;
            }
        } else if ( AsmBuffer[i]->token == T_DIRECTIVE ) {
            if ( AsmBuffer[i]->value == T_PROTO ) {
                /* dont scan this line further */
                /* CreateProto() will define a SYM_EXTERNAL */
                sym = CreateProto( i + 1, token );
                DebugMsg(("ExternDirective(%s): CreateProto()=%X\n", token, sym));
                if ( sym == NULL )
                    return( ERROR );
                else if ( sym->state == SYM_EXTERNAL ) {
                    sym->weak = FALSE;
                    HandleAltname( altname, sym );
                    return( NOT_ERROR );
                } else {
                    AsmErr( SYMBOL_REDEFINITION, sym->name );
                    return( ERROR );
                }
            } else if ( AsmBuffer[i]->value == T_PROC ) {
                mem_type = SimpleType[ST_PROC].mem_type;
            }
        } else if ( AsmBuffer[i]->token == T_RES_ID ) {
            if ( AsmBuffer[i]->type == RWT_TYPE ) {
                mem_type = SimpleType[AsmBuffer[i]->value8].mem_type;
                if ( SimpleType[AsmBuffer[i]->value8].Ofssize != USE_EMPTY )
                    Ofssize = SimpleType[AsmBuffer[i]->value8].Ofssize;
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
        } else if ( mem_type == MT_PTR && AsmBuffer[i+1]->token != T_FINAL ) {
            /* if it is a pointer to something (not VOID),
             an anonymous TYPEDEF has to be created
             */
            if ( sym && sym->mem_type == MT_TYPE ) {
                symtype = sym->type;
                for( ; i < Token_Count && AsmBuffer[i]->token != T_COMMA; i++ );
            } else
                if ( ( symtype = CreateTypeDef( NULL, &i ) ) == NULL )
                    return( ERROR );
            DebugMsg(("ExternDirective(%s): CreateTypeDef()=%X\n", token, symtype));
            mem_type = MT_TYPE;
        } else {
            i++;
        }
        DebugMsg1(("ExternDirective(%s): mem_type=%Xh\n", token, mem_type ));

        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            /* v2.04: emit the error at the PUBLIC directive */
            //if ( sym && sym->public == TRUE ) {
            //    AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
            //    return( ERROR );
            //}
            if(( sym = MakeExtern( token, mem_type, symtype, sym, Ofssize )) == NULL )
                return( ERROR );

        } else {
            if ( sym->state != SYM_EXTERNAL ) {
                DebugMsg(("ExternDirective: symbol %s redefinition\n", token ));
                AsmErr( SYMBOL_REDEFINITION, token );
                return( ERROR );
            }
            if( sym->mem_type != mem_type ||
                ( langtype != LANG_NONE && sym->langtype != LANG_NONE && sym->langtype != langtype )) {
                AsmErr( SYMBOL_TYPE_CONFLICT, token );
                return( ERROR );
            }
        }

        sym->isdefined = TRUE;
        HandleAltname( altname, sym );

        SetMangler( sym, mangle_type, langtype );

        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA &&  ( (i + 1) < Token_Count ) ) {
                i++;
            } else {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
    }  while ( i < Token_Count );

    return( NOT_ERROR );
}

/* helper for COMM directive */

static asm_sym *MakeComm( char *name, asm_sym *sym, int size, int count, bool isfar )
/***********************************************************************************/
{
    sym = CreateComm( sym, name );
    if( sym == NULL )
        return( NULL );

    sym->total_length = count;
    sym->isfar = isfar;

    /* v2.04: don't set segment if communal is far and -Zg is set */
    if ( Options.masm_compat_gencode == FALSE || isfar == FALSE )
        sym->segment = &CurrSeg->sym;

    MemtypeFromSize( size, &sym->mem_type );

    /* v2.04: warning added ( Masm emits an error ) */
    if ( ( count * size ) > 0x10000UL )
        AsmWarn( 2, COMM_VAR_EXCEEDS_64K, sym->name );

    sym->total_size = count * size;

    return( sym );
}

/* define "communal" items
 * syntax:
 * COMM [langtype] [NEAR|FAR] label:type[:count] [, ... ]
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

    i++; /* skip COMM token */
    for( ; i < Token_Count; i++ ) {
#if MANGLERSUPP
        mangle_type = Check4Mangler( &i );
#endif
        /* get the symbol language type if present */
        langtype = ModuleInfo.langtype;
        GetLangType( &i, &langtype );

        /* get the distance ( near or far ) */
        isfar = FALSE;
        if ( AsmBuffer[i]->token == T_RES_ID )
            switch ( AsmBuffer[i]->value ) {
            case T_FAR:
            case T_FAR16:
            case T_FAR32:
                if ( ModuleInfo.model == MOD_FLAT ) {
                    AsmError( FAR_NOT_ALLOWED_IN_FLAT_MODEL_COMM_VARIABLES );
                } else
                    isfar = TRUE;
                /* no break */
            case T_NEAR:
            case T_NEAR16:
            case T_NEAR32:
                i++;
            }

        /* get the symbol name */
        token = AsmBuffer[i++]->string_ptr;

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        i++;
        /* the evaluator cannot handle a ':' so scan for one first */
        for ( tmp = i; tmp < Token_Count;tmp++ )
            if ( AsmBuffer[tmp]->token == T_COLON )
                break;
        if ( EvalOperand( &i, tmp, &opndx, TRUE ) == ERROR )
            return( ERROR );
        /* v2.03: syntax COMM varname:<string>:<string> is accepted by Masm */
        //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
        if ( opndx.kind != EXPR_CONST ) {
            AsmError( CONSTANT_EXPECTED );
            opndx.value = 1;
        }
        if ( opndx.value == 0 ) {
            AsmError( POSITIVE_VALUE_EXPECTED );
            opndx.value = 1;
        }
        size = opndx.value;

        count = 1;
        if( AsmBuffer[i]->token == T_COLON ) {
            i++;
            /* get optional count argument */
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                return( ERROR );
            /* v2.03: a string constant is acceptable! */
            //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
            if ( opndx.kind != EXPR_CONST ) {
                AsmError( CONSTANT_EXPECTED );
                opndx.value = 1;
            }
            if ( opndx.value == 0 ) {
                AsmError( POSITIVE_VALUE_EXPECTED );
                opndx.value = 1;
            }
            count = opndx.value;
        }

        sym = SymSearch( token );
        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = MakeComm( token, sym, size, count, isfar );
            if ( sym == NULL )
                return( ERROR );
        } else if ( sym->state != SYM_EXTERNAL || sym->comm != TRUE ) {
            AsmErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        } else {
            tmp = sym->total_size / sym->total_length;
            if( count != sym->total_length || size != tmp ) {
                AsmErr( NON_BENIGN_XXX_REDEFINITION, szCOMM, sym->name );
                return( ERROR );
            }
        }
        sym->isdefined = TRUE;
        SetMangler( sym, mangle_type, langtype );

        if ( AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}

void AddPublicData( asm_sym *sym )
/*********************************/
{
    DebugMsg1(("AddPublicData(%s)\n", sym->name ));
    QAddItem( &ModuleInfo.g.PubQueue, sym );
}

/* get (next) PUBLIC item */

asm_sym * GetPublicData( void * *vp )
/***********************************/
{
    qnode * *curr = (qnode * *)vp;

    if ( ModuleInfo.g.PubQueue.head == NULL )
        return( NULL );

    if (*curr == NULL)
        *curr = ModuleInfo.g.PubQueue.head;
    else
        *curr = (*curr)->next;

    for ( ; *curr ; *curr = (*curr)->next ) {
        asm_sym *sym = (*curr)->elmt;
        if ( sym->state == SYM_INTERNAL )
            return ( sym );
#if 0 //FASTPASS  /* v2.04: this is now done in PassOneCheck() */
        /* skip anything not EXTERNAL, also COMMs and EXTERNs */
        if ( sym->state != SYM_EXTERNAL || sym->weak == FALSE ) {
            /* v1.95: make a full second pass and
             * emit the error msg later, on PUBLIC directive
             * v2.04: this check works for OMF format only!
             */
            //AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
            SkipSavedState();
        }
#endif
    }
    return( NULL );
}

void FreePubQueue( void )
/***********************/
{
    qnode *curr;
    qnode *next;
    for( curr = ModuleInfo.g.PubQueue.head; curr; curr = next ) {
        next = curr->next;
        AsmFree( curr );
    }
    ModuleInfo.g.PubQueue.head = NULL;
}

/* syntax: PUBLIC [lang_type] name [, ...] */

ret_code PublicDirective( int i )
/*******************************/
{
    char                *mangle_type = NULL;
    char                *token;
    struct asm_sym      *sym;
    //dir_node            *dir;
    char                skipitem;
    lang_type           langtype;

    i++; /* skip PUBLIC directive */
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

        DebugMsg1(("PublicDirective(%s)\n", token ));

        /* Add the public name */
        sym = SymSearch( token );
        if ( Parse_Pass == PASS_1 ) {
            if ( sym == NULL ) {
                if ( sym = SymCreate( token, TRUE ) ) {
                    dir_add_table( &Tables[TAB_UNDEF], (dir_node *)sym );
                    DebugMsg1(("PublicDirective(%s): new symbol\n", sym->name ));
                } else
                    return( ERROR ); /* name was too long */
            }
            skipitem = FALSE;
        } else {
            if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
                AsmErr( SYMBOL_NOT_DEFINED, token );
                //return( ERROR ); /* v2.04: dont exit */
            }
        }
        if ( sym ) {
            switch ( sym->state ) {
            case SYM_UNDEFINED:
                break;
            case SYM_INTERNAL:
                if ( sym->scoped == TRUE ) {
                    AsmErr( CANNOT_DECLARE_SCOPED_CODE_LABEL_AS_PUBLIC, sym->name );
                    skipitem = TRUE;
                    //return( ERROR );
                }
                break;
            case SYM_EXTERNAL:
                if ( sym->comm == TRUE ) {
                    AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
                    skipitem = TRUE;
                    //return( ERROR );
                } else if ( sym->weak == FALSE ) {
                    /* for EXTERNs, emit a different error msg */
                    AsmErr( SYMBOL_REDEFINITION, sym->name );
                    skipitem = TRUE;
                    //return( ERROR );
                }
                break;
            default:
                AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
                skipitem = TRUE;
                //return( ERROR );
            }
            if( Parse_Pass == PASS_1 && skipitem == FALSE ) {
                if ( sym->public == FALSE ) {
                    sym->public = TRUE;
                    AddPublicData( sym ); /* put it into the public table */
                }
                SetMangler( sym, mangle_type, langtype );
            }
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
