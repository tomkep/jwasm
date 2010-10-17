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
* Description:  JWasm conditional processing routines. Handles directives
*               [ELSE]IF[E], ELSE, ENDIF
*               [ELSE]IFB, [ELSE]IFNB
*               [ELSE]IFDEF, [ELSE]IFNDEF
*               [ELSE]IFDIF[I], [ELSE]IFIDN[I]
*               [ELSE]IF1, [ELSE]IF2
*               .ERR, .ERRNZ, .ERRE
*               .ERRB, .ERRNB
*               .ERRDEF, .ERRNDEF
*               .ERRDIF[I], .ERRIDN[I]
*               .ERR1, .ERR2
****************************************************************************/


#include <ctype.h>

#include "globals.h"
#include "parser.h"
#include "directiv.h"
#include "condasm.h"
#include "insthash.h"
#include "expreval.h"
#include "listing.h"
#include "input.h"
#include "macro.h"
#include "fastpass.h"

extern bool SkipMacroMode;

/*
 the current if-block can be in one of 3 states:
  state              assembly     possible next states
 -----------------------------------------------------------------------
  inactive           off          condition check, active
  active             on           done
  done               off          -
  -----------------------------------------------------------------------
  up to v2.04, there was a forth state:
  condition check    on           active, inactive
  it was necessary because conditional_assembly_prepare()
  may be called multiple times for the very same source line.
*/

if_state CurrIfState;
static int blocknestlevel;
static int falseblocknestlevel;

#ifdef DEBUG_OUT
static const char *GetCurrIfStatString( void )
/********************************************/
{
    const char *p;
    switch ( CurrIfState ) {
    case BLOCK_ACTIVE:
        p = "BLOCK_ACTIVE";
        break;
    case BLOCK_INACTIVE:
        p = "BLOCK_INACTIVE";
        break;
    default:
        p = "BLOCK_DONE";
        break;
    }
    return( p );
}
#endif

/*
 * this code runs after the first token has been scanned
 * and it is a IFx, ELSEx or ENDIF.
 * Updates CurrIfState:
 * - inactive -> condition check
 * - active -> done
 * and updates variable <falseblocknesting>.
*/

void conditional_assembly_prepare( int directive )
/************************************************/
{
    switch( directive ) {
    case T_IF:
    case T_IF1:
    case T_IF2:
    case T_IFB:
    case T_IFDEF:
    case T_IFDIF:
    case T_IFDIFI:
    case T_IFE:
    case T_IFIDN:
    case T_IFIDNI:
    case T_IFNB:
    case T_IFNDEF:
        /* this must be done HERE to avoid expansion if the current
         * state is INACTIVE.
         */
        if( CurrIfState != BLOCK_ACTIVE ) {
            falseblocknestlevel++;
            DebugMsg1(("condasm_prepare(%s): state=%s, level=%u, falselevel=%u\n", GetResWName( directive, NULL), GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
            return;
        }
        DebugMsg1(("condasm_prepare(%s): state=%s, level=%u, falselevel=%u\n", GetResWName( directive, NULL), GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
        break;
    case T_ELSE:
    case T_ELSEIF:
    case T_ELSEIF1:
    case T_ELSEIF2:
    case T_ELSEIFB:
    case T_ELSEIFDEF:
    case T_ELSEIFDIF:
    case T_ELSEIFDIFI:
    case T_ELSEIFE:
    case T_ELSEIFIDN:
    case T_ELSEIFIDNI:
    case T_ELSEIFNB:
    case T_ELSEIFNDEF:
        if ( blocknestlevel ) { /* v2.04: do nothing if there was no IFx */
            if ( falseblocknestlevel > 0 ) {
                DebugMsg1(("condasm_prepare(%s): state=%s, level=%u, falselevel=%u\n", GetResWName( directive, NULL), GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
                return;
            }
            /* status may change from inactive to active,
             * or active to done
             */
            CurrIfState = (( CurrIfState == BLOCK_INACTIVE ) ? BLOCK_ACTIVE : BLOCK_DONE );
        }
        DebugMsg1(("condasm_prepare(%s): state=%s, level=%u, falselevel=%u\n", GetResWName( directive, NULL), GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
        break;
    case T_ENDIF:
        if ( falseblocknestlevel > 0 ) {
            falseblocknestlevel--;
            DebugMsg1(("condasm_prepare(%s): state=%s, level=%u, falselevel=%u\n", GetResWName( directive, NULL), GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
            return;
        }
        CurrIfState = BLOCK_ACTIVE; /* v2.04: added */
        DebugMsg1(("condasm_prepare(%s): state=%s, level=%u, falselevel=%u\n", GetResWName( directive, NULL), GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
        break;
    }
    return;
}

/* handle [ELSE]IF[N]DEF
 * <string> is
 * - the value of a T_ID item!
 * - "" (item is T_FINAL)
 */

static bool check_defd( const char *string )
/******************************************/
{
    struct asm_sym      *sym;
#if 0 /* v2.02: removed */
    char                *ptr;
    char                *end;

    /* isolate 1st word */
    ptr = string + strspn( string, " \t" );
    end = ptr + strcspn( ptr, " \t" );
    *end = NULLC;

    /* there might be no argument at all, which is valid syntax
      and should return FALSE */
    if ( *ptr == NULLC )
        return( FALSE );
    sym = SymSearch( ptr );
#else
    if ( *string == NULLC )
        return( FALSE );
    sym = SymSearch( string );
#endif

    if( sym != NULL ) {
        DebugMsg1(("check_defd(%s): state=%u defined=%u\n", string, sym->state, sym->isdefined ));
        /* v2.04: changed. the "defined" flag is active for ALL symbols */
        //if ( sym->state == SYM_INTERNAL || sym->state == SYM_MACRO || sym->state == SYM_TMACRO || sym->state == SYM_UNDEFINED ) {
        return( sym->isdefined );
        //}
        //return( TRUE );
    }
    DebugMsg1(("check_defd(%s): sym=NULL\n", string ));
    return( FALSE );
}

/* handle [ELSE]IF[N]B
 */
static bool check_blank( const char *string )
/*******************************************/
{
    for ( ;*string; string++ )
        if ( isspace(*string) == FALSE )
            return FALSE;
    return( TRUE );
}

/* Are two strings different?
 * Used by [ELSE]IFDIF[I] and [ELSE]IFIDN[I]
 */

static bool check_dif( const char *string1, const char *string2, bool sensitive )
/*******************************************************************************/
{
#if 1
    /* v2.02: transform to the "visible" format first */
    char s1[MAX_LINE_LEN];
    char s2[MAX_LINE_LEN];

    GetLiteralValue( s1, string1 );
    GetLiteralValue( s2, string2 );
    if( sensitive ) {
        return( strcmp( s1, s2 ) != 0 );
    } else {
        return( _stricmp( s1, s2 ) != 0 );
    }
#else
    if( sensitive ) {
        return( strcmp( string1, string2 ) != 0 );
    } else {
        return( _stricmp( string1, string2 ) != 0 );
    }
#endif
}

ret_code conditional_assembly_directive( int i, int directive )
/*************************************************************/
{
    char *string1;
    char *string2;
    if_state NextIfState;
    expr_list opndx;

    if ( CurrIfState != BLOCK_ACTIVE ) {
        DebugMsg1(("conditional_assembly_directive(%s), CurrIfState=%u(%s), level=%u, falselevel=%u\n",
                   GetResWName(directive, NULL), CurrIfState, GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
        if ( i || ModuleInfo.listif ) {
            LstWriteSrcLine();
        }
        return( NOT_ERROR );
    }

    if ( ModuleInfo.list == TRUE ) {
        if ( MacroLevel == 0 ||
            ModuleInfo.list_macro == LM_LISTMACROALL ||
            ModuleInfo.listif )
            LstWriteSrcLine();
    }

    DebugMsg1(("conditional_assembly_directive(%s), BLOCK_ACTIVE, level=%u, falselevel=%u\n", GetResWName(directive, NULL), blocknestlevel, falseblocknestlevel));
    switch (directive) {
    case T_IF:
    case T_IF1:
    case T_IF2:
    case T_IFB:
    case T_IFDEF:
    case T_IFDIF:
    case T_IFDIFI:
    case T_IFE:
    case T_IFIDN:
    case T_IFIDNI:
    case T_IFNB:
    case T_IFNDEF:
        if( blocknestlevel == MAX_IF_NESTING ) {
            DebugMsg(("conditional_assembly_directive(%s): nesting limit reached, level=%u\n", GetResWName(directive, NULL), blocknestlevel ));
            AsmError( NESTING_LEVEL_TOO_DEEP );
            return( ERROR );
        }
        blocknestlevel++;
        if ( SkipMacroMode == TRUE ) {
            /* avoid expression evaluation if in "skipmacro" mode */
            CurrIfState = BLOCK_DONE;
            DebugMsg1(("conditional_assembly_directive(%s): skip mode, block done\n", GetResWName(directive, NULL) ));
            return( NOT_ERROR );
        }
        break;
    case T_ENDIF:
        if ( blocknestlevel == 0 ) {
            DebugMsg(("conditional_assembly_directive(%s): nesting error, level=%u\n", GetResWName(directive, NULL), blocknestlevel ));
            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        blocknestlevel--;
        break;
    default: /* ELSE[IFxx] */
        if ( blocknestlevel == 0 ) {
            DebugMsg(("conditional_assembly_directive(%s): nesting error, level=%u\n", GetResWName(directive, NULL), blocknestlevel ));
            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        break;
    }

    i++; /* go past IFx, ELSEx, ENDIF */

    /* check params and call appropriate test routine */

    switch( directive ) {
    case T_IF:
    case T_IFE:
    case T_ELSEIF:
    case T_ELSEIFE:
        if ( ( ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ) ) )
            return( ERROR );

        /* no forward reference allowed, symbol must be defined */
        if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED ) {
            AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
        } else if ( opndx.kind == EXPR_CONST )
            ;
        else if ( opndx.kind == EXPR_ADDR && opndx.indirect == FALSE )
            opndx.value += opndx.sym->offset;
        else {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( directive == T_IF || directive == T_ELSEIF )
            NextIfState = ( opndx.value ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        else
            NextIfState = ( !opndx.value ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFDIF:
    case T_IFDIFI:
    case T_ELSEIFDIF:
    case T_ELSEIFDIFI:
    case T_IFIDN:
    case T_IFIDNI:
    case T_ELSEIFIDN:
    case T_ELSEIFIDNI:
        string1 = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
            if ( AsmBuffer[i]->token == T_ID && SymSearch( string1 ) == NULL )
                AsmErr( SYMBOL_NOT_DEFINED, string1 );
            else
                AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        i++;
        if ( AsmBuffer[i]->token != T_COMMA ) {
            AsmErr( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
        string2 = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
            if ( AsmBuffer[i]->token == T_ID && SymSearch( string2 ) == NULL )
                AsmErr( SYMBOL_NOT_DEFINED, string2 );
            else
                AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        i++;
        DebugMsg1(("conditional_assembly_directive(%s), cmp >%s< and >%s<\n", GetResWName(directive, NULL), string1, string2 ));
        switch ( directive ) {
        case T_IFDIF:
        case T_ELSEIFDIF:
            NextIfState = check_dif( string1, string2, TRUE ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
            break;
        case T_IFDIFI:
        case T_ELSEIFDIFI:
            NextIfState = check_dif( string1, string2, FALSE ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
            break;
        case T_IFIDN:
        case T_ELSEIFIDN:
            NextIfState = !check_dif( string1, string2, TRUE ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
            break;
        default:
            NextIfState = !check_dif( string1, string2, FALSE ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        }
        break;
    case T_IFB:
    case T_IFNB:
    case T_ELSEIFB:
    case T_ELSEIFNB:
        string1 = AsmBuffer[i]->string_ptr;

        if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
            if ( AsmBuffer[i]->token == T_ID && SymSearch( string1 ) == NULL )
                AsmErr( SYMBOL_NOT_DEFINED, string1 );
            else
                AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        i++;
        if ( directive == T_IFB || directive == T_ELSEIFB ) {
            NextIfState = check_blank( string1 ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        } else {
            NextIfState = !check_blank( string1 ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        }
        break;
    case T_IF1:
    case T_ELSEIF1:
        /* v2.04: changed */
        //NextIfState = ((Parse_Pass == PASS_1) ? BLOCK_ACTIVE : BLOCK_INACTIVE);
        NextIfState = BLOCK_ACTIVE;
        break;
    case T_IF2:
    case T_ELSEIF2:
        if ( ModuleInfo.setif2 == FALSE ) {
            AsmError( IF2_NOT_ALLOWED );
            break;
        }
        /* v2.04: changed */
        //NextIfState = ((Parse_Pass == PASS_1) ? BLOCK_INACTIVE : BLOCK_ACTIVE);
        NextIfState = BLOCK_ACTIVE;
        break;
    case T_IFDEF:
    case T_ELSEIFDEF:
    case T_IFNDEF:
    case T_ELSEIFNDEF:
        NextIfState = BLOCK_INACTIVE;
        /* Masm's implementation works with IDs as arguments only. The rest
         * will return FALSE. However, it's nice to be able to check whether
         * a reserved word is defined or not.
         */
        /* v2.0: [ELSE]IF[N]DEF is valid *without* an argument! */
        //if ( AsmBuffer[i]->token == T_ID && AsmBuffer[i+1]->token == T_FINAL) {
        if ( ( AsmBuffer[i]->token == T_ID && AsmBuffer[i+1]->token == T_FINAL ) ||
            AsmBuffer[i]->token == T_FINAL ) {
            NextIfState = ( check_defd( AsmBuffer[i]->string_ptr )  ? BLOCK_ACTIVE : BLOCK_INACTIVE );
            if ( AsmBuffer[i]->token != T_FINAL )
                i++;
        } else if ( Options.strict_masm_compat == FALSE && (
                    AsmBuffer[i]->token == T_RES_ID ||
                    AsmBuffer[i]->token == T_INSTRUCTION ||
                    AsmBuffer[i]->token == T_DIRECTIVE ||
                    //AsmBuffer[i]->token == T_UNARY_OP ||
                    //AsmBuffer[i]->token == T_BINARY_OP ||
                    AsmBuffer[i]->token == T_REG ) &&
                   AsmBuffer[i+1]->token == T_FINAL ) {
            NextIfState = BLOCK_ACTIVE;
            i++;
        } else {
            AsmWarn( 2, IFDEF_EXPECTS_SYMBOL_ARGUMENT, AsmBuffer[i-1]->pos );
            while ( AsmBuffer[i]->token != T_FINAL ) i++;
        }
        if ( directive == T_IFNDEF || directive == T_ELSEIFNDEF )
            NextIfState = ( ( NextIfState == BLOCK_ACTIVE ) ? BLOCK_INACTIVE : BLOCK_ACTIVE );
        break;
    default: /* ELSE and ENDIF */
        NextIfState = BLOCK_ACTIVE;
        break;
    }

    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    CurrIfState = NextIfState;

    DebugMsg1(("conditional_assembly_directive(%s) exit, state=%s, level=%u, falselevel=%u\n",
               GetResWName(directive, NULL), GetCurrIfStatString(), blocknestlevel, falseblocknestlevel));
    return( NOT_ERROR );
}

static void DispError( int msg, int idx )
/***************************************/
{
    char s1[MAX_LINE_LEN];

    s1[0] = NULLC;
    if ( idx != EMPTY )
        GetLiteralValue( s1, AsmBuffer[idx]->string_ptr );
    AsmErr( msg, s1 );
    return;

}

ret_code conditional_error_directive( int i )
/*******************************************/
{
    expr_list opndx;
    uint direct;
    char *string1;
    char *string2;
    bool forced_error = FALSE;
    int errtxt = EMPTY;

    direct = AsmBuffer[i]->value;

    i++; /* go past directive */

    /* get an expression if necessary */
    switch( direct ) {
    case T_DOT_ERR2:
        if (ModuleInfo.setif2 == FALSE) {
            AsmError( IF2_NOT_ALLOWED );
            return( ERROR );
        }
    case T_DOT_ERR1:
    case T_DOT_ERR:
        if ( ModuleInfo.setif2 == TRUE ) {
            if ( direct == T_DOT_ERR1 && Parse_Pass != PASS_1 )
                return( NOT_ERROR );
            else if ( direct == T_DOT_ERR2 && Parse_Pass == PASS_1 )
                return( NOT_ERROR );
        }
        if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
            errtxt = i;
            i++;
        }
        if ( AsmBuffer[i]->token != T_FINAL )
            break;
        if ( errtxt != EMPTY )
            DispError( FORCED_ARBITRARY, errtxt );
        else
            AsmError( FORCED_ERR );
        break;
    case T_DOT_ERRE:
    case T_DOT_ERRNZ:
        if (( ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ) ))
            return( ERROR );
        if ( opndx.kind == EXPR_CONST )
            ;
        else if ( opndx.kind == EXPR_ADDR && opndx.indirect == FALSE )
            opndx.value += opndx.sym->offset;
        else {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
                errtxt = i;
                i++;
            } else {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
        }
        if ( AsmBuffer[i]->token != T_FINAL )
            break;
        if ( direct == T_DOT_ERRNZ && opndx.value )
            forced_error = TRUE;
        else if ( direct == T_DOT_ERRE && !opndx.value )
            forced_error = TRUE;

        if ( forced_error == TRUE )
            if ( errtxt != EMPTY )
                DispError( FORCED_ARBITRARY, i );
            else if (direct == T_DOT_ERRNZ)
                AsmErr( FORCED_EQUAL, opndx.value );
            else
                AsmErr( FORCED_NOT_ZERO, opndx.value );
        break;
    case T_DOT_ERRDEF:
    case T_DOT_ERRNDEF:

        if ( AsmBuffer[i]->token == T_ID ) {
            asm_sym * sym;
            char * p1 = AsmBuffer[i]->string_ptr;
            i++;
            if ( AsmBuffer[i]->token == T_COMMA ) {
                i++;
                if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
                    errtxt = i; /* errtxt is ignored */
                    i++;
                } else {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                    return( ERROR );
                }
            }

            if ( AsmBuffer[i]->token != T_FINAL )
                break;

            /* should run on pass 2 only! */
            if ( Parse_Pass == PASS_1 )
                break;

            /* don't use check_defd()! */
            sym = SymSearch( p1 );
            if (sym && sym->state == SYM_UNDEFINED)
                sym = NULL;

            if( direct == T_DOT_ERRDEF && sym != NULL )
                AsmErr( FORCED_DEF, p1 );
            else if( direct == T_DOT_ERRNDEF && sym == NULL )
                AsmErr( FORCED_NOT_DEF, p1 );
        } else {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRB:
    case T_DOT_ERRNB:
        string1 = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
            if ( AsmBuffer[i]->token == T_ID && SymSearch( string1 ) == NULL )
                AsmErr( SYMBOL_NOT_DEFINED, string1 );
            else
                AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        i++;
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
                errtxt = i;
                i++;
            } else {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
        }
        if ( AsmBuffer[i]->token == T_FINAL ) {
            if ( direct == T_DOT_ERRB && check_blank( string1 ) )
                DispError( FORCED_BLANK, errtxt );
            else if ( direct == T_DOT_ERRNB && !check_blank( string1 ) )
                DispError( FORCED_NOT_BLANK, errtxt );
        }
        break;
    case T_DOT_ERRDIF:
    case T_DOT_ERRDIFI:
    case T_DOT_ERRIDN:
    case T_DOT_ERRIDNI:
        string1 = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
            if ( AsmBuffer[i]->token == T_ID && SymSearch( string1 ) == NULL )
                AsmErr( SYMBOL_NOT_DEFINED, string1 );
            else
                AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        i++;
        if ( AsmBuffer[i]->token != T_COMMA ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
        string2 = AsmBuffer[i]->string_ptr;
        if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
            if ( AsmBuffer[i]->token == T_ID && SymSearch( string2 ) == NULL )
                AsmErr( SYMBOL_NOT_DEFINED, string2 );
            else
                AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        i++;
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
                errtxt = i;
                i++;
            }
        }
        if ( AsmBuffer[i]->token != T_FINAL )
            break;

        switch ( direct ) {
        case T_DOT_ERRDIF:
            forced_error = check_dif( string1, string2, TRUE );
            break;
        case T_DOT_ERRDIFI:
            forced_error = check_dif( string1, string2, FALSE );
            break;
        case T_DOT_ERRIDN:
            forced_error = !check_dif( string1, string2, TRUE );
            break;
        default:
            forced_error = !check_dif( string1, string2, FALSE );
        }
        if ( forced_error == TRUE ) {
            if ( errtxt == EMPTY ) {
                if ( direct == T_DOT_ERRDIF || direct == T_DOT_ERRDIFI )
                    AsmErr( FORCED_DIF, string1, string2 );
                else
                    AsmErr( FORCED_IDN, string1, string2 );
            } else
                DispError( FORCED_ARBITRARY, errtxt );
        }
        break;
    }
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

void CondCheckOpen( void )
/************************/
{
    if( blocknestlevel > 0 ) {
        AsmErr( BLOCK_NESTING_ERROR, "if-else" );
    }
    return;
}

/* init (called once per module) */

void CondInit( void )
/*******************/
{
    CurrIfState = BLOCK_ACTIVE;
    blocknestlevel = 0;
    falseblocknestlevel = 0;
}
