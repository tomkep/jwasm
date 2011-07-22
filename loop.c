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
* Description:  Implements FOR/IRP, FORC/IRPC, REPEAT/REPT, WHILE
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "input.h"
#include "equate.h"
#include "expreval.h"
#include "tokenize.h"
#include "macro.h"
#include "listing.h"
#include "reswords.h"

#define USELOCALMAC 1 /* 1=create the macro onto the stack */

ret_code LoopDirective( int i, struct asm_tok tokenarray[] )
/**********************************************************/
{
    int directive = tokenarray[i].tokval;
    int arg_loc;
    int len;
    //bool first = TRUE;
    char *parmstring;
    char *oldbufferend;
    char *ptr;
    struct dsym *macro;
    bool is_exitm;
    struct expr opndx;
#if USELOCALMAC
    struct macro_info macinfo;
    struct dsym tmpmacro;
#endif
#ifdef DEBUG_OUT
    uint_32 count = 0;
#endif
    char line[MAX_LINE_LEN]; /* used to store FOR/FORC argument */

    DebugMsg1(("LoopDirective(%s) enter\n", GetResWName( directive, NULL ) ));

    i++; /* skip directive */
    if ( ModuleInfo.list == TRUE )
        LstWrite( LSTTYPE_MACRO, 0, NULL );

    switch ( directive ) {
    case T_WHILE:
        arg_loc = i;
        /* v2.06: obsolete */
        /* the expression must be saved, since token buffer will be destroyed */
        //strcpy( line, tokenarray[i].tokpos );
        /* no break */
    case T_REPT:
    case T_REPEAT:
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) { /* syntax <REPEAT|WHILE 'A'> is valid! */
            /* the expression is "critical", that is, no forward
             * referenced symbols may be used here!
             */
            if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED )
                AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
            else
                AsmError( CONSTANT_EXPECTED );
            opndx.kind = EXPR_CONST;
            opndx.value = 0;
        }
        if( tokenarray[i].token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
            return( ERROR );
        }
        break;
    default: /* FOR, FORC, IRP, IRPC */
        /* get the formal parameter and the argument list */
        /* the format parameter will become a macro parameter, so it can
         * be a simple T_ID, but also an instruction or something else.
         * v2.02: And it can begin with a '.'!
         */
        if( tokenarray[i].token == T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i-1].tokpos );
            return( ERROR );
        }
        /* v2.02: allow parameter name to begin with a '.' */
        //c = *tokenarray[i].string_ptr;
        //if( ( is_valid_id_char(c) == FALSE ) || ( isdigit(c) == TRUE ) ) {
        if( is_valid_id_first_char( *tokenarray[i].string_ptr ) == FALSE ) {
            DebugMsg(( "LoopDirective(FOR/FORC): token %s is not a valid parameter name\n", tokenarray[i].string_ptr ));
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
            return( ERROR );
        }
        arg_loc = i;
        i++;

        if( directive == T_FORC || directive == T_IRPC ) {
            if( tokenarray[i].token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;
            /* FORC/IRPC accepts anything as "argument list", even nothing! */
            if( tokenarray[i].token == T_STRING && tokenarray[i].string_delim == '<' ) {
                parmstring = myalloca( strlen( tokenarray[i].string_ptr ) + 1 );
                strcpy( parmstring, tokenarray[i].string_ptr );
                /* v2.02: if there's additional stuff behind the <> literal,
                 * it's an error!
                 */
                if ( tokenarray[i+1].token != T_FINAL )
                    AsmErr( SYNTAX_ERROR_EX, tokenarray[i+1].string_ptr );
            } else {
                char *ptr2;
                ptr = tokenarray[i].tokpos;
                ptr2 = ptr;
                /* this is what Masm does: use the string until a space
                 * is detected. Anything beyond the space is ignored.
                 */
                while ( *ptr2 && ( isspace( *ptr2 ) == FALSE ) )
                    ptr2++;
                len = ptr2 - ptr;
                parmstring = myalloca( len + 1 );
                memcpy( parmstring, ptr, len );
                *(parmstring+len) = NULLC;
            }
        } else {
            /* for FOR/IRP, skip everything between the name and the comma!
             * these items will be stored as (first) macro parameter.
             * for example, valid syntax is:
             * FOR xxx,<a, ...>
             * FOR xxx:REQ,<a, ...>
             */
            while ( tokenarray[i].token != T_FINAL && tokenarray[i].token != T_COMMA )
                i++;
            if( tokenarray[i].token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;
            /* FOR/IRP accepts a literal enclosed in <> only */
            if( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
                AsmErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
                return( ERROR );
            }
            /* v2.03: also ensure that the literal is the last item */
            if( tokenarray[i+1].token != T_FINAL ) {
                AsmErr( SYNTAX_ERROR_EX, tokenarray[i+1].tokpos );
                return( ERROR );
            }
            /* v2.0: use GetLiteralValue() instead of strcpy!!! */
            //strcpy( line, tokenarray[i].string_ptr );
            GetLiteralValue( line, tokenarray[i].string_ptr );
            DebugMsg1(("LoopDirective(FOR): param string >%s<\n", line));
        }
        /* to run StoreMacro(), tokenarray must be setup correctly. */
        /* clear contents beginning with the comma! */
        i--;
        tokenarray[i].token = T_FINAL;
        Token_Count = i;
        i = arg_loc;
    }

    /* now make a temporary macro */
#if USELOCALMAC
    macro = &tmpmacro;
    memset( &tmpmacro, 0, sizeof(tmpmacro) );
    tmpmacro.sym.name = "";
    tmpmacro.e.macroinfo = &macinfo;
    memset( &macinfo, 0, sizeof(macinfo) );
#else
    macro = CreateMacro( "" );
#endif
    macro->e.macroinfo->srcfile = get_curr_srcfile();

    DebugMsg1(("LoopDirective(%s): calling StoreMacro\n", GetResWName( directive, NULL )));
    if( StoreMacro( i, tokenarray, macro, TRUE ) == ERROR ) {
#if USELOCALMAC
        ReleaseMacroData( macro );
#else
        SymFree( (struct asym *)macro );
#endif
        return( ERROR );
    }
    /* EXITM is allowed inside a macro loop.
     * This doesn't make the loop a macro function, reset the bit!
     */
    macro->sym.isfunc = FALSE;

    /* now run the just created macro in a loop */

    /* don't run the macro if there are no lines (macroinfo->data == NULL)!
     * this isn't exactly what Masm does; an empty 'WHILE 1'
     * will loop "forever" in Masm,
     */
    if ( macro->e.macroinfo->data ) /* added in v2.01 */
    switch ( directive ) {
    case T_REPEAT:
    case T_REPT:
        /* negative repeat counts are accepted and are treated like 0 */
        for ( ; macro->sym.value < opndx.value; macro->sym.value++ ) {
            RunMacro( macro, "", NULL, FALSE, &is_exitm );
            if ( is_exitm )
                break;
            DebugMsg1(("LoopDirective REPT: iteration=%" FU32 "\n", ++count ));
            //first = FALSE;
        }
        break;
    case T_WHILE:
        oldbufferend = StringBufferEnd;
        while ( opndx.kind == EXPR_CONST && opndx.value != 0 ) {
            DebugMsg1(("LoopDirective WHILE: cnt=%u\n", count++ ));
            RunMacro( macro, "", NULL, FALSE, &is_exitm );
            if ( is_exitm )
                break;
            i = arg_loc;
            /* v2.06: obsolete */
            /* Restore StringBufferEnd so it won't increase
             * with each iteration! */
            //StringBufferEnd = oldbufferend;
            //i = 1;
            //Token_Count = Tokenize( line, i, TRUE );
            if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
                break;
            macro->sym.value++;
        }
        break;
    case T_FORC:
    case T_IRPC:
        for( ptr = parmstring; *ptr; macro->sym.value++ ) {
            char *ptr2 = line;
            *ptr2++ = '<';
            /* v1.96: '"' and '\'' added, '!' removed */
            if (*ptr == '<' || *ptr == '>' || *ptr == '%' || *ptr == '"' || *ptr == '\'')
                *ptr2++ = '!';
            else if (*ptr == '!' ) { /* v1.96: handling of ! changed */
                *ptr2++ = *ptr++;
                if ( *ptr == NULLC )
                    ptr = "\t";  /* make sure there's something != NULLC */
            }
            *ptr2++ = *ptr++;
            *ptr2++ = '>';
            *ptr2 = NULLC;
            RunMacro( macro, line, NULL, FALSE, &is_exitm );
            if ( is_exitm )
                break;
            //first = FALSE;
            DebugMsg1(("LoopDirective FORC: call RunMacro(), cnt=%" FU32 ", param=>%s<\n", count++, line ));
        }
        break;
    default: /* T_FOR, T_IRP */
        /* a FOR/IRP parameter can be a macro function call */
        /* that's why the macro calls must be run synchronously */
        /* v2.05: reset an optional VARARG attribute for the macro
         * parameter */
        macro->sym.mac_vararg = FALSE;
        for( ptr = line; *ptr; macro->sym.value++ ) {
            DebugMsg1(("LoopDirective FOR: cnt=%" FU32 ", calling RunMacro( param=>%s<, prefix=NULL, runit=1, insert=1, addbrackets=0 )\n", count++, ptr ));
            len = RunMacro( macro, ptr, NULL, FALSE, &is_exitm );
            if ( len < 1 || is_exitm )
                break;
            ptr += len;
        }
    }
#if USELOCALMAC
    ReleaseMacroData( macro );
#else
    SymFree( (struct asym *)macro );
#endif
    DebugMsg1(("LoopDirective(%s) exit\n", GetResWName( directive, NULL ) ));
    return( NOT_ERROR );
}
