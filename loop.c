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
#include "directiv.h"
#include "input.h"
#include "equate.h"
#include "expreval.h"
#include "tokenize.h"
#include "labels.h"
#include "macro.h"
#include "listing.h"

#define USELOCALMAC 1 /* 1=create the macro onto the stack */

ret_code LoopDirective( int i, int directive )
/********************************************/
{
    int start = i - 1; /* location of "directive name .. after any labels" */
    int arg_loc;
    int len;
    //bool first = TRUE;
    //char c;
    char *parmstring;
    char *oldbufferend;
    char *ptr;
    dir_node * macro;
    expr_list opndx;
#if USELOCALMAC
    macro_info macinfo;
    dir_node tmpmacro;
#endif
#ifdef DEBUG_OUT
    uint_32 count = 0;
#endif
    char line[MAX_LINE_LEN];

    DebugMsg(("LoopDirective(%u, %u) enter\n", i, directive));

    if ( ModuleInfo.list == TRUE )
        LstWrite( LSTTYPE_MACRO, 0, NULL );

    switch ( directive ) {
    case T_REPT:
    case T_REPEAT:
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) { /* syntax <REPEAT 'A'> is valid! */
            AsmError( CONSTANT_EXPECTED );
            opndx.value = 0;
        }
        if( AsmBuffer[i]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        break;
    case T_WHILE:
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) { /* syntax <WHILE 'A'> is valid! */
            AsmError( CONSTANT_EXPECTED );
            opndx.kind = EXPR_CONST;
            opndx.value = 0;
        }
        if( AsmBuffer[i]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        /* the expression must be saved, since AsmBuffer will be destroyed */
        ptr = AsmBuffer[start]->pos + 5;  /* 5 = strlen("WHILE") */
        while ( isspace( *ptr ) ) ptr++;
        strcpy( line, ptr );
        break;
    default: /* FOR, FORC, IRP, IRPC */
        /* get the formal parameter and the argument list */
        /* the format parameter will become a macro parameter, so it can
         * be a simple T_ID, but also an instruction or something else.
         * v2.02: And it can begin with a '.'!
         */
        if( AsmBuffer[i]->token == T_FINAL) {
            AsmError( OPERAND_EXPECTED );
            return( ERROR );
        }
        /* v2.02: allow parameter name to begin with a '.' */
        //c = *AsmBuffer[i]->string_ptr;
        //if( ( is_valid_id_char(c) == FALSE ) || ( isdigit(c) == TRUE ) ) {
        if( is_valid_id_first_char( *AsmBuffer[i]->string_ptr ) == FALSE ) {
            DebugMsg(( "LoopDirective(FOR/FORC): token %s is not a valid parameter name\n", AsmBuffer[i]->string_ptr ));
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        arg_loc = i;
        i++;

        if( directive == T_FORC || directive == T_IRPC ) {
            if( AsmBuffer[i]->token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;
            /* FORC accepts anything as "argument list", even nothing! */
            if( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
                parmstring = AsmTmpAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1 );
                strcpy( parmstring, AsmBuffer[i]->string_ptr );
                /* v2.02: if there's additional stuff behind the <> literal,
                 * it's an error!
                 */
                if ( AsmBuffer[i+1]->token != T_FINAL )
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+1]->string_ptr );
            } else {
                char *ptr2;
                /* AsmBuffer[i]->pos cannot be used (T_NUM!) */
                ptr = AsmBuffer[i-1]->pos;
                ptr++; /* skip comma */
                while ( isspace(*ptr) ) ptr++;
                ptr2 = ptr;
                /* this is what Masm does: use the string until a space
                 * is detected. Anything beyond the space is ignored.
                 */
                while (*ptr2 && ( isspace(*ptr2) == FALSE) )
                    ptr2++;
                len = ptr2 - ptr;
                parmstring = AsmTmpAlloc( len + 1 );
                memcpy( parmstring, ptr, len );
                *(parmstring+len) = NULLC;
            }
        } else {
            /* for FOR, skip everything between the name and the comma!
             * these items will be stored as (first) macro parameter.
             * for example, valid syntax is:
             * FOR xxx,<a, ...>
             * FOR xxx:REQ,<a, ...>
             */
            while ( AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA )
                i++;
            if( AsmBuffer[i]->token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;
            /* FOR/IRP accepts a literal enclosed in <> only */
            if( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
            parmstring = AsmTmpAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1 );
            //v2.0: use GetLiteralValue() instead of strcpy!!!
            //strcpy( parmstring, AsmBuffer[i]->string_ptr );
            GetLiteralValue( parmstring, AsmBuffer[i]->string_ptr );
            DebugMsg(("LoopDirective(FOR): param string >%s<\n", parmstring));
        }
        /* to run StoreMacro(), AsmBuffer must be setup correctly. */
        /* clear contents beginning with the comma! */
        i--;
        AsmBuffer[i]->token = T_FINAL;
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

    DebugMsg(("LoopDirective: calling StoreMacro\n"));
    if( StoreMacro( macro, i, TRUE ) == ERROR ) {
#if USELOCALMAC
        ReleaseMacroData( macro );
#else
        dir_free(macro, FALSE);
#endif
        return( ERROR );
    }
    /* EXITM is allowed inside a macro loop */
    /* this doesn't make the loop a macro function, reset the bit! */
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
        /* currently there's just one queue generated for all
         * repetitions. This won't work if a GOTO is contained!
         * Same might be the case for FORC (see below!).
         * Also, the line numbering probably won't work correctly
         * for iteration > 1.
         */
        for ( ; macro->sym.value < opndx.value; macro->sym.value++ ) {
            //RunMacro( macro, "", NULL, len == 1, first, FALSE );
            RunMacro( macro, "", NULL, TRUE, TRUE, FALSE );
            if ( AsmBuffer[0]->value == T_EXITM )
                break;
            DebugMsg(("LoopDirective REPT: iteration=%u\n", macro->sym.total_length ));
            //first = FALSE;
        }
        break;
    case T_WHILE:
        oldbufferend = StringBufferEnd;
        while ( opndx.kind == EXPR_CONST && opndx.value != 0 ) {
            DebugMsg(("LoopDirective WHILE: cnt=%u\n", count++ ));
            RunMacro( macro, "", NULL, TRUE, TRUE, FALSE );
            if ( AsmBuffer[0]->value == T_EXITM )
                break;
            /* Don't use the first item in AsmBuffer! This ensures
             that the line buffer isn't set to our local buffer.
             And restore StringBufferEnd so it won't increase
             with each iteration! */
            i = 1;
            StringBufferEnd = oldbufferend;
            Token_Count = Tokenize( line, i );
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                break;
            macro->sym.value++;
        }
        break;
    case T_FORC:
    case T_IRPC:
        for( ptr = parmstring; *ptr; macro->sym.value++ ) {
            char * ptr2 = line;
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
            //RunMacro( macro, line, NULL, *ptr == NULLC, first, FALSE);
            RunMacro( macro, line, NULL, TRUE, TRUE, FALSE);
            if ( AsmBuffer[0]->value == T_EXITM )
                break;
            //first = FALSE;
            DebugMsg(("LoopDirective FORC: call RunMacro(), cnt=%u, param=>%s<\n", count++, line ));
        }
        break;
    default: /* T_FOR, T_IRP */
        /* a FOR/IRP parameter can be a macro function call */
        /* that's why the macro calls must be run synchronously */
        for( ptr = parmstring; *ptr; macro->sym.value++ ) {
            DebugMsg(("LoopDirective FOR: cnt=%u, calling RunMacro( param=>%s<, prefix=NULL, runit=1, insert=1, addbrackets=0 )\n", count++, ptr ));
            len = RunMacro( macro, ptr, NULL, TRUE, TRUE, FALSE);
            if ( len < 1 || AsmBuffer[0]->value == T_EXITM )
                break;
            ptr += len;
        }
    }
#if USELOCALMAC
    ReleaseMacroData( macro );
#else
    /* free the temporary macro. dir_free() doesn't really free the whole
     * thing, but with FASTMEM=1 this is pretty irrelevant.
     */
    dir_free( macro, FALSE );
#endif
    DebugMsg(("LoopDirective exit\n"));
    return( NOT_ERROR );
}
