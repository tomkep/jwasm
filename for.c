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


#include "asmglob.h"
#include <ctype.h>

#include "asmdefs.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "equate.h"
#include "expreval.h"
#include "asminput.h"
#include "labels.h"
#include "macro.h"

extern void             AddTokens( struct asm_tok **, int, int );

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

int ForDirective( int i, int directive )
/*************************************/
{
    int start = i - 1; /* location of "directive name .. after any labels" */
    int arg_loc;
    bool first = TRUE;
    char *parmstring;
    char *ptr;
    char c;
    char *next_parm;
    char *end_of_parms;
    dir_node * macro;
    expr_list opndx;
    char buffer[MAX_LINE_LEN];
    int len;

    DebugMsg(("ForDirective(%u, %u) enter\n", i, directive));

    if (start > 1 && AsmBuffer[start-1]->token == T_COLON)
        MakeLabel( AsmBuffer[start-2]->string_ptr, MT_NEAR, NULL, TRUE );

    switch (directive) {
    case T_REPT:
    case T_REPEAT:
        if ((ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE )) || opndx.type != EXPR_CONST) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        len = opndx.value;
        if( AsmBuffer[i]->token != T_FINAL ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        break;
    case T_WHILE:
        arg_loc = i;
        if ((ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE )) || opndx.type != EXPR_CONST) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        /* the expression must be saved, since AsmBuffer will be
         destroyed
         */
        buffer[0] = '\0';
        for (i = arg_loc;i < Token_Count;i++) {
            strcat(buffer, AsmBuffer[i]->string_ptr);
            strcat(buffer, " ");
        }
        break;
    default:
        /* save the parm list, make a temporary macro, then call it with each parm */
        /* the first parameter will become a macro parameter, so it can
          be a simple T_ID, but also an instruction or something else */
        if( AsmBuffer[i]->token == T_FINAL) {
            AsmError( OPERAND_EXPECTED );
            return( ERROR );
        }
        c = *AsmBuffer[i]->string_ptr;
        if(( is_valid_id_char(c) == FALSE) || (isdigit(c) == TRUE)) {
            DebugMsg(("ForDirective(FOR): token %s is not an ID\n", AsmBuffer[i]->string_ptr));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        arg_loc = i;

        i++;
        while (AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA)
            i++;

        if( AsmBuffer[i]->token != T_COMMA ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }

        i++;
        // FORC accepts anything as parameter
        if( directive == T_FORC || directive == T_IRPC) {
            if (AsmBuffer[i]->token == T_FINAL) {
                AsmError( PARM_REQUIRED );
                return( ERROR );
            }
            if( AsmBuffer[i]->token == T_STRING) {
                parmstring = AsmTmpAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1 );
                strcpy( parmstring, AsmBuffer[i]->string_ptr );
            } else {
                char *ptr2;
                /* AsmBuffer[i]->pos cannot be used (T_NUM!) */
                ptr = AsmBuffer[i-1]->pos;
                ptr++;
                while (isspace(*ptr)) ptr++;
                ptr2 = ptr;
                while (*ptr2 && (isspace(*ptr2) == FALSE))
                    ptr2++;
                len = ptr2 - ptr;
                parmstring = AsmTmpAlloc( len + 1 );
                memcpy( parmstring, ptr, len );
                *(parmstring+len) = '\0';
            }
        } else {
            // FOR accepts a string only
            if( AsmBuffer[i]->token != T_STRING) {
                AsmError( PARM_REQUIRED );
                return( ERROR );
            }
            parmstring = AsmTmpAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1 );
            strcpy( parmstring, AsmBuffer[i]->string_ptr );
        }

        /* skip the comma and the string */
        i--;
        AsmBuffer[i]->token = T_FINAL;
        Token_Count = i;
        i = arg_loc;
    }

    /* now make a macro */
    macro = dir_insert_ex( "", TAB_MACRO );
    macro->e.macroinfo->hidden = TRUE;
    macro->e.macroinfo->srcfile = get_curr_srcfile();

    DebugMsg(("ForDirective: calling FillMacro\n"));
    if( FillMacro( macro, i, TRUE ) == ERROR ) {
        dir_free(macro, FALSE);
        return( ERROR );
    }
    /* EXITM is allowed inside a loop construct */
    /* this doesn't make the loop a macro function, reset the bit! */
    macro->e.macroinfo->isfunc = FALSE;

    /* now call the above macro with each of the given parms */

    switch (directive) {
    case T_REPEAT:
    case T_REPT:
        for (;len;len--) {
            RunMacro( macro, "", NULL, len == 1, first, FALSE );
            first = FALSE;
        }
        break;
    case T_WHILE:
        while (opndx.type == EXPR_CONST && opndx.value != 0) {
            RunMacro( macro, "", NULL, TRUE, TRUE, FALSE );
            if (AsmBuffer[0]->value == T_EXITM)
                break;
            Token_Count = Tokenize(buffer,0);
            i = 0;
            EvalOperand( &i, Token_Count, &opndx, TRUE );
        }
        break;
    default: /* T_FOR, T_FORC, T_IRP, T_IRPC */
        end_of_parms = parmstring + strlen( parmstring );
        /* a FOR/IRP parameter can be a macro function call */
        /* that's why the macro calls cannot be buffered */
        for( ptr = parmstring; ptr < end_of_parms; ) {
            buffer[0] = '\0';
            if( directive == T_FORC || directive == T_IRPC) {
                char * ptr2 = buffer;
                *ptr2++ = '<';
                if (*ptr == '!' || *ptr == '<' || *ptr == '>')
                    *ptr2++ = '!';
                *ptr2++ = *ptr++;
                *ptr2++ = '>';
                *ptr2 = NULLC;
                RunMacro( macro, buffer, NULL, ptr >= end_of_parms, first, FALSE);
                first = FALSE;
            } else {
                while (isspace(*ptr)) ptr++;
                if (*ptr == '"') {
                    next_parm = ptr + strcspn( ptr+1, "\"\0");
                    next_parm++;
                    if (*next_parm == '"')
                        next_parm++;
                } else
                    next_parm = ptr + strcspn( ptr, ",\0" );
                *next_parm = NULLC;
                next_parm++;
                strcat( buffer, ptr );
                ptr = next_parm;
                RunMacro( macro, buffer, NULL, TRUE, TRUE, FALSE);
                if (AsmBuffer[0]->value == T_EXITM)
                    break;
            }
            DebugMsg(("ForDirective: call RunMacro, param=>%s<\n", buffer));
        }
    }
    /* free the temporary macro */
    dir_free(macro, FALSE);
    DebugMsg(("ForDirective exit\n"));
    return( NOT_ERROR );
}
