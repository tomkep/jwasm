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


#include "globals.h"
#include <ctype.h>

#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "equate.h"
#include "expreval.h"
#include "input.h"
#include "labels.h"
#include "macro.h"
#include "listing.h"

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

int LoopDirective( int i, int directive )
/*************************************/
{
    int start = i - 1; /* location of "directive name .. after any labels" */
    int arg_loc;
    int len;
    bool first = TRUE;
    char c;
    char *parmstring;
    char *ptr;
    dir_node * macro;
    expr_list opndx;
    char buffer[MAX_LINE_LEN];

    DebugMsg(("LoopDirective(%u, %u) enter\n", i, directive));

    if (ModuleInfo.list == TRUE)
        WriteLstFile(LSTTYPE_MACRO, 0, NULL);

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
        if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
            return( ERROR );
        if (opndx.type != EXPR_CONST || opndx.string != NULL)
            AsmError( CONSTANT_EXPECTED );
        /* the expression must be saved, since AsmBuffer will be destroyed */
        ptr = AsmBuffer[start]->pos + 5;  /* 5 = strlen("WHILE") */
        while (isspace(*ptr)) ptr++;
        strcpy(buffer, ptr);
        break;
    default: /* FOR, FORC, IRP, IRPC */
        /* get the formal parameter and the argument list */
        /* the format parameter will become a macro parameter, so it can
          be a simple T_ID, but also an instruction or something else */
        if( AsmBuffer[i]->token == T_FINAL) {
            AsmError( OPERAND_EXPECTED );
            return( ERROR );
        }
        c = *AsmBuffer[i]->string_ptr;
        if(( is_valid_id_char(c) == FALSE) || (isdigit(c) == TRUE)) {
            DebugMsg(("LoopDirective(FOR): token %s is not an ID\n", AsmBuffer[i]->string_ptr));
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
        // FORC accepts anything as "argument list"
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
            /* FOR/IRP accepts a string only */
            if( AsmBuffer[i]->token != T_STRING) {
                AsmError( PARM_REQUIRED );
                return( ERROR );
            }
            parmstring = AsmTmpAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1 );
            strcpy( parmstring, AsmBuffer[i]->string_ptr );
            DebugMsg(("LoopDirective(FOR): param string >%s<\n", parmstring));
        }
        /* to run FillMacro(), AsmBuffer must be setup correctly. */
        /* the comma and the string must be made invisible */
        i--;
        AsmBuffer[i]->token = T_FINAL;
        Token_Count = i;
        i = arg_loc;
    }

    /* now make a temporary macro */
    macro = CreateMacro( "" );
    macro->e.macroinfo->srcfile = get_curr_srcfile();

    DebugMsg(("LoopDirective: calling FillMacro\n"));
    if( FillMacro( macro, i, TRUE ) == ERROR ) {
        dir_free(macro, FALSE);
        return( ERROR );
    }
    /* EXITM is allowed inside a loop construct */
    /* this doesn't make the loop a macro function, reset the bit! */
    macro->sym.isfunc = FALSE;

    /* now run the just created macro in a loop */

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
    case T_FORC:
    case T_IRPC:
        for( ptr = parmstring; *ptr; ) {
            char * ptr2 = buffer;
            *ptr2++ = '<';
            if (*ptr == '!' || *ptr == '<' || *ptr == '>')
                *ptr2++ = '!';
            *ptr2++ = *ptr++;
            *ptr2++ = '>';
            *ptr2 = NULLC;
            RunMacro( macro, buffer, NULL, *ptr == NULLC, first, FALSE);
            first = FALSE;
            DebugMsg(("LoopDirective FORC: call RunMacro(), param=>%s<\n", buffer));
        }
        break;
    default: /* T_FOR, T_IRP */
        /* a FOR/IRP parameter can be a macro function call */
        /* that's why the macro calls cannot be buffered */
        for( ptr = parmstring; *ptr;) {
            DebugMsg(("LoopDirective FOR: calling RunMacro( param=>%s<, prefix=NULL, runit=1, insert=1, addbrackets=0 )\n", ptr ));
            len = RunMacro( macro, ptr, NULL, TRUE, TRUE, FALSE);
            if (len < 1 || AsmBuffer[0]->value == T_EXITM)
                break;
            ptr += len;
#if 0
            /* RunMacro() has skipped the comma already! */
            if (*ptr && *ptr != ',') {
                AsmError(EXPECTING_COMMA);
                break;
            }
            if (*ptr) ptr++;
#endif
        }
    }
    /* free the temporary macro. dir_free() doesn't really free the whole
     thing, but with FASTMEM=1 this is pretty irrelevant.
     */
    dir_free(macro, FALSE);
    DebugMsg(("LoopDirective exit\n"));
    return( NOT_ERROR );
}
