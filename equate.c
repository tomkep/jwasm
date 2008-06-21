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
* Description:  handles EQU and EQU2 ('=') directives
* this file has been rewritten for JWasm.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "parser.h"
#include "expreval.h"
#include "equate.h"

#include "memalloc.h"
#include "symbols.h"
#include "directiv.h"
#include "labels.h"
#include "input.h"
#include "macro.h"

#include "myassert.h"

extern void GetInsString( enum asm_token , char *, int );

// DefineConstant is used by
//   EQU:    i=0, redefine=0
//   EQU2:   i=0, redefine=1
// (EQU2 is the "=" directive).

int DefineConstant( int i, bool redefine )
/***********************************************************/
{
    asm_sym *sym;

    if( i != 0 || AsmBuffer[0]->token != T_ID) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (sym = CreateConstant( AsmBuffer[0]->string_ptr, 0, 2, redefine ) ) {
        WriteLstFile(LSTTYPE_EQUATE, 0, sym);
        return(NOT_ERROR);
    }
    return(ERROR);
}

// this is to define values like
// __386__, __486__
// if directives like .386, .486, ... are found

void MakeConstantUnderscored( int token )
/*****************************/
{
    char buffer[23];

    /* define a macro */

    strcpy( buffer, "__" );
    GetInsString( (enum asm_token)token, buffer+2, 18 );
    strcat( buffer, "__" );
    strupr( buffer );
    CreateConstant( buffer, 1, -1, TRUE );
    return;
}


// CreateConstant (the worker)
// <value> is only used if start is -1
// then a numeric constant/variable is defined with this value.
// if <start> is != -1, the AsmBuffer is evaluated.
// EQU:     redefine = FALSE
// '=':     redefine = TRUE
// if <start> is 2, it is a standard EQU/=/TEXTEQU
// for EQU, the pos in the original source line is then
// AsmBuffer[1]->pos + 4

void * CreateConstant( char *name, int value, int start, bool redefine )
/**********************************************************************************************/
{
    struct asm_sym      *sym;
//    dir_node            *dir;
    int                 i;
    int                 j;
    char                *ptr;
    int                 count;
    bool                cmpvalue = FALSE;
    expr_list           opndx;

    DebugMsg(( "CreateConstant(%s, value=%u, redef=%u) enter\n", name, value, redefine));

    sym = SymSearch( name );

    /* if we've never seen it before, put it in */
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        /* wait with definition until TYPE is clear */
    } else if( sym->state == SYM_TMACRO && redefine == FALSE) {
        /* it's a text macro, valid for EQU only */
        goto define_tmacro;
    } else if( sym->state != SYM_INTERNAL && sym->state != SYM_EXTERNAL) {
        /* it is defined as something else, get out */
        DebugMsg(( "CreateConstant(%s) state=%u, mem_type=%u, exit\n", name, sym->state, sym->mem_type));
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    } else if (redefine == FALSE) {
        if (sym->defined == TRUE && sym->state != SYM_EXTERNAL)
            cmpvalue = TRUE;
        sym->defined = TRUE;
        /* if EQU is a CONST, it is FIX. Compare values (cannot be changed) */
#if 0  // sometimes an equate isn't correctly defined in pass1
        // i.e. it might be the difference of 2 labels, and this value may
        // change
        if (Parse_Pass > PASS_1) { /* no need to rescan numbers */
            return(sym);
        }
#endif
    }

    /* define a numeric constant/variable?  */

    if( start == -1 ) {
        if (!sym)
            sym = SymCreate( name, TRUE );
        sym->defined = TRUE;
        sym->state = SYM_INTERNAL;
        sym->mem_type = MT_ABS;
        sym->variable = redefine;
        sym->offset = value;
        sym->equate = TRUE;
        DebugMsg(( "CreateConstant(%s) exit\n", name));
        return( sym );
    }

    // try to evalate the expression for EQU and '='

    i = start;
#if 1 // a tiny optimization to avoid calling the evaluator for simple numbers
    if (AsmBuffer[i]->token == T_NUM && AsmBuffer[i+1]->token == T_FINAL) {
        opndx.llvalue = AsmBuffer[i]->llvalue;
        opndx.hlvalue = AsmBuffer[i]->hlvalue;
        opndx.type = EXPR_CONST;
        opndx.string = NULL;
        opndx.instr = EMPTY;
        opndx.labeldiff = FALSE;
        j = NOT_ERROR;
        i++;
    } else
#endif
    j = EvalOperand( &i, Token_Count, &opndx, FALSE );
    /* for EQU, don't allow value to change */
    if (cmpvalue) {
        sym->defined = TRUE;
        sym->state = SYM_INTERNAL;
        sym->equate = TRUE;
        if (j != ERROR &&
            AsmBuffer[i]->token == T_FINAL &&
            opndx.string == NULL &&
            (opndx.type == EXPR_CONST || opndx.type == EXPR_ADDR)) {
            DebugMsg(( "CreateConstant(%s): expression evaluated, value=%X, string=%X\n", name, opndx.value, opndx.string));
            if ((opndx.type == EXPR_CONST) && (sym->value == opndx.value)) {
                return( sym );
            }
            // if ((opndx.type == EXPR_ADDR) && (dir->e.constinfo->sym->offset == opndx.sym->offset))
            if (opndx.type == EXPR_ADDR) {
                if (opndx.sym && (sym->offset == (opndx.sym->offset + opndx.value)) && (sym->segment == opndx.sym->segment)) {
                    return( sym );
                }
                if (opndx.mbr && (sym->offset == opndx.mbr->offset)) {
                    return( sym );
                }
            }
        }
#ifdef DEBUG_OUT
        if (opndx.type == EXPR_CONST)
            DebugMsg(("CreateConstant(%s), value changed: old=%X, new=%X\n", name, sym->offset, opndx.value));
        else if (opndx.type == EXPR_ADDR)
            DebugMsg(("CreateConstant(%s), value changed: old=%X, new ofs+val=%X+%X\n", name, sym->offset, opndx.sym->offset, opndx.value));
#endif
        if (opndx.type == EXPR_CONST) {
#if FLAG_LABELDIFF
            /* skip error if constant is a difference of 2 labels and
             a phase error has occured */
            if (opndx.labeldiff && PhaseError) {
                goto noerr;
            }
#endif
            AsmErr( SYMBOL_REDEFINITION, name );
            return( NULL );
        }
    }
noerr:
    /* if a const value was evaluated, set it and exit */
    if (j != ERROR &&
        AsmBuffer[i]->token == T_FINAL &&
        opndx.string == NULL &&
        opndx.indirect == FALSE &&
#if 1
        /* the CONST's magnitude must be <= 32 */
        ((opndx.type == EXPR_CONST &&
          ((opndx.hvalue == 0 && opndx.hlvalue == 0) ||
           (opndx.value < 0 && opndx.hvalue == -1))) ||
         opndx.type == EXPR_ADDR) &&
#else
        (opndx.type == EXPR_CONST || opndx.type == EXPR_ADDR) &&
#endif
        opndx.instr == EMPTY) {
        if (!sym)
            sym = SymCreate( name, TRUE );
        sym->variable = redefine;
        sym->defined = TRUE;
        sym->equate = TRUE;
        sym->state = SYM_INTERNAL;
        if (opndx.type == EXPR_CONST) {
            sym->mem_type = MT_ABS;
            sym->value = opndx.value;
            DebugMsg(("CreateConstant(%s), CONST, value set: %I64X\n", name, opndx.llvalue));
        } else {
            // dir->sym.mem_type = opndx.sym->mem_type;
            sym->mem_type = opndx.mem_type;
            if (opndx.sym) {
                DebugMsg(("CreateConstant(%s), ADDR, value set: ofs=%X/value=%X (sym=%X)\n", name, opndx.sym->offset, opndx.value, opndx.sym));
                sym->offset = opndx.sym->offset + opndx.value;
                sym->segment = opndx.sym->segment;
            } else {
                /* the evaluator handles structure fields strange and should be fixed */
                sym->mem_type = MT_ABS;
                DebugMsg(("CreateConstant(%s), ADDR, value set: ofs=%X/value=%X (mbr=%X)\n", name, opndx.mbr->offset, opndx.value, opndx.mbr));
                sym->offset = opndx.mbr->offset; // + opndx.value;
                sym->segment = opndx.mbr->segment;
            }
        }
        return( sym );
    }

    /* value cannot be evaluated.
     This is an error for '=' (redefine == TRUE)
     and EQU if it was previously defined as a number (cmpvalue == TRUE)
     */

    if (redefine == TRUE || cmpvalue == TRUE) {
        if (Parse_Pass == PASS_1) {
            DebugMsg(("CreateConstant(%s): value is NOT numeric\n", name));
            // force another pass?
            return( sym );
        } else {
            AsmErr( UNDEFINED_SYMBOL, name );
            return( NULL );
        }
    }

    DebugMsg(("CreateConstant(%s): value is NOT numeric, will be stored as a string\n", name));

define_tmacro:

    if (sym == NULL)
        sym = SymCreate( name, TRUE );

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;

    /* it is EQU and items cannot be evaluated.
     So a string is to be defined.
     the original source is used, since the tokenizer has
     deleted some information
     */
#if 0
    if (AsmBuffer[1]->pos == 0) {
        printf("failure when creating %s\n", name);
        exit(1);
    }
#endif
    ptr = AsmBuffer[1]->pos + 4;
    while (isspace(*ptr)) ptr++;
    count = strlen(ptr);
    if (count) {
        for (;count;count--)
            if (isspace(*(ptr+count-1)) == FALSE)
                break;
        if (count)
            if (*ptr == '<' && *(ptr+count-1) == '>') {
                ptr++;
                count = count - 2;
            }
    }
    if (sym->string_ptr)
        AsmFree(sym->string_ptr);
    sym->string_ptr = (char *)AsmAlloc( count + 1);
    memcpy(sym->string_ptr, ptr, count);
    *(sym->string_ptr+count) = '\0';

    DebugMsg(("CreateConstant >%s<: value is >%s<, exit\n", name, sym->string_ptr));
    return( sym );
}

