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
#include "fastpass.h"

#include "myassert.h"

extern void GetInsString( enum asm_token , char *, int );

// DefineConstant is used by
//   EQU:    redefine=FALSE
//   '=':    redefine=TRUE

int DefineConstant( bool redefine )
/***********************************************************/
{
    asm_sym *sym;

    if( AsmBuffer[0]->token != T_ID) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (sym = CreateConstant( AsmBuffer[0]->string_ptr, 0, 2, redefine ) ) {
        if (ModuleInfo.list == TRUE) {
            WriteLstFile(LSTTYPE_EQUATE, 0, sym);
            directive_listed = TRUE;
        }
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

#if FASTPASS

void SaveEquateState(asm_sym *sym)
{
    equ_item *p;
    sym->saved = TRUE;
    p = AsmAlloc(sizeof(equ_item));
    p->next = NULL;
    p->sym = sym;
    p->value   = sym->value;
    p->defined = sym->defined;
    if (modstate.EquTail) {
        modstate.EquTail->next = p;
        modstate.EquTail = p;
    } else {
        modstate.EquHead = modstate.EquTail = p;
    }
//    printf("state of symbol >%s< saved, value=%u, defined=%u\n", sym->name, sym->value, sym->defined);
}
#endif

// CreateConstant (the worker)
// <value> is used only if start is -1
// then a numeric constant/variable is defined with this value.
// if <start> is != -1, the AsmBuffer is evaluated.
// EQU:     redefine = FALSE
// '=':     redefine = TRUE
// if <start> is 2, it is a standard EQU/=/TEXTEQU
// for EQU, the pos in the original source line is then
// AsmBuffer[1]->pos + 4

asm_sym * CreateConstant( char *name, int value, int start, bool redefine )
/**********************************************************************************************/
{
    struct asm_sym      *sym;
    int                 i;
    int                 j;
    bool                cmpvalue = FALSE;
    expr_list           opndx;

    DebugMsg(( "CreateConstant(%s, value=%u, redef=%u) enter\n", name, value, redefine));

    sym = SymSearch( name );

    /* if we've never seen it before, put it in */
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        /* wait with definition until TYPE is clear */
    } else if( sym->state == SYM_TMACRO && redefine == FALSE) {
        /* it's a text macro, valid for EQU only */
        return ( SetTextMacro(sym, name, AsmBuffer[1]->pos + 4));
    } else if( sym->state != SYM_INTERNAL && sym->state != SYM_EXTERNAL) {
        /* it is defined as something else, get out */
        DebugMsg(( "CreateConstant(%s) state=%u, mem_type=%u, value=%X, symbol redefinition\n", name, sym->state, sym->mem_type, sym->value));
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    } else if (redefine == FALSE) {
        if (sym->defined == TRUE && sym->state != SYM_EXTERNAL)
            cmpvalue = TRUE;
        /* if EQU is a CONST, it is FIX. Compare values (cannot be changed) */
#if 0  // sometimes an equate isn't correctly defined in pass1
        // i.e. it might be the difference of 2 labels, and this value may
        // change
        if (Parse_Pass > PASS_1) { /* no need to rescan numbers */
            sym->defined = TRUE;
            return(sym);
        }
#endif
    }

    /* define a numeric constant/variable?  */

    if( start == -1 ) {
        if (!sym) {
            sym = SymCreate( name, TRUE );
#if FASTPASS
            sym->saved = FALSE;
#endif
        }
#if FASTPASS
        if (StoreState && redefine && sym->saved == FALSE) {
            SaveEquateState(sym);
        }
#endif
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
        opndx.indirect = FALSE;
        j = NOT_ERROR;
        i++;
    } else
#endif
    j = EvalOperand( &i, Token_Count, &opndx, FALSE );
    /* for EQU, don't allow value to change */
    if (cmpvalue) {
        if (j != ERROR &&
            AsmBuffer[i]->token == T_FINAL && opndx.string == NULL &&
            (opndx.type == EXPR_CONST ||
             (opndx.type == EXPR_ADDR && opndx.sym != NULL))) {
            DebugMsg(( "CreateConstant(%s): expression evaluated, value=%X, string=%X\n", name, opndx.value, opndx.string));
            if ((opndx.type == EXPR_CONST) && (sym->value == opndx.value)) {
                return( sym );
            }
            // if ((opndx.type == EXPR_ADDR) && (dir->e.constinfo->sym->offset == opndx.sym->offset))
            if (opndx.type == EXPR_ADDR) {
                if ((sym->offset == (opndx.sym->offset + opndx.value)) && (sym->segment == opndx.sym->segment)) {
                    return( sym );
                }
            }
        }
#ifdef DEBUG_OUT
        if (opndx.type == EXPR_CONST)
            DebugMsg(("CreateConstant(%s), CONST value changed: old=%X, new=%X\n", name, sym->offset, opndx.value));
        else if (opndx.type == EXPR_ADDR && opndx.sym)
            DebugMsg(("CreateConstant(%s), ADDR value changed: old=%X, new ofs+val=%X+%X\n", name, sym->offset, opndx.sym->offset, opndx.value));
        else
            DebugMsg(("CreateConstant(%s), ADDR value changed: old=%X, new sym=NULL, value=%X\n", name, sym->offset, opndx.value));
#endif
        if (opndx.type == EXPR_CONST) {
#if FLAG_LABELDIFF
            /* skip error if constant is the difference of 2 labels and
             a phase error has occured */
//            if (opndx.labeldiff && PhaseError) {
            if (opndx.labeldiff) {
                goto noerr;
            }
#endif
            AsmErr( SYMBOL_REDEFINITION, name );
            return( NULL );
        }
    }
noerr:
    /* what is an acceptable 'number' for EQU?
     1. a constant value - if it is not a string and magnitude is <= 32.
        This includes struct fields.
     2. an address - if it is direct and doesn't contain an external reference.
     Anything else will be stored as a string.
     */
    if (j != ERROR &&
        AsmBuffer[i]->token == T_FINAL &&
        opndx.string == NULL &&
        opndx.indirect == FALSE &&
        /* the CONST's magnitude must be <= 32 */
        ((opndx.type == EXPR_CONST &&
          ((opndx.hvalue == 0 && opndx.hlvalue == 0) ||
           (opndx.value < 0 && opndx.hvalue == -1))) ||
         (opndx.type == EXPR_ADDR && opndx.sym != NULL && opndx.sym->state != SYM_EXTERNAL)) &&
        opndx.instr == EMPTY) {
        if (!sym) {
            sym = SymCreate( name, TRUE );
#if FASTPASS
            sym->saved = FALSE;
#endif
        }
#if FASTPASS
        if (StoreState && redefine && sym->saved == FALSE) {
            SaveEquateState(sym);
        }
#endif
        sym->variable = redefine;
        sym->defined = TRUE;
        sym->equate = TRUE;
        sym->state = SYM_INTERNAL;
        if (opndx.type == EXPR_CONST) {
            sym->mem_type = MT_ABS;
            sym->value = opndx.value;
            DebugMsg(("CreateConstant(%s), CONST, value set: %X\n", name, opndx.value));
        } else {
            sym->mem_type = opndx.mem_type;
            sym->offset = opndx.sym->offset + opndx.value;
            sym->segment = opndx.sym->segment;
            DebugMsg(("CreateConstant(%s), ADDR, value set: ofs=%X/value=%X (sym=%X)\n", name, opndx.sym->offset, opndx.value, opndx.sym));
        }
        return( sym );
    }

    /* value cannot be evaluated.
     This is an error for '=' (redefine == TRUE)
     and EQU if it was previously defined as a number (cmpvalue == TRUE)
     */

    if (redefine == TRUE || cmpvalue == TRUE) {
        if (Parse_Pass == PASS_1) {
            DebugMsg(("CreateConstant(%s): value is NOT numeric, opndx.string=%X\n", name, opndx.string));
            return( sym );
        } else {
            // AsmErr( UNDEFINED_SYMBOL, name );
            AsmError( CONSTANT_EXPECTED );
            return( NULL );
        }
    }

    DebugMsg(("CreateConstant(%s): value is NOT numeric, opndx.string=%X, calling SetTextMacro()\n", name, opndx.string));

    return ( SetTextMacro(sym, name, AsmBuffer[1]->pos + 4));
}

