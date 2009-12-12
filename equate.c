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
#include "tokenize.h"
#include "macro.h"
#include "fastpass.h"
#include "listing.h"
#include "input.h"

#if FASTPASS

/* for FASTPASS, just pass 1 is a full pass, the other passes
 don't start from scratch and they just assemble the preprocessed
 source. To be able to restart the assembly process from a certain
 location within the source, it's necessary to save the value of
 assembly time variables.
 */

static void SaveEquateState(asm_sym *sym)
/********************************/
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
// EQU:     redefine = FALSE
// '=':     redefine = TRUE

asm_sym * CreateConstant( bool redefine )
/***************************************/
{
    struct asm_sym      *sym;
    char                *name = AsmBuffer[0]->string_ptr;
    int                 i = 2;
    ret_code            rc;
    bool                cmpvalue = FALSE;
    expr_list           opndx;
    char                buffer[MAX_LINE_LEN];
    char                nameb[MAX_ID_LEN+1];

    DebugMsg(( "%lu. CreateConstant(%s, redef=%u) enter\n", LineNumber, name, redefine ));

    sym = SymSearch( name );

    if( sym == NULL ) {
        /*
         * if we've never seen it before
         * wait with definition until type of equate is clear
         */
    } else if( sym->state == SYM_UNDEFINED ) {

        dir_remove_table( (dir_node *)sym );

    } else if( sym->state == SYM_TMACRO ) {
        /* a text macro, this is valid for EQU only */
        if ( redefine == FALSE) {
            DebugMsg(( "CreateConstant: %s is a text macro, calling SetTextMacro(%s)\n", name, AsmBuffer[1]->pos + 4 ));
            return ( SetTextMacro( sym, name, AsmBuffer[1]->pos + 4 ) );
        }
        /* this should never happen, since the equate id at pos 0
         has been replaced by its value already!
         */
        AsmErr( SYMBOL_REDEFINITION, sym->name );
        return( NULL );
    } else if( sym->equate == FALSE ) {
        /* it is defined as something else, get out */
        DebugMsg(( "CreateConstant(%s) state=%u, mem_type=%Xh, value=%lX, symbol redefinition\n", name, sym->state, sym->mem_type, sym->value));
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    } else if ( redefine == FALSE || sym->variable == FALSE ) {
        if ( sym->defined == TRUE && sym->state != SYM_EXTERNAL ) {
            /* ensure that value doesn't change! */
            cmpvalue = TRUE;
        }
    }

    // try to evalate the expression for EQU and '='

    if (AsmBuffer[i]->token == T_NUM &&
        AsmBuffer[i+1]->token == T_FINAL &&
        AsmBuffer[i]->hvalue == 0 ) {
        opndx.llvalue = AsmBuffer[i]->value64;
        opndx.hlvalue = 0;
        opndx.string = NULL;
        opndx.instr = EMPTY;
        opndx.kind = EXPR_CONST;
        opndx.flags = 0;
        rc = NOT_ERROR;
        DebugMsg(( "%lu. CreateConstant(%s): simple numeric value=%lX\n", LineNumber, name, AsmBuffer[i]->value64 ));
        i++;
    } else {
//        if ( redefine == FALSE ) {
        if ( redefine == FALSE && Parse_Pass == PASS_1 ) {
            /* handle the special EQU case: if the expression can be evaluated
             to a numeric value, it's a numeric equate. If no, the EQU is to
             become a text macro, but the value of this macro is the ORIGINAL,
             unexpanded line!!! Also important is that macro function calls
             are NEVER resolved, the equate will always become a text.
             */
            int k;
            char *p;
            p = AsmBuffer[1]->pos+4;
            strcpy( buffer, p ); /* save original line */
            /* the name string might get destroyed if a macro is executed */
            strcpy( nameb, name );
            name = nameb;

            /* expand the line */
            while ( 1 ) {
                rc = NOT_ERROR;
                for( k = 2; k < Token_Count; k++ ) {
                    if ( ExpandToken( k, p, FALSE, TRUE ) == STRING_EXPANDED )
                        rc = STRING_EXPANDED;
                }
                /* if there was an expansion, the tokenizer must be called. */
                /* if Token_Count is 0, there was a macro function call and
                 the loop must continue. this should never happen, however! */
                if ( rc == STRING_EXPANDED ) {
                    k = Token_Count;
                    Token_Count = Tokenize( p, 2 );
                    if (k)
                        break;
                } else
                    break;
            }
        }
        rc = EvalOperand( &i, Token_Count, &opndx, redefine );
    }
    /* for EQU, don't allow value to change */
    if ( cmpvalue ) {
        if ( rc != ERROR &&
            AsmBuffer[i]->token == T_FINAL &&
            (opndx.kind == EXPR_CONST ||
             (opndx.kind == EXPR_ADDR && opndx.sym != NULL ))) {
            DebugMsg(( "CreateConstant(%s): expression evaluated, value=%lX, string=%X, labeldiff=%u\n", name, opndx.value, opndx.string, opndx.labeldiff ));
            if ( opndx.kind == EXPR_CONST && sym->value == opndx.value ) {
                return( sym );
            }
            // if ((opndx.kind == EXPR_ADDR) && (dir->e.constinfo->sym->offset == opndx.sym->offset))
            if ( opndx.kind == EXPR_ADDR ) {
#if 0
                /* test case:

                 db 0
                 L1 equ $
                 db L2 - L1 dup (0)
                 L2 equ $

                 result should be an "endless" loop, but
                 when the #if above is activated, it stops after 3 loops.
                 */
                if ( Parse_Pass > PASS_1 )
                    return( sym );
#endif
                if ((sym->offset == (opndx.sym->offset + opndx.value)) && (sym->segment == opndx.sym->segment)) {
                    return( sym );
                }
                PhaseError = TRUE;
#ifdef DEBUG_OUT
                printf("%u: %s: equate caused a phase error >%s<\n", Parse_Pass + 1, sym->name, AsmBuffer[0]->pos );
                printf("%u: %s: curr: type=%u addr=%X:%lX\n", Parse_Pass + 1, sym->name,
                       sym->type, sym->segment, sym->offset );
                printf("%u: %s: new: name=%s type=%u addr=%X:%lX, value=%lX\n", Parse_Pass + 1, sym->name,
                       opndx.sym->name, opndx.sym->type, opndx.sym->segment, opndx.sym->offset, opndx.value );
#endif
                sym->offset = opndx.sym->offset + opndx.value;
                sym->segment = opndx.sym->segment;
                sym->mem_type = opndx.mem_type;
                return( sym );
            }
        }
#ifdef DEBUG_OUT
        if (opndx.kind == EXPR_CONST)
            DebugMsg(("CreateConstant(%s), CONST value changed: old=%X, new=%X\n", name, sym->offset, opndx.value ));
        else if (opndx.kind == EXPR_ADDR && opndx.sym)
            DebugMsg(("CreateConstant(%s), ADDR value changed: old=%X, new ofs+val=%X+%X\n", name, sym->offset, opndx.sym->offset, opndx.value));
        else
            DebugMsg(("CreateConstant(%s), ADDR value changed: old=%X, new sym=NULL, value=%X\n", name, sym->offset, opndx.value));
#endif
        if ( opndx.kind == EXPR_CONST ) {
#if FLAG_LABELDIFF
            /* skip error if constant is the difference of 2 labels and
             * a phase error has occured.
             * v2.01: the PhaseError variable cannot be queried here. It is
             * set when a label's value changes, but this needn't have happened
             * yet.
             */
            //if ( opndx.labeldiff && PhaseError ) {
            if ( opndx.labeldiff ) {
                goto noerr;
            }
#endif
            AsmErr( SYMBOL_REDEFINITION, name );
            return( NULL );
        }
    }
noerr:
    /* what is an acceptable 'number' for EQU?
     1. a constant value - if magnitude is <= 32.
        This includes struct fields.
     2. an address - if it is direct and doesn't contain an external reference.
     Anything else will be stored as a string.
     */
    if ( rc != ERROR &&
        AsmBuffer[i]->token == T_FINAL &&
        //opndx.string == NULL &&
        opndx.indirect == FALSE &&
        
         /* CONSTs must be internal, and the magnitude must be <= 32 */
        ( ( opndx.kind == EXPR_CONST && opndx.abs == FALSE &&
          (( opndx.hvalue == 0 && opndx.hlvalue == 0 ) ||
           ( opndx.hvalue == -1 && opndx.uvalue != 0 ) ) ) ||
         ( opndx.kind == EXPR_ADDR && opndx.sym != NULL && opndx.sym->state != SYM_EXTERNAL ) ) &&
        ( opndx.instr == EMPTY || redefine == TRUE ) ) {
        if (!sym) {
            sym = SymCreate( name, TRUE );
#if FASTPASS
            //sym->saved = FALSE;
            /* don't save symbols which are defined after StoreState has been set */
            sym->saved = StoreState;
#endif
        }
#if FASTPASS
        else if ( StoreState && redefine ) {
            if ( sym->saved == FALSE && sym->defined == TRUE )
                SaveEquateState( sym );
            sym->saved = TRUE; /* don't try to save this symbol (anymore) */
        }
#endif
        if ( sym->state != SYM_INTERNAL ) { /* SYM_UNDEFINED or SYM_EXTERNAL? */
            sym->variable = redefine;
            sym->equate = TRUE;
            sym->state = SYM_INTERNAL;
        }
        sym->defined = TRUE;
        if ( opndx.kind == EXPR_CONST ) {
            sym->mem_type = MT_ABS;
            sym->uvalue = opndx.uvalue;
            sym->sign = (opndx.hvalue < 0);
            DebugMsg(("%lu. CreateConstant(%s), CONST, pass=%u, value=%lX, sign=%u, labeldiff=%u\n", LineNumber, name, Parse_Pass+1, sym->uvalue, sym->sign, opndx.labeldiff ));
        } else {
#if 1 /* v2.01: allow PROC equates */
            if ( opndx.sym->isproc ) {
                dir_node *dir = (dir_node *)sym;
                sym->equate = TRUE;
                sym->isproc = TRUE;
                /* just copy the procinfo extension! */
                dir->e.procinfo = ((dir_node *)opndx.sym)->e.procinfo;
            }
#endif
            sym->mem_type = opndx.mem_type;
            /* v2.01: allow equates of variables with arbitrary type.
             * Currently the expression evaluator sets opndx.mem_type
             * to the mem_type of the type (i.e. QWORD for a struct with size 8),
             * which is a bad idea in this case. So the original mem_type of the
             * label is used instead.
             */
            if ( opndx.sym->mem_type == MT_TYPE && opndx.explicit == FALSE ) {
                sym->mem_type = opndx.sym->mem_type;
                sym->type = opndx.sym->type;
            }
            sym->offset = opndx.sym->offset + opndx.value;
            sym->segment = opndx.sym->segment;
            DebugMsg(("%lu. CreateConstant(%s), ADDR, pass=%u, values: ofs=%lX/value=%lX (sym=%s)\n", LineNumber, name, Parse_Pass+1, opndx.sym->offset, opndx.value, opndx.sym->name ));
        }
        return( sym );
    }

    /* value cannot be evaluated.
     This is an error for '=' (redefine == TRUE)
     and EQU if it was previously defined as a number (cmpvalue == TRUE)
     */
    if ( cmpvalue ) {
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    }

    if ( redefine == TRUE ) {
#if FASTPASS==0
        if ( Parse_Pass == PASS_1 ) {
            DebugMsg(("CreateConstant(%s): value is NOT numeric: >%s<\n", name, AsmBuffer[1]->pos + strlen( AsmBuffer[1]->string_ptr) ));
            return( sym );
        }
#endif
        DebugMsg(("%u:CreateConstant(%s): value is NOT numeric/constant: >%s<\n", Parse_Pass+1, name, AsmBuffer[1]->pos + strlen( AsmBuffer[1]->string_ptr) ));
        if ( opndx.hvalue != 0 && opndx.hvalue != -1 )
            AsmError( CONSTANT_VALUE_TOO_LARGE );
        else
            AsmError( CONSTANT_EXPECTED );
        return( NULL );
    }
#if FASTPASS
    /* the text macro might have been used before it's defined.
     this is valid for Masm, but it's a problem for FASTPASS.
     Since it's also bad programming practice, just simply deactivate
     the fastpass feature for this module!
     */
    if ( sym && sym->state == SYM_UNDEFINED ) {
        SkipSavedState();
        AsmWarn( 2, TEXT_MACRO_USED_BEFORE_DEFINITION, sym->name );
    }
#endif

    DebugMsg(("CreateConstant(%s): value is NOT numeric, opndx.string=%X, calling SetTextMacro()\n", name, opndx.string));

    return ( SetTextMacro( sym, name, buffer ) );
}

// CreateConstantEx
// define an assembly time variable directly without using the token buffer.
// this is used for some internally generated variables.

asm_sym * CreateConstantEx( const char *name, int value )
/*******************************************************/
{
    struct asm_sym      *sym;

    DebugMsg(( "CreateConstantEx(%s, %d ) enter\n", name, value ));

    sym = SymSearch( name );
    if( sym == NULL ) {
        sym = SymCreate( name, TRUE );
#if FASTPASS
        sym->saved = FALSE;
#endif
    } else if ( sym->state == SYM_UNDEFINED ) {
        dir_remove_table( (dir_node *)sym );
    } else if ( sym->equate == FALSE ) {
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    }
#if FASTPASS
    if ( StoreState && sym->saved == FALSE ) {
        SaveEquateState(sym);
    }
#endif
    sym->defined  = TRUE;
    sym->state    = SYM_INTERNAL;
    sym->mem_type = MT_ABS;
    sym->variable = TRUE;
    sym->offset   = value;
    sym->equate   = TRUE;
    return( sym );
}

// DefineConstant is used by
//   EQU:    redefine=FALSE
//   '=':    redefine=TRUE

ret_code DefineConstant( bool redefine )
/**************************************/
{
    asm_sym *sym;

    if( AsmBuffer[0]->token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }
    if ( sym = CreateConstant( redefine ) ) {
        if ( ModuleInfo.list == TRUE ) {
            LstWrite( LSTTYPE_EQUATE, 0, sym );
        }
        return( NOT_ERROR );
    }
    return( ERROR );
}

