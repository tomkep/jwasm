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
* Description:  expression evaluator.
*
****************************************************************************/


#include "globals.h"
#include "parser.h"
#include "expreval.h"
#include "myassert.h"
#include "directiv.h"
#include "segment.h"
#include "proc.h"
#include "assume.h"
#include "input.h"
#include "tokenize.h"
#include "types.h"
#include "stddef.h"
#include "labels.h"

static int              op_sq_bracket_level;

/* error msg flag. Also indicates if EQU mode is on (error_msg=0).
 TRUE=display error messages.
 */
static bool             error_msg;

static asm_sym *thissym;  /* helper symbol for THIS operator */

enum process_flag {
    PROC_BRACKET, /* evaluate until the end or a closing bracket is found */
    PROC_OPERAND  /* stop if an operator with lower precedence is found */
};

static ret_code evaluate( expr_list *, int *, int, enum process_flag );

static void init_expr( expr_list *new )
/*************************************/
{
    new->value    = 0;
    new->hvalue   = 0;
    new->hlvalue  = 0;
    new->string   = NULL;
    new->base_reg = EMPTY;
    new->idx_reg  = EMPTY;
    new->label    = EMPTY;
    new->override = EMPTY;
    new->instr    = EMPTY;
    new->type     = EXPR_EMPTY;
    new->mem_type = MT_EMPTY;
    new->scale    = 1;
    new->ofs_size = OFSSIZE_EMPTY;
    new->flags    = 0;
    new->sym      = NULL;
    new->mbr      = NULL;
    new->assume   = NULL;
}

static void TokenAssign( expr_list *t1, expr_list *t2 )
/*****************************************************/
{
#if 1
    /* note that offsetof() is used. This means, don't change position
     of field <assume> in expr_list! */
    memcpy( t1, t2, offsetof( expr_list, assume) );
#else
    t1->value    = t2->value;
    t1->hvalue   = t2->hvalue;
    t1->value6495  = t2->value6495;
    t1->value96127 = t2->value96127;
    t1->string   = t2->string;
    t1->base_reg = t2->base_reg;
    t1->idx_reg  = t2->idx_reg;
    t1->label    = t2->label;
    t1->override = t2->override;
    t1->instr    = t2->instr;
    t1->type     = t2->type;
    t1->mem_type = t2->mem_type;
    t1->scale    = t2->scale;
    t1->ofs_size = t2->ofs_size;
    t1->flags    = t2->flags;
    t1->sym      = t2->sym;
    t1->mbr      = t2->mbr;
//    t1->assume   = t2->assume;
#endif
}

#define BRACKET_PRECEDENCE 1
#define PTR_PRECEDENCE     4
#define PLUS_PRECEDENCE    9

static int get_precedence( int i, int bracket_precedence )
/********************************/
{
    /* The following table is taken verbatim from MASM 6.1 Programmer's Guide,
     * page 14, Table 1.3.
     */

//    1             (), []
//    2             LENGTH, SIZE, WIDTH, MASK, LENGTHOF, SIZEOF
//    3             . (structure-field-name operator)
//    4             : (segment override operator), PTR
//    5             LROFFSET, OFFSET, SEG, THIS, TYPE
//    6             HIGH, HIGHWORD, LOW, LOWWORD
//    7             +, - (unary)
//    8             *, /, MOD, SHL, SHR
//    9             +, - (binary)
//    10            EQ, NE, LT, LE, GT, GE
//    11            NOT
//    12            AND
//    13            OR, XOR
//    14            OPATTR, SHORT, .TYPE

    /* The following table appears in QuickHelp online documentation for
     * both MASM 6.0 and 6.1. It's slightly different!
     */

//    1             LENGTH, SIZE, WIDTH, MASK
//    2             (), []
//    3             . (structure-field-name operator)
//    4             : (segment override operator), PTR
//    5             THIS, OFFSET, SEG, TYPE
//    6             HIGH, LOW
//    7             +, - (unary)
//    8             *, /, MOD, SHL, SHR
//    9             +, - (binary)
//    10            EQ, NE, LT, LE, GT, GE
//    11            NOT
//    12            AND
//    13            OR, XOR
//    14            SHORT, OPATTR, .TYPE, ADDR

    /* japheth: the first table is the prefered one. Reasons:
     - () and [] must be first
     - it contains operators SIZEOF, LENGTHOF, HIGHWORD, LOWWORD, LROFFSET
     - ADDR is no operator for expressions

     However, what's wrong in both tables is the precedence of
     the dot operator: Actually for both JWasm and Wasm the dot precedence
     is 2 and LENGTH, SIZE, ... have precedence 3 instead.

     Precedence of TYPE was 5 in original Wasm source. It has been changed
     to 4, as described in the Masm docs. This allows syntax
     "TYPE DWORD ptr xxx"

     Also, the precedence of '(' and '[' depends on the context.
     If the bracket is the current operator, its precedence is 1.
     However, for a new operator, the precedence is 9, the same as for '+'.
     */

    switch( AsmBuffer[i]->token ) {
    case T_UNARY_OPERATOR:
    case T_BINARY_OPERATOR:
        return( AsmBuffer[i]->precedence );
    case T_OP_BRACKET:
    case T_OP_SQ_BRACKET:
        return( bracket_precedence );
    case T_DOT:
        return( 2 );
    case T_COLON:
        return( 4 );
    case T_POSITIVE:
    case T_NEGATIVE:
        return( 7 );
    case '*':
    case '/':
        return( 8 );
    case '+':
    case '-':
        return( 9 );
    }
    /* shouldn't happen! */
    DebugMsg(("get_precedence: unexpected operator=%s\n", AsmBuffer[i]->string_ptr));
    AsmError( SYNTAX_ERROR );
    return( ERROR );
}

// get value for types
// NEAR, FAR and PROC are handled slightly differently:
// the HIBYTE is set to 0xFF, and PROC depends on the memory model

static int GetTypeSize( int i )
{
    int size;
    size = SimpleType[i].size;
    if ( size != -1 )
        return( size );
    switch ( i ) {
    case ST_NEAR:
        return (0xFF00 | (Use32 ? 4 : 2));
    case ST_FAR:
        return (0xFF00 | (Use32 ? 6 : 4));
    case ST_PROC:
        switch ( ModuleInfo.model ) {
        case MOD_MEDIUM:
        case MOD_LARGE:
        case MOD_HUGE:
            return ( 0xFF00 | (Use32 ? 6 : 4 ));
        default:
            return ( 0xFF00 | (Use32 ? 4 : 2 ));
        }
    }
    /* shouldn't happen */
    return 0;
}

static void CAsmError( int msg )
{
    if ( error_msg )
        AsmError( msg );
}

/* get an operand. operands are:
 - a number (not a float!) : EXPR_CONST
 - a string                : EXPR_CONST
 - a register              : EXPR_REG (indirect = 1/0)
 - a user identifier (T_ID): EXPR_ADDR | EXPR_CONST
 - a reserved ID (T_RES_ID): EXPR_CONST ( EXPR_ADDR if id=FLAT )

 valid user identifiers are
 - a TYPE (struct/union, typedef, record)
 - a STRUCT FIELD (also bitfield)
 - a variable (internal, external, stack ) or constant (EQU, '=')
 valid reserved IDs are types (BYTE, WORD, ... ) and FLAT
 */

static ret_code get_operand( expr_list *new, int *start, int end )
/*********************************************************************************/
{
    char        *tmp;
    int         i = *start;
    asm_sym     *sym2;
    dir_node    *dir;
    int         j;

    DebugMsg(("get_operand(start=%u, end=%u) enter\n", *start, end));
    switch( AsmBuffer[i]->token ) {
    case T_NUM:
        DebugMsg(("get_operand: T_NUM\n"));
        new->type = EXPR_CONST;
        new->llvalue = AsmBuffer[i]->llvalue;
        new->hlvalue = AsmBuffer[i]->hlvalue;
        break;
    case T_STRING:
        DebugMsg(("get_operand: T_STRING string=%s, value=%X\n", AsmBuffer[i]->string_ptr, AsmBuffer[i]->value));
        /* string enclosed in <> or {} are rejected since v1.94! */
        if ( AsmBuffer[i]->string_delim != '"' && AsmBuffer[i]->string_delim != '\'') {
            if ( new->is_opattr ) /* OPATTR operator accepts anything! */
                break;
            if ( error_msg )
                AsmErr( UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        new->type = EXPR_CONST;
        new->string = AsmBuffer[i]->string_ptr;
        //new->value = 0;
        tmp = new->string + 1; /* skip the quote */
        j = AsmBuffer[i]->value;
        /* string size exceeds 4? */
        if ( j > sizeof( int_32 ) ) {
            j = sizeof( int_32 );
            new->hvalue = -2; /* make it "invalid" for EQU and = */
        }
        for( ; j; tmp++,j-- ) {
            new->value <<= 8;
            new->value |= (*tmp);
        }
        break;
    case T_REG:
        DebugMsg(("get_operand: T_REG\n"));
        new->type = EXPR_REG;
        new->base_reg = i;
        j = AsmResWord[AsmBuffer[i]->value].position;
#if 1
        /* this check was previously done in the parser.
         Check if the register needs an extensions (which are bit masks).
         If no, then check if the cpu is sufficient.
         */
        if( ( ( AsmOpTable[j].cpu & P_EXT_MASK ) &&
            ((AsmOpTable[j].cpu & ModuleInfo.curr_cpu & P_EXT_MASK) == 0) ||
              ( ModuleInfo.curr_cpu & P_CPU_MASK ) < ( AsmOpTable[j].cpu & P_CPU_MASK ) ) ) {
            AsmError( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
            return( ERROR );
        }
#endif
        if( op_sq_bracket_level > 0 ) {
            /* a valid index register? */
            if ( AsmOpTable[j].allowed_prefix == AP_IREG ) {
                new->indirect = TRUE;
                /* opcode contains register number */
                if ( sym2 = GetStdAssume( AsmBuffer[i]->opcode ) ) {
                    /* skip "alias" types */
                    for ( ; sym2->type; sym2 = sym2->type );
                    new->assume = sym2;
                }
            } else if ( AsmOpTable[j].opnd_type[1] == OP_SR86 || AsmOpTable[j].opnd_type[1] == OP_SR386 ) {
                /* a segment register CS, SS, DS, ES, FS, GS? */
                if( AsmBuffer[i+1]->token != T_COLON ) {
                    CAsmError( INVALID_USE_OF_REGISTER );
                    return( ERROR );
                }
            } else {
                CAsmError( INVALID_USE_OF_REGISTER );
                return( ERROR );
            }
        } else if( AsmBuffer[i]->value == T_ST ) {

            expr_list   sti;

            // read st(i), put i into idx_reg
            i++;
            switch( AsmBuffer[i]->token ) {
            case T_OP_BRACKET:
            case T_OP_SQ_BRACKET:
            // case T_NUM: /* syntax "st 0" is NOT valid! */
                *start = i;
                init_expr( &sti );
                if( evaluate( &sti, start, end, PROC_OPERAND ) == ERROR ) {
                    return( ERROR );
                }
                if( sti.type != EXPR_CONST || sti.string != NULL ) {
                    CAsmError( CONSTANT_OPERAND_EXPECTED );
                    return( ERROR );
                }
                new->idx_reg = sti.value;
                DebugMsg(("get_operand exit, ok\n"));
                return( NOT_ERROR );
            default:
                new->idx_reg = 0; // st = st(0)
                break;
            }
        }
        break;
    case T_ID:
        if ( new->assume ) {
            DebugMsg(("get_operand: T_ID %s assume=%s\n", AsmBuffer[i]->string_ptr, new->assume->name));
            new->value = 0;
            /* skip a type specifier matching the variable's type */
            /* that's something like "var.TYPE.xxx" */
            if ( SymCmpFunc(new->assume->name, AsmBuffer[i]->string_ptr) == 0 ) {
                new->sym = new->assume;
                new->assume = NULL;
            } else {
                new->sym = SearchNameInStruct((asm_sym *)new->assume, AsmBuffer[i]->string_ptr, (unsigned int *)&new->value);
                DebugMsg(("get_operand: SearchNameInStruct()=%X, value=%u\n", new->sym, new->value));
            }
        } else {
            DebugMsg(("get_operand: T_ID %s\n", AsmBuffer[i]->string_ptr));
            /* ensure anonym labels are uppercase */
            if (*AsmBuffer[i]->string_ptr == '@' && *(AsmBuffer[i]->string_ptr+2) == '\0') {
                if (*(AsmBuffer[i]->string_ptr+1) == 'b' || *(AsmBuffer[i]->string_ptr+1) == 'B')
                    GetCurrAnonLabel(AsmBuffer[i]->string_ptr);
                else if (*(AsmBuffer[i]->string_ptr+1) == 'f' || *(AsmBuffer[i]->string_ptr+1) == 'F')
                    GetNextAnonLabel(AsmBuffer[i]->string_ptr);
            }
            new->sym = SymSearch( AsmBuffer[i]->string_ptr );
            if( new->sym == NULL || new->sym->state == SYM_MACRO || new->sym->state == SYM_TMACRO) {
#ifdef DEBUG_OUT
                // macros and text macros shouldn't occur here!
                if (new->sym) {
                    if (new->sym->state == SYM_MACRO) {
                        DebugMsg(("get_operand: internal error, macro >%s< found by expression evaluator\n", new->sym->name ));
                    } else if (new->sym->state == SYM_TMACRO) {
                        DebugMsg(("get_operand: internal error, text macro >%s< found by expression evaluator\n", new->sym->name ));
                    }
                }
#endif
                new->sym = NULL;
            }
        }
        if ( new->sym == NULL || new->sym->state == SYM_UNDEFINED ) {
            /* for OPATTR, an undefined symbol is ok */
            if ( new->is_opattr ) {
                new->type = EXPR_UNDEF;
                break;
            }
            if( Parse_Pass == PASS_1 ) {
                /* if it is EQU (error_msg==FALSE), return an error */
                if ( error_msg == FALSE ) {
                    DebugMsg(("get_operand for EQU: %s not defined\n", AsmBuffer[i]->string_ptr));
                    return( ERROR );
                }
                if ( new->sym == NULL ) {
                    new->sym = SymLookup( AsmBuffer[i]->string_ptr );
                    new->sym->state = SYM_UNDEFINED;
                }
                DebugMsg(("get_operand: %s not (yet) defined\n", AsmBuffer[i]->string_ptr));
            } else {
#ifdef DEBUG_OUT
                if (CurrProc)
                    DebugMsg(("get_operand: %s not defined, pass > 1, curr proc=>%s<, \n", AsmBuffer[i]->string_ptr, CurrProc->sym.name));
                else
                    DebugMsg(("get_operand: %s not defined, pass > 1, curr proc=NULL, \n", AsmBuffer[i]->string_ptr));
#endif
                if( error_msg)
                    if ( new->assume ) {
                        sprintf( StringBufferEnd, "%s.%s", new->assume->name, AsmBuffer[i]->string_ptr);
                        AsmErr( SYMBOL_NOT_DEFINED, StringBufferEnd );
                    } else {
                        AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
                    }
                return( ERROR );
            }
        }
        /* set default values */
        new->type = EXPR_ADDR;
        new->label = i;
        new->sym->used = TRUE;
        DebugMsg(("get_operand T_ID: sym->state=%u/type=%X/ofs=%X/memtype=%u,total_size=%u\n",
                  new->sym->state, new->sym->type, new->sym->offset, new->sym->mem_type, new->sym->total_size));
        switch ( new->sym->state ) {
        case SYM_TYPE:
            dir = (dir_node *)(new->sym);
            if ( dir->e.structinfo->isOpen == FALSE ) {
                new->type = EXPR_CONST;
                //new->mem_type = MT_ABS;
                new->mem_type = new->sym->mem_type;
                new->is_type = TRUE;
                DebugMsg(("get_operand: STRUCT/UNION/TYPEDEF/RECORD %s\n", new->sym->name ));
#if 1
                if (AsmBuffer[i-1]->token != T_DOT && AsmBuffer[i+1]->token != T_DOT)
                    new->value = new->sym->total_size;
#endif
            } else {
                DebugMsg(("get_operand: open STRUCT/UNION/TYPEDEF/RECORD %s\n", new->sym->name ));
                /* a valid constant should be returned if
                 1. the struct is open      AND
                 2. it's not an EQU operand
                 the number isn't used then (except if it's the first DUP operand)
                 */
                if ( error_msg == FALSE )
                    new->type = EXPR_UNDEF;
                else {
                    new->type = EXPR_CONST;
                    new->mem_type = MT_ABS;
                    new->value = -1;
                    new->is_type = TRUE;
                }
            }
            /* skip "alias" types */
            for ( sym2 = new->sym; sym2->type; sym2 = sym2->type );
            new->assume = sym2;
            new->sym = NULL;  /* sym must be NULL, it's not a label */
            //new->label = EMPTY; /* activate this? */
            break;
        case SYM_STRUCT_FIELD:
            DebugMsg(("get_operand: structure field: %s\n", new->sym->name));

            /* new->value might have been set by SearchNameInStruct() already! */
            new->value += new->sym->offset;
            new->type = EXPR_CONST;
            /* skip "alias" types */
            for ( sym2 = new->sym; sym2->type; sym2 = sym2->type );
            dir = (dir_node *)sym2;
            /*
             check if the member field has arbitrary type.
             If yes, set the <assume> member!
             It's probably better to handle this case in PrepareOp() for
             the T_DOT operator.
             */
            if (sym2->state == SYM_TYPE && dir->e.structinfo->typekind != TYPE_TYPEDEF ) {
                new->assume = sym2;
                if ( dir->e.structinfo->typekind == TYPE_RECORD )
                    new->mem_type = dir->sym.mem_type;
                DebugMsg(("get_operand: new current struct: %s, mem_type=%u\n", sym2->name, sym2->mem_type));
            } else {
                new->mem_type = sym2->mem_type;
                DebugMsg(("get_operand: mem_type=%u\n", new->mem_type ));
            }
            new->mbr = new->sym;
            new->sym = NULL;
            //new->label = EMPTY; /* activate this? */
            break;
        default: /* SYM_INTERNAL, SYM_EXTERNAL, SYM_STACK, SYM_PROC,... */
            /* remove the symbol reference if it's a constant and isn't external */
            if( new->sym->mem_type == MT_ABS && new->sym->state == SYM_INTERNAL ) {
                new->llvalue = new->sym->value;
                new->type = EXPR_CONST;
                new->sym = NULL;
                new->label = EMPTY;
            } else {
                if( new->sym->type ) { /* a variable with arbitrary type? */
                    /* skip "alias" types */
                    for ( sym2 = new->sym; sym2->type; sym2 = sym2->type );
                    new->mem_type = sym2->mem_type;
                } else {
                    new->mem_type = new->sym->mem_type;
                    if ( new->sym->mem_type == MT_ABS ) {
                        new->abs = TRUE;
                    }
                }
                /* since there is no fixup for auto variables, the "offset"
                 must be stored in the <value> field */
                if ( new->sym->state == SYM_STACK ) {
                    new->value = new->sym->offset;
                    new->indirect = TRUE;
                }
            }
            break;
        }
        break;
    case T_RES_ID:
        DebugMsg(("get_operand: T_RES_ID, >%s<, value=%X\n", AsmBuffer[i]->string_ptr, AsmBuffer[i]->value));
        /* for types, return the size as numeric constant */
        if ( AsmBuffer[i]->rm_byte == OP_TYPE ) {
            /* opcode contains index into SimpleType table */
            new->value = GetTypeSize( AsmBuffer[i]->opcode );
            new->mem_type = SimpleType[AsmBuffer[i]->opcode].mem_type;
            new->ofs_size = SimpleType[AsmBuffer[i]->opcode].ofs_size;
            new->type = EXPR_CONST;
            new->is_type = TRUE;
        } else if ( AsmBuffer[i]->value == T_FLAT ) {
            new->label = i;
            new->sym = SymSearch("FLAT");
            new->type = EXPR_ADDR;
        } else {
            if( error_msg )
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        break;
    default:
        DebugMsg(("get_operand: default, i=%d, string=%s\n", i, AsmBuffer[i]->string_ptr));
        if( error_msg )
            if (AsmBuffer[i]->token == T_BAD_NUM)
                AsmErr( NONDIGIT_IN_NUMBER, AsmBuffer[i]->string_ptr );
            else if ( AsmBuffer[i]->token == T_COLON )
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
            else
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    (*start)++;
    DebugMsg(("get_operand exit, ok, type=%X, value=%d, mem_type=%u, abs=%u, string=%X, assume=>%s<\n",
              new->type, new->value, new->mem_type, new->abs, new->string, new->assume ? new->assume->name : "NULL" ));
    return( NOT_ERROR );
}

static bool is_operator( int i )
/**************************/
/* determine if it is an operator */
{
    switch( AsmBuffer[i]->token ) {
    case T_RES_ID:
    case T_REG:
    case T_NUM:
    case T_ID:
    case T_STRING:
        return( FALSE );
    }
    return( TRUE );
}

static bool is_unary( int i, char sign )
/**************************************/
/* determine if it is an unary operator */
{
//    DebugMsg(("is_unary: i=%u, string=%s\n", i, AsmBuffer[i]->string_ptr));
    switch( AsmBuffer[i]->token ) {
    case T_UNARY_OPERATOR:
        return( TRUE );
    case T_POSITIVE: /* unary + op */
    case T_NEGATIVE: /* unary - op */
        return( TRUE );
    case '+':
        if( sign ) {
            AsmBuffer[i]->token = T_POSITIVE;
            return( TRUE );
        }
        break;
    case '-':
        if( sign ) {
            AsmBuffer[i]->token = T_NEGATIVE;
            return( TRUE );
        }
        break;
    }
    return( FALSE );
}

static bool check_same( expr_list *tok_1, expr_list *tok_2, int_8 type )
/**********************************************************************/
/* Check if both tok_1 and tok_2 equal type */
{
    if( tok_1->type == type &&
        tok_2->type == type ) {
        return( TRUE );
    } else {
        return( FALSE );
    }
}

static bool check_both( expr_list *tok_1, expr_list *tok_2, int_8 type1, int_8 type2 )
/************************************************************************************/
/* Check if tok_1 == type1 and tok_2 == type2 or vice versa */
{
    if( tok_1->type == type1 &&
        tok_2->type == type2 ) {
        return( TRUE );
    } else if( tok_1->type == type2 &&
               tok_2->type == type1 ) {
        return( TRUE );
    } else {
        return( FALSE );
    }
}

static void index_connect( expr_list *tok_1, expr_list *tok_2 )
/*************************************************************/
/* Connects the register lists */
{
    if( tok_1->base_reg == EMPTY ) {
        if( tok_2->base_reg != EMPTY ) {
            tok_1->base_reg = tok_2->base_reg;
            tok_2->base_reg = EMPTY;
        } else if( ( tok_2->idx_reg != EMPTY ) && ( tok_2->scale == 1 ) ) {
            tok_1->base_reg = tok_2->idx_reg;
            tok_2->idx_reg = EMPTY;
        }
    }
    if( tok_1->idx_reg == EMPTY ) {
        if( tok_2->idx_reg != EMPTY ) {
            tok_1->idx_reg = tok_2->idx_reg;
            tok_1->scale = tok_2->scale;
        } else if( tok_2->base_reg != EMPTY ) {
            tok_1->idx_reg = tok_2->base_reg;
            tok_1->scale = 1;
        }
    }
}

static void MakeConst( expr_list *token )
/***************************************/
{
    if( token->type != EXPR_ADDR )
        return;

    if( token->sym != NULL )
        /* added for v1.94: if the evaluator assumed an address because
         the label wasn't defined yet, then negate this */
        if ( Parse_Pass > PASS_1 || token->sym->state != SYM_UNDEFINED )
            return;

    token->label = EMPTY;
    if( token->mbr != NULL ) {
        if( token->mbr->state == SYM_STRUCT_FIELD ) {
        } else if( token->mbr->state == SYM_TYPE ) {
            token->value += token->mbr->total_size;
            token->mbr = NULL;
        } else {
            return;
        }
    }
    if( token->base_reg != EMPTY )
        return;
    if( token->idx_reg != EMPTY )
        return;
    if( token->override != EMPTY )
        return;
    token->instr = EMPTY;
    token->type = EXPR_CONST;
    token->indirect = FALSE;
    token->explicit = FALSE;
    token->mem_type = MT_EMPTY;
}

static void fix_struct_value( expr_list *token )
/**********************************************/
{
    if( token->mbr != NULL ) {
        if( token->mbr->state == SYM_TYPE ) {
            token->value += token->mbr->total_size;
            token->mbr = NULL;
        }
    }
}

static int check_direct_reg( expr_list *token_1, expr_list *token_2 )
/*******************************************************************/
{
    if( ( token_1->type == EXPR_REG ) && ( token_1->indirect == FALSE )
        || ( token_2->type == EXPR_REG ) && ( token_2->indirect == FALSE ) ) {
        return( ERROR );
    } else {
        return( NOT_ERROR );
    }
}

static ret_code calculate( expr_list *token_1, expr_list *token_2, int oper )
/**************************************************************************/
/* Perform the operation between token_1 and token_2
 <oper> is the index of the operator token.
 possible operators:
  T_POSITIVE (unary +)
  T_NEGATIVE (unary -)
  T_OP_BRACKET       is an alias for '+'
  T_OP_SQ_BRACKET    is an alias for '+'
  '+'
  T_DOT
  '-'
  '*'
  '/'
  T_COLON
  T_BINARY_OPERATOR ( PTR, MOD, GE, GT, LE, GT, EQ, NE, also AND, OR, XOR, SHL, SHR )
  T_UNARY_OPERATOR ( OFFSET, SHORT, ... , also NOT )
 */
{
    int                 temp;
    struct asm_sym      *sym;
    char                *name;

    DebugMsg(("calculate enter\n"));
    token_1->string = NULL;

    switch( AsmBuffer[oper]->token ) {
    case T_POSITIVE:
        /*
         * The only format allowed is:
         *        + constant
         */

        MakeConst( token_2 );
        if( token_2->type != EXPR_CONST ) {
            CAsmError( POSITIVE_SIGN_CONSTANT_EXPECTED );
            DebugMsg(("calculate exit 1, error\n"));
            return( ERROR );
        }
        token_1->type = EXPR_CONST;
        token_1->llvalue = token_2->llvalue;
        break;
    case T_NEGATIVE:
        DebugMsg(("calculate: T_NEGATIVE\n"));
        /*
         * The only format allowed is:
         *        - constant
         */

        MakeConst( token_2 );
        if( token_2->type != EXPR_CONST ) {
            CAsmError( NEGATIVE_SIGN_CONSTANT_EXPECTED );
            DebugMsg(("calculate exit 2, error\n"));
            return( ERROR );
        }
        token_1->type = EXPR_CONST;
        token_1->llvalue = -token_2->llvalue;
        break;
    case T_OP_BRACKET:
    case T_OP_SQ_BRACKET:
    case '+':
        DebugMsg(("calculate: '+' or '[' operator, token1.memtype=%u, token2.memtype=%u\n", token_1->mem_type, token_2->mem_type ));
        /*
         * The only formats allowed are:
         *        constant + constant
         *        constant + address
         *         address + register       ( only inside [] )
         *        register + register       ( only inside [] )
         *        register + constant       ( only inside [] )
         *        address  + address        ( only inside [] )
         */

        if( check_direct_reg( token_1, token_2 ) == ERROR ) {
            CAsmError( INVALID_USE_OF_REGISTER );
            DebugMsg(("calculate exit 3, error\n"));
            return( ERROR );
        }
        if( check_same( token_1, token_2, EXPR_CONST ) ) {

            token_1->llvalue += token_2->llvalue;
            token_1->labeldiff |= token_2->labeldiff;

        } else if( check_same( token_1, token_2, EXPR_ADDR ) ) {

            fix_struct_value( token_1 );
            fix_struct_value( token_2 );
            index_connect( token_1, token_2 );
            token_1->indirect |= token_2->indirect;
            if( token_2->sym != NULL ) {
                /* no error msg in pass one or in EQU mode */
                if ( ( token_1->sym != NULL ) && ( Parse_Pass > PASS_1 || error_msg == FALSE ) ) {
                    CAsmError( CANNOT_ADD_TWO_RELOC_LABELS );
                    DebugMsg(("calculate exit 4, error\n"));
                    return( ERROR );
                } else  {
                    token_1->label = token_2->label;
                    token_1->sym = token_2->sym;
                }
            }
            token_1->value += token_2->value;

        } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_ADDR ) ) {

            DebugMsg(("calculate CONST - ADDR\n"));
            if( token_1->type == EXPR_CONST ) {
                token_2->value += token_1->value;
                token_2->indirect |= token_1->indirect;
                if( token_1->explicit == TRUE ) {
                    token_2->explicit = TRUE;
                    token_2->mem_type = token_1->mem_type;
                } else if ( token_2->mem_type == MT_EMPTY )
                    token_2->mem_type = token_1->mem_type;
                TokenAssign( token_1, token_2 );
            } else {
                token_1->value += token_2->value;
            }
            fix_struct_value( token_1 );

        } else if( check_both( token_1, token_2, EXPR_ADDR, EXPR_REG ) ) {

            DebugMsg(("calculate ADDR - REG\n"));
            if( token_1->type == EXPR_REG ) {
#if 0
                /* why is this an error? [ecx+offset xxxtable] */
                if( token_2->instr != EMPTY ) {
                    CAsmError( LABEL_EXPECTED );
                    DebugMsg(("calculate exit 5, error\n"));
                    return( ERROR );
                }
#endif
                index_connect( token_2, token_1 );
                token_2->indirect |= token_1->indirect;
                TokenAssign( token_1, token_2 );
            } else {
                index_connect( token_1, token_2 );
                token_1->indirect |= token_2->indirect;
            }
            fix_struct_value( token_1 );

        } else if( check_same( token_1, token_2, EXPR_REG ) ) {

            index_connect( token_1, token_2 );
            token_1->indirect |= token_2->indirect;
            token_1->type = EXPR_ADDR;

        } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_REG ) ) {

            DebugMsg(("calculate CONST - REG\n"));
            if( token_2->type == EXPR_REG ) {
                token_1->base_reg = token_2->base_reg;
                token_1->idx_reg = token_2->idx_reg;
                token_2->base_reg = EMPTY;
                token_2->idx_reg = EMPTY;
            }

            token_1->value += token_2->value;
            token_1->indirect |= token_2->indirect;
            token_1->type = EXPR_ADDR;
        } else {
            CAsmError( CONSTANT_EXPECTED );
            DebugMsg(("calculate exit 6, error\n"));
            return( ERROR );
        }
        break;
    case T_DOT:
        DebugMsg(("calculate T_DOT\n"));
        /*
         * The only formats allowed are:
         *        register . address       ( only inside [] )
         *        address  . address
         *        address  . constant
         *
         *        constant . constant is now also allowed
         *        TYPE . TYPE | STRUCT_FIELD
         *        or
         *        <segreg>: [NUM] . STRUCT_FIELD
         */

        if( check_direct_reg( token_1, token_2 ) == ERROR ) {
            CAsmError( INVALID_USE_OF_REGISTER );
            DebugMsg(("calculate exit 7, error\n"));
            return( ERROR );
        }
        if( check_same( token_1, token_2, EXPR_ADDR ) ) {

            DebugMsg(("calculate T_DOT, ADDR - ADDR, t1.memtype=%u, t2.memtype=%u\n", token_1->mem_type, token_2->mem_type));

            index_connect( token_1, token_2 );
            token_1->indirect |= token_2->indirect;
            if( token_2->sym != NULL ) {
                if( (Parse_Pass > PASS_1) && (token_1->sym != NULL) ) {
                    CAsmError( SYNTAX_ERROR );
                    DebugMsg(("calculate exit 8, error\n"));
                    return( ERROR );
                } else {
                    token_1->label = token_2->label;
                    token_1->sym = token_2->sym;
                }
            }
            if( token_2->mbr != NULL ) {
                token_1->mbr = token_2->mbr;
            }
            token_1->value += token_2->value;
            if( token_1->explicit == FALSE ) {
                token_1->mem_type = token_2->mem_type;
            }

#ifdef DEBUG_OUT
                if (token_1->assume && token_2->assume)
                    DebugMsg(("calculate T_DOT, ADDR - ADDR, t1.assume=%X (%s), t2.assume=%X (%s)\n",
                              token_1->assume, token_1->assume->name, token_2->assume, token_2->assume->name));
                else if (token_1->assume)
                    DebugMsg(("calculate T_DOT, ADDR - ADDR, t1.assume=%X (%s), t2.assume=%X\n",
                              token_1->assume, token_1->assume->name, token_2->assume));
                else if (token_2->assume)
                    DebugMsg(("calculate T_DOT, ADDR - ADDR, t1.assume=%X, t2.assume=%X (%s)\n",
                              token_1->assume, token_2->assume, token_2->assume->name ));
                else
                    DebugMsg(("calculate T_DOT, ADDR - ADDR, t1.assume=%X, t2.assume=%X\n", token_1->assume, token_2->assume));
#endif
                if (token_2->assume)
                    token_1->assume = token_2->assume;

        } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_ADDR ) ) {

            if( token_1->type == EXPR_CONST ) {
                DebugMsg(("calculate T_DOT, CONST - ADDR, t1.memtype=%u, t2.memtype=%u\n", token_1->mem_type, token_2->mem_type));
                /* for TYPE.xxx, return offset instead of size */
                if ((token_1->mbr) && (token_1->mbr->state == SYM_TYPE))
                    token_1->value = token_1->mbr->offset;
                token_2->indirect |= token_1->indirect;
                token_2->value += token_1->value;
                TokenAssign( token_1, token_2 );
            } else {
                DebugMsg(("calculate T_DOT, ADDR - CONST, t1.memtype=%u, t2.memtype=%u\n", token_1->mem_type, token_2->mem_type));
                /* for [var].TYPE | STRUCT_FIELD, use offset instead of size */
                if (( token_2->mbr ) && (token_2->mbr->state == SYM_TYPE))
                    token_2->value = token_2->mbr->offset;
                token_1->value += token_2->value;
                if( token_2->mbr != NULL ) {
                    token_1->mbr = token_2->mbr;
                    if( token_1->explicit == FALSE )
                        token_1->mem_type = token_2->mem_type;  /* added */
                }
#ifdef DEBUG_OUT
                if (token_1->assume && token_2->assume)
                    DebugMsg(("calculate T_DOT, ADDR - CONST, t1.assume=%X (%s), t2.assume=%X (%s)\n",
                              token_1->assume, token_1->assume->name, token_2->assume, token_2->assume->name));
                else if (token_1->assume)
                    DebugMsg(("calculate T_DOT, ADDR - CONST, t1.assume=%X (%s), t2.assume=%X\n",
                              token_1->assume, token_1->assume->name, token_2->assume));
                else if (token_2->assume)
                    DebugMsg(("calculate T_DOT, ADDR - CONST, t1.assume=%X, t2.assume=%X (%s)\n",
                              token_1->assume, token_2->assume, token_2->assume->name ));
                else
                    DebugMsg(("calculate T_DOT, ADDR - CONST, t1.assume=%X, t2.assume=%X\n", token_1->assume, token_2->assume));
#endif
                if (token_2->assume)
                    token_1->assume = token_2->assume;
            }

        } else if( check_both( token_1, token_2, EXPR_ADDR, EXPR_REG ) ) {

            DebugMsg(("calculate T_DOT, ADDR - REG, t1.memtype=%u, t2.memtype=%u\n", token_1->mem_type, token_2->mem_type));

            if( token_1->type == EXPR_REG ) {
                if( token_2->instr != EMPTY ) {
                    CAsmError( LABEL_EXPECTED );
                    DebugMsg(("calculate exit 9, error\n"));
                    return( ERROR );
                }
                index_connect( token_2, token_1 );
                token_2->indirect |= token_1->indirect;
                TokenAssign( token_1, token_2 );
            } else {
                index_connect( token_1, token_2 );
                token_1->indirect |= token_2->indirect;
            }

        } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_REG ) ) {

            DebugMsg(("calculate T_DOT, CONST - REG, t1.memtype=%u, t2.memtype=%u\n", token_1->mem_type, token_2->mem_type));

            if( token_2->type == EXPR_REG ) {
                token_1->base_reg = token_2->base_reg;
                token_1->idx_reg = token_2->idx_reg;
                token_2->base_reg = EMPTY;
                token_2->idx_reg = EMPTY;
            } else {
                /* for [reg].TYPE.xxx, return offset instead of size */
                /* this is most likely obsolete */
                if (token_2->mbr) {
                    if (token_2->mbr->state == SYM_TYPE)
                        token_2->value = token_2->mbr->offset;
                    else if (token_1->mbr == NULL)
                        token_1->mbr = token_2->mbr;
                }

                //if( token_1->mem_type == MT_EMPTY)
                token_1->mem_type = token_2->mem_type;
                token_1->assume = token_2->assume;
            }
            token_1->value += token_2->value;
            token_1->indirect |= token_2->indirect;
            token_1->type = EXPR_ADDR;
        } else {
            if (token_1->type == EXPR_CONST && token_2->type == EXPR_CONST) {
                DebugMsg(("calculate T_DOT, CONST - CONST, t1.value=%u, memtype=%u, t2.value, memtype=%u\n", token_1->value, token_1->mem_type, token_2->value, token_2->mem_type));
                if (token_1->assume != NULL && token_2->mbr != NULL) {
                    /* old token is a type */
                    /* the value (=size) is ignored then */
                    token_1->value = token_2->value;
                    token_1->mbr = token_2->mbr;
                    token_1->is_type = FALSE;
                    /* either clear assume or use the renewed one */
                    if (token_1->assume != token_2->assume)
                        token_1->assume = token_2->assume;
                    else
                        token_1->assume = NULL;
                    break;
                } else if (token_2->mbr != NULL) {
                    /* old token is NOT a type */
                    /* most likely a number or an MT_ABS symbol! */
                    /* so the TOTAL of both constants is required */
                    token_1->value += token_2->value;
                    token_1->mbr = token_2->mbr;
                    token_1->mem_type = token_2->mem_type;
                    break;
                }
            }
            DebugMsg(("calculate T_DOT error: token_1->type=%u, token_2->type=%u\n", token_1->type, token_2->type));
            CAsmError( SYNTAX_ERROR );
            DebugMsg(("calculate exit 10, error\n"));
            return( ERROR );
        }
        break;
    case '-':
        /*
         * The only formats allowed are:
         *        constant - constant
         *         address - constant       ( only in this order )
         *         address - address
         *        register - constant       ( only inside [] and in this
         *                                    order )
         */

        DebugMsg(("calculate '-': types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));

        if( check_direct_reg( token_1, token_2 ) == ERROR ) {
            CAsmError( INVALID_USE_OF_REGISTER );
            DebugMsg(("calculate exit 11, error\n"));
            return( ERROR );
        }

        /* added for v1.94. It's related to the change done in MakeConst()!
         */
        if ( token_1->type == EXPR_ADDR &&
             token_2->type == EXPR_ADDR &&
             token_2->sym &&
             token_2->sym->state == SYM_UNDEFINED &&
             error_msg == TRUE )
            ; /* don't convert token2 to a constant! */
        else
            MakeConst( token_2 );

        if( check_same( token_1, token_2, EXPR_CONST ) ) {

            token_1->llvalue -= token_2->llvalue;

        } else if( token_1->type == EXPR_ADDR &&
                   token_2->type == EXPR_CONST ) {

            DebugMsg(("calculate '-': ADDR-CONST\n" ));
            token_1->value -= token_2->value;
            fix_struct_value( token_1 );

        } else if( check_same( token_1, token_2, EXPR_ADDR ) ){

            fix_struct_value( token_1 );
            fix_struct_value( token_2 );
            if( token_2->base_reg != EMPTY || token_2->idx_reg != EMPTY ) {
                CAsmError( INVALID_USE_OF_REGISTER );
                DebugMsg(("calculate exit 12, error\n"));
                return( ERROR );
            }
            if( token_2->label == EMPTY ) {
                token_1->value -= token_2->value;
                token_1->indirect |= token_2->indirect;
            } else {
                if( token_1->label == EMPTY || token_1->sym == NULL || token_2->sym == NULL ) {
                    CAsmError( SYNTAX_ERROR );
                    DebugMsg(("calculate exit 13, error\n"));
                    return( ERROR );
                }
                /* handle first operand */
                sym = token_1->sym;
//                if( Parse_Pass > PASS_1 && sym->defined == FALSE ) {
                if( Parse_Pass > PASS_1 && sym->state == SYM_UNDEFINED ) {
                    if( error_msg )
                        AsmErr( LABEL_NOT_DEFINED, sym->name );
                    DebugMsg(("calculate exit 14, error\n"));
                    return( ERROR );
                }
                token_1->value += sym->offset;
                /* handle second operand */
                sym = token_2->sym;
                if( Parse_Pass > PASS_1) {
                    if( sym->state == SYM_UNDEFINED ) {
                        if( error_msg )
                            AsmErr( LABEL_NOT_DEFINED, sym->name );
                        DebugMsg(("calculate exit 15, error\n"));
                        return( ERROR );
                    }
                    /* if symbol is external, error - unless it's the same symbol */
                    if ((sym->state == SYM_EXTERNAL ||
                         token_1->sym->state == SYM_EXTERNAL) &&
                        sym != token_1->sym) {
                        CAsmError(INVALID_USE_OF_EXTERNAL_SYMBOL);
                        return( ERROR );
                    }
                    /* check if the 2 offsets belong to the same segment */
                    if (sym->segment != token_1->sym->segment ) {
                        CAsmError( OPERANDS_MUST_BE_IN_SAME_SEGMENT );
                        return( ERROR );
                    }
                }
                token_1->value -= sym->offset;
                token_1->value -= token_2->value;
                token_1->label = EMPTY;
                token_1->sym = NULL;
                if( token_1->base_reg == EMPTY && token_1->idx_reg == EMPTY ) {

                    if( token_1->instr == T_OFFSET && token_2->instr == T_OFFSET )
                        token_1->instr = EMPTY;

                    token_1->type = EXPR_CONST;
#if FLAG_LABELDIFF
                    /* this flag tells EQU that it should tolerate a changed
                     value since it may be due to a phase error.
                     */
                    token_1->labeldiff = TRUE;
#endif
                    token_1->indirect = FALSE;
                } else {
                    DebugMsg(("calculate '-', exit, ADDR, base=%u, idx=%u\n", token_1->base_reg, token_1->idx_reg ));
                    token_1->type = EXPR_ADDR;
                    token_1->indirect |= token_2->indirect;
                }
                token_1->explicit = FALSE;
                token_1->mem_type = MT_EMPTY;
            }

        } else if( token_1->type == EXPR_REG &&
                   token_2->type == EXPR_CONST ) {

            token_1->value = -1 * token_2->value;
            token_1->indirect |= token_2->indirect;
            token_1->type = EXPR_ADDR;

        } else {
            DebugMsg(("calculate '-', exit, error: types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));
            CAsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        break;
    case '*':
        /*
         * The only formats allowed are:
         *        constant * constant
         *        register * scaling factor ( 1, 2, 4 or 8 )
         *                   386 only
         */
        DebugMsg(("calculate '*': types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));
        MakeConst( token_1 );
        MakeConst( token_2 );
        if( check_same( token_1, token_2, EXPR_CONST ) ) {
            token_1->llvalue *= token_2->llvalue;
        } else if( check_both( token_1, token_2, EXPR_REG, EXPR_CONST ) ) {
            if( check_direct_reg( token_1, token_2 ) == ERROR ) {
                CAsmError( INVALID_USE_OF_REGISTER );
                return( ERROR );
            }
            /* scaling factor */
            if( token_2->type == EXPR_REG ) {
                /* scale * reg */
                token_1->idx_reg = token_2->base_reg;
                token_1->base_reg = EMPTY;
                token_1->scale = token_1->value;
                token_1->value = 0;
                token_2->base_reg = EMPTY;
            } else {
                /* reg * scale */
                token_1->idx_reg = token_1->base_reg;
                token_1->base_reg = EMPTY;
                token_1->scale = token_2->value;
            }
            token_1->indirect |= token_2->indirect;
            token_1->type = EXPR_ADDR;
        } else {
            if( ( Parse_Pass > PASS_1 || error_msg == FALSE ) &&
                (token_1->type == EXPR_ADDR || token_2->type == EXPR_ADDR ) ) {
                CAsmError( CONSTANT_EXPECTED );
                DebugMsg(("calculate exit 17, error\n"));
                return( ERROR );
            }
            token_1->type = EXPR_CONST;
            break;
        }
        break;
    case '/':
        /*
         * The only formats allowed are:
         *        constant / constant
         */
        DebugMsg(("calculate '/': types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));
        MakeConst( token_1 );
        MakeConst( token_2 );
        if( check_same( token_1, token_2, EXPR_CONST ) ) {
            if (token_2->llvalue == 0)
                AsmError(DIVIDE_BY_ZERO_IN_EXPR);
            else
                token_1->llvalue /= token_2->llvalue;
        } else {
            if( (Parse_Pass > PASS_1 || error_msg == FALSE ) &&
                (token_1->type == EXPR_ADDR || token_2->type == EXPR_ADDR ) ) {
                CAsmError( CONSTANT_EXPECTED );
                DebugMsg(("calculate exit 18, error\n"));
                return( ERROR );
            }
            token_1->type = EXPR_CONST;
            break;
        }
        break;
    case T_COLON:
        /*
         * The only formats allowed are:
         *        register : anything ----- segment override
         *           label : address ( label = address with no offset
         *                             and no instruction attached;
         *                             also only segment or group is
         *                             allowed. )
         */
        DebugMsg(("calculate T_COLON: types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));
        if( token_2->override != EMPTY ) {
            CAsmError( MORE_THAN_ONE_OVERRIDE );
            DebugMsg(("calculate exit 19, error\n"));
            return( ERROR );
        }

        if( token_1->type == EXPR_REG ) {

            if( token_1->base_reg != EMPTY && token_1->idx_reg != EMPTY ) {
                CAsmError( INVALID_USE_OF_REGISTER );
                DebugMsg(("calculate exit 20, error\n"));
                return( ERROR );
            }
            token_2->override = token_1->base_reg;
            token_2->indirect |= token_1->indirect;
            if ( token_2->type == EXPR_CONST )
                token_2->type = EXPR_ADDR;
            if( token_1->explicit ) {
                token_2->explicit = token_1->explicit;
                token_2->mem_type = token_1->mem_type;
                token_2->ofs_size = token_1->ofs_size;
            }
            TokenAssign( token_1, token_2 );

            /*
             currently the assume token isn't copied by
             TokenAssign (which is probably just for historical reasons).
             So copy it manually!
             */
            token_1->assume = token_2->assume;

        } else if( token_1->type == EXPR_ADDR &&
                   /* token_2->type == EXPR_ADDR && */
                   token_1->override == EMPTY &&
                   token_1->instr == EMPTY &&
                   token_1->value == 0 &&
                   token_1->base_reg == EMPTY &&
                   token_1->idx_reg == EMPTY ) {

            sym = token_1->sym;
            if( sym == NULL ) {
                DebugMsg(("calculate exit 21, error\n"));
                CAsmError( SYNTAX_ERROR );
                return( ERROR );
            }
#if 0
            if( AsmBuffer[token_1->label]->token == T_RES_ID ) {
                /* Kludge for "FLAT" */
                AsmBuffer[token_1->label]->token = T_ID;
            }
#endif
            if( sym->state == SYM_GRP || sym->state == SYM_SEG ) {
                token_2->type = EXPR_ADDR;
                token_2->override = token_1->label;
                token_2->indirect |= token_1->indirect;
                if( token_1->explicit ) {
                    token_2->explicit = token_1->explicit;
                    token_2->mem_type = token_1->mem_type;
                    token_2->ofs_size = token_1->ofs_size;
                }
                TokenAssign( token_1, token_2 );
                token_1->assume = token_2->assume;

            } else if( Parse_Pass > PASS_1 || sym->state != SYM_UNDEFINED ) {
                CAsmError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
                DebugMsg(("calculate exit 22, error\n"));
                return( ERROR );
            }
        } else {
            CAsmError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
            DebugMsg(("calculate exit 24, error\n"));
            return( ERROR );
        }
        break; /* end case T_COLON */
    case T_BINARY_OPERATOR:
        DebugMsg(("calculate T_BINARY_OPERATOR: %s, types tok1=%u, tok2=%u\n", AsmBuffer[oper]->string_ptr, token_1->type, token_2->type ));

        if (AsmBuffer[oper]->value == T_PTR) {
            if ( token_1->is_type == FALSE ) {
                CAsmError( INVALID_TYPE_EXPRESSION );
                return( ERROR );
            }
            token_2->explicit = TRUE;
            token_2->mem_type = token_1->mem_type;
            token_2->ofs_size = token_1->ofs_size;
            if ( token_1->override != EMPTY ) {
                if ( token_2->override == EMPTY )
                    token_2->override = token_1->override;
                token_2->type = EXPR_ADDR;
            }
            //if ( token_1->mbr )
            //    token_2->mbr = token_1->mbr;
            //if ( token_1->sym )
            //    token_2->sym = token_1->sym;
            //token_2->instr = token_1->instr;
            TokenAssign( token_1, token_2 );
            break;
        }

        MakeConst( token_1 );
        MakeConst( token_2 );
        /* if it's EQ, NE, LE, ... and operands are 2 direct labels in
         the very same segment, take them as constants */

        if (token_1->type == EXPR_CONST && token_2->type == EXPR_CONST)
            ;
        else if (token_1->type == EXPR_ADDR &&
                 token_2->type == EXPR_ADDR &&
                 token_1->indirect == FALSE &&
                 token_2->indirect == FALSE &&
                 ( token_1->sym && token_2->sym &&
                   token_1->sym->segment == token_2->sym->segment)) {
            switch (AsmBuffer[oper]->value) {
            case T_EQ:
            case T_NE:
            case T_LT:
            case T_LE:
            case T_GT:
            case T_GE:
                token_1->type = EXPR_CONST;
                token_1->value += token_1->sym->offset;
                token_2->type = EXPR_CONST;
                token_2->value += token_2->sym->offset;
                break;
            default:
                CAsmError( CONSTANT_OPERAND_EXPECTED );
                DebugMsg(("calculate exit 28a, error\n"));
                return( ERROR );
            }
        } else {
            DebugMsg(("calculate: token1.type=%u, token2.type=%u\n", token_1->type, token_2->type));
            if ( token_1->type == EXPR_ADDR && token_1->indirect == FALSE )
                CAsmError( OPERAND_MUST_BE_RELOCATABLE );
            else
                CAsmError( CONSTANT_OPERAND_EXPECTED );
            DebugMsg(("calculate exit 28, error\n"));
            return( ERROR );
        }
        switch( AsmBuffer[oper]->value ) {
        case T_EQ:
            token_1->value = ( token_1->value == token_2->value ? -1:0 );
            break;
        case T_NE:
            token_1->value = ( token_1->value != token_2->value ? -1:0 );
            break;
        case T_LT:
            token_1->value = ( token_1->value < token_2->value ? -1:0 );
            break;
        case T_LE:
            token_1->value = ( token_1->value <= token_2->value ? -1:0 );
            break;
        case T_GT:
            token_1->value = ( token_1->value > token_2->value ? -1:0 );
            break;
        case T_GE:
            token_1->value = ( token_1->value >= token_2->value ? -1:0 );
            break;
        case T_MOD:
            token_1->llvalue %= token_2->llvalue;
            break;
        case T_SHL:
            if (token_1->hvalue == -1) {
                token_1->hvalue = 0;
                token_1->hlvalue = 0;
            }
            token_1->llvalue = token_1->llvalue << token_2->value;
            break;
        case T_SHR:
            if (token_1->hvalue == -1) {
                token_1->hvalue = 0;
                token_1->hlvalue = 0;
            }
            token_1->llvalue = token_1->llvalue >> token_2->value;
            break;
        case T_AND:
            token_1->llvalue &= token_2->llvalue;
            break;
        case T_OR:
            token_1->llvalue |= token_2->llvalue;
            break;
        case T_XOR:
            token_1->llvalue ^= token_2->llvalue;
            break;
        }
        break; /* end case T_BINARY_OPERATOR */
    case T_UNARY_OPERATOR:
        DebugMsg(("calculate T_UNARY_OPERATOR %s, i=%u, token_2 type=%u memtype=%X is_type=%u indirect=%u\n", AsmBuffer[oper]->string_ptr, oper, token_2->type, token_2->mem_type, token_2->is_type, token_2->indirect ));
        /* NOT has no valid flags in opnd_type[1]! */
        if( AsmBuffer[oper]->value == T_NOT ) {
            MakeConst( token_2 );
            if( token_2->type != EXPR_CONST ) {
                CAsmError( CONSTANT_OPERAND_EXPECTED );
                DebugMsg(("calculate exit 27, error\n"));
                return( ERROR );
            }
            TokenAssign( token_1, token_2 );
            token_1->llvalue = ~(token_2->llvalue);
            break;
        }

        /* operator         accepts
         ----------------------------------------------
         SIZEOF/SIZE        label, type, struct field
         LENGTHOF/LENGTH    label, struct field
         TYPE               label, type, struct field, register, number
         LOW                constant, label (OMF only)
         HIGH               constant, label (OMF only)
         LOWWORD            constant, label
         HIGHWORD           constant
         THIS               type
         OPATTR/.TYPE       label, type, struct field, register, number
         SHORT              label
         SEG                label
         OFFSET/LROFFSET    label, struct field, number
         IMAGEREL           label
         SECTIONREL         label
         WIDTH/MASK         bitfields or RECORD type
         */

        temp = AsmOpTable[AsmResWord[AsmBuffer[oper]->value].position].opnd_type[1];

        sym = token_2->sym;
        if( token_2->mbr != NULL )
            sym = token_2->mbr;

        if ( token_2->instr != EMPTY )
            name = AsmBuffer[oper]->pos + strlen( AsmBuffer[oper]->string_ptr );
        else if ( sym )
            name = sym->name;
        else if ( token_2->base_reg != EMPTY && token_2->indirect == FALSE )
            name = AsmBuffer[token_2->base_reg]->string_ptr;
        else
            name = AsmBuffer[oper]->pos + strlen( AsmBuffer[oper]->string_ptr );

        switch ( token_2->type ) {
        case EXPR_CONST:
            /* is item a type? */
            if ( token_2->is_type ) {
                if ( ( temp & AT_TYPE ) == 0 ) {
                    if ( error_msg )
                        AsmErr( INVALID_OPERAND_FOR_OPERATOR, AsmBuffer[oper]->string_ptr, name );
                    return( ERROR );
                }
                /* is item a struct field? */
            } else if ( token_2->mbr != NULL && token_2->mbr->state != SYM_TYPE ) {
                if ( token_2->mbr->mem_type == MT_BITS ) { /* bitfield? */
                    if ( ( temp & AT_BF ) == 0 ) {
                        if ( error_msg )
                            AsmErr( INVALID_OPERAND_FOR_OPERATOR, AsmBuffer[oper]->string_ptr, name );
                        return( ERROR );
                    }
                } else {
                    if ( ( temp & AT_FIELD ) == 0 ) {
                        if ( error_msg )
                            AsmErr( INVALID_OPERAND_FOR_OPERATOR, AsmBuffer[oper]->string_ptr, name );
                        return( ERROR );
                    }
                }
            } else { /*  or is it a number? */
                if ( ( temp & AT_NUM ) == 0 ) {
                    if ( error_msg )
                        AsmErr( INVALID_OPERAND_FOR_OPERATOR, AsmBuffer[oper]->string_ptr, name );
                    return( ERROR );
                }
            }
            break;
        case EXPR_ADDR:
            /* an indirect memory operand? (not an auto variable) */
            if ( token_2->indirect == TRUE && token_2->sym == NULL ) {
                if ( ( temp & AT_IND ) == 0) {
                    if ( error_msg )
                        AsmErr( INVALID_OPERAND_FOR_OPERATOR, AsmBuffer[oper]->string_ptr, name );
                    return( ERROR );
                }
            } else {
                if ( ( temp & AT_LABEL ) == 0) {
                    if ( error_msg )
                        AsmErr( INVALID_OPERAND_FOR_OPERATOR, AsmBuffer[oper]->string_ptr, name );
                    return( ERROR );
                }
            }

            if( token_2->instr != EMPTY ) {
                /* if instr is set, it's not a full address */
                switch (AsmBuffer[oper]->value) {
                case T_LOW:
                case T_HIGH:
                case T_LOWWORD:
                case T_HIGHWORD:
                case T_TYPE:
                    break;
                default:
                    CAsmError( LABEL_EXPECTED );
                    DebugMsg(("calculate exit 30, error\n"));
                    return( ERROR );
                }
            }
            break;
        case EXPR_REG:
            if ( ( temp & AT_REG ) == 0) {
                if ( error_msg )
                    AsmErr( INVALID_OPERAND_FOR_OPERATOR, AsmBuffer[oper]->string_ptr, name );
                return( ERROR );
            }
            break;
        }
        switch( AsmBuffer[oper]->value ) {
        case T_LENGTH:   /* number of items of first initializer */
        case T_SIZE:     /* size in bytes of first initializer */
        case T_LENGTHOF: /* number of elements in an array */
        case T_SIZEOF:   /* size in bytes of item (array/struct) */
            token_1->type = EXPR_CONST;

            token_1->label = EMPTY;
            token_1->sym = NULL;
            token_1->base_reg = EMPTY;
            token_1->idx_reg = EMPTY;
            token_1->override = EMPTY;
            token_1->instr = EMPTY;
            token_1->indirect = FALSE;
            token_1->explicit = FALSE;
            token_1->mem_type = MT_EMPTY;

            DebugMsg(("calculate SIZE/SIZEOF/LENGTH/LENGTHOF: sym=%X, mbr=%X, assume=>%s<\n",
                      token_2->sym, token_2->mbr, token_2->assume ? token_2->assume->name : "NULL" ));

#if 0
            if (token_2->assume) {
                sym = token_2->assume;
                token_2->assume = NULL;
            }
#endif
            /*  these 4 operators accept structure fields,
             stack variables and data labels.
             the old SIZE and LENGTH ops also accept code labels.
             */
            if ( sym ) {
                if ( sym->state == SYM_STRUCT_FIELD ||
                     sym->state == SYM_STACK )
                    ;
                else if ( sym->state == SYM_UNDEFINED && Parse_Pass == PASS_1 )
                    ;
                else if (( sym->state == SYM_EXTERNAL ||
                         sym->state == SYM_INTERNAL) &&
                        sym->mem_type != MT_ABS &&
                        sym->mem_type != MT_PROC &&
                        sym->mem_type != MT_FAR &&
                        sym->mem_type != MT_NEAR )
                    ;
                else if ( AsmBuffer[oper]->value == T_SIZE ||
                          AsmBuffer[oper]->value == T_LENGTH )
                    ;
                else {
                    CAsmError( EXPECTED_DATA_LABEL );
                    return( ERROR );
                }
            }

            switch( AsmBuffer[oper]->value ) {
            case T_LENGTH:
                if(token_2->type == EXPR_CONST ) {
                    token_1->value = token_2->mbr->first_length ? token_2->mbr->first_length : 1;
                } else if ( sym->state == SYM_EXTERNAL || sym->state == SYM_PROC ) {
                    token_1->value = 1;
                } else if( sym->mem_type == MT_EMPTY ) {
                    token_1->value = 0;
                } else {
                    token_1->value = sym->first_length ? sym->first_length : 1;
                }
                break;
            case T_LENGTHOF:
                /* LENGTHOF needs either a data label or a structure field */
                /* a TYPE (structure, typedef) is invalid */
                if( token_2->type == EXPR_CONST ) {
                    token_1->value = token_2->mbr->total_length;
                } else if( sym->state == SYM_UNDEFINED && Parse_Pass == PASS_1 ) {
                    token_1->value = sym->total_length;
                } else if ( sym->state == SYM_EXTERNAL ) {
                    /* for externals, total_length field is used otherwise */
                    token_1->value = 1;
                } else {
                    token_1->value = sym->total_length;
                }
                break;
            case T_SIZE:
                /* if it is a TYPE, first_size isn't set. then use
                 total_size.
                 */
                if( sym == NULL ) {
                    token_1->value = token_2->value;
                } else if( sym->state == SYM_TYPE ) {
                    token_1->value = sym->total_size;
                } else if( sym->mem_type == MT_NEAR ) {
                    token_1->value = SymIs32( sym ) ? 0xFF04 : 0xFF02;
                } else if( sym->mem_type == MT_FAR ) {
                    token_1->value = SymIs32( sym ) ? 0xFF06 : 0xFF04;
                } else {
                    DebugMsg(("calculate, SIZE: symbol %s, first_size=%u\n", sym->name, sym->first_size));
                    token_1->value = sym->first_size;
                }
                break;
            case T_SIZEOF:
#ifdef DEBUG_OUT
                if (sym)
                    DebugMsg(("calculate, sizeof: symbol %s, state=%u, size=%u\n", sym->name, sym->state, sym->total_size ));
                else
                    DebugMsg(("calculate, sizeof: symbol NULL, token2.value=%u\n", token_2->value ));
#endif
                /* if sym = NULL then operand is a type constant */
                if ( sym == NULL )
                    token_1->value = token_2->value;
                else
                    token_1->value = sym->total_size;
                break;
            }
            break;
        case T_TYPE:     /* size of one/longest item (array/struct) */
            token_1->type = EXPR_CONST;
            /* TYPE accepts arrays/structs/unions */
            if ( sym == NULL ) {
                DebugMsg(("calculate, TYPE, operand without symbol\n" ));
                /* for types, return total_size */
                if ( token_2->is_type == TRUE )
                    //token_1->value = token_2->value;
                    TokenAssign( token_1, token_2 );
                else if (token_2->type == EXPR_REG && token_2->indirect == FALSE ) {
                    token_1->value = SizeFromRegister(AsmBuffer[token_2->base_reg]->value);
#if 0 /* Masm returns 0 for TYPE <segment_register> */
                    /* if it is a segment register, use default word size */
                    if (token_1->value == 0)
                        token_1->value = Use32 ? 4 : 2;
#endif
                } else /* it is a number or EXPR_REG + indirect */
                    token_1->value = 0;
#if 0
            } else if ( sym->state == SYM_TYPE ) {
                DebugMsg(("calculate, TYPE, operand >%s< is type\n", sym->name ));
                TokenAssign( token_1, token_2 );
                token_1->assume = sym;
            } else if( sym->mem_type == MT_TYPE ) {
                DebugMsg(("calculate, TYPE, operand >%s< is structured variable\n", sym->name ));
                token_1->value = sym->type->total_size;
#endif
            } else {
                DebugMsg(("calculate, TYPE, operand >%s< is simple variable\n", sym->name ));
                //if ( token_2->explicit )
                    token_1->value = SizeFromMemtype( token_2->mem_type, SymIs32( sym ));
                //else
                //    token_1->value = SizeFromMemtype( sym->mem_type, SymIs32( sym ));
                if ( sym->mem_type == MT_NEAR ||
                     sym->mem_type == MT_FAR ||
                     sym->mem_type == MT_PROC)
                    token_1->value |= 0xFF00;
            }
            break;
        case T_DOT_TYPE: /* implement .TYPE as an alias for OPATTR */
        case T_OPATTR:
            token_1->type = EXPR_CONST;
            token_1->sym = NULL;  /* clear symbol in case it is undef */
            token_1->value = 0;
            token_1->mem_type = MT_EMPTY;
            if ( token_2->type == EXPR_EMPTY )
                break;
            if (token_2->sym != NULL &&
                token_2->sym->defined == TRUE &&
                token_2->sym->segment != NULL &&
                ((dir_node *)token_2->sym->segment)->e.seginfo->segtype == SEGTYPE_CODE)
                token_1->value |= 0x01; /* is code label */
            if (token_2->indirect == TRUE ||
                (token_2->sym != NULL &&
                 token_2->sym->defined == TRUE &&
                (token_2->sym->state == SYM_INTERNAL ||
                 token_2->sym->state == SYM_STACK ||
                 token_2->sym->state == SYM_EXTERNAL)))
                token_1->value |= 0x02; /* is memory variable or relocatable data label */
            if (token_2->type == EXPR_CONST)
                token_1->value |= 0x04; /* is immediate value */
            if (token_2->type == EXPR_ADDR &&
                token_2->indirect == FALSE &&
                token_2->base_reg == EMPTY &&
                (token_2->sym == NULL || token_2->sym->defined == TRUE))
                token_1->value |= 0x08; /* uses direct memory addressing */
            if (token_2->type == EXPR_REG && token_2->indirect == FALSE)
                token_1->value |= 0x10; /* is a register value */
            if (token_2->type != EXPR_UNDEF && (token_2->sym == 0 || token_2->sym->defined == TRUE))
                token_1->value |= 0x20; /* no reference to undefined label */
            if (token_2->sym && token_2->sym->state == SYM_STACK )
                token_1->value |= 0x40; /* it's an auto variable */
            if (token_2->sym && token_2->sym->state == SYM_EXTERNAL )
                token_1->value |= 0x80; /* it's an external label */
            if (AsmBuffer[oper]->value == T_OPATTR)
                if (token_2->sym)
                    token_1->value |= token_2->sym->langtype << 8;
            DebugMsg(("OPATTR returns %u\n", token_1->value));
            break;
        case T_SHORT:
            if ( token_2->type != EXPR_ADDR ||
                 ( token_2->mem_type != MT_EMPTY &&
                   token_2->mem_type != MT_NEAR &&
                   token_2->mem_type != MT_FAR &&
                   token_2->mem_type != MT_PROC ) ) {
                CAsmError( EXPRESSION_MUST_BE_A_CODE_ADDRESS );
                return( ERROR );
            }
            TokenAssign( token_1, token_2 );
            token_1->explicit = TRUE;
            token_1->mem_type = MT_SHORT;
            break;
        case T_SEG:
            if ( token_2->sym->state == SYM_STACK ) {
                CAsmError( OPERAND_MUST_BE_RELOCATABLE );
                return( ERROR );
            }
            TokenAssign( token_1, token_2 );
            token_1->instr = AsmBuffer[oper]->value;
            break;
        case T_OFFSET:
        case T_LROFFSET:
#if IMAGERELSUPP
        case T_IMAGEREL:
#endif
#if SECTIONRELSUPP
        case T_SECTIONREL:
#endif
#if 0
            if ( token_2->sym && token_2->sym->state == SYM_STACK ) {
                CAsmError( CANNOT_OFFSET_AUTO );
                return( ERROR );
            }
#endif
            /* offset operator accepts types, but returns always 0 */
            if ( token_2->is_type )
                token_2->value = 0;

            TokenAssign( token_1, token_2 );
            token_1->instr = AsmBuffer[oper]->value;

            /* skip memory type of operand, just address is needed */
            //token_1->mem_type = MT_EMPTY;
            //if ( token_2->type == EXPR_ADDR )
                token_1->mem_type = MT_NEAR;
            /* clear overrides ("offset SEG:xxx") */
            token_1->override = EMPTY;
            break;
        case T_LOWWORD:
            TokenAssign( token_1, token_2 );
            if (token_2->type == EXPR_ADDR && token_2->instr != T_SEG) {
                token_1->instr = T_LOWWORD;
                token_1->mem_type = MT_WORD;
            }
            token_1->value = token_1->value & 0xffff;
            break;
        case T_HIGHWORD:
            TokenAssign( token_1, token_2 );
            if (token_2->type == EXPR_ADDR && token_2->instr != T_SEG) {
                token_1->instr = T_HIGHWORD;
                token_1->mem_type = MT_WORD;
            }
            token_1->value = token_1->value >> 16;
            break;
        case T_LOW:
            TokenAssign( token_1, token_2 );
            if (token_2->type == EXPR_ADDR && token_2->instr != T_SEG) {
                /* COFF knows 16 and 32bit relocs only */
                if ( Options.output_format != OFORMAT_OMF && token_2->sym ) {
                    if ( error_msg )
                        AsmErr( SYMBOL_TYPE_CONFLICT, token_2->sym->name );
                    return( ERROR );
                }
                token_1->instr = T_LOW;
                token_1->mem_type = MT_EMPTY;
            }
            token_1->value = token_1->value & 0xff;
            break;
        case T_HIGH:
            TokenAssign( token_1, token_2 );
            if (token_2->type == EXPR_ADDR && token_2->instr != T_SEG) {
                if (Options.output_format != OFORMAT_OMF && token_2->sym) {
                    if ( error_msg )
                        AsmErr( SYMBOL_TYPE_CONFLICT, token_2->sym->name );
                    return( ERROR );
                }
                token_1->instr = T_HIGH;
                token_1->mem_type = MT_EMPTY;
            }
            token_1->value = token_1->value >> 8;
            token_1->value = token_1->value & 0xff;
            break;
        case T_THIS:
            if ( token_2->is_type == FALSE ) {
                CAsmError( INVALID_TYPE_EXPRESSION );
                return(ERROR);
            }
            if (thissym == NULL) {
                thissym = SymCreate("", FALSE);
                thissym->state = SYM_INTERNAL;
                thissym->defined = TRUE;
            }

            token_1->type = EXPR_ADDR;
            thissym->mem_type = token_2->mem_type;
            if ( token_2->sym && token_2->sym->mem_type == MT_TYPE )
                thissym->type = token_2->sym->type;
            token_1->sym  = thissym;
            SetSymSegOfs( thissym );
            token_1->mem_type = thissym->mem_type;
            break;
        case T_WIDTH:
        case T_MASK:
            /* additional check needed if operand is a type */
            if ( token_2->is_type ) {
                sym = token_2->assume;
                if ((( dir_node *)sym)->e.structinfo->typekind != TYPE_RECORD) {
                    CAsmError( OPERAND_MUST_BE_RECORD );
                    return( ERROR );
                }
            } else if (token_2->type == EXPR_CONST) {
                sym = token_2->mbr;
            } else {
                sym = token_2->sym;
            }
            if (AsmBuffer[oper]->value == T_MASK) {
                int i;
                token_1->value = 0;
                if ( token_2->is_type ) { /* get mask of the RECORD? */
                    dir_node *dir = (dir_node *)sym;
                    field_list *fl;
                    for ( fl = dir->e.structinfo->head; fl; fl = fl->next ) {
                        sym = fl->sym;
                        for (i = sym->offset ;i < sym->offset + sym->total_size; i++)
                            token_1->value |= 1 << i;
                    }
                } else { /* get mask of the bitfield */
                    for (i = sym->offset ;i < sym->offset + sym->total_size;i++)
                        token_1->value |= 1 << i;
                }
            } else {
                if ( token_2->is_type ) { /* get width of the RECORD? */
                    dir_node *dir = (dir_node *)sym;
                    field_list *fl;
                    for ( fl = dir->e.structinfo->head; fl; fl = fl->next )
                        token_1->value += fl->sym->total_size;
                } else
                    token_1->value = sym->total_size;
            }
            token_1->type = EXPR_CONST;
            break;
        default: /* shouldn't happen */
            CAsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        break; /* end case T_UNARY_OPERATOR */
    //case T_RES_ID:
    default: /* shouldn't happen */
        CAsmError( SYNTAX_ERROR );
        return( ERROR );
    } /* end switch( AsmBuffer[oper]->token ) */
    DebugMsg(("calculate exit, ok, token1->type=%X value=%d memtype=0x%X assume=>%s<\n",
              token_1->type, token_1->value, token_1->mem_type, token_1->assume ? token_1->assume->name : "NULL" ));
    return( NOT_ERROR );
}

// this code runs BEFORE the - right - operand of an operator is read

static void PrepareOp( int oper, expr_list *new, expr_list *old )
{
    switch ( AsmBuffer[oper]->token ) {
    case T_DOT:
        DebugMsg(("PrepareOp: DOT operator found, old.sym=%X, expr=%s\n", old->sym, AsmBuffer[oper]->pos + 1 ));
        if (old->assume) {
            DebugMsg(("PrepareOp: implicit assume: %s\n", old->assume->name));
            new->assume = old->assume;
        } else if (old->sym && old->sym->mem_type == MT_TYPE) {
            DebugMsg(("PrepareOp: implicit assume: %s\n", old->sym->type->name));
            for (new->assume = old->sym->type; new->assume->type; new->assume = new->assume->type);
        }
        break;
    case T_UNARY_OPERATOR:
        switch ( AsmBuffer[oper]->value ) {
        case T_OPATTR:
        case T_DOT_TYPE:
            new->is_opattr = TRUE;
        }
        break;
    }
}

#if 0
static bool cmp_token( int i, enum state tok )
/********************************************/
/* compare AsmBuffer[i] and tok */
{
    if( AsmBuffer[i]->token == tok ) {
        return( TRUE );
    } else {
        return( FALSE );
    }
}
#endif

#define IsCurrToken( tok )  ( AsmBuffer[*i]->token == tok )

static ret_code evaluate( expr_list *operand1, int *i, int end, enum process_flag proc_flag )
/*******************************************************************************************/
{
    char                token_needed;
    int                 curr_operator;
    bool                next_operator;
    expr_list           operand2;

    DebugMsg(("evaluate(i=%d, end=%d, operand.type=%d, sym=%X) enter\n", *i, end, operand1->type, operand1->sym));

    token_needed = FALSE;

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /* Look at first token, which may be an unary operator or an operand */
    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

    if( operand1->type == EXPR_EMPTY ) {

        if( IsCurrToken( T_OP_BRACKET ) ) {
            DebugMsg(("evaluate: OP_BRACKET \n"));
            (*i)++;
            if( *i > end ) {
                CAsmError( OPERAND_EXPECTED );
                DebugMsg(("evaluate exit 1, error, i=%d\n", *i ));
                return( ERROR );
            }
            if( evaluate( operand1, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 2, error, i=%d\n", *i ));
                return( ERROR );
            }
            if( !IsCurrToken( T_CL_BRACKET ) ) {
                CAsmError( MISSING_RIGHT_PARENTHESIS_IN_EXPRESSION );
                DebugMsg(("evaluate exit 3, error, i=%d\n", *i ));
                return( ERROR );
            }
            (*i)++;
        } else if( IsCurrToken( T_OP_SQ_BRACKET ) ) {
            DebugMsg(("evaluate: OP_SQ_BRACKET \n"));
            (*i)++;
            if( *i > end ) {
                CAsmError( OPERAND_EXPECTED );
                DebugMsg(("evaluate exit 4, error, i=%d\n", *i ));
                return( ERROR );
            }
            op_sq_bracket_level++;
            if( evaluate( operand1, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 5, error, i=%d\n", *i ));
                return( ERROR );
            }
            if( !IsCurrToken( T_CL_SQ_BRACKET ) ) {
                CAsmError( MISSING_RIGHT_PARENTHESIS_IN_EXPRESSION );
                DebugMsg(("evaluate exit 6, error, i=%d\n", *i ));
                return( ERROR );
            }
            op_sq_bracket_level--;
            (*i)++;
        } else if( is_unary( *i, TRUE ) ) {
            DebugMsg(("evaluate: unary operand detected: %s, value=%u\n", AsmBuffer[*i]->string_ptr, AsmBuffer[*i]->value));
            token_needed = TRUE;
        } else if( get_operand( operand1, i, end ) == ERROR ) {
            DebugMsg(("evaluate exit 7, error, i=%d\n", *i ));
            return( ERROR );
        }
    }

    /* If an unary operator wasn't found, read the operator now */

    if( !token_needed ) {
        /* is the expression a single item? */
        if( *i > end || IsCurrToken( T_CL_BRACKET ) || IsCurrToken( T_CL_SQ_BRACKET ) ) {
            DebugMsg(("evaluate exit, ok, type=%X value=%d string=%X memtype=%u indirect=%u assume=%X\n",
                      operand1->type,      operand1->value,
                      operand1->string,    operand1->mem_type,
                      operand1->indirect,  operand1->assume ));
            return( NOT_ERROR );
        }

        /* Read the operator. Must be binary or open bracket */

        if( !is_operator(*i) ) {
            CAsmError( OPERATOR_EXPECTED );
            DebugMsg(("evaluate exit 8, error, i=%d token=%X string=>%s<\n", *i, AsmBuffer[*i]->token, AsmBuffer[*i]->string_ptr ));
            return( ERROR );
        }
    }

    do {
        curr_operator = *i;
        DebugMsg(("evaluate loop enter, operator index=%u, token=%X, operand1->sym=%X\n", curr_operator, AsmBuffer[curr_operator]->token, operand1->sym));
        (*i)++;

        init_expr( &operand2 );
        PrepareOp( curr_operator, &operand2, operand1 );

        /* read the (next) operand */

        if( *i > end || IsCurrToken( T_CL_BRACKET )) {
            if ( operand2.is_opattr == TRUE )  /* OPATTR needs no operand */
                goto do_calc;
            CAsmError( OPERAND_EXPECTED );
            DebugMsg(("evaluate exit 9, error, i=%d, end=%d\n", *i, end));
            return( ERROR );
        }

        if( AsmBuffer[curr_operator]->token == T_OP_BRACKET ||
            AsmBuffer[curr_operator]->token == T_OP_SQ_BRACKET ) {
            int op;
            int sblvl = op_sq_bracket_level;
            if ( AsmBuffer[curr_operator]->token == T_OP_BRACKET)
                op = T_CL_BRACKET;
            else {
                op_sq_bracket_level++;
                op = T_CL_SQ_BRACKET;
            }
            if( evaluate( &operand2, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 10, error, i=%d\n", *i));
                return( ERROR );
            }
            if( !IsCurrToken( op ) ) {
                CAsmError( MISSING_RIGHT_PARENTHESIS_IN_EXPRESSION );
                DebugMsg(("evaluate exit 11, error, i=%d\n", *i ));
                return( ERROR );
            }
            op_sq_bracket_level = sblvl;
            (*i)++;
        } else if( IsCurrToken( T_OP_BRACKET ) || IsCurrToken( T_OP_SQ_BRACKET ) ) {
            int op;
            int sblvl = op_sq_bracket_level;
            if ( AsmBuffer[*i]->token == T_OP_BRACKET)
                op = T_CL_BRACKET;
            else {
                op_sq_bracket_level++;
                op = T_CL_SQ_BRACKET;
            }
            (*i)++;
            if( evaluate( &operand2, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 12, error, i=%d\n", *i ));
                return( ERROR );
            }
            if( !IsCurrToken( op ) ) {
                CAsmError( MISSING_RIGHT_PARENTHESIS_IN_EXPRESSION );
                DebugMsg(("evaluate exit 13, error, i=%d\n", *i ));
                return( ERROR );
            }
            op_sq_bracket_level = sblvl;
            (*i)++;

        } else if( is_unary( *i, TRUE ) ) {
            if( evaluate( &operand2, i, end, PROC_OPERAND ) == ERROR ) {
                DebugMsg(("evaluate exit 14, error, i=%d\n", *i ));
                return( ERROR );
            }
        } else if( is_operator( *i ) ) {
            CAsmError( OPERAND_EXPECTED );
            DebugMsg(("evaluate exit 15, error at token %u [%s]\n", *i, AsmBuffer[*i]->string_ptr));
            return( ERROR );
        } else if( get_operand( &operand2, i, end ) == ERROR ) {
            DebugMsg(("evaluate exit 16, error, i=%d\n", *i ));
            return( ERROR );
        }

        /* Look at the next operator and compare its priority with the
         current one. Continue to do so until an operator with a higher
         priority is found.
         */

        next_operator = FALSE;
        while( *i <= end && !IsCurrToken( T_CL_BRACKET) && !IsCurrToken( T_CL_SQ_BRACKET) ) {

            if( !is_operator( *i ) || is_unary( *i, FALSE ) ) {
                CAsmError( OPERATOR_EXPECTED );
                DebugMsg(("evaluate exit 17, error, i=%d\n", *i ));
                return( ERROR );
            }
            if( get_precedence( *i, PLUS_PRECEDENCE ) < get_precedence( curr_operator, BRACKET_PRECEDENCE ) ) {
                if( evaluate( &operand2, i, end, PROC_OPERAND ) == ERROR ) {
                    DebugMsg(("evaluate exit 18, error, i=%d\n", *i ));
                    return( ERROR );
                }
            } else {
                if( proc_flag == PROC_BRACKET )
                    next_operator = TRUE;
                break;
            }
        }
    do_calc:
        if( calculate( operand1, &operand2, curr_operator ) == ERROR ) {
            DebugMsg(("evaluate exit 19, error, i=%d\n", *i ));
            return( ERROR );
        }

    } while ( next_operator == TRUE ||
              ( proc_flag == PROC_BRACKET &&
                *i < end &&
                !IsCurrToken( T_CL_BRACKET ) &&
                !IsCurrToken( T_CL_SQ_BRACKET ) ) );

    DebugMsg(("evaluate exit, ok, type=%X value=%d string=%X memtype=0x%X indirect=%u assume=>%s<\n",
              operand1->type,     operand1->value,
              operand1->string,   operand1->mem_type,
              operand1->indirect, operand1->assume ? operand1->assume->name : "NULL" ));

    return( NOT_ERROR );
}

static bool is_expr_item( int i, int first )
/***************************/
/* Check if a token is a valid part of an expression
 also done here:
 T_INSTRUCTION  SHL, SHR, AND, OR, XOR changed to T_BINARY_OPERATOR
 T_INSTRUCTION  NOT                    changed to T_UNARY_OPERATOR
 T_RES_ID       PTR                    changed to T_BINARY_OPERATOR
 T_DIRECTIVE    PROC                   changed to T_RES_ID
 for the new operators the precedence is set */
{
    switch( AsmBuffer[i]->token ) {
    case T_INSTRUCTION:
        switch( AsmBuffer[i]->value ) {
        case T_SHL:
        case T_SHR:
            AsmBuffer[i]->token = T_BINARY_OPERATOR;
            AsmBuffer[i]->precedence = 8;
            return( TRUE );
        case T_NOT:
            AsmBuffer[i]->token = T_UNARY_OPERATOR;
            AsmBuffer[i]->precedence = 11;
            return( TRUE );
        case T_AND:
            AsmBuffer[i]->token = T_BINARY_OPERATOR;
            AsmBuffer[i]->precedence = 12;
            return( TRUE );
        case T_OR:
        case T_XOR:
            AsmBuffer[i]->token = T_BINARY_OPERATOR;
            AsmBuffer[i]->precedence = 13;
            return( TRUE );
        }
        return( FALSE );
    case T_RES_ID:
        if ( AsmBuffer[i]->rm_byte == OP_TYPE )
            break;
        else if ( AsmBuffer[i]->value == T_PTR ) {
            AsmBuffer[i]->token = T_BINARY_OPERATOR;
            AsmBuffer[i]->precedence = PTR_PRECEDENCE;
            break;
        } else if ( AsmBuffer[i]->value == T_FLAT ) {
            DefineFlatGroup();
            break;
        } else if ( AsmBuffer[i]->value == T_DUP ) /* DUP must terminate the expression */
            return( FALSE );

        /* other reserved words are changed to a T_ID */
        /* they have no meaning in an expression */
        /* ADDR, VARARG, BASIC, C, PASCAL, ... */

        AsmBuffer[i]->token = T_ID;
        break;
    case '+':
    case '-':
#if 1
        /* this can happen!: "db x dup(-1)" */
    case T_NEGATIVE:
    case T_POSITIVE:
#endif
        /* hack to stop expreval from hanging on floating point numbers */
        if( AsmBuffer[i+1]->token == T_FLOAT )
            return( FALSE );
        break;
    case T_DIRECTIVE:
        if ( AsmBuffer[i]->value == T_PROC ) { // PROC is converted to a type
            AsmBuffer[i]->token = T_RES_ID;
            AsmBuffer[i]->rm_byte = OP_TYPE;
            AsmBuffer[i]->opcode = ST_PROC;
            return( TRUE );
        }
        /* fall through. Other directives will end the expression */
    case T_COMMA:
    case T_FLOAT:
    case T_QUESTION_MARK:
        return( FALSE );
    case T_COLON: /* ':' is a special case. It "might" end an expression! */
        if ( i != first &&
             ( AsmBuffer[i-1]->token == T_REG ||
               AsmBuffer[i-1]->token == T_RES_ID ||
               AsmBuffer[i-1]->token == T_ID ) )
            return( TRUE );
        return( FALSE );
    }
    return( TRUE );
}

// evaluate an operand
// start_tok: index of first token of expression
// end_tok:   index of last  token of expression

extern ret_code EvalOperand( int *start_tok, int end_tok, expr_list *result, bool flag_msg )
/***********************************************************************************/
{
    int         i;
    int         first = *start_tok;

    DebugMsg(("EvalOperand(start=%u [token=%X], end=%u, flag_msg=%u) enter\n", *start_tok, AsmBuffer[*start_tok]->token, end_tok, flag_msg ));

    init_expr( result );

    for( i = first; i < end_tok; i++ ) {
        if( is_expr_item( i, first ) == FALSE )
            break;
    }
    if ( i == *start_tok )
        return( NOT_ERROR );

    op_sq_bracket_level = 0;
    error_msg = flag_msg;
    return ( evaluate( result, start_tok, i-1, PROC_BRACKET ) );
}

// global init (called once for each module)

void ExprEvalInit()
{
    thissym = NULL;
}
