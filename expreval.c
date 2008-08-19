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
#include "input.h"
#include "types.h"
#include "stddef.h"
#include "labels.h"

// USEDUMMY=1 uses a dummy symbol for unknown symbols.
// however, it's faster to create a true - undefined - symbol
// for forward references. This makes the assembler need less passes.
#define USEDUMMY 0

#define SETCURRSTR 0

extern void             DefineFlatGroup( void );

static int              op_sq_bracket_level;
static bool             error_msg;
static bool             is_opattr; /* accepts undefined symbols */
#if SETCURRSTR
static dir_node *       curr_struct;
#endif
#if USEDUMMY
static asm_sym *dummysym;
#endif
static asm_sym *thissym;

enum process_flag {
    PROC_BRACKET,
    PROC_OPERAND
};

static int evaluate( expr_list *, int *, int, enum process_flag );

static void init_expr( expr_list *new )
/*************************************/
{
    new->type     = EMPTY;
    new->value    = 0;
    new->hvalue   = 0;
    new->hlvalue  = 0;
    new->string   = NULL;
    new->base_reg = EMPTY;
    new->idx_reg  = EMPTY;
    new->label    = EMPTY;
    new->override = EMPTY;
    new->instr    = EMPTY;
    new->indirect = FALSE;
    new->explicit = FALSE;
    new->empty    = TRUE;
    new->abs      = FALSE;
    new->stackbased = FALSE;
#if FLAG_LABELDIFF
    new->labeldiff = FALSE;
#endif
    new->ofs_size = OFSSIZE_EMPTY;
    new->mem_type = MT_EMPTY;
    new->scale    = 1;
    new->sym      = NULL;
    new->mbr      = NULL;
    new->assume   = NULL;
}

static void TokenAssign( expr_list *t1, expr_list *t2 )
/*****************************************************/
{
#if 1
    // code deactivated because calculate ADDR.ADDR might also need
    // the current struct
//    if ((t2->type == EXPR_ADDR) || (t2->type == EXPR_REG && t2->indirect == TRUE))
//        memcpy(t1, t2, sizeof(expr_list));
//    else
        memcpy(t1, t2, offsetof(expr_list,assume));
#else
    t1->type     = t2->type;
    t1->value    = t2->value;
    t1->hvalue   = t2->hvalue;
    t1->hlvalue  = t2->hlvalue;
    t1->string   = t2->string;
    t1->base_reg = t2->base_reg;
    t1->idx_reg  = t2->idx_reg;
    t1->label    = t2->label;
    t1->override = t2->override;
    t1->instr    = t2->instr;
    t1->indirect = t2->indirect;
    t1->explicit = t2->explicit;
    t1->empty    = t2->empty;
    t1->abs      = t2->abs;
    t1->stackbased = t2->stackbased;
#if FLAG_LABELDIFF
    t1->labeldiff = t2->labeldiff;
#endif
    t1->mem_type = t2->mem_type;
    t1->scale    = t2->scale;
    t1->sym      = t2->sym;
    t1->mbr      = t2->mbr;
    t1->assume   = t2->assume;
#endif
}

#define PLUS_PRECEDENCE 9

static int get_precedence( int i )
/********************************/
{
    /* The following table is taken verbatim from MASM 6.1 Programmer's Guide,
     * page 14, Table 1.3. Sadly, it flatly contradicts QuickHelp online
     * documentation shipped with said product and should not be taken as gospel.
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
     * both MASM 6.0 and 6.1. Typical Microsoft mess.
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

    switch( AsmBuffer[i]->token ) {
    case T_UNARY_OPERATOR:
        return(AsmBuffer[i]->opcode);

    case T_INSTRUCTION:
        switch( AsmBuffer[i]->value ) {
        case T_MOD:
        case T_SHL:
        case T_SHR:
            return( 8 );
#if defined( _STANDALONE_ )
        case T_EQ:
        case T_NE:
        case T_LT:
        case T_LE:
        case T_GT:
        case T_GE:
            return( 10 );
#endif
        case T_NOT:
            return( 11 );
        case T_AND:
            return( 12 );
        case T_OR:
        case T_XOR:
            return( 13 );
        }
        break;
    case T_RES_ID:
        if (AsmBuffer[i]->rm_byte == OP_TYPE)
            return( 5 );
        else if (AsmBuffer[i]->value == T_PTR)
            return( 5 );
        break;
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
    case T_DOT:
        return( 2 );
    case T_OP_SQ_BRACKET:
        return( 1 );
    default:
        DebugMsg(("get_precedence: internal error with operator=%s\n", AsmBuffer[i]->string_ptr));
        AsmError(SYNTAX_ERROR);
        break;
    }
    return( ERROR );
}

static int getresidvalue(int value)
{
    switch( value ) {
    case T_BYTE: return 1;
    case T_WORD: return 2;
    case T_DWORD: return 4;
    case T_FWORD: return 6;
    case T_QWORD: return 8;
    case T_TBYTE: return 10;
    case T_OWORD: return 16;
    case T_NEAR: return (Use32 ? 4 : 2);
    case T_FAR: return (Use32 ? 6 : 4);
#if defined( _STANDALONE_ )
    case T_SBYTE: return 1;
    case T_SWORD: return 2;
    case T_REAL4:
    case T_SDWORD: return 4;
    case T_REAL8:  return 8;
    case T_REAL10:  return 10;
    case T_NEAR16: return 2;
    case T_NEAR32: return 4;
    case T_FAR16: return 4;
    case T_FAR32: return 6;
#endif
    case T_PTR: ; return (Use32 ? 4 : 2);
    }
    return 0;
}

static int get_operand( expr_list *new, int *start, int end )
/*********************************************************************************/
{
    char        *tmp;
    int         i = *start;
    int         j;
    bool        is32;

    DebugMsg(("get_operand(start=%u, end=%u) enter\n", *start, end));
//    init_expr( new );
    switch( AsmBuffer[i]->token ) {
    case T_NUM:
        DebugMsg(("get_operand: T_NUM\n"));
        new->empty = FALSE;
        new->type = EXPR_CONST;
        new->llvalue = AsmBuffer[i]->llvalue;
        new->hlvalue = AsmBuffer[i]->hlvalue;
        break;
    case T_STRING:
        DebugMsg(("get_operand: T_STRING string=%s, value=%X\n", AsmBuffer[i]->string_ptr, AsmBuffer[i]->value));
        new->empty = FALSE;
        new->type = EXPR_CONST;
        new->string = AsmBuffer[i]->string_ptr;
        new->value = 0;
        tmp = new->string;
        if (*tmp == '"' || *tmp == '\'')
            tmp++;
#if 0
        else {
            AsmError(UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION);
            return( ERROR );
        }
#endif
        for( j = AsmBuffer[i]->value; j; tmp++,j-- ) {
            new->value <<= 8;
            new->value |= (*tmp);
        }
        break;
    case T_REG:
        DebugMsg(("get_operand: T_REG\n"));
        new->empty = FALSE;
        new->type = EXPR_REG;
        new->base_reg = i;
        if( op_sq_bracket_level > 0 ) {
            asm_sym *sym;
            switch( AsmBuffer[i]->value ) {
            case T_EAX:
            case T_EBX:
            case T_ECX:
            case T_EDX:
            case T_EDI:
            case T_ESI:
            case T_EBP:
            case T_ESP:
            case T_BX:
            case T_BP:
            case T_DI:
            case T_SI:
                new->indirect = TRUE;
                /* opcode contains register number */
                if ( sym = GetStdAssume( AsmBuffer[i]->opcode ) ) {
                    for (; sym->type; sym = sym->type);
                    new->assume = sym;
                }
                break;
            case T_DS:
            case T_CS:
            case T_ES:
            case T_SS:
            case T_FS:
            case T_GS:
                if( AsmBuffer[i+1]->token != T_COLON ) {
                    if( error_msg )
                        AsmError( ILLEGAL_USE_OF_REGISTER );
                    new->type = EXPR_UNDEF;
                    return( ERROR );
                }
                break;
            default:
                if( error_msg )
                    AsmError( ILLEGAL_USE_OF_REGISTER );
                new->type = EXPR_UNDEF;
                return( ERROR );
            }
        } else if( AsmBuffer[i]->value == T_ST ) {

            expr_list   sti;

            // read st(i), put i into idx_reg
            i++;
            switch( AsmBuffer[i]->token ) {
            case T_OP_BRACKET:
            case T_OP_SQ_BRACKET:
            case T_NUM:
                *start = i;
                init_expr( &sti );
                if( evaluate( &sti, start, end, PROC_OPERAND ) == ERROR ) {
                    new->type = EXPR_UNDEF;
                    return( ERROR );
                }
                if( sti.type != EXPR_CONST ) {
                    if( error_msg )
                        AsmError( CONSTANT_OPERAND_EXPECTED );
                    new->type = EXPR_UNDEF;
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
#if defined( _STANDALONE_ )
#if SETCURRSTR
        if (curr_struct && i > 0 && (AsmBuffer[i-1]->token == T_DOT || AsmBuffer[i-1]->value == T_PTR)) {
            DebugMsg(("get_operand: T_ID %s curr_struct=%s\n", AsmBuffer[i]->string_ptr, curr_struct->sym.name));
            new->value = 0;
            new->sym = SearchNameInStruct((asm_sym *)curr_struct, AsmBuffer[i]->string_ptr, (unsigned int *)&new->value);
            DebugMsg(("get_operand: SearchNameInStruct()=%X, value=%u\n", new->sym, new->value));
        } else if (new->assume) {
#else
        if (new->assume) {
#endif
            DebugMsg(("get_operand: T_ID %s assume=%s\n", AsmBuffer[i]->string_ptr, new->assume->name));
            new->value = 0;
            /* skip a type specifier matching the variable's type */
            /* that's something like "var.TYPE.xxx" */
            if (SymCmpFunc(new->assume->name, AsmBuffer[i]->string_ptr) == 0) {
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
#if USEDUMMY
                if (!dummysym) {
                    dummysym = SymCreate("", FALSE);
#if 0
                    dummysym->state = SYM_INTERNAL;
                    dummysym->mem_type = MT_ABS;
#endif
                }
                dummysym->state = SYM_UNDEFINED;
                dummysym->mem_type = MT_EMPTY;
                dummysym->defined = FALSE;
#endif
            }
        }
        if (new->sym == NULL || new->sym->state == SYM_UNDEFINED) {
            /* for OPATTR, an undefined symbol is ok */
            if (is_opattr) {
                new->type = EXPR_UNDEF;
#if USEDUMMY
                new->sym = dummysym;
#endif
                break;
            }
            if( Parse_Pass == PASS_1 ) {
                /* if it is EQU (error_msg==FALSE), return an error */
                if (error_msg == FALSE) {
                    DebugMsg(("get_operand for EQU: %s not defined\n", AsmBuffer[i]->string_ptr));
                    new->type = EXPR_UNDEF;
                    return( ERROR );
                }
                if (new->sym == NULL) {
#if USEDUMMY
                    new->sym = dummysym;
#else
                    new->sym = SymLookup(AsmBuffer[i]->string_ptr);
                    new->sym->state = SYM_UNDEFINED;
#endif
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
#if SETCURRSTR
                    if (curr_struct) {
                        sprintf(CurrStringEnd, "%s.%s", curr_struct->sym.name, AsmBuffer[i]->string_ptr);
                        AsmErr( SYMBOL_NOT_DEFINED, CurrStringEnd );
#else
                    if (new->assume) {
                        sprintf(CurrStringEnd, "%s.%s", new->assume->name, AsmBuffer[i]->string_ptr);
                        AsmErr( SYMBOL_NOT_DEFINED, CurrStringEnd );
#endif
                    } else {
                        AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
                    }
                new->type = EXPR_UNDEF;
                new->sym = NULL;
                return( ERROR );
            }
        }
        /* set default values */
        new->type = EXPR_ADDR;
        new->label = i;
        if( new->sym != NULL ) {
            new->sym->used = TRUE;
            DebugMsg(("get_operand: sym->state=%u/type=%X/ofs=%X/memtype=%u,total_size=%u\n",
                      new->sym->state, new->sym->type, new->sym->offset, new->sym->mem_type, new->sym->total_size));
            if( new->sym->state == SYM_TYPE ) {
                dir_node *dir = (dir_node *)(new->sym);
                /* with types there is a problem. Sometimes the size */
                /* is needed, sometimes the "offset" */

                /* this 'if' isn't a good idea. a type may be used inside
                 a struct definition (sizeof <type>). it's ways better to
                 check the state of the isOpen member */
                // if (StructDef.struct_depth == 0) {
                if (dir->e.structinfo->isOpen == FALSE) {
                    asm_sym *sym2;
                    new->type = EXPR_CONST;
                    new->mem_type = MT_ABS;
                    DebugMsg(("get_operand: STRUCT/TYPEDEF used: %s\n", new->sym->name));
#if SETCURRSTR
                    new->value = new->sym->total_size;
                    if (AsmBuffer[i+1]->token == T_DOT) {
                        /* skip aliases */
                        for (sym2 = new->sym;sym2->type;sym2 = sym2->type);
                        curr_struct = (dir_node *)sym2;
                        DebugMsg(("get_operand: namespace extended (1) by %s\n", sym2->name));
                    } else if (AsmBuffer[i+1]->token == T_RES_ID && AsmBuffer[i+1]->value == T_PTR) {
                    } else {
                        DebugMsg(("get_operand: using a TYPEs mem_type\n"));
                        new->mem_type = new->sym->mem_type;
                    }
#else
                    if (AsmBuffer[i-1]->token != T_DOT && AsmBuffer[i+1]->token != T_DOT)
                        new->value = new->sym->total_size;
                    for (sym2 = new->sym;sym2->type;sym2 = sym2->type);
                    new->assume = sym2;
#endif
                } else {
                    if (dir->e.structinfo->isOpen) {
                        new->sym = NULL;
                        new->type = EXPR_UNDEF;
                        break;
                    } else
                        new->value = new->sym->offset;
                }
#if SETCURRSTR
                new->mbr = new->sym; /* save symbol here */
#endif
                new->sym = NULL;  /* sym must be NULL, it's not a label */
                new->empty = FALSE;
                break;
            } else if( new->sym->state == SYM_STRUCT_FIELD ) {
                dir_node *dir2;
                DebugMsg(("get_operand: structure field: %s\n", new->sym->name));
                /* new->value might have been set by SearchNameInStruct() already! */
                new->value += new->sym->offset;
                new->type = EXPR_CONST;
                /* skip aliases */
                for (dir2 = (dir_node *)(new->sym); dir2->sym.type; dir2 = (dir_node *)(dir2->sym.type));
                /* not a good idea to check for the DOT */
                /* the ID might be followed by a [..] before the DOT comes */
//                if (AsmBuffer[i+1]->token == T_DOT) {
                if (dir2->sym.state == SYM_TYPE && dir2->e.structinfo->typekind != TYPE_TYPEDEF ) {
#if SETCURRSTR
                    curr_struct = dir2;
#else
                    new->assume = (asm_sym *)dir2;
#endif
                    if ( dir2->e.structinfo->typekind == TYPE_RECORD )
                        new->mem_type = dir2->sym.mem_type;
                    DebugMsg(("get_operand: namespace extended (2) by %s, mem_type=%u\n", dir2->sym.name, dir2->sym.mem_type));
                } else {
                    new->mem_type = dir2->sym.mem_type;
                    DebugMsg(("get_operand: new mem_type=%u\n", new->mem_type));
                }
                new->mbr = new->sym;
                new->sym = NULL;
                new->empty = FALSE;
                break;
            } else if(new->sym->type) { /* a structured variable? */
                dir_node *dir2;
                /* skip aliases */
                for (dir2 = (dir_node *)(new->sym); dir2->sym.type; dir2 = (dir_node *)(dir2->sym.type));
                new->mem_type = dir2->sym.mem_type;
                if (new->sym->state == SYM_STACK) {
                    new->indirect = TRUE;
                    new->stackbased = TRUE;
                    new->value = new->sym->offset;
                }
            } else {
            /* remove the symbol reference if it's not external */
                if( new->sym->mem_type == MT_ABS && new->sym->state == SYM_INTERNAL) {
                    new->llvalue = new->sym->value;
                    new->type = EXPR_CONST;
                    new->sym = NULL;
                    new->label = EMPTY;
                } else {
                    new->mem_type = new->sym->mem_type;
                    if (new->sym->mem_type == MT_ABS) {
                        new->abs = TRUE;
                    }
                    if (new->sym->state == SYM_STACK) {
                        DebugMsg(("stackbased variable access\n"));
                        new->indirect = TRUE;
                        new->stackbased = TRUE;
                        new->value = new->sym->offset;
                    }
                }
            }
        }
#else
        new->label = i;
        new->type = EXPR_ADDR;
        new->sym = SymLookup( AsmBuffer[i]->string_ptr );
        new->mem_type = new->sym->mem_type;
#endif
        new->empty = FALSE;
        break;
    case T_RES_ID:
        DebugMsg(("get_operand: T_RES_ID, %s, value=%X\n", AsmBuffer[i]->string_ptr, AsmBuffer[i]->value));
        new->empty = FALSE;
        /* for types, return the size as numeric constant */
        if (new->value = getresidvalue(AsmBuffer[i]->value)) {
            new->mem_type = MT_ABS;
            new->type = EXPR_CONST;
        } else if (AsmBuffer[i]->value == T_FLAT) {
            new->label = i;
            new->sym = SymSearch("FLAT");
            new->type = EXPR_ADDR;
        } else {
//            new->label = i;
//            new->type = EXPR_ADDR;
            new->type = EXPR_UNDEF;
        }
        break;
    default:
        DebugMsg(("get_operand: default, i=%d, string=%s\n", i, AsmBuffer[i]->string_ptr));
        if( error_msg )
            if (AsmBuffer[i]->token == T_BAD_NUM)
                AsmErr( NONDIGIT_IN_NUMBER, AsmBuffer[i]->string_ptr );
            else
                AsmErr( SYNTAX_ERROR, AsmBuffer[i]->string_ptr );
        new->type = EXPR_UNDEF;
        return( ERROR );
    }
    (*start)++;
    DebugMsg(("get_operand exit, ok, type=%X, value=%d, mem_type=%u, abs=%u, string=%X \n", new->type, new->value, new->mem_type, new->abs, new->string));
    return( NOT_ERROR );
}

static bool is_optr( int i )
/**************************/
/* determine if it is an operator */
{
    switch( AsmBuffer[i]->token ) {
    case T_RES_ID:
        if (AsmBuffer[i]->value == T_PTR)
            return(TRUE);
    case T_REG:
    case T_NUM:
    case T_ID:
    case T_STRING:
    case T_PATH:
    case T_OP_BRACKET:
//    case T_CL_BRACKET:
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
        if (AsmBuffer[i]->value == T_OPATTR || AsmBuffer[i]->value == T_DOT_TYPE)
            is_opattr = TRUE;
        return( TRUE );
    case T_INSTRUCTION:
        if( AsmBuffer[i]->value == T_NOT )
            return( TRUE );
        break;
    case T_POSITIVE:
    case T_NEGATIVE:
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
    case T_RES_ID:
        if (AsmBuffer[i]->opcode) {
            if (AsmBuffer[i+1]->token == T_RES_ID && AsmBuffer[i+1]->value == T_PTR) //added for test!!!
                return( TRUE );
        } else if (AsmBuffer[i]->value == T_SHORT || AsmBuffer[i]->value == T_PTR)
            return( TRUE );
        break;
    default:
        break;
    }
//    DebugMsg(("is_unary: no\n"));
    return( FALSE );
}

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
        return;
    token->label = EMPTY;
    if( token->mbr != NULL ) {
#if defined( _STANDALONE_ )
        if( token->mbr->state == SYM_STRUCT_FIELD ) {
        } else if( token->mbr->state == SYM_TYPE ) {
            token->value += token->mbr->total_size;
            token->mbr = NULL;
        } else {
            return;
        }
#else
        return;
#endif
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
#if defined( _STANDALONE_ )
    if( token->mbr != NULL ) {
        if( token->mbr->state == SYM_TYPE ) {
            token->value += token->mbr->total_size;
            token->mbr = NULL;
        }
    }
#endif
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

static int calculate( expr_list *token_1, expr_list *token_2, uint_8 index )
/**************************************************************************/
/* Perform the operation between token_1 and token_2 */
{
    struct asm_sym      *sym;
    long                value;
    int                 idx;

    DebugMsg(("calculate enter\n"));
    token_1->string = NULL;

    switch( AsmBuffer[index]->token ) {
    case T_POSITIVE:
        /*
         * The only format allowed is:
         *        + constant
         */

        MakeConst( token_2 );
        if( token_2->type != EXPR_CONST ) {
            if( error_msg )
                AsmError( POSITIVE_SIGN_CONSTANT_EXPECTED );
            token_1->type = EXPR_UNDEF;
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
            if( error_msg )
                AsmError( NEGATIVE_SIGN_CONSTANT_EXPECTED );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 2, error\n"));
            return( ERROR );
        }
        token_1->type = EXPR_CONST;
        token_1->llvalue = -token_2->llvalue;
        break;
    case T_OP_SQ_BRACKET:
    case '+':
        DebugMsg(("calculate: '+' or '[' operator\n"));
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
            if( error_msg )
                AsmError( ILLEGAL_USE_OF_REGISTER );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 3, error\n"));
            return( ERROR );
        }
        if( check_same( token_1, token_2, EXPR_CONST ) ) {

            token_1->llvalue += token_2->llvalue;
            if (token_2->labeldiff)
                token_1->labeldiff = token_2->labeldiff;

        } else if( check_same( token_1, token_2, EXPR_ADDR ) ) {

            fix_struct_value( token_1 );
            fix_struct_value( token_2 );
            index_connect( token_1, token_2 );
            token_1->indirect |= token_2->indirect;
            if( token_2->sym != NULL ) {
                if ((Parse_Pass > PASS_1) && ( token_1->sym != NULL )) {
                    if( error_msg )
                        AsmError( CANNOT_ADD_TWO_RELOC_LABELS );
                    token_1->type = EXPR_UNDEF;
                    DebugMsg(("calculate exit 4, error\n"));
                    return( ERROR );
                } else  {
                    token_1->label = token_2->label;
                    token_1->sym = token_2->sym;
                }
            }
            token_1->value += token_2->value;

        } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_ADDR ) ) {

            if( token_1->type == EXPR_CONST ) {
                token_2->value += token_1->value;
                token_2->indirect |= token_1->indirect;
                if( token_1->explicit ) {
                    token_2->explicit |= token_1->explicit;
                    token_2->mem_type = token_1->mem_type;
                }
                TokenAssign( token_1, token_2 );
            } else {
                token_1->value += token_2->value;
            }
            fix_struct_value( token_1 );

        } else if( check_both( token_1, token_2, EXPR_ADDR, EXPR_REG ) ) {

            if( token_1->type == EXPR_REG ) {
#if 0
                /* why is this an error? [ecx+offset xxxtable] */
                if( token_2->instr != EMPTY ) {
                    if( error_msg )
                        AsmError( LABEL_IS_EXPECTED );
                    token_1->type = EXPR_UNDEF;
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
            /* Error */
            if( error_msg )
                AsmError( ADDITION_CONSTANT_EXPECTED );
            token_1->type = EXPR_UNDEF;
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
            if( error_msg )
                AsmError( ILLEGAL_USE_OF_REGISTER );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 7, error\n"));
            return( ERROR );
        }
        if( check_same( token_1, token_2, EXPR_ADDR ) ) {

            DebugMsg(("calculate T_DOT, ADDR - ADDR, t1.memtype=%u, t2.memtype=%u\n", token_1->mem_type, token_2->mem_type));

            index_connect( token_1, token_2 );
            token_1->indirect |= token_2->indirect;
            if( token_2->sym != NULL ) {
                if( (Parse_Pass > PASS_1) && (token_1->sym != NULL) ) {
                    if( error_msg )
                        AsmError( SYNTAX_ERROR );
                    token_1->type = EXPR_UNDEF;
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

#if SETCURRSTR == 0
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
#endif

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
#if SETCURRSTR == 0
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
#endif
            }

        } else if( check_both( token_1, token_2, EXPR_ADDR, EXPR_REG ) ) {

            DebugMsg(("calculate T_DOT, ADDR - REG, t1.memtype=%u, t2.memtype=%u\n", token_1->mem_type, token_2->mem_type));

            if( token_1->type == EXPR_REG ) {
                if( token_2->instr != EMPTY ) {
                    if( error_msg )
                        AsmError( LABEL_IS_EXPECTED );
                    token_1->type = EXPR_UNDEF;
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

                if( token_1->mem_type == MT_EMPTY)
                    token_1->mem_type = token_2->mem_type;
#if SETCURRSTR == 0
                token_1->assume = token_2->assume;
#endif
            }
            token_1->value += token_2->value;
            token_1->indirect |= token_2->indirect;
            token_1->type = EXPR_ADDR;
        } else {
            if (token_1->type == EXPR_CONST && token_2->type == EXPR_CONST) {
                DebugMsg(("calculate T_DOT, CONST - CONST, t1.value=%u, memtype=%u, t2.value, memtype=%u\n", token_1->value, token_1->mem_type, token_2->value, token_2->mem_type));
#if SETCURRSTR
                if (token_1->mbr != NULL && token_2->mbr != NULL) {
#else
                if (token_1->assume != NULL && token_2->mbr != NULL) {
#endif
                    /* old token is a type */
                    /* the value (=size) is ignored then */
                    token_1->value = token_2->value;
                    token_1->mbr = token_2->mbr;
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
            /* Error */
            if( error_msg )
                AsmError( SYNTAX_ERROR );
            token_1->type = EXPR_UNDEF;
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

        if( check_direct_reg( token_1, token_2 ) == ERROR ) {
            if( error_msg )
                AsmError( ILLEGAL_USE_OF_REGISTER );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 11, error\n"));
            return( ERROR );
        }
        MakeConst( token_2 );
        if( check_same( token_1, token_2, EXPR_CONST ) ) {

            token_1->llvalue -= token_2->llvalue;

        } else if( token_1->type == EXPR_ADDR &&
                   token_2->type == EXPR_CONST ) {

            token_1->value -= token_2->value;
            fix_struct_value( token_1 );

        } else if( check_same( token_1, token_2, EXPR_ADDR ) ){

            fix_struct_value( token_1 );
            fix_struct_value( token_2 );
            if( token_2->base_reg != EMPTY || token_2->idx_reg != EMPTY ) {
                if( error_msg )
                    AsmError( ILLEGAL_USE_OF_REGISTER );
                token_1->type = EXPR_UNDEF;
                DebugMsg(("calculate exit 12, error\n"));
                return( ERROR );
            }
            if( token_2->label == EMPTY ) {
                token_1->value -= token_2->value;
                token_1->indirect |= token_2->indirect;
            } else {
                if( token_1->label == EMPTY ) {
                    if( error_msg )
                        AsmError( SYNTAX_ERROR );
                    token_1->type = EXPR_UNDEF;
                    DebugMsg(("calculate exit 13, error\n"));
                    return( ERROR );
                }
                /* handle first operand */
                sym = token_1->sym;
                if( sym == NULL ) {
                    DebugMsg(("calculate exit 13a, error\n"));
                    return( ERROR );
                }
#if defined( _STANDALONE_ )
//                if( Parse_Pass > PASS_1 && sym->defined == FALSE ) {
                if( Parse_Pass > PASS_1 && sym->state == SYM_UNDEFINED ) {
                    if( error_msg )
                        AsmErr( LABEL_NOT_DEFINED, sym->name );
                    token_1->type = EXPR_UNDEF;
                    DebugMsg(("calculate exit 14, error\n"));
                    return( ERROR );
                }
                token_1->value += sym->offset;
#else
                token_1->value += sym->addr;
#endif
                /* handle second operand */
                sym = token_2->sym;
                if( sym == NULL ) {
                    DebugMsg(("calculate exit 14a, error\n"));
                    return( ERROR );
                }
#if defined( _STANDALONE_ )
                if( Parse_Pass > PASS_1) {
                    if( sym->state == SYM_UNDEFINED ) {
                        if( error_msg )
                            AsmErr( LABEL_NOT_DEFINED, sym->name );
                        token_1->type = EXPR_UNDEF;
                        DebugMsg(("calculate exit 15, error\n"));
                        return( ERROR );
                    }
                    /* if symbol is external, error - unless it's the same symbol */
                    if ((sym->state == SYM_EXTERNAL ||
                         token_1->sym->state == SYM_EXTERNAL) &&
                        sym != token_1->sym) {
                        if ( error_msg )
                            AsmError(INVALID_USE_OF_EXTERNAL_SYMBOL);
                        token_1->type = EXPR_UNDEF;
                        return( ERROR );
                    }
                    /* check if the 2 offsets belong to the same segment */
                    if (sym->segment != token_1->sym->segment ) {
                        if ( error_msg )
                            AsmError(OPERANDS_MUST_BE_IN_SAME_SEGMENT);
                        token_1->type = EXPR_UNDEF;
                        return( ERROR );
                    }
                }
                token_1->value -= sym->offset;
#else
                token_1->value -= sym->addr;
#endif
                token_1->value -= token_2->value;
                token_1->label = EMPTY;
                token_1->sym = NULL;
                if( token_1->base_reg == EMPTY && token_1->idx_reg == EMPTY ) {

                    if( token_1->instr == T_OFFSET && token_2->instr == T_OFFSET )
                        token_1->instr = EMPTY;

                    token_1->type = EXPR_CONST;
#if FLAG_LABELDIFF
                    token_1->labeldiff = TRUE;
#endif
                    token_1->indirect = FALSE;
                } else {
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
            /* Error */
            if( error_msg )
                AsmError( SUBTRACTION_CONSTANT_EXPECTED );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 16, error\n"));
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
        MakeConst( token_1 );
        MakeConst( token_2 );
        if( check_same( token_1, token_2, EXPR_CONST ) ) {
            token_1->llvalue *= token_2->llvalue;
        } else if( check_both( token_1, token_2, EXPR_REG, EXPR_CONST ) ) {
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
            if( Parse_Pass == PASS_1) {
                token_1->type = EXPR_CONST;
                break;
            }
            if( error_msg )
                AsmError( MULTIPLICATION_CONSTANT_EXPECTED );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 17, error\n"));
            return( ERROR );
        }
        break;
    case '/':
        /*
         * The only formats allowed are:
         *        constant / constant
         */
        MakeConst( token_1 );
        MakeConst( token_2 );
        if( check_same( token_1, token_2, EXPR_CONST ) ) {
            if (token_2->llvalue == 0)
                AsmError(DIVIDE_BY_ZERO_IN_EXPR);
            else
                token_1->llvalue /= token_2->llvalue;
        } else {
            if( Parse_Pass == PASS_1) {
                token_1->type = EXPR_CONST;
                break;
            }
            if( error_msg )
                AsmError( DIVISION_CONSTANT_EXPECTED );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 18, error\n"));
            return( ERROR );
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
        if( token_2->override != EMPTY ) {
            /* Error */
            if( error_msg )
                AsmError( MORE_THAN_ONE_OVERRIDE );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 19, error\n"));
            return( ERROR );
        }

        if( token_1->type == EXPR_REG ) {

            if( token_1->base_reg != EMPTY && token_1->idx_reg != EMPTY ) {
                if( error_msg )
                    AsmError( ILLEGAL_USE_OF_REGISTER );
                token_1->type = EXPR_UNDEF;
                DebugMsg(("calculate exit 20, error\n"));
                return( ERROR );
            }
            token_2->override = token_1->base_reg;
            token_2->indirect |= token_1->indirect;
            token_2->type = EXPR_ADDR;
            if( token_1->explicit ) {
                token_2->explicit = token_1->explicit;
                token_2->mem_type = token_1->mem_type;
            }
            TokenAssign( token_1, token_2 );

        } else if( token_2->type == EXPR_ADDR
            && token_1->type == EXPR_ADDR
            && token_1->override == EMPTY
            && token_1->instr == EMPTY
            && token_1->value == 0
            && token_1->base_reg == EMPTY
            && token_1->idx_reg == EMPTY ) {

            sym = token_1->sym;
            if( sym == NULL ) {
                DebugMsg(("calculate exit 21, error\n"));
                return( ERROR );
            }
#if defined( _STANDALONE_ )
            if( AsmBuffer[token_1->label]->token == T_RES_ID ) {
                /* Kludge for "FLAT" */
                AsmBuffer[token_1->label]->token = T_ID;
            }
            if( sym->state == SYM_GRP || sym->state == SYM_SEG ) {
                token_2->override = token_1->label;
                token_2->indirect |= token_1->indirect;
                if( token_1->explicit ) {
                    token_2->explicit = token_1->explicit;
                    token_2->mem_type = token_1->mem_type;
                }
                TokenAssign( token_1, token_2 );
            } else if( Parse_Pass > PASS_1 ) {
                if( error_msg )
                    AsmError( ONLY_SEG_OR_GROUP_ALLOWED );
                token_1->type = EXPR_UNDEF;
                DebugMsg(("calculate exit 22, error\n"));
                return( ERROR );
            }
#else
            if( error_msg )
                AsmError( ONLY_SEG_OR_GROUP_ALLOWED );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 23, error\n"));
            return( ERROR );
#endif
        } else {
            if( error_msg )
                AsmError( REG_OR_LABEL_EXPECTED_IN_OVERRIDE );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 24, error\n"));
            return( ERROR );
        }
        break;
    case T_RES_ID:
        DebugMsg(("calculate T_RES_ID (%s)\n", AsmBuffer[index]->string_ptr));
        if (AsmBuffer[index]->rm_byte == OP_TYPE) {
            if( ( AsmBuffer[index + 1]->token != T_RES_ID )
                || ( AsmBuffer[index + 1]->value != T_PTR ) ) {
                // Missing PTR operator
                if( error_msg )
                    AsmError( MISSING_PTR_OPERATOR );
                token_1->type = EXPR_UNDEF;
                DebugMsg(("calculate exit 25, error\n"));
                return( ERROR );
            }
            TokenAssign( token_1, token_2 );
            token_1->explicit = TRUE;
            if ((idx = FindSimpleType(AsmBuffer[index]->value)) != ERROR) {
                if (token_1->type == EXPR_REG && token_1->indirect == FALSE) {
                    int size;
                    size = SizeFromRegister(AsmBuffer[token_2->base_reg]->value);
                    /* segment register? Then both WORD and DWORD size is ok */
                    if (size == 0 && (SimpleType[idx].size == 2 || SimpleType[idx].size == 4))
                        ;
                    else if (size != SimpleType[idx].size) {
                        AsmError(INVALID_USE_OF_REGISTER);
                        return(ERROR);
                    }
                }
                token_1->mem_type = SimpleType[idx].mem_type;
                token_1->ofs_size = SimpleType[idx].ofs_size;
            } else if (AsmBuffer[index]->value == T_SHORT)
                token_1->mem_type = MT_SHORT; /* is SHORT a mem_type? */
        } else if (AsmBuffer[index]->value == T_PTR) {
            if( AsmBuffer[index - 1]->token == T_RES_ID ||
                AsmBuffer[index - 1]->token == T_DIRECTIVE) {
                idx = FindSimpleType(AsmBuffer[index - 1]->value);
                if (idx != ERROR) {
                    TokenAssign( token_1, token_2 );
                    token_1->mem_type = SimpleType[idx].mem_type;
                    token_1->ofs_size = SimpleType[idx].ofs_size;
                    token_1->explicit = TRUE;
//                    if( token_1->instr == T_OFFSET )
//                        token_1->instr = EMPTY;
                    break;
                }
            } else if( AsmBuffer[index - 1]->token == T_ID ) {
                asm_sym *sym;
                sym = SymSearch(AsmBuffer[index-1]->string_ptr);
                if (sym && sym->state == SYM_TYPE) {
                    TokenAssign( token_1, token_2 );
                    /* <type> ptr ... ? */
                    if (((dir_node *)sym)->e.structinfo->typekind != TYPE_TYPEDEF )
#if SETCURRSTR
                        curr_struct = (dir_node *)sym;
#else
                        token_1->assume = sym;
#endif
                    else {
                        token_1->explicit = TRUE;
                        token_1->mem_type = sym->mem_type;
                        token_1->mbr = sym;
                    }
                    break;
                }
            }

            // find 'ptr' but no 'byte', 'word' etc in front of it
            if( error_msg )
                AsmError( NO_SIZE_GIVEN_BEFORE_PTR_OPERATOR );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 26, error\n"));
            return( ERROR );
        } else if (AsmBuffer[index]->value == T_SHORT) {
            TokenAssign( token_1, token_2 );
            token_1->explicit = TRUE;
            token_1->mem_type = MT_SHORT;
        }
        break;
    case T_INSTRUCTION:
        DebugMsg(("calculate T_INSTRUCTION\n"));
        MakeConst( token_1 );
        MakeConst( token_2 );
        if( AsmBuffer[index]->value == T_NOT ) {
            if( token_2->type != EXPR_CONST ) {
                if( error_msg )
                    AsmError( CONSTANT_OPERAND_EXPECTED );
                token_1->type = EXPR_UNDEF;
                DebugMsg(("calculate exit 27, error\n"));
                return( ERROR );
            }
            token_1->type = EXPR_CONST;
        } else {
            /* if it's EQ, NE, LE, ... and operands are 2 direct labels in
             the very same segment, take them as constants */
            if (token_1->type == EXPR_CONST && token_2->type == EXPR_CONST)
                ;
            else if (token_1->type == EXPR_ADDR && token_2->type == EXPR_ADDR) {
                if (token_1->indirect == FALSE && token_2->indirect == FALSE)
                    if (token_1->sym && token_2->sym)
                        if (token_1->sym->segment == token_2->sym->segment)
                            switch (AsmBuffer[index]->value) {
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
                            }
            } else {
                DebugMsg(("calculate: token1.type=%u, token2.type=%u\n", token_1->type, token_2->type));
                if( error_msg )
                    if (token_1->type == EXPR_ADDR && token_1->indirect == FALSE)
                        AsmError( OPERAND_MUST_BE_RELOCATABLE );
                    else
                        AsmError( CONSTANT_OPERAND_EXPECTED );
                token_1->type = EXPR_UNDEF;
                DebugMsg(("calculate exit 28, error\n"));
                return( ERROR );
            }
        }
        switch( AsmBuffer[index]->value ) {
#if defined( _STANDALONE_ )
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
#endif
        case T_MOD:
            token_1->value %= token_2->value;
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
        case T_NOT:
            token_1->value = ~(token_2->value);
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
        break;
    case T_UNARY_OPERATOR:
        DebugMsg(("calculate T_UNARY_OPERATOR %s, i=%u\n", AsmBuffer[index]->string_ptr, index ));
        if( AsmBuffer[index]->value == T_OPATTR || AsmBuffer[index]->value == T_DOT_TYPE)
            is_opattr = FALSE; /* reset the flag */
        else if( token_2->type == EXPR_CONST ) {
            switch (AsmBuffer[index]->value) {
            case T_OFFSET:
            case T_LROFFSET:
#if IMAGERELSUPP
            case T_IMAGEREL:
#endif
            case T_SIZEOF: /* SIZEOF/SIZE and TYPE accept a type as well */
            case T_SIZE:
            case T_TYPE:
            case T_LENGTH: /* LENGTH accepts a structure field as well */
            case T_LENGTHOF: /* LENGTHOF accepts a structure field as well */
            case T_LOW:
            case T_HIGH:
            case T_LOWWORD:
            case T_HIGHWORD:
            case T_THIS: /* THIS requires a TYPE operand */
            case T_WIDTH:
            case T_MASK:
                break;
            default:
                if( error_msg )
                    AsmError( LABEL_IS_EXPECTED );
                token_1->type = EXPR_UNDEF;
                return( ERROR );
            }
        } else if( AsmBuffer[index]->value == T_TYPE && token_2->type == EXPR_REG ) {
            ;
        } else if( token_2->type != EXPR_ADDR ) {
            if( error_msg )
                AsmError( LABEL_IS_EXPECTED );
            token_1->type = EXPR_UNDEF;
            DebugMsg(("calculate exit 29, error\n"));
            return( ERROR );
        } else if( token_2->instr != EMPTY ) {
            /* if instr is set, it's not a full address */
            switch (AsmBuffer[index]->value) {
            case T_LOW:
            case T_HIGH:
            case T_LOWWORD:
//            case T_HIGHWORD:
                break;
            default:
                if( error_msg )
                    AsmError( LABEL_IS_EXPECTED );
                token_1->type = EXPR_UNDEF;
                DebugMsg(("calculate exit 30, error\n"));
                return( ERROR );
            }
        }
        switch( AsmBuffer[index]->value ) {
#if defined( _STANDALONE_ )
        case T_LENGTH:   /* number of items of first initializer */
        case T_SIZE:     /* size in bytes of first initializer */
        case T_TYPE:     /* size of one/longest item (array/struct) */
        case T_LENGTHOF: /* number of elements in an array */
        case T_SIZEOF:   /* size in bytes of item (array/struct) */
            token_1->label = EMPTY;
            token_1->sym = NULL;
            token_1->base_reg = EMPTY;
            token_1->idx_reg = EMPTY;
            token_1->override = EMPTY;
            token_1->instr = EMPTY;
            token_1->type = EXPR_CONST;
            token_1->indirect = FALSE;
            token_1->explicit = FALSE;
            token_1->mem_type = MT_EMPTY;
            sym = token_2->sym;
            DebugMsg(("calculate SIZE/SIZEOF/TYPE/LENGTH/LENGTHOF: sym=%X, mbr=%X, assume=%X\n", token_2->sym, token_2->mbr, token_2->assume));
            if( token_2->mbr != NULL )
                sym = token_2->mbr;
#if SETCURRSTR == 0
            if (token_2->assume) {
                sym = token_2->assume;
                token_2->assume = NULL;
            }
#endif
            /* sym = NULL might indicate that operand is a simple type */
            /* SIZEOF and TYPE accept those */
            /* TYPE additionally accepts registers */
            if( sym == NULL &&
                AsmBuffer[index]->value != T_SIZEOF &&
                AsmBuffer[index]->value != T_TYPE) {
                if( error_msg ) {
                    AsmError( INVALID_USE_OF_LENGTH_SIZE_OPERATOR );
                }
                DebugMsg(("calculate exit 31, error\n"));
                return( ERROR );
            }
            switch( AsmBuffer[index]->value ) {
            case T_LENGTH:
                if(token_2->type == EXPR_CONST ) {
                    if (token_2->mbr == NULL || token_2->mbr->state == SYM_TYPE) {
                        AsmError( LABEL_IS_EXPECTED );
                    } else
                        token_1->value = token_2->mbr->first_length ? token_2->mbr->first_length : 1;
                } else if (sym->state == SYM_EXTERNAL || sym->state == SYM_PROC) {
                    token_1->value = 1;
                } else if( sym->mem_type == MT_EMPTY ) {
                    token_1->value = 0;
                } else {
                    token_1->value = sym->first_length ? sym->first_length : 1;
                }
                break;
            case T_LENGTHOF:
                /* LENGTHOF needs either a true data label or a structure field */
                /* a TYPE (structure, typedef) is invalid */
                if(token_2->type == EXPR_CONST ) {
                    if (token_2->mbr == NULL || token_2->mbr->state == SYM_TYPE) {
                        AsmError( LABEL_IS_EXPECTED );
                    } else
                        token_1->value = token_2->mbr->total_length;
                } else if( sym->mem_type == MT_EMPTY ) {
                    token_1->value = 0;
                } else {
                    token_1->value = sym->total_length;
                }
                break;
            case T_SIZE:
                /* if it is a TYPE, first_size isn't set. then use
                 total_size.
                 */
                if( sym->state == SYM_TYPE )
                    token_1->value = sym->total_size;
                else if( sym->mem_type == MT_NEAR ) {
                    token_1->value = 0xFF02;
                } else if( sym->mem_type == MT_FAR ) {
                    token_1->value = 0xFF04;
                } else {
                    DebugMsg(("calculate, SIZE: symbol %s, first_size=%u\n", sym->name, sym->first_size));
                    token_1->value = sym->first_size;
                }
                break;
            case T_TYPE:
                /* TYPE accepts arrays/structs/unions */
                /* for types, return total_size */
                if (sym == NULL)
                    if (token_2->type == EXPR_REG) {
                        token_1->value = SizeFromRegister(AsmBuffer[token_2->base_reg]->value);
#if 0 /* Masm returns 0 for TYPE <segment_register> */
                        /* if it is a segment register, use default word size */
                        if (token_1->value == 0)
                            token_1->value = Use32 ? 4 : 2;
#endif
                    } else
                        token_1->value = token_2->value;
                else if (sym->state == SYM_TYPE )
                    token_1->value = sym->total_size;
                else if( sym->mem_type == MT_TYPE )
                    token_1->value = sym->type->total_size;
                else
                    token_1->value = SizeFromMemtype(sym->mem_type, Use32);
                break;
            case T_SIZEOF:
#ifdef DEBUG_OUT
                if (sym)
                    DebugMsg(("calculate, sizeof: symbol %s, state=%u, size=%u\n", sym->name, sym->state, sym->total_size));
                else
                    DebugMsg(("calculate, sizeof: symbol NULL\n"));
#endif
                /* for sizeof, just return the value for NUM consts */
                /* simple types (like DWORD) don't have a sym attached */
                if (sym == NULL)
                    token_1->value = token_2->value;
                else {
                    token_1->value = sym->total_size;
                    if (sym->mem_type == MT_TYPE || sym->state == SYM_TYPE)
                        ;// arbitrary types can have size 0
                    else if( Parse_Pass != PASS_1 && token_1->value == 0 ) {
                        if( error_msg )
                            AsmError( SIZEOF_NEEDS_TYPE_OR_DATA_LABEL );
                        token_1->type = EXPR_UNDEF;
                        DebugMsg(("calculate exit 32, error\n"));
                        return( ERROR );
                    }
                }
                break;
            }
            break;
        case T_DOT_TYPE: /* implement .TYPE as an alias for OPATTR */
        case T_OPATTR:
            token_1->value = 0;
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
            if (token_2->stackbased == TRUE)
                token_1->value |= 0x40; /* it's a stackbased variable */
            if (token_2->sym && token_2->sym->state == SYM_EXTERNAL)
                token_1->value |= 0x80; /* it's an external label */
            if (AsmBuffer[index]->value == T_OPATTR)
                if (token_2->sym)
                    token_1->value |= token_2->sym->langtype << 8;
            token_1->type = EXPR_CONST;
            token_1->sym = NULL;  /* clear symbol in case it is undef */
            DebugMsg(("OPATTR returns %u\n", token_1->value));
            token_1->mem_type = MT_EMPTY;
            break;
        case T_LROFFSET:
        case T_OFFSET:
#if IMAGERELSUPP
        case T_IMAGEREL:
#endif
            if (token_2->stackbased == TRUE) {
                AsmError( CANNOT_OFFSET_AUTO );
            }
            TokenAssign( token_1, token_2 );
            token_1->instr = AsmBuffer[index]->value;
            /* skip memory type of operand, just address is needed */
            token_1->mem_type = MT_EMPTY;
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
                if (Options.output_format != OFORMAT_OMF && token_2->sym)
                    AsmErr(SYMBOL_TYPE_CONFLICT, token_2->sym->name);
                token_1->instr = T_LOW;
                token_1->mem_type = MT_EMPTY;
            }
            token_1->value = token_1->value & 0xff;
            break;
        case T_HIGH:
            TokenAssign( token_1, token_2 );
            if (token_2->type == EXPR_ADDR && token_2->instr != T_SEG) {
                if (Options.output_format != OFORMAT_OMF && token_2->sym)
                    AsmErr(SYMBOL_TYPE_CONFLICT, token_2->sym->name);
                token_1->instr = T_HIGH;
                token_1->mem_type = MT_EMPTY;
            }
            token_1->value = token_1->value >> 8;
            token_1->value = token_1->value & 0xff;
            break;
        case T_THIS:
            // if( error_msg )
            //    AsmError( THIS_NOT_SUPPORTED );
            idx = ERROR;
            if (AsmBuffer[index+1]->token == T_RES_ID) {
                idx = FindSimpleType(AsmBuffer[index+1]->value);
                if (idx != ERROR)
                    idx = SimpleType[idx].mem_type;
            } else if (AsmBuffer[index+1]->token == T_ID) {
                sym = SymSearch(AsmBuffer[index+1]->string_ptr);
                if (sym && sym->state == SYM_TYPE)
                    idx = sym->mem_type;
            }
            if (idx == ERROR) {
                AsmError( INVALID_TYPE_EXPRESSION );
                return(ERROR);
            }
            token_1->type = EXPR_ADDR;
            if (thissym == NULL) {
                thissym = SymCreate("", FALSE);
                thissym->state = SYM_INTERNAL;
                thissym->defined = TRUE;
            }
            thissym->mem_type = idx;
            if (idx == MT_TYPE)
                thissym->type = sym->type;
            token_1->sym  = thissym;
            SetSymSegOfs( thissym );
            token_1->mem_type = thissym->mem_type;
            break;
        case T_WIDTH:
        case T_MASK:
            if (token_2->type == EXPR_ADDR)
                sym = token_2->sym;
            else if (token_2->type == EXPR_CONST)
                sym = token_2->mbr;
            else
                sym = NULL;
            if (sym == NULL || sym->mem_type != MT_BITS) {
                if( error_msg )
                    AsmError( OPERAND_MUST_BE_RECORD );
                return( ERROR );
            }
            if (AsmBuffer[index]->value == T_MASK) {
                int i;
                token_1->value = 0;
                for (i = sym->offset ;i < sym->offset + sym->total_size;i++)
                    token_1->value |= 1 << i;
            } else {
                token_1->value = sym->total_size;
            }
            token_1->type = EXPR_CONST;
            break;
#endif
        default:
            TokenAssign( token_1, token_2 );
            token_1->instr = AsmBuffer[index]->value;
            break;
        }
        break;
    }
    token_1->empty = FALSE;
    DebugMsg(("calculate exit, ok, token1->type=%X,value=%d,memtype=%u,assume=%X\n", token_1->type, token_1->value, token_1->mem_type, token_1->assume));
    return( NOT_ERROR );
}

// if there's the DOT operator and the left operand is a structured variable
// use it for assume

static void PrepareExpr(int operator, expr_list *new, expr_list *old)
{
#if SETCURRSTR
    if (operator == T_DOT)
        if (old->sym && old->sym->mem_type == MT_TYPE) {
            DebugMsg(("PrepareExpr: implicit assume: %s\n", old->sym->type->name));
            for (new->assume = old->sym->type; new->assume->type; new->assume = new->assume->type);
        } else if (old->assume)
            new->assume = old->assume;
#else
    if (operator == T_DOT)
        if (old->assume) {
            DebugMsg(("PrepareExpr: implicit assume: %s\n", old->assume->name));
            new->assume = old->assume;
        } else if (old->sym && old->sym->mem_type == MT_TYPE) {
            DebugMsg(("PrepareExpr: implicit assume: %s\n", old->sym->type->name));
            for (new->assume = old->sym->type; new->assume->type; new->assume = new->assume->type);
        }
#endif
}

static int evaluate(
    expr_list *operand1,
    int *i,
    int end,
    enum process_flag proc_flag )
/******************************/
{
    expr_list           operand2;
    char                token_needed;
    int                 curr_operator;
    int                 next_operator;
    int                 op_sq_bracket;

    DebugMsg(("evaluate(i=%d, end=%d, operand.empty=%u, sym=%X) enter\n", *i, end, operand1->empty, operand1->sym));
    token_needed = FALSE;
    curr_operator = EMPTY;
    op_sq_bracket = op_sq_bracket_level;

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /* Look at first token, which may be an unary operator or an operand */
    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

    if( operand1->empty ) {
        if( cmp_token( *i, T_OP_BRACKET ) ) {
            DebugMsg(("evaluate: OP_BRACKET \n"));
            (*i)++;
            if( *i > end ) {
                if( error_msg )
                    AsmError( OPERAND_EXPECTED );
                operand1->type = EXPR_UNDEF;
                DebugMsg(("evaluate exit 20, error\n"));
                return( ERROR );
            }
            if( evaluate( operand1, i, end, PROC_BRACKET) == ERROR ) {
                DebugMsg(("evaluate exit 19, error\n"));
                return( ERROR );
            }
            if( cmp_token( *i, T_CL_SQ_BRACKET ) ) {
                // error open ( close ]
                if( error_msg )
                    AsmError( BRACKETS_NOT_BALANCED );
                operand1->type = EXPR_UNDEF;
                DebugMsg(("evaluate exit 18, error\n"));
                return( ERROR );
            }
            (*i)++;
        } else if( is_unary( *i, TRUE ) ) {
            DebugMsg(("evaluate: unary operand detected: %s, value=%u\n", AsmBuffer[*i]->string_ptr, AsmBuffer[*i]->value));
            token_needed = TRUE;
        } else if( cmp_token( *i, T_OP_SQ_BRACKET ) ) {
            DebugMsg(("evaluate: OP_SQ_BRACKET \n"));
            if( *i == 0 ) {
                DebugMsg(("evaluate exit 17, error\n"));
                return( ERROR );
            }
            /**/myassert( !cmp_token( (*i)-1, T_CL_BRACKET ) );
            (*i)++;
            if( *i > end ) {
                if( error_msg )
                    AsmError( OPERAND_EXPECTED );
                operand1->type = EXPR_UNDEF;
                DebugMsg(("evaluate exit 16, error\n"));
                return( ERROR );
            }
            op_sq_bracket_level++;
            if( evaluate( operand1, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 15, error\n"));
                return( ERROR );
            }
            if( cmp_token( *i, T_CL_BRACKET ) ) {
                // error open [ close )
                if( error_msg )
                    AsmError( BRACKETS_NOT_BALANCED );
                operand1->type = EXPR_UNDEF;
                DebugMsg(("evaluate exit 14, error\n"));
                return( ERROR );
            }
            if( cmp_token( *i, T_CL_SQ_BRACKET ) ) {
                op_sq_bracket_level--;
            }
            (*i)++;
        } else if( get_operand( operand1, i, end ) == ERROR ) {
            DebugMsg(("evaluate exit 13, error\n"));
            return( ERROR );
        }
    } else {
        token_needed = FALSE;
    }

    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /* If an unary operator is not found, now read the operator */
    /*::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

    if( !token_needed ) {
        if( *i > end ) {
            /* no operator is found; result is in operand1 */
            if( op_sq_bracket_level != op_sq_bracket ) {
                // error "missing ]"
                if( error_msg )
                    AsmError( BRACKETS_NOT_BALANCED );
                operand1->type = EXPR_UNDEF;
                DebugMsg(("evaluate exit 8, error\n"));
                return( ERROR );
            } else {
                goto noterror;
            }
        }
        /* Read the operator */
        if( cmp_token( *i, T_CL_BRACKET ) ) {
            if( op_sq_bracket_level != op_sq_bracket ) {
                // error close ) but [ is open
                if( error_msg )
                    AsmError( BRACKETS_NOT_BALANCED );
                operand1->type = EXPR_UNDEF;
                DebugMsg(("evaluate exit 7, error\n"));
                return( ERROR );
            } else {
                goto noterror;
            }
        } else if( cmp_token( *i, T_CL_SQ_BRACKET ) ) {
            goto noterror;
#if 0
        } else if( cmp_token( *i, T_OP_SQ_BRACKET ) ) {
            /* WHY????????? */
            AsmBuffer[*i]->token = '+';
            op_sq_bracket_level++;
#endif
        } else if( !is_optr(*i) ) {
            if( error_msg ) {
                AsmError( OPERATOR_EXPECTED );
            }
            operand1->type = EXPR_UNDEF;
            DebugMsg(("evaluate exit 6, error, token=%X, string=%s\n", AsmBuffer[*i]->token, AsmBuffer[*i]->string_ptr));
            return( ERROR );
        }
    }

    do {
        curr_operator = *i;
        DebugMsg(("evaluate loop enter, operator index=%u, token=%X, operand1->sym=%X\n", curr_operator, AsmBuffer[curr_operator]->token, operand1->sym));
        (*i)++;

        /* read the next operand */

        if( *i > end ) {
            if( error_msg )
                AsmError( OPERAND_EXPECTED );
            operand1->type = EXPR_UNDEF;
            DebugMsg(("evaluate exit 1, error, i=%d, end=%d\n", *i, end));
            return( ERROR );
        }

        init_expr( &operand2 );
#if 1
        if (AsmBuffer[curr_operator]->token == T_DOT) {
            DebugMsg(("evaluate: DOT operator found, op1.sym=%X\n", operand1->sym));
            PrepareExpr(T_DOT, &operand2, operand1);
        }
#endif
#if 1
        if( AsmBuffer[curr_operator]->token == T_OP_SQ_BRACKET) {
            op_sq_bracket_level++;
            if( evaluate( &operand2, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 3, error\n"));
                return( ERROR );
            }
        } else
#endif
        if( cmp_token( *i, T_OP_BRACKET ) ) {
            (*i)++;
            if( evaluate( &operand2, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 12, error\n"));
                return( ERROR );
            }
            if( cmp_token( *i, T_CL_BRACKET ) ) {
                (*i)++;
            }
        } else if( cmp_token( *i, T_OP_SQ_BRACKET ) ) {
            op_sq_bracket_level++;
            (*i)++;
            if( evaluate( &operand2, i, end, PROC_BRACKET ) == ERROR ) {
                DebugMsg(("evaluate exit 3, error\n"));
                return( ERROR );
            }
        } else if( is_unary( *i, TRUE ) ) {
            if( evaluate( &operand2, i, end, PROC_OPERAND ) == ERROR ) {
                DebugMsg(("evaluate exit 2, error\n"));
                return( ERROR );
            }
        } else if( is_optr( *i ) ) {
            if( error_msg )
                AsmError( OPERAND_EXPECTED );
            operand1->type = EXPR_UNDEF;
            DebugMsg(("evaluate exit 4, error at token %u [%s]\n", *i, AsmBuffer[*i]->string_ptr));
            return( ERROR );
        } else if( get_operand( &operand2, i, end ) == ERROR ) {
            DebugMsg(("evaluate exit 5, error\n"));
            return( ERROR );
        }

        /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /* Close all available brackets                                    */
        /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

        while( ( *i <= end )
            && ( op_sq_bracket_level > op_sq_bracket )
            && cmp_token( *i, T_CL_SQ_BRACKET ) ) {
            (*i)++;
            op_sq_bracket_level--;
        }

        /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
        /* Look at the next operator and compare its priority with 1st one */
        /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

        next_operator = FALSE;
        if( *i <= end ) {
            if( !is_optr( *i )
                || is_unary( *i, FALSE )
                || cmp_token( *i, T_OP_BRACKET ) ) {
                if( error_msg )
                    AsmError( OPERATOR_EXPECTED );
                operand1->type = EXPR_UNDEF;
                DebugMsg(("evaluate exit 11, error\n"));
                return( ERROR );
            } else if( !cmp_token( *i, T_CL_BRACKET ) &&
                       !cmp_token( *i, T_CL_SQ_BRACKET ) ) {
#if 0
                if( cmp_token( *i, T_OP_SQ_BRACKET ) ) {
                    /* WHY????????? */
                    if( ( PLUS_PRECEDENCE <= get_precedence( curr_operator ) )
                        || ( proc_flag == PROC_BRACKET ) ) {
                        AsmBuffer[*i]->token = '+';
                        op_sq_bracket_level++;
                        next_operator = TRUE;
                    }
                } else {
#endif
                next_op:
                    if( get_precedence( *i ) < get_precedence( curr_operator ) ) {
                        if( evaluate( &operand2, i, end, PROC_OPERAND ) == ERROR ) {
                            DebugMsg(("evaluate exit 22, error\n"));
                            return( ERROR );
                        }
                        while( ( *i <= end )
                            && ( op_sq_bracket_level > op_sq_bracket )
                            && cmp_token( *i, T_CL_SQ_BRACKET ) ) {
                            (*i)++;
                            op_sq_bracket_level--;
                        }
                        /* WHY????????? */
                        if( cmp_token( *i, T_OP_SQ_BRACKET ) ) {
                            if( proc_flag == PROC_BRACKET ) {
                                AsmBuffer[*i]->token = '+';
                                op_sq_bracket_level++;
                                next_operator = TRUE;
                            }
                        }

                        /* the fact that the following code is necessary
                         is an indication that there is something wrong
                         with the evaluator. At least evaluate() has to be
                         rewritten.
                         */
                        /* test if there's an operator at current position.
                         if yes, continue with check whether it has higher
                         precedence than the current operator */
                        if (*i < end &&
                            AsmBuffer[*i]->token != T_CL_BRACKET &&
                            AsmBuffer[*i]->token != T_CL_SQ_BRACKET &&
                            is_optr(*i))
                            goto next_op;

                    } else if( proc_flag == PROC_BRACKET ) {
                        next_operator = TRUE;
                    }
#if 0
                }
#endif
            }
        }

        /*::::::::::::::*/
        /* Now evaluate */
        /*::::::::::::::*/

        if( calculate( operand1, &operand2, curr_operator ) == ERROR ) {
            DebugMsg(("evaluate exit 10, error\n"));
            return( ERROR );
        }

    } while ( ( next_operator == TRUE )
        || ( ( proc_flag == PROC_BRACKET )
            && !cmp_token( *i, T_CL_BRACKET )
            && !cmp_token( *i, T_CL_SQ_BRACKET )
            && ( *i < end ) ) );
    if( op_sq_bracket_level != op_sq_bracket ) {
        if( error_msg )
            AsmError( BRACKETS_NOT_BALANCED );
        operand1->type = EXPR_UNDEF;
        DebugMsg(("evaluate exit 9, error\n"));
        return( ERROR );
    }
noterror:
    DebugMsg(("evaluate exit, ok, op1.type=%X/value=%d/string=%X/memtype=%u\n",
              operand1->type,
              operand1->value,
              operand1->string,
              operand1->mem_type));
    return( NOT_ERROR );
}

// called by EvalOperand to determine the end of the expression

static bool is_expr( int i )
/***************************/
/* Check if the token is part of an expression */
{
//    DebugMsg(("is_expr: %u, %X, %X\n", i, AsmBuffer[i]->token, AsmBuffer[i]->value));
    switch( AsmBuffer[i]->token ) {
    case T_INSTRUCTION:
//        DebugMsg(("is_expr: T_INSTRUCTION\n"));
        switch( AsmBuffer[i]->value ) {
#if defined( _STANDALONE_ )
        case T_EQ:
        case T_NE:
        case T_LT:
        case T_LE:
        case T_GT:
        case T_GE:
#endif
        case T_MOD:
            return( TRUE );
        case T_SHL:
        case T_SHR:
        case T_NOT:
        case T_AND:
        case T_OR:
        case T_XOR:
            if( i == 0 ) {
                /* It is an instruction instead */
                break;
            } else if( AsmBuffer[i-1]->token == T_COLON ) {
                /* It is an instruction instead */
                break;
            } else if( AsmBuffer[i-1]->value == T_LOCK ) {
                /* It is an instruction:
                         lock and dword ptr [ebx], 1
                */
                break;
            } else {
                return( TRUE );
            }
        default:
            break;
        }
        break;
    case T_UNARY_OPERATOR:
        return( TRUE );
    case T_RES_ID:
//        DebugMsg(("is_expr: T_RES_ID (%s)\n", AsmBuffer[i]->string_ptr));
        if (AsmBuffer[i]->value == T_FLAT) {
            DefineFlatGroup();
            return( TRUE );
        } else if (AsmBuffer[i]->rm_byte == OP_TYPE)
            return( TRUE );
        else if (AsmBuffer[i]->value == T_PTR ||
                 AsmBuffer[i]->value == T_SHORT)
            return( TRUE );
        else if (AsmBuffer[i]->value == T_DUP)
            /* DUP must terminate the expression */
            return( FALSE );
        /* other reserved words are changed to a T_ID */
        /* they have no meaning in an expression */
        /* examples: BASIC, PASCAL,  */

        AsmBuffer[i]->token = T_ID;
        return( TRUE );
        break;
    case T_REG:
//        DebugMsg(("is_expr: T_REG\n"));
        return( TRUE );
    case '+':
    case '-':
#if 1
        /* this can happen!: "db x dup(-1)" */
    case T_NEGATIVE:
    case T_POSITIVE:
#endif
        /* hack to stop expreval from hanging on floating point numbers */
        if( AsmBuffer[i+1]->token == T_FLOAT )
            break;
        return( TRUE );
    case '*':
    case '/':
    case T_NUM:
    case T_BAD_NUM:
    case T_OP_BRACKET:
    case T_CL_BRACKET:
    case T_OP_SQ_BRACKET:
    case T_CL_SQ_BRACKET:
        return(  TRUE );
    case T_COLON:
//        DebugMsg(("is_expr: T_COLON\n"));
#if 0 // defined( _STANDALONE_ )
        /* check if it is a ":=". Is this relevant at all? */
        if( AsmBuffer[i+1]->token == T_DIRECTIVE &&
            AsmBuffer[i+1]->value == T_EQU &&
            AsmBuffer[i+1]->opcode == 1)
            break;
#endif
        return( TRUE );
    case T_ID:
//        DebugMsg(("is_expr: T_ID\n"));
        return( TRUE );
    case T_STRING:
//        DebugMsg(("is_expr: T_STRING\n"));
        return( TRUE );
    case T_DOT:
        return( TRUE );
    case T_PATH:
    default:
//        DebugMsg(("is_expr: default\n"));
        break;
    }
    return( FALSE );
}

// evaluate an operand
// start_tok: index of first token of expression in AsmBuffer
// count: (max) end index of token of expression

extern int EvalOperand( int *start_tok, int count, expr_list *result, bool flag_msg )
/***********************************************************************************/
{
    int         i = *start_tok;
    int         rc;
    int         num;            // number of tokens in the expression

    DebugMsg(("EvalOperand(i=%u [token=%X], cnt=%u, errmsg=%u) enter\n", i, AsmBuffer[i]->token, count, flag_msg ));
    init_expr( result );
    if( AsmBuffer[i]->token == T_FINAL )
        return( NOT_ERROR );
    if( !is_expr( i ) )
        return( NOT_ERROR );

    num = 0;
    for( ;; ) {
        i++;
        if( i >= count )
            break;
        if( !is_expr( i ) )
            break;
        num++;
    }
    op_sq_bracket_level = 0;
    error_msg = flag_msg;
    is_opattr = FALSE;
#if SETCURRSTR
    curr_struct = NULL;
#endif

    rc = evaluate( result, start_tok, *start_tok + num, PROC_BRACKET );

    DebugMsg(("EvalOperand()=%X exit, result.type=%u value=%X string=%X memtype=%u indirect=%u\n", rc,
              result->type, result->value, result->string, result->mem_type, result->indirect));
    return( rc );
}

// global init (called once for each module)

void ExprEvalInit()
{
#if USEDUMMY
    dummysym = NULL;
#endif
    thissym = NULL;
}
