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

#include <stddef.h>

#include "globals.h"
#include "parser.h"
#include "directiv.h"
#include "expreval.h"
#include "segment.h"
#include "proc.h"
#include "assume.h"
#include "input.h"
#include "tokenize.h"
#include "types.h"
#include "labels.h"

#define ALIAS_IN_EXPR 1 /* allow alias names in expression */

/* activate if a detailed error location is needed and -d6 cant be used */
#if 0
#define ERRLOC( i ) printf("Error at %s.%u: %u >%s< >%s<\n", __FILE__, __LINE__, i, AsmBuffer[i]->string_ptr, CurrSource )
//#undef DebugMsg1
//#define DebugMsg1( x ) printf x
#else
#define ERRLOC( i )
#endif

static int op_sq_bracket_level;

/* error msg flag.
 * 1=display error messages.
 * for historical reasons, it's also used to detect
 * "EQU mode" (error_msg = 0).
 */
static char error_msg;
static char eflags;

static asm_sym *thissym;     /* helper symbol for THIS operator */
static dir_node *nullstruct; /* used for T_DOT if there's no current type */
static asm_sym *nullmbr;    /* used for T_DOT if struct is a forward ref */

enum process_flag {
    PROC_BRACKET, /* evaluate until the end or a closing bracket is found */
    PROC_OPERAND  /* stop if an operator with lower precedence is found */
};

enum labelsize {
    LS_SHORT  = 0xFF01, /* it's documented, but can a label be "short"? */
    LS_NEAR16 = 0xFF02,
    LS_NEAR32 = 0xFF04,
    LS_FAR16  = 0xFF05,
    LS_FAR32  = 0xFF06
};

static ret_code evaluate( expr_list *, int *, int, enum process_flag );

static void init_expr( expr_list *opndx )
/***************************************/
{
    opndx->value    = 0;
    opndx->hvalue   = 0;
    opndx->hlvalue  = 0;
    opndx->string   = NULL;
    opndx->base_reg = EMPTY;
    opndx->idx_reg  = EMPTY;
    opndx->label    = EMPTY;
    opndx->override = EMPTY;
    opndx->instr    = EMPTY;
    opndx->kind     = EXPR_EMPTY;
    opndx->mem_type = MT_EMPTY;
    opndx->scale    = 1;
    opndx->Ofssize  = USE_EMPTY;
    opndx->flags1   = 0;
    opndx->sym      = NULL;
    opndx->mbr      = NULL;
    opndx->type     = NULL;
}

static void TokenAssign( expr_list *t1, const expr_list *t2 )
/***********************************************************/
{
#if 1
    /* note that offsetof() is used. This means, don't change position
     of field <type> in expr_list! */
    memcpy( t1, t2, offsetof( expr_list, type ) );
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
    t1->Ofssize  = t2->Ofssize;
    t1->flags    = t2->flags;
    t1->sym      = t2->sym;
    t1->mbr      = t2->mbr;
//    t1->type     = t2->type;
#endif
}

#define BRACKET_PRECEDENCE 1
#define PTR_PRECEDENCE     4
#define PLUS_PRECEDENCE    9
#define CMP_PRECEDENCE    10

static int get_precedence( int i, int bracket_precedence )
/********************************************************/
{
    /* The following table is taken verbatim from MASM 6.1 Programmer's Guide,
     * page 14, Table 1.3.

     * 1            (), []
     * 2            LENGTH, SIZE, WIDTH, MASK, LENGTHOF, SIZEOF
     * 3            . (structure-field-name operator)
     * 4            : (segment override operator), PTR
     * 5            LROFFSET, OFFSET, SEG, THIS, TYPE
     * 6            HIGH, HIGHWORD, LOW, LOWWORD
     * 7            +, - (unary)
     * 8            *, /, MOD, SHL, SHR
     * 9            +, - (binary)
     * 10           EQ, NE, LT, LE, GT, GE
     * 11           NOT
     * 12           AND
     * 13           OR, XOR
     * 14           OPATTR, SHORT, .TYPE

     * The following table appears in QuickHelp online documentation for
     * both MASM 6.0 and 6.1. It's slightly different!

     * 1            LENGTH, SIZE, WIDTH, MASK
     * 2            (), []
     * 3            . (structure-field-name operator)
     * 4            : (segment override operator), PTR
     * 5            THIS, OFFSET, SEG, TYPE
     * 6            HIGH, LOW
     * 7            +, - (unary)
     * 8            *, /, MOD, SHL, SHR
     * 9            +, - (binary)
     * 10           EQ, NE, LT, LE, GT, GE
     * 11           NOT
     * 12           AND
     * 13           OR, XOR
     * 14           SHORT, OPATTR, .TYPE, ADDR

     * japheth: the first table is the prefered one. Reasons:
     * - () and [] must be first.
     * - it contains operators SIZEOF, LENGTHOF, HIGHWORD, LOWWORD, LROFFSET
     * - ADDR is no operator for expressions. It's exclusively used inside
     *   INVOKE directive.

     * However, what's wrong in both tables is the precedence of
     * the dot operator: Actually for both JWasm and Wasm the dot precedence
     * is 2 and LENGTH, SIZE, ... have precedence 3 instead.

     * Precedence of operator TYPE was 5 in original Wasm source. It has
     * been changed to 4, as described in the Masm docs. This allows syntax
     * "TYPE DWORD ptr xxx"

     * v2.02: another case which is problematic:
     *     mov al,BYTE PTR CS:[]
     * Since PTR and ':' have the very same priority, the evaluator will
     * first calculate 'BYTE PTR CS'. This is invalid, but didn't matter
     * prior to v2.02 because register coercion was never checked for
     * plausibility. Solution: priority of ':' is changed from 4 to 3.
     */

    switch( AsmBuffer[i]->token ) {
    case T_UNARY_OPERATOR:
    case T_BINARY_OPERATOR:
        return( AsmBuffer[i]->precedence );
    case T_OP_BRACKET:
    case T_OP_SQ_BRACKET:
        //return( bracket_precedence ); /* changed for v1.95 */
        return( 1 );
    case T_DOT:
        return( 2 );
    case T_COLON:
        //return( 4 );
        return( 3 ); /* changed for v2.02 */
    case T_POSITIVE: /* unary + */
    case T_NEGATIVE: /* unary - */
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
    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
    return( ERROR );
}

/* get value for types
 * NEAR, FAR and PROC are handled slightly differently:
 * the HIBYTE is set to 0xFF, and PROC depends on the memory model
 */
static unsigned int GetTypeSize( int i )
/**************************************/
{
    if ( (SimpleType[i].mem_type & MT_SPECIAL) == 0 )
        return( ( SimpleType[i].mem_type & MT_SIZE_MASK ) + 1 );
    switch ( i ) {
    case ST_NEAR16: return( 2 );
    case ST_NEAR32: return( 4 );
    case ST_FAR16:  return( 4 );
    case ST_FAR32:  return( 6 );
    case ST_NEAR:   return (0xFF00 | ( 2 << ModuleInfo.Ofssize ) ) ;
    case ST_FAR:    return (0xFF00 | ( ( 2 << ModuleInfo.Ofssize ) + 2 ) );
    case ST_PROC:
        if ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) )
            return ( 0xFF00 | ( ( 2 << ModuleInfo.Ofssize ) + 2 ) );
        else
            return ( 0xFF00 | ( 2 << ModuleInfo.Ofssize ) );
    }
    /* shouldn't happen */
    return( 0 );
}

static void CAsmError( int msg )
/******************************/
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
/****************************************************************/
{
    char        *tmp;
    int         i = *start;
    asm_sym     *sym2;
    dir_node    *dir;
    int         j;

    DebugMsg1(("get_operand(start=%u >%s<, end=%u) enter, memtype=%Xh\n", *start, AsmBuffer[*start]->tokpos, end, new->mem_type ));
    switch( AsmBuffer[i]->token ) {
    case T_NUM:
        DebugMsg1(("get_operand: T_NUM (%s)\n", AsmBuffer[i]->string_ptr ));
        new->kind = EXPR_CONST;
        new->llvalue = AsmBuffer[i]->value64;
        new->hlvalue = ( AsmBuffer[i]->hivalflg == HV_NULL ? 0 : *(uint_64 *)(AsmBuffer[i]->string_ptr-sizeof(uint_64)) );
        break;
    case T_STRING:
        DebugMsg1(("get_operand: T_STRING (%s), value=%X\n", AsmBuffer[i]->string_ptr, AsmBuffer[i]->value ));
        /* string enclosed in <> or {} are rejected since v1.94! */
        if ( AsmBuffer[i]->string_delim != '"' && AsmBuffer[i]->string_delim != '\'') {
            if ( new->is_opattr ) /* OPATTR operator accepts anything! */
                break;
            if ( error_msg )
                /* v2.0: display a comprehensible error msg if a quote is missing */
                if ( AsmBuffer[i]->string_delim == NULLC &&
                    ( *AsmBuffer[i]->string_ptr == '"' || *AsmBuffer[i]->string_ptr == '\'' ))
                    AsmErr( MISSING_QUOTATION_MARK_IN_STRING );
                else
                    AsmErr( UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        new->kind = EXPR_CONST;
        new->string = AsmBuffer[i]->string_ptr;
        //new->value = 0;
        tmp = new->string + 1; /* skip the quote */
        j = AsmBuffer[i]->value;
        /* string size exceeds 4? */
#if AMD64_SUPPORT
        if ( ModuleInfo.Ofssize == USE64 ) {
            if ( j > 8 )
                j = 8;
            for( ; j; tmp++,j-- ) {
                new->llvalue <<= 8;
                new->llvalue |= (*tmp);
            }
            break;
        }
#endif
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
        DebugMsg1(( "get_operand: T_REG (%s)\n", AsmBuffer[i]->string_ptr ));
        new->kind = EXPR_REG;
        new->base_reg = i;
        j = AsmBuffer[i]->value;
#if 1
        /* this check was previously done in the parser.
         Check if the register needs an extensions (which are bit masks).
         If no, then check if the cpu is sufficient.
         */
        if( ( ( GetCpuSp( j ) & P_EXT_MASK ) &&
            (( GetCpuSp( j ) & ModuleInfo.curr_cpu & P_EXT_MASK) == 0) ||
              ( ModuleInfo.curr_cpu & P_CPU_MASK ) < ( GetCpuSp( j ) & P_CPU_MASK ) ) ) {
            CAsmError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
            return( ERROR );
        }
#endif
        if( op_sq_bracket_level > 0 ) {
            /* a valid index register? */
            if ( GetSflagsSp( j ) & SFR_IREG ) {
                new->indirect = TRUE;
                /* <value8> contains register number */
                if ( sym2 = GetStdAssumeEx( AsmBuffer[i]->value8 ) ) {
                    DebugMsg1(( "get_operand, T_REG: assume=%s memtype=%X isptr=%u type=%s target_type=%s ptr_memt=%X\n",
                               sym2->name, sym2->mem_type, sym2->is_ptr,
                               sym2->type ? sym2->type->name : "NULL",
                               sym2->target_type ? sym2->target_type->name : "NULL",
                               sym2->ptr_memtype ));
                    /* skip "alias" types */
                    /* v2.05: obsolete */
                    //for ( ; sym2->type; sym2 = sym2->type );
                    /* v2.05: new */
                    if ( sym2->mem_type == MT_TYPE )
                        new->type = sym2->type;
                    else if ( sym2->is_ptr ) {
                        new->type = sym2->target_type;
                        if ( sym2->target_type )
                            new->mem_type = sym2->target_type->mem_type;
                        else
                            new->mem_type = sym2->ptr_memtype;
                    }
                }
            } else if ( GetValueSp( j ) & OP_SR ) {
                /* a segment register CS, SS, DS, ES, FS, GS? */
                if( AsmBuffer[i+1]->token != T_COLON ) {
                    CAsmError( INVALID_USE_OF_REGISTER );
                    return( ERROR );
                }
            } else {
                CAsmError( MUST_BE_INDEX_OR_BASE_REGISTER );
                return( ERROR );
            }
        } else if( AsmBuffer[i]->value == T_ST ) {

            expr_list   sti;

            /* read st(i), put i into idx_reg */
            i++;
            switch( AsmBuffer[i]->token ) {
            case T_OP_BRACKET:    /* syntax st(0) */
            case T_OP_SQ_BRACKET: /* syntax st[0] */
            // case T_NUM: /* syntax "st 0" is NOT valid! */
                *start = i;
                init_expr( &sti );
                if( evaluate( &sti, start, end, PROC_OPERAND ) == ERROR ) {
                    return( ERROR );
                }
                if( sti.kind != EXPR_CONST || sti.string != NULL ) {
                    CAsmError( CONSTANT_OPERAND_EXPECTED );
                    return( ERROR );
                }
                new->idx_reg = sti.value;
                DebugMsg(("get_operand exit, ok\n"));
                return( NOT_ERROR );
            default:
                new->idx_reg = 0; /* st = st(0) */
                break;
            }
        }
        break;
    case T_ID:
        if ( new->type ) {
            DebugMsg1(("get_operand(%s): ID, type=%s\n", AsmBuffer[i]->string_ptr, new->type->name ));
            new->value = 0;
            /*
             * skip a type specifier matching the variable's type
             * that's something like "var.TYPE.xxx"
             */
            new->sym = SearchNameInStruct((asm_sym *)new->type, AsmBuffer[i]->string_ptr, (unsigned int *)&new->value, 0 );
            DebugMsg1(("get_operand(%s): SearchNameInStruct(%s)=%X, value=%u\n",
                       AsmBuffer[i]->string_ptr, new->type->name, new->sym, new->value));
            if ( new->sym == NULL ) {
                sym2 = SymSearch( AsmBuffer[i]->string_ptr );
                if ( sym2 ) {
                    if ( sym2->state == SYM_TYPE ) {
                        new->sym = sym2;
                    } else if ( ModuleInfo.oldstructs &&
                               ( sym2->state == SYM_STRUCT_FIELD ||
                                sym2->state == SYM_EXTERNAL || /* v2.01: added */
                                /* v2.05: changed */
                                //( sym2->state == SYM_INTERNAL && sym2->mem_type == MT_ABS ) ) )
                                sym2->state == SYM_INTERNAL ) )
                        new->sym = sym2;
                }
            }
        } else {
            DebugMsg1(("get_operand(%s): ID\n", AsmBuffer[i]->string_ptr));
            /* ensure anonym labels are uppercase */
            if ( *AsmBuffer[i]->string_ptr == '@' && *(AsmBuffer[i]->string_ptr+2 ) == NULLC ) {
                if ( *(AsmBuffer[i]->string_ptr+1) == 'b' || *(AsmBuffer[i]->string_ptr+1 ) == 'B' )
                    GetCurrAnonLabel( AsmBuffer[i]->string_ptr );
                else if (*(AsmBuffer[i]->string_ptr+1) == 'f' || *(AsmBuffer[i]->string_ptr+1 ) == 'F' )
                    GetNextAnonLabel( AsmBuffer[i]->string_ptr );
            }
            new->sym = SymSearch( AsmBuffer[i]->string_ptr );
        }
        if ( new->sym == NULL ||
            new->sym->state == SYM_UNDEFINED ||
#if ALIAS_IN_EXPR == 0
            new->sym->state == SYM_ALIAS || /* v2.04: added */
#endif
            new->sym->state == SYM_MACRO ||
            new->sym->state == SYM_TMACRO ) {

            /* for OPATTR, anything is ok */
            if ( new->is_opattr ) {
                DebugMsg1(( "get_operand(%s): OPATTR, symbol invalid\n", AsmBuffer[i]->string_ptr ));
                new->kind = EXPR_UNDEF;
                break;
            }
            /* if it is EQU (then error_msg is FALSE), don't display an error,
             but return ERROR */
            if ( error_msg == FALSE ) {
                DebugMsg1(("get_operand(%s): EQU, symbol invalid\n", AsmBuffer[i]->string_ptr));
                return( ERROR );
            }
            //if( Parse_Pass == PASS_1 ) {
            if( Parse_Pass == PASS_1 ) {
                /* if symbol wasn't found, assume it is a forward ref! */
                if ( new->sym == NULL ) {
                    if ( new->type == NULL && !( eflags & EXPF_NOLCREATE ) ) { /* added v1.95 */
                        new->sym = SymLookup( AsmBuffer[i]->string_ptr );
                        new->sym->state = SYM_UNDEFINED;
                        dir_add_table( &Tables[TAB_UNDEF], (dir_node *)new->sym ); /* add UNDEFINED */
                        DebugMsg1(("get_operand(%s): symbol not (yet) defined, CurrProc=%s\n", AsmBuffer[i]->string_ptr, CurrProc ? CurrProc->sym.name : "NULL" ));
                    } else if ( new->type == NULL || new->type != (asm_sym *)nullstruct ) {
                        /* the struct is known, it isn't the dummy struct */
                        DebugMsg(("get_operand(%s): symbol error, type=%s\n", new->type ? new->type->name : "NULL" ));
                        AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
                        return( ERROR );
                    } else {
                        /* forward reference to a struct.
                         * In these cases, assume everything is ok.
                         */
                        if ( !nullmbr )
                            nullmbr = SymCreate( "", FALSE );
                        new->mbr = nullmbr;
                        new->kind = EXPR_CONST;
                        break;
                    }
                } else if ( new->sym->state != SYM_UNDEFINED ) {
                    DebugMsg1(("get_operand(%s): symbol is macro/textmacro/alias!\n", AsmBuffer[i]->string_ptr));
                    AsmErr( INVALID_SYMBOL_TYPE_IN_EXPRESSION, new->sym->name );
                    return( ERROR );
                }
            } else {
                DebugMsg1(("get_operand(%s): symbol not defined, pass > 1, curr proc=>%s<, \n", AsmBuffer[i]->string_ptr, CurrProc ? CurrProc->sym.name : "NULL" ));
                if ( new->type && *new->type->name ) {
                    sprintf( StringBufferEnd, "%s.%s", new->type->name, AsmBuffer[i]->string_ptr );
                    AsmErr( SYMBOL_NOT_DEFINED, StringBufferEnd );
                } else {
                    AsmErr( SYMBOL_NOT_DEFINED, *(AsmBuffer[i]->string_ptr+1) == '&' ? "@@" : AsmBuffer[i]->string_ptr );
                }
                return( ERROR );
            }
        }
#if ALIAS_IN_EXPR /* v2.04b: added */
        else if ( new->sym->state == SYM_ALIAS ) {
            /* ALIAS symbols are not really useable in expressions.
             * The alias' substitute symbol is, however.
             */
            new->sym = new->sym->substitute; /* can't be NULL */
        }
#endif
        /* set default values */
        new->kind = EXPR_ADDR;
        new->sym->used = TRUE;
        DebugMsg1(("get_operand(%s): sym->state=%u type=%X ofs=%X memtype=%Xh total_size=%u defined=%u\n",
                  AsmBuffer[i]->string_ptr, new->sym->state, new->sym->type, new->sym->offset, new->sym->mem_type, new->sym->total_size, new->sym->isdefined ));
        switch ( new->sym->state ) {
        case SYM_TYPE:
            dir = (dir_node *)(new->sym);
            if ( dir->e.structinfo->isOpen == FALSE ) {
                new->kind = EXPR_CONST;
                //new->mem_type = MT_ABS;
                new->mem_type = new->sym->mem_type;
                new->is_type = TRUE;
                DebugMsg1(("get_operand(%s): symbol.kind=%u (STRUCT/UNION/TYPEDEF/RECORD)\n", new->sym->name, dir->e.structinfo->typekind ));
#if 1
                if ( AsmBuffer[i-1]->token != T_DOT && AsmBuffer[i+1]->token != T_DOT )
                    new->value = new->sym->total_size;
#endif
            } else {
                DebugMsg1(("get_operand(%s): symbol definition isn't closed!\n", new->sym->name ));
                /* a valid constant should be returned if
                 1. the struct is open      AND
                 2. it's not an EQU operand
                 the number isn't used then (except if it's the first DUP operand)
                 */
                if ( error_msg == FALSE )
                    new->kind = EXPR_UNDEF;
                else {
                    new->kind = EXPR_CONST;
                    new->mem_type = MT_ABS;
                    new->value = -1;
                    new->is_type = TRUE;
                }
            }
            /* skip "alias" types */
            for ( sym2 = new->sym; sym2->type; sym2 = sym2->type );
            new->type = sym2;
            new->sym = NULL;  /* sym must be NULL, it's not a label */
            break;
        case SYM_STRUCT_FIELD:
            DebugMsg1(("get_operand(%s): structure field, ofs=%Xh\n", new->sym->name, new->sym->offset ));

            /* new->value might have been set by SearchNameInStruct() already! */
            new->value += new->sym->offset;
            new->kind = EXPR_CONST;
            /* skip "alias" types */
            for ( sym2 = new->sym; sym2->type; sym2 = sym2->type );
            dir = (dir_node *)sym2;
            /*
             * check if the member field has arbitrary type.
             * If yes, set the <type> member!
             * It's probably better to handle this case in PrepareOp() for
             * the T_DOT operator.
             */
            if ( sym2->state == SYM_TYPE && dir->e.structinfo->typekind != TYPE_TYPEDEF ) {
                new->type = sym2;
                if ( dir->e.structinfo->typekind == TYPE_RECORD )
                    new->mem_type = dir->sym.mem_type;
                DebugMsg1(("get_operand: new current struct: %s, mem_type=%Xh\n", sym2->name, sym2->mem_type));
            } else {
                new->type = NULL; /* added v1.96 */
                new->mem_type = sym2->mem_type;
                DebugMsg1(("get_operand: mem_type=%Xh\n", new->mem_type ));
            }
            new->mbr = new->sym;
            new->sym = NULL;
            break;
        default: /* SYM_INTERNAL, SYM_EXTERNAL, SYM_SEG, SYM_GRP, SYM_STACK */
            /* call internal function (@Line, ... ) */
            if ( new->sym->predefined && new->sym->sfunc_ptr )
                new->sym->sfunc_ptr( new->sym );
            if( new->sym->mem_type == MT_ABS ) {
                if( new->sym->state == SYM_INTERNAL ) {
                    new->kind = EXPR_CONST;
                    new->uvalue = new->sym->uvalue;
                    new->hvalue = new->sym->value3264;
                    DebugMsg1(("get_operand(%s): equate hval=%Xh, lval=%Xh\n", new->sym->name, new->hvalue, new->uvalue ));
                    /* remove the symbol reference, it isn't a label */
                    new->sym = NULL;
                } else { /* ABS external? */
                    /* type remains EXPR_ADDR, to force fixup creation */
                    new->mem_type = new->sym->mem_type;
                    new->abs = TRUE;
                }
            } else {
                new->label = i;
                /* a variable with arbitrary type? */
                /* v2.05: added check for MT_EMPTY */
                //if( new->sym->type ) { 
                if( new->sym->type && new->sym->type->mem_type != MT_EMPTY ) {
                    /* skip "alias" types */
                    /* v2.05: obsolete */
                    //for ( sym2 = new->sym; sym2->type; sym2 = sym2->type );
                    //new->mem_type = sym2->mem_type;
                    new->mem_type = new->sym->type->mem_type;
                } else {
                    new->mem_type = new->sym->mem_type;
                }
                /* since there is no fixup for auto variables, the "offset"
                 must be stored in the <value> field */
                if ( new->sym->state == SYM_STACK ) {
                    new->llvalue = new->sym->offset;
                    new->indirect = TRUE;
                }
            }
            break;
        }
        break;
    case T_STYPE:
        DebugMsg1(("get_operand: T_STYPE, >%s<, value=%X\n", AsmBuffer[i]->string_ptr, AsmBuffer[i]->value));
        /* for types, return the size as numeric constant */
        /* <value8> contains index into SimpleType table */
        new->value = GetTypeSize( AsmBuffer[i]->value8 );
        /* fixme: mem_type should be set only when used as first arg of PTR op! */
        new->mem_type = SimpleType[AsmBuffer[i]->value8].mem_type;
        new->Ofssize = SimpleType[AsmBuffer[i]->value8].Ofssize;
        new->kind = EXPR_CONST;
        new->is_type = TRUE;
        break;
    case T_RES_ID:
        DebugMsg1(("get_operand: T_RES_ID, >%s<, value=%X\n", AsmBuffer[i]->string_ptr, AsmBuffer[i]->value));
        if ( AsmBuffer[i]->value == T_FLAT ) {
            if ( error_msg ) /* don't define FLAT group in EQU expression! */
                DefineFlatGroup();
            new->label = i;
            if ( new->sym = SymSearch("FLAT") )
                new->kind = EXPR_ADDR;
            else
                return( ERROR );
        } else {
            if( error_msg )
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        break;
    case T_FLOAT: /* v2.05 */
        new->kind = EXPR_FLOAT;
        new->base_reg = i;
        break;
    case T_CL_BRACKET:
    case T_CL_SQ_BRACKET:
        if ( new->is_opattr ) {    /* for OPATTR, allow empty () or [] operand */
            DebugMsg1(("get_operand: OPATTR, no operand\n" ));
            return( NOT_ERROR );
        }
    default:
        DebugMsg1(("get_operand: default, i=%d, string=%s\n", i, AsmBuffer[i]->string_ptr));
        if( error_msg )
            if (AsmBuffer[i]->token == T_BAD_NUM)
                /* Masm complains even if in EQU-mode */
                AsmErr( NONDIGIT_IN_NUMBER, AsmBuffer[i]->string_ptr );
            else if ( AsmBuffer[i]->token == T_COLON )
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
            else
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->tokpos );
        return( ERROR );
    }
    (*start)++;
    DebugMsg1(("get_operand exit, ok, value=%I64X hvalue=%I64X mem_type=%Xh abs=%u string=%s type=>%s<\n",
              new->llvalue, new->hlvalue, new->mem_type, new->abs, new->string ? new->string : "NULL", new->type ? new->type->name : "NULL" ));
    return( NOT_ERROR );
}

static bool is_operator( int i )
/******************************/
/* determine if it is an operator */
{
    switch( AsmBuffer[i]->token ) {
    case T_RES_ID:
    case T_STYPE:
    case T_REG:
    case T_NUM:
    case T_BAD_NUM:
    case T_ID:
    case T_STRING:
    case T_FLOAT: /* v2.05 */
        return( FALSE );
    }
    return( TRUE );
}

static bool is_unary( int i, char first )
/***************************************/
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
        if( first ) {
            AsmBuffer[i]->token = T_POSITIVE;
            return( TRUE );
        }
        break;
    case '-':
        if( first ) {
            AsmBuffer[i]->token = T_NEGATIVE;
            return( TRUE );
        }
        break;
    }
    return( FALSE );
}

#if 0
static bool check_same( expr_list *tok_1, expr_list *tok_2, enum exprtype kind )
/******************************************************************************/
/* Check if both tok_1 and tok_2 equal type */
{
    if( tok_1->kind == kind &&
        tok_2->kind == kind ) {
        return( TRUE );
    } else {
        return( FALSE );
    }
}
#else
#define check_same( first, second, KIND ) (first->kind == KIND && second->kind == KIND )
#endif

static bool check_both( const expr_list *tok_1, const expr_list *tok_2, enum exprtype type1, enum exprtype type2 )
/****************************************************************************************************************/
/* Check if tok_1 == type1 and tok_2 == type2 or vice versa */
{
    if( tok_1->kind == type1 && tok_2->kind == type2 )
        return( TRUE );
    if( tok_1->kind == type2 && tok_2->kind == type1 )
        return( TRUE );
    return( FALSE );
}

static ret_code index_connect( expr_list *tok_1, expr_list *tok_2 )
/*****************************************************************/
/* Connects the register lists */
{
    if ( tok_2->base_reg != EMPTY ) {
        if ( tok_1->base_reg == EMPTY )
            tok_1->base_reg = tok_2->base_reg;
        else if ( tok_1->idx_reg == EMPTY ) {
            tok_1->idx_reg = tok_2->base_reg;
            tok_1->scale = 1;
        } else {
            CAsmError( MULTIPLE_INDEX_REGISTERS_NOT_ALLOWED );
            return( ERROR );
        }
        tok_1->indirect = 1;
    }
    if( tok_2->idx_reg != EMPTY ) {
        if ( tok_2->scale == 1 && tok_1->base_reg == EMPTY ) {
            tok_1->base_reg = tok_2->idx_reg;
        } else if ( tok_1->idx_reg == EMPTY ) {
            tok_1->idx_reg = tok_2->idx_reg;
            tok_1->scale = tok_2->scale;
        } else {
            CAsmError( MULTIPLE_INDEX_REGISTERS_NOT_ALLOWED );
            return( ERROR );
        }
        tok_1->indirect = 1;
    }
    return( NOT_ERROR );
}

/* convert an address operand to a const operand if possible.
 * called for '*', '/', '+', '-' operators.
 */

static void MakeConst( expr_list *token )
/***************************************/
{
    if( token->kind != EXPR_ADDR )
        return;

    if( token->sym ) {
        if ( Parse_Pass > PASS_1 )
            return;
        /* added for v1.94: if the evaluator assumed an address because
         the label wasn't defined yet, then negate this. Also, an
         EXTERNDEF:ABS is to be accepted. */
        if ( token->sym->state == SYM_UNDEFINED ||
            ( token->sym->state == SYM_EXTERNAL && token->sym->weak == TRUE && token->abs == TRUE ) )
            ;
        else
            return;
        /* assume a value != 0 to avoid problems with div */
        token->value = 1;
    }

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
    token->kind = EXPR_CONST;
    token->indirect = FALSE;
    token->explicit = FALSE;
    token->mem_type = MT_EMPTY;
}

/* used by EQ, NE, GT, GE, LE, LT if item is a direct address
 */

static ret_code MakeConst2( expr_list *token_1, expr_list *token_2 )
/******************************************************************/
{

    if ( token_1->sym->state == SYM_EXTERNAL ) {
        if ( error_msg )
            AsmErr( INVALID_USE_OF_EXTERNAL_SYMBOL, token_1->sym->name );
        return( ERROR );
    } else if ( token_1->sym->segment != token_2->sym->segment ||
               token_2->sym->state == SYM_EXTERNAL ) {
        CAsmError( OPERANDS_MUST_BE_IN_SAME_SEGMENT );
        return( ERROR );
    }
    token_1->kind = EXPR_CONST;
    token_1->value += token_1->sym->offset;
    token_2->kind = EXPR_CONST;
    token_2->value += token_2->sym->offset;
    return( NOT_ERROR );
}

static ret_code ConstError( expr_list *token_1, expr_list *token_2 )
/******************************************************************/
{
    if ( token_1->is_opattr )
        return( NOT_ERROR );
    if ( token_1->kind == EXPR_FLOAT || token_2->kind == EXPR_FLOAT )
        CAsmError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
    else
        CAsmError( CONSTANT_EXPECTED );
    return( ERROR );
}

/* used by + and - binary operators */

static void fix_struct_value( expr_list *token )
/**********************************************/
{
    if( token->mbr && ( token->mbr->state == SYM_TYPE ) ) {
        token->value += token->mbr->total_size;
        token->mbr = NULL;
    }
}

static int check_direct_reg( const expr_list *token_1, const expr_list *token_2 )
/*******************************************************************************/
{
    if( ( token_1->kind == EXPR_REG ) && ( token_1->indirect == FALSE )
        || ( token_2->kind == EXPR_REG ) && ( token_2->indirect == FALSE ) ) {
        return( ERROR );
    }
    return( NOT_ERROR );
}

static unsigned GetSizeValue( asm_sym *sym )
/******************************************/
{
    if ( sym->mem_type == MT_PTR )
        return( SizeFromMemtype( sym->isfar ? MT_FAR : MT_NEAR, sym->Ofssize, sym->type ) );
    return( SizeFromMemtype( sym->mem_type, sym->Ofssize, sym->type ) );
}

static unsigned IsOffset( expr_list *token )
/******************************************/
{
    if ( token->mem_type == MT_EMPTY )
        if ( token->instr == T_OFFSET ||
#if IMAGERELSUPP
            token->instr == T_IMAGEREL ||
#endif
#if SECTIONRELSUPP
            token->instr == T_SECTIONREL ||
#endif
            token->instr == T_LROFFSET )
            return( 1 );
    return( 0 );
}

static void invalid_operand( char *oprtr, char *operand )
/*******************************************************/
{
    if ( error_msg )
        AsmErr( INVALID_OPERAND_FOR_OPERATOR, _strupr( oprtr), operand );
}

/* operators
 * LENGTH:    number of items of first initializer
 * SIZE:      size in bytes of first initializer
 * LENGTHOF:  number of elements in an array
 * SIZEOF:    size in bytes of item (array/struct)
 */

static ret_code sizlen_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/*****************************************************************************************************/
{
    token_1->kind = EXPR_CONST;
#if 0
    token_1->label = EMPTY;
    token_1->sym = NULL;
    token_1->base_reg = EMPTY;
    token_1->idx_reg = EMPTY;
    token_1->override = EMPTY;
    token_1->instr = EMPTY;
    token_1->indirect = FALSE;
    token_1->explicit = FALSE;
    token_1->mem_type = MT_EMPTY;
#endif
    DebugMsg1(("sizlen_op(%s): sym=%X, mbr=%X, type=>%s<\n", GetResWName( oper, NULL ),
               token_2->sym, token_2->mbr, token_2->type ? token_2->type->name : "NULL" ));

    /*  these 4 operators accept structure fields,
     * stack variables and data labels.
     * the old SIZE and LENGTH ops also accept code labels.
     */
    if ( sym ) {
        if ( sym->state == SYM_STRUCT_FIELD || sym->state == SYM_STACK )
            ;
        else if ( sym->state == SYM_UNDEFINED && Parse_Pass == PASS_1 )
            ;
        else if ( ( sym->state == SYM_EXTERNAL ||
                 sym->state == SYM_INTERNAL) &&
                 sym->mem_type != MT_ABS &&
                 sym->mem_type != MT_PROC &&
                 sym->mem_type != MT_FAR &&
                 sym->mem_type != MT_NEAR )
            ;
        else if ( sym->state == SYM_GRP || sym->state == SYM_SEG ) {
            CAsmError( EXPECTED_DATA_LABEL );
            return( ERROR );
        } else if ( oper == T_SIZE || oper == T_LENGTH )
            ;
        else {
            CAsmError( EXPECTED_DATA_LABEL );
            return( ERROR );
        }
    }

    switch( oper ) {
    case T_LENGTH:
#if 1
        /* data items and struct fields have a "first" count.
         * OTOH, procedure locals have none, although they may be arrays.
         */
        token_1->value = ( sym->state != SYM_STACK && sym->isarray ) ? sym->first_length : 1;
#else
        if( token_2->kind == EXPR_CONST ) {
            token_1->value = token_2->mbr->first_length ? token_2->mbr->first_length : 1;
            /* v2.05: first_length no longer set for SYM_STACK */
            //} else if ( sym->state == SYM_EXTERNAL || ( sym->state == SYM_INTERNAL && sym->isproc ) ) {
        } else if ( sym->state == SYM_EXTERNAL || sym->state == SYM_STACK || ( sym->state == SYM_INTERNAL && sym->isproc ) ) {
            token_1->value = 1;
        } else if( sym->mem_type == MT_EMPTY ) {
            token_1->value = 0;
        } else {
            token_1->value = sym->first_length ? sym->first_length : 1;
        }
#endif
        break;
    case T_LENGTHOF:
        /* LENGTHOF needs either a data label or a structure field */
        /* a TYPE (structure, typedef) is invalid */
        if( token_2->kind == EXPR_CONST ) {
            token_1->value = token_2->mbr->total_length;
        } else if( sym->state == SYM_UNDEFINED && Parse_Pass == PASS_1 ) {
            token_1->value = sym->total_length;
        } else if ( sym->state == SYM_EXTERNAL && sym->comm == FALSE ) {
            /* for externals other than COMM, total_length field is used otherwise */
            token_1->value = 1;
        } else {
            token_1->value = sym->total_length;
        }
        break;
    case T_SIZE:
        /* if it is a TYPE, first_size isn't set. then use
         * total_size.
         * v2.04: first_size is no longer set for SYM_STACK.
         */
        if( sym == NULL ) {
            token_1->value = token_2->value;
        } else if ( sym->isdata ) {
            token_1->value = sym->first_size;
        } else if( sym->state == SYM_TYPE ) {
            token_1->value = sym->total_size;
        } else if( sym->state == SYM_STACK ) {
            token_1->value = GetSizeValue( sym );
        } else if( sym->mem_type == MT_NEAR ) {
            token_1->value = GetSymOfssize( sym ) ? LS_NEAR32 : LS_NEAR16;
        } else if( sym->mem_type == MT_FAR ) {
            token_1->value = GetSymOfssize( sym ) ? LS_FAR32 : LS_FAR16;
        } else {
            token_1->value = GetSizeValue( sym );
        }
        DebugMsg1(("sizlen_op(SIZE): result=%u [symbol %s, first_size=%u]\n", token_1->value, sym ? sym->name : "NULL", sym ? sym->first_size : 0 ));
        break;
    case T_SIZEOF:
#ifdef DEBUG_OUT
        if (sym)
            DebugMsg1(("sizlen_op(sizeof): symbol %s, state=%u, size=%u\n", sym->name, sym->state, sym->total_size ));
        else
            DebugMsg1(("sizlen_op(sizeof): symbol NULL, token2.value=%u\n", token_2->value ));
#endif
        /* if sym = NULL then operand is a type constant */
        if ( sym == NULL ) {
            token_1->value = token_2->value;
#if 1 /* v2.05: don't use total_size for externals anymore! */
        } else if ( sym->state == SYM_EXTERNAL && sym->comm == FALSE ) {
            token_1->value = GetSizeValue( sym );
            if ( sym->comm == TRUE )
                token_1->value *= sym->total_length;
#endif
        } else
            token_1->value = sym->total_size;
        break;
    }
    return( NOT_ERROR );
}

/* TYPE operator */

static ret_code type_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/***************************************************************************************************/
{
    DebugMsg1(("type_op: argument kind=%u memtype=%X sym=%s type=%s instr=%d istype=%u explicit=%u\n",
               token_2->kind,
               token_2->mem_type,
               sym ? sym->name : "NULL",
               token_2->type ? token_2->type->name : "NULL",
               token_2->instr,
               token_2->is_type,
               token_2->explicit ));
    token_1->kind = EXPR_CONST;
    /* TYPE accepts arrays/structs/unions */
    if( token_2->instr != EMPTY ) {
        if ( token_2->sym ) {
            switch ( token_2->instr ) {
            case T_LOW:
            case T_HIGH:
                token_1->value = 1;
                break;
            case T_LOWWORD:
            case T_HIGHWORD:
                //case T_SEG: /* masm returns 0 for TYPE SEG <label>! */
                token_1->value = 2;
                break;
#if LOHI32
            case T_LOW32:
            case T_HIGH32:
                token_1->value = 4;
                break;
#endif
            case T_OFFSET:
            case T_LROFFSET:
#if SECTIONRELSUPP
            case T_SECTIONREL: /* masm returns 0 for TYPE SECTIONREL <label>! */
#endif
#if IMAGERELSUPP
            case T_IMAGEREL: /* masm returns 0 for TYPE IMAGEREL <label>! */
#endif
                token_1->value = 2 << GetSymOfssize( token_2->sym );
                token_1->is_type = TRUE; /* v2.03: added */
                break;
            }
        }
    } else if ( sym == NULL ) {
        /* for types, return total_size */
        if ( token_2->is_type == TRUE ) {
            //token_1->value = token_2->value;
            TokenAssign( token_1, token_2 );
            /* v2.05: added, (type procptr) ptr <reg>
             * the type probably should be moved generally,
             * but this variant is the one used by INVOKE, other
             * usages are virtually irrelevant.
             */
            if ( token_2->mem_type == MT_PROC )
                token_1->type = token_2->type;
        } else if (token_2->kind == EXPR_REG && token_2->indirect == FALSE ) {
            token_1->value = SizeFromRegister( AsmBuffer[token_2->base_reg]->value );
#if 1 /* v2.03: added */
            token_1->is_type = TRUE;
            if ( token_1->mem_type == MT_EMPTY )
                MemtypeFromSize( token_1->value, &token_1->mem_type );
#endif
#if 0 /* Masm returns 0 for TYPE <segment_register> */
            /* if it is a segment register, use default word size */
            if (token_1->value == 0)
                token_1->value = Use32 ? 4 : 2;
#endif
        //} else if ( token_2->explicit ) { /* v2.05: changed */
        } else if ( token_2->mem_type != MT_EMPTY ) {
            token_1->value = SizeFromMemtype( token_2->mem_type, ModuleInfo.Ofssize, token_2->type );
            token_1->is_type = TRUE; /* v2.04: added */
            token_1->mem_type = token_2->mem_type; /* v2.04: added */
        } else /* it is a number or EXPR_REG + indirect */
            token_1->value = 0;
#if 0
    } else if ( sym->state == SYM_TYPE ) {
        TokenAssign( token_1, token_2 );
        token_1->type = sym;
#endif
        //} else if( sym->mem_type == MT_TYPE ) { /* v2.04: check for explicit */
    } else if( sym->mem_type == MT_TYPE && token_2->explicit == FALSE ) {
        token_1->value = sym->type->total_size;
#if 1 /* v2.03: added */
        token_1->is_type = TRUE;
        if ( token_1->mem_type == MT_EMPTY )
            token_1->mem_type = token_2->mem_type;
#endif
    } else {
#if 1 /* v2.03: added */
        token_1->is_type = TRUE;
        if ( token_1->mem_type == MT_EMPTY )
            token_1->mem_type = token_2->mem_type;
#endif
        /* v2.05: stack vars pointer types? */
        if ( sym->mem_type == MT_PTR )
            token_1->value = SizeFromMemtype( sym->isfar ? MT_FAR : MT_NEAR, sym->Ofssize, NULL );
        else if( sym->mem_type == MT_NEAR )
            token_1->value = GetSymOfssize( sym ) ? LS_NEAR32 : LS_NEAR16;
        else if( sym->mem_type == MT_FAR )
            token_1->value = GetSymOfssize( sym ) ? LS_FAR32 : LS_FAR16;
        else
            token_1->value = SizeFromMemtype( token_2->mem_type, GetSymOfssize( sym ), sym->type );
    }
    DebugMsg1(("type_op: result value=%u type=%s\n", token_1->value, token_1->type ? token_1->type->name : "NULL" ));
    return( NOT_ERROR );
}

/*
 * T_DOT_TYPE: implement .TYPE as an alias for OPATTR
 * T_OPATTR:
 */
static ret_code opattr_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/*****************************************************************************************/
{

    DebugMsg1(("opattr_op: arg kind=%u memtype=%X sym=%s\n",
               token_2->kind, token_2->mem_type,
               token_2->sym ? token_2->sym->name : "NULL" ));
    token_1->kind = EXPR_CONST;
    token_1->sym = NULL;  /* clear symbol in case it is undef */
    token_1->value = 0;
    token_1->mem_type = MT_EMPTY;
    token_1->is_opattr = FALSE; /* v2: added */

    if ( token_2->kind == EXPR_EMPTY )
        return( NOT_ERROR );

    /* bit 0: code label (near|far)? */
    if ( token_2->kind == EXPR_ADDR ) {
        if ( token_2->sym && token_2->sym->state != SYM_STACK &&
            ( token_2->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )
            token_1->value |= 0x01;

        if ( IsOffset( token_2 ) &&
            token_2->sym &&
            ( token_2->sym->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )
            token_1->value |= 0x01;

        /* bit 1: memory variable, relocatable data label? */
        if ( token_2->sym &&
            (( token_2->sym->mem_type == MT_TYPE ||
              ( token_2->mem_type & MT_SPECIAL ) == 0 ) ||
             ( token_2->mem_type == MT_EMPTY &&
              ( token_2->sym->mem_type & MT_SPECIAL ) == 0 )))
            token_1->value |= 0x02;
    }
    /* kind==EXPR_ADDR is not reliably set for indirect register addressing! */
    if ( token_2->indirect )
        token_1->value |= 0x02;


    /* bit 2: immediate value? */
    if ( token_2->kind == EXPR_CONST ||
        ( token_2->kind == EXPR_ADDR &&
         token_2->indirect == FALSE &&
         (( token_2->mem_type == MT_EMPTY && IsOffset(token_2) ) ||
          (( token_2->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )) &&
         ( token_2->sym->state == SYM_INTERNAL ||
          token_2->sym->state == SYM_EXTERNAL ) ) )
        token_1->value |= 0x04;

    /* bit 3: uses direct memory addressing?
     */
    if ( token_2->kind == EXPR_ADDR &&
        token_2->indirect == FALSE &&
        //token_2->base_reg == EMPTY &&
        (( token_2->mem_type == MT_EMPTY && token_2->instr == EMPTY ) ||
         ( token_2->mem_type == MT_TYPE ) || /* v2.05: added */
         (( token_2->mem_type & MT_SPECIAL ) == 0 ) ||
         token_2->mem_type == MT_PTR ) &&
        (token_2->sym == NULL ||
         token_2->sym->state == SYM_INTERNAL ||
         token_2->sym->state == SYM_EXTERNAL ) )
        token_1->value |= 0x08;

    if ( token_2->kind == EXPR_REG && token_2->indirect == FALSE )
        token_1->value |= 0x10; /* is a register value */

    //if ( token_2->kind != EXPR_UNDEF && ( token_2->sym == 0 || token_2->sym->isdefined == TRUE ) )
    if ( token_2->kind != EXPR_UNDEF && token_2->kind != EXPR_FLOAT && ( token_2->sym == 0 || token_2->sym->isdefined == TRUE ) )
        token_1->value |= 0x20; /* no reference to undefined label */

    if ( token_2->sym && token_2->sym->state == SYM_STACK ||
        ( token_2->indirect == TRUE &&
         token_2->base_reg != EMPTY &&
         ( AsmBuffer[token_2->base_reg]->value == T_ESP ||
          AsmBuffer[token_2->base_reg]->value == T_EBP ||
          AsmBuffer[token_2->base_reg]->value == T_BP ) ) )
        token_1->value |= 0x40; /* is relative to SS */

    if ( token_2->sym && token_2->sym->state == SYM_EXTERNAL )
        token_1->value |= 0x80; /* it's an external label */

    if ( oper == T_OPATTR )
        if ( token_2->sym )
            token_1->value |= token_2->sym->langtype << 8;

    DebugMsg1(("opattr_op returns %Xh\n", token_1->value));
    return( NOT_ERROR );
}

static ret_code short_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/****************************************************************************************/
{
    if ( token_2->kind != EXPR_ADDR ||
        ( token_2->mem_type != MT_EMPTY &&
         token_2->mem_type != MT_NEAR &&
         token_2->mem_type != MT_FAR &&
         token_2->mem_type != MT_PROC ) ) {
        CAsmError( EXPRESSION_MUST_BE_A_CODE_ADDRESS );
        return( ERROR );
    }
    TokenAssign( token_1, token_2 );
    token_1->instr = oper;
    return( NOT_ERROR );
}

static ret_code seg_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/**************************************************************************************/
{
    if ( token_2->sym->state == SYM_STACK ) {
        CAsmError( OPERAND_MUST_BE_RELOCATABLE );
        return( ERROR );
    }
    TokenAssign( token_1, token_2 );
    token_1->instr = oper;
    token_1->mem_type = MT_EMPTY; /* v2.04a */
    return( NOT_ERROR );
}

/* handles offset operators:
 * OFFSET, LROFFSEG, IMAGEREL, SECTIONREL
 */

static ret_code offset_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/*****************************************************************************************/
{
    if ( oper == T_OFFSET ) {
        /* if operand is a constant value, skip OFFSET operator */
        if ( token_2->kind == EXPR_CONST ) {
            TokenAssign( token_1, token_2 );
            return( NOT_ERROR );
        }
    }
    if ( sym && sym->state == SYM_GRP ) {
        invalid_operand( GetResWName( oper, NULL ), name );
        return( ERROR );
    }
    /* offset operator accepts types, but returns always 0 */
    if ( token_2->is_type )
        token_2->value = 0;

    TokenAssign( token_1, token_2 );
    token_1->instr = oper;

    if ( token_2->indirect ) {
        /* Masm v5.1 allows indirect operands, but Masm v6 with -Zm
         * won't accept it.
         */
        invalid_operand( GetResWName( oper, NULL ), name );
        return( ERROR );
    }
    /* skip memory type of operand, just address is needed */
    //token_1->mem_type = MT_NEAR;
    token_1->mem_type = MT_EMPTY;
    /* clear overrides ("offset SEG:xxx") */
    /* v2.01: override information is important for fixup creation!
     * the reason why it was cleared probably was to avoid creation
     * of a segment prefix. This case is now handled in the parser.
     */
    // token_1->override = EMPTY;
    return( NOT_ERROR );
}

static ret_code lowword_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/******************************************************************************************/
{
    TokenAssign( token_1, token_2 );
    if ( token_2->kind == EXPR_ADDR && token_2->instr != T_SEG ) {
        token_1->instr = T_LOWWORD;
        //token_1->mem_type = MT_WORD; /* v2.05 */
        token_1->mem_type = MT_EMPTY;
    }
    token_1->llvalue &= 0xffff;
    return( NOT_ERROR );
}

static ret_code highword_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/*******************************************************************************************/
{
    TokenAssign( token_1, token_2 );
    if ( token_2->kind == EXPR_ADDR && token_2->instr != T_SEG ) {
        token_1->instr = T_HIGHWORD;
        //token_1->mem_type = MT_WORD; /* v2.05 */
        token_1->mem_type = MT_EMPTY;
    }
    token_1->value = token_1->value >> 16;
    return( NOT_ERROR );
}

static ret_code low_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/**************************************************************************************/
{
    TokenAssign( token_1, token_2 );
    if ( token_2->kind == EXPR_ADDR && token_2->instr != T_SEG ) {
        /* LOW works for OMF/BIN only */
        if ( Options.output_format != OFORMAT_OMF &&
            Options.output_format != OFORMAT_BIN && token_2->sym ) {
            if ( error_msg )
                AsmErr( SYMBOL_TYPE_CONFLICT, token_2->sym->name );
            return( ERROR );
        }
        token_1->instr = T_LOW;
        token_1->mem_type = MT_EMPTY;
    }
    token_1->llvalue &= 0xff;
    return( NOT_ERROR );
}

static ret_code high_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/***************************************************************************************/
{
    TokenAssign( token_1, token_2 );
    if ( token_2->kind == EXPR_ADDR && token_2->instr != T_SEG ) {
        if ( Options.output_format != OFORMAT_OMF &&
            Options.output_format != OFORMAT_BIN && token_2->sym ) {
            if ( error_msg )
                AsmErr( SYMBOL_TYPE_CONFLICT, token_2->sym->name );
            return( ERROR );
        }
        token_1->instr = T_HIGH;
        token_1->mem_type = MT_EMPTY;
    }
    token_1->value = token_1->value >> 8;
    token_1->llvalue &= 0xff;
    return( NOT_ERROR );
}

#if LOHI32

static ret_code low32_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/****************************************************************************************/
{
    TokenAssign( token_1, token_2 );
    if ( token_2->kind == EXPR_ADDR && token_2->instr != T_SEG ) {
        token_1->instr = T_LOW32;
        token_1->mem_type = MT_DWORD;
    }
    token_1->llvalue &= 0xffffffff;
    return( NOT_ERROR );
}

static ret_code high32_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/*****************************************************************************************/
{
    TokenAssign( token_1, token_2 );
    if ( token_2->kind == EXPR_ADDR && token_2->instr != T_SEG ) {
        token_1->instr = T_HIGH32;
        token_1->mem_type = MT_DWORD;
    }
    token_1->llvalue = token_1->llvalue >> 32;
    return( NOT_ERROR );
}

#endif

static ret_code this_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/***************************************************************************************/
{
    if ( token_2->is_type == FALSE ) {
        CAsmError( INVALID_TYPE_EXPRESSION );
        return( ERROR );
    }
    if ( thissym == NULL ) {
        thissym = SymCreate( "", FALSE );
        /* fixme: set thissym->variable? */
        thissym->state = SYM_INTERNAL;
        thissym->isdefined = TRUE;
    }

    token_1->kind = EXPR_ADDR;
    thissym->mem_type = token_2->mem_type;
    if ( token_2->sym && token_2->sym->mem_type == MT_TYPE )
        thissym->type = token_2->sym->type;
    token_1->sym  = thissym;
    SetSymSegOfs( thissym );
    token_1->mem_type = thissym->mem_type;
    return( NOT_ERROR );
}

/* WIDTH and MASK operators */

static ret_code wimask_op( int oper, expr_list *token_1, expr_list *token_2, asm_sym *sym, char *name )
/*****************************************************************************************/
{
    /* additional check needed if operand is a type */
    if ( token_2->is_type ) {
        sym = token_2->type;
        if ((( dir_node *)sym)->e.structinfo->typekind != TYPE_RECORD ) {
            CAsmError( OPERAND_MUST_BE_RECORD );
            return( ERROR );
        }
    } else if ( token_2->kind == EXPR_CONST ) {
        sym = token_2->mbr;
    } else {
        sym = token_2->sym;
    }
    if ( oper == T_MASK ) {
        int i;
        token_1->value = 0;
        if ( token_2->is_type ) { /* get mask of the RECORD? */
            dir_node *dir = (dir_node *)sym;
            field_list *fl;
            for ( fl = dir->e.structinfo->head; fl; fl = fl->next ) {
                sym = fl->sym;
                for ( i = sym->offset ;i < sym->offset + sym->total_size; i++ )
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
    token_1->kind = EXPR_CONST;
    return( NOT_ERROR );
}

#define  res(token, function) function ,
static ret_code (* const unaryop[])( int, expr_list *, expr_list *, asm_sym *, char * ) = {
#include "unaryop.h"
};
#undef res

static ret_code plus_op( expr_list *token_1, expr_list *token_2 )
/***************************************************************/
{
    DebugMsg1(("plus_op: memtype=%Xh-%Xh value=%d-%d mbr=%s-%s type=%s-%s\n",
               token_1->mem_type, token_2->mem_type, 
               token_1->value, token_2->value,
               token_1->mbr ? token_1->mbr->name : "NULL",
               token_2->mbr ? token_2->mbr->name : "NULL",
               token_1->type ? token_1->type->name : "NULL",
               token_2->type ? token_2->type->name : "NULL" ));
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
        DebugMsg(("plus_op: error direct register\n" ));
        CAsmError( INVALID_USE_OF_REGISTER );
        return( ERROR );
    }
    if( check_same( token_1, token_2, EXPR_CONST ) ) {

        token_1->llvalue += token_2->llvalue;

    } else if( check_same( token_1, token_2, EXPR_ADDR ) ) {

        DebugMsg1(("plus_op: ADDR - ADDR\n" ));
        fix_struct_value( token_1 );
        fix_struct_value( token_2 );
        if ( index_connect( token_1, token_2 ) == ERROR )
            return( ERROR );
        if( token_2->sym != NULL ) {
            /* two relocatable labels not allowed */
            /* v2.05: changed */
            //if ( ( token_1->sym != NULL ) && ( Parse_Pass > PASS_1 || error_msg == FALSE ) ) {
            if ( token_1->sym != NULL &&
                token_1->sym->state != SYM_UNDEFINED &&
                token_2->sym->state != SYM_UNDEFINED ) {
                DebugMsg(("plus_op: two relocatable labels: %s - %s \n", token_1->sym->name, token_2->sym->name ));
                CAsmError( CANNOT_ADD_TWO_RELOCATABLE_LABELS );
                return( ERROR );
            }
            token_1->label = token_2->label;
            token_1->sym = token_2->sym;
            /* v2.05: added */
            if ( token_1->mem_type == MT_EMPTY )
                token_1->mem_type = token_2->mem_type;
        }
        token_1->value += token_2->value;

    } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_ADDR ) ) {

        DebugMsg1(("plus_op: CONST - ADDR\n" ));
        if( token_1->kind == EXPR_CONST ) {
            token_2->llvalue += token_1->llvalue;
            token_2->indirect |= token_1->indirect;
            if( token_1->explicit == TRUE ) {
                token_2->explicit = TRUE;
                token_2->mem_type = token_1->mem_type;
            } else if ( token_2->mem_type == MT_EMPTY )
                token_2->mem_type = token_1->mem_type;
            /* v2.05: added. See dotop2.asm, "mov eax, v2.f1[ebx*2]" */
            if ( token_2->mbr == NULL )
                token_2->mbr = token_1->mbr;
            TokenAssign( token_1, token_2 );
        } else {
            token_1->llvalue += token_2->llvalue;
            /* v2.04: added. to make this case behave like
             * the CONST - REG case (see below).
             */
            if ( token_1->mem_type == MT_EMPTY )
                token_1->mem_type = token_2->mem_type;
        }
        fix_struct_value( token_1 );

    } else if( check_both( token_1, token_2, EXPR_ADDR, EXPR_REG ) ) {

        DebugMsg1(("plus_op: ADDR - REG\n" ));
        if( token_1->kind == EXPR_REG ) {
#if 0
            /* why is this an error? [ecx+offset xxxtable] */
            if( token_2->instr != EMPTY ) {
                CAsmError( LABEL_EXPECTED );
                DebugMsg(("plus_op: error 3\n" ));
                return( ERROR );
            }
#endif
            if ( index_connect( token_2, token_1 ) == ERROR )
                return( ERROR );
            TokenAssign( token_1, token_2 );
        } else {
            if ( index_connect( token_1, token_2 ) == ERROR )
                return( ERROR );
        }
        fix_struct_value( token_1 );

    } else if( check_same( token_1, token_2, EXPR_REG ) ) {

        if ( index_connect( token_1, token_2 ) == ERROR )
            return( ERROR );
        token_1->kind = EXPR_ADDR;

    } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_REG ) ) {

        DebugMsg1(("plus_op: CONST - REG\n" ));
        if( token_2->kind == EXPR_REG ) {
            token_1->base_reg = token_2->base_reg;
            token_1->idx_reg = token_2->idx_reg;
            token_2->base_reg = EMPTY;
            token_2->idx_reg = EMPTY;
        }

        /* v2: to make '+' work like '.'
         * example: [bx+<struct_mbr>],-1
         * will set expression's mem_type to the struct_mbr's one.
         */
        /* v2.01: use it for SYM_STRUCT_FIELDs ONLY! */
        //if ( ModuleInfo.oldstructs && token_1->mem_type == MT_EMPTY )
        /* v2.03: call [bx+struct.mbr] should also work */
        //if ( ModuleInfo.oldstructs && token_1->mem_type == MT_EMPTY && token_2->mbr )
        if ( token_1->mem_type == MT_EMPTY && token_2->mbr )
            token_1->mem_type = token_2->mem_type;

        token_1->llvalue += token_2->llvalue;
        token_1->indirect |= token_2->indirect;
        token_1->kind = EXPR_ADDR;
        //} else {
    } else {
        DebugMsg(("plus_op: error 4\n" ));
        return( ConstError( token_1, token_2 ) );
    }
    return( NOT_ERROR );
}

static ret_code minus_op( expr_list *token_1, expr_list *token_2 )
/****************************************************************/
{
    struct asm_sym      *sym;
    /*
     * The only formats allowed are:
     *        constant - constant
     *         address - constant       ( only in this order )
     *         address - address
     *        register - constant       ( only inside [] and in this
     *                                    order )
     */

    DebugMsg1(("minus_op: types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));

    if( check_direct_reg( token_1, token_2 ) == ERROR ) {
        DebugMsg(("minus_op: error direct register\n"));
        CAsmError( INVALID_USE_OF_REGISTER );
        return( ERROR );
    }

    /* added for v1.94. It's related to the change done in MakeConst()!
     */
    if ( token_1->kind == EXPR_ADDR &&
        token_2->kind == EXPR_ADDR &&
        token_2->sym &&
        token_2->sym->state == SYM_UNDEFINED &&
        error_msg != FALSE )
        ; /* don't convert token2 to a constant! */
    else
        MakeConst( token_2 );

    if( check_same( token_1, token_2, EXPR_CONST ) ) {

        DebugMsg1(("minus_op: CONST-CONST\n" ));
        token_1->llvalue -= token_2->llvalue;

    } else if( token_1->kind == EXPR_ADDR &&
              token_2->kind == EXPR_CONST ) {

        DebugMsg1(("minus_op: ADDR-CONST\n" ));
        token_1->llvalue -= token_2->llvalue;
        fix_struct_value( token_1 );

    } else if( check_same( token_1, token_2, EXPR_ADDR ) ){

        DebugMsg1(("minus_op: ADDR-ADDR\n" ));
        fix_struct_value( token_1 );
        fix_struct_value( token_2 );
        if( token_2->base_reg != EMPTY || token_2->idx_reg != EMPTY ) {
            CAsmError( INVALID_USE_OF_REGISTER );
            DebugMsg(("minus_op error 2\n"));
            return( ERROR );
        }
        if( token_2->label == EMPTY ) {
            token_1->value -= token_2->value;
            token_1->indirect |= token_2->indirect;
        } else {
            if( token_1->label == EMPTY || token_1->sym == NULL || token_2->sym == NULL ) {
                /* v2.05: error msg changed */
                //CAsmError( SYNTAX_ERROR );
                CAsmError( OPERAND_MUST_BE_RELOCATABLE );
                DebugMsg(("minus_op error 3\n"));
                return( ERROR );
            }
            /* handle first operand */
            sym = token_1->sym;
#if 0 /* v2.05: can't happen */
            //if( Parse_Pass > PASS_1 && sym->defined == FALSE ) {
            if( Parse_Pass > PASS_1 && sym->state == SYM_UNDEFINED ) {
                if( error_msg )
                    AsmErr( LABEL_NOT_DEFINED, sym->name );
                DebugMsg(("minus_op error 4\n"));
                return( ERROR );
            }
#endif
            token_1->value += sym->offset;

            /* handle second operand */
            sym = token_2->sym;
            if( Parse_Pass > PASS_1 ) {
#if 0 /* v2.05: can't happen */
                if( sym->state == SYM_UNDEFINED ) {
                    if( error_msg )
                        AsmErr( LABEL_NOT_DEFINED, sym->name );
                    DebugMsg(("minus_op error 5\n"));
                    return( ERROR );
                }
#endif
                /* if symbol is external, error - unless it's the same symbol */
                if ( ( sym->state == SYM_EXTERNAL ||
                     token_1->sym->state == SYM_EXTERNAL) &&
                    sym != token_1->sym ) {
                    if ( error_msg )
                        AsmErr(INVALID_USE_OF_EXTERNAL_SYMBOL, token_1->sym->name );
                    DebugMsg(("minus_op error 6\n"));
                    return( ERROR );
                }
                /* check if the 2 offsets belong to the same segment */
                if ( sym->segment != token_1->sym->segment ) {
                    CAsmError( OPERANDS_MUST_BE_IN_SAME_SEGMENT );
                    DebugMsg(("minus_op error 7\n"));
                    return( ERROR );
                }
            }
            /* v2.05: if at least one label is undefined, assume result=1 */
            if ( token_1->sym->state == SYM_UNDEFINED ||
                token_2->sym->state == SYM_UNDEFINED )
                token_1->value = 1;
            else {
                token_1->value -= sym->offset;
                token_1->value -= token_2->value;
            }
            token_1->label = EMPTY;
            token_1->sym = NULL;
            if( token_1->base_reg == EMPTY && token_1->idx_reg == EMPTY ) {

                if( token_1->instr == T_OFFSET && token_2->instr == T_OFFSET )
                    token_1->instr = EMPTY;

                token_1->kind = EXPR_CONST;
                /* the type changes from address to constant.
                 */
                token_1->indirect = FALSE;
            } else {
                DebugMsg1(("minus_op, exit, ADDR, base=%u, idx=%u\n", token_1->base_reg, token_1->idx_reg ));
                token_1->kind = EXPR_ADDR;
                token_1->indirect |= token_2->indirect;
            }
            token_1->explicit = FALSE;
            token_1->mem_type = MT_EMPTY;
        }

    } else if( token_1->kind == EXPR_REG &&
              token_2->kind == EXPR_CONST ) {

        token_1->llvalue = -1 * token_2->llvalue;
        token_1->indirect |= token_2->indirect;
        token_1->kind = EXPR_ADDR;

    } else {
        DebugMsg(("minus_op, exit, error: types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));
        return( ConstError( token_1, token_2 ) );
    }
    return( NOT_ERROR );
}

static ret_code dot_op( expr_list *token_1, expr_list *token_2 )
/**************************************************************/
{
    /* this code needs cleanup! some stuff is obsolete. */

    DebugMsg1(("dot_op: t1-t2 type=%s-%s mbr=%s-%s\n",
               token_1->type ? token_1->type->name : "NULL",
               token_2->type ? token_2->type->name : "NULL",
               token_1->mbr  ? token_1->mbr->name  : "NULL",
               token_2->mbr  ? token_2->mbr->name  : "NULL" ));

    /*
     * The formats allowed are:
     *        [register]      . (type) constant
     *        label           . (type) constant
     *        (type) constant . (type) constant
     *
     * with OPTION OLDSTRUCTS:
     *        [register]      . address
     *        address         . address
     */

    if( check_direct_reg( token_1, token_2 ) == ERROR ) {
        DebugMsg(("dot_op: error direct register\n"));
        CAsmError( INVALID_USE_OF_REGISTER );
        return( ERROR );
    }
    if( check_same( token_1, token_2, EXPR_ADDR ) ) {

        DebugMsg1(("dot_op, ADDR - ADDR, t1-t2 memtype=%X-%X sym=%s-%s\n",
                   token_1->mem_type, token_2->mem_type,
                   token_1->sym  ? token_1->sym->name  : "NULL",
                   token_2->sym  ? token_2->sym->name  : "NULL" ));

#if 1 /* v2.05: error */
        if ( token_2->mbr == NULL && !ModuleInfo.oldstructs ) {
            CAsmError( STRUCTURE_FIELD_EXPECTED );
            return( ERROR );
        }
#endif
        if ( index_connect( token_1, token_2 ) == ERROR )
            return( ERROR );

        if( token_2->sym != NULL ) {
            if( token_1->sym != NULL &&
                token_1->sym->state != SYM_UNDEFINED &&
                token_2->sym->state != SYM_UNDEFINED ) {
                DebugMsg(("dot_op: error, two relocatable labels: %s - %s \n", token_1->sym->name, token_2->sym->name ));
                CAsmError( CANNOT_ADD_TWO_RELOCATABLE_LABELS );
                return( ERROR );
            }
            token_1->label = token_2->label;
            token_1->sym = token_2->sym;
        }
        if( token_2->mbr != NULL ) {
            token_1->mbr = token_2->mbr;
        }
        token_1->value += token_2->value;
        if( token_1->explicit == FALSE ) {
            token_1->mem_type = token_2->mem_type;
        }
        if ( token_2->type )
            token_1->type = token_2->type;

    } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_ADDR ) ) {

        if( token_1->kind == EXPR_CONST ) {
            DebugMsg1(("dot_op, CONST - ADDR, t1-t2 memtype=%Xh-%Xh istype=%u-%u\n",
                       token_1->mem_type, token_2->mem_type, token_1->is_type, token_2->is_type ));
#if 1 /* v2.05: error */
            /* <structname>.<member>[<index_reg>] is ALWAYS ok! */
            if ( ( !ModuleInfo.oldstructs ) &&
                ( token_1->is_type == FALSE && token_1->mbr == NULL ) )
                CAsmError( STRUCTURE_FIELD_EXPECTED );
#endif
            /* for TYPE.xxx, return offset instead of size */
            if ((token_1->mbr) && (token_1->mbr->state == SYM_TYPE))
                token_1->llvalue = token_1->mbr->offset;
            token_2->indirect |= token_1->indirect;
            token_2->llvalue += token_1->llvalue;
            TokenAssign( token_1, token_2 );
        } else {
            DebugMsg1(("dot_op, ADDR - CONST, t1-t2 memtype=%Xh-%Xh t1.explicit=%u\n",
                       token_1->mem_type, token_2->mem_type, token_1->explicit ));
            /* for [var].TYPE | STRUCT_FIELD, use offset instead of size */
            if ( token_2->mbr && token_2->mbr->state == SYM_TYPE )
                token_2->llvalue = token_2->mbr->offset;
            token_1->llvalue += token_2->llvalue;
            if( token_2->mbr != NULL ) {
                token_1->mbr = token_2->mbr;
#if 1
                /* temp. disabled in v1.95, test case:
                 * mov eax,(<struct> ptr [ebx]).F1
                 * however: mov ax, word ptr var[bx].F1 ???
                 * the condition can't be disabled. Instead the PTR
                 * operator must NOT set the explicit flag if the
                 * first operand is a structure.
                 */
                if( token_1->explicit == FALSE )
#endif
                    token_1->mem_type = token_2->mem_type;
            }

            DebugMsg1(("dot_op, ADDR - CONST, t1.type=%X (%s), t2.type=%X (%s)\n",
                       token_1->type,
                       token_1->type ? token_1->type->name : "",
                       token_2->type,
                       token_2->type ? token_2->type->name : "" ));
#if 0 /* v1.96 */
            if ( token_2->type )
#endif
                token_1->type = token_2->type;
        }

    } else if( token_1->kind == EXPR_REG && token_2->kind == EXPR_ADDR ) {

        DebugMsg1(("dot_op, REG - ADDR, t1-t2 memtype=%Xh-%Xh\n", token_1->mem_type, token_2->mem_type));
#if 1 /* v2.05: error */
        if ( !ModuleInfo.oldstructs ) {
            CAsmError( STRUCTURE_FIELD_EXPECTED );
            return( ERROR );
        }
#endif
        if( token_2->instr != EMPTY ) {
            CAsmError( STRUCTURE_FIELD_EXPECTED );
            DebugMsg(("dot_op error 3\n"));
            return( ERROR );
        }
        if ( index_connect( token_2, token_1 ) == ERROR )
            return( ERROR );

        TokenAssign( token_1, token_2 );

    } else if( token_1->kind == EXPR_ADDR && token_2->kind == EXPR_REG ) {

#if 1 /* v2.05: error */
        if ( !ModuleInfo.oldstructs )
            CAsmError( STRUCTURE_FIELD_EXPECTED );
#endif
        DebugMsg1(("dot_op, ADDR - REG, t1-t2 memtype=%Xh-%Xh\n", token_1->mem_type, token_2->mem_type));
        if ( index_connect( token_1, token_2 ) == ERROR )
            return( ERROR );


    } else if( check_both( token_1, token_2, EXPR_CONST, EXPR_REG ) ) {

        if( token_2->kind == EXPR_REG ) {
            DebugMsg1(("dot_op, CONST - REG, t1-t2 memtype=%Xh-%Xh\n", token_1->mem_type, token_2->mem_type));
#if 1 /* v2.05: error */
            if ( !ModuleInfo.oldstructs )
                CAsmError( STRUCTURE_FIELD_EXPECTED );
#endif
            token_1->base_reg = token_2->base_reg;
            token_1->idx_reg = token_2->idx_reg;
            token_2->base_reg = EMPTY;
            token_2->idx_reg = EMPTY;
        } else {
            DebugMsg1(("dot_op, REG - CONST, t1-t2 memtype=%Xh-%Xh\n", token_1->mem_type, token_2->mem_type));
#if 1 /* v2.05: error */
            if ( !ModuleInfo.oldstructs ) {
                if ( token_2->mbr == NULL && ( token_2->type == NULL || token_2->type == (asm_sym *)nullstruct ) )
                    CAsmError( STRUCTURE_FIELD_EXPECTED );
            }
#endif
            /* for [reg].TYPE.xxx, return offset instead of size */
            /* this is most likely obsolete */
            if (token_2->mbr) {
                if (token_2->mbr->state == SYM_TYPE)
                    token_2->llvalue = token_2->mbr->offset;
                else if (token_1->mbr == NULL)
                    token_1->mbr = token_2->mbr;
            }

            //if( token_1->mem_type == MT_EMPTY)
            token_1->mem_type = token_2->mem_type;
            token_1->type = token_2->type;
        }
        token_1->llvalue += token_2->llvalue;
        token_1->indirect |= token_2->indirect;
        token_1->explicit = FALSE; /* added v1.95 */
        token_1->kind = EXPR_ADDR;
    } else if ( token_1->kind == EXPR_CONST && token_2->kind == EXPR_CONST ) {
        DebugMsg1(("dot_op, CONST - CONST, t1-t2 value=%u-%u, memtype=%Xh-%Xh istype=%u-%u\n",
                   token_1->value, token_2->value, token_1->mem_type, token_2->mem_type, token_1->is_type, token_2->is_type));
        if ( token_2->mbr == NULL && !ModuleInfo.oldstructs ) {
            DebugMsg(("dot_op: error, token_2.mbr=NULL\n" ));
            CAsmError( STRUCTURE_FIELD_EXPECTED );
            return( ERROR );
        }
        if ( token_1->type != NULL ) {
            /* old token is a type */
            /* the value (=size) is ignored then */
            token_1->llvalue = token_2->llvalue;
            token_1->mbr = token_2->mbr;
            /* v2.0: copy mem_type (test case: mov ds:[<struct>.<mbr>], 123) */
            token_1->mem_type = token_2->mem_type;
            /* v2.05: removed, it's still a type constant */
            //token_1->is_type = FALSE;
            token_1->is_type = token_2->is_type;
            /* either clear <type> or use the renewed one */
            if ( token_1->type != token_2->type )
                token_1->type = token_2->type;
            else
                token_1->type = NULL;
        } else {
            /* old token is NOT a type */
            /* most likely a number or an MT_ABS symbol! */
            /* so the TOTAL of both constants is required */
            token_1->llvalue += token_2->llvalue;
            token_1->mbr = token_2->mbr;
            token_1->mem_type = token_2->mem_type;
        }
    } else {
        DebugMsg(("dot_op: error, unknown type combination, token_1->type=%u, token_2->type=%u\n", token_1->type, token_2->type ));
        CAsmError( STRUCTURE_FIELD_EXPECTED );
        return( ERROR );
    }
    return( NOT_ERROR );
}

static ret_code colon_op( expr_list *token_1, expr_list *token_2 )
/****************************************************************/
{
    int_32              temp;
    struct asm_sym      *sym;
    /*
     * The only formats allowed are:
     *         seg_reg : const
     *         seg_reg : address
     *         seg_label : const
     *         seg_label : address
     *         ( seg_label = segment or group symbol )
     */
    DebugMsg1(("colon_op: t1-t2 kind=%u-%u type=%s-%s is_type=%u-%u\n",
               token_1->kind, token_2->kind,
               token_1->type ? token_1->type->name : "NULL",
               token_2->type ? token_2->type->name : "NULL",
               token_1->is_type, token_2->is_type ));
    if( token_2->override != EMPTY ) {
        if ( error_msg && Parse_Pass == PASS_1 )
            AsmWarn( 2, MULTIPLE_OVERRIDES, AsmBuffer[token_2->override]->string_ptr );
        DebugMsg(("colon_op: ignored override=%s\n", AsmBuffer[token_2->override]->string_ptr ));
        //return( ERROR ); /* v2.01: emit a warning and continue */
    }
    switch ( token_2->kind ) {
    case EXPR_REG:
        /* v2.05: register as second operand must be enclosed in [] */
        if ( token_2->indirect == FALSE ) {
            CAsmError( INVALID_USE_OF_REGISTER );
            return( ERROR );
        }
        break;
    case EXPR_FLOAT:
        CAsmError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
        return( ERROR );
    }

    if( token_1->kind == EXPR_REG ) {

        if( token_1->idx_reg != EMPTY ) {
            CAsmError( INVALID_USE_OF_REGISTER );
            DebugMsg(("colon_op error 2\n"));
            return( ERROR );
        }
        /* make sure it's a segment register BEFORE the ':' */
        temp = AsmBuffer[token_1->base_reg]->value;
        if ( ( GetValueSp( temp ) & OP_SR ) == 0 ) {
            CAsmError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
            return( ERROR );
        }

        token_2->override = token_1->base_reg;
        token_2->indirect |= token_1->indirect;

        if ( token_2->kind == EXPR_CONST ) {
            token_2->kind = EXPR_ADDR;
            /* v2.05: type flag cleared HERE, not in dot_op()
             * v2.05rc17 problem: mov es:byte ptr <var>,0
             * so the flag isn't cleared at all now.
             */
            //token_2->is_type = FALSE;
        }

        if( token_1->explicit ) {
            token_2->explicit = token_1->explicit;
            token_2->mem_type = token_1->mem_type;
            token_2->Ofssize  = token_1->Ofssize;
        }
        TokenAssign( token_1, token_2 );

        /*
         currently the <type> token isn't copied by
         TokenAssign (which is probably just for historical reasons).
         So copy it manually!
         v1.95: only copy if it is != NULL!
         Testcase: (<type> ptr DS:[0]).<struct_field> ...
         In this case the DS:[] will clear the <type>, as a result
         the dot operator won't have a valid assume and the code fails.
         */
        if ( token_2->type )
            token_1->type = token_2->type;

    } else if( token_1->kind == EXPR_ADDR &&
              /* token_2->kind == EXPR_ADDR && */
              token_1->override == EMPTY &&
              token_1->instr == EMPTY &&
              token_1->value == 0 &&
              token_1->sym &&
              token_1->base_reg == EMPTY &&
              token_1->idx_reg == EMPTY ) {

        sym = token_1->sym;

        if( sym->state == SYM_GRP || sym->state == SYM_SEG ) {
            token_2->kind = EXPR_ADDR;
            token_2->override = token_1->label;
            token_2->indirect |= token_1->indirect;
            if( token_1->explicit ) {
                token_2->explicit = token_1->explicit;
                token_2->mem_type = token_1->mem_type;
                token_2->Ofssize  = token_1->Ofssize;
            }
            TokenAssign( token_1, token_2 );
            token_1->type = token_2->type;

        } else if( Parse_Pass > PASS_1 || sym->state != SYM_UNDEFINED ) {
            CAsmError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
            DebugMsg(("colon_op error 4\n"));
            return( ERROR );
        }
    } else {
        CAsmError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
        DebugMsg(("colon_op error 5\n"));
        return( ERROR );
    }
    return( NOT_ERROR );
}

static ret_code calculate( expr_list *token_1, expr_list *token_2, int oper )
/***************************************************************************/
/* Perform the operation between token_1 and token_2
 * <oper> is the index of the operator token.
 * possible operators:
 *  T_POSITIVE (unary +)
 *  T_NEGATIVE (unary -)
 *  T_OP_BRACKET       is an alias for '+'
 *  T_OP_SQ_BRACKET    is an alias for '+'
 *  '+'
 *  '-'
 *  '*'
 *  '/'
 *  T_DOT
 *  T_COLON
 *  T_BINARY_OPERATOR ( PTR, MOD, GE, GT, LE, GT, EQ, NE, also AND, OR, XOR, SHL, SHR )
 *  T_UNARY_OPERATOR ( OFFSET, SHORT, ... , also NOT )
 *
 * to be done: the code can be simplified because currently
 *             expression type is set to EXPR_REG when a
 *             register was found - even if it is inside [].
 *             A reg inside [] should ALWAYS give EXPR_ADDR!
 */
{
    int_32              temp;
    struct asm_sym      *sym;
    char                *name;

    /* avoid to use the <string> member once it's part of an expression!
     * the <value> member is the one to be used then.
     * test case: db "a"+80h
     */
    token_1->string = NULL;

    switch( AsmBuffer[oper]->token ) {
    case T_POSITIVE:
        /*
         * The formats allowed are:
         *        + constant
         *        + float
         */

        MakeConst( token_2 );
        if( token_2->kind == EXPR_CONST ) {
            token_1->kind = EXPR_CONST;
            token_1->llvalue = token_2->llvalue;
        } else if( token_2->kind == EXPR_FLOAT ) {
            token_1->kind = EXPR_FLOAT;
            token_1->base_reg = token_2->base_reg;
            token_1->negative = token_2->negative;
        } else {
            CAsmError( CONSTANT_EXPECTED );
            DebugMsg(("calculate(T_POSITIVE): error 1\n"));
            return( ERROR );
        }
        break;
    case T_NEGATIVE:
        DebugMsg1(("calculate(T_NEGATIVE): value=%I64X\n", token_2->llvalue ));
        /*
         * The formats allowed are:
         *        - constant
         *        - float
         */

        MakeConst( token_2 );
        if( token_2->kind == EXPR_CONST ) {
            token_1->kind = EXPR_CONST;
            token_1->llvalue = -token_2->llvalue;
        } else if( token_2->kind == EXPR_FLOAT ) {
            token_1->kind = EXPR_FLOAT;
            token_1->base_reg = token_2->base_reg;
            token_1->negative = 1 - token_2->negative;
        } else {
            CAsmError( CONSTANT_EXPECTED );
            DebugMsg(("calculate(T_NEGATIVE), error 1\n"));
            return( ERROR );
        }
        break;
    case T_OP_BRACKET:
    case T_OP_SQ_BRACKET:
#if 1
        /* v2.03: make JWasm reject syntax variants
         * "mov eax, DWORD [EBX]"
         * "mov eax, DWORD [var_name]"
         * variants still valid:
         * "mov eax, DWORD [WORD]"
         * "mov eax, DWORD [4]"
         * "mov eax, [DWORD][EBX]"
         */
        if ( token_1->is_type == TRUE &&
            (token_2->kind == EXPR_ADDR || token_2->kind == EXPR_REG ) ) {
            CAsmError( SYNTAX_ERROR_IN_EXPRESSION );
            DebugMsg(("calculate(%s): incompatible usage of (simple) type\n", AsmBuffer[oper]->string_ptr ));
            return( ERROR );
        }
#endif
        DebugMsg1(("calculate(%s): calling plus_op()\n", AsmBuffer[oper]->string_ptr ));
        /* no break */
    case '+':
#ifdef DEBUG_OUT
        if ( plus_op( token_1, token_2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( plus_op( token_1, token_2 ) );
#endif
    case '-':
#ifdef DEBUG_OUT
        if ( minus_op( token_1, token_2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( minus_op( token_1, token_2 ) );
#endif
    case T_DOT:
#ifdef DEBUG_OUT
        if ( dot_op( token_1, token_2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( dot_op( token_1, token_2 ) );
#endif
    case T_COLON:
#ifdef DEBUG_OUT
        if ( colon_op( token_1, token_2 ) == ERROR )
            return( ERROR );
        break;
#else
        return( colon_op( token_1, token_2 ) );
#endif
    case '*':
        /*
         * The only formats allowed are:
         *        constant * constant
         *        register * scaling factor ( 1, 2, 4 or 8 )
         *                   386 only
         */
        DebugMsg1(("calculate(*): kind=%u-%u value=%d-%d mbr=%X-%X\n",
                   token_1->kind,  token_2->kind,
                   token_1->value, token_2->value,
                   token_1->mbr,   token_2->mbr ));

        MakeConst( token_1 );
        MakeConst( token_2 );

        if( check_same( token_1, token_2, EXPR_CONST ) ) {
            token_1->llvalue *= token_2->llvalue;
        } else if( check_both( token_1, token_2, EXPR_REG, EXPR_CONST ) ) {
            if( check_direct_reg( token_1, token_2 ) == ERROR ) {
                DebugMsg(("calculate(*) error direct register\n"));
                CAsmError( INVALID_USE_OF_REGISTER );
                return( ERROR );
            }
            /* scaling factor */
            if( token_2->kind == EXPR_REG ) {
                /* scale * reg */
                token_1->idx_reg = token_2->base_reg;
                token_1->scale = token_1->value;
                token_1->value = 0;
                //token_2->base_reg = EMPTY;
            } else {
                /* reg * scale */
                token_1->idx_reg = token_1->base_reg;
                token_1->scale = token_2->value;
            }
            token_1->base_reg = EMPTY;
            token_1->indirect |= token_2->indirect;
            token_1->kind = EXPR_ADDR;
        } else {
            DebugMsg(("calculate(*) error 2\n"));
            return( ConstError( token_1, token_2 ) );
        }
        break;
    case '/':
        /*
         * The only formats allowed are:
         *        constant / constant
         */
        DebugMsg1(("calculate(/): types tok1=%u, tok2=%u\n", token_1->type, token_2->type ));
        MakeConst( token_1 );
        MakeConst( token_2 );

        if( check_same( token_1, token_2, EXPR_CONST ) == FALSE ) {
            DebugMsg(("calculate(/) error 1\n"));
            return( ConstError( token_1, token_2 ) );
        }

        if ( token_2->llvalue == 0 ) {
            CAsmError( DIVIDE_BY_ZERO_IN_EXPR );
            DebugMsg(("calculate(/) error 2\n"));
            return( ERROR );
        }

        token_1->llvalue /= token_2->llvalue;
        break;
    case T_BINARY_OPERATOR:
        DebugMsg1(("calculate(%s [T_BINARY_OPERATOR] ): t1-t2 kind %d-%d memtype %X-%X sym %s-%s type %s-%s\n",
                   AsmBuffer[oper]->string_ptr,
                   token_1->kind, token_2->kind,
                   token_1->mem_type, token_2->mem_type,
                   token_1->sym  ? token_1->sym->name  : "NULL",
                   token_2->sym  ? token_2->sym->name  : "NULL",
                   token_1->type ? token_1->type->name : "NULL",
                   token_2->type ? token_2->type->name : "NULL" ));

        if ( AsmBuffer[oper]->value == T_PTR ) {
            if ( token_1->is_type == FALSE ) {
                if ( token_1->sym && token_1->sym->state == SYM_UNDEFINED ) {
                    CreateTypeSymbol( token_1->sym, NULL, TRUE );
                    token_1->type = token_1->sym;
                    token_1->sym = NULL;
                    token_1->is_type = TRUE;
                } else {
                    DebugMsg(("calculate(PTR), error 1: t1 is_type == FALSE\n"));
                    CAsmError( INVALID_TYPE_EXPRESSION );
                    return( ERROR );
                }
            }
            token_2->explicit = TRUE;
            /* v2.02: if operand is a register, make sure
             * that invalid combinations ("DWORD PTR AX") are flagged.
             */
            if ( token_2->kind == EXPR_REG && token_2->indirect == FALSE ) {
                temp = AsmBuffer[token_2->base_reg]->value;
                /* for segment registers, both size 2 and 4 is ok.*/
                if ( GetValueSp( temp ) & OP_SR ) {
                    if ( token_1->value != 2 && token_1->value != 4 ) {
                        CAsmError( INVALID_USE_OF_REGISTER );
                        return( ERROR );
                    }
                } else if ( token_1->value != SizeFromRegister( temp ) ) {
                    CAsmError( INVALID_USE_OF_REGISTER );
                    return( ERROR );
                }
            } else if ( token_2->kind == EXPR_FLOAT ) {
                if ( !( token_1->mem_type & MT_FLOAT ) ) {
                    CAsmError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
                    return( ERROR );
                }
            }
            token_2->mem_type = token_1->mem_type;
            token_2->Ofssize  = token_1->Ofssize;
            /* v2.04: added */
            if ( token_2->is_type )
                token_2->value  = token_1->value;
            if ( token_1->override != EMPTY ) {
                if ( token_2->override == EMPTY )
                    token_2->override = token_1->override;
                token_2->kind = EXPR_ADDR;
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

        if ( check_same( token_1, token_2, EXPR_CONST ) )
            ;
        /* if it's EQ, NE, LE [, ...], operands may be either constants
         or relocatable labels */
        else if ( AsmBuffer[oper]->precedence == CMP_PRECEDENCE &&
                 token_1->kind != EXPR_CONST ) {
            if ( token_1->kind == EXPR_ADDR && token_1->indirect == FALSE && token_1->sym )
                if ( token_2->kind == EXPR_ADDR && token_2->indirect == FALSE && token_2->sym ) {
                    if ( MakeConst2( token_1, token_2 ) == ERROR ) {
                        DebugMsg(("calculate(%s) error 1\n", AsmBuffer[oper]->string_ptr ));
                        return( ERROR );
                    }
                } else {
                    CAsmError( OPERAND_MUST_BE_RELOCATABLE );
                    DebugMsg(("calculate(%s) error 2\n", AsmBuffer[oper]->string_ptr ));
                    return( ERROR );
                }
            else {
                CAsmError( CONSTANT_OR_RELOCATABLE_LABEL_EXPECTED );
                DebugMsg(("calculate(%s) error 3\n", AsmBuffer[oper]->string_ptr ));
                return( ERROR );
            }
        } else {
            DebugMsg(("calculate(%s) error 4\n", AsmBuffer[oper]->string_ptr ));
            return( ConstError( token_1, token_2 ) );
        }

        switch( AsmBuffer[oper]->value ) {
        case T_EQ:
#if 1 /* v2.03: added */
            DebugMsg1(("calculate(EQ): values=%I64u/%I64u types=%u/%u memtypes=%X/%X\n",
                      token_1->value64, token_2->value64, token_1->is_type, token_2->is_type, token_1->mem_type, token_2->mem_type  ));
            /* if both operands are types, do a more strict comparison! */
            if ( token_1->is_type && token_2->is_type ) {
                token_1->value64 = ( ((token_1->value64 == token_2->value64) &&
                ( token_1->mem_type == token_2->mem_type )) ? -1:0 );
            } else
#endif
            token_1->value64 = ( token_1->value64 == token_2->value64 ? -1:0 );
            break;
        case T_NE:
#if 1 /* v2.03: added */
            DebugMsg1(("calculate(NE): values=%I64u/%I64u istypes=%u/%u memtypes=%X/%X\n",
                       token_1->value64, token_2->value64, token_1->is_type, token_2->is_type, token_1->mem_type, token_2->mem_type ));
            /* if both operands are types, do a more strict comparison! */
            if ( token_1->is_type && token_2->is_type ) {
                token_1->value64 = ( ((token_1->value64 != token_2->value64) ||
                ( token_1->mem_type != token_2->mem_type )) ? -1:0 );
            } else
#endif
            token_1->value64 = ( token_1->value64 != token_2->value64 ? -1:0 );
            break;
        case T_LT:
            token_1->value64 = ( token_1->value64 <  token_2->value64 ? -1:0 );
            break;
        case T_LE:
            token_1->value64 = ( token_1->value64 <= token_2->value64 ? -1:0 );
            break;
        case T_GT:
            token_1->value64 = ( token_1->value64 >  token_2->value64 ? -1:0 );
            break;
        case T_GE:
            token_1->value64 = ( token_1->value64 >= token_2->value64 ? -1:0 );
            break;
        case T_MOD:
            if ( token_2->llvalue == 0 ) {
                CAsmError( DIVIDE_BY_ZERO_IN_EXPR );
                return( ERROR );
            } else
                token_1->llvalue %= token_2->llvalue;
            break;
        case T_SHL:
            /* v2.04: check for shift count < 0 */
            DebugMsg1(("calculate(SHL): value=%I64X << %lu (max=%u)\n", token_1->llvalue, token_2->value, 8 * sizeof( token_1->llvalue ) ));
            if ( token_2->value < 0 )
                CAsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            else if ( token_2->value >= ( 8 * sizeof( token_1->llvalue ) ) )
                token_1->llvalue = 0;
            else
                token_1->llvalue = token_1->llvalue << token_2->value;
            /* v2.01: result is 64-bit only if mode is USE64 */
            if ( ModuleInfo.Ofssize <= USE32 ) {
                token_1->hvalue = 0;
                token_1->hlvalue = 0;
            }
            break;
        case T_SHR:
            /* Masm v8 works with unsigned 64-bit,
             * Masm v6 masks shift count with 0x3F.
             * v2.04: does behave like Masm v8+.
             * there is a problem with some compilers if shift
             * count is >= 64. So in this case the result is zeroed manually
             */
#if 0
            if ( token_1->hvalue == -1 ) {
                token_1->hvalue = 0;
                token_1->hlvalue = 0;
            }
#endif
            /* v2.04: check for shift count < 0 */
            if ( token_2->value < 0 )
                CAsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            else if ( token_2->value >= ( 8 * sizeof( token_1->llvalue ) ) )
                token_1->llvalue = 0;
            else
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
        DebugMsg1(("calculate(%s [T_UNARY_OPERATOR]): i=%u, token_2 type=%s memtype=%X is_type=%u indirect=%u\n",
                   AsmBuffer[oper]->string_ptr, oper,
                   token_2->type ? token_2->type->name : "NULL",
                   token_2->mem_type, token_2->is_type, token_2->indirect ));
        /* NOT is an instruction and hence has no valid
         * value to be returned by GetValueSp() or GetSflagsSp()!
         */
        if( AsmBuffer[oper]->value == T_NOT ) {
            MakeConst( token_2 );
            if( token_2->kind != EXPR_CONST ) {
                CAsmError( CONSTANT_OPERAND_EXPECTED );
                DebugMsg(("calculate(%s) error 1\n", AsmBuffer[oper]->string_ptr ));
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
         LOW                constant, label (OMF+BIN only)
         HIGH               constant, label (OMF+BIN only)
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

        temp = GetValueSp( AsmBuffer[oper]->value );

        sym = token_2->sym;
        if( token_2->mbr != NULL )
            sym = token_2->mbr;

        if ( token_2->instr != EMPTY )
            name = AsmBuffer[oper]->tokpos + strlen( AsmBuffer[oper]->string_ptr );
        else if ( sym )
            name = sym->name;
        else if ( token_2->base_reg != EMPTY && token_2->indirect == FALSE )
            name = AsmBuffer[token_2->base_reg]->string_ptr;
        else
            name = AsmBuffer[oper]->tokpos + strlen( AsmBuffer[oper]->string_ptr );

        switch ( token_2->kind ) {
        case EXPR_CONST:
            /* v2.05: conditions "struct-field" and "istype" exchanged */
            /* is item a struct field? */
            if ( token_2->mbr != NULL && token_2->mbr->state != SYM_TYPE ) {
                if ( token_2->mbr->mem_type == MT_BITS ) { /* bitfield? */
                    if ( ( temp & AT_BF ) == 0 ) {
                        invalid_operand( AsmBuffer[oper]->string_ptr, name );
                        return( ERROR );
                    }
                } else {
                    if ( ( temp & AT_FIELD ) == 0 ) {
                        invalid_operand( AsmBuffer[oper]->string_ptr, name );
                        return( ERROR );
                    }
                }
            } else if ( token_2->is_type ) { /* is item a type? */
                if ( ( temp & AT_TYPE ) == 0 ) {
                    invalid_operand( AsmBuffer[oper]->string_ptr, name );
                    return( ERROR );
                }
            } else { /*  or is it a number? */
                if ( ( temp & AT_NUM ) == 0 ) {
                    invalid_operand( AsmBuffer[oper]->string_ptr, name );
                    return( ERROR );
                }
            }
            break;
        case EXPR_ADDR:
            /* an indirect memory operand? (not an auto variable) */
            if ( token_2->indirect == TRUE && token_2->sym == NULL ) {
                if ( ( temp & AT_IND ) == 0) {
                    invalid_operand( AsmBuffer[oper]->string_ptr, name );
                    return( ERROR );
                }
            } else {
                if ( ( temp & AT_LABEL ) == 0) {
                    invalid_operand( AsmBuffer[oper]->string_ptr, name );
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
#if LOHI32
                case T_LOW32:
                case T_HIGH32:
#endif
                case T_TYPE:
                case T_OPATTR:
                case T_DOT_TYPE:
                    break;
                default:
                    CAsmError( LABEL_EXPECTED );
                    DebugMsg(("calculate %s error 2\n", AsmBuffer[oper]->string_ptr ));
                    return( ERROR );
                }
            }
            break;
        case EXPR_REG:
            if ( ( temp & AT_REG ) == 0 ) {
                invalid_operand( AsmBuffer[oper]->string_ptr, name );
                return( ERROR );
            }
            break;
        case EXPR_FLOAT: /* v2.05: added */
            if ( temp != AT_ALL ) {
                DebugMsg(("calculate %s 'float' error\n", AsmBuffer[oper]->string_ptr ));
                CAsmError( REAL_OR_BCD_NUMBER_NOT_ALLOWED );
                return( ERROR );
            }
            break;
        }
#ifdef DEBUG_OUT
        if ( unaryop[ GetSflagsSp( AsmBuffer[oper]->value ) ]( AsmBuffer[oper]->value, token_1, token_2, sym, name ) == ERROR )
            return( ERROR );
        break;
#else
        return( unaryop[ GetSflagsSp( AsmBuffer[oper]->value ) ]( AsmBuffer[oper]->value, token_1, token_2, sym, name ) );
#endif
    //case T_RES_ID:
    default: /* shouldn't happen */
        DebugMsg(("calculate(%s): unknown operator\n", AsmBuffer[oper]->string_ptr ));
        if ( error_msg )
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[oper]->string_ptr );
        return( ERROR );
    } /* end switch( AsmBuffer[oper]->token ) */
#ifdef DEBUG_OUT
    if ( token_1->hvalue ) {
        DebugMsg1(("calculate(%s) exit, ok, value=%I64d(0x%I64X) memtype=0x%X indirect=%u type=>%s<\n",
                   AsmBuffer[oper]->string_ptr,
                   token_1->llvalue, token_1->llvalue,
                   token_1->mem_type,
                   token_1->indirect, token_1->type ? token_1->type->name : "NULL" ));
    } else {
        DebugMsg1(("calculate(%s) exit, ok, value=%d(0x%X) memtype=0x%X ind=%u exp=%u type=%s mbr=%s\n",
                   AsmBuffer[oper]->string_ptr,
                   token_1->value, token_1->value,
                   token_1->mem_type,
                   token_1->indirect, token_1->explicit,
                   token_1->type ? token_1->type->name : "NULL",
                   token_1->mbr ? token_1->mbr->name : "NULL" ));
    }
#endif
    return( NOT_ERROR );
}

/* this code runs BEFORE the - right - operand of an operator is read */

static void PrepareOp( int oper, expr_list *new, const expr_list *old )
/*********************************************************************/
{
    switch ( AsmBuffer[oper]->token ) {
    case T_DOT:
        DebugMsg(("PrepareOp: DOT operator found, old.sym=%X, old.type=%s, expr=%s\n", old->sym, (old->type ? old->type->name : "NULL" ), AsmBuffer[oper]->tokpos + 1 ));
        if ( old->type ) {
            DebugMsg(("PrepareOp: implicit type: %s\n", old->type->name));
            new->type = old->type;
        } else if ( old->sym && old->sym->mem_type == MT_TYPE ) {
            DebugMsg(("PrepareOp: implicit type: %s\n", old->sym->type->name));
            for (new->type = old->sym->type; new->type->type; new->type = new->type->type);
        } else if ( !ModuleInfo.oldstructs ) {
            if ( !nullstruct ) {
                nullstruct = (dir_node *)CreateTypeSymbol( NULL, "", FALSE );
                nullstruct->e.structinfo->typekind = TYPE_STRUCT;
            }
            new->type = (asm_sym *)nullstruct;
            /* a - probably unnecessary - hack */
            //new->type->type = old->sym;
        }
        break;
    case T_UNARY_OPERATOR:
        switch ( AsmBuffer[oper]->value ) {
        case T_OPATTR:
        case T_DOT_TYPE:
            DebugMsg(("PrepareOp: OPATTR operator found, old.sym=%X, old.type=%s, expr=%s\n", old->sym, (old->type ? old->type->name : "NULL" ), AsmBuffer[oper]->tokpos + 1 ));
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

    DebugMsg1(("evaluate(i=%d, end=%d, flags=%X) enter [opndx: kind=%d type=%s]\n",
               *i, end, proc_flag, operand1->kind, operand1->type ? operand1->type->name : "NULL" ));

    token_needed = FALSE;

    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/
    /* Look at first token, which may be an unary operator or an operand */
    /*:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::*/

    if( operand1->kind == EXPR_EMPTY ) {

        if( IsCurrToken( T_OP_BRACKET ) ) {
            DebugMsg1(("evaluate: OP_BRACKET\n"));
            (*i)++;
            if( *i >= end ) {
                CAsmError( OPERAND_EXPECTED ); ERRLOC(*i);
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
            /* v1.96: clear explicit flag! This flag is treated somewhat
             * special in the parser, it "forces" a type (this is to be
             * changed), but if it occurs within (), this behavior must
             * be suppressed. example:
             * mov (<type> PTR ds:[0]).<struct_field>, ax
             */
            operand1->explicit = FALSE;
            (*i)++;
        } else if( IsCurrToken( T_OP_SQ_BRACKET ) ) {
            DebugMsg1(("evaluate: OP_SQ_BRACKET\n"));
            (*i)++;
            if( *i >= end ) {
                CAsmError( OPERAND_EXPECTED ); ERRLOC(*i);
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
#if 1  /* v2.03: allow [DWORD][4] */
            operand1->is_type = FALSE;
#endif
            op_sq_bracket_level--;
            (*i)++;
        } else if( is_unary( *i, TRUE ) ) {
            DebugMsg1(("evaluate: unary operand detected: %s, value=%u\n", AsmBuffer[*i]->string_ptr, AsmBuffer[*i]->value));
            token_needed = TRUE;
        } else if( get_operand( operand1, i, end ) == ERROR ) {
            DebugMsg(("evaluate exit 7, error (get_operand() failed), i=%d\n", *i ));
            return( ERROR );
        }
    }

    /* If an unary operator wasn't found, read the operator now */

    if( !token_needed ) {
        /* is the expression a single item? */
        if( *i >= end || IsCurrToken( T_CL_BRACKET ) || IsCurrToken( T_CL_SQ_BRACKET ) ) {
            DebugMsg1(("evaluate exit, ok, kind=%u value=%d string=%X memtype=%Xh indirect=%u ofssize=%d type=%X\n",
                      operand1->kind,      operand1->value,
                      operand1->string,    operand1->mem_type,
                      operand1->indirect,  operand1->Ofssize, operand1->type ));
            return( NOT_ERROR );
        }

        /* Read the operator. Must be binary or open bracket */

        if( !is_operator( *i ) ) {
            DebugMsg(("evaluate: 1. error 'operator expected', i=%d token=%X string=>%s<\n", *i, AsmBuffer[*i]->token, AsmBuffer[*i]->string_ptr ));
            CAsmError( OPERATOR_EXPECTED ); ERRLOC(*i);
            return( ERROR );
        }
    }

    do {
        curr_operator = *i;
        DebugMsg1(("evaluate loop enter, operator index=%u ('%s'), operand1->sym=%X, type=%s\n",
                  curr_operator, AsmBuffer[curr_operator]->string_ptr, operand1->sym, (operand1->type ? operand1->type->name : "NULL") ));
        (*i)++;

        init_expr( &operand2 );
        PrepareOp( curr_operator, &operand2, operand1 );

        /* read the (next) operand */

        if( *i >= end || IsCurrToken( T_CL_BRACKET )) {
            /* OPATTR needs no operand */
            if ( operand2.is_opattr == TRUE ) {
                DebugMsg(("evaluate: OPATTR, goto do_calc reached\n" ));
                next_operator = FALSE; /* v2.05 added */
                goto do_calc;
            }
            /* v2.0: also allow OPATTR(xxx()) */
            if ( operand1->is_opattr &&
                AsmBuffer[curr_operator]->token == T_OP_BRACKET &&
                IsCurrToken( T_CL_BRACKET ) ) {
                DebugMsg(("evaluate: OPATTR, xxx() found\n" ));
                (*i)++;
                break;
            }
            CAsmError( OPERAND_EXPECTED ); ERRLOC(*i);
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
            DebugMsg1(("evaluate: operator '%s', calling evaluate()\n", AsmBuffer[*i]->string_ptr ));
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
            CAsmError( OPERAND_EXPECTED ); ERRLOC(*i);
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
        while( *i < end && !IsCurrToken( T_CL_BRACKET) && !IsCurrToken( T_CL_SQ_BRACKET) ) {

            if( !is_operator( *i ) || is_unary( *i, FALSE ) ) {
                DebugMsg(("evaluate: 2. error 'operator expected', i=%d\n", *i ));
                CAsmError( OPERATOR_EXPECTED ); ERRLOC(*i);
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

#ifdef DEBUG_OUT
    if ( operand1->hvalue != -1 && operand1->hvalue != 0 ) {
        DebugMsg1(("evaluate exit, ok, value=%I64d(0x%I64X) kind=%u string=%s memtype=0x%X indirect=%u type=>%s<\n",
                  operand1->llvalue, operand1->llvalue,
                  operand1->kind,
                  operand1->string ? operand1->string : "NULL",
                  operand1->mem_type,
                  operand1->indirect, operand1->type ? operand1->type->name : "NULL" ));
    } else {
        DebugMsg1(("evaluate exit, ok, value=%ld(0x%" FX32 ") kind=%u string=%s memtype=0x%X ind=%u exp=%u ofssiz=%u type=>%s<\n",
                  operand1->value, operand1->value,
                  operand1->kind,
                  operand1->string ? operand1->string : "NULL",
                  operand1->mem_type,
                  operand1->indirect, operand1->explicit, operand1->Ofssize,
                  operand1->type ? operand1->type->name : "NULL" ));
    }
#endif
    return( NOT_ERROR );
}

static bool is_expr_item( int i )
/******************************************/
/* Check if a token is a valid part of an expression.
 * chars + - * / . : [] and () are operators.
 * also done here:
 * T_INSTRUCTION  SHL, SHR, AND, OR, XOR changed to T_BINARY_OPERATOR
 * T_INSTRUCTION  NOT                    changed to T_UNARY_OPERATOR
 * T_RES_ID       PTR                    changed to T_BINARY_OPERATOR
 * T_DIRECTIVE    PROC                   changed to T_STYPE
 * for the new operators the precedence is set
 */
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
        if ( AsmBuffer[i]->value == T_PTR ) {
            AsmBuffer[i]->token = T_BINARY_OPERATOR;
            AsmBuffer[i]->precedence = PTR_PRECEDENCE;
        } else if ( AsmBuffer[i]->value == T_DUP ) /* DUP must terminate the expression */
            return( FALSE );
#if 0 /* v2.05: no more special treatment of other reserved words */
        else if ( AsmBuffer[i]->value == T_FLAT ) {
            /* v2.0: this happens too early. If FLAT is used in an
             * EQU expression, it should NOT be defined! */
            //DefineFlatGroup();
        } else {
            /* other reserved words are changed to a T_ID */
            /* they have no meaning in an expression */
            /* ADDR, VARARG, BASIC, C, PASCAL, ... */
            AsmBuffer[i]->token = T_ID;
        }
#endif
        break;
#if 0 /* v2.05 */
    case '+':
    case '-':
        /* this can happen!: "db x dup(-1)" */
    case T_NEGATIVE:
    case T_POSITIVE:
        /* hack to stop expreval from hanging on floating point numbers */
        if( AsmBuffer[i+1]->token == T_FLOAT )
            return( FALSE );
        break;
#endif
    case T_DIRECTIVE:
        if ( AsmBuffer[i]->value == T_PROC ) { // PROC is converted to a type
            AsmBuffer[i]->token = T_STYPE;
            AsmBuffer[i]->value8 = ST_PROC;
            return( TRUE );
        }
        /* fall through. Other directives will end the expression */
    case T_COMMA:
    //case T_FLOAT: /* v2.05 */
    case T_QUESTION_MARK:
        return( FALSE );
    }
    return( TRUE );
}

/* evaluate an operand
 * start_tok: index of first token of expression
 * end_tok:   index of last  token of expression
 */
ret_code EvalOperand( int *start_tok, int end_tok, expr_list *result, uint_8 flags )
/**********************************************************************************/
{
    int         i;

    DebugMsg1(("EvalOperand(start=%u, end=%u, flags=%X) enter: >%s<\n", *start_tok, end_tok, flags, AsmBuffer[*start_tok]->tokpos ));

    init_expr( result );

    for( i = *start_tok; ( i < end_tok ) && is_expr_item( i ); i++ );
    if ( i == *start_tok )
        return( NOT_ERROR );

    op_sq_bracket_level = 0;
    error_msg = !(flags & EXPF_NOERRMSG);
    eflags = flags;
    return ( evaluate( result, start_tok, i, PROC_BRACKET ) );
}

/* global init (called once for each module) */

void ExprEvalInit()
/*****************/
{
    thissym = NULL;
    nullstruct = NULL;
    nullmbr = NULL;
}
