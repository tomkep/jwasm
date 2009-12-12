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
* Description:  interface to expression evaluator
*
****************************************************************************/


#ifndef EXPREVAL_H
#define EXPREVAL_H

#define FLAG_LABELDIFF 1 /* flag if 2 label were subtracted in expression */

enum exprtype {
    EXPR_EMPTY = EMPTY,
    EXPR_UNDEF = 0,     // undefined type when error occures or result is undefined
    EXPR_ADDR,          // e.g. "foo", "seg foo" and "offset foo"
    EXPR_CONST,         // a constant; note that "label1 - label2" -> constant
    EXPR_REG            // a register
};

// argument types accepted by unary operators

enum oparg_types {
    AT_TYPE  = 0x01, /* type */
    AT_LABEL = 0x02, /* label (direct memory) */
    AT_IND   = 0x04, /* indirect memory */
    AT_REG   = 0x08, /* register */
    AT_FIELD = 0x10, /* struct field */
    AT_NUM   = 0x20, /* number */
    AT_BF    = 0x40, /* bitfield and record types */
    AT_UNDEF = 0x80, /* undefined label */
    AT_CONST = AT_TYPE | AT_NUM,
    AT_TL    = AT_TYPE | AT_LABEL,
    AT_TLN   = AT_TYPE | AT_LABEL | AT_NUM,
    AT_TLF   = AT_TYPE | AT_LABEL | AT_FIELD,
    AT_TLFN  = AT_TYPE | AT_LABEL | AT_FIELD | AT_NUM,
    AT_TBF   = AT_TYPE | AT_BF,
    AT_LF    = AT_LABEL| AT_FIELD,
    AT_LIF   = AT_LABEL| AT_IND | AT_FIELD,
    AT_LFN   = AT_LABEL| AT_FIELD | AT_NUM,
    AT_TLR   = AT_TYPE | AT_LABEL | AT_REG,
    AT_ALL   = AT_TYPE | AT_LABEL | AT_IND | AT_REG | AT_FIELD | AT_NUM | AT_UNDEF | AT_BF
};

typedef struct expr_list {
    union {
        struct {
            union {
                int_32      value;  // For constant, may also be a label offset
                uint_32     uvalue; // For constant, may also be a label offset
                float       fvalue; // For constant
            };
            int_32          hvalue; // high 32bit of 64bit number
            union {
                int_32      value6495; // bits 64-95 of 128bit number
                int_16      value6479; // bits 64-79 of 80bit number
            };
            int_32          value96127; // bits 96-127 of 128bit number
        };
        struct {
            union {
            uint_64         llvalue;
            int_64          value64;
            };
            uint_64         hlvalue;
        };
    };
    char            *string;        // for strings only -- NULL otherwise
    int             base_reg;       // position of token for base register
                                    // if type is EXPR_REG, it holds register
    int             idx_reg;        // position of token for index register
    int             label;          // position of token holding the label
    int             override;       // position of token holding the override label
                                    //   or register
    enum asm_token  instr;          // instruction token for operator

    enum exprtype   kind;           // Type of expression
    memtype         mem_type;       // Whether expr is BYTE, WORD, DWORD, etc.
    uint_8          scale;          // scaling factor 1, 2, 4, or 8 - 386 code only
    uint_8          Ofssize;        // for MT_NEAR | MT_FAR
    union {
        uint_8      flags;
        struct {
            unsigned        indirect : 1;   // Whether inside [] or not
            unsigned        explicit : 1;   // Whether expression type explicitly given
            unsigned        abs      : 1;   // external ABS
#if FLAG_LABELDIFF
            unsigned        labeldiff : 1;
#endif
            unsigned        is_type : 1;     // constant is a type
            unsigned        is_opattr : 1;   // current operator is OPATTR
        };
    };
    struct asm_sym  *sym;   // label used
    struct asm_sym  *mbr;   // struct member
    struct asm_sym  *type;  // for DOT operator. Must be last (see TokenAssign)!
} expr_list;

extern ret_code     EvalOperand( int *, int, expr_list *, bool );
extern void         ExprEvalInit( void );

#endif
