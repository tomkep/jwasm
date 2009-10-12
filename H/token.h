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
* Description:  token definitions
*
****************************************************************************/


#ifndef _TOKEN_H_
#define _TOKEN_H_

enum state {
        T_FINAL,
        T_INSTRUCTION,
        T_REG,
        T_DIRECTIVE,
        T_UNARY_OPERATOR,
        T_BINARY_OPERATOR,
        T_RES_ID,
        T_ID,
        T_STRING,
        T_NUM,
        T_FLOAT,
        T_POSITIVE,
        T_NEGATIVE,
//        T_ID_IN_BACKQUOTES,
        T_BAD_NUM,

        T_OP_BRACKET    = '(',
        T_OP_SQ_BRACKET = '[',
        T_CL_BRACKET    = ')',
        T_CL_SQ_BRACKET = ']',
        T_COMMA         = ',',
        T_COLON         = ':',
        T_PLUS          = '+',
        T_MINUS         = '-',
        T_DOT           = '.',
        T_QUESTION_MARK = '?',
        T_PERCENT       = '%'
};

struct asm_tok {
#ifdef __WATCOMC__
    enum state      token;
#else
    unsigned char   token;
#endif
    union {
        unsigned char  dirtype;    /* T_DIRECTIVE: type */
        unsigned char  opcode;     /* T_RES_ID */
        unsigned char  precedence; /* T_UNARY_OPERATOR/T_BINARY_OPERATOR */
        char  string_delim;        /* T_STRING: string delimiter  */
    };
    union {
        uint_16     flags;  /* for T_DIRECTIVE only */
        uint_8      rm_byte;/* for T_RES_ID only */
    };
    char            *string_ptr;
    union {
        struct {
            int_32  value;
            union {
                char   *pos;    /* points to item in CurrSource */
                int_32 hvalue; /* for T_NUM only */
            };
        };
        unsigned char bytes[16]; /* used by T_FLOAT + T_NUM */
        struct {
            uint_64 value64;
            uint_64 hivalue64;
        };
    };
};

#endif
