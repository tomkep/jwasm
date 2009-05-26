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
* Description:  Parser items
*
****************************************************************************/

#ifndef PARSER_H
#define PARSER_H

#include "operands.h"
#include "symbols.h"
#include "token.h"

enum prefix_reg {
    PREFIX_EMPTY = EMPTY,
    PREFIX_ES = 0x26,
    PREFIX_CS = 0x2E,
    PREFIX_SS = 0x36,
    PREFIX_DS = 0x3E,
    PREFIX_FS = 0x64,
    PREFIX_GS = 0x65
};

enum asm_stypes {
#undef pick
#define pick( name, memtype, size, type, ofssize ) ST_ ## name ,
#include "stypes.h"
};

// structure of items in the "reserved names" table AsmResWord
// (file reswords.h)

struct ReservedWord {
    struct ReservedWord *next;
    const char *name;        // reserved word (char[])
    unsigned short len;      // length of reserved word, e.g. "AX" = 2
    unsigned short position; // starting position in AsmOpTable
};

/*
 values for <byte1_info>
 000  : the first byte is opcode, follow by rm_byte
 F_16 : the first byte is OPSIZ prefix if in use32 segment
 F_32 : the first byte is OPSIZ prefix if in use16 segment
 F_0F : the first byte is 0x0F, follow by opcode and rm_byte
*/
enum BYTE1_INFO {
    F_16   = 0x1,   // 16bit variant, 66h switches
    F_32   = 0x2,   // 32bit variant, 66h switches
    F_0F   = 0x3,   // 0F prefix
    F_F3   = 0x4,   // F3 prefix (pause: F3 90)
    F_0F0F = 0x5,   // AMD 3DNow prefix
    F_660F = 0x6,   // SSEx prefix 1
    F_F20F = 0x7,   // SSEx prefix 2
    F_F30F = 0x8,   // SSEx prefix 3
    F_0FNO66 = 0x9, // 0F prefix, no size prefix
    F_16A = 0xA,    // 16bit variant, 67h switches
    F_32A = 0xB     // 32bit variant, 67h switches
};

/*
 values for <rm_info>
 000              -> has rm_byte with w-, d- and/or s-bit in opcode
 001( + no_RM   ) -> no rm_byte
 010( + no_WDS  ) -> has rm_byte, but w-bit, d-bit, s-bit of opcode are absent
 011( + R_in_OP ) -> no rm_byte, reg field is included in opcode
 100( + no_WDSx ) -> similar to no_WDS, + "instruction rm_byte" is additional opcode byte
 */
enum RM_INFO {
    no_RM   = 0x1,
    no_WDS  = 0x2,
    R_in_OP = 0x3,
    no_WDSx = 0x4
};

// values for <allowed_prefix>
enum ALLOWED_PREFIX {
 AP_NO_PREFIX= 0x00,
 AP_LOCK     = 0x01,
 APR_IREG    = 0x01,   /* for OP_REGISTER: INDEX register. it's a flag! */
 AP_REP      = 0x02,
 AP_REPxx    = 0x03,
 AP_FWAIT    = 0x04,
 AP_NO_FWAIT = 0x05
};

// asm_ins is the structure used to store instructions, directives and
// other reserved words in AsmOpTable (instruct.h).
// <token> is stored to detect group changes.

struct asm_ins {
    //enum asm_token      token;                  /* T_ADD, etc */
    unsigned            token           :16;    /* this format is shorter */
    unsigned            allowed_prefix  : 3;    /* allowed prefix */
    unsigned            disabled        : 1;    /* keyword is disabled */
    unsigned            byte1_info      : 4;    /* flags for 1st byte */
    unsigned            rm_info         : 3;    /* info on r/m byte */
    unsigned            opnd_type_3rd   : 4;    /* info on 3rd operand */
    unsigned            opnd_dir        : 1;    /* operand direction */
    OPNDTYPE            opnd_type[2];           /* asm_opnds */
    enum asm_cpu        cpu;                    /* CPU type */
    unsigned char       opcode;                 /* opcode byte */
    union {
        unsigned char   rm_byte;                /* mod_rm_byte */
        enum special_type specialtype;          /* for OP_SPECIAL */
    };
};

// asm_code is used by the code generator

struct asm_code {
    struct {
        struct asm_sym  *SegOverride;  // segment override if symbol
        signed short    ins;           // prefix before instruction, e.g. lock
        enum prefix_reg RegOverride;   // segment override if register
        unsigned        adrsiz:1;      // address size prefix
        unsigned        opsiz:1;       // operand size prefix
    } prefix;
    enum asm_token  token;
    memtype         mem_type;       // byte / word / etc. NOT near/far
    OPNDTYPE        opnd_type[3];
    long            data[3];
    struct asmfixup *InsFixup[3];
    unsigned char   opcode;
    unsigned char   rm_byte;
    //signed char     extended_ins;
    unsigned char   sib;
    unsigned        use32:1;
    //unsigned        indirect:1;     // CALL/JMP indirect jump
    unsigned        mem_type_fixed:1;
    unsigned        isfar:1;        // CALL/JMP far
};

// values for <opcode> flags (OP_DIRECTIVE)
enum directive_flags {
 DF_CEXPR    = 0x01, /* avoid '<' being used as string delimiter (.IF, ...) */
 DF_CONDDIR  = 0x02, /* conditional assembly directive (IF, ELSE, ...) */
 DF_ERRDIR   = 0x04, /* error directive (.ERR, .ERRNZ, ...) */
 DF_LOOPDIR  = 0x08, /* loop directive (FOR, REPEAT, WHILE, ...) */
 DF_STRPARM  = 0x10, /* directive expects string param(s) (IFB, IFDIF, ...) */
                     /* enclose strings in <> in macro expansion step */
 DF_NOEXPAND = 0x20, /* don't expand params for directive (PURGE, FOR, IFDEF, ...) */
 DF_DATADIR  = 0x40, /* data definition directive */
 DF_LABEL    = 0x80, /* directive requires a label */
 DF_PREPROC  = DF_ERRDIR /* special preprocessor directive */
};

/* NOTE: The order of table is IMPORTANT !! */
/* OP_A should put before OP_R16 & OP_R
   OP_R16   "     "    "    OP_R
   OP_I8    "     "    "    OP_I
   OP_M ( without extension ) should follow OP_M_x
   OP_R ( without extension ) should follow OP_Rx
   OP_I ( without extension ) should follow OP_Ix  */

extern struct asm_ins      AsmOpTable[];
extern struct ReservedWord AsmResWord[];
extern bool   line_listed;

extern int      OperandSize( OPNDTYPE opnd );
extern int      InRange( long val, unsigned bytes );
extern void     find_frame( struct asm_sym *sym );
extern ret_code ParseItems( void );
extern ret_code ParseInit( void );

#endif
