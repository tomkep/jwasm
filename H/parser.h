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

enum asm_stypes {
#undef pick
#define pick( name, memtype, ofssize ) ST_ ## name ,
#include "stypes.h"
};

// structure of items in the "reserved names" table AsmResWord

struct ReservedWord {
    short next;              /* index next entry (used for hash table) */
    unsigned char len;       /* length of reserved word, i.e. 'AX' = 2 */
    unsigned char flags;
    const char *name;        /* reserved word (char[]) */
};


enum reservedword_flags {
 RWF_SPECIAL  = 1, /* keyword is NO instruction */
 RWF_DISABLED = 2, /* keyword disabled */
 RWF_IA32     = 4, /* keyword specific to IA32 mode */
 RWF_X64      = 8  /* keyword specific to IA32+ mode */
};

/*
 * values for <byte1_info>
 * 000  : the first byte is opcode, follow by rm_byte
 * F_16 : the first byte is OPSIZ prefix if in use32 segment
 * F_32 : the first byte is OPSIZ prefix if in use16 segment
 * F_0F : the first byte is 0x0F, follow by opcode and rm_byte
 * the entries must be sorted related to F_0F prefix:
 * entries < F_0F emit NO 0F prefix, entries >= F_0F emit one.
 */
enum BYTE1_INFO {
    F_16   = 1,    // 16bit variant, 66h switches
    F_32   = 2,    // 32bit variant, 66h switches
    F_16A  = 3,    // 16bit variant, 67h switches
    F_32A  = 4,    // 32bit variant, 67h switches
    F_F3   = 5,    // F3 prefix (pause: F3 90)
#if AMD64_SUPPORT
    F_48  =  6,    // REX.W prefix
#endif
    F_0F   = 7,    // 0F prefix
    F_0F0F = 8,    // AMD 3DNow prefix
    F_660F = 9,    // SSEx prefix 1
    F_F20F = 10,   // SSEx prefix 2
    F_F30F = 11,   // SSEx prefix 3
    F_0FNO66 = 12, // 0F prefix, no size prefix
#if AMD64_SUPPORT
    F_480F =  13,  // REX.W + 0F prefix ( cmpxchg16b )
#endif
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
 AP_REP      = 0x02,
 AP_REPxx    = 0x03,
 AP_FWAIT    = 0x04,
 AP_NO_FWAIT = 0x05
};

// values for field specialtype:

enum special_type {
 RWT_REGISTER,
 RWT_RES_ID,
 RWT_DIRECTIVE,
 RWT_TYPE,
 RWT_BINARY_OP,
 RWT_UNARY_OP
};

// values for operand1 if register
enum op1_flags {
 SFR_IREG = 1,
 SFR_SIZ2 = 2,
 SFR_SIZ4 = 4,
 SFR_SIZ8 = 8,
 SFR_SIZMSK = 0xE
};

#if AMD64_SUPPORT
enum rex_bits {
 REX_B = 1,
 REX_X = 2,
 REX_R = 4,
 REX_W = 8
};
#endif

/* asm_ins is the structure used to store instructions, directives and
 * other reserved words in AsmOpTable (instruct.h & special.h).
 * Most compilers will use unsigned type for enums, just OW
 * allows to use the smallest size possible.
 */

struct asm_ins {
    OPNDTYPE        opnd_type[2];           /* operands 1 + 2 */
    unsigned short /* the bitfields aren't used for non-instruction entries */
        allowed_prefix  : 3,    /* allowed prefix */
        first           : 1,    /* first entry for opcode */
        byte1_info      : 4,    /* flags for 1st byte */
        rm_info         : 3,    /* info on r/m byte */
        opnd_type_3rd   : 4,    /* info on 3rd operand */
        opnd_dir        : 1;    /* operand direction */
#ifdef __WATCOMC__
    enum asm_cpu        cpu;                /* CPU type */
#else
    unsigned short      cpu;                /* CPU type */
#endif
    unsigned char   opcode;                 /* opcode byte */
    union {
        unsigned char   rm_byte;            /* mod_rm_byte */
#ifdef __WATCOMC__
        enum special_type specialtype;      /* for OP_SPECIAL */
#else
        unsigned char   specialtype;
#endif
    };
};

// code_info describes the current instruction. It's the communication
// structure between parser and code generator.

struct code_info {
    struct {
        enum asm_token  ins;           // prefix before instruction, e.g. lock, rep, repnz
        enum assume_segreg RegOverride;// segment override (0=ES,1=CS,2=SS,3=DS,...)
#if AMD64_SUPPORT
        unsigned char   rex;
#endif
        unsigned char   adrsiz:1;      // address size prefix 0x67 is to be emitted
        unsigned char   opsiz:1;       // operand size prefix 0x66 is to be emitted
    } prefix;
    const struct asm_ins  *pcurr;      /* current pointer into AsmOpTable */
    enum asm_token  token;
    memtype         mem_type;          // byte / word / etc. NOT near/far
    OPNDTYPE        opnd_type[3];
    long            data[3];
    struct asmfixup *InsFixup[3];
    unsigned char   opcode;
    unsigned char   rm_byte;
    unsigned char   sib;
    unsigned char   Ofssize;
    union {
        unsigned char       flags;
        struct {
            unsigned char   isdirect:1;     /* 1=direct addressing mode */
            unsigned char   isfar:1;        /* CALL/JMP far */
            unsigned char   const_size_fixed:1; /* v2.01 */
#if AMD64_SUPPORT
            unsigned char   x86hi_used:1;   /* AH,BH,CH,DH used */
            unsigned char   x64lo_used:1;   /* SPL,BPL,SIL,DIL used */
#endif
        };
    };
};

#define OPND1 0
#define OPND2 1
#define OPND3 2

/* branch instructions are still sorted:
 * CALL, JMP, Jcc, J[e|r]CXZ, LOOP, LOOPcc
 */

#define IS_CALL( inst )       ( inst == T_CALL )
#define IS_JMPCALL( inst )    ( inst == T_CALL || inst == T_JMP    )
#define IS_JMP( inst )        ( inst >= T_JMP  && inst < T_LOOP  )
#define IS_JCC( inst )        ( inst >  T_JMP  && inst < T_JCXZ  )
#define IS_BRANCH( inst )     ( inst >= T_CALL && inst < T_LOOP  )
#define IS_ANY_BRANCH( inst ) ( inst >= T_CALL && inst <= T_LOOPNZW )
#define IS_XCX_BRANCH( inst ) ( inst >= T_JCXZ && inst <= T_LOOPNZW )

#define SET_OPSIZ( s, x ) ( s->prefix.opsiz = (( x ) ^ ( s->Ofssize )) ? TRUE : FALSE )
#define SET_OPSIZ_32( s ) ( s->prefix.opsiz = ( s->Ofssize ) ? FALSE : TRUE )
#define SET_OPSIZ_16( s ) ( s->prefix.opsiz = ( s->Ofssize ) ? TRUE : FALSE )
#define SET_OPSIZ_NO( s ) ( s->prefix.opsiz = FALSE )

#define IS_OPER_32( s )   ( s->Ofssize ? ( s->prefix.opsiz == FALSE ) : ( s->prefix.opsiz == TRUE ))

/* for special names, the optable_idx helper table isn't needed */
//#define GetRegNo( x ) AsmOpTable[optable_idx[x]].opcode
//#define GetOpndType( x, i ) AsmOpTable[optable_idx[x]].opnd_type[i]
//#define GetRegCpu( x ) AsmOpTable[optable_idx[x]].cpu
#define GetRegNo( x ) AsmOpTable[x].opcode
#define GetOpndType( x, i ) AsmOpTable[x].opnd_type[i]
#define GetRegCpu( x ) AsmOpTable[x].cpu

// values for <op2> flags (RWT_DIRECTIVE entries)
enum directive_flags {
 DF_CEXPR    = 0x01, /* avoid '<' being used as string delimiter (.IF, ...) */
 DF_STRPARM  = 0x02, /* directive expects string param(s) (IFB, IFDIF, ...) */
                     /* enclose strings in <> in macro expansion step */
 DF_NOEXPAND = 0x04, /* don't expand params for directive (PURGE, FOR, IFDEF, ...) */
 DF_LABEL    = 0x08  /* directive requires a label */
};

/* values for <dir_type> (RWT_DIRECTIVE entries)
 * CONDDIR, ERRDIR, LOOPDIR and INCLUDE must be consecutive
 */
enum directive_type {
 DRT_CONDDIR  = 0x01, /* preprocessor conditional assembly directive (IF, ELSE, ...) */
 DRT_ERRDIR   = 0x02, /* preprocessor error directive (.ERR, .ERRNZ, ...) */
 DRT_LOOPDIR  = 0x03, /* preprocessor loop directive (FOR, REPEAT, WHILE, ...) */
 DRT_INCLUDE  = 0x04, /* preprocessor include directive */
 DRT_DATADIR  = 0x05, /* data definition directive */
 DRT_EQUALSGN = 0x06, /* '=' directive */
};

extern const struct asm_ins AsmOpTable[];
extern struct ReservedWord  AsmResWord[];
extern short  optable_idx[];
extern bool   line_listed;

extern int      OperandSize( OPNDTYPE opnd, const struct code_info * );
extern int      InRange( long val, unsigned bytes );
extern void     find_frame( struct asm_sym *sym );
extern void     find_frame2( struct asm_sym *sym );
extern uint     IsKeywordDisabled( const char *, int );
extern void     DisableKeyword( uint token );
#if RENAMEKEY
extern void     RenameKeyword( uint token, const char *name, uint_8 len );
#endif
extern ret_code ParseItems( void );
extern ret_code ParseInit( void );
#if AMD64_SUPPORT
extern void     Set64Bit( bool );
#endif

#endif
