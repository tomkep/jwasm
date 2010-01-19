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
* Description:  Parser
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "insthash.h"
#include "codegen.h"
#include "equate.h"
#include "fixup.h"
#include "expreval.h"
#include "labels.h"
#include "directiv.h"
#include "segment.h"
#include "assume.h"
#include "proc.h"
#include "myassert.h"
#include "input.h"
#include "tokenize.h"
#include "types.h"
#include "listing.h"
#include "data.h"
#include "fastpass.h"
#include "queue.h"

#define ONEXMM 1 /* 1=use ONE .xmm directive (Masm compatible) */

#define SET_ADRSIZ( s, x ) ( s->prefix.adrsiz = (( x ) ^ ( s->Ofssize )) ? TRUE : FALSE )
#define IS_ADDR32( s )  ( s->Ofssize ? ( s->prefix.adrsiz == FALSE ) : ( s->prefix.adrsiz == TRUE ))

/* the parser tables are now generated:
 * 1. AsmOpTable: contains info for instructions and other reserved words.
 *    instructions may have multiple entries!
 * 2. optable_idx: array of indices for AsmOpTable.
 * 3. AsmResWord: array of reserved words (name, name length, flags).
 *
 * Each reserved word has a "token" value assigned, which is a short integer.
 * This integer can be used as index for AsmResWord and for optable_idx.
 */

// create AsmOpTable table.

const struct asm_ins AsmOpTable[] = {

#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
    { {op1,op2}, 0,      1 ,0,          0,       0,   0,      cpu, opcode, rm_byte },
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 0, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#include "special.h"
#include "instruct.h"
#include "instr64.h"
ins (NULL,0,0,0,0,0,0,0,0,0,0,0,0)
#undef insx
#undef insn
#undef insm
#undef ins
#undef res
};

// define indices for AsmOpTable

enum res_idx {
#define  res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) T_ ## tok ## _I,
#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I,
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) T_ ## tok ## _I,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix,
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix,
#include "special.h"
#include "instruct.h"
#undef ins
#undef insm
#undef insn
#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I64,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#include "instr64.h"
#undef insx
#undef insm
#undef insn
#undef ins
#undef res
T_NULL_I
};

// create optable_idx, the index array for AsmOpTable

short optable_idx[] = {
#define  res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) T_ ## tok ## _I,
#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I,
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) T_ ## tok ## _I,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#include "special.h"
#include "instruct.h"
#undef ins
#undef insm
#undef insn
#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I64,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#include "instr64.h"
#undef insx
#undef insn
#undef insm
#undef ins
#undef res
T_NULL_I
};

// create the strings for all reserved words

static const char resw_strings[] = {
#if AMD64_SUPPORT
#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
 # string
#else
#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
 # string
#endif
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
 # string
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
 # string
#include "special.h"
#include "instruct.h"
};
#undef insx
#undef insn
#undef insm
#undef ins
#undef res

// create the 'reserved words' table (AsmResWord).
// this table's entries will be used to create the instruction hash table.

struct ReservedWord AsmResWord[] = {
#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
    { 0, len, RWF_SPECIAL | flags, NULL },
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { 0, len, 0, NULL },
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flags) \
    { 0, len, flags, NULL },
#include "special.h"
#include "instruct.h"
#undef insx
#undef insn
#undef insm
#undef ins
#undef res
    { 0, 0, 0, NULL }
};

/* parsing of branch instructions with imm operand is found in branch.c */
extern ret_code         process_branch( struct code_info *, const expr_list * );

extern bool             DefineProc;
extern struct asm_sym   symPC; /* '$' symbol */
extern bool             in_epilogue;

// v1.96: CodeInfo has become a stack variable.
// it's more or less the central object for the parser and code generator.
//static struct code_info Code_Info;
//static struct code_info *CodeInfo = &Code_Info;

unsigned                Opnd_Count;
bool                    line_listed;
#if AMD64_SUPPORT
static bool             b64bit = FALSE;
static const char       *syscallname;   /* "true" syscall name stored here */
#endif
int_8                   Frame;          // Frame of current fixup
uint_16                 Frame_Datum;    // Frame datum of current fixup
struct asm_sym          *SegOverride;
static enum assume_segreg  LastRegOverride;// needed for CMPS
static bool             fInit = FALSE;

/* global queue of "disabled" reserved words.
 * just indices of AsmResWord[] are used.
 */
static short RemovedFirst = EMPTY;
static short RemovedTail  = EMPTY;
#if RENAMEKEY
static qdesc renamed_keys = { NULL, NULL };
#endif

static int comp_mem16( int reg1, int reg2 )
/*****************************************/
/*
- compare and return the r/m field encoding of 16-bit address mode;
- call by mem2code() only;
*/
{
    switch( reg1 ) {
    case T_BX:
        switch( reg2 ) {
        case T_SI: return( MEM_BX_SI );
        case T_DI: return( MEM_BX_DI );
        }
        break;
    case T_BP:
        switch( reg2 ) {
        case T_SI: return( MEM_BP_SI );
        case T_DI: return( MEM_BP_DI );
        }
        break;
    }
    AsmError( INVALID_MEMORY_POINTER );
    return( ERROR );
}

/*
 * segment override with a symbol (i.e. DGROUP )
 * it has been checked in the expression evaluator that the
 * symbol has type SYM_SEG/SYM_GRP.
 */

static void SetFixupFrame( asm_sym *sym )
/***************************************/
{
    asm_sym *grp;

    if( sym ) {
        switch ( sym->state ) {
        case SYM_INTERNAL:
        case SYM_EXTERNAL:
            if( sym->segment != NULL ) {
                if( grp = GetGrp( sym ) ) {
                    Frame = FRAME_GRP;
                    Frame_Datum = GetGrpIdx( grp );
                } else {
                    Frame = FRAME_SEG;
                    Frame_Datum = GetSegIdx( sym->segment );
                }
            }
            break;
        case SYM_SEG:
            Frame = FRAME_SEG;
            Frame_Datum = GetSegIdx( sym->segment );
            break;
        case SYM_GRP:
            Frame = FRAME_GRP;
            Frame_Datum = GetGrpIdx( sym );
            break;
        }
    }
}

/* set global vars Frame and Frame_Datum.
 * called by idata_fixup()
 */

void find_frame( struct asm_sym *sym )
/************************************/
{
    if( SegOverride != NULL )
        SetFixupFrame( SegOverride );
    else
        SetFixupFrame( sym );
}

/* set frame if OPTION OFFSET:SEGMENT is on
 */
void find_frame2( struct asm_sym *sym )
/*************************************/
{
    if( SegOverride == NULL &&
       sym->segment != NULL &&
       (sym->state == SYM_INTERNAL || sym->state == SYM_EXTERNAL ) ) {
        Frame = FRAME_SEG;
        Frame_Datum = GetSegIdx( sym->segment );
    } else {
        find_frame( sym );
    }
}

static void check_assume( struct code_info *CodeInfo, struct asm_sym *sym, enum assume_segreg default_reg )
/*********************************************************************************************************/
/* Check if an assumed segment register is found, and
 * set CodeInfo->RegOverride if necessary.
 * called by seg_override().
 * at least either sym or SegOverride is != NULL.
 */
{
    enum assume_segreg     reg;
    asm_sym                *assume;

    if( sym && sym->state == SYM_UNDEFINED )
        return;

    reg = GetAssume( SegOverride, sym, default_reg, &assume );
    /* set global vars Frame and Frame_Datum */
    SetFixupFrame( assume );

    if( reg == ASSUME_NOTHING ) {
        if ( sym ) {
            //if( sym->state != SYM_EXTERNAL && sym->state != SYM_STACK ) {
            /* v1.95: condition changed. Now there's an error msg only if
             * the symbol has an explicite segment.
             */
            if( sym->segment != NULL ) {
                DebugMsg(("no segment register available to access label %s\n", sym->name ));
                AsmErr( CANNOT_ACCESS_LABEL_THROUGH_SEGMENT_REGISTERS, sym->name );
            } else
                CodeInfo->prefix.RegOverride = default_reg;
        } else {
            DebugMsg(("no segment register available to access seg-label %s\n", SegOverride->name ));
            AsmErr( CANNOT_ACCESS_LABEL_THROUGH_SEGMENT_REGISTERS, SegOverride->name );
        }
    } else if( default_reg != EMPTY ) {
        CodeInfo->prefix.RegOverride = reg;
    }
}

static void seg_override( struct code_info *CodeInfo, int seg_reg, asm_sym *sym, bool direct )
/********************************************************************************************/
/*
 * called by mem2code(). determine if segment override is necessary
 * with the current address mode;
 * - seg_reg: address register
 */
{
    enum assume_segreg  default_seg;
    asm_sym             *assume;

    /* don't touch segment overrides for string instructions */
    /* fixme: what about SSE2 CMPSD/MOVSD? */
    if ( AsmOpTable[optable_idx[CodeInfo->token]].allowed_prefix == AP_REP ||
         AsmOpTable[optable_idx[CodeInfo->token]].allowed_prefix == AP_REPxx )
        return;

    if( CodeInfo->token == T_LEA ) {
        CodeInfo->prefix.RegOverride = EMPTY; /* skip segment override */
        SetFixupFrame( sym );
        return;
    }

    switch( seg_reg ) {
    case T_SS:
    case T_BP:
    case T_EBP:
    case T_ESP:
        default_seg = ASSUME_SS;
        break;
    default:
        default_seg = ASSUME_DS;
    }

    if( CodeInfo->prefix.RegOverride != EMPTY ) {
        assume = GetOverrideAssume( CodeInfo->prefix.RegOverride );
        /* assume now holds assumed SEG/GRP symbol */
        if ( sym ) {
            if ( assume )
                SetFixupFrame( assume );
            else {
                SetFixupFrame( sym );
            }
        } else if ( direct ) {
            /* no label attached (DS:[0]). No fixup is to be created! */
            if ( assume )
                SET_ADRSIZ( CodeInfo, GetSymOfssize( assume ) );
            else {
                /* v2.01: if -Zm, then use current CS offset size.
                 * This isn't how Masm v6 does it, but it matches Masm v5.
                 */
                if ( ModuleInfo.m510 )
                    SET_ADRSIZ( CodeInfo, ModuleInfo.Ofssize );
                else
                    SET_ADRSIZ( CodeInfo, ModuleInfo.defOfssize );
            }
        }
    } else {
        if ( sym || SegOverride )
            check_assume( CodeInfo, sym, default_seg );
        if ( sym == NULL && SegOverride ) {
            SET_ADRSIZ( CodeInfo, GetSymOfssize( SegOverride ) );
        }
    }

    if( CodeInfo->prefix.RegOverride == default_seg ) {
        CodeInfo->prefix.RegOverride = EMPTY;
    }
}

int OperandSize( OPNDTYPE opnd, const struct code_info *CodeInfo )
/****************************************************************/
{
    /* v2.0: OP_M8_R8 and OP_M16_R16 have the DFT bit set! */
    if( opnd == OP_NONE ) {
        return( 0 );
    } else if( opnd == OP_M ) {
        return( SizeFromMemtype( CodeInfo->mem_type, CodeInfo->Ofssize ) );
    } else if( opnd & ( OP_R8 | OP_M8 | OP_I8 | OP_I_1 | OP_I_3 | OP_I8_U ) ) {
        return( 1 );
    } else if( opnd & ( OP_R16 | OP_M16 | OP_I16 | OP_SR ) ) {
        return( 2 );
    } else if( opnd & ( OP_R32 | OP_M32 | OP_I32 ) ) {
        return( 4 );
#if AMD64_SUPPORT
    } else if( opnd & ( OP_R64 | OP_M64 | OP_MMX | OP_I64 ) ) {
#else
    } else if( opnd & ( OP_M64 | OP_MMX ) ) {
#endif
        return( 8 );
//    } else if( opnd & ( OP_I | OP_J48 ) ) {
    } else if( opnd & ( OP_J48 | OP_M48 ) ) {
        return( 6 );
    } else if( opnd & ( OP_STI | OP_M80 ) ) {
        return( 10 );
    } else if( opnd & ( OP_XMM | OP_M128 ) ) {
        return( 16 );
    } else if( opnd & OP_SPEC_REG ) {
#if AMD64_SUPPORT
        return( ( CodeInfo->Ofssize == USE64 ) ? 8 : 4 );
#else
        return( 4 );
#endif
    }
    return( 0 );
}

/*
 * Can <val> be represented in <size> bytes?
 * currently called with size=1|2 only.
 */
int InRange( long val, unsigned size )
/************************************/
{
#if 0
    unsigned long max;
    unsigned long mask;
    /* get max: FF for 1, FFFF for 2 */
    max = ( 1UL << ( bytes * 8 ) ) - 1;
    if( val <= max ) /* absolute value fits */
        return( 1 );
    /* FF -> FFFFFF80, FFFF -> FFFF8000 */
    mask = ~(max >> 1);
    if( ( val & mask ) == mask ) /* just a sign extension */
        return( 1 );
    return( 0 );
#endif
#if 1
    if ( size == 1 )
        return( (val > 255 || val < -255) ? 0 : 1);
    else
        return( (val > 65535 || val < -65535) ? 0 : 1);
#endif
}

static ret_code mem2code( struct code_info *CodeInfo, char ss, int index, int base, asm_sym *sym )
/************************************************************************************************/
/*
 * encode the memory operand to machine code.
 * called by memory_operand().
 * in:  ss = scale factor (00=1,40=2,80=4,C0=8)
 *   index = index register (T_DI, T_ESI, ...)
 *    base = base register (T_EBP, ... )
 *     sym = symbol (direct addressing, displacement)
 * out: CodeInfo->sib, CodeInfo->rm_byte
*/
{
    int                 temp;
    unsigned char       mod_field;
    unsigned char       rm_field;
    unsigned char       base_reg;
    unsigned char       idx_reg;
#if AMD64_SUPPORT
    unsigned char       bit3_base;
    unsigned char       bit3_idx;
    unsigned char       rex;
#endif

    DebugMsg(("mem2code(scale=%u, index=%d, base=%d, sym=%X) enter\n", 1 << (ss >> 6), index, base, sym));

    // clear mod
    rm_field = 0;
#if AMD64_SUPPORT
    bit3_base = 0;
    bit3_idx = 0;
    rex = 0;
#endif
    if( CodeInfo->InsFixup[Opnd_Count] != NULL ) { /* symbolic displacement given? */
        mod_field = MOD_10;
    } else if( CodeInfo->data[Opnd_Count] == 0 ) { /* no displacement (or 0) */
        mod_field = MOD_00;
    } else if( ( CodeInfo->data[Opnd_Count] > SCHAR_MAX )
       || ( CodeInfo->data[Opnd_Count] < SCHAR_MIN ) ) {
        mod_field = MOD_10; /* full size displacement */
    } else {
        mod_field = MOD_01; /* byte size displacement */
    }

    if( ( index == EMPTY ) && ( base == EMPTY ) ) {
        // direct memory
        // clear the rightmost 3 bits
        CodeInfo->isdirect = TRUE;
        mod_field = MOD_00;

        // default is DS:[], DS: segment override is not needed
        seg_override( CodeInfo, T_DS, sym, TRUE );

        //if( !IS_ADDR32( CodeInfo ) ) {
        if( (CodeInfo->Ofssize == USE16 && CodeInfo->prefix.adrsiz == 0 ) ||
            (CodeInfo->Ofssize == USE32 && CodeInfo->prefix.adrsiz == 1 )) {
            if( !InRange( CodeInfo->data[Opnd_Count], 2 ) ) {
                // expect 16-bit but got 32-bit address
                AsmError( MAGNITUDE_OF_OFFSET_EXCEEDS_16BIT );
                return( ERROR );
            }
            rm_field = D16; /* D16=6 */
        } else {
            rm_field = D32; /* D32=5 */
        }
        DebugMsg(("mem2code, direct, CodeInfo->prefix.adrsiz=%u\n", CodeInfo->prefix.adrsiz ));
    } else if( ( index == EMPTY ) && ( base != EMPTY ) ) {
        switch( base ) {
        case T_SI:
            rm_field = 0x04; // default is DS:[], DS: segment override is not needed
            break;
        case T_DI:
            rm_field = 0x05; // default is DS:[], DS: segment override is not needed
            break;
        case T_BP:
            rm_field = 0x06; // default is SS:[], SS: segment override is not needed
            if( mod_field == MOD_00 ) {
                mod_field = MOD_01;
            }
            break;
        case T_BX:
            rm_field = 0x07; // default is DS:[], DS: segment override is not needed
            break;
#if AMD64_SUPPORT
        case T_RBP:
#endif
        case T_EBP:
            DebugMsg(("mem2code: base is EBP\n"));
            rm_field = 0x05; // (is EBP register #)
            if( mod_field == MOD_00 ) {
                mod_field = MOD_01;
            }
            // default is SS:[], SS: segment override is not needed
            break;
#if AMD64_SUPPORT
        case T_RSP:
#endif
        case T_ESP:
            rm_field = 0x04; // ( is ESP register #)
            // ss = 00, index = 100 ( no index ), base = 100 ( ESP )
            CodeInfo->sib = 0x24;
            // default is SS:[], SS: segment override is not needed
            break;
        default: // for 386 and up
            base_reg = GetRegNo( base );
#if AMD64_SUPPORT
            bit3_base = base_reg >> 3;
            base_reg &= BIT_012;
#endif
            rm_field = base_reg;
#if AMD64_SUPPORT
            /* v2.02 */
            //rex = ( bit3_base << 2 ); /* set REX_R */
            rex = bit3_base; /* set REX_R */
#endif
            // default is DS:[], DS: segment override is not needed
        }
#if AMD64_SUPPORT
        DebugMsg(("mem2code, indirect with base, mod_field=%X, rm_field=%X, rex=%X\n", mod_field, rm_field, rex ));
#else
        DebugMsg(("mem2code, indirect with base, rm_field=%X\n", rm_field ));
#endif
        seg_override( CodeInfo, base, sym, FALSE );
    } else if( ( index != EMPTY ) && ( base == EMPTY ) ) {
        idx_reg = GetRegNo( index );
#if AMD64_SUPPORT
        bit3_idx = idx_reg >> 3;
        idx_reg &= BIT_012;
#endif
        // mod field is 00
        mod_field = MOD_00;
        // s-i-b is present ( r/m = 100 )
        rm_field = S_I_B;
        // scale factor, index, base ( 0x05 => no base reg )
        CodeInfo->sib = ( ss | ( idx_reg << 3 ) | 0x05 );
#if AMD64_SUPPORT
        rex = (bit3_idx << 1); /* set REX_X */
#endif
        // default is DS:[], DS: segment override is not needed
        seg_override( CodeInfo, T_DS, sym, FALSE );
    } else {
        // base != EMPTY && index != EMPTY
        base_reg = GetRegNo( base );
        idx_reg  = GetRegNo( index );
#if AMD64_SUPPORT
        bit3_base = base_reg >> 3;
        bit3_idx  = idx_reg  >> 3;
        base_reg &= BIT_012;
        idx_reg  &= BIT_012;
#endif
        switch( index ) {
        case T_BX:
        case T_BP:
            if( ( temp = comp_mem16( index, base ) ) == ERROR )
                return( ERROR );
            rm_field = temp;
            seg_override( CodeInfo, index, sym, FALSE );
            break;
        case T_SI:
        case T_DI:
            if( ( temp = comp_mem16( base, index ) ) == ERROR )
                return( ERROR );
            rm_field = temp;
            seg_override( CodeInfo, base, sym, FALSE );
            break;
#if AMD64_SUPPORT
        case T_RSP:
#endif
        case T_ESP:
            //AsmErr( CANNOT_BE_USED_AS_INDEX_REGISTER, ??? );
            AsmError( INVALID_USE_OF_REGISTER );
            return( ERROR );
        default:
#if AMD64_SUPPORT
            if ( ( GetOpndType( base, 0 ) & GetOpndType( index, 0) & SFR_SIZMSK ) == 0 ) {
                AsmError( CANNOT_MIX_16_AND_32_BIT_REGISTERS );
                return( ERROR );
#else
            if( base < T_EAX ) {
                AsmError( CANNOT_MIX_16_AND_32_BIT_REGISTERS );
                return( ERROR );
#endif
            } else if( base == T_EBP ) {
                if( mod_field == MOD_00 ) {
                    mod_field = MOD_01;
                }
            }
            // s-i-b is present ( r/m = 100 )
            rm_field |= S_I_B;
            CodeInfo->sib = ( ss | idx_reg << 3 | base_reg );
#if AMD64_SUPPORT
            rex = (bit3_idx << 1) + (bit3_base); /* set REX_X + REX_B */
#endif
            seg_override( CodeInfo, base, sym, FALSE );
        } /* end switch(index) */
#if AMD64_SUPPORT
        DebugMsg(("mem2code, indirect, base+index: mod_field=%X, rm_field=%X, rex=%X\n", mod_field, rm_field, rex ));
#else
        DebugMsg(("mem2code, indirect, base+index: rm_field=%X\n", rm_field ));
#endif
    }
    if( Opnd_Count == OPND2 ) {
        // shift the register field to left by 3 bit
        CodeInfo->rm_byte = mod_field | ( rm_field << 3 ) | ( CodeInfo->rm_byte & BIT_012 );
#if AMD64_SUPPORT
        //v2.02: exchange B and R, keep X
        //CodeInfo->prefix.rex |= (rex >> 2 );
        CodeInfo->prefix.rex |= ( ( rex >> 2 ) | ( rex & REX_X ) | (( rex & 1) << 2 ) );
#endif
    } else if( Opnd_Count == OPND1 ) {
        CodeInfo->rm_byte = mod_field | rm_field;
#if AMD64_SUPPORT
        CodeInfo->prefix.rex |= rex;
#endif
    }
    return( NOT_ERROR );
}

/*
 set fields in CodeInfo:
 - CodeInfo->mem_type
 - <obsolete: CodeInfo->isfar>
 - CodeInfo->prefix.opsiz
 called by memory_operand()
*/
static void Set_Memtype( struct code_info *CodeInfo, memtype mem_type )
/*********************************************************************/
{
    if( CodeInfo->token == T_LEA )
        return;

    if( mem_type != MT_EMPTY && mem_type != MT_TYPE ) {
        CodeInfo->mem_type = mem_type;
    }
    if( CodeInfo->Ofssize > USE16 && ( IS_MEM_TYPE( CodeInfo->mem_type, WORD ) ) ) {
        /* if we are in use32 mode, we have to add OPSIZ prefix for
         * most of the 386 instructions when operand has type WORD.
         * Exceptions ( MOVSX and MOVZX ) are handled in check_size().
         */
        CodeInfo->prefix.opsiz = TRUE;

    } else if( CodeInfo->Ofssize == USE16 && ( IS_MEM_TYPE( CodeInfo->mem_type, DWORD ) ) ) {

        /* in 16bit mode, a DWORD memory access usually requires an OPSIZ
         * prefix. A few instructions, which access m16:16 operands,
         * are exceptions.
         */
        switch( CodeInfo->token ) {
        case T_LDS:
        case T_LES:
        case T_LFS:
        case T_LGS:
        case T_LSS:
        case T_CALL:
        case T_JMP:
            /* in these cases, opsize does NOT need to be changed  */
            break;
        default:
            CodeInfo->prefix.opsiz = TRUE;
            break;
        }
    }
    return;
}

/* override handling
 * It's either
 * - a register:       set CodeInfo->prefix.RegOverride
 * - a SEG/GRP symbol: set SegOverride
 * called by
 * - process_branch()
 * - idata_fixup()
 * - memory_operand()
 * - data_item()
 */
ret_code segm_override( const expr_list *opndx, struct code_info *CodeInfo )
/**************************************************************************/
{
    struct asm_sym      *sym;

    if( opndx->override != EMPTY ) {
        if( AsmBuffer[opndx->override]->token == T_REG ) {
            int temp = GetRegNo( AsmBuffer[opndx->override]->value );
            if ( SegAssumeTable[temp].error ) {
                DebugMsg(("segm_override: assume error, reg=%u\n", temp ));
                AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
                return( ERROR );
            }
            sym = GetOverrideAssume( temp );
            if ( CodeInfo ) {
                /* hack: save the previous reg override value (needed for CMPS) */
                LastRegOverride = CodeInfo->prefix.RegOverride;
                CodeInfo->prefix.RegOverride = temp;
            }
        } else {
            sym = SymSearch( AsmBuffer[opndx->override]->string_ptr );
        }
        if ( sym && ( sym->state == SYM_GRP || sym->state == SYM_SEG ))
            SegOverride = sym;
    }
    return( NOT_ERROR );
}

static ret_code idata_nofixup( struct code_info *CodeInfo, const expr_list *opndx )
/*********************************************************************************/
{
    OPNDTYPE    op_type;
    int_32      value;
    int         size;

    DebugMsg(("idata_nofixup(kind=%u mem_type=%Xh value=%I64X) enter [CodeInfo->mem_type=%Xh]\n", opndx->kind, opndx->mem_type, opndx->value64, CodeInfo->mem_type));

     // jmp/call/jxx/loop/jcxz/jecxz?
    if( IS_ANY_BRANCH( CodeInfo->token ) ) { 
        return( process_branch( CodeInfo, opndx ) );
    }
    value = opndx->value;
    CodeInfo->data[Opnd_Count] = value;

#if AMD64_SUPPORT
    /* 64bit immediates are restricted to MOV <reg>,<imm64>
     */
    if ( opndx->hvalue != 0 && opndx->hvalue != -1 ) {
        if ( CodeInfo->token == T_MOV && ( CodeInfo->opnd_type[OPND1] & OP_R64 ) ) {
            CodeInfo->opcode |= W_BIT; // set w-bit
            CodeInfo->opnd_type[Opnd_Count] = OP_I64;
            if ( Opnd_Count < OPND3 )
                CodeInfo->data[Opnd_Count+1] = opndx->hvalue;
            DebugMsg(("idata_nofixup exit, op_type=OP_I64\n" ));
            return( NOT_ERROR );
        }
        DebugMsg(("idata_nofixup: error, hvalue=%Xh\n", opndx->hvalue ));
        AsmError( CONSTANT_VALUE_TOO_LARGE );
        return( ERROR );
    }
#endif

    /* size coercion for immediate value? */
    CodeInfo->const_size_fixed = opndx->explicit;

    switch( CodeInfo->mem_type ) {
    case MT_EMPTY:
        switch ( CodeInfo->token ) {
        case T_PUSH:
            /* PUSH <type> PTR <value>? */
            if( opndx->explicit ) {
                if ( opndx->Ofssize != USE_EMPTY )
                    size = SizeFromMemtype( opndx->mem_type, opndx->Ofssize );
                else
                    size = SizeFromMemtype( opndx->mem_type, ModuleInfo.Ofssize );
                if ( size == 1) {
                    op_type = OP_I8;
                } else if( size == 2 ) {
                    op_type = OP_I16;
                    SET_OPSIZ_16( CodeInfo );
                } else if( size == 4 ) { /* size 4 is max also for x86-64! */
                    op_type = OP_I32;
                    SET_OPSIZ_32( CodeInfo );
                } else {
                    DebugMsg(("idata_nofixup: invalid operand, size=%u\n", size ));
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                    return( ERROR );
                }
                break;
            }
            /* for both USE32 and USE64, an OP_I32 is max. */
            if( CodeInfo->Ofssize > USE16 ) {
                if( (int_8)value == value ) {
                    op_type = OP_I8;
                } else {
                    op_type = OP_I32;
                }
            } else {
                /* 16bit mode without typecast. extend to DWORD only if
                 the value cannot be expressed as 16bit */
                //if( (int_8)value == (int_32)value ) {
                if( (int_8)value == value ) {
                    op_type = OP_I8;
                //} else if( (unsigned long)value > USHRT_MAX ) {
                } else if( value <= USHRT_MAX && value >= - USHRT_MAX ) {
                    op_type = OP_I16;
                } else {
                    SET_OPSIZ_32( CodeInfo );
                    op_type = OP_I32;
                }
            }
            break;
        case T_PUSHW:
            op_type = OP_I16;
            if( (int_8)value == (int_16)value ) {
                op_type = OP_I8;
            }
            break;
        case T_PUSHD:
            op_type = OP_I32;
            if( (int_8)value == (int_32)value ) {
                op_type = OP_I8;
            }
            break;
        default:
            if (opndx->explicit) {
                if ( opndx->Ofssize != USE_EMPTY )
                    size = SizeFromMemtype(opndx->mem_type, opndx->Ofssize );
                else
                    size = SizeFromMemtype(opndx->mem_type, ModuleInfo.Ofssize );
                if (size == 1)
                    op_type = OP_I8;
                else if (size == 2)
                    op_type = OP_I16;
                else if (size == 4)
                    op_type = OP_I32;
                else {
                    DebugMsg(("idata_nofixup: invalid operand, size=%u\n", size ));
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                    return( ERROR );
                }
                break;
            }
            /* use true signed values for BYTE only!
             */
            if ( (int_8)value == value )
                op_type = OP_I8;
            //else if ( value <= SHRT_MAX && value >= SHRT_MIN )
            else if ( value <= USHRT_MAX && value >= SHRT_MIN )
                op_type = OP_I16;
            else {
                op_type = OP_I32;
            }
        }
        break;
    case MT_BYTE:
    case MT_SBYTE:
        /* Masm allows to store 255 and -255 in a variable of type SBYTE.
         */
        if( ( value > UCHAR_MAX ) || ( value < - UCHAR_MAX ) ) {
            DebugMsg(("idata_nofixup: BYTE, out of range, value=%lXh\n", value ));
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        }
        op_type = OP_I8;
        break;
    case MT_WORD:
    case MT_SWORD:
        /* Masm allows to store 65535 and -65535 in a variable of type SWORD.
         * Masm v6.15 (and below) even accepts values beyond these limits!
         */
        //if( ( value > SHRT_MAX ) || ( value < SHRT_MIN ) ) {
        /* using USHRT_MAX gives problems with OW 1.8 wcc (JWASMR) */
        //if( ( value > USHRT_MAX ) || ( value < - USHRT_MAX ) ) {
        if( ( value > 65535 ) || ( value < -65535 ) ) {
            DebugMsg(("idata_nofixup: WORD, out of range, value=%lXh\n", value ));
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        }
        if( (int_8)value != value || opndx->mem_type == MT_WORD || opndx->mem_type == MT_SWORD )
            op_type = OP_I16;
        else
            op_type = OP_I8;
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
    case MT_REAL4:
    case MT_DWORD:
    case MT_SDWORD:
        if( (int_8)value != value || opndx->mem_type == MT_DWORD || opndx->mem_type == MT_SDWORD )
            op_type = OP_I32;
        else
            op_type = OP_I8;
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
    case MT_REAL8:
    case MT_QWORD:
    case MT_SQWORD:
    case MT_OWORD:  /* v2.01: example: blendpd xmm0, oword ptr [], 1 */
        /* QWORD is handled like DWORD. the x86-64 architecture doesn't
         * allow 64bit immediate with one exception, and this exception
         * is handled above.
         */
        if ( (int_8)value != value )
            op_type = OP_I32;
        else
            op_type = OP_I8;
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
    //case MT_FAR:
    //    DebugMsg(("idata_nofixup, MT_FAR branch\n" ));
    //case MT_NEAR:
    //    DebugMsg(("idata_nofixup, MT_NEAR branch\n" ));
    //case MT_SHORT:
    //    DebugMsg(("idata_nofixup: mem_type=MT_SHORT\n" ));
    default:
        AsmErr( INVALID_INSTRUCTION_OPERANDS );
        return( ERROR );
    }
    CodeInfo->opnd_type[Opnd_Count] = op_type;
    DebugMsg(("idata_nofixup exit, op_type=%X\n", op_type ));
    return( NOT_ERROR );
}

// create immediate data with fixup (offsets)

static ret_code idata_fixup( struct code_info *CodeInfo, expr_list *opndx )
/*************************************************************************/
{
    //struct asmfixup     *fixup;
    enum fixup_types    fixup_type;
    int                 size;
    uint_8              Ofssize; // 1=32bit, 0=16bit offset for fixup

    DebugMsg(("idata_fixup(type=%u, mem_type=%Xh) enter [CodeInfo.mem_type=%Xh]\n", opndx->type, opndx->mem_type, CodeInfo->mem_type));

     // jmp/call/jcc/loopcc/jxcxz?
    if( IS_ANY_BRANCH( CodeInfo->token ) ) {
        return( process_branch( CodeInfo, opndx ) );
    }
    CodeInfo->data[Opnd_Count] = opndx->value;

    if ( opndx->Ofssize != USE_EMPTY ) {
        Ofssize = opndx->Ofssize;
    } else if( ( opndx->sym->state == SYM_SEG )
        || ( opndx->sym->state == SYM_GRP )
        || ( opndx->instr == T_SEG ) ) {
        Ofssize = USE16;
    } else if( opndx->abs ) {  /* an (external) absolute symbol? */
        Ofssize = USE16;
    } else {
        Ofssize = GetSymOfssize( opndx->sym );
    }

    if( opndx->instr != EMPTY ) {
        if( ( opndx->base_reg != EMPTY ) || ( opndx->idx_reg != EMPTY ) ) {
            AsmError( INVALID_MEMORY_POINTER );
            return( ERROR );
        }
        switch ( opndx->instr ) {
        case T_OFFSET:
            if ( opndx->sym->state == SYM_GRP ) {
                AsmError( CANNOT_OFFSET_GRP );
                return( ERROR );
            }
            break;
        }
    }

    if( opndx->mem_type == MT_SHORT ) {
        /* short works for branch instructions only */
        AsmErr( INVALID_INSTRUCTION_OPERANDS );
        return( ERROR );
    }

    if ( CodeInfo->mem_type == MT_EMPTY && Opnd_Count > OPND1 ) {
        size = OperandSize( CodeInfo->opnd_type[OPND1], CodeInfo );
        //if( opndx->instr != EMPTY && size && (size < 2 || ( Ofssize && size < 4 ))) {
        /* may be a forward reference, so wait till pass 2 */
        if( Parse_Pass > PASS_1 && opndx->instr != EMPTY && size && (size < 2 || ( Ofssize && size < 4 ))) {
            switch ( opndx->instr ) {
            case T_OFFSET:
            case T_LROFFSET:
#if IMAGERELSUPP
            case T_IMAGEREL:
#endif
#if SECTIONRELSUPP
            case T_SECTIONREL:
#endif
                AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, size, ( 2 << Ofssize ) );
                return( ERROR );
            }
        }
        switch ( size ) {
        case 1: CodeInfo->mem_type = MT_BYTE; break;
        case 2: CodeInfo->mem_type = MT_WORD; break;
        case 4: CodeInfo->mem_type = MT_DWORD;break;
#if AMD64_SUPPORT
        case 8: CodeInfo->mem_type = MT_QWORD;break;
#endif
        }
    }
    if ( CodeInfo->mem_type == MT_EMPTY ) {
        if( opndx->abs ) {
            if( opndx->mem_type != MT_EMPTY && opndx->mem_type != MT_ABS ) {
                CodeInfo->mem_type = opndx->mem_type;
            } else {
                if( IS_OPER_32( CodeInfo ) )
                    CodeInfo->mem_type = MT_DWORD;
                else
                    CodeInfo->mem_type = MT_WORD;
            }
        } else {
            switch ( CodeInfo->token ) {
            case T_PUSHW:
            case T_PUSHD:
            case T_PUSH:
                /* for forward reference, assume BYTE */
                /* v2.02: don't assume BYTE if it is SEG/GRP */
                //if ( opndx->mem_type == MT_EMPTY ) {
                if ( opndx->mem_type == MT_EMPTY && opndx->instr != T_SEG ) {
                    opndx->mem_type = MT_BYTE;
                    break;
                }
                /* default: push offset only */
                /* for PUSH + undefined symbol, assume BYTE */
                if ( opndx->mem_type == MT_FAR && ( opndx->explicit == FALSE ) )
                    opndx->mem_type = MT_NEAR;
                if ( CodeInfo->token == T_PUSHW )
                    if ( SizeFromMemtype( opndx->mem_type, Ofssize ) < 2 )
                        opndx->mem_type = MT_WORD;
                else if ( CodeInfo->token == T_PUSHD )
                    if ( SizeFromMemtype( opndx->mem_type, Ofssize ) < 4 )
                        opndx->mem_type = MT_DWORD;
                break;
            }
            /* if a WORD size is given, don't override it with */
            /* anything what might look better at first glance */
            if( opndx->mem_type != MT_EMPTY )
                CodeInfo->mem_type = opndx->mem_type;
            else if ( Ofssize > USE16 )
                CodeInfo->mem_type = MT_DWORD;
            else
                CodeInfo->mem_type = MT_WORD;
        }
    }
    switch( SizeFromMemtype ( CodeInfo->mem_type, Ofssize ) ) {
    case 1:  CodeInfo->opnd_type[Opnd_Count] = OP_I8;  SET_OPSIZ_NO( CodeInfo );  break;
    case 2:  CodeInfo->opnd_type[Opnd_Count] = OP_I16; SET_OPSIZ_16( CodeInfo );  break;
    case 4:  CodeInfo->opnd_type[Opnd_Count] = OP_I32; SET_OPSIZ_32( CodeInfo );  break;
#if AMD64_SUPPORT
    case 8:  CodeInfo->opnd_type[Opnd_Count] = OP_I64; SET_OPSIZ_32( CodeInfo );  break;
#endif
#ifdef DEBUG_OUT
    default:
        DebugMsg(("idata_fixup, unexpected size %u\n", SizeFromMemtype ( CodeInfo->mem_type, Ofssize ) ));
#endif
    }
    if( opndx->instr == T_SEG ) {
        fixup_type = FIX_SEG;
    } else if( CodeInfo->mem_type == MT_BYTE ) {
        DebugMsg(("idata_fixup, mem_type=BYTE\n" ));
        if ( opndx->instr == T_HIGH ) {
            DebugMsg(("idata_fixup, FIX_HIBYTE\n" ));
            fixup_type = FIX_HIBYTE;
        } else {
            DebugMsg(("idata_fixup, FIX_LOBYTE\n" ));
            fixup_type = FIX_LOBYTE;
        }
    } else if( CodeInfo->mem_type == MT_FAR ) {
        fixup_type = ( Ofssize ) ? FIX_PTR32 : FIX_PTR16;
        CodeInfo->isfar = TRUE; /* needed for MarkFixupp() */
        if ( opndx->Ofssize != USE_EMPTY )
            CodeInfo->Ofssize = opndx->Ofssize;
    } else if( IS_OPER_32( CodeInfo ) ) {
#if AMD64_SUPPORT
        if ( Ofssize == USE64 && CodeInfo->mem_type == MT_QWORD )
            fixup_type = FIX_OFF64;
        else
#endif
            fixup_type = ( Ofssize ) ? FIX_OFF32 : FIX_OFF16;
    } else {
        if( Ofssize ) {
            // fixme !!!! warning
            // operand size is 16bit
            // but fixup is 32-bit
        }
        fixup_type = FIX_OFF16;
    }
//    ConstantOnly = TRUE;
    CodeInfo->opcode |= W_BIT;

    /* set global vars Frame, Frame_Datum and SegOverride */
    segm_override( opndx, NULL );
    if ( ModuleInfo.offsettype == OT_SEGMENT &&
        ( opndx->instr == T_OFFSET || opndx->instr == T_SEG ))
        find_frame2( opndx->sym );
    else
        find_frame( opndx->sym );

    DebugMsg(("idata_fixup: calling AddFixup(%s, %u)\n", opndx->sym->name, fixup_type ));
    CodeInfo->InsFixup[Opnd_Count] = AddFixup( opndx->sym, fixup_type, OPTJ_NONE );

    if (opndx->instr == T_LROFFSET)
        CodeInfo->InsFixup[Opnd_Count]->loader_resolved = TRUE;

#if IMAGERELSUPP
    if (opndx->instr == T_IMAGEREL && fixup_type == FIX_OFF32)
        CodeInfo->InsFixup[Opnd_Count]->type = FIX_OFF32_IMGREL;
#endif
#if SECTIONRELSUPP
    if (opndx->instr == T_SECTIONREL && fixup_type == FIX_OFF32)
        CodeInfo->InsFixup[Opnd_Count]->type = FIX_OFF32_SECREL;
#endif
    DebugMsg(("idata_fixup exit [CodeInfo.mem_type=%Xh, Ofssize=%u, fixup->frame=%d]\n", CodeInfo->mem_type, CodeInfo->Ofssize, CodeInfo->InsFixup[Opnd_Count]->frame ));

    return( NOT_ERROR );
}

static void add_frame( const struct code_info *CodeInfo )
/*******************************************************/
/* add frame data in Frame and Frame_Datum to current fixup */
{
    struct asmfixup     *fixup;

    if( Parse_Pass != PASS_1 ) {
        fixup = CodeInfo->InsFixup[Opnd_Count];
        if( fixup == NULL )
            return;
        fixup->frame = Frame;
        fixup->frame_datum = Frame_Datum;
    }
}

/*
 in: opndx=operand to process
 in: Opnd_Count=no of operand (0=first operand,1=second operand)
 out: CodeInfo->data[]
 out: CodeInfo->opnd_type[]
*/

static ret_code memory_operand( struct code_info *CodeInfo, expr_list *opndx, bool with_fixup )
/*********************************************************************************************/
{
    char                ss = SCALE_FACTOR_1;
    int                 index = EMPTY;
    int                 base = EMPTY;
    int                 j;
    int                 size;
    struct asm_sym      *sym;
    uint_8              Ofssize;
    enum fixup_types    fixup_type;

    DebugMsg(("memory_operand(opndx->value=%X, memtype=%Xh, fixup=%u) enter, [CodeInfo->memtype=%Xh, Ofssize=%u]\n",
              opndx->value, opndx->mem_type, with_fixup, CodeInfo->mem_type, CodeInfo->Ofssize ));
    CodeInfo->data[Opnd_Count] = opndx->value;
    CodeInfo->opnd_type[Opnd_Count] = OP_M;

#if AMD64_SUPPORT
    /*
     * set rex Wide bit if a QWORD operand is found (not for FPU/MMX/SSE instr).
     * This looks pretty hackish now and is to be cleaned!
     * v2.01: also had issues with SSE2 MOVSD/CMPSD, now fixed!
     */
    if ( IS_MEM_TYPE( opndx->mem_type, QWORD ) ) {
        if ( CodeInfo->token != T_CMPXCHG8B &&
            CodeInfo->token != T_LEA &&
            CodeInfo->token != T_PUSH &&
            CodeInfo->token != T_POP &&
            !( CodeInfo->pcurr->cpu & ( P_FPU_MASK | P_EXT_MASK ) ) )
            CodeInfo->prefix.rex |= REX_W;
    }
#endif

    sym = opndx->sym;

    segm_override( opndx, CodeInfo );

#if 1
    /* convert MT_PTR to MT_WORD, MT_DWORD, MT_FWORD, MT_QWORD. */
    /* MT_PTR cannot be set explicitely (by the PTR operator),
     * so this value must come from a label or a structure field.
     * (above comment is most likely plain wrong, see 'PF16 ptr [reg]'!
     * This code needs cleanup!
     */
    if ( opndx->mem_type == MT_PTR ) {
        asm_sym *sym2 = sym;
        size = 0;
        if ( opndx->mbr )  /* the mbr field has higher priority */
            sym2 = opndx->mbr;
        if (sym2 && sym2->type) {
            size = sym2->type->total_size;
            CodeInfo->isfar = sym2->type->isfar;

            /* there's an ambiguity with pointers of size DWORD,
             since they can be either NEAR32 or FAR16 */
            if ( size == 4 && sym2->type->Ofssize != CodeInfo->Ofssize )
                opndx->Ofssize = sym2->type->Ofssize;

        } else if (sym2)  {
            size = sym2->total_size;
#if 1 /* v2.0: handle PF16 ptr [ebx], which didn't work in v1.96 */
        } else if ( opndx->explicit && opndx->type ) {
            size = opndx->type->total_size;
            CodeInfo->isfar = opndx->type->isfar;
#endif
        } else {
            switch ( ModuleInfo.model ) {
            case MOD_COMPACT:
            case MOD_LARGE:
            case MOD_HUGE:
                DebugMsg(("memory_operand: MT_PTR, model with FAR data pointers\n" ));
                size = 2;
            default:
                size += (2 << ModuleInfo.defOfssize );
            }
        }
        if ( size )
            MemtypeFromSize( size, &opndx->mem_type );
        DebugMsg(("memory_operand: MT_PTR, size=%u, new memtype=0x%x\n", size, opndx->mem_type ));
    }
#endif
    Set_Memtype( CodeInfo, opndx->mem_type );
    if( opndx->mbr != NULL ) {
        /* if the struct field is just another struct, use it's total size
         * to set CodeInfo->mem_type.
         */
        //if ( opndx->mbr->mem_type == MT_TYPE ) {
        //v2: don't overwrite opndx->mem_type,
        //    testcase: cmp (dword ptr <struct_field>), 0
        if ( opndx->mbr->mem_type == MT_TYPE && opndx->mem_type == MT_EMPTY ) {
            memtype mem_type;
            DebugMsg(("memory_operand: mbr %s has mem_type MT_TYPE, total_size=%u\n", opndx->mbr->name, opndx->mbr->total_size ));
            if ( MemtypeFromSize( opndx->mbr->total_size, &mem_type ) == NOT_ERROR )
                Set_Memtype( CodeInfo, mem_type );
        }
        //else  /* v2: obsolete */
        //    Set_Memtype( CodeInfo, opndx->mbr->mem_type );
    }

    /* if memory operand has no mem_type, but contains a symbol
     * with a mem_type, then use the symbol's memtype.
     * test case: jmp [ebx*4 + label1]
     */
    if( CodeInfo->mem_type == MT_EMPTY && opndx->instr == EMPTY &&
       sym &&  sym->mem_type != MT_EMPTY ) {
        Set_Memtype( CodeInfo, sym->mem_type );
    }

    /* instruction-specific handling */
    switch ( CodeInfo->token ) {
    case T_JMP:
    case T_CALL:
        /* v1.95: convert MT_NEAR/MT_FAR and display error if no type.
         * For memory operands, expressions of type MT_NEAR/MT_FAR are
         * call [bx+<code_label>]
         */
        switch ( CodeInfo->mem_type ) {
        case MT_EMPTY:
            /* with -Zm, no size needed for indirect CALL/JMP */
            if ( ModuleInfo.m510 == FALSE &&
                ( Parse_Pass > PASS_1 || opndx->sym == NULL ) ) {
                DebugMsg(("memory_operand, JMP/CALL: CodeInfo->memtype=empty, instruction operand must have size\n" ));
                AsmError( INSTRUCTION_OPERAND_MUST_HAVE_SIZE );
                return( ERROR );
            }
            /* fall through */
        case MT_NEAR:
             /* changed v2.0 */
            CodeInfo->mem_type = (CodeInfo->Ofssize == USE64) ? MT_QWORD : (CodeInfo->Ofssize == USE32) ? MT_DWORD : MT_WORD;
            DebugMsg(("memory_operand, JMP/CALL: CodeInfo->memtype set to %Xh\n", CodeInfo->mem_type ));
            //CodeInfo->mem_type = CodeInfo->Ofssize ? MT_DWORD : MT_WORD;
            break;
        case MT_FAR:
             /* changed v2.0 */
            CodeInfo->mem_type = (CodeInfo->Ofssize == USE64) ? MT_TBYTE : (CodeInfo->Ofssize == USE32) ? MT_FWORD : MT_DWORD;
            //CodeInfo->mem_type = CodeInfo->Ofssize ? MT_FWORD : MT_DWORD;
            break;
        }
        j = SizeFromMemtype( CodeInfo->mem_type, CodeInfo->Ofssize );
#if AMD64_SUPPORT
        if ( ( CodeInfo->Ofssize != USE64 ) && ( j == 1 || j > 6 ) ) {
#else
        if ( j == 1 || j > 6 ) {
#endif
            /* CALL/JMP possible for WORD/DWORD/FWORD memory operands only */
            DebugMsg(("memory_operand: invalid operand, size=%u\n", j ));
            AsmError( INVALID_OPERAND_SIZE );
            return( ERROR );
        }

        if( opndx->mem_type == MT_FAR || CodeInfo->mem_type == MT_FWORD ||
#if AMD64_SUPPORT
           ( CodeInfo->mem_type == MT_TBYTE && CodeInfo->Ofssize == USE64 ) ||
#endif
            ( CodeInfo->mem_type == MT_DWORD &&
              ((CodeInfo->Ofssize == USE16 && opndx->Ofssize != USE32) ||
               (CodeInfo->Ofssize == USE32 && opndx->Ofssize == USE16 )))) {
            CodeInfo->isfar = TRUE;
        }
        DebugMsg(("memory_operand: JMP/CALL, CodeInfo->far=%u\n", CodeInfo->isfar ));
        break;
    }

    if ( ( CodeInfo->mem_type & MT_SPECIAL) == 0 ) {
        switch ( CodeInfo->mem_type & MT_SIZE_MASK ) {
            /* size is encoded 0-based */
        case  0:  CodeInfo->opnd_type[Opnd_Count] = OP_M8;   break;
        case  1:  CodeInfo->opnd_type[Opnd_Count] = OP_M16;  break;
        case  3:  CodeInfo->opnd_type[Opnd_Count] = OP_M32;  break;
        case  5:  CodeInfo->opnd_type[Opnd_Count] = OP_M48;  break;
        case  7:  CodeInfo->opnd_type[Opnd_Count] = OP_M64;  break;
        case  9:  CodeInfo->opnd_type[Opnd_Count] = OP_M80;  break;
        case 15:  CodeInfo->opnd_type[Opnd_Count] = OP_M128; break;
        }
    }

    // check for base registers

    if (opndx->base_reg != EMPTY ) {
        base = AsmBuffer[opndx->base_reg]->value;
        if ( ( ( GetOpndType( base, 1 ) & OP_R32) && CodeInfo->Ofssize == USE32 ) ||
#if AMD64_SUPPORT
            ( ( GetOpndType( base, 1 ) & OP_R64) && CodeInfo->Ofssize == USE64 ) ||
#endif
            ( ( GetOpndType( base, 1 ) & OP_R16) && CodeInfo->Ofssize == USE16 ) )
            CodeInfo->prefix.adrsiz = FALSE;
        else {
            CodeInfo->prefix.adrsiz = TRUE;
#if AMD64_SUPPORT
            /* 16bit addressing modes don't exist in long mode */
            if ( ( GetOpndType( base, 1 ) & OP_R16) && CodeInfo->Ofssize == USE64 ) {
                AsmError( INVALID_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING );
                return( ERROR );
            }
#endif
            //DebugMsg(("addr size prefix set, opnd_type=%X\n", AsmOpTable[j].opnd_type[1] ));
        }
    }

    // check for index registers

    if( opndx->idx_reg != EMPTY ) {
        index = AsmBuffer[opndx->idx_reg]->value;
        if ( ( ( GetOpndType( index, 1 ) & OP_R32) && CodeInfo->Ofssize == USE32 ) ||
#if AMD64_SUPPORT
            ( ( GetOpndType( index, 1 ) & OP_R64) && CodeInfo->Ofssize == USE64 ) ||
#endif
            ( ( GetOpndType( index, 1 ) & OP_R16) && CodeInfo->Ofssize == USE16 ) ) {
            CodeInfo->prefix.adrsiz = FALSE;
        } else {
            CodeInfo->prefix.adrsiz = TRUE;
        }
#if AMD64_SUPPORT
        if( index == T_ESP || index == T_RSP ) {
#else
        if( index == T_ESP ) {
#endif
            if( opndx->scale == 1 ) { /* if scale 1, swap base - index regs */
                index = base;
                base = AsmBuffer[opndx->idx_reg]->value;
            } else {
                AsmErr( CANNOT_BE_USED_AS_INDEX_REGISTER, AsmBuffer[opndx->idx_reg]->string_ptr );
                return( ERROR );
            }
        }
        //if( IS_ADDR32( CodeInfo ) ) {
        if( ( CodeInfo->Ofssize == USE16 && CodeInfo->prefix.adrsiz == 1 ) ||
#if AMD64_SUPPORT
           CodeInfo->Ofssize == USE64  ||
#endif
           ( CodeInfo->Ofssize == USE32 && CodeInfo->prefix.adrsiz == 0 ) ) {
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                if( CodeInfo->Ofssize == USE16 )
                    CodeInfo->prefix.adrsiz = TRUE;
                switch( index ) {
#if AMD64_SUPPORT
                case T_RSP:
#endif
                case T_ESP:
                case T_BX:
                case T_BP:
                case T_SI:
                case T_DI:
                    // cannot use ESP|RSP or 16-bit reg as index
                    AsmError( INVALID_INDEX_REGISTER );
                    return( ERROR );
                default:
                    /* scale, 1->00, 2->40, 4->80, 8->C0 */
                    switch( opndx->scale ) {
                    case 1:  break; /* ss = 00 */
                    case 2: ss = SCALE_FACTOR_2; break; /* ss = 01 */
                    case 4: ss = SCALE_FACTOR_4; break; /* ss = 10 */
                    case 8: ss = SCALE_FACTOR_8; break; /* ss = 11 */
                    default: /* must be * 1, 2, 4 or 8 */
                        AsmError( SCALE_FACTOR_MUST_BE_1_2_4_OR_8 );
                        return( ERROR );
                    }
                }
            } else {
                // 286 and down cannot use this memory mode
                AsmError( INVALID_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING );
                return( ERROR );
            }
        } else {
            /* v2.01: 16-bit addressing mode. No scale > 1 possible */
            if ( opndx->scale > 1 ) {
                AsmError( INVALID_USE_OF_REGISTER );
                return( ERROR );
            }
        }
    }

    if( with_fixup ) {

        if( opndx->abs ) {
            Ofssize = IS_ADDR32( CodeInfo );
        } else if ( sym ) {
            Ofssize = GetSymOfssize( sym );
        } else if ( SegOverride ) {
            Ofssize = GetSymOfssize( SegOverride );
        } else
            Ofssize = CodeInfo->Ofssize;

        /* now set fixup_type.
         * for direct addressing, the fixup type can easily be set by
         * the symbol's offset size.
         */
        if( ( opndx->base_reg == EMPTY ) && ( opndx->idx_reg == EMPTY ) ) {
            SET_ADRSIZ( CodeInfo, Ofssize );
#if AMD64_SUPPORT
            if ( Ofssize == USE64 )
                fixup_type = FIX_RELOFF32;
            else
#endif
                fixup_type = ( Ofssize ) ? FIX_OFF32 : FIX_OFF16;
            DebugMsg(( "memory_operand: direct addressing, fixup type=%u\n", fixup_type ));
        } else {
            DebugMsg(( "memory_operand: CodeInfo->Ofssize=%u/prefix.adrsize=%u, Ofssize=%u\n",
                      CodeInfo->Ofssize, CodeInfo->prefix.adrsiz, Ofssize ));
#if AMD64_SUPPORT
            if( Ofssize == USE64 ) {
                fixup_type = FIX_OFF32;
            } else
#endif
            if( IS_ADDR32( CodeInfo ) ) { /* address prefix needed? */
                /* changed for v1.95. Probably more tests needed!
                 * test case:
                 *   mov eax,[ebx*2-10+offset var] ;code and var are 16bit!
                 * the old code usually works fine because HiWord of the
                 * symbol's offset is zero. However, if there's an additional
                 * displacement which makes the value stored at the location
                 * < 0, then the target's HiWord becomes <> 0.
                 */
                //fixup_type = ( Ofssize ) ? FIX_OFF32 : FIX_OFF16;
                fixup_type = FIX_OFF32;
            } else {
                fixup_type = FIX_OFF16;
                if( Ofssize && Parse_Pass == PASS_2 ) {
                    // address size is 16bit but label is 32-bit.
                    // example: use a 16bit register as base in FLAT model:
                    //   test buff[di],cl
                    AsmWarn( 2, WORD_FIXUP_FOR_32BIT_LABEL, sym->name );
                }
            }
        }

#ifdef DEBUG_OUT
        if ( sym )
            DebugMsg(("memory_operand: calling AddFixup(%s, fixup=%u) [CodeInfo->memtype=%Xh]\n", sym->name, fixup_type, CodeInfo->mem_type));
        else
            DebugMsg(("memory_operand: calling AddFixup(NULL, fixup=%u) [CodeInfo->memtype=%Xh]\n", fixup_type, CodeInfo->mem_type));
#endif
        /* no fixups are needed for memory operands of string instructions and XLAT.
         * However, CMPSD and MOVSD are also SSE2 opcodes, so the fixups must be generated
         * anyways.
         */
        if ( CodeInfo->token != T_XLAT )
            CodeInfo->InsFixup[Opnd_Count] = AddFixup( sym, fixup_type, OPTJ_NONE );

        if( Modend ) { /* is current operand END argument? */
            asm_sym *assume;
            /* set global vars Frame and Frame_Datum */
            GetAssume( SegOverride, sym, ASSUME_NOTHING, &assume);
            SetFixupFrame( assume );
        } else {
            if( mem2code( CodeInfo, ss, index, base, sym ) == ERROR ) {
                return( ERROR );
            }
        }
        /* set again current fixup in CodeInfo */
        add_frame( CodeInfo );

    } else { /* branch without fixup */
#if 0
        /* if an undefined label is moved (mov [xxx],offset yyy)
         * change to immediate data. this might be placed better in
         * check_size()!
         * v2.0 todo: check if this code runs at all. forward references
         * will create a fixup since v2.
         */
        if (sym && sym->state == SYM_UNDEFINED &&
            CodeInfo->token == T_MOV &&
            Opnd_Count == OPND2 &&
            (CodeInfo->opnd_type[OPND1] & OP_M_ANY) )
            CodeInfo->opnd_type[Opnd_Count] = OP_I8;
#endif
        if (sym && sym->state == SYM_STACK) {
            if( base != EMPTY ) {
                if( index != EMPTY ) {
                    /* no free index register */
                    AsmError( TOO_MANY_BASE_REGISTERS );
                    return( ERROR );
                } else {
                    index = base;
                }
            }
            if( CodeInfo->Ofssize == USE32 ) {
                base = T_EBP;
#if AMD64_SUPPORT
            } else if( CodeInfo->Ofssize == USE64 ) {
                base = T_RBP;
#endif
            } else {
                base = T_BP;
            }
            //v2.0: removed
            //if( CodeInfo->mem_type == MT_EMPTY ) {
            //    Set_Memtype( CodeInfo, sym->mem_type );
            //}
        }
        if( mem2code( CodeInfo, ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
    } /* end if ( with_fixup ) */

    DebugMsg(("memory_operand exit, ok, opndx.type/value=%Xh/%Xh, CodeInfo.memtype/rmbyte=%X/%X opndtype=%Xh fix=%Xh\n",
              opndx->type, opndx->value, CodeInfo->mem_type, CodeInfo->rm_byte, CodeInfo->opnd_type[Opnd_Count], CodeInfo->InsFixup[Opnd_Count] ));
    return( NOT_ERROR );
}

ret_code process_address( struct code_info *CodeInfo, expr_list *opndx )
/**********************************************************************/
/*
 * parse the memory reference operand
 * Opnd_Count is 0 for first operand, 1 for second, ...
 * valid return values: NOT_ERROR, ERROR
 */
{
    memtype     mem_type;

    if( opndx->indirect ) {  /* indirect register operand or stack var */

        DebugMsg(("process_address: INDIRECT, sym=%s, adrsiz=%u\n", opndx->sym ? opndx->sym->name : "NULL", CodeInfo->prefix.adrsiz ));
        if ( opndx->hvalue && ( opndx->hvalue != -1 || opndx->value >= 0 ) ) {
            DebugMsg(("process_address: displacement doesn't fit in 32 bits: %I64X\n", opndx->value64 ));
            AsmError( CONSTANT_VALUE_TOO_LARGE );
            return( ERROR );
        }
        if( opndx->sym == NULL || opndx->sym->state == SYM_STACK ) {
            return( memory_operand( CodeInfo, opndx, FALSE ) );
        } else {
            return( memory_operand( CodeInfo, opndx, TRUE ) );
        }
    } else if( opndx->instr != EMPTY ) {
        /* instr is OFFSET | LROFFSET | SEG | LOW | LOWWORD, ... */
        DebugMsg(("process_address: instr != EMPTY\n"));
        if( opndx->sym == NULL ) { /* better to check opndx->type? */
            return( idata_nofixup( CodeInfo, opndx ) );
        } else {
            /* allow "lea <reg>, [offset <sym>]" */
            if( CodeInfo->token == T_LEA && opndx->instr == T_OFFSET )
                return( memory_operand( CodeInfo, opndx, TRUE ) );
            return( idata_fixup( CodeInfo, opndx ) );
        }
    } else if( opndx->sym == NULL ) { // direct operand without symbol
        DebugMsg(("process_address: symbol=NULL\n" ));
        if( opndx->override != EMPTY ) {
            /* direct absolute memory without symbol.
             DS:[0] won't create a fixup, but
             DGROUP:[0] will create one! */
#if AMD64_SUPPORT
            /* for 64bit, always create a fixup, since RIP-relative addressing is used */
            if ( AsmBuffer[opndx->override]->token == T_REG && CodeInfo->Ofssize != USE64 )
#else
            if ( AsmBuffer[opndx->override]->token == T_REG )
#endif
                return( memory_operand( CodeInfo, opndx, FALSE ) );
            else
                return( memory_operand( CodeInfo, opndx, TRUE ) );
        } else {
            return( idata_nofixup( CodeInfo, opndx ) );
        }
    } else if( ( opndx->sym->state == SYM_UNDEFINED ) && !opndx->explicit ) {
        DebugMsg(("process_address: sym=SYM_UNDEFINED, name=%s, state=%X\n", opndx->sym->name, opndx->sym->state ));
        if( Parse_Pass != PASS_1 ) {
            AsmErr( SYMBOL_NOT_DEFINED, opndx->sym->name );
            return( ERROR );
        }
        /* undefined symbol, it's not possible to determine
         * operand type and size currently. However, for backpatching
         * a fixup should be created.
         */
        /* jmp/call/jxx/loop/jcxz/jecxz? Then assume a code label */
        if( IS_ANY_BRANCH( CodeInfo->token ) )
            return( process_branch( CodeInfo, opndx ) );

        switch( CodeInfo->token ) {
        case T_PUSH:
        case T_PUSHW:
        case T_PUSHD:
            /* v2.0: don't assume immediate operand if cpu is 8086 */
            if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) > P_86 ) {
                //return( idata_nofixup( CodeInfo, opndx ) ); /* v1.96: changed */
                return( idata_fixup( CodeInfo, opndx ) );
            }
            break;
        default:
            /* if it is the second operand and the first one
             is a memory reference, then assume it's an immediate!
             example: mov [var], label
             */
            if ( Opnd_Count == OPND2 &&
                ( CodeInfo->opnd_type[OPND1] & OP_M_ANY ) ) {
                return( idata_fixup( CodeInfo, opndx ) );
            }
        }
        /* v2.0: fixups should be generated for undefined operands
         * to allow backpatching!
         */
        //return( memory_operand( CodeInfo, opndx, FALSE ) );
        return( memory_operand( CodeInfo, opndx, TRUE ) );

    } else if( ( opndx->sym->state == SYM_SEG ) ||
               ( opndx->sym->state == SYM_GRP ) ) {
        DebugMsg(("process_address: sym->state=SEG/GROUP\n"));
        // SEGMENT and GROUP symbol is converted to SEG symbol
        // for next processing
        opndx->instr = T_SEG;
        return( idata_fixup( CodeInfo, opndx ) );
    } else {
        DebugMsg(("process_address direct, sym=%s\n", opndx->sym->name ));
        //mem_type = ( opndx->explicit ) ? opndx->mem_type : opndx->sym->mem_type;
        /* to fade usage of 'explicit' */
        mem_type = opndx->mem_type;

        /* symbol external, but absolute? */
        if( opndx->abs ) {
            return( idata_fixup( CodeInfo, opndx ) );
        }

        // CODE location is converted to OFFSET symbol
        switch( mem_type ) {
        case MT_FAR:
        case MT_NEAR:
        case MT_SHORT:
        case MT_PROC:
            if( CodeInfo->token == T_LEA ) {
                return( memory_operand( CodeInfo, opndx, TRUE ) );
            } else if( opndx->sym == &symPC ) {
                return( idata_fixup( CodeInfo, opndx ) );
            } else if( opndx->mbr != NULL ) { // structure field?
                return( memory_operand( CodeInfo, opndx, TRUE ) );
            } else {
                return( idata_fixup( CodeInfo, opndx ) );
            }
            break;
        default:
            // direct memory with fixup
            return( memory_operand( CodeInfo, opndx, TRUE ) );
            break;
        }
    }
    return( NOT_ERROR );
}

// handle constant operands
// these might also need a fixup if they are externals (EXTERN:ABS!)

static ret_code process_const( struct code_info *CodeInfo, expr_list *opndx )
/***************************************************************************/
{
    /* hack for IMUL: compress the operands so there are 2 only */
    if( ( CodeInfo->token == T_IMUL ) &&
       ( CodeInfo->opnd_type[OPND1] & OP_R ) ) {
        if( Opnd_Count == OPND2 ) {
#if AMD64_SUPPORT
            CodeInfo->prefix.rex |= ((CodeInfo->prefix.rex & REX_B) ? REX_R : 0);
#endif
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & ~BIT_345 )
                          | ( ( CodeInfo->rm_byte & BIT_012 ) << 3 );
        } else if( Opnd_Count == OPND3 ) {
            CodeInfo->opnd_type[OPND1] = CodeInfo->opnd_type[OPND2];
            CodeInfo->opnd_type[OPND2] = OP_NONE;
            CodeInfo->data[OPND1] = CodeInfo->data[OPND2];
            CodeInfo->data[OPND2] = 0;
            CodeInfo->InsFixup[OPND1] = CodeInfo->InsFixup[OPND2];
            CodeInfo->InsFixup[OPND2] = NULL;
            Opnd_Count = OPND2;
        }
    }
    return( idata_nofixup( CodeInfo, opndx ) );
}

static ret_code process_register( struct code_info *CodeInfo, const expr_list *opndx )
/************************************************************************************/
/*
- parse and encode direct register operands;
*/
{
    int                 temp;
    int                 reg;

    DebugMsg(( "process_register enter (%s)\n", AsmBuffer[opndx->base_reg]->string_ptr ));
    temp = AsmBuffer[opndx->base_reg]->value;
    reg = GetRegNo( temp );
    /* the register's "OP-flags" are stored in opnd_type[1] */
    CodeInfo->opnd_type[Opnd_Count] = GetOpndType( temp, 1 );
    switch( GetOpndType( temp, 1 ) ) {
    case OP_R8:
#if AMD64_SUPPORT
        if ( CodeInfo->Ofssize == USE64 && reg >=4 && reg <=7 )
            if ( AsmOpTable[temp].cpu == P_86 )
                CodeInfo->x86hi_used = 1;
            else
                CodeInfo->x64lo_used = 1;
#endif
        /* fall through */
    case OP_AL:
        CodeInfo->opcode &= NOT_W_BIT;         // clear w-bit
        break;
    case OP_CL: /* only appears in "shift opnd,CL" instructions */
        break;
    case OP_AX:
    case OP_DX: /* only appears in "in" and "out" instructions  */
    case OP_R16:
        CodeInfo->opcode |= W_BIT;             // set w-bit
        if( CodeInfo->Ofssize > USE16 )
            CodeInfo->prefix.opsiz = TRUE;
        break;
#if AMD64_SUPPORT
    case OP_RAX:
    case OP_R64:
        CodeInfo->prefix.rex |= REX_W;
        /* fall through */
#endif
    case OP_EAX:
    case OP_R32:
#ifdef DEBUG_OUT
        DebugMsg(("process_register(%s) R32/R64 reg=%u prefix.rex=%X\n", AsmBuffer[opndx->base_reg]->string_ptr, reg, CodeInfo->prefix.rex ));
#endif
        CodeInfo->opcode |= W_BIT;             // set w-bit
        if( CodeInfo->Ofssize == USE16 )
            CodeInfo->prefix.opsiz = TRUE;
        break;
    case OP_SR386:  // 386 segment register
        /* fall through */
    case OP_SR86:   // 8086 segment register
        if( reg == 1 ) { /* 1 is CS */
            // POP CS is not allowed
            if( CodeInfo->token == T_POP ) {
                AsmError( POP_CS_IS_NOT_ALLOWED );
                return( ERROR );
            }
        }
        break;
    case OP_ST:
        temp = opndx->idx_reg;
        if ( temp > 7 ) { /* v1.96: index check added */
            AsmError( INVALID_COPROCESSOR_REGISTER );
            return( ERROR );
        }
        CodeInfo->rm_byte |= temp;
        if( temp != 0 )
            CodeInfo->opnd_type[Opnd_Count] = OP_ST_REG;
        break;
    case OP_TR: /* Test registers */
        switch( reg ) {
        case 3:
        case 4:
        case 5:
            if( ( ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_486 )
               || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_686 ) )
                && ( ( AsmOpTable[temp].cpu & P_CPU_MASK ) >= P_486 ) ) {
                // TR3-TR5 are available on 486 only
                AsmErr( CANNOT_USE_TRN_TO_TRM_WITH_CURRENT_CPU_SETTING, 3, 5 );
                return( ERROR );
            }
            break;
        case 6:
        case 7:
            if( ( ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 )
               || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_686 ) )
                && ( ( AsmOpTable[temp].cpu & P_CPU_MASK ) >= P_386 ) ) {
                // TR6+TR7 are available on 386...586 only
                AsmErr( CANNOT_USE_TRN_TO_TRM_WITH_CURRENT_CPU_SETTING, 6, 7 );
                return( ERROR );
            }
            break;
        }
        /* fall through */
    case OP_CR:                 // Control registers
    case OP_DR:                 // Debug registers
        if( CodeInfo->token != T_MOV ) {
            AsmError( ONLY_MOV_CAN_USE_SPECIAL_REGISTER );
            return( ERROR );
        }
        break;
    }

#if AMD64_SUPPORT
    /* if it's a x86-64 register (SIL, R8W, R8D, RSI, ... */
    if ( (AsmOpTable[temp].cpu & P_CPU_MASK ) == P_64 )
        CodeInfo->prefix.rex |= 0x40;
#endif

    if( Opnd_Count == OPND1 ) {
        // the first operand
        // r/m is treated as a 'reg' field
        CodeInfo->rm_byte |= MOD_11;
#if AMD64_SUPPORT
        CodeInfo->prefix.rex |= (reg & 8 ) >> 3; /* set REX_B */
        reg &= BIT_012;
#endif
        // fill the r/m field
        CodeInfo->rm_byte |= reg;
    } else {
        // the second operand
        // XCHG can use short form if op1 is AX/EAX/RAX
        if( ( CodeInfo->token == T_XCHG ) && ( CodeInfo->opnd_type[OPND1] & OP_A ) &&
             ( 0 == (CodeInfo->opnd_type[OPND1] & OP_R8 ) ) ) {
#if AMD64_SUPPORT
            CodeInfo->prefix.rex |= (reg & 8 ) >> 3; /* set REX_B */
            reg &= BIT_012;
#endif
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & BIT_67 ) | reg;
        } else {
            // fill reg field with reg
#if AMD64_SUPPORT
            CodeInfo->prefix.rex |= (reg & 8 ) >> 1; /* set REX_R */
            reg &= BIT_012;
#endif
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & ~BIT_345 ) | ( reg << 3 );
        }
    }
    return( NOT_ERROR );
}


/* special handling for string instructions
 * the peculiarity is that these instructions ( optionally )
 * have memory operands, which aren't used for code generation
 * <opndx> contains the last operand.
 */

static void HandleStringInstructions( struct code_info *CodeInfo, const expr_list *opndx )
/****************************************************************************************/
{
    int opndidx = OPND1;

    switch( CodeInfo->token ) {
    case T_CMPSD:
        /* filter SSE2 opcode CMPSD */
        if ( CodeInfo->opnd_type[OPND1] & (OP_XMM | OP_MMX)) {
            /* v2.01: QWORD operand for CMPSD/MOVSD may have set REX_W! */
#if AMD64_SUPPORT
            CodeInfo->prefix.rex &= ~REX_W;
#endif
            return;
        }
        /* fall through */
    case T_CMPS:
    case T_CMPSB:
    case T_CMPSW:
         /* cmps allows prefix for the first operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY ) {
            if ( opndx->override != EMPTY ) {
                if ( CodeInfo->prefix.RegOverride == ASSUME_ES ) {
                    /* content of LastRegOverride is valid if
                     * CodeInfo->RegOverride is != EMPTY.
                     */
                    if ( LastRegOverride == ASSUME_DS )
                        CodeInfo->prefix.RegOverride = EMPTY;
                    else
                        CodeInfo->prefix.RegOverride = LastRegOverride;
                } else {
                    DebugMsg(("HandleStringInstructions: CMPS: CodeInfo->RegOverride=%X, opndx->override=%s\n", CodeInfo->prefix.RegOverride, AsmBuffer[opndx->override]->string_ptr ));
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                }
            } else if ( CodeInfo->prefix.RegOverride == ASSUME_DS ) {
                /* prefix for first operand? */
                CodeInfo->prefix.RegOverride = EMPTY;
            }
        }
        break;
    case T_MOVSD:
        /* filter SSE2 opcode MOVSD */
        if ( ( CodeInfo->opnd_type[OPND1] & (OP_XMM | OP_MMX) ) ||
            ( CodeInfo->opnd_type[OPND2] & (OP_XMM | OP_MMX) ) ) {
            /* v2.01: QWORD operand for CMPSD/MOVSD may have set REX_W! */
#if AMD64_SUPPORT
            CodeInfo->prefix.rex &= ~REX_W;
#endif
            return;
        }
        /* fall through */
    case T_MOVS:
    case T_MOVSB:
    case T_MOVSW:
        /* movs allows prefix for the second operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY )
            if ( opndx->override == EMPTY )
                AsmError( INVALID_INSTRUCTION_OPERANDS );
            else if ( CodeInfo->prefix.RegOverride == ASSUME_DS )
                CodeInfo->prefix.RegOverride = EMPTY;
        break;
    case T_OUTS:
    case T_OUTSB:
    case T_OUTSW:
    case T_OUTSD:
        /* v2.01: remove default DS prefix */
        if ( CodeInfo->prefix.RegOverride == ASSUME_DS )
            CodeInfo->prefix.RegOverride = EMPTY;
        opndidx = OPND2;
        break;
    case T_SCAS:
    case T_SCASB:
    case T_SCASW:
    case T_SCASD:
    case T_STOS:
    case T_STOSB:
    case T_STOSW:
    case T_STOSD:
    case T_INS:
    case T_INSB:
    case T_INSW:
    case T_INSD:
        /* INS, SCAS, STOS don't allow any segment prefix != ES
         for the memory operand.
         */
        if (CodeInfo->prefix.RegOverride != EMPTY)
            if (CodeInfo->prefix.RegOverride == ASSUME_ES)
                CodeInfo->prefix.RegOverride = EMPTY;
            else
                AsmError( INVALID_INSTRUCTION_OPERANDS );
    }

    /* size an string instruction based on it's operand's size */
    /* this is useful for the instruction variants without suffix only */
    if ( CodeInfo->opnd_type[opndidx] != OP_NONE ) {
        int op_size = OperandSize( CodeInfo->opnd_type[opndidx], CodeInfo );
        switch( op_size ) {
        case 1:
            CodeInfo->mem_type = MT_BYTE;
            CodeInfo->opcode &= NOT_W_BIT;
            if( CodeInfo->Ofssize )
                CodeInfo->prefix.opsiz = FALSE;
            break;
        case 2:
            CodeInfo->mem_type = MT_WORD;
            CodeInfo->opcode |= W_BIT;
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? TRUE : FALSE;
            break;
        case 4:
            CodeInfo->mem_type = MT_DWORD;
            CodeInfo->opcode |= W_BIT;
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? FALSE : TRUE;
            break;
#if AMD64_SUPPORT
        case 8:
            if ( CodeInfo->Ofssize == USE64 ) {
                CodeInfo->mem_type = MT_QWORD;
                CodeInfo->opcode |= W_BIT;
                CodeInfo->prefix.opsiz = FALSE;
                CodeInfo->prefix.rex = REX_W;
            }
            break;
#endif
        }
    }
    return;
}

static ret_code check_size( struct code_info *CodeInfo, const expr_list * opndx )
/*******************************************************************************/
/*
- use to make sure the size of first operand match the size of second operand;
- optimize MOV instruction;
- opndx contains last operand
*/
{
    OPNDTYPE    op1 = CodeInfo->opnd_type[OPND1];
    OPNDTYPE    op2 = CodeInfo->opnd_type[OPND2];
    ret_code    rc = NOT_ERROR;
    int         op1_size;
    int         op2_size;
    //int         op_size = 0;

    DebugMsg(("check_size enter\n"));
    switch( CodeInfo->token ) {
#if 0
    case T_PSLLW:
    case T_PSLLD:
    case T_PSLLQ:
    case T_PSRLW:
    case T_PSRLD:
    case T_PSRLQ:
    case T_PSRAW:
    case T_PSRAD:
        // check was wrong - instructions take a m64 OR an 8 bit immediate
        if( op2 & OP_I ) {
            op_size = OperandSize( op2, CodeInfo );
            if( op_size >= 2 ) {
                AsmError( OP2_TOO_BIG );
                rc = ERROR;
            }
        }
        break;
#endif
    case T_IN:
        if( op2 == OP_DX ) {
            switch( op1 ) {
            case OP_AX:
                break;
            case OP_AL:
                CodeInfo->opcode &= NOT_W_BIT;         // clear w-bit
            case OP_EAX:
                if( CodeInfo->Ofssize ) {
                    CodeInfo->prefix.opsiz = FALSE;
                }
                break;
            }
        }
        break;
    case T_OUT:
        if( op1 == OP_DX ) {
            switch( op2 ) {
            case OP_AX:
                break;
            case OP_AL:
                CodeInfo->opcode &= NOT_W_BIT;         // clear w-bit
            case OP_EAX:
                if( CodeInfo->Ofssize ) {
                    CodeInfo->prefix.opsiz = FALSE;
                }
            }
        }
        break;
    case T_LEA:
#if 0
        /* first op must be 16/32 register, but this condition is checked
         in CodeGen. operands 1 and 2 can be mixed:
         lea cx,[bp]
         lea cx,[ebp]
         lea ecx,[bp]
         lea ecx,[ebp]
         are all valid. However, Masm sometimes complains
         "cannot use 16-bit register with a 32-bit address"
         */
        switch( OperandSize( op1, CodeInfo ) ) {
        case 2:
        case 4:
            break;
        default:
            AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, OperandSize( op1, CodeInfo ), ModuleInfo.Ofssize ? 4 : 2);
            rc = ERROR;
        }
#endif
        break;
    case T_RCL:
    case T_RCR:
    case T_ROL:
    case T_ROR:
    case T_SAL:
    case T_SAR:
    case T_SHL:
    case T_SHR:
        /* v2.0: if second argument is a forward reference,
         * change type to "immediate 1"
         */
        if ( opndx->kind == EXPR_ADDR &&
            Parse_Pass == PASS_1 &&
            Opnd_Count == OPND2 &&
            opndx->indirect == FALSE &&
            opndx->sym &&
            opndx->sym->state == SYM_UNDEFINED ) {
            CodeInfo->opnd_type[OPND2] = OP_I8;
            CodeInfo->data[OPND2] = 1;
        }
        break;
    case T_LDS:
    case T_LES:
    case T_LFS:
    case T_LGS:
    case T_LSS:
        op1_size = OperandSize( op1, CodeInfo ) + 2; /* add 2 for the impl. segment register */
        op2_size = OperandSize( op2, CodeInfo );
        if (op2_size != 0 && op1_size != op2_size) {
            AsmError( INVALID_OPERAND_SIZE );
            return( ERROR );
        }
        break;
    case T_ENTER:
        // ENTER has to be OP_I16, OP_I8
        if( op1 == OP_I32 ) {
            //parse_phase_1 will treat 16-bit data as OP_I32 if CPU is 386
            if( CodeInfo->data[OPND1] > (signed long)USHRT_MAX ) {
                // if op1 is really 32-bit data, then error
                AsmError( INVALID_OPERAND_SIZE );
                rc = ERROR;
            }
        }
        // type cast op1 to OP_I16
        CodeInfo->opnd_type[OPND1] = OP_I16;
        // op2 have to be 8-bit data
        if( op2 >= OP_I16 ) {
            if( CodeInfo->data[OPND2] > UCHAR_MAX ) {
                AsmError( INVALID_OPERAND_SIZE );
                rc = ERROR;
            }
            CodeInfo->opnd_type[OPND2] = OP_I8;
        }
        break;
    case T_MOVSX:
    case T_MOVZX:
        CodeInfo->opcode &= NOT_W_BIT;
        op1_size = OperandSize( op1, CodeInfo );
        op2_size = OperandSize( op2, CodeInfo );
        DebugMsg(("check_size, MOVZX/MOVSX: op2_size=%u, opndx.memtype=%Xh, opndx.sym=%X\n", op2_size, opndx->mem_type, opndx->sym ));
        if ( op2_size == 0 && Parse_Pass == PASS_2 )
            if ( op1_size == 2 ) {
                AsmWarn( 2, SIZE_NOT_SPECIFIED_ASSUMING, "BYTE" );
            } else
                AsmErr( INSTRUCTION_OPERAND_MUST_HAVE_SIZE );
        switch( op1_size ) {
#if AMD64_SUPPORT
        case 8:
            //if ( CodeInfo->Ofssize == USE64 )
            //    break;
#endif
        case 4:
            if (op2_size < 2)
                ;
            else if (op2_size == 2)
                CodeInfo->opcode |= W_BIT;
            else {
                AsmError( OP2_TOO_BIG );
                rc = ERROR;
            }
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? FALSE : TRUE;
            break;
        case 2:
            if( op2_size >= 2 ) {
                AsmError( OP2_TOO_BIG );
                rc = ERROR;
            }
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? TRUE : FALSE;
            break;
        default:
            /* op1 must be r16/r32/r64 */
            AsmError( OP1_TOO_SMALL );
            rc = ERROR;
        }
        break;
    case T_LSL:                                 /* 19-sep-93 */
        op1_size = OperandSize( op1, CodeInfo );
        switch( op1_size ) {
        case 2:
            if( CodeInfo->Ofssize )
                CodeInfo->prefix.opsiz = TRUE;
            break;
        case 4:
            if( CodeInfo->Ofssize )
                CodeInfo->prefix.opsiz = FALSE;
            break;
        default:
            AsmError( INVALID_OPERAND_SIZE );
            return( ERROR );
        }
        op2_size = OperandSize( op2, CodeInfo );
        switch( op2_size ) {
        case 2:
        case 4:
            break;
        default:
            AsmError( INVALID_OPERAND_SIZE );
            rc = ERROR;
            break;
        }
        break;
    case T_CVTSD2SI:
    case T_CVTTSD2SI:
    case T_CVTSS2SI:
    case T_CVTTSS2SI:
    case T_MOVNTI:
        break;
#if SSE4SUPP
    case T_CRC32:
        /* v2.02: for CRC32, the second operand determines whether an
         * OPSIZE prefix byte is to be written.
         */
        op2_size = OperandSize( op2, CodeInfo );
        if ( op2_size < 2)
            CodeInfo->prefix.opsiz = FALSE;
        else if ( op2_size == 2 )
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? TRUE : FALSE;
        else
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? FALSE : TRUE;
        break;
#endif
    case T_MOVD:
#if 0
        op1_size = OperandSize( op1, CodeInfo );
        op2_size = OperandSize( op2, CodeInfo );
        if( ( op1_size != 0 ) && ( op1_size != 4 )
            || ( op2_size != 0 ) && ( op2_size != 4 ) ) {
            AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, op1_size, op2_size );
            rc = ERROR;
        }
#endif
        break;
    case T_MOV:
        /* OP_SR are segment registers */
        if( op1 & OP_SR ) {
            op2_size = OperandSize( op2, CodeInfo );
            if( ( op2_size == 2 ) || ( op2_size == 4 ) ) {
                return( NOT_ERROR );
            }
        } else if( op2 & OP_SR ) {
            op1_size = OperandSize( op1, CodeInfo );
            if( ( op1_size == 2 ) || ( op1_size == 4 ) ) {
                return( NOT_ERROR );
            }
        } else if( ( op1 & OP_M ) || ( op2 & OP_M ) ) {
            /*
             * one operand is a memory reference.
             * if address mode is indirect and the other operand is AX/EAX/RAX,
             * don't use the short format (opcodes A0-A3).
             * for 64bit, opcodes A0-A3 expect a moffset (full 64bit offset)
             */
            if ( CodeInfo->isdirect == FALSE ) {
                if( CodeInfo->opnd_type[OPND1] & OP_A ) {
                    // short form exists for direct addressing only, change OP_A to OP_R!
                    CodeInfo->opnd_type[OPND1] &= ~OP_A;
                    DebugMsg(("check_size: op1 changed to %X\n", CodeInfo->opnd_type[OPND1] ));
                } else if ( CodeInfo->opnd_type[OPND2] & OP_A ) {
                    // short form exists for direct addressing only, change OP_A to OP_R!
                    CodeInfo->opnd_type[OPND2] &= ~OP_A;
                    DebugMsg(("check_size: op2 changed to %X\n", CodeInfo->opnd_type[OPND2] ));
                }
            }
#if AMD64_SUPPORT
            else if ( CodeInfo->Ofssize == USE64 ) {
                if( CodeInfo->opnd_type[OPND1] & OP_A ) {
                    CodeInfo->opnd_type[OPND1] &= ~OP_A;
                } else if ( CodeInfo->opnd_type[OPND2] & OP_A ) {
                    CodeInfo->opnd_type[OPND2] &= ~OP_A;
                }
            }
#endif
        } else if( ( op1 & OP_SPEC_REG ) || ( op2 & OP_SPEC_REG ) ) {
            CodeInfo->prefix.opsiz = FALSE;
            //return( rc ); // v1.96: removed
        }
        /* fall through */
    default:
        // make sure the 2 opnds are of the same type
        op1_size = OperandSize( op1, CodeInfo );
        op2_size = OperandSize( op2, CodeInfo );
        DebugMsg(("check_size default: size1(%X)=%u, size2(%X)=%u\n", op1, op1_size, op2, op2_size));
        if( op1_size > op2_size ) {
            if( ( op2 >= OP_I8 ) && ( op2 <= OP_I32 ) ) {     /* immediate */
                op2_size = op1_size;    /* promote to larger size */
            }
        }
        if( ( op1_size == 1 ) && ( op2 == OP_I16 )
            && ( CodeInfo->data[OPND2] <= UCHAR_MAX ) ) {
            return( rc ); // OK cause no sign extension
        }
        if( ( op1_size == 2 ) && ( op2 == OP_I32 )
            && ( CodeInfo->data[OPND2] <= USHRT_MAX ) ) {
            return( rc ); // OK cause no sign extension
        }
        if( op1_size != op2_size ) {
            /* if one or more are !defined, set them appropriately */
            if( ( op1 | op2 ) & ( OP_MMX | OP_XMM ) ) {
            } else if( ( op1_size != 0 ) && ( op2_size != 0 ) ) {
                AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, op1_size, op2_size );
                rc = ERROR;
            }
            if( op1_size == 0 ) {
                if( ( op1 & OP_M_ANY ) && ( op2 & OP_I ) ) {
                    char *p = "WORD";
                    if( (unsigned long)CodeInfo->data[OPND2] > USHRT_MAX || op2_size == 4) {
                        CodeInfo->opcode |= W_BIT;
                        DebugMsg(("check_size: op1=%X op1_size=0, op2=%X, op2_size=%u CodeInfo->data[2]=%X\n", op1, op2, op2_size, CodeInfo->data[OPND2] ));
#if 1 /* added v1.95: in 16bit code, 'mov [di],8000h' should warn: assuming WORD */
                        if ( ModuleInfo.Ofssize == USE16 && op2_size > 2 && InRange(CodeInfo->data[OPND2], 2))
                            op2_size = 2;
#endif
                        if (op2_size <= 2 && CodeInfo->data[OPND2] > SHRT_MIN && ModuleInfo.Ofssize == USE16 ) {
                            CodeInfo->mem_type = MT_WORD;
                            CodeInfo->opnd_type[OPND2] = OP_I16;
                        } else {
                            CodeInfo->mem_type = MT_DWORD;
                            CodeInfo->opnd_type[OPND2] = OP_I32;
                            p = "DWORD";
                        }
                    } else if( (unsigned long)CodeInfo->data[OPND2] > UCHAR_MAX || op2_size == 2) {
                         CodeInfo->mem_type = MT_WORD;
                         CodeInfo->opcode |= W_BIT;
                         CodeInfo->opnd_type[OPND2] = OP_I16;
                    } else {
                         CodeInfo->mem_type = MT_BYTE;
                         CodeInfo->opnd_type[OPND2] = OP_I8;
                         p = "BYTE";
                    }
                    if( Parse_Pass == PASS_2 && opndx->explicit == FALSE) {
                        AsmWarn( 1, SIZE_NOT_SPECIFIED_ASSUMING, p );
                    }
                } else if( ( op1 & OP_M_ANY ) && ( op2 & ( OP_R | OP_SR ) ) ) {
                } else if( ( op1 & ( OP_MMX | OP_XMM ) ) && ( op2 & OP_I ) ) {
                    if( (unsigned long)CodeInfo->data[OPND2] > USHRT_MAX ) {
                         CodeInfo->opnd_type[OPND2] = OP_I32;
                    } else if( (unsigned long)CodeInfo->data[OPND2] > UCHAR_MAX ) {
                         CodeInfo->opnd_type[OPND2] = OP_I16;
                    } else {
                         CodeInfo->opnd_type[OPND2] = OP_I8;
                    }
                } else if( ( op1 | op2 ) & ( OP_MMX | OP_XMM ) ) {
                } else {
                    //AsmIntErr( 1 ); /* printf("internal error = %u", 1 ) */
                    switch( op2_size ) {
                    case 1:
                        CodeInfo->mem_type = MT_BYTE;
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, SIZE_NOT_SPECIFIED_ASSUMING, "BYTE" );
                        }
                        break;
                    case 2:
                        CodeInfo->mem_type = MT_WORD;
                        CodeInfo->opcode |= W_BIT;
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, SIZE_NOT_SPECIFIED_ASSUMING, "WORD" );
                        }
                        if( CodeInfo->Ofssize )
                            CodeInfo->prefix.opsiz = TRUE;
                        break;
                    case 4:
                        CodeInfo->mem_type = MT_DWORD;
                        CodeInfo->opcode |= W_BIT;
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, SIZE_NOT_SPECIFIED_ASSUMING, "DWORD" );
                        }
                        break;
                    }
                }
            }
        }
    }
    DebugMsg(("check_size exit [CodeInfo->mem_type=%Xh]\n", CodeInfo->mem_type));
    return( rc );
}

/*
 this is the parser entry. It scans through the tokens in AsmBuffer[]
 and fills a CodeInfo structure. It calls
 - for code labels: LabelCreate()
 - for instructions: match_phase_1(), the code generator
 - for directives: directive()
 - for data items: data_init()
 */

ret_code ParseItems( void )
/*************************/
{
    int                 i;
    struct code_info    Code_Info;
    struct code_info    *CodeInfo = &Code_Info;
    ret_code            temp;
    asm_sym             *sym;
    uint_32             oldofs;
#ifdef DEBUG_OUT
    char                *instr;
#endif
    expr_list           opndx;

    DebugMsg(("ParseItems enter, Token_Count=%u, queue_level=%u, ofs=%Xh\n",
              Token_Count, queue_level, GetCurrOffset() ));

    if( DefineProc == TRUE )
        if ( ModuleInfo.proc_prologue == NULL || *ModuleInfo.proc_prologue == NULLC )
            proc_check();

    // init CodeInfo
    //CodeInfo->prefix.SegOverride = NULL;
    CodeInfo->prefix.ins         = EMPTY;
    CodeInfo->prefix.RegOverride = EMPTY;
#if AMD64_SUPPORT
    CodeInfo->prefix.rex     = 0;
#endif
    CodeInfo->prefix.adrsiz  = FALSE;
    CodeInfo->prefix.opsiz   = FALSE;
    CodeInfo->token          = T_NULL;
    CodeInfo->mem_type       = MT_EMPTY;
    for( i = 0; i < 3; i++ ) {
        CodeInfo->opnd_type[i] = OP_NONE;
        CodeInfo->data[i] = 0;
        CodeInfo->InsFixup[i] = NULL;
    }
    CodeInfo->opcode         = 0;
    CodeInfo->rm_byte        = 0;
    CodeInfo->sib            = 0;            // assume ss is *1
    CodeInfo->Ofssize        = ModuleInfo.Ofssize;
    CodeInfo->flags          = 0;

    Opnd_Count = OPND1; /* to be moved to CodeInfo */
    Frame = EMPTY;
    SegOverride = NULL;
    line_listed = FALSE;
    list_pos_start = list_pos;

    i = 0;

    /* Does line start with a code label? */
    if ( AsmBuffer[0]->token == T_ID && AsmBuffer[1]->token == T_COLON ) {
        i = 2;
        DebugMsg(("ParseItems T_COLON, code label=%s\n", AsmBuffer[0]->string_ptr ));
        if ( SegAssumeTable[ASSUME_CS].error ) { /* CS assumed to ERROR? */
            AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
            return( ERROR );
        }
        if (( AsmBuffer[2]->token == T_COLON ) || ( CurrProc == NULL )) {
            if( LabelCreate( AsmBuffer[0]->string_ptr, MT_NEAR, NULL, FALSE ) == NULL ) {
                DebugMsg(("ParseItems, error creating global label, exit\n"));
                return( ERROR );
            }
            if (AsmBuffer[2]->token == T_COLON)
                i++; /* skip the second colon */
        } else {
            if( LabelCreate( AsmBuffer[0]->string_ptr, MT_NEAR, NULL, TRUE ) == NULL ) {
                DebugMsg(("ParseItems, error creating local label, exit\n"));
                return( ERROR );
            }
        }
        if ( AsmBuffer[i]->token == T_FINAL ) {
            if ( FileInfo.file[LST] ) {
                LstWrite( LSTTYPE_LABEL, 0, NULL );
            }
            return( NOT_ERROR );
        }
    }

    if ( FileInfo.file[LST] ) {
        oldofs = GetCurrOffset();
    }

    /* handle directives and (anonymous) data items */

    if ( AsmBuffer[i]->token != T_INSTRUCTION ) {
        /* a code label before a data item is only accepted in Masm5 compat mode */
        int label = -1;
        if ( AsmBuffer[i]->token == T_ID && ( i == 0 || ModuleInfo.m510 == TRUE ) ) {
            /* if next token is a directive, it can't be an anonymous data item.
             * this also filters the "xxx STRUCT" and "xxx ENDS" cases.
             * v2.0: take care of special case:
             * <label> <type> ...
             * where both <label> and <type> are userdefined types. This is valid
             * syntax inside a STRUCT!
             */
            if( AsmBuffer[i+1]->token != T_DIRECTIVE &&
               /* v2.01: the second item might be a predefined byte (T_RES_ID)! */
               ( AsmBuffer[i+1]->token != T_RES_ID || AsmBuffer[i+1]->rm_byte != RWT_TYPE ) &&
                ( sym = SymIsType( AsmBuffer[i]->string_ptr ) ) &&
                ( CurrStruct == NULL || ( SymIsType( AsmBuffer[i+1]->string_ptr ) == FALSE ) ) ) {
                DebugMsg(("ParseItems: anonymous data item >%s<\n", AsmBuffer[i]->string_ptr ));
                return( data_init( CodeInfo, label, i, (dir_node *)sym ) );
            }
            label = i;
            i++;
        }
        switch ( AsmBuffer[i]->token ) {
        case T_DIRECTIVE:
            DebugMsg(("ParseItems: T_DIRECTIVE >%s<\n", AsmBuffer[i]->string_ptr ));
            if ( AsmBuffer[i]->dirtype == DRT_DATADIR ) {
                return( data_init( CodeInfo, label, i, NULL ) );
            }
            /* label allowed for directive? */
            if ( AsmBuffer[i]->flags & DF_LABEL ) {
                if ( i && AsmBuffer[0]->token != T_ID ) {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
                    return( ERROR );
                }
            } else if ( i && AsmBuffer[i-1]->token != T_COLON ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->string_ptr );
                return( ERROR );
            }
            temp = directive( i, CodeInfo );
            /* v2.0: for generated code it's important that list file is 
             * written in ALL passes, to update file position! */
            //if ( ModuleInfo.list && line_listed == FALSE && Parse_Pass == PASS_1 )
            if ( ModuleInfo.list &&
                line_listed == FALSE &&
                ( Parse_Pass == PASS_1 || GeneratedCode ) )
                LstWriteSrcLine();
            return( temp );
        case T_RES_ID:
            DebugMsg(("ParseItems: T_RES_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if ( AsmBuffer[i]->rm_byte == RWT_TYPE )
                return( data_init( CodeInfo, label, i, NULL ) );
            break;
        case T_ID:
            DebugMsg(("ParseItems: T_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if( sym = SymIsType( AsmBuffer[i]->string_ptr ) ) {
                return( data_init( CodeInfo, label, i, (dir_node *)sym ) );
            }
            break;
        default:
            if ( AsmBuffer[i]->token == T_COLON ) {
                DebugMsg(("ParseItems: unexpected colon\n" ));
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
                return( ERROR );
            }
            break;
        } /* end switch (AsmBuffer[i]->token) */
        if ( label != -1 )
            i--;
        DebugMsg(("ParseItems: unexpected token=%u, i=%u, string=%s\n", AsmBuffer[i]->token, i, AsmBuffer[i]->string_ptr));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    DebugMsg(("ParseItems: %s\n", AsmBuffer[i]->string_ptr));

    // instruction prefix?
    // T_LOCK, T_REP, T_REPE, T_REPNE, T_REPNZ, T_REPZ
    if ( AsmBuffer[i]->value >= T_LOCK && AsmBuffer[i]->value <= T_REPZ ) {
        CodeInfo->prefix.ins = AsmBuffer[i]->value;
        i++;
        // prefix has to be followed by an instruction
        if( AsmBuffer[i]->token != T_INSTRUCTION ) {
            DebugMsg(("ParseItems: unexpected token after prefix, exit, error\n"));
            AsmError( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION );
            return( ERROR );
        }
        DebugMsg(("ParseItems: %s\n", AsmBuffer[i]->string_ptr));
    };

    if( CurrProc ) {
        switch( AsmBuffer[i]->value ) {
        case T_RET:
        case T_IRET:
        case T_IRETD: /* IRETW doesn't exist. Masm bug? */
#if AMD64_SUPPORT
        case T_IRETQ:
#endif
            if (in_epilogue == FALSE ) {
                if ( ModuleInfo.proc_epilogue == NULL ) {
                    /* if epilogue:none and proc is far,
                     just convert RET to RETF */
                    if ( CurrProc->sym.mem_type == MT_FAR &&
                         AsmBuffer[i]->value == T_RET )
                        AsmBuffer[i]->value = T_RETF;
                } else {
                    in_epilogue = TRUE;
                    temp = RetInstr( i, Token_Count );
                    in_epilogue = FALSE;
                    return( temp );
                }
            }
        }
    }

#ifdef DEBUG_OUT
    instr = AsmBuffer[i]->string_ptr;
#endif
    CodeInfo->token = AsmBuffer[i]->value;
    // get the instruction's start position in AsmOpTable
    CodeInfo->pcurr = &AsmOpTable[optable_idx[CodeInfo->token]];
    i++;

    if( CurrSeg == NULL ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }
    if( CurrSeg->e.seginfo->segtype == SEGTYPE_UNDEF ) {
        CurrSeg->e.seginfo->segtype = SEGTYPE_CODE;
    }
#if FASTPASS
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
        SaveState();
    }
#endif

    /* get the instruction's arguments */

    while ( AsmBuffer[i]->token != T_FINAL ) {

        DebugMsg(("ParseItems(%s,%u): calling EvalOperand, i=%u\n", instr, Opnd_Count, i));
        if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
            DebugMsg(("ParseItems(%s,%u): EvalOperand() failed\n", instr, Opnd_Count ));
            return( ERROR );
        }

        /* SHLD and SHRD currently need some special handling
         * if 3. operand is CL, it's treated as if 2 operands exist only.
         */
        if( Opnd_Count == OPND3 ) {
            if ( CodeInfo->token == T_SHLD ||
                 CodeInfo->token == T_SHRD ) {
                switch ( opndx.kind ) {
                case EXPR_CONST:
                    Opnd_Count--;
                    break;
                case EXPR_REG:
                    if ( opndx.indirect == FALSE &&
                         AsmBuffer[opndx.base_reg]->value == T_CL ) {
                        Opnd_Count--;
                        continue; /* skip processing this operand */
                    }
                default:
                    AsmError( INVALID_SHLD_SHRD_FORMAT );
                    return( ERROR );
                }
            } else if ( opndx.kind == EXPR_REG &&
                       opndx.indirect == FALSE ) {
                /* SSE4.1, XMM0 third argument? */
                if ( CodeInfo->pcurr->opnd_type_3rd == OP3_XMM0 &&
                    AsmBuffer[opndx.base_reg]->value == T_XMM0 ) {
                    CodeInfo->opnd_type[OPND3] = OP_XMM;
                    continue;
                }
            }
        }

        DebugMsg(("ParseItems(%s,%u): type/value/mem_type=%Xh/%Xh/%Xh\n", instr, Opnd_Count, opndx.kind, opndx.value, opndx.mem_type));
        switch( opndx.kind ) {
        case EXPR_ADDR:
            DebugMsg(("ParseItems(%s,%u): type ADDRESS\n", instr, Opnd_Count ));
            if ( process_address( CodeInfo, &opndx ) == ERROR )
                return( ERROR );
            break;
        case EXPR_CONST:
            /* optimization: skip <value> if it is 0 and instruction
             is RET[F]. <value> must not be external */
            if ( Opnd_Count == OPND1 && opndx.value == 0 &&
                 (CodeInfo->token == T_RET ||
                  CodeInfo->token == T_RETN ||
                  CodeInfo->token == T_RETF)) {
                if (opndx.sym == NULL || opndx.sym->state == SYM_INTERNAL) {
                    break;
                }
            }
            DebugMsg(("ParseItems(%s,%u): type CONST, opndx.memtype=%Xh\n", instr, Opnd_Count, opndx.mem_type));
            if ( process_const( CodeInfo, &opndx ) == ERROR )
                return( ERROR );
            break;
        case EXPR_REG:
            DebugMsg(("ParseItems(%s,%u): type REG\n", instr, Opnd_Count ));
            if( opndx.indirect ) { /* indirect operand ( "[EBX+...]" )? */
                if ( process_address( CodeInfo, &opndx ) == ERROR )
                    return( ERROR );
            } else {
                if ( process_register( CodeInfo, &opndx ) == ERROR )
                    return( ERROR );
            }
            break;
        case EXPR_EMPTY:
            if ( Opnd_Count == OPND2 && (AsmBuffer[i]->token == T_FLOAT ||
                 ((AsmBuffer[i]->token == '+' || AsmBuffer[i]->token == '-') &&
                  AsmBuffer[i+1]->token == T_FLOAT ) ) ) {
#if FPIMMEDIATE
                if ( Options.strict_masm_compat == FALSE ) {
                    double double_value;
                    float  float_value;
                    int index = i;
                    if (AsmBuffer[i]->token != T_FLOAT )
                        index++;
                    double_value = strtod( AsmBuffer[index]->string_ptr, NULL );
                    if ( AsmBuffer[i]->token == '-' )
                        double_value *= -1;
                    i = index + 1;
                    float_value = double_value;
                    opndx.kind = EXPR_CONST;
                    opndx.fvalue = float_value;
                    process_const( CodeInfo, &opndx );
                    break;
                }
#endif
                /* Masm message is: real or BCD number not allowed */
                AsmError( FLOAT_OPERAND );
                return( ERROR );
            }
            /* fall through */
        default:
            DebugMsg(("ParseItems(%s,%u): unexpected operand kind=%d, error, exit\n", instr, Opnd_Count, opndx.kind ));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if( Opnd_Count == OPND3 ) {
                AsmError( TOO_MANY_COMMAS );
                return( ERROR );
            }
            Opnd_Count++;
            Frame = EMPTY;
            SegOverride = NULL; /* segreg prefix is stored in RegOverride */
        }
    } /* end while */

    /* for FAR calls/jmps there was a non-compatible solution implemented
     * in WASM which required two additional opcodes, CALLF and JMPF.
     * this has been removed for JWasm, but there's now the need to
     * skip the "near" entries in AsmOpTable for CALL/JMP.
     */
    if ( CodeInfo->isfar ) {
        if ( CodeInfo->token == T_CALL || CodeInfo->token == T_JMP ) {
            do {
                CodeInfo->pcurr++;
            } while ( CodeInfo->pcurr->first == FALSE );
        }
    }

    /* special handling for string instructions */

    if ( CodeInfo->pcurr->allowed_prefix == AP_REP ||
         CodeInfo->pcurr->allowed_prefix == AP_REPxx ) {
        HandleStringInstructions( CodeInfo, &opndx );
    } else {
        if( Opnd_Count > OPND1 ) {
            if( check_size( CodeInfo, &opndx ) == ERROR ) {
                DebugMsg(("ParseItems(%s): check_size() failed, exit\n", instr ));
                return( ERROR );
            }
            /* v1.96: check if a third argument is ok */
            if ( Opnd_Count == OPND3 ) {
                do {
                    if ( CodeInfo->pcurr->opnd_type_3rd != OP3_NONE )
                        break;
                    CodeInfo->pcurr++;
                    if ( CodeInfo->pcurr->first == TRUE ) {
                        for ( ; AsmBuffer[i]->token != T_COMMA; i-- );
                        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->pos );
                        return( ERROR );
                    }
                } while ( 1 );
            }
        }
#if AMD64_SUPPORT
        if ( CodeInfo->Ofssize == USE64 ) {
            //if ( CodeInfo->x86hi_used && ( CodeInfo->x64lo_used || CodeInfo->prefix.rex & 7 ))
            if ( CodeInfo->x86hi_used && CodeInfo->prefix.rex )
                AsmError(INVALID_USAGE_OF_AHBHCHDH );
            if ( CodeInfo->token == T_PUSH || CodeInfo->token == T_POP ) {
                //if ( ( OperandSize( CodeInfo->opnd_type[OPND1], CodeInfo ) == 4 ) ||
                if ( ( CodeInfo->opnd_type[OPND1] & ( OP_R32 | OP_SR86 ) ) ||
                    ( CodeInfo->opnd_type[OPND1] == OP_M32 ) ) {
                    DebugMsg(("ParseItems: PUSH/POP operand with size 4 or CS/DS/ES/SS\n"));
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                    return( ERROR );
                }
                if ( CodeInfo->opnd_type[OPND1] & OP_R64 )
                    CodeInfo->prefix.rex &= 0x7;
            } else if ( CodeInfo->token == T_CALL || CodeInfo->token == T_JMP ) {
                /* v2.02: previously rex-prefix was cleared entirely,
                 * but bits 0-2 are needed to make "call rax" and "call r8"
                 * distinguishable!
                 */
                //CodeInfo->prefix.rex = 0;
                CodeInfo->prefix.rex &= 0x7;
            } else if ( CodeInfo->token == T_MOV ) {
                if ( CodeInfo->opnd_type[OPND1] & OP_SPEC_REG || CodeInfo->opnd_type[OPND2] & OP_SPEC_REG )
                    CodeInfo->prefix.rex &= 0x7;
            }
        }
#endif
    }

    /* now call the code generator */

    if ( FileInfo.file[LST] ) {
        temp = match_phase_1( CodeInfo );
        LstWrite( LSTTYPE_LIDATA, oldofs, NULL );
        return( temp );
    }
    return( match_phase_1( CodeInfo ) );
}

#if RENAMEKEY

struct rename_node {
    struct rename_node *next;
    const char *name;
    uint_16 token;
    uint_8 length;
};

void RenameKeyword( uint token, const char *newname, uint_8 length )
/******************************************************************/
{
    struct rename_node *rn;

    RemoveResWord( &AsmResWord[token] );
    /* if it is the first rename action for this keyword,
     * save the original name
     */
    if ( AsmResWord[token].name >= resw_strings &&
        AsmResWord[token].name < ( resw_strings + sizeof( resw_strings ) ) ) {
        rn = AsmAlloc( sizeof( struct rename_node ) );
        rn->next = NULL;
        rn->name = AsmResWord[token].name;
        rn->token = token;
        rn->length = AsmResWord[token].len;
        if ( renamed_keys.head == NULL ) {
            renamed_keys.head = renamed_keys.tail = rn;
        } else {
            ((struct rename_node *)renamed_keys.tail)->next = rn;
            renamed_keys.tail = rn;
        }
    } else {
        AsmFree( (void *)AsmResWord[token].name );
    }
    AsmResWord[token].name = AsmAlloc( length );
    memcpy( (void *)AsmResWord[token].name, newname, length );
    AsmResWord[token].len = length;
    AddResWord( &AsmResWord[token] );
}

#endif

#if AMD64_SUPPORT

static const enum asm_token patchtab64[] = {
    T_SPL,             /* add x64 register part of special.h */
    T_DOT_ALLOCSTACK,  /* add x64 directive part of special.h */
    T_JRCXZ,           /* branch instructions must be grouped together */
    T_CDQE,            /* add x64 part of instruct.h */
    T_NULL };

static const enum asm_token patchtab32[] = {
    T_DOT_SAFESEH,  /* directives invalid for IA32+             */
    T_AAA,          /* instructions invalid for IA32+           */
    T_JCXZ,         /* 1. branch instructions invalid for IA32+ */
    T_LOOPW,        /* 2. branch instructions invalid for IA32+ */
    T_NULL };

struct replace_ins {
    const enum asm_token tok;
    short idx32;
    short idx64;
};

static const struct replace_ins patchtabr[] = {
    { T_LGDT, T_LGDT_I, T_LGDT_I64 },
    { T_LIDT, T_LIDT_I, T_LIDT_I64 },
    { T_CALL, T_CALL_I, T_CALL_I64 },
    { T_JMP,  T_JMP_I,  T_JMP_I64  },
    { T_NULL}
};

/* depending on 64bit on or off, some instructions must be added,
 * some removed. Currently this is a bit hackish.
 */
void Set64Bit( bool newmode )
/***************************/
{
    struct ReservedWord *resw;
    const enum asm_token *ppt;
    const struct replace_ins  *repl;

    if ( newmode != b64bit ) {
        DebugMsg(("Set64Bit(%u): mode is to change\n", newmode ));
        if ( newmode != FALSE ) {
            optable_idx[T_INC]++;   /* skip the one-byte register INC */
            optable_idx[T_DEC]++;   /* skip the one-byte register DEC */
            /* change SYSCALL to SYSCALL_ language in long mode.
             * one cannot just change the name, since the hash value
             * will differ!
             */
            RemoveResWord( &AsmResWord[T_SYSCALL] );
            syscallname = AsmResWord[T_SYSCALL].name; /* save the "true" name */
            AsmResWord[T_SYSCALL].name = "syscall_";
            AsmResWord[T_SYSCALL].len++;
            AddResWord( &AsmResWord[T_SYSCALL] );
            for (ppt = patchtab64; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_X64; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        AddResWord( resw );
            for (ppt = patchtab32; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_IA32; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        RemoveResWord( resw );
            for (repl = patchtabr; repl->tok != T_NULL; repl++ ) {
                optable_idx[repl->tok] = repl->idx64;
            }
        } else  {
            optable_idx[T_INC]--;   /* restore the one-byte register INC */
            optable_idx[T_DEC]--;   /* restore the one-byte register DEC */
            for (ppt = patchtab64; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_X64; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        RemoveResWord( resw );
            for (ppt = patchtab32; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_IA32; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        AddResWord( resw );
            for (repl = patchtabr; repl->tok != T_NULL; repl++ ) {
                optable_idx[repl->tok] = repl->idx32;
            }
            /* change calling convention syscall_ back to syscall */
            RemoveResWord( &AsmResWord[T_SYSCALL] );
            AsmResWord[T_SYSCALL].name = syscallname; /* restore "true" name */
            AsmResWord[T_SYSCALL].len--;
            AddResWord( &AsmResWord[T_SYSCALL] );
        }
        b64bit = newmode;
    }
}
#endif

void DisableKeyword( uint token )
/*******************************/
{
    if ( !( AsmResWord[token].flags & RWF_DISABLED ) ) {
        RemoveResWord( &AsmResWord[token] );
        AsmResWord[token].next = EMPTY;
        AsmResWord[token].flags |= RWF_DISABLED;
        if ( RemovedFirst == EMPTY )
            RemovedFirst = RemovedTail = token;
        else {
            AsmResWord[RemovedTail].next = token;
            RemovedTail = token;
        }
    }
}

/* check if a keyword is in the list of disabled words.
 */

uint IsKeywordDisabled( const char *name, int len )
/*************************************************/
{
    uint  resw;
    for ( resw = RemovedFirst; resw != EMPTY; resw = AsmResWord[resw].next )
        if( AsmResWord[resw].name[ len ] == NULLC && _memicmp( name, AsmResWord[resw].name, len ) == 0 )
            return( resw );
    return( EMPTY );
}

/* ParseInit() is called once per module */

void InitInstHashTable( void );

ret_code ParseInit( void )
/************************/
{
    struct ReservedWord *curr;
    struct ReservedWord *last;
#if RENAMEKEY
    struct rename_node  *rencurr;
#endif

    DebugMsg(("ParseInit() enter\n"));

    /* initialize the AsmBuffer[] table */
    InitTokenBuffer();

    if( fInit == FALSE ) {  // if not initialized
        const char *p = resw_strings;
        /* if first call, initialize hash table (IA32 mode) */
        fInit = TRUE;
        InitInstHashTable();
        for( curr = &AsmResWord[0], last = &AsmResWord[T_NULL]; curr < last; curr++ ) {
            curr->name = p;
            p += curr->len;
#if AMD64_SUPPORT /* don't add the words specific to x64 */
            if ( !(curr->flags & RWF_X64 ) )
#endif
                AddResWord( curr );
        }
    } else {
        short next;
        short i;
        /* reenter disabled keywords */
        for( i = RemovedFirst; i != EMPTY; i = next ) {
            next = AsmResWord[i].next;
            AsmResWord[i].flags &= ~RWF_DISABLED;
#if AMD64_SUPPORT /* don't add the words specific to x64 */
            if ( !(AsmResWord[i].flags & RWF_X64 ) )
#endif
                AddResWord( &AsmResWord[i] );
        }
        RemovedFirst = RemovedTail = EMPTY;
#if RENAMEKEY
        /* restore renamed keywords */
        for ( rencurr = renamed_keys.head; rencurr; ) {
            struct rename_node *tmp = rencurr->next;
            RenameKeyword( rencurr->token, rencurr->name, rencurr->length );
            AsmFree( (void *)rencurr->name );
            AsmFree( rencurr );
            rencurr = tmp;
        }
        renamed_keys.head = NULL;
#endif
    }
    DebugMsg(("ParseInit() exit\n"));
    return( NOT_ERROR );
}
