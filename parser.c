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
#include "directiv.h"
#include "insthash.h"
#include "codegen.h"
#include "equate.h"
#include "fixup.h"
#include "expreval.h"
#include "labels.h"
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
#include "omf.h"
#include "omfspec.h"

#define ONEXMM 1 /* 1=use ONE .xmm directive (Masm compatible) */

#define SET_ADRSIZ( s, x ) ( s->prefix.adrsiz = (( x ) ^ ( s->Ofssize )) ? TRUE : FALSE )
#define IS_ADDR32( s )  ( s->Ofssize ? ( s->prefix.adrsiz == FALSE ) : ( s->prefix.adrsiz == TRUE ))

#define InWordRange( val ) ( (val > 65535 || val < -65535) ? FALSE : TRUE )

/* the parser tables are now generated:
 * 1. SpecialTable: contains info for reserved words which are
 *    NOT instructions. One row each.
 * 2. InstrTable: contains info for instructions.
 *    instructions may need multiple rows!
 * 3. optable_idx: array of indices for InstrTable.
 * 4. resw_strings: strings of reserved words. No terminating x'00'!
 * 5. AsmResWord: array of reserved words (name, name length, flags).
 *
 * Each reserved word has a "token" value assigned, which is a short integer.
 * This integer can be used as index for:
 * - SpecialTable
 * - optable_idx ( needs adjustment, better use macro IndexFromToken() )
 * - AsmResWord
 */

/* create SpecialTable. */

const struct asm_special SpecialTable[] = {
#define res(tok, string, len, type, value, value8, flags, cpu, sflags ) \
    { value, sflags, cpu, value8, type },
#include "special.h"
#undef res
};

/* create InstrTable. */

const struct asm_ins InstrTable[] = {
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 0, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#include "instruct.h"
#include "instr64.h"
ins (NULL,0,0,0,0,0,0,0,0,0,0,0,0) /* T_NULL entry */
#undef insm
#undef insn
#undef insx
#undef ins
};

/* define indices for InstrTable */

enum res_idx {
#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I,
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) T_ ## tok ## _I,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix,
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix,
#include "instruct.h"
#undef insm
#undef insn
#undef ins

#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I64,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#include "instr64.h"
#undef insm
#undef insn
#undef insx
#undef ins
T_NULL_I
};

/* create optable_idx, the index array for InstrTable.
 * This is needed because instructions often need more than
 * one entry in InstrTable.
 */

short optable_idx[] = {

#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I,
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) T_ ## tok ## _I,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#include "instruct.h"
#undef insm
#undef insn
#undef ins

#define  ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I64,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix ## _I64,
#include "instr64.h"
#undef insm
#undef insn
#undef insx
#undef ins
T_NULL_I
};

/* create the strings for all reserved words */

#ifdef __I86__ /* JWASMR.EXE: make strings public ( for insthash.c) */
const char resw_strings[] = {
#else
static const char resw_strings[] = {
#endif
#if AMD64_SUPPORT
#define res(tok, string, len, type, value, value8, flags, cpu, sflags) \
 # string
#else
#define res(tok, string, len, type, value, value8, flags, cpu, sflags) \
 # string
#endif
#include "special.h"
#undef res

#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
 # string
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
 # string
#include "instruct.h"
};
#undef insx
#undef insm
#undef insn
#undef ins

/* create the 'reserved words' table (AsmResWord).
 * this table's entries will be used to create the instruction hash table.
 */
struct ReservedWord AsmResWord[] = {
#define res(tok, string, len, type, value, value8, flags, cpu, sflags) \
    { 0, len, RWF_SPECIAL | flags, NULL },
#include "special.h"
#undef res

#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { 0, len, 0, NULL },
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flags) \
    { 0, len, flags, NULL },
#include "instruct.h"
#undef insx
#undef insm
#undef insn
#undef ins
    { 0, 0, 0, NULL } /* dummy entry for T_NULL */
};

/* parsing of branch instructions with imm operand is found in branch.c */
extern ret_code         process_branch( struct code_info *, const expr_list * );

extern bool             DefineProc;
extern struct asm_sym   symPC; /* '$' symbol */
extern bool             in_epilogue;
extern int_64           maxintvalues[];
extern int_64           minintvalues[];

/* v1.96: CodeInfo has become a stack variable.
 * it's more or less the central object for the parser and code generator.
 * static struct code_info Code_Info;
 * static struct code_info *CodeInfo = &Code_Info;
 */
unsigned                Opnd_Count;
#if AMD64_SUPPORT
static bool             b64bit = FALSE;
static const char       *syscallname;   /* "true" syscall name stored here */
#endif
int_8                   Frame;          /* Frame of current fixup */
uint_16                 Frame_Datum;    /* Frame datum of current fixup */
struct asm_sym          *SegOverride;
static enum assume_segreg  LastRegOverride;/* needed for CMPS */
static bool             fInit = FALSE;

/* global queue of "disabled" reserved words.
 * just indices of AsmResWord[] are used.
 */
static short RemovedFirst = EMPTY;
static short RemovedTail  = EMPTY;
#if RENAMEKEY
static qdesc renamed_keys = { NULL, NULL };
#endif
static int tbaseptr[] = { T_BP, T_EBP
#if AMD64_SUPPORT
, T_RBP
#endif
};

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
        case T_SI: return( RM_BX_SI );
        case T_DI: return( RM_BX_DI );
        }
        break;
    case T_BP:
        switch( reg2 ) {
        case T_SI: return( RM_BP_SI );
        case T_DI: return( RM_BP_DI );
        }
        break;
    default:
        AsmError( MULTIPLE_INDEX_REGISTERS_NOT_ALLOWED );
        return( ERROR );
    }
    AsmError( MULTIPLE_BASE_REGISTERS_NOT_ALLOWED );
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
    dir_node *grp;

    if( sym ) {
        switch ( sym->state ) {
        case SYM_INTERNAL:
        case SYM_EXTERNAL:
            if( sym->segment != NULL ) {
                if( grp = (dir_node *)GetGroup( sym ) ) {
                    Frame = FRAME_GRP;
                    Frame_Datum = grp->e.grpinfo->grp_idx;
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
            Frame_Datum = ((dir_node *)sym)->e.grpinfo->grp_idx;
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
                DebugMsg1(("check_assume: no segment register available to access label %s\n", sym->name ));
                AsmErr( CANNOT_ACCESS_LABEL_THROUGH_SEGMENT_REGISTERS, sym->name );
            } else
                CodeInfo->prefix.RegOverride = default_reg;
        } else {
            DebugMsg1(("check_assume: no segment register available to access seg-label %s\n", SegOverride->name ));
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
    //if ( InstrTable[optable_idx[CodeInfo->token]].allowed_prefix == AP_REP ||
    //     InstrTable[optable_idx[CodeInfo->token]].allowed_prefix == AP_REPxx )
    if ( CodeInfo->pcurr->allowed_prefix == AP_REP ||
         CodeInfo->pcurr->allowed_prefix == AP_REPxx )
        return;

    if( CodeInfo->token == T_LEA ) {
        CodeInfo->prefix.RegOverride = EMPTY; /* skip segment override */
        SetFixupFrame( sym );
        return;
    }

    switch( seg_reg ) {
    case T_SS: /* doesn't happen */
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
        return( SizeFromMemtype( CodeInfo->mem_type, CodeInfo->Ofssize, NULL ) );
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
    } else if( opnd & OP_SPECREG ) {
#if AMD64_SUPPORT
        return( ( CodeInfo->Ofssize == USE64 ) ? 8 : 4 );
#else
        return( 4 );
#endif
    }
    DebugMsg1(("OperandSize: unhandled operand type %Xh!!!\n", opnd ));
    return( 0 );
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
 * out: CodeInfo->rm_byte, CodeInfo->sib
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

    DebugMsg1(("mem2code(scale=%u, index=%d, base=%d, sym=%X) enter\n", 1 << (ss >> 6), index, base, sym));

    /* clear mod */
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
        /* direct memory.
         * clear the rightmost 3 bits
         */
        CodeInfo->isdirect = TRUE;
        mod_field = MOD_00;

        /* default is DS:[], DS: segment override is not needed */
        seg_override( CodeInfo, T_DS, sym, TRUE );

        //if( !IS_ADDR32( CodeInfo ) ) {
        if( (CodeInfo->Ofssize == USE16 && CodeInfo->prefix.adrsiz == 0 ) ||
            (CodeInfo->Ofssize == USE32 && CodeInfo->prefix.adrsiz == 1 )) {
            if( !InWordRange( CodeInfo->data[Opnd_Count] ) ) {
                /* expect 16-bit but got 32-bit address */
                AsmError( MAGNITUDE_OF_OFFSET_EXCEEDS_16BIT );
                return( ERROR );
            }
            rm_field = RM_D16; /* D16=110b */
        } else {
            rm_field = RM_D32; /* D32=101b */
#if AMD64_SUPPORT
            /* v2.03: the non-RIP encoding for 64bit uses a redundant SIB
             * mode (base=none, index=none) */
            if ( CodeInfo->Ofssize == USE64 &&
                CodeInfo->prefix.RegOverride != EMPTY &&
                SegOverride != &ModuleInfo.flat_grp->sym ) {
                DebugMsg1(( "mem2code: 64-bit non-RIP direct addressing: SegOverride=%X, flat=%X\n", SegOverride, ModuleInfo.flat_grp ));
                rm_field = RM_SIB;
                CodeInfo->sib = 0x25; /* IIIBBB, base=101b, index=100b */
            }
#endif
        }
        DebugMsg1(("mem2code, direct, CodeInfo->prefix.adrsiz=%u\n", CodeInfo->prefix.adrsiz ));
    } else if( ( index == EMPTY ) && ( base != EMPTY ) ) {
        /* for SI, DI and BX: default is DS:[],
         * DS: segment override is not needed
         * for BP: default is SS:[], SS: segment override is not needed
         */
        switch( base ) {
        case T_SI:
            rm_field = RM_SI;
            break;
        case T_DI:
            rm_field = RM_DI;
            break;
        case T_BP:
            rm_field = RM_BP;
            if( mod_field == MOD_00 ) {
                mod_field = MOD_01;
            }
            break;
        case T_BX:
            rm_field = RM_BX;
            break;
        default: /* for 386 and up */
            base_reg = GetRegNo( base );
#if AMD64_SUPPORT
            bit3_base = base_reg >> 3;
            base_reg &= BIT_012;
#endif
            rm_field = base_reg;
            DebugMsg1(("mem2code: base_reg is %u\n", base_reg ));
            if ( base_reg == 4 ) {
                /* 4 is RSP/ESP or R12/R12D, which must use SIB encoding.
                 * SSIIIBBB, ss = 00, index = 100b ( no index ), base = 100b ( ESP ) */
                CodeInfo->sib = 0x24;
            } else if ( base_reg == 5 && mod_field == MOD_00 ) {
                /* 5 is RBP/EBP or R13/R13D. Needs displacement */
                mod_field = MOD_01;
            }
#if AMD64_SUPPORT
            /* v2.02 */
            //rex = ( bit3_base << 2 ); /* set REX_R */
            rex = bit3_base; /* set REX_R */
#endif
        }
#if AMD64_SUPPORT
        DebugMsg1(("mem2code, indirect with base, mod_field=%X, rm_field=%X, rex=%X\n", mod_field, rm_field, rex ));
#else
        DebugMsg1(("mem2code, indirect with base, rm_field=%X\n", rm_field ));
#endif
        seg_override( CodeInfo, base, sym, FALSE );
    } else if( ( index != EMPTY ) && ( base == EMPTY ) ) {
        idx_reg = GetRegNo( index );
#if AMD64_SUPPORT
        bit3_idx = idx_reg >> 3;
        idx_reg &= BIT_012;
#endif
        /* mod field is 00 */
        mod_field = MOD_00;
        /* s-i-b is present ( r/m = 100b ) */
        rm_field = RM_SIB;
        /* scale factor, index, base ( 0x05 => no base reg ) */
        CodeInfo->sib = ( ss | ( idx_reg << 3 ) | 0x05 );
#if AMD64_SUPPORT
        rex = (bit3_idx << 1); /* set REX_X */
#endif
        /* default is DS:[], DS: segment override is not needed */
        seg_override( CodeInfo, T_DS, sym, FALSE );
    } else {
        /* base != EMPTY && index != EMPTY */
        base_reg = GetRegNo( base );
        idx_reg  = GetRegNo( index );
#if AMD64_SUPPORT
        bit3_base = base_reg >> 3;
        bit3_idx  = idx_reg  >> 3;
        base_reg &= BIT_012;
        idx_reg  &= BIT_012;
#endif
        if ( ( GetSflagsSp( base ) & GetSflagsSp( index ) & SFR_SIZMSK ) == 0 ) {
            AsmError( CANNOT_MIX_16_AND_32_BIT_REGISTERS );
            return( ERROR );
        }
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
            if( base_reg == 5 ) { /* v2.03: EBP/RBP/R13/R13D? */
                if( mod_field == MOD_00 ) {
                    mod_field = MOD_01;
                }
            }
            /* s-i-b is present ( r/m = 100b ) */
            rm_field |= RM_SIB;
            CodeInfo->sib = ( ss | idx_reg << 3 | base_reg );
#if AMD64_SUPPORT
            rex = (bit3_idx << 1) + (bit3_base); /* set REX_X + REX_B */
#endif
            seg_override( CodeInfo, base, sym, FALSE );
        } /* end switch(index) */
#if AMD64_SUPPORT
        DebugMsg1(("mem2code, indirect, base+index: mod_field=%X, rm_field=%X, rex=%X\n", mod_field, rm_field, rex ));
#else
        DebugMsg1(("mem2code, indirect, base+index: rm_field=%X\n", rm_field ));
#endif
    }
    if( Opnd_Count == OPND2 ) {
        /* shift the register field to left by 3 bit */
        CodeInfo->rm_byte = mod_field | ( rm_field << 3 ) | ( CodeInfo->rm_byte & BIT_012 );
#if AMD64_SUPPORT
        /* v2.02: exchange B and R, keep X */
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
                DebugMsg1(("segm_override: assume error, reg=%u\n", temp ));
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

    DebugMsg1(("idata_nofixup(kind=%u mem_type=%Xh value=%I64X) enter [CodeInfo->mem_type=%Xh]\n", opndx->kind, opndx->mem_type, opndx->value64, CodeInfo->mem_type));

    /* jmp/call/jxx/loop/jcxz/jecxz? */
    if( IS_ANY_BRANCH( CodeInfo->token ) ) { 
        return( process_branch( CodeInfo, opndx ) );
    }
    value = opndx->value;
    CodeInfo->data[Opnd_Count] = value;

#if AMD64_SUPPORT
    /* 64bit immediates are restricted to MOV <reg>,<imm64>
     */
    if ( opndx->hlvalue != 0 ) { /* magnitude > 64 bits? */
        DebugMsg1(("idata_nofixup: error, hlvalue=%I64X\n", opndx->hlvalue ));
        AsmError( CONSTANT_VALUE_TOO_LARGE );
        return( ERROR );
    }
    /* v2.03: handle QWORD type coercion here as well!
     * This change also reveals an old problem in the expression evaluator:
     * the mem_type field is set whenever a (simple) type token is found.
     * It should be set ONLY when the type is used in conjuction with the
     * PTR operator!
     * current workaround: query the 'explicit' flag.
     */
    //if ( opndx->value64 <= minintvalues[0] || opndx->value64 > maxintvalues[0] ) {
    /* use long format of MOV for 64-bit if value won't fit in a signed DWORD */
    if ( CodeInfo->Ofssize == USE64 &&
        CodeInfo->token == T_MOV &&
        Opnd_Count == OPND2 &&
        ( CodeInfo->opnd_type[OPND1] & OP_R64 ) &&
        ( opndx->value64 > LONG_MAX || opndx->value64 < LONG_MIN ||
         (opndx->explicit && ( opndx->mem_type == MT_QWORD || opndx->mem_type == MT_SQWORD) ) ) ) {
        CodeInfo->iswide = TRUE;
        CodeInfo->opnd_type[Opnd_Count] = OP_I64;
        CodeInfo->data[Opnd_Count+1] = opndx->hvalue;
        return( NOT_ERROR );
    }
    if ( opndx->value64 <= minintvalues[0] || opndx->value64 > maxintvalues[0] ) {
        DebugMsg1(("idata_nofixup: error, hvalue=%Xh\n", opndx->hvalue ));
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
                size = SizeFromMemtype( opndx->mem_type,
                                       opndx->Ofssize,
                                       opndx->type );
                if ( size == 1 ) {
                    op_type = OP_I8;
                } else if( size == 2 ) {
                    op_type = OP_I16;
                    SET_OPSIZ_16( CodeInfo );
                } else if( size == 4 ) { /* size 4 is max also for x86-64! */
                    op_type = OP_I32;
                    SET_OPSIZ_32( CodeInfo );
                } else {
                    DebugMsg1(("idata_nofixup: invalid operand, size=%u\n", size ));
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
            if ( opndx->explicit ) {
                size = SizeFromMemtype(opndx->mem_type,
                                       opndx->Ofssize,
                                       opndx->type );
                if ( size == 1 )
                    op_type = OP_I8;
                else if ( size == 2 )
                    op_type = OP_I16;
                else if ( size == 4 )
                    op_type = OP_I32;
                else {
                    DebugMsg1(("idata_nofixup: invalid operand, size=%u\n", size ));
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
            /* v2.04: range FFFF0000-FFFF7FFF is also acceptable for 16-bit */
            //else if ( value <= USHRT_MAX && value >= SHRT_MIN )
            /* v2.04b: JWASMR needs a 1L */
            //else if( value <= USHRT_MAX && value >= - (USHRT_MAX+1) )
            else if( value <= USHRT_MAX && value >= - (USHRT_MAX+1L) )
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
            DebugMsg1(("idata_nofixup: BYTE, out of range, value=%" FX32 "h\n", value ));
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
            DebugMsg1(("idata_nofixup: WORD, out of range, value=%" FX32 "h\n", value ));
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        }
        if( (int_8)value != value || opndx->mem_type == MT_WORD || opndx->mem_type == MT_SWORD )
            op_type = OP_I16;
        else
            op_type = OP_I8;
        CodeInfo->iswide = 1;
        break;
    case MT_REAL4:
    case MT_DWORD:
    case MT_SDWORD:
        if( (int_8)value != value || opndx->mem_type == MT_DWORD || opndx->mem_type == MT_SDWORD )
            op_type = OP_I32;
        else
            op_type = OP_I8;
        CodeInfo->iswide = 1;
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
        CodeInfo->iswide = 1;
        break;
    //case MT_FAR:
    //    DebugMsg1(("idata_nofixup, MT_FAR branch\n" ));
    //case MT_NEAR:
    //    DebugMsg1(("idata_nofixup, MT_NEAR branch\n" ));
    default:
        AsmErr( INVALID_INSTRUCTION_OPERANDS );
        return( ERROR );
    }
    CodeInfo->opnd_type[Opnd_Count] = op_type;
    DebugMsg1(("idata_nofixup exit, op_type=%" FX32 "\n", op_type ));
    return( NOT_ERROR );
}

/* create immediate data with fixup (offsets) */

static ret_code idata_fixup( struct code_info *CodeInfo, expr_list *opndx )
/*************************************************************************/
{
    //struct fixup      *fixup;
    enum fixup_types    fixup_type;
    int                 size;
    uint_8              Ofssize; /* 1=32bit, 0=16bit offset for fixup */

    DebugMsg1(("idata_fixup(opndx.type/mem_type=%u/%Xh) enter [CodeInfo.mem_type=%Xh]\n", opndx->type, opndx->mem_type, CodeInfo->mem_type));

    /* jmp/call/jcc/loopcc/jxcxz? */
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

    /* v2.04: looks like nonsense. */
    //if( opndx->instr != EMPTY ) {
    //    if( ( opndx->base_reg != EMPTY ) || ( opndx->idx_reg != EMPTY ) ) {
    //        AsmError( INVALID_MEMORY_POINTER );
    //        return( ERROR );
    //    }
    //}

    if( opndx->instr == T_SHORT ) {
        /* short works for branch instructions only */
        AsmErr( INVALID_INSTRUCTION_OPERANDS );
        return( ERROR );
    }

    /* v2.03: don't ignore a "NEAR32 ptr" qualifier */
    //if ( CodeInfo->mem_type == MT_EMPTY && Opnd_Count > OPND1 ) {
    if ( CodeInfo->mem_type == MT_EMPTY && Opnd_Count > OPND1 && opndx->Ofssize == USE_EMPTY ) {
        size = OperandSize( CodeInfo->opnd_type[OPND1], CodeInfo );
        /* may be a forward reference, so wait till pass 2 */
        if( Parse_Pass > PASS_1 && opndx->instr != EMPTY ) {
            switch ( opndx->instr ) {
            case T_SEG: /* v2.04a: added */
                if( size && (size < 2 ) ) {
                    AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, size, 2 );
                    return( ERROR );
                }
                break;
            case T_OFFSET:
            case T_LROFFSET:
#if IMAGERELSUPP
            case T_IMAGEREL:
#endif
#if SECTIONRELSUPP
            case T_SECTIONREL:
#endif
                if( size && (size < 2 || ( Ofssize && size < 4 ))) {
                    AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, size, ( 2 << Ofssize ) );
                    return( ERROR );
                }
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
                /* v2.04: curly brackets added */
                if ( CodeInfo->token == T_PUSHW ) {
                    if ( SizeFromMemtype( opndx->mem_type, Ofssize, opndx->type ) < 2 )
                        opndx->mem_type = MT_WORD;
                } else if ( CodeInfo->token == T_PUSHD ) {
                    if ( SizeFromMemtype( opndx->mem_type, Ofssize, opndx->type ) < 4 )
                        opndx->mem_type = MT_DWORD;
                }
                break;
            }
            /* if a WORD size is given, don't override it with */
            /* anything what might look better at first glance */
            if( opndx->mem_type != MT_EMPTY )
                CodeInfo->mem_type = opndx->mem_type;
            /* v2.04: assume BYTE size if symbol is undefined */
            else if ( opndx->sym->state == SYM_UNDEFINED )
                CodeInfo->mem_type = MT_BYTE;
            else if ( Ofssize > USE16 )
                CodeInfo->mem_type = MT_DWORD;
            else
                CodeInfo->mem_type = MT_WORD;
        }
    }
    size = SizeFromMemtype( CodeInfo->mem_type, Ofssize, NULL );
    switch( size ) {
    case 1:  CodeInfo->opnd_type[Opnd_Count] = OP_I8;  SET_OPSIZ_NO( CodeInfo );  break;
    case 2:  CodeInfo->opnd_type[Opnd_Count] = OP_I16; SET_OPSIZ_16( CodeInfo );  break;
    case 4:  CodeInfo->opnd_type[Opnd_Count] = OP_I32; SET_OPSIZ_32( CodeInfo );  break;
#if AMD64_SUPPORT
    case 8:  CodeInfo->opnd_type[Opnd_Count] = OP_I64; SET_OPSIZ_32( CodeInfo );  break;
#endif
#ifdef DEBUG_OUT
    default:
        DebugMsg1(("idata_fixup, unexpected size %u\n", SizeFromMemtype( CodeInfo->mem_type, Ofssize, NULL ) ));
#endif
    }
    if( opndx->instr == T_SEG ) {
        fixup_type = FIX_SEG;
    } else if( CodeInfo->mem_type == MT_BYTE ) {
        DebugMsg1(("idata_fixup, mem_type=BYTE\n" ));
        if ( opndx->instr == T_HIGH ) {
            DebugMsg1(("idata_fixup, FIX_HIBYTE\n" ));
            fixup_type = FIX_HIBYTE;
        } else {
            DebugMsg1(("idata_fixup, FIX_LOBYTE\n" ));
            fixup_type = FIX_LOBYTE;
        }
#if 0
    } else if( CodeInfo->mem_type == MT_FAR ) {
        /* v2.04: to be tested. this code is most likely obsolete.
         * There's never a PTR16|PTR32 fixup here. Far JMP/CALL are handled
         * elsewhere, and data items also.
         */
        /* temporary */
        printf("idata_fixup: MT_FAR occured at %s:%lu\n", FileInfo.fname[ASM], LineNumber );
        fixup_type = ( Ofssize ) ? FIX_PTR32 : FIX_PTR16;
        CodeInfo->isfar = TRUE; /* needed for mark_fixupp() */
        if ( opndx->Ofssize != USE_EMPTY )
            CodeInfo->Ofssize = opndx->Ofssize;
#endif
    } else if( IS_OPER_32( CodeInfo ) ) {
#if AMD64_SUPPORT
        if ( Ofssize == USE64 && CodeInfo->mem_type == MT_QWORD )
            fixup_type = FIX_OFF64;
        else
#endif
            /* v2.04: changed, no longer depends on OfsSize */
            if ( size == 4 )
                fixup_type = FIX_OFF32;
            else
                fixup_type = FIX_OFF16;
    } else {
        /* v2.04: changed, no longer depends on OfsSize */
        //if ( CodeInfo->mem_type == MT_DWORD ) {
            /* fixme !!!! warning
             * operand size is 16bit
             * but fixup is 32-bit */
        //    fixup_type = FIX_OFF32;
        //} else
            fixup_type = FIX_OFF16;
    }
    /* v2.04: 'if' added, don't set W bit if size == 1
     * code example:
     *   extern x:byte
     *   or al,x
     */
    if ( size != 1 )
        CodeInfo->iswide = 1;

    /* set global vars Frame, Frame_Datum and SegOverride */
    segm_override( opndx, NULL );
    if ( ModuleInfo.offsettype == OT_SEGMENT &&
        ( opndx->instr == T_OFFSET || opndx->instr == T_SEG ))
        find_frame2( opndx->sym );
    else
        find_frame( opndx->sym );

    //DebugMsg1(("idata_fixup: calling AddFixup(%s, %u)\n", opndx->sym->name, fixup_type ));
    CodeInfo->InsFixup[Opnd_Count] = AddFixup( opndx->sym, fixup_type, OPTJ_NONE );

    if ( opndx->instr == T_LROFFSET )
        CodeInfo->InsFixup[Opnd_Count]->loader_resolved = TRUE;

#if IMAGERELSUPP
    if ( opndx->instr == T_IMAGEREL && fixup_type == FIX_OFF32 )
        CodeInfo->InsFixup[Opnd_Count]->type = FIX_OFF32_IMGREL;
#endif
#if SECTIONRELSUPP
    if ( opndx->instr == T_SECTIONREL && fixup_type == FIX_OFF32 )
        CodeInfo->InsFixup[Opnd_Count]->type = FIX_OFF32_SECREL;
#endif
    DebugMsg1(("idata_fixup exit [CodeInfo.mem_type=%Xh Ofssize=%u opsiz=%u fixup->frame=%d]\n", CodeInfo->mem_type, CodeInfo->Ofssize, CodeInfo->prefix.opsiz, CodeInfo->InsFixup[Opnd_Count]->frame ));

    return( NOT_ERROR );
}

static void add_frame( const struct code_info *CodeInfo )
/*******************************************************/
/* add frame data in Frame and Frame_Datum to current fixup */
{
    if( Parse_Pass != PASS_1 ) {
        if ( CodeInfo->InsFixup[Opnd_Count] ) {
            CodeInfo->InsFixup[Opnd_Count]->frame = Frame;
            CodeInfo->InsFixup[Opnd_Count]->frame_datum = Frame_Datum;
        }
    }
}

/* convert MT_NEAR/MT_FAR/MT_PTR to MT_WORD, MT_DWORD, MT_FWORD, MT_QWORD.
 * MT_PTR cannot be set explicitely (by the PTR operator),
 * so this value must come from a label or a structure field.
 * (above comment is most likely plain wrong, see 'PF16 ptr [reg]'!
 * This code needs cleanup!
 */
static void SetPtrMemtype( struct code_info *CodeInfo, expr_list *opndx )
/***********************************************************************/
{
    asm_sym *sym = opndx->sym;
    int size = 0;

    /* v2.04c: added */
    if ( opndx->mem_type == MT_FAR || opndx->mem_type == MT_NEAR ) {
        int size;
        if ( opndx->Ofssize == USE_EMPTY )
            if ( sym && sym->state == SYM_STACK )
                opndx->Ofssize = sym->Ofssize;
        size = SizeFromMemtype( opndx->mem_type, opndx->Ofssize, NULL );
        MemtypeFromSize( size, &opndx->mem_type );
        return;
    }
    if ( opndx->mbr )  /* the mbr field has higher priority */
        sym = opndx->mbr;
    if ( sym && sym->type ) {
        size = sym->type->total_size;
        CodeInfo->isfar = sym->type->isfar;

        /* there's an ambiguity with pointers of size DWORD,
         since they can be either NEAR32 or FAR16 */
        if ( size == 4 && sym->type->Ofssize != CodeInfo->Ofssize )
            opndx->Ofssize = sym->type->Ofssize;

    } else if ( sym )  {
        if ( sym->isarray )
            size = sym->total_size / sym->total_length;
        else
            size = sym->total_size;
#if 1 /* v2.0: handle PF16 ptr [ebx], which didn't work in v1.96 */
    } else if ( opndx->explicit && opndx->type ) {
        size = opndx->type->total_size;
        CodeInfo->isfar = opndx->type->isfar;
#endif
    } else {
        if ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) ) {
            DebugMsg1(("SetPtrMemtype: model with FAR data pointers\n" ));
            size = 2;
        }
        size += (2 << ModuleInfo.defOfssize );
    }
    if ( size )
        MemtypeFromSize( size, &opndx->mem_type );
    DebugMsg1(("SetPtrMemtype: size=%u, new memtype=0x%x\n", size, opndx->mem_type ));
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
    struct asm_sym      *sym;
    uint_8              Ofssize;
    enum fixup_types    fixup_type;

    DebugMsg1(("memory_operand(opndx->value=%X, memtype=%Xh, fixup=%u) enter, [CodeInfo->memtype=%Xh, Ofssize=%u]\n",
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
    /* change pointer types ( MT_NEAR, MT_FAR, MT_PTR */
    /* v2.04a: should not be called if OFFSET was used */
    //if ( opndx->mem_type == MT_PTR ) /* this was before v2.04 */
    //if ( ( opndx->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )
    if ( ( opndx->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS && opndx->instr == EMPTY )
        SetPtrMemtype( CodeInfo, opndx );
#endif

    Set_Memtype( CodeInfo, opndx->mem_type );
    if( opndx->mbr != NULL ) {
        /* if the struct field is just another struct, use it's total size
         * to set CodeInfo->mem_type.
         */
        //if ( opndx->mbr->mem_type == MT_TYPE ) {
        /* v2: don't overwrite opndx->mem_type,
         *    testcase: cmp (dword ptr <struct_field>), 0
         */
        if ( opndx->mbr->mem_type == MT_TYPE && opndx->mem_type == MT_EMPTY ) {
            memtype mem_type;
            DebugMsg1(("memory_operand: mbr %s has mem_type MT_TYPE, total_size=%u\n", opndx->mbr->name, opndx->mbr->total_size ));
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
                DebugMsg1(("memory_operand, JMP/CALL: CodeInfo->memtype=empty, instruction operand must have size\n" ));
                AsmError( INSTRUCTION_OPERAND_MUST_HAVE_SIZE );
                return( ERROR );
            }
            /* fall through */
        case MT_NEAR:
             /* changed v2.0 */
#if AMD64_SUPPORT
            CodeInfo->mem_type = (CodeInfo->Ofssize == USE64) ? MT_QWORD : (CodeInfo->Ofssize == USE32) ? MT_DWORD : MT_WORD;
#else
            CodeInfo->mem_type = (CodeInfo->Ofssize == USE32) ? MT_DWORD : MT_WORD;
#endif
            DebugMsg1(("memory_operand, JMP/CALL: CodeInfo->memtype set to %Xh\n", CodeInfo->mem_type ));
            //CodeInfo->mem_type = CodeInfo->Ofssize ? MT_DWORD : MT_WORD;
            break;
        case MT_FAR:
             /* changed v2.0 */
#if AMD64_SUPPORT
            CodeInfo->mem_type = (CodeInfo->Ofssize == USE64) ? MT_TBYTE : (CodeInfo->Ofssize == USE32) ? MT_FWORD : MT_DWORD;
#else
            CodeInfo->mem_type = (CodeInfo->Ofssize == USE32) ? MT_FWORD : MT_DWORD;
#endif
            //CodeInfo->mem_type = CodeInfo->Ofssize ? MT_FWORD : MT_DWORD;
            break;
        }
        j = SizeFromMemtype( CodeInfo->mem_type, CodeInfo->Ofssize, NULL );
#if AMD64_SUPPORT
        if ( ( CodeInfo->Ofssize != USE64 ) && ( j == 1 || j > 6 ) ) {
#else
        if ( j == 1 || j > 6 ) {
#endif
            /* CALL/JMP possible for WORD/DWORD/FWORD memory operands only */
            DebugMsg1(("memory_operand: invalid operand, size=%u\n", j ));
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
        DebugMsg1(("memory_operand: JMP/CALL, CodeInfo->far=%u\n", CodeInfo->isfar ));
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

    /* check for base registers */

    if ( opndx->base_reg != EMPTY ) {
        base = AsmBuffer[opndx->base_reg]->value;
        if ( ( ( GetValueSp( base ) & OP_R32) && CodeInfo->Ofssize == USE32 ) ||
#if AMD64_SUPPORT
            ( ( GetValueSp( base ) & OP_R64) && CodeInfo->Ofssize == USE64 ) ||
#endif
            ( ( GetValueSp( base ) & OP_R16) && CodeInfo->Ofssize == USE16 ) )
            CodeInfo->prefix.adrsiz = FALSE;
        else {
            CodeInfo->prefix.adrsiz = TRUE;
#if AMD64_SUPPORT
            /* 16bit addressing modes don't exist in long mode */
            if ( ( GetValueSp( base ) & OP_R16) && CodeInfo->Ofssize == USE64 ) {
                AsmError( INVALID_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING );
                return( ERROR );
            }
#endif
        }
    }

    /* check for index registers */

    if( opndx->idx_reg != EMPTY ) {
        index = AsmBuffer[opndx->idx_reg]->value;
        if ( ( ( GetValueSp( index ) & OP_R32) && CodeInfo->Ofssize == USE32 ) ||
#if AMD64_SUPPORT
            ( ( GetValueSp( index ) & OP_R64) && CodeInfo->Ofssize == USE64 ) ||
#endif
            ( ( GetValueSp( index ) & OP_R16) && CodeInfo->Ofssize == USE16 ) ) {
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
                    /* cannot use ESP|RSP or 16-bit reg as index */
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
                /* 286 and down cannot use this memory mode */
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
                /* v2.03: override with a segment assumed != FLAT? */
                if ( opndx->override != EMPTY &&
                    SegOverride != &ModuleInfo.flat_grp->sym )
                    fixup_type = FIX_OFF32;
                else
                    fixup_type = FIX_RELOFF32;
            else
#endif
                fixup_type = ( Ofssize ) ? FIX_OFF32 : FIX_OFF16;
            DebugMsg1(( "memory_operand: direct addressing, fixup type=%u\n", fixup_type ));
        } else {
            DebugMsg1(( "memory_operand: CodeInfo->Ofssize=%u/prefix.adrsize=%u, Ofssize=%u\n",
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
                    /* address size is 16bit but label is 32-bit.
                     * example: use a 16bit register as base in FLAT model:
                     *   test buff[di],cl */
                    AsmWarn( 2, WORD_FIXUP_FOR_32BIT_LABEL, sym->name );
                }
            }
        }

        /* no fixups are needed for memory operands of string instructions and XLAT.
         * However, CMPSD and MOVSD are also SSE2 opcodes, so the fixups must be generated
         * anyways.
         */
        if ( CodeInfo->token != T_XLAT ) {
            //DebugMsg1(("memory_operand: calling AddFixup(%s, fixup=%u) [CodeInfo->memtype=%Xh]\n", sym ? sym->name : "NULL", fixup_type, CodeInfo->mem_type));
            CodeInfo->InsFixup[Opnd_Count] = AddFixup( sym, fixup_type, OPTJ_NONE );
        }
        /* v2.03: Modend is obsolete */
        //if( Modend ) { /* is current operand END argument? */
        //    asm_sym *assume;
        //    /* set global vars Frame and Frame_Datum */
        //    GetAssume( SegOverride, sym, ASSUME_NOTHING, &assume);
        //    SetFixupFrame( assume );
        //} else {
            if( mem2code( CodeInfo, ss, index, base, sym ) == ERROR ) {
                return( ERROR );
            }
        //}
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
        if ( sym && sym->state == SYM_UNDEFINED &&
            CodeInfo->token == T_MOV &&
            Opnd_Count == OPND2 &&
            (CodeInfo->opnd_type[OPND1] & OP_M_ANY) )
            CodeInfo->opnd_type[Opnd_Count] = OP_I8;
#endif
        if ( sym && sym->state == SYM_STACK ) {
            if( base != EMPTY ) {
                if( index != EMPTY ) {
                    /* no free index register */
                    AsmError( MULTIPLE_INDEX_REGISTERS_NOT_ALLOWED );
                    return( ERROR );
                } else {
                    index = base;
                }
            }
            base = tbaseptr[CodeInfo->Ofssize];
            /* v2.0: removed */
            //if( CodeInfo->mem_type == MT_EMPTY ) {
            //    Set_Memtype( CodeInfo, sym->mem_type );
            //}
        }
        if( mem2code( CodeInfo, ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
    } /* end if ( with_fixup ) */

    DebugMsg1(("memory_operand exit, ok, opndx.type/value=%Xh/%Xh, CodeInfo.memtype/rmbyte=%X/%X opndtype=%Xh fix=%Xh\n",
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

        DebugMsg1(("process_address: INDIRECT, sym=%s, adrsiz=%u\n", opndx->sym ? opndx->sym->name : "NULL", CodeInfo->prefix.adrsiz ));
        if ( opndx->hvalue && ( opndx->hvalue != -1 || opndx->value >= 0 ) ) {
            /* Masm (both ML and ML64) just truncates.
             * JWasm throws an error in 64bit mode and
             * warns (level 3) in the other modes.
             */
            DebugMsg1(("process_address: displacement doesn't fit in 32 bits: %I64X\n", opndx->value64 ));
#if AMD64_SUPPORT
            /* todo: check if really useful */
            if ( ModuleInfo.Ofssize == USE64 ) {
                AsmError( CONSTANT_VALUE_TOO_LARGE );
                return( ERROR );
            }
#endif
            AsmWarn( 3, DISPLACEMENT_OUT_OF_RANGE, opndx->value64 );
        }
        if( opndx->sym == NULL || opndx->sym->state == SYM_STACK ) {
            return( memory_operand( CodeInfo, opndx, FALSE ) );
        } else {
            return( memory_operand( CodeInfo, opndx, TRUE ) );
        }
    } else if( opndx->instr != EMPTY ) {
        /* instr is OFFSET | LROFFSET | SEG | LOW | LOWWORD, ... */
        DebugMsg1(("process_address: instr=%s\n", GetResWName( opndx->instr, NULL ) ));
        if( opndx->sym == NULL ) { /* better to check opndx->type? */
            return( idata_nofixup( CodeInfo, opndx ) );
        } else {
            /* allow "lea <reg>, [offset <sym>]" */
            if( CodeInfo->token == T_LEA && opndx->instr == T_OFFSET )
                return( memory_operand( CodeInfo, opndx, TRUE ) );
            return( idata_fixup( CodeInfo, opndx ) );
        }
    } else if( opndx->sym == NULL ) { /* direct operand without symbol */
        DebugMsg1(("process_address: symbol=NULL\n" ));
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
        DebugMsg1(("process_address: sym=SYM_UNDEFINED, name=%s, state=%X\n", opndx->sym->name, opndx->sym->state ));
        /* v2.04: unnecessary, the expression evaluator will have emitted an error already */
        //if( Parse_Pass != PASS_1 ) {
        //    AsmErr( SYMBOL_NOT_DEFINED, opndx->sym->name );
        //    return( ERROR );
        //}
        /* undefined symbol, it's not possible to determine
         * operand type and size currently. However, for backpatching
         * a fixup should be created.
         */
        /* assume a code label for branch instructions! */
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
            /* v2.04: if operand is the second argument (and the first is NOT
             * a segment register!), scan the instruction
             * instruction table if the instruction allows an immediate!
             * If so, assume the undefined symbol is a constant.
             */
            if ( Opnd_Count == OPND2 && (( CodeInfo->opnd_type[OPND1] & OP_SR ) == 0 ) ) {
                const struct asm_ins  *p = CodeInfo->pcurr;
                do {
                    if ( p->opnd_type[OPND2] & OP_I )
                        return( idata_fixup( CodeInfo, opndx ) );
                    p++;
                } while ( p->first == FALSE );
            }
        }
        /* v2.0: fixups should be generated for undefined operands
         * to allow backpatching!
         */
        //return( memory_operand( CodeInfo, opndx, FALSE ) );
        return( memory_operand( CodeInfo, opndx, TRUE ) );

    } else if( ( opndx->sym->state == SYM_SEG ) ||
               ( opndx->sym->state == SYM_GRP ) ) {
        DebugMsg1(("process_address: sym->state=SEG/GROUP\n"));
        /* SEGMENT and GROUP symbol is converted to SEG symbol
         * for next processing */
        opndx->instr = T_SEG;
        return( idata_fixup( CodeInfo, opndx ) );
    } else {
        DebugMsg1(("process_address direct, sym=%s\n", opndx->sym->name ));
        //mem_type = ( opndx->explicit ) ? opndx->mem_type : opndx->sym->mem_type;
        /* to fade usage of 'explicit' */
        mem_type = opndx->mem_type;

        /* symbol external, but absolute? */
        if( opndx->abs ) {
            return( idata_fixup( CodeInfo, opndx ) );
        }

        /* CODE location is converted to OFFSET symbol */
        switch( mem_type ) {
        case MT_FAR:
        case MT_NEAR:
        case MT_PROC:
            if( CodeInfo->token == T_LEA ) {
                return( memory_operand( CodeInfo, opndx, TRUE ) );
            } else if( opndx->sym == &symPC ) {
                return( idata_fixup( CodeInfo, opndx ) );
            } else if( opndx->mbr != NULL ) { /* structure field? */
                return( memory_operand( CodeInfo, opndx, TRUE ) );
            } else {
                return( idata_fixup( CodeInfo, opndx ) );
            }
            break;
        default:
            /* direct memory with fixup */
            return( memory_operand( CodeInfo, opndx, TRUE ) );
            break;
        }
    }
    return( NOT_ERROR );
}

/* handle constant operands
 * these might also need a fixup if they are externals (EXTERN:ABS!)
 */
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
            /* v2.04b: if op2 was assumed an immediate due to fwd ref,
             * change it back to a mem ref now.
             */
            if ( ( CodeInfo->opnd_type[OPND2] & OP_I ) &&
                CodeInfo->InsFixup[OPND2] &&
                CodeInfo->InsFixup[OPND2]->sym->state == SYM_UNDEFINED )
                CodeInfo->opnd_type[OPND2] = OP_M;
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
    int  reg;
    int  regno;
    uint_32 flags;

    DebugMsg1(( "process_register enter (%s)\n", AsmBuffer[opndx->base_reg]->string_ptr ));
    reg = AsmBuffer[opndx->base_reg]->value;
    regno = GetRegNo( reg );
    /* the register's "OP-flags" are stored in the 'value' field */
    flags = GetValueSp( reg );
    CodeInfo->opnd_type[Opnd_Count] = flags;
    if ( flags & OP_R8 ) {
        /* it's probably better to not reset the wide bit at all */
        if ( flags != OP_CL )      /* problem: SHL AX|AL, CL */
            CodeInfo->iswide = 0;

#if AMD64_SUPPORT
        if ( CodeInfo->Ofssize == USE64 && regno >=4 && regno <=7 )
            if ( SpecialTable[reg].cpu == P_86 )
                CodeInfo->x86hi_used = 1;
            else
                CodeInfo->x64lo_used = 1;
#endif
        if ( StdAssumeTable[regno].error & (( reg >= T_AH && reg <= T_BH ) ? RH_ERROR : RL_ERROR ) ) {
            AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
            return( ERROR );
        }
    } else if ( flags & OP_R ) {
        CodeInfo->iswide = 1;
        if ( StdAssumeTable[regno].error & flags & OP_R ) {
            AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
            return( ERROR );
        }
        if ( flags & OP_R16 ) {
            if ( CodeInfo->Ofssize > USE16 )
                CodeInfo->prefix.opsiz = TRUE;
        } else {
            if( CodeInfo->Ofssize == USE16 )
                CodeInfo->prefix.opsiz = TRUE;
        }
    } else if ( flags & OP_SR ) {
        if( regno == 1 ) { /* 1 is CS */
            /* POP CS is not allowed */
            if( CodeInfo->token == T_POP ) {
                AsmError( POP_CS_IS_NOT_ALLOWED );
                return( ERROR );
            }
        }
    } else if ( flags & OP_ST ) {
        reg = opndx->idx_reg;
        if ( reg > 7 ) { /* v1.96: index check added */
            AsmError( INVALID_COPROCESSOR_REGISTER );
            return( ERROR );
        }
        CodeInfo->rm_byte |= reg;
        if( reg != 0 )
            CodeInfo->opnd_type[Opnd_Count] = OP_ST_REG;
    } else if ( flags & OP_SPECREG ) { /* CRx, DRx, TRx */
        if( CodeInfo->token != T_MOV ) {
            AsmError( ONLY_MOV_CAN_USE_SPECIAL_REGISTER );
            return( ERROR );
        }
        /* v2.04: previously there were 3 flags, OP_CR, OP_DR and OP_TR.
         * this was summoned to one flag OP_SPECREG to free 2 flags, which
         * are needed if AVC ( new YMM registers ) is to be supported.
         * To distinguish between CR, DR and TR, the register number is
         * used now: CRx are numbers 0-F, DRx are numbers 0x10-0x1F and
         * TRx are 0x20-0x2F.
         */
        if ( regno >= 0x20 ) { /* TRx? */
            CodeInfo->opc_or |= 0x04;
            switch( regno ) {
            case 0x23:
            case 0x24:
            case 0x25:
                if( ( ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_486 )
                     || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_686 ) )
                   && ( ( SpecialTable[reg].cpu & P_CPU_MASK ) >= P_486 ) ) {
                    /* TR3-TR5 are available on 486 only */
                    AsmErr( CANNOT_USE_TRN_TO_TRM_WITH_CURRENT_CPU_SETTING, 3, 5 );
                    return( ERROR );
                }
                break;
            case 0x26:
            case 0x27:
                if( ( ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 )
                     || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_686 ) )
                   && ( ( SpecialTable[reg].cpu & P_CPU_MASK ) >= P_386 ) ) {
                    /* TR6+TR7 are available on 386...586 only */
                    AsmErr( CANNOT_USE_TRN_TO_TRM_WITH_CURRENT_CPU_SETTING, 6, 7 );
                    return( ERROR );
                }
                break;
            }
        } else if ( regno >= 0x10 ) { /* DRx? */
            CodeInfo->opc_or |= 0x01;
        }
        regno &= 0x0F;
    }

#if AMD64_SUPPORT
    /* if it's a x86-64 register (SIL, R8W, R8D, RSI, ... */
    if ( ( SpecialTable[reg].cpu & P_CPU_MASK ) == P_64 ) {
        CodeInfo->prefix.rex |= 0x40;
        if ( flags & OP_R64 )
            CodeInfo->prefix.rex |= REX_W;
    }
#endif

    if( Opnd_Count == OPND1 ) {
        /* the first operand
         * r/m is treated as a 'reg' field */
        CodeInfo->rm_byte |= MOD_11;
#if AMD64_SUPPORT
        CodeInfo->prefix.rex |= (regno & 8 ) >> 3; /* set REX_B */
        regno &= BIT_012;
#endif
        /* fill the r/m field */
        CodeInfo->rm_byte |= regno;
    } else {
        /* the second operand
         * XCHG can use short form if op1 is AX/EAX/RAX */
        if( ( CodeInfo->token == T_XCHG ) && ( CodeInfo->opnd_type[OPND1] & OP_A ) &&
             ( 0 == (CodeInfo->opnd_type[OPND1] & OP_R8 ) ) ) {
#if AMD64_SUPPORT
            CodeInfo->prefix.rex |= (regno & 8 ) >> 3; /* set REX_B */
            regno &= BIT_012;
#endif
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & BIT_67 ) | regno;
        } else {
            /* fill reg field with reg */
#if AMD64_SUPPORT
            CodeInfo->prefix.rex |= (regno & 8 ) >> 1; /* set REX_R */
            regno &= BIT_012;
#endif
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & ~BIT_345 ) | ( regno << 3 );
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
                    DebugMsg1(("HandleStringInstructions: CMPS: CodeInfo->RegOverride=%X, opndx->override=%s\n", CodeInfo->prefix.RegOverride, AsmBuffer[opndx->override]->string_ptr ));
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
            CodeInfo->iswide = 0;
            if( CodeInfo->Ofssize )
                CodeInfo->prefix.opsiz = FALSE;
            break;
        case 2:
            CodeInfo->mem_type = MT_WORD;
            CodeInfo->iswide = 1;
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? TRUE : FALSE;
            break;
        case 4:
            CodeInfo->mem_type = MT_DWORD;
            CodeInfo->iswide = 1;
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? FALSE : TRUE;
            break;
#if AMD64_SUPPORT
        case 8:
            if ( CodeInfo->Ofssize == USE64 ) {
                CodeInfo->mem_type = MT_QWORD;
                CodeInfo->iswide = 1;
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
- todo: BOUND second operand check ( may be WORD/DWORD or DWORD/QWORD )
*/
{
    OPNDTYPE    op1 = CodeInfo->opnd_type[OPND1];
    OPNDTYPE    op2 = CodeInfo->opnd_type[OPND2];
    ret_code    rc = NOT_ERROR;
    int         op1_size;
    int         op2_size;
    //int         op_size = 0;

    DebugMsg1(("check_size enter, optype1=%X, optype2=%X\n", op1, op2 ));
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
        /* check was wrong - instructions take a m64 OR an 8 bit immediate */
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
            /* wide and size is NOT determined by DX, but
             * by the first operand, AL|AX|EAX
             */
            switch( op1 ) {
            case OP_AX:
                break;
            case OP_AL:
                CodeInfo->iswide = 0;         /* clear w-bit */
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
                CodeInfo->iswide = 0;         /* clear w-bit */
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
        /* ENTER has to be OP_I16, OP_I8 */
        if( op1 == OP_I32 ) {
            /* parse_phase_1 will treat 16-bit data as OP_I32 if CPU is 386 */
            if( CodeInfo->data[OPND1] > (signed long)USHRT_MAX ) {
                /* if op1 is really 32-bit data, then error */
                AsmError( INVALID_OPERAND_SIZE );
                rc = ERROR;
            }
        }
        /* type cast op1 to OP_I16 */
        CodeInfo->opnd_type[OPND1] = OP_I16;
        /* op2 have to be 8-bit data */
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
        CodeInfo->iswide = 0;
        op1_size = OperandSize( op1, CodeInfo );
        op2_size = OperandSize( op2, CodeInfo );
        DebugMsg1(("check_size, MOVZX/MOVSX: op2_size=%u, opndx.memtype=%Xh, opndx.sym=%X\n", op2_size, opndx->mem_type, opndx->sym ));
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
                CodeInfo->iswide = 1;
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
#if AMD64_SUPPORT
    case T_MOVSXD:
        break;
#endif
#if AMD64_SUPPORT
    case T_LAR: /* v2.04: added */
    case T_LSL: /* 19-sep-93 */
#if 1 /* v2.04: changed */
        if ( ModuleInfo.Ofssize != USE64 || ( ( op2 & OP_M ) == 0 ) )
            goto def_check;
        /* in 64-bit, if second argument is memory operand,
         * ensure it has size WORD ( or 0 if a forward ref )
         */
        op2_size = OperandSize( op2, CodeInfo );
        if ( op2_size != 2 && op2_size != 0 ) {
            AsmError( INVALID_OPERAND_SIZE );
            return( ERROR );
        }
        /* the opsize prefix depends on the FIRST operand only! */
        op1_size = OperandSize( op1, CodeInfo );
        if ( op1_size != 2 )
            CodeInfo->prefix.opsiz = FALSE;
#else
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
#endif
        break;
#endif
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
            if( ( op2_size == 2 ) || ( op2_size == 4 )
#if AMD64_SUPPORT
               || ( op2_size == 8 && ModuleInfo.Ofssize == USE64 )
#endif
              ) {
                return( NOT_ERROR );
            }
        } else if( op2 & OP_SR ) {
            op1_size = OperandSize( op1, CodeInfo );
            if( ( op1_size == 2 ) || ( op1_size == 4 )
#if AMD64_SUPPORT
               || ( op1_size == 8 && ModuleInfo.Ofssize == USE64 )
#endif
              ) {
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
                    /* short form exists for direct addressing only, change OP_A to OP_R! */
                    CodeInfo->opnd_type[OPND1] &= ~OP_A;
                    DebugMsg1(("check_size: op1 changed to %X\n", CodeInfo->opnd_type[OPND1] ));
                } else if ( CodeInfo->opnd_type[OPND2] & OP_A ) {
                    /* short form exists for direct addressing only, change OP_A to OP_R! */
                    CodeInfo->opnd_type[OPND2] &= ~OP_A;
                    DebugMsg1(("check_size: op2 changed to %X\n", CodeInfo->opnd_type[OPND2] ));
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
        } else if( ( op1 & OP_SPECREG ) || ( op2 & OP_SPECREG ) ) {
            CodeInfo->prefix.opsiz = FALSE;
            //return( rc ); /* v1.96: removed */
        }
        /* fall through */
    default:
#if AMD64_SUPPORT
    def_check:
#endif
        /* make sure the 2 opnds are of the same type */
        op1_size = OperandSize( op1, CodeInfo );
        op2_size = OperandSize( op2, CodeInfo );
        DebugMsg1(("check_size default: size1(%X)=%u, size2(%X)=%u\n", op1, op1_size, op2, op2_size));
        if( op1_size > op2_size ) {
            if( ( op2 >= OP_I8 ) && ( op2 <= OP_I32 ) ) {     /* immediate */
                op2_size = op1_size;    /* promote to larger size */
            }
        }
#if 1
        /* v2.04: check in idata_nofixup was signed,
         * so now add -256 - -129 and 128-255 to acceptable byte range.
         * Since Masm v8, the check is more restrictive, -255 - -129
         * is no longer accepted.
         */
        if( ( op1_size == 1 ) && ( op2 == OP_I16 ) &&
            ( CodeInfo->data[OPND2] <= UCHAR_MAX ) &&
            //CodeInfo->data[OPND2] >= -128 ) ) {
            ( CodeInfo->data[OPND2] >= -255 ) ) {
            return( rc ); /* OK cause no sign extension */
        }
#endif
#if 0
        /* v2.03: this "if" made JWasm accept any 32-bit constant
         *        for 16-bit destinations, which is Masm compatibel,
         *      "mov ax, 12345h"
         * the test is a bit too liberal here, IMO, because
         * it makes JWasm accept "mov ax, near32 ptr var",
         * which is rejected by Masm.
         */
        if( ( op1_size == 2 ) && ( op2 == OP_I32 )
            && ( CodeInfo->data[OPND2] <= USHRT_MAX ) ) {
            return( rc ); /* OK cause no sign extension */
        }
#endif
        if( op1_size != op2_size ) {
            /* if one or more are !defined, set them appropriately */
            if( ( op1 | op2 ) & ( OP_MMX | OP_XMM ) ) {
            } else if( ( op1_size != 0 ) && ( op2_size != 0 ) ) {
                AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, op1_size, op2_size );
                rc = ERROR;
            }
            /* size == 0 is assumed to mean "undefined", but there
             * is also the case of an "empty" struct or union. The
             * latter case isn't handled correctly.
             */
            if( op1_size == 0 ) {
                if( ( op1 & OP_M_ANY ) && ( op2 & OP_I ) ) {
                    char *p = "WORD";
                    if( (unsigned long)CodeInfo->data[OPND2] > USHRT_MAX || op2_size == 4 ) {
                        CodeInfo->iswide = 1;
                        DebugMsg1(("check_size: op1=%X op1_size=0, op2=%X, op2_size=%u CodeInfo->data[2]=%X\n", op1, op2, op2_size, CodeInfo->data[OPND2] ));
#if 1 /* added v1.95: in 16bit code, 'mov [di],8000h' should warn: assuming WORD */
                        if ( ModuleInfo.Ofssize == USE16 && op2_size > 2 && InWordRange( CodeInfo->data[OPND2] ) )
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
                         CodeInfo->iswide = 1;
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
                        CodeInfo->iswide = 1;
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, SIZE_NOT_SPECIFIED_ASSUMING, "WORD" );
                        }
                        if( CodeInfo->Ofssize )
                            CodeInfo->prefix.opsiz = TRUE;
                        break;
                    case 4:
                        CodeInfo->mem_type = MT_DWORD;
                        CodeInfo->iswide = 1;
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, SIZE_NOT_SPECIFIED_ASSUMING, "DWORD" );
                        }
                        break;
                    }
                }
            }
        }
    }
    DebugMsg1(("check_size exit [CodeInfo->mem_type=%Xh]\n", CodeInfo->mem_type));
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

    DebugMsg1(("ParseItems enter, Token_Count=%u, queue_level=%u, ofs=%Xh\n",
              Token_Count, queue_level, GetCurrOffset() ));

    if( DefineProc == TRUE )
        if ( ModuleInfo.proc_prologue == NULL || *ModuleInfo.proc_prologue == NULLC )
            proc_check();

    /* init CodeInfo */
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
    CodeInfo->rm_byte        = 0;
    CodeInfo->sib            = 0;            /* assume ss is *1 */
    CodeInfo->Ofssize        = ModuleInfo.Ofssize;
    CodeInfo->opc_or         = 0;
    CodeInfo->flags          = 0;

    Opnd_Count = OPND1; /* to be moved to CodeInfo */
    Frame = EMPTY;
    SegOverride = NULL;
    list_pos_start = list_pos;

    i = 0;

    /* Does line start with a code label? */
    if ( AsmBuffer[0]->token == T_ID && ( AsmBuffer[1]->token == T_COLON || AsmBuffer[1]->token == T_DBL_COLON ) ) {
        i = 2;
        DebugMsg1(("ParseItems T_COLON, code label=%s\n", AsmBuffer[0]->string_ptr ));
        if ( SegAssumeTable[ASSUME_CS].error ) { /* CS assumed to ERROR? */
            AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
            return( ERROR );
        }
        if (( AsmBuffer[1]->token == T_DBL_COLON ) || ( CurrProc == NULL )) {
            if( LabelCreate( AsmBuffer[0]->string_ptr, MT_NEAR, NULL, FALSE ) == NULL ) {
                DebugMsg1(("ParseItems, error creating global label, exit\n"));
                return( ERROR );
            }
        } else {
            if( LabelCreate( AsmBuffer[0]->string_ptr, MT_NEAR, NULL, TRUE ) == NULL ) {
                DebugMsg1(("ParseItems, error creating local label, exit\n"));
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
               ( AsmBuffer[i+1]->token != T_RES_ID || AsmBuffer[i+1]->type != RWT_TYPE ) &&
                ( sym = SymIsType( AsmBuffer[i]->string_ptr ) ) &&
                ( CurrStruct == NULL || ( SymIsType( AsmBuffer[i+1]->string_ptr ) == FALSE ) ) ) {
                DebugMsg1(("ParseItems: anonymous data item >%s<\n", AsmBuffer[i]->string_ptr ));
                return( data_init( CodeInfo, label, i, (dir_node *)sym ) );
            }
            label = i;
            i++;
        }
        switch ( AsmBuffer[i]->token ) {
        case T_DIRECTIVE:
            DebugMsg1(("ParseItems: T_DIRECTIVE >%s<\n", AsmBuffer[i]->string_ptr ));
            if ( AsmBuffer[i]->dirtype == DRT_DATADIR ) {
                return( data_init( CodeInfo, label, i, NULL ) );
            }
            if( CurrStruct && ( GetValueSp( AsmBuffer[i]->value ) & DF_NOSTRUC ) ) {
                AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
                return( ERROR );
            }
            /* label allowed for directive? */
            //if ( AsmBuffer[i]->flags & DF_LABEL ) {
            if ( GetValueSp( AsmBuffer[i]->value ) & DF_LABEL ) {
                if ( i && AsmBuffer[0]->token != T_ID ) {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
                    return( ERROR );
                }
            } else if ( i && AsmBuffer[i-1]->token != T_COLON && AsmBuffer[i-1]->token != T_DBL_COLON ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->string_ptr );
                return( ERROR );
            }
            temp = directive( i, CodeInfo );
            /* v2.0: for generated code it's important that list file is 
             * written in ALL passes, to update file position! */
            //if ( ModuleInfo.list && line_listed == FALSE && Parse_Pass == PASS_1 )
            if ( ModuleInfo.list &&
                ModuleInfo.line_listed == FALSE &&
                ( Parse_Pass == PASS_1 || GeneratedCode ) )
                LstWriteSrcLine();
            return( temp );
        case T_RES_ID:
            DebugMsg1(("ParseItems: T_RES_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if ( AsmBuffer[i]->type == RWT_TYPE )
                return( data_init( CodeInfo, label, i, NULL ) );
            break;
        case T_ID:
            DebugMsg1(("ParseItems: T_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if( sym = SymIsType( AsmBuffer[i]->string_ptr ) ) {
                return( data_init( CodeInfo, label, i, (dir_node *)sym ) );
            }
            break;
        default:
            if ( AsmBuffer[i]->token == T_COLON ) {
                DebugMsg1(("ParseItems: unexpected colon\n" ));
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
                return( ERROR );
            }
            break;
        } /* end switch (AsmBuffer[i]->token) */
        if ( label != -1 )
            i--;
        DebugMsg1(("ParseItems: unexpected token=%u, i=%u, string=%s\n", AsmBuffer[i]->token, i, AsmBuffer[i]->string_ptr));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    DebugMsg1(("ParseItems: %s\n", AsmBuffer[i]->string_ptr));
    /* v2.04 added */
    if( CurrStruct ) {
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
        return( ERROR );
    }

    /* instruction prefix?
     * T_LOCK, T_REP, T_REPE, T_REPNE, T_REPNZ, T_REPZ */
    if ( AsmBuffer[i]->value >= T_LOCK && AsmBuffer[i]->value <= T_REPZ ) {
        CodeInfo->prefix.ins = AsmBuffer[i]->value;
        i++;
        /* prefix has to be followed by an instruction */
        if( AsmBuffer[i]->token != T_INSTRUCTION ) {
            DebugMsg1(("ParseItems: unexpected token after prefix, exit, error\n"));
            AsmError( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION );
            return( ERROR );
        }
        DebugMsg1(("ParseItems: %s\n", AsmBuffer[i]->string_ptr));
    };

    if( CurrProc ) {
        switch( AsmBuffer[i]->value ) {
        case T_RET:
        case T_IRET:
        case T_IRETD: /* IRETW doesn't exist. Masm bug? */
#if AMD64_SUPPORT
        case T_IRETQ:
#endif
            if ( in_epilogue == FALSE ) {
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
    /* get the instruction's start position in InstrTable */
    CodeInfo->pcurr = &InstrTable[IndexFromToken( CodeInfo->token )];
    i++;

    if( CurrSeg == NULL ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }
    if( CurrSeg->e.seginfo->segtype == SEGTYPE_UNDEF ) {
        CurrSeg->e.seginfo->segtype = SEGTYPE_CODE;
    }
    if ( ModuleInfo.CommentDataInCode )
        omf_OutSelect( FALSE );
#if FASTPASS
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
        SaveState();
    }
#endif

    /* get the instruction's arguments */

    while ( AsmBuffer[i]->token != T_FINAL ) {

        DebugMsg1(("ParseItems(%s,%u): calling EvalOperand, i=%u\n", instr, Opnd_Count, i));
        if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
            DebugMsg1(("ParseItems(%s,%u): EvalOperand() failed\n", instr, Opnd_Count ));
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

        DebugMsg1(("ParseItems(%s,%u): type/value/mem_type/ofssize=%Xh/%I64Xh/%Xh/%d\n", instr, Opnd_Count, opndx.kind, opndx.value64, opndx.mem_type, opndx.Ofssize ));
        switch( opndx.kind ) {
        case EXPR_ADDR:
            DebugMsg1(("ParseItems(%s,%u): type ADDRESS\n", instr, Opnd_Count ));
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
            DebugMsg1(("ParseItems(%s,%u): type CONST, opndx.memtype=%Xh\n", instr, Opnd_Count, opndx.mem_type));
            if ( process_const( CodeInfo, &opndx ) == ERROR )
                return( ERROR );
            break;
        case EXPR_REG:
            DebugMsg1(("ParseItems(%s,%u): type REG\n", instr, Opnd_Count ));
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
                    bool negative = FALSE;
                    if ( AsmBuffer[i]->token != T_FLOAT ) {
                        negative = ( AsmBuffer[i]->token == '-' ? TRUE : FALSE );
                        i++;
                    }
                    opndx.kind = EXPR_CONST;
                    /* always convert to float */
                    //atofloat( &opndx.fvalue, i, OperandSize( CodeInfo->opnd_type[0], CodeInfo ), negative );
                    atofloat( &opndx.fvalue, i, 4, negative );
                    i++;
                    process_const( CodeInfo, &opndx );
                    break;
                }
#endif
                /* Masm message is: real or BCD number not allowed */
                AsmError( FP_INITIALIZER_IGNORED );
                return( ERROR );
            }
            /* fall through */
        default:
            DebugMsg1(("ParseItems(%s,%u): unexpected operand kind=%d, error, exit\n", instr, Opnd_Count, opndx.kind ));
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
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
     * skip the "near" entries in InstrTable for CALL/JMP.
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
                DebugMsg1(("ParseItems(%s): check_size() failed, exit\n", instr ));
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
                    DebugMsg1(("ParseItems: PUSH/POP operand with size 4 or CS/DS/ES/SS\n"));
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
                if ( CodeInfo->opnd_type[OPND1] & OP_SPECREG || CodeInfo->opnd_type[OPND2] & OP_SPECREG )
                    CodeInfo->prefix.rex &= 0x7;
            }
        }
#endif
    }

    /* now call the code generator */

    if ( FileInfo.file[LST] ) {
        temp = match_phase_1( CodeInfo );
        LstWrite( LSTTYPE_CODE, oldofs, NULL );
        return( temp );
    }
    return( match_phase_1( CodeInfo ) );
}

/* get current name of a reserved word */

char *GetResWName( enum asm_token reg, char *buff )
/*************************************************/
{
    static char intbuff[32];
    if ( !buff )
        buff = intbuff;
    memcpy( buff, AsmResWord[reg].name, AsmResWord[reg].len );
    buff[AsmResWord[reg].len] = NULLC;
    return( buff );
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
    T_FRAME,           /* add x64 reserved word part of special.h */
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
    { T_LGDT - SPECIAL_LAST, T_LGDT_I, T_LGDT_I64 },
    { T_LIDT - SPECIAL_LAST, T_LIDT_I, T_LIDT_I64 },
    { T_CALL - SPECIAL_LAST, T_CALL_I, T_CALL_I64 },
    { T_JMP  - SPECIAL_LAST, T_JMP_I,  T_JMP_I64  },
#if 1
    /* with ML.EXE, SLDT|SMSW|STR accept a WORD argument only -
     * ML64.EXE also accepts 32- and 64-bit registers!
     */
    { T_SLDT - SPECIAL_LAST, T_SLDT_I, T_SLDT_I64 },
    { T_SMSW - SPECIAL_LAST, T_SMSW_I, T_SMSW_I64 },
    { T_STR  - SPECIAL_LAST, T_STR_I,  T_STR_I64  },
#endif
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
            optable_idx[ T_INC - SPECIAL_LAST ]++;   /* skip the one-byte register INC */
            optable_idx[ T_DEC - SPECIAL_LAST ]++;   /* skip the one-byte register DEC */
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
                optable_idx[ repl->tok] = repl->idx64;
            }
        } else  {
            optable_idx[T_INC - SPECIAL_LAST]--;   /* restore the one-byte register INC */
            optable_idx[T_DEC - SPECIAL_LAST]--;   /* restore the one-byte register DEC */
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

void InitInstHashTable( void );

/* ParseInit() is called once per module */

ret_code ParseInit( void )
/************************/
{
    struct ReservedWord *curr;
    struct ReservedWord *last;
#if RENAMEKEY
    struct rename_node  *rencurr;
#endif

    DebugMsg(("ParseInit() enter\n"));

    if( fInit == FALSE ) {  /* if not initialized */
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
#if 0 //def DEBUG_OUT
    { int i;
    const struct asm_special *cas;
    char buffer[32];
    DebugMsg(("SpecialTable\n"));
    DebugMsg(("keyword             value   sflags  cpu val8 type\n"));
    DebugMsg(("-------------------------------------------------\n"));
    for ( i = 0, cas = SpecialTable; cas < SpecialTable + sizeof( SpecialTable) / sizeof ( struct asm_special ); cas++, i++ ) {
        memcpy( buffer, AsmResWord[i].name, AsmResWord[i].len );
        buffer[ AsmResWord[i].len ] = NULLC;
        DebugMsg(("%-16s %8X %8X %4X %4X  %2X\n", buffer, cas->value, cas->sflags, cas->cpu, cas->value8, cas->type ));
    }
    DebugMsg(("-------------------------------------------------\n"));
    }
#endif
    DebugMsg(("ParseInit() exit\n"));
    return( NOT_ERROR );
}
