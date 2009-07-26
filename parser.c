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

#define ONEXMM 1 /* 1=use ONE .xmm directive (Masm compatible) */

#define SET_ADRSIZ( s, x ) ( s->prefix.adrsiz = (( x ) ^ ( s->Ofssize )) ? TRUE : FALSE )
#define IS_ADDR32( s )  ( s->Ofssize ? ( s->prefix.adrsiz == FALSE ) : ( s->prefix.adrsiz == TRUE ))

// create AsmOpTable table.

const struct asm_ins AsmOpTable[] = {

#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
    { {op1,op2}, 0,      1 ,0,          0,       0,   0,      cpu, opcode, rm_byte },
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { {op1,op2}, prefix, 0, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
    { {op1,op2}, prefix, 1, byte1_info, rm_info, op3, op_dir, cpu, opcode, rm_byte },
#include "special.h"
#include "instruct.h"
#include "instr64.h"
ins (NULL,0,0,0,0,0,0,0,0,0,0,0,0)
#undef insx
#undef insn
#undef ins
#undef res
};

// define indices for AsmOpTable

enum res_idx {
#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) T_ ## tok ## _I,
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _ ## suffix,
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) T_ ## tok ## _I,
#include "special.h"
#include "instruct.h"
#undef ins
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) T_ ## tok ## _I64,
#include "instr64.h"
#undef insx
#undef insn
#undef ins
#undef res
T_NULL_I
};

// create the strings for all reserved words

#if AMD64_SUPPORT
#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
static const char T_ ## tok ## str[sizeof(#string)-1] = { # string };
#else
#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
static const char T_ ## tok ## str[len] = { # string };
#endif
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
static const char T_ ## tok ## str[len] = { # string };
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs) \
static const char T_ ## tok ## str[len] = { # string };
#include "special.h"
#include "instruct.h"
static const char T_NULLstr[1] = {"."};
#undef insx
#undef insn
#undef ins
#undef res

// create the 'reserved words' table (AsmResWord).
// this table's entries will populate the instruction hash table.

struct ReservedWord AsmResWord[] = {
#define res(tok, string, len, rm_byte, op2, opcode, flags, cpu, op1) \
    { NULL, T_ ## tok ## str, len, RWF_SPECIAL | flags, T_ ## tok ## _I },
#define ins(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    { NULL, T_ ## tok ## str, len, 0, T_ ## tok ## _I },
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insx(tok,string,len,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flags) \
    { NULL, T_ ## tok ## str, len, flags, T_ ## tok ## _I },
#include "special.h"
#include "instruct.h"
#undef insx
#undef insn
#undef ins
#undef res
    { NULL, T_NULLstr, 1, 0, T_NULL_I }
};

/* parsing of branch instructions with imm operand is in branch.c */
extern ret_code         branch( struct code_info *, expr_list * );

extern bool             DefineProc;
extern struct asm_sym   symPC; /* '$' symbol */
extern bool             in_epilogue;
extern bool             CheckSeg;

// v1.96: CodeInfo has become a stack variable.
// it's more or less the central object for the parser and code generator.
//static struct code_info Code_Info;
//static struct code_info *CodeInfo = &Code_Info;

unsigned                Opnd_Count;
bool                    line_listed;
#if AMD64_SUPPORT
static bool             b64bit = FALSE;
#endif
int_8                   Frame;          // Frame of current fixup
uint_16                 Frame_Datum;    // Frame datum of current fixup
static enum prefix_reg  LastRegOverride;// needed for CMPS
static bool             fInit = FALSE;

/* global queue of "disabled" reserved words */
static struct ReservedWord *RemovedFirst = NULL;
static struct ReservedWord *RemovedTail  = NULL;

static char sr_prefix[] =
    { PREFIX_ES, PREFIX_CS, PREFIX_SS, PREFIX_DS, PREFIX_FS, PREFIX_GS };

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
        case SYM_PROC:
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

void find_frame( struct code_info *CodeInfo, asm_sym *sym )
/*********************************************************/
{
    if( CodeInfo->prefix.SegOverride != NULL )
        SetFixupFrame( CodeInfo->prefix.SegOverride );
    else
        SetFixupFrame( sym );
}

/* set frame if OPTION OFFSET:SEGMENT is on
 */
void find_frame2( struct code_info *CodeInfo, struct asm_sym *sym )
/*****************************************************************/
{
    if( CodeInfo->prefix.SegOverride == NULL &&
       sym->segment != NULL &&
       (sym->state == SYM_INTERNAL || sym->state == SYM_PROC || sym->state == SYM_EXTERNAL ) ) {
        Frame = FRAME_SEG;
        Frame_Datum = GetSegIdx( sym->segment );
    } else {
        find_frame( CodeInfo, sym );
    }
}

static enum assume_segreg Prefix2Assume( enum prefix_reg prefix )
/***************************************************************/
{
    switch( prefix ) {
    case PREFIX_ES:  return( ASSUME_ES );
    case PREFIX_CS:  return( ASSUME_CS );
    case PREFIX_SS:  return( ASSUME_SS );
    case PREFIX_DS:  return( ASSUME_DS );
    case PREFIX_FS:  return( ASSUME_FS );
    case PREFIX_GS:  return( ASSUME_GS );
    }
    return( ASSUME_NOTHING );
}

static void check_assume( struct code_info *CodeInfo, struct asm_sym *sym, enum prefix_reg default_reg )
/******************************************************************************************************/
/* Check if an assumed segment register is found, and
 * set CodeInfo->RegOverride if necessary.
 * called by seg_override().
 */
{
    enum assume_segreg     reg;
    enum assume_segreg     def_reg;
    asm_sym                *assume;

    if( sym && sym->state == SYM_UNDEFINED )
        return;

    switch( default_reg ) {
    case PREFIX_SS:
        def_reg = ASSUME_SS;
        break;
    case PREFIX_DS:
        def_reg = ASSUME_DS;
        break;
    default:
        def_reg = ASSUME_NOTHING;
        break;
    }

    reg = GetAssume( CodeInfo->prefix.SegOverride, sym, def_reg, &assume );
    /* set global vars Frame and Frame_Datum */
    SetFixupFrame( assume );

    if( reg == ASSUME_NOTHING ) {
        if ( sym ) {
            //if( sym->state != SYM_EXTERNAL && sym->state != SYM_PROC && sym->state != SYM_STACK ) {
            /* v1.95: condition changed. Now there's an error msg only if
             * the symbol has an explicite segment.
             */
            if( sym->segment != NULL ) {
                DebugMsg(("no segment register available to access label %s\n", sym->name ));
                AsmErr( CANNOT_ACCESS_LABEL_THROUGH_SEGMENT_REGISTERS, sym->name );
            } else
                CodeInfo->prefix.RegOverride = default_reg;
        } else {
            DebugMsg(("no segment register available to access seg-label %s\n", CodeInfo->prefix.SegOverride->name ));
            AsmErr( CANNOT_ACCESS_LABEL_THROUGH_SEGMENT_REGISTERS, CodeInfo->prefix.SegOverride->name );
        }
    } else if( default_reg != EMPTY ) {
        CodeInfo->prefix.RegOverride = sr_prefix[ reg ];
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
    enum prefix_reg     default_seg;
    enum assume_segreg  assume_seg;
    asm_sym             *assume;

    /* don't touch segment overrides for string instructions */
    /* fixme: what about SSE2 CMPSD/MOVSD? */
    if ( AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REP ||
         AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REPxx )
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
        default_seg = PREFIX_SS;
        break;
    default:
        default_seg = PREFIX_DS;
    }

    if( CodeInfo->prefix.RegOverride != EMPTY ) {
        assume_seg = Prefix2Assume( CodeInfo->prefix.RegOverride );
        assume = GetOverrideAssume( assume_seg );
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
            else
                SET_ADRSIZ( CodeInfo, ModuleInfo.defOfssize );
        }
    } else {
        if ( sym || CodeInfo->prefix.SegOverride )
            check_assume( CodeInfo, sym, default_seg );
        if ( sym == NULL && CodeInfo->prefix.SegOverride ) {
            SET_ADRSIZ( CodeInfo, GetSymOfssize( CodeInfo->prefix.SegOverride ) );
        }
    }

    if( CodeInfo->prefix.RegOverride == default_seg ) {
        CodeInfo->prefix.RegOverride = EMPTY;
    }
}

int OperandSize( OPNDTYPE opnd, struct code_info *CodeInfo )
/**********************************************************/
{
    if( opnd == OP_NONE ) {
        return( 0 );
    } else if( opnd == OP_M ) {
        return( SizeFromMemtype( CodeInfo->mem_type, CodeInfo->Ofssize ) );
    } else if( opnd & ( OP_M8_R8 | OP_M_B | OP_I8 | OP_I_1 | OP_I_3 | OP_I8_U ) ) {
        return( 1 );
    } else if( opnd & ( OP_M16_R16 | OP_M_W | OP_I16 | OP_SR ) ) {
        return( 2 );
    } else if( opnd & ( OP_R32 | OP_M_DW | OP_I32 ) ) {
        return( 4 );
#if AMD64_SUPPORT
    } else if( opnd & ( OP_R64 | OP_M_QW | OP_MMX | OP_I64 ) ) {
#else
    } else if( opnd & ( OP_M_QW | OP_MMX ) ) {
#endif
        return( 8 );
//    } else if( opnd & ( OP_I | OP_J48 ) ) {
    } else if( opnd & OP_J48 ) {
        return( 6 );
    } else if( opnd & ( OP_STI | OP_M_TB ) ) {
        return( 10 );
    } else if( opnd & ( OP_M_OW | OP_XMM ) ) {
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
 Can 'val' be represented in 'bytes' bytes?
 currently called with bytes=1/2 only.
*/
int InRange( long val, unsigned bytes )
/*************************************/
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
    if (bytes == 1)
        return( (val > 255 || val < -255) ? 0 : 1);
    else
        return( (val > 65535 || val < -65535) ? 0 : 1);
#endif
}

#define GetRegNo( x ) AsmOpTable[AsmResWord[x].position].opcode

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
        mod_field = MOD_01;
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
            rex = ( bit3_base << 2 ); /* set REX_R */
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
            if( base < T_EAX ) {
                AsmError( CANNOT_MIX_16_AND_32_BIT_REGISTERS );
                return( ERROR );
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
        }
    }
    if( Opnd_Count == OPND2 ) {
        // shift the register field to left by 3 bit
        CodeInfo->rm_byte = mod_field | ( rm_field << 3 ) | ( CodeInfo->rm_byte & BIT_012 );
#if AMD64_SUPPORT
        CodeInfo->prefix.rex |= (rex >> 2 );
#endif
    } else if( Opnd_Count == OPND1 ) {
        CodeInfo->rm_byte = mod_field | rm_field;
#if AMD64_SUPPORT
        CodeInfo->prefix.rex = rex;
#endif
    }
    return( NOT_ERROR );
}

/*
 set fields in CodeInfo:
 - CodeInfo->mem_type
 - CodeInfo->mem_type_fixed
 - <obsolete: CodeInfo->isfar>
 - CodeInfo->prefix.opsiz
 called by
 - process_branchs
 - memory_operand
*/
static void Set_Memtype( struct code_info *CodeInfo, memtype mem_type, bool fix_mem_type )
/****************************************************************************************/
{
    if( CodeInfo->token == T_LEA )
        return;

    if( mem_type != MT_EMPTY && mem_type != MT_TYPE &&
        CodeInfo->mem_type_fixed == FALSE ) {
        CodeInfo->mem_type = mem_type;
        if( fix_mem_type ) {
            CodeInfo->mem_type_fixed = TRUE;
            // next 2 lines commented out for v1.95
            //if( ( mem_type == MT_FAR ) && IS_JMPCALL( CodeInfo->token ) )
            //    CodeInfo->isfar = TRUE;
        }
    }
    if( CodeInfo->Ofssize > USE16 && ( IS_MEM_TYPE( CodeInfo->mem_type, WORD ) ) ) {
        /* if we are in use32 mode, we have to add OPSIZ prefix for
         most of the 386 instructions ( except MOVSX and MOVZX )
         when we find WORD PTR
         */
        if( !IS_BRANCH( CodeInfo->token ) ) {
            switch( CodeInfo->token ) {
            case T_MOVSX:
            case T_MOVZX:
                break;
            default:
                CodeInfo->prefix.opsiz = TRUE;
                break;
            }
        }

    } else if( CodeInfo->Ofssize == USE16 && ( IS_MEM_TYPE( CodeInfo->mem_type, DWORD ) ) ) {

        /* if we are not in use32 mode, we have to add OPSIZ
         * when we find DWORD PTR
         * unless we have a LxS ins.
         * which moves a DWORD ptr into SR:word reg
         * fixme  - can this be done by breaking up the LXS instructions in
         *          instruct.h, and then putting F_32 or F_16 to append
         *      opsize bytes when necessary ?
         */
        if( !IS_BRANCH( CodeInfo->token ) ) {

            switch( CodeInfo->token ) {
            case T_LDS:
            case T_LES:
            case T_LFS:
            case T_LGS:
            case T_LSS:
                /* in these cases, opsize does NOT need to be changed  */
                break;
            default:
                CodeInfo->prefix.opsiz = TRUE;
                break;
            }
        }
    }
    return;
}

/* set segment override info.
 It's either
 - a register:       set CodeInfo->prefix.RegOverride
 - a SEG/GRP symbol: set CodeInfo->prefix.SegOverride
 this function is also called by data_item()
 */

ret_code segm_override( struct code_info *CodeInfo, expr_list *opndx )
/********************************************************************/
{
    struct asm_sym      *sym;

    if( opndx->override != EMPTY ) {
        if( AsmBuffer[opndx->override]->token == T_REG ) {
            int temp = AsmOpTable[AsmResWord[AsmBuffer[opndx->override]->value].position].opcode;
            if ( SegAssumeTable[temp].error ) {
                DebugMsg(("segm_override: assume error, reg=%u\n", temp ));
                AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
                return( ERROR );
            }
            /* hack: save the previous reg override value (needed for CMPS) */
            LastRegOverride = CodeInfo->prefix.RegOverride;
            CodeInfo->prefix.RegOverride = sr_prefix[temp];
        } else {
            sym = SymSearch( AsmBuffer[opndx->override]->string_ptr );
            if ( sym &&
                 sym->state == SYM_GRP || sym->state == SYM_SEG ) {
                CodeInfo->prefix.SegOverride = sym;
            } else {
                /* override is NOT a SEG or GROUP symbol. Shouldn't happen,
                 the error condition is handled inside the expr. eval.
                 */
                AsmError( SYNTAX_ERROR );
                return( ERROR );
            }
        }
    }
    return( NOT_ERROR );
}

static ret_code process_branchs( struct code_info *CodeInfo, expr_list *opndx )
/*****************************************************************************/
/*
 parse the branchs instructions operand.
 called by idata_nofixup() and idata_fixup().
 called for CALL <imm_op>, JMP <imm_op>, Jxx, JxCXZ, LOOPxx.
*/
{
    /*
     Masm checks overrides for branch instructions with immediate operand.
     However, no segment prefix byte is emitted.
     */
    if( opndx->override != EMPTY ) {
        if ( segm_override( CodeInfo, opndx ) == NOT_ERROR ) {
            if ( CodeInfo->prefix.RegOverride != EMPTY ) {
                enum assume_segreg assume_seg = Prefix2Assume( CodeInfo->prefix.RegOverride );
                asm_sym *assume = GetOverrideAssume( assume_seg );
                /* clear the segment prefix */
                CodeInfo->prefix.RegOverride = EMPTY;
                if ( opndx->sym && opndx->sym->segment && assume != opndx->sym->segment ) {
                    if ( opndx->sym )
                        AsmErr( CANNOT_ACCESS_LABEL_THROUGH_SEGMENT_REGISTERS, opndx->sym->name );
                    else
                        AsmErr( CANNOT_ACCESS_LABEL_THROUGH_SEGMENT_REGISTERS, "" );
                }
            }
        }
    }

    Set_Memtype( CodeInfo, opndx->mem_type, opndx->explicit );
    if( opndx->mbr != NULL ) {
        Set_Memtype( CodeInfo, opndx->mbr->mem_type, FALSE );
    }

    if ( branch( CodeInfo, opndx ) == NOT_ERROR )
        return( NOT_ERROR );
    /* branch() might have returned ERROR or INDIRECT_JMP */
    return( ERROR );
}

static ret_code idata_nofixup( struct code_info *CodeInfo, expr_list *opndx )
/***************************************************************************/
{
    OPNDTYPE    op_type;
#if AMD64_SUPPORT
    int_64      value;
#else
    long        value;
#endif
    int         size;

    DebugMsg(("idata_nofixup(type=%u mem_type=%u value=%X) enter [CodeInfo->mem_type=%Xh]\n", opndx->type, opndx->mem_type, opndx->value, CodeInfo->mem_type));

     // jmp/call/jxx/loop/jcxz/jecxz?
    if( IS_ANY_BRANCH( CodeInfo->token ) ) { 
        return( process_branchs( CodeInfo, opndx ) );
    }
#if AMD64_SUPPORT
    value = opndx->value64;
#else
    value = opndx->value;
#endif
    CodeInfo->data[Opnd_Count] = value;

    switch( CodeInfo->mem_type ) {
    case MT_EMPTY:
        /* don't set the CodeInfo->mem_type!
         */
        if( CodeInfo->token == T_PUSH ) {
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
                if( (int_8)value == (int_32)value ) {
                    op_type = OP_I8;
                } else {
                    op_type = OP_I32;
                }
            } else {
                /* 16bit mode without typecast. extend to DWORD only if
                 the value cannot be expressed as 16bit */
                if( (int_8)value == (int_32)value ) {
                    op_type = OP_I8;
                } else if( (unsigned long)value > USHRT_MAX ) {
                    SET_OPSIZ_32( CodeInfo );
                    op_type = OP_I32;
                } else {
                    op_type = OP_I16;
                }
            }
            break;
        } else if( CodeInfo->token == T_PUSHW ) {
            op_type = OP_I16;
            if( (int_8)value == (int_16)value ) {
                op_type = OP_I8;
            }
            break;
        } else if( CodeInfo->token == T_PUSHD ) {
            op_type = OP_I32;
            if( (int_8)value == (int_32)value ) {
                op_type = OP_I8;
            }
            break;
        } else {
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
                if (size >= 1 && size <= 4) {
                    break;
                }
            }
        }
        /* changing this code is dangerous! */
        /* creation of the short, signed forms rely on this info */
#if AMD64_SUPPORT
        if( value > ULONG_MAX ) {
            op_type = OP_I64;
            if ( Opnd_Count < OPND3 )
                CodeInfo->data[Opnd_Count+1] = value >> 32;
        } else
#endif
        if( ( value > SHRT_MAX ) || ( value < SHRT_MIN ) ) {
//        if( ( value > USHRT_MAX ) || ( value < SHRT_MIN ) ) {
            op_type = OP_I32;
        } else if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
//        } else if( ( value > UCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            op_type = OP_I16;
        } else {
            op_type = OP_I8;
        }
        break;
    case MT_FAR: /* most likely dead code */
        DebugMsg(("idata_nofixup, MT_FAR branch\n" ));
#if 0 /* v1.96: commented out */
        if( CodeInfo->Ofssize == USE16 ) {
            op_type = OP_J32;
        } else {
            op_type = OP_J48;
        }
        break;
#endif
    case MT_NEAR: /* most likely dead code */
        DebugMsg(("idata_nofixup, MT_NEAR branch\n" ));
#if 0 /* v1.96: commented out */
        if( CodeInfo->Ofssize == USE16 ) {
            op_type = OP_I16;
        } else {
            op_type = OP_I32;
        }
        break;
#endif
    case MT_SHORT:
#if 0
        /* v1.96: cannot imagine that SHORT is useful outside branch()
         * instruction context (LEA?). This code is probably never reached. */
        if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            // expect 8-bit but got 16 bit
            DebugMsg(("idata_nofixup: jump out of range, value=%d\n", value ));
            AsmErr( JUMP_OUT_OF_RANGE, value - SCHAR_MAX );
            return( ERROR );
        } else {
            op_type = OP_I8;
        }
        break;
#else
        DebugMsg(("idata_nofixup: mem_type=MT_SHORT\n" ));
        AsmErr( INVALID_INSTRUCTION_OPERANDS );
        return( ERROR );
#endif
    case MT_BYTE:
        if( !InRange( value, 1 ) ) {
            // expect 8-bit but got 16/32 bit
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        } else {
            op_type = OP_I8;
        }
        break;
    case MT_SBYTE:
        if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        } else {
            op_type = OP_I8;
        }
        break;
    case MT_SWORD:
        if( ( value > SHRT_MAX ) || ( value < SHRT_MIN ) ) {
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        } else if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            op_type = OP_I16;
        } else {
            op_type = OP_I8;
        }
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
    case MT_WORD:
        if( Options.sign_value ) {
            if( !InRange( value, 2 ) ) {
                AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
                return( ERROR );
            } else if( value > UCHAR_MAX ) {
                op_type = OP_I16;
            } else {
                op_type = OP_I8;
            }
        } else {
            if( !InRange( value, 2 ) ) {
                AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
                return( ERROR );
            } else if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I16;
            } else {
                op_type = OP_I8;
            }
        }
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
   case MT_SDWORD:
        if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            op_type = OP_I32;
        } else {
            op_type = OP_I8;
        }
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
    case MT_DWORD:
        if( Options.sign_value ) {
            if( value > UCHAR_MAX ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
        } else {
            if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
        }
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
#if AMD64_SUPPORT
    case MT_REAL8:
        op_type = OP_I64;
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
#endif
    case MT_QWORD:
        if( Options.sign_value ) {
            if( value > UCHAR_MAX ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
        } else {
            if ( value <= UCHAR_MAX ) {
                op_type = OP_I8;
#if AMD64_SUPPORT
            } else if( value > ULONG_MAX ) {
                op_type = OP_I64;
                if ( Opnd_Count < OPND3 )
                    CodeInfo->data[Opnd_Count+1] = value >> 32;
#endif
            } else
                op_type = OP_I32;
        }
        CodeInfo->opcode |= W_BIT; // set w-bit
        break;
    default: /* shouldn't happen? */
        op_type = OP_I32;
        CodeInfo->opcode |= W_BIT; // set w-bit
        DebugMsg(("idata_nofixup, default branch\n" ));
        break;
    }
    CodeInfo->opnd_type[Opnd_Count] = op_type;
    DebugMsg(("idata_nofixup exit, op_type=%X [CodeInfo->mem_type=%Xh]\n", op_type, CodeInfo->mem_type));
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
        return( process_branchs( CodeInfo, opndx ) );
    }
    CodeInfo->data[Opnd_Count] = opndx->value;
    segm_override( CodeInfo, opndx );

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
#if SECRELSUPP
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
            if( opndx->mem_type == MT_SHORT ) {
                AsmErr( INVALID_INSTRUCTION_OPERANDS );
                return( ERROR );
            }
            switch ( CodeInfo->token ) {
            case T_PUSHW:
            case T_PUSHD:
            case T_PUSH:
                /* for forward reference, assume BYTE */
                if ( opndx->mem_type == MT_EMPTY ) {
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

    /* set global vars Frame and Frame_Datum */
    if ( ModuleInfo.offsettype == OT_SEGMENT &&
        ( opndx->instr == T_OFFSET || opndx->instr == T_SEG ))
        find_frame2( CodeInfo, opndx->sym );
    else
        find_frame( CodeInfo, opndx->sym );


    DebugMsg(("idata_fixup: calling AddFixup(%s, %u)\n", opndx->sym->name, fixup_type ));
    CodeInfo->InsFixup[Opnd_Count] = AddFixup( opndx->sym, fixup_type, OPTJ_NONE );

    if (opndx->instr == T_LROFFSET)
        CodeInfo->InsFixup[Opnd_Count]->loader_resolved = TRUE;

#if IMAGERELSUPP
    if (opndx->instr == T_IMAGEREL && fixup_type == FIX_OFF32)
        CodeInfo->InsFixup[Opnd_Count]->type = FIX_OFF32_IMGREL;
#endif
#if SECRELSUPP
    if (opndx->instr == T_SECTIONREL && fixup_type == FIX_OFF32)
        CodeInfo->InsFixup[Opnd_Count]->type = FIX_OFF32_SECREL;
#endif
    DebugMsg(("idata_fixup exit [CodeInfo.mem_type=%Xh, Ofssize=%u, fixup->frame=%d]\n", CodeInfo->mem_type, CodeInfo->Ofssize, CodeInfo->InsFixup[Opnd_Count]->frame ));

    return( NOT_ERROR );
}

static void add_frame( struct code_info *CodeInfo )
/*************************************************/
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
    unsigned char       base_lock = FALSE;
    unsigned char       Ofssize;
    enum fixup_types    fixup_type;

    DebugMsg(("memory_operand(opndx->value=%X, memtype=%Xh, fixup=%u) enter, [CodeInfo->memtype=%Xh, Ofssize=%u]\n",
              opndx->value, opndx->mem_type, with_fixup, CodeInfo->mem_type, CodeInfo->Ofssize ));
    CodeInfo->data[Opnd_Count] = opndx->value;
    CodeInfo->opnd_type[Opnd_Count] = OP_M; //OP_M == 0x870000
    sym = opndx->sym;

    segm_override( CodeInfo, opndx );

#if 1
    /* convert MT_PTR to MT_WORD, MT_DWORD, MT_FWORD, MT_QWORD. */
    /* MT_PTR cannot be set explicitely (by the PTR operator),
     so this value must come from a label or a structure field.
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
    Set_Memtype( CodeInfo, opndx->mem_type, opndx->explicit );
    if( opndx->mbr != NULL ) {
        /* if the struct field is just another struct, use it's total size
         to set the operand size.
         */
        if ( opndx->mbr->mem_type == MT_TYPE ) {
            memtype mem_type;
            if ( MemtypeFromSize( opndx->mbr->total_size, &mem_type ) == NOT_ERROR )
                Set_Memtype( CodeInfo, mem_type, FALSE );
        } else
            Set_Memtype( CodeInfo, opndx->mbr->mem_type, FALSE );
    }

    // check for base registers

    if (opndx->base_reg != EMPTY ) {
        base = AsmBuffer[opndx->base_reg]->value;
        j = AsmResWord[base].position;
        if ( ( ( AsmOpTable[j].opnd_type[1] & OP_R32) && CodeInfo->Ofssize == USE32 ) ||
#if AMD64_SUPPORT
            ( ( AsmOpTable[j].opnd_type[1] & OP_R64) && CodeInfo->Ofssize == USE64 ) ||
#endif
            ( ( AsmOpTable[j].opnd_type[1] & OP_R16) && CodeInfo->Ofssize == USE16 ) )
            CodeInfo->prefix.adrsiz = 0;
        else {
            CodeInfo->prefix.adrsiz = 1;
#if AMD64_SUPPORT
            /* 16bit addressing modes don't exist in long mode */
            if ( ( AsmOpTable[j].opnd_type[1] & OP_R16) && CodeInfo->Ofssize == USE64 ) {
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
        j = AsmResWord[index].position;
        if ( ( ( AsmOpTable[j].opnd_type[1] & OP_R32) && CodeInfo->Ofssize == USE32 ) ||
#if AMD64_SUPPORT
            ( ( AsmOpTable[j].opnd_type[1] & OP_R64) && CodeInfo->Ofssize == USE64 ) ||
#endif
            ( ( AsmOpTable[j].opnd_type[1] & OP_R16) && CodeInfo->Ofssize == USE16 ) ) {
            CodeInfo->prefix.adrsiz = 0;
        } else {
            CodeInfo->prefix.adrsiz = 1;
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
        }
    }

    if( with_fixup ) {

        if ( sym ) {
            switch( sym->state ) {
            case SYM_UNDEFINED:
                // forward reference
                break;
            case SYM_SEG:
            case SYM_GRP:
                // error !!!!!
                break;
            default:
                /* don't use symbol's size if OFFSET is involved! */
                /* example: mov eax,[esi+offset buffer] */
                // if( CodeInfo->mem_type == MT_EMPTY ) {
                if( CodeInfo->mem_type == MT_EMPTY && opndx->instr == EMPTY ) {
                    Set_Memtype( CodeInfo, sym->mem_type, FALSE );
                }
                break;
            }
        }

        if( opndx->abs ) {
            Ofssize = IS_ADDR32( CodeInfo );
        } else if ( sym ) {
            Ofssize = GetSymOfssize( sym );
        } else if ( CodeInfo->prefix.SegOverride ) {
            Ofssize = GetSymOfssize( CodeInfo->prefix.SegOverride );
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
            GetAssume( CodeInfo->prefix.SegOverride, sym, ASSUME_NOTHING, &assume);
            SetFixupFrame( assume );
        } else {
            if( mem2code( CodeInfo, ss, index, base, sym ) == ERROR ) {
                return( ERROR );
            }
        }
        /* set again current fixup in CodeInfo */
        add_frame( CodeInfo );

    } else { /* branch without fixup */
#if 1
        /* if an undefined label is moved (mov [xxx],offset yyy)
         * change to immediate data. (this might be placed better in
         * check_size()! )
         */
        if (sym && sym->state == SYM_UNDEFINED &&
            CodeInfo->token == T_MOV &&
            Opnd_Count == OPND2 &&
            (CodeInfo->opnd_type[OPND1] & OP_M_ANY) )
            CodeInfo->opnd_type[Opnd_Count] = OP_I8;
#endif
        if (sym && sym->state == SYM_STACK) {
            if( base != EMPTY ) {
                if( base_lock == TRUE ) {
                    // [reg + data][reg + data] is not allowed
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
            base_lock = TRUE;   // add lock
            if( CodeInfo->mem_type == MT_EMPTY ) {
                Set_Memtype( CodeInfo, sym->mem_type, FALSE );
            }
        }
        if( mem2code( CodeInfo, ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
    } /* end if ( with_fixup ) */

    switch ( CodeInfo->token ) {
    case T_JMP:
    case T_CALL:
        /* v1.95: convert MT_NEAR/MT_FAR and display error if no type.
         * For memory operands, expressions of type MT_NEAR/MT_FAR are
         * call [bx+<code_label>]
         */
        switch ( CodeInfo->mem_type ) {
        case MT_EMPTY:
            if (Parse_Pass > PASS_1) {
                AsmError( INSTRUCTION_OPERAND_MUST_HAVE_SIZE );
                return( ERROR );
            }
            /* fall through */
        case MT_NEAR:
            //CodeInfo->mem_type = 2 << CodeInfo->Ofssize; /* might be better */
            CodeInfo->mem_type = CodeInfo->Ofssize ? MT_DWORD : MT_WORD;
            break;
        case MT_FAR:
            //CodeInfo->mem_type = 2 + (2 << CodeInfo->Ofssize); /* might be better */
            CodeInfo->mem_type = CodeInfo->Ofssize ? MT_FWORD : MT_DWORD;
            break;
        }

        j = SizeFromMemtype(CodeInfo->mem_type, ModuleInfo.Ofssize );
        /* the next check is probably not necessary anymore, because
         * the entry with OP_M in instruct.h has been removed.
         */
#if AMD64_SUPPORT
        if ( j == 1 || j > 8 || ( CodeInfo->Ofssize == USE64 && j == 4 ) ) {
#else
        if ( j == 1 || j > 6 ) {
#endif
            /* CALL/JMP possible for WORD/DWORD/FWORD memory operands only */
            DebugMsg(("memory_operand: invalid operand, size=%u\n", j ));
            AsmError( INVALID_INSTRUCTION_OPERANDS );
            return( ERROR );
        }
        if( opndx->mem_type == MT_FAR || CodeInfo->mem_type == MT_FWORD ||
            ( CodeInfo->mem_type == MT_DWORD &&
              ((CodeInfo->Ofssize == USE16 && opndx->Ofssize != USE32) ||
               (CodeInfo->Ofssize == USE32 && opndx->Ofssize == USE16 )))) {
            CodeInfo->isfar = TRUE;
        }
        break;
    case T_PUSH:
    case T_POP:
        j = SizeFromMemtype( CodeInfo->mem_type, ModuleInfo.Ofssize );
#if AMD64_SUPPORT
        if ( j == 1 || j > CurrWordSize ) {
#else
        if ( j == 1 || j > 4 ) {
#endif
            /* PUSH/POP possible for WORD or DWORD|QWORD memory operands only */
            DebugMsg(("memory_operand: PUSH/POP invalid operand, size=%u\n", size ));
            AsmError(INVALID_INSTRUCTION_OPERANDS);
            return(ERROR);
        }
    }
    DebugMsg(("memory_operand exit, no error, opndx.type/value=%Xh/%Xh, CodeInfo.memtype=%Xh opndtype=%Xh fix=%Xh\n",
              opndx->type, opndx->value, CodeInfo->mem_type, CodeInfo->opnd_type[Opnd_Count], CodeInfo->InsFixup[Opnd_Count] ));
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

        DebugMsg(("process_address: INDIRECT, sym=%X, adrsiz=%u\n", opndx->sym, CodeInfo->prefix.adrsiz ));
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
            return( process_branchs( CodeInfo, opndx ) );

        switch( CodeInfo->token ) {
        case T_PUSH:
        case T_PUSHW:
        case T_PUSHD:
            //return( idata_nofixup( CodeInfo, opndx ) ); /* v1.96: changed */
            return( idata_fixup( CodeInfo, opndx ) );
        }
        return( memory_operand( CodeInfo, opndx, FALSE ) );

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
            /* fixme: fill the rex prefix */
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

static ret_code process_register( struct code_info *CodeInfo, expr_list *opndx )
/******************************************************************************/
/*
- parse and encode direct register operands;
*/
{
    int                 temp;
    int                 reg;

    DebugMsg(( "process_register enter (%s)\n", AsmBuffer[opndx->base_reg]->string_ptr ));
    temp = AsmResWord[AsmBuffer[opndx->base_reg]->value].position;
    /* the register number is stored in opcode field */
    reg = AsmOpTable[temp].opcode;
    /* the "OP-name" of the register is stored in opnd_type[OPND2] */
    CodeInfo->opnd_type[Opnd_Count] = AsmOpTable[temp].opnd_type[OPND2];
    switch( AsmOpTable[temp].opnd_type[OPND2] ) {
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
        if( CodeInfo->Ofssize )
            CodeInfo->prefix.opsiz = TRUE;
        break;
    case OP_EAX:
    case OP_R32:
        DebugMsg(("process_register, R32, reg=%u\n", reg ));
#if 0 /* is checked in expression evaluator now */
        if( ( ModuleInfo.curr_cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
            // 8086 ins cannot use 386 register
            AsmError( CANNOT_USE_386_REGISTER_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
#endif
        CodeInfo->opcode |= W_BIT;             // set w-bit
        if( CodeInfo->Ofssize == USE16 )
            CodeInfo->prefix.opsiz = TRUE;
        break;
#if AMD64_SUPPORT
    case OP_RAX:
    case OP_R64:
        DebugMsg(("process_register, R64, reg=%u\n", reg ));
        CodeInfo->opcode |= W_BIT;             // set w-bit
        if( CodeInfo->Ofssize == USE16 )
            CodeInfo->prefix.opsiz = TRUE;
        CodeInfo->prefix.rex |= REX_W;
        break;
#endif
    case OP_SR386:  // 386 segment register
#if 0 /* is checked in expression evaluator now */
        if( ( ModuleInfo.curr_cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
            // 8086 ins cannot use 80386 segment register
            AsmError( CANNOT_USE_386_SEGMENT_REGISTER_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
#endif
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

static void HandleStringInstructions( struct code_info *CodeInfo, expr_list *opndx )
/**********************************************************************************/
{
    int opndidx = OPND1;

    switch( CodeInfo->token ) {
    case T_CMPSD:
        /* filter SSE2 opcode CMPSD */
        if ( CodeInfo->opnd_type[OPND1] & (OP_XMM | OP_MMX))
            return;
        /* fall through */
    case T_CMPS:
    case T_CMPSB:
    case T_CMPSW:
         /* cmps allows prefix for the first operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY ) {
            if ( opndx->override != EMPTY ) {
                if ( CodeInfo->prefix.RegOverride == PREFIX_ES )
                    /* content of LastRegOverride is valid if
                     * CodeInfo->RegOverride is != EMPTY.
                     */
                    CodeInfo->prefix.RegOverride = LastRegOverride;
                else {
                    DebugMsg(("HandleStringInstructions: CMPS: CodeInfo->RegOverride=%X, opndx->override=%s\n", CodeInfo->prefix.RegOverride, AsmBuffer[opndx->override]->string_ptr ));
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                }
            }
        }
        break;
    case T_MOVSD:
        /* filter SSE2 opcode MOVSD */
        if ( ( CodeInfo->opnd_type[OPND1] & (OP_XMM | OP_MMX) ) ||
            ( CodeInfo->opnd_type[OPND2] & (OP_XMM | OP_MMX) ) )
            return;
        /* fall through */
    case T_MOVS:
    case T_MOVSB:
    case T_MOVSW:
        /* movs allows prefix for the second operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY )
            if ( opndx->override == EMPTY )
                AsmError( INVALID_INSTRUCTION_OPERANDS );
            else if ( CodeInfo->prefix.RegOverride == PREFIX_DS )
                CodeInfo->prefix.RegOverride = EMPTY;
        break;
    case T_OUTS:
    case T_OUTSB:
    case T_OUTSW:
    case T_OUTSD:
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
            if (CodeInfo->prefix.RegOverride == PREFIX_ES)
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

static ret_code check_size( struct code_info *CodeInfo, expr_list * opndx )
/*************************************************************************/
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
            AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, OperandSize(op1, CodeInfo ), ModuleInfo.Ofssize ? 4 : 2);
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
        // checking will be done later
        break;
    case T_LDS:
    case T_LES:
    case T_LFS:
    case T_LGS:
    case T_LSS:
        op1_size = OperandSize( op1, CodeInfo ) + 2; /* add 2 for the impl. segment register */
        op2_size = OperandSize( op2, CodeInfo );
        if (op2_size != 0 && op1_size != op2_size) {
            AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, op1_size, op2_size );
            return( ERROR );
        }
        break;
    case T_ENTER:
        // ENTER has to be OP_I16, OP_I8
        if( op1 == OP_I32 ) {
            //parse_phase_1 will treat 16-bit data as OP_I32 if CPU is 386
            if( CodeInfo->data[OPND1] > (signed long)USHRT_MAX ) {
                // if op1 is really 32-bit data, then error
                AsmError( IMMEDIATE_DATA_TOO_BIG );
                rc = ERROR;
            }
        }
        // type cast op1 to OP_I16
        CodeInfo->opnd_type[OPND1] = OP_I16;
        // op2 have to be 8-bit data
        if( op2 >= OP_I16 ) {
            if( CodeInfo->data[OPND2] > UCHAR_MAX ) {
                AsmError( IMMEDIATE_DATA_TOO_BIG );
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
        DebugMsg(("check_size, MOVZX/MOVSX: op2_size=%u, opndx.memtype=%u\n", op2_size, opndx->mem_type));
        if (op2_size == 0 && Parse_Pass == PASS_2)
            if (op1_size == 2)
                AsmWarn(2, SIZE_NOT_SPECIFIED_ASSUMING, "BYTE" );
            else
                AsmWarn(2, INSTRUCTION_OPERAND_MUST_HAVE_SIZE);
        switch( op1_size ) {
        case 4:
            if (op2_size < 2)
                ;
            else if (op2_size == 2)
                CodeInfo->opcode |= W_BIT;
            else {
                AsmError( OP2_TOO_BIG );
                rc = ERROR;
            }
            if( CodeInfo->Ofssize ) {
                CodeInfo->prefix.opsiz = FALSE;     // - don't need opnd size prefix
            }
            break;
        case 2:
            if( op2_size >= 2 ) {
                AsmError( OP2_TOO_BIG );
                rc = ERROR;
            }
            break;
#if AMD64_SUPPORT
        case 8:
            if ( CodeInfo->Ofssize == USE64 )
                break;
#endif
        default:
            // op1 have to be r16/r32
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
            AsmError( INVALID_SIZE );
            return( ERROR );
        }
        op2_size = OperandSize( op2, CodeInfo );
        switch( op2_size ) {
        case 2:
        case 4:
            break;
        default:
            AsmError( INVALID_SIZE );
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
        } else if( ( op1 == OP_M ) || ( op2 == OP_M ) ) {
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
    DebugMsg(("check_size exit [CodeInfo->mem_type=%u]\n", CodeInfo->mem_type));
    return( rc );
}

/*
 this is the parser, which scans through the tokens in AsmBuffer[]
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
    CodeInfo->prefix.SegOverride = NULL;
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

    Opnd_Count = OPND1;
    CheckSeg = TRUE;
    Frame = EMPTY;
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
        if ((AsmBuffer[2]->token == T_COLON) || (CurrProc == NULL)) {
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
            /* if next token is a directive, it can't be an anonymous data item */
            /* this also filters the "xxx STRUCT" and "xxx ENDS" cases */
            if( ( AsmBuffer[i+1]->token != T_DIRECTIVE ) &&
                ( sym = IsLabelType( AsmBuffer[i]->string_ptr ))) {
                DebugMsg(("ParseItems anonymous data item >%s<\n", AsmBuffer[i]->string_ptr ));
                return( data_init( CodeInfo, label, i, (dir_node *)sym ) );
            }
            label = i;
            i++;
        }
        switch ( AsmBuffer[i]->token ) {
        case T_DIRECTIVE:
            DebugMsg(("ParseItems T_DIRECTIVE >%s<\n", AsmBuffer[i]->string_ptr ));
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
            if ( ModuleInfo.list && line_listed == FALSE && Parse_Pass == PASS_1 )
                LstWriteSrcLine();
            return( temp );
        case T_RES_ID:
            DebugMsg(("ParseItems T_RES_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if ( AsmBuffer[i]->rm_byte == RWT_TYPE )
                return( data_init( CodeInfo, label, i, NULL ) );
            break;
        case T_ID:
            DebugMsg(("ParseItems T_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if( sym = IsLabelType( AsmBuffer[i]->string_ptr ) ) {
                return( data_init( CodeInfo, label, i, (dir_node *)sym ) );
            }
            break;
        default:
            if ( AsmBuffer[i]->token == T_COLON ) {
                DebugMsg(("ParseItems unexpected colon\n" ));
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
                return( ERROR );
            }
            break;
        } /* end switch (AsmBuffer[i]->token) */
        if ( label != -1 )
            i--;
        DebugMsg(("ParseItems unexpected token=%u, i=%u, string=%s\n", AsmBuffer[i]->token, i, AsmBuffer[i]->string_ptr));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    DebugMsg(("ParseItems: %s\n", AsmBuffer[i]->string_ptr));

    // instruction prefix?
    switch( AsmBuffer[i]->value ) {
    case T_LOCK:
    case T_REP:
    case T_REPE:
    case T_REPNE:
    case T_REPNZ:
    case T_REPZ:
        CodeInfo->prefix.ins = AsmBuffer[i]->value;
        // prefix has to be followed by an instruction
        if( AsmBuffer[i+1]->token != T_INSTRUCTION ) {
            DebugMsg(("ParseItems: unexpected token after prefix, exit, error\n"));
            AsmError( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION );
            return( ERROR );
        }
        i++;
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
    CodeInfo->pcurr = &AsmOpTable[AsmResWord[CodeInfo->token].position];
    i++;

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
            }
        }

        DebugMsg(("ParseItems(%s,%u): type/value/mem_type=%Xh/%Xh/%Xh\n", instr, Opnd_Count, opndx.kind, opndx.value, opndx.mem_type));
        switch( opndx.kind ) {
        case EXPR_ADDR:
            DebugMsg(("ParseItems(%s,%u): type ADDRESS\n", instr, Opnd_Count ));
#if 1 /* v1.96: block moved BEFORE process_address() */
            /* assume second operand - if it isn't defined yet and direct -
             is a CONST if first operand is a memory reference
             example: mov [var], label
             */
            if (Opnd_Count == OPND2 &&
                opndx.sym != NULL &&
                opndx.sym->state == SYM_UNDEFINED &&
                Parse_Pass == PASS_1 &&
                opndx.indirect == FALSE &&
                (CodeInfo->opnd_type[OPND1] & OP_M_ANY)) {
                opndx.kind = EXPR_CONST;
                goto process_const;
            }
#endif
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
        process_const:
            DebugMsg(("ParseItems(%s,%u): type CONST, opndx.memtype=%Xh\n", instr, Opnd_Count, opndx.mem_type));
            if (process_const( CodeInfo, &opndx ) == ERROR )
                return( ERROR );
            break;
        case EXPR_REG:
            DebugMsg(("ParseItems(%s,%u): type REG\n", instr, Opnd_Count ));
            if( opndx.indirect ) { /* indirect operand ( "[EBX+...]" )? */
                temp  = process_address( CodeInfo, &opndx );
                if ( temp != NOT_ERROR )
                    return( temp );
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
            CodeInfo->prefix.SegOverride = NULL; /* segreg prefix is stored in RegOverride */
        }
    } /* end while */

    /* for FAR calls/jmps there was a non-compatible solution implemented
     * in WASM which required two additional opcodes, CALLF and JMPF.
     * this has been removed for JWasm, but there's now the need to
     * skip the "near" entries in AsmOpTable for CALL/JMP.
     */
    if ( CodeInfo->isfar ) {
        if ( CodeInfo->token == T_CALL )
            CodeInfo->pcurr += NUMCALLN;
        else if (CodeInfo->token == T_JMP)
            CodeInfo->pcurr += NUMJMPN;
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
            if ( CodeInfo->x86hi_used && ( CodeInfo->x64lo_used || CodeInfo->prefix.rex & 7 ))
                AsmError(INVALID_USAGE_OF_AHBHCHDH );
            if ( CodeInfo->token == T_PUSH || CodeInfo->token == T_POP ) {
                if (( OperandSize( CodeInfo->opnd_type[OPND1], CodeInfo ) == 4 ) ||
                    ( CodeInfo->opnd_type[OPND1] & OP_SR86 ) ) {
                    DebugMsg(("ParseItems: PUSH/POP operand with size 4 or CS/DS/ES/SS\n"));
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                    return( ERROR );
                }
                if ( CodeInfo->opnd_type[OPND1] & OP_R64 )
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

#if AMD64_SUPPORT

static const enum asm_token patchtab64[] = {
    T_SPL,   /* add x64 part of special.h */
    T_JRCXZ, /* branch instructions must be grouped together */
    T_CDQE,  /* add x64 part of instruct.h */
    T_NULL };

static const enum asm_token patchtab32[] = {
    T_AAA,   /* instructions invalid for IA32+ */
    T_JCXZ,  /* 1. branch instructions invalid for IA32+ */
    T_LOOPW, /* 2. branch instructions invalid for IA32+ */
    T_NULL };

struct replace_ins {
    const enum asm_token tok;
    short idx32;
    short idx64;
};

static struct replace_ins patchtabr[] = {
    { T_LGDT, T_LGDT_I, T_LGDT_I64 },
    { T_LIDT, T_LIDT_I, T_LIDT_I64 },
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
    struct replace_ins  *repl;

    if ( newmode != b64bit ) {
        DebugMsg(("Set64Bit(%u): mode is to change\n", newmode ));
        if ( newmode != FALSE ) {
            AsmResWord[T_INC].position++;   /* skip the one-byte register INC */
            AsmResWord[T_DEC].position++;   /* skip the one-byte register DEC */
            /* change calling convention syscall to syscall_ */
            AsmResWord[T_SYSCALL].len++;
            for (ppt = patchtab64; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_X64; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        AddResWord( resw );
            for (ppt = patchtab32; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_IA32; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        RemoveResWord( resw );
            for (ppt = patchtab32; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_IA32; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        RemoveResWord( resw );
            for (repl = patchtabr; repl->tok != T_NULL; repl++ ) {
                resw = &AsmResWord[repl->tok];
                resw->position = repl->idx64;
            }
        } else  {
            AsmResWord[T_INC].position--;   /* restore the one-byte register INC */
            AsmResWord[T_DEC].position--;   /* restore the one-byte register DEC */
            for (ppt = patchtab64; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_X64; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        RemoveResWord( resw );
            for (ppt = patchtab32; *ppt != T_NULL; ppt++ )
                for( resw = &AsmResWord[*ppt]; resw->flags & RWF_IA32; resw++ )
                    if ( !( resw->flags & RWF_DISABLED ) )
                        AddResWord( resw );
            for (repl = patchtabr; repl->tok != T_NULL; repl++ ) {
                resw = &AsmResWord[repl->tok];
                resw->position = repl->idx32;
            }
            /* change calling convention syscall_ back to syscall */
            AsmResWord[T_SYSCALL].len--;
        }
        b64bit = newmode;
    }
}
#endif

void DisableKeyword( struct ReservedWord *resw )
/**********************************************/
{
    if ( !( resw->flags & RWF_DISABLED ) ) {
        RemoveResWord( resw );
        resw->next = NULL;
        resw->flags |= RWF_DISABLED;
        if ( RemovedFirst == NULL )
            RemovedFirst = RemovedTail = resw;
        else {
            RemovedTail->next = resw;
            RemovedTail = resw;
        }
    }
}

/* check if a keyword is in the list of disabled words.
 */

struct ReservedWord * IsKeywordDisabled( char *name, int len )
/************************************************************/
{
    struct ReservedWord *resw;
    for ( resw = RemovedFirst; resw; resw = resw->next )
        if( resw->name[ len ] == NULLC && _memicmp( name, resw->name, len ) == 0)
            return( resw );
    return( NULL );
}

/* ParseInit() is called once per module */

ret_code ParseInit( void )
/************************/
{
    struct ReservedWord *curr;
    struct ReservedWord *last;

    DebugMsg(("ParseInit()\n"));

    /* initialize the AsmBuffer[] table */
    InitTokenBuffer();

    if( fInit == FALSE ) {  // if not initialized
        /* if first call, initialize hash table (IA32 mode) */
        fInit = TRUE;
        for( curr = &AsmResWord[0], last = &AsmResWord[T_NULL]; curr < last; curr++ )
#if AMD64_SUPPORT /* don't add the words specific to x64 */
            if ( !(curr->flags & RWF_X64 ) )
#endif
                AddResWord( curr );
    } else {
        struct ReservedWord *next;
        /* reenter disabled keywords */
        for( curr = RemovedFirst; curr; curr = next ) {
            next = curr->next;
            curr->flags &= ~RWF_DISABLED;
#if AMD64_SUPPORT /* don't add the words specific to x64 */
            if ( !(curr->flags & RWF_X64 ) )
#endif
                AddResWord( curr );
        }
        RemovedFirst = RemovedTail = NULL;
    }
    return( NOT_ERROR );
}
