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
* Description:  instruction encoding, scans opcode table and emits code.
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "codegen.h"
#include "memalloc.h"
#include "fixup.h"
#include "directiv.h"
#include "fpfixup.h"

#define ins(tok,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insa(tok,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#include "instruct.h"

static ret_code match_phase_3( int *i, OPNDTYPE determinant );

static ret_code output_3DNow( int i )
/******************************/
{
    const struct asm_ins *ins = &AsmOpTable[i];

    if( ins->byte1_info == F_0F0F ) {
        OutputCodeByte( ins->opcode | CodeInfo->opcode );
    }
    return( NOT_ERROR );
}

static ret_code output( int i )
/************************/
/*
- determine what code should be output and their order;
- output prefix bytes ( ADRSIZ, OPSIZ, LOCK, REPxx, segment override prefix, etc )
  , opcode, "mod r/m" byte and "s-i-b" byte;
- i: index into AsmOpTable
- possible return codes: ERROR, NOT_ERROR
*/
{
    const struct asm_ins *ins = &AsmOpTable[i];
    struct asm_code      *rCode = CodeInfo;
    uint_8           tmp;

    /*
     * Output debug info - line numbers
     */
    if( Options.line_numbers && write_to_file == TRUE ) {
        AddLinnumDataRef();
    }

    /*
     * Check if FP is valid
     */
    if((( ins->cpu & P_FPU_MASK ) != P_NO87 ) && ( Options.floating_point == FPO_DISABLED )) {
        AsmError( NO_FP_WITH_FPC_SET );
        return( ERROR );
    }
    /*
     * Output FP fixup if required
     */
    if(( ModuleInfo.emulator == TRUE )
        && ( !rCode->use32 )
        && ( ins->allowed_prefix != AP_NO_FWAIT )
        && (( ins->allowed_prefix == AP_FWAIT ) || (( ins->cpu & P_FPU_MASK ) != P_NO87 ))) {
            if( AddFloatingPointEmulationFixup( ins, FALSE ) == ERROR ) {
                return( ERROR );
            }
    }

    /*
     * Check if CPU and FPU is valid for output code
     */
    if( ( ins->cpu & P_CPU_MASK ) > ( ModuleInfo.curr_cpu & P_CPU_MASK )
        || ( ins->cpu & P_FPU_MASK ) > ( ModuleInfo.curr_cpu & P_FPU_MASK )
        || ( ins->cpu & P_EXT_MASK ) > ( ins->cpu & ModuleInfo.curr_cpu & P_EXT_MASK ) ) {
        DebugMsg(("output: wrong cpu setting: instr=%X, Module=%X\n", ins->cpu, ModuleInfo.curr_cpu ));
        /* if instruction is valid for 16bit cpu, but operands aren't,
         then display a more specific error message! */
        if( ins->cpu == P_386 &&
            ( ( AsmOpTable[AsmResWord[CodeInfo->token].position].cpu & P_CPU_MASK ) <= P_386 ))
            AsmError( INSTRUCTION_FORM_REQUIRES_80386 );
        else
            AsmError( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING );
        return( ERROR );
    }

    /*
     * Output instruction prefix LOCK, REP or REPNE
     */
    if( rCode->prefix.ins != EMPTY ) {
        switch( rCode->prefix.ins ) {
        case T_LOCK:
            if( ins->allowed_prefix != AP_LOCK ) {
                AsmError( LOCK_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION );
                return( ERROR );
            }
            break;
        case T_REP:
            if( ins->allowed_prefix != AP_REP ) {
                if (ModuleInfo.m510 == FALSE) {
                    AsmError( REP_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION );
                    return( ERROR );
                }
            }
            break;
        case T_REPZ:
        case T_REPE:
        case T_REPNZ:
        case T_REPNE:
            if( ins->allowed_prefix != AP_REPxx ) {
                AsmError( REPX_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION );
                return( ERROR );
            }
            break;
        default:
            break;
        }
        OutputCodeByte( AsmOpTable[AsmResWord[rCode->prefix.ins].position].opcode );
    }
    /*
     * Output instruction prefix REP or REPNE for SSEx instructions
     */
    switch( ins->byte1_info ) {
    case F_F20F:
        OutputCodeByte( 0xF2 );
        break;
    case F_F3:
    case F_F30F:
        OutputCodeByte( 0xF3 );
        break;
#if 0
    case F_16:
    case F_16A:
    case F_32:
    case F_32A:
    case F_660F:
    case F_0F:
    case F_0FNO66:
    case F_0F0F:
#endif
    default:
        break;
    }

    /*
     * Output FP FWAIT if required
     */
    if( ins->token == T_FWAIT ) {
        if(( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
            if(( ModuleInfo.emulator == TRUE ) && ( !rCode->use32 )) {
                OutputCodeByte( OP_NOP );
            }
        }
    } else if( ins->allowed_prefix == AP_FWAIT ) {
        OutputCodeByte( OP_WAIT );
    } else if(( ModuleInfo.emulator == TRUE )
        && ( !rCode->use32 )
        && ( ins->allowed_prefix != AP_NO_FWAIT )
        && (( ins->allowed_prefix == AP_FWAIT ) || (( ins->cpu & P_FPU_MASK ) != P_NO87 ))) {
        OutputCodeByte( OP_WAIT );
    } else if( ins->allowed_prefix != AP_NO_FWAIT ) {
        // implicit FWAIT synchronization for 8087 (CPU 8086/80186)
        if((( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_286 )
            && (( ins->cpu & P_FPU_MASK ) == P_87 )) {
            OutputCodeByte( OP_WAIT );
        }
    }

    /*
     * Output FP fixup if required
     */
    if(( ModuleInfo.emulator == TRUE )
        && ( !rCode->use32 )
        && ( ins->allowed_prefix != AP_NO_FWAIT )
        && (( ins->allowed_prefix == AP_FWAIT ) || (( ins->cpu & P_FPU_MASK ) != P_NO87 ))) {
        if( AddFloatingPointEmulationFixup( ins, TRUE ) == ERROR ) {
            return( ERROR );
        }
    }
    /*
     * check if address/operand size prefix is to be set
     */
    switch( ins->byte1_info ) {
    case F_16:
        if( rCode->use32 ) rCode->prefix.opsiz = TRUE;
        break;
    case F_16A:
        if( rCode->use32 ) rCode->prefix.adrsiz = TRUE;
        break;
    case F_32:
        if( !rCode->use32 ) rCode->prefix.opsiz = TRUE;
        break;
    case F_32A:
        if( !rCode->use32 ) rCode->prefix.adrsiz = TRUE;
        break;
    case F_660F:
        rCode->prefix.opsiz = TRUE;
        break;
    case F_0FNO66:
        rCode->prefix.opsiz = FALSE;
        break;
#if 0
    case F_0F:
    case F_0F0F:
    case F_F20F:
    case F_F30F:
#endif
    default:
        break;
    }
    /*
     * Output address and operand size prefixes
     */
    if( rCode->prefix.adrsiz == TRUE ) {
        OutputCodeByte( ADRSIZ );
    }
    if( rCode->prefix.opsiz == TRUE ) {
#if 1
        if(( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
            DebugMsg(("output: instruction form requires 386\n"));
            AsmError( INSTRUCTION_FORM_REQUIRES_80386 );
            return( ERROR );
        }
#endif
        OutputCodeByte( OPSIZ );
    }
    /*
     * Output segment prefix
     */
    if( rCode->prefix.RegOverride != EMPTY ) {
        OutputCodeByte( rCode->prefix.RegOverride );
    }
    /*
     * Output extended opcode
     * special case for some 286 and 386 instructions
     * or 3DNow!, MMX and SSEx instructions
     */
    switch( ins->byte1_info ) {
    case F_0F0F:
        OutputCodeByte( EXTENDED_OPCODE );
        // no break
    case F_0F:
    case F_0FNO66:
    case F_660F:
    case F_F20F:
    case F_F30F:
        OutputCodeByte( EXTENDED_OPCODE );
        break;
    //case F_F3:
    default:
        break;
    }
    if( ins->opnd_dir ) {
        /* The reg and r/m fields are backwards */
        tmp = rCode->rm_byte;
        rCode->rm_byte = ( tmp & 0xc0 ) | ((tmp >> 3) & 0x7) | ((tmp << 3) & 0x38);
    }
    switch( ins->rm_info ) {
    case R_in_OP:
        OutputCodeByte( ins->opcode | ( rCode->rm_byte & NOT_BIT_67 ) );
        break;
    case no_RM:
        OutputCodeByte( ins->opcode | rCode->opcode );
        break;
    case no_WDSx:
        /* for SSSE3, instruction rm_byte is additional opcode byte */
        OutputCodeByte( ins->opcode );
        OutputCodeByte( ins->rm_byte );
        OutputCodeByte( rCode->rm_byte );
        goto common_wds;
    case no_WDS:
        rCode->opcode = 0;
        // no break
    default:
        // don't output opcode for 3DNow! instructions
        if( ins->byte1_info != F_0F0F ) {
            OutputCodeByte( ins->opcode | rCode->opcode );
        }
        tmp = ins->rm_byte | rCode->rm_byte;
        OutputCodeByte( tmp );
    common_wds:
        if( addr_32( rCode ) ) {
            switch ( tmp & NOT_BIT_345 ) {
            case 0x04:
                 // mod = 00, r/m = 100
                 // s-i-b is present
            case 0x44:
                 // mod = 01, r/m = 100
                 // s-i-b is present
            case 0x84:
                 // mod = 10, r/m = 100
                 // s-i-b is present
                 OutputCodeByte( rCode->sib );
            }
        }
        break;
    }
    return( NOT_ERROR );
}

static ret_code output_data( OPNDTYPE determinant, int index )
/*******************************************************/
/*
 output address displacement and immediate data;
 possible return codes: NOT_ERROR
*/
{
    int                 out = 0;

    DebugMsg(("output_data(%X, %u) enter\n", determinant, index));

#if 1
    /* it's not just CMPS/LODS/MOVS/SCAS/STOS, but also
     the variants with B,W and D suffix which accept a memory operand
     */
    if ( CodeInfo->token == T_XLAT ||
         AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REP ||
         AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REPxx ) {
#else
    switch( CodeInfo->token ) {
    case T_CMPS:
    case T_LODS:
    case T_MOVS:
    case T_OUTS:
    case T_INS:
    case T_SCAS:
    case T_STOS:
    case T_XLAT:
#endif
        /* these instructions don't really want the memory operand */
        return( NOT_ERROR );
    }

    mark_fixupp( determinant, index );
    store_fixup( index );

    if( determinant & OP_I8 ) {   // 8 bit
        out = 1;
    } else if( determinant & OP_I16 ) { // 16 bit
        out = 2;
    } else if( determinant & ( OP_I32 | OP_J32 ) ) { // 32 bit
        out = 4;
    } else if( determinant & OP_J48 ) {
        out = 6;
    } else if( determinant & OP_M_ANY ) {
        // switch on the mode ( the leftmost 2 bits )
        switch( CodeInfo->rm_byte & BIT_67 ) {
        case MOD_01:  // mode = 01
            out = 1;
            break;
        case MOD_00:
            if( !addr_32( CodeInfo ) ) {
                if( ( CodeInfo->rm_byte & BIT_012 ) == D16 ) {
                     out = 2;
                }
            } else {
                switch( CodeInfo->rm_byte & BIT_012 ) {
                case S_I_B:
                    if( ( CodeInfo->sib & BIT_012 ) != D32 ) {
                        break;  // out = 0
                    }
                    // no break
                case D32:
                    out = 4;
                }
            }
            break;
        case MOD_10:  // mode = 10
            if( !addr_32( CodeInfo ) ) {
                out = 2;
            } else {
                out = 4;
            }
        }
    }
    DebugMsg(("output_data: out=%u\n", out));

    while( out > 0 ) {
        OutputCodeByte( CodeInfo->data[index] );
        CodeInfo->data[index] >>= 8;
        out--;
    }
    return( NOT_ERROR );
}

static ret_code match_phase_2( int *i )
/*********************************
- a routine used by match_phase_1() to determine whether both operands match
  with that in the assembly instructions table;
- i: index into AsmOpTable
- call by match_phase_1() only;
- possible return codes: EMPTY, ERROR, NOT_ERROR
*/
{
//    DebugMsg(("match_phase_2(%u) enter\n", *i));
    if( CodeInfo->opnd_type[OPND2] != OP_NONE ) {
        // 2 opnds instruction
        return( match_phase_3( i, AsmOpTable[*i].opnd_type[OPND1] ) );
    } else {
        // 1 opnd instruction
        // make sure the second opnd also match, i.e. has to be OP_NONE
        if( AsmOpTable[*i].opnd_type[OPND2] == OP_NONE ) {
            if( output( *i ) == ERROR ) {
                DebugMsg(("match_phase_2: error with op1\n"));
                return( ERROR );
            }
            // output idata or disp ( if first opnd is OP_M / OP_I )
            return( output_data( CodeInfo->opnd_type[OPND1], OPND1 ) );
        } else {
            // still cannot find match
            return( EMPTY );
        }
    }
}

ret_code match_phase_1( struct asm_code *CodeInfo )
/************************
- this routine will look up the assembler opcode table and try to match
  the first operand in table with what we get;
- if first operand match then it will call match_phase_2() to determine if the
  second operand also match; if not, it must be error;
*/
{
    int             i;
    ret_code        retcode;
    signed char     temp_opsiz = 0;
    OPNDTYPE        cur_opnd;
    OPNDTYPE        pre_opnd;
    OPNDTYPE        asm_op1;

    // look up the hash table to get the real position of instruction
    i = AsmResWord[CodeInfo->token].position;

    /* for FAR calls/jmps there was a non-compatible solution implemented
     * in WASM which required two additional opcodes, CALLF and JMPF.
     * this has been removed for JWasm, but there's now the need to
     * find the correct entries in AsmOpTable. The following numbers
     * are required to find the correct starting index for the FAR variant.
     */

    if (CodeInfo->isfar) {
        if (CodeInfo->token == T_CALL)
            i = i + NUMCALLN;
        else if (CodeInfo->token == T_JMP)
            i = i + NUMJMPN;
    }

    // make sure the user want 80x87 ins
    if( ( AsmOpTable[i].cpu & P_FPU_MASK ) != P_NO87 ) {
        if( ( ModuleInfo.curr_cpu & P_FPU_MASK ) == P_NO87 ) {
            DebugMsg(("match_phase_1: wrong cpu setting: curr=%X - table=%X\n", ModuleInfo.curr_cpu, AsmOpTable[i].cpu));
            AsmError( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        } else {
            CodeInfo->prefix.opsiz = FALSE;
        }
    }

    // make sure the user want 80x86 privileged mode ins
    if( ( AsmOpTable[i].cpu & P_PM ) != 0 ) {
        if( ( ModuleInfo.curr_cpu & P_PM ) == 0 ) {
            AsmError( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
    }
    cur_opnd = CodeInfo->opnd_type[OPND1];
    pre_opnd = cur_opnd;

    //if( cur_opnd & OP_M_ANY ) {
    if( cur_opnd == OP_M ) {
        if ( CodeInfo->mem_type == MT_EMPTY ) {
            if ( CodeInfo->InsFixup[0] && CodeInfo->InsFixup[0]->sym )
                CodeInfo->mem_type = CodeInfo->InsFixup[0]->sym->mem_type;
        }
        DebugMsg(( "match_phase_1 enter: codeinfo->mem_type=%Xh\n", CodeInfo->mem_type ));
        switch (CodeInfo->mem_type) {
        case MT_BYTE:
        case MT_SBYTE:
            pre_opnd = OP_M_B;
            break;
        case MT_WORD:
        case MT_SWORD:
            pre_opnd = OP_M_W;
            break;
        case MT_DWORD:
        case MT_SDWORD:
            pre_opnd = OP_M_DW;
            break;
        case MT_FWORD:
            pre_opnd = OP_M_FW;
            break;
        case MT_QWORD:
            pre_opnd = OP_M_QW;
            break;
        case MT_TBYTE:
            pre_opnd = OP_M_TB;
            break;
        case MT_OWORD:
            pre_opnd = OP_M_OW;
            break;
#if 0 /* v1.95: commented out! */
        case MT_FAR:
            pre_opnd = ( CodeInfo->use32 ) ? OP_M_FW : OP_M_DW ;
            break;
        case MT_NEAR:
            pre_opnd = ( CodeInfo->use32 ) ? OP_M_DW : OP_M_W ;
            break;
#endif
        case MT_EMPTY:
            for (; AsmOpTable[i+1].token == CodeInfo->token; i++ )
                if ( AsmOpTable[i].opnd_type[OPND1] & OP_M_ANY )
                    break;
            /* use the first variant with a memory operand1 */
            if ( AsmOpTable[i].opnd_type[OPND1] & OP_M_ANY )
                pre_opnd = AsmOpTable[i].opnd_type[OPND1];
            else
                pre_opnd = OP_M_B;
            break;
        }
    } else if( cur_opnd & OP_I ) {
#if 0
        if (CodeInfo->mem_type != MT_EMPTY) {
            if (CodeInfo->mem_type == MT_BYTE || CodeInfo->mem_type == MT_SBYTE)
                pre_opnd = OP_I8;
            else if (CodeInfo->mem_type == MT_WORD || CodeInfo->mem_type == MT_SWORD)
                pre_opnd = OP_GE_16;
            else
                pre_opnd = OP_I32;
        } else if( cur_opnd == OP_I8 ) {
#else
        if( cur_opnd == OP_I8 ) {
#endif
            pre_opnd = OP_GE_8;
        } else if( cur_opnd == OP_I16 ) {
            pre_opnd = OP_GE_16;
        }
   }

    DebugMsg(("match_phase_1: cur_opnd=%X, pre_opnd=%X, codeinfo->mem_type=%Xh\n", cur_opnd, pre_opnd, CodeInfo->mem_type));

    for ( ; AsmOpTable[i].token == CodeInfo->token; i++ ) {
        // get the operand type from the instruction table
        asm_op1 = AsmOpTable[i].opnd_type[OPND1];
        cur_opnd = CodeInfo->opnd_type[OPND1];

        if( cur_opnd & OP_M_ANY ) {
            if( !( ( asm_op1 & cur_opnd ) && ( ( asm_op1 & OP_M_DFT ) == ( cur_opnd & OP_M_DFT ) ) ) )
                cur_opnd = pre_opnd;
        } else if ( cur_opnd & OP_I )
            cur_opnd = pre_opnd;

        switch( asm_op1 ) {
        case OP_MMX:
             if( cur_opnd & OP_MMX ) {
                 retcode = match_phase_2( &i );
                 if ( retcode != EMPTY )
                     return( retcode );
             }
             break;
        case OP_XMM:
             if( cur_opnd & OP_XMM ) {
                 retcode = match_phase_2( &i );
                 if ( retcode != EMPTY )
                     return( retcode );
             }
             break;
        case OP_R16:
            if( cur_opnd & asm_op1 ) {
                temp_opsiz = CodeInfo->prefix.opsiz;
                CodeInfo->prefix.opsiz = FALSE;
                retcode = match_phase_2( &i );
                if( retcode != EMPTY )
                    return( retcode );
                CodeInfo->prefix.opsiz = temp_opsiz;
            }
            break;
        case OP_M16:
            if( cur_opnd & asm_op1 ) {
                temp_opsiz = CodeInfo->prefix.opsiz;
                retcode = match_phase_2( &i );
                if( retcode != EMPTY )
                    return( retcode );
                CodeInfo->prefix.opsiz = temp_opsiz;
            }
            break;
        case OP_I32:
        case OP_I16:
            if( cur_opnd & asm_op1 ) {
                CodeInfo->opnd_type[OPND1] = asm_op1;
                retcode = match_phase_2( &i );
                if( retcode != EMPTY )
                    return( retcode );
            }
            break;
        case OP_I8_U:
            if( cur_opnd & OP_I ) {
                if( CodeInfo->data[OPND1] <= UCHAR_MAX ) {
                    temp_opsiz = CodeInfo->prefix.opsiz;
                    CodeInfo->opnd_type[OPND1] = OP_I8;
                    retcode = match_phase_2( &i );
                    if( retcode != EMPTY )
                        return( retcode );
                    CodeInfo->prefix.opsiz = temp_opsiz;
                }
            }
            break;
        case OP_I_3:                    // for INT only
            if( ( ( cur_opnd & OP_I8 ) && CodeInfo->data[OPND1] == 3 ) &&
                                CodeInfo->opnd_type[OPND2] == OP_NONE ) {
                return( output( i ) );
            }
            break;
        case OP_NONE:
            if( cur_opnd == OP_NONE &&
                    CodeInfo->opnd_type[OPND2] == OP_NONE ) {
                return( output( i ) );
            }
            break;
        default:
//            DebugMsg(("match_phase_1: default case\n"));
            /*
             - if ( asm_op1 & cur_opnd ) then types of operand 1 do match.
             - if ( cur_opnd & OP_M_ANY == 0 ) then current operand1 is NOT
               a memory operand.
               if ( asm_op1 & OP_M_DFT == cur_opnd & OP_M_DFT ) then ...

             */
            if( ( asm_op1 & cur_opnd )
                && ( ( ( cur_opnd & OP_M_ANY ) == 0 )
                    || ( ( asm_op1 & OP_M_DFT ) == ( cur_opnd & OP_M_DFT ) ) ) ) {
                retcode = match_phase_2( &i );
                if( retcode != EMPTY ) {
                    return( retcode );
                }
            }
            break;
        }
    } /* end for */
    AsmError( INVALID_INSTRUCTION_OPERANDS );
    return( ERROR );
}

static ret_code check_3rd_operand( int i )
/***********************************/
{
    OPNDTYPE    cur_opnd;

    cur_opnd = CodeInfo->opnd_type[OPND3];
    if( ( AsmOpTable[i].opnd_type_3rd == OP3_NONE )
        || ( AsmOpTable[i].opnd_type_3rd & OP3_HID ) ) {
        if( cur_opnd == OP_NONE ) {
            return( NOT_ERROR );
        } else {
            return( ERROR );
        }
    } else if( cur_opnd == OP_NONE ) {
        return( ERROR );
    } else {
        return( NOT_ERROR );
    }
}

static ret_code output_3rd_operand( int i )
/************************************/
{
    if( AsmOpTable[i].opnd_type_3rd == OP3_NONE ) {
        return( NOT_ERROR );
    } else if( AsmOpTable[i].opnd_type_3rd == OP3_I8_U ) {
        if( CodeInfo->opnd_type[OPND3] & OP_I ) {
            return( output_data( OP_I8, OPND3 ) );
        } else {
            return( ERROR );
        }
    } else if( AsmOpTable[i].opnd_type_3rd & OP3_HID ) {
        CodeInfo->data[OPND3] = AsmOpTable[i].opnd_type_3rd & ~OP3_HID;
        return( output_data( OP_I8, OPND3 ) );
    } else {
        return( NOT_ERROR );
    }
}

static ret_code match_phase_3( int *i, OPNDTYPE determinant )
/*******************************************************
- this routine will look up the assembler opcode table and try to match
  the second operand with what we get;
- if second operand match then it will output code; if not, pass back to
  match_phase_1() and try again;
- call by match_phase_2() only;
- i: index into AsmOpTable
- possible return codes: EMPTY, NOT_ERROR, ERROR
*/
{
    OPNDTYPE    cur_opnd;
    OPNDTYPE    last_opnd;
    OPNDTYPE    asm_op2;
    unsigned    instruction;

    instruction = AsmOpTable[*i].token;

    last_opnd = CodeInfo->opnd_type[OPND1];
    cur_opnd  = CodeInfo->opnd_type[OPND2];

    DebugMsg(("match_phase_3 enter, op1=%X, op2=%X\n", last_opnd, cur_opnd));

    while( AsmOpTable[*i].opnd_type[OPND1] == determinant &&
           AsmOpTable[*i].token == instruction ) {
        asm_op2 = AsmOpTable[*i].opnd_type[OPND2];
        switch( asm_op2 ) {
        case OP_CR:
        case OP_DR:
        case OP_TR:
        case OP_ST:
        case OP_STI:
            if( cur_opnd & asm_op2 ) {
                DebugMsg(("match_phase_3: OP_CR/DR/TR/ST/STI\n"));
                return( output( *i ) );
            }
            break;
        case OP_SR:
        case OP_DX:
        case OP_A:
        case OP_R:
        case OP_R1632:
        case OP_R32:
            if( cur_opnd & asm_op2 ) {
                DebugMsg(("match_phase_3: OP_SR/DX/A/R/R1632/R32\n"));
                if( check_3rd_operand( *i ) == ERROR )
                    break;
                if( output( *i ) == ERROR )
                    return( ERROR );
                if( output_data( last_opnd, OPND1 ) == ERROR )
                    return( ERROR );
                return( output_3rd_operand( *i ) );
            }
            break;
        case OP_CL:
            if( cur_opnd & asm_op2 ) {
                DebugMsg(("match_phase_3: OP_CL\n"));
                // CL is encoded in bit 345 of rm_byte, but we don't need it
                // so clear it here
                CodeInfo->rm_byte &= NOT_BIT_345;
                if( output( *i ) == ERROR )
                    return( ERROR );
                return( output_data( last_opnd, OPND1 ) );
            }
            break;
        case OP_R16:
            if( cur_opnd & asm_op2 ) {
                DebugMsg(("match_phase_3: OP_R16\n"));
                CodeInfo->prefix.opsiz = FALSE;
                if( output( *i ) == ERROR )
                    return( ERROR );
                return( output_data( last_opnd, OPND1 ) );
            }
            break;
        case OP_I:
            if( cur_opnd & asm_op2 ) {
                long operand = CodeInfo->data[OPND2];
                DebugMsg(("match_phase_3: OP_I\n"));
                if( last_opnd & OP_R8 ) {
                    // 8-bit register, so output 8-bit data
                    if( Parse_Pass == PASS_1 && !InRange( operand, 1 ) ) {
                        DebugMsg(("imm const too large (08): %X\n", operand));
                        AsmWarn( 1, IMMEDIATE_CONSTANT_TOO_LARGE );
                    }
                    CodeInfo->prefix.opsiz = FALSE;
                    cur_opnd = OP_I8;
                    if( CodeInfo->InsFixup[OPND2] != NULL ) {
                        CodeInfo->InsFixup[OPND2]->type = FIX_LOBYTE;
                    }
                } else if( last_opnd & OP_R16 ) {
                    // 16-bit register, so output 16-bit data
                    if( Parse_Pass == PASS_1 && !InRange( operand, 2 ) ) {
                        DebugMsg(("imm const too large (16): %X\n", operand));
                        AsmWarn( 1, IMMEDIATE_CONSTANT_TOO_LARGE );
                    }
                    cur_opnd = OP_I16;
                } else if( last_opnd & OP_R32 ) {
                    // 32-bit register, so output 32-bit data
                    CodeInfo->prefix.opsiz = CodeInfo->use32 ? FALSE : TRUE;/* 12-feb-92 */
                    cur_opnd = OP_I32;
                } else if( last_opnd & OP_M_ANY ) {
                    /* there is no reason this should be only for T_MOV */
                    switch( OperandSize( last_opnd ) ) {
                    case 1:
                        cur_opnd = OP_I8;
                        CodeInfo->prefix.opsiz = FALSE;
                        break;
                    case 2:
                        cur_opnd = OP_I16;
                        CodeInfo->prefix.opsiz = CodeInfo->use32 ? TRUE : FALSE;
                        break;
                    case 4:
                        cur_opnd = OP_I32;
                        CodeInfo->prefix.opsiz = CodeInfo->use32 ? FALSE : TRUE;
                        break;
                    default:
                        AsmError( INVALID_INSTRUCTION_OPERANDS );
                        break;
                    }
                }
                if( output( *i ) == ERROR )
                    return( ERROR );
                if( output_data( last_opnd, OPND1 ) == ERROR )
                    return( ERROR );
                return( output_data( cur_opnd, OPND2 ) );
            }
            break;
        case OP_I8_U:
            DebugMsg(("match_phase_3: OP_I8_U\n"));
            if( ( cur_opnd != OP_I8 )
                && ( cur_opnd != OP_I8_U )
                && ( cur_opnd != OP_I16 ) ) {
                break;
            }
            // range of unsigned 8-bit is 0 - 255
            if( CodeInfo->data[OPND2] > UCHAR_MAX ) {
                break;
            }
            if( output( *i ) == ERROR )
                return( ERROR );
            if( output_data( last_opnd, OPND1 ) == ERROR )
                return( ERROR );
            return( output_data( OP_I8, OPND2 ) );
        case OP_I8:
            DebugMsg(("match_phase_3: OP_I8\n"));
            if( cur_opnd == OP_I8 ) {
                /* do nothing yet */
                if( CodeInfo->InsFixup[OPND2] != NULL ) {
                    break;
                }
            } else if( cur_opnd == OP_I16 || cur_opnd == OP_I32) {
                /* if there was a typecast to avoid to use the short, signed
                 instructions, skip this. Example
                 "cmp ax,word ptr 1"
                 */
                break;
            } else if( ( cur_opnd & OP_I )
                && ( CodeInfo->InsFixup[OPND2] == NULL )
                && ( ( last_opnd & OP_R16 )
                || ( last_opnd & OP_M16 ) && ( MEM_TYPE( CodeInfo->mem_type, WORD ) ) ) ) {
                if( (int_8)CodeInfo->data[OPND2] ==
                    (int_16)CodeInfo->data[OPND2] ) {
                    CodeInfo->opnd_type[OPND2] = OP_I8;
                    CodeInfo->data[OPND2] = (int_8)CodeInfo->data[OPND2];
                } else {
                    break;
                }
            } else if( ( cur_opnd & OP_I )
                && ( CodeInfo->InsFixup[OPND2] == NULL )
                && ( ( last_opnd & OP_R32 )
                || ( last_opnd & OP_M32 ) && ( MEM_TYPE( CodeInfo->mem_type, DWORD ) ) ) ) {
                if( (int_8)CodeInfo->data[OPND2] ==
                    (int_32)CodeInfo->data[OPND2] ) {
                    CodeInfo->opnd_type[OPND2] = OP_I8;
                    CodeInfo->data[OPND2] = (int_8)CodeInfo->data[OPND2];
                } else {
                    break;
                }
            } else {
                break;
            }
            if( output( *i ) == ERROR )
                return( ERROR );
            if( output_data( last_opnd, OPND1 ) == ERROR )
                return( ERROR );
            return( output_data( OP_I8, OPND2 ) );
        case OP_I_1:
            DebugMsg(("match_phase_3: OP_I_1\n"));
            if( cur_opnd == OP_I8  &&  CodeInfo->data[OPND2] == 1 ) {
                if( output( *i ) == ERROR )
                    return( ERROR );
                return( output_data( last_opnd, OPND1 ) );
            }
            break;
        case OP_M16:
            if( cur_opnd & OP_M
                && ( MEM_TYPE( CodeInfo->mem_type, WORD )
                || CodeInfo->mem_type == MT_EMPTY ) ) {
                DebugMsg(("match_phase_3: OP_M16\n"));
                if( output( *i ) == ERROR )
                    return( ERROR );
                if( output_data( last_opnd, OPND1 ) == ERROR )
                    return( ERROR );
                return( output_data( cur_opnd, OPND2 ) );
            }
            break;
        case OP_M:
            if( cur_opnd & asm_op2 ) {
                DebugMsg(("match_phase_3: OP_M\n"));
                if( check_3rd_operand( *i ) == ERROR )
                    break;
                if( output( *i ) == ERROR )
                    return( ERROR );
                if( output_data( last_opnd, OPND1 ) == ERROR )
                    return( ERROR );
                if( output_data( cur_opnd, OPND2 ) == ERROR )
                    return( ERROR );
                if( output_3rd_operand( *i ) == ERROR )
                    return( ERROR );
                return( output_3DNow( *i ) );
            }
            break;
        default:
            if( ( asm_op2 & ( OP_MMX | OP_XMM ) )
                && ( cur_opnd & asm_op2 ) ) {
                DebugMsg(("match_phase_3: default\n"));
                if( check_3rd_operand( *i ) == ERROR )
                    break;
                if( output( *i ) == ERROR )
                    return( ERROR );
                if( output_data( last_opnd, OPND1 ) == ERROR )
                    return( ERROR );
                if( ( cur_opnd  & OP_M_ANY ) && ( asm_op2 & OP_M_ANY ) ) {
                    if( output_data( cur_opnd, OPND2 ) == ERROR ) {
                        return( ERROR );
                    }
                }
                if( output_3rd_operand( *i ) == ERROR )
                    return( ERROR );
                return( output_3DNow( *i ) );
            }
            break;
        }
        (*i)++;
    }
    (*i)--;
    DebugMsg(("match_phase_3: returns EMPTY!\n"));
    return( EMPTY );
}
