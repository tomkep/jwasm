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

#if defined( _STANDALONE_ )

#include "directiv.h"

extern void AddLinnumDataRef( void );
extern int  AddFloatingPointEmulationFixup( const struct asm_ins ASMFAR *, bool );

#endif

// for FAR calls/jmps there was a non-compatible solution
// implemented which required two additional opcodes, CALLF and JMPF.
// this has been removed for JWasm, but there's now the need to
// find the correct entries in AsmOpTable. The following numbers
// are required to find the correct starting index for the FAR variant.

#define NUMCALLN 8  /* no of entries for CALLN in INSTRUCT.H */
#define NUMJMPN  9  /* no of entries for JMPN in INSTRUCT.H */

static int match_phase_3( int *i, OPNDTYPE determinant );

static int output_3DNow( int i )
/******************************/
{
    const struct asm_ins ASMFAR *ins = &AsmOpTable[i];

    if( ins->byte1_info == F_0F0F ) {
        OutputCodeByte( ins->opcode | CodeInfo->info.opcode );
    }
    return( NOT_ERROR );
}

static int output( int i )
/************************/
/*
- determine what code should be output and their order;
- output prefix bytes ( ADRSIZ, OPSIZ, LOCK, REPxx, segment override prefix, etc )
  , opcode, "mod r/m" byte and "s-i-b" byte;
*/
{
    const struct asm_ins ASMFAR *ins = &AsmOpTable[i];
    struct asm_code             *rCode = CodeInfo;
    unsigned_8                  tmp;

#if defined( _STANDALONE_ )
    /*
     * Output debug info - line numbers
     */
    if( Options.line_numbers && !PhaseError && (Parse_Pass != PASS_1) ) {
        AddLinnumDataRef();
    }

    /*
     * Check if FP is valid
     */
    if((( ins->cpu & P_FPU_MASK ) != P_NO87 ) && ( Options.floating_point == NO_FP_ALLOWED )) {
        AsmError( NO_FP_WITH_FPC_SET );
        return( ERROR );
    }
    /*
     * Output FP fixup if required
     */
    if(( ModuleInfo.emulator == TRUE )
        && ( !rCode->use32 )
        && ( ins->allowed_prefix != NO_FWAIT )
        && (( ins->allowed_prefix == FWAIT ) || (( ins->cpu&P_FPU_MASK ) != P_NO87 ))) {
            if( AddFloatingPointEmulationFixup( ins, FALSE ) == ERROR ) {
                return( ERROR );
            }
    }
#endif

    /*
     * Check if CPU and FPU is valid for output code
     */
    if( ( ins->cpu & P_CPU_MASK ) > ( rCode->info.cpu & P_CPU_MASK )
        || ( ins->cpu & P_FPU_MASK ) > ( rCode->info.cpu & P_FPU_MASK )
        || ( ins->cpu & P_EXT_MASK ) > ( ins->cpu & rCode->info.cpu & P_EXT_MASK ) ) {
        AsmError( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING );
        return( ERROR );
    }

    /*
     * Output instruction prefix LOCK, REP or REPNE
     */
    if( rCode->prefix.ins != EMPTY ) {
        switch( rCode->prefix.ins ) {
        case T_LOCK:
            if( ins->allowed_prefix != LOCK ) {
                AsmError( LOCK_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION );
                return( ERROR );
            }
            break;
        case T_REP:
            if( ins->allowed_prefix != REP ) {
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
            if( ins->allowed_prefix != REPxx ) {
                AsmError( REPX_PREFIX_IS_NOT_ALLOWED_ON_THIS_INSTRUCTION );
                return( ERROR );
            }
            break;
        default:
            break;
        }
        OutputCodeByte( AsmOpTable[AsmOpcode[rCode->prefix.ins].position].opcode );
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
    case F_16:
    case F_32:
    case F_660F:
    case F_0F:
    case F_0F0F:
    default:
        break;
    }

    /*
     * Output FP FWAIT if required
     */
    if( ins->token == T_FWAIT ) {
        if(( rCode->info.cpu&P_CPU_MASK ) < P_386 ) {
#if defined( _STANDALONE_ )
            if(( ModuleInfo.emulator == TRUE ) && ( !rCode->use32 )) {
                OutputCodeByte( OP_NOP );
            }
#else
            OutputCodeByte( OP_NOP );
#endif
        }
    } else if( ins->allowed_prefix == FWAIT ) {
        OutputCodeByte( OP_WAIT );
#if defined( _STANDALONE_ )
    } else if(( ModuleInfo.emulator == TRUE )
        && ( !rCode->use32 )
        && ( ins->allowed_prefix != NO_FWAIT )
        && (( ins->allowed_prefix == FWAIT ) || (( ins->cpu&P_FPU_MASK ) != P_NO87 ))) {
        OutputCodeByte( OP_WAIT );
#endif
    } else if( ins->allowed_prefix != NO_FWAIT ) {
        // implicit FWAIT synchronization for 8087 (CPU 8086/80186)
        if((( rCode->info.cpu&P_CPU_MASK ) < P_286 )
            && (( ins->cpu&P_FPU_MASK ) == P_87 )) {
            OutputCodeByte( OP_WAIT );
        }
    }

#if defined( _STANDALONE_ )
    /*
     * Output FP fixup if required
     */
    if(( ModuleInfo.emulator == TRUE )
        && ( !rCode->use32 )
        && ( ins->allowed_prefix != NO_FWAIT )
        && (( ins->allowed_prefix == FWAIT ) || (( ins->cpu&P_FPU_MASK ) != P_NO87 ))) {
        if( AddFloatingPointEmulationFixup( ins, TRUE ) == ERROR ) {
            return( ERROR );
        }
    }
#endif
    /*
     * Output address size prefix
     */
    if( rCode->prefix.adrsiz == TRUE ) {
        OutputCodeByte( ADRSIZ );
    }
    /*
     * Output operand size prefix
     */
    if( ins->cpu & NO_OPPRFX )
        rCode->prefix.opsiz = FALSE;
    switch( ins->byte1_info ) {
    case F_16:
        if( rCode->use32 ) rCode->prefix.opsiz = TRUE;
        break;
    case F_32:
        if( !rCode->use32 ) rCode->prefix.opsiz = TRUE;
        break;
    case F_660F:
        rCode->prefix.opsiz = TRUE;
        break;
    case F_0F:
    case F_0F0F:
    case F_F20F:
    case F_F30F:
    default:
        break;
    }
    if( rCode->prefix.opsiz == TRUE ) {
        if(( rCode->info.cpu & P_CPU_MASK ) < P_386 ) {
            AsmError( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
        /*
            Certain instructions use the ADDRSIZE prefix when they really
            should use OPERSIZE prefix (well, I think so!). Stupid Intel.
        */
        switch( ins->token ) {
        case T_JCXZ:
        case T_JECXZ:
        case T_LOOPD:
        case T_LOOPW:
        case T_LOOPED:
        case T_LOOPEW:
        case T_LOOPNED:
        case T_LOOPNEW:
        case T_LOOPZD:
        case T_LOOPZW:
        case T_LOOPNZD:
        case T_LOOPNZW:
            OutputCodeByte( ADRSIZ );
            break;
        default:
            OutputCodeByte( OPSIZ );
            break;
        }
    }
    /*
     * Output segment prefix
     */
    if( rCode->prefix.seg != EMPTY ) {
        OutputCodeByte( rCode->prefix.seg );
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
    case F_660F:
    case F_F20F:
    case F_F30F:
        OutputCodeByte( EXTENDED_OPCODE );
        break;
    case F_F3:
    default:
        break;
    }
    if( ins->opnd_dir ) {
        /* The reg and r/m fields are backwards */
        tmp = rCode->info.rm_byte;
        rCode->info.rm_byte = ( tmp & 0xc0 ) | ((tmp >> 3) & 0x7) | ((tmp << 3) & 0x38);
    }
    switch( ins->rm_info ) {
    case R_in_OP:
        OutputCodeByte( ins->opcode | ( rCode->info.rm_byte & NOT_BIT_67 ) );
        break;
    case no_RM:
        OutputCodeByte( ins->opcode | rCode->info.opcode );
        break;
    case no_WDS:
        rCode->info.opcode = 0;
        // no break
    default:
        // don't output opcode for 3DNow! instructions
        if( ins->byte1_info != F_0F0F ) {
            OutputCodeByte( ins->opcode | rCode->info.opcode );
        }
        tmp = ins->rm_byte | rCode->info.rm_byte;
        OutputCodeByte( tmp );
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

static int output_data( OPNDTYPE determinant, int index )
/*******************************************************/
/*
  output address displacement and immediate data;
*/
{
    int                 out = 0;

    DebugMsg(("output_data(%X, %u) enter\n", determinant, index));

    switch( CodeInfo->info.token ) {
    case T_CMPS:
    case T_LODS:
    case T_MOVS:
    case T_OUTS:
    case T_INS:
    case T_SCAS:
    case T_STOS:
    case T_XLAT:
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
        switch( CodeInfo->info.rm_byte & BIT_67 ) {
        case MOD_01:  // mode = 01
            out = 1;
            break;
        case MOD_00:
            if( !addr_32( CodeInfo ) ) {
                if( ( CodeInfo->info.rm_byte & BIT_012 ) == D16 ) {
                     out = 2;
                }
            } else {
                switch( CodeInfo->info.rm_byte & BIT_012 ) {
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

static int match_phase_2( int *i )
/*********************************
- a routine used by match_phase_1() to determine whether both operands match
  with that in the assembly instructions table;
- call by match_phase_1() only;
*/
{
//    DebugMsg(("match_phase_2(%u) enter\n", *i));
    if( CodeInfo->info.opnd_type[OPND2] != OP_NONE ) {
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
            return( output_data( CodeInfo->info.opnd_type[OPND1], OPND1 ) );
        } else {
            // still cannot find match
            return( EMPTY );
        }
    }
}

int match_phase_1( void )
/************************
- this routine will look up the assembler opcode table and try to match
  the first operand in table with what we get;
- if first operand match then it will call match_phase_2() to determine if the
  second operand also match; if not, it must be error;
*/
{
    int             i;
    int             retcode;
    signed char     temp_opsiz = 0;
    OPNDTYPE        cur_opnd;
    OPNDTYPE        pre_opnd;
    OPNDTYPE        asm_op1;

    // if nothing inside, no need to output anything
    if( CodeInfo->info.token == T_NULL ) {
        if( CodeInfo->prefix.seg != EMPTY ) {
            /* we have:     REG: on line */
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        } else if( CodeInfo->info.opnd_type[OPND1] == OP_NONE ) {
            return( NOT_ERROR );
        } else {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
    }
    // look up the hash table to get the real position of instruction
    i = AsmOpcode[CodeInfo->info.token].position;

    // this "hack" makes the CALLF/JMPF hack obsolete. Just skip the
    // "near" entries for CALL/JMP if the transition is FAR.
    if (CodeInfo->isfar) {
        if (CodeInfo->info.token == T_CALL)
            i = i + NUMCALLN;
        else if (CodeInfo->info.token == T_JMP)
            i = i + NUMJMPN;
    }

    // make sure the user want 80x87 ins
    if( ( AsmOpTable[i].cpu & P_FPU_MASK ) != P_NO87 ) {
        if( ( CodeInfo->info.cpu & P_FPU_MASK ) == P_NO87 ) {
            AsmError( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        } else {
            CodeInfo->prefix.opsiz = FALSE;
        }
    }

    // make sure the user want 80x86 protected mode ins
    if( ( AsmOpTable[i].cpu & P_PM ) != 0 ) {
        if( ( CodeInfo->info.cpu & P_PM ) == 0 ) {
            AsmError( INVALID_INSTRUCTION_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
    }
    cur_opnd = CodeInfo->info.opnd_type[OPND1];

    // if( cur_opnd & OP_M ) {

    pre_opnd = cur_opnd;
    if( cur_opnd & OP_M_ANY ) {
        DebugMsg(("match_phase_1 enter: codeinfo->mem_type=%u\n", CodeInfo->mem_type));
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
        case MT_FAR:
            pre_opnd = ( CodeInfo->use32 ) ? OP_M_FW : OP_M_DW ;
            break;
        case MT_NEAR:
            pre_opnd = ( CodeInfo->use32 ) ? OP_M_DW : OP_M_W ;
            break;
        default:
#if 1
            /* if there's just one variation of the instruction
             and no size has been specified, use it */
            if (CodeInfo->mem_type == MT_EMPTY &&
                AsmOpTable[i+1].token != CodeInfo->info.token &&
                (AsmOpTable[i].opnd_type[OPND1] & OP_M_ANY) &&
                AsmOpTable[i].opnd_type[OPND1] != OP_M )
                pre_opnd = AsmOpTable[i].opnd_type[OPND1];
            else
#endif
            if ((Parse_Pass == PASS_1) && (cur_opnd == OP_M))
                pre_opnd = OP_M_B;
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

    DebugMsg(("match_phase_1: cur_opnd=%X, pre_opnd=%X, codeinfo->mem_type=%u\n", cur_opnd, pre_opnd, CodeInfo->mem_type));

    while( AsmOpTable[i].token == CodeInfo->info.token ) {
        // get the real opnd
        asm_op1 = AsmOpTable[i].opnd_type[OPND1];
        cur_opnd = CodeInfo->info.opnd_type[OPND1];

        if( cur_opnd & OP_M_ANY ) {
            if( !( ( asm_op1 & cur_opnd ) && ( ( asm_op1 & OP_M_DFT ) == ( cur_opnd & OP_M_DFT ) ) ) )
                cur_opnd = pre_opnd;
        } else if ( cur_opnd & OP_I )
            cur_opnd = pre_opnd;

        switch( asm_op1 ) {
        case OP_MMX:
             if( cur_opnd & OP_MMX ) {
                 return( match_phase_2( &i ) );
             }
             break;
        case OP_XMM:
             if( cur_opnd & OP_XMM ) {
                 return( match_phase_2( &i ) );
             }
             break;
        case OP_R16:
            if( cur_opnd & asm_op1 ) {
                temp_opsiz = CodeInfo->prefix.opsiz;
                CodeInfo->prefix.opsiz = FALSE;
                switch( match_phase_2( &i ) ) {
                case EMPTY:
                    CodeInfo->prefix.opsiz = temp_opsiz;
                    break;
                case ERROR:
                    return( ERROR );
                case NOT_ERROR:
                    return( NOT_ERROR );
                }
            }
            break;
        case OP_M16:
            if( cur_opnd & asm_op1 ) {
                temp_opsiz = CodeInfo->prefix.opsiz;
                switch( match_phase_2( &i ) ) {
                case EMPTY:
                    CodeInfo->prefix.opsiz = temp_opsiz;
                    break;
                case ERROR:
                    return( ERROR );
                case NOT_ERROR:
                    return( NOT_ERROR );
                }
            }
            break;
        case OP_I32:
        case OP_I16:
            if( cur_opnd & asm_op1 ) {
                CodeInfo->info.opnd_type[OPND1] = asm_op1;
                switch( match_phase_2( &i ) ) {
                case EMPTY:
                    break;
                case ERROR:
                    return( ERROR );
                case NOT_ERROR:
                    return( NOT_ERROR );
                }
            }
            break;
        case OP_I8_U:
            if( cur_opnd & OP_I ) {
                if( CodeInfo->data[OPND1] <= UCHAR_MAX ) {
                    temp_opsiz = CodeInfo->prefix.opsiz;
                    CodeInfo->info.opnd_type[OPND1] = OP_I8;
                    switch( match_phase_2( &i ) ) {
                    case EMPTY:
                        CodeInfo->prefix.opsiz = temp_opsiz;
                        break;
                    case ERROR:
                        return( ERROR );
                    case NOT_ERROR:
                        return( NOT_ERROR );
                    }
                }
            }
            break;
        case OP_I_3:                    // for INT only
            if( ( ( cur_opnd & OP_I8 ) && CodeInfo->data[OPND1] == 3 ) &&
                                CodeInfo->info.opnd_type[OPND2] == OP_NONE ) {
                return( output( i ) );
            }
            break;
        case OP_NONE:
            if( cur_opnd == OP_NONE &&
                    CodeInfo->info.opnd_type[OPND2] == OP_NONE ) {
                return( output( i ) );
            }
            break;
        default:
//            DebugMsg(("match_phase_1: default case\n"));
            /* this is the check to see if the operand we have matches
             * the type of the one we need
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
        i++;
    } /* end while */
    AsmError( INVALID_INSTRUCTION_OPERANDS );
    return( ERROR );
}

static int check_3rd_operand( int i )
/***********************************/
{
    OPNDTYPE    cur_opnd;

    cur_opnd = CodeInfo->info.opnd_type[OPND3];
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

static int output_3rd_operand( int i )
/************************************/
{
    if( AsmOpTable[i].opnd_type_3rd == OP3_NONE ) {
        return( NOT_ERROR );
    } else if( AsmOpTable[i].opnd_type_3rd == OP3_I8_U ) {
        if( CodeInfo->info.opnd_type[OPND3] & OP_I ) {
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

static int match_phase_3( int *i, OPNDTYPE determinant )
/*******************************************************
- this routine will look up the assembler opcode table and try to match
  the second operand with what we get;
- if second operand match then it will output code; if not, pass back to
  match_phase_1() and try again;
- call by match_phase_2() only;
*/
{
    OPNDTYPE    cur_opnd;
    OPNDTYPE    last_opnd;
    OPNDTYPE    asm_op2;
    unsigned    instruction;

    instruction = AsmOpTable[*i].token;

    last_opnd = CodeInfo->info.opnd_type[OPND1];
    cur_opnd  = CodeInfo->info.opnd_type[OPND2];

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
            DebugMsg(("match_phase_3: OP_CR/DR/TR/ST/STI\n"));
            if( cur_opnd & asm_op2 ) {
                return( output( *i ) );
            }
            break;
        case OP_SR:
        case OP_DX:
        case OP_A:
        case OP_R:
        case OP_R1632:
        case OP_R32:
            DebugMsg(("match_phase_3: OP_SR/DX/A/R/R1632/R32\n"));
            if( cur_opnd & asm_op2 ) {
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
            DebugMsg(("match_phase_3: OP_CL\n"));
            if( cur_opnd & asm_op2 ) {
                // CL is encoded in bit 345 of rm_byte, but we don't need it
                // so clear it here
                CodeInfo->info.rm_byte &= NOT_BIT_345;
                if( output( *i ) == ERROR )
                    return( ERROR );
                return( output_data( last_opnd, OPND1 ) );
            }
            break;
        case OP_R16:
            DebugMsg(("match_phase_3: OP_R16\n"));
            if( cur_opnd & asm_op2 ) {
                CodeInfo->prefix.opsiz = FALSE;
                if( output( *i ) == ERROR )
                    return( ERROR );
                return( output_data( last_opnd, OPND1 ) );
            }
            break;
        case OP_I:
            DebugMsg(("match_phase_3: OP_I\n"));
            if( cur_opnd & asm_op2 ) {
#if defined(_STANDALONE_)
                long operand = CodeInfo->data[OPND2];
#endif
                if( last_opnd & OP_R8 ) {
                    // 8-bit register, so output 8-bit data
#if defined(_STANDALONE_)
                    if( Parse_Pass == PASS_1 && !InRange( operand, 1 ) ) {
                        DebugMsg(("imm const too large (08): %X\n", operand));
                        AsmWarn( 1, IMMEDIATE_CONSTANT_TOO_LARGE );
                    }
#endif
                    CodeInfo->prefix.opsiz = FALSE;
                    cur_opnd = OP_I8;
                    if( InsFixups[OPND2] != NULL ) {
                        InsFixups[OPND2]->type = FIX_LOBYTE;
                    }
                } else if( last_opnd & OP_R16 ) {
                    // 16-bit register, so output 16-bit data
#if defined(_STANDALONE_)
                    if( Parse_Pass == PASS_1 && !InRange( operand, 2 ) ) {
                        DebugMsg(("imm const too large (16): %X\n", operand));
                        AsmWarn( 1, IMMEDIATE_CONSTANT_TOO_LARGE );
                    }
#endif
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
                if( InsFixups[OPND2] != NULL ) {
                    break;
                }
            } else if( cur_opnd == OP_I16 || cur_opnd == OP_I32) {
                /* if there was a typecast to avoid to use the short, signed
                 instructions, skip this. Example
                 "cmp ax,word ptr 1"
                 */
                break;
            } else if( ( cur_opnd & OP_I )
                && ( InsFixups[OPND2] == NULL )
                && ( ( last_opnd & OP_R16 )
                || ( last_opnd & OP_M16 ) && ( MEM_TYPE( CodeInfo->mem_type, WORD ) ) ) ) {
                if( (int_8)CodeInfo->data[OPND2] ==
                    (int_16)CodeInfo->data[OPND2] ) {
                    CodeInfo->info.opnd_type[OPND2] = OP_I8;
                    CodeInfo->data[OPND2] = (int_8)CodeInfo->data[OPND2];
                } else {
                    break;
                }
            } else if( ( cur_opnd & OP_I )
                && ( InsFixups[OPND2] == NULL )
                && ( ( last_opnd & OP_R32 )
                || ( last_opnd & OP_M32 ) && ( MEM_TYPE( CodeInfo->mem_type, DWORD ) ) ) ) {
                if( (int_8)CodeInfo->data[OPND2] ==
                    (int_32)CodeInfo->data[OPND2] ) {
                    CodeInfo->info.opnd_type[OPND2] = OP_I8;
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
            DebugMsg(("match_phase_3: OP_M16\n"));
            if( cur_opnd & OP_M
                && ( MEM_TYPE( CodeInfo->mem_type, WORD )
                || CodeInfo->mem_type == MT_EMPTY ) ) {
                if( output( *i ) == ERROR )
                    return( ERROR );
                if( output_data( last_opnd, OPND1 ) == ERROR )
                    return( ERROR );
                return( output_data( cur_opnd, OPND2 ) );
            }
            break;
        case OP_M:
            DebugMsg(("match_phase_3: OP_M\n"));
            if( cur_opnd & asm_op2 ) {
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
            DebugMsg(("match_phase_3: default\n"));
            if( ( asm_op2 & ( OP_MMX | OP_XMM ) )
                && ( cur_opnd & asm_op2 ) ) {
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
    return( EMPTY );
}
