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
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "codegen.h"
#include "fixup.h"
#include "fpfixup.h"
#include "segment.h"
#include "input.h"

/* v2.03: OutputCodeByte no longer needed */
#define OutputCodeByte( x ) OutputByte( x )

enum prefix_reg {
    PREFIX_ES = 0x26,
    PREFIX_CS = 0x2E,
    PREFIX_SS = 0x36,
    PREFIX_DS = 0x3E,
    PREFIX_FS = 0x64,
    PREFIX_GS = 0x65
};

static const char sr_prefix[] =
    { PREFIX_ES, PREFIX_CS, PREFIX_SS, PREFIX_DS, PREFIX_FS, PREFIX_GS };

static ret_code output_3DNow( struct code_info *CodeInfo )
/********************************************************/
{
    if( CodeInfo->pcurr->byte1_info == F_0F0F ) {
        OutputCodeByte( CodeInfo->pcurr->opcode | CodeInfo->iswide );
    }
    return( NOT_ERROR );
}

static ret_code output_opc( struct code_info *CodeInfo )
/******************************************************/
/*
 * - determine what code should be output and their order;
 * - output prefix bytes ( LOCK, REPxx, FWAIT, ADRSIZ, OPSIZ, segment override prefix, etc )
 * - output opcode, "mod r/m" byte and "s-i-b" byte;
 * - possible return codes: ERROR, NOT_ERROR
 */
{
    const struct asm_ins *ins = CodeInfo->pcurr;
    uint_8           tmp;
    uint_8           fpfix = FALSE;

    DebugMsg1(("output_opc enter, ins.opc/rm=%X/%X, byte1_info=%X CodeInfo->rm=%X opsiz=%u\n", ins->opcode, ins->rm_byte, ins->byte1_info, CodeInfo->rm_byte, CodeInfo->prefix.opsiz ));
    /*
     * Output debug info - line numbers
     */
    if( Options.line_numbers )
        AddLinnumDataRef( LineNumber );

    /* if it's a FPU instr, reset opsiz */
    //if( ins->cpu & P_FPU_MASK ) {
    /* v2.02: if it's a FPU or MMX/SSE instr, reset opsiz!
     * [this code has been moved here from match_phase_1()]
     */
    if( ins->cpu & ( P_FPU_MASK | P_MMX | P_SSEALL ) ) {
#if SSE4SUPP
        /* there are 2 exceptions. how to avoid this ugly hack? */
        if ( CodeInfo->token != T_CRC32 &&
            CodeInfo->token != T_POPCNT )
#endif
        CodeInfo->prefix.opsiz = FALSE;
    }

    /*
     * Check if CPU, FPU and extensions are within the limits
     */
    if( ( ins->cpu & P_CPU_MASK ) > ( ModuleInfo.curr_cpu & P_CPU_MASK )
        || ( ins->cpu & P_FPU_MASK ) > ( ModuleInfo.curr_cpu & P_FPU_MASK )
        || ( ins->cpu & P_EXT_MASK ) > ( ModuleInfo.curr_cpu & P_EXT_MASK ) ) {
        DebugMsg(("output_opc: wrong cpu setting: instr.cpu=%X, ModuleInfo.cpu=%X, opnd1=%" FX32 "\n",
                  ins->cpu, ModuleInfo.curr_cpu, ins->opnd_type[OPND1] ));
        /* if instruction is valid for 16bit cpu, but operands aren't,
         then display a more specific error message! */
        if( ins->cpu == P_386 &&
            ( ( InstrTable[ IndexFromToken( CodeInfo->token )].cpu & P_CPU_MASK ) <= P_386 ))
            AsmError( INSTRUCTION_FORM_REQUIRES_80386 );
        else
            AsmError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
        //return( ERROR );
    }

    /*
     * Output FP fixup if required
     * the OPs with NOWAIT are the instructions beginning with
     * FN, except FNOP.
     * the OPs with WAIT are the instructions:
     * FCLEX, FDISI, FENI, FINIT, FSAVEx, FSTCW, FSTENVx, FSTSW
     */
    if(( ModuleInfo.emulator == TRUE ) &&
       ( CodeInfo->Ofssize == USE16 ) &&
       ( ins->cpu & P_FPU_MASK ) &&
       ( ins->allowed_prefix != AP_NO_FWAIT ) ) {
        fpfix = TRUE;
        /* v2.04: no error is returned */
        AddFloatingPointEmulationFixup( CodeInfo );
    }

    /*
     * Output instruction prefix LOCK, REP or REP[N]E|Z
     */
    if( CodeInfo->prefix.ins != EMPTY ) {
        tmp = InstrTable[ IndexFromToken( CodeInfo->prefix.ins )].rm_byte;
        /* instruction prefix must be ok. However, with -Zm, the plain REP
         * is also ok for instructions which expect REPxx.
         */
        if ( ModuleInfo.m510 == TRUE &&
            tmp == AP_REP &&
            ins->allowed_prefix == AP_REPxx )
            tmp = AP_REPxx;

        if( ins->allowed_prefix != tmp ) {
            AsmError( INSTRUCTION_PREFIX_NOT_ALLOWED );
        } else
            OutputCodeByte( InstrTable[ IndexFromToken( CodeInfo->prefix.ins )].opcode );
    }
    /*
     * Output instruction prefix REP or REPNE for SSEx instructions
     */
    switch( ins->byte1_info ) {
    case F_F20F:
        OutputCodeByte( 0xF2 );
        break;
    case F_F3: /* PAUSE instruction */
    case F_F30F:
        OutputCodeByte( 0xF3 );
        break;
    }

    /*
     * Output FP FWAIT if required
     */
    if ( ins->cpu & P_FPU_MASK ) {
        if( CodeInfo->token == T_FWAIT ) {
            /* v2.04: Masm will always insert a NOP if emulation is active,
             * no matter what the current cpu is. The reason is simple: the
             * nop is needed because of the fixup which was inserted.
             */
            //if(( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
            //    if(( ModuleInfo.emulator == TRUE ) && ( CodeInfo->Ofssize == USE16 )) {
            if( fpfix ) {
                OutputCodeByte( OP_NOP );
            }
        } else if( fpfix || ins->allowed_prefix == AP_FWAIT ) {
            OutputCodeByte( OP_WAIT );
        } else if( ins->allowed_prefix != AP_NO_FWAIT ) {
            /* implicit FWAIT synchronization for 8087 (CPU 8086/80186) */
            if(( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_286 )
               OutputCodeByte( OP_WAIT );
        }
    }

    /*
     * check if address/operand size prefix is to be set
     */
    switch( ins->byte1_info ) {
    case F_16:
        if( CodeInfo->Ofssize >= USE32 ) CodeInfo->prefix.opsiz = TRUE;
        break;
    case F_16A:
        /* doesnt exist for IA32+ */
        if( CodeInfo->Ofssize == USE32 ) CodeInfo->prefix.adrsiz = TRUE;
        break;
    case F_32:
        if( CodeInfo->Ofssize == USE16 ) CodeInfo->prefix.opsiz = TRUE;
        break;
    case F_32A:
#if AMD64_SUPPORT
        /* in IA32+, the 32bit version gets an 0x67 prefix */
        if ( CodeInfo->Ofssize != USE32)  CodeInfo->prefix.adrsiz = TRUE;
#else
        if( CodeInfo->Ofssize == USE16 ) CodeInfo->prefix.adrsiz = TRUE;
#endif
        break;
    case F_660F:
        CodeInfo->prefix.opsiz = TRUE;
        break;
    case F_0FNO66:
        CodeInfo->prefix.opsiz = FALSE;
        break;
#if AMD64_SUPPORT
    case F_48:
    case F_480F:
        CodeInfo->prefix.rex |= REX_W;
        break;
#endif
    }
    /*
     * Output address and operand size prefixes.
     * These bytes are NOT compatible with FP emulation fixups,
     * which expect that the FWAIT/NOP first "prefix" byte is followed
     * by either a segment prefix or the opcode byte.
     * Neither Masm nor JWasm emit a warning, though.
     */
    if( CodeInfo->prefix.adrsiz == TRUE ) {
        OutputCodeByte( ADRSIZ );
#ifdef DEBUG_OUT
        if ( fpfix )
            DebugMsg(("output_opc: ERROR: FP emulation byte sequence destroyed by 32-bit address prefix!\n"));
#endif
    }
    if( CodeInfo->prefix.opsiz == TRUE ) {
#if 1
        if(( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
            DebugMsg(("output_opc: instruction form requires 386\n"));
            AsmError( INSTRUCTION_FORM_REQUIRES_80386 );
            return( ERROR );
        }
#endif
        OutputCodeByte( OPSIZ );
    }
    /*
     * Output segment prefix
     */
    if( CodeInfo->prefix.RegOverride != EMPTY ) {
        OutputCodeByte( sr_prefix[CodeInfo->prefix.RegOverride] );
    }

    if( ins->opnd_dir ) {
        /* The reg and r/m fields are backwards */
        tmp = CodeInfo->rm_byte;
        CodeInfo->rm_byte = ( tmp & 0xc0 ) | ((tmp >> 3) & 0x7) | ((tmp << 3) & 0x38);
#if AMD64_SUPPORT
        tmp = CodeInfo->prefix.rex;
        CodeInfo->prefix.rex = ( tmp & 0xFA ) | (( tmp & REX_R ) >> 2 ) | (( tmp & REX_B ) << 2 );
#endif
    }

#if AMD64_SUPPORT
    /* the REX prefix must be located after the other prefixes */
    if( CodeInfo->prefix.rex != 0 ) {
        if ( CodeInfo->Ofssize != USE64 ) {
            AsmError( INVALID_OPERAND_SIZE );
        }
        OutputCodeByte( CodeInfo->prefix.rex | 0x40 );
    }
#endif

    /*
     * Output extended opcode
     * special case for some 286 and 386 instructions
     * or 3DNow!, MMX and SSEx instructions
     */
    if ( ins->byte1_info >= F_0F ) {
        OutputCodeByte( EXTENDED_OPCODE );
        if ( ins->byte1_info == F_0F0F )
            OutputCodeByte( EXTENDED_OPCODE );
    }

    switch( ins->rm_info ) {
    case R_in_OP:
        OutputCodeByte( ins->opcode | ( CodeInfo->rm_byte & NOT_BIT_67 ) );
        return( NOT_ERROR );
    case no_RM:
        OutputCodeByte( ins->opcode | CodeInfo->iswide );
        return( NOT_ERROR );
    case no_WDSx:
        /* for SSSE3, instruction rm_byte is additional opcode byte */
        OutputCodeByte( ins->opcode );
        OutputCodeByte( ins->rm_byte );
        OutputCodeByte( CodeInfo->rm_byte );
        tmp = CodeInfo->rm_byte; /* v2.01: tmp wasn't set in v2.01 */
        break;
    case no_WDS:
        CodeInfo->iswide = 0;
        /* no break */
    default:
        /* don't output opcode for 3DNow! instructions */
        if( ins->byte1_info != F_0F0F ) {
            OutputCodeByte( ins->opcode | CodeInfo->iswide | CodeInfo->opc_or );
        }
        tmp = ins->rm_byte | CodeInfo->rm_byte;
        OutputCodeByte( tmp );
    }

    if( ( CodeInfo->Ofssize == USE16 && CodeInfo->prefix.adrsiz == 0 ) ||
       ( CodeInfo->Ofssize == USE32 && CodeInfo->prefix.adrsiz == 1 ) )
        return( NOT_ERROR ); /* no SIB for 16bit */

    switch ( tmp & NOT_BIT_345 ) {
    case 0x04: /* mod = 00, r/m = 100, s-i-b is present */
    case 0x44: /* mod = 01, r/m = 100, s-i-b is present */
    case 0x84: /* mod = 10, r/m = 100, s-i-b is present */
        OutputCodeByte( CodeInfo->sib );
    }
    return( NOT_ERROR );
}

#if 0 /* v2.04: removed */

#define FIXUP_WARNING 1

static void mark_fixupp( struct code_info *CodeInfo, struct fixup *fixup, OPNDTYPE determinant )
/**********************************************************************************************/
/*
 * called by codegen:output_data()
 * modify fixup type for immediates.
 */
{
    if ( CodeInfo->isfar && determinant == OP_I32 )
        determinant = OP_I16;

    switch( determinant ) {
    case OP_I16:
        switch( fixup->type ) {
        case FIX_OFF32:
            /* mov ax, offset <32-bit-label>. Currently JWasm will
             * rejected this. Masm allows it!
             */
#if FIXUP_WARNING
            if ( Parse_Pass == PASS_2 )
                AsmWarn( 0, FIXUP_CHANGED, "Offset32 to Offset16" );
#endif
            fixup->type = FIX_OFF16;
            break;
        case FIX_PTR32: /* ??? can this happen? */
#if FIXUP_WARNING
            if ( Parse_Pass == PASS_2 )
                AsmWarn( 0, FIXUP_CHANGED, "Pointer32 -> Pointer16" );
#endif
            fixup->type = FIX_PTR16;
            break;
        }
        break;
    case OP_I32:
    case OP_J48:
        switch( fixup->type ) {
        case FIX_OFF16: /* mov eax, offset <16-bit-label> */
#if FIXUP_WARNING
            if ( Parse_Pass == PASS_2 )
                AsmWarn( 0, FIXUP_CHANGED, "Offset16 to Offset32" );
#endif
            fixup->type = FIX_OFF32;
            break;
        case FIX_PTR16: /* ??? can this happen? */
#if FIXUP_WARNING
            if ( Parse_Pass == PASS_2 )
                AsmWarn( 0, FIXUP_CHANGED, "Pointer16 -> Pointer32" );
#endif
            fixup->type = FIX_PTR32;
            break;
        }
        break;
    }
}
#endif

static void output_data( struct code_info *CodeInfo, OPNDTYPE determinant, int index )
/************************************************************************************/
/*
 * output address displacement and immediate data;
 */
{
    int       size = 0;

    /* skip the memory operand for XLAT and string instructions! */
    if ( CodeInfo->token == T_XLAT ||
        CodeInfo->pcurr->allowed_prefix == AP_REP ||
        CodeInfo->pcurr->allowed_prefix == AP_REPxx ) {
        CodeInfo->InsFixup[index] = NULL;
        return;
    }
#ifdef DEBUG_OUT
    if ( CodeInfo->InsFixup[index] )
        DebugMsg1(("output_data(idx=%u, op=%" FX32 " [data=%" FX32 " fixup=%p typ=%u] ) enter [rm=%X]\n", index, determinant, CodeInfo->data[index], CodeInfo->InsFixup[index], CodeInfo->InsFixup[index]->type ,CodeInfo->rm_byte ));
    else
        DebugMsg1(("output_data(idx=%u, op=%" FX32 " [data=%" FX32 " fixup=NULL] ) enter [rm=%X]\n", index, determinant, CodeInfo->data[index], CodeInfo->rm_byte ));
#endif

    /* determine size */

    if( determinant & OP_I8 ) {
        size = 1;
    } else if( determinant & OP_I16 ) {
        size = 2;
    } else if( determinant & ( OP_I32 ) ) {
        size = 4;
    } else if( determinant & OP_J48 ) {
        size = 6;
#if AMD64_SUPPORT
    } else if( determinant & OP_I64 ) {
        size = 8;
#endif
    } else if( determinant & OP_M_ANY ) {
        /* switch on the mode ( the leftmost 2 bits ) */
        switch( CodeInfo->rm_byte & BIT_67 ) {
        case MOD_01:  /* displacement size is 1 */
            size = 1;
            break;
        case MOD_00: /* direct; base and/or index with no disp */
            if( ( CodeInfo->Ofssize == USE16 && CodeInfo->prefix.adrsiz == 0 ) ||
               ( CodeInfo->Ofssize == USE32 && CodeInfo->prefix.adrsiz == 1 ) ) {
                if( ( CodeInfo->rm_byte & BIT_012 ) == RM_D16 ) {
                     size = 2; /* = size of displacement */
                }
            } else {
                switch( CodeInfo->rm_byte & BIT_012 ) {
                case RM_SIB: /* 0x04 (equals register # for ESP) */
                    if( ( CodeInfo->sib & BIT_012 ) != RM_D32 ) {
                        break;  /* size = 0 */
                    }
                    /* no break */
                case RM_D32: /* 0x05 (equals register # for EBP) */
                    size = 4; /* = size of displacement */
                }
            }
            break;
        case MOD_10:  /* displacement size is 2/4 */
            if( ( CodeInfo->Ofssize == USE16 && CodeInfo->prefix.adrsiz == 0 ) ||
               ( CodeInfo->Ofssize == USE32 && CodeInfo->prefix.adrsiz == 1 ) ) {
                size = 2;
            } else {
                size = 4;
            }
        }
    }
    DebugMsg1(( "output_data: size=%u\n", size ));

    if ( size ) {
        if ( CodeInfo->InsFixup[index] && write_to_file ) {
            CodeInfo->InsFixup[index]->location = GetCurrOffset();
#if 0 /* v2.04: removed */
            mark_fixupp( CodeInfo, CodeInfo->InsFixup[index], determinant );
#endif
            //store_fixup( CodeInfo->InsFixup[index], &CodeInfo->data[index] );
            OutputBytes( (unsigned char *)&CodeInfo->data[index],
                                size, CodeInfo->InsFixup[index] );
        } else {
            OutputBytes( (unsigned char *)&CodeInfo->data[index], size, NULL );
        }
    }
    return;
}

static ret_code check_3rd_operand( struct code_info *CodeInfo )
/*************************************************************/
{
    if( ( CodeInfo->pcurr->opnd_type_3rd == OP3_NONE ) ||
       ( CodeInfo->pcurr->opnd_type_3rd & OP3_HID ) )
        return( ( CodeInfo->opnd_type[OPND3] == OP_NONE ) ? NOT_ERROR : ERROR );
    else {
        DebugMsg(("check_3rd_operand: %X-%X\n", CodeInfo->pcurr->opnd_type_3rd, CodeInfo->opnd_type[OPND3] ));
        /* current variant needs a 3rd operand */
        switch ( CodeInfo->pcurr->opnd_type_3rd ) {
        case OP3_CL:
            if ( CodeInfo->opnd_type[OPND3] == OP_CL )
                return( NOT_ERROR );
            break;
        case OP3_I8_U:
            /* idata_nofixup doesn't work reliably for 3rd arguments */
            if ( ( CodeInfo->opnd_type[OPND3] & OP_I ) &&
                CodeInfo->data[OPND3] >= 0 &&
                CodeInfo->data[OPND3] < 0x100 )
                return( NOT_ERROR );
            break;
        case OP3_XMM0:
            if ( CodeInfo->opnd_type[OPND3] == OP_XMM &&
                CodeInfo->data[OPND3] == 0 )
                return( NOT_ERROR );
            break;
        }
        return(  ERROR );
    }
}

static ret_code output_3rd_operand( struct code_info *CodeInfo )
/**************************************************************/
{
    if( CodeInfo->pcurr->opnd_type_3rd == OP3_NONE ) {
        return( NOT_ERROR );
    } else if( CodeInfo->pcurr->opnd_type_3rd == OP3_I8_U ) {
        DebugMsg1(("output_3rd_operand, expected I8, op3=%" FX32 "\n", CodeInfo->opnd_type[OPND3] ));
        if( CodeInfo->opnd_type[OPND3] & OP_I ) {
            output_data( CodeInfo, OP_I8, OPND3 );
        } else {
            AsmError( INVALID_INSTRUCTION_OPERANDS );
            return( ERROR );
        }
    } else if( CodeInfo->pcurr->opnd_type_3rd & OP3_HID ) {
        DebugMsg1(("output_3rd_operand, expected OP3_HID, op3=%" FX32 "\n", CodeInfo->opnd_type[OPND3] ));
        CodeInfo->data[OPND3] = CodeInfo->pcurr->opnd_type_3rd & ~OP3_HID;
        output_data( CodeInfo, OP_I8, OPND3 );
    }
    return( NOT_ERROR );
}

static ret_code match_phase_3( struct code_info *CodeInfo, OPNDTYPE determinant )
/********************************************************************************
- this routine will look up the assembler opcode table and try to match
  the second operand with what we get;
- if second operand match then it will output code; if not, pass back to
  match_phase_1() and continue to scan InstrTable;
- possible return codes: EMPTY (continue scan), NOT_ERROR (done), ERROR
*/
{
    OPNDTYPE    opnd2;
    OPNDTYPE    opnd1;
    OPNDTYPE    tbl_op2;

    opnd1 = CodeInfo->opnd_type[OPND1];
    opnd2 = CodeInfo->opnd_type[OPND2];

    DebugMsg1(("match_phase_3 enter, op1=%X, searching op2=%X, CodeInfo->memtype=%X\n", opnd1, opnd2, CodeInfo->mem_type ));

    do  {
        tbl_op2 = CodeInfo->pcurr->opnd_type[OPND2];
        DebugMsg1(("match_phase_3: instr table op2=%X\n", tbl_op2 ));
        switch( tbl_op2 ) {
        case OP_SPECREG:
        case OP_ST:
        case OP_STI:
            if( opnd2 & tbl_op2 ) {
                DebugMsg1(("match_phase_3: OP_CR/DR/TR/ST/STI\n"));
                return( output_opc( CodeInfo ) );
            }
            break;
        case OP_A:
            /* v2.01: added */
            if ( !( opnd2 & OP_A ) )
                break;
        case OP_SR:
        case OP_R:
        case OP_RGT8:
        case OP_R32:
#if AMD64_SUPPORT
        case OP_R64:
        case OP_RGT16:
#endif
            if( opnd2 & tbl_op2 ) {
                DebugMsg1(("match_phase_3: OP_SR/A/R/R1632/R32\n"));
                if( check_3rd_operand( CodeInfo ) == ERROR )
                    break;
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                return( output_3rd_operand( CodeInfo ) );
            }
            break;
        case OP_DX: /* v2.01: accept only DX for IN */
            if( opnd2 == tbl_op2 ) {
                DebugMsg1(("match_phase_3: OP_DX\n"));
                return( output_opc( CodeInfo ) );
            }
            break;
        case OP_CL:
            //if( opnd2 & tbl_op2 ) { /* v2.01: accept CL only */
            if( opnd2 == tbl_op2 ) {
                DebugMsg1(("match_phase_3: OP_CL\n"));
                /* CL is encoded in bit 345 of rm_byte, but we don't need it
                 * so clear it here */
                CodeInfo->rm_byte &= NOT_BIT_345;
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                return( NOT_ERROR );
            }
            break;
        case OP_R16:
            if( opnd2 & tbl_op2 ) {
                DebugMsg1(("match_phase_3: OP_R16\n"));
                CodeInfo->prefix.opsiz = FALSE;
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                return( NOT_ERROR );
            }
            break;
        case OP_I:
            if( opnd2 & tbl_op2 ) {
                /* This branch exits with either ERROR or NOT_ERROR.
                 * So it can modify the CodeInfo fields without harm.
                 */
                //long operand = CodeInfo->data[OPND2];
                DebugMsg1(("match_phase_3: OP_I\n"));
                if( opnd1 & OP_R8 ) {
                    /* 8-bit register, so output 8-bit data */
                    /* v2.04: the check has already happened in check_size() or idata_xxx() */
                    //if( Parse_Pass == PASS_1 && !InRange( operand, 1 ) ) {
                    //    DebugMsg(("imm const too large (08): %X\n", operand));
                    //    AsmWarn( 1, IMMEDIATE_CONSTANT_TOO_LARGE );
                    //}
                    CodeInfo->prefix.opsiz = FALSE;
                    opnd2 = OP_I8;
                    if( CodeInfo->InsFixup[OPND2] != NULL ) {
                    /* v1.96: make sure FIX_HIBYTE isn't overwritten! */
                        if ( CodeInfo->InsFixup[OPND2]->type != FIX_HIBYTE )
                            CodeInfo->InsFixup[OPND2]->type = FIX_OFF8;
                    }
                } else if( opnd1 & OP_R16 ) {
                    /* v2.04: the check has already happened in check_size() or idata_xxx() */
                    //if( Parse_Pass == PASS_1 && !InRange( operand, 2 ) ) {
                    //    DebugMsg(("imm const too large (16): %X\n", operand));
                    //    AsmWarn( 1, IMMEDIATE_CONSTANT_TOO_LARGE );
                    //}
                    /* 16-bit register, so output 16-bit data */
                    opnd2 = OP_I16;
#if AMD64_SUPPORT
                } else if( opnd1 & (OP_R32 | OP_R64 ) ) {
#else
                } else if( opnd1 & OP_R32 ) {
#endif
                    /* 32- or 64-bit register, so output 32-bit data */
                    CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? 0 : 1;/* 12-feb-92 */
                    opnd2 = OP_I32;
                } else if( opnd1 & OP_M_ANY ) {
                    /* there is no reason this should be only for T_MOV */
                    switch( OperandSize( opnd1, CodeInfo ) ) {
                    case 1:
                        opnd2 = OP_I8;
                        CodeInfo->prefix.opsiz = FALSE;
                        break;
                    case 2:
                        opnd2 = OP_I16;
                        CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? 1 : 0;
                        break;
#if AMD64_SUPPORT
                        /* mov [mem], imm64 doesn't exist. It's ensured that
                         * immediate data is 32bit only
                         */
                    case 8:
#endif
                    case 4:
                        opnd2 = OP_I32;
                        CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? 0 : 1;
                        break;
                    default:
                        AsmError( INVALID_INSTRUCTION_OPERANDS );
                        return( ERROR );
                    }
                }
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                output_data( CodeInfo, opnd2, OPND2 );
                return( NOT_ERROR );
            }
            break;
#if AMD64_SUPPORT
        case OP_I64:
            DebugMsg1(("match_phase_3: OP_I64\n"));
            if( opnd2 & tbl_op2 ) {
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                output_data( CodeInfo, opnd2, OPND2 );
                return( NOT_ERROR );
            }
            break;
#endif
        case OP_I8_U: /* shift+rotate, ENTER, IN, PSxx[D|Q|W] */
            DebugMsg1(("match_phase_3: OP_I8_U\n"));
            if( ( opnd2 != OP_I8 )
                && ( opnd2 != OP_I8_U )
                && ( opnd2 != OP_I16 ) ) {
                break;
            }
            /* range of unsigned 8-bit is 0 - 255 */
            /* v2.03: lower bound wasn't checked */
            //if( CodeInfo->data[OPND2] > UCHAR_MAX ) {
            if( CodeInfo->data[OPND2] > UCHAR_MAX || CodeInfo->data[OPND2] < -128 ) {
                break;
            }
            if( output_opc( CodeInfo ) == ERROR )
                return( ERROR );
            output_data( CodeInfo, opnd1, OPND1 );
            output_data( CodeInfo, OP_I8, OPND2 );
#if 0 //SSE4SUPP /* the EXTRQ instruction has 2 immediate operands */
            return( output_3rd_operand( CodeInfo ) );
#else
            return( NOT_ERROR );
#endif
        case OP_I8:
            DebugMsg1(("match_phase_3: OP_I8\n"));
            /* if this branch modifies CodeInfo fields, it
             * will exit with either NOT_ERROR or ERROR.
             */
            if( opnd2 == OP_I8 ) {
               if ( CodeInfo->InsFixup[OPND2] != NULL ) /* external? then skip */
                   break;
               /* v2.04: added */
               if( ModuleInfo.NoSignExtend &&
                  ( CodeInfo->token == T_AND ||
                   CodeInfo->token == T_OR ||
                   CodeInfo->token == T_XOR ) )
                   break;
            /* v2.01: is const size forced? */
            // } else if( opnd2 == OP_I16 || opnd2 == OP_I32) {
            } else if( CodeInfo->const_size_fixed &&
                      ( opnd2 == OP_I16 || opnd2 == OP_I32 ) ) {
                /* if there was a typecast to avoid to use the short, signed
                 instructions, skip this. Example
                 "cmp ax,word ptr 1"
                 */
                break;
            } else if( ( opnd2 & OP_I )
                && ( CodeInfo->InsFixup[OPND2] == NULL )
                && ( ( opnd1 & OP_R16 )
                || ( opnd1 & OP_M16 ) && ( IS_MEM_TYPE( CodeInfo->mem_type, WORD ) ) ) ) {
                if( (int_8)CodeInfo->data[OPND2] ==
                    (int_16)CodeInfo->data[OPND2] ) {
                    DebugMsg1(("match_phase_3: OP_I8, op1=R16/M16, CI->mem_type=%Xh\n", CodeInfo->mem_type ));
                    CodeInfo->opnd_type[OPND2] = OP_I8;
                    CodeInfo->data[OPND2] = (int_8)CodeInfo->data[OPND2];
                } else {
                    break;
                }
            } else if( ( opnd2 & OP_I )
                && ( CodeInfo->InsFixup[OPND2] == NULL )
                && ( ( opnd1 & OP_R32 )
                || ( opnd1 & OP_M32 ) && ( IS_MEM_TYPE( CodeInfo->mem_type, DWORD ) ) ) ) {
                if( (int_8)CodeInfo->data[OPND2] ==
                    (int_32)CodeInfo->data[OPND2] ) {
                    DebugMsg1(("match_phase_3: OP_I8, op1=R32/M32\n"));
                    CodeInfo->opnd_type[OPND2] = OP_I8;
                    CodeInfo->data[OPND2] = (int_8)CodeInfo->data[OPND2];
                } else {
                    break;
                }
            } else {
                break;
            }
            if( output_opc( CodeInfo ) == ERROR )
                return( ERROR );
            output_data( CodeInfo, opnd1, OPND1 );
            output_data( CodeInfo, OP_I8, OPND2 );
            return( NOT_ERROR );
        case OP_I_1:
            DebugMsg1(("match_phase_3: OP_I_1\n"));
            if( opnd2 == OP_I8  &&  CodeInfo->data[OPND2] == 1 ) {
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                return( NOT_ERROR );
            }
            break;
        case OP_M16:
            if( ( opnd2 & OP_M ) &&
                ( IS_MEMTYPE_SIZ( CodeInfo->mem_type, sizeof(uint_16) ) || CodeInfo->mem_type == MT_EMPTY ) ) {
                DebugMsg1(("match_phase_3: OP_M16\n"));
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                output_data( CodeInfo, opnd2, OPND2 );
                return( output_3rd_operand( CodeInfo ) );
            }
            break;
        case OP_RM3264: /* v2.05: added, replaces OP_M32 */
            if( opnd2 & tbl_op2 ) {
                DebugMsg1(("match_phase_3: OP_RM3264\n"));
#if AMD64_SUPPORT
#if 1 /* still needed for CVTxx */
                if ( opnd2 & OP_M64 )
                    CodeInfo->prefix.rex |= REX_W;
#endif
#endif
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                output_data( CodeInfo, opnd2, OPND2 );
                return( NOT_ERROR );
            }
            break;
#if 0 /* v2.05: replaced by OP_RM3264 */
        case OP_M32: /* v2.0: added for MOVD */
            if( ( opnd2 & OP_M ) &&
               ( IS_MEMTYPE_SIZ( CodeInfo->mem_type, sizeof(uint_32) ) || CodeInfo->mem_type == MT_EMPTY ) ) {
                DebugMsg1(("match_phase_3: OP_M32\n"));
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                output_data( CodeInfo, opnd2, OPND2 );
                return( NOT_ERROR );
            }
            break;
#endif
        default:
            if( ( opnd2 & tbl_op2 ) || (CodeInfo->mem_type == MT_EMPTY && (opnd2 & OP_M_ANY) && (tbl_op2 & OP_M_ANY) )) {
                DebugMsg1(("match_phase_3: default branch\n"));
                if( check_3rd_operand( CodeInfo ) == ERROR )
                    break;
                if( output_opc( CodeInfo ) == ERROR )
                    return( ERROR );
                output_data( CodeInfo, opnd1, OPND1 );
                if( ( opnd2  & OP_M_ANY ) && ( tbl_op2 & OP_M_ANY ) ) {
                    output_data( CodeInfo, opnd2, OPND2 );
                }
                if( output_3rd_operand( CodeInfo ) == ERROR )
                    return( ERROR );
                return( output_3DNow( CodeInfo ) );
            }
            break;
        }
        CodeInfo->pcurr++;
    } while ( CodeInfo->pcurr->opnd_type[OPND1] == determinant && CodeInfo->pcurr->first == FALSE );
    CodeInfo->pcurr--; /* pointer will be increased in match_phase_1 */
    DebugMsg(("match_phase_3: returns EMPTY\n"));
    return( EMPTY );
}

static ret_code match_phase_2( struct code_info *CodeInfo )
/**********************************************************
 * a routine used by match_phase_1() to determine whether both operands match
 * with that in the assembly instructions table;
 * i: pointer to index into InstrTable, updated!
 * possible return codes: EMPTY, ERROR, NOT_ERROR
 */
{
    if( CodeInfo->opnd_type[OPND2] != OP_NONE ) {
        /* 2 opnds instruction */
        ret_code rc = match_phase_3( CodeInfo, CodeInfo->pcurr->opnd_type[OPND1] );
        if ( rc == NOT_ERROR ) {
#if AMD64_SUPPORT
            /* for rip-relative fixups, the instruction end is needed */
            if ( CodeInfo->Ofssize == USE64 ) {
                if ( CodeInfo->InsFixup[OPND1] && CodeInfo->InsFixup[OPND1]->type == FIX_RELOFF32 )
                    CodeInfo->InsFixup[OPND1]->addbytes = GetCurrOffset() - CodeInfo->InsFixup[OPND1]->location;
                if ( CodeInfo->InsFixup[OPND2] && CodeInfo->InsFixup[OPND2]->type == FIX_RELOFF32 )
                    CodeInfo->InsFixup[OPND2]->addbytes = GetCurrOffset() - CodeInfo->InsFixup[OPND2]->location;
            }
#endif
        }
        return( rc );
    } else {
        DebugMsg1(("match_phase_2() enter\n" ));
        /* 1 opnd instruction */
        /* make sure the second opnd also match, i.e. has to be OP_NONE */
        if( CodeInfo->pcurr->opnd_type[OPND2] == OP_NONE ) {
            if( output_opc( CodeInfo ) == ERROR ) {
                return( ERROR );
            }
            /* output idata or disp ( if first opnd is OP_M / OP_I ) */
            output_data( CodeInfo, CodeInfo->opnd_type[OPND1], OPND1 );
#if AMD64_SUPPORT
            if ( CodeInfo->Ofssize == USE64 && CodeInfo->InsFixup[OPND1] && CodeInfo->InsFixup[OPND1]->type == FIX_RELOFF32 )
                CodeInfo->InsFixup[OPND1]->addbytes = GetCurrOffset() - CodeInfo->InsFixup[OPND1]->location;
#endif
            return( NOT_ERROR );
        } else {
            /* still cannot find match */
            return( EMPTY );
        }
    }
}

ret_code match_phase_1( struct code_info *CodeInfo )
/***************************************************
- this routine will look up the assembler opcode table and try to match
  the first operand in table with what we get;
- if first operand match then it will call match_phase_2() to determine if the
  second operand also match; if not, it must be error;
*/
{
    ret_code        retcode;
    //signed char     temp_opsiz = 0;
    OPNDTYPE        opnd1;
    OPNDTYPE        asm_op1;

    /* privileged instructions ok? */
    if( ( CodeInfo->pcurr->cpu & P_PM ) > ( ModuleInfo.curr_cpu & P_PM ) ) {
        AsmError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
        return( ERROR );
    }
    opnd1 = CodeInfo->opnd_type[OPND1];

    if( opnd1 & OP_I ) {
        if( opnd1 == OP_I8 ) {
            opnd1 = OP_IGE8;
        } else if( opnd1 == OP_I16 ) {
            opnd1 = OP_IGE16;
        }
    }

#if AMD64_SUPPORT
    DebugMsg1(("match_phase_1(ofs=%X): opnd1=%X, codeinfo: ofssize=%u mem_type=%Xh rex=%Xh rm=%X sib=%X\n",
              CurrSeg->sym.offset, opnd1, CodeInfo->Ofssize, CodeInfo->mem_type, CodeInfo->prefix.rex, CodeInfo->rm_byte, CodeInfo->sib ));
#endif
    /* scan the instruction table for a matching first operand */
    do  {
        asm_op1 = CodeInfo->pcurr->opnd_type[OPND1];

        switch( asm_op1 ) {
#if 1 /* to be fixed: why is this needed?! */
        case OP_I32: /* CALL, JMP, PUSHD */
        case OP_I16: /* CALL, JMP, RETx, ENTER, PUSHW */
            if( opnd1 & asm_op1 ) {
                CodeInfo->opnd_type[OPND1] = asm_op1;
                retcode = match_phase_2( CodeInfo );
                if( retcode != EMPTY )
                    return( retcode );
            }
            break;
#endif
        case OP_DX: /* v2.01: accept reg DX only! */
            if( opnd1 == asm_op1 ) {
                retcode = match_phase_2( CodeInfo );
                if( retcode != EMPTY )
                    return( retcode );
            }
            break;
        case OP_I8_U: /* INT, OUT */
            if( opnd1 & OP_I ) {
                if( CodeInfo->data[OPND1] <= UCHAR_MAX ) {
                    //temp_opsiz = CodeInfo->prefix.opsiz;
                    CodeInfo->opnd_type[OPND1] = OP_I8;
                    retcode = match_phase_2( CodeInfo );
                    if( retcode != EMPTY )
                        return( retcode );
                    //CodeInfo->prefix.opsiz = temp_opsiz;
                }
            }
            break;
        case OP_I_3: /* INT */
            if( ( ( opnd1 & OP_I8 ) && CodeInfo->data[OPND1] == 3 ) &&
                                CodeInfo->opnd_type[OPND2] == OP_NONE ) {
                return( output_opc( CodeInfo ) );
            }
            break;
        case OP_NONE: /* instruction without operands */
            if( opnd1 == OP_NONE &&
               CodeInfo->opnd_type[OPND2] == OP_NONE ) {
                return( output_opc( CodeInfo ) );
            }
            break;
#if 1 /* can probably be removed again */
        case OP_M:
            if( opnd1 & OP_M_ANY ) {
                retcode = match_phase_2( CodeInfo );
                if( retcode != EMPTY )
                    return( retcode );
            }
            break;
#endif
        default:
            if( asm_op1 & opnd1 ) {
                retcode = match_phase_2( CodeInfo );
                if( retcode != EMPTY )
                    return( retcode );
            }
            break;
        }
        CodeInfo->pcurr++;
    } while ( CodeInfo->pcurr->first == FALSE );

    DebugMsg(("match_phase_1: no matching format found\n"));
    AsmError( INVALID_INSTRUCTION_OPERANDS );
    return( ERROR );
}

