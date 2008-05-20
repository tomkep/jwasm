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


/* set this macro to 0 if you do NOT want to allow structures to be defined
 * ie:
 *    struct foo
 *      ...
 *    foo ends
 *    ...
 *    bar foo < 4, 2, 3 >
 */
#define ALLOW_STRUCT_INIT 1

#include "asmglob.h"

#include "asminsd.h"
#include "asmdefs.h"
#include "equate.h"
#include "asmfixup.h"
#include "expreval.h"
#include "labels.h"

#if defined( _STANDALONE_ )
  #include "directiv.h"
  #include "myassert.h"
  #include "asminput.h"
  #include "types.h"
#endif

extern int              match_phase_1( void );
extern int              ptr_operator( memtype, uint_8 );
extern int              jmp( expr_list * );

static struct asm_code  Code_Info;
struct asm_code         *CodeInfo = &Code_Info;

unsigned char           Opnd_Count;

static void             SizeString( unsigned op_size );
static int              check_size( expr_list * );
static int              segm_override_jumps( expr_list *opndx );

#if defined( _STANDALONE_ )

extern int              directive( int , long );

static void             check_assume( struct asm_sym *, enum prefix_reg );

extern int_8            DefineProc;     // TRUE if the definition of procedure
                                        // has not ended

uint_8                  CheckSeg;       // if checking of opened segment is needed
int_8                   Frame;          // Frame of current fixup
uint_8                  Frame_Datum;    // Frame datum of current fixup
struct asm_sym          *SegOverride;

static int              in_epilogue = 0;

#else

#define     directive( i, value )   cpu_directive( value )

#endif

extern void             make_inst_hash_table( void );

static int              curr_ptr_type;
static char             ConstantOnly;

static int              mem2code( char, int, int, asm_sym * );

/* moved here from asmline */
static struct asm_tok   tokens[MAX_TOKEN];

struct asm_tok          *AsmBuffer[MAX_TOKEN];  // buffer to store token

#if defined( _STANDALONE_ )
void find_frame( struct asm_sym *sym )
/*******************************************/
{
    if( SegOverride != NULL ) {
        sym = SegOverride;
        if( sym->state == SYM_GRP ) {
            Frame = FRAME_GRP;
            Frame_Datum = GetGrpIdx( sym );
        } else if( sym->segment != NULL ) {
            Frame = FRAME_SEG;
            Frame_Datum = GetSegIdx( sym->segment );
        }
    } else {
        switch( sym->state ) {
        case SYM_INTERNAL:
        case SYM_PROC:
        case SYM_EXTERNAL:
            if( sym->segment != NULL ) {
                if( GetGrp( sym ) != NULL ) {
                    Frame = FRAME_GRP;
                    Frame_Datum = GetGrpIdx( GetGrp( sym ) );
                } else {
                    Frame = FRAME_SEG;
                    Frame_Datum = GetSegIdx( sym->segment );
                }
            }
            break;
        case SYM_GRP:
            Frame = FRAME_GRP;
            Frame_Datum = GetGrpIdx( sym );
            break;
        case SYM_SEG:
            Frame = FRAME_SEG;
            Frame_Datum = GetSegIdx( sym->segment );
            break;
        default:
            break;
        }
    }
}
#endif

static int comp_mem( int reg1, int reg2 )
/***************************************/
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

static void seg_override( int seg_reg, asm_sym *sym )
/***************************************************/
/*
- determine if segment override is necessary with the current address mode;
*/
{
    enum prefix_reg     default_seg;
#if defined( _STANDALONE_ )
    enum assume_segreg     assume_seg;

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
    if( sym != NULL ) {
        if( CodeInfo->prefix.seg == EMPTY ) {
            if( CodeInfo->info.token == T_LEA ) {
                check_assume( sym, EMPTY );
            } else {
                check_assume( sym, default_seg );
            }
        } else {
            switch( CodeInfo->prefix.seg ) {
            case PREFIX_ES:
                assume_seg = ASSUME_ES;
                break;
            case PREFIX_CS:
                assume_seg = ASSUME_CS;
                break;
            case PREFIX_SS:
                assume_seg = ASSUME_SS;
                break;
            case PREFIX_DS:
                assume_seg = ASSUME_DS;
                break;
            case PREFIX_FS:
                assume_seg = ASSUME_FS;
                break;
            case PREFIX_GS:
                assume_seg = ASSUME_GS;
                break;
            default:
                break;
            }
            if( GetPrefixAssume( sym, assume_seg ) == ASSUME_NOTHING ) {
//                AsmWarn( 3, CANNOT_ADDRESS_WITH_ASSUMED_REGISTER );
            }
        }
    }

    if( CodeInfo->prefix.seg == default_seg ) {
        CodeInfo->prefix.seg = EMPTY;
    }
#else
    if( CodeInfo->prefix.seg != EMPTY ) {
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
        if( CodeInfo->prefix.seg == default_seg ) {
            CodeInfo->prefix.seg = EMPTY;
        }
    }
#endif
}

#if defined( _STANDALONE_ )

static void check_assume( struct asm_sym *sym, enum prefix_reg default_reg )
/**************************************************************************/
/* Check if an assumed register is found, and prefix a register if necessary */
{
    enum assume_segreg     reg;
    enum assume_segreg     def_reg;

    /**/myassert( sym != NULL );
    if( sym->state == SYM_UNDEFINED )
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

    reg = GetAssume( sym, def_reg );

    if( reg == ASSUME_NOTHING ) {
        if( ( sym->state != SYM_EXTERNAL ) && ( sym->state != SYM_PROC ) ) {
            if( Parse_Pass == PASS_2 ) {
                AsmWarn( 3, CANNOT_ADDRESS_WITH_ASSUMED_REGISTER );
            }
        } else {
            CodeInfo->prefix.seg = default_reg;
        }
    } else if( default_reg != EMPTY ) {
        switch( reg ) {
        case ASSUME_ES:
            CodeInfo->prefix.seg = PREFIX_ES;
            break;
        case ASSUME_CS:
            CodeInfo->prefix.seg = PREFIX_CS;
            break;
        case ASSUME_DS:
            CodeInfo->prefix.seg = PREFIX_DS;
            break;
        case ASSUME_GS:
            CodeInfo->prefix.seg = PREFIX_GS;
            break;
        case ASSUME_FS:
            CodeInfo->prefix.seg = PREFIX_FS;
            break;
        case ASSUME_SS:
            CodeInfo->prefix.seg = PREFIX_SS;
            break;
        default:
            break;
        }
    }
}

int check_override( int *i )
/**************************/
/* Check if there is a register, segment or group override */
{
    int         index;

    index = *i;

    if( ( index + 2 ) < Token_Count ) {
        if( AsmBuffer[index+1]->token == T_COLON ) {
            switch( AsmBuffer[index]->token ) {
            case T_REG:
                CodeInfo->prefix.seg =
                    AsmOpTable[AsmOpcode[AsmBuffer[index]->value].position].opcode;
                (*i) += 2;
                if( *i >= Token_Count ) {
                    AsmError( LABEL_EXPECTED_AFTER_COLON );
                    return( ERROR );
                }
                break;
            case T_ID:      // Segment or Group override
                if( FixOverride(*i) != NOT_ERROR ) {
                    return( ERROR );
                }
                (*i) += 2;
                if( *i >= Token_Count ) {
                    AsmError( LABEL_EXPECTED_AFTER_COLON );
                    return( ERROR );
                }
                break;
            default:
                break;
            }
        }
    }
    return( NOT_ERROR );
}
#endif

static int Reg386( int reg_token )
/********************************/
{
    switch( reg_token ) {
    case T_EAX:         return( 0 );
    case T_ECX:         return( 1 );
    case T_EDX:         return( 2 );
    case T_EBX:         return( 3 );
    case T_ESP:         return( 4 );
    case T_EBP:         return( 5 );
    case T_ESI:         return( 6 );
    case T_EDI:         return( 7 );
    }
    /* I don't think this should happen */
    return( 0 );
}

int OperandSize( OPNDTYPE opnd )
/******************************/
{
    if( ( opnd == OP_NONE ) || ( opnd & OP_SPECIAL ) ) {
        return( 0 );
    } else if( opnd == OP_M ) {
        int i = SizeFromMemtype(CodeInfo->mem_type, NULL);
        if (i == ERROR)
            i = 0;
        else if (i == 0)
            i = 0;
        return (i);
    } else if( opnd & ( OP_M8_R8 | OP_M_B | OP_I8 | OP_I_1 | OP_I_3 | OP_I8_U ) ) {
        return( 1 );
    } else if( opnd & ( OP_M16_R16 | OP_M_W | OP_I16 | OP_SR ) ) {
        return( 2 );
    } else if( opnd & ( OP_R32 | OP_M_DW | OP_I32 | OP_J32 | OP_SPEC_REG ) ) {
        return( 4 );
    } else if( opnd & ( OP_I | OP_J48 ) ) {
        return( 6 );
    } else if( opnd & ( OP_M_QW | OP_MMX ) ) {
        return( 8 );
    } else if( opnd & ( OP_STI | OP_M_TB ) ) {
        return( 10 );
    } else if( opnd & ( OP_M_OW | OP_XMM ) ) {
        return( 16 );
    }
    return( 0 );
}

int InRange( long val, unsigned bytes )
/**********************************************/
/*
     Can 'val' be represented in 'bytes' bytes?
*/
{
    unsigned long max;
    unsigned long mask;

#if 1
    if (bytes == 1) {
        if (val > 255 || val < -255)
            return(0);
        return(1);
    } else {
        if (val > 65535 || val < -65535)
            return(0);
        return(1);
    }
#endif
#if 0
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
}

static int mem2code( char ss, int index, int base, asm_sym *sym )
/***************************************************************/
/*
 encode the memory operand to machine code
 ss = scale factor???
 index = index register (T_DI, T_ESI, ...)
 base = base register (T_EBP, ... )
 sym = ???
*/
{
    struct asm_code     *rCode = CodeInfo;
    int                 temp;
    unsigned char       mod_field;
    unsigned char       rm_field;

    DebugMsg(("mem2code(ss=%u, index=%d, base=%d, sym=%X) enter\n", ss, index, base, sym));

    // clear mod
    rm_field = 0;
    if( InsFixups[Opnd_Count] != NULL ) {
        mod_field = MOD_10;
    } else if( rCode->data[Opnd_Count] == 0 ) {
        mod_field = MOD_00;
    } else if( ( rCode->data[Opnd_Count] > SCHAR_MAX )
       || ( rCode->data[Opnd_Count] < SCHAR_MIN ) ) {
        mod_field = MOD_10;
    } else {
        mod_field = MOD_01;
    }
    if( ( index == EMPTY ) && ( base == EMPTY ) ) {
        // direct memory
        // clear the rightmost 3 bits
        mod_field = MOD_00;
        if( !addr_32( rCode ) ) {
            if( !InRange( rCode->data[Opnd_Count], 2 ) ) {
                // expect 16-bit but got 32-bit address
                AsmError( DISPLACEMENT_OUT_OF_RANGE );
                return( ERROR );
            }
            rm_field = D16;
        } else {
            rm_field = D32;
        }
        // default is DS:[], DS: segment override is not needed
        seg_override( T_DS, sym );
    } else if( ( index == EMPTY ) && ( base != EMPTY ) ) {
        switch( base ) {
        case T_SI:
            rm_field = 0x04; // SI
            // default is DS:[], DS: segment override is not needed
            break;
        case T_DI:
            rm_field = 0x05; // DI
            // default is DS:[], DS: segment override is not needed
            break;
        case T_BP:
            rm_field = BP;
            if( mod_field == MOD_00 ) {
                mod_field = MOD_01;
            }
            // default is SS:[], SS: segment override is not needed
            break;
        case T_BX:
            rm_field = 0x07; // BX
            // default is DS:[], DS: segment override is not needed
            break;
        case T_EBP:
            DebugMsg(("mem2code: base is EBP\n"));
            rm_field = EBP;
            if( mod_field == MOD_00 ) {
                mod_field = MOD_01;
            }
            // default is SS:[], SS: segment override is not needed
            break;
        case T_ESP:
            rm_field = ESP;
            // ss = 00, index = 100 ( no index ), base = 100 ( ESP )
            rCode->sib = 0x24;
            // default is SS:[], SS: segment override is not needed
            break;
        default: // for 386 and up
            rm_field = Reg386( base );
            // default is DS:[], DS: segment override is not needed
        }
        seg_override( base, sym );
    } else if( ( index != EMPTY ) && ( base == EMPTY ) ) {
        // mod field is 00
        mod_field = MOD_00;
        // s-i-b is present ( r/m = 100 )
        rm_field = S_I_B;
        // scale factor, index, base ( 0x05 => no base reg )
        rCode->sib = ( ss | ( Reg386(index) << 3 ) | 0x05 );
        // default is DS:[], DS: segment override is not needed
        seg_override( T_DS, sym );
    } else {
        // base != EMPTY && index != EMPTY
        switch( index ) {
        case T_BX:
        case T_BP:
            if( ( temp = comp_mem( index, base ) ) == ERROR )
                return( ERROR );
            rm_field = temp;
            seg_override( index, sym );
            break;
        case T_SI:
        case T_DI:
            if( ( temp = comp_mem( base, index ) ) == ERROR )
                return( ERROR );
            rm_field = temp;
            seg_override( base, sym );
            break;
        case T_ESP:
            AsmError( ESP_CANNOT_BE_USED_AS_INDEX );
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
            rCode->sib = ( ss | (Reg386(index) << 3) | Reg386(base) );
            seg_override( base, sym );
        }
    }
    if( Opnd_Count == OPND2 ) {
        // shift the register field to left by 3 bit
        rCode->info.rm_byte = mod_field | ( rm_field << 3 ) | ( rCode->info.rm_byte & BIT_012 );
    } else if( Opnd_Count == OPND1 ) {
        rCode->info.rm_byte = mod_field | rm_field;
    }
    return( NOT_ERROR );
}

static int comp_opt( uint direct )
/********************************/
/*
  Compare function for CPU directive
*/
{
    // follow Microsoft MASM
    switch( direct ) {
    case T_DOT_NO87:
        return( P_NO87 );
    case T_DOT_8086:
        return( P_86 );
    case T_DOT_8087:
        return( P_87 );
    case T_DOT_186:
        return( P_186 );
    case T_DOT_286:
        return( P_286 );
    case T_DOT_287:
        return( P_287 );
    case T_DOT_286P:
        return( P_286p );
    case T_DOT_386:
        return( P_386 );
    case T_DOT_387:
        return( P_387 );
    case T_DOT_386P:
        return( P_386p );
    case T_DOT_486:
        return( P_486 );
    case T_DOT_486P:
        return( P_486p );
    case T_DOT_586:
        return( P_586 );
    case T_DOT_586P:
        return( P_586p );
    case T_DOT_686:
        return( P_686 );
    case T_DOT_686P:
        return( P_686p );
    case T_DOT_MMX:
        return( P_MMX );
    case T_DOT_K3D:
        return( P_K3D | P_MMX );
    case T_DOT_XMM:
        return( P_SSE | P_MMX );
    case T_DOT_XMM2:
        return( P_SSE2 | P_SSE | P_MMX );
    case T_DOT_XMM3:
        return( P_SSE3 | P_SSE2 | P_SSE | P_MMX );
    default:
        // not found
        return( EMPTY );
    }
}

static int def_fpu( uint direct )
/********************************/
/*
  get FPU from CPU directive
*/
{
    switch( direct ) {
    case T_DOT_8086:
    case T_DOT_186:
        return( P_87 );
    case T_DOT_286:
    case T_DOT_286P:
        return( P_287 );
    case T_DOT_386:
    case T_DOT_386P:
    case T_DOT_486:
    case T_DOT_486P:
    case T_DOT_586:
    case T_DOT_586P:
    case T_DOT_686:
    case T_DOT_686P:
        return( P_387 );
    default:
        return( 0 );
    }
}

#if defined( _STANDALONE_ )
static void MakeCPUConstant( int i )
/**********************************/
{
#if 0
    MakeConstantUnderscored( i );

    switch( i ) {
    // fall right through
    case T_DOT_686P:
    case T_DOT_686:
        MakeConstantUnderscored( T_DOT_686 );
    case T_DOT_586P:
    case T_DOT_586:
        MakeConstantUnderscored( T_DOT_586 );
    case T_DOT_486P:
    case T_DOT_486:
        MakeConstantUnderscored( T_DOT_486 );
    case T_DOT_386P:
    case T_DOT_386:
        MakeConstantUnderscored( T_DOT_386 );
        break;
    case T_DOT_286P:
    case T_DOT_286:
        MakeConstantUnderscored( T_DOT_286 );
    }
#endif
    return;
}
#endif

int cpu_directive( int i )
/************************/
// handles .8086, .80386
{
    int                 temp;

    if( ( temp = comp_opt( i ) ) != EMPTY ) {
        if( i == T_DOT_NO87 ) {
            CodeInfo->info.cpu &= ~P_FPU_MASK;                 // turn off FPU bits
        } else if( temp & P_EXT_MASK ) {
            CodeInfo->info.cpu |= temp & P_EXT_MASK;           // turn on desired bit(s)
        } else if( temp & P_FPU_MASK ) {
            CodeInfo->info.cpu &= ~P_FPU_MASK;
            CodeInfo->info.cpu |= temp & P_FPU_MASK;           // setup FPU bits
        } else {
            CodeInfo->info.cpu &= ~( P_CPU_MASK | P_PM );
            CodeInfo->info.cpu |= temp & ( P_CPU_MASK | P_PM );// setup CPU bits
            CodeInfo->info.cpu &= ~P_FPU_MASK;
            CodeInfo->info.cpu |= def_fpu( i ) & P_FPU_MASK;   // setup FPU bits
        }
        /* set the MASM compatible @CPU value */
        // 0001: 86
        // 0002: 186
        // 0004: 286
        // 0008: 386
        // 0010: 486
        // 0020: Pentium
        // 0040: PPro
        // 0080: protected instructions ok
        // 0100: 87
        // 0400: 287
        // 0800: 387
        DebugMsg(("CodeInfo->info.cpu=%X\n", CodeInfo->info.cpu));
        ModuleInfo.cpu = 1;
        temp = CodeInfo->info.cpu & P_CPU_MASK;
        switch (temp) {
        case P_186:
            ModuleInfo.cpu = ModuleInfo.cpu | 0x3;
            break;
        case P_286:
            ModuleInfo.cpu = ModuleInfo.cpu | 0x7;
            break;
        case P_386:
            ModuleInfo.cpu = ModuleInfo.cpu | 0xF;
            break;
        case P_486:
            ModuleInfo.cpu = ModuleInfo.cpu | 0x1F;
            break;
        case P_586:
            ModuleInfo.cpu = ModuleInfo.cpu | 0x3F;
            break;
        case P_686:
            ModuleInfo.cpu = ModuleInfo.cpu | 0x5F;
            break;
        }
        if (CodeInfo->info.cpu & P_PM)
            ModuleInfo.cpu = ModuleInfo.cpu | 0x80;

        temp = CodeInfo->info.cpu & P_FPU_MASK;
        switch (temp) {
        case P_87:
            ModuleInfo.cpu = ModuleInfo.cpu | 0x100;
            break;
        case P_287:
            ModuleInfo.cpu = ModuleInfo.cpu | 0x500;
            break;
        case P_387:
            ModuleInfo.cpu = ModuleInfo.cpu | 0xD00;
            break;
        }
    } else {
        AsmError( UNKNOWN_DIRECTIVE );
        return( ERROR );
    }

#if defined( _STANDALONE_ )
    MakeCPUConstant( i );
    switch( i ) {
    case T_DOT_686P:
    case T_DOT_686:
    case T_DOT_586P:
    case T_DOT_586:
    case T_DOT_486P:
    case T_DOT_486:
    case T_DOT_386P:
    case T_DOT_386:
        SetUse32Def( TRUE );
        break;
    case T_DOT_286P:
    case T_DOT_286:
    case T_DOT_186:
    case T_DOT_8086:
        SetUse32Def( FALSE );
        break;
    default:
        // set FPU
        break;
    }
    /* Set @Cpu */
    /* differs from Codeinfo cpu setting */

    DebugMsg(("cpu_directive: @Cpu=%X\n", ModuleInfo.cpu));
    CreateConstant( "@Cpu", ModuleInfo.cpu, -1, TRUE);

#endif

    return( NOT_ERROR );
}

static int idata_float( long value )
/**********************************/
/*
  check the correct operand/data size for float immediate operand;
*/
{
    switch( CodeInfo->mem_type ) {
    case MT_EMPTY:
        if( CodeInfo->info.token == T_PUSHW ) { // sigh. another special case
            // expect 32-bit code but get 16-bit
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        }
        break;
    case MT_FAR:
    case MT_NEAR:
    case MT_SHORT:
#if defined( _STANDALONE_ )
    case MT_PROC:
#endif
        AsmError( SYNTAX_ERROR );
        return( ERROR );
#if defined( _STANDALONE_ )
    case MT_SBYTE:
    case MT_SWORD:
#endif
    case MT_BYTE:
    case MT_WORD:
        AsmError( OPERANDS_MUST_BE_THE_SAME_SIZE );
        return( ERROR );
#if defined( _STANDALONE_ )
    case MT_SDWORD:
#endif
    case MT_DWORD:
        // set w-bit
        CodeInfo->info.opcode |= W_BIT;
        break;
    default:
        break;
    }
    SET_OPSIZ_32( CodeInfo );
    CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
    CodeInfo->data[Opnd_Count] = value;
    return( NOT_ERROR );
}

static unsigned char get_sr_rm_byte( enum prefix_reg seg_prefix )
/***************************************************************/
{
    switch( seg_prefix ) {
    case PREFIX_ES:
        return( 0 );
    case PREFIX_CS:
        return( 1 );
    case PREFIX_SS:
        return( 2 );
    case PREFIX_DS:
        return( 3 );
    case PREFIX_FS:
        return( 4 );
    case PREFIX_GS:
        return( 5 );
    default:
        break;
    }
#if defined( _STANDALONE_ )
    /**/myassert( 0 );
#endif
    return( 0 );
}

static int process_jumps( expr_list *opndx )
/********************************************/
/*
  parse the jumps instructions operands
*/
{
    int         temp;
    bool        flag;

    segm_override_jumps( opndx );

    flag = ( opndx->explicit ) ? TRUE : FALSE ;
    if( ptr_operator( opndx->mem_type, flag ) == ERROR )
        return( ERROR );
    if( ptr_operator( MT_PTR, flag ) == ERROR ) {
        return( ERROR );
    }
    if( opndx->mbr != NULL ) {
        flag = FALSE;
        if( ptr_operator( opndx->mbr->mem_type, flag ) == ERROR )
            return( ERROR );
        if( ptr_operator( MT_PTR, flag ) == ERROR ) {
            return( ERROR );
        }
    }
    temp = jmp( opndx );
    switch( temp ) {
    case ERROR:
        return( ERROR );
    case SCRAP_INSTRUCTION:
        return( SCRAP_INSTRUCTION );
    case INDIRECT_JUMP:
        return( ERROR );
    default:
        return( NOT_ERROR );
    }
}

static int segm_override_jumps( expr_list *opndx )
/******************************************/
{
    if( opndx->override != EMPTY ) {
        if( AsmBuffer[opndx->override]->token == T_REG ) {
            CodeInfo->prefix.seg = AsmOpTable[AsmOpcode[AsmBuffer[opndx->override]->value].position].opcode;
        } else {
#if defined( _STANDALONE_ )
            if( FixOverride( opndx->override ) != NOT_ERROR ) {
                return( ERROR );
            }
#endif
        }
    } else {
    }
    return( NOT_ERROR );
}

static int segm_override_idata( expr_list *opndx )
/******************************************/
{
    if( opndx->override != EMPTY ) {
        if( AsmBuffer[opndx->override]->token == T_REG ) {
            CodeInfo->prefix.seg = AsmOpTable[AsmOpcode[AsmBuffer[opndx->override]->value].position].opcode;
        } else {
#if defined( _STANDALONE_ )
            if( FixOverride( opndx->override ) != NOT_ERROR ) {
                return( ERROR );
            }
#endif
        }
    } else {
    }
    return( NOT_ERROR );
}

static int segm_override_memory( expr_list *opndx )
/******************************************/
{
    if( opndx->override != EMPTY ) {
        if( AsmBuffer[opndx->override]->token == T_REG ) {
            CodeInfo->prefix.seg = AsmOpTable[AsmOpcode[AsmBuffer[opndx->override]->value].position].opcode;
        } else {
#if defined( _STANDALONE_ )
            if( FixOverride( opndx->override ) != NOT_ERROR ) {
                return( ERROR );
            }
#endif
        }
    } else {
    }
    return( NOT_ERROR );
}

static int idata_nofixup( expr_list *opndx )
/******************************************/
{
    OPNDTYPE    op_type;
    long        value;

    DebugMsg(("idata_nofixup(type=%u mem_type=%u value=%X) enter [CodeInfo->mem_type=%u]\n", opndx->type, opndx->mem_type, opndx->value, CodeInfo->mem_type));

    if( IS_ANY_BRANCH( CodeInfo->info.token ) ) {  // jumps/call processing
        return( process_jumps( opndx ) );
    }
    value = opndx->value;
    CodeInfo->data[Opnd_Count] = value;

    switch( CodeInfo->mem_type ) {
    case MT_EMPTY:
        if( CodeInfo->info.token == T_PUSH ) {
            if( opndx->explicit ) {
                if( opndx->mem_type == MT_BYTE ) {
                    op_type = OP_I8;
                } else if( opndx->mem_type == MT_WORD ) {
                    op_type = OP_I16;
                    SET_OPSIZ_16( CodeInfo );
                } else if( opndx->mem_type == MT_DWORD ) {
                    op_type = OP_I32;
                    SET_OPSIZ_32( CodeInfo );
                } else {
                    // FIXME !!
                    op_type = OP_I32;
                    SET_OPSIZ_32( CodeInfo );
                }
                break;
            }
            if( CodeInfo->use32 ) {
                if( (int_8)value == (int_32)value ) {
                    op_type = OP_I8;
                } else {
                    op_type = OP_I32;
                }
            } else if( (unsigned long)value > USHRT_MAX ) {
                SET_OPSIZ_32( CodeInfo );
                if( (int_8)value == (int_32)value ) {
                    op_type = OP_I8;
                } else {
                    op_type = OP_I32;
                }
            } else {
                if( (int_8)value == (int_16)value ) {
                    op_type = OP_I8;
                } else {
                    op_type = OP_I16;
                }
            }
            break;
        } else if( CodeInfo->info.token == T_PUSHW ) {
            op_type = OP_I16;
            if( (int_8)value == (int_16)value ) {
                op_type = OP_I8;
            }
            break;
        } else if( CodeInfo->info.token == T_PUSHD ) {
            op_type = OP_I32;
            if( (int_8)value == (int_32)value ) {
                op_type = OP_I8;
            }
            break;
        } else {
            if (opndx->explicit) {
                int size = SizeFromMemtype(opndx->mem_type, 0);
                if (size == 1)
                    op_type = OP_I8;
                else if (size == 2)
                    op_type = OP_I16;
                else if (size == 4)
                    op_type = OP_I32;
                if (size >= 1 && size <= 4)
                    break;
            }
        }
        if( ( value > SHRT_MAX ) || ( value < SHRT_MIN ) ) {
            op_type = OP_I32;
        } else if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            op_type = OP_I16;
        } else {
            op_type = OP_I8;
        }
        break;
    case MT_FAR:
        if( !CodeInfo->use32 ) {
            op_type = OP_J48;
        } else {
            op_type = OP_J32;
        }
        break;
    case MT_NEAR:
        if( !CodeInfo->use32 ) {
            op_type = OP_I16;
        } else {
            op_type = OP_I32;
        }
        break;
    case MT_SHORT:
        if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            // expect 8-bit but got 16 bit
            AsmError( JUMP_OUT_OF_RANGE );
            return( ERROR );
        } else {
            op_type = OP_I8;
        }
        break;
    case MT_BYTE:
        if( !InRange( value, 1 ) ) {
            // expect 8-bit but got 16/32 bit
            AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
            return( ERROR );
        } else {
            op_type = OP_I8;
        }
        break;
#if defined( _STANDALONE_ )
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
        // set w-bit
        CodeInfo->info.opcode |= W_BIT;
        break;
#endif
    case MT_WORD:
#if defined( _STANDALONE_ )
        if( Options.sign_value ) {
            if( !InRange( value, 2 ) ) {
                AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
                return( ERROR );
            } else if( value > UCHAR_MAX ) {
                op_type = OP_I16;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->info.opcode |= W_BIT;
        } else {
#endif
            if( !InRange( value, 2 ) ) {
                AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
                return( ERROR );
            } else if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I16;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->info.opcode |= W_BIT;
#if defined( _STANDALONE_ )
        }
#endif
        break;
#if defined( _STANDALONE_ )
   case MT_SDWORD:
        if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            op_type = OP_I32;
        } else {
            op_type = OP_I8;
        }
        // set w-bit
        CodeInfo->info.opcode |= W_BIT;
        break;
#endif
    case MT_DWORD:
#if defined( _STANDALONE_ )
        if( Options.sign_value ) {
            if( value > UCHAR_MAX ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->info.opcode |= W_BIT;
        } else {
#endif
            if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->info.opcode |= W_BIT;
#if defined( _STANDALONE_ )
        }
#endif
        break;
    case MT_QWORD:
#if defined( _STANDALONE_ )
        if( Options.sign_value ) {
            if( value > UCHAR_MAX ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->info.opcode |= W_BIT;
        } else {
#endif
            if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->info.opcode |= W_BIT;
#if defined( _STANDALONE_ )
        }
#endif
        break;
    default:
        break;
    }
    CodeInfo->info.opnd_type[Opnd_Count] = op_type;
    DebugMsg(("idata_nofixup exit\n"));
    return( NOT_ERROR );
}

// create immediate data with fixup (offsets)

static int idata_fixup( expr_list *opndx )
/****************************************/
{
    struct asmfixup     *fixup;
    enum fixup_types    fixup_type;
    int                 type;
    int                 sym32; // 1=32bit, 0=16bit offset for fixup

    DebugMsg(("idata_fixup(type=%u, mem_type=%u) enter [CodeInfo.mem_type=%u]\n", opndx->type, opndx->mem_type, CodeInfo->mem_type));

    if( IS_ANY_BRANCH( CodeInfo->info.token ) ) {  // jumps/call processing
        return( process_jumps( opndx ) );
    }
    CodeInfo->data[Opnd_Count] = opndx->value;
    segm_override_idata( opndx );

#if defined( _STANDALONE_ )
    if( ( opndx->sym->state == SYM_SEG )
        || ( opndx->sym->state == SYM_GRP )
        || ( opndx->instr == T_SEG ) ) {
        sym32 = 0;
    } else if( opndx->abs ) {
        sym32 = 0;
    } else {
        sym32 = SymIs32( opndx->sym );
    }
#else
    sym32 = CodeInfo->use32;
#endif
    if( opndx->instr != EMPTY ) {
        if( ( opndx->base_reg != EMPTY )
            || ( opndx->idx_reg != EMPTY ) ) {
            AsmError( INVALID_MEMORY_POINTER );
            return( ERROR );
        }
        if( opndx->sym->state == SYM_STACK ) {
            AsmError( CANNOT_OFFSET_AUTO );
            return( ERROR );
        }
    }
    if (opndx->instr == T_OFFSET) {
        if( MEM_TYPE( CodeInfo->mem_type, BYTE ) ) {
            AsmError( OFFSET_TOO_LARGE );
            return( ERROR );
        }
        if( opndx->sym->state == SYM_GRP ) {
            AsmError( CANNOT_OFFSET_GRP );
            return( ERROR );
        }
    }

    switch( CodeInfo->mem_type ) {
    case MT_EMPTY:
        if( Opnd_Count > OPND1 ) {
            type = OperandSize( CodeInfo->info.opnd_type[OPND1] );
            if( type == 4 ) {
                CodeInfo->mem_type = MT_DWORD;
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                SET_OPSIZ_32( CodeInfo );
                break;
            } else if( type == 2 ) {
                CodeInfo->mem_type = MT_WORD;
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
                SET_OPSIZ_16( CodeInfo );
                break;
            }
        }
        if( opndx->abs ) {
            if( opndx->mem_type == MT_BYTE ) {
                CodeInfo->mem_type = MT_BYTE;
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                break;
            } else if( opndx->mem_type == MT_EMPTY ) {
                SET_OPSIZ_NO( CodeInfo );
                if( oper_32( CodeInfo ) ) {
                    CodeInfo->mem_type = MT_DWORD;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                    sym32 = 1;
                    break;
                }
            } else if( opndx->mem_type == MT_DWORD ) {
                sym32 = 1;
            }
        }
        /* if a WORD size is given, don't override it with */
        /* anything what might look better at first glance */
        if( opndx->mem_type != MT_WORD )
            if( ( CodeInfo->info.token == T_PUSHD ) || sym32 ) {
                CodeInfo->mem_type = MT_DWORD;
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                SET_OPSIZ_32( CodeInfo );
                break;
            }
        CodeInfo->mem_type = MT_WORD;
        // no break
#if defined( _STANDALONE_ )
    case MT_SWORD:
#endif
    case MT_WORD:
        CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
        SET_OPSIZ_16( CodeInfo );
        break;
#if defined( _STANDALONE_ )
    case MT_SDWORD:
#endif
    case MT_DWORD:
        CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
        SET_OPSIZ_32( CodeInfo );
        break;
    default:
        break;
    }
    if( opndx->instr == T_SEG ) {
        fixup_type = FIX_SEG;
    } else {
        if( CodeInfo->mem_type == MT_BYTE ) {
            fixup_type = FIX_LOBYTE;
        } else if( oper_32( CodeInfo ) ) {
            fixup_type = ( sym32 ) ? FIX_OFF32 : FIX_OFF16;
        } else {
            if( sym32 ) {
                // fixme !!!! warning
                // operand size is 16bit
                // but fixup is 32-bit
            }
            fixup_type = FIX_OFF16;
        }
    }
    ConstantOnly = TRUE;
    CodeInfo->info.opcode |= W_BIT;

#if defined( _STANDALONE_ )
    find_frame( opndx->sym );
#endif

    fixup = AddFixup( opndx->sym, fixup_type, OPTJ_NONE );
//    add_frame();  ???
    if( fixup == NULL ) {
        return( ERROR );
    } else {
        return( NOT_ERROR );
    }
}

// important function
// tries to create a fixup for memory operands
// currently stack variables also get a fixup (strange)
// but fortunately it won't find its way to the object module
// in: opndx=operand to process
// in: Opnd_Count=no of operand (1,2)
// out: CodeInfo

static int memory_operand( expr_list *opndx, bool with_fixup )
/************************************************************/
{
    char                ss = SCALE_FACTOR_1;
    int                 index = EMPTY;
    int                 base = EMPTY;
    struct asm_sym      *sym;
    char                base_lock = FALSE;
    enum fixup_types    fixup_type;
    int                 flag;
#if defined( _STANDALONE_ )
    int                 sym32;
#endif

    DebugMsg(("memory_operand(opndx->value=%u, fixup=%u) enter, [CodeInfo->memtype=%u]\n", opndx->value, with_fixup, CodeInfo->mem_type));
    CodeInfo->data[Opnd_Count] = opndx->value;
    CodeInfo->info.opnd_type[Opnd_Count] = OP_M; //OP_M == 0x870000
    sym = opndx->sym;

    segm_override_memory( opndx );

    flag = ( opndx->explicit ) ? TRUE : FALSE ;
#if 1
    /* there's some confusion about PTR, because it's sometimes an operator
       and sometimes a TYPE. However, memory type MT_PTR is always a TYPE! */
    if (opndx->mem_type == MT_PTR ||
        opndx->mem_type == MT_NEAR ||
        opndx->mem_type == MT_FAR) {
        int size;
        switch (opndx->ofs_size) {
        case OFSSIZE_16:
            if (opndx->mem_type == MT_FAR)
                opndx->mem_type = MT_DWORD;
            else
                opndx->mem_type = MT_WORD;
            break;
        case OFSSIZE_32:
            if (opndx->mem_type == MT_FAR)
                opndx->mem_type = MT_FWORD;
            else
                opndx->mem_type = MT_DWORD;
            break;
        default:
            if (flag == FALSE) {
                if (opndx->mbr && opndx->mbr->state == SYM_TYPE)
                    size = sym->type->total_size;
                else
                    size = SizeFromMemtype(opndx->mem_type, NULL);
                switch (size) {
                case 2:
                    opndx->mem_type = MT_WORD;
                    break;
                case 4:
                    opndx->mem_type = MT_DWORD;
                    break;
                case 6:
                    opndx->mem_type = MT_FWORD;
                    break;
                }
            }
        }
    }
#endif
    if( ptr_operator( opndx->mem_type, flag ) == ERROR )
        return( ERROR );
    if( ptr_operator( MT_PTR, flag ) == ERROR ) {
        return( ERROR );
    }
    if( opndx->mbr != NULL ) {
        flag = FALSE;
        if( ptr_operator( opndx->mbr->mem_type, flag ) == ERROR )
            return( ERROR );
        if( ptr_operator( MT_PTR, flag ) == ERROR ) {
            return( ERROR );
        }
    }

    if (opndx->base_reg != EMPTY )
        base = AsmBuffer[opndx->base_reg]->value;

    // check for base registers

    if(base != EMPTY ) {
        switch( base ) {
        case T_EAX:
        case T_EBX:
        case T_ECX:
        case T_EDX:
        case T_ESP:
        case T_EBP:
        case T_ESI:
        case T_EDI:
            if( ( CodeInfo->info.cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
                // 286 and down cannot use 386 registers
                AsmError( CANNOT_USE_386_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING );
                return( ERROR );
            }
            SET_ADRSIZ_32( CodeInfo );
            break;
        case T_BX:
        case T_BP:
        case T_SI:
        case T_DI:
            SET_ADRSIZ_16( CodeInfo );
            break;
        default:
            AsmError( INVALID_MEMORY_POINTER );
            return( ERROR );
        }
    }

    // check for index registers

    if( opndx->idx_reg != EMPTY ) {
        index = AsmBuffer[opndx->idx_reg]->value;
        switch( index ) {
        case T_EAX:
        case T_EBX:
        case T_ECX:
        case T_EDX:
        case T_ESP:
        case T_EBP:
        case T_ESI:
        case T_EDI:
            if( ( CodeInfo->info.cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
                // 286 and down cannot use 386 registers
                AsmError( CANNOT_USE_386_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING );
                return( ERROR );
            }
            SET_ADRSIZ_32( CodeInfo );
            break;
        case T_BX:
        case T_BP:
        case T_SI:
        case T_DI:
            SET_ADRSIZ_16( CodeInfo );
            break;
        default:
            AsmError( INVALID_MEMORY_POINTER );
            return( ERROR );
        }
        if( AsmBuffer[opndx->idx_reg]->value == T_ESP ) {
            if( opndx->scale == 1 ) {
                index = base;
                base = AsmBuffer[opndx->idx_reg]->value;
            } else {
                AsmError( ESP_CANNOT_BE_USED_AS_INDEX );
                return( ERROR );
            }
        }
        if( addr_32( CodeInfo ) ) {
            if( ( CodeInfo->info.cpu & P_CPU_MASK ) >= P_386 ) {
                if( !CodeInfo->use32 )
                    CodeInfo->prefix.adrsiz = TRUE;
                switch( index ) {
                case T_ESP:
                case T_BX:
                case T_BP:
                case T_SI:
                case T_DI:
                    // cannot use ESP or 16-bit reg as index
                    AsmError( INVALID_INDEX_REGISTER );
                    return( ERROR );
                default:
                    if( !CodeInfo->use32 )
                        CodeInfo->prefix.adrsiz = TRUE;
                    switch( opndx->scale ) {
                    case 1:
                        // ss = 00
                        break;
                    case 2:
                        // ss = 01
                        ss = SCALE_FACTOR_2;
                        break;
                    case 4:
                        // ss = 10
                        ss = SCALE_FACTOR_4;
                        break;
                    case 8:
                        // ss = 11
                        ss = SCALE_FACTOR_8;
                        break;
                    default: // must be * 1, 2, 4 or 8
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
        switch( sym->state ) {
        case SYM_UNDEFINED:
            // forward reference
            break;
#if defined( _STANDALONE_ )
        case SYM_SEG:
        case SYM_GRP:
            // error !!!!!
            break;
#endif
        default:
            if( CodeInfo->mem_type == MT_EMPTY ) {
                if( ptr_operator( sym->mem_type, FALSE ) == ERROR )
                    return( ERROR );
                if( ptr_operator( MT_PTR, FALSE ) == ERROR ) {
                    return( ERROR );
                }
            }
            break;
        }

#if defined( _STANDALONE_ )

        if( opndx->abs ) {
            sym32 = addr_32( CodeInfo );
        } else {
            sym32 = SymIs32( sym );
        }
        if( ( opndx->base_reg == EMPTY ) && ( opndx->idx_reg == EMPTY ) ) {
            SET_ADRSIZ( CodeInfo, sym32 );
            fixup_type = ( sym32 ) ? FIX_OFF32 : FIX_OFF16;
        } else {
            if( addr_32( CodeInfo ) ) {
                fixup_type = ( sym32 ) ? FIX_OFF32 : FIX_OFF16;
            } else {
                if( sym32 ) {
                    // fixme !!!! warning
                    // address size is 16bit
                    // but fixup is 32-bit
                }
                fixup_type = FIX_OFF16;
            }
        }

        DebugMsg(("memory_operand: calling AddFixup(%s, %u)\n", sym->name, fixup_type));
        AddFixup( sym, fixup_type, OPTJ_NONE );

        if( Modend ) {
            GetAssume( sym, ASSUME_NOTHING );
        } else {
            if( mem2code( ss, index, base, sym ) == ERROR ) {
                return( ERROR );
            }
        }
        add_frame();
#else
        fixup_type = ( CodeInfo->use32 ) ? FIX_OFF32 : FIX_OFF16;

        AddFixup( sym, fixup_type, OPTJ_NONE );

        if( mem2code( ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
#endif
    } else { /* branch without fixup */
#if 1
        /* if an undefined label is moved (mov [xxx],offset yyy)
         change to immediate data */
        if (sym && sym->state == SYM_UNDEFINED && Opnd_Count == OPND2)
            CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
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
            if( CodeInfo->use32 ) {
                base = T_EBP;
            } else {
                base = T_BP;
            }
            base_lock = TRUE;   // add lock
            if( CodeInfo->mem_type == MT_EMPTY ) {
                if( ptr_operator( sym->mem_type, FALSE ) == ERROR )
                    return( ERROR );
                if( ptr_operator( MT_PTR, FALSE ) == ERROR ) {
                    return( ERROR );
                }
            }
        }
        if( mem2code( ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
    }
    if( IS_JMPCALL( CodeInfo->info.token ) ) {
        if( CodeInfo->mem_type == MT_DWORD ) {
            if( !CodeInfo->use32 ) {
                CodeInfo->isfar = TRUE;
            }
        }
    }
    DebugMsg(("memory_operand exit, no error, type/value=%X/%X\n", opndx->type, opndx->value));
    return( NOT_ERROR );
}

static int process_address( expr_list *opndx )
/********************************************/
/*
  parse the memory reference operand
  */
{
    memtype     mem_type;
    /* this is to make function ptrs (MT_PTR) work */
#if 0
    if( opndx->indirect ) {           // indirect operand
#else
    if( opndx->indirect || opndx->mem_type == MT_PTR) {
#endif
        DebugMsg(("process_address: INDIRECT, sym=%X\n", opndx->sym));
        if( opndx->sym == NULL || opndx->sym->state == SYM_STACK) {
            return( memory_operand( opndx, FALSE ) );
        } else {
            return( memory_operand( opndx, TRUE ) );
        }
    } else {                          // direct operand
        if( opndx->instr != EMPTY ) { // OFFSET ..., SEG ...
            DebugMsg(("process_address: DIRECT, fixup\n"));
            if( opndx->sym == NULL ) {
                return( idata_nofixup( opndx ) );
            } else {
                return( idata_fixup( opndx ) );
            }
        } else {                      // direct operand only
            if( opndx->sym == NULL ) {       // without symbol
                if( opndx->override != EMPTY ) {
                    // direct absolute memory without fixup ... DS:[0]
                    return( memory_operand( opndx, FALSE ) );
                } else {
                    return( idata_nofixup( opndx ) );  // error ????
                }
            } else {                  // with symbol
#if defined( _STANDALONE_ )
                DebugMsg(("process_address: DIRECT with symbol, name=%s, state=%X\n", opndx->sym->name, opndx->sym->state));
                if( ( opndx->sym->state == SYM_UNDEFINED ) && !opndx->explicit ) {
                    if( Parse_Pass != PASS_1 ) {
                        AsmErr( SYMBOL_NOT_DEFINED, opndx->sym->name );
                        return( ERROR );
                    }
                    // undefined symbol, it is not possible to determine
                    // operand type and size
                    switch( CodeInfo->info.token ) {
                    case T_PUSH:
                    case T_PUSHW:
                    case T_PUSHD:
                        return( idata_nofixup( opndx ) );
                        break;
                    default:
                        if( IS_ANY_BRANCH( CodeInfo->info.token ) ) {  // jumps/call processing
                            return( idata_nofixup( opndx ) );
                        } else {
                            return( memory_operand( opndx, FALSE ) );
                        }
                        break;
                    }
#else
                if( ( opndx->sym->state == SYM_UNDEFINED ) && !opndx->explicit ) {
                    // undefined symbol, it is not possible to determine
                    // operand type and size
                    switch( CodeInfo->info.token ) {
                    case T_PUSH:
                    case T_PUSHW:
                    case T_PUSHD:
                        return( idata_nofixup( opndx ) );
                        break;
                    default:
                        if( IS_ANY_BRANCH( CodeInfo->info.token ) ) {  // jumps/call processing
                            return( idata_nofixup( opndx ) );
                        } else {
                            return( memory_operand( opndx, TRUE ) );
                        }
                        break;
                    }
#endif
#if defined( _STANDALONE_ )
                } else if( ( opndx->sym->state == SYM_SEG )
                    || ( opndx->sym->state == SYM_GRP ) ) {
                    // SEGMENT and GROUP symbol is converted to SEG symbol
                    // for next prrocessing
                    opndx->instr = T_SEG;
                    return( idata_fixup( opndx ) );
#endif
                } else {
                    DebugMsg(("process_address direct CALL/JMP/...\n"));
                    // CODE location is converted to OFFSET symbol
                    mem_type = ( opndx->explicit ) ? opndx->mem_type : opndx->sym->mem_type;
#if defined( _STANDALONE_ )
                    if( opndx->abs ) {
                        return( idata_fixup( opndx ) );
                    }
#endif
                    switch( mem_type ) {
                    case MT_FAR:
                    case MT_NEAR:
                    case MT_SHORT:
#if defined( _STANDALONE_ )
                    case MT_PROC:
#endif
                        if( CodeInfo->info.token == T_LEA ) {
                            return( memory_operand( opndx, TRUE ) );
#if defined( _STANDALONE_ )
                        } else if( IS_SYM_COUNTER( opndx->sym->name ) ) {
                            return( idata_fixup( opndx ) );
#endif
                        } else if( opndx->mbr != NULL ) { // structure or structure member
                            return( memory_operand( opndx, TRUE ) );
                        } else {
                            return( idata_fixup( opndx ) );
                        }
                        break;
                    default:
                        // direct memory with fixup
                        return( memory_operand( opndx, TRUE ) );
                        break;
                    }
                }
            }
        }
    }
    return( NOT_ERROR );
}

// handle constant operands
// these might also need a fixup if they are externals (EXTERN:ABS!)

static int process_const( expr_list *opndx )
/******************************************/
{
    if( ( CodeInfo->info.token == T_IMUL )
        && ( CodeInfo->info.opnd_type[OPND1] & OP_R ) ) {
        if( Opnd_Count == OPND2 ) {
            CodeInfo->info.rm_byte = ( CodeInfo->info.rm_byte & ~BIT_345 )
                          | ( ( CodeInfo->info.rm_byte & BIT_012 ) << 3 );
        } else if( Opnd_Count == OPND3 ) {
            CodeInfo->info.opnd_type[OPND1] = CodeInfo->info.opnd_type[OPND2];
            CodeInfo->info.opnd_type[OPND2] = OP_NONE;
            CodeInfo->data[OPND1] = CodeInfo->data[OPND2];
            CodeInfo->data[OPND2] = 0;
            InsFixups[OPND1] = InsFixups[OPND2];
            InsFixups[OPND2] = NULL;
            Opnd_Count = OPND2;
        }
    }
    return( idata_nofixup( opndx ) );
}

static int process_reg( expr_list *opndx )
/****************************************/
/*
- parse and encode the register operand;
*/
{
    int                 temp;
    int                 reg;

    if( opndx->indirect )  // simple register indirect operand ... [EBX]
        return( process_address( opndx ) );
    temp = AsmOpcode[AsmBuffer[opndx->base_reg]->value].position;
    reg = AsmOpTable[temp].opcode;
    CodeInfo->info.opnd_type[Opnd_Count] = AsmOpTable[temp].opnd_type[OPND2];
    switch( AsmOpTable[temp].opnd_type[OPND2] ) {
    case OP_AL:
    case OP_R8:
        CodeInfo->info.opcode &= NOT_W_BIT;         // clear w-bit
        break;
    case OP_CL: /* only appears in "shift opnd,CL" instructions */
        break;
    case OP_AX:
    case OP_DX: /* only appears in "in" and "out" instructions  */
    case OP_R16:
        CodeInfo->info.opcode |= W_BIT;             // set w-bit
        if( CodeInfo->use32 )
            CodeInfo->prefix.opsiz = TRUE;
        break;
    case OP_MMX:
        break;
    case OP_XMM:
        break;
    case OP_ST:
        temp = opndx->idx_reg & BIT_012;
        CodeInfo->info.rm_byte |= temp;
        if( temp != 0 )
            CodeInfo->info.opnd_type[Opnd_Count] = OP_ST_REG;
        break;
    case OP_SR3:                        // 386 segment register
        if( ( CodeInfo->info.cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
            // 8086 ins cannot use 80386 segment register
            AsmError( CANNOT_USE_386_SEGMENT_REGISTER_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
    case OP_SR:                                 // any seg reg
    case OP_SR2:                                // 8086 segment register
        if( AsmBuffer[opndx->base_reg]->value == T_CS ) {
            // POP CS is not allowed
            if( CodeInfo->info.token == T_POP ) {
                AsmError( POP_CS_IS_NOT_ALLOWED );
                return( ERROR );
            }
        }
        reg = get_sr_rm_byte( AsmOpTable[temp].opcode );
        break;
    case OP_EAX:
    case OP_R32:
        if( ( CodeInfo->info.cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
            // 8086 ins cannot use 386 register
            AsmError( CANNOT_USE_386_REGISTER_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
        CodeInfo->info.opcode |= W_BIT;             // set w-bit
        if( !CodeInfo->use32 )
            CodeInfo->prefix.opsiz = TRUE;
        break;
    case OP_TR:                 // Test registers
        switch( AsmBuffer[opndx->base_reg]->value ) {
        case T_TR3:
        case T_TR4:
        case T_TR5:
            if( ( ( ( CodeInfo->info.cpu & P_CPU_MASK ) < P_486 )
               || ( ( CodeInfo->info.cpu & P_CPU_MASK ) >= P_686 ) )
                && ( ( AsmOpTable[temp].cpu & P_CPU_MASK ) >= P_486 ) ) {
                // TR3, TR4, TR5 are available on 486 only
                AsmError( CANNOT_USE_TR3_TR4_TR5_IN_CURRENT_CPU_SETTING );
                return( ERROR );
            }
            break;
        case T_TR6:
        case T_TR7:
            if( ( ( ( CodeInfo->info.cpu & P_CPU_MASK ) < P_386 )
               || ( ( CodeInfo->info.cpu & P_CPU_MASK ) >= P_686 ) )
                && ( ( AsmOpTable[temp].cpu & P_CPU_MASK ) >= P_386 ) ) {
                // TR6, TR7 are available on 386...586 only
                AsmError( CANNOT_USE_TR3_TR4_TR5_IN_CURRENT_CPU_SETTING );
                return( ERROR );
            }
            break;
        }
    case OP_CR:                 // Control registers
    case OP_DR:                 // Debug registers
        if( CodeInfo->info.token != T_MOV ) {
            AsmError( ONLY_MOV_CAN_USE_SPECIAL_REGISTER );
            return( ERROR );
        }
        break;
    }
    if( Opnd_Count == OPND1 ) {
        // the first operand
        // r/m is treated as a 'reg' field
        CodeInfo->info.rm_byte |= MOD_11;
        // fill the r/m field
        CodeInfo->info.rm_byte |= reg;
    } else {
        // the second operand
        if( ( CodeInfo->info.token == T_XCHG )
            && ( ( CodeInfo->info.opnd_type[OPND1] == OP_AX )
            || ( CodeInfo->info.opnd_type[OPND1] == OP_EAX ) ) ) {
            // XCHG can use short form if op1 is AX or EAX
            CodeInfo->info.rm_byte = ( CodeInfo->info.rm_byte & BIT_67 ) | reg;
        } else {
            // fill reg field with reg
            CodeInfo->info.rm_byte = ( CodeInfo->info.rm_byte & ~BIT_345 ) | ( reg << 3 );
        }
    }
    return( NOT_ERROR );
}

int ParseItems( void )
/******************/
/*
- co-ordinate the parsing process;
- it is a basically a big loop to loop through all the tokens and identify them
  with the switch statement;
*/
{
    int                 i;
    OPNDTYPE            cur_opnd = OP_NONE;
    OPNDTYPE            last_opnd = OP_NONE;
    struct asm_code     *rCode = CodeInfo;
    expr_list           opndx;
    int                 temp;

    DebugMsg(("ParseItems enter, Token_Count=%u\n", Token_Count));

#if 1
    /* avoid to rescan structures if pass is > 1 */
    if (Definition.struct_depth > 0 && Parse_Pass > PASS_1) {
        if (Token_Count > 1 && AsmBuffer[1]->token == T_DIRECTIVE && AsmBuffer[1]->value == T_ENDS)
            ;
        else
            return(NOT_ERROR);
    }
#endif

#if defined( _STANDALONE_ )
    CodeInfo->use32 = Use32;
    i = proc_check();
    if( i == ERROR ) {
        DebugMsg(("ParseItems exit, proc_check error\n"));
        return( ERROR );
    } else if( i == TRUE ) {
        DebugMsg(("ParseItems exit, proc_check causes skip\n"));
        return( NOT_ERROR );
    }
#endif

    //init
    rCode->info.token     = T_NULL;
    rCode->info.opcode    = 0;
    rCode->info.rm_byte   = 0;
    rCode->prefix.ins     = EMPTY;
    rCode->prefix.seg     = EMPTY;
    rCode->prefix.adrsiz  = FALSE;
    rCode->prefix.opsiz   = FALSE;
    rCode->mem_type       = MT_EMPTY;
    rCode->mem_type_fixed = FALSE;
    rCode->extended_ins   = EMPTY;
    rCode->sib            = 0;            // assume ss is *1
    rCode->indirect       = FALSE;
    rCode->isfar          = FALSE;

    for( i = 0; i < 3; i++ ) {
        rCode->info.opnd_type[i] = OP_NONE;
        rCode->data[i] = 0;
        InsFixups[i] = NULL;
    }
    Opnd_Count = 0;
    curr_ptr_type = EMPTY;

#if defined( _STANDALONE_ )
    CheckSeg = TRUE;
    Frame = EMPTY;
    SegOverride = NULL;
#endif

    for( i = 0; i < Token_Count; i++ ) {

        switch( AsmBuffer[i]->token ) {
        case T_INSTRUCTION:
            DebugMsg(("ParseItems T_INSTRUCTION %s\n", AsmBuffer[i]->string_ptr));
            if( last_opnd != OP_NONE ) {
                // illegal operand is put before instruction
                AsmError( SYNTAX_ERROR );
                DebugMsg(("ParseItems exit 18, error\n"));
                return( ERROR );
            }
            cur_opnd = OP_NONE;
#if defined( _STANDALONE_ )
            if( ( AsmBuffer[i+1]->token == T_DIRECTIVE )
                || ( AsmBuffer[i+1]->token == T_COLON ) ) {
                // instruction name is label
                AsmBuffer[i]->token = T_ID;
                i--;
                continue;
            }
#endif
            switch( AsmBuffer[i]->value ) {
            // prefix
            case T_LOCK:
            case T_REP:
            case T_REPE:
            case T_REPNE:
            case T_REPNZ:
            case T_REPZ:
                rCode->prefix.ins = AsmBuffer[i]->value;
                // prefix has to be followed by an instruction
                if( AsmBuffer[i+1]->token != T_INSTRUCTION ) {
                    AsmError( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION );
                    DebugMsg(("ParseItems exit 19, error\n"));
                    return( ERROR );
                }
                continue;
#if defined( _STANDALONE_ )
            case T_RET:
                if( ( CurrProc != NULL ) && ( in_epilogue == 0 ) ) {
                    in_epilogue = 1;
                    return( Ret( i, Token_Count, FALSE ) );
                }
            case T_RETN:
            case T_RETF:
                in_epilogue = 0;
                rCode->info.token = AsmBuffer[i]->value;
                break;
            case T_IRET:
            case T_IRETD:
                if( ( CurrProc != NULL ) && ( in_epilogue == 0 ) ) {
                    in_epilogue = 1;
                    return( Ret( i, Token_Count, TRUE ) );
                }
            case T_IRETF:
            case T_IRETDF:
                in_epilogue = 0;
                rCode->info.token = AsmBuffer[i]->value;
                break;
#endif
            default:
                rCode->info.token = AsmBuffer[i]->value;
                break;
            }

            i++;
            DebugMsg(("ParseItems T_INSTRUCTION: call EvalOperand i=%u for first operand\n", i));
            if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
                DebugMsg(("ParseItems exit 17, error\n"));
                return( ERROR );
            }
            DebugMsg(("ParseItems T_INSTRUCTION EvalOperand type/value/mem_type=%X/%X/%u\n", opndx.type, opndx.value, opndx.mem_type));
            if( opndx.empty ) {
                break;
            }
            switch( opndx.type ) {
            case EXPR_ADDR:
                DebugMsg(("ParseItems operand 1 ADDR\n"));
                temp = process_address( &opndx );
                if( temp == SCRAP_INSTRUCTION )
                    return( SCRAP_INSTRUCTION );
                if( temp == ERROR ) {
                    DebugMsg(("ParseItems exit 16, error\n"));
                    return( ERROR );
                }
                break;
            case EXPR_CONST:
                DebugMsg(("ParseItems operand CONST, opndx.memtype=%u\n", opndx.mem_type));
                process_const( &opndx );
                break;
            case EXPR_REG:
                DebugMsg(("ParseItems operand REG\n"));
                process_reg( &opndx );
                break;
            case EXPR_UNDEF:
                DebugMsg(("ParseItems exit 15, error\n"));
                return( ERROR );
            default:
                DebugMsg(("ParseItems operand def\n"));
                break;
            }
            i--;
            break;
        case T_RES_ID:
            DebugMsg(("ParseItems T_RES_ID, i=%u\n", i));
            if( rCode->info.token == T_NULL ) {
                temp = ( i == 0 ) ? -1 : 0;
                return( data_init( temp, i ) );
            }
            AsmError( SYNTAX_ERROR );
            DebugMsg(("ParseItems exit 14, error\n"));
            return( ERROR );
        case T_DIRECTIVE:
            DebugMsg(("ParseItems T_DIRECTIVE\n"));
            return( directive( i, AsmBuffer[i]->value ) );
            break;
#if defined( _STANDALONE_ )
        case T_DIRECT_EXPR:
            DebugMsg(("ParseItems T_DIRECT_EXPR\n"));
            if( Parse_Pass != PASS_1 ) {
                Modend = TRUE;
                temp = i;
                temp++;
                if( EvalOperand( &temp, Token_Count, &opndx, TRUE ) == ERROR ) {
                    DebugMsg(("ParseItems exit 13, error\n"));
                    return( ERROR );
                }
                DebugMsg(("ParseItems T_DIRECT EvalOperand type/value=%X/%X\n", opndx.type, opndx.value));
                if( !opndx.empty && ( opndx.type == EXPR_ADDR ) ) {
                    process_address( &opndx );
                }
            }
            return( directive( i, AsmBuffer[i]->value ) );
            break;
#endif
        case T_ID:
            DebugMsg(("ParseItems T_ID, i=%u, string=%s\n", i, AsmBuffer[i]->string_ptr));
#if 0
            if( ( last_opnd != OP_NONE )
                && ( last_opnd != OP_M )
                && ( last_opnd != OP_I ) ) {
                AsmError( SYNTAX_ERROR );
                DebugMsg(("ParseItems exit 11, error\n"));
                return( ERROR );
            }
#endif
            if( i == 0 ) {   // a new label
#if ALLOW_STRUCT_INIT
#if defined( _STANDALONE_ )
                /* <ID=SYM_TYPE> ...? */
                if( IsLabelStruct( AsmBuffer[i]->string_ptr )
                    && ( AsmBuffer[i+1]->token != T_DIRECTIVE ) ) {
                    AsmBuffer[i]->token = T_DIRECTIVE;
                    AsmBuffer[i]->value = T_STRUCT;
                    return( data_init( -1, 0 ) );
                }
#endif
#endif

                switch( AsmBuffer[i+1]->token ) {
                case T_COLON:
                    cur_opnd = OP_LABEL;
                    break;
#if ALLOW_STRUCT_INIT
#if defined( _STANDALONE_ )
                case T_ID:
                    /* structure declaration */
                    if( IsLabelStruct( AsmBuffer[i+1]->string_ptr ) ) {
                        AsmBuffer[i+1]->token = T_DIRECTIVE;
                        AsmBuffer[i+1]->value = T_STRUCT;
                    } else {
                        AsmError( SYNTAX_ERROR );
                        DebugMsg(("ParseItems exit 10, error\n"));
                        return( ERROR );
                    }
                    /* fall through */
#endif
#endif
                case T_RES_ID:
                    return( data_init( i, i+1 ) );
                    break;
#if defined( _STANDALONE_ )
                case T_DIRECTIVE:
                    return( directive( i+1, AsmBuffer[i+1]->value ) );
                    break;
#endif
                default:
                    AsmError( SYNTAX_ERROR );
                    DebugMsg(("ParseItems exit 9, i+1=%u, token=%X, string=%s error\n", i+1, AsmBuffer[i+1]->token, AsmBuffer[i+1]->string_ptr));
                    return( ERROR );
                }
            }
            break;
        case T_COMMA:
            DebugMsg(("ParseItems T_COMMA\n"));
            if( Opnd_Count == OPND1 ) {
                Opnd_Count++;
            } else if( Opnd_Count == OPND2 ) {
                switch( rCode->info.token ) {
                case T_SHLD:
                case T_SHRD:
                    switch( AsmBuffer[i+1]->token ) {
                    case T_NUM:
                        break;
                    case T_REG:
                        i++;
                        if( AsmBuffer[i]->value == T_CL ) {
                            break;
                        }
                    default:
                        AsmError( INVALID_SHLD_SHRD_FORMAT );
                        return( ERROR );
                    }
                    break;
                case T_NULL:
                    break;
                default:
                    Opnd_Count++;
                    break;
                }
            } else {
                AsmError( TOO_MANY_COMMAS );
                return( ERROR );
            }
            i++;
            cur_opnd = OP_NONE;
            curr_ptr_type = EMPTY;
#if defined( _STANDALONE_ )
            Frame = EMPTY;
            SegOverride = NULL;
#endif
            DebugMsg(("ParseItems T_COMMA, call EvalOperand for second operand\n"));
            if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
                DebugMsg(("ParseItems exit 8, T_COMMA, EvalOperand failed\n"));
                return( ERROR );
            }
            DebugMsg(("ParseItems T_COMMA EvalOperand type/value=%X/%X\n", opndx.type, opndx.value));
            if( opndx.empty ) {
                break;
            }
            switch( opndx.type ) {
            case EXPR_ADDR:
                DebugMsg(("ParseItems operand 2 ADDR\n"));
                temp = process_address( &opndx );
                if( temp == SCRAP_INSTRUCTION )
                    return( SCRAP_INSTRUCTION );
                if( temp == ERROR ) {
                    DebugMsg(("ParseItems exit 7, error\n"));
                    return( ERROR );
                }
                break;
            case EXPR_CONST:
                DebugMsg(("ParseItems operand 2 CONST, opndx.memtype=%u\n", opndx.mem_type));
                process_const( &opndx );
                break;
            case EXPR_REG:
                DebugMsg(("ParseItems operand 2 REG\n"));
                process_reg( &opndx );
                break;
            case EXPR_UNDEF:
                DebugMsg(("ParseItems exit 6, error\n"));
                return( ERROR );
            default: /* EXPR_EMPTY */
                DebugMsg(("ParseItems operand default\n"));
                break;
            }
            i--;
            break;
        case T_COLON:
            DebugMsg(("ParseItems T_COLON\n"));
            if ( last_opnd == OP_LABEL ) {
                if( AsmBuffer[i+1]->token != T_RES_ID ) {
                    if ((AsmBuffer[i+1]->token == T_COLON) || (CurrProc == NULL)) {
                        if( MakeLabel( AsmBuffer[i-1]->string_ptr, MT_NEAR, NULL, FALSE )==ERROR ) {
                            DebugMsg(("ParseItems exit 5, error\n"));
                            return( ERROR );
                        }
                        if (AsmBuffer[i+1]->token == T_COLON)
                            i++; /* skip the second colon */
                    } else {
                        if( MakeLabel( AsmBuffer[i-1]->string_ptr, MT_NEAR, NULL, TRUE )==ERROR ) {
                            DebugMsg(("ParseItems exit 4, error\n"));
                            return( ERROR );
                        }
                    }
                }
                cur_opnd = OP_NONE;
            } else {
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
                DebugMsg(("ParseItems exit 3, error\n"));
                return( ERROR );
            }
            break;
        case T_FLOAT:
            DebugMsg(("ParseItems T_FLOAT\n"));
            if( idata_float( AsmBuffer[i]->value ) == ERROR ) {
                DebugMsg(("ParseItems exit 2, error\n"));
                return( ERROR );
            }
            if( AsmBuffer[i-1]->token == T_MINUS ) {
                rCode->data[Opnd_Count] ^= 0x80000000;
            }
#if defined( _STANDALONE_ )
            AsmWarn( 4, FLOAT_OPERAND );
#endif
            break;
        default:
            DebugMsg(("ParseItems error, i=%u, token=%X, string=%s\n", i, AsmBuffer[i]->token, AsmBuffer[i]->string_ptr));
#if defined( _STANDALONE_ )
            /* in case string/macro expansion has lead to a "void" */
//            if (Token_Count == 1 && AsmBuffer[0]->token == T_STRING && *(AsmBuffer[0]->string_ptr) == '\0')
//                return(NOT_ERROR);
            /* myassert( 0 ) ;*/
            AsmError( SYNTAX_ERROR );
            return( ERROR );
#endif
        }
        last_opnd = cur_opnd;
    }
    switch( rCode->info.token ) {
    case T_LODS:
    case T_SCAS:
    case T_STOS:
        SizeString( OperandSize( CodeInfo->info.opnd_type[OPND1] ) );
        break;
    }
    if( Opnd_Count > OPND1 ) {
        if( check_size(&opndx) == ERROR ) {
            DebugMsg(("ParseItems exit 1, error\n"));
            return( ERROR );
        }
    }
    DebugMsg(("ParseItems exit, jmp to match_phase_1()\n"));
    return( match_phase_1() );
}

static void SizeString( unsigned op_size )
/****************************************/
{
    /* size an string instruction based on it's operand's size */
    switch( op_size ) {
    case 1:
        CodeInfo->mem_type = MT_BYTE;
        CodeInfo->info.opcode &= NOT_W_BIT;
        if( CodeInfo->use32 )
            CodeInfo->prefix.opsiz = FALSE;
        break;
    case 2:
        CodeInfo->mem_type = MT_WORD;
        CodeInfo->info.opcode |= W_BIT;
        CodeInfo->prefix.opsiz = CodeInfo->use32 ? TRUE : FALSE;
        break;
    case 4:
        CodeInfo->mem_type = MT_DWORD;
        CodeInfo->info.opcode |= W_BIT;
        CodeInfo->prefix.opsiz = CodeInfo->use32 ? FALSE : TRUE;
        break;
    }
}

static int check_size( expr_list * opndx )
/***************************/
/*
- use to make sure the size of first operand match the size of second operand;
- optimize MOV instruction;
*/
{
    OPNDTYPE    op1 = CodeInfo->info.opnd_type[OPND1];
    OPNDTYPE    op2 = CodeInfo->info.opnd_type[OPND2];
    int         state = NOT_ERROR;
    int         temp;
    int         op1_size;
    int         op2_size;
    int         op_size = 0;

    DebugMsg(("check_size enter\n"));
    switch( CodeInfo->info.token ) {
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
            op_size = OperandSize( op2 );
            if( op_size >= 2 ) {
                AsmError( OP_2_TOO_BIG );
                state = ERROR;
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
                CodeInfo->info.opcode &= NOT_W_BIT;         // clear w-bit
            case OP_EAX:
                if( CodeInfo->use32 ) {
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
                CodeInfo->info.opcode &= NOT_W_BIT;         // clear w-bit
            case OP_EAX:
                if( CodeInfo->use32 ) {
                    CodeInfo->prefix.opsiz = FALSE;
                }
            }
        }
        break;
    case T_INS:
    case T_CMPS:
        op_size = OperandSize( op1 );
        /* fall through */
    case T_MOVS:
    case T_OUTS:
        if( op_size == 0 )
            op_size = OperandSize( op2 );

        SizeString( op_size );
        break;
    case T_LEA:
        switch( OperandSize( op1 ) ) {
        case 2:
        case 4:
            break;
        default:
            AsmError( OPERANDS_MUST_BE_THE_SAME_SIZE );
            state = ERROR;
        }
        break;
    case T_RCL:
    case T_RCR:
    case T_ROL:
    case T_ROR:
    case T_SAL:
    case T_SAR:
    case T_SHL:
    case T_SHR:
    case T_LDS:
    case T_LES:
    case T_LFS:
    case T_LGS:
    case T_LSS:
        // checking will be done later
        break;
    case T_ENTER:
        // ENTER has to be OP_I16, OP_I8
        if( op1 == OP_I32 ) {
            //parse_phase_1 will treat 16-bit data as OP_I32 if CPU is 386
            if( CodeInfo->data[OPND1] > (signed long)USHRT_MAX ) {
                // if op1 is really 32-bit data, then error
                AsmError( IMMEDIATE_DATA_TOO_BIG );
                state = ERROR;
            }
        }
        // type cast op1 to OP_I16
        CodeInfo->info.opnd_type[OPND1] = OP_I16;
        // op2 have to be 8-bit data
        if( op2 >= OP_I16 ) {
            if( CodeInfo->data[OPND2] > UCHAR_MAX ) {
                AsmError( IMMEDIATE_DATA_TOO_BIG );
                state = ERROR;
            }
            CodeInfo->info.opnd_type[OPND2] = OP_I8;
        }
        break;
#if 0
    case T_CALLF:
    case T_JMPF:
        // segment can only be 16-bit
        if( op1 > OP_I16 ) {
            AsmError( SEGMENT_TOO_BIG );
            state = ERROR;
        }
        if( ( CodeInfo->info.cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
            // offset can only be 16-bit if CPU is 286 and down
            if( op2 > OP_I16 ) {
                AsmError( OFFSET_TOO_BIG );
                state = ERROR;
            }
        }
        // swap the 2 opnds to make output easier
        if( InsFixups[OPND2] != NULL ) {
            // absolute segment + offset nnnn,offset
            CodeInfo->info.opnd_type[OPND1] = CodeInfo->info.opnd_type[OPND2];
            InsFixups[OPND1] = InsFixups[OPND2];
            InsFixups[OPND2] = NULL;
        } else {
            // absolute ptr nnnn,nnnn
            CodeInfo->info.opnd_type[OPND1] = CodeInfo->use32 ? OP_I32 : OP_I16;
        }
        temp = CodeInfo->data[OPND1];
        CodeInfo->data[OPND1] = CodeInfo->data[OPND2];
        CodeInfo->data[OPND2] = temp;
        CodeInfo->info.opnd_type[OPND2] = OP_I16;
        CodeInfo->info.opcode = 0;
        break;
#endif
    case T_MOVSX:
    case T_MOVZX:
        CodeInfo->info.opcode &= NOT_W_BIT;
        op2_size = OperandSize( op2 );
        switch( OperandSize( op1 ) ) {
        case 4:
            switch( op2_size ) {
            case 2:
                CodeInfo->info.opcode |= W_BIT;
                break;
            case 4:
                AsmError( OP_2_TOO_BIG );
                state = ERROR;
            }
            if( CodeInfo->use32 ) {
                CodeInfo->prefix.opsiz = FALSE;     // - don't need opnd size prefix
            }
            break;
        case 2:
            if( op2_size >= 2 ) {
                AsmError( OP_2_TOO_BIG );
                state = ERROR;
            }
            break;
        default:
            // op1 have to be r16/r32
            AsmError( OP_1_TOO_SMALL );
            state = ERROR;
        }
        break;
    case T_LSL:                                 /* 19-sep-93 */
        op1_size = OperandSize( op1 );
        switch( op1_size ) {
        case 2:
            if( CodeInfo->use32 )
                CodeInfo->prefix.opsiz = TRUE;
            break;
        case 4:
            if( CodeInfo->use32 )
                CodeInfo->prefix.opsiz = FALSE;
            break;
        default:
            AsmError( INVALID_SIZE );
            return( ERROR );
        }
        op2_size = OperandSize( op2 );
        switch( op2_size ) {
        case 2:
        case 4:
            break;
        default:
            AsmError( INVALID_SIZE );
            state = ERROR;
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
        op1_size = OperandSize( op1 );
        op2_size = OperandSize( op2 );
        if( ( op1_size != 0 ) && ( op1_size != 4 )
            || ( op2_size != 0 ) && ( op2_size != 4 ) ) {
            AsmError( OPERANDS_MUST_BE_THE_SAME_SIZE );
            state = ERROR;
        }
#endif
        break;
    case T_MOV:
        if( op1 & OP_SR ) {
            op2_size = OperandSize( op2 );
            if( ( op2_size == 2 ) || ( op2_size == 4 ) ) {
//                CodeInfo->prefix.opsiz = FALSE;
                return( state );
            }
        } else if( op2 & OP_SR ) {
            op1_size = OperandSize( op1 );
            if( ( op1_size == 2 ) || ( op1_size == 4 ) ) {
//                if( op1 == OP_M )
//                    CodeInfo->prefix.opsiz = FALSE;
                return( state );
            }
        } else if( ( op1 == OP_M ) || ( op2 == OP_M ) ) {
            // one operand is memory address: to optimize MOV
            DebugMsg(("check_size: MOV to/from memory, mem_type=%u, op1=%X, op2=%X\n", CodeInfo->mem_type, op1, op2));
            temp = CodeInfo->info.rm_byte;
            if( CodeInfo->info.opnd_type[OPND1] & OP_A ) {
                temp = ( temp & BIT_67 ) | ( ( temp & BIT_012 ) << 3 ) | ( ( temp & BIT_345 ) >> 3 );
                if( addr_32( CodeInfo ) && ( temp == D32 )
                    || !addr_32( CodeInfo ) && ( temp == D16 ) ) {
                    // DS:[d32] or DS:[d16] can use MOV Mem with Acc (short form)
                } else {
                    // we have to change OP_A to OP_R
                    CodeInfo->info.opnd_type[OPND1] &= ~OP_A;
                }
            } else if( CodeInfo->info.opnd_type[OPND2] & OP_A ) {
                if( addr_32( CodeInfo ) && ( temp == D32 )
                    || !addr_32( CodeInfo ) && ( temp == D16 ) ) {
                    // DS:[d32] or DS:[d16] can use MOV Mem with Acc (short form)
                    temp = EMPTY;
                } else {
                    // we have to change OP_A to OP_R
                    CodeInfo->info.opnd_type[OPND2] &= ~OP_A;
                }
            }
        } else if( ( op1 & OP_SPEC_REG ) || ( op2 & OP_SPEC_REG ) ) {
            CodeInfo->prefix.opsiz = FALSE;
            return( state );
        }
        // no break;
    default:
        // make sure the 2 opnds are of the same type
        op1_size = OperandSize( op1 );
        op2_size = OperandSize( op2 );
        if( op1_size > op2_size ) {
            if( ( op2 >= OP_I8 ) && ( op2 <= OP_I32 ) ) {     /* immediate */
                op2_size = op1_size;    /* promote to larger size */
            }
        }
        if( ( op1_size == 1 ) && ( op2 == OP_I16 )
            && ( CodeInfo->data[OPND2] <= UCHAR_MAX ) ) {
            return( state ); // OK cause no sign extension
        }
        if( ( op1_size == 2 ) && ( op2 == OP_I32 )
            && ( CodeInfo->data[OPND2] <= USHRT_MAX ) ) {
            return( state ); // OK cause no sign extension
        }
        if( op1_size != op2_size ) {
            /* if one or more are !defined, set them appropriately */
            if( ( op1 | op2 ) & ( OP_MMX | OP_XMM ) ) {
            } else if( ( op1_size != 0 ) && ( op2_size != 0 ) ) {
                AsmError( OPERANDS_MUST_BE_THE_SAME_SIZE );
//                state = ERROR;
            }
            if( op1_size == 0 ) {
                if( ( op1 & OP_M_ANY ) && ( op2 & OP_I ) ) {
                    if( (unsigned long)CodeInfo->data[OPND2] > USHRT_MAX || op2_size == 4) {
                         CodeInfo->mem_type = MT_DWORD;
                         CodeInfo->info.opcode |= W_BIT;
                         CodeInfo->info.opnd_type[OPND2] = OP_I32;
                         temp = ASSUMING_DWORD;
                    } else if( (unsigned long)CodeInfo->data[OPND2] > UCHAR_MAX || op2_size == 2) {
                         CodeInfo->mem_type = MT_WORD;
                         CodeInfo->info.opcode |= W_BIT;
                         CodeInfo->info.opnd_type[OPND2] = OP_I16;
                         temp = ASSUMING_WORD;
                    } else {
                         CodeInfo->mem_type = MT_BYTE;
                         CodeInfo->info.opnd_type[OPND2] = OP_I8;
                         temp = ASSUMING_BYTE;
                    }
#if defined( _STANDALONE_ )
                    if( Parse_Pass > PASS_1 && opndx->explicit == FALSE) {
                        AsmWarn( 1, temp );
                    }
#endif
                } else if( ( op1 & OP_M_ANY ) && ( op2 & ( OP_R | OP_SR ) ) ) {
                } else if( ( op1 & ( OP_MMX | OP_XMM ) ) && ( op2 & OP_I ) ) {
                    if( (unsigned long)CodeInfo->data[OPND2] > USHRT_MAX ) {
                         CodeInfo->info.opnd_type[OPND2] = OP_I32;
                    } else if( (unsigned long)CodeInfo->data[OPND2] > UCHAR_MAX ) {
                         CodeInfo->info.opnd_type[OPND2] = OP_I16;
                    } else {
                         CodeInfo->info.opnd_type[OPND2] = OP_I8;
                    }
                } else if( ( op1 | op2 ) & ( OP_MMX | OP_XMM ) ) {
                } else {
#if defined( _STANDALONE_ )
                    AsmIntErr( 1 );
#endif
                    switch( op2_size ) {
                    case 1:
                        CodeInfo->mem_type = MT_BYTE;
#if defined( _STANDALONE_ )
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, ASSUMING_BYTE );
                        }
#endif
                        break;
                    case 2:
                        CodeInfo->mem_type = MT_WORD;
                        CodeInfo->info.opcode |= W_BIT;
#if defined( _STANDALONE_ )
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, ASSUMING_WORD );
                        }
#endif
                        if( CodeInfo->use32 )
                            CodeInfo->prefix.opsiz = TRUE;
                        break;
                    case 4:
                        CodeInfo->mem_type = MT_DWORD;
                        CodeInfo->info.opcode |= W_BIT;
#if defined( _STANDALONE_ )
                        if( ( Parse_Pass == PASS_1 ) && ( op2 & OP_I ) ) {
                            AsmWarn( 1, ASSUMING_DWORD );
                        }
#endif
                        break;
                    }
                }
            }
        }
    }
    return( state );
}

void ParseInit( int cpu, int fpu, int use32, int extn )
/***************************************************/
{
    int         pos = 0;
    enum asm_token  token_value = 1;
    int         size = sizeof( AsmOpTable ) / sizeof( AsmOpTable[0] );
    int         count;

    for( count = 0; count < MAX_TOKEN; count ++ ) {
        AsmBuffer[count] = &tokens[count];
    }

    if( use32 < 0 )
        use32 = 0;   // default is 16-bit segment
    if( cpu < 0 )
        cpu = 0;     // default is 8086 CPU
    if( fpu < 0 )
        fpu = 1;     // default is FPU use
    if( extn < 0 )
        extn = 0;    // default is no CPU extension instructions
    switch( use32 ) {
    case 0:
        CodeInfo->use32 = 0;
        break;
    case 1:
        CodeInfo->use32 = 1;
        break;
    }
    switch( cpu ) {
    case 0:
        CodeInfo->info.cpu = P_86;
        if( fpu )
            CodeInfo->info.cpu |= P_87;
        break;
    case 1:
        CodeInfo->info.cpu = P_186;
        if( fpu )
            CodeInfo->info.cpu |= P_87;
        break;
    case 2:
        CodeInfo->info.cpu = P_286p;
        if( fpu )
            CodeInfo->info.cpu |= P_287;
        break;
    case 3:
        CodeInfo->info.cpu = P_386p;
        if( fpu )
            CodeInfo->info.cpu |= P_387;
        break;
    case 4:
        CodeInfo->info.cpu = P_486p;
        if( fpu )
            CodeInfo->info.cpu |= P_387;
        break;
    case 5:
        CodeInfo->info.cpu = P_586p;
        if( fpu )
            CodeInfo->info.cpu |= P_387;
        if( extn )
            CodeInfo->info.cpu |= P_K3D | P_MMX;
        break;
    case 6:
        CodeInfo->info.cpu = P_686p;
        if( fpu )
            CodeInfo->info.cpu |= P_387;
        if( extn )
            CodeInfo->info.cpu |= P_K3D | P_MMX | P_SSE | P_SSE2 | P_SSE3;
        break;
    }

    // initialize AsmOpcode table to point to entry in AsmOpTable
    // token has its own value, e.g. T_AAA is 0, T_ADD is 1, etc.

    if( AsmOpcode[1].position == 0 ) {  // if not initialized
        while( AsmOpcode[token_value].len != 0 ) {
            do {
                pos++;
            } while ( AsmOpTable[pos].token != token_value && pos <= size );
            if( pos > size ) {
#if defined( _STANDALONE_ )
#ifdef DEBUG_OUT
                printf("internal error: pos=%u, size=%u, AsmOpTable[%u]=%u\n", pos, size, pos, AsmOpTable[pos].token);
#endif
#endif
                AsmError( INTERNAL_ERROR_1 );
                exit( -1 );
            }
            AsmOpcode[token_value].position = pos;
            token_value++;
        }
        make_inst_hash_table();
    }
}

#if !defined( _STANDALONE_ )

static enum asm_cpu CPUinfo;

void AsmSaveCPUInfo( void )
{
    CPUinfo = CodeInfo->info.cpu;
}

void AsmRestoreCPUInfo( void )
{
    CodeInfo->info.cpu = CPUinfo;
}

#endif
