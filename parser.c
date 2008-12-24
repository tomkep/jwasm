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

#define ONEXMM 1 /* 1=use ONE .xmm directive (Masm compatible) */

// create AsmOpTable table.

#define ins(tok,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    {tok,prefix,0,byte1_info,rm_info,op3,op_dir,{op1,op2},cpu,opcode,rm_byte},

#define insa(tok,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix) \
    ins(tok,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)

struct asm_ins AsmOpTable[] = {
#include "instruct.h"
};

// create the strings for all reserved words

#define resword( token, string, len ) static const char token ## str[len] = { string };
#include "reswords.h"
#undef resword

// create the 'reserved words' table (AsmResWord).
// this table's entries will populate the instruction hash table.

#define resword( token, string, len ) {NULL, token ## str, len, 0 },
struct ReservedWord AsmResWord[] = {
#include "reswords.h"
};
#undef resword

/* parsing of jump and call instructions is in a separate module: jumps.c */
extern ret_code         jmp( expr_list * );

extern bool             DefineProc;
extern struct asm_sym   symPC; /* '$' symbol */
extern bool             in_epilogue;
extern bool             CheckSeg;

static struct asm_code  Code_Info;
struct asm_code         *CodeInfo = &Code_Info;
unsigned                Opnd_Count;
bool                    line_listed;
int_8                   Frame;          // Frame of current fixup
uint_16                 Frame_Datum;    // Frame datum of current fixup

//static void             check_assume( struct asm_sym *, enum prefix_reg );
//static ret_code         check_size( expr_list * );
//static ret_code         mem2code( char, int, int, asm_sym * );

static int comp_mem16( int reg1, int reg2 )
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

static void SetFixupFrame( asm_sym *sym)
{
    if( !sym ) {
    } else if( sym->state == SYM_SEG ) {
        Frame = FRAME_SEG;
        Frame_Datum = GetSegIdx( sym->segment );
    } else if ( sym->state == SYM_GRP ) {
        Frame = FRAME_GRP;
        Frame_Datum = GetGrpIdx( sym );
    }
}

/* set global vars Frame and Frame_Datum */

void find_frame( struct asm_sym *sym )
/*******************************************/
{
    if( CodeInfo->prefix.SegOverride != NULL ) {
        SetFixupFrame( CodeInfo->prefix.SegOverride );
    } else {
        asm_sym *grp;
        switch( sym->state ) {
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

static enum assume_segreg Prefix2Assume( enum prefix_reg prefix )
{
    switch( prefix ) {
    case PREFIX_CS:  return( ASSUME_CS );
    case PREFIX_SS:  return( ASSUME_SS );
    case PREFIX_DS:  return( ASSUME_DS );
    case PREFIX_ES:  return( ASSUME_ES );
    case PREFIX_FS:  return( ASSUME_FS );
    case PREFIX_GS:  return( ASSUME_GS );
    }
    return( ASSUME_NOTHING );
}

static enum prefix_reg Assume2Prefix( enum assume_segreg reg )
{
    switch( reg ) {
    case ASSUME_CS: return( PREFIX_CS );
    case ASSUME_SS: return( PREFIX_SS );
    case ASSUME_DS: return( PREFIX_DS );
    case ASSUME_ES: return( PREFIX_ES );
    case ASSUME_FS: return( PREFIX_FS );
    case ASSUME_GS: return( PREFIX_GS );
    }
    return( PREFIX_EMPTY );
}

static void check_assume( struct asm_sym *sym, enum prefix_reg default_reg )
/**************************************************************************/
/* Check if an assumed segment register is found, and
 set CodeInfo->RegOverride if necessary */
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

    reg = GetAssume( sym, def_reg, &assume );
    /* set global vars Frame and Frame_Datum */
    SetFixupFrame( assume );

    if( reg == ASSUME_NOTHING ) {
        if ( sym ) {
            if( sym->state != SYM_EXTERNAL && sym->state != SYM_PROC && sym->state != SYM_STACK ) {
                DebugMsg(("no segment register available to access label %s\n", sym->name ));
                AsmErr( CANNOT_ACCESS_LABEL_WITH_SEGMENT_REGISTERS, sym->name );
            } else
                CodeInfo->prefix.RegOverride = default_reg;
        } else {
            DebugMsg(("no segment register available to access seg-label %s\n", CodeInfo->prefix.SegOverride->name ));
            AsmErr( CANNOT_ACCESS_LABEL_WITH_SEGMENT_REGISTERS, CodeInfo->prefix.SegOverride->name );
        }
    } else if( default_reg != EMPTY ) {
        CodeInfo->prefix.RegOverride = Assume2Prefix( reg );
    }
}

static void seg_override( int seg_reg, asm_sym *sym, bool direct )
/***************************************************/
/*
 called by mem2code().
- determine if segment override is necessary with the current address mode;
*/
{
    enum prefix_reg     default_seg;
    enum assume_segreg  assume_seg;
    asm_sym             *assume;

    /* don't touch segment overrides for string instructions */
    if ( AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REP ||
         AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REPxx )
        return;

    if( CodeInfo->token == T_LEA ) {
        CodeInfo->prefix.RegOverride = EMPTY; /* skip segment override */
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
        if ( sym ) {
            SetFixupFrame( assume );
        } else if ( direct ) {
            /* no label attached (DS:[0]). No fixup is to be created! */
            if ( assume )
                SET_ADRSIZ( CodeInfo, SymIs32( assume ) );
            else
                SET_ADRSIZ( CodeInfo, ModuleInfo.defUse32 );
        }
    } else {
        if ( sym || CodeInfo->prefix.SegOverride )
            check_assume( sym, default_seg );
        if ( sym == NULL && CodeInfo->prefix.SegOverride ) {
            SET_ADRSIZ( CodeInfo, SymIs32( CodeInfo->prefix.SegOverride ) );
        }
    }

    if( CodeInfo->prefix.RegOverride == default_seg ) {
        CodeInfo->prefix.RegOverride = EMPTY;
    }
}

int OperandSize( OPNDTYPE opnd )
/******************************/
{
    if( ( opnd == OP_NONE ) || ( opnd & OP_SPECIAL ) ) {
        return( 0 );
    } else if( opnd == OP_M ) {
        return( SizeFromMemtype(CodeInfo->mem_type, Use32) );
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

#define Reg386( x ) AsmOpTable[AsmResWord[x].position].opcode

static ret_code mem2code( char ss, int index, int base, asm_sym *sym )
/***************************************************************/
/*
 encode the memory operand to machine code
 in ss = scale factor???
 in index = index register (T_DI, T_ESI, ...)
 in base = base register (T_EBP, ... )
 in sym = ???
 out: CodeInfo->sib, CodeInfo->rm_byte
*/
{
    int                 temp;
    unsigned char       mod_field;
    unsigned char       rm_field;

    DebugMsg(("mem2code(ss=%u, index=%d, base=%d, sym=%X) enter\n", ss, index, base, sym));

    // clear mod
    rm_field = 0;
    if( CodeInfo->InsFixup[Opnd_Count] != NULL ) {
        mod_field = MOD_10;
    } else if( CodeInfo->data[Opnd_Count] == 0 ) {
        mod_field = MOD_00;
    } else if( ( CodeInfo->data[Opnd_Count] > SCHAR_MAX )
       || ( CodeInfo->data[Opnd_Count] < SCHAR_MIN ) ) {
        mod_field = MOD_10;
    } else {
        mod_field = MOD_01;
    }
    if( ( index == EMPTY ) && ( base == EMPTY ) ) {
        // direct memory
        // clear the rightmost 3 bits
        mod_field = MOD_00;

        // default is DS:[], DS: segment override is not needed
        seg_override( T_DS, sym, TRUE );

        if( !addr_32( CodeInfo ) ) {
            if( !InRange( CodeInfo->data[Opnd_Count], 2 ) ) {
                // expect 16-bit but got 32-bit address
                AsmError( MAGNITUDE_OF_OFFSET_EXCEEDS_16BIT );
                return( ERROR );
            }
            rm_field = D16;
        } else {
            rm_field = D32;
        }
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
            CodeInfo->sib = 0x24;
            // default is SS:[], SS: segment override is not needed
            break;
        default: // for 386 and up
            rm_field = Reg386( base );
            // default is DS:[], DS: segment override is not needed
        }
        seg_override( base, sym, FALSE );
    } else if( ( index != EMPTY ) && ( base == EMPTY ) ) {
        // mod field is 00
        mod_field = MOD_00;
        // s-i-b is present ( r/m = 100 )
        rm_field = S_I_B;
        // scale factor, index, base ( 0x05 => no base reg )
        CodeInfo->sib = ( ss | ( Reg386(index) << 3 ) | 0x05 );
        // default is DS:[], DS: segment override is not needed
        seg_override( T_DS, sym, FALSE );
    } else {
        // base != EMPTY && index != EMPTY
        switch( index ) {
        case T_BX:
        case T_BP:
            if( ( temp = comp_mem16( index, base ) ) == ERROR )
                return( ERROR );
            rm_field = temp;
            seg_override( index, sym, FALSE );
            break;
        case T_SI:
        case T_DI:
            if( ( temp = comp_mem16( base, index ) ) == ERROR )
                return( ERROR );
            rm_field = temp;
            seg_override( base, sym, FALSE );
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
            CodeInfo->sib = ( ss | (Reg386( index ) << 3) | Reg386( base ) );
            seg_override( base, sym, FALSE );
        }
    }
    if( Opnd_Count == OPND2 ) {
        // shift the register field to left by 3 bit
        CodeInfo->rm_byte = mod_field | ( rm_field << 3 ) | ( CodeInfo->rm_byte & BIT_012 );
    } else if( Opnd_Count == OPND1 ) {
        CodeInfo->rm_byte = mod_field | rm_field;
    }
    return( NOT_ERROR );
}

/* get segment register rm_byte */

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
    /**/myassert( 0 );
    return( 0 );
}

/*
 set fields in CodeInfo:
 - CodeInfo->mem_type
 - CodeInfo->mem_type_fixed
 - CodeInfo->isfar
 - CodeInfo->prefix.opsiz
*/
void Set_Memtype( memtype mem_type, bool fix_mem_type )
/***********************************************/
{
    if( CodeInfo->token == T_LEA )
        return;

    if( mem_type != MT_EMPTY && mem_type != MT_TYPE &&
        CodeInfo->mem_type_fixed == FALSE ) {
        CodeInfo->mem_type = mem_type;
        if( fix_mem_type ) {
            CodeInfo->mem_type_fixed = TRUE;
            if( ( mem_type == MT_FAR ) && IS_JMPCALL( CodeInfo->token ) ) {
                CodeInfo->isfar = TRUE;
            }
        }
    }
    if( CodeInfo->use32 && MEM_TYPE( CodeInfo->mem_type, WORD ) ) {
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

    } else if( !CodeInfo->use32 && MEM_TYPE( CodeInfo->mem_type, DWORD ) ) {

        /* if we are not in use32 mode, we have to add OPSIZ
         * when we find DWORD PTR
         * unless we have a LxS ins.
         * which moves a DWORD ptr into SR:word reg
         * fixme  - can this be done by breaking up the LXS instructions in
         *          asmins.h, and then putting F_32 or F_16 to append
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
 */

ret_code segm_override( expr_list *opndx )
/******************************************/
{
    struct asm_sym      *sym;

    if( opndx->override != EMPTY ) {
        if( AsmBuffer[opndx->override]->token == T_REG ) {
            CodeInfo->prefix.RegOverride = AsmOpTable[AsmResWord[AsmBuffer[opndx->override]->value].position].opcode;
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

static ret_code process_jumps( expr_list *opndx )
/********************************************/
/*
 parse the jumps instructions operands.
 called by idata_nofixup() and idata_fixup().
 called for CALL, JMP, Jxx, JCXZ, JECXZ, LOOPx.
*/
{
    /*
     Masm checks overrides for branch instructions with immediate operand.
     However, no segment prefix byte is emitted.
     */
    if( opndx->override != EMPTY ) {
        if ( segm_override( opndx ) == NOT_ERROR ) {
            if ( CodeInfo->prefix.RegOverride != EMPTY ) {
                enum assume_segreg assume_seg = Prefix2Assume( CodeInfo->prefix.RegOverride );
                asm_sym *assume = GetOverrideAssume( assume_seg );
                /* clear the segment prefix */
                CodeInfo->prefix.RegOverride = EMPTY;
                if ( opndx->sym && opndx->sym->segment && assume != opndx->sym->segment ) {
                    if ( opndx->sym )
                        AsmErr( CANNOT_ACCESS_LABEL_WITH_SEGMENT_REGISTERS, opndx->sym->name );
                    else
                        AsmErr( CANNOT_ACCESS_LABEL_WITH_SEGMENT_REGISTERS, "" );
                }
            }
        }
    }

    Set_Memtype( opndx->mem_type, opndx->explicit );
    if( opndx->mbr != NULL ) {
        Set_Memtype( opndx->mbr->mem_type, FALSE );
    }

    if ( jmp( opndx ) == NOT_ERROR)
        return( NOT_ERROR );
    /* jmp() might have returned ERROR or INDIRECT_JMP */
    return( ERROR );
}

static ret_code idata_nofixup( expr_list *opndx )
/******************************************/
{
    OPNDTYPE    op_type;
    long        value;
    int         size;

    DebugMsg(("idata_nofixup(type=%u mem_type=%u value=%X) enter [CodeInfo->mem_type=%u]\n", opndx->type, opndx->mem_type, opndx->value, CodeInfo->mem_type));

     // jmp/call/jxx/loop/jcxz/jecxz?
    if( IS_ANY_BRANCH( CodeInfo->token ) ) { 
        return( process_jumps( opndx ) );
    }
    value = opndx->value;
    CodeInfo->data[Opnd_Count] = value;

    switch( CodeInfo->mem_type ) {
    case MT_EMPTY:
        /* don't set the CodeInfo->mem_type!
         */
        if( CodeInfo->token == T_PUSH ) {
            /* PUSH <type> PTR <value>? */
            if( opndx->explicit ) {
                int size = SizeFromMemtype( opndx->mem_type, Use32 );
                if ( size == 1) {
                    op_type = OP_I8;
                } else if( size == 2 ) {
                    op_type = OP_I16;
                    SET_OPSIZ_16( CodeInfo );
                } else if( size == 4 ) {
                    op_type = OP_I32;
                    SET_OPSIZ_32( CodeInfo );
                } else {
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                    return( ERROR );
                }
                break;
            }
            if( CodeInfo->use32 ) {
                if( (int_8)value == (int_32)value ) {
                    op_type = OP_I8;
                } else {
                    op_type = OP_I32;
                }
            } else {
                /* 16bit mode without typecast. extend to DWORD only if
                 the value cannot be expressed as signed 8bit */
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
                size = SizeFromMemtype(opndx->mem_type, Use32);
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
        CodeInfo->opcode |= W_BIT;
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
            // set w-bit
            CodeInfo->opcode |= W_BIT;
        } else {
            if( !InRange( value, 2 ) ) {
                AsmError( IMMEDIATE_DATA_OUT_OF_RANGE );
                return( ERROR );
            } else if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I16;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->opcode |= W_BIT;
        }
        break;
   case MT_SDWORD:
        if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
            op_type = OP_I32;
        } else {
            op_type = OP_I8;
        }
        // set w-bit
        CodeInfo->opcode |= W_BIT;
        break;
    case MT_DWORD:
        if( Options.sign_value ) {
            if( value > UCHAR_MAX ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->opcode |= W_BIT;
        } else {
            if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->opcode |= W_BIT;
        }
        break;
    case MT_QWORD:
        if( Options.sign_value ) {
            if( value > UCHAR_MAX ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->opcode |= W_BIT;
        } else {
            if( ( value > SCHAR_MAX ) || ( value < SCHAR_MIN ) ) {
                op_type = OP_I32;
            } else {
                op_type = OP_I8;
            }
            // set w-bit
            CodeInfo->opcode |= W_BIT;
        }
        break;
    default:
        /* shouldn't happen? */
        DebugMsg(("idata_nofixup, default branch\n" ));
        break;
    }
    CodeInfo->opnd_type[Opnd_Count] = op_type;
    DebugMsg(("idata_nofixup exit, op_type=%X [CodeInfo->mem_type=%u]\n", op_type, CodeInfo->mem_type));
    return( NOT_ERROR );
}

// create immediate data with fixup (offsets)

static ret_code idata_fixup( expr_list *opndx )
/****************************************/
{
    struct asmfixup     *fixup;
    enum fixup_types    fixup_type;
    int                 type;
    int                 sym32; // 1=32bit, 0=16bit offset for fixup

    DebugMsg(("idata_fixup(type=%u, mem_type=%u) enter [CodeInfo.mem_type=%u]\n", opndx->type, opndx->mem_type, CodeInfo->mem_type));

     // jmp/call/jxx/loop/jcxz/jecxz?
    if( IS_ANY_BRANCH( CodeInfo->token ) ) {
        return( process_jumps( opndx ) );
    }
    CodeInfo->data[Opnd_Count] = opndx->value;
    segm_override( opndx );

    if( ( opndx->sym->state == SYM_SEG )
        || ( opndx->sym->state == SYM_GRP )
        || ( opndx->instr == T_SEG ) ) {
        sym32 = FALSE;
    } else if( opndx->abs ) {  /* an (external) absolute symbol? */
        sym32 = FALSE;
    } else {
        sym32 = SymIs32( opndx->sym );
    }
    if( opndx->instr != EMPTY ) {
        if( ( opndx->base_reg != EMPTY )
            || ( opndx->idx_reg != EMPTY ) ) {
            AsmError( INVALID_MEMORY_POINTER );
            return( ERROR );
        }
    }
    if ( opndx->instr == T_OFFSET ) {
        if( opndx->sym->state == SYM_GRP ) {
            AsmError( CANNOT_OFFSET_GRP );
            return( ERROR );
        }
    }

    switch( CodeInfo->mem_type ) {
    case MT_EMPTY:
        if( Opnd_Count > OPND1 ) {
            type = OperandSize( CodeInfo->opnd_type[OPND1] );
            if( opndx->instr != EMPTY && type && (type < 2 || ( sym32 && type < 4 ))) {
                switch ( opndx->instr ) {
                case T_OFFSET:
                case T_LROFFSET:
#if IMAGERELSUPP
                case T_IMAGEREL:
#endif
#if SECRELSUPP
                case T_SECTIONREL:
#endif
                    AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, type, sym32 ? 4 : 2 );
                    return( ERROR );
                }
            }
            if( type == 4 ) {
                CodeInfo->mem_type = MT_DWORD;
                CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                SET_OPSIZ_32( CodeInfo );
                break;
            } else if( type == 2 ) {
                CodeInfo->mem_type = MT_WORD;
                CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                SET_OPSIZ_16( CodeInfo );
                break;
#if 1  /* mov al, E1 (E1=absolute external) */
            } else if( type == 1 ) {
                CodeInfo->mem_type = MT_BYTE;
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                SET_OPSIZ_NO( CodeInfo );
                break;
#endif
            }
        }
        if( opndx->abs ) {
            if( opndx->mem_type == MT_BYTE ) {
                CodeInfo->mem_type = MT_BYTE;
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                break;
            //} else if( opndx->mem_type == MT_EMPTY ) {
            } else if( opndx->mem_type == MT_EMPTY || opndx->mem_type == MT_ABS ) {
                SET_OPSIZ_NO( CodeInfo );
                if( oper_32( CodeInfo ) ) {
                    CodeInfo->mem_type = MT_DWORD;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                    sym32 = TRUE;
                    break;
                }
            } else if( opndx->mem_type == MT_DWORD ) {
                sym32 = TRUE;
            }
        }
        /* if a WORD size is given, don't override it with */
        /* anything what might look better at first glance */
        if( opndx->mem_type != MT_WORD )
            if( ( CodeInfo->token == T_PUSHD ) || sym32 ) {
                CodeInfo->mem_type = MT_DWORD;
                CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                SET_OPSIZ_32( CodeInfo );
                break;
            }
        CodeInfo->mem_type = MT_WORD;
        // no break
    case MT_SWORD:
    case MT_WORD:
        CodeInfo->opnd_type[Opnd_Count] = OP_I16;
        SET_OPSIZ_16( CodeInfo );
        break;
    case MT_SDWORD:
    case MT_DWORD:
        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
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
//    ConstantOnly = TRUE;
    CodeInfo->opcode |= W_BIT;

    /* set global vars Frame and Frame_Datum */
    find_frame( opndx->sym );

    DebugMsg(("idata_fixup: calling AddFixup(%s, %u)\n", opndx->sym->name, fixup_type));
    fixup = AddFixup( opndx->sym, fixup_type, OPTJ_NONE );
    if( fixup == NULL )
        return( ERROR );

    if (opndx->instr == T_LROFFSET)
        fixup->loader_resolved = TRUE;

#if IMAGERELSUPP
    if (opndx->instr == T_IMAGEREL && fixup_type == FIX_OFF32)
        fixup->type = FIX_OFF32_IMGREL;
#endif
#if SECRELSUPP
    if (opndx->instr == T_SECTIONREL && fixup_type == FIX_OFF32)
        fixup->type = FIX_OFF32_SECREL;
#endif
    DebugMsg(("idata_fixup exit [CodeInfo.mem_type=%u]\n", CodeInfo->mem_type ));

    return( NOT_ERROR );
}

static void add_frame( void )
/********************/
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

static ret_code memory_operand( expr_list *opndx, bool with_fixup )
/************************************************************/
{
    char                ss = SCALE_FACTOR_1;
    int                 index = EMPTY;
    int                 base = EMPTY;
    int                 j;
    int                 size;
    struct asm_sym      *sym;
    char                base_lock = FALSE;
    enum fixup_types    fixup_type;
    int                 sym32;

    DebugMsg(("memory_operand(opndx->value=%X, fixup=%u) enter, [CodeInfo->memtype=%u]\n", opndx->value, with_fixup, CodeInfo->mem_type));
    CodeInfo->data[Opnd_Count] = opndx->value;
    CodeInfo->opnd_type[Opnd_Count] = OP_M; //OP_M == 0x870000
    sym = opndx->sym;

    segm_override( opndx );

#if 1
    /* convert MT_PTR to MT_WORD, MT_DWORD or MT_FWORD. */
    /* MT_PTR cannot be set explicitely by the PTR operator,
     so this value must come from a label or a structure field.
     */
    if (opndx->mem_type == MT_PTR) {
        asm_sym *sym2 = sym;
        size = 0;
        if ( opndx->mbr )  /* the mbr field has higher priority */
            sym2 = opndx->mbr;
        if (sym2 && sym2->type) {
            size = sym2->type->total_size;
            CodeInfo->isfar = sym2->type->isfar;

            /* there's an ambiguity with pointers of size DWORD,
             since they can be either NEAR32 or FAR16 */
            if ( size == 4 && sym2->type->use32 != CodeInfo->use32 )
                opndx->ofs_size = sym2->type->use32 ? OFSSIZE_32 : OFSSIZE_16;

        } else if (sym2)  {
            size = sym2->total_size;
        }
        if ( size )
            MemtypeFromSize( size, &opndx->mem_type );
    }
#endif
    Set_Memtype( opndx->mem_type, opndx->explicit );
    if( opndx->mbr != NULL ) {
        /* if the struct field is just another struct, use it's total size
         to set the operand size.
         */
        if ( opndx->mbr->mem_type == MT_TYPE ) {
            memtype mem_type;
            if ( MemtypeFromSize( opndx->mbr->total_size, &mem_type ) == NOT_ERROR )
                Set_Memtype( mem_type, FALSE );
        } else
            Set_Memtype( opndx->mbr->mem_type, FALSE );
    }

    if (opndx->base_reg != EMPTY )
        base = AsmBuffer[opndx->base_reg]->value;

    // check for base registers

    if( base != EMPTY ) {
        j = AsmResWord[base].position;
        if ( AsmOpTable[j].allowed_prefix == AP_IREG ) {
            if ( AsmOpTable[j].cpu == P_386 ) {
#if 0 /* is checked in expression evaluator now */
                if( ( ModuleInfo.curr_cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
                    // 286 and down cannot use 386 registers
                    AsmError( CANNOT_USE_386_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING );
                    return( ERROR );
                }
#endif
                SET_ADRSIZ_32( CodeInfo );
            } else {
                SET_ADRSIZ_16( CodeInfo );
            }
        } else {
            AsmError( INVALID_MEMORY_POINTER );
            return( ERROR );
        }
    }

    // check for index registers

    if( opndx->idx_reg != EMPTY ) {
        index = AsmBuffer[opndx->idx_reg]->value;
        j = AsmResWord[index].position;
        if ( AsmOpTable[j].allowed_prefix == AP_IREG ) {
            if ( AsmOpTable[j].cpu == P_386 ) {
#if 0 /* is checked in expression evaluator now */
                if( ( ModuleInfo.curr_cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
                    // 286 and down cannot use 386 registers
                    AsmError( CANNOT_USE_386_ADDRESSING_MODE_WITH_CURRENT_CPU_SETTING );
                    return( ERROR );
                }
#endif
                SET_ADRSIZ_32( CodeInfo );
            } else {
                SET_ADRSIZ_16( CodeInfo );
            }
        } else {
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
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
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
                    Set_Memtype( sym->mem_type, FALSE );
                }
                break;
            }
        }

        if( opndx->abs ) {
            sym32 = addr_32( CodeInfo );
        } else if ( sym ) {
            sym32 = SymIs32( sym );
        } else if ( CodeInfo->prefix.SegOverride ) {
            sym32 = SymIs32( CodeInfo->prefix.SegOverride );
        }

        if( ( opndx->base_reg == EMPTY ) && ( opndx->idx_reg == EMPTY ) ) {
            SET_ADRSIZ( CodeInfo, sym32 );
            fixup_type = ( sym32 ) ? FIX_OFF32 : FIX_OFF16;
        } else {
            if( addr_32( CodeInfo ) ) {
                fixup_type = ( sym32 ) ? FIX_OFF32 : FIX_OFF16;
            } else {
                fixup_type = FIX_OFF16;
                if( sym32 && Parse_Pass == PASS_2 ) {
                    // address size is 16bit but label is 32-bit.
                    // example: use a 16bit register as base in FLAT model:
                    //   test buff[di],cl
                    AsmWarn( 2, WORD_FIXUP_FOR_32BIT_LABEL, sym->name );
                }
            }
        }

#ifdef DEBUG_OUT
        if ( sym )
            DebugMsg(("memory_operand: calling AddFixup(%s, fixup=%u) [CodeInfo->memtype=%u]\n", sym->name, fixup_type, CodeInfo->mem_type));
        else
            DebugMsg(("memory_operand: calling AddFixup(NULL, fixup=%u) [CodeInfo->memtype=%u]\n", fixup_type, CodeInfo->mem_type));
#endif
        AddFixup( sym, fixup_type, OPTJ_NONE );

        if( Modend ) {
            asm_sym *assume;
            /* set global vars Frame and Frame_Datum */
            GetAssume( sym, ASSUME_NOTHING, &assume);
            SetFixupFrame( assume );
        } else {
            if( mem2code( ss, index, base, sym ) == ERROR ) {
                return( ERROR );
            }
        }
        /* set again current fixup in CodeInfo */
        add_frame();

    } else { /* branch without fixup */
#if 1
        /* if an undefined label is moved (mov [xxx],offset yyy)
         change to immediate data. (this might be placed better in
         check_size()! )
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
            if( CodeInfo->use32 ) {
                base = T_EBP;
            } else {
                base = T_BP;
            }
            base_lock = TRUE;   // add lock
            if( CodeInfo->mem_type == MT_EMPTY ) {
                Set_Memtype( sym->mem_type, FALSE );
            }
        }
        if( mem2code( ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
    }
    switch ( CodeInfo->token ) {
    case T_JMP:
    case T_CALL:
        j = SizeFromMemtype(CodeInfo->mem_type, Use32);
        if ( j == 1 || j > 6 ) {
            /* CALL/JMP possible for WORD/DWORD/FWORD memory operands only */
            AsmError(INVALID_INSTRUCTION_OPERANDS);
            return(ERROR);
        }
        if( opndx->mem_type == MT_FAR || CodeInfo->mem_type == MT_FWORD ||
            ( CodeInfo->mem_type == MT_DWORD &&
              ((CodeInfo->use32 == FALSE && opndx->ofs_size != OFSSIZE_32) ||
               (CodeInfo->use32 == TRUE && opndx->ofs_size == OFSSIZE_16 )))) {
            CodeInfo->isfar = TRUE;
        }
        break;
    case T_PUSH:
    case T_POP:
        j = SizeFromMemtype(CodeInfo->mem_type, Use32);
        if ( j == 1 || j > 4 ) {
            /* PUSH/POP possible for WORD/DWORD memory operands only */
            AsmError(INVALID_INSTRUCTION_OPERANDS);
            return(ERROR);
        }
    }
    DebugMsg(("memory_operand exit, no error, type/value=%X/%X, CodeInfo->memtype=%u\n", opndx->type, opndx->value, CodeInfo->mem_type));
    return( NOT_ERROR );
}

ret_code process_address( expr_list *opndx )
/********************************************/
/*
 parse the memory reference operand
 Opnd_Count is 0 for first operand, 1 for second, ...
 valid return values: NOT_ERROR, ERROR
  */
{
    memtype     mem_type;

    if( opndx->indirect ) {  /* indirect register operand or stack var */

        DebugMsg(("process_address: INDIRECT, sym=%X\n", opndx->sym));
        if( opndx->sym == NULL || opndx->sym->state == SYM_STACK ) {
            return( memory_operand( opndx, FALSE ) );
        } else {
            return( memory_operand( opndx, TRUE ) );
        }
    } else if( opndx->instr != EMPTY ) {
        /* instr is OFFSET | LROFFSET | SEG | LOW | LOWWORD, ... */
        DebugMsg(("process_address: instr != EMPTY\n"));
        if( opndx->sym == NULL ) { /* better to check opndx->type? */
            return( idata_nofixup( opndx ) );
        } else {
            return( idata_fixup( opndx ) );
        }
    } else if( opndx->sym == NULL ) { // direct operand without symbol
        DebugMsg(("process_address: symbol=NULL\n" ));
        if( opndx->override != EMPTY ) {
            /* direct absolute memory without symbol.
             DS:[0] won't create a fixup, but
             DGROUP:[0] will create one! */
            if ( AsmBuffer[opndx->override]->token == T_REG )
                return( memory_operand( opndx, FALSE ) );
            else
                return( memory_operand( opndx, TRUE ) );
        } else {
            return( idata_nofixup( opndx ) );
        }
    } else if( ( opndx->sym->state == SYM_UNDEFINED ) && !opndx->explicit ) {
        DebugMsg(("process_address: sym=SYM_UNDEFINED, name=%s, state=%X\n", opndx->sym->name, opndx->sym->state ));
        if( Parse_Pass != PASS_1 ) {
            AsmErr( SYMBOL_NOT_DEFINED, opndx->sym->name );
            return( ERROR );
        }
        // undefined symbol, it's not possible to determine
        // operand type and size
        switch( CodeInfo->token ) {
        case T_PUSH:
        case T_PUSHW:
        case T_PUSHD:
            return( idata_nofixup( opndx ) );
        default:
            // jmp/call/jxx/loop/jcxz/jecxz?
            if( IS_ANY_BRANCH( CodeInfo->token ) ) {
                return( idata_nofixup( opndx ) );
            } else {
                return( memory_operand( opndx, FALSE ) );
            }
        }
    } else if( ( opndx->sym->state == SYM_SEG ) ||
               ( opndx->sym->state == SYM_GRP ) ) {
        DebugMsg(("process_address: sym->state=SEG/GROUP\n"));
        // SEGMENT and GROUP symbol is converted to SEG symbol
        // for next processing
        opndx->instr = T_SEG;
        return( idata_fixup( opndx ) );
    } else {
        DebugMsg(("process_address direct, sym=%s\n", opndx->sym->name ));
        mem_type = ( opndx->explicit ) ? opndx->mem_type : opndx->sym->mem_type;
        if( opndx->abs ) {
            return( idata_fixup( opndx ) );
        }
        // CODE location is converted to OFFSET symbol
        switch( mem_type ) {
        case MT_FAR:
        case MT_NEAR:
        case MT_SHORT:
        case MT_PROC:
            if( CodeInfo->token == T_LEA ) {
                return( memory_operand( opndx, TRUE ) );
            } else if( opndx->sym == &symPC ) {
                return( idata_fixup( opndx ) );
            } else if( opndx->mbr != NULL ) { // structure field?
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
    return( NOT_ERROR );
}

// handle constant operands
// these might also need a fixup if they are externals (EXTERN:ABS!)

static ret_code process_const( expr_list *opndx )
/******************************************/
{
    if( ( CodeInfo->token == T_IMUL )
        && ( CodeInfo->opnd_type[OPND1] & OP_R ) ) {
        if( Opnd_Count == OPND2 ) {
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
    return( idata_nofixup( opndx ) );
}

static ret_code process_reg( expr_list *opndx )
/****************************************/
/*
- parse and encode direct register operands;
*/
{
    int                 temp;
    int                 reg;

    DebugMsg(( "process_reg enter\n"));
    temp = AsmResWord[AsmBuffer[opndx->base_reg]->value].position;
    /* the register number is stored in opcode field */
    reg = AsmOpTable[temp].opcode;
    /* the "OP-name" of the register is stored in opnd_type[OPND2] */
    CodeInfo->opnd_type[Opnd_Count] = AsmOpTable[temp].opnd_type[OPND2];
    switch( AsmOpTable[temp].opnd_type[OPND2] ) {
    case OP_AL:
    case OP_R8:
        CodeInfo->opcode &= NOT_W_BIT;         // clear w-bit
        break;
    case OP_CL: /* only appears in "shift opnd,CL" instructions */
        break;
    case OP_AX:
    case OP_DX: /* only appears in "in" and "out" instructions  */
    case OP_R16:
        CodeInfo->opcode |= W_BIT;             // set w-bit
        if( CodeInfo->use32 )
            CodeInfo->prefix.opsiz = TRUE;
        break;
    case OP_MMX:
        break;
    case OP_XMM:
        break;
    case OP_ST:
        temp = opndx->idx_reg & BIT_012;
        CodeInfo->rm_byte |= temp;
        if( temp != 0 )
            CodeInfo->opnd_type[Opnd_Count] = OP_ST_REG;
        break;
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
        if( AsmBuffer[opndx->base_reg]->value == T_CS ) {
            // POP CS is not allowed
            if( CodeInfo->token == T_POP ) {
                AsmError( POP_CS_IS_NOT_ALLOWED );
                return( ERROR );
            }
        }
        /* translate PREFIX to segment register no (ES=0 CS=1 SS=2 DS=3 ...*/
        reg = get_sr_rm_byte( AsmOpTable[temp].opcode );
        break;
    case OP_EAX:
    case OP_R32:
#if 0 /* is checked in expression evaluator now */
        if( ( ModuleInfo.curr_cpu & ( P_CPU_MASK | P_PM ) ) <= P_286p ) {
            // 8086 ins cannot use 386 register
            AsmError( CANNOT_USE_386_REGISTER_WITH_CURRENT_CPU_SETTING );
            return( ERROR );
        }
#endif
        CodeInfo->opcode |= W_BIT;             // set w-bit
        if( !CodeInfo->use32 )
            CodeInfo->prefix.opsiz = TRUE;
        break;
    case OP_TR:                 // Test registers
        switch( AsmBuffer[opndx->base_reg]->value ) {
        case T_TR3:
        case T_TR4:
        case T_TR5:
            if( ( ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_486 )
               || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_686 ) )
                && ( ( AsmOpTable[temp].cpu & P_CPU_MASK ) >= P_486 ) ) {
                // TR3-TR5 are available on 486 only
                AsmErr( CANNOT_USE_TRN_TO_TRM_WITH_CURRENT_CPU_SETTING, 3, 5 );
                return( ERROR );
            }
            break;
        case T_TR6:
        case T_TR7:
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

    if( Opnd_Count == OPND1 ) {
        // the first operand
        // r/m is treated as a 'reg' field
        CodeInfo->rm_byte |= MOD_11;
        // fill the r/m field
        CodeInfo->rm_byte |= reg;
    } else {
        // the second operand
        if( ( CodeInfo->token == T_XCHG )
            && ( ( CodeInfo->opnd_type[OPND1] == OP_AX )
            || ( CodeInfo->opnd_type[OPND1] == OP_EAX ) ) ) {
            // XCHG can use short form if op1 is AX or EAX
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & BIT_67 ) | reg;
        } else {
            // fill reg field with reg
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & ~BIT_345 ) | ( reg << 3 );
        }
    }
    return( NOT_ERROR );
}


// special handling for string instructions
// the peculiarity is that these instructions ( optionally )
// have memory operands, which aren't used for code generation

static void HandleStringInstructions( struct asm_code *CodeInfo, expr_list *opndx )
{
    int opndidx = OPND1;

    switch( CodeInfo->token ) {
    case T_CMPS:
    case T_CMPSB:
    case T_CMPSW:
    case T_CMPSD:
         /* cmps allows prefix for the first operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY )
            if ( opndx->override != EMPTY )
                AsmError( INVALID_INSTRUCTION_OPERANDS );
        break;
    case T_MOVS:
    case T_MOVSB:
    case T_MOVSW:
    case T_MOVSD:
        /* movs allows prefix for the second operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY )
            if ( opndx->override == EMPTY )
                AsmError( INVALID_INSTRUCTION_OPERANDS );
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
        int op_size = OperandSize( CodeInfo->opnd_type[opndidx] );
        switch( op_size ) {
        case 1:
            CodeInfo->mem_type = MT_BYTE;
            CodeInfo->opcode &= NOT_W_BIT;
            if( CodeInfo->use32 )
                CodeInfo->prefix.opsiz = FALSE;
            break;
        case 2:
            CodeInfo->mem_type = MT_WORD;
            CodeInfo->opcode |= W_BIT;
            CodeInfo->prefix.opsiz = CodeInfo->use32 ? TRUE : FALSE;
            break;
        case 4:
            CodeInfo->mem_type = MT_DWORD;
            CodeInfo->opcode |= W_BIT;
            CodeInfo->prefix.opsiz = CodeInfo->use32 ? FALSE : TRUE;
            break;
        }
    }
    return;
}

static ret_code check_size( expr_list * opndx )
/***************************/
/*
- use to make sure the size of first operand match the size of second operand;
- optimize MOV instruction;
- opndx contains last operand
*/
{
    OPNDTYPE    op1 = CodeInfo->opnd_type[OPND1];
    OPNDTYPE    op2 = CodeInfo->opnd_type[OPND2];
    ret_code    rc = NOT_ERROR;
    int         temp;
    int         op1_size;
    int         op2_size;
    int         op_size = 0;

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
            op_size = OperandSize( op2 );
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
                CodeInfo->opcode &= NOT_W_BIT;         // clear w-bit
            case OP_EAX:
                if( CodeInfo->use32 ) {
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
        switch( OperandSize( op1 ) ) {
        case 2:
        case 4:
            break;
        default:
            AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, OperandSize(op1), Use32 ? 4 : 2);
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
        op1_size = OperandSize( op1 ) + 2; /* add 2 for the impl. segment register */
        op2_size = OperandSize( op2 );
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
        op1_size = OperandSize( op1 );
        op2_size = OperandSize( op2 );
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
            if( CodeInfo->use32 ) {
                CodeInfo->prefix.opsiz = FALSE;     // - don't need opnd size prefix
            }
            break;
        case 2:
            if( op2_size >= 2 ) {
                AsmError( OP2_TOO_BIG );
                rc = ERROR;
            }
            break;
        default:
            // op1 have to be r16/r32
            AsmError( OP1_TOO_SMALL );
            rc = ERROR;
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
        op1_size = OperandSize( op1 );
        op2_size = OperandSize( op2 );
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
            op2_size = OperandSize( op2 );
            if( ( op2_size == 2 ) || ( op2_size == 4 ) ) {
                return( NOT_ERROR );
            }
        } else if( op2 & OP_SR ) {
            op1_size = OperandSize( op1 );
            if( ( op1_size == 2 ) || ( op1_size == 4 ) ) {
                return( NOT_ERROR );
            }
        } else if( ( op1 == OP_M ) || ( op2 == OP_M ) ) {
            // one operand is memory address: to optimize MOV
            DebugMsg(("check_size: MOV to/from memory, mem_type=%u, op1=%X, op2=%X\n", CodeInfo->mem_type, op1, op2));
            temp = CodeInfo->rm_byte;
            if( CodeInfo->opnd_type[OPND1] & OP_A ) {
                temp = ( temp & BIT_67 ) | ( ( temp & BIT_012 ) << 3 ) | ( ( temp & BIT_345 ) >> 3 );
                if( addr_32( CodeInfo ) && ( temp == D32 )
                    || !addr_32( CodeInfo ) && ( temp == D16 ) ) {
                    // DS:[d32] or DS:[d16] can use MOV Mem with Acc (short form)
                } else {
                    // we have to change OP_A to OP_R
                    CodeInfo->opnd_type[OPND1] &= ~OP_A;
                }
            } else if( CodeInfo->opnd_type[OPND2] & OP_A ) {
                if( addr_32( CodeInfo ) && ( temp == D32 )
                    || !addr_32( CodeInfo ) && ( temp == D16 ) ) {
                    // DS:[d32] or DS:[d16] can use MOV Mem with Acc (short form)
                    temp = EMPTY;
                } else {
                    // we have to change OP_A to OP_R
                    CodeInfo->opnd_type[OPND2] &= ~OP_A;
                }
            }
        } else if( ( op1 & OP_SPEC_REG ) || ( op2 & OP_SPEC_REG ) ) {
            CodeInfo->prefix.opsiz = FALSE;
            return( rc );
        }
        /* fall through */
    default:
        // make sure the 2 opnds are of the same type
        op1_size = OperandSize( op1 );
        op2_size = OperandSize( op2 );
        DebugMsg(("check_size default: size1=%u, size2=%u\n", op1_size, op2_size));
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
//                rc = ERROR;
            }
            if( op1_size == 0 ) {
                if( ( op1 & OP_M_ANY ) && ( op2 & OP_I ) ) {
                    char *p = "WORD";
                    if( (unsigned long)CodeInfo->data[OPND2] > USHRT_MAX || op2_size == 4) {
                        CodeInfo->opcode |= W_BIT;
                        if (op2_size <= 2 && CodeInfo->data[OPND2] > SHRT_MIN && Use32 == 0) {
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
                        if( CodeInfo->use32 )
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
{
    int                 i;
    struct asm_code     *CodeInfo = &Code_Info; /* to reduce size */
    ret_code            temp;
    asm_sym             *sym;
    uint_32             oldofs;
    expr_list           opndx;

    DebugMsg(("ParseItems enter, Token_Count=%u, queue_level=%u\n", Token_Count, queue_level ));

#if 1
    /* avoid to rescan structures if pass is > 1 */
    if (StructDef.struct_depth > 0 && Parse_Pass > PASS_1) {
        if (Token_Count > 1 && AsmBuffer[1]->token == T_DIRECTIVE && AsmBuffer[1]->value == T_ENDS)
            ;
        else
            return( NOT_ERROR );
    }
#endif

    if( DefineProc == TRUE )
        if ( ModuleInfo.proc_prologue == NULL || *ModuleInfo.proc_prologue == NULLC )
            proc_check();

    // init CodeInfo
    CodeInfo->prefix.ins     = EMPTY;
    CodeInfo->prefix.SegOverride = NULL;
    CodeInfo->prefix.RegOverride = EMPTY;
    CodeInfo->prefix.adrsiz  = FALSE;
    CodeInfo->prefix.opsiz   = FALSE;
    CodeInfo->token          = T_NULL;
    CodeInfo->mem_type       = MT_EMPTY;
    CodeInfo->opcode         = 0;
    CodeInfo->rm_byte        = 0;
    //CodeInfo->extended_ins   = EMPTY;
    CodeInfo->sib            = 0;            // assume ss is *1
    CodeInfo->use32          = Use32;
    //CodeInfo->indirect       = FALSE;
    CodeInfo->mem_type_fixed = FALSE;
    CodeInfo->isfar          = FALSE;

    for( i = 0; i < 3; i++ ) {
        CodeInfo->opnd_type[i] = OP_NONE;
        CodeInfo->data[i] = 0;
        CodeInfo->InsFixup[i] = NULL;
    }
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
        if ( SegAssumeTable[ASSUME_CS].error == TRUE ) { /* CS assumed to ERROR? */
            AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
            return( ERROR );
        }
        if ((AsmBuffer[2]->token == T_COLON) || (CurrProc == NULL)) {
            if( LabelCreate( AsmBuffer[0]->string_ptr, MT_NEAR, NULL, FALSE ) == ERROR ) {
                DebugMsg(("ParseItems, error creating global label, exit\n"));
                return( ERROR );
            }
            if (AsmBuffer[2]->token == T_COLON)
                i++; /* skip the second colon */
        } else {
            if( LabelCreate( AsmBuffer[0]->string_ptr, MT_NEAR, NULL, TRUE )==ERROR ) {
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
            /* if next token is a directive, it can't be a anonymous data item */
            /* this also filters the "xxx STRUCT" and "xxx ENDS" cases */
            if( ( AsmBuffer[i+1]->token != T_DIRECTIVE ) &&
                ( sym = IsLabelType( AsmBuffer[i]->string_ptr ))) {
                DebugMsg(("ParseItems anonymous data item >%s<\n", AsmBuffer[i]->string_ptr ));
                return( data_init( label, i, (dir_node *)sym ) );
            }
            label = i;
            i++;
        }
        switch ( AsmBuffer[i]->token ) {
        case T_DIRECTIVE:
            DebugMsg(("ParseItems T_DIRECTIVE >%s<\n", AsmBuffer[i]->string_ptr ));
            if ( AsmBuffer[i]->opcode & DF_DATADIR ) {
                return( data_init( label, i, NULL ) );
            }
            if ( Parse_Pass == PASS_1 && FileInfo.file[LST] ) {
                temp = directive( i, AsmBuffer[i]->value );
                if ( line_listed == FALSE )
                    LstWriteSrcLine();
                return( temp );
            }
            return( directive( i, AsmBuffer[i]->value ) );
        case T_RES_ID:
            DebugMsg(("ParseItems T_RES_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if ( AsmBuffer[i]->rm_byte == OP_TYPE )
                return( data_init( label, i, NULL ) );
            break;
        case T_ID:
            DebugMsg(("ParseItems T_ID >%s<\n", AsmBuffer[i]->string_ptr ));
            if( sym = IsLabelType( AsmBuffer[i]->string_ptr ) ) {
                return( data_init( label, i, (dir_node *)sym ) );
            }
            break;
        default:
            if ( AsmBuffer[i]->token == T_COLON ) {
                DebugMsg(("ParseItems unexpected colon\n" ));
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
                return( ERROR );
            } else if (i > 0 )
                i--;
            break;
        } /* end switch (AsmBuffer[i]->token) */
        DebugMsg(("ParseItems unexpected token=%u, i=%u, string=%s\n", AsmBuffer[i]->token, i, AsmBuffer[i]->string_ptr));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    DebugMsg(("ParseItems T_INSTRUCTION %s\n", AsmBuffer[i]->string_ptr));

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
            DebugMsg(("ParseItems, unexpected token after prefix, exit, error\n"));
            AsmError( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION );
            return( ERROR );
        }
        i++;
    };

    if( CurrProc ) {
        switch( AsmBuffer[i]->value ) {
        case T_RET:
        case T_IRET:
        case T_IRETD: /* IRETW doesn't exist. Masm bug? */
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

    CodeInfo->token = AsmBuffer[i]->value;
    i++;

    /* get the instruction's arguments */

    while ( AsmBuffer[i]->token != T_FINAL ) {

        DebugMsg(("ParseItems, calling EvalOperand for operand %u, i=%u\n", Opnd_Count, i));
        if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
            DebugMsg(("ParseItems: EvalOperand() for operand %u failed\n", Opnd_Count ));
            return( ERROR );
        }

        /* SHLD and SHRD currently need some special handling
         if 3. operand is CL, it's treated as if 2 operands exist only.
         */
        if( Opnd_Count == OPND3 ) {
            if ( CodeInfo->token == T_SHLD ||
                 CodeInfo->token == T_SHRD ) {
                switch (opndx.type) {
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

        DebugMsg(("ParseItems T_INSTRUCTION: operand 1 type/value/mem_type=%X/%X/%u\n", opndx.type, opndx.value, opndx.mem_type));
        switch( opndx.type ) {
        case EXPR_ADDR:
            DebugMsg(("ParseItems operand=%u, ADDR\n", Opnd_Count ));
            if ( process_address( &opndx ) == ERROR )
                return( ERROR );
#if 1
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
                opndx.type = EXPR_CONST;
                goto process_const;
            }
#endif
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
            DebugMsg(("ParseItems operand=%u, CONST, opndx.memtype=%u\n", Opnd_Count, opndx.mem_type));
            if (process_const( &opndx ) == ERROR )
                return( ERROR );
            break;
        case EXPR_REG:
            DebugMsg(("ParseItems operand=%u, REG\n", Opnd_Count ));
            if( opndx.indirect ) { /* indirect operand ( "[EBX+...]" )? */
                temp  = process_address( &opndx );
                if ( temp != NOT_ERROR )
                    return( temp );
            } else {
                if ( process_reg( &opndx ) == ERROR )
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
                    opndx.type = EXPR_CONST;
                    opndx.fvalue = float_value;
                    process_const( &opndx );
                    break;
                }
#endif
                /* Masm message is: real or BCD number not allowed */
                AsmError( FLOAT_OPERAND );
                return( ERROR );
            }
            /* fall through */
        default:
            DebugMsg(("ParseItems operand=%u, unexpected operand type=%d, error, exit\n", Opnd_Count, opndx.type ));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA && AsmBuffer[i+1]->token != T_FINAL ) {
            if( Opnd_Count > OPND2 ) {
                AsmError( TOO_MANY_COMMAS );
                return( ERROR );
            }
            Opnd_Count++;
            i++;
            Frame = EMPTY;
            CodeInfo->prefix.SegOverride = NULL;
        }
    } /* end while */


    /* special handling for string instructions */

    if ( AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REP ||
         AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REPxx ) {
        HandleStringInstructions( CodeInfo, &opndx );
    } else {
        if( Opnd_Count > OPND1 ) {
            if( check_size( &opndx ) == ERROR ) {
                DebugMsg(("ParseItems, check_size() failed, exit\n"));
                return( ERROR );
            }
        }
    }

    /* now call the code generator */

    if ( FileInfo.file[LST] ) {
        temp = match_phase_1( CodeInfo );
        LstWrite( LSTTYPE_LIDATA, oldofs, NULL );
        return( temp );
    }
    return( match_phase_1( CodeInfo ) );
}

/* ParseInit() is called once per module */

ret_code ParseInit( void )
/***************************************************/
{
    int  token_value;

    DebugMsg(("ParseInit()\n"));

    /* initialize the AsmBuffer table */
    InitTokenBuffer();

    ModuleInfo.curr_cpu = P_86 | P_87;

    /*
     initialize field <position> in the 'reserved words' table.
     This field is an index into the opcode table (AsmOpTable).
     Both tables must be sorted.
     After the field is initialized, create a hash table.
    */

    if( AsmResWord[1].position == 0 ) {  // if not initialized
        int         pos = 0;
        int         size = sizeof( AsmOpTable ) / sizeof( AsmOpTable[0] );
        for( token_value = 1; token_value < T_NULL; token_value++ ) {
            for( ; pos < size && AsmOpTable[pos].token != token_value; pos++)
                ;
            /*
             if an error occurs, the two tables are inconsistent.
             Either a reserved word is missing in AsmOpTable or at least
             one table isn't sorted.
             */
            if( pos >= size ) {
                AsmErr( INCONSISTENT_INTERNAL_TABLES, AsmResWord[token_value].len, AsmResWord[token_value].name );
                return( ERROR );
            }
            AsmResWord[token_value].position = pos;
        }
        make_inst_hash_table();
    } else {
        /* scan the reserved word table, reset the "disabled" flags */
        for ( token_value = 0; token_value < T_NULL; token_value++)
            AsmOpTable[AsmResWord[token_value].position].disabled = FALSE;
    }
    return( NOT_ERROR );
}

