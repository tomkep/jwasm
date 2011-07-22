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
#include "reswords.h"
#include "codegen.h"
#include "fixup.h"
#include "expreval.h"
#include "types.h"
#include "labels.h"
#include "segment.h"
#include "assume.h"
#include "proc.h"
#include "myassert.h"
#include "input.h"
#include "tokenize.h"
#include "listing.h"
#include "data.h"
#include "fastpass.h"
#include "omf.h"
#include "omfspec.h"
#include "condasm.h"

#define SET_ADRSIZ( s, x ) ( s->prefix.adrsiz = (( x ) ^ ( s->Ofssize )) ? TRUE : FALSE )
#define IS_ADDR32( s )  ( s->Ofssize ? ( s->prefix.adrsiz == FALSE ) : ( s->prefix.adrsiz == TRUE ))

#define SET_OPSIZ_32( s ) ( s->prefix.opsiz = ( s->Ofssize ) ? FALSE : TRUE )
#define SET_OPSIZ_16( s ) ( s->prefix.opsiz = ( s->Ofssize ) ? TRUE : FALSE )
#define SET_OPSIZ_NO( s ) ( s->prefix.opsiz = FALSE )

#define InWordRange( val ) ( (val > 65535 || val < -65535) ? FALSE : TRUE )

extern ret_code (* const directive[])( int, struct asm_tok[] );

/* parsing of branch instructions with imm operand is found in branch.c */
extern ret_code         process_branch( struct code_info *, unsigned, const struct expr * );

extern bool             DefineProc;
//extern struct asym   symPC; /* '$' symbol */
extern bool             in_epilogue;
extern int_64           maxintvalues[];
extern int_64           minintvalues[];
extern const struct opnd_class opnd_clstab[];
#if AVXSUPP
extern uint_8           vex_flags[];
#endif

int_8                   Frame_Type;     /* curr fixup frame type: SEG|GRP|EXT|ABS */
uint_16                 Frame_Datum;    /* curr fixup frame value */
struct asym             *SegOverride;
static enum assume_segreg  LastRegOverride;/* needed for CMPS */

static uint_16 tbaseptr[] = { T_BP, T_EBP
#if AMD64_SUPPORT
, T_RBP
#endif
};

struct symbol_queue SymTables[TAB_LAST];/* tables of definitions */

void sym_add_table( struct symbol_queue *queue, struct dsym *item )
/*****************************************************************/
{
#ifdef DEBUG_OUT
    if ( queue == &SymTables[TAB_UNDEF] )
        item->sym.forward = TRUE;
#endif
    /* put the new entry into the queue for its type of symbol */
    if( queue->head == NULL ) {
        queue->head = queue->tail = item;
        item->next = item->prev = NULL;
    } else {
        item->prev = queue->tail;
        queue->tail->next = item;
        queue->tail = item;
        item->next = NULL;
    }
}

/* remove an item from a symbol queue.
 * this is called only for TAB_UNDEF and TAB_EXT,
 * segments, groups, procs or aliases never change their state.
 */
void sym_remove_table( struct symbol_queue *queue, struct dsym *item )
/********************************************************************/
{
    /* unlink the node */
    if( item->prev )
        item->prev->next = item->next;
    if( item->next )
        item->next->prev = item->prev;

    //if ( dir->next == NULL )
    //    dir->next = dir->prev;

    if ( queue->head == item )
        queue->head = item->next;
    if ( queue->tail == item )
        queue->tail = item->prev;

    item->prev = NULL;
    item->next = NULL;
}

void sym_ext2int( struct asym *sym )
/**********************************/
/* Change symbol state from SYM_EXTERNAL to SYM_INTERNAL
 * this is for PROTOs (isproc=1) and EXTERNDEFs (weak=1)
 */
{
    sym_remove_table( &SymTables[TAB_EXT], (struct dsym *)sym );
    if ( sym->isproc == FALSE ) /* v2.01: don't clear flags for PROTO */
        sym->first_size = 0;
    sym->state = SYM_INTERNAL;
}

ret_code GetLangType( int *i, struct asm_tok tokenarray[], enum lang_type *plang )
/********************************************************************************/
{
    if( tokenarray[*i].token == T_RES_ID ) {
#if 1 /* v2.03: simplified */
        if ( tokenarray[(*i)].tokval >= T_C &&
            tokenarray[(*i)].tokval <= T_FASTCALL ) {
            *plang = tokenarray[(*i)].bytval;
            (*i)++;
            return( NOT_ERROR );
        }
#else
        switch( tokenarray[(*i)].tokval ) {
        case T_C:        *plang = LANG_C;        break;
        case T_SYSCALL:  *plang = LANG_SYSCALL;  break;
        case T_STDCALL:  *plang = LANG_STDCALL;  break;
        case T_PASCAL:   *plang = LANG_PASCAL;   break;
        case T_FORTRAN:  *plang = LANG_FORTRAN;  break;
        case T_BASIC:    *plang = LANG_BASIC;    break;
        case T_FASTCALL: *plang = LANG_FASTCALL; break;
        default:
            return( ERROR );
        }
        (*i)++;
        return( NOT_ERROR );
#endif
    }
    return( ERROR );
}

/* get size of a register
 * v2.06: rewritten, since the sflags field
 * does now contain size for GPR, STx, MMX, XMM regs.
 */

int SizeFromRegister( int registertoken )
/***************************************/
{
    uint flags;
    flags = GetSflagsSp( registertoken ) & SFR_SIZMSK;

    if ( flags )
        return( flags );

    flags = GetValueSp( registertoken );
    if ( flags & OP_SR )
        return( CurrWordSize );

    /* CRx, DRx, TRx remaining */
#if AMD64_SUPPORT
    return( ModuleInfo.Ofssize == USE64 ? 8 : 4 );
#else
    return( 4 );
#endif
}

/* get size from memory type
 * is32 param used only for MT_NEAR/MT_FAR
 */
int SizeFromMemtype( enum memtype mem_type, int Ofssize, struct asym *type )
/**************************************************************************/
{
    if ( ( mem_type & MT_SPECIAL) == 0 )
        return ( (mem_type & MT_SIZE_MASK) + 1 );

    if ( Ofssize == USE_EMPTY )
        Ofssize = ModuleInfo.Ofssize;

    switch ( mem_type ) {
    case MT_NEAR:
        DebugMsg1(("SizeFromMemtype( MT_NEAR, Ofssize=%u )=%u\n", Ofssize, 2 << Ofssize ));
        return ( 2 << Ofssize );
    case MT_FAR:
        DebugMsg1(("SizeFromMemtype( MT_FAR, Ofssize=%u )=%u\n", Ofssize, ( 2 << Ofssize ) + 2 ));
        return ( ( 2 << Ofssize ) + 2 );
    case MT_PROC:
        DebugMsg1(("SizeFromMemtype( MT_PROC, Ofssize=%u )=%u\n", Ofssize, ( 2 << Ofssize ) + ( ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) ) ? 2 : 0 ) ));
        return( ( 2 << Ofssize ) + ( ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) ) ? 2 : 0 ) );
    case MT_PTR:
        DebugMsg1(("SizeFromMemtype( MT_PTR, Ofssize=%u )=%u\n", Ofssize, ( 2 << Ofssize ) + ( ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) ) ? 2 : 0 ) ));
        return( ( 2 << Ofssize ) + ( ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) ) ? 2 : 0 ) );
    case MT_TYPE:
        if ( type )
            return( type->total_size );
    default:
        DebugMsg1(("SizeFromMemtype( memtype=%Xh, Ofssize=%u )=%u\n", mem_type, Ofssize, 0 ));
        return( 0 );
    }
}

/* get memory type from size */

ret_code MemtypeFromSize( int size, enum memtype *ptype )
/*******************************************************/
{
    int i;
    for ( i = T_BYTE; SpecialTable[i].type == RWT_STYPE; i++ ) {
        if( ( SpecialTable[i].bytval & MT_SPECIAL ) == 0 ) {
            /* the size is encoded 0-based in field mem_type */
            if( ( ( SpecialTable[i].bytval & MT_SIZE_MASK) + 1 ) == size ) {
                *ptype = SpecialTable[i].bytval;
                return( NOT_ERROR );
            }
        }
    }
    return( ERROR );
}

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

static void SetFixupFrame( const struct asym *sym )
/*************************************************/
{
    struct dsym *grp;

    if( sym ) {
        switch ( sym->state ) {
        case SYM_INTERNAL:
        case SYM_EXTERNAL:
            if( sym->segment != NULL ) {
                if( grp = (struct dsym *)GetGroup( sym ) ) {
                    Frame_Type = FRAME_GRP;
                    Frame_Datum = grp->e.grpinfo->grp_idx;
                } else {
                    Frame_Type = FRAME_SEG;
                    Frame_Datum = GetSegIdx( sym->segment );
                }
            }
            break;
        case SYM_SEG:
            Frame_Type = FRAME_SEG;
            Frame_Datum = GetSegIdx( sym->segment );
            break;
        case SYM_GRP:
            Frame_Type = FRAME_GRP;
            Frame_Datum = ((struct dsym *)sym)->e.grpinfo->grp_idx;
            break;
        }
    }
}

/* set global vars Frame and Frame_Datum.
 * called by:
 * - idata_fixup()
 * - process_branch() in branch.c
 * - data_item() in data.c
 */

void find_frame( const struct asym *sym )
/***************************************/
{
    SetFixupFrame( SegOverride ? SegOverride : sym );
}

/* set frame if OPTION OFFSET:SEGMENT is set and
 * OFFSET or SEG operator was used.
 * called by:
 * - idata_fixup()
 * - data_item()
 * hint: sym->segment != NULL can only be true for SYM_INTERNAL|SYM_EXTERNAL.
 */
void find_frame2( const struct asym *sym )
/****************************************/
{
    if( SegOverride == NULL &&
       sym->segment != NULL &&
       ( sym->state == SYM_INTERNAL || sym->state == SYM_EXTERNAL ) ) {
        Frame_Type = FRAME_SEG;
        Frame_Datum = GetSegIdx( sym->segment );
    } else {
        find_frame( sym );
    }
}

static void check_assume( struct code_info *CodeInfo, const struct asym *sym, enum assume_segreg default_reg )
/************************************************************************************************************/
/* Check if an assumed segment register is found, and
 * set CodeInfo->RegOverride if necessary.
 * called by seg_override().
 * at least either sym or SegOverride is != NULL.
 */
{
    enum assume_segreg     reg;
    struct asym            *assume;

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

static void seg_override( struct code_info *CodeInfo, int seg_reg, const struct asym *sym, bool direct )
/******************************************************************************************************/
/*
 * called by mem2code(). determine if segment override is necessary
 * with the current address mode;
 * - seg_reg: address register
 */
{
    enum assume_segreg  default_seg;
    struct asym         *assume;

    /* don't touch segment overrides for string instructions */
    //if ( InstrTable[optable_idx[CodeInfo->token]].allowed_prefix == AP_REP ||
    //     InstrTable[optable_idx[CodeInfo->token]].allowed_prefix == AP_REPxx )
    if ( CodeInfo->pinstr->allowed_prefix == AP_REP ||
         CodeInfo->pinstr->allowed_prefix == AP_REPxx )
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

int OperandSize( enum operand_type opnd, const struct code_info *CodeInfo )
/*************************************************************************/
{
    /* v2.0: OP_M8_R8 and OP_M16_R16 have the DFT bit set! */
    if( opnd == OP_NONE ) {
        return( 0 );
    } else if( opnd == OP_M ) {
        return( SizeFromMemtype( CodeInfo->mem_type, CodeInfo->Ofssize, NULL ) );
    } else if( opnd & ( OP_R8 | OP_M08 | OP_I8 ) ) {
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
//    } else if( opnd & ( OP_I | OP_I48 ) ) {
    } else if( opnd & ( OP_I48 | OP_M48 ) ) {
        return( 6 );
    } else if( opnd & ( OP_STI | OP_M80 ) ) {
        return( 10 );
    } else if( opnd & ( OP_XMM | OP_M128 ) ) {
        return( 16 );
#if AVXSUPP
    } else if( opnd & ( OP_YMM | OP_M256 ) ) {
        return( 32 );
#endif
    } else if( opnd & OP_RSPEC ) {
#if AMD64_SUPPORT
        return( ( CodeInfo->Ofssize == USE64 ) ? 8 : 4 );
#else
        return( 4 );
#endif
    }
    DebugMsg1(("OperandSize: unhandled operand type %Xh!!!\n", opnd ));
    return( 0 );
}

static ret_code mem2code( struct code_info *CodeInfo, unsigned CurrOpnd, char ss, int index, int base, const struct asym *sym )
/*****************************************************************************************************************************/
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
    if( CodeInfo->opnd[CurrOpnd].InsFixup != NULL ) { /* symbolic displacement given? */
        mod_field = MOD_10;
    } else if( CodeInfo->opnd[CurrOpnd].data == 0 ) { /* no displacement (or 0) */
        mod_field = MOD_00;
    } else if( ( CodeInfo->opnd[CurrOpnd].data > SCHAR_MAX )
       || ( CodeInfo->opnd[CurrOpnd].data < SCHAR_MIN ) ) {
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
            if( !InWordRange( CodeInfo->opnd[CurrOpnd].data ) ) {
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
    if( CurrOpnd == OPND2 ) {
        /* shift the register field to left by 3 bit */
        CodeInfo->rm_byte = mod_field | ( rm_field << 3 ) | ( CodeInfo->rm_byte & BIT_012 );
#if AMD64_SUPPORT
        /* v2.02: exchange B and R, keep X */
        //CodeInfo->prefix.rex |= (rex >> 2 );
        CodeInfo->prefix.rex |= ( ( rex >> 2 ) | ( rex & REX_X ) | (( rex & 1) << 2 ) );
#endif
    } else if( CurrOpnd == OPND1 ) {
        CodeInfo->rm_byte = mod_field | rm_field;
#if AMD64_SUPPORT
        CodeInfo->prefix.rex |= rex;
#endif
    }
    return( NOT_ERROR );
}

/* override handling
 * It's either
 * - a register:       set CodeInfo->prefix.RegOverride
 * - a SEG/GRP symbol: set SegOverride
 * called by
 * - process_branch()
 * - idata_fixup()
 * - memory_operand() (CodeInfo != NULL)
 * - data_item() 
 */
ret_code segm_override( const struct expr *opndx, struct code_info *CodeInfo )
/****************************************************************************/
{
    struct asym      *sym;

    if( opndx->override != NULL ) {
        if( opndx->override->token == T_REG ) {
            int temp = GetRegNo( opndx->override->tokval );
            if ( SegAssumeTable[temp].error ) {
                DebugMsg(("segm_override: assume error, reg=%u\n", temp ));
                AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
                return( ERROR );
            }
#if AMD64_SUPPORT
            /* ES,CS,SS and DS overrides are invalid in 64-bit */
            if ( CodeInfo && CodeInfo->Ofssize == USE64 && temp < 4 ) {
                AsmError( ILLEGAL_USE_OF_SEGMENT_REGISTER );
                return( ERROR );
            }
#endif
            sym = GetOverrideAssume( temp );
            if ( CodeInfo ) {
                /* hack: save the previous reg override value (needed for CMPS) */
                LastRegOverride = CodeInfo->prefix.RegOverride;
                CodeInfo->prefix.RegOverride = temp;
            }
        } else {
            sym = SymSearch( opndx->override->string_ptr );
        }
        if ( sym && ( sym->state == SYM_GRP || sym->state == SYM_SEG ))
            SegOverride = sym;
    }
    return( NOT_ERROR );
}

/* get an immediate operand without a fixup.
 * output:
 * - ERROR: error
 * - NOT_ERROR: ok,
 *   CodeInfo->opnd_type[CurrOpnd] = OP_Ix
 *   CodeInfo->data[CurrOpnd]      = value
 *   CodeInfo->prefix.opsiz
 *   CodeInfo->iswide
 */
static ret_code idata_nofixup( struct code_info *CodeInfo, unsigned CurrOpnd, const struct expr *opndx )
/******************************************************************************************************/
{
    enum operand_type op_type;
    int_32      value;
    int         size;

    DebugMsg1(("idata_nofixup(kind=%u mem_type=%Xh value=%I64X) enter\n", opndx->kind, opndx->mem_type, opndx->value64));

    /* jmp/call/jxx/loop/jcxz/jecxz? */
    if( IS_ANY_BRANCH( CodeInfo->token ) ) { 
        return( process_branch( CodeInfo, CurrOpnd, opndx ) );
    }
    value = opndx->value;
    CodeInfo->opnd[CurrOpnd].data = value;

#if AMD64_SUPPORT
    /* 64bit immediates are restricted to MOV <reg>,<imm64>
     */
    if ( opndx->hlvalue != 0 ) { /* magnitude > 64 bits? */
        DebugMsg1(("idata_nofixup: error, hlvalue=%I64X\n", opndx->hlvalue ));
        EmitConstError( opndx );
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
        CurrOpnd == OPND2 &&
        ( CodeInfo->opnd[OPND1].type & OP_R64 ) &&
        ( opndx->value64 > LONG_MAX || opndx->value64 < LONG_MIN ||
         (opndx->explicit && ( opndx->mem_type == MT_QWORD || opndx->mem_type == MT_SQWORD) ) ) ) {
        CodeInfo->iswide = 1;
        CodeInfo->opnd[CurrOpnd].type = OP_I64;
        CodeInfo->opnd[CurrOpnd].datahigh = opndx->hvalue;
        return( NOT_ERROR );
    }
    if ( opndx->value64 <= minintvalues[0] || opndx->value64 > maxintvalues[0] ) {
        DebugMsg1(("idata_nofixup: error, hvalue=%Xh\n", opndx->hvalue ));
        EmitConstError( opndx );
        return( ERROR );
    }
#endif

    /* v2.06: code simplified.
     * to be fixed: the "wide" bit should not be set here!
     * Problem: the "wide" bit isn't set in memory_operand(),
     * probably because of the instructions which accept both
     * signed and unsigned arguments (ADD, CMP, ... ).
     */

    if ( opndx->explicit ) {
        /* size coercion for immediate value */
        CodeInfo->const_size_fixed = TRUE;
        size = SizeFromMemtype( opndx->mem_type,
                               opndx->Ofssize,
                               opndx->type );
        /* don't check if size and value are compatible. */
        switch ( size ) {
        case 1: op_type = OP_I8;  break;
        case 2: op_type = OP_I16; break;
        case 4: op_type = OP_I32; break;
        default:
            DebugMsg1(("idata_nofixup: invalid size %d for immediate operand\n", size ));
            AsmError( INVALID_INSTRUCTION_OPERANDS );
            return( ERROR );
        }
    } else {
        /* use true signed values for BYTE only! */
        if ( (int_8)value == value )
            op_type = OP_I8;
        //else if ( value <= SHRT_MAX && value >= SHRT_MIN )
        /* v2.04: range FFFF0000-FFFF7FFF is also acceptable for 16-bit */
        //else if ( value <= USHRT_MAX && value >= SHRT_MIN )
        /* v2.04b: JWASMR needs a 1L */
        //else if( value <= USHRT_MAX && value >= - (USHRT_MAX+1) )
        else if( value <= USHRT_MAX && value >= - USHRT_MAX )
            op_type = OP_I16;
        else {
            op_type = OP_I32;
        }
    }

    switch ( CodeInfo->token ) {
    case T_PUSH:
        if ( opndx->explicit == FALSE ) {
            /* PUSH <type> PTR <value>? */
            if ( CodeInfo->Ofssize > USE16 && op_type == OP_I16 )
                op_type = OP_I32;
        }
        if ( op_type == OP_I16 )
            SET_OPSIZ_16( CodeInfo );
        else if ( op_type == OP_I32 )
            SET_OPSIZ_32( CodeInfo );
        break;
    case T_PUSHW:
        if ( op_type != OP_I32 ) {
            op_type = OP_I16;
            if( (int_8)value == (int_16)value ) {
                op_type = OP_I8;
            }
        }
        break;
    case T_PUSHD:
        if ( op_type == OP_I16 )
            op_type = OP_I32;
        break;
    }

    if ( CodeInfo->mem_type & MT_SIZE_MASK )
        CodeInfo->iswide = 1;

    CodeInfo->opnd[CurrOpnd].type = op_type;
    DebugMsg1(("idata_nofixup exit, op_type=%" FX32 "\n", op_type ));
    return( NOT_ERROR );
}

/* get an immediate operand with a fixup.
 * output:
 * - ERROR: error
 * - NOT_ERROR: ok,
 *   CodeInfo->opnd_type[CurrOpnd] = OP_Ix
 *   CodeInfo->data[CurrOpnd]      = value
 *   CodeInfo->InsFixup[CurrOpnd]  = fixup
 *   CodeInfo->mem_type
 *   CodeInfo->prefix.opsiz
 * to be fixed: don't modify CodeInfo->mem_type here!
 */
static ret_code idata_fixup( struct code_info *CodeInfo, unsigned CurrOpnd, struct expr *opndx )
/**********************************************************************************************/
{
    //struct fixup      *fixup;
    enum fixup_types    fixup_type;
    enum fixup_options  fixup_option = OPTJ_NONE;
    int                 size;
    uint_8              Ofssize; /* 1=32bit, 0=16bit offset for fixup */

    DebugMsg1(("idata_fixup(opndx.type/mem_type=%u/%Xh) enter [CodeInfo.mem_type=%Xh]\n", opndx->type, opndx->mem_type, CodeInfo->mem_type));

    /* jmp/call/jcc/loopcc/jxcxz? */
    if( IS_ANY_BRANCH( CodeInfo->token ) ) {
        return( process_branch( CodeInfo, CurrOpnd, opndx ) );
    }
    CodeInfo->opnd[CurrOpnd].data = opndx->value;

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
    //    if( ( opndx->base_reg != NULL ) || ( opndx->idx_reg != NULL ) ) {
    //        AsmError( INVALID_MEMORY_POINTER );
    //        return( ERROR );
    //    }
    //}

    if( opndx->instr == T_SHORT ) {
        /* short works for branch instructions only */
        AsmErr( INVALID_INSTRUCTION_OPERANDS );
        return( ERROR );
    }

    /* v2.06: added */
    if ( opndx->explicit ) {
        CodeInfo->const_size_fixed = TRUE;
        if ( CodeInfo->mem_type == MT_EMPTY )
            CodeInfo->mem_type = opndx->mem_type;
    }
    /* v2.03: don't ignore a "NEAR32 ptr" qualifier */
    //if ( CodeInfo->mem_type == MT_EMPTY && CurrOpnd > OPND1 ) {
    if ( CodeInfo->mem_type == MT_EMPTY && CurrOpnd > OPND1 && opndx->Ofssize == USE_EMPTY ) {
        size = OperandSize( CodeInfo->opnd[OPND1].type, CodeInfo );
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
        case 1:
            /* v2.05: if () added */
            if ( opndx->abs || opndx->instr == T_LOW || opndx->instr == T_HIGH )
                CodeInfo->mem_type = MT_BYTE;
            break;
        case 2:
            /* v2.05: if () added */
            if ( opndx->abs ||
                CodeInfo->Ofssize == USE16 ||
                opndx->instr == T_LOWWORD ||
                opndx->instr == T_HIGHWORD )
                CodeInfo->mem_type = MT_WORD;
            break;
        case 4:
            CodeInfo->mem_type = MT_DWORD;
            break;
#if AMD64_SUPPORT
        case 8:
            /* v2.05: it's questionable if size 8 is a good assumption for an
             * immediate constant. It's valid for MOV <reg>, <imm> only.
             */
            //case 8: CodeInfo->mem_type = MT_QWORD;break;
            /* v2.05a: added */
            if ( Ofssize == USE64 &&
                CodeInfo->token == T_MOV &&
                ( CodeInfo->opnd[OPND1].type & OP_R64 ) )
                CodeInfo->mem_type = MT_QWORD;
            break;
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
                if ( opndx->mem_type == MT_EMPTY  ) {
                    if ( opndx->instr == EMPTY ||
                        opndx->instr == T_LOW ||
                        opndx->instr == T_HIGH )
                        opndx->mem_type = MT_BYTE;
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
            else if ( opndx->sym->state == SYM_UNDEFINED ) {
                CodeInfo->mem_type = MT_BYTE;
                fixup_option = OPTJ_PUSH;
            } else
#if AMD64_SUPPORT
                /* v2.06d: changed */
                CodeInfo->mem_type = ( Ofssize == USE64 ? MT_QWORD : Ofssize == USE32 ? MT_DWORD : MT_WORD );
#else
                CodeInfo->mem_type = ( Ofssize > USE16 ? MT_DWORD : MT_WORD );
#endif
        }
    }
    size = SizeFromMemtype( CodeInfo->mem_type, Ofssize, NULL );
    switch( size ) {
    case 1:  CodeInfo->opnd[CurrOpnd].type = OP_I8;  SET_OPSIZ_NO( CodeInfo );  break;
    case 2:  CodeInfo->opnd[CurrOpnd].type = OP_I16; SET_OPSIZ_16( CodeInfo );  break;
    case 4:  CodeInfo->opnd[CurrOpnd].type = OP_I32; SET_OPSIZ_32( CodeInfo );  break;
#if AMD64_SUPPORT
    case 8:
        /* v2.05: do only assume size 8 if the constant won't fit in 4 bytes. */
        if ( opndx->value64 > LONG_MAX || opndx->value64 < LONG_MIN ||
            (opndx->explicit && ( opndx->mem_type & MT_SIZE_MASK ) == 7 ) ) {
            CodeInfo->opnd[CurrOpnd].type = OP_I64;
            CodeInfo->opnd[CurrOpnd].datahigh = opndx->hvalue;
        } else if ( Ofssize == USE64 ) {
            /* v2.06d: in 64-bit, ALWAYS set OP_I64, so "mov m64, ofs" will fail,
             * This was accepted in v2.05-v2.06c)
             */
            CodeInfo->opnd[CurrOpnd].type = OP_I64;
            CodeInfo->opnd[CurrOpnd].datahigh = opndx->hvalue;
        } else {
            CodeInfo->opnd[CurrOpnd].type = OP_I32;
        }
        SET_OPSIZ_32( CodeInfo );
        break;
#endif
#ifdef DEBUG_OUT
    default:
        DebugMsg1(("idata_fixup, unexpected size %u\n", size ));
#endif
    }

    /* set fixup_type */

    if( opndx->instr == T_SEG ) {
        fixup_type = FIX_SEG;
    } else if( CodeInfo->mem_type == MT_BYTE ) {
        DebugMsg1(("idata_fixup, mem_type=BYTE\n" ));
        if ( opndx->instr == T_HIGH ) {
            DebugMsg1(("idata_fixup, FIX_HIBYTE\n" ));
            fixup_type = FIX_HIBYTE;
        } else {
            DebugMsg1(("idata_fixup, FIX_OFF8\n" ));
            fixup_type = FIX_OFF8;
        }
#if 0
    } else if( CodeInfo->mem_type == MT_FAR ) {
        /* v2.04: to be tested. this code is most likely obsolete.
         * There's never a PTR16|PTR32 fixup here. Far JMP/CALL are handled
         * elsewhere, and data items also.
         */
        /* temporary */
        printf("idata_fixup: MT_FAR occured at %s:%lu\n", CurrFName[ASM], LineNumber );
        fixup_type = ( Ofssize ) ? FIX_PTR32 : FIX_PTR16;
        CodeInfo->isfar = TRUE; /* needed for mark_fixupp() */
        if ( opndx->Ofssize != USE_EMPTY )
            CodeInfo->Ofssize = opndx->Ofssize;
#endif
    } else if( IS_OPER_32( CodeInfo ) ) {
#if AMD64_SUPPORT
        /* v2.06: changed */
        //if ( Ofssize == USE64 && CodeInfo->mem_type == MT_QWORD )
        if ( CodeInfo->opnd[CurrOpnd].type == OP_I64 )
            fixup_type = FIX_OFF64;
        else
#endif
            /* v2.04: changed, no longer depends on OfsSize */
            /* v2.05a: changed, so size==8 won't get a FIX_OFF16 type */
            //if ( size == 4 )
            if ( size >= 4 ) {
                /* v2.06: added branch for PTR16 fixup.
                 * it's only done if type coercion is FAR (Masm-compat)
                 */
                if ( opndx->explicit && Ofssize == USE16 && opndx->mem_type == MT_FAR )
                    fixup_type = FIX_PTR16;
                else
                    fixup_type = FIX_OFF32;
            } else
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

    segm_override( opndx, NULL ); /* set SegOverride global var */

    /* set global vars Frame_Type and Frame_Datum for fixup creation */
    if ( ModuleInfo.offsettype == OT_SEGMENT &&
        ( opndx->instr == T_OFFSET || opndx->instr == T_SEG ))
        find_frame2( opndx->sym );
    else
        find_frame( opndx->sym );

    //DebugMsg1(("idata_fixup: calling CreateFixup(%s, %u)\n", opndx->sym->name, fixup_type ));
    CodeInfo->opnd[CurrOpnd].InsFixup = CreateFixup( opndx->sym, fixup_type, fixup_option );

    if ( opndx->instr == T_LROFFSET )
        CodeInfo->opnd[CurrOpnd].InsFixup->loader_resolved = TRUE;

#if IMAGERELSUPP
    if ( opndx->instr == T_IMAGEREL && fixup_type == FIX_OFF32 )
        CodeInfo->opnd[CurrOpnd].InsFixup->type = FIX_OFF32_IMGREL;
#endif
#if SECTIONRELSUPP
    if ( opndx->instr == T_SECTIONREL && fixup_type == FIX_OFF32 )
        CodeInfo->opnd[CurrOpnd].InsFixup->type = FIX_OFF32_SECREL;
#endif
    DebugMsg1(("idata_fixup exit [CodeInfo.mem_type=%Xh Ofssize=%u opsiz=%u fixup->frame=%d]\n", CodeInfo->mem_type, CodeInfo->Ofssize, CodeInfo->prefix.opsiz, CodeInfo->opnd[CurrOpnd].InsFixup->frame_type ));

    return( NOT_ERROR );
}

/* convert MT_PTR to MT_WORD, MT_DWORD, MT_FWORD, MT_QWORD.
 * MT_PTR cannot be set explicitely (by the PTR operator),
 * so this value must come from a label or a structure field.
 * (above comment is most likely plain wrong, see 'PF16 ptr [reg]'!
 * This code needs cleanup!
 */
static void SetPtrMemtype( struct code_info *CodeInfo, struct expr *opndx )
/*************************************************************************/
{
    struct asym *sym = opndx->sym;
    int size = 0;

    if ( opndx->mbr )  /* the mbr field has higher priority */
        sym = opndx->mbr;
    if ( sym ) {
        if ( sym->type ) {
            size = sym->type->total_size;
            CodeInfo->isfar = sym->type->isfar;

            /* there's an ambiguity with pointers of size DWORD,
             since they can be either NEAR32 or FAR16 */
            if ( size == 4 && sym->type->Ofssize != CodeInfo->Ofssize )
                opndx->Ofssize = sym->type->Ofssize;

        } else if ( sym->mem_type == MT_PTR ) {
            size = SizeFromMemtype( sym->isfar ? MT_FAR : MT_NEAR, sym->Ofssize, NULL );
            CodeInfo->isfar = sym->isfar;
        } else  {
            if ( sym->isarray )
                size = sym->total_size / sym->total_length;
            else
                size = sym->total_size;
        }
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
 * set fields in CodeInfo:
 * - mem_type
 * - prefix.opsiz
 * - prefix.rex REX_W
 * called by memory_operand()
 */
static void Set_Memtype( struct code_info *CodeInfo, enum memtype mem_type )
/**************************************************************************/
{
    if( CodeInfo->token == T_LEA )
        return;

    /* v2.05: changed. Set "data" types only. */
    if( mem_type == MT_EMPTY || mem_type == MT_TYPE ||
       mem_type == MT_NEAR || mem_type == MT_FAR )
        return;

    CodeInfo->mem_type = mem_type;

    if( CodeInfo->Ofssize > USE16 ) {
        /* if we are in use32 mode, we have to add OPSIZ prefix for
         * most of the 386 instructions when operand has type WORD.
         * Exceptions ( MOVSX and MOVZX ) are handled in check_size().
         */
        if ( IS_MEM_TYPE( mem_type, WORD ) )
            CodeInfo->prefix.opsiz = TRUE;
#if AMD64_SUPPORT
        /*
         * set rex Wide bit if a QWORD operand is found (not for FPU/MMX/SSE instr).
         * This looks pretty hackish now and is to be cleaned!
         * v2.01: also had issues with SSE2 MOVSD/CMPSD, now fixed!
         */
        /* v2.06: with AVX, SSE tokens may exist twice, one
         * for "legacy", the other for VEX encoding!
         */
        else if ( IS_MEMTYPE_SIZ( mem_type, sizeof( uint_64 ) ) ) {
            switch( CodeInfo->token ) {
            case T_CMPXCHG8B:
            case T_PUSH: /* for PUSH/POP, REX_W isn't needed (no 32-bit variants in 64-bit mode) */
            case T_POP:
                break;
            default:
                /* don't set REX for opcodes that accept memory operands
                 * of any size.
                 */
                if ( opnd_clstab[CodeInfo->pinstr->opclsidx].opnd_type[OPND1] == OP_M_ANY )
                    break;
                /* don't set REX for FPU opcodes */
                if ( CodeInfo->pinstr->cpu & P_FPU_MASK )
                    break;
                /* don't set REX for - most - MMX/SSE opcodes */
                if ( CodeInfo->pinstr->cpu & P_EXT_MASK ) {
                    switch ( CodeInfo->token ) {
                        /* [V]CMPSD and [V]MOVSD are also candidates,
                         * but currently they are handled in HandleStringInstructions()
                         */
                    case T_CVTSI2SD: /* v2.06: added */
                    case T_CVTSI2SS: /* v2.06: added */
                    case T_PEXTRQ: /* v2.06: added */
                    case T_PINSRQ: /* v2.06: added */
                    case T_MOVD:
#if AVXSUPP
                    case T_VCVTSI2SD:
                    case T_VCVTSI2SS:
                    case T_VPEXTRQ:
                    case T_VPINSRQ:
                    case T_VMOVD:
#endif
                        CodeInfo->prefix.rex |= REX_W;
                        break;
                    default:
                        break;
                    }
                }
                else
                    CodeInfo->prefix.rex |= REX_W;
            }
        }
#endif

    /* v2.05: IS_MEM_TYPE() doesn't work with MT_REALx */
    //} else if( CodeInfo->Ofssize == USE16 && ( IS_MEM_TYPE( mem_type, DWORD ) ) ) {
    } else {
        if( IS_MEMTYPE_SIZ( mem_type, sizeof(uint_32) ) ) {

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
            case T_CALL: /* v2.0: added */
            case T_JMP:  /* v2.0: added */
                /* in these cases, opsize does NOT need to be changed  */
                break;
            default:
                CodeInfo->prefix.opsiz = TRUE;
                break;
            }
        }
#if AMD64_SUPPORT
        /* v2.06: added because in v2.05, 64-bit memory operands were
         * accepted in 16-bit code
         */
        else if ( IS_MEMTYPE_SIZ( mem_type, sizeof(uint_64) ) ) {
            if ( opnd_clstab[CodeInfo->pinstr->opclsidx].opnd_type[OPND1] == OP_M_ANY )
                ;
            else if ( CodeInfo->pinstr->cpu & ( P_FPU_MASK | P_EXT_MASK ) )
                ;
            else if ( CodeInfo->token != T_CMPXCHG8B )
                /* setting REX.W will cause an error in codegen */
                CodeInfo->prefix.rex |= REX_W;
        }
#endif
    }
    return;
}

/*
 in: opndx=operand to process
 in: CurrOpnd=no of operand (0=first operand,1=second operand)
 out: CodeInfo->data[]
 out: CodeInfo->opnd_type[]
*/

static ret_code memory_operand( struct code_info *CodeInfo, unsigned CurrOpnd, struct expr *opndx, bool with_fixup )
/******************************************************************************************************************/
{
    char                ss = SCALE_FACTOR_1;
    int                 index = EMPTY;
    int                 base = EMPTY;
    int                 j;
    struct asym         *sym;
    uint_8              Ofssize;
    enum fixup_types    fixup_type;

    DebugMsg1(("memory_operand(opndx->value=%X, memtype=%Xh, fixup=%u) enter, [CodeInfo->memtype=%Xh, Ofssize=%u]\n",
              opndx->value, opndx->mem_type, with_fixup, CodeInfo->mem_type, CodeInfo->Ofssize ));
    CodeInfo->opnd[CurrOpnd].data = opndx->value;
    CodeInfo->opnd[CurrOpnd].type = OP_M;

    sym = opndx->sym;

    segm_override( opndx, CodeInfo );

    /* change pointer types ( MT_NEAR, MT_FAR, MT_PTR */
    /* v2.04a: should not be called if OFFSET was used */
    //if ( opndx->mem_type == MT_PTR ) /* this was before v2.04 */
    //if ( ( opndx->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS )
    /* v2.05: change reverted */
    //if ( ( opndx->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS && opndx->instr == EMPTY )
    if ( opndx->mem_type == MT_PTR )
        SetPtrMemtype( CodeInfo, opndx );
    else if ( ( opndx->mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS ) {
        int size;
        if ( opndx->Ofssize == USE_EMPTY && sym )
            opndx->Ofssize = GetSymOfssize( sym );
        size = SizeFromMemtype( opndx->mem_type, opndx->Ofssize, NULL );
        MemtypeFromSize( size, &opndx->mem_type );
    }

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
            enum memtype mem_type;
            DebugMsg1(("memory_operand: mbr %s has mem_type MT_TYPE, total_size=%u\n", opndx->mbr->name, opndx->mbr->total_size ));
            if ( MemtypeFromSize( opndx->mbr->total_size, &mem_type ) == NOT_ERROR )
                Set_Memtype( CodeInfo, mem_type );
        }
        //else  /* v2: obsolete */
        //    Set_Memtype( CodeInfo, opndx->mbr->mem_type );
        /* v2.06b: tell codegen that the member is a forward ref */
        if ( opndx->mbr->state == SYM_UNDEFINED )
            CodeInfo->undefined = TRUE;
    }

    /* instruction-specific handling */
    switch ( CodeInfo->token ) {
    case T_JMP:
    case T_CALL:
        /* the 2 branch instructions are peculiar because they
         * will work with an unsized label.
         */
        /* v1.95: convert MT_NEAR/MT_FAR and display error if no type.
         * For memory operands, expressions of type MT_NEAR/MT_FAR are
         * call [bx+<code_label>]
         */
        if ( CodeInfo->mem_type == MT_EMPTY ) {
            /* with -Zm, no size needed for indirect CALL/JMP */
            if ( ModuleInfo.m510 == FALSE &&
                ( Parse_Pass > PASS_1 && opndx->sym == NULL ) ) {
                DebugMsg1(("memory_operand, JMP/CALL: CodeInfo->memtype=empty, instruction operand must have size\n" ));
                AsmError( INSTRUCTION_OPERAND_MUST_HAVE_SIZE );
                return( ERROR );
            }
#if AMD64_SUPPORT
            opndx->mem_type = (CodeInfo->Ofssize == USE64) ? MT_QWORD : (CodeInfo->Ofssize == USE32) ? MT_DWORD : MT_WORD;
#else
            opndx->mem_type = (CodeInfo->Ofssize == USE32) ? MT_DWORD : MT_WORD;
#endif
            Set_Memtype( CodeInfo, opndx->mem_type );
            DebugMsg1(("memory_operand, JMP/CALL: CodeInfo->memtype set to %Xh\n", CodeInfo->mem_type ));
        }
        j = SizeFromMemtype( CodeInfo->mem_type, CodeInfo->Ofssize, NULL );
        if ( ( j == 1 || j > 6 )
#if AMD64_SUPPORT
            && ( CodeInfo->Ofssize != USE64 )
#endif
        ) {
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
        case  0:  CodeInfo->opnd[CurrOpnd].type = OP_M08;  break;
        case  1:  CodeInfo->opnd[CurrOpnd].type = OP_M16;  break;
        case  3:  CodeInfo->opnd[CurrOpnd].type = OP_M32;  break;
        case  5:  CodeInfo->opnd[CurrOpnd].type = OP_M48;  break;
        case  7:  CodeInfo->opnd[CurrOpnd].type = OP_M64;  break;
        case  9:  CodeInfo->opnd[CurrOpnd].type = OP_M80;  break;
        case 15:  CodeInfo->opnd[CurrOpnd].type = OP_M128; break;
#if AVXSUPP
        case 31:  CodeInfo->opnd[CurrOpnd].type = OP_M256; break;
#endif
        }
#if 0
        /* v2.06: the wide flag isn't set for memory operands currently, */
        if ( CodeInfo->opnd_type[CurrOpnd] & ( OP_M16 | OP_M32 | OP_M64 ) )
            CodeInfo->iswide = 1;
#endif
    } else if ( CodeInfo->mem_type == MT_EMPTY ) {
        /* v2.05: added */
        switch ( CodeInfo->token ) {
        case T_INC:
        case T_DEC:
            /* jwasm v1.94-v2.04 accepted unsized operand for INC/DEC */
            if ( opndx->sym == NULL ) {
                AsmError( INSTRUCTION_OPERAND_MUST_HAVE_SIZE );
                return( ERROR );
            }
            break;
        case T_PUSH:
        case T_POP:
            if ( opndx->mem_type == MT_TYPE ) {
                AsmError( INVALID_INSTRUCTION_OPERANDS );
                return( ERROR );
            }
            break;
        }
    }

    /* check for base registers */

    if ( opndx->base_reg != NULL ) {
        base = opndx->base_reg->tokval;
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

    if( opndx->idx_reg != NULL ) {
        index = opndx->idx_reg->tokval;
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
                base = opndx->idx_reg->tokval;
            } else {
                AsmErr( CANNOT_BE_USED_AS_INDEX_REGISTER, opndx->idx_reg->string_ptr );
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
        if( ( opndx->base_reg == NULL ) && ( opndx->idx_reg == NULL ) ) {
            SET_ADRSIZ( CodeInfo, Ofssize );
#if AMD64_SUPPORT
            if ( Ofssize == USE64 )
                /* v2.03: override with a segment assumed != FLAT? */
                if ( opndx->override != NULL &&
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

        /* no fixups are needed for memory operands of string instructions and XLAT/XLATB.
         * However, CMPSD and MOVSD are also SSE2 opcodes, so the fixups must be generated
         * anyways.
         */
        if ( CodeInfo->token != T_XLAT && CodeInfo->token != T_XLATB ) {
            //DebugMsg1(("memory_operand: calling CreateFixup(%s, fixup=%u) [CodeInfo->memtype=%Xh]\n", sym ? sym->name : "NULL", fixup_type, CodeInfo->mem_type));
            CodeInfo->opnd[CurrOpnd].InsFixup = CreateFixup( sym, fixup_type, OPTJ_NONE );
        }
        /* v2.03: Modend is obsolete */
        //if( Modend ) { /* is current operand END argument? */
        //    struct asym *assume;
        //    /* set global vars Frame and Frame_Datum */
        //    GetAssume( SegOverride, sym, ASSUME_NOTHING, &assume);
        //    SetFixupFrame( assume );
        //} else {
        if( mem2code( CodeInfo, CurrOpnd, ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
        //}
        /* set frame type/data in fixup */
        if ( CodeInfo->opnd[CurrOpnd].InsFixup ) {
            CodeInfo->opnd[CurrOpnd].InsFixup->frame_type = Frame_Type;
            CodeInfo->opnd[CurrOpnd].InsFixup->frame_datum = Frame_Datum;
        }

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
            CurrOpnd == OPND2 &&
            (CodeInfo->opnd_type[OPND1] & OP_M_ANY) )
            CodeInfo->opnd_type[CurrOpnd] = OP_I8;
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
        if( mem2code( CodeInfo, CurrOpnd, ss, index, base, sym ) == ERROR ) {
            return( ERROR );
        }
    } /* end if ( with_fixup ) */

    DebugMsg1(("memory_operand exit, ok, opndx.type/value=%Xh/%Xh, CodeInfo.memtype/rmbyte=%X/%X opndtype=%Xh fix=%Xh\n",
              opndx->type, opndx->value, CodeInfo->mem_type, CodeInfo->rm_byte, CodeInfo->opnd[CurrOpnd].type, CodeInfo->opnd[CurrOpnd].InsFixup ));
    return( NOT_ERROR );
}

ret_code process_address( struct code_info *CodeInfo, unsigned CurrOpnd, struct expr *opndx )
/*******************************************************************************************/
/*
 * parse the memory reference operand
 * CurrOpnd is 0 for first operand, 1 for second, ...
 * valid return values: NOT_ERROR, ERROR
 */
{
    enum memtype mem_type;

    if( opndx->indirect ) {  /* indirect register operand or stack var */

        DebugMsg1(("process_address: INDIRECT, sym=%s mbr=%s ci.adrsiz=%u\n",
                   opndx->sym ? opndx->sym->name : "NULL",
                   opndx->mbr ? opndx->mbr->name : "NULL",
                   CodeInfo->prefix.adrsiz ));
        if ( opndx->hvalue && ( opndx->hvalue != -1 || opndx->value >= 0 ) ) {
            /* Masm (both ML and ML64) just truncates.
             * JWasm throws an error in 64bit mode and
             * warns (level 3) in the other modes.
             */
            DebugMsg1(("process_address: displacement doesn't fit in 32 bits: %I64X\n", opndx->value64 ));
#if AMD64_SUPPORT
            /* todo: check if really useful */
            if ( ModuleInfo.Ofssize == USE64 ) {
                EmitConstError( opndx );
                return( ERROR );
            }
#endif
            AsmWarn( 3, DISPLACEMENT_OUT_OF_RANGE, opndx->value64 );
        }
        if( opndx->sym == NULL || opndx->sym->state == SYM_STACK ) {
            return( memory_operand( CodeInfo, CurrOpnd, opndx, FALSE ) );
        } else {
            return( memory_operand( CodeInfo, CurrOpnd, opndx, TRUE ) );
        }
    } else if( opndx->instr != EMPTY ) {
        /* instr is OFFSET | LROFFSET | SEG | LOW | LOWWORD, ... */
        DebugMsg1(("process_address: instr=%s\n", GetResWName( opndx->instr, NULL ) ));
        if( opndx->sym == NULL ) { /* better to check opndx->type? */
            return( idata_nofixup( CodeInfo, CurrOpnd, opndx ) );
        } else {
            /* allow "lea <reg>, [offset <sym>]" */
            if( CodeInfo->token == T_LEA && opndx->instr == T_OFFSET )
                return( memory_operand( CodeInfo, CurrOpnd, opndx, TRUE ) );
            return( idata_fixup( CodeInfo, CurrOpnd, opndx ) );
        }
    } else if( opndx->sym == NULL ) { /* direct operand without symbol */
        DebugMsg1(("process_address: symbol=NULL\n" ));
        if( opndx->override != NULL ) {
            /* direct absolute memory without symbol.
             DS:[0] won't create a fixup, but
             DGROUP:[0] will create one! */
#if AMD64_SUPPORT
            /* for 64bit, always create a fixup, since RIP-relative addressing is used */
            if ( opndx->override->token == T_REG && CodeInfo->Ofssize != USE64 )
#else
            if ( opndx->override->token == T_REG )
#endif
                return( memory_operand( CodeInfo, CurrOpnd, opndx, FALSE ) );
            else
                return( memory_operand( CodeInfo, CurrOpnd, opndx, TRUE ) );
        } else {
            return( idata_nofixup( CodeInfo, CurrOpnd, opndx ) );
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
            return( process_branch( CodeInfo, CurrOpnd, opndx ) );

        switch( CodeInfo->token ) {
        case T_PUSH:
        case T_PUSHW:
        case T_PUSHD:
            /* v2.0: don't assume immediate operand if cpu is 8086 */
            if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) > P_86 ) {
                //return( idata_nofixup( CodeInfo, opndx ) ); /* v1.96: changed */
                return( idata_fixup( CodeInfo, CurrOpnd, opndx ) );
            }
            break;
        default:
            /* v2.04: if operand is the second argument (and the first is NOT
             * a segment register!), scan the
             * instruction table if the instruction allows an immediate!
             * If so, assume the undefined symbol is a constant.
             */
            if ( CurrOpnd == OPND2 && (( CodeInfo->opnd[OPND1].type & OP_SR ) == 0 ) ) {
                const struct instr_item  *p = CodeInfo->pinstr;
                do {
                    if ( opnd_clstab[p->opclsidx].opnd_type[OPND2] & OP_I )
                        return( idata_fixup( CodeInfo, CurrOpnd, opndx ) );
                    p++;
                } while ( p->first == FALSE );
            }
        }
        /* v2.0: fixups should be generated for undefined operands
         * to allow backpatching!
         */
        //return( memory_operand( CodeInfo, opndx, FALSE ) );
        return( memory_operand( CodeInfo, CurrOpnd, opndx, TRUE ) );

    } else if( ( opndx->sym->state == SYM_SEG ) ||
               ( opndx->sym->state == SYM_GRP ) ) {
        DebugMsg1(("process_address: sym->state=SEG/GROUP\n"));
        /* SEGMENT and GROUP symbol is converted to SEG symbol
         * for next processing */
        opndx->instr = T_SEG;
        return( idata_fixup( CodeInfo, CurrOpnd, opndx ) );
    } else {
        DebugMsg1(("process_address direct, sym=%s sym.memtype=%X opndx.memtype=%X\n", opndx->sym->name, opndx->sym->mem_type, opndx->mem_type ));
        //mem_type = ( opndx->explicit ) ? opndx->mem_type : opndx->sym->mem_type;
        /* to fade usage of 'explicit' */
        mem_type = opndx->mem_type;

        /* symbol external, but absolute? */
        if( opndx->abs ) {
            return( idata_fixup( CodeInfo, CurrOpnd, opndx ) );
        }

        /* CODE location is converted to OFFSET symbol */
        switch( mem_type ) {
        case MT_FAR:
        case MT_NEAR:
        case MT_PROC:
            if( CodeInfo->token == T_LEA ) {
                return( memory_operand( CodeInfo, CurrOpnd, opndx, TRUE ) );
            //} else if( opndx->sym == &symPC ) {
            //    return( idata_fixup( CodeInfo, opndx ) );
            } else if( opndx->mbr != NULL ) { /* structure field? */
                return( memory_operand( CodeInfo, CurrOpnd, opndx, TRUE ) );
            } else {
                return( idata_fixup( CodeInfo, CurrOpnd, opndx ) );
            }
            break;
        default:
            /* direct memory with fixup */
            return( memory_operand( CodeInfo, CurrOpnd, opndx, TRUE ) );
            break;
        }
    }
    return( NOT_ERROR );
}

/* Handle constant operands.
 * These never need a fixup. Externals - even "absolute" ones -
 * are always labeled as EXPR_ADDR by the expression evaluator.
 */
static ret_code process_const( struct code_info *CodeInfo, unsigned CurrOpnd, struct expr *opndx )
/************************************************************************************************/
{
#if 0 /* v2.06: obsolete */
    /* hack for IMUL: compress the operands so there are 2 only */
    if( ( CodeInfo->token == T_IMUL ) &&
       ( CodeInfo->opnd_type[OPND1] & OP_R ) ) {
        if( CurrOpnd == OPND2 ) {
#if AMD64_SUPPORT
            CodeInfo->prefix.rex |= ((CodeInfo->prefix.rex & REX_B) ? REX_R : 0);
#endif
            CodeInfo->rm_byte = ( CodeInfo->rm_byte & ~BIT_345 )
                          | ( ( CodeInfo->rm_byte & BIT_012 ) << 3 );
        } else if( CurrOpnd == OPND3 ) {
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
            CurrOpnd = OPND2;
        }
    }
#endif
    /* optimization: skip <value> if it is 0 and instruction
     * is RET[W|D|N|F]. */
    /* v2.06: moved here and checked the opcode directly, so
     * RETD and RETW are also handled. */
    if ( ( ( CodeInfo->pinstr->opcode & 0xf7 ) == 0xc2 ) &&
        CurrOpnd == OPND1 && opndx->value == 0 ) {
        //     (CodeInfo.token == T_RET ||
        //      CodeInfo.token == T_RETN ||
        //      CodeInfo.token == T_RETF)) {
        //if ( opndx.sym == NULL || opndx.sym->state == SYM_INTERNAL ) {
        return( NOT_ERROR );
    }
    return( idata_nofixup( CodeInfo, CurrOpnd, opndx ) );
}

static ret_code process_register( struct code_info *CodeInfo, unsigned CurrOpnd, const struct expr opndx[] )
/**********************************************************************************************************/
/*
 * parse and encode direct register operands. Modifies:
 * - CodeInfo->opnd_type
 * - CodeInfo->rm_byte (depending on CurrOpnd)
 * - CodeInfo->iswide
 * - CodeInfo->x86hi_used/x64lo_used
 * - CodeInfo->prefix.rex
 */
{
    enum special_token regtok;
    int  regno;
    uint_32 flags;

    DebugMsg1(( "process_register enter (%s)\n", opndx[CurrOpnd].base_reg->string_ptr ));
    regtok = opndx[CurrOpnd].base_reg->tokval;
    regno = GetRegNo( regtok );
    /* the register's "OP-flags" are stored in the 'value' field */
    flags = GetValueSp( regtok );
    CodeInfo->opnd[CurrOpnd].type = flags;
    if ( flags & OP_R8 ) {
        /* it's probably better to not reset the wide bit at all */
        if ( flags != OP_CL )      /* problem: SHL AX|AL, CL */
            CodeInfo->iswide = 0;

#if AMD64_SUPPORT
        if ( CodeInfo->Ofssize == USE64 && regno >=4 && regno <=7 )
            if ( SpecialTable[regtok].cpu == P_86 )
                CodeInfo->x86hi_used = 1; /* it's AH,BH,CH,DH */
            else
                CodeInfo->x64lo_used = 1; /* it's SPL,BPL,SIL,DIL */
#endif
        if ( StdAssumeTable[regno].error & (( regtok >= T_AH && regtok <= T_BH ) ? RH_ERROR : RL_ERROR ) ) {
            DebugMsg(("process_register: assume error, reg=%u\n", regno ));
            AsmError( USE_OF_REGISTER_ASSUMED_TO_ERROR );
            return( ERROR );
        }
    } else if ( flags & OP_R ) { /* 16-, 32- or 64-bit GPR? */
        CodeInfo->iswide = 1;
        if ( StdAssumeTable[regno].error & flags & OP_R ) {
            DebugMsg(("process_register: assume error, reg=%u\n", regno ));
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

        regno = opndx[CurrOpnd].st_idx;
        if ( regno > 7 ) { /* v1.96: index check added */
            AsmError( INVALID_COPROCESSOR_REGISTER );
            return( ERROR );
        }
        CodeInfo->rm_byte |= regno;
        if( regno != 0 )
            CodeInfo->opnd[CurrOpnd].type = OP_ST_REG;
        /* v2.06: exit, rm_byte is already set. */
        return( NOT_ERROR );

    } else if ( flags & OP_RSPEC ) { /* CRx, DRx, TRx */
        if( CodeInfo->token != T_MOV ) {
            AsmError( ONLY_MOV_CAN_USE_SPECIAL_REGISTER );
            return( ERROR );
        }
        /* v2.04: previously there were 3 flags, OP_CR, OP_DR and OP_TR.
         * this was summoned to one flag OP_RSPEC to free 2 flags, which
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
                   && ( ( SpecialTable[regtok].cpu & P_CPU_MASK ) >= P_486 ) ) {
                    /* TR3-TR5 are available on 486 only */
                    AsmErr( CANNOT_USE_TRN_TO_TRM_WITH_CURRENT_CPU_SETTING, 3, 5 );
                    return( ERROR );
                }
                break;
            case 0x26:
            case 0x27:
                if( ( ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 )
                     || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_686 ) )
                   && ( ( SpecialTable[regtok].cpu & P_CPU_MASK ) >= P_386 ) ) {
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
    if ( ( SpecialTable[regtok].cpu & P_CPU_MASK ) == P_64 ) {
        CodeInfo->prefix.rex |= 0x40;
        if ( flags & OP_R64 )
            CodeInfo->prefix.rex |= REX_W;
    }
#endif

    if( CurrOpnd == OPND1 ) {
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
        if( ( CodeInfo->token == T_XCHG ) && ( CodeInfo->opnd[OPND1].type & OP_A ) &&
             ( 0 == (CodeInfo->opnd[OPND1].type & OP_R8 ) ) ) {
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
 * CMPS[B|W|D|Q]
 *  INS[B|W|D]
 * LODS[B|W|D|Q]
 * MOVS[B|W|D|Q]
 * OUTS[B|W|D]
 * SCAS[B|W|D|Q]
 * STOS[B|W|D|Q]
 * the peculiarity is that these instructions ( optionally )
 * have memory operands, which aren't used for code generation
 * <opndx> contains the last operand.
 */

static void HandleStringInstructions( struct code_info *CodeInfo, const struct expr opndx[] )
/*******************************************************************************************/
{
    int opndidx = OPND1;
    int op_size;

    switch( CodeInfo->token ) {
#if AVXSUPP
    case T_VCMPSD:
#endif
    case T_CMPSD:
        /* filter SSE2 opcode CMPSD */
        if ( CodeInfo->opnd[OPND1].type & (OP_XMM | OP_MMX)) {
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
#if AMD64_SUPPORT
    case T_CMPSQ:
#endif
         /* cmps allows prefix for the first operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY ) {
            if ( opndx[OPND2].override != NULL ) {
                if ( CodeInfo->prefix.RegOverride == ASSUME_ES ) {
                    /* content of LastRegOverride is valid if
                     * CodeInfo->RegOverride is != EMPTY.
                     */
                    if ( LastRegOverride == ASSUME_DS )
                        CodeInfo->prefix.RegOverride = EMPTY;
                    else
                        CodeInfo->prefix.RegOverride = LastRegOverride;
                } else {
                    DebugMsg1(("HandleStringInstructions: CMPS: CodeInfo->RegOverride=%X, opndx->override=%s\n", CodeInfo->prefix.RegOverride, opndx[OPND2].override->string_ptr ));
                    AsmError( INVALID_INSTRUCTION_OPERANDS );
                }
            } else if ( CodeInfo->prefix.RegOverride == ASSUME_DS ) {
                /* prefix for first operand? */
                CodeInfo->prefix.RegOverride = EMPTY;
            }
        }
        break;
#if AVXSUPP
    case T_VMOVSD:
#endif
    case T_MOVSD:
        /* filter SSE2 opcode MOVSD */
        if ( ( CodeInfo->opnd[OPND1].type & (OP_XMM | OP_MMX) ) ||
            ( CodeInfo->opnd[OPND2].type & (OP_XMM | OP_MMX) ) ) {
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
#if AMD64_SUPPORT
    case T_MOVSQ:
#endif
        /* movs allows prefix for the second operand (=source) only */
        if ( CodeInfo->prefix.RegOverride != EMPTY )
            if ( opndx[OPND2].override == NULL )
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
    case T_LODS:
    case T_LODSB:
    case T_LODSW:
    case T_LODSD:
#if AMD64_SUPPORT
    case T_LODSQ:
#endif
        break;
    default: /*INS[B|W|D], SCAS[B|W|D|Q], STOS[B|W|D|Q] */
        /* INSx, SCASx and STOSx don't allow any segment prefix != ES
         for the memory operand.
         */
        if ( CodeInfo->prefix.RegOverride != EMPTY )
            if ( CodeInfo->prefix.RegOverride == ASSUME_ES )
                CodeInfo->prefix.RegOverride = EMPTY;
            else
                AsmError( INVALID_INSTRUCTION_OPERANDS );
    }

    if ( opnd_clstab[CodeInfo->pinstr->opclsidx].opnd_type[opndidx] == OP_NONE ) {
        CodeInfo->iswide = 0;
        CodeInfo->prefix.opsiz = FALSE;
    }

    /* if the instruction is the variant without suffix (MOVS, LODS, ..),
     * then use the operand's size to get further info.
     */
    //if ( CodeInfo->pinstr->opnd_type[opndidx] != OP_NONE &&
    if ( opnd_clstab[CodeInfo->pinstr->opclsidx].opnd_type[opndidx] != OP_NONE &&
        CodeInfo->opnd[opndidx].type != OP_NONE ) {
        op_size = OperandSize( CodeInfo->opnd[opndidx].type, CodeInfo );
        /* v2.06: added. if memory operand has no size */
        if ( op_size == 0 ) {
            if ( CodeInfo->opnd[opndidx].InsFixup == NULL || CodeInfo->opnd[opndidx].InsFixup->sym->state != SYM_UNDEFINED )
                AsmError( INSTRUCTION_OPERAND_MUST_HAVE_SIZE );
            op_size = 1; /* assume shortest format */
        }
        switch( op_size ) {
        case 1:
            CodeInfo->iswide = 0;
            //if( CodeInfo->Ofssize )
                CodeInfo->prefix.opsiz = FALSE;
            break;
        case 2:
            CodeInfo->iswide = 1;
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? TRUE : FALSE;
            break;
        case 4:
            CodeInfo->iswide = 1;
            CodeInfo->prefix.opsiz = CodeInfo->Ofssize ? FALSE : TRUE;
            break;
#if AMD64_SUPPORT
        case 8:
            if ( CodeInfo->Ofssize == USE64 ) {
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

static ret_code check_size( struct code_info *CodeInfo, const struct expr opndx[] )
/*********************************************************************************/
/*
 * - use to make sure the size of first operand match the size of second operand;
 * - optimize MOV instruction;
 * - opndx contains last operand
 * todo: BOUND second operand check ( may be WORD/DWORD or DWORD/QWORD ).
 * tofix: add a flag in instr_table[] if there's NO check to be done.
 */
{
    enum operand_type op1 = CodeInfo->opnd[OPND1].type;
    enum operand_type op2 = CodeInfo->opnd[OPND2].type;
    ret_code    rc = NOT_ERROR;
    int         op1_size;
    int         op2_size;
    //int         op_size = 0;

    DebugMsg1(("check_size enter, optype1=%X, optype2=%X\n", op1, op2 ));
    switch( CodeInfo->token ) {
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
        if ( opndx[OPND2].kind == EXPR_ADDR &&
            Parse_Pass == PASS_1 &&
            opndx[OPND2].indirect == FALSE &&
            opndx[OPND2].sym &&
            opndx[OPND2].sym->state == SYM_UNDEFINED ) {
            CodeInfo->opnd[OPND2].type = OP_I8;
            CodeInfo->opnd[OPND2].data = 1;
        }
        /* v2.06: added (because if first operand is memory, wide bit
         * isn't set!)
         */
        if ( OperandSize( op1, CodeInfo ) > 1 )
            CodeInfo->iswide = 1;
        /* v2.06: makes the OP_CL_ONLY case in codegen.c obsolete */
        if ( op2 == OP_CL ) {
            /* CL is encoded in bit 345 of rm_byte, but we don't need it
             * so clear it here */
            CodeInfo->rm_byte &= NOT_BIT_345;
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
            if( CodeInfo->opnd[OPND1].data > (signed long)USHRT_MAX ) {
                /* if op1 is really 32-bit data, then error */
                AsmError( INVALID_OPERAND_SIZE );
                rc = ERROR;
            }
        }
        /* type cast op1 to OP_I16 */
        CodeInfo->opnd[OPND1].type = OP_I16;
        /* op2 have to be 8-bit data */
        if( op2 >= OP_I16 ) {
            if( CodeInfo->opnd[OPND2].data > UCHAR_MAX ) {
                AsmError( INVALID_OPERAND_SIZE );
                rc = ERROR;
            }
            CodeInfo->opnd[OPND2].type = OP_I8;
        }
        break;
    case T_MOVSX:
    case T_MOVZX:
        CodeInfo->iswide = 0;
        op1_size = OperandSize( op1, CodeInfo );
        op2_size = OperandSize( op2, CodeInfo );
        DebugMsg1(("check_size, MOVZX/MOVSX: op2_size=%u, opndx.memtype=%Xh, opndx.sym=%X\n", op2_size, opndx[OPND2].mem_type, opndx[OPND2].sym ));
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
    case T_ARPL: /* v2.06: new, avoids the OP_R16 hack in codegen.c */
        CodeInfo->prefix.opsiz = 0;
        goto def_check;
        break;
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
    case T_IMUL: /* v2.06: check for 3rd operand must be done here */
        if ( CodeInfo->opnd[OPND3].type != OP_NONE ) {
            int op3_size;
            op1_size = OperandSize( op1, CodeInfo );
            op3_size = OperandSize( CodeInfo->opnd[OPND3].type, CodeInfo );
            /* the only case which must be checked here
             * is a WORD register as op1 and a DWORD immediate as op3 */
            if ( op1_size == 2 && op3_size > 2 ) {
                AsmErr( OPERANDS_MUST_BE_THE_SAME_SIZE, op1_size, op3_size );
                rc = ERROR;
                break;
            }
            if ( CodeInfo->opnd[OPND3].type & ( OP_I16 | OP_I32 ) )
                CodeInfo->opnd[OPND3].type = ( op1_size == 2 ? OP_I16 : OP_I32 );
        }
        goto def_check;
        break;
    case T_CVTSD2SI:
    case T_CVTTSD2SI:
    case T_CVTSS2SI:
    case T_CVTTSS2SI:
    //case T_MOVNTI: /* v2.05: removed */
#if AVXSUPP
    case T_VBROADCASTSD:
    case T_VBROADCASTF128:
    case T_VEXTRACTF128:
    case T_VINSERTF128:
    case T_VCVTSD2SI:
    case T_VCVTTSD2SI:
    case T_VCVTSS2SI:
    case T_VCVTTSS2SI:
#endif
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
        if( op1 & OP_SR ) { /* segment register as op1? */
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
             */
            if ( CodeInfo->isdirect == FALSE ) {
                if( CodeInfo->opnd[OPND1].type & OP_A ) {
                    /* short form exists for direct addressing only, change OP_A to OP_R! */
                    CodeInfo->opnd[OPND1].type &= ~OP_A;
                    DebugMsg1(("check_size: op1 changed to %X\n", CodeInfo->opnd[OPND1].type ));
                } else if ( CodeInfo->opnd[OPND2].type & OP_A ) {
                    /* short form exists for direct addressing only, change OP_A to OP_R! */
                    CodeInfo->opnd[OPND2].type &= ~OP_A;
                    DebugMsg1(("check_size: op2 changed to %X\n", CodeInfo->opnd[OPND2].type ));
                }
            }
#if AMD64_SUPPORT
            else if ( CodeInfo->Ofssize == USE64 ) {
                /* for 64bit, direct addressing with AL/AX/EAX/RAX (opc A0-A3)
                 * expect a full 64-bit moffs. There's currently no syntax
                 * available to allow this variant, so the OP_A flag must be
                 * reset.
                 */
                if( CodeInfo->opnd[OPND1].type & OP_A ) {
                    CodeInfo->opnd[OPND1].type &= ~OP_A;
                } else if ( CodeInfo->opnd[OPND2].type & OP_A ) {
                    CodeInfo->opnd[OPND2].type &= ~OP_A;
                }
            }
#endif
        /* v2.06: moves to/from special regs are now marked with
         * F_0FNO66 in instruct.h, making this branch obsolete.
         */
        //} else if( ( op1 & OP_RSPEC ) || ( op2 & OP_RSPEC ) ) {
        //    CodeInfo->prefix.opsiz = FALSE;
        //    return( rc ); /* v1.96: removed */
        }
        /* fall through */
    default:
//#if AMD64_SUPPORT
    def_check:
//#endif
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
            ( CodeInfo->opnd[OPND2].data <= UCHAR_MAX ) &&
            //CodeInfo->opnd[OPND2].data >= -128 ) ) {
            ( CodeInfo->opnd[OPND2].data >= -255 ) ) {
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
#if AVXSUPP
            if( ( op1 | op2 ) & ( OP_MMX | OP_XMM | OP_YMM ) ) {
#else
            if( ( op1 | op2 ) & ( OP_MMX | OP_XMM ) ) {
#endif
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
                    if( (unsigned long)CodeInfo->opnd[OPND2].data > USHRT_MAX || op2_size == 4 ) {
                        CodeInfo->iswide = 1;
                        DebugMsg1(("check_size: op1=%X op1_size=0, op2=%X, op2_size=%u CodeInfo->data[2]=%X\n", op1, op2, op2_size, CodeInfo->opnd[OPND2].data ));
#if 1 /* added v1.95: in 16bit code, 'mov [di],8000h' should warn: assuming WORD */
                        if ( ModuleInfo.Ofssize == USE16 && op2_size > 2 && InWordRange( CodeInfo->opnd[OPND2].data ) )
                            op2_size = 2;
#endif
                        if (op2_size <= 2 && CodeInfo->opnd[OPND2].data > SHRT_MIN && ModuleInfo.Ofssize == USE16 ) {
                            CodeInfo->mem_type = MT_WORD;
                            CodeInfo->opnd[OPND2].type = OP_I16;
                        } else {
                            CodeInfo->mem_type = MT_DWORD;
                            CodeInfo->opnd[OPND2].type = OP_I32;
                            p = "DWORD";
                        }
                    } else if( (unsigned long)CodeInfo->opnd[OPND2].data > UCHAR_MAX || op2_size == 2) {
                         CodeInfo->mem_type = MT_WORD;
                         CodeInfo->iswide = 1;
                         CodeInfo->opnd[OPND2].type = OP_I16;
                    } else {
                         CodeInfo->mem_type = MT_BYTE;
                         CodeInfo->opnd[OPND2].type = OP_I8;
                         p = "BYTE";
                    }
                    if( opndx[OPND2].explicit == FALSE ) {
                        /* v2.06: emit warning at pass one if mem op isn't a forward ref */
                        /* v2.06b: added "undefined" check */
                        if ( ( CodeInfo->opnd[OPND1].InsFixup == NULL && Parse_Pass == PASS_1 && CodeInfo->undefined == FALSE ) ||
                            ( CodeInfo->opnd[OPND1].InsFixup && Parse_Pass == PASS_2 ) )
                                AsmWarn( 1, SIZE_NOT_SPECIFIED_ASSUMING, p );
                    }
                } else if( ( op1 & OP_M_ANY ) && ( op2 & ( OP_R | OP_SR ) ) ) {
                } else if( ( op1 & ( OP_MMX | OP_XMM ) ) && ( op2 & OP_I ) ) {
                    if( (unsigned long)CodeInfo->opnd[OPND2].data > USHRT_MAX ) {
                         CodeInfo->opnd[OPND2].type = OP_I32;
                    } else if( (unsigned long)CodeInfo->opnd[OPND2].data > UCHAR_MAX ) {
                         CodeInfo->opnd[OPND2].type = OP_I16;
                    } else {
                         CodeInfo->opnd[OPND2].type = OP_I8;
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

static struct asym *IsType( const char *name )
/********************************************/
{
    struct asym *sym;

    sym = SymSearch( name );
    if ( sym && (sym->state == SYM_TYPE ) )
        return( sym );
    return( NULL );
}

/*
 * ParseLine() is the main parser function.
 * It scans the tokens in tokenarray[] and does:
 * - for code labels: call CreateLabel()
 * - for data items and data directives: call data_dir()
 * - for other directives: call directive[]()
 * - for instructions: fill CodeInfo and call codegen()
 */

ret_code ParseLine( struct asm_tok tokenarray[] )
/**********************************************/
{
    int                 i;
    int                 j;
    unsigned            CurrOpnd;
    ret_code            temp;
    struct asym         *sym;
    uint_32             oldofs;
#ifdef DEBUG_OUT
    char                *instr;
#endif
    struct code_info    CodeInfo;
#if AVXSUPP
    struct expr         opndx[MAX_OPND+1];
#else
    struct expr         opndx[MAX_OPND];
#endif

    DebugMsg1(("ParseLine enter, Token_Count=%u, queue_level=%u, ofs=%Xh\n",
              Token_Count, queue_level, GetCurrOffset() ));

    i = 0;

    /* Does line start with a code label? */
    if ( tokenarray[0].token == T_ID && ( tokenarray[1].token == T_COLON || tokenarray[1].token == T_DBL_COLON ) ) {
        i = 2;
        DebugMsg1(("ParseLine T_COLON, code label=%s\n", tokenarray[0].string_ptr ));
        if( DefineProc == TRUE ) write_prologue();

        /* create a global or local code label */
        if( CreateLabel( tokenarray[0].string_ptr, MT_NEAR, NULL,
                        ( CurrProc != NULL && tokenarray[1].token != T_DBL_COLON  ) ) == ERROR ) {
            DebugMsg(("ParseLine, CreateLabel(%s) failed, exit\n", tokenarray[0].string_ptr ));
            return( ERROR );
        }
        if ( tokenarray[i].token == T_FINAL ) {
            /* v2.06: this is a bit too late. Should be done BEFORE
             * CreateLabel, because of '@@'. There's a flag supposed to
             * be used for this handling, LOF_STORED in line_flags.
             * It's only a problem if a '@@:' is the first line
             * in the code section.
             */
            FStoreLine();
            if ( CurrFile[LST] ) {
                LstWrite( LSTTYPE_LABEL, 0, NULL );
            }
            return( NOT_ERROR );
        }
    }

    /* handle directives and (anonymous) data items */

    if ( tokenarray[i].token != T_INSTRUCTION ) {
        /* a code label before a data item is only accepted in Masm5 compat mode */
        Frame_Type = EMPTY;
        SegOverride = NULL;
        if ( i == 0 && tokenarray[0].token == T_ID ) {
            /* token at pos 0 may be a label.
             * it IS a label if:
             * 1. token at pos 1 is a directive (lbl dd ...)
             * 2. token at pos 0 is NOT a userdef type ( lbl DWORD ...)
             * 3. inside a struct and token at pos 1 is a userdef type
             *    or a predefined type. (usertype DWORD|usertype ... )
             *    the separate namespace allows this syntax here.
             */
            if( tokenarray[1].token == T_DIRECTIVE )
                i++;
            else {
                sym = IsType( tokenarray[0].string_ptr );
                if ( sym == NULL )
                    i++;
                else if ( CurrStruct &&
                         ( ( tokenarray[1].token == T_STYPE ) ||
                          ( tokenarray[1].token == T_ID && ( IsType( tokenarray[1].string_ptr ) ) ) ) )
                    i++;
            }
        }
        switch ( tokenarray[i].token ) {
        case T_DIRECTIVE:
            DebugMsg1(("ParseLine: T_DIRECTIVE >%s<\n", tokenarray[i].string_ptr ));
            if ( tokenarray[i].dirtype == DRT_DATADIR ) {
                return( data_dir( i, tokenarray, NULL ) );
            }
            if( CurrStruct && ( GetValueSp( tokenarray[i].tokval ) & DF_NOSTRUC ) ) {
                AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
                return( ERROR );
            }
            /* label allowed for directive? */
            //if ( tokenarray[i].flags & DF_LABEL ) {
            if ( GetValueSp( tokenarray[i].tokval ) & DF_LABEL ) {
                if ( i && tokenarray[0].token != T_ID ) {
                    AsmErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
                    return( ERROR );
                }
            } else if ( i && tokenarray[i-1].token != T_COLON && tokenarray[i-1].token != T_DBL_COLON ) {
                AsmErr( SYNTAX_ERROR_EX, tokenarray[i-1].string_ptr );
                return( ERROR );
            }
            /* must be done BEFORE FStoreLine()! */
            if( DefineProc == TRUE ) {
                if ( GetValueSp( tokenarray[i].tokval ) & DF_PROC )
                    write_prologue();
            }
#if FASTPASS
            if ( StoreState || ( GetValueSp( tokenarray[i].tokval ) & DF_STORE ) )
                FStoreLine();
#endif
            if ( tokenarray[i].dirtype > DRT_DATADIR ) {
                temp = directive[tokenarray[i].dirtype]( i, tokenarray );
            } else {
                temp = ERROR;
                /* ENDM, EXITM and GOTO directives should never be seen here */
                switch( tokenarray[i].tokval ) {
                case T_ENDM:
                    AsmError( UNMATCHED_MACRO_NESTING );
                    break;
                case T_EXITM:
                case T_GOTO:
                    AsmError( DIRECTIVE_MUST_APPEAR_INSIDE_A_MACRO );
                    break;
                default:
                    /* this error may happen if
                     * CATSTR, SUBSTR, MACRO, ...
                     * aren't at pos 1
                     */
                    AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
                    break;
                }
            }
            /* v2.0: for generated code it's important that list file is 
             * written in ALL passes, to update file position! */
            //if ( ModuleInfo.list && (( line_flags & LOF_LISTED ) == 0 ) && Parse_Pass == PASS_1 )
            if ( ModuleInfo.list &&
                ( ( ModuleInfo.line_flags & LOF_LISTED ) == 0 ) &&
                ( Parse_Pass == PASS_1 || GeneratedCode ) )
                LstWriteSrcLine();
            return( temp );
        case T_STYPE:
            DebugMsg1(("ParseLine: T_STYPE >%s<\n", tokenarray[i].string_ptr ));
            return( data_dir( i, tokenarray, NULL ) );
        case T_ID:
            DebugMsg1(("ParseLine: T_ID >%s<\n", tokenarray[i].string_ptr ));
            if( sym = IsType( tokenarray[i].string_ptr ) ) {
                return( data_dir( i, tokenarray, (struct dsym *)sym ) );
            }
            break;
        default:
            if ( tokenarray[i].token == T_COLON ) {
                DebugMsg(("ParseLine: unexpected colon\n" ));
                AsmError( SYNTAX_ERROR_UNEXPECTED_COLON );
                return( ERROR );
            }
            break;
        } /* end switch (tokenarray[i].token) */
        if ( i && tokenarray[i-1].token == T_ID )
            i--;
        DebugMsg(("ParseLine: unexpected token=%u, i=%u, string=%s\n", tokenarray[i].token, i, tokenarray[i].string_ptr));
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    DebugMsg1(("ParseLine: %s\n", tokenarray[i].string_ptr));
    /* v2.04 added */
    if( CurrStruct ) {
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
        return( ERROR );
    }

    if( DefineProc == TRUE ) write_prologue();

    FStoreLine(); /* must be placed AFTER write_prologue() */

    if ( CurrFile[LST] ) oldofs = GetCurrOffset();

    /* init CodeInfo */
    CodeInfo.prefix.ins         = EMPTY;
    CodeInfo.prefix.RegOverride = EMPTY;
#if AMD64_SUPPORT
    CodeInfo.prefix.rex     = 0;
#endif
    CodeInfo.prefix.adrsiz  = FALSE;
    CodeInfo.prefix.opsiz   = FALSE;
    CodeInfo.mem_type       = MT_EMPTY;
    for( j = 0; j < MAX_OPND; j++ ) {
        CodeInfo.opnd[j].type = OP_NONE;
#ifdef DEBUG_OUT
        CodeInfo.opnd[j].data = -1;
        /* make sure it's invalid */
        CodeInfo.opnd[j].InsFixup = (void *)0xffffffff;
#endif
    }
    CodeInfo.rm_byte        = 0;
    CodeInfo.sib            = 0;            /* assume ss is *1 */
    CodeInfo.Ofssize        = ModuleInfo.Ofssize;
    CodeInfo.opc_or         = 0;
#if AVXSUPP
    CodeInfo.vexregop       = 0;
#endif
    CodeInfo.flags          = 0;

    /* instruction prefix?
     * T_LOCK, T_REP, T_REPE, T_REPNE, T_REPNZ, T_REPZ */
    if ( tokenarray[i].tokval >= T_LOCK && tokenarray[i].tokval <= T_REPZ ) {
        CodeInfo.prefix.ins = tokenarray[i].tokval;
        i++;
        /* prefix has to be followed by an instruction */
        if( tokenarray[i].token != T_INSTRUCTION ) {
            DebugMsg(("ParseLine: unexpected token %u after prefix, exit, error\n", tokenarray[i].token ));
            AsmError( PREFIX_MUST_BE_FOLLOWED_BY_AN_INSTRUCTION );
            return( ERROR );
        }
        DebugMsg1(("ParseLine: %s\n", tokenarray[i].tokpos));
    };

    if( CurrProc ) {
        switch( tokenarray[i].tokval ) {
        case T_RET:
        case T_IRET:
        case T_IRETD: /* IRETW doesn't exist. Masm bug? */
#if AMD64_SUPPORT
        case T_IRETQ:
#endif
            if ( in_epilogue == FALSE ) {
                if ( ModuleInfo.epiloguemode == PEM_NONE ) {
                    /* if epilogue:none and proc is far,
                     just convert RET to RETF */
                    if ( CurrProc->sym.mem_type == MT_FAR &&
                         tokenarray[i].tokval == T_RET )
                        tokenarray[i].tokval = T_RETF;
                } else {
                    in_epilogue = TRUE;
                    temp = RetInstr( i, tokenarray, Token_Count );
                    in_epilogue = FALSE;
                    return( temp );
                }
            }
        }
    }

#ifdef DEBUG_OUT
    instr = tokenarray[i].string_ptr;
#endif
    CodeInfo.token = tokenarray[i].tokval;
    /* get the instruction's start position in InstrTable[] */
    CodeInfo.pinstr = &InstrTable[IndexFromToken( CodeInfo.token )];
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

    /* get the instruction's arguments.
     * This loop accepts up to 4 arguments if AVXSUPP is on */

    for ( j = 0; j < sizeof(opndx)/sizeof(opndx[0]) && tokenarray[i].token != T_FINAL; j++ ) {
        if ( j ) {
            if ( tokenarray[i].token != T_COMMA )
                break;
            i++;
        }
        DebugMsg1(("ParseLine(%s,%u): calling EvalOperand, i=%u\n", instr, CurrOpnd, i));
        if( EvalOperand( &i, tokenarray, Token_Count, &opndx[j], 0 ) == ERROR ) {
            DebugMsg(("ParseLine(%s,%u): EvalOperand() failed\n", instr, CurrOpnd ));
            return( ERROR );
        }
        switch ( opndx[j].kind ) {
        case EXPR_FLOAT:
            /* v2.06: accept float constants for PUSH */
            if ( j == OPND2 || CodeInfo.token == T_PUSH || CodeInfo.token == T_PUSHD ) {
#if FPIMMEDIATE
                if ( Options.strict_masm_compat == FALSE ) {
                    /* convert to REAL4, unless REAL8 coercion is requested */
                    atofloat( &opndx[j].fvalue, opndx[j].floatstr, opndx[j].mem_type == MT_REAL8 ? 8 : 4, opndx[j].negative, opndx[j].ftype );
                    opndx[j].kind = EXPR_CONST;
                    opndx[j].base_reg = NULL;
                    break;
                }
#endif
                /* Masm message is: real or BCD number not allowed */
                AsmError( FP_INITIALIZER_IGNORED );
                return( ERROR );
            }
            /* fall through */
        case EXPR_EMPTY:
        case EXPR_UNDEF:
            DebugMsg(("ParseLine(%s,%u): unexpected operand kind=%d, error, exit\n", instr, CurrOpnd, opndx[j].kind ));
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
    }
    if ( tokenarray[i].token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }

    for ( CurrOpnd = 0; CurrOpnd < j && CurrOpnd < MAX_OPND; CurrOpnd++ ) {

        Frame_Type = EMPTY;
        SegOverride = NULL; /* segreg prefix is stored in RegOverride */
        CodeInfo.opnd[CurrOpnd].data = 0;
        CodeInfo.opnd[CurrOpnd].InsFixup = NULL;

#if AVXSUPP
        /* if encoding is VEX and destination op is XMM, YMM or memory,
         * the second argument may be stored in the vexregop field.
         */
        if ( CodeInfo.token >= VEX_START &&
            CurrOpnd == OPND2 &&
            ( CodeInfo.opnd[OPND1].type & ( OP_XMM | OP_YMM | OP_M | OP_M256 ) ) ) {
            if ( vex_flags[CodeInfo.token - VEX_START] & VX_NND )
                ;
            else if ( ( vex_flags[CodeInfo.token - VEX_START] & VX_IMM ) &&
                     ( opndx[OPND3].kind == EXPR_CONST ) && ( j > 2 ) )
                ;
            else if ( ( vex_flags[CodeInfo.token - VEX_START] & VX_NMEM ) &&
                     ( CodeInfo.opnd[OPND1].type & OP_M ) )
                ;
            else {
                if ( opndx[OPND2].kind != EXPR_REG ||
                    ( ! ( GetValueSp( opndx[CurrOpnd].base_reg->tokval ) & ( OP_XMM | OP_YMM ) ) ) ) {
                    AsmErr( INVALID_INSTRUCTION_OPERANDS );
                    return( ERROR );
                }
                if ( ( vex_flags[CodeInfo.token - VEX_START] & VX_DST ) &&
                    ( opndx[OPND3].kind == EXPR_CONST ) ) {
                    if ( opndx[OPND1].base_reg ) {
                        /* handle VEX.NDD */
                        CodeInfo.vexregop = opndx[OPND1].base_reg->bytval + 1;
                        memcpy( &opndx[OPND1], &opndx[CurrOpnd], sizeof( opndx[0] ) * 3 );
                        CodeInfo.rm_byte = 0;
                        if ( process_register( &CodeInfo, OPND1, opndx ) == ERROR )
                            return( ERROR );
                    }
                } else {
                    unsigned flags = GetValueSp( opndx[CurrOpnd].base_reg->tokval );
                    if ( ( flags & ( OP_XMM | OP_M128 ) ) &&
                        ( CodeInfo.opnd[OPND1].type & (OP_YMM | OP_M256 ) ) ||
                        ( flags & ( OP_YMM | OP_M256 ) ) &&
                        ( CodeInfo.opnd[OPND1].type & (OP_XMM | OP_M128 ) ) ) {
                        AsmErr( INVALID_INSTRUCTION_OPERANDS );
                        return( ERROR );
                    }
                    CodeInfo.vexregop = opndx[CurrOpnd].base_reg->bytval + 1;
                    memcpy( &opndx[CurrOpnd], &opndx[CurrOpnd+1], sizeof( opndx[0] ) * 2 );
                }
                j--;
            }
        }
#endif
        DebugMsg1(("ParseLine(%s,%u): type/value/mem_type/ofssize=%Xh/%I64Xh/%Xh/%d\n", instr, CurrOpnd, opndx[CurrOpnd].kind, opndx[CurrOpnd].value64, opndx[CurrOpnd].mem_type, opndx[CurrOpnd].Ofssize ));
        switch( opndx[CurrOpnd].kind ) {
        case EXPR_ADDR:
            DebugMsg1(("ParseLine(%s,%u): type ADDRESS\n", instr, CurrOpnd ));
            if ( process_address( &CodeInfo, CurrOpnd, &opndx[CurrOpnd] ) == ERROR )
                return( ERROR );
            break;
        case EXPR_CONST:
            DebugMsg1(("ParseLine(%s,%u): type CONST, opndx.memtype=%Xh\n", instr, CurrOpnd, opndx[CurrOpnd].mem_type));
            if ( process_const( &CodeInfo, CurrOpnd, &opndx[CurrOpnd] ) == ERROR )
                return( ERROR );
            break;
        case EXPR_REG:
            DebugMsg1(("ParseLine(%s,%u): type REG\n", instr, CurrOpnd ));
            if( opndx[CurrOpnd].indirect ) { /* indirect operand ( "[EBX+...]" )? */
                if ( process_address( &CodeInfo, CurrOpnd, &opndx[CurrOpnd] ) == ERROR )
                    return( ERROR );
            } else {
                /* process_register() can't handle 3rd operand */
                if ( CurrOpnd == OPND3 ) {
                    CodeInfo.opnd[OPND3].type = GetValueSp( opndx[CurrOpnd].base_reg->tokval );
                    CodeInfo.opnd[OPND3].data = opndx[CurrOpnd].base_reg->bytval;
                } else if ( process_register( &CodeInfo, CurrOpnd, opndx ) == ERROR )
                    return( ERROR );
            }
            break;
        }
    } /* end for */
#if AVXSUPP
    /* 4 arguments are valid vor AVX only */
    if ( CurrOpnd != j ) {
        for ( ; tokenarray[i].token != T_COMMA; i-- );
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
        return( ERROR );
    }
#endif

    /* for FAR calls/jmps some special handling is required:
     * in the instruction tables, the "far" entries are located BEHIND
     * the "near" entries, that's why it's needed to skip all items
     * until the next "first" item is found.
     */
    if ( CodeInfo.isfar ) {
        if ( CodeInfo.token == T_CALL || CodeInfo.token == T_JMP ) {
            do {
                CodeInfo.pinstr++;
            } while ( CodeInfo.pinstr->first == FALSE );
        }
    }
    /* special handling for string instructions */

    if ( CodeInfo.pinstr->allowed_prefix == AP_REP ||
         CodeInfo.pinstr->allowed_prefix == AP_REPxx ) {
        HandleStringInstructions( &CodeInfo, opndx );
    } else {
        if( CurrOpnd > 1 ) {
            /* v1.96: check if a third argument is ok */
            if ( CurrOpnd > 2 ) {
                do {
                    //if ( CodeInfo.pinstr->opnd_type_3rd != OP3_NONE )
                    if ( opnd_clstab[CodeInfo.pinstr->opclsidx].opnd_type_3rd != OP3_NONE )
                        break;
                    CodeInfo.pinstr++;
                    if ( CodeInfo.pinstr->first == TRUE ) {
                        for ( ; tokenarray[i].token != T_COMMA; i-- );
                        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].tokpos );
                        return( ERROR );
                    }
                } while ( 1 );
            }
            /* v2.06: moved here from process_const() */
            if ( CodeInfo.token == T_IMUL ) {
                /* the 2-operand form with an immediate as second op
                 * is actually a 3-operand form. That's why the rm byte
                 * has to be adjusted. */
                if ( CodeInfo.opnd[OPND3].type == OP_NONE && ( CodeInfo.opnd[OPND2].type & OP_I ) ) {
#if AMD64_SUPPORT
                    CodeInfo.prefix.rex |= ((CodeInfo.prefix.rex & REX_B) ? REX_R : 0 );
#endif
                    CodeInfo.rm_byte = ( CodeInfo.rm_byte & ~BIT_345 ) | ( ( CodeInfo.rm_byte & BIT_012 ) << 3 );
                } else if ( ( CodeInfo.opnd[OPND3].type != OP_NONE ) &&
                           ( CodeInfo.opnd[OPND2].type & OP_I ) &&
                           CodeInfo.opnd[OPND2].InsFixup &&
                           CodeInfo.opnd[OPND2].InsFixup->sym->state == SYM_UNDEFINED )
                    CodeInfo.opnd[OPND2].type = OP_M;
            }
            if( check_size( &CodeInfo, opndx ) == ERROR ) {
                DebugMsg(("ParseLine(%s): check_size() failed, exit\n", instr ));
                return( ERROR );
            }
        }
#if AMD64_SUPPORT
        if ( CodeInfo.Ofssize == USE64 ) {
            //if ( CodeInfo.x86hi_used && ( CodeInfo.x64lo_used || CodeInfo.prefix.rex & 7 ))
            if ( CodeInfo.x86hi_used && CodeInfo.prefix.rex )
                AsmError( INVALID_USAGE_OF_AHBHCHDH );
            switch ( CodeInfo.token ) {
            case T_PUSH:
            case T_POP:
                /* v2.06: REX.W prefix is always 0, because size is either 2 or 8 */
                //if ( CodeInfo.opnd_type[OPND1] & OP_R64 )
                CodeInfo.prefix.rex &= 0x7;
                break;
            case T_CALL:
            case T_JMP:
                /* v2.02: previously rex-prefix was cleared entirely,
                 * but bits 0-2 are needed to make "call rax" and "call r8"
                 * distinguishable!
                 */
                //CodeInfo.prefix.rex = 0;
                CodeInfo.prefix.rex &= 0x7;
                break;
            case T_MOV:
                /* don't use the Wide bit for moves to/from special regs */
                if ( CodeInfo.opnd[OPND1].type & OP_RSPEC || CodeInfo.opnd[OPND2].type & OP_RSPEC )
                    CodeInfo.prefix.rex &= 0x7;
                break;
            }
        }
#endif
    }

    /* now call the code generator */

    return( codegen( &CodeInfo, oldofs ) );
}
