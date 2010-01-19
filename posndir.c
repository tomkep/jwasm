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
* Description:  ALIGN, EVEN, ORG directives
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "expreval.h"
#include "types.h"
#include "listing.h"
#include "posndir.h"
#include "fastpass.h"

#include "myassert.h"

static const uint_8 NopList16[] = {
    3,                  /* objlen of first NOP pattern */
    0x2E, 0x8b, 0xc0,   /* MOV AX,AX */
    0x89, 0xc0,         /* MOV AX,AX */
    0x90                /* NOP */
};

/* 32bit alignment fillers.
 For 5 bytes, Masm uses "add eax,dword ptr 0",
 which modifies the flags!
 */

static const uint_8 NopList32[] = {
    7,
    0x8d,0xa4,0x24,0,0,0,0,         // lea     esp,[esp+00000000]
    0x8d,0x80,0,0,0,0,              // lea     eax,[eax+00000000]
#if 0
    0x8d,0x40,0x00,                 // lea     eax,[eax+00]
    0x8b,0xc9,                      // mov     ecx,ecx
#else
    0x2e,0x8d,0x44,0x20,0x00,       // lea     eax,cs:[eax+no_index_reg+00H]
#endif
    0x8d,0x44,0x20,0x00,            // lea     eax,[eax+no_index_reg+00H]
    0x8d,0x40,0x00,                 // lea     eax,[eax+00H]
    0x8b,0xff,                      // mov     edi,edi
    0x90                            // nop
};

#if AMD64_SUPPORT
static const uint_8 NopList64[] = {
    7,
    0x0f,0x1f,0x80,0,0,0,0,         // nop dword ptr [rax+0]
    0x66,0x0f,0x1f,0x44,0,0,        // nop word ptr [rax+rax]
    0x0f,0x1f,0x44,0,0,             // nop dword ptr [rax+rax]
    0x0f,0x1f,0x40,0,               // nop dword ptr [rax]
    0x0f,0x1f,0,                    // nop dword ptr [rax]
    0x66,0x90,                      // xchg ax,ax
    0x90,                           // nop
};

/* just use the 32bit nops for 64bit */
static const uint_8 * const NopLists[] = { NopList16, NopList32, NopList64 };
#else
static const uint_8 * const NopLists[] = { NopList16, NopList32 };
#endif

ret_code OrgDirective( int i )
/****************************/
{
    //struct asm_sym  *sym;
    //int_32          value = 0;
    expr_list opndx;

    i++;
    if ( ( ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ) ) )
        return( ERROR );
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if ( CurrStruct ) {
        if ( opndx.kind == EXPR_CONST )
            return( SetStructCurrentOffset( opndx.value ) );
    } else {
#if FASTPASS
        if ( StoreState == FALSE && Parse_Pass == PASS_1 )
            SaveState();
#endif
        if ( opndx.kind == EXPR_CONST )
            return( SetCurrOffset( opndx.value, FALSE, FALSE ) );
        else if ( opndx.kind == EXPR_ADDR && opndx.indirect == FALSE )
            return( SetCurrOffset( opndx.sym->offset + opndx.value, FALSE, FALSE ) );
    }
    AsmError( EXPECTING_NUMBER );
    return( ERROR );
}

static void fill_in_objfile_space( uint size )
/********************************************/
{
    int i;
    int nop_type;

    /* emit
     - nothing ... for BSS
     - x'00'   ... for DATA
     - nops    ... for CODE
     */

    if( CurrSeg->e.seginfo->segtype != SEGTYPE_CODE ) {

        if (CurrSeg->e.seginfo->segtype == SEGTYPE_BSS ||
            CurrSeg->e.seginfo->segtype == SEGTYPE_ABS ) {

            SetCurrOffset( size, TRUE, TRUE );

        } else {
            /* just output nulls */
            FillDataBytes( 0x00, size );
        }

    } else {
        /* output appropriate NOP type instructions to fill in the gap */

        while( size > NopLists[ ModuleInfo.Ofssize ][0] ) {
            for( i = 1; i <= NopLists[ ModuleInfo.Ofssize ][0]; i++ ) {
                OutputByte( NopLists[ ModuleInfo.Ofssize ][i] );
            }
            size -= NopLists[ ModuleInfo.Ofssize ][0];
        }
        if( size == 0 ) return;

        i=1; /* here i is the index into the NOP table */
        for( nop_type = NopLists[ ModuleInfo.Ofssize ][0]; nop_type > size ; nop_type-- ) {
            i+=nop_type;
        }
        /* i now is the index of the 1st part of the NOP that we want */
        for( ; nop_type > 0; nop_type--,i++ ) {
            OutputByte( NopLists[ ModuleInfo.Ofssize ][i] );
        }
    }
}

// align current offset to value ( alignment is 2^value )

void AlignCurrOffset( int value )
/*******************************/
{
    int seg_align;
    int alignment = (1 << value);
    unsigned int CurrAddr;

    CurrAddr = GetCurrOffset();
    seg_align = CurrAddr % alignment;
    if( seg_align ) {
        alignment -= seg_align;
        fill_in_objfile_space( alignment );
    }
}

ret_code AlignDirective( int directive, int i )
/********************************************/
{
    int_32 align_val;
    int seg_align;
    expr_list opndx;
    unsigned int CurrAddr;

    DebugMsg(("AlignDirective enter\n"));

    i++;
    switch( directive ) {
    case T_ALIGN:
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if ( opndx.kind == EXPR_CONST ) {
            int power;
            align_val = opndx.value;
            /* check that the parm is a power of 2 */
            for( power = 1; power < align_val; power <<= 1 );
            if( power != align_val ) {
                AsmError( POWER_OF_2 );
                return( ERROR );
            }
        } else if ( opndx.kind == EXPR_EMPTY ) {
            align_val = GetCurrSegAlign();
        } else {
            AsmError( EXPECTING_NUMBER );
            return( ERROR );
        }
        break;
    case T_EVEN:
        align_val = 2;
        break;
    }
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    /* ALIGN/EVEN inside a STRUCT definition? */
    if ( CurrStruct )
        return( AlignInStruct( align_val ));

#if FASTPASS
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
        SaveState();
    }
#endif
    seg_align = GetCurrSegAlign(); // # of bytes
    if( seg_align <= 0 ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }
    if( align_val > seg_align ) {
        if ( Parse_Pass == PASS_1 )
            AsmWarn( 1, ALIGN_TOO_HIGH );
        //return( ERROR ); /* v2.0: don't exit */
    }
    /* find out how many bytes past alignment we are & add the remainder */
    //store temp. value
    CurrAddr = GetCurrOffset();
    seg_align = CurrAddr % align_val;
    if( seg_align ) {
        align_val -= seg_align;
        fill_in_objfile_space( align_val );
    }
    if ( FileInfo.file[LST] ) {
        LstWrite( LSTTYPE_LIDATA, CurrAddr, NULL );
    }
    DebugMsg(("AlignDirective exit\n"));
    return( NOT_ERROR );
}
