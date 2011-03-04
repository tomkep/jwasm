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
* Description:  code generator operand definitions
*
****************************************************************************/

#ifndef OPERANDS_H
#define OPERANDS_H

/* v1.96: OP_J32 (for far CALL/JMP) has been removed, now used for OP_I64.
 * v2.04: 2 bits freed ( OP_CR, OP_DR and OP_TR replaced by OP_SPECREG )
 * Also OP_SPECREG, OP_SRxx and OP_STxx moved just behind the other
 * register operands.
 */

enum operand_type {
    OP_NONE     = 0,
    OP_R8       = 0x00000001,
    OP_R16      = 0x00000002,
    OP_R32      = 0x00000004,
#if AMD64_SUPPORT
    OP_R64      = 0x00000008,
#endif
    OP_MMX      = 0x00000010,  /* MMx register */
    OP_XMM      = 0x00000020,  /* XMMx register */
//  OP_YMM      = 0x00000040,  /* YMMx register */
    OP_A        = 0x00000080,  /* AL, AX, EAX, RAX registers */
    OP_C        = 0x00000100,  /* CL register */
    OP_D        = 0x00000200,  /* DX register */
    OP_SPECREG  = 0x00000400,  /* CRx, DRx, TRx registers */
    OP_SR86     = 0x00000800,  /* CS, DS, ES, SS registers */
    OP_SR386    = 0x00001000,  /* FS, GS registers */

    OP_ST       = 0x00002000,  /* ST0 register */
    OP_ST_REG   = 0x00004000,  /* ST1-ST7 registers */

    OP_AL       = ( OP_A | OP_R8 ),
    OP_AX       = ( OP_A | OP_R16 ),
    OP_EAX      = ( OP_A | OP_R32 ),
#if AMD64_SUPPORT
    OP_RAX      = ( OP_A | OP_R64 ),
#endif
    OP_CL       = ( OP_C | OP_R8 ),
    OP_DX       = ( OP_D | OP_R16 ),
#if AMD64_SUPPORT
    OP_RGT8     = ( OP_R16 | OP_R32 | OP_R64 ),
    OP_RGT16    = ( OP_R32 | OP_R64 ),
    OP_R        = ( OP_R8 | OP_R16 | OP_R32 | OP_R64 ),
#else
    OP_RGT8     = ( OP_R16 | OP_R32 ),
    OP_RGT16    = ( OP_R32 ),
    OP_R        = ( OP_R8 | OP_R16 | OP_R32 ),
#endif
    // OP_RMX      = ( OP_MMX | OP_XMM ),
    OP_SR       = ( OP_SR86 | OP_SR386 ),
    OP_STI      = ( OP_ST | OP_ST_REG ),

    OP_I8       = 0x00010000,
    OP_I16      = 0x00020000,
    OP_I32      = 0x00040000,
#if AMD64_SUPPORT
    OP_I64      = 0x00080000,
#endif
    OP_I_1      = 0x00100000,
    OP_I_3      = 0x00200000,
    OP_I8_U     = 0x00400000,
    OP_J48      = 0x00800000,

    OP_I        = ( OP_I8 | OP_I_1 | OP_I_3 | OP_I8_U | OP_I16 | OP_I32 ),
    OP_IGE8     = ( OP_I8 | OP_I8_U | OP_I16 | OP_I32 ),
    OP_IGE16    = ( OP_I16 | OP_I32 ),
    //OP_GE_U8    = ( OP_I8_U | OP_I16 | OP_I32 ),

    OP_M8       = 0x01000000,
    OP_M16      = 0x02000000,
    OP_M32      = 0x04000000,
    OP_M64      = 0x08000000,
    OP_M128     = 0x10000000,
    OP_M48      = 0x20000000,
    OP_M80      = 0x40000000,

    OP_MGT8     = ( OP_M16 | OP_M32 | OP_M64 ),
    OP_MGT16    = ( OP_M32 | OP_M64 ),
#if AMD64_SUPPORT
    OP_RM3264   = ( OP_R32 | OP_R64 | OP_M32 | OP_M64 ), /* v2.05: added */
#else
    OP_RM3264   = ( OP_R32 | OP_M32 ),
#endif
    OP_MFPTR    = ( OP_M32 | OP_M48 | OP_M80 ),
    OP_M_ANY    = ( OP_M8 | OP_M16 | OP_M32 | OP_M48 | OP_M64 | OP_M80 | OP_M128 ),
    /* OP_M without M80 will make some instr (i.e. FBSTP [esp]) fail! */
    //OP_M        = ( OP_M8 | OP_M16 | OP_M32 | OP_M64 | OP_M128 ),
    OP_M        = ( OP_M8 | OP_M16 | OP_M32 | OP_M64 | OP_M80 | OP_M128 ),

};

typedef enum operand_type OPNDTYPE;

enum operand3_type { /* this is a 4bit field only! */
    OP3_NONE = 0x00,
    OP3_CL   = 0x01,
    OP3_I8_U = 0x02,
    OP3_XMM0 = 0x03, /* v2.01: introduced with SSE4.1 */
    OP3_HID  = 0x08 /* this is a flag, data in bits 0-2! */
};

#endif
