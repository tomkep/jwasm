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

#if 1 // defined( __WATCOMC__ )

enum operand_type {
    OP_NONE     = 0,
    OP_R8       = 0x00000001,
    OP_R16      = 0x00000002,
    OP_R32      = 0x00000004,
    OP_MMX      = 0x00000008,
    OP_XMM      = 0x00000010,
    OP_A        = 0x00000020,  // AL, AX, EAX registers
    OP_C        = 0x00000040,  // CL register
    OP_D        = 0x00000080,  // DX register

    OP_AL       = ( OP_A | OP_R8 ),
    OP_AX       = ( OP_A | OP_R16 ),
    OP_EAX      = ( OP_A | OP_R32 ),
    OP_CL       = ( OP_C | OP_R8 ),
    OP_DX       = ( OP_D | OP_R16 ),
    OP_R1632    = ( OP_R16 | OP_R32 ),
    OP_R        = ( OP_R8 | OP_R16 | OP_R32 ),
    // OP_RMX      = ( OP_MMX | OP_XMM ),

    OP_I8       = 0x00000100,
    OP_I_1      = 0x00000200,
    OP_I_3      = 0x00000400,
    OP_I8_U     = 0x00000800,
    OP_I16      = 0x00001000,
    OP_I32      = 0x00002000,
    OP_J32      = 0x00004000,
    OP_J48      = 0x00008000,
    OP_I        = ( OP_I8 | OP_I_1 | OP_I_3 | OP_I8_U | OP_I16 | OP_I32 ),
    OP_GE_8     = ( OP_I8 | OP_I8_U | OP_I16 | OP_I32 ),
    OP_GE_16    = ( OP_I16 | OP_I32 ),
    OP_GE_U8    = ( OP_I8_U | OP_I16 | OP_I32 ),

    OP_M_B      = 0x00010000,
    OP_M_W      = 0x00020000,
    OP_M_DW     = 0x00040000,
    OP_M_FW     = 0x00080000,
    OP_M_QW     = 0x00100000,
    OP_M_TB     = 0x00200000,
    OP_M_OW     = 0x00400000,
    OP_M_DFT    = 0x00800000,

    OP_M8       = ( OP_M_B | OP_M_DFT ),
    OP_M16      = ( OP_M_W | OP_M_DFT ),
    OP_M32      = ( OP_M_DW | OP_M_DFT ),
    //OP_M64      = ( OP_M_QW | OP_M_DFT ),
    //OP_M128     = ( OP_M_OW | OP_M_DFT ),

    OP_M        = ( OP_M_B | OP_M_W | OP_M_DW | OP_M_DFT ),
    OP_M_ANY    = ( OP_M_B | OP_M_W | OP_M_DW | OP_M_FW | OP_M_QW | OP_M_TB | OP_M_OW | OP_M_DFT ),
    OP_M8_R8    = ( OP_M_B | OP_M_DFT | OP_R8 ),
    OP_M16_R16  = ( OP_M_W | OP_M_DFT | OP_R16 ),
    OP_M32_R32  = ( OP_M_DW | OP_M_DFT | OP_R32 ),

    OP_CR       = 0x01000000,
    OP_DR       = 0x02000000,
    OP_TR       = 0x04000000,
    OP_SPEC_REG = ( OP_CR | OP_DR | OP_TR ),

    OP_SR86     = 0x08000000,
    OP_SR386    = 0x10000000,
    OP_SR       = ( OP_SR86 | OP_SR386 ),

    OP_ST       = 0x20000000,
    OP_ST_REG   = 0x40000000,
    OP_STI      = ( OP_ST | OP_ST_REG ),

    /* OP_SPECIAL is a flag! */
    /* field specialtype provides further info */
    OP_SPECIAL  = 0x80000000
};

typedef enum operand_type OPNDTYPE;

#else

#define OP_NONE     0
#define OP_R8       0x00000001
#define OP_R16      0x00000002
#define OP_R32      0x00000004
#define OP_MMX      0x00000008
#define OP_XMM      0x00000010
#define OP_A        0x00000020  // AL AX EAX registers
#define OP_C        0x00000040  // CL register
#define OP_D        0x00000080  // DX register

#define OP_AL       ( OP_A | OP_R8 )
#define OP_AX       ( OP_A | OP_R16 )
#define OP_EAX      ( OP_A | OP_R32 )
#define OP_CL       ( OP_C | OP_R8 )
#define OP_DX       ( OP_D | OP_R16 )
#define OP_R1632    ( OP_R16 | OP_R32 )
#define OP_R        ( OP_R8 | OP_R16 | OP_R32 )
#define OP_RMX      ( OP_MMX | OP_XMM )

#define OP_I8       0x00000100
#define OP_I_1      0x00000200
#define OP_I_3      0x00000400
#define OP_I8_U     0x00000800
#define OP_I16      0x00001000
#define OP_I32      0x00002000
#define OP_J32      0x00004000
#define OP_J48      0x00008000
#define OP_I        ( OP_I8 | OP_I_1 | OP_I_3 | OP_I8_U | OP_I16 | OP_I32 )
#define OP_GE_8     ( OP_I8 | OP_I8_U | OP_I16 | OP_I32 )
#define OP_GE_16    ( OP_I16 | OP_I32 )
#define OP_GE_U8    ( OP_I8_U | OP_I16 | OP_I32 )

#define OP_M_B      0x00010000
#define OP_M_W      0x00020000
#define OP_M_DW     0x00040000
#define OP_M_FW     0x00080000
#define OP_M_QW     0x00100000
#define OP_M_TB     0x00200000
#define OP_M_OW     0x00400000
#define OP_M_DFT    0x00800000

#define OP_M8       ( OP_M_B | OP_M_DFT )
#define OP_M16      ( OP_M_W | OP_M_DFT )
#define OP_M32      ( OP_M_DW | OP_M_DFT )
//#define OP_M64      ( OP_M_QW | OP_M_DFT )
//#define OP_M128     ( OP_M_OW | OP_M_DFT )

#define OP_M        ( OP_M_B | OP_M_W | OP_M_DW | OP_M_DFT )
#define OP_M_ANY    ( OP_M_B | OP_M_W | OP_M_DW | OP_M_FW | OP_M_QW | OP_M_TB | OP_M_OW | OP_M_DFT )
#define OP_M8_R8    ( OP_M_B | OP_M_DFT | OP_R8 )
#define OP_M16_R16  ( OP_M_W | OP_M_DFT | OP_R16 )
#define OP_M32_R32  ( OP_M_DW | OP_M_DFT | OP_R32 )

#define OP_CR       0x01000000
#define OP_DR       0x02000000
#define OP_TR       0x04000000
#define OP_SPEC_REG ( OP_CR | OP_DR | OP_TR )

#define OP_SR86     0x08000000
#define OP_SR386    0x10000000
#define OP_SR       ( OP_SR86 | OP_SR386 )

#define OP_ST       0x20000000
#define OP_ST_REG   0x40000000
#define OP_STI      ( OP_ST | OP_ST_REG )

#define OP_SPECIAL  0x80000000

typedef uint_32 OPNDTYPE;

#endif


enum operand3_type {
    OP3_NONE = 0x00,
    OP3_CL   = 0x01,
    OP3_I8_U = 0x02,
    OP3_I    = 0x03,
    OP3_HID  = 0x08
};

/*
 for OP_SPECIAL, the specialtype field will contain further info:
 */

enum special_type {
 OP_REGISTER,
 OP_RES_ID,
 OP_DIRECTIVE,
 OP_ARITHOP,
 OP_TYPE,
 OP_UNARY_OPERATOR
};

#endif
