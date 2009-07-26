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
* Description:  definitions for code generator interface
*
****************************************************************************/

#ifndef _CODEGEN_H_INCLUDED
#define _CODEGEN_H_INCLUDED

#define BIT_012         0x07
#define BIT_345         0x38
#define BIT_67          0xC0
#define NOT_BIT_012     0xF8
#define NOT_BIT_345     0xC7
#define NOT_BIT_67      0x3F

#define MOD_00          0x00
#define MOD_01          0x40
#define MOD_10          0x80
#define MOD_11          0xC0

#define W_BIT           0x01
#define NOT_W_BIT       0xFE

#define ADRSIZ          0x67
#define OPSIZ           0x66
#define OP_WAIT         0x9B
#define EXTENDED_OPCODE 0x0F
#define OP_NOP          0x90

#define S_I_B           0x04
#define D32             0x05  /* direct 32 */
#define D16             0x06  /* direct 16 */

//#define ESP             0x04
//#define EBP             0x05
//#define BP              0x06

#define MEM_BX_SI       0x00
#define MEM_BX_DI       0x01
#define MEM_BP_SI       0x02
#define MEM_BP_DI       0x03

#define SCALE_FACTOR_1  0x00
#define SCALE_FACTOR_2  0x40
#define SCALE_FACTOR_4  0x80
#define SCALE_FACTOR_8  0xC0

#define FPE_MIN         0xD8
#define FPE_MAX         0xDF

#define IS_MEM_TYPE( op, typ ) ( (op) == MT_##typ || (op) == MT_S##typ )

extern ret_code         match_phase_1( struct code_info * );

#endif
