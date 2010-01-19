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
* Description:  fixup related variables and routines
*
****************************************************************************/

#ifndef FIXUP_H
#define FIXUP_H

#include "operands.h"

/* RELOFF8 - RELOFF32 must be consecutive */

enum fixup_types {
        FIX_VOID = 0,       /*  0, fixup is to be ignored */
        FIX_RELOFF8,        /*  1, 1 byte */
        FIX_RELOFF16,       /*  2, 2 byte */
        FIX_RELOFF32,       /*  3, 4 byte */
        FIX_LOBYTE,         /*  4, 1 byte, OMF only */
        FIX_OFF16,          /*  5, 2 byte */
        FIX_OFF32,          /*  6, 4 byte */
#if AMD64_SUPPORT
        FIX_OFF64,          /*  7, 8 byte, COFF64+BIN only */
#endif
        FIX_SEG = 8,        /*  8, 2 byte */
        FIX_PTR16,          /*  9, 4 byte, OMF only */
        FIX_PTR32,          /* 10, 6 byte, OMF only */
        FIX_HIBYTE,         /* 11, 1 byte, OMF only */
        FIX_OFF32_IMGREL,   /* 12, 4 byte, COFF+ELF only */
        FIX_OFF32_SECREL,   /* 13, 4 byte, COFF+ELF only */
};

#define COFF_DISALLOWED 0x0E12
#define  ELF_DISALLOWED 0x0F00
#define  BIN_DISALLOWED 0x0000

/* fixups are also used for backpatching of forward references in pass one.
 * the instructions which depend on the distance are CALL, JMP, PUSH <imm>.
 * OPTJ_EXPLICIT: JMP SHORT <label> or Jcc SHORT <label>, size cannot change
 * OPTJ_EXTEND:   Jcc <label> for cpu < 80386, size may change (2 -> 5/7 or 8/10)
 * OPTJ_JXX:      Jcc <label> for cpu >= 80386, size may change (2 -> 5 )
 * OPTJ_CALL:     call <label>, may become push cs, call NEAR or call FAR
 */

enum fixup_options {
        OPTJ_NONE,      /* normal jump, PUSH */
        OPTJ_EXPLICIT,
        OPTJ_EXTEND,
        OPTJ_JXX,
        OPTJ_CALL
};

struct asmfixup {
    struct asmfixup         *nextbp;       /* PASS 1: linked list backpatch */
    struct asmfixup         *nextrlc;      /* PASS >1: linked list relocs */
    uint_32                 offset;        /* symbol's offset */
    uint_32                 fixup_loc;     /* location of fixup */
    enum fixup_types        type;
    enum fixup_options      option;
#if AMD64_SUPPORT
    /* the IP relative addressing needs to know where the instruction ends.
     * the result <end of instruction> - <fixup location> is stored here.
     */
    uint_8                  addbytes;
#endif
    unsigned char loader_resolved:1;        /* operator LROFFSET */

    union {
        struct {
            int_8           frame;          /* frame specifier (GRP,SEG,...) */
            uint_16         frame_datum;    /* additional data, usually index */
        };
        asm_sym             *segment;       /* symbol's segment if assembly time var */
    };
    struct dir_node         *def_seg;       /* segment the fixup is in */
    struct asm_sym          *sym;
};

extern struct asmfixup  *AddFixup( struct asm_sym *sym, enum fixup_types fixup_type, enum fixup_options fixup_option );
extern ret_code         store_fixup( struct asmfixup *, int_32 * );

extern ret_code         BackPatch( struct asm_sym *sym );

#endif
