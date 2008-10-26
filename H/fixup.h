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

enum fixup_types {
        FIX_SEG,       /* 0 */
        FIX_LOBYTE,    /* 1, OMF only */
        FIX_HIBYTE,    /* 2, OMF only */
        FIX_RELOFF8,   /* 3 */
        FIX_RELOFF16,  /* 4 */
        FIX_RELOFF32,  /* 5 */
        FIX_OFF16,     /* 6 */
        FIX_OFF32,     /* 7 */
        FIX_PTR16,     /* 8, OMF only */
        FIX_PTR32,     /* 9, OMF only */
        FIX_OFF32_IMGREL, /* 10, COFF+ELF only */
        FIX_OFF32_SECREL  /* 11, COFF+ELF only */
};

enum fixup_options {
        OPTJ_NONE,
        OPTJ_EXPLICIT,           /* forward reference explicit J... SHORT */
        OPTJ_EXTEND,             /* forward reference JXX (SHORT 8086), can be extend by JMP NEAR */
        OPTJ_JXX,                /* forward reference JXX (SHORT/NEAR 386) */
        OPTJ_CALL                /* forward reference CALL (NEAR or converted FAR to NEAR) */
};

struct asmfixup {
    struct asmfixup         *next1;        /* linked list backpatch */
    struct asmfixup         *next2;        /* linked list relocs */
    unsigned long           offset;        /* symbol's offset */
    unsigned                fixup_loc;     /* location of fixup */
    enum fixup_types        type;
    enum fixup_options      fixup_option;
    unsigned loader_resolved:1;

    union {
        struct {
            int_8           frame;          /* frame specifier (GRP,SEG,...) */
            uint_16         frame_datum;    /* frame_datum of the fixup */
        };
        asm_sym             *segment;       /* symbol's segment if assembly time var */
    };
    struct dir_node         *def_seg;       /* segment the fixup is in */
    struct asm_sym          *sym;
};

extern int_8            Frame;
extern uint_8           Frame_Datum;

extern void             find_frame( struct asm_sym *sym );
extern struct asmfixup  *InsFixups[3];
extern struct asmfixup  *AddFixup( struct asm_sym *sym, enum fixup_types fixup_type, enum fixup_options fixup_option );
extern void             add_frame( void );
extern int              BackPatch( struct asm_sym *sym );
extern void             mark_fixupp( OPNDTYPE determinant, int index );
extern struct fixup     *CreateFixupRec( int index );
extern int              store_fixup( int index );
extern int              MakeFpFixup( struct asm_sym *sym );

#endif
