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
* Description:  handles OMF fixup generation
*
****************************************************************************/


#ifndef OMFFIXUP_H
#define OMFFIXUP_H 1
/*
    A lot of this file assumes you're familiar with the Intel OMF book...
*/


typedef struct {
    uint_16 frame;          /* frame number of physical reference       */
    uint_32 offset;         /* offset into reference                    */
} physref;

typedef struct {
    uint_8  frame       :3; /* F_ types from omfpc.h                    */
    uint_8  target      :3; /* T_ types from omfpc.h (only T0-T3)       */
    uint_8  is_secondary:1; /* can write target in a secondary manner   */

    uint_16 frame_datum;    /* datum for different frame methods        */
    uint_16 target_datum;   /* datum for different target methods       */
    int_32  target_offset;  /* offset of target for target method       */
} logref;

typedef union {
    logref  log;
    physref phys;
} logphys;

typedef struct fixup fixup;
typedef struct fixinfo fixinfo;

#include "omfpc.h"
#include "omfio.h"

/*
    The ordering of this enumerated type is depended on in several places.
*/
enum {                      /* method of fixing up location:            */
    FIXO_LO_BYTE,           /* relocate lo byte of offset               */
    FIXO_OFFSET,            /* relocate offset (2 bytes)                */
    FIXO_BASE,              /* relocate base (2 bytes)                  */
    FIXO_POINTER,           /* relocate pointer (base:offset 4 bytes)   */
    FIXO_HI_BYTE,           /* relocate hi byte of offset               */
    FIXO_OFFSET386,         /* relocate offset (4 bytes)                */
    FIXO_POINTER386,        /* relocate pointer (base:offset 6 bytes)   */
    /* don't define more than 8 values without changing bit field */
};

struct fixup {
    fixup   *next;          /* useful for placing in linked list        */
    uint_8  loc_method      :3;
    uint_8  self_relative   :1; /* self or seg relative                 */
    uint_8  loader_resolved :1; /* loader resolved relocation           */
    uint_32 loc_offset;     /* see note below                           */
    logref  lr;             /* logical reference data                   */
};

/*
    fixup.loc_offset has two purposes:  when the fixup is built by FixGetFix,
    this field will contain the value of the "data_rec_offset" 10-bit offset
    into the previous LE/LIDATA.  When the fixup is written, this field must
    contain a similar value (10-bit number which is offset from beginning of
    most recent LE/LIDATA).  However, it is a full 32-bit wide field to allow
    other routines room to play.  i.e., in the WATCOM parser uses the 32-bit
    field as a full offset into the debugging information segments ($$TYPES or
    $$SYMBOLS); this is handled by WAT2CAN0.C.
*/

struct fixinfo {
    /* caller should not touch these fields */
    struct {
        uint_8  method;
        uint_16 datum;
    } trd[ 8 ];
};

enum fixgen_types {
    FIX_GEN_INTEL,
    FIX_GEN_MS386
};
#define FIX_GEN_MAX     11          /* max number of bytes FixGenFix requires */

extern void OmfFixInit( void );
extern void OmfFixFini( void );
/*
    OmfFixInit must be called before any other routines in this modules.
    OmfFixFini free's any memory used by this module, along with all the
    outstanding fixups.
*/

extern fixup *OmfFixNew( void );
/* Allocate memory for a new fixup. */

extern fixup *OmfFixDup( const fixup *fix );
/* Allocate a new fixup, and copy fix into it. */

extern void OmfFixKill( fixup *fix );
/* Free the memory used by fix. */

/********** the following are used for generating fixups *************/

extern size_t OmfFixGenFix( fixup *fix, uint_8 *buf, int type );
/*
    Create the binary representation of fix, for the OMF described by
    fixgen_type type, into buf.  Returns the number of bytes written.
    buf must be at least FIX_GEN_MAX bytes large.
*/

//extern size_t OmfFixGenLRef( logref *log, uint_8 *buf, int type );
//extern size_t OmfFixGenPRef( physref *phys, uint_8 *buf, int type );
extern size_t OmfFixGenRef( logphys *lp, int is_logical, uint_8 *buf, int type );
/* Generate references.  See OmfFixGenFix comment for details. */

#endif
