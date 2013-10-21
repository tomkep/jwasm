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
* Description:  OMF internal definitions.
*
****************************************************************************/


#ifndef OMFINT_H
#define OMFINT_H    1

#define WriteU16(p,n)   (*(uint_16*)(p) = (uint_16)(n))
#define WriteU32(p,n)   (*(uint_32*)(p) = (uint_32)(n))
//#define WriteS16(p,n)   (*(int_16*)(p) = (int_16)(n))
//#define WriteS32(p,n)   (*(int_32*)(p) = (int_32)(n))

/* v2.11: max object record size reduced to 0xFF0 so the buffer may
 * be located on the stack as a local variable.
 * for 8086, size reduced to 1024. This should only affect GRPDEF
 * records, since all other record types are already ensured not to
 * exceed 1024.
 */
#ifdef __I86__
#define OBJ_BUFFER_SIZE 0x0400
#else
#define OBJ_BUFFER_SIZE 0x0FF0
#endif

/*-----------------------------------------------------*/

/*  A COMENT record needs additional data to be attached. */
struct coment_info {
    uint_8  attr;           /* attribute field from coment record       */
    uint_8  cmt_class;      /* class field from coment record           */
};

struct physref {
    uint_16 frame;          /* frame number of physical reference       */
    uint_32 offset;         /* offset into reference                    */
};

/* logical data for fixup subrecord creation */
struct logref {
    uint_8  frame;          /* see enum frame_types in omfspec.h        */
    uint_8  target;         /* see enum target_types in omfspec.h       */
    uint_8  is_secondary;   /* can write target in a secondary manner   */

    uint_16 frame_datum;    /* datum for different frame methods        */
    uint_16 target_datum;   /* datum for different target methods       */
    int_32  target_offset;  /* offset of target for target method       */
};

#if 0
union logphys {
    struct logref  log;
    struct physref phys;
};
#endif

struct modend_info {
    uint_8  main_module;  /* module is a main module                  */
    uint_8  start_addrs;  /* module has start address                 */
    struct logref ref;    /* a relocatable reference                  */
};

/*
 *   LNAMES, EXTDEFs, and COMDEFs all use this structure. The actual
 *   LNAMES/etc are in the data attached to the record.
 */
struct lnames_info {
    uint_16 first_idx;      /* index of first name in this record       */
    uint_16 num_names;      /* number of names in this record           */
};

struct grpdef_info {
    uint_16 idx;            /* index of this grpdef record              */
};

struct segdef_info {
    uint_16 idx;            /* index for this segment                   */
    uint_8  use_32;         /* use_32 for this segment                  */
    uint_8  align;          /* align field (see omfspec.h)              */
    uint_8  combine;        /* combine field (see omfspec.h)            */
    //uint_8  access_valid;   /* does next field have valid value         */
    //uint_8  access_attr;    /* PharLab Easy OMF access attributes       */
    struct physref abs;     /* (conditional) absolute physical reference*/
    uint_32 seg_length;     /* length of this segment                   */
    uint_16 seg_lname_idx;  /* segment's name lname index               */
    uint_16 class_lname_idx;/* segment's class lname index              */
    uint_16 ovl_lname_idx;  /* segment's overlay lname index            */
};

/* the contents of the LEDATA is attached */
struct ledata_info {
    uint_16 idx;            /* index of segment the data belongs to     */
    uint_32 offset;         /* offset into segment of start of data     */
};

/* base info is supplied by PUBDEF, LINNUM and COMDAT */
struct base_info {
    uint_16 grp_idx;        /* index of the group base                  */
    uint_16 seg_idx;        /* index of the segment                     */
    uint_16 frame;          /* used if grp_idx == 0 && seg_idx == 0     */
};

/* line number info - the items itself are attached */
struct linnum_info {
    struct base_info base;
};

/* public info - the items itself are attached */
struct pubdef_info {
    struct base_info base;
};

#if COMDATSUPP
/* the data the comprises the record should be attached. */
struct comdat_info {
    struct base_info base;
    uint_8      flags;
    uint_8      attributes;
    uint_8      align;
    uint_32     offset; /* may be 16- or 32-bit */
    uint_16     type_idx;
    uint_16     public_name_idx;
};

struct linsym_info {
    uint_8 flags;
    uint_16 public_name_idx;
};
#endif

struct omf_rec {
    uint        length;  /* # of bytes in 'data'    (PRIVATE)  */
    uint        curoff;  /* curr. pos in 'data'     (PRIVATE)  */
    uint_8      *data;   /* data for this record    (PRIVATE)  */
    uint_8      command; /* the command field for this record  */
    uint_8      is_32;   /* a Microsoft 32bit record?          */
    union {
        struct coment_info  coment;  /* CMD_COMENT */
        struct modend_info  modend;  /* CMD_MODEND */
        struct lnames_info  lnames;  /* CMD_LNAMES */
        struct lnames_info  extdef;  /* CMD_EXTDEF */
        struct lnames_info  comdef;  /* CMD_COMDEF */
        struct grpdef_info  grpdef;  /* CMD_GRPDEF */
        struct segdef_info  segdef;  /* CMD_SEGDEF */
        struct ledata_info  ledata;  /* CMD_LEDATA */
        struct linnum_info  linnum;  /* CMD_LINNUM */
        struct pubdef_info  pubdef;  /* CMD_PUBDEF */
#if COMDATSUPP
        struct comdat_info  comdat;  /* CMD_COMDAT */
        struct linsym_info  linsym;  /* CMD_LINSYM */
#endif
#if 0 /* not used */
        struct lnames_info  llnames; /* CMD_LLNAMES */
        struct lnames_info  cextdf;  /* CMD_CEXTDEF */
        struct ledata_info  lidata;  /* CMD_LIDATA  */
#endif
    } d; /* data depending on record type */
};

extern void omf_write_record( const struct omf_rec * );

#define FIX_GEN_MAX     11   /* max number of bytes OmfFixGenFix may generate */

enum fixgen_types {
    FIX_GEN_INTEL,
    FIX_GEN_MS386
};

extern uint OmfFixGenFix( const struct fixup *fix, uint_32 start_loc, uint_8 *buf, enum fixgen_types type );
//extern uint OmfFixGenRef( const union logphys *lp, int is_logical, uint_8 *buf, enum fixgen_types type );
extern uint OmfFixGenLogRef( const struct logref *, uint_8 *buf, enum fixgen_types type );

#endif
