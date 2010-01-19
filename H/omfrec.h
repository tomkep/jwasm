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
* Description:  OMF record definitions.
*
****************************************************************************/


#ifndef OMFREC_H
#define OMFREC_H    1
typedef struct obj_rec      obj_rec;
typedef struct linnum_data  linnum_data;
typedef struct pubdef_data  pubdef_data;

#include "omfpc.h"
#include "omffixup.h"

#define WriteU16(p,n)   (*(uint_16*)(p) = (uint_16)(n))
#define WriteU32(p,n)   (*(uint_32*)(p) = (uint_32)(n))
//#define WriteS16(p,n)   (*(int_16*)(p) = (int_16)(n))
//#define WriteS32(p,n)   (*(int_32*)(p) = (int_32)(n))

#pragma pack( push, 1 )

struct coment_info {
    uint_8  attr;           /* attribute field from coment record       */
    uint_8  class;          /* class field from coment record           */
};
/*
    A COMENT record is created by filling in the above fields, and attaching
    any appropriate data with the Obj...() functions below.
*/


struct modend_info {
    uint_8  main_module :1; /* module is a main module                  */
    uint_8  start_addrs :1; /* module has start address                 */
    uint_8  is_logical  :1; /* is logical or physical reference         */
    logphys ref;            /* a logical or physical reference          */
};
/*
    A MODEND is described completely by the above information; no data
    should be attached to a MODEND.
*/


struct lnames_info {
    uint_16 first_idx;      /* index of first name in this record       */
    uint_16 num_names;      /* number of names in this record           */
};
/*
    LNAMES, EXTDEFs, and COMDEFs all use this structure.  The actual
    LNAMES/etc are in the data attached to the record.
*/


struct grpdef_info {
    uint_16 idx;            /* index of this grpdef record              */
};
/*
    The data that defines the GRPDEF should be attached to this record.
*/


enum segdef_align_values {
    SEGDEF_ALIGN_ABS        = 0,/* absolute segment - no alignment          */
    SEGDEF_ALIGN_BYTE       = 1,/* relocatable seg  - byte aligned          */
    SEGDEF_ALIGN_WORD       = 2,/*                  - word aligned          */
    SEGDEF_ALIGN_PARA       = 3,/*                  - para aligned          */
    SEGDEF_ALIGN_PAGE       = 4,/*                  - page aligned          */
    SEGDEF_ALIGN_DWORD      = 5,/*                  - dword aligned         */
    SEGDEF_ALIGN_4KPAGE     = 6 /*                  - 4k page aligned       */
    /* if more than 16 types then adjust bitfield width in segdef_info */
};

struct segdef_info {
    uint_16 idx;            /* index for this segment                   */
    uint_8      align       :4; /* align field (enum segdef_align_values)   */
    uint_8      combine     :4; /* combine field (values in omfpc.h)        */
    uint_8      use_32      :1; /* use_32 for this segment                  */
    uint_8      access_valid:1; /* does next field have valid value         */
    uint_8      access_attr :2; /* easy omf access attributes (see omfpc.h) */
    physref abs;            /* (conditional) absolute physical reference*/
    uint_32 seg_length;     /* length of this segment                   */
    uint_16 seg_name_idx;   /* name index of this segment               */
    uint_16 class_name_idx; /* class name index of this segment         */
    uint_16 ovl_name_idx;   /* overlay name index of this segment       */
};
/*
    All data necessary for a SEGDEF is defined in the above structure.  No
    data should be attached to the record.
*/


struct ledata_info {
    uint_16 idx;            /* index of segment the data belongs to     */
    uint_32 offset;         /* offset into segment of start of data     */
};
/*
    LEDATAs and LIDATAs both use this structure.  The data that comprises the
    record should be attached.
*/


struct base_info {
    uint_16 grp_idx;        /* index of the group base                  */
    uint_16 seg_idx;        /* index of the segment                     */
    uint_16 frame;          /* valid if grp_idx == 0 && seg_idx == 0    */
};                          /* appears at beginning of appropriate recs */
/*
    This appears at the beginning of LINNUMs and PUBDEFs.  (see the
    appropriate structures.
*/


struct comdat_info {
    struct base_info base;
    uint_8      flags;
    uint_8      attributes;
    uint_8      align;
    uint_32     offset;
    uint_16     type_idx;
    uint_16     public_name_idx;
};
/*
    The data the comprises the record should be attached.
*/


struct fixup_info {
    obj_rec *data_rec;      /* ptr to the data record this belongs to   */
    fixup   *fixup;         /* linked list of processed fixups          */
};
/*
    No data should be attached to these records; all information is in
    the linked list of fixup records.
*/


struct linnum_info {
    union {
        struct base_info base;/* base information                       */
        struct {
            uint_8 flags;       /* for LINSYM records                   */
            uint_16 public_name_idx; /* for LINSYM records              */
        } linsym;
    } d;
    uint_16 num_lines;      /* number of elements in following array    */
    struct linnum_data {
        uint_16 number;     /* line number in source file               */
        uint_32 offset;     /* offset into segment                      */
    } *lines;               /* array of size num_lines                  */
};
/*
    No data should be attached to these records.  All necessary information
    is in the lines array.
*/


struct pubdef_info {
    struct base_info base;  /* base information                         */
    uint_16 num_pubs;       /* number of publics in following array     */
    struct pubdef_data {
        //name_handle name; /* name of this public                      */
        char *name;         /* name of this public                      */
        uint_32 offset;     /* public offset                            */
        union {             /* see PUBDEF.h for more information        */
            uint_16 idx;    /* Intel OMF type index                     */
        } type;
    } *pubs;                /* array of size num_pubs                   */
};
/*
    (This format for PUBDEFs is probably only useful for WOMP.)  No data
    should be attached to this record.  Everything is described by the
    pubs array.
*/


union objrec_info {
    struct coment_info  coment;
    struct modend_info  modend;
    struct lnames_info  lnames;
    struct lnames_info  llnames;
    struct lnames_info  extdef;
    struct lnames_info  comdef;
    struct lnames_info  cextdf;
    struct grpdef_info  grpdef;
    struct segdef_info  segdef;
    struct ledata_info  ledata;
    struct ledata_info  lidata;
    struct base_info    base;
    struct fixup_info   fixup;
    struct linnum_info  linnum;
    struct linnum_info  linsym;
    struct pubdef_info  pubdef;
    struct comdat_info  comdat;
};

struct obj_rec {
    obj_rec     *next;
    uint_16     length;     /* the length field for this record  (PRIVATE)  */
    uint_16     curoff;     /* offset of next read within record (PRIVATE)  */
    uint_8      *data;      /* data for this record              (PRIVATE)  */
    uint_8      command;    /* the command field for this record            */
    uint_8      is_32   : 1;/* is this a Microsoft 32bit record             */
    uint_8      free_data:1;/* should we MemFree( data )??       (PRIVATE)  */
    union objrec_info d;    /* data depending on record type                */
};

#pragma pack( pop )

/*
    Nothing should rely on the data pointing to the same buffer all the time.
    i.e., any routine is allowed to OmfDetachData( objr ) and
        OmfAttachData( objr, ptr ) or OmfAllocData( objr, len )

    Most of the above structure is private to objrec.c (and the macros
    defined below).  See the following functions for instructions about
    manipulating the above structure.
*/


extern void         OmfRecInit( void );
extern void         OmfRecFini( void );
/*
    OmfRecInit must be called before any of the other routines in this
    module.  OmfRecFini free's all existing object records, and any
    memory used by the module.
*/


extern obj_rec      *OmfNewRec( uint_8 command );
/*
    Create an object record of type 'command'.  Does not allocate or attach
    any data to the record, or fill in any of the specific fields for each
    object record.
*/


extern void         OmfKillRec( obj_rec *objr );
/*
    Free's the memory used by an object record.  If the record had data
    allocated for it (OmfAllocData) or OmfCanFree was called on the record,
    then the data is free'd as well.  Records with extra memory (such as
    the fixup chain on FIXUPs, or the line number array on LINNUM) have the
    extra memory free'd as well.
*/


extern void         OmfAllocData( obj_rec *objr, size_t len );
/*
    Allocate a piece of memory of length len, attach it to the object
    record, and set the can_free bit.  This is the most common way to
    fill in data for object records.  Data allocated this way will be
    freed when OmfKillRec/OmfDetachData is called on the record.
*/


extern void         OmfAttachData( obj_rec *objr, uint_8 *data, size_t len );
/*
    This is useful for attaching constants to an object record.  For example,
    when creating the 80386 comment record for pharlap OMF, you could do the
    following:

        objr = OmfNewRec( CMD_COMENT );
        objr->d.coment.attr = 0x80;
        coment->d.coment.class = CMT_EASY_OMF;
        OmfAttachData( coment, "80386", 5 );

    Memory attached this way is not free'd by OmfKillRec or OmfDetachData.
*/


extern void         OmfDetachData( obj_rec *objr );
/*
    Free's the data associated with an object record, but does not free
    the actual object record itself.  Called as part of OmfKillRec().
*/

extern uint_8       *OmfGet( obj_rec *objr, size_t len );
//extern int          OmfEOR( obj_rec *objr );
extern void         OmfPut8( obj_rec *objr, uint_8 byte );
extern void         OmfPut16( obj_rec *objr, uint_16 word );
extern void         OmfPut32( obj_rec *objr, uint_32 dword );
extern void         OmfPutIndex( obj_rec *objr, size_t idx );
//extern void         OmfPutEither( obj_rec *objr, uint_32 val );
extern void         OmfPut( obj_rec *objr, const uint_8 *data, size_t len );
extern void         OmfPutName( obj_rec *objr, const char *name, size_t len );
/*
    Notes:

    The data attached/allocated for an object record is treated like a
    small file.  Initially the "file" pointer points 0 characters into
    the data.  The following functions are used to read and write data
    and modify the "file" pointer.

    OmfGet          return ptr to len bytes, bump ptr by len.
                    The ptr to the entire data record is returned by
                    the call OmfGet( rec, 0 );
    OmfEOR          returns TRUE (non-zero) if pointer is at end of record
    OmfPut8         write uint_8 at pointer, and bump pointer by 1
    OmfPut16        write uint_16 at pointer, and bump pointer by 2
    OmfPut32        write uint_32 at pointer, and bump pointer by 4
    OmfPutEither    if record is_32 then OmfPut32 else OmfPut16
    OmfPutIndex     return the intel index at pointer, bump ptr by 1 or 2
    OmfPut          put len bytes of data at pointer, and bump ptr by len
    OmfPutName      OmfPut8( len ) then OmfPut( name, len )
*/

#endif
