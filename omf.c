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
* Description:  Write OMF object module.
*
****************************************************************************/


#include "globals.h"
#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <io.h>

#include "memalloc.h"
#include "symbols.h"
#include "autodept.h"
#include "directiv.h"
#include "fatal.h"
#include "mangle.h"
#include "queues.h"
#include "fixup.h"
#include "omf.h"
#include "omffixup.h"
#include "omfname.h"
#include "omfio.h"
#include "omfprs.h"
#include "fastpass.h"

#include "myassert.h"

#define SEPARATE_FIXUPP_16_32

#if FASTPASS
extern bool UseSavedState;
#endif

extern symbol_queue     Tables[];       // tables of definitions
extern obj_rec          *ModendRec;     // Record for Modend (OMF)
extern pobj_state       pobjState;      // object file information

extern uint             segdefidx;      // Number of Segment definition
extern FNAME            *FNames;
extern unsigned         total_segs;

uint_32                 LastCodeBufSize;
static char             **NameArray;

#define JUMP_OFFSET(cmd)    ((cmd)-CMD_POBJ_MIN_CMD)

static pobj_filter      jumpTable[ CMD_MAX_CMD - CMD_POBJ_MIN_CMD + 1 ];

void omf_RegList( const pobj_list *list, size_t len )
/***************************************************/
{

    size_t  i;

    for( i = 0; i < len; ++i ) {
        jumpTable[ JUMP_OFFSET( list[i].command ) ] = list[i].func;
    }
}

void omf_UnRegList( const pobj_list *list, size_t len )
/*****************************************************/
{

    list = list;
    len = len;
}

// call a function in omfgenms.c

void omf_write_record( obj_rec *objr, char kill )
/*******************************************/
{
    /**/myassert( objr != NULL );
    ObjRSeek( objr, 0 );
    jumpTable[ JUMP_OFFSET(objr->command) ] ( objr, &pobjState );
    if( kill ) {
        ObjKillRec( objr );
    }
}

void omf_FlushCurrSeg( void )
/***************************/
{
    unsigned i;
    unsigned size;

    /* this IF is for code segments which contain entirely data up to the
     * point when they are flushed
     * outselect calls flushcurrseg right back
     */
    if( CurrSeg == NULL)
        return;

    /* first check if the last fixup overlaps the end of the ledata record
     * if so, wait until we get more bytes output so that it will not
     */

    if( CurrSeg->seg->e.seginfo->FixupListTail != NULL ) {
        switch( CurrSeg->seg->e.seginfo->FixupListTail->loc_method ) {
        case FIX_LO_BYTE:
        case FIX_HI_BYTE:
            i = 1;
            break;
        case FIX_OFFSET:
        case FIX_BASE:
            i = 2;
            break;
        case FIX_POINTER:
        case FIX_OFFSET386:
            i = 4;
            break;
        case FIX_POINTER386:
            i = 6;
            break;
        default:
            i = 0;
        }
        size = CurrSeg->seg->e.seginfo->current_loc - CurrSeg->seg->e.seginfo->start_loc;
        if( CurrSeg->seg->e.seginfo->FixupListTail->loc_offset + i > size ) {
#ifdef DEBUG_OUT
            if (size > 1024) {
                _asm int 3;
            }
#endif
            return; // can't output the ledata record as is
        }
    }

    omf_write_ledata();
    if (Options.no_comment_data_in_code_records == FALSE)
        omf_OutSelect( FALSE );
}

/* write OMF comment records about data in code */

void omf_OutSelect( bool starts )
/***************************/
{
    obj_rec             *objr;
    unsigned long       curr;

    if( starts ) {
        if( GlobalVars.data_in_code ||
            !GlobalVars.code_seg ||
            Options.output_format != OFORMAT_OMF)
            return;
        GlobalVars.sel_start = GetCurrAddr();
        GlobalVars.data_in_code = TRUE;
    } else {
        if( !GlobalVars.data_in_code || CurrSeg == NULL)
            return;
        GlobalVars.sel_idx = GetSegIdx( &CurrSeg->seg->sym );
        GlobalVars.data_in_code = FALSE;

        if( write_to_file == TRUE ) {
            objr = ObjNewRec( CMD_COMENT );
            objr->d.coment.attr = 0x80;
            objr->d.coment.class = CMT_DISASM_DIRECTIVE;

            ObjAllocData( objr, 11 );
            curr = GetCurrAddr();
            if( (GlobalVars.sel_start > 0xffffUL) || (curr > 0xffffUL) ) {
                ObjPut8( objr, DDIR_SCAN_TABLE_32 );
                ObjPutIndex( objr, GlobalVars.sel_idx );
                ObjPut32( objr, GlobalVars.sel_start );
                ObjPut32( objr, GetCurrAddr() );
            } else {
                ObjPut8( objr, DDIR_SCAN_TABLE );
                ObjPutIndex( objr, GlobalVars.sel_idx );
                ObjPut16( objr, GlobalVars.sel_start );
                ObjPut16( objr, GetCurrAddr() );
            }
            ObjTruncRec( objr );
            omf_write_record( objr, TRUE );
        }
        GlobalVars.sel_idx = 0;
        GlobalVars.sel_start = 0;
    }
}

/* write line number debug info */

void omf_write_linnum( void )
/******************************/
{
    struct linnum_data  *ldata;
    int                 count;
    obj_rec             *objr;
    bool                need_32;

    count = GetLinnumData( &ldata, &need_32 );
    if( count == 0 )
        return;
    if( ldata == NULL ) {
        AsmError( NO_MEMORY );
    } else {
        objr = ObjNewRec( CMD_LINNUM );
        objr->is_32 = need_32;
        objr->d.linnum.num_lines = count;
        objr->d.linnum.lines = ldata;
        objr->d.linnum.d.base.grp_idx = GetGrpIdx( GetGrp( &GetCurrSeg()->sym ) ); // fixme ?
        objr->d.linnum.d.base.seg_idx = CurrSeg->seg->e.seginfo->segrec->d.segdef.idx;
        objr->d.linnum.d.base.frame = 0; // fixme ?

        omf_write_record( objr, TRUE );
    }
}

#ifdef SEPARATE_FIXUPP_16_32

static void omf_split_fixup_list( struct fixup **fl16, struct fixup **fl32 ) {
/**********************************************/
/* divide fixup record list to the 16-bit or 32-bit list of a fixup record */

    struct fixup *fix;
    struct fixup *fix16;
    struct fixup *fix32;

    fix16 = NULL;
    fix32 = NULL;
    for( fix = CurrSeg->seg->e.seginfo->FixupListHead; fix != NULL; fix = fix->next ) {
        switch( fix->loc_method ) {
        case FIX_OFFSET386:
        case FIX_POINTER386:
            if( fix32 == NULL ) {
                *fl32 = fix;
            } else {
                fix32->next = fix;
            }
            fix32 = fix;
            break;
        default:
            if( fix16 == NULL ) {
                *fl16 = fix;
            } else {
                fix16->next = fix;
            }
            fix16 = fix;
            break;
        }
    }
    if( fix32 != NULL ) {
        fix32->next = NULL;
    }
    if( fix16 != NULL ) {
        fix16->next = NULL;
    }
}

#else

static void omf_check_need_32bit( obj_rec *objr ) {
/**********************************************/
/* figure out if we need the 16-bit or 32-bit form of a fixup record */

    struct fixup        *fix;

    fix = objr->d.fixup.fixup;
    for( ;; ) {
        if( fix == NULL )
            break;
        switch( fix->loc_method ) {
        case FIX_OFFSET386:
        case FIX_POINTER386:
            objr->is_32 = 1;
            break;
        }
        if( (unsigned_32)fix->lr.target_offset > 0xffffUL ) {
            objr->is_32 = 1;
        }
        if( objr->is_32 )
            break;
        fix = fix->next;
    }
}

#endif


/* write an LEDATA record, optionally write fixups and line numbers */

void omf_write_ledata( void )
/******************************/
{
    obj_rec         *objr;
    uint_32         size;
#ifdef SEPARATE_FIXUPP_16_32
    struct fixup    *fl16 = NULL;
    struct fixup    *fl32 = NULL;
#endif

    size = CurrSeg->seg->e.seginfo->current_loc - CurrSeg->seg->e.seginfo->start_loc;
    if( size > 0 && write_to_file == TRUE) {
        LastCodeBufSize = size;
        objr = ObjNewRec( CMD_LEDATA );
        ObjAttachData( objr, CurrSeg->seg->e.seginfo->CodeBuffer, size );
        objr->d.ledata.idx = CurrSeg->seg->e.seginfo->segrec->d.segdef.idx;
        objr->d.ledata.offset = CurrSeg->seg->e.seginfo->start_loc;
        if( objr->d.ledata.offset > 0xffffUL )
            objr->is_32 = TRUE;
        omf_write_record( objr, TRUE );

        /* Process Fixup, if any */
        if( CurrSeg->seg->e.seginfo->FixupListHead != NULL ) {
#ifdef SEPARATE_FIXUPP_16_32
            omf_split_fixup_list( &fl16, &fl32 );
            /* Process Fixup, if any */
            if( fl16 != NULL ) {
                objr = ObjNewRec( CMD_FIXUP );
                objr->is_32 = FALSE;
                objr->d.fixup.fixup = fl16;
                omf_write_record( objr, TRUE );
            }
            if( fl32 != NULL ) {
                objr = ObjNewRec( CMD_FIXUP );
                objr->is_32 = TRUE;
                objr->d.fixup.fixup = fl32;
                omf_write_record( objr, TRUE );
            }
#else
            objr = ObjNewRec( CMD_FIXUP );
            objr->d.fixup.fixup = CurrSeg->seg->e.seginfo->FixupListHead;
            omf_check_need_32bit( objr );
            omf_write_record( objr, TRUE );
#endif
            CurrSeg->seg->e.seginfo->FixupListHead = CurrSeg->seg->e.seginfo->FixupListTail = NULL;
        }
        /* add line numbers if debugging info is desired */
        if( Options.line_numbers ) {
            omf_write_linnum();
        }
    }
    CurrSeg->seg->e.seginfo->start_loc = CurrSeg->seg->e.seginfo->current_loc;
}

/////////////////////////////////////////////////////

void omf_write_end_of_pass1( void )
/************************************/
{
    obj_rec     *objr;

    objr = ObjNewRec( CMD_COMENT );
    objr->d.coment.attr = 0x00;
    objr->d.coment.class = CMT_MS_END_PASS_1;
    ObjAttachData( objr, (uint_8 *)"\x001", 1 );
    omf_write_record( objr, TRUE );
}

void omf_write_dosseg( void )
/******************************/
{
    obj_rec     *objr;

    objr = ObjNewRec( CMD_COMENT );
    objr->d.coment.attr = 0x80;
    objr->d.coment.class = CMT_DOSSEG;
    ObjAttachData( objr, (uint_8 *)"", 0 );
    omf_write_record( objr, TRUE );
}

void omf_write_lib( void )
/***************************/
{
    obj_rec             *objr;
    struct dir_node     *curr;
    char                *name;

    for( curr = Tables[TAB_LIB].head; curr; curr = curr->next ) {
        name = curr->sym.name;
        objr = ObjNewRec( CMD_COMENT );
        objr->d.coment.attr = 0x80;
        objr->d.coment.class = CMT_DEFAULT_LIBRARY;
        ObjAttachData( objr, (uint_8 *)name, strlen( name ) );
        omf_write_record( objr, TRUE );
    }
}

void omf_write_export( void )
/******************************/
{
    dir_node    *dir;
    obj_rec     *objr;
    char        *name;
    char        buffer[MAX_LINE_LEN];

    for( dir = Tables[TAB_PROC].head; dir != NULL; dir = dir->next ) {
        if( dir->e.procinfo->export ) {

            objr = ObjNewRec( CMD_COMENT );
            objr->d.coment.attr = 0x00;
            objr->d.coment.class = CMT_DLL_ENTRY;

            Mangle( &dir->sym, buffer );

            ObjAllocData( objr, 4 + strlen( buffer )  );
            ObjPut8( objr, 2 );
            ObjPut8( objr, 0 );             // temporary
            ObjPutName( objr, buffer, strlen( buffer ) );
            ObjPut8( objr, 0 );
            omf_write_record( objr, TRUE );
        }
    }
}

/* write OMF GRPDEF records */

void omf_write_grp( void )
/***************************/
{
    dir_node        *curr;
    dir_node        *segminfo;
    seg_item        *seg;
    obj_rec         *grp;
    unsigned long   line_num;
    char            writeseg;

    DebugMsg(("omf_write_grp enter\n"));
    line_num = LineNumber;

    for( curr = Tables[TAB_GRP].head; curr; curr = curr->next ) {

        grp = ObjNewRec( CMD_GRPDEF );

        grp->d.grpdef.idx = curr->e.grpinfo->idx;

        /* we might need up to 3 bytes for each seg in dgroup and 1 byte for
           the group name index */
        ObjAllocData( grp, 1 + 3 * curr->e.grpinfo->numseg );
        ObjPut8( grp, GetLnameIdx( curr->sym.name ) );

        for( seg = curr->e.grpinfo->seglist; seg; seg = seg->next ) {
            writeseg = TRUE;
            segminfo = (dir_node *)(seg->seg);
            if( ( segminfo->sym.state != SYM_SEG ) || ( segminfo->sym.segment == NULL ) ) {
#if FASTPASS
                /* make a full second pass and report errors there */
                ResetUseSavedState();
#endif
                // LineNumber = curr->line_num;
                // AsmErr( SEG_NOT_DEFINED, segminfo->sym.name );
                // LineNumber = line_num;
            } else {
                ObjPut8( grp, GRP_SEGIDX );
                ObjPutIndex( grp, segminfo->e.seginfo->segrec->d.segdef.idx);
            }
        }
        ObjTruncRec( grp );
        omf_write_record( grp, TRUE );
    }
    DebugMsg(("omf_write_grp exit\n"));
}

/* write segment table */

void omf_write_seg( void )
/***************************/
{
    dir_node    *curr;
    obj_rec     *objr;
    uint        seg_index;

    DebugMsg(("omf_write_seg enter\n"));
    for( seg_index = 1; seg_index <= total_segs; seg_index++ ) {
        /* find segment by index */
        for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
            if( GetSegIdx( curr->sym.segment ) == seg_index ) {
                break;
            }
        }
        if( curr == NULL )
            continue;
        if( curr->sym.state != SYM_SEG ) {
            AsmErr( SEG_NOT_DEFINED, curr->sym.name );
            continue;
        }
        objr = curr->e.seginfo->segrec;
        objr->is_32 = TRUE;
        objr->d.segdef.use_32 = curr->e.seginfo->Use32;
        objr->d.segdef.ovl_name_idx = 1;
        objr->d.segdef.seg_name_idx = GetLnameIdx( curr->sym.name );
        omf_write_record( objr, FALSE );
        if( curr->e.seginfo->segtype == SEGTYPE_CODE ) {
            obj_rec     *rec;

            rec = ObjNewRec( CMD_COMENT );
            rec->d.coment.attr = CMT_TNP;
            rec->d.coment.class = CMT_LINKER_DIRECTIVE;
            ObjAllocData( rec, 3  );
            ObjPut8( rec, LDIR_OPT_FAR_CALLS );
            ObjPutIndex( rec, seg_index );
            omf_write_record( rec, TRUE );
        }
    }
    DebugMsg(("omf_write_seg exit\n"));
}

void omf_write_lnames( void )
/******************************/
{
    obj_rec     *objr;
    uint        total_size = 0;
    char        *lname = NULL;

    objr = ObjNewRec( CMD_LNAMES );
    objr->d.lnames.first_idx = 1;
    objr->d.lnames.num_names = LnamesIdx;
    total_size = GetLnameData( &lname );
    /* fixme: what if lnames are > 1024? */
    if( total_size > 0 ) {
        ObjAttachData( objr, (uint_8 *)lname, total_size );
    }
//    ObjCanFree( objr ); /* tell that data attachment can be freed */
    omf_write_record( objr, TRUE );
}

// write EXTDEF records

void omf_write_extdef( )
/**********************************************/
{
    obj_rec     *objr;
    dir_node    *curr;
    uint        num;
    uint        total_size;
    uint        i;
    char        name[MAX_EXT_LENGTH];
    char        buffer[MAX_LINE_LEN];
    uint        len;

    total_size = 0;
    i = 0;
    num = 0;

    DebugMsg(("omf_write_extdef enter\n"));
    objr = ObjNewRec( CMD_EXTDEF );
    objr->d.extdef.first_idx = 0;

    // first scan the EXTERN/EXTERNDEF items

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if ((curr->sym.comm == 1) || (curr->sym.weak == 1))
            continue;
        DebugMsg(("omf_write_extdef: %s\n", curr->sym.name));
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        if( total_size + len + 2 >= MAX_EXT_LENGTH ) {
            ObjAttachData( objr, (uint_8 *)name, total_size );
            objr->d.extdef.num_names = num;
            omf_write_record( objr, TRUE );
            total_size = 0;
            i = 0;
            num = 0;
            objr = ObjNewRec( CMD_EXTDEF );
            objr->d.extdef.first_idx = curr->sym.idx - 1;
        }
        total_size += len + 2;
        num++;

        name[i] = (char)len;
        i++;
        memcpy( name+i, buffer, len );
        i += len;
        name[i++] = 0;      // for the type index
    }

    // now scan the PROTO items

    for(curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        /* the item must be USED and UNDEFINED */
        if( curr->sym.used && (curr->e.procinfo->defined == 0 )) {
            DebugMsg(("omf_write_extdef: %s\n", curr->sym.name));
            Mangle( &curr->sym, buffer );
            len = strlen( buffer );

            if( total_size + len + 2 >= MAX_EXT_LENGTH ) {
                ObjAttachData( objr, (uint_8 *)name, total_size );
                objr->d.extdef.num_names = num;
                omf_write_record( objr, TRUE );
                total_size = 0;
                i = 0;
                num = 0;
                objr = ObjNewRec( CMD_EXTDEF );
                objr->d.extdef.first_idx = curr->sym.idx - 1;
            }
            total_size += len + 2;
            num++;

            name[i] = (char)len;
            i++;
            memcpy( name+i, buffer, len );
            i += len;
            name[i++] = 0;      // for the type index
        }
    }
    DebugMsg(("omf_write_extdef: attach data, curr=%X, size=%u, last=%u, names=%u, MAX=%u\n", curr, total_size, len, num, MAX_EXT_LENGTH));
    if( num != 0 ) {
        ObjAttachData( objr, (uint_8 *)name, total_size );
        objr->d.extdef.num_names = num;
        omf_write_record( objr, TRUE );
    } else {
        ObjKillRec( objr );
    }
    DebugMsg(("omf_write_extdef exit\n"));
    return;
}

static int opsize( memtype mem_type )
/************************************/
{
    int i = SizeFromMemtype(mem_type, Use32);
    if (i == ERROR)
        return(0);
    return(i);
}

#define THREE_BYTE_MAX ( (1UL << 24) - 1 )

static int get_number_of_bytes_for_size_in_commdef( unsigned long value )
/****************************************************************/
{
    /* The spec allows up to 128 in a one byte size field, but lots
       of software has problems with that, so we'll restrict ourselves
       to 127.
    */
    if( value < 128 ) {
        return( 1 );    /* 1 byte value */
    } else if( value <= USHRT_MAX ) {
        return( 3 );    /* 1 byte flag + 2 byte value */
    } else if( value <= THREE_BYTE_MAX ) {
        return( 4 );    /* 1 byte flag + 3 byte value */
    } else { // if( value <= ULONG_MAX )
        return( 5 );    /* 1 byte flag + 4 byte value */
    }
}

// write OMF COMDEF records

int omf_write_comdef( )
/**********************************************/
{
    obj_rec     *objr;
    dir_node    *curr;
    uint        num;
    uint        total_size;
    uint        varsize;
    uint        symsize;
    uint        start;
    uint        i;
    uint        j;
    uint        len;
    unsigned long value;
    char        *ptr;
    char        buffer[MAX_LINE_LEN];
    char        name[MAX_EXT_LENGTH];

    DebugMsg(("omf_write_comdef enter\n"));
    curr = Tables[TAB_EXT].head;
    while (curr) {
        for(num = 0, total_size = 0, i = 0; curr != NULL ; curr = curr->next ) {
            if (curr->sym.comm == FALSE)
                continue;
            ptr = Mangle( &curr->sym, buffer );
            len = strlen( ptr );
            symsize = 3 + len;
            /* 3 = 1 for string len + 1 for type index + 1 for data type */

            varsize = opsize( curr->sym.mem_type );
            if (varsize == 0)
                varsize = curr->sym.total_size / curr->sym.total_length;
            if( curr->sym.isfar == TRUE ) {
                symsize += get_number_of_bytes_for_size_in_commdef( varsize );
                symsize += get_number_of_bytes_for_size_in_commdef( curr->sym.total_length );
            } else {
                symsize += get_number_of_bytes_for_size_in_commdef( curr->sym.total_length );
            }
            if (total_size + symsize > MAX_EXT_LENGTH)
                break;

            total_size += symsize;

            if (num == 0)
                start = curr->sym.idx - 1;

            num++;

            name[i] = (char)len;
            i++;
            memcpy( name+i, ptr, len );
            i += len;
            name[i++] = 0;      // for the type index

            /* now add the data type & communal length */
            if( curr->sym.isfar == TRUE ) {
                name[i++] = COMDEF_FAR;
            } else {
                name[i++] = COMDEF_NEAR;
            }

            value = curr->sym.total_length;
            varsize = get_number_of_bytes_for_size_in_commdef( value );
            switch( varsize ) {
            case 1:
                break;
            case 3:
                name[i++] = COMDEF_LEAF_2;
                break;
            case 4:
                name[i++] = COMDEF_LEAF_3;
                break;
            case 5:
                name[i++] = COMDEF_LEAF_4;
                break;
            }
            if( varsize > 1 )
                varsize--; /* we already output 1 byte */

            symsize = opsize( curr->sym.mem_type );
            if (symsize == 0)
                symsize = curr->sym.total_size / curr->sym.total_length;
            if( curr->sym.isfar == FALSE ) {
                value *= symsize;
            }

            for( j = 0; j < varsize; j++ ) {
                name[i++] = value % ( UCHAR_MAX + 1 );
                value >>= 8;
            }

            if( curr->sym.isfar == TRUE ) {
                /* mem type always needs <= 1 byte */
                myassert( symsize < UCHAR_MAX );
                name[i++] = symsize;
            }
        } /* end for */

        if( num > 0 ) {
            objr = ObjNewRec( CMD_COMDEF );
            objr->d.comdef.first_idx = start;
            ObjAttachData( objr, (uint_8 *)name, total_size );
            objr->d.comdef.num_names = num;
            omf_write_record( objr, TRUE );
        }
    }
    DebugMsg(("omf_write_comdef exit\n"));
    return( NOT_ERROR );
}

void omf_write_header( void )
/******************************/
{
    obj_rec     *objr;
    unsigned    len;
    char        *name;

    objr = ObjNewRec( CMD_THEADR );
    if( Options.module_name != NULL ) {
        name = Options.module_name;
    } else {
        name = ModuleInfo.srcfile->fullname;
        len = strlen( name );
        name += len;
        for (;name > ModuleInfo.srcfile->fullname && *(name-1) != '/' && *(name-1) != '\\';name--);
    }
    len = strlen( name );
    ObjAllocData( objr, len + 1 );
    ObjPutName( objr, name, len );
    ObjTruncRec( objr );
    omf_write_record( objr, TRUE );
}

int omf_write_autodep( void )
/******************************/
{
    obj_rec         *objr;
    char            buff[2*PATH_MAX + 5];
    unsigned int    len;
    const FNAME     *curr;

    for( curr = FNames; curr != NULL; curr = curr->next ) {
        objr = ObjNewRec( CMD_COMENT );
        objr->d.coment.attr = 0x80;
        objr->d.coment.class = CMT_DEPENDENCY;

        len = strlen(curr->name);
        *((time_t *)buff) = _timet2dos(curr->mtime);
        *(buff + 4) = (unsigned char)len;
        strcpy(buff + 5, curr->name);
        len += 5;

        ObjAttachData( objr, (uint_8 *)buff, len );

        omf_write_record( objr, TRUE );
    }
    // one NULL dependency record must be on the end
    objr = ObjNewRec( CMD_COMENT );
    objr->d.coment.attr = 0x80;
    objr->d.coment.class = CMT_DEPENDENCY;
    ObjAttachData( objr, (uint_8 *)"", 0 );
    omf_write_record( objr, TRUE );
    return NOT_ERROR;
}

void omf_write_alias( void )
/*****************************/
{
    obj_rec             *objr;
    char                *alias;
    char                *subst;
    char                *new;
    char                len1;
    char                len2;
    bool                first = TRUE;
    dir_node            *curr;

    for( curr = Tables[TAB_ALIAS].head; curr; curr = curr->next ) {
        alias = curr->sym.name;
        subst = curr->sym.string_ptr;

        /* output an alias record for this alias */
        len1 = strlen( alias );
        len2 = strlen( subst );

        new = AsmTmpAlloc( len1 + len2 + 2 );

        *new = len1;
        new++;
        strncpy( new, alias, len1 );
        new+=len1;
        *new = len2;
        new++;
        strncpy( new, subst, len2 );
        new -= len1 + 2;

        objr = ObjNewRec( CMD_ALIAS );
        ObjAttachData( objr, (uint_8 *)new, len1+len2+2);
        omf_write_record( objr, TRUE );
        first = FALSE;
    }
}

int omf_write_pub( void )
/**************************/
/* note that procedures with public or export visibility are written out here */
{
    obj_rec             *objr;
    struct pubdef_data  *data;
    uint                i;
    uint                count = 0;
    uint                seg;
    uint                grp;
    char                cmd;
    bool                first = TRUE;
    bool                need32 = FALSE;

    DebugMsg(("omf_write_pub enter\n"));
    while( ( count = GetPublicData( &seg, &grp, &cmd, &NameArray, &data, &need32, first) ) > 0 ) {

        /* create a public record for this segment */

        objr = ObjNewRec( cmd );
        objr->is_32 = need32;
        objr->d.pubdef.base.grp_idx = grp;
        objr->d.pubdef.base.seg_idx = seg;
        objr->d.pubdef.base.frame = 0;
        objr->d.pubdef.num_pubs = count;
        objr->d.pubdef.pubs = data;
//        objr->d.pubdef.free_pubs = TRUE;
        objr->d.pubdef.free_pubs = FALSE; /* don't free the data */
        omf_write_record( objr, TRUE );

        /* free the names table */
        for( i = 0; i < count; i++ ) {
            if( NameArray[i] != NULL ) {
                AsmFree( NameArray[i] );
            }
        }
        AsmFree( NameArray );
        first = FALSE;
    }
    DebugMsg(("omf_write_pub exit\n"));
    return( NOT_ERROR );
}

const char *omf_NameGet( uint_16 hdl )
/********************************/
{
    return( NameArray[hdl] );
}
