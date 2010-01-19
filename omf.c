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
* Description:  handle OMF output format.
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "directiv.h"
#include "segment.h"
#include "mangle.h"
#include "queues.h"
#include "fixup.h"
#include "omf.h"
#include "omfgenms.h"
#include "fastpass.h"
#include "myassert.h"
#include "tokenize.h" /* needed because of StringBufferEnd usage */
#include "input.h"

#define SEPARATE_FIXUPP_16_32
#define TRUNCATE 1

#if TRUNCATE
#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)
#include "unistd.h"
#else
#include "io.h"
#endif
#endif

#define OmfTruncRec(objr)       (void)( (objr)->length = (objr)->curoff )

extern void cv_write_debug_tables( dir_node *, dir_node *);

extern symbol_queue     Tables[];       // tables of definitions
extern obj_rec          *ModendRec;     // Record for Modend (OMF)
extern struct format_options formatoptions[];
extern qdesc            LinnumQueue;    // queue of line_num_info items

extern uint             segdefidx;      // Number of Segment definition
extern FNAME            *FNames;
extern uint             cnt_fnames;
extern uint             LnamesIdx;      // Number of LNAMES definition


extern const char szNull[];

OBJ_WFILE               *file_out;
uint_32                 LastCodeBufSize;

static unsigned long    seg_pos;        // file pos of SEGDEF record(s)
static unsigned long    public_pos;     // file pos of PUBDEF record(s)
static unsigned long    end_of_header;  // file pos of "end of header"

static const char szCVSymbols[]  = { "$$SYMBOLS"};
static const char szCVTypes[]    = { "$$TYPES"};
static const char szCVSymClass[] = { "DEBSYM" };
static const char szCVTypClass[] = { "DEBTYP" };

enum {
    TIME_SEC_B  = 0,
    TIME_SEC_F  = 0x001f,
    TIME_MIN_B  = 5,
    TIME_MIN_F  = 0x07e0,
    TIME_HOUR_B = 11,
    TIME_HOUR_F = 0xf800
};

enum {
    DATE_DAY_B  = 0,
    DATE_DAY_F  = 0x001f,
    DATE_MON_B  = 5,
    DATE_MON_F  = 0x01e0,
    DATE_YEAR_B = 9,
    DATE_YEAR_F = 0xfe00
};

typedef union {
    struct {
        unsigned short time;
        unsigned short date;
    } dos;
    time_t timet;
} DOS_DATETIME;

static time_t timet2dostime(time_t x)
/***********************************/
{
    struct tm *    ltime;
    DOS_DATETIME   dt;

    ltime = localtime( &x );
    dt.dos.date = (( ltime->tm_year - 80 ) << DATE_YEAR_B )
             | (( ltime->tm_mon + 1 ) << DATE_MON_B )
             | (( ltime->tm_mday ) << DATE_DAY_B );
    dt.dos.time = (( ltime->tm_hour ) << TIME_HOUR_B )
             | (( ltime->tm_min ) << TIME_MIN_B )
             | (( ltime->tm_sec / 2 ) << TIME_SEC_B );
    return dt.timet;
}

void omf_init( module_info *ModuleInfo )
/*************************/
{
    omf_GenMSInit();
    file_out = OmfWriteOpen( FileInfo.file[OBJ] );
    return;
}

void omf_fini( void )
/*******************/
{
    DebugMsg(("omf_fini enter\n"));
    if ( file_out ) {
        OmfWriteClose( file_out );
        file_out = NULL;
    }
    omf_GenMSFini();
}

/* write OMF comment records about data in code */

void omf_OutSelect( bool starts )
/*******************************/
{
    obj_rec             *objr;
    uint_32             currofs;

    if( starts ) {
        if( GlobalVars.data_in_code || !GlobalVars.code_seg )
            return;
        GlobalVars.sel_start = GetCurrOffset();
        GlobalVars.data_in_code = TRUE;
    } else {
        if( !GlobalVars.data_in_code || CurrSeg == NULL)
            return;
        GlobalVars.sel_idx = GetSegIdx( &CurrSeg->sym );

        if( write_to_file == TRUE ) {
            objr = OmfNewRec( CMD_COMENT );
            objr->d.coment.attr = CMT_TNP;
            objr->d.coment.class = CMT_DISASM_DIRECTIVE;

            OmfAllocData( objr, 11 );
            currofs = GetCurrOffset();
            if( (GlobalVars.sel_start > 0xffffUL) || (currofs > 0xffffUL) ) {
                OmfPut8( objr, DDIR_SCAN_TABLE_32 );
                OmfPutIndex( objr, GlobalVars.sel_idx );
                OmfPut32( objr, GlobalVars.sel_start );
                OmfPut32( objr, currofs );
            } else {
                OmfPut8( objr, DDIR_SCAN_TABLE );
                OmfPutIndex( objr, GlobalVars.sel_idx );
                OmfPut16( objr, GlobalVars.sel_start );
                OmfPut16( objr, currofs );
            }
            OmfTruncRec( objr );
            omf_write_record( objr, TRUE );
        }
        GlobalVars.data_in_code = FALSE;
        //GlobalVars.sel_idx = 0;
        //GlobalVars.sel_start = 0;
    }
}

/* get line numbers for OMF.*/

static int GetLinnumData( struct linnum_data **ldata, bool *need32 )
/******************************************************************/
{
    struct line_num_info    *node;
    struct line_num_info    *next;
    int                     count, i;

    for (count = 0, node = LinnumQueue.head; node; count++, node = node->next );
    if( count == 0 )
        return( count );
    *need32 = FALSE;
    *ldata = AsmAlloc( count * sizeof( struct linnum_data ) );
    for( i = 0, node = LinnumQueue.head; i < count; i++, node = next ) {
        next = node->next;
        (*ldata)[i].number = node->number;
        (*ldata)[i].offset = node->offset;
        if( node->offset > 0xffffUL ) {
            *need32 = TRUE;
        }
        AsmFree( node );
    }
    LinnumQueue.head = NULL;
    return( count );
}

/* write line number debug info */

void omf_write_linnum( void )
/***************************/
{
    struct linnum_data  *ldata;
    int                 count;
    obj_rec             *objr;
    bool                need_32;

    count = GetLinnumData( &ldata, &need_32 );
    if( count == 0 )
        return;
    if( ldata ) { /* always true */
        objr = OmfNewRec( CMD_LINNUM );
        objr->is_32 = need_32;
        objr->d.linnum.num_lines = count;
        objr->d.linnum.lines = ldata;
        objr->d.linnum.d.base.grp_idx = GetGrpIdx( GetGrp( &CurrSeg->sym ) ); // fixme ?
        objr->d.linnum.d.base.seg_idx = CurrSeg->e.seginfo->segrec->d.segdef.idx;
        objr->d.linnum.d.base.frame = 0; // fixme ?

        omf_write_record( objr, TRUE );
    }
}

#ifdef SEPARATE_FIXUPP_16_32

static void omf_split_fixup_list( dir_node *seg, struct fixup **fl16, struct fixup **fl32 )
/*****************************************************************************************/
{
/* divide fixup record list to the 16-bit or 32-bit list of a fixup record */

    struct fixup *fix;
    struct fixup *fix16;
    struct fixup *fix32;
#ifdef DEBUG_OUT
    int cntfix16 = 0;
    int cntfix32 = 0;
#endif

    fix16 = NULL;
    fix32 = NULL;
    for( fix = seg->e.seginfo->FixupListHead; fix != NULL; fix = fix->next ) {
        switch( fix->loc_method ) {
        case FIXO_OFFSET386:
        case FIXO_POINTER386:
#ifdef DEBUG_OUT
            cntfix32++;
#endif
            if( fix32 == NULL ) {
                *fl32 = fix;
            } else {
                fix32->next = fix;
            }
            fix32 = fix;
            break;
        default:
#ifdef DEBUG_OUT
            cntfix16++;
#endif
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
        DebugMsg(("omf_split_fixup_list: %u 32-bit fixups\n", cntfix32 ));
    }
    if( fix16 != NULL ) {
        fix16->next = NULL;
        DebugMsg(("omf_split_fixup_list: %u 16-bit fixups\n", cntfix16 ));
    }
}

#else

static void omf_check_need_32bit( obj_rec *objr )
/***********************************************/
{
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
        if( (uint_32)fix->lr.target_offset > 0xffffUL ) {
            objr->is_32 = 1;
        }
        if( objr->is_32 )
            break;
        fix = fix->next;
    }
}

#endif


/* write an LEDATA record, optionally write fixups */

void omf_write_ledata( dir_node *seg )
/************************************/
{
    obj_rec         *objr;
    uint_32         size;
#ifdef SEPARATE_FIXUPP_16_32
    struct fixup    *fl16 = NULL;
    struct fixup    *fl32 = NULL;
#endif

    size = seg->e.seginfo->current_loc - seg->e.seginfo->start_loc;
    DebugMsg(( "omf_write_ledata enter, start ofs=%X, size=%X\n",
              seg->e.seginfo->start_loc, size ));
    if( size > 0 && write_to_file == TRUE ) {
        LastCodeBufSize = size;
        objr = OmfNewRec( CMD_LEDATA );
        OmfAttachData( objr, seg->e.seginfo->CodeBuffer, size );
        objr->d.ledata.idx = seg->e.seginfo->segrec->d.segdef.idx;
        objr->d.ledata.offset = seg->e.seginfo->start_loc;
        if( objr->d.ledata.offset > 0xffffUL )
            objr->is_32 = TRUE;
        omf_write_record( objr, TRUE );

        /* Process Fixup, if any */
        if( seg->e.seginfo->FixupListHead != NULL ) {
            DebugMsg(( "omf_write_ledata: write fixups\n" ));
#ifdef SEPARATE_FIXUPP_16_32
            omf_split_fixup_list( seg, &fl16, &fl32 );
            /* Process Fixup, if any */
            if( fl16 != NULL ) {
                objr = OmfNewRec( CMD_FIXUP );
                objr->is_32 = FALSE;
                objr->d.fixup.fixup = fl16;
                omf_write_record( objr, TRUE );
            }
            if( fl32 != NULL ) {
                objr = OmfNewRec( CMD_FIXUP );
                objr->is_32 = TRUE;
                objr->d.fixup.fixup = fl32;
                omf_write_record( objr, TRUE );
            }
#else
            objr = OmfNewRec( CMD_FIXUP );
            objr->d.fixup.fixup = seg->e.seginfo->FixupListHead;
            omf_check_need_32bit( objr );
            omf_write_record( objr, TRUE );
#endif
            seg->e.seginfo->FixupListHead = seg->e.seginfo->FixupListTail = NULL;
        }
    }
    seg->e.seginfo->start_loc = seg->e.seginfo->current_loc;
}

void omf_FlushCurrSeg( void )
/***************************/
{
    //unsigned i;
    //unsigned size;

    DebugMsg(( "omf_FlushCurrSeg enter\n" ));

#if 0 /* v2: check is obsolete */
    /* check if the last fixup overlaps the end of the ledata record
     * if so, wait until we get more bytes output so that it will not
     */
    if( CurrSeg->e.seginfo->FixupListTail != NULL ) {
        switch( CurrSeg->e.seginfo->FixupListTail->loc_method ) {
        case FIXO_LO_BYTE:
        case FIXO_HI_BYTE:
            i = 1;
            break;
        case FIXO_OFFSET:
        case FIXO_BASE:
            i = 2;
            break;
        case FIXO_POINTER:
        case FIXO_OFFSET386:
            i = 4;
            break;
        case FIXO_POINTER386:
            i = 6;
            break;
        default:
            i = 0;
        }
        size = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
        if( CurrSeg->e.seginfo->FixupListTail->loc_offset + i > size ) {
            DebugMsg(( "omf_FlushCurrSeg: output has to wait, curr size=%u, i=%u, loc_offset=%X\n",
                      size, i, CurrSeg->e.seginfo->FixupListTail->loc_offset ));
            return(0); // can't output the ledata record as is
        }
    }
#endif

    omf_write_ledata( CurrSeg );
    /* add line numbers if debugging info is desired */
    if( write_to_file && Options.line_numbers ) {
        omf_write_linnum();
    }
    if ( Options.no_comment_data_in_code_records == FALSE )
        omf_OutSelect( FALSE );
    return;
}

/////////////////////////////////////////////////////

void omf_end_of_pass1( void )
/***************************/
{
    obj_rec     *objr;

    objr = OmfNewRec( CMD_COMENT );
    objr->d.coment.attr = 0x00;
    objr->d.coment.class = CMT_MS_END_PASS_1;
    OmfAttachData( objr, (uint_8 *)"\x001", 1 );
    omf_write_record( objr, TRUE );
    end_of_header = ftell( file_out->file );
}

void omf_set_filepos( void )
/**************************/
{
    DebugMsg(( "omf_set_filepos: reset file pos to %X\n", end_of_header ));
    fseek( file_out->file, end_of_header, SEEK_SET );
}

void omf_write_dosseg( void )
/***************************/
{
    obj_rec     *objr;

    objr = OmfNewRec( CMD_COMENT );
    objr->d.coment.attr = CMT_TNP;
    objr->d.coment.class = CMT_DOSSEG;
    OmfAttachData( objr, (uint_8 *)"", 0 );
    omf_write_record( objr, TRUE );
}

void omf_write_lib( void )
/************************/
{
    obj_rec             *objr;
    struct dir_node     *curr;
    char                *name;

    DebugMsg(("omf_write_lib() enter\n"));
    for( curr = Tables[TAB_LIB].head; curr; curr = curr->next ) {
        name = curr->sym.name;
        objr = OmfNewRec( CMD_COMENT );
        objr->d.coment.attr = CMT_TNP;
        objr->d.coment.class = CMT_DEFAULT_LIBRARY;
        OmfAttachData( objr, (uint_8 *)name, strlen( name ) );
        omf_write_record( objr, TRUE );
    }
    DebugMsg(("omf_write_lib() exit\n"));
}

void omf_write_export( void )
/***************************/
{
    uint_8      parmcnt;
    dir_node    *dir;
    dir_node    *parm;
    obj_rec     *objr;
    //char        *name;
    char        buffer[MAX_ID_LEN+8+1]; /* leave space for decoration */

    for( dir = Tables[TAB_PROC].head; dir != NULL; dir = dir->next ) {
        if( dir->e.procinfo->export ) {

            objr = OmfNewRec( CMD_COMENT );
            objr->d.coment.attr = 0x00;
            objr->d.coment.class = CMT_DLL_ENTRY;

            if ( Options.no_export_decoration == FALSE )
                Mangle( &dir->sym, buffer );
            else
                strcpy( buffer, dir->sym.name );
            OmfAllocData( objr, 4 + strlen( buffer )  );
            OmfPut8( objr, 2 ); /* subtype 02 is EXPDEF */
            /* write the "Exported Flag" byte:
             * bits 0-4: parameter count
             * bit 5: no data (entry doesn't use initialized data )
             * bit 6: resident (name should be kept resident)
             * bit 7: ordinal ( if 1, 2 byte index must follow name)
             */
            for ( parm = dir->e.procinfo->paralist, parmcnt = 0; parm; parm = parm->nextparam, parmcnt++ );
            parmcnt &= 0x1F; /* ensure bits 5-7 are still 0 */
            OmfPut8( objr, parmcnt ); /* v2.01: changed from fix 0x00 */
            OmfPutName( objr, buffer, strlen( buffer ) );
            OmfPut8( objr, 0 );
            omf_write_record( objr, TRUE );
        }
    }
}

/* write OMF GRPDEF records */

void omf_write_grp( void )
/************************/
{
    dir_node        *curr;
    dir_node        *segminfo;
    seg_item        *seg;
    obj_rec         *grp;
    //uint_32         line_num;
    //char            writeseg;

    DebugMsg(("omf_write_grp enter\n"));
    //line_num = LineNumber;

    for( curr = Tables[TAB_GRP].head; curr; curr = curr->next ) {

        grp = OmfNewRec( CMD_GRPDEF );

        grp->d.grpdef.idx = curr->e.grpinfo->idx;

        /* we might need:
         * - 1 or 2 bytes for the group name index
         * - 2 or 3 bytes for each segment in the group
         */
        OmfAllocData( grp, 2 + 3 * curr->e.grpinfo->numseg );
        /* v2.01: the LName index of the group may be > 0xff */
        //OmfPut8( grp, GetLnameIdx( curr->sym.name ) );
        OmfPutIndex( grp, GetLnameIdx( curr->sym.name ) );

        for( seg = curr->e.grpinfo->seglist; seg; seg = seg->next ) {
            //writeseg = TRUE;
            segminfo = (dir_node *)(seg->seg);
            if( ( segminfo->sym.state != SYM_SEG ) || ( segminfo->sym.segment == NULL ) ) {
#if FASTPASS
                /* make a full second pass and report errors there */
                SkipSavedState();
#endif
                // LineNumber = curr->line_num;
                // AsmErr( SEG_NOT_DEFINED, segminfo->sym.name );
                // LineNumber = line_num;
            } else {
                OmfPut8( grp, GRP_SEGIDX );
                OmfPutIndex( grp, segminfo->e.seginfo->segrec->d.segdef.idx );
            }
        }
        OmfTruncRec( grp );
        omf_write_record( grp, TRUE );
    }
    DebugMsg(("omf_write_grp exit\n"));
}

/* write segment table.
 * This is done after pass 1.
 * There might exist entries of undefined segments in
 * the segment list!
 */

void omf_write_seg( bool initial )
/********************************/
{
    dir_node    *curr;
    obj_rec     *objr;
    uint        seg_index;

    DebugMsg(("omf_write_seg enter\n"));

    /* in pass one, save current file pos */
    if ( initial ) {
        seg_pos = ftell( file_out->file );
    } else {
        fseek( file_out->file , seg_pos, SEEK_SET );
    }

    for( seg_index = 1; seg_index <= ModuleInfo.total_segs; seg_index++ ) {
        /* find segment by index */
        for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
            if( GetSegIdx( curr->sym.segment ) == seg_index ) {
                break;
            }
        }
        if( curr == NULL ) {
            DebugMsg(( "omf_write_seg: unknown segment #u\n", seg_index ));
            continue;
        }
        if( curr->sym.state != SYM_SEG ) {
            AsmErr( SEG_NOT_DEFINED, curr->sym.name );
            continue;
        }
        objr = curr->e.seginfo->segrec;
        if ( curr->e.seginfo->Ofssize ) {
            objr->is_32 = ( curr->e.seginfo->force32 || ( curr->sym.max_offset >= 0x10000) );
        } else {
            objr->is_32 = FALSE;
        }
        objr->d.segdef.seg_length = curr->sym.max_offset;
        switch ( curr->e.seginfo->alignment ) {
        case  1:
            objr->d.segdef.align = SEGDEF_ALIGN_WORD;
            break;
        case  2:
            objr->d.segdef.align = SEGDEF_ALIGN_DWORD;
            break;
        case  4:
            objr->d.segdef.align = SEGDEF_ALIGN_PARA;
            break;
        case  8:
            objr->d.segdef.align = SEGDEF_ALIGN_PAGE;
            break;
        case 12: /* this is probably invalid for MS OMF */
            objr->d.segdef.align = SEGDEF_ALIGN_4KPAGE;
            break;
        case MAX_SEGALIGNMENT:
            objr->d.segdef.align = SEGDEF_ALIGN_ABS;
            break;
        default:
            objr->d.segdef.align = SEGDEF_ALIGN_BYTE;
            break;
        }
        objr->d.segdef.use_32 = curr->e.seginfo->Ofssize;
        objr->d.segdef.ovl_name_idx = 1;
        objr->d.segdef.seg_name_idx = GetLnameIdx( curr->sym.name );
        omf_write_record( objr, FALSE );
        DebugMsg(("omf_write_seg(%u): %s, seg_name_idx=%u, class_name_idx=%u, align=%u, comb=%u\n",
                  seg_index,
                  curr->sym.name,
                  objr->d.segdef.seg_name_idx,
                  objr->d.segdef.class_name_idx,
                  objr->d.segdef.align,
                  objr->d.segdef.combine
                 ));
        /* write a comment for the linker.
         * this is something not done by Masm, it has
         * been inherited from Wasm.
         */
        if( curr->e.seginfo->segtype == SEGTYPE_CODE ) {
            obj_rec     *rec;

            rec = OmfNewRec( CMD_COMENT );
            rec->d.coment.attr = CMT_TNP;
            rec->d.coment.class = CMT_LINKER_DIRECTIVE;
            OmfAllocData( rec, 3  );
            OmfPut8( rec, LDIR_OPT_FAR_CALLS );
            OmfPutIndex( rec, seg_index );
            omf_write_record( rec, TRUE );
        }
    }
    DebugMsg(("omf_write_seg exit\n"));
}

/* the lnames are stored in a queue. read
 * the items one by one and take care that
 * the record size doesn't exceed 1024 bytes.
 */

void omf_write_lnames( void )
/***************************/
{
    obj_rec     *objr;
    int         size;
    int         items;
    int         startitem;
    char        *p;
    void        *pv = NULL;
    asm_sym     *sym;
    char        buffer[1024];

    DebugMsg(("omf_write_lnames() enter\n"));
    p = buffer;
    *p++ = NULLC; /* start with the NULL entry */
    items = 1;
    startitem = 1;

    do {
        GetLnameData(&pv, &sym );
        size = p - buffer;
        if ( sym == NULL || ( ( size + sym->name_size + 1 ) > 1024 )) {
            if( size ) {
                objr = OmfNewRec( CMD_LNAMES );
                objr->d.lnames.first_idx = startitem;
                objr->d.lnames.num_names = LnamesIdx;
                OmfAttachData( objr, buffer, size );
                omf_write_record( objr, TRUE );
                startitem = items;
            }
            p = buffer;
        }
        if ( sym  ) {
            *p++ = (char)sym->name_size;
            strcpy( p, sym->name );
            /* lnames are converted for casemaps ALL and NOTPUBLIC */
            if ( ModuleInfo.case_sensitive == FALSE )
                _strupr( p );
            DebugMsg(("omf_write_lnames: %u=%s\n", items, p ));
            p += sym->name_size; // overwrite the null char
            items++;
        }
    } while ( sym );

    DebugMsg(("omf_write_lnames() exit\n"));
}

// write EXTDEF records

void omf_write_extdef( )
/**********************/
{
    obj_rec     *objr;
    dir_node    *curr;
    uint        num;
    uint        total_size;
    uint        i;
    char        name[MAX_EXT_LENGTH];
    char        buffer[MAX_ID_LEN+1];
    uint        len;

    total_size = 0;
    i = 0;
    num = 0;

    DebugMsg(("omf_write_extdef enter\n"));
    objr = OmfNewRec( CMD_EXTDEF );
    objr->d.extdef.first_idx = 0;

    // first scan the EXTERN/EXTERNDEF items

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        //DebugMsg(("omf_write_extdef: %s, weak=%u, used=%u\n", curr->sym.name, curr->sym.weak, curr->sym.used ));
        if ( ( curr->sym.comm == TRUE ) || ( curr->sym.weak == TRUE ) )
            continue;
        DebugMsg(("omf_write_extdef: %s\n", curr->sym.name));
        Mangle( &curr->sym, buffer );
        if ( ModuleInfo.convert_uppercase )
            _strupr( buffer );

        len = strlen( buffer );

        if( total_size + len + 2 >= MAX_EXT_LENGTH ) {
            DebugMsg(("omf_write_extdef: write record, names=%u, size=%u, MAX=%u\n", num, total_size, MAX_EXT_LENGTH ));
            OmfAttachData( objr, (uint_8 *)name, total_size );
            objr->d.extdef.num_names = num;
            omf_write_record( objr, TRUE );
            total_size = 0;
            i = 0;
            num = 0;
            objr = OmfNewRec( CMD_EXTDEF );
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

#if 0 /* v2.01: PROTOs have been moved to TAB_EXT */
    // now scan the PROTO items
    for(curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        /* the item must be USED and UNDEFINED */
        if( curr->sym.used == TRUE && curr->sym.isproc == FALSE ) {
            DebugMsg(("omf_write_extdef: %s\n", curr->sym.name));
            Mangle( &curr->sym, buffer );
            if ( ModuleInfo.convert_uppercase )
                _strupr( buffer );

            len = strlen( buffer );

            if( total_size + len + 2 >= MAX_EXT_LENGTH ) {
                OmfAttachData( objr, (uint_8 *)name, total_size );
                objr->d.extdef.num_names = num;
                omf_write_record( objr, TRUE );
                total_size = 0;
                i = 0;
                num = 0;
                objr = OmfNewRec( CMD_EXTDEF );
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
#endif

    if( num != 0 ) {
        DebugMsg(("omf_write_extdef: write record, names=%u, size=%u, MAX=%u\n", num, total_size, MAX_EXT_LENGTH ));
        OmfAttachData( objr, (uint_8 *)name, total_size );
        objr->d.extdef.num_names = num;
        omf_write_record( objr, TRUE );
    } else {
        OmfKillRec( objr );
    }
    DebugMsg(("omf_write_extdef exit\n"));
    return;
}

#define THREE_BYTE_MAX ( (1UL << 24) - 1 )

static int get_size_of_comdef_number( unsigned long value )
/*********************************************************/
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

// for COMDEF: write item size (or number of items)

static uint put_comdef_number( uint_8 *buffer, uint_32 value )
/************************************************************/
{
    uint i;
    uint symsize;

    symsize = get_size_of_comdef_number( value );
    switch( symsize ) {
    case 1:  *buffer = value; break;
    case 3:  *buffer++ = COMDEF_LEAF_2;  break; /* 0x81 */
    case 4:  *buffer++ = COMDEF_LEAF_3;  break; /* 0x84 */
    case 5:  *buffer++ = COMDEF_LEAF_4;  break; /* 0x88 */
    }

    for( i = 1; i < symsize; i++ ) {
        *buffer++ = value % ( UCHAR_MAX + 1 );
        value >>= 8;
    }
    return( symsize );
}

// write OMF COMDEF records

ret_code omf_write_comdef( )
/**************************/
{
    obj_rec     *objr;
    dir_node    *curr;
    uint        num;
    uint        total_size;
    uint        varsize;
    uint        symsize;
    uint        start; /* record's start index */
    uint        i;
    uint        len;
    char        *ptr;
    char        buffer[MAX_ID_LEN+1];
    char        name[MAX_EXT_LENGTH];

    DebugMsg(("omf_write_comdef enter\n"));
    curr = Tables[TAB_EXT].head;
    while (curr) {
        for( num = 0, total_size = 0, i = 0; curr != NULL ; curr = curr->next ) {
            if ( curr->sym.comm == FALSE )
                continue;
            ptr = Mangle( &curr->sym, buffer );
            len = strlen( ptr );
            symsize = 3 + len;
            /* 3 = 1 (string len) + 1 (type index) + 1 (data type) */

            varsize = SizeFromMemtype( curr->sym.mem_type, ModuleInfo.Ofssize );

            DebugMsg(("omf_write_comdef: %s, size=%u, sym.total_size=%u, sym.total_length=%u, sym.isfar=%u\n",
                      curr->sym.name, varsize, curr->sym.total_size, curr->sym.total_length, curr->sym.isfar ));
            if ( varsize == 0 )
                varsize = curr->sym.total_size / curr->sym.total_length;
            if( curr->sym.isfar == TRUE ) {
                symsize += get_size_of_comdef_number( curr->sym.total_length );
                symsize += get_size_of_comdef_number( varsize );
                DebugMsg(("omf_write_comdef: symsize=%u, length=%u, varsize=%u\n",
                          symsize, curr->sym.total_length, varsize ));
            } else {
                symsize += get_size_of_comdef_number( curr->sym.total_length * varsize );
                DebugMsg(("omf_write_comdef: symsize=%u, value=%u\n",
                          symsize, curr->sym.total_length * varsize ));
            }
            /* make sure the record doesn't exceed 1024 */
            if ( total_size + symsize > MAX_EXT_LENGTH )
                break;

            total_size += symsize;

            if ( num == 0 )
                start = curr->sym.idx - 1;

            num++;

            /* copy name ( including size prefix ) */
            name[i] = (char)len;
            i++;
            memcpy( name+i, ptr, len );
            i += len;
            name[i++] = 0;      // for the type index

            /* now add the data type & communal length */
            if ( curr->sym.isfar == TRUE ) {
                name[i++] = COMDEF_FAR;  /* 0x61 */
                i += put_comdef_number( &name[i], curr->sym.total_length );
                i += put_comdef_number( &name[i], varsize );
            } else {
                name[i++] = COMDEF_NEAR; /* 0x62 */
                i += put_comdef_number( &name[i], curr->sym.total_length * varsize );
            }

        } /* end for */

        if( num > 0 ) {
            objr = OmfNewRec( CMD_COMDEF );
            objr->d.comdef.first_idx = start;
            OmfAttachData( objr, (uint_8 *)name, total_size );
            objr->d.comdef.num_names = num;
            omf_write_record( objr, TRUE );
        }
    }
    DebugMsg(("omf_write_comdef exit\n"));
    return( NOT_ERROR );
}

/* Write a THEADR record. If -Zi is set, a comment class
 * A1 record (MS extensions present) is also written.
 */
void omf_write_header( void )
/***************************/
{
    obj_rec     *objr;
    unsigned    len;
    char        *name;
    const FNAME *fn;

    DebugMsg(("omf_write_header() enter\n"));

    objr = OmfNewRec( CMD_THEADR );
    if( Options.module_name != NULL ) {
        name = Options.module_name;
    } else {
        /* may be better to use ModuleInfo.name!!! */
        fn = GetFName( ModuleInfo.srcfile );
        name = fn->fullname;
        len = strlen( name );
        name += len;
        for (;name > fn->fullname && *(name-1) != '/' && *(name-1) != '\\';name--);
    }
    len = strlen( name );
    OmfAllocData( objr, len + 1 );
    OmfPutName( objr, name, len );
    OmfTruncRec( objr );
    omf_write_record( objr, TRUE );

    /* -Zi option set? */
    if ( Options.debug_symbols )
        omf_write_header_dbgcv();

    DebugMsg(("omf_write_header() exit\n"));
}

ret_code omf_write_autodep( void )
/********************************/
{
    obj_rec         *objr;
    FNAME           *curr;
    char            buff[2*PATH_MAX + 5];
    unsigned int    len;
    unsigned        idx;

    DebugMsg(("omf_write_autodep() enter\n"));
    for( idx = 0, curr = FNames; idx < cnt_fnames; idx++, curr++ ) {
        DebugMsg(("omf_write_autodep(): write record for %s\n", curr->name ));
        objr = OmfNewRec( CMD_COMENT );
        objr->d.coment.attr = CMT_TNP;
        objr->d.coment.class = CMT_DEPENDENCY;

        len = strlen( curr->name );
        *((time_t *)buff) = timet2dostime( curr->mtime );
        *(buff + 4) = (unsigned char)len;
        strcpy( buff + 5, curr->name );
        len += 5;

        OmfAttachData( objr, (uint_8 *)buff, len );

        omf_write_record( objr, TRUE );
    }
    // one NULL dependency record must be on the end
    objr = OmfNewRec( CMD_COMENT );
    objr->d.coment.attr = CMT_TNP;
    objr->d.coment.class = CMT_DEPENDENCY;
    OmfAttachData( objr, (uint_8 *)"", 0 );
    omf_write_record( objr, TRUE );
    DebugMsg(("omf_write_autodep() exit\n"));
    return( NOT_ERROR );
}

void omf_write_alias( void )
/**************************/
{
    obj_rec             *objr;
    char                *alias;
    char                *subst;
    char                *new;
    char                len1;
    char                len2;
    //bool                first = TRUE;
    dir_node            *curr;

    for( curr = Tables[TAB_ALIAS].head; curr; curr = curr->next ) {
        alias = curr->sym.name;
        subst = curr->sym.string_ptr;

        /* output an alias record for this alias */
        len1 = strlen( alias );
        len2 = strlen( subst );

        new = (char *)AsmTmpAlloc( len1 + len2 + 2 );

        *new = len1;
        new++;
        strncpy( new, alias, len1 );
        new+=len1;
        *new = len2;
        new++;
        strncpy( new, subst, len2 );
        new -= len1 + 2;

        objr = OmfNewRec( CMD_ALIAS );
        OmfAttachData( objr, (uint_8 *)new, len1+len2+2);
        omf_write_record( objr, TRUE );
        //first = FALSE;
    }
}

static void WritePubRec( uint_8 cmd, asm_sym *curr_seg, uint count, bool need32, struct pubdef_data * data)
/*********************************************************************************************************/
{
    obj_rec             *objr;
    uint                seg;
    uint                grp;
    //uint                i;
    //struct pubdef_data  *d;

    if( curr_seg == NULL ) { // absolute symbol, no segment
        seg = 0;
        grp = 0;
    } else {
        seg = GetSegIdx( curr_seg );
        grp = GetGrpIdx( GetGrp( curr_seg ) );
    }
    objr = OmfNewRec( cmd );
    objr->is_32 = need32;
    objr->d.pubdef.base.grp_idx = grp;
    objr->d.pubdef.base.seg_idx = seg;
    objr->d.pubdef.base.frame = 0;
    objr->d.pubdef.num_pubs = count;
    objr->d.pubdef.pubs = data;
    omf_write_record( objr, TRUE );
#if 0
    /* free the names */
    for( i = 0, d = data; i < count; i++, d++ ) {
        if( d->name != NULL ) {
            AsmFree( d->name );
        }
    }
#endif
    return;
}

#define PUBITEMBASELEN (4+2+1)  /* sizes offset + index + name len */

ret_code omf_write_public( bool initial )
/***************************************/
{
    struct asm_sym      *sym;
    struct asm_sym      *curr_seg;
    struct pubdef_data  *d;
    void                *vp;
    uint                count;
    uint                size;
    uint_8              cmd = CMD_PUBDEF;
    bool                need32;
    char                *pbuf;

    DebugMsg(("omf_write_pub enter\n"));

    if ( initial ) {
        public_pos = ftell( file_out->file );
    } else {
        fseek( file_out->file, public_pos, SEEK_SET);
    }

    vp = NULL;
    d = (struct pubdef_data *)StringBufferEnd;
    pbuf = StringBufferEnd + 1024;
    size = 10; /* =size of an empty PUBDEF record */
    count = 0;
    need32 = FALSE;
    while ( sym = (asm_sym *)GetPublicData( &vp ) ) {
        d->name = Mangle( sym, pbuf );
        /* if segment changes of record becomes too big, write record */
        if( ( count && ( sym->segment != curr_seg )) ||
           ( ( size + strlen( d->name ) + PUBITEMBASELEN ) > MAX_PUB_LENGTH )) {
            WritePubRec( cmd, curr_seg, count, need32, (struct pubdef_data *)StringBufferEnd );
            d = (struct pubdef_data *)StringBufferEnd;
            pbuf = StringBufferEnd + 1024;
            d->name = Mangle( sym, pbuf );
            size = 10; /* =size of an empty PUBDEF record */
            count = 0;
            need32 = FALSE;
        }
        if ( ModuleInfo.convert_uppercase )
            _strupr( d->name );
        pbuf += strlen( pbuf ) + 1;
        curr_seg = sym->segment;
        if( sym->offset > 0xffffUL )
            need32 = TRUE;

        size += strlen( d->name ) + PUBITEMBASELEN;
        d->offset = sym->offset;
        d->type.idx = 0;
        count++;
        DebugMsg(("omf_write_pub(%u): %s, ofs=%Xh, rec_size=%u\n", count, d->name, d->offset, size ));
        d++;
    }
    if ( count )
        WritePubRec( cmd, curr_seg, count, need32, (struct pubdef_data *)StringBufferEnd );

    DebugMsg(("omf_write_pub exit\n"));
    return( NOT_ERROR );
}

void omf_write_modend( void )
/***************************/
{
#if TRUNCATE
    int fh;
    uint_32 size;
#endif

    omf_write_record( ModendRec, TRUE );

#if TRUNCATE
    /* under some very rare conditions, the object
     * module might become shorter! Hence the file
     * must be truncated now. The problem is that there
     * is no stream function for this task.
     * the final solution will be to save the segment contents
     * in buffers and write the object module once everything
     * is done ( as it is done for the other formats already).
     */
    size = ftell( file_out->file );
#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)
    fh = fileno( file_out->file );
    ftruncate( fh, size );
#else
    fh = _fileno( file_out->file );
    _chsize( fh, size );
#endif

#endif
}

/* translate generic 'asmfixup' to omf-specific 'fixup'.
 * Note that if Modend is TRUE, it means the fixup is the starting address
 * for the module.
 */
struct fixup *omf_create_fixup( struct asmfixup *fixup )
/******************************************************/
{
    struct fixup        *fixomf;    // OMF fixup structure
    struct asm_sym      *sym;

    //fixup = CodeInfo->InsFixup[index];
    sym = fixup->sym; /* may be NULL! */
    fixomf = OmfFixNew();
    fixomf->next = NULL;
    fixomf->self_relative = FALSE;

    if( !Modend ) {
        fixomf->lr.is_secondary = TRUE;
        fixomf->lr.target_offset = 0;
        switch( fixup->type ) {
        case FIX_RELOFF8:
            fixomf->self_relative = TRUE;
        case FIX_LOBYTE:
            fixomf->loc_method = FIXO_LO_BYTE;
            break;
        case FIX_HIBYTE:
            fixomf->loc_method = FIXO_HI_BYTE;
            break;
        case FIX_RELOFF16:
            fixomf->self_relative = TRUE;
        case FIX_OFF16:
            fixomf->loc_method = FIXO_OFFSET;
            break;
        case FIX_RELOFF32:
            fixomf->self_relative = TRUE;
        case FIX_OFF32:
            fixomf->loc_method = FIXO_OFFSET386;
            break;
        case FIX_SEG:
            fixomf->loc_method = FIXO_BASE;
            break;
        case FIX_PTR16:
            fixomf->loc_method = FIXO_POINTER;
            break;
        case FIX_PTR32:
            fixomf->loc_method = FIXO_POINTER386;
            break;
        default:
            AsmErr( UNSUPPORTED_FIXUP_TYPE,
                   formatoptions[Options.output_format].formatname,
                   fixup->sym ? fixup->sym->name : szNull );
            return( NULL );
        }
        /* set the fixup's location in current LEDATA */
        /* CurrSeg->curr_loc - CurrSeg->start_loc */

        fixomf->loc_offset = GetCurrOffset() - CurrSeg->e.seginfo->start_loc;

    } else {
        fixomf->lr.is_secondary = FALSE;
        fixomf->lr.target_offset = fixup->offset;
        fixomf->loc_offset = 0;
    }

#ifdef DEBUG_OUT
    if (sym)
        DebugMsg(("omf_create_fixup(%X): sym=%s, state=%u, fixup->type=%u\n", fixup, sym->name, sym->state, fixup->type ));
    else
        DebugMsg(("omf_create_fixup(%X): sym=NULL, fixup->type=%u\n", fixup, fixup->type ));
#endif

    /* loader_resolved is FALSE for OFFSET, TRUE for LROFFSET */
    fixomf->loader_resolved = fixup->loader_resolved;

    /*------------------------------------*/
    /* Determine the Target and the Frame */
    /*------------------------------------*/

    if( sym == NULL ) {

        if ( fixup->frame == EMPTY ) /* v1.96: nothing to do without a frame */
            return( NULL );
        fixomf->lr.target = fixup->frame;
        fixomf->lr.target_datum = fixup->frame_datum;
        fixomf->lr.frame = FRAME_TARG;

    } else if( sym->state == SYM_UNDEFINED ) { /* shouldn't happen */
        DebugMsg(("omf_create_fixup(%X): state of >%s< is SYM_UNDEFINED\n", fixup, sym->name));
        AsmErr( SYMBOL_NOT_DEFINED, sym->name );
        return( NULL );
    } else if( sym->state == SYM_GRP ) {

        DebugMsg(("omf_create_fixup(%X): GROUP %s\n", fixup, sym->name));

        fixomf->lr.target = TARGET_GRP;
        fixomf->lr.target_datum = GetGrpIdx( sym );
        if( fixup->frame != EMPTY ) {
            fixomf->lr.frame = fixup->frame;
            fixomf->lr.frame_datum = fixup->frame_datum;
        } else {
            fixomf->lr.frame = FRAME_GRP;
            fixomf->lr.frame_datum = fixomf->lr.target_datum;
        }

    } else if( sym->state == SYM_SEG ) {

        DebugMsg(("omf_create_fixup(%X): SEG %s\n", fixup, sym->name));

        fixomf->lr.target = TARGET_SEG;
        fixomf->lr.target_datum = GetSegIdx( sym );
        if( fixup->frame != EMPTY ) {
            fixomf->lr.frame = fixup->frame;
            fixomf->lr.frame_datum = fixup->frame_datum;
        } else {
            fixomf->lr.frame = FRAME_SEG;
            fixomf->lr.frame_datum = fixomf->lr.target_datum;
        }

    } else {

        /* symbol is a label */

        if( sym->state == SYM_EXTERNAL ) {
            DebugMsg(("omf_create_fixup(%X): EXTERNAL %s\n", fixup, sym->name));
            if( Modend ) {
                fixomf->lr.target = TARGET_EXT & TARGET_WITH_DISPL;
            } else {
                fixomf->lr.target = TARGET_EXT;
            }

            fixomf->lr.target_datum = sym->idx;

            if( fixup->frame == FRAME_GRP && fixup->frame_datum == 0 ) {
                /* set the frame to the frame of the corresponding segment */
                fixup->frame_datum = GetGrpIdx( sym );
            }
        } else {
            //asm_sym *grpsym;
            /* it's a SYM_INTERNAL */
            DebugMsg(("omf_create_fixup(%X): fixup->frame, datum=%u.%u sym->name=%s state=%X segm=%X\n",
                      fixup, fixup->frame, fixup->frame_datum, sym->name, sym->state, sym->segment ));
            if ( sym->segment == NULL ) {
                AsmErr( SEGMENT_MISSING_FOR_FIXUP, sym->name );
                return (NULL);
            }

            if( Modend )
                fixomf->lr.target = TARGET_SEG & TARGET_WITH_DISPL;
            else
                fixomf->lr.target = TARGET_SEG;
#if 0
            if ( grpsym = GetGrp( sym ) ) {
                fixup->frame = FRAME_GRP;
                fixup->frame_datum = GetGrpIdx( grpsym );
            }
#endif
            fixomf->lr.target_datum = GetSegIdx( sym->segment );
        }

        if( fixup->frame != EMPTY ) {
            fixomf->lr.frame = (uint_8)fixup->frame;
        } else {
            fixomf->lr.frame = FRAME_TARG;
        }
        fixomf->lr.frame_datum = fixup->frame_datum;

        if( Modend ) {
            DebugMsg(( "omf_create_fixup: ModEnd fixup=%X/%X/%X/%X\n", fixomf->lr.target, fixomf->lr.frame, fixomf->lr.frame_datum, fixomf->loc_offset ));
            return( fixomf );
        }
    }

    /*--------------------*/
    /* Optimize the fixup */
    /*--------------------*/

    if( fixomf->lr.frame == ( fixomf->lr.target - TARGET_SEG ) ) {
        fixomf->lr.frame = FRAME_TARG;
    }

    return( fixomf );
}

/* add segments $$SYMBOLS, $$TYPES to the segment table */

void omf_write_header_dbgcv( void )
/*********************************/
{
    obj_rec     *objr;
    asm_sym *symbols;
    asm_sym *types;

    objr = OmfNewRec( CMD_COMENT );
    objr->d.coment.attr = 0x00;
    objr->d.coment.class = CMT_MS_OMF; /* MS extensions present */
    OmfAttachData( objr, "\001CV", 3 );
    omf_write_record( objr, TRUE );
    symbols = CreateSegment( szCVSymbols, szCVSymClass, 0, USE32 );
    ((dir_node *)symbols)->e.seginfo->force32 = TRUE;
    types = CreateSegment( szCVTypes, szCVTypClass, 0, USE32 );
    ((dir_node *)types)->e.seginfo->force32 = TRUE;
    ModuleInfo.total_segs += 2;
    return;
}

/* write contents of segments $$SYMBOLS and $$TYPES */

void omf_write_debug_tables( void )
/*********************************/
{
    dir_node *types = (dir_node *)SymSearch( szCVTypes );
    dir_node *symbols = (dir_node *)SymSearch( szCVSymbols );
    cv_write_debug_tables( symbols, types );
}
