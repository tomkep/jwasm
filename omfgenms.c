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
* Description:  Microsoft style OMF output routines.
*
****************************************************************************/


#include <stddef.h>

#include "globals.h"
#include "memalloc.h"
#include "omfrec.h"
#include "omfprs.h"
#include "omfio.h"
#include "omfpc.h"
#include "omfgenms.h"
#include "myassert.h"
#include "msdbg.h"

#define WRN 0x8000

extern void PrtMsg( unsigned msg, ... );

static int writeMisc( obj_rec *objr, pobj_state *state ) {
/*
    For 16-bit records which are the same under Intel and MS OMFs
*/
    uint_8  *ptr;
    uint_16 len;
    uint_16 save;

/**/myassert( objr != NULL );
/**/myassert( objr->data != NULL );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    save = OmfRTell( objr );
    OmfRSeek( objr, 0 );
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
    OmfWriteRec( state->file_out, objr->command, len, ptr );
    OmfRSeek( objr, save );
    return( 0 );
}

static int writeMisc32( obj_rec *objr, pobj_state *state ) {
/*
    For 32-bit records which are the same under Intel and MS OMFs
*/
    uint_8  *ptr;
    uint_16 len;
    uint_8  cmd;
    uint_16 save;

/**/myassert( objr != NULL );
/**/myassert( objr->data != NULL );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    save = OmfRTell( objr );
    OmfRSeek( objr, 0 );
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
    cmd = objr->command;
    if( objr->is_32 ) {
        cmd |= 0x01;
    }
    OmfWriteRec( state->file_out, cmd, len, ptr );
    OmfRSeek( objr, save );
    return( 0 );
}

static int writeComent( obj_rec *objr, pobj_state *state ) {

    uint_8  *ptr;
    uint_16 len;
    uint_16 save;
    OBJ_WFILE *out;

/**/myassert( objr != NULL );
/**/myassert( objr->data != NULL );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    out = state->file_out;
    OmfWBegRec( out, CMD_COMENT );
    OmfWrite8( out, objr->d.coment.attr );
    OmfWrite8( out, objr->d.coment.class );
    save = OmfRTell( objr );
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
    OmfWrite( out, ptr, len );
    OmfRSeek( objr, save );
    OmfWEndRec( out );
    return( 0 );
}

static int writeSegdef( obj_rec *objr, pobj_state *state ) {

    OBJ_WFILE   *out;
    int         is32;
    uint_8      acbp;
    uint_8      align;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_SEGDEF );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    out = state->file_out;
    //is32 = objr->d.segdef.use_32;
    is32 = objr->is_32;
    OmfWBegRec( out, is32 ? CMD_SEGD32 : CMD_SEGDEF );
    acbp = ( objr->d.segdef.combine << 2 ) | ( objr->d.segdef.use_32 != 0 );
    align = objr->d.segdef.align;
    switch( align ) {
    case SEGDEF_ALIGN_ABS:      acbp |= ALIGN_ABS << 5;     break;
    case SEGDEF_ALIGN_BYTE:     acbp |= ALIGN_BYTE << 5;    break;
    case SEGDEF_ALIGN_WORD:     acbp |= ALIGN_WORD << 5;    break;
    case SEGDEF_ALIGN_PARA:     acbp |= ALIGN_PARA << 5;    break;
    case SEGDEF_ALIGN_PAGE:     acbp |= ALIGN_PAGE << 5;    break;
    case SEGDEF_ALIGN_DWORD:    acbp |= ALIGN_DWORD << 5;   break;
    case SEGDEF_ALIGN_4KPAGE:
        acbp |= ALIGN_PAGE;
        PrtMsg( WRN | MSG_MS386_NO_4KPAGE );
        break;
    default:
/**/    never_reach();
    }
    if( is32 == 0 && objr->d.segdef.seg_length == 0x10000 ) {
        acbp |= 0x02;   /* set BIG bit */
    }
    OmfWrite8( out, acbp );
    if( align == SEGDEF_ALIGN_ABS ) {
        // absolut segment has frame=word and offset=byte
        // it isn't fixupp physical reference
        // and don't depend on segment size (16/32bit)
        OmfWrite16( out, objr->d.segdef.abs.frame );
        OmfWrite8( out, objr->d.segdef.abs.offset );
    }
    if( is32 ) {
        OmfWrite32( out, objr->d.segdef.seg_length );
    } else {
        OmfWrite16( out, objr->d.segdef.seg_length & 0xffff );
    }

    OmfWriteIndex( out, objr->d.segdef.seg_name_idx );
    OmfWriteIndex( out, objr->d.segdef.class_name_idx );
    OmfWriteIndex( out, objr->d.segdef.ovl_name_idx );
    if( objr->d.segdef.access_valid ) {
        PrtMsg( MSG_MS386_NO_ACCESS );
    }
    OmfWEndRec( out );
    return( 0 );
}

static int writeFixup( obj_rec *objr, pobj_state *state ) {

    OBJ_WFILE   *out;
    int         is32;
    fixup       *walk;
    uint_8      buf[ FIX_GEN_MAX ];
    size_t      len;
    size_t      len_written;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_FIXUP );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    walk = objr->d.fixup.fixup;
    if( walk == NULL ) {
            /* huh? this shouldn't really happen... */
        return( 0 );
    }
    out = state->file_out;
    is32 = objr->is_32;
    /* we don't want to write FIXUP records that are too large, so we limit
       our records to approximately 1024 bytes */
    do {
        len_written = 0;
        OmfWBegRec( out, is32 ? CMD_FIXU32 : CMD_FIXUP );
        while( walk != NULL && len_written < 1024 - FIX_GEN_MAX ) {
            len = FixGenFix( walk, buf, is32 ? FIX_GEN_MS386 : FIX_GEN_INTEL );
            OmfWrite( out, buf, len );
            walk = walk->next;
            len_written += len;
        }
        OmfWEndRec( out );
    } while( walk != NULL );
    return( 0 );
}

static int writeLedata( obj_rec *objr, pobj_state *state ) {

    OBJ_WFILE   *out;
    uint_16     save;
    uint_8      *ptr;
    uint_16     len;
    int         is32;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LEDATA );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    out = state->file_out;
    is32 = ( objr->is_32 ) != 0;
    OmfWBegRec( out, is32 ? CMD_LEDA32 : CMD_LEDATA );
    OmfWriteIndex( out, objr->d.ledata.idx );
    if( is32 ) {
        OmfWrite32( out, objr->d.ledata.offset );
    } else {
        OmfWrite16( out, objr->d.ledata.offset );
    }
    save = OmfRTell( objr );
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
/**/myassert( len <= 1024 );
    OmfWrite( out, ptr, len );
    OmfWEndRec( out );
    OmfRSeek( objr, save );
    return( 0 );
}

static int writeLidata( obj_rec *objr, pobj_state *state ) {

    OBJ_WFILE   *out;
    uint_16     save;
    uint_8      *ptr;
    uint_16     len;
    int         is32;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LIDATA );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    out = state->file_out;
    save = OmfRTell( objr );
    is32 = objr->is_32;
    OmfWBegRec( out, is32 ? CMD_LIDA32 : CMD_LIDATA );
    OmfWriteIndex( out, objr->d.lidata.idx );
    if( is32 ) {
        OmfWrite32( out, objr->d.lidata.offset );
    } else {
        OmfWrite16( out, objr->d.lidata.offset );
    }
    /* ok, already in our format */
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
    OmfWrite( out, ptr, len );
    OmfWEndRec( out );
    OmfRSeek( objr, save );
    return( 0 );
}

static int writeTheadr( obj_rec *objr, pobj_state *state ) {

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_THEADR );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );

    return( writeMisc( objr, state ) );
}

static int writeModend( obj_rec *objr, pobj_state *state ) {

    size_t  len;
    char    is32;
    uint_8  buf[ 1 + FIX_GEN_MAX ];
    uint_8  is_log;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_MODEND );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    is32 = objr->is_32;
    len = 1;
    buf[0]  = objr->d.modend.main_module ? 0x80 : 0;
    if( objr->d.modend.start_addrs ) {
        is_log = objr->d.modend.is_logical ? 1 : 0;
        buf[0] |= 0x40 | is_log;
        len += FixGenRef( &objr->d.modend.ref, is_log, buf + 1,
            is32 ? FIX_GEN_MS386 : FIX_GEN_INTEL );
    }
    if( buf[0] == 0 ) {
        is32 = 0;       /* no need for MODE32 in this case */
    }
    OmfWriteRec( state->file_out, is32 ? CMD_MODE32 : CMD_MODEND,
        len, buf );

    return( 0 );
}

static void writeBase( obj_rec *objr, OBJ_WFILE *out ) {

    uint_16 grp_idx;
    uint_16 seg_idx;

    grp_idx = objr->d.base.grp_idx;
    seg_idx = objr->d.base.seg_idx;
    OmfWriteIndex( out, grp_idx );
    OmfWriteIndex( out, seg_idx );
    if( grp_idx == 0 && seg_idx == 0 ) {
        OmfWrite16( out, objr->d.base.frame );
    }
}

static int writeComdat( obj_rec *objr, pobj_state *state ) {

    OBJ_WFILE   *out;
    uint_8      *ptr;
    uint_16     len;
    uint_16     save;
    int         is_32;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_COMDAT );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    save = OmfRTell( objr );
    is_32 = objr->is_32;
    out = state->file_out;
    OmfWBegRec( out, objr->command | ( is_32 ? 1 : 0 ) );
    OmfWrite8( out, objr->d.comdat.flags );
    OmfWrite8( out, objr->d.comdat.attributes );
    OmfWrite8( out, objr->d.comdat.align );
    if( is_32 ) {
        OmfWrite32( out, objr->d.comdat.offset );
    } else {
        OmfWrite16( out, objr->d.comdat.offset );
    }
    OmfWriteIndex( out, objr->d.comdat.type_idx );
    if( ( objr->d.comdat.attributes & COMDAT_ALLOC_MASK ) == COMDAT_EXPLICIT ) {
        writeBase( objr, out );
    }
    OmfWriteIndex( out, objr->d.comdat.public_name_idx );
    /* record is already in ms omf format */
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
    /**/    myassert( len <= 1024 );
    OmfWrite( out, ptr, len );
    OmfWEndRec( out );
    OmfRSeek( objr, save );
    return( 0 );
}

static int writePubdef( obj_rec *objr, pobj_state *state ) {

    int         is32;
    OBJ_WFILE   *out;
    const char  *name;
    uint_8      name_len;
    pubdef_data *pubdata;
    pubdef_data *pubstop;

/**/myassert( objr != NULL );
/**/myassert(   objr->command == CMD_PUBDEF ||
                objr->command == CMD_STATIC_PUBDEF );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    is32 = objr->is_32;
    out = state->file_out;
    OmfWBegRec( out, objr->command | ( is32 ? 1 : 0 ) );
    writeBase( objr, out );
    pubdata = objr->d.pubdef.pubs;
    if( pubdata != NULL ) {
        pubstop = pubdata + objr->d.pubdef.num_pubs;
        while( pubdata < pubstop ) {
            name = omf_NameGet( pubdata->name );
            name_len = strlen( name );
            OmfWrite8( out, name_len );
            OmfWrite( out, (uint_8 *)name, (size_t)name_len );
            if( is32 ) {
                OmfWrite32( out, pubdata->offset );
            } else {
                OmfWrite16( out, pubdata->offset );
            }
            OmfWriteIndex( out, pubdata->type.idx );
            ++pubdata;
        }
    }
    OmfWEndRec( out );
    return( 0 );
}

static void writeLinnumData( obj_rec *objr, OBJ_WFILE *out ) {

    int is32;

/**/myassert( objr != NULL );
/**/myassert( out != NULL );
    is32 = objr->is_32;
#if defined( __BIG_ENDIAN__ )
    {
        linnum_data *cur;
        linnum_data *stop;

        cur = objr->d.linnum.lines;
        stop = cur + objr->d.linnum.num_lines;
        while( cur < stop ) {
            OmfWrite16( out, cur->number );
            if( is32 ) {
                OmfWrite32( out, cur->offset );
            } else {
                OmfWrite16( out, (uint_16)cur->offset );
            }
            ++cur;
        }
    }
#else
    if( is32 ) {
        OmfWrite( out, (uint_8 *)objr->d.linnum.lines,
            6 * objr->d.linnum.num_lines );
/**/    myassert( sizeof( linnum_data ) == 6 );
    } else {
        linnum_data *cur;
        linnum_data *stop;

        cur = objr->d.linnum.lines;
        stop = cur + objr->d.linnum.num_lines;
        while( cur < stop ) {
            OmfWrite16( out, cur->number );
/**/        myassert( ( cur->offset & 0xffff0000 ) == 0 );
            OmfWrite16( out, (uint_16)cur->offset );
            ++cur;
        }
    }
#endif
}

static int writeLinnum( obj_rec *objr, pobj_state *state ) {

    int         is32;
    OBJ_WFILE   *out;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LINNUM );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    out = state->file_out;
    is32 = objr->is_32;
    OmfWBegRec( out, is32 ? CMD_LINN32 : CMD_LINNUM );
    writeBase( objr, out );
    writeLinnumData(objr, out );
    OmfWEndRec( out );
    return( 0 );
}

static int writeLinsym( obj_rec *objr, pobj_state *state ) {

    int         is32;
    OBJ_WFILE   *out;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LINSYM );
/**/myassert( state != NULL );
/**/myassert( state->pass == POBJ_WRITE_PASS );
    out = state->file_out;
    is32 = objr->is_32;
    OmfWBegRec( out, is32 ? CMD_LINS32 : CMD_LINSYM );
    OmfWrite8( out, objr->d.linsym.d.linsym.flags );
    OmfWriteIndex( out, objr->d.linsym.d.linsym.public_name_idx );
    writeLinnumData( objr, out );
    OmfWEndRec( out );
    return( 0 );
}

static const pobj_list myFuncs[] = {
    { CMD_THEADR,       POBJ_WRITE_PASS, writeTheadr },
    { CMD_COMENT,       POBJ_WRITE_PASS, writeComent },
    { CMD_MODEND,       POBJ_WRITE_PASS, writeModend },
    { CMD_EXTDEF,       POBJ_WRITE_PASS, writeMisc },
    { CMD_LNAMES,       POBJ_WRITE_PASS, writeMisc },
    { CMD_SEGDEF,       POBJ_WRITE_PASS, writeSegdef },
    { CMD_GRPDEF,       POBJ_WRITE_PASS, writeMisc },
    { CMD_LEDATA,       POBJ_WRITE_PASS, writeLedata },
    { CMD_FIXUP,        POBJ_WRITE_PASS, writeFixup },
    { CMD_PUBDEF,       POBJ_WRITE_PASS, writePubdef },
    { CMD_TYPDEF,       POBJ_WRITE_PASS, writeMisc },
    { CMD_LINNUM,       POBJ_WRITE_PASS, writeLinnum },
    { CMD_LIDATA,       POBJ_WRITE_PASS, writeLidata },
    { CMD_COMDEF,       POBJ_WRITE_PASS, writeMisc },
    { CMD_CEXTDF,       POBJ_WRITE_PASS, writeMisc },
    { CMD_STATIC_EXTDEF,POBJ_WRITE_PASS, writeMisc32 },
    { CMD_STATIC_PUBDEF,POBJ_WRITE_PASS, writePubdef },
    { CMD_STATIC_COMDEF,POBJ_WRITE_PASS, writeMisc },
    { CMD_BAKPAT,       POBJ_WRITE_PASS, writeMisc32 },
    { CMD_COMDAT,       POBJ_WRITE_PASS, writeComdat },
    { CMD_LINSYM,       POBJ_WRITE_PASS, writeLinsym },
    { CMD_ALIAS,        POBJ_WRITE_PASS, writeMisc },
    { CMD_NBKPAT,       POBJ_WRITE_PASS, writeMisc32 },
    { CMD_LLNAMES,      POBJ_WRITE_PASS, writeMisc }
};
#define NUM_FUNCS   ( sizeof( myFuncs ) / sizeof( pobj_list ) )

void omf_GenMSInit( void ) {
/***********************/
    omf_RegList( myFuncs, NUM_FUNCS );
}

void omf_GenMSFini( void ) {
/***********************/
    omf_UnRegList( myFuncs, NUM_FUNCS );
}
