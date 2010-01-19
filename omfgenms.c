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
#include "omfio.h"
#include "omfpc.h"
#include "omfgenms.h"
#include "myassert.h"
#include "queue.h"

#define OmfRTell(objr)          ( (objr)->curoff )
#define OmfRSeek(objr,set)      (void)( (objr)->curoff = set )
#define OmfRemain(objr)         ( (objr)->length - (objr)->curoff )

typedef int (*pobj_filter)( OBJ_WFILE *out, obj_rec *objr );

typedef struct {
    uint_8      command;
    pobj_filter func;
} pobj_list;

extern OBJ_WFILE *file_out;

static int writeMisc( OBJ_WFILE *out, obj_rec *objr )
/***************************************************/
{
/*
    For 16-bit records which are the same under Intel and MS OMFs
*/
    uint_8  *ptr;
    size_t  len;
    size_t  save;

/**/myassert( objr != NULL );
/**/myassert( objr->data != NULL );

    save = OmfRTell( objr );
    OmfRSeek( objr, 0 );
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
    OmfWriteRec( out, objr->command, len, ptr );
    OmfRSeek( objr, save );
    return( 0 );
}

static int writeMisc32( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
/*
    For 32-bit records which are the same under Intel and MS OMFs
*/
    uint_8  *ptr;
    size_t  len;
    uint_8  cmd;
    size_t  save;

/**/myassert( objr != NULL );
/**/myassert( objr->data != NULL );

    save = OmfRTell( objr );
    OmfRSeek( objr, 0 );
    len = OmfRemain( objr );
    ptr = OmfGet( objr, len );
    cmd = objr->command;
    if( objr->is_32 ) {
        cmd |= 0x01;
    }
    OmfWriteRec( out, cmd, len, ptr );
    OmfRSeek( objr, save );
    return( 0 );
}

static int writeComent( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    uint_8  *ptr;
    size_t len;
    size_t save;

/**/myassert( objr != NULL );
/**/myassert( objr->data != NULL );

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

static int writeSegdef( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    int         is32;
    uint_8      acbp;
    uint_8      align;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_SEGDEF );

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
        AsmError( NO_4KPAGE_ALIGNED_SEGMENTS );
        break;
    default:
        /**/never_reach();
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
#if 0
    if( objr->d.segdef.access_valid ) {
        AsmError( ACCESS_CLASSES_NOT_SUPPORTED );
    }
#endif
    OmfWEndRec( out );
    return( 0 );
}

static int writeFixup( OBJ_WFILE *out, obj_rec *objr )
/****************************************************/
{
    int         is32;
    fixup       *walk;
    size_t      len;
    size_t      len_written;
    uint_8      buf[ FIX_GEN_MAX ];

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_FIXUP );

    walk = objr->d.fixup.fixup;
    if( walk == NULL ) {
        /* huh? this shouldn't really happen... */
        return( 0 );
    }
    is32 = objr->is_32;
    /* we don't want to write FIXUP records that are too large, so we limit
       our records to approximately 1024 bytes */
    do {
        len_written = 0;
        OmfWBegRec( out, is32 ? CMD_FIXU32 : CMD_FIXUP );
        while( walk != NULL && len_written < 1024 - FIX_GEN_MAX ) {
            len = OmfFixGenFix( walk, buf, is32 ? FIX_GEN_MS386 : FIX_GEN_INTEL );
            OmfWrite( out, buf, len );
            walk = walk->next;
            len_written += len;
        }
        OmfWEndRec( out );
    } while( walk != NULL );
    return( 0 );
}

static int writeLedata( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    size_t      save;
    size_t      len;
    uint_8      *ptr;
    int         is32;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LEDATA );

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

/* CMD_LIDATA isn't used currently. JWasm always uses
 * LEDATA instead. This differs from Masm.
 */

static int writeLidata( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    size_t      save;
    size_t      len;
    uint_8      *ptr;
    int         is32;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LIDATA );

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

static int writeTheadr( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_THEADR );

    return( writeMisc( out, objr ) );
}

static int writeModend( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    size_t  len;
    int     is32;
    uint_8  is_log;
    uint_8  buf[ 1 + FIX_GEN_MAX ];

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_MODEND );

    is32 = objr->is_32;
    len = 1;
    buf[0]  = objr->d.modend.main_module ? 0x80 : 0;
    if( objr->d.modend.start_addrs ) {
        is_log = objr->d.modend.is_logical ? 1 : 0;
        buf[0] |= 0x40 | is_log;
        len += OmfFixGenRef( &objr->d.modend.ref, is_log, buf + 1,
            is32 ? FIX_GEN_MS386 : FIX_GEN_INTEL );
    }
    if( buf[0] == 0 ) {
        is32 = 0;       /* no need for MODE32 in this case */
    }
    OmfWriteRec( out, is32 ? CMD_MODE32 : CMD_MODEND,
        len, buf );

    return( 0 );
}

static void writeBase( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    size_t grp_idx;
    size_t seg_idx;

    grp_idx = objr->d.base.grp_idx;
    seg_idx = objr->d.base.seg_idx;
    OmfWriteIndex( out, grp_idx );
    OmfWriteIndex( out, seg_idx );
    if( grp_idx == 0 && seg_idx == 0 ) {
        OmfWrite16( out, objr->d.base.frame );
    }
}

/* COMDATs are initialized communal data records.
 * This isn't used currently, the Masm COMM directive
 * defines uninitialized communal data items.
 */

static int writeComdat( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    uint_8      *ptr;
    size_t      len;
    size_t      save;
    int         is_32;

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_COMDAT );

    save = OmfRTell( objr );
    is_32 = objr->is_32;
    /* write CMD_COMDAT or CMD_COMD32 */
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
        writeBase( out, objr );
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

static int writePubdef( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    int         is32;
    const char  *name;
    size_t      name_len;
    pubdef_data *pubdata;
    pubdef_data *pubstop;

/**/myassert( objr != NULL );
/**/myassert(   objr->command == CMD_PUBDEF ||
                objr->command == CMD_LPUBDEF );

    is32 = objr->is_32;
    OmfWBegRec( out, objr->command | ( is32 ? 1 : 0 ) );
    writeBase( out, objr );
    pubdata = objr->d.pubdef.pubs;
    if( pubdata != NULL ) {
        pubstop = pubdata + objr->d.pubdef.num_pubs;
        while( pubdata < pubstop ) {
            name = pubdata->name;
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

static void writeLinnumData( OBJ_WFILE *out, obj_rec *objr )
/**********************************************************/
{
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

static int writeLinnum( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LINNUM );

    OmfWBegRec( out, objr->is_32 ? CMD_LINN32 : CMD_LINNUM );
    writeBase( out, objr );
    writeLinnumData( out, objr );
    OmfWEndRec( out );
    return( 0 );
}

/* Not used. these record types are only used in conjunction
 * with CMD_COMDAT.
 */

static int writeLinsym( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{

/**/myassert( objr != NULL );
/**/myassert( objr->command == CMD_LINSYM );

    OmfWBegRec( out, objr->is_32 ? CMD_LINS32 : CMD_LINSYM );
    OmfWrite8( out, objr->d.linsym.d.linsym.flags );
    OmfWriteIndex( out, objr->d.linsym.d.linsym.public_name_idx );
    writeLinnumData( out, objr );
    OmfWEndRec( out );
    return( 0 );
}

static const pobj_list myFuncs[] = {
    { CMD_THEADR,       writeTheadr },  /* 0x80 */
    { CMD_COMENT,       writeComent },  /* 0x88 */
    { CMD_MODEND,       writeModend },  /* 0x8a */
    { CMD_EXTDEF,       writeMisc },    /* 0x8c */
    { CMD_TYPDEF,       writeMisc },    /* 0x8e not used */
    { CMD_PUBDEF,       writePubdef },  /* 0x90 */
    { CMD_LINNUM,       writeLinnum },  /* 0x94 */
    { CMD_LNAMES,       writeMisc },    /* 0x96 */
    { CMD_SEGDEF,       writeSegdef },  /* 0x98 */
    { CMD_GRPDEF,       writeMisc },    /* 0x9a */
    { CMD_FIXUP,        writeFixup },   /* 0x9c */
    { CMD_LEDATA,       writeLedata },  /* 0xa0 */
    { CMD_LIDATA,       writeLidata },  /* 0xa2 not used */
    { CMD_COMDEF,       writeMisc },    /* 0xb0 */
    { CMD_BAKPAT,       writeMisc32 },  /* 0xb2 not used */
    { CMD_STATIC_EXTDEF,writeMisc32 },  /* 0xb4 not used */
    { CMD_LPUBDEF,      writePubdef },  /* 0xb6 not used */
    { CMD_STATIC_COMDEF,writeMisc },    /* 0xb8 not used */
    { CMD_CEXTDF,       writeMisc },    /* 0xbc not used */
    { CMD_COMDAT,       writeComdat },  /* 0xc2 not used */
    { CMD_LINSYM,       writeLinsym },  /* 0xc4 not used */
    { CMD_ALIAS,        writeMisc },    /* 0xc6 */
    { CMD_NBKPAT,       writeMisc32 },  /* 0xc8 not used */
    { CMD_LLNAMES,      writeMisc }     /* 0xca not used */
};
#define NUM_FUNCS   ( sizeof( myFuncs ) / sizeof( pobj_list ) )

#define JUMP_OFFSET(cmd)    ( ( cmd ) - CMD_MIN_CMD )

static pobj_filter      jumpTable[ CMD_MAX_CMD - CMD_MIN_CMD + 1 ];

// call a function

void omf_write_record( obj_rec *objr, char kill )
/***********************************************/
{

/**/myassert( objr != NULL );
    DebugMsg(("omf_write_record( command=%X, kill=%u )\n", objr->command, kill ));
    OmfRSeek( objr, 0 );
    jumpTable[ JUMP_OFFSET(objr->command) ] ( file_out, objr );
    if( kill ) {
        OmfKillRec( objr );
    }
}

static void RegList( const pobj_list *list, size_t len )
/******************************************************/
{
    size_t  i;

    for( i = 0; i < len; ++i ) {
        jumpTable[ JUMP_OFFSET( list[i].command ) ] = list[i].func;
    }
}

static void UnRegList( const pobj_list *list, size_t len )
/********************************************************/
{
}

void omf_GenMSInit( void )
/************************/
{
    RegList( myFuncs, NUM_FUNCS );
}

void omf_GenMSFini( void )
/************************/
{
    UnRegList( myFuncs, NUM_FUNCS );
}
