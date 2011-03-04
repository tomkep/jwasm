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
* Description:  OMF output routines.
*
****************************************************************************/

#include <stddef.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "fixup.h"
#include "omfint.h"
#include "omfspec.h"
#include "myassert.h"
#include "queue.h"
#include "fatal.h"

typedef int (*pobj_filter)( OBJ_WFILE *out, obj_rec *objr );

typedef struct {
    uint_8      command;
    pobj_filter func;
} pobj_list;

OBJ_WFILE     *file_out;

#define WRITE_REC 0 /* 1=enable unbuffered record write */

static void safeWrite( FILE *file, const uint_8 *buf, size_t len )
/****************************************************************/
{
    if( fwrite( buf, 1, len, file ) != len )
        WriteError();
}

#if 0
/* this function was needed to reposition to the record's
 * length field for update. Now always the full record is
 * kept in the buffer until WEndRec().
 */
static void safeSeek( FILE *file, long offset, int mode )
/*******************************************************/
{
    if( fseek( file, offset, mode ) != 0 )
        SeekError();
}
#endif

/* start a buffered OMF record output */

static void WBegRec( OBJ_WFILE *out, uint_8 command )
/***************************************************/
{

    /**/myassert( out != NULL && !out->cmd );

    out->in_buf = 0;
    out->cmd = command;
}

/* WEndRec() finish a buffered record.
 * - calculate checksum
 * - store checksum behind buffer contents
 * - writes the contents of the buffer(cmd, length, contents, checksum)
 */

static void WEndRec( OBJ_WFILE *out )
/***********************************/
{
    uint_8  checksum;
    uint_8  *p;

    /**/myassert( out != NULL && out->cmd );

    out->reclen = out->in_buf + 1; /* add 1 for checksum byte */
    checksum = out->cmd + ( out->reclen & 0xff ) + (( out->reclen ) >> 8);
    for( p = out->buffer; p < out->buffer + out->in_buf; ) {
        checksum += *p++;
    }
    checksum = - checksum;
    *p = checksum; /* store chksum in buffer */
    /* write buffer + 4 extra bytes (cmd, length, chksum) */
    safeWrite( out->file, &out->cmd, out->in_buf + 4 );
    out->cmd = 0;
}

#if WRITE_REC
/* Write an OMF record: command, length, content, checksum.
 * Contents and length don't include checksum
 */
static void WriteRec( OBJ_WFILE *out, uint_8 command, size_t length, const uint_8 *contents )
/*******************************************************************************************/
{
    uint_8  buf[3];
    uint_8  checksum;
    const uint_8  *p;
    const uint_8  *p2;

    /**/myassert( out != NULL );

    /* get checksum. calculated from all bytes (including cmd and length) */
    checksum  = buf[0] = command;
    checksum += buf[1] = ( length + 1 ) & 0xff;
    checksum += buf[2] = ( length + 1 ) >> 8;
    for ( p = contents, p2 = contents + length; p < p2; ) {
        checksum += *p++;
    }
    checksum = -checksum;
    safeWrite( out->file, buf, 3 );
    safeWrite( out->file, contents, length );
    safeWrite( out->file, &checksum, 1 );
}
#endif

/* write a byte to the current record */

static void PutByte( OBJ_WFILE *out, uint_8 value )
/*************************************************/
{
    /**/myassert( out != NULL && out->cmd );

    out->buffer[ out->in_buf++ ] = value;
}

/* write an index - 1|2 byte(s) - to the current record */

static void PutIndex( OBJ_WFILE *out, uint_16 index )
/***************************************************/
{
    if( index > 0x7f ) {
        PutByte( out, 0x80 | ( index >> 8 ) );
    }
    PutByte( out, index & 0xff );
}

/* write a word to the current record */

static void PutWord( OBJ_WFILE *out, uint_16 value )
/**************************************************/
{
    /**/myassert( out != NULL && out->cmd );

    WriteU16( out->buffer + out->in_buf, value );
    out->in_buf += sizeof( uint_16 );
}

/* write a dword to the current record */

static void PutDword( OBJ_WFILE *out, uint_32 value )
/***************************************************/
{
    /**/myassert( out != NULL && out->cmd );

    WriteU32( out->buffer + out->in_buf, value );
    out->in_buf += sizeof( uint_32 );
}

/* write a byte sequence to the current record */

static void PutMem( OBJ_WFILE *out, const uint_8 *buf, size_t length )
/********************************************************************/
{
    const uint_8    *curr;
    size_t          amt;

    /**/myassert( out != NULL && buf != NULL );

    curr = buf;
    amt = OBJ_BUFFER_SIZE - out->in_buf;
    if( amt >= length ) {
        memcpy( &out->buffer[ out->in_buf ], curr, length );
        out->in_buf += length;
    } else {
        /* this "shouldn't happen". */
        AsmErr( INTERNAL_ERROR, "omfint" );
    }
}

/*--------------------------------------------------------*/

/* For 16-bit records which are the same under Intel and MS OMFs */

static int writeMisc( OBJ_WFILE *out, obj_rec *objr )
/***************************************************/
{
    /**/myassert( objr != NULL );
    /**/myassert( objr->data != NULL );

#if WRITE_REC
    WriteRec( out, objr->command, objr->length, objr->data );
#else
    WBegRec( out, objr->command );
    PutMem( out, objr->data, objr->length );
    WEndRec( out );
#endif
    return( 0 );
}

/* For 32-bit records which are the same under Intel and MS OMFs */

static int writeMisc32( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{

    /**/myassert( objr != NULL );
    /**/myassert( objr->data != NULL );

    if( objr->is_32 ) {
        objr->command |= 0x01;
    }
#if WRITE_REC
    WriteRec( out, objr->command, objr->length, objr->data );
#else
    WBegRec( out, objr->command );
    PutMem( out, objr->data, objr->length );
    WEndRec( out );
#endif
    return( 0 );
}

static int writeComent( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    /**/myassert( objr != NULL );
    /**/myassert( objr->data != NULL );

    WBegRec( out, CMD_COMENT );
    PutByte( out, objr->d.coment.attr );
    PutByte( out, objr->d.coment.class );
    PutMem( out, objr->data, objr->length );
    WEndRec( out );
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
    WBegRec( out, is32 ? CMD_SEGD32 : CMD_SEGDEF );

    /* ACBP: bits=AAACCCBP
     * AAA=alignment
     * CCC=combination
     * B=big
     * P=32bit
     */
    acbp = ( objr->d.segdef.combine << 2 ) | ( objr->d.segdef.use_32 != 0 );
    align = objr->d.segdef.align;
    switch( align ) {
    case SEGDEF_ALIGN_ABS:      acbp |= ALIGN_ABS << 5;     break;
    case SEGDEF_ALIGN_BYTE:     acbp |= ALIGN_BYTE << 5;    break;
    case SEGDEF_ALIGN_WORD:     acbp |= ALIGN_WORD << 5;    break;
    case SEGDEF_ALIGN_PARA:     acbp |= ALIGN_PARA << 5;    break;
    case SEGDEF_ALIGN_PAGE:     acbp |= ALIGN_PAGE << 5;    break;
    case SEGDEF_ALIGN_DWORD:    acbp |= ALIGN_DWORD << 5;   break;
#if PAGE4K
    case SEGDEF_ALIGN_4KPAGE:
        acbp |= ALIGN_4KPAGE << 5;
        if ( Parse_Pass == PASS_1 )
            AsmWarn( 2, NO_4KPAGE_ALIGNED_SEGMENTS );
        break;
#endif
    default:
        /**/never_reach();
    }
    /* set BIG bit. should also be done for 32-bit segments
     * if their size is exactly 4 GB. Currently JWasm won't
     * support segments with size 4 GB.
     */
    if( is32 == 0 && objr->d.segdef.seg_length == 0x10000 ) {
        acbp |= 0x02;
    }

    /* the segdef record is small (16bit: size 6 - 9 ):
     * - byte acbp
     * - word (32bit:dword) length
     * - index seg name
     * - index class name
     * - index ovl name
     * ABS segdefs are 3 bytes longer
    */

    PutByte( out, acbp );
    if( align == SEGDEF_ALIGN_ABS ) {
        /* absolut segment has frame=word and offset=byte
         * it isn't fixupp physical reference
         * and don't depend on segment size (16/32bit)
         */
        PutWord( out, objr->d.segdef.abs.frame );
        PutByte( out, objr->d.segdef.abs.offset );
    }
    if( is32 ) {
        PutDword( out, objr->d.segdef.seg_length );
    } else {
        PutWord( out, objr->d.segdef.seg_length & 0xffff );
    }

    PutIndex( out, objr->d.segdef.seg_name_idx );
    PutIndex( out, objr->d.segdef.class_name_idx );
    PutIndex( out, objr->d.segdef.ovl_name_idx );
    //if( objr->d.segdef.access_valid ) {
    //    AsmError( ACCESS_CLASSES_NOT_SUPPORTED );
    //}
    WEndRec( out );
    return( 0 );
}

static int writeFixup( OBJ_WFILE *out, obj_rec *objr )
/****************************************************/
{
    struct fixup    *curr;
    size_t          written;
    uint_8          buf[ 1024 ];

    /**/myassert( objr != NULL );
    /**/myassert( objr->command == CMD_FIXUP );

    curr = objr->d.fixup.fixup;
    /* make sure FIXUP records won't go beyond the 1024 size limit */
    do {
        WBegRec( out, objr->is_32 ? CMD_FIXU32 : CMD_FIXUP );
        written = 0;
        while( curr != NULL && written < 1020 - FIX_GEN_MAX ) {
            written += OmfFixGenFix( curr, buf + written, objr->is_32 ? FIX_GEN_MS386 : FIX_GEN_INTEL );
            curr = curr->nextrlc;
        }
        PutMem( out, buf, written );
        WEndRec( out );
    } while( curr != NULL );
    return( 0 );
}

/*
 * Write LEDATA or LIDATA record.
 * the overhead is:
 * - 1 byte cmd
 * - 2 byte len
 * - 1/2 bytes segment index
 * - 2/4 bytes starting offset
 * - 1 byte chksum
 * so the data size "should" not exceed 1024-10 = 1014
 *
 * For LIDATA, the structure is equal.
 * The structure of the data block differs, however:
 * - 2/4: repeat count
 * - 2: block count
 * - content
 */

static int writeLedata( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    /**/myassert( objr != NULL );
    /**/myassert( objr->command == CMD_LEDATA || objr->command == CMD_LIDATA );

    WBegRec( out, objr->is_32 ? objr->command | 1: objr->command );
    PutIndex( out, objr->d.ledata.idx );
    if( objr->is_32 ) {
        PutDword( out, objr->d.ledata.offset );
    } else {
        PutWord( out, objr->d.ledata.offset );
    }
    PutMem( out, objr->data, objr->length );
    WEndRec( out );
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
    int     is32 = FALSE;
    uint_8  is_log;
    uint_8  mtype;
    uint_8  buf[ FIX_GEN_MAX ];

    /**/myassert( objr != NULL );
    /**/myassert( objr->command == CMD_MODEND );

    /* first byte is Module Type:
     * bit 7: 1=main program module
     * bit 6: 1=contains start address
     * bit 5: Segment bit
     * bit 1-4: must be 0
     * bit 0: start address subfield contains relocatable (logical) address
     */
    if ( objr->is_32 && objr->d.modend.start_addrs )
        is32 = TRUE;
    WBegRec( out, is32 ? CMD_MODE32 : CMD_MODEND );
    mtype = objr->d.modend.main_module ? 0x80 : 0;
    if( objr->d.modend.start_addrs ) {
        is_log = objr->d.modend.is_logical;
        mtype |= 0x40 | is_log;
        PutByte( out, mtype );
        len = OmfFixGenRef( &objr->d.modend.ref, is_log, buf,
            is32 ? FIX_GEN_MS386 : FIX_GEN_INTEL );
        PutMem( out, buf, len );
    } else
        PutByte( out, mtype );

    WEndRec( out );

    return( 0 );
}

static void writeBase( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    size_t grp_idx;
    size_t seg_idx;

    grp_idx = objr->d.base.grp_idx;
    seg_idx = objr->d.base.seg_idx;
    PutIndex( out, grp_idx );
    PutIndex( out, seg_idx );
    if( grp_idx == 0 && seg_idx == 0 ) {
        PutWord( out, objr->d.base.frame );
    }
}

/* COMDATs are initialized communal data records.
 * This isn't used currently, the Masm COMM directive
 * defines uninitialized communal data items (COMDEFs).
 */

static int writeComdat( OBJ_WFILE *out, obj_rec *objr )
/*****************************************************/
{
    int         is_32;

    /**/myassert( objr != NULL );
    /**/myassert( objr->command == CMD_COMDAT );

    is_32 = objr->is_32;
    /* write CMD_COMDAT or CMD_COMD32 */
    WBegRec( out, objr->command | ( is_32 ? 1 : 0 ) );
    PutByte( out, objr->d.comdat.flags );
    PutByte( out, objr->d.comdat.attributes );
    PutByte( out, objr->d.comdat.align );
    if( is_32 ) {
        PutDword( out, objr->d.comdat.offset );
    } else {
        PutWord( out, objr->d.comdat.offset );
    }
    PutIndex( out, objr->d.comdat.type_idx );
    if( ( objr->d.comdat.attributes & COMDAT_ALLOC_MASK ) == COMDAT_EXPLICIT ) {
        writeBase( out, objr );
    }
    PutIndex( out, objr->d.comdat.public_name_idx );
    /* record is already in ms omf format */
    PutMem( out, objr->data, objr->length );
    WEndRec( out );
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
    WBegRec( out, objr->command | ( is32 ? 1 : 0 ) );
    writeBase( out, objr );
    pubdata = objr->d.pubdef.pubs;
    if( pubdata != NULL ) {
        pubstop = pubdata + objr->d.pubdef.num_pubs;
        while( pubdata < pubstop ) {
            name = pubdata->name;
            name_len = strlen( name );
            PutByte( out, name_len );
            PutMem( out, (uint_8 *)name, (size_t)name_len );
            if( is32 ) {
                PutDword( out, pubdata->offset );
            } else {
                PutWord( out, pubdata->offset );
            }
            PutIndex( out, pubdata->type.idx );
            ++pubdata;
        }
    }
    WEndRec( out );
    return( 0 );
}

static void writeLinnumData( OBJ_WFILE *out, obj_rec *objr )
/**********************************************************/
{
    int is32;

    is32 = objr->is_32;
#if defined( __BIG_ENDIAN__ )
    {
        linnum_data *cur;
        linnum_data *stop;

        cur = objr->d.linnum.lines;
        stop = cur + objr->d.linnum.num_lines;
        while( cur < stop ) {
            PutWord( out, cur->number );
            if( is32 ) {
                PutDword( out, cur->offset );
            } else {
                PutWord( out, (uint_16)cur->offset );
            }
            ++cur;
        }
    }
#else
    if( is32 ) {
        PutMem( out, (uint_8 *)objr->d.linnum.lines,
            6 * objr->d.linnum.num_lines );
/**/    myassert( sizeof( linnum_data ) == 6 );
    } else {
        linnum_data *cur;
        linnum_data *stop;

        cur = objr->d.linnum.lines;
        stop = cur + objr->d.linnum.num_lines;
        while( cur < stop ) {
            PutWord( out, cur->number );
/**/        myassert( ( cur->offset & 0xffff0000 ) == 0 );
            PutWord( out, (uint_16)cur->offset );
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

    WBegRec( out, objr->is_32 ? CMD_LINN32 : CMD_LINNUM );
    writeBase( out, objr );
    writeLinnumData( out, objr );
    WEndRec( out );
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

    WBegRec( out, objr->is_32 ? CMD_LINS32 : CMD_LINSYM );
    PutByte( out, objr->d.linsym.d.linsym.flags );
    PutIndex( out, objr->d.linsym.d.linsym.public_name_idx );
    writeLinnumData( out, objr );
    WEndRec( out );
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
    { CMD_LIDATA,       writeLedata },  /* 0xa2 not used */
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

#define JUMP_OFFSET(cmd)    ( ( ( cmd ) - CMD_MIN_CMD ) >> 1 )

static pobj_filter      jumpTable[ ( CMD_MAX_CMD - CMD_MIN_CMD + 2 ) >> 1 ];

/* call a function - bit 0 of command is always 0 */

void omf_write_record( obj_rec *objr )
/************************************/
{

    /**/myassert( objr != NULL && objr->command >= CMD_MIN_CMD && objr->command <= CMD_MAX_CMD + 1 );
    DebugMsg(("omf_write_record( %p ) cmd=%X data=%p length=%u\n", objr, objr->command, objr->data, objr->length ));
    objr->curoff = 0;
    jumpTable[ JUMP_OFFSET(objr->command) ] ( file_out, objr );
}

static void RegList( const pobj_list *list, size_t len )
/******************************************************/
{
    size_t  i;

    for( i = 0; i < len; ++i ) {
        jumpTable[ JUMP_OFFSET( list[i].command ) ] = list[i].func;
    }
}

#if 0
static void UnRegList( const pobj_list *list, size_t len )
/********************************************************/
{
}
#endif

static OBJ_WFILE *WriteOpen( FILE *file )
/***************************************/
{
    OBJ_WFILE    *out;

    out = AsmAlloc( sizeof( *out ) + OBJ_BUFFER_SIZE );
    out->file = file;
    out->cmd = 0;

    return( out );
}

static void WriteClose( OBJ_WFILE *out )
/**************************************/
{

    /**/myassert( out != NULL );

    /* this function is called from inside close_files(),
     * which in turn is called on fatal errors. Therefore don't
     * access object module file if write_to_file is FALSE!
     */
    /* v2.04: removed. if there's something in the output buffer,
     * then a fatal error has occured and the object file will be
     * deleted.
     */
    //if( out->in_rec && write_to_file )
    //    OmfWEndRec( out );
    out->file = NULL;
    AsmFree( out );
}

void omf_intInit( void )
/**********************/
{
    RegList( myFuncs, NUM_FUNCS );
    file_out = WriteOpen( AsmFile[OBJ] );
}

void omf_intFini( void )
/**********************/
{
    if ( file_out ) {
        WriteClose( file_out );
        file_out = NULL;
    }
    //UnRegList( myFuncs, NUM_FUNCS );
}
