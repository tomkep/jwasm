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
* Description:  OMF (Object Module Format) I/O.
*
****************************************************************************/

#include "globals.h"
#include "memalloc.h"
#include "omfrec.h"
#include "myassert.h"
#include "omfio.h"
#include "fatal.h"

OBJ_WFILE *OmfWriteOpen( FILE *file )
/***********************************/
{
    OBJ_WFILE    *new;

    new = AsmAlloc( sizeof( *new ) + OBJ_BUFFER_SIZE );
    new->file = file;
    new->in_buf = 0;
    new->in_rec = FALSE;

    return( new );
}

void OmfWriteClose( OBJ_WFILE *obj )
/**********************************/
{

    /**/myassert( obj != NULL );

    /* this function is called from inside close_files(),
     * which in turn is called on fatal errors. Therefore don't
     * access object module file if write_to_file is FALSE!
     */
    if( obj->in_rec && write_to_file ) {
        OmfWEndRec( obj );
    }
    obj->file = NULL;
    AsmFree( obj );
}

static void safeWrite( FILE *file, const uint_8 *buf, size_t len )
/****************************************************************/
{
    if( fwrite( buf, 1, len, file ) != len )
        WriteError();
}

/* start to write an OMF record */

void OmfWBegRec( OBJ_WFILE *obj, uint_8 command )
/***********************************************/
{
    uint_8  buf[3];

/**/myassert( obj != NULL && !obj->in_rec );

    buf[0] = command;
    buf[1] = 0;
    buf[2] = 0;
    safeWrite( obj->file, buf, 3 );
    obj->in_rec = TRUE;
    obj->checksum = command;
    obj->in_buf = 0;
    obj->length = 0;
}

static void OmfFlushBuffer( OBJ_WFILE *obj )
/******************************************/
{
    uint_8  checksum;
    uint_8  *p;

/**/myassert( obj != NULL );

    if( obj->in_buf == 0 )
        return;
    checksum = obj->checksum;
    for( p = obj->buffer; p < obj->buffer + obj->in_buf; ++p ) {
        checksum += *p;
    }
    obj->checksum = checksum;
    obj->length += obj->in_buf;
    safeWrite( obj->file, obj->buffer, obj->in_buf );
    obj->in_buf = 0;
}

static void safeSeek( FILE *file, long offset, int mode )
/*******************************************************/
{
    if( fseek( file, offset, mode ) != 0 )
        SeekError();
}

/* OmfWEndRec() finishes a record.
 * - writes the contents of the buffer
 * - writes the checksum byte
 * - writes record length at start+1
 * - repositions to end of record
 */

void OmfWEndRec( OBJ_WFILE *obj )
/*******************************/
{
    uint_8  buf[2];
    uint_8  checksum;

/**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf > 0 ) {
        OmfFlushBuffer( obj );
    }
    ++obj->length;                   /* add 1 for checksum byte */
    WriteU16( buf, obj->length );
    checksum = obj->checksum + buf[0] + buf[1];
    checksum = -checksum;
    safeWrite( obj->file, &checksum, 1 );
    /* back up to length */
    safeSeek( obj->file, - (int_32)obj->length - 2, SEEK_CUR );
    safeWrite( obj->file, buf, 2 );  /* write the length */
    safeSeek( obj->file, + (int_32)obj->length, SEEK_CUR );
    obj->in_rec = FALSE;
}

/* the following code checks if the max buffer size (4096) is reached.
 * However, this never happens in JWasm - no record will exceed
 * the 1024 OMF record maximum.
 */

void OmfWrite8( OBJ_WFILE *obj, uint_8 value )
/********************************************/
{
/**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf == OBJ_BUFFER_SIZE ) {
        OmfFlushBuffer( obj );
    }
    obj->buffer[ obj->in_buf++ ] = value;
}

void OmfWrite16( OBJ_WFILE *obj, uint_16 value )
/**********************************************/
{
    /**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf >= OBJ_BUFFER_SIZE - 1 ) {
        OmfFlushBuffer( obj );
    }
    WriteU16( obj->buffer + obj->in_buf, value );
    obj->in_buf += sizeof( uint_16 );
}

void OmfWrite32( OBJ_WFILE *obj, uint_32 value )
/**********************************************/
{
    /**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf >= OBJ_BUFFER_SIZE - 3 ) {
        OmfFlushBuffer( obj );
    }
    WriteU32( obj->buffer + obj->in_buf, value );
    obj->in_buf += sizeof( uint_32 );
}

void OmfWriteIndex( OBJ_WFILE *obj, uint_16 index )
/*************************************************/
{
    if( index > 0x7f ) {
        OmfWrite8( obj, 0x80 | ( index >> 8 ) );
    }
    OmfWrite8( obj, index & 0xff );
}

void OmfWrite( OBJ_WFILE *obj, const uint_8 *buf, size_t length )
/***************************************************************/
{
    const uint_8    *curr;
    size_t          amt;

    /**/myassert( obj != NULL && buf != NULL );

    curr = buf;
    for(;;) {
        amt = OBJ_BUFFER_SIZE - obj->in_buf;
        if( amt >= length ) {
            memcpy( &obj->buffer[ obj->in_buf ], curr, length );
            obj->in_buf += length;
            break;
        } else if( amt > 0 ) {
            memcpy( &obj->buffer[ obj->in_buf ], curr, amt );
            obj->in_buf += amt;
            curr += amt;
            length -= amt;
        }
        OmfFlushBuffer( obj );
    }
}

static uint_8 getcheckSum( const uint_8 *buf, uint_16 length )
/************************************************************/
{
    uint_8 checksum;

    checksum = 0;
    while( length ) {
        checksum += *buf;
        ++buf;
        --length;
    }
    return( checksum );
}

/* Write an OMF record: command, length, content, checksum.
 * Contents and length don't include checksum
 */
void OmfWriteRec( OBJ_WFILE *obj, uint_8 command, size_t length, const uint_8 *contents )
/***************************************************************************************/
{
    uint_8  buf[3];
    uint_8  checksum;

/**/myassert( obj != NULL && !obj->in_rec );

    checksum  = buf[0] = command;
    checksum += buf[1] = ( length + 1 ) & 0xff;
    checksum += buf[2] = ( length + 1 ) >> 8;
    safeWrite( obj->file, buf, 3 );
    checksum += getcheckSum( contents, length );
    safeWrite( obj->file, contents, length );
    checksum = -checksum;
    safeWrite( obj->file, &checksum, 1 );
}

