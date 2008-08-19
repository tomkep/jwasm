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


#include <fcntl.h>
#ifdef __WATCOMC__
#include <unistd.h>
#endif
#include <stdio.h>      /* for SEEK_SET, SEEK_CUR, SEEK_END */
#include <sys/stat.h>   /* _S_IREAD ... */
#include <string.h>
#include "memalloc.h"
#include "omfrec.h"
#include "genutil.h"
#include "myassert.h"
#include "omfio.h"


extern  void            WriteError( void );
#define Fatal(__x,__y)  WriteError()

#ifdef __UNIX__
#define OP_MODE         (O_RDWR | O_CREAT | O_TRUNC)
#define OP_PERM         (0666)
#else
#define OP_MODE         (O_RDWR | O_CREAT | O_TRUNC | O_BINARY)
#ifdef __WATCOMC__
#define OP_PERM         (S_IREAD | S_IWRITE)
#else
#define OP_PERM         (_S_IREAD | _S_IWRITE)
#endif
#endif

static void safeSeek( int fh, long offset, int mode ) {

    if( lseek( fh, offset, mode ) == -1 ) {
        Fatal( MSG_DISK_ERROR, "lseek" );
    }
}

static void safeWrite( int fh, const uint_8 *buf, size_t len ) {

    if( write( fh, buf, len ) != len ) {
        Fatal( MSG_DISK_ERROR, "write" );
    }
}

OBJ_WFILE *ObjWriteOpen( const char *filename ) {
/*********************************************/
    int         fh;
    OBJ_WFILE    *new;

    fh = open( filename, OP_MODE, OP_PERM );
    if( fh < 0 ) {
        return( NULL );
    }
    new = AsmAlloc( sizeof( *new ) + OBJ_BUFFER_SIZE );
    new->fh = fh;
    new->in_buf = 0;
    new->in_rec = 0;

    return( new );
}

void ObjWriteClose( OBJ_WFILE *obj ) {
/**********************************/
/**/myassert( obj != NULL );

    if( obj->in_rec ) {
        ObjWEndRec( obj );
    }
    close( obj->fh );
    AsmFree( obj );
}

void ObjWBegRec( OBJ_WFILE *obj, uint_8 command ) {
/***********************************************/
    uint_8  buf[3];

/**/myassert( obj != NULL && !obj->in_rec );

    buf[0] = command;
    buf[1] = 0;
    buf[2] = 0;
    safeWrite( obj->fh, buf, 3 );
    obj->in_rec = 1;
    obj->checksum = command;
    obj->in_buf = 0;
    obj->length = 0;
}

void ObjWFlushBuffer( OBJ_WFILE *obj ) {
/*******************************************/
    size_t  len_to_write;
    uint_8  checksum;
    uint_8  *p;

/**/myassert( obj != NULL );

    len_to_write = obj->in_buf;
    if( len_to_write == 0 )  return;
    checksum = obj->checksum;
    for( p = obj->buffer; p < obj->buffer + len_to_write; ++p ) {
        checksum += *p;
    }
    obj->checksum = checksum;
    obj->length += len_to_write;
    safeWrite( obj->fh, obj->buffer, len_to_write );
    obj->in_buf = 0;
}

void ObjWEndRec( OBJ_WFILE *obj ) {
/*******************************/
    uint_8  buf[2];
    uint_8  checksum;

/**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf > 0 ) {
        ObjWFlushBuffer( obj );
    }
    ++obj->length;                  /* add 1 for checksum byte */
    WriteU16( buf, obj->length );
    checksum = obj->checksum + buf[0] + buf[1];
    checksum = -checksum;
    safeWrite( obj->fh, &checksum, 1 );
        /* back up to length */
    safeSeek( obj->fh, - (int_32)obj->length - 2, SEEK_CUR );
    safeWrite( obj->fh, buf, 2 );                   /* write the length */
    safeSeek( obj->fh, + (int_32)obj->length, SEEK_CUR );
//    safeSeek( obj->fh, 0L, SEEK_END );       /* move to end of file again */
    obj->in_rec = 0;
}

void ObjWrite8( OBJ_WFILE *obj, uint_8 byte ) {
/*******************************************/
/**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf == OBJ_BUFFER_SIZE ) {
        ObjWFlushBuffer( obj );
    }
    obj->buffer[ obj->in_buf++ ] = byte;
}

void ObjWrite16( OBJ_WFILE *obj, uint_16 word ) {
/*********************************************/
/**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf >= OBJ_BUFFER_SIZE - 1 ) {
        ObjWFlushBuffer( obj );
    }
    WriteU16( obj->buffer + obj->in_buf, word );
    obj->in_buf += 2;
}

void ObjWrite32( OBJ_WFILE *obj, uint_32 dword ) {
/**********************************************/
/**/myassert( obj != NULL && obj->in_rec );

    if( obj->in_buf >= OBJ_BUFFER_SIZE - 3 ) {
        ObjWFlushBuffer( obj );
    }
    WriteU32( obj->buffer + obj->in_buf, dword );
    obj->in_buf += 4;
}

void ObjWriteIndex( OBJ_WFILE *obj, uint_16 index ) {
/*************************************************/
    if( index > 0x7f ) {
        ObjWrite8( obj, 0x80 | ( index >> 8 ) );
    }
    ObjWrite8( obj, index & 0xff );
}

void ObjWrite( OBJ_WFILE *obj, const uint_8 *buf, size_t length ) {
/***************************************************************/
    const uint_8    *write;
    size_t          amt;

/**/myassert( obj != NULL && buf != NULL );

    write = buf;
    for(;;) {
        amt = OBJ_BUFFER_SIZE - obj->in_buf;
        if( amt >= length ) {
            memcpy( &obj->buffer[ obj->in_buf ], write, length );
            obj->in_buf += length;
            break;
        } else if( amt > 0 ) {
            memcpy( &obj->buffer[ obj->in_buf ], write, amt );
            obj->in_buf += amt;
            write += amt;
            length -= amt;
        }
        ObjWFlushBuffer( obj );
    }
}

static uint_8 checkSum( const uint_8 *buf, uint_16 length ) {
/***********************************************************/
    uint_8 checksum;

    checksum = 0;
    while( length ) {
        checksum += *buf;
        ++buf;
        --length;
    }
    return( checksum );
}

void ObjWriteRec( OBJ_WFILE *obj, uint_8 command, uint_16 length,
    const uint_8 *contents ) {
/***************************************************************/
/*
    Contents and length don't include checksum
*/
    uint_8  buf[3];
    uint_8  checksum;

/**/myassert( obj != NULL && !obj->in_rec );

    checksum  = buf[0] = command;
    checksum += buf[1] = ( length + 1 ) & 0xff;
    checksum += buf[2] = ( length + 1 ) >> 8;
    safeWrite( obj->fh, buf, 3 );
    checksum += checkSum( contents, length );
    safeWrite( obj->fh, contents, length );
    checksum = -checksum;
    safeWrite( obj->fh, &checksum, 1 );
}

