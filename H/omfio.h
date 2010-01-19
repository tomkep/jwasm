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
* Description:  OMF low-level file i/o
*
****************************************************************************/

#ifndef OMFIO_H
#define OMFIO_H 1
#include <stddef.h>
#include "omfrec.h"

typedef struct {
    FILE        *file;      /* file                                         */
    uint_16     length;     /* number of bytes written since rec header     */
    size_t      in_buf;     /* number of bytes in buffer                    */
    uint_8      checksum;   /* for current record                           */
    uint_8      in_rec : 1; /* a record is open                             */
    uint_8      buffer[1];  /* for writing                                  */
} OBJ_WFILE;

#define OBJ_BUFFER_SIZE 0x1000      /* 4k (must be less than 64k) */
#define OBJ_MAX_REC     0x1000      /* maximum record size (<64k) */

#if OBJ_MAX_REC > OBJ_BUFFER_SIZE
#error "OBJ_MAX_REC must be smaller than OBJ_BUFFER_SIZE"
#endif

extern OBJ_WFILE    *OmfWriteOpen( FILE *file );
extern void         OmfWriteClose( OBJ_WFILE *obj );

extern void         OmfWBegRec( OBJ_WFILE *obj, uint_8 command );
extern void         OmfWEndRec( OBJ_WFILE *obj );
extern void         OmfWrite8( OBJ_WFILE *obj, uint_8 byte );
extern void         OmfWrite16( OBJ_WFILE *obj, uint_16 word );
extern void         OmfWrite32( OBJ_WFILE *obj, uint_32 dword );
extern void         OmfWriteIndex( OBJ_WFILE *obj, uint_16 index );
extern void         OmfWrite( OBJ_WFILE *obj, const uint_8 *buffer, size_t len );
extern void         OmfWriteRec( OBJ_WFILE *obj, uint_8 command, size_t length, const uint_8 *contents );
//extern void         OmfFlushBuffer( OBJ_WFILE *out );
#endif
