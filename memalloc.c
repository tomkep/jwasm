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
* Description:  Memory manipulation routines.
*
****************************************************************************/


/*
    if TRMEM is defined, trmem functions are used which will help tracking
    memory usage.
*/

// FAST is a simple memory alloc approach which allocates chunks of 1 MB
// and will release it only at MemFini()

#define FAST 1

#if FAST
#define BLKSIZE 0x80000
#define WIN32_LEAN_AND_MEAN 1
//#define __W32API_USE_DLLIMPORT__
#include <windows.h>
#endif

#include <stdlib.h>
#include <fcntl.h>
//#include <unistd.h>
#ifdef DEBUG_OUT
#include <stdio.h>
#endif

#include "memalloc.h"
#include "fatal.h"


#ifdef TRMEM
#include "trmem.h"

static _trmem_hdl   memHandle;
static int          memFile;     /* file handle we'll write() to */
#endif

#if TRMEM
static void memLine( int *fh, const char *buf, unsigned size )
{
    write( 2, "***", 3 );
    write( 2, buf, size );
    if( *fh != -1 ) {
        write( *fh, buf, size );
    }
}
#endif

#if FAST
BYTE * pBase;
BYTE * pCurr;
int blocks;
int currfree;
#endif

void MemInit( void )
{
#ifdef TRMEM
    memFile = open( "mem.trk", O_WRONLY | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR );
    memHandle = _trmem_open( malloc, free, realloc, _expand, &memFile, memLine,
        _TRMEM_ALLOC_SIZE_0 |
        _TRMEM_FREE_NULL |
        _TRMEM_OUT_OF_MEMORY |
        _TRMEM_CLOSE_CHECK_FREE
    );
    if( memHandle == NULL ) {
        exit( EXIT_FAILURE );
    }
#endif
#if FAST
    currfree = 0;
    blocks = 0;
    pBase = NULL;
#endif
}

void MemFini( void )
{
#ifdef TRMEM
    if( memHandle != NULL ) {
        _trmem_prt_list( memHandle );
        _trmem_close( memHandle );
        if( memFile != -1 ) {
            close( memFile );
        }
        memHandle = NULL;
    }
#endif
#if FAST
#ifdef DEBUG_OUT
    printf("memory used: %u kB\n", (blocks * BLKSIZE - currfree) / 1024);
#endif
    while (pBase) {
        BYTE * pNext = *((BYTE * *)pBase);
        VirtualFree(pBase, 0, MEM_RELEASE);
        pBase = pNext;
    }
#endif
}

void *AsmAlloc( size_t size )
{
    void        *ptr;

#if FAST
    size = (size + 3) & ~3;
    if (currfree < size) {
        if (size > BLKSIZE-4) {
            pCurr = VirtualAlloc(0, size+4, MEM_COMMIT, PAGE_READWRITE);
            currfree = size;
        } else {
            pCurr = VirtualAlloc(0, BLKSIZE, MEM_COMMIT, PAGE_READWRITE);
            currfree = BLKSIZE-sizeof(BYTE *);
        }
        if (!pCurr) {
            Fatal( MSG_OUT_OF_MEMORY );
        }
        *(BYTE * *)pCurr = pBase;
        pBase = pCurr;
        pCurr += sizeof(BYTE *);
        blocks++;
    }
    ptr = pCurr;
    pCurr += size;
    currfree -= size;
#else

#ifdef TRMEM
    ptr = _trmem_alloc( size, _trmem_guess_who(), memHandle );
#else
    ptr = malloc( size );
#endif
    if( ptr == NULL ) {
        Fatal( MSG_OUT_OF_MEMORY );
    }
#endif
    return( ptr );
}

void AsmFree( void *ptr )
{
#if FAST
    return;
#endif
    if( ptr != NULL ) {
#ifdef TRMEM
        _trmem_free( ptr, _trmem_guess_who(), memHandle );
#else
        free( ptr );
#endif
    }
}


void *MemAlloc( size_t size )
{
    void        *ptr;
    ptr = malloc( size );
    if( ptr == NULL ) {
        Fatal( MSG_OUT_OF_MEMORY );
    }
    return( ptr );
}

void MemFree( void *ptr )
{
    free( ptr );
    return;
}

void *MemRealloc( void *ptr, size_t size ) {
/****************************************/
    void *new;

    new = realloc( ptr, size );
    if( new == NULL && size != 0 ) {
        Fatal( MSG_OUT_OF_MEMORY );
    }
    return( new );
}
