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
* Description:  Memory allocation routines.
*
****************************************************************************/

/*
    if TRMEM is defined, trmem functions are used which will help tracking
    memory usage.
*/

#ifdef __WATCOMC__
    #include <malloc.h>
#else
    #include <stdlib.h>
#endif
#if defined(__UNIX__) && defined(__GNUC__)
    #include <sys/mman.h>
#endif

#include "globals.h"

// FASTMEM is a simple memory alloc approach which allocates chunks of 512 kB
// and will release it only at MemFini().
#if FASTMEM
 #define BLKSIZE 0x80000
 #ifndef __UNIX__
  #if defined(__OS2__)
   #include "os2.h"
  #elif defined(__DJGPP__)
   #include "dpmi.h"
  #else
   #include "win32.h"
  #endif
 #endif
#endif

#include "memalloc.h"
#include "fatal.h"

#ifdef TRMEM
#include <fcntl.h>
#ifdef __UNIX__
 #include <unistd.h>
#else
 #include <io.h>
#endif
#include <sys/stat.h>
#include "trmem.h"

static _trmem_hdl   memHandle;
static int          memFile;     /* file handle we'll write() to */
#ifdef DEBUG_OUT
static int memcalls;
#endif
#endif

#ifdef TRMEM
static void memLine( int *fh, const char *buf, unsigned size )
{
    write( 2, "***", 3 );
    write( 2, buf, size );
    if( *fh != -1 ) {
        write( *fh, buf, size );
    }
}
#endif

#if defined(__UNIX__) && defined(__WATCOMC__)

#define SYS_mmap                 90
#define SYS_munmap               91

uint_32 sys_call1( uint_32 func, uint_32 r_ebx );
#pragma aux sys_call1 =                         \
    "int    0x80"                               \
    parm [eax] [ebx]                            \
    value [eax];

uint_32 sys_call2( uint_32 func, uint_32 r_ebx, uint_32 r_ecx );
#pragma aux sys_call2 =                         \
    "int    0x80"                               \
    parm [eax] [ebx] [ecx]                      \
    value [eax];

typedef struct mmap {
    uint_32 base;   // linear base (or 0)
    uint_32 size;   // size in bytes
    uint_32 access; // prot_read + prot_write = 3
    uint_32 flags;  // 0x22 = map_anonymous + map_private
    uint_32 fd;     // should be -1
    uint_32 offset; // ignored
} mmap;
// 0x22 = MAP_PRIVATE | MAP_ANON
static mmap mymmap = {0, 0, 3, 0x22, -1, 0};
#endif
#if defined(__GNUC__)
uint_32 mymmap_size = 0;   // size in bytes
#endif

#if FASTMEM
static uint_8 *pBase; /* start list of 512 kB blocks */
static uint_8 *pCurr; /* points into current block */
static int currfree;  /* free memory left in current block */
static int blocks;    /* number of blocks allocated so far */
#endif

void MemInit( void )
/******************/
{
#ifdef TRMEM
    memFile = _open( "~jwasm.trk", O_WRONLY | O_CREAT | O_TRUNC, S_IREAD | S_IWRITE );
    memHandle = _trmem_open( malloc, free, realloc, _expand, &memFile, memLine,
        _TRMEM_ALLOC_SIZE_0 |
        _TRMEM_FREE_NULL |
        _TRMEM_OUT_OF_MEMORY |
        _TRMEM_CLOSE_CHECK_FREE
    );
    if( memHandle == NULL ) {
        exit( EXIT_FAILURE );
    }
#ifdef DEBUG_OUT
    memcalls = 0;
#endif
#endif
#if FASTMEM
    currfree = 0;
    blocks = 0;
    pBase = NULL;
#endif
}

void MemFini( void )
/******************/
{
#ifdef TRMEM
    if( memHandle != NULL ) {
        _trmem_prt_list( memHandle );
        _trmem_close( memHandle );
        if( memFile != -1 ) {
            _close( memFile );
        }
        memHandle = NULL;
    }
#endif

#if FASTMEM
#ifdef DEBUG_OUT
    if ( Options.quiet == FALSE )
        printf("memory used: %u kB\n", (blocks * BLKSIZE - currfree) / 1024);
#endif
    while (pBase) {
        uint_8 * pNext = *((uint_8 * *)pBase);
#ifndef __UNIX__
 #if defined(__OS2__)
        DosFreeMem( pBase );
 #elif defined(__NT__)
        VirtualFree( pBase, 0, MEM_RELEASE );
 #else
        free( pBase );
 #endif
#else
  #if defined(__WATCOMC__)
        sys_call2( SYS_munmap, (uint_32)pBase, 0 );
  #else
        munmap( (void *)pBase, 0 );
  #endif
#endif
        pBase = pNext;
    }
#endif
}

void *AsmAlloc( size_t size )
/***************************/
{
    void        *ptr;

#if FASTMEM
    size = (size + 3) & ~3;
    if (currfree < size) {
        DebugMsg(("AsmAlloc: new block, req. size=%Xh\n", size ));
        if (size > BLKSIZE-4) {
#ifndef __UNIX__
 #if defined(__OS2__)
            DosAllocMem( (void**)&pCurr, size+4, PAG_COMMIT|PAG_READ|PAG_WRITE);
 #elif defined(__NT__)
            pCurr = (uint_8 *)VirtualAlloc(NULL, size+4, MEM_COMMIT, PAGE_READWRITE);
 #else
            pCurr = malloc( size+4 );
 #endif
#else
 #if defined(__GNUC__)
            mymmap_size = size+4;
            pCurr = (char *)mmap( 0, mymmap_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0 );
            if ( pCurr == MAP_FAILED )
                pCurr = NULL;
 #else
            mymmap.size = size+4;
            pCurr = (char *)sys_call1( SYS_mmap, (uint_32)&mymmap);
 #endif
#endif
            currfree = size;
        } else {
#ifndef __UNIX__
 #if defined(__OS2__)
            DosAllocMem( (void **)&pCurr, BLKSIZE, PAG_COMMIT|PAG_READ|PAG_WRITE );
 #elif defined(__NT__)
            pCurr = (uint_8 *)VirtualAlloc(NULL, BLKSIZE, MEM_COMMIT, PAGE_READWRITE);
 #else
            pCurr = malloc( BLKSIZE );
 #endif
#else
 #if defined(__GNUC__)
            mymmap_size = BLKSIZE;
            pCurr = (char *)mmap( 0, mymmap_size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0 );
            if ( pCurr == MAP_FAILED )
                pCurr = NULL;
 #else
            mymmap.size = BLKSIZE;
            pCurr = (char *)sys_call1( SYS_mmap, (uint_32)&mymmap);
 #endif
#endif
            currfree = BLKSIZE - sizeof(uint_8 *);
        }
        if (!pCurr) {
            currfree = 0;
            Fatal( FATAL_OUT_OF_MEMORY );
        }
        *(uint_8 * *)pCurr = pBase;
        pBase = pCurr;
        pCurr += sizeof(uint_8 *);
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
        Fatal( FATAL_OUT_OF_MEMORY );
    }
#endif
    return( ptr );
}

#if FASTMEM==0
void AsmFree( void *ptr )
/***********************/
{
    if( ptr != NULL ) {
#ifdef TRMEM
        _trmem_free( ptr, _trmem_guess_who(), memHandle );
#else
        free( ptr );
#endif
    }
}
#endif

void *MemAlloc( size_t size )
/***************************/
{
    void        *ptr;
    ptr = malloc( size );
    if( ptr == NULL ) {
        Fatal( FATAL_OUT_OF_MEMORY );
    }
#ifdef TRMEM
    memcalls++;
    DebugMsg(("MemAlloc(%Xh)=%X cnt=%d\n", size, ptr, memcalls ));
#endif
    return( ptr );
}

void MemFree( void *ptr )
/***********************/
{
#ifdef TRMEM
    memcalls--;
    DebugMsg(("MemFree(%Xh) cnt=%d\n", ptr, memcalls ));
#endif
    free( ptr );
    return;
}

#if 0
void *MemRealloc( void *ptr, size_t size )
/****************************************/
{
    void *new;

    new = realloc( ptr, size );
    if( new == NULL && size != 0 ) {
        Fatal( FATAL_OUT_OF_MEMORY );
    }
    return( new );
}
#endif

