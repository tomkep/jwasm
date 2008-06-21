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
* Description:  handle/display error messages
*
****************************************************************************/


#include "globals.h"

#include <fcntl.h>

#ifdef __WATCOMC__
  #include <unistd.h>
  #include <conio.h>
  #include <process.h>
#endif

#undef ERROR
#include "windows.h"

/*
 USEUSER32=1 will dynamically get address of LoadStringA() and
 use it to load string resources. USEUSER32=0 will avoid to use
 LoadStringA() - which is a USER32 function - and do string resource
 handling with KERNEL32 functions FindResourceA(), LoadResource(),
 LockResource and WideCharToMultiByte().
 */

#define USEUSER32 0

#if USEUSER32
static HINSTANCE hUser32 = NULL;
static int WINAPI (*pLoadString)(HINSTANCE, UINT, LPTSTR, int) = NULL;
#else
#endif

extern  int             trademark( void );
extern char             banner_printed;

#ifdef __OSI__

extern char             *_Copyright;

#endif

#ifndef __UNIX__

static const unsigned char PressReturn[] = {
"    (Press return to continue)"
};

static void con_output( const unsigned char *text )
{
    char c;

    do {
        c = *text;
        putchar( c );
        ++text;
    } while( *text );
    putchar( '\n' );
}
#endif

int MsgInit( void )
{
    return( 1 );
}

void MsgFini( void )
{
#if USEUSER32
    if (hUser32) {
        FreeLibrary(hUser32);
        hUser32 = NULL;
    }
#endif
}

int MsgGet( int resourceid, char *buffer )
{
#if USEUSER32
    if (pLoadString == NULL) {
        if (hUser32 = LoadLibrary("user32"))
            pLoadString = GetProcAddress(hUser32, "LoadStringA");
    }
    if (pLoadString) {
        if (pLoadString(NULL, resourceid, buffer, 128) == 0) {
            buffer[0] = '\0';
            return( 0 );
        }
    }
    return( 1 );
#else
    HRSRC hRsrc;
    HGLOBAL hRes;
    WORD * pId;
    int i;

    buffer[0] = '\0';
    hRsrc = FindResource(NULL, MAKEINTRESOURCE(1 + (resourceid >> 4)), RT_STRING);
    if (hRsrc) {
        hRes = LoadResource(NULL, hRsrc);
        if (hRes) {
            pId = LockResource(hRes);
            for (i = resourceid % 16;i;i--)
                pId += *pId + 1;
            i = *pId++;
            WideCharToMultiByte(CP_ACP, 0, pId, i, buffer, 128, 0, 0);
            if (i < 128)
                buffer[i] = 0;
            return( 1 );
        }
    }
    DebugMsg(("MsgGet(%u): Msg not found!!!\n", resourceid));
    return( 0 );
#endif
}

void MsgPrintf( int resourceid )
{
    char        msgbuf[128];

    if( !banner_printed ) {
        banner_printed = TRUE;
        trademark();
    }
    MsgGet( resourceid, msgbuf );
    printf( msgbuf );
}

void MsgPrintf1( int resourceid, char *token )
{
    char        msgbuf[128];

    if( !banner_printed ) {
        banner_printed = TRUE;
        trademark();
    }
    MsgGet( resourceid, msgbuf );
    printf( msgbuf, token );
}

#ifndef __UNIX__
static void Wait_for_return( void )
{
    if( isatty( fileno(stdout) ) ) {
        con_output( PressReturn );
        fflush( stdout );
        getch();
    }
}
#endif

#if 0
void PrintfUsage( int first_ln )
{
    char        msg_buff[128];
    unsigned    count;

    count = trademark();
#ifdef __OSI__
    if( _Copyright != NULL ) {
        puts( _Copyright );
        count += 1;
    }
#endif
    for( ;; first_ln++ ) {
#ifndef __UNIX__
        if( ++count >= 23 ) {
            Wait_for_return();
            count = 0;
        }
#endif
        MsgGet( first_ln, msg_buff );
        if( ( msg_buff[ 0 ] == '.' ) && ( msg_buff[ 1 ] == 0 ) )
            break;
        puts( msg_buff );
    }
}
#endif

