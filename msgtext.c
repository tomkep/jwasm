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
* Description:  handle/display (error) message texts
*
****************************************************************************/

#include <fcntl.h>

#include "globals.h"
#include "banner.h"

//#ifdef __WATCOMC__
//  #include <unistd.h>
//  #include <conio.h>
//  #include <process.h>
//#endif

#define USERESOURCES 0

#define MAXMSGSIZE 128

#if USERESOURCES
#include "win32.h"
typedef void * HRSRC;
typedef void * HGLOBAL;
WINBASEAPI HRSRC   WINAPI FindResource( void *, char *, uint_32 );
WINBASEAPI HGLOBAL WINAPI LoadResource( void *, HRSRC );
WINBASEAPI void *  WINAPI LockResource( HGLOBAL );
WINBASEAPI void    WINAPI WideCharToMultiByte( uint_32, uint_32, uint_16 *, uint_32, uint_16 *, uint_32, uint_32, uint_32 );

#else

#include "errmsg.h"

static const char usagex[] = {
#include "usage.h"
};

#undef pick
#define pick( code, string_eng, string_jap )  string_eng,

static const char * msgtexts[] = { usagex,
#include "msgtext.h"
};

#endif

static const char usage[] = {
"\nusage: JWasm [options] asm-file [options] [asm-file] ... [@env_var]\n"
"Run \"JWasm -?\" or \"JWasm -h\" for more info\n"
};

const char *FingerMsg[] = {
    banner1w( "Assembler", _JWASM_VERSION_ ),
    banner2( "1992" ),
    banner3,
    0
};

extern  int             trademark( void );

#ifdef __OSI__

extern char             *_Copyright;

#endif

#if 0 // ndef __UNIX__

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
}

char * MsgGet( int resourceid, char *buffer )
{
#if USERESOURCES
    static char sbuffer[MAXMSGSIZE];
    HRSRC hRsrc;
    HGLOBAL hRes;
    WORD * pId;
    int i;
    hRsrc = FindResource( NULL, MAKEINTRESOURCE(1 + (resourceid >> 4)), RT_STRING );
    if (hRsrc) {
        hRes = LoadResource( NULL, hRsrc );
        if (hRes) {
            pId = LockResource( hRes );
            for (i = resourceid % 16;i;i--)
                pId += *pId + 1;
            i = *pId++;
            if (buffer == NULL)
                buffer = sbuffer;
            WideCharToMultiByte( CP_ACP, 0, pId, i, buffer, MAXMSGSIZE, 0, 0 );
            if (i < MAXMSGSIZE)
                buffer[i] = 0;
            return( buffer );
        }
    }
#else
    if (resourceid < MSG_LAST) {
        if (buffer) {
            strncpy( buffer, msgtexts[resourceid], MAXMSGSIZE );
            return( buffer );
        } else
            return( (char *)msgtexts[resourceid] );
    }
#endif
    DebugMsg(("MsgGet(%u): Msg not found!!!\n", resourceid));
    sprintf(buffer, "Msg %u", resourceid);
    return( buffer );
}

void MsgPrintf( int resourceid )
{
    trademark();
    printf( MsgGet( resourceid, NULL ));
}

void MsgPrintf1( int resourceid, char *token )
{
    trademark();
    printf( MsgGet( resourceid, NULL ), token );
}
void MsgPrintUsage( void )
{
    trademark();
    printf("%s", usage);
}
