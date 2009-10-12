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

#include "globals.h"
#include "tokenize.h"
#include "msgtext.h"

#define USERESOURCES 0 /* 1=use Win string resources, won't work for Linux! */

char banner_printed = FALSE;

#if USERESOURCES

/*
 If Win32 resource strings are to be used, the
 makefiles must contain a call of the resource compiler!
 Resource file is H/JWasm.rc.
 */

#include "win32.h"
typedef void * HRSRC;
typedef void * HGLOBAL;
WINBASEAPI HRSRC   WINAPI FindResource( void *, char *, uint_32 );
WINBASEAPI HGLOBAL WINAPI LoadResource( void *, HRSRC );
WINBASEAPI void *  WINAPI LockResource( HGLOBAL );
WINBASEAPI void    WINAPI WideCharToMultiByte( uint_32, uint_32, uint_16 *, uint_32, uint_16 *, uint_32, uint_32, uint_32 );

#else

#include "errmsg.h"

#undef pick
#define pick( code, string_eng, string_jap )  string_eng,
static const char * const msgtexts[] = {
#include "msgdef.h"
};

#endif

static const char usage[] = {
#include "usage.h"
};

ret_code MsgInit( void )
/**********************/
{
    return( NOT_ERROR );
}

void MsgFini( void )
/******************/
{
}

char * MsgGet( int msgid, char *buffer )
/**************************************/
{
#if USERESOURCES
    HRSRC hRsrc;
    HGLOBAL hRes;
    WORD * pId;
    int i;

    hRsrc = FindResource( NULL, MAKEINTRESOURCE(1 + (msgid >> 4)), RT_STRING );
    if (hRsrc) {
        hRes = LoadResource( NULL, hRsrc );
        if (hRes) {
            pId = LockResource( hRes );
            for (i = msgid % 16;i;i--)
                pId += *pId + 1;
            i = *pId++;
            if (buffer == NULL)
                buffer = StringBufferEnd;
            WideCharToMultiByte( CP_ACP, 0, pId, i, buffer, -1, 0, 0 );
            buffer[i] = NULLC;
            return( buffer );
        }
    }
#else
    if ( msgid < MSG_LAST ) {
        if (buffer) {
            strncpy( buffer, msgtexts[msgid], MAXMSGSIZE );
            return( buffer );
        } else
            return( (char *)msgtexts[msgid] );
    }
#endif
    DebugMsg(("MsgGet(%u): Msg not found!!!\n", msgid));
    if ( buffer == NULL )
        buffer = StringBufferEnd;
    sprintf( buffer, "Msg %u", msgid );
    return( buffer );
}

char *MsgGetEx( int msgid )
/*************************/
{
    return( MsgGet( msgid, NULL ) );
}

void MsgPrintf( int msgid )
/*************************/
{
    trademark();
    printf( MsgGet( msgid, NULL ));
}

#if 0
void MsgPrintf1( int msgid, char *token )
/***************************************/
{
    trademark();
    printf( MsgGet( msgid, NULL ), token );
}
#endif

void MsgPrintUsage( void )
/************************/
{
    const char *p;
    trademark();
    for ( p = usage; *p != '\n'; ) {
        const char *p2 = p +strlen(p) + 1;
        printf("%-20s %s\n", p, p2 );
        p = p2 + strlen(p2) + 1;
    }
}

char *MsgGetJWasmName( char * buffer )
/************************************/
{
    sprintf( buffer, MsgGet( MSG_JWASM, NULL ), _JWASM_VERSION_, __DATE__ );
    return( buffer );
}

int trademark( void )
/*******************/
{
    char buffer[128];
    if( banner_printed == FALSE ) {
        banner_printed = TRUE;
        printf( MsgGet( MSG_BANNER, NULL ), MsgGetJWasmName( buffer ) );
        return( 4 ); /* return number of lines printed */
    }
    return( 0 );
}

