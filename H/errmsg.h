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
* Description:  Assembler message output interface.
*
****************************************************************************/


#ifndef _ERRMSG_H_INCLUDED
#define _ERRMSG_H_INCLUDED

#define ASMFAR

#ifdef DEBUG_OUT
    extern void DoDebugMsg( const char *format, ... );
    #define DebugMsg( x ) DoDebugMsg x
#else
    #define DebugMsg( x )
#endif
// use DebugMsg((....)) to call it

#define         AsmWarning( errno )             AsmWarn( 0,errno )

extern void AsmError( int msgnum );
extern void AsmErr( int msgnum, ... );
extern void AsmWarn( int level, int msgnum, ... );
extern void AsmNote( int msgnum, ... );

#if DEBUG_OUT
    #define DebugCurrLine() printf( "%s\n", CurrString );
    #define AsmIntErr( x ) DebugCurrLine(); printf( "Internal error = %d\n", x )
#else
    #define DebugCurrLine()
    #define AsmIntErr( x ) printf( "Internal error = %d\n", x )
#endif

#if 0

#define MSG_JWASM_RC_BASE   1
#include "msg.gh"

#else

#undef pick
#define pick( code, string_eng, string_jap )  code,

enum msgno {
    MSG_USAGE = 0,
#include "msgtext.h"
    MSG_LAST
};
#endif

#define MAX_RESOURCE_SIZE   128

extern int  MsgInit( void );
extern void MsgFini( void );
extern char * MsgGet( int, char * );

#endif
