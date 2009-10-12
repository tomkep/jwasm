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
* Description:  Diagnostics routines (errors/warnings/notes)
*
****************************************************************************/

#include <stdarg.h>
#include <ctype.h>

#include "globals.h"
#include "symbols.h"
#include "directiv.h"
#include "input.h"
#include "tokenize.h"
#include "symbols.h"
#include "queues.h"
#include "macro.h"
#include "fatal.h"
#include "msgtext.h"
#include "listing.h"
#include "segment.h"

extern char             *MsgGet( int resourceid, char *buffer );
extern void             print_source_nesting_structure( void );
extern char             banner_printed;

//    WngLvls[level] // warning levels associated with warning messages
//    CompFlags.errout_redirected

/* globals to this module */
#define ErrLimit Options.error_limit
#define WngLevel Options.warning_level

//static bool             Errfile_Written;

//static void             PrtMsg( int severity, int msgnum, va_list args1, va_list args2 );
//void                    PutMsg( FILE *fp, int severity, int msgnum, va_list args );

void AsmError( int msgnum )
/*************************/
{
    AsmErr( msgnum );
}

#ifdef DEBUG_OUT
void DoDebugMsg( const char *format, ... )
/****************************************/
{
    va_list args;
    if( !Options.debug ) return;

    if( ModuleInfo.cref == FALSE ) return;

    va_start( args, format );
    vprintf( format, args );
    va_end( args );
}
#endif

static void OpenErrFile( void )
/**********************/
{
//    if( !isatty( fileno( errout ) ) ) return;
    if( FileInfo.fname[ERR] != NULL ) {
        FileInfo.file[ERR] = fopen( FileInfo.fname[ERR], "w" );
    }
}

void PutMsg( FILE *fp, int severity, int msgnum, va_list args )
/********************************************************************/
{
    int             i,j;
    char           *type;
    char            msgbuf[MAXMSGSIZE];
    char            buffer[MAX_LINE_LEN];

    if( fp != NULL ) {

        MsgGet( msgnum, msgbuf );
        switch (severity ) {
        case 1:  type = MsgGetEx( MSG_FATAL_PREFIX );   break;
        case 2:  type = MsgGetEx( MSG_ERROR_PREFIX );   break;
        case 4:  type = MsgGetEx( MSG_WARNING_PREFIX ); break;
        default:  type = NULL; i = 0; break;
        }
        if ( type )
            i = sprintf( buffer, "%s A%4u: ", type, severity * 1000 + msgnum );
        i += vsprintf( buffer+i, msgbuf, args );
        //buffer[i] = NULLC;

        if ( severity && (j = GetCurrSrcPos( msgbuf ))) {
            fwrite( msgbuf, 1, j, fp );
        } else
            j = 0;
        fwrite( buffer, 1, i, fp );
        fwrite( "\n", 1, 1, fp );

        /* if in Pass 1, add the error msg to the listing */
        if ( FileInfo.file[LST] &&
             severity &&
             Parse_Pass == PASS_1 &&
             fp == FileInfo.file[ERR] ) {
            LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), 0 );
            LstPrintf( "                           %s", buffer );
            LstNL();
        }
    }
}

static void PrtMsg( int severity, int msgnum, va_list args1, va_list args2 )
/***************************************************************************/
{
    if( !banner_printed )
        trademark();

    if( FileInfo.file[ERR] == NULL )
        OpenErrFile();
    PutMsg( errout, severity, msgnum, args1 );
    fflush( errout );                       /* 27-feb-90 */
    if( FileInfo.file[ERR] ) {
        //Errfile_Written = TRUE;
        PutMsg( FileInfo.file[ERR], severity, msgnum, args2 );
    }
}

/* notes: "included by", "macro called from", ... */

void PrintNote( int msgnum, ... )
/*****************************/
{
    va_list args1, args2;

    va_start( args1, msgnum );
    va_start( args2, msgnum );

    PrtMsg( 0, msgnum, args1, args2 );
    va_end( args1 );
    va_end( args2 );
}

void AsmErr( int msgnum, ... )
/****************************/
{
    va_list args1, args2;

#ifdef DEBUG_OUT
    DebugCurrLine();
#endif
    va_start( args1, msgnum );
    va_start( args2, msgnum );
    PrtMsg( 2, msgnum, args1, args2 );
    va_end( args1 );
    va_end( args2 );
    ModuleInfo.error_count++;
    write_to_file = FALSE;
    print_source_nesting_structure();
    if( ErrLimit != -1  &&  ModuleInfo.error_count == ErrLimit+1 ) {
        PrtMsg( 2, TOO_MANY_ERRORS, args1, args2 );
        /* Just simulate the END directive, don't do a fatal exit!
         This allows to continue to assemble further modules.
         */
        ModuleInfo.EndDirectiveFound = TRUE;
    }
}

void AsmWarn( int level, int msgnum, ... )
/****************************************/
{
    va_list args1, args2;

    if( level <= WngLevel ) {
#ifdef DEBUG_OUT
        DebugCurrLine();
#endif
        va_start( args1, msgnum );
        va_start( args2, msgnum );
        if( !Options.warning_error ) {
            PrtMsg( 4, msgnum, args1, args2 );
            ModuleInfo.warning_count++;
        } else {
            PrtMsg( 2, msgnum, args1, args2 );
            ModuleInfo.error_count++;
        }
        va_end( args1 );
        va_end( args2 );
        print_source_nesting_structure();
    }
}

void InitErrFile( void )
/*********************/
{
    remove( FileInfo.fname[ERR] );
}

#ifndef NDEBUG

int InternalError( const char *file, unsigned line )
/**************************************************/
// it's used by myassert() function in debug version
{
    char buffer[MAX_LINE_LEN];
    DebugMsg(("InternalError enter\n"));
    ModuleInfo.error_count++;
    GetCurrSrcPos( buffer );
    fprintf( errout, "%s", buffer );
    fprintf( errout, MsgGetEx( INTERNAL_ERROR ), file, line );
    close_files();
    exit( EXIT_FAILURE );
    return(0);
}
#endif

