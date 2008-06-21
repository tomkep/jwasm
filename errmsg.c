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
* Description:  Diagnostics routines (errors/warnings/notes, listings)
*
****************************************************************************/


#if defined( _STANDALONE_ )

#include <stdarg.h>
#include <ctype.h>

#include "globals.h"
#include "fatal.h"
#include "symbols.h"
#include "directiv.h"
#include "input.h"
#include "symbols.h"
#include "queues.h"

extern void             MsgPrintf( int resourceid ); // don't use this
extern int              MsgGet( int resourceid, char *buffer );
extern int              trademark( void );
extern char             banner_printed;

extern uint_32          LastCodeBufSize;

void                    OpenErrFile( void );
void                    print_include_file_nesting_structure( void );

//    WngLvls[level] // warning levels associated with warning messages
//    CompFlags.errout_redirected

#ifdef __UNIX__
#define errout stderr
#else
#define errout stdout
#endif

/* globals to this module */
#define ErrLimit Options.error_limit
#define WngLevel Options.warning_level
#define ErrCount ModuleInfo.error_count
#define WngCount ModuleInfo.warning_count

static int              Errfile_Written = FALSE;
static FILE             *ErrFile;

static void             PutMsg( FILE *fp, char *prefix, int msgnum, va_list args );
static void             PrtMsg1( char *prefix, int msgnum, va_list args1, va_list args2 );

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

void AsmNote( int msgnum, ... )
/*****************************/
{
    va_list args1, args2;

    va_start( args1, msgnum );
    va_start( args2, msgnum );

    PrtMsg1( "Note!", msgnum, args1, args2 );
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
    PrtMsg1( "Error!", msgnum, args1, args2 );
    va_end( args1 );
    va_end( args2 );
    ++ErrCount;
    write_to_file = FALSE;
    print_include_file_nesting_structure();
    if( ErrLimit != -1  &&  ErrCount >= ErrLimit ) {
        PrtMsg1( "", ERR_TOO_MANY_ERRORS, args1, args2 );
        AsmShutDown();
        exit (1);
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
            PrtMsg1( "Warning!", msgnum, args1, args2 );
            ++WngCount;
        } else {
            PrtMsg1( "Error!", msgnum, args1, args2 );
            ++ErrCount;
        }
        va_end( args1 );
        va_end( args2 );
        print_include_file_nesting_structure();
    }
}

static void PrtMsg1( char *prefix, int msgnum, va_list args1, va_list args2 )
/***************************************************************************/
// print messages from WOMP !!!
{
    if( ErrFile == NULL ) OpenErrFile();
    PutMsg( errout, prefix, msgnum, args1 );
    fflush( errout );                       /* 27-feb-90 */
    if( ErrFile ) {
        Errfile_Written = TRUE;
        PutMsg( ErrFile, prefix, msgnum, args2 );
    }
}

void PrtMsg( int msgnum, ... )
/****************************/
// print messages from WOMP !!!
{
    va_list args1;

    if( !banner_printed ) {
        banner_printed = TRUE;
        trademark();
    }
    if( ErrFile == NULL )
        OpenErrFile();
    va_start( args1, msgnum );
    PutMsg( errout, "Warning!", msgnum, args1 );
    fflush( errout );
}

void InitErrFile( void )
/*********************/
{
    // fixme if( CompFlags.errout_redirected ) return;
    remove( AsmFiles.fname[ERR] );
    ErrFile = NULL;
    Errfile_Written = FALSE;
}

void OpenErrFile( void )
/**********************/
{
//    if( !isatty( fileno( errout ) ) ) return;
    if( AsmFiles.fname[ERR] != NULL ) {
        ErrFile = fopen( AsmFiles.fname[ERR], "w" );
    }
}

void LstMsg( const char *format, ... )
/************************************/
{
    va_list     args;

    if( AsmFiles.file[LST] ) {
        va_start( args, format );
        vfprintf( AsmFiles.file[LST], format, args );
        va_end( args );
    }
}

void OpenLstFile( void )
/**********************/
{
    if( AsmFiles.fname[LST] != NULL && Options.write_listing ) {
        AsmFiles.file[LST] = fopen( AsmFiles.fname[LST], "w" );
    }
}

void WriteLstFile( int type, unsigned int oldofs, void * value )
{
    unsigned int newofs;
    asm_sym * sym = value;
    int len;
    int idx;
    char * p;
    char buffer[128];

    if (ModuleInfo.list == FALSE || AsmFiles.file[LST] == NULL)
        return;
    if (type == LSTTYPE_LIDATA) {
        newofs = GetCurrAddr();
        len = sprintf(buffer, "%08X: ", oldofs);
        fwrite(buffer, 1, len, AsmFiles.file[LST]);

        len = 8;

        if (CurrSeg == NULL)
            return;

        if (CurrSeg->seg->e.seginfo->segtype == SEGTYPE_BSS) {
            while (oldofs < newofs && len) {
                sprintf(buffer, "%02X", 0);
                //                    sprintf(buffer, "??");
                fwrite(buffer, 1, 2, AsmFiles.file[LST]);
                oldofs++;
                len--;
            }
            goto nodump;
        }

        if (write_to_file == FALSE)
            goto nodump;

        /* OMF hold just a small buffer for one LEDATA record */
        /* if it has been flushed, use LastCodeBufSize */
        idx = (CurrSeg->seg->e.seginfo->current_loc - CurrSeg->seg->e.seginfo->start_loc)
            - (newofs - oldofs);
        if (Options.output_format == OFORMAT_OMF) {
            while (idx < 0 && len) {
                sprintf(buffer, "%02X", CurrSeg->seg->e.seginfo->CodeBuffer[idx+LastCodeBufSize]);
                fwrite(buffer, 1, 2, AsmFiles.file[LST]);
                idx++;
                oldofs++;
                len--;
            }
        } else if (idx < 0)
            idx = 0;

        while (oldofs < newofs && len) {
            sprintf(buffer, "%02X", CurrSeg->seg->e.seginfo->CodeBuffer[idx]);
            fwrite(buffer, 1, 2, AsmFiles.file[LST]);
            idx++;
            oldofs++;
            len--;
        }
    nodump:
        for (;len;len--)
            fwrite("  ", 1, 2, AsmFiles.file[LST]);

    } else if (type == LSTTYPE_EQUATE) {
        if (sym->state == SYM_INTERNAL) {
            sprintf(buffer, " = %-23X", sym->value);
            fwrite(buffer, 1, strlen(buffer), AsmFiles.file[LST]);
        } else if (sym->state == SYM_TMACRO) {
            sprintf(buffer, " = %-23.80s", sym->string_ptr);
            fwrite(buffer, 1, strlen(buffer), AsmFiles.file[LST]);
        }
    } else {
        if (type != LSTTYPE_MACRO && (CurrSeg || value))
            len = sprintf(buffer, "%08X:                 ", oldofs);
        else
            memset(buffer, ' ', 3*8+2);
        fwrite(buffer, 1, 3*8+2, AsmFiles.file[LST]);
    }

    fwrite(" ", 1, 1, AsmFiles.file[LST]);
    p = CurrString;
    while(isspace(*p)) p++;
    len = strlen(p);
    fwrite(p, 1, len, AsmFiles.file[LST]);
    fwrite("\n", 1, 1, AsmFiles.file[LST]);
    return;
    }

static void PutMsg( FILE *fp, char *prefix, int msgnum, va_list args )
/********************************************************************/
{
    const FNAME     *fname;
    char            msgbuf[MAX_LINE_LEN];

    if( fp != NULL ) {
        fname = get_curr_srcfile();
        if( LineNumber != 0 ) {
            if( fname != NULL ) {
                fprintf( fp, "%s(%lu): ", fname->name, LineNumber );
            }
        }
        fprintf( fp, "%s %c%03d: ", prefix, *prefix, msgnum );
        // CGetMsg( msgbuf, msgnum );
        MsgGet( msgnum, msgbuf );
        vfprintf( fp, msgbuf, args );
        fprintf( fp, "\n" );
#if 0
        if (AsmFiles.file[LST] && fp == ErrFile) {
            fprintf( AsmFiles.file[LST], "%s %c%03d: ", prefix, *prefix, msgnum );
            fprintf( AsmFiles.file[LST], msgbuf );
            fprintf( AsmFiles.file[LST], "\n" );
        }
#endif
    }
}

#ifndef NDEBUG

int InternalError( const char *file, unsigned line )
/**************************************************/
// it is used by WOMP myassert function in debug version
{

    char msgbuf[80];

    MsgGet( MSG_INTERNAL_ERROR, msgbuf );
    fprintf( errout, msgbuf, file, line );
    fflush( errout );
    AsmShutDown();
    exit( EXIT_FAILURE );
    return( 0 );
}
#endif

void WriteError( void )
/************************/
{
    MsgPrintf( OBJECT_WRITE_ERROR );
    AsmShutDown();
    exit( EXIT_FAILURE );
};

#endif
