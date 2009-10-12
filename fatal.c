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
* Description:  Fatal errors processing
*
****************************************************************************/

#include <stdarg.h>

#include "globals.h"
#include "memalloc.h"
#include "fatal.h"
#include "symbols.h"
#include "directiv.h"
#include "input.h"
#include "msgtext.h"

extern void PutMsg( FILE *fp, int severity, int msgnum, va_list args );

typedef void (*err_act)( void );

typedef struct {
    short     message;        // message displayed
    uint_8    num;            // arguments
    uint_8    ret;            // exit code
    err_act   action;         // function to call, if any
} Msg_Struct;

static const Msg_Struct Fatal_Msg[] = {
#undef fix
#define fix( cmd, argc, act, ret )     { cmd, argc, ret, act },
#include "fatalmsg.h"
};

// fatal error (out of memory, unable to open files for write, ...)
// don't use functions which need to alloc memory here!

void Fatal( unsigned msg, ... )
/*****************************/
{
    va_list     args;
    //int         i;
    //const FNAME *fname;

    //MsgPrintf( MSG_FATAL_ERROR );
    //if ( fname = GetFName(get_curr_srcfile()) )
    //    printf(" (%s,%d)", fname->name, LineNumber);
    //printf(": ");

    va_start( args, msg );

    if ( msg >= FATAL_LAST ) {
        DebugMsg(("Fatal: unknown fatal msg %u\n", msg ));
        exit(-1);
    }

    PutMsg( stdout, 1, Fatal_Msg[msg].message, args );

    va_end( args );

    ModuleInfo.error_count++;
    write_to_file = FALSE;

    /* run exit code */
    if( Fatal_Msg[msg].action != NULL ) {
        Fatal_Msg[msg].action();
    }
    exit( Fatal_Msg[msg].ret );
}

void SeekError( void )
/********************/
{
    DebugMsg(("SeekError occured\n"));
    Fatal( FATAL_FILE_SEEK_ERROR, FileInfo.fname[OBJ], errno );
};

void WriteError( void )
/*********************/
{
    DebugMsg(("WriteError occured\n"));
    Fatal( FATAL_FILE_WRITE_ERROR, FileInfo.fname[OBJ], errno );
};

