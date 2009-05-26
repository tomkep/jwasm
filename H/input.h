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
* Description:  prototypes for input queueing/processing procedures
*
****************************************************************************/


#ifndef _INPUT_H_INCLUDED
#define _INPUT_H_INCLUDED

extern void     PushLineQueue( void );
extern void     AddLineQueue( char *line );
extern void     AddMacroLineQueue( char *line );
extern ret_code InputQueueFile( char *path, FILE * *pfile );
extern char     *GetTextLine( char *string, int max );
extern void     PushMacro( struct asm_sym *sym );
extern void     PushMacroGoto( struct asm_sym *sym, int lineno );
extern void     AddStringToIncludePath( char *string );
extern void     InputInit( void );
extern void     InputPassInit( void );
extern void     InputFini( void );
extern int      GetPreprocessedLine( char * );
extern int      GetCurrSrcPos( char * );
extern void     ClearFileStack( void );
extern uint     get_curr_srcfile( void );
extern const FNAME  *GetFName( uint );

typedef struct line_list {
    struct line_list    *next;
    char line[];
} line_list;

typedef struct input_queue {
    struct line_list    *head;
    struct line_list    *tail;
} input_queue;

extern input_queue *line_queue;
extern int queue_level;

#endif
