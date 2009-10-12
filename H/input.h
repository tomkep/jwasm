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

extern uint_32 GetLineNumber( asm_sym * );
#define LineNumber GetLineNumber( NULL )

extern void     PushLineQueue( void );
//extern void     PopLineQueue( void );
extern void     AddLineQueue( const char *line );
extern ret_code InputQueueFile( const char *path, FILE * *pfile );
extern char     *GetTextLine( char *buffer, int max );
extern void     PushMacro( struct asm_sym *sym );
extern void     PushMacroGoto( struct asm_sym *sym, int lineno );
extern void     AddStringToIncludePath( const char *string );
extern void     InputInit( void );
extern void     InputPassInit( void );
extern void     InputFini( void );
extern int      GetPreprocessedLine( char * );
extern int      GetCurrSrcPos( char * );
extern void     ClearFileStack( void );
extern uint     get_curr_srcfile( void );
extern void     set_curr_srcfile( uint, uint_32 );
extern const FNAME  *GetFName( uint );
#ifdef DEBUG_OUT
extern uint_32  GetTopLine( void );
#endif

typedef struct line_list {
    struct line_list    *next;
    char line[];
} line_list;

typedef struct input_queue {
    struct line_list    *head;
    struct line_list    *tail;
} input_queue;

/* for line numbers, the source files have to be stored
 * in a list in the very same order as they appear in
 * the input stream.
 */
struct file_seq {
    struct file_seq *next;
    uint_16         file;           /* index of file in FNames */
};

extern input_queue *line_queue;
extern int queue_level;

#endif
