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

/* item of a line queue */
struct line_list {
    struct line_list *next;
#ifdef DEBUG_OUT
    char lineno;
#endif
    char line[1];
};

struct macro_instance {
    struct srcline *currline;
    struct srcline *startline;
    uint_32 localstart;
    char * *parm_array;
    uint parmcnt;
};

struct input_queue {
    struct line_list    *head;
    struct line_list    *tail;
};

/* for line numbers, the source files have to be stored
 * in a list in the very same order as they appear in
 * the input stream.
 */
struct file_seq {
    struct file_seq *next;
    uint_16         file;           /* index of file in FNames */
};

extern void     UpdateLineNumber( struct asym * );
extern uint_32  GetLineNumber( void );
#define LineNumber GetLineNumber()

extern void     PushLineQueue( void );
//extern void     PopLineQueue( void );
extern void     AddLineQueue( const char *line );
extern void     AddLineQueueX( const char *fmt, ... );
extern ret_code InputQueueFile( const char *path, FILE * *pfile );
extern char     *GetTextLine( char *buffer );
extern void     PushMacro( struct dsym *, struct macro_instance *, unsigned );
#if FASTMEM==0
extern bool     MacroInUse( struct dsym * );
#endif
extern void     AddStringToIncludePath( const char *string );
extern void     InputInit( void );
extern void     InputPassInit( void );
extern void     InputFini( void );
extern int      GetPreprocessedLine( char *, int, struct asm_tok[] );
extern int      GetCurrSrcPos( char * );
extern void     ClearFileStack( void );
extern uint     get_curr_srcfile( void );
extern void     set_curr_srcfile( uint, uint_32 );
extern const struct fname_list *GetFName( uint );
#ifdef DEBUG_OUT
extern char     *GetTopLine( char * );
#endif

extern struct input_queue *line_queue;
extern int                queue_level;
extern char               *CurrSource;   /* current source line */

#define GetNewLineBuffer( x ) ( x + ( ( strlen( x ) + 1 + 3 ) & ~3 ) )

#endif
