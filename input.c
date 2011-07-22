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
* Description:  processing input line data and line queueing for macros
*
****************************************************************************/

#include <ctype.h>
#include <stdarg.h>
#include <sys/stat.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "reswords.h"
#include "condasm.h"
#include "equate.h"
#include "macro.h"
#include "labels.h"
#include "input.h"
#include "tokenize.h"
#include "proc.h"
#include "fastpass.h"
#include "listing.h"
#include "fatal.h"

extern bool expansion;
extern bool DefineProc;
extern struct ReservedWord  ResWordTable[];
extern ret_code (* const directive[])( int, struct asm_tok[] );

#define REMOVECOMENT 0 /* 1=remove comments from source       */
#define DETECTEOC 1
#define DETECTCTRLZ 1

/* FILESEQ: if 1, stores a linked list of source files, ordered
 * by usage. Masm stores such a list in the COFF symbol table
 * when -Zd/-Zi is set. It isn't necessary, however, and JWasm's
 * COFF code currently will ignore the list.
 */

#define FILESEQ 0

char   *CurrSource;      /* Current Input Line */
struct asym *FileCur = NULL;
struct asym LineCur = { NULL,"@Line", 0 };

int Token_Count;    /* number of tokens in curr line */
int queue_level;    /* number of open (macro) queues */


#if DETECTEOC
extern char inside_comment;
#endif

/* MemAlloc() uses the normal C heap functions.
 * AsmAlloc() uses the "fast" replacement if FASTMEM=1.
 * for "line sources" it's best to use the first, since
 * they are very "short-lived".
 */
#define SrcAlloc(x) MemAlloc(x)
#define SrcFree(x)  MemFree(x)

struct file_list {
    struct file_list    *next;
    union {
        FILE            *file;      /* if item is a file */
        struct macro_instance *mi;  /* if item is a macro */
        struct input_queue  *lines; /* if item is a line queue */
    };
    uint_32             line_num;   /* current line */
    struct asym         *macro;     /* the symbol if it is a macro */
    uint_16             srcfile;    /* index of file in FNames */
    unsigned char       hidden:1;
    unsigned char       islinesrc:1;
};

struct fname_list       *FNames;    /* array of input files */

/* NOTE: the line queue is a simple list of lines.
 if it must be nested, it is converted to a file_list item
 and pushed onto the file stack.
 */
struct input_queue      *line_queue;    /* line queue */
static struct file_list *file_stack;    /* source item (file/macro) stack */
static char             *IncludePath;
uint                    cnt_fnames;     /* items in FNames array */
#if FILESEQ
struct qdesc            FileSeq;
#endif

#ifdef DEBUG_OUT
static char currlqline;
static int cntppl0;
static int cntppl1;
static int cntppl2;
int lq_line;
int cnttok0;
int cnttok1;
#endif

char *CurrComment;

/* buffer for source lines
 * since the lines are sometimes concatenated
 * the buffer must be a multiple of MAX_LINE_LEN
 */
#ifdef __I86__
static char *srclinebuffer;
#else
static char srclinebuffer[ MAX_LINE_LEN * MAX_SYNC_MACRO_NESTING ];
#endif

/* fixme: add '|| defined(__CYGWIN__)' ? */
#if defined(__UNIX__)

#define INC_PATH_DELIM      ':'
#define INC_PATH_DELIM_STR  ":"
#define DIR_SEPARATOR       '/'
#define filecmp strcmp
#define _stat stat

#else

#define INC_PATH_DELIM      ';'
#define INC_PATH_DELIM_STR  ";"
#define DIR_SEPARATOR       '\\'
#define filecmp _stricmp

#if defined(__CYGWIN__)
#define _stat stat
#endif

#endif

static char *GetFullPath( const char *name, char *buff, size_t max )
/******************************************************************/
{
    char        *p;

    p = _fullpath( buff, name, max );
    if( p == NULL )
        p = (char *)name;

#if defined(__UNIX__)
    if( (p[0] == '/' && p[1] == '/') && (name[0] != '/' || name[1] != '/') ) {
        /*
         * if the _fullpath result has a node number and
         * the user didn't specify one, strip the node number
         * off before returning
         */
        p += 2;
        while( *(p++) != '/' ) ;
    }
#endif
    return( p );
}

static time_t GetFileTimeStamp( const char *filename )
/****************************************************/
{
    struct _stat statbuf;

    if( _stat( filename, &statbuf ) != 0 ) {
        return( 0 );
    }
    return( statbuf.st_mtime );
}

/* check if a file is in the array of known files.
 * if no, store the file at the array's end.
 * returns array index.
 */
static uint AddFile( char const *fname )
/**************************************/
{
    struct fname_list *newfn;
    uint    index;
    char    name[_MAX_FNAME];
    char    ext[_MAX_EXT];

    DebugMsg(("AddFile(%s) enter\n", fname ));
    for( index = 0; index < cnt_fnames; index++ ) {
        if( filecmp( fname, (FNames+index)->fullname ) == 0 )
            return( index );
    }

    if ( ( index % 64 ) == 0 ) {
        newfn = (struct fname_list *)SrcAlloc( ( index + 64) * sizeof( struct fname_list ) );
        if ( FNames ) {
            memcpy( newfn, FNames, index * sizeof( struct fname_list ) );
            SrcFree( FNames );
        }
        FNames = newfn;
    }
    cnt_fnames = index + 1;

    _splitpath( fname, NULL, NULL, name, ext );

#if 0
    (FNames+index)->mtime = GetFileTimeStamp( fname );
    (FNames+index)->name = (char *)AsmAlloc( strlen( name ) + strlen( ext ) + 1 );
    strcpy( (FNames+index)->name, name );
    strcat( (FNames+index)->name, ext );
    (FNames+index)->fullname = (char *)AsmAlloc( strlen( fname ) + 1 );
    strcpy( (FNames+index)->fullname, fname );
#else
    newfn = FNames + index;
    /* timestamp needed for autodependancy records only */
    if( Options.line_numbers )
        newfn->mtime = GetFileTimeStamp( fname );
    newfn->name = (char *)AsmAlloc( strlen( name ) + strlen( ext ) + 1 );
    strcpy( newfn->name, name );
    strcat( newfn->name, ext );
    newfn->fullname = (char *)AsmAlloc( strlen( fname ) + 1 );
    strcpy( newfn->fullname, fname );
#endif
    return( index );
}

const struct fname_list *GetFName( uint index )
/*********************************************/
{
    return( FNames+index );
}

/* free the file array */

static void FreeFiles( void )
/***************************/
{
#if FASTMEM==0
    int i;
    for ( i = 0; i < cnt_fnames; i++ ) {
        AsmFree( (FNames+i)->name );
        AsmFree( (FNames+i)->fullname );
    }
#endif
    SrcFree( FNames );
    FNames = NULL;
    return;
}

/* free a line queue */

static void FreeLineQueue( struct input_queue *queue )
/****************************************************/
{
    struct line_list   *curr;
    struct line_list   *next;

    for( curr = queue->head; curr; curr = next ) {
        next = curr->next;
        SrcFree( curr );
    }
    SrcFree( queue );
}

/* clear input source stack (include files and open macros).
 Usually the stack is empty when the END directive occurs,
 but it isn't required that the END directive is located in
 the main source file. Also, an END directive might be
 simulated if a "too many errors" condition occurs.
*/

void ClearFileStack( void )
/*************************/
{
    struct file_list   *nextfile;

    if ( line_queue ) {
        FreeLineQueue( line_queue );
        line_queue = NULL;
    }
    /* dont close the last item (which is the main src file) */
    for( ; file_stack->next ; file_stack = nextfile ) {
        nextfile = file_stack->next;
        if ( file_stack->islinesrc && ( file_stack->macro == NULL ) ) {
            FreeLineQueue( file_stack->lines );
        } else {
            fclose( file_stack->file );
        }
        SrcFree( file_stack );
    }
    return;
}

/* returns value of predefined symbol @Line */

void UpdateLineNumber( struct asym *sym )
/***************************************/
{
    struct file_list *fl;
    for ( fl = file_stack; fl ; fl = fl->next )
        if ( fl->islinesrc == FALSE ) {
            sym->value = fl->line_num;
            break;
        }
    return;
}

uint_32 GetLineNumber( void )
/***************************/
{
    UpdateLineNumber( &LineCur );
    return( LineCur.uvalue );
}

#ifdef DEBUG_OUT
char *GetTopLine( char *buffer )
/******************************/
{
    *buffer = NULLC;
    if ( lq_line )
        sprintf( buffer, "(%u)", lq_line );
    else if( file_stack->islinesrc == TRUE )
        sprintf( buffer, "[%lu]", file_stack->line_num );
    return( buffer );
}
#endif

/* read one line from current source file.
 * returns NULL if EOF has been detected and no char stored in buffer
 */
static char *my_fgets( char *buffer, int max, FILE *fp )
/******************************************************/
{
    char        *ptr = buffer;
    char        *last = buffer + max;
    int         c;

    c = getc( fp );
    while( ptr < last ) {
        switch ( c ) {
        case '\r':
            break; /* don't store CR */
        case '\n':
            /* fall through */
        case '\0':
            *ptr = NULLC;
            return( buffer );
#if DETECTCTRLZ
        case 0x1a:
            /* since source files are opened in binary mode, ctrl-z
             * handling must be done here.
             */
            /* no break */
#endif
        case EOF:
            *ptr = NULLC;
            return( ptr > buffer ? buffer : NULL );
        default:
            *ptr++ = c;
        }
        c = getc( fp );
    }
    AsmErr( LINE_TOO_LONG );
    *(ptr-1) = NULLC;
    return( buffer );
}

#if FILESEQ
void AddFileSeq( uint file )
/**************************/
{
    struct file_seq *node;
    node = AsmAlloc( sizeof( struct file_seq ) );
    node->next = NULL;
    node->file = file;
    if ( FileSeq.head == NULL )
        FileSeq.head = FileSeq.tail = node;
    else {
        ((struct file_seq *)FileSeq.tail)->next = node;
        FileSeq.tail = node;
    }
}
#endif

/* add a new item to the top of the file stack.
 * is_linesrc: TRUE=item is a macro or a line queue.
 * sym = macro symbol or NULL (for a real file or the line queue)
 */
static struct file_list *PushLineSource( bool is_linesrc, struct asym *sym )
/**************************************************************************/
{
    struct file_list   *fl;

    fl = SrcAlloc( sizeof( struct file_list ) );
    fl->next = file_stack;
    fl->hidden = FALSE;
    fl->islinesrc = is_linesrc;
    fl->line_num = 0;
    fl->macro = sym;
    file_stack = fl;
#ifdef DEBUG_OUT
    currlqline = 0;
#endif
    return( fl );
}

/*
 * If there's a current line queue, push it onto the file stack.
 */

void PushLineQueue( void )
/************************/
{
    DebugMsg1(( "PushLineQueue() enter [line_queue=%X]\n", line_queue ));
    if ( line_queue ) {
        struct file_list *fl;
        fl = PushLineSource( TRUE, NULL );
        fl->srcfile = get_curr_srcfile();
        fl->lines = line_queue;
        fl->hidden = TRUE;
        line_queue = NULL;
    }
#ifdef DEBUG_OUT
    currlqline = 0;
#endif
}

#if 0
void PopLineQueue( void )
/***********************/
{
    struct file_list *fl = file_stack;
    if ( fl->islinesrc && fl->lines->head == NULL ) {
        queue_level--;
        file_stack = fl->next;
        SrcFree( fl->lines );
        SrcFree( fl );
    }
}
#endif

/* Add a line to the current line queue. */

void AddLineQueue( const char *line )
/***********************************/
{
    unsigned i = strlen( line );
    struct line_list   *new;

    DebugMsg1(( "AddLineQueue(%u): >%s<\n", ++currlqline, line ));

    if ( line_queue == NULL ) {
        line_queue = SrcAlloc( sizeof( struct input_queue ) );
        line_queue->tail = NULL;
        queue_level++;
    }
    new = SrcAlloc( sizeof( struct line_list ) + i );
    new->next = NULL;
#ifdef DEBUG_OUT
    new->lineno = currlqline;
#endif
    memcpy( new->line, line, i + 1 );

    if( line_queue->tail == NULL ) {
        line_queue->head = new;
    } else {
        /* insert at the tail */
        line_queue->tail->next = new;
    }
    line_queue->tail = new;
    return;
}

/* Add a line to the current line queue, "printf" format. */

void AddLineQueueX( const char *fmt, ... )
/****************************************/
{
    va_list args;
    char *d;
    int i;
    long l;
    const char *s;
    char buffer[MAX_LINE_LEN];

    va_start( args, fmt );
    for ( s = fmt, d = buffer; *s; s++ ) {
        if ( *s == '%' ) {
            s++;
            switch ( *s ) {
            case 'r':
                i = va_arg( args, int );
                GetResWName( i , d );
                /* v2.06: the name is already copied */
                //memcpy( d, ResWordTable[i].name, ResWordTable[i].len );
                d += ResWordTable[i].len;
                break;
            case 's':
                strcpy( d, va_arg( args, char * ) );
                d += strlen( d );
                break;
            case 'd':
            case 'u':
            case 'x':
                l = va_arg( args, long );
                if ( *s == 'x' )
                    myltoa( l, d, 16, FALSE, FALSE );
                else
                    myltoa( l, d, 10, l < 0, FALSE );
                d += strlen( d );
                break;
            default:
                *d++ = *s;
            }
        } else
            *d++ = *s;
    }
    *d = NULLC;
    va_end( args );
    AddLineQueue( buffer );
    return;
}

/* push the current line queue onto the file stack and
 * associate a macro name to it so it can be displayed
 * in case of errors. Param <line> is != 0 when GOTO
 * is handled.
 */
void PushMacro( struct dsym *macro, struct macro_instance *mi, unsigned line )
/****************************************************************************/
{
    struct file_list *fl;

    DebugMsg1(( "PushMacro(%s), queue level=%u\n", macro->sym.name, queue_level ));
    if ( queue_level >= MAX_QUEUE_NESTING ) {
        Fatal( FATAL_NESTING_LEVEL_TOO_DEEP );
    }
    fl = PushLineSource( TRUE, &macro->sym );
    fl->mi = mi;
    //fl->hidden = *sym->name ? FALSE : TRUE; /* v2.0: don't hide loop macros */
    fl->hidden = FALSE;
    //line_queue = NULL;
    queue_level++;
    fl->line_num = line;
    return;
}
#if FASTMEM==0
bool MacroInUse( struct dsym *macro )
/***********************************/
{
    struct file_list *fl;

    for ( fl = file_stack; fl ; fl = fl->next )
        if ( fl->macro == &macro->sym )
            return( TRUE );

    return( FALSE );
}
#endif

uint get_curr_srcfile( void )
/***************************/
{
#if 1
    struct file_list *fl;
    for ( fl = file_stack; fl ; fl = fl->next )
        if ( fl->islinesrc == FALSE )
            return( fl->srcfile );
    return( ModuleInfo.srcfile );
#else
    return( file_stack ? file_stack->srcfile : ModuleInfo.srcfile );
#endif
}

void set_curr_srcfile( uint file, uint_32 line_num )
/**************************************************/
{
    if ( file != 0xFFF ) /* 0xFFF is the special value for macro lines */
        file_stack->srcfile = file;
    file_stack->line_num = line_num;
    return;
}

/* for error listing, render the current source file and line */
/* this function is also called if pass is > 1,
 * which is a problem for FASTPASS because the file stack is empty.
 */
int GetCurrSrcPos( char *buffer )
/*******************************/
{
    struct file_list *fl;
    uint_32 line;

    line = LineNumber;
    for( fl = file_stack; fl && fl->islinesrc; fl = fl->next );
    if ( fl ) {
        if ( line )
            return( sprintf( buffer, "%s(%lu) : ", GetFName( fl->srcfile )->name , line ) );
        else
            return( sprintf( buffer, "%s : ", GetFName( fl->srcfile )->name ) );
    }
    *buffer = NULLC;
    return( 0 );
}

/* for error listing, render the source nesting structure.
 * the structure consists of include files and macros.
 */

void print_source_nesting_structure( void )
/*****************************************/
{
    struct file_list       *fl;
    unsigned        tab = 1;

    /* in main source file? */
    if ( file_stack == NULL || file_stack->next == NULL )
        return;

    for( fl = file_stack; fl->next ; fl = fl->next ) {
        if( fl->hidden == FALSE ) {
            if( fl->islinesrc == FALSE ) {
                PrintNote( NOTE_INCLUDED_BY, tab, "", GetFName( fl->srcfile)->name, fl->line_num );
            } else if ( fl->macro != NULL ) {
                char fname[_MAX_FNAME+_MAX_EXT];
                char fext[_MAX_EXT];
                if (*(fl->macro->name) == NULLC ) {
                    PrintNote( NOTE_ITERATION_MACRO_CALLED_FROM, tab, "", "MacroLoop", fl->line_num, fl->macro->value + 1 );
                } else {
                    _splitpath( GetFName(((struct dsym *)fl->macro)->e.macroinfo->srcfile)->name, NULL, NULL, fname, fext );
                    strcat( fname, fext );
                    PrintNote( NOTE_MACRO_CALLED_FROM, tab, "", fl->macro->name, fl->line_num, fname );
                }
            }
            tab++;
        }
    }
    PrintNote( NOTE_MAIN_LINE_CODE, tab, "", GetFName(fl->srcfile)->name, fl->line_num );
}

/* Scan the include path for a file!
 * variable IncludePath also contains directories set with -I cmdline option
 */
static FILE *open_file_in_include_path( const char *name, char *fullpath )
/************************************************************************/
{
    char            *curr;
    char            *next;
    int             i;
    int             namelen;
    FILE            *file = NULL;
    char            buffer[_MAX_PATH];

    while( isspace( *name ) )
        name++;

    curr = IncludePath;
    namelen= strlen( name );

    DebugMsg(("open_file_in_include_path(%s) enter\n", name ));
    for ( ; curr; curr = next ) {
        next = strchr( curr, INC_PATH_DELIM );
        if ( next ) {
            i = next - curr;
            next++; /* skip path delimiter char (; or :) */
        } else {
            i = strlen( curr );
        }

        /* v2.06: ignore
         * - "empty" entries in PATH
         * - entries which would cause a buffer overflow
         */
        if ( i == 0 || ( ( i + namelen ) >= sizeof( buffer ) ) )
            continue;

        memcpy( buffer, curr, i );
        if( buffer[i-1] != '/'
#if !defined(__UNIX__)
           && buffer[i-1] != '\\' && buffer[i-1] != ':'
#endif
        ) {
            buffer[i] = DIR_SEPARATOR;
            i++;
        }
        strcpy( buffer+i, name );

        DebugMsg(("open_file_in_include_path: >%s<\n", buffer ));
        file = fopen( buffer, "rb" );
        if( file ) {
            strcpy( fullpath, buffer );
            break;
        }
    }
    DebugMsg(("open_file_in_include_path()=%p\n", file ));
    return( file );
}

/* the worker behind the INCLUDE directive */

ret_code InputQueueFile( const char *path, FILE * *pfile )
/********************************************************/
{
    FILE        *file = NULL;
    struct file_list   *fl;
    char        fullpath[ _MAX_PATH ];
    char        buffer[ _MAX_PATH ];
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    char        drive2[_MAX_DRIVE];
    char        dir2[_MAX_DIR];

    DebugMsg(("InputQueueFile(%s) enter\n", path ));

    _splitpath( path, drive, dir, fname, ext );
    DebugMsg(("InputQueueFile(): drive=%s, dir=%s, fname=%s, ext=%s\n", drive, dir, fname, ext ));

    /* if no absolute path is given, then search in the directory
     of the current source file first! */

    if ( dir[0] != '\\' && dir[0] != '/' ) {
        for ( fl = file_stack; fl ; fl = fl->next ) {
            if ( fl->islinesrc == FALSE ) {
                _splitpath( GetFName( fl->srcfile )->fullname, drive2, dir2, NULL, NULL );
                DebugMsg(("InputQueueFile(): curr src=%s, split into drive=%s, dir=%s\n", GetFName( fl->srcfile)->fullname, drive2, dir2 ));
                if ( dir2[0] == '\\' || dir2[0] == '/' ) {
                    _makepath( fullpath, drive2, dir2, fname, ext );
                    file = fopen( fullpath, "rb" );
                    DebugMsg(("InputQueueFile(): makepath()=%s, file=%X\n", fullpath, file ));
                }
                break;
            }
        }
    }
    if ( file == NULL ) {
        strcpy( fullpath, path );
        file = fopen( fullpath, "rb" );
        DebugMsg(("InputQueueFile(): file=%X\n", file ));
        //DebugMsg(("InputQueueFile(): fopen()=%X\n", file ));
        if( file == NULL &&
            IncludePath != NULL &&
            /* don't search include path if an absolute path is given */
            dir[0] != '\\' && dir[0] != '/') {
            //file = open_file_in_include_path( path, tmp );
            file = open_file_in_include_path( path, fullpath );
            DebugMsg(("InputQueueFile(): open_file_in_include_path(%s, %s) returned file=%X\n", path, fullpath, file ));
        }
    }
    if( file == NULL ) {
        AsmErr( CANNOT_OPEN_INCLUDE_FILE, fullpath );
        return( ERROR );
    }
    if ( pfile )
        *pfile = file;
    else {
        fl = PushLineSource( FALSE, NULL );
        fl->srcfile = AddFile( GetFullPath( fullpath, buffer, sizeof( buffer ) ) );
        FileCur->string_ptr = GetFName( fl->srcfile )->name;
#if FILESEQ
        if ( Options.line_numbers && Parse_Pass == PASS_1 )
            AddFileSeq( fl->srcfile );
#endif
        fl->file = file;
    }
    return( NOT_ERROR );
}

/* get the next source line. */

char *GetTextLine( char *buffer )
/*******************************/
{
    struct line_list   *inputline;
    struct file_list   *fl;

    *buffer = NULLC;

    CurrComment = NULL;

    /* Check the line_queue first!
     * The line_queue is global and there is ONE only.
     * If it must be nested, it's pushed onto the file stack.
     */

    if ( line_queue != NULL ) {
        /* if there's a line_queue, it can't be empty */
        inputline = line_queue->head;
        strcpy( buffer, inputline->line );
        line_queue->head = inputline->next;
        SrcFree( inputline );
        if( line_queue->head == NULL ) {
            SrcFree( line_queue );
            line_queue = NULL;
            queue_level--;
        }
#ifdef DEBUG_OUT
        lq_line++;
#endif
        return( buffer );
    }
#ifdef DEBUG_OUT
    lq_line = 0;
#endif
    /* Now check the file stack!
     * items on the file stack may be
     * - pushed line queues ( islinesrc == TRUE && macro == NULL )
     * - macro line queues ( islinesrc == TRUE && macro != NULL )
     * - assembly include files. ( islinesrc == FALSE )
     */

    while( 1 ) {
        fl = file_stack;
        if( fl->islinesrc == FALSE ) {
            if( my_fgets( buffer, MAX_LINE_LEN, fl->file ) ) {
                fl->line_num++;
                return( buffer );
            }
            /* EOF of main module reached? */
            if ( fl->next == NULL )
                break;

            file_stack = fl->next;
            fclose( fl->file );
            DebugMsg1(("GetTextLine: ***** EOF file %s *****\n", GetFName( fl->srcfile )->name ));
            SrcFree( fl );
#if FILESEQ
            if ( Options.line_numbers && Parse_Pass == PASS_1 )
                AddFileSeq( file_stack->srcfile );
#endif
            if ( file_stack->islinesrc == FALSE )
                FileCur->string_ptr = GetFName( file_stack->srcfile)->name;

        } else if ( fl->macro ) {
            /* item is a macro */
            if ( fl->mi->currline ) {
                /* if line contains placeholders, replace them by current values */
                if ( fl->mi->currline->ph_count ) {
                    fill_placeholders( buffer,
                                    fl->mi->currline->line,
                                    fl->mi->parmcnt,
                                    fl->mi->localstart, fl->mi->parm_array );
                } else {
                    strcpy( buffer, fl->mi->currline->line );
                }
                DebugMsg1(("GetTextLine: qlevel=%u stack=%p (macro=%s)\n", queue_level, file_stack, fl->macro->name ));
                fl->mi->currline = fl->mi->currline->next;
                fl->line_num++;
                return( buffer );
            }
            queue_level--;
            file_stack = fl->next;
            SrcFree( fl );
            break;
        } else {
            /* item is a line queue */
            inputline = fl->lines->head;
            DebugMsg1(("GetTextLine: qlevel=%u stack=%p inputline=%p\n", queue_level, file_stack, inputline ));
            if( inputline != NULL ) {
                fl->line_num++;
                strcpy( buffer, inputline->line );
                fl->lines->head = inputline->next;
                SrcFree( inputline );
                return( buffer );
            }
            queue_level--;
            file_stack = fl->next;
            SrcFree( fl->lines );
            SrcFree( fl );
            break;
        }
        DebugMsg1(("GetTextLine, new qlevel=%u, stack=%p, buffer=>%s<\n", queue_level, file_stack, buffer ));
    }
    return( NULL ); /* end of main source file reached */
}

/* add a string to the include path.
 * called for -I cmdline options.
 * the include path is rebuilt for each assembled module
 */
void AddStringToIncludePath( const char *string )
/***********************************************/
{
    char *tmp;
    int len;

    DebugMsg(("AddStringToIncludePath(%s) enter\n", string ));
    while( isspace( *string ) )
        string++;
    len = strlen( string );
    if ( len == 0 )
        return;
    if( IncludePath == NULL ) {
        IncludePath = SrcAlloc( len + 1 );
        strcpy( IncludePath, string );
    } else {
        tmp = IncludePath;
        IncludePath = SrcAlloc( strlen( tmp ) + sizeof( INC_PATH_DELIM_STR ) +
                                len + 1 );
        strcpy( IncludePath, tmp );
        strcat( IncludePath, INC_PATH_DELIM_STR );
        strcat( IncludePath, string );
        SrcFree( tmp );
    }
}

/* Initializer, called once for each module. */

void InputInit( void )
/********************/
{
    struct file_list   *fl;
    char        path[_MAX_PATH];
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];

    DebugMsg(( "InputInit() enter\n" ));
    cnt_fnames = 0;
    FNames = NULL;
    IncludePath = NULL;
    file_stack = NULL;
#if FILESEQ
    FileSeq.head = NULL;
#endif
    fl = PushLineSource( FALSE, NULL );
    fl->file = CurrFile[ASM];
    fl->srcfile = ModuleInfo.srcfile = AddFile( CurrFName[ASM] );
    FileCur->string_ptr = GetFName( fl->srcfile )->name;

#ifdef DEBUG_OUT
    cntppl0 = 0;
    cntppl1 = 0;
    cntppl2 = 0;
    cnttok0 = 0;
    cnttok1 = 0;
#endif

    /* add path of main module to the include path */
    _splitpath( CurrFName[ASM], drive, dir, NULL, NULL );
    if ( drive[0] || dir[0] ) {
        _makepath( path, drive, dir, NULL, NULL );
        AddStringToIncludePath( path );
    }
#ifdef __I86__
    srclinebuffer = AsmAlloc( MAX_LINE_LEN * MAX_SYNC_MACRO_NESTING );
#endif
    /* create token array and token string buffer */
    CreateTokenBuffer();
    DebugMsg(( "InputInit() exit\n" ));
}

/* init for each pass */

void InputPassInit( void )
/************************/
{
    line_queue = NULL;
    queue_level = 0;
    file_stack->line_num = 0;
    inside_comment = NULLC;
    CurrSource = srclinebuffer;
}

void InputFini( void )
/********************/
{
#ifdef DEBUG_OUT
    int   i;
    for( i = 0; i < cnt_fnames; i++) {
        DebugMsg(( "InputFini: idx=%u name=%s full=%s\n", i, FNames[i].name, FNames[i].fullname ));
    }
#endif
    DestroyTokenBuffer();
    if ( IncludePath )
        SrcFree( IncludePath );
    FreeFiles();
#ifdef DEBUG_OUT
    if ( Options.quiet == FALSE )
        printf("invokations: GetPreprocessedLine=%u/%u/%u, Tokenize=%u/%u\n", cntppl0, cntppl1, cntppl2, cnttok0, cnttok1 );
#endif
#ifdef __I86__
    AsmFree( srclinebuffer );
#endif
    /* v2.03: clear file stack to ensure that GetCurrSrcPos()
     * won't find something when called from main().
     */
    file_stack = NULL;
#ifdef DEBUG_OUT
    CurrSource = NULL;
#endif
}

/* INCLUDE directive.
 * If a full path is specified, the directory where the included file
 * is located becomes the "source" directory, that is, it is searched
 * FIRST if further INCLUDE directives are found inside the included file.
 */
ret_code IncludeDirective( int i, struct asm_tok tokenarray[] )
/*************************************************************/
{

    DebugMsg(("IncludeDirective enter\n"));

    if ( CurrFile[LST] ) {
        LstWriteSrcLine();
    }

    i++; /* skip directive */
    /* v2.03: allow plain numbers as file name argument */
    //if ( tokenarray[i].token == T_FINAL || tokenarray[i].token == T_NUM ) {
    if ( tokenarray[i].token == T_FINAL ) {
        AsmError( EXPECTED_FILE_NAME );
        return( ERROR );
    }

    /* if the filename is enclosed in <>, just use this literal */

    if ( tokenarray[i].token == T_STRING && tokenarray[i].string_delim == '<' ) {
        if ( tokenarray[i+1].token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i+1].string_ptr );
        } else {
            InputQueueFile( tokenarray[i].string_ptr, NULL );
        }
    } else {
        char *name;
        char *p;
        /* if the filename isn't enclosed in <>, use anything which comes
         * after INCLUDE
         */
        name = tokenarray[i].tokpos;
        for ( p = name; *p; p++ );
        for ( p--; p > name && isspace(*p); *p = NULLC, p-- );
        InputQueueFile( name, NULL );
    }
    return( NOT_ERROR );
}

/* GetPreprocessedLine() is the "preprocessor".
 * 1. a line is read with GetTextLine()
 * 2. the line is tokenized with Tokenize(), Token_Count set
 * 3. (text) macros are expanded by ExpandLine()
 * 4. "preprocessor" directives are executed
 */
int GetPreprocessedLine( char *line, int Skip, struct asm_tok tokenarray[] )
/****************************************************************************/
{
    int i;

    if( GetTextLine( line ) == NULL ) {
        DebugMsg1(("GetPreprocessedLine: GetTextLine() returned NULL (end of file/macro)\n" ));
        return( -1 ); /* EOF */
    }
    /* v2.06: moved here from Tokenize() */
    ModuleInfo.line_flags = 0;

    /* Token_Count is the number of tokens scanned */
    Token_Count = Tokenize( line, 0, FALSE );

    if ( Skip )
        return( Token_Count );

#ifdef DEBUG_OUT
    cntppl0++;
    if ( file_stack && file_stack->islinesrc ) {
        if ( file_stack->macro )
            DebugMsg1(("GetPreprocessedLine(mac=%s): >%s<\n", file_stack->macro->name, line ));
        else
            DebugMsg1(("GetPreprocessedLine: >%s<\n", line ));
    } else if ( file_stack && file_stack->srcfile ) {
        DebugMsg1(("GetPreprocessedLine(%s): >%s<\n", GetFName( file_stack->srcfile )->name, line ));
    } else
        DebugMsg1(("GetPreprocessedLine(cnt=%u): >%s<\n", Token_Count, line));
#endif

#if REMOVECOMENT == 0
    if ( Token_Count == 0 && ( CurrIfState == BLOCK_ACTIVE || ModuleInfo.listif ) )
        LstWriteSrcLine();
#endif

    /* expand the line */
    if ( CurrIfState == BLOCK_ACTIVE ) {
        /* expand (text) macros. If expansion occured, rescan the line */
        while ( Token_Count > 0 && ExpandLine( line, tokenarray ) == STRING_EXPANDED ) {
            DebugMsg1(("GetPreprocessedLine: expanded line is >%s<\n", line));
            Token_Count = Tokenize( line, 0, TRUE );
        }
    }

    if ( Token_Count == 0 )
        return( 0 );

#ifdef DEBUG_OUT
    cntppl1++;
#endif

    i = 0;
    if ( Token_Count > 2 && ( tokenarray[1].token == T_COLON || tokenarray[1].token == T_DBL_COLON ) )
        i = 2;

    /* handle "preprocessor" directives:
     * IF, ELSE, ENDIF, ...
     * FOR, REPEAT, WHILE, ...
     * PURGE
     * INCLUDE
     * since v2.05, error directives are no longer handled here!
     */
    if ( tokenarray[i].token == T_DIRECTIVE &&
        tokenarray[i].dirtype <= DRT_INCLUDE ) {

        /* if i != 0, then a code label is located before the directive */
        if ( i > 1 ) {
            int oldcnt;
            int oldtoken;
            char oldchar;
            if ( tokenarray[0].token != T_ID ) {
                AsmErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
                return( 0 );
            }
            /* v2.04: call ParseLine() to parse the "label" part of the line */
            oldcnt = Token_Count;
            oldtoken = tokenarray[i].token;
            oldchar = *tokenarray[i].tokpos;
            Token_Count = i;
            tokenarray[i].token = T_FINAL;
            *tokenarray[i].tokpos = NULLC;
            ParseLine( tokenarray );
            if ( Options.preprocessor_stdout == TRUE )
                WritePreprocessedLine( line, tokenarray );
            Token_Count = oldcnt;
            tokenarray[i].token = oldtoken;
            *tokenarray[i].tokpos = oldchar;
        }

        directive[tokenarray[i].dirtype]( i, tokenarray );

        return( 0 );
    }

    /* handle preprocessor directives which need a label */

    if ( tokenarray[0].token == T_ID && tokenarray[1].token == T_DIRECTIVE ) {
        struct asym *sym;
        switch ( tokenarray[1].dirtype ) {
        case DRT_EQU:
            /*
             * EQU is a special case:
             * If an EQU directive defines a text equate
             * it MUST be handled HERE and 0 must be returned to the caller.
             * This will prevent function OnePass() in assemble.c to store the
             * "preprocessed" line.
             * Since we cannot decide whether EQU defines a text equate or
             * a number before it has scanned its argument, we'll have to
             * handle it in ANY case and if it defines a number, the usual
             * line processing done in OnePass() has to be emulated here.
             */
            if ( sym = CreateConstant( tokenarray ) ) {
                if ( sym->state != SYM_TMACRO ) {
#if FASTPASS
                    if ( StoreState ) FStoreLine();
#endif
                    if ( Options.preprocessor_stdout == TRUE )
                        WritePreprocessedLine( line, tokenarray );
                }
                /* v2.03: LstWrite() must be called AFTER StoreLine()! */
                if ( ModuleInfo.list == TRUE ) {
                    LstWrite( LSTTYPE_EQUATE, 0, sym );
                }
            }
            return( 0 );
        case DRT_MACRO:
        case DRT_CATSTR: /* CATSTR + TEXTEQU directives */
        case DRT_SUBSTR:
            directive[tokenarray[1].dirtype]( 1, tokenarray );
            return( 0 );
        }
    }

#ifdef DEBUG_OUT
    cntppl2++;
#endif
    return( Token_Count );
}
