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
#include <sys/stat.h>

#include "globals.h"
#include "parser.h"
#include "directiv.h"
#include "memalloc.h"
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
#include "queue.h"

extern bool expansion;
extern bool DefineProc;
extern bool SkipMacroMode;
extern int  MacroLocals;

#define DETECTEOC 1
#define DETECTCTRLZ 1

/* FILESEQ: if 1, stores a linked list of source files, ordered
 * by usage. Masm stores such a list in the COFF symbol table
 * when -Zd/-Zi is set. It isn't necessary, however, and JWasm's
 * COFF code currently will ignore the list.
 */

#define FILESEQ 0

struct asm_sym FileCur = {NULL,"@FileCur", NULL, 0};
struct asm_sym LineCur = {NULL,"@Line", NULL, 0};

int Token_Count;    // number of tokens in curr line
int queue_level;    // number of open (macro) queues
uint_8 MacroLevel;  // current macro nesting level

#if DETECTEOC
extern bool inside_comment;
#endif

/* MemAlloc() uses the normal C heap functions.
 * AsmAlloc() uses the "fast" replacement if FASTMEM=1.
 * for "line sources" it's best to use the first, since
 * they are very "short-lived".
 */
#define SrcAlloc(x) MemAlloc(x)
#define SrcFree(x)  MemFree(x)

typedef struct file_list {
    struct file_list    *next;
    union {
        FILE            *file;
        struct input_queue  *lines;
    };
    uint_32             line_num;   /* current line */
    asm_sym             *macro;     /* the symbol if it is a macro */
    uint_16             srcfile;    /* index of file in FNames */
    unsigned char       hidden:1;
    unsigned char       islinesrc:1;
} file_list;

FNAME                   *FNames;    /* array of input files */

/* NOTE: the line queue is a simple list of lines.
 if it must be nested, it is converted to a file_list item
 and pushed onto the file stack.
 */
input_queue             *line_queue;    /* line queue */
file_list               *file_stack;    /* source item (file/macro) stack */
static char             *IncludePath;
uint                    cnt_fnames;     /* items in FNames array */
#if FILESEQ
qdesc                   FileSeq;
#endif

#ifdef DEBUG_OUT
static int cntppl0;
static int cntppl1;
static int cntppl2;
int cnttok0;
int cnttok1;
#endif

#if defined(__UNIX__)

#define INC_PATH_DELIM      ':'
#define INC_PATH_DELIM_STR  ":"
#define DIR_SEPARATOR       '/'
#define DIR_SEP_STRING      "/"
#define filecmp strcmp

#else

#define INC_PATH_DELIM      ';'
#define INC_PATH_DELIM_STR  ";"
#define DIR_SEPARATOR       '\\'
#define DIR_SEP_STRING      "\\"
#define filecmp _stricmp

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
        //
        // if the _fullpath result has a node number and
        // the user didn't specify one, strip the node number
        // off before returning
        //
        p += 2;
        while( *(p++) != '/' ) ;
    }
#endif
    return( p );
}

static time_t GetFileTimeStamp( const char *filename )
/****************************************************/
{
#ifdef __POCC__
    struct _stat statbuf;
#define stat _stat
#else
    struct stat statbuf;
#endif

    if( stat( filename, &statbuf ) != 0 ) {
        return( 0 );
    } else {
        return( statbuf.st_mtime );
    }
}

// check if a file is in the array of known files.
// if no, store the file at the array's end.
// returns array index.

static uint AddFile( char const *fname )
/**************************************/
{
    FNAME   *newfn;
    uint    index;
    char    name[_MAX_FNAME];
    char    ext[_MAX_EXT];

    DebugMsg(("AddFile(%s) enter\n", fname ));
    for( index = 0; index < cnt_fnames; index++ ) {
        if( filecmp( fname, (FNames+index)->fullname ) == 0 )
            return( index );
    }

    if ( ( index % 64 ) == 0 ) {
        newfn = (FNAME *)SrcAlloc( ( index + 64) * sizeof(FNAME) );
        if ( FNames ) {
            memcpy( newfn, FNames, index * sizeof(FNAME) );
            SrcFree( FNames );
        }
        FNames = newfn;
    }
    cnt_fnames = index + 1;

    _splitpath( fname, NULL, NULL, name, ext );

    (FNames+index)->mtime = GetFileTimeStamp( fname );
    (FNames+index)->name = (char *)AsmAlloc( strlen( name ) + strlen( ext ) + 1 );
    strcpy( (FNames+index)->name, name );
    strcat( (FNames+index)->name, ext );
    (FNames+index)->fullname = (char *)AsmAlloc( strlen( fname ) + 1 );
    strcpy( (FNames+index)->fullname, fname );

    return( index );
}

const FNAME *GetFName( uint index )
/*********************************/
{
    return( FNames+index );
}

// free the file array

static void FreeFiles( void )
/***************************/
{
#if FASTMEM==0
    int i;
    for (i = 0; i < cnt_fnames; i++ ) {
        AsmFree( (FNames+i)->name );
        AsmFree( (FNames+i)->fullname );
    }
#endif
    SrcFree( FNames );
    FNames = NULL;
    return;
}

static void FreeLineQueue( input_queue *queue )
/*********************************************/
{
    line_list   *curr;
    line_list   *next;

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
    file_list   *nextfile;

    if ( line_queue ) {
        FreeLineQueue( line_queue );
        line_queue = NULL;
    }
    /* dont close the last item (which is the main src file) */
    for( ; file_stack->next ; file_stack = nextfile ) {
        nextfile = file_stack->next;
        if ( file_stack->islinesrc ) {
            FreeLineQueue( file_stack->lines );
        } else {
            fclose( file_stack->file );
        }
        SrcFree( file_stack );
    }
    return;
}

/* returns value of predefined symbol @Line */

uint_32 GetLineNumber( asm_sym *sym )
/***********************************/
{
    file_list *fl;
    for ( fl = file_stack; fl ; fl = fl->next )
        if ( fl->islinesrc == FALSE )
            return( fl->line_num );
    return( 0 );
}

#ifdef DEBUG_OUT
uint_32 GetTopLine( void )
/************************/
{
    return( file_stack->line_num );
}
#endif

/* read one line from current source file */
/* returns FALSE if EOF has been detected */

static bool GetLineFromFile( char *ptr, unsigned max, FILE *fp )
/**************************************************************/
{
    char        quote = 0;
    char        *lastbs = NULL;
    bool        skip;
    int         c;
    int         got_something;

    skip = FALSE;
    got_something = FALSE;
#if DETECTEOC
    /* don't skip comments inside a COMMENT */
    if ( inside_comment ) {
#ifdef DEBUG_OUT
        char *p2 = ptr;
#endif
        c = getc( fp);
        while (c != EOF && c != '\n') {
            if (c != '\r' && max > 1) {
                *ptr++ = c;
                max--;
            }
            c = getc( fp );
        }
        *ptr = NULLC;
        DebugMsg(("GetLineFromFile: COMMENT >%s<\n", p2 ));
        return( c != EOF );
    }
#endif
    for( ;; ) {
        c = getc( fp );
        /* copy the line until we hit a NULL, newline, or ; not in a quote */
        if (quote) {
            if (c == quote)
                quote = 0;
        } else {
            switch( c ) {
            case '\'':
            case '"':
            case '`':
                quote = c;
                break;
            case '<':
                quote = '>';
                break;
            case ';':
                skip = TRUE;
                break;
#if DETECTCTRLZ
            case 0x1a:
                c = EOF;
#endif
            }
        }
        switch (c) {
        case '\r':
            continue; /* don't store character in string */
        case '\n':
            /* if continuation character found, pass over newline */
            if (lastbs) {
                char * p;
                for (p = lastbs+1;p < ptr;p++)
                    if (!isspace(*p))
                        break;
                if (p == ptr || (*p == ';' && quote == '>' )) {
                    max += p - lastbs;
                    ptr = lastbs;
                    if (!quote)
                        *ptr++ = ' ';
                    file_stack->line_num++;
                    lastbs = NULL;
                    skip = FALSE;
                    do {
                        c = getc( fp );
                    } while (c == ' ' || c == '\t');
                    ungetc(c, fp);
                    continue;
                }
            }
            *ptr = '\0';
            // fall through
        case '\0': /* we have found the end of the line */
            return( TRUE );
        case EOF:
            *ptr = '\0';
            return( got_something );
        case '\\': /* continuation works even inside strings! */
            if (skip == FALSE)
                lastbs = ptr;
            break;
        }
        if( !skip ) {
            *ptr++ = c;
            if( --max <= 1 )
                skip = TRUE;
            got_something = TRUE;
        }
    }
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
static file_list *PushLineSource( bool is_linesrc, asm_sym *sym )
/***************************************************************/
{
    file_list   *new;

    new = SrcAlloc( sizeof( file_list ) );
    new->next = file_stack;
    new->hidden = FALSE;
    new->islinesrc = is_linesrc;
    new->line_num = 0;
    new->macro = sym;
    file_stack = new;
    return( new );
}

/*
 * If there's a current line queue, push it onto the file stack.
 * Then create a new one.
 */

void PushLineQueue( void )
/************************/
{
    DebugMsg(( "PushLineQueue() enter [line_queue=%X]\n", line_queue ));

    /* if the line queue is in use, push it onto the file stack */
    if ( line_queue ) {
        file_list *fl;
        fl = PushLineSource( TRUE, NULL );
        fl->srcfile = get_curr_srcfile();
        fl->lines = line_queue;
        fl->hidden = TRUE;
        line_queue = NULL;
    }
}

#if 0
void PopLineQueue( void )
/************************/
{
    file_list *fl = file_stack;
    if ( fl->islinesrc && fl->lines->head == NULL ) {
        queue_level--;
        file_stack = fl->next;
        SrcFree( fl->lines );
        SrcFree( fl );
    }
}
#endif

/* Add a line to the current line queue.
 */

void AddLineQueue( const char *line )
/***********************************/
{
    unsigned i = strlen(line) + 1;
    line_list   *new;

    DebugMsg(( "AddLineQueue(#=%lu): %s\n", GetTopLine(), line ));

    if ( line_queue == NULL ) {
        line_queue = SrcAlloc( sizeof( input_queue ) );
        line_queue->tail = NULL;
        queue_level++;
    }
    new = SrcAlloc( sizeof( line_list ) + i );
    new->next = NULL;
    memcpy( new->line, line, i );

    if( line_queue->tail == NULL ) {
        line_queue->head = new;
    } else {
        /* insert at the tail */
        line_queue->tail->next = new;
    }
    line_queue->tail = new;
    return;
}

// push the current line queue onto the file stack and
// associate a macro name to it so it can be displayed
// in case of errors.

void PushMacro( asm_sym *sym )
/****************************/
{
    file_list *fl;

    DebugMsg(( "%lu. PushMacro(%s), queue level=%u, old macro level=%u\n", GetTopLine(), sym->name, queue_level, MacroLevel ));
    if ( queue_level >= MAX_QUEUE_NESTING )
        Fatal( FATAL_NESTING_LEVEL_TOO_DEEP );
    fl = PushLineSource( TRUE, sym );
    fl->lines = line_queue;
    //fl->hidden = *sym->name ? FALSE : TRUE; //v2.0: don't hide loop macros
    fl->hidden = FALSE;
    line_queue = NULL;
    MacroLevel++;

    if ( ModuleInfo.list && fl->hidden == FALSE ) {
        if ( ModuleInfo.list_macro == LM_LISTMACROALL || MacroLevel == 0 )
            LstWriteSrcLine();
    }
}

/* special version of PushMacro() for GOTO */

void PushMacroGoto( asm_sym *macro, int line )
/********************************************/
{
    file_list *fl;

    DebugMsg(( "%lu. PushMacroGoto(%s), queue level=%u, old macro level=%u\n", GetTopLine(), macro->name, queue_level, MacroLevel ));
    fl = PushLineSource( TRUE, macro );
    fl->lines = line_queue;
    fl->hidden = *macro->name ? FALSE : TRUE;
    line_queue = NULL;
    MacroLevel++;
    fl->line_num = line;
}

uint get_curr_srcfile( void )
/***************************/
{
#if 1
    file_list *fl;
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
    file_stack->srcfile = file;
    file_stack->line_num = line_num;
    return;
}

/* for error listing, render the current source file and line */
/* this function is also called if pass is > 1,
 * which is a problem for FASTPASS because the file stack is empty.
 */
int GetCurrSrcPos( char * buffer)
/*******************************/
{
    file_list *fl;
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
    file_list       *fl;
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
                    _splitpath( GetFName(((dir_node *)fl->macro)->e.macroinfo->srcfile)->name, NULL, NULL, fname, fext );
                    strcat( fname, fext );
                    PrintNote( NOTE_MACRO_CALLED_FROM, tab, "", fl->macro->name, fl->line_num, fname );
                }
            }
            tab++;
        }
    }
    PrintNote( NOTE_MAIN_LINE_CODE, tab, "", GetFName(fl->srcfile)->name, fl->line_num );
}

// Scan the include path for a file!
// variable IncludePath also contains directories set with -I cmdline option

static FILE *open_file_in_include_path( const char *name, char *fullpath )
/************************************************************************/
{
    char            *curr;
    char            *next;
    FILE            *file = NULL;
    char            buffer[_MAX_PATH];

    while( isspace( *name ) )
        name++;

    curr = IncludePath;

    for ( ; curr; curr = next ) {
        next = strchr( curr, INC_PATH_DELIM );
        if ( next ) {
            memcpy( buffer, curr, next - curr );
            buffer[next - curr] = NULLC;
            next++; /* skip ';'/':' */
        } else
            strcpy( buffer, curr );

        // NYI: this is no good for DOS - have to check '/', '\\', and ':'
        if( buffer[ strlen( buffer ) - 1] != DIR_SEPARATOR ) {
            strcat( buffer, DIR_SEP_STRING );
        }
        strcat( buffer, name );

        file = fopen( buffer, "rb" );
        if( file ) {
            strcpy( fullpath, buffer );
            break;
        }
    }
    return( file );
}

// the worker behind the INCLUDE directive

ret_code InputQueueFile( const char *path, FILE * *pfile )
/********************************************************/
{
    FILE        *file = NULL;
    file_list   *fl;
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
        FileCur.string_ptr = GetFName( fl->srcfile )->name;
#if FILESEQ
        if ( Options.line_numbers && Parse_Pass == PASS_1 )
            AddFileSeq( fl->srcfile );
#endif
        fl->file = file;
    }
    return( NOT_ERROR );
}

/* get the next source line. */

char *GetTextLine( char *string, int max )
/****************************************/
{
    line_list   *inputline;
    file_list   *fl;

    *string = NULLC;

    /* Check the line_queue first!
     * The line_queue is global and there is ONE only.
     * If it must be nested, it's pushed onto the file stack.
     */

    if ( line_queue != NULL ) {
        /* if there's a line_queue, it can't be empty */
        inputline = line_queue->head;
        strcpy( string, inputline->line );
        line_queue->head = inputline->next;
        SrcFree( inputline );
        if( line_queue->head == NULL ) {
            SrcFree( line_queue );
            line_queue = NULL;
            queue_level--;
        }
        return( string );
    }

    /* Now check the file stack!
     * The file stack may contain
     * - pushed line queues (macro == -1)
     * - macro line queues (macro != NULL && macro != -1)
     * - assembly include files. (macro == NULL)
     */

    while( 1 ) {
        fl = file_stack;
        if( fl->islinesrc == FALSE ) {
            if( GetLineFromFile( string, max, fl->file ) ) {
                fl->line_num++;
                return( string );
            }
            /* EOF of main module reached? */
            if ( fl->next == NULL )
                break;

            file_stack = fl->next;
            fclose( fl->file );
            DebugMsg(("GetTextLine: ***** EOF file %s *****\n", GetFName( fl->srcfile )->name ));
            SrcFree( fl );
#if FILESEQ
            if ( Options.line_numbers && Parse_Pass == PASS_1 )
                AddFileSeq( file_stack->srcfile );
#endif
            if ( file_stack->islinesrc == FALSE )
                FileCur.string_ptr = GetFName( file_stack->srcfile)->name;

        } else {
            /* item is a (macro) line queue */
            inputline = fl->lines->head;
#ifdef DEBUG_OUT
            if ( fl->macro )
                DebugMsg(("%lu. GetTextLine(%d) stack=%X (macro=%s) inputline=%X\n", fl->line_num, queue_level, file_stack, fl->macro->name, inputline ));
            else
                DebugMsg(("%lu. GetTextLine(%d) stack=%X inputline=%X\n", fl->line_num, queue_level, file_stack, inputline ));
#endif
            if( inputline != NULL ) {
                fl->line_num++;
                strcpy( string, inputline->line );
                fl->lines->head = inputline->next;
                SrcFree( inputline );
                return( string );
            }
            queue_level--;
            file_stack = fl->next;
            SrcFree( fl->lines );
            SrcFree( fl );
            break;
        }
#ifdef DEBUG_OUT
        if ( file_stack )
            if ( file_stack->islinesrc )
                if ( file_stack->macro )
                    DebugMsg(("GetTextLine(%d) new queue level, stack=%X (macro=%s), inputline=%X string=>%s<, next #=%u\n",
                              queue_level, file_stack, file_stack->macro->name, inputline, string, file_stack->line_num ));
                else
                    DebugMsg(("GetTextLine(%d) new queue level, stack=%X (macro=%d), inputline=%X string=>%s<\n",
                              queue_level, file_stack, file_stack->macro, inputline, string ));
            else
                DebugMsg(("GetTextLine(%d) new queue level, stack=%X (%s), inputline=%X string=>%s<, next #=%u\n",
                          queue_level, file_stack, GetFName( file_stack->srcfile )->name, inputline, string, file_stack->line_num  ));
        else
            DebugMsg(("GetTextLine(%d) new queue level, stack=NULL, next #=%u\n", queue_level, file_stack->line_num ));
#endif
    }
    return( NULL ); /* end of main source file reached */
}

// add a string to the include path.
// called for -I cmdline options.
// the include path is rebuilt for each assembled module

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
        IncludePath = SrcAlloc( strlen( tmp ) + strlen( INC_PATH_DELIM_STR ) +
                                len + 1 );
        strcpy( IncludePath, tmp );
        strcat( IncludePath, INC_PATH_DELIM_STR );
        strcat( IncludePath, string );
        SrcFree( tmp );
    }
}

// Initializer, called once for each module.
// It's called very early, cmdline options aren't set yet.
// So no debug displays possible in here!

void InputInit( void )
/********************/
{
    file_list   *fl;
    char        path[_MAX_PATH];
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];

    //DebugMsg(( "InputInit()\n" ));
    cnt_fnames = 0;
    FNames = NULL;
    IncludePath = NULL;
    file_stack = NULL;
#if FILESEQ
    FileSeq.head = NULL;
#endif
    fl = PushLineSource( FALSE, NULL );
    fl->file = FileInfo.file[ASM];
    fl->srcfile = ModuleInfo.srcfile = AddFile( FileInfo.fname[ASM] );

#ifdef DEBUG_OUT
    cntppl0 = 0;
    cntppl1 = 0;
    cntppl2 = 0;
    cnttok0 = 0;
    cnttok1 = 0;
#endif

    /* add path of main module to the include path */
    _splitpath( FileInfo.fname[ASM], drive, dir, NULL, NULL );
    _makepath( path, drive, dir, NULL, NULL );
    AddStringToIncludePath( path );

}

/* init for each pass */

void InputPassInit( void )
/************************/
{
    line_queue = NULL;
    queue_level = 0;
    file_stack->line_num = 0;

    MacroLevel = 0;
    MacroLocals = 0;
    SkipMacroMode = FALSE;
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
    if ( IncludePath )
        SrcFree( IncludePath );
    FreeFiles();
#ifdef DEBUG_OUT
    if ( Options.quiet == FALSE )
        printf("invokations: GetPreprocessedLine=%u/%u/%u, Tokenize=%u/%u\n", cntppl0, cntppl1, cntppl2, cnttok0, cnttok1 );
#endif
}

// multi lines must be concatenated BEFORE the macro expansion step is done
// dont concat EQU, MACRO, ECHO [v2.0: + FORC/IRPC ] lines!

static bool IsMultiLine(void)
/***************************/
{
    asm_sym *sym;
    int i = 0;

    if ( AsmBuffer[1]->token == T_COLON )
        i = 2;
    if ( AsmBuffer[1]->token == T_DIRECTIVE && AsmBuffer[1]->value == T_EQU )
        return(FALSE);
    else if ( AsmBuffer[i]->token == T_ID ) {
        sym = SymSearch(AsmBuffer[i]->string_ptr);
        if (sym && (sym->state == SYM_MACRO))
            return(FALSE);
    } else if ( AsmBuffer[i]->token == T_DIRECTIVE &&
               ( AsmBuffer[i]->value == T_ECHO ||
                AsmBuffer[i]->value == T_FORC ||
                AsmBuffer[i]->value == T_IRPC ) ) {
        return( FALSE );
    }
    return( TRUE );
}

// INCLUDE directive.
// If a full path is specified, the directory where the included file
// is located becomes the "source" directory, that is, it is searched
// FIRST if further INCLUDE directives are found inside the included file.

static void IncludeDirective( int i )
/***********************************/
{
    int size;

    DebugMsg(("IncludeDirective enter\n"));

    if ( FileInfo.file[LST] ) {
        LstWriteSrcLine();
    }

    if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM) {
        AsmError( EXPECTED_FILE_NAME );
        return;
    }

    /* if the filename is enclosed in <>, just use this literal */

    if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<') {
        if (AsmBuffer[i+1]->token != T_FINAL) {
            AsmError( SYNTAX_ERROR );
        } else {
            InputQueueFile( AsmBuffer[i]->string_ptr, NULL );
        }
    } else {
        char * p;
        /* if the filename isn't enclosed in <>, use the current position.
         However, the filename may begin with a number, in case of which
         the pos field isn't set, so better start with the pos field of
         the INCLUDE directive itself!
         */
        p = AsmBuffer[i-1]->pos + 7; /* 7 = sizeof("INCLUDE") */
        while (isspace(*p)) p++;
        size = strlen(p);
        while (size && isspace(*(p+size-1))) size--;
        *(p+size) = NULLC;
        InputQueueFile( p, NULL );
    }
    return;
}

// GetPreprocessedLine:
// this is the "preprocessor"
// 1. a line is read with GetTextLine()
// 2. the line is tokenized with Tokenize(), Token_Count set
// 3. (text) macros are expanded by ExpandLine()
// 4. "preprocessor" directives are executed

int GetPreprocessedLine( char *string )
/*************************************/
{
    int i;

    if( GetTextLine( string, MAX_LINE_LEN ) == NULL ) {
        DebugMsg(("%lu. GetPreprocessedLine: GetTextLine() returned NULL (end of file)\n", GetTopLine() ));
        return -1; // EOF
    }

    // Token_Count is the number of tokens scanned
    Token_Count = Tokenize( string, 0);

    // if a comma is last token, concat lines ... with some exceptions

    if ( Token_Count > 1 && AsmBuffer[Token_Count-1]->token == T_COMMA ) {
        DebugMsg(("%lu. GetPreprocessedLine: calling IsMultiLine()\n", GetTopLine() ));
        if ( IsMultiLine() ) {
            char * ptr = string;
            DebugMsg(("IsMultiLine()=TRUE\n"));
            do {
                ptr = ptr + strlen( ptr );
                if ( GetTextLine( ptr, MAX_LINE_LEN) == NULL )
                    return -1;
                while (isspace(*ptr)) ptr++;
                if ( *ptr == NULLC )
                    break;
                if ( (Token_Count = Tokenize( ptr, Token_Count )) == 0 )
                    //return -1; /* v2.01: too many tokens?   */
                    break;       /* then just skip this line! */
            } while ( AsmBuffer[Token_Count-1]->token == T_COMMA );
        }
    }

#ifdef DEBUG_OUT
    cntppl0++;
    if ( file_stack && file_stack->islinesrc ) {
        if ( file_stack->macro )
            DebugMsg(("%lu. GetPreprocessedLine(mac=%s): >%s<\n", file_stack->line_num, file_stack->macro->name, string ));
        else
            DebugMsg(("%lu. GetPreprocessedLine: >%s<\n", file_stack->line_num, string ));
    } else if ( file_stack && file_stack->srcfile ) {
        DebugMsg(("%lu. GetPreprocessedLine(%s): >%s<\n", file_stack->line_num, GetFName( file_stack->srcfile )->name, string ));
    } else
        DebugMsg(("%lu. GetPreprocessedLine: >%s<\n", file_stack->line_num, string));
#endif

    /* no expansion if current macro is skipped (EXITM does this) */
    /* then all what has to run are the conditional directives */
    /* (this could possibly be improved) */

    if (SkipMacroMode == FALSE)

     /* expand (text) macros. If expansion occured, rescan the line */

    while (Token_Count > 0 && ExpandLine( string ) == STRING_EXPANDED) {
        DebugMsg(("%lu. GetPreprocessedLine: expanded line is >%s<\n", file_stack->line_num, string));
        Token_Count = Tokenize( string, 0 );
    }

    if (Token_Count == 0)
        return(0);

#ifdef DEBUG_OUT
    cntppl1++;
#endif

    i = 0;
    if (Token_Count > 2 && AsmBuffer[1]->token == T_COLON)
        i = 2;

    // handle "preprocessor" directives:
    // INCLUDE
    // IF, ELSE, ENDIF, ...
    // .ERR, ...
    // FOR, REPEAT, WHILE, ...

    if ( AsmBuffer[i]->token == T_DIRECTIVE &&
        AsmBuffer[i]->dirtype >= DRT_CONDDIR &&
        AsmBuffer[i]->dirtype <= DRT_INCLUDE ) {

        if ( i > 1 && SkipMacroMode == FALSE ) {
            if (AsmBuffer[i-2]->token != T_ID) {
                AsmError( SYNTAX_ERROR );
                return(0);
            }
            if ( LabelCreate( AsmBuffer[i-2]->string_ptr, MT_NEAR, NULL, TRUE ) == NULL )
                return(0);
        }

        if ( AsmBuffer[i]->value == T_INCLUDE ) {
            if ( SkipMacroMode == FALSE )
                IncludeDirective( i + 1 );
        } else if ( AsmBuffer[i]->dirtype == DRT_CONDDIR ) { /* conditional directive? */
            conditional_assembly_directive( i, AsmBuffer[i]->value );
        } else if ( AsmBuffer[i]->dirtype == DRT_ERRDIR ) { /* error directive? */
            if (SkipMacroMode == FALSE)
                conditional_error_directive( i );
        } else if ( AsmBuffer[i]->dirtype == DRT_LOOPDIR ) { /* loop directive? */
            if (SkipMacroMode == FALSE)
                LoopDirective ( i+1, AsmBuffer[i]->value );
        }
        return( 0 );
    }
    if ( SkipMacroMode )
        return( Token_Count );

    if (Token_Count > 1 &&  AsmBuffer[1]->token == T_DIRECTIVE ) {
        switch (AsmBuffer[1]->value) {
        case T_MACRO:
            MacroDef ( 0 );
            return(0);
#if FASTPASS
        case T_EQU:
            /*
             this case deserves explanation. It didn't exist in v1.90-1.93,
             which was an error. If an EQU directive defines a text equate
             it MUST be handled HERE and 0 must be returned to the caller.
             This will prevent function OnePass() in assemble.c to store the
             "preprocessed" line. For EQU, the "preprocessed" line was NOT
             expanded in ExpandLine(), because the unexpanded content of
             the line must be assigned to the equate id.
             */
            if ( AsmBuffer[1]->dirtype == 0 ) {  /* is 0 for true EQU */
                if( AsmBuffer[0]->token == T_ID ) {
                    asm_sym *sym;
                    if ( sym = CreateConstant( FALSE ) ) {
                        if ( ModuleInfo.list == TRUE ) {
                            LstWrite( LSTTYPE_EQUATE, 0, sym );
                        }
                        if ( sym->state != SYM_TMACRO ) {
                            if ( Options.preprocessor_stdout == TRUE )
                                WritePreprocessedLine( string );
                            if ( StoreState )
                                StoreLine( string );
                        }
                    }
                    return( 0 );
                }
            }
            break;
#else
        case T_EQU:
            DefineConstant( AsmBuffer[1]->dirtype == DRT_EQUALSGN );
            return(0);
        case T_SIZESTR:
            SizeStrDef( 1 );
            return(0);
        case T_INSTR:
            InStrDef( 1 );
            return(0);
#endif
        case T_TEXTEQU:
        case T_CATSTR: /* TEXTEQU and CATSTR are synonyms! */
            CatStrDef( 1 );
            return(0);
        case T_SUBSTR:
            SubStrDef( 1 );
            return(0);
        }
    }
    /* is a user-defined prologue macro set? */
    if ( DefineProc &&
         ModuleInfo.proc_prologue &&
         *ModuleInfo.proc_prologue )
        proc_check();

#ifdef DEBUG_OUT
    cntppl2++;
#endif
    return( Token_Count );
}
