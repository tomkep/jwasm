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

#include "globals.h"
#include "autodept.h"
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

extern bool expansion;
extern bool DefineProc;
extern bool SkipMacroMode;
extern int  MacroLocals;

#define DETECTEOC 1
#define DETECTCTRLZ 1

int Token_Count;    // number of tokens in curr line
int queue_level;    // number of open (macro) queues
static int restorelinenumber;
uint_8 MacroLevel;  // current macro nesting level

#if DETECTEOC
extern bool inside_comment;
#endif

// MemAlloc() uses the normal C heap functions
// AsmAlloc() uses the "fast" replacement if FASTMEM=1

#if 1
#define SrcAlloc(x) MemAlloc(x)
#define SrcFree(x)  MemFree(x)
#else
#define SrcAlloc(x) AsmAlloc(x)
#define SrcFree(x)  AsmFree(x)
#endif

/* NOTE: the line queue is a simple list of lines.
 if it must be nested, it is converted to a file_list item
 and pushed onto the file stack.
 */
typedef struct file_list {
    struct file_list    *next;
    union {
        FILE            *file;
        struct input_queue  *lines;
    };
    uint                srcfile;    /* index of file in FNames */
    uint_32             line_num;   /* current line in parent file */
    asm_sym             *macro;     /* the symbol if it is a macro */
    unsigned int        hidden:1;
} file_list;

FNAME                   *FNames;    /* list of input files */

input_queue             *line_queue;    /* line queue */
static file_list        *file_stack;    /* source item (file/macro) stack */
static char             *IncludePath;

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

// check if a file is in the file list
// if yes, return the FNAME entry;
// if no, add the file to the list's tail.

static uint AddFlist( char const *name )
/***************************************/
{
    FNAME   *curr;
    FNAME   *last;
    uint    index;
    char    *fname;
    char    buff[_MAX_PATH];

    DebugMsg(("AddFlist(%s) enter\n", name ));
    fname = GetFilenameFullPath( buff, name, sizeof( buff ) );
    DebugMsg(("AddFlist: full path=%s\n", fname ));
    for( index = 0, curr = FNames; curr != NULL; index++, curr = curr->next ) {
        if( filecmp( name, curr->name ) == 0 )
            return( index );
        if( filecmp( fname, curr->fullname ) == 0 )
            return( index );
        last = curr;
    }

    curr = (FNAME *)AsmAlloc( sizeof( FNAME ) );

    curr->next = NULL;
    curr->mtime = GetFilenameTimeStamp( fname );
    curr->name = (char *)AsmAlloc( strlen( name ) + 1 );
    strcpy( curr->name, name );
    curr->fullname = (char *)AsmAlloc( strlen( fname ) + 1 );
    strcpy( curr->fullname, fname );

    if( FNames == NULL ) {
        FNames = curr;
    } else {
        last->next = curr;
    }
    return( index );
}

const FNAME *GetFName( uint index )
/*********************************/
{
    FNAME   *curr;
    for( curr = FNames; index && curr ; index--, curr = curr->next );
    return( curr );
}

#if FASTMEM==0

// free all items in the file list.
// unnecessary if FASTMEM==1.

static void FreeFlist( void )
/***************************/
{
    const FNAME   *curr;
    const FNAME   *next;

    for( curr = FNames; curr != NULL; ) {
        AsmFree( curr->fullname );
        AsmFree( curr->name );
        next = curr->next;
        AsmFree( (void *)curr );
        curr = next;
    }
    return;
}
#endif

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
    for( ; file_stack; file_stack = nextfile ) {
        nextfile = file_stack->next;
        if ( file_stack->macro ) {
            FreeLineQueue( file_stack->lines );
        } else {
            fclose( file_stack->file );
        }
        SrcFree( file_stack );
    }
    return;
}

/* read one line from current source file */
/* returns FALSE if EOF has been detected */

static bool GetLineFromFile( char *ptr, unsigned max, FILE *fp )
/**********************************************************/
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
    if (inside_comment) {
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
                    LineNumber++;
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

void RestoreLineNumber( void )
{
    if ( restorelinenumber ) {
        LineNumber = restorelinenumber;
        restorelinenumber = 0;
    }
}

/*
 create a new assembly line source (is a macro or file).
 add the new item to the top of the file stack.
 name = name (or NULL if it's the current line queue )
 sym = macro symbol or NULL (for a real file or the line queue)
 */
static file_list *PushLineSource( const char *name, asm_sym *sym )
/**************************************************************/
{
    file_list   *new;

    new = SrcAlloc( sizeof( file_list ) );
    new->next = file_stack;
    file_stack = new;
    new->line_num = restorelinenumber ? restorelinenumber : LineNumber;
    new->hidden = FALSE;
    new->macro = sym;
    if( sym ) {
        dir_node *dir = (dir_node *)sym;
        new->srcfile = dir->e.macroinfo->srcfile;
        LineNumber = 0;
    } else if ( name == NULL ) {
        new->srcfile = get_curr_srcfile();
        new->macro = (asm_sym *)-1;
    } else {
        new->srcfile = AddFlist( name );
        LineNumber = 0;
    }
    restorelinenumber = 0;
    return( new );
}

/*
 If there's a current line queue, push it onto the file stack.
 */

void PushLineQueue( void )
/************************/
{
    DebugMsg(( "PushLineQueue() enter [line_queue=%X]\n", line_queue ));

    /* if the line queue is in use, push it onto the file stack */
    if ( line_queue ) {
        file_list *fl;
        fl = PushLineSource( NULL, NULL );
        fl->lines = line_queue;
        fl->hidden = TRUE;
        line_queue = NULL;
    }
}

/* Add a line to the current line queue.
 If there's no current line queue, create one.
 */

void AddLineQueue( char *line )
/*****************************/
{
    unsigned i = strlen(line) + 1;
    line_list   *new;

    DebugMsg(( "AddLineQueue(#=%u): %s\n", LineNumber, line ));

    new = SrcAlloc( sizeof( line_list ) + i );
    new->next = NULL;

    /* if line queue is empty, create one */
    if( line_queue == NULL ) {
        line_queue = SrcAlloc( sizeof( input_queue ) );
        line_queue->head = line_queue->tail = NULL;
        queue_level++;
    }

    if( line_queue->tail == NULL ) {
        line_queue->head = new;
    } else {
        /* insert at the tail */
        line_queue->tail->next = new;
    }
    line_queue->tail = new;
    memcpy( new->line, line, i );
    return;
}

// push the current line queue onto the file stack and
// associate a macro name to it so it can be displayed
// in case of errors.

void PushMacro( asm_sym * sym )
/*****************************/
{
    file_list *fl;

    DebugMsg(( "%lu. PushMacro(%s), queue level=%u, old macro level=%u, restorelinenumber=%u\n", LineNumber, sym->name, queue_level, MacroLevel, restorelinenumber ));
    if ( queue_level >= MAX_QUEUE_NESTING )
        Fatal( FATAL_NESTING_LEVEL_TOO_DEEP );
    fl = PushLineSource( sym->name, sym );
    fl->lines = line_queue;
    fl->hidden = *sym->name ? FALSE : TRUE;
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

    DebugMsg(( "%lu. PushMacroGoto(%s), queue level=%u, old macro level=%u, restorelinenumber=%u\n", LineNumber, macro->name, queue_level, MacroLevel, restorelinenumber ));
    fl = PushLineSource( macro->name, macro );
    fl->lines = line_queue;
    fl->hidden = *macro->name ? FALSE : TRUE;
    line_queue = NULL;
    MacroLevel++;

    fl->line_num = restorelinenumber;
    restorelinenumber = 0;
    LineNumber = line;
}

uint get_curr_srcfile( void )
/****************************/
{
    return( file_stack == NULL ? ModuleInfo.srcfile : file_stack->srcfile );
}

/* for error listing, render the current source file and line */
/* this function is also called if pass is > 1,
 * which is a problem for FASTPASS because the file stack is empty.
 */
int GetCurrSrcPos( char * buffer)
/*******************************/
{
    uint fi;
    file_list *fl;
    uint_32 line;
    const FNAME * fn;

    line = LineNumber;
    fi = ModuleInfo.srcfile;
    for( fl = file_stack; fl; fl = fl->next ) {
        if ( fl->macro == NULL ) {
            fi = fl->srcfile;
            break;
        }
        line = fl->line_num;
    }

    fn = GetFName(fi);
    if (fn)
        if (line)
            return( sprintf( buffer, "%s(%lu): ", fn->name , line ) );
        else
            return( sprintf( buffer, "%s: ", fn->name ) );
    *buffer = NULLC;
    return( 0 );
}

/* for error listing, render the macro/include file hierarchy */

void print_include_file_nesting_structure( void )
/***********************************************/
{
    file_list       *fl;
    unsigned        tab = 1;
    int             line = LineNumber;

    fl = file_stack;
    if( fl == NULL )
        return;

    for( ; fl != NULL ; fl = fl->next ) {
        if( !fl->hidden ) {
            if( fl->macro == NULL ) {
                PrintNote( NOTE_INCLUDED_BY, tab, "", GetFName(fl->srcfile)->name, line );
            } else if ( fl->macro != (asm_sym *)-1 ) {
                char fname[_MAX_FNAME+_MAX_EXT];
                char fext[_MAX_EXT];
                _splitpath( GetFName(((dir_node *)fl->macro)->e.macroinfo->srcfile)->name, NULL, NULL, fname, fext );
                strcat( fname, fext );
                PrintNote( NOTE_MACRO_CALLED_FROM, tab, "", fl->macro->name, line, fname );
            }
            tab++;
        }
        line = fl->line_num;
    }
    PrintNote( NOTE_MAIN_LINE_CODE, tab, "", GetFName(ModuleInfo.srcfile)->name, line );

}

// Scan the include path for a file!
// variable IncludePath also contains directories set with -I cmdline option

static FILE *open_file_in_include_path( char *name, char *fullpath )
/******************************************************************/
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

ret_code InputQueueFile( char *path, FILE * *pfile )
/**************************************************/
{
    FILE        *file = NULL;
    file_list   *fl;
    char        *tmp;
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
            if ( fl->macro == NULL ) {
                _splitpath( GetFName(fl->srcfile)->fullname, drive2, dir2, NULL, NULL );
                DebugMsg(("InputQueueFile(): curr src=%s, split into drive=%s, dir=%s\n", GetFName(fl->srcfile)->fullname, drive2, dir2 ));
                if ( dir2[0] == '\\' || dir2[0] == '/' ) {
                    _makepath( fullpath, drive2, dir2, fname, ext );
                    DebugMsg(("InputQueueFile(): makepath()=%s\n", fullpath ));
                    file = fopen( fullpath, "rb" );
                    //DebugMsg(("InputQueueFile(): fopen()=%X\n", file ));
                    tmp = fullpath;
                }
                break;
            }
        }
    }
    if ( file == NULL ) {
        _makepath( fullpath, drive, dir, fname, ext );
        DebugMsg(("InputQueueFile(): makepath()=%s\n", fullpath ));
        file = fopen( fullpath, "rb" );
        //DebugMsg(("InputQueueFile(): fopen()=%X\n", file ));
        tmp = path;
        if( file == NULL &&
            IncludePath != NULL &&
            /* don't search include path if an absolute path is given */
            dir[0] != '\\' && dir[0] != '/') {
            tmp = buffer;
            file = open_file_in_include_path( path, tmp );
        }
    }
    if( file == NULL ) {
        AsmErr( CANNOT_OPEN_INCLUDE_FILE, fullpath );
        return( ERROR );
    }
    if ( pfile )
        *pfile = file;
    else {
        fl = PushLineSource( tmp, NULL );
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

    if ( restorelinenumber ) {
        DebugMsg(( "GetTextLine: line number restored, old #=%u, new #=%u\n", LineNumber, restorelinenumber ));
        LineNumber = restorelinenumber;
        restorelinenumber = 0;
        //MacroLevel--;
    }

    /* Now check the file stack!
     * The file stack may contain
     * - pushed line queues (macro == -1)
     * - macro line queues (macro != NULL && macro != -1)
     * - assembly include files. (macro == NULL)
     */

    while( file_stack != NULL ) {
        fl = file_stack;
        if( fl->macro == NULL ) {
            if( GetLineFromFile( string, max, fl->file ) ) {
                LineNumber++;
                return( string );
            }
            /* EOF reached, remove item from file stack! */
            file_stack = fl->next;
            fclose( fl->file );
            LineNumber = fl->line_num;
            SrcFree( fl );
        } else {
            /* item is a (macro) line queue */
            inputline = fl->lines->head;
            LineNumber++;
#ifdef DEBUG_OUT
            if ( fl->macro != (asm_sym *)-1)
                DebugMsg(("%lu. GetTextLine(%d) stack=%X (macro=%s) inputline=%X\n", LineNumber, queue_level, file_stack, fl->macro->name, inputline ));
            else
                DebugMsg(("%lu. GetTextLine(%d) stack=%X (macro=%d) inputline=%X\n", LineNumber, queue_level, file_stack, fl->macro, inputline ));
#endif
            if( inputline != NULL ) {

//                if ( fl->macro != (asm_sym *)-1) {
//                    MacroLevel = inputline->macrolevel;
//                }
                strcpy( string, inputline->line );
                fl->lines->head = inputline->next;
                SrcFree( inputline );
                /* if there's another line, exit! */
                if ( fl->lines->head )
                    return( string );
            }
            /* free the file_list item NOW, don't wait till the
             next line is read. That's because macro loops will
             exit when the ENDM line is found.
             */
            queue_level--;
            file_stack = fl->next;
#ifdef DEBUG_OUT
            if ( file_stack )
                if ( file_stack->macro )
                    if ( file_stack->macro != (asm_sym *)-1)
                        DebugMsg(("GetTextLine(%d) new queue level, stack=%X (macro=%s), inputline=%X string=>%s<, next #=%u\n",
                                  queue_level, file_stack, file_stack->macro->name, inputline, string, fl->line_num ));
                    else
                        DebugMsg(("GetTextLine(%d) new queue level, stack=%X (macro=%d), inputline=%X string=>%s<\n",
                                  queue_level, file_stack, file_stack->macro, inputline, string ));
                else
                    DebugMsg(("GetTextLine(%d) new queue level, stack=%X (%s), inputline=%X string=>%s<, next #=%u\n",
                              queue_level, file_stack, GetFName(file_stack->srcfile)->name, inputline, string, fl->line_num  ));
            else
                DebugMsg(("GetTextLine(%d) new queue level, stack=NULL, next #=%u\n", queue_level, fl->line_num ));
#endif
            SrcFree( fl->lines );
            restorelinenumber = fl->line_num;
            SrcFree( fl );
            if ( inputline )
                return( string );
        }
    }
    /* Nothing on the file stack anymore.
     * So read main source file!
     */
    if( GetLineFromFile( string, max, FileInfo.file[ASM] ) ) {
        LineNumber++;
        return( string );
    }
    return( NULL ); /* end of main source file reached */
}

// add a string to the include path.
// called for -I cmdline options.
// the include path is rebuilt for each assembled module

void AddStringToIncludePath( char *string )
/*****************************************/
{
    char *tmp;

    DebugMsg(("AddStringToIncludePath(%s) enter\n", string ));
    while( isspace( *string ) )
        string++;
    if ( *string == NULLC )
        return;
    if( IncludePath == NULL ) {
        IncludePath = SrcAlloc( strlen( string ) + 1 );
        strcpy( IncludePath, string );
    } else {
        tmp = IncludePath;
        IncludePath = SrcAlloc( strlen( tmp ) + strlen( INC_PATH_DELIM_STR ) +
                                strlen( string ) + 1 );
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
    char        path[_MAX_PATH];
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];

    //DebugMsg(( "InputInit()\n" ));
    FNames = NULL;
    IncludePath = NULL;
    file_stack = NULL;
#ifdef DEBUG_OUT
    cntppl0 = 0;
    cntppl1 = 0;
    cntppl2 = 0;
    cnttok0 = 0;
    cnttok1 = 0;
#endif

    // add main source file to file list
    ModuleInfo.srcfile = AddFlist( FileInfo.fname[ASM] );

    /* add path of main module to the include path */
    _splitpath( FileInfo.fname[ASM], drive, dir, NULL, NULL );
    _makepath( path, drive, dir, NULL, NULL );
    AddStringToIncludePath( path );

}

/* init for each pass */

void InputPassInit( void )
/*************************/
{
    line_queue = NULL;
    queue_level = 0;
    restorelinenumber = 0;

    MacroLevel = 0;
    MacroLocals = 0;
    SkipMacroMode = FALSE;
}

void InputFini( void )
/*********************/
{
#ifdef DEBUG_OUT
    const FNAME   *curr;
    int   i;
    for( i = 0, curr = FNames; curr != NULL; curr = curr->next, i++) {
        DebugMsg(( "InputFini: file %u name=%s full=%s\n", i, curr->name, curr->fullname ));
    }
#endif
    SrcFree( IncludePath );
#if FASTMEM==0
    FreeFlist();
#endif
#ifdef DEBUG_OUT
    if ( Options.quiet == FALSE )
        printf("invokations: GetPreprocessedLine=%u/%u/%u, Tokenize=%u/%u\n", cntppl0, cntppl1, cntppl2, cnttok0, cnttok1 );
#endif
}

// multi lines must be concatenated BEFORE the macro expansion step is done
// dont concat EQU, MACRO or ECHO lines!

static bool IsMultiLine(void)
/********************/
{
    asm_sym *sym;
    int i = 0;

    if (AsmBuffer[1]->token == T_COLON)
        i = 2;
    if (AsmBuffer[1]->token == T_DIRECTIVE && AsmBuffer[1]->value == T_EQU)
        return(FALSE);
    else if (AsmBuffer[i]->token == T_ID) {
        sym = SymSearch(AsmBuffer[i]->string_ptr);
        if (sym && (sym->state == SYM_MACRO))
            return(FALSE);
    } else if (AsmBuffer[i]->token == T_DIRECTIVE && AsmBuffer[i]->value == T_ECHO) {
        return(FALSE);
    }
    return(TRUE);
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
        DebugMsg(("%lu. GetPreprocessedLine() returns -1 (end of file)\n", LineNumber ));
        return -1; // EOF
    }

    // Token_Count is the number of tokens scanned
    Token_Count = Tokenize( string, 0);

    // if a comma is last token, concat lines ... with some exceptions

    if (Token_Count > 1 && AsmBuffer[Token_Count-1]->token == T_COMMA) {
        DebugMsg(("%lu. GetPreprocessedLine: calling IsMultiLine()\n", LineNumber ));
        if (IsMultiLine()) {
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
                    return -1;
            } while ( AsmBuffer[Token_Count-1]->token == T_COMMA );
        }
    }

#ifdef DEBUG_OUT
    cntppl0++;
    if ( file_stack && file_stack->macro ) {
        if ( file_stack->macro != (asm_sym *)-1 )
            DebugMsg(("%lu. GetPreprocessedLine(mac=%s): >%s<\n", LineNumber, file_stack->macro->name, string ));
        else
            DebugMsg(("%lu. GetPreprocessedLine: >%s<\n", LineNumber, string ));
    } else if ( file_stack && file_stack->srcfile ) {
        DebugMsg(("%lu. GetPreprocessedLine(%s): >%s<\n", LineNumber, GetFName(file_stack->srcfile)->name, string ));
    } else
        DebugMsg(("%lu. GetPreprocessedLine: >%s<\n", LineNumber, string));
#endif

    /* no expansion if current macro is skipped (EXITM does this) */
    /* then all what has to run are the conditional directives */
    /* (this could possibly be improved) */

    if (SkipMacroMode == FALSE)

     /* expand (text) macros. If expansion occured, rescan the line */

    while (Token_Count > 0 && ExpandLine( string ) == STRING_EXPANDED) {
        DebugMsg(("%lu. GetPreprocessedLine: expanded line is >%s<\n", LineNumber, string));
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
            CatStrDef( 1 );
            return(0);
        case T_CATSTR:
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
