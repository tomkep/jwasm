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


#include "globals.h"
#include "autodept.h"
#include <ctype.h>

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
extern bool SkipMacroMode;
extern bool DefineProc;

#define DETECTEOC 1

int Token_Count;    // number of tokens in curr line
int queue_level;    // number of open (macro) queues
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
    const FNAME         *srcfile;   /* name of include file */
    unsigned long       line_num;   /* current line in parent file */
    asm_sym             *macro;     /* the symbol if it is a macro */
    unsigned int        hidden:1;
    unsigned int        setlineno:1;
} file_list;

FNAME                   *FNames;    /* list of input files */

input_queue             *line_queue;    /* line queue */
static file_list        *file_stack;    /* source item (file/macro) stack */
static char             *IncludePath;

#if defined(__UNIX__)

#define INCLUDE_PATH_DELIM  ":"
#define DIR_SEPARATOR       '/'
#define DIR_SEP_STRING      "/"
#define filecmp strcmp

#else

#define INCLUDE_PATH_DELIM  ";"
#define DIR_SEPARATOR       '\\'
#define DIR_SEP_STRING      "\\"
#define filecmp stricmp

#endif

// add a file to the file list if it's not in there already

const FNAME *AddFlist( char const *name )
/***************************************/
{
    FNAME   *flist;
    FNAME   *last;
    //int     index;
    char    *fname;
    char    buff[_MAX_PATH];

    DebugMsg(("AddFlist(%s) enter\n", name ));
    fname = _getFilenameFullPath( buff, name, sizeof( buff ) );
    last = FNames;
    for( flist = last; flist != NULL; flist = flist->next ) {
        if( filecmp( name, flist->name ) == 0 )
            return( flist );
        if( filecmp( fname, flist->fullname ) == 0 )
            return( flist );
        last = flist;
    }

    flist = (FNAME *)AsmAlloc( sizeof( FNAME ) );

    flist->name = (char *)AsmAlloc( strlen( name ) + 1 );
    strcpy( flist->name, name );

    flist->fullname = (char *)AsmAlloc( strlen( fname ) + 1 );
    strcpy( flist->fullname, fname );

    flist->mtime = _getFilenameTimeStamp( fname );
    flist->next = NULL;
    if( FNames == NULL ) {
        FNames = flist;
    } else {
        last->next = flist;
    }
    return( flist );
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

static bool get_asmline( char *ptr, unsigned max, FILE *fp )
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
        c = getc( fp);
        while (c != EOF && c != '\n') {
            got_something = TRUE;
            if (c != '\r' && max > 1) {
                *ptr++ = c;
                max--;
            }
            c = getc( fp );
        }
        *ptr = NULLC;
        return( got_something );
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
    new->line_num = LineNumber;
    new->hidden = FALSE;
    new->setlineno = FALSE;
    new->macro = sym;
    if( sym ) {
        dir_node *dir = (dir_node *)sym;
        new->srcfile = dir->e.macroinfo->srcfile;
        LineNumber = 0;
    } else if ( name == NULL ) {
        new->srcfile = NULL;
        new->macro = (asm_sym *)-1;
    } else {
        new->srcfile = AddFlist( name );
        LineNumber = 0;
    }
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
/*******************************/
{
    unsigned i = strlen(line);
    line_list   *new;

    DebugMsg(( "AddLineQueue: %s  ( line %lu ) \n", line, LineNumber ));

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
    memcpy( new->line, line, i+1 );
    return;
}

// push the current line queue onto the file stack and
// associate a macro name to it so it can be displayed
// in case of errors.

void PushMacro( asm_sym * sym )
/*********************************************/
{
    file_list *fl;

    DebugMsg(( "PushMacro(%s), level=%u\n", sym->name, queue_level ));
    if ( queue_level >= MAX_QUEUE_NESTING )
        Fatal( MSG_NESTING_LEVEL_TOO_DEEP );
    fl = PushLineSource( sym->name, sym );
    fl->lines = line_queue;
    fl->hidden = *sym->name ? FALSE : TRUE;
    line_queue = NULL;

    if ( ModuleInfo.list && fl->hidden == FALSE ) {
        if ( ModuleInfo.list_macro == LM_LISTMACROALL || MacroLevel == 0 )
            LstWriteSrcLine();
    }
}

const FNAME *get_curr_srcfile( void )
/************************************/
{
    return( file_stack == NULL ? ModuleInfo.srcfile : file_stack->srcfile );
}

/* for error listing, render the current source file and line */

int GetCurrSrcPos( char * buffer)
{
    const FNAME *fn;
    file_list *fl;
    int line;

    line = LineNumber;
    for( fl = file_stack; fl; fl = fl->next ) {
        if ( fl->macro == NULL )
            break;
        line = fl->line_num;
    }

    if ( fl )
        fn = fl->srcfile;
    else
        fn = ModuleInfo.srcfile;

    if ( fn == NULL )
        return( 0 );

    return( sprintf( buffer, "%s(%u): ", fn->name, line ) );
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
                PrintNote( NOTE_INCLUDED_BY, tab, "", fl->srcfile->name, line );
            } else if ( fl->srcfile ) {
                char fname[_MAX_FNAME+_MAX_EXT];
                char fext[_MAX_EXT];
                _splitpath( ((dir_node *)fl->macro)->e.macroinfo->srcfile->name, NULL, NULL, fname, fext );
                strcat( fname, fext );
                PrintNote( NOTE_MACRO_CALLED_FROM, tab, "", fl->macro->name, line, fname );
            }
            tab++;
        }
        line = fl->line_num;
    }
    PrintNote( NOTE_MAIN_LINE_CODE, tab, "", ModuleInfo.srcfile->name, line );

}

// open a file. Scan the include path!
// The include path also contains directories set with -I cmdline option

static FILE *open_file_in_include_path( char *name, char *fullpath )
/******************************************************************/
{
    char            *inc_path_list;
    char            *next_path;
    char            buffer[MAX_LINE_LEN];
    FILE            *file = NULL;

    while( isspace( *name ) )
        name++;

    inc_path_list = (char *)AsmTmpAlloc( strlen( IncludePath ) + 1 );
    strcpy( inc_path_list, IncludePath );
    next_path = strtok( inc_path_list, INCLUDE_PATH_DELIM ";");

    while( ( file == NULL ) && ( next_path != NULL ) ) {
        strcpy( buffer, next_path );
        //NYI: this is no good for DOS - have to check '/', '\\', and ':'
        if( buffer[ strlen( buffer ) - 1] != DIR_SEPARATOR ) {
            strcat( buffer, DIR_SEP_STRING );
        }
        strcat( buffer, name );

//      file = fopen( buffer, "r" );
        file = fopen( buffer, "rb" );
        if( file ) {
            break;
        }
        next_path = strtok( NULL, INCLUDE_PATH_DELIM ";");
    }
    strcpy( fullpath, buffer );
    return( file );
}

// the worker behind the INCLUDE directive

ret_code InputQueueFile( char *path, FILE * *pfile )
/******************************/
{
    FILE        *file = NULL;
    file_list   *new;
    char        fullpath[ _MAX_PATH ];
    char        buffer[ _MAX_PATH ];
    char        *tmp;
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    char        drive2[_MAX_DRIVE];
    char        dir2[_MAX_DIR];

    DebugMsg(("InputQueueFile(%s) enter\n", path ));

    _splitpath( path, drive, dir, fname, ext );

    /* if there is no absolute path given, first search in the directory
     of the current source file */

    if (dir[0] != '\\' && dir[0] != '/' && file_stack && file_stack->macro == NULL ) {
        _splitpath( file_stack->srcfile->fullname, drive2, dir2, NULL, NULL );
        if (dir2[0] == '\\' || dir2[0] == '/') {
            _makepath( fullpath, drive2, dir2, fname, ext );
            //file = fopen( fullpath, "r" );
            file = fopen( fullpath, "rb" );
            tmp = fullpath;
        }
    }
    if ( file == NULL ) {
        _makepath( fullpath, drive, dir, fname, ext );
        //file = fopen( fullpath, "r" );
        file = fopen( fullpath, "rb" );
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
    } else {
        if ( pfile )
            *pfile = file;
        else {
            new = PushLineSource( tmp, NULL );
            new->file = file;
        }
        return( NOT_ERROR );
    }
}

static char *input_get( char *string )
/************************************/
{
    line_list   *inputline;
    file_list   *inputfile;

    /* Check the current line_queue first */

    /* the line_queue is global and there is ONE only.
     If it must be nested, the current line queue is pushed onto
     the file stack.
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

    while( file_stack != NULL ) {
        inputfile = file_stack;
        if ( inputfile->setlineno ) {
            LineNumber = inputfile->line_num;
            inputfile->setlineno = FALSE;
            MacroLevel--;
        }
        if( inputfile->macro == NULL ) {
            if( get_asmline( string, MAX_LINE_LEN, inputfile->file ) ) {
                LineNumber++;
                return( string );
            }
            /* EOF is reached */
            file_stack = inputfile->next;
            fclose( inputfile->file );
            LineNumber = inputfile->line_num;
            SrcFree( inputfile );
        } else {
            /* this "file" is just a line queue for a macro */
            inputline = inputfile->lines->head;
#ifdef DEBUG_OUT
            if ( inputfile->macro != (asm_sym *)-1)
                DebugMsg(("input_get(%d) stack=%X (%s) inputline=%X\n", queue_level, file_stack, inputfile->macro->name, inputline ));
            else
                DebugMsg(("input_get(%d) stack=%X (macro=%d) inputline=%X\n", queue_level, file_stack, inputfile->macro, inputline ));
#endif
            LineNumber++;
            if( inputline != NULL ) {
                if ( inputfile->macro != (asm_sym *)-1) {
                    MacroLevel = inputline->macrolevel;
                }
                strcpy( string, inputline->line );
                inputfile->lines->head = inputline->next;
                SrcFree( inputline );
                /* if there's another line, exit! */
                if ( inputfile->lines->head )
                    return( string );
            }
            /* free the file_list item NOW, don't wait till the
             next line is read. That's because macro loops will
             exit when the ENDM line is found.
             */
            queue_level--;
            file_stack = inputfile->next;
#ifdef DEBUG_OUT
            if ( file_stack )
                if ( file_stack->macro )
                    if ( file_stack->macro != (asm_sym *)-1)
                        DebugMsg(("input_get(%d) new queue level, stack=%X (%s), inputline=%X string=>%s<\n", queue_level, file_stack, file_stack->macro->name, inputline, string ));
                    else
                        DebugMsg(("input_get(%d) new queue level, stack=%X (macro=%d), inputline=%X string=>%s<\n", queue_level, file_stack, file_stack->macro, inputline, string ));
                else
                    DebugMsg(("input_get(%d) new queue level, stack=%X (%s), inputline=%X string=>%s<\n", queue_level, file_stack, file_stack->srcfile->name, inputline, string ));
            else
                DebugMsg(("input_get(%d) new queue level, stack=NULL\n", queue_level ));
#endif
            SrcFree( inputfile->lines );
            inputfile->setlineno = TRUE;
            SrcFree( inputfile );
            if ( inputline )
                return( string );
        }
    }
    MacroLevel = 0;
    return( NULL );
}

char *ReadTextLine( char *string, int max )
/********************************/
{
    /* get a new line, first checking the line queue & the file stack,
     * then looking in the assembly file itself.
     */

    *string = NULLC;
    if (input_get( string ))
        return( string );
    if( !get_asmline( string, max, FileInfo.file[ASM] ) )
        return( NULL );
    LineNumber++;
    return( string );
}

// add a string to the include path.
// called for -I cmdline options.
// the include path is rebuilt for each assembled module

void AddStringToIncludePath( char *string )
/*****************************************/
{
    char *tmp;

    while( isspace( *string ) )
        string++;
    if( IncludePath == NULL ) {
        IncludePath = SrcAlloc( strlen( string ) + 1 );
        strcpy( IncludePath, string );
    } else {
        tmp = IncludePath;
        IncludePath = SrcAlloc( strlen( tmp ) + strlen( INCLUDE_PATH_DELIM ) +
                                strlen( string ) + 1 );
        strcpy( IncludePath, tmp );
        strcat( IncludePath, INCLUDE_PATH_DELIM );
        strcat( IncludePath, string );
        SrcFree( tmp );
    }
}

// Initializer, called once for each module.
// It's called very early, cmdline options aren't set yet.
// So no debug displays possible in here!

void InputInit( void )
/**************************/
{
    //DebugMsg(( "InputInit()\n" ));
    FNames = NULL;
    IncludePath = NULL;
    file_stack = NULL;
    line_queue = NULL;
    queue_level = 0;
    MacroLevel = 0;
}

void InputFini( void )
/**************************/
{
    SrcFree( IncludePath );
#if FASTMEM==0
    FreeFlist();
#endif
}

// multi lines must be concatenated BEFORE the macro expansion step is done
// dont concat EQU, MACRO or ECHO lines!

bool IsMultiLine(void)
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
/******************/
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
// 1. a line is read with ReadTextLine()
// 2. the line is tokenized with Tokenize(), Token_Count set
// 3. (text) macros are expanded by ExpandMacro()
// 4. "preprocessor" directives are executed

// for 100% MASM compatibility, step 2 and 3 must be exchanged!


int GetPreprocessedLine( char *string )
/**************************/
{
    int i;

    if( ReadTextLine( string, MAX_LINE_LEN ) == NULL ) {
        DebugMsg(("GetPreprocessedLine() returns -1 (end of file)\n"));
        return -1; // EOF
    }

    // Token_Count is the number of tokens scanned
    Token_Count = Tokenize( string, 0);

    // if a comma is last token, concat lines ... with some exceptions

    if (Token_Count > 1 && AsmBuffer[Token_Count-1]->token == T_COMMA) {
        DebugMsg(("GetPreprocessedLine: calling IsMultiLine()\n"));
        if (IsMultiLine()) {
            char * ptr = string;
            DebugMsg(("IsMultiLine()=TRUE\n"));
            do {
                ptr = ptr + strlen( ptr );
                if ( ReadTextLine( ptr, MAX_LINE_LEN) == NULL )
                    return -1;
                if ( (Token_Count = Tokenize( ptr, Token_Count )) == 0 )
                    return -1;
            } while ( AsmBuffer[Token_Count-1]->token == T_COMMA );
        }
    }

    DebugMsg(("GetPreprocessedLine: >%s<\n", string));

    /* no expansion if current macro is skipped (EXITM does this) */
    /* then all what has to run are the conditional directives */
    /* (this could possibly be improved) */

    if (SkipMacroMode == FALSE)

     /* expand (text) macros. If expansion occured, rescan the line */

    while (Token_Count > 0 && ExpandMacro( string ) == STRING_EXPANDED) {
        DebugMsg(("GetPreprocessedLine(%u): expanded line is >%s<\n", LineNumber, string));
        Token_Count = Tokenize( string, 0 );
    }

    if (Token_Count == 0)
        return(0);

    i = 0;
    if (Token_Count > 2 && AsmBuffer[1]->token == T_COLON)
        i = 2;

    // handle "preprocessor" directives:
    // IF, ELSE, ENDIF, ...
    // .ERR, ...
    // FOR, REPEAT, WHILE, ...

    if ( AsmBuffer[i]->token == T_DIRECTIVE &&
         ( AsmBuffer[i]->opcode & ( DF_CONDDIR + DF_ERRDIR + DF_LOOPDIR ))) {

        if ( i > 1 && SkipMacroMode == FALSE ) {
            if (AsmBuffer[i-2]->token != T_ID) {
                AsmError( SYNTAX_ERROR );
                return(0);
            }
            if ( LabelCreate( AsmBuffer[i-2]->string_ptr, MT_NEAR, NULL, TRUE ) == ERROR )
                return(0);
        }

        if ( AsmBuffer[i]->value == T_INCLUDE )
            IncludeDirective( i + 1 );
        else if ( AsmBuffer[i]->opcode & DF_CONDDIR ) { /* conditional directive? */
            conditional_assembly_directive( i, AsmBuffer[i]->value );
        } else if ( AsmBuffer[i]->opcode & DF_ERRDIR ) { /* error directive? */
            if (SkipMacroMode == FALSE)
                conditional_error_directive( i );
        } else if ( AsmBuffer[i]->opcode & DF_LOOPDIR ) { /* loop directive? */
            if (SkipMacroMode == FALSE)
                LoopDirective ( i+1, AsmBuffer[i]->value );
        }
        return( 0 );
    }

    if (Token_Count > 1 &&  AsmBuffer[1]->token == T_DIRECTIVE && SkipMacroMode == FALSE) {
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
             expanded in ExpandMacro(), because the unexpanded content of
             the line must be assigned to the equate id.
             */
            if (!(AsmBuffer[1]->opcode & 1)) {
                if( AsmBuffer[0]->token == T_ID ) {
                    asm_sym *sym;
                    if ( sym = CreateConstant( AsmBuffer[0]->string_ptr, 0, 2, FALSE ) ) {
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
            DefineConstant( AsmBuffer[1]->opcode & 1 );
            return(0);
        case T_SIZESTR:
            SizeStrDef( 1 );
            return(0);
        case T_INSTR:
            InStrDef( 1, string );
            return(0);
#endif
        case T_TEXTEQU:
            CatStrDef( 2, NULL );
            return(0);
        case T_CATSTR:
            CatStrDef( 2, NULL );
            return(0);
        case T_SUBSTR:
            SubStrDef( 1, string  );
            return(0);
        }
    }
    /* is a user-defined prologue macro set? */
    if ( DefineProc &&
         ModuleInfo.proc_prologue &&
         *ModuleInfo.proc_prologue &&
         SkipMacroMode == FALSE )
        proc_check();

    return (Token_Count);
}
