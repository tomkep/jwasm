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

#if defined( _STANDALONE_ )

#include "directiv.h"
#include "memalloc.h"
#include "condasm.h"
#include "equate.h"
#include "macro.h"
#include "labels.h"
#include "input.h"
#include "proc.h"
#include "fastpass.h"

extern bool in_prologue;
extern int MacroLevel;
extern bool expansion;
extern bool SkipMacroMode;
extern bool DefineProc;

#define DETECTEOC 1

#if DETECTEOC
extern bool inside_comment;
#endif

#define CURRSRC 1

#if 1
#define SrcAlloc(x) MemAlloc(x)
#define SrcFree(x)  MemFree(x)
#else
#define SrcAlloc(x) AsmAlloc(x)
#define SrcFree(x)  AsmFree(x)
#endif

typedef struct line_list {
    struct line_list    *next;
    char                *line;
} line_list;

/* NOTE: the line queue is now a STACK of line queues
 *       when a macro is being expanded, we push a new line queue on the stack
 *       thus there is 1 queue on the stack for every level of nesting in macros
 */
typedef struct input_queue {
    struct line_list    *head;
    struct line_list    *tail;
    struct input_queue  *next;
} input_queue;

typedef struct file_list {
    struct file_list    *next;
    union {
        FILE            *file;
        struct input_queue  *lines;
    };
    const FNAME         *srcfile;   /* name of include file */
    unsigned long       line_num;   /* current line in parent file */
    char                is_a_file;
    bool                hidden;
} file_list;

//extern void             heap( char * );

FNAME                   *FNames;

static input_queue      *line_queue;    // line queue
static file_list        *file_stack;    // top of included file stack
static char             *IncludePath;

#if defined(__UNIX__)
#define                 INCLUDE_PATH_DELIM  ":"
#define                 DIR_SEPARATOR       '/'
#define                 DIR_SEP_STRING      "/"
#else
#define                 INCLUDE_PATH_DELIM  ";"
#define                 DIR_SEPARATOR       '\\'
#define                 DIR_SEP_STRING      "\\"
#endif

#endif

int                     Token_Count;        // number of tokens in curr line

// add a file to the file list

const FNAME *AddFlist( char const *name )
/***************************************/
{
    FNAME   *flist;
    FNAME   *last;
    int     index;
    char    *fname;
    char    buff[2*_MAX_PATH];

    index = 0;
    fname = _getFilenameFullPath( buff, name, sizeof( buff ) );
    last = FNames;
    for( flist = last; flist != NULL; flist = flist->next ) {
        if( strcmp( name, flist->name ) == 0 )
            return( flist );
        if( strcmp( fname, flist->fullname ) == 0 )
            return( flist );
        index++;
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

void FreeFlist( void )
/***************************/
{
    const FNAME   *curr;
    const FNAME   *last;

    for( curr = FNames; curr != NULL; ) {
        AsmFree( curr->fullname );
        AsmFree( curr->name );
        last = curr;
        curr = curr->next;
        AsmFree( (void *)last );
    }
    FNames = NULL;
    return;
}

#if defined( _STANDALONE_ )

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
            if (c != '\r' && max > 1) {
                *ptr++ = c;
                max--;
            }
            c = getc( fp );
        }
        *ptr = NULLC;
        return( TRUE );
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
                if (p == ptr) {
                    max += ptr - lastbs;
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

void PushLineQueue( void )
/************************/
{
    input_queue *new;

    DebugMsg(( "PushLineQueue\n" ));

    new = SrcAlloc( sizeof( input_queue ) );
    new->next = line_queue;
    new->head = new->tail = NULL;
    line_queue = new;
}

bool PopLineQueue( void )
/***********************/
/* remove a line queue from the top of the stack & throw it away */
{
    input_queue *tmp;

    /* pop the line_queue stack */
    tmp = line_queue;
    in_prologue = FALSE;
    if( tmp == NULL )
        return( FALSE );
    line_queue = line_queue->next;
    SrcFree( tmp );
    return( TRUE );
}

bool GetQueueMacroHidden( void )
/******************************/
{
    if(( file_stack != NULL ) && !file_stack->is_a_file ) {
        return( file_stack->hidden );
    } else {
        return( FALSE );
    }
}

static line_list *enqueue( void )
/*******************************/
{
    line_list   *new;

    new = SrcAlloc( sizeof( line_list ) );
    new->next = NULL;

    if( line_queue == NULL ) {
        line_queue = SrcAlloc( sizeof( input_queue ) );
        line_queue->next = NULL;
        line_queue->tail = NULL;
        line_queue->head = NULL;
    }

    if( line_queue->tail == NULL ) {
        line_queue->head = new;
        line_queue->tail = new;
    } else {
        /* insert at the tail */
        line_queue->tail->next = new;
        line_queue->tail = new;
    }
    return( new );
}

static file_list *push_flist( const char *name, asm_sym *sym )
/**************************************************************/
{
    file_list   *new;

    new = SrcAlloc( sizeof( file_list ) );
    new->next = file_stack;
    file_stack = new;
    new->line_num = LineNumber;
    if (sym == NULL)
        new->is_a_file = TRUE;
    else
        new->is_a_file = FALSE;
    new->hidden = 0;
    if( sym ) {
        dir_node *dir = (dir_node *)sym;
        LineNumber = dir->line_num;
        new->srcfile = dir->e.macroinfo->srcfile;
    } else {
        new->srcfile = AddFlist( name );
        LineNumber = 0;
    }
    return( new );
}

const FNAME *get_curr_srcfile( void )
/************************************/
{
    return( file_stack == NULL ? ModuleInfo.srcfile : file_stack->srcfile );
}

void print_include_file_nesting_structure( void )
/***********************************************/
{
    file_list       *tmp;
    unsigned        tab = 1;

    tmp = file_stack;
    if( tmp == NULL )
        return;

    for( ; tmp != NULL ; ) {
        if( tmp->next == NULL )
            break;
        if( !tmp->hidden ) {
            if( tmp->is_a_file ) {
                AsmNote( NOTE_INCLUDED_BY, tmp->next->srcfile->name, tmp->line_num );
            } else if (tmp->next->srcfile) {
                AsmNote( NOTE_MACRO_CALLED_FROM, tmp->next->srcfile->name, tmp->line_num );
            }
        }
        tmp = tmp->next;
        tab++;
    }

    if( !tmp->hidden ) {
        if( tmp->is_a_file ) {
            AsmNote( NOTE_INCLUDED_BY, ModuleInfo.srcfile->name, tmp->line_num );
        } else {
            AsmNote( NOTE_MACRO_CALLED_FROM, ModuleInfo.srcfile->name, tmp->line_num );
        }
    }
}

void InputQueueLine( char *line )
/*******************************/
{
    line_list   *new;
    int i;

    DebugMsg(( "InputQueueLine: %s  ( line %lu ) \n", line, LineNumber ));
    new = enqueue();
    i = strlen(line) + 1;
    if (new->line = SrcAlloc(i))
        memcpy( new->line, line, i );
}

// scan INCLUDE variable
// also contains directories set with -I cmdline option

static FILE *open_file_in_include_path( char *name, char *fullpath )
/******************************************************************/
{
    char            *inc_path_list;
    char            *next_path;
    char            buffer[MAX_LINE_LEN];
    FILE            *file = NULL;

    while( isspace( *name ) )
        name++;

    inc_path_list = AsmTmpAlloc( strlen( IncludePath ) + 1 );
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

int InputQueueFile( char *path )
/******************************/
{
    FILE        *file;
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


    _splitpath( path, drive, dir, fname, ext );
    /* if there is no absolute path given, first search in the directory
     of the current source file */
    if (dir[0] != '\\' && dir[0] != '/' && file_stack && file_stack->is_a_file == 1) {
        _splitpath( file_stack->srcfile->fullname, drive2, dir2, NULL, NULL );
        if (dir2[0] == '\\' || dir2[0] == '/') {
            _makepath( fullpath, drive2, dir2, fname, ext );
//            file = fopen( fullpath, "r" );
            file = fopen( fullpath, "rb" );
            tmp = fullpath;
            if (file)
                goto ffound;
        }
    }
    _makepath( fullpath, drive, dir, fname, ext );
//    file = fopen( fullpath, "r" );
    file = fopen( fullpath, "rb" );
    tmp = path;
    if( file == NULL &&
        IncludePath != NULL &&
        /* don't search include path if an absolute path is given */
        dir[0] != '\\' && dir[0] != '/') {
        tmp = buffer;
        file = open_file_in_include_path( path, tmp );
    }

    if( file == NULL ) {
        AsmErr( CANNOT_OPEN_INCLUDE_FILE, fullpath );
        return( ERROR );
    } else {
    ffound:
        new = push_flist( tmp, NULL );
        new->file = file;
        return( NOT_ERROR );
    }
}

static char *input_get( char *string )
/************************************/
{
    line_list   *inputline;
    file_list   *inputfile;

    /* Check the line_queue first; then the file_stack */

    while( line_queue != NULL ) {
        if( line_queue->head != NULL ) {
            inputline = line_queue->head;
            strcpy( string, inputline->line );
            line_queue->head = inputline->next;
            if( line_queue->head == NULL )
                line_queue->tail = NULL;
            SrcFree( inputline->line );
            SrcFree( inputline );
            return( string );
        }
        PopLineQueue();
    }

    while( file_stack != NULL ) {
        inputfile = file_stack;
        if( inputfile->is_a_file ) {
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
            LineNumber++;
            if( inputline != NULL ) {
                strcpy( string, inputline->line );
                inputfile->lines->head = inputline->next;
                SrcFree( inputline->line );
                SrcFree( inputline );
                return( string );
            }

            file_stack = inputfile->next;
            SrcFree( inputfile->lines );
            LineNumber = inputfile->line_num;
            SrcFree( inputfile );
        }
    }
    return( NULL );
}

char *ReadTextLine( char *string, int max )
/********************************/
{
    /* get a new line, first checking the line queue & the file stack,
     * then looking in the assembly file itself.
     */

    string[0] = '\0';
    if (input_get( string ))
        return( string );
    if( !get_asmline( string, max, AsmFiles.file[ASM] ) )
        return( NULL );
    LineNumber++;
    return( string );
}

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

void InputInit( void )
/**************************/
{
    FNames = NULL;
    IncludePath = NULL;
    file_stack = NULL;
    line_queue = NULL;
}

void InputFini( void )
/**************************/
{
    SrcFree( IncludePath );
    FreeFlist();
}

// push macro name onto the file stack so it
// can be displayed in case of errors.

void PushMacro( asm_sym * sym, bool hidden )
/*********************************************/
{
    file_list *new;

    DebugMsg(( "PUSH_MACRO\n" ));
    new = push_flist( sym->name, sym );
    new->lines = line_queue;
    new->hidden = hidden;
    line_queue = line_queue->next;
}

void preprocessor_output( char *string )
/****************************/
/* print out a simplied version of the source line
   after it is parsed and text is expanded */
{
    int             i;
    static bool PrintEmptyLine = TRUE;

    /* flag generated code */
    for (i = MacroLevel;i;i--)
        printf("*");

    if (Token_Count > 0) {
        PrintEmptyLine = TRUE;
        printf("%s\n", string);
    } else if (PrintEmptyLine) {
        PrintEmptyLine = FALSE;
        printf("\n");
    }
}

#endif

// multi lines must be concatenated BEFORE the macro expansion step is done

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
    } else if (AsmBuffer[i]->token == T_DIRECTIVE) {
        if (AsmBuffer[i]->value == T_ECHO)
            return(FALSE);
    }
    return(TRUE);
}

// AsmLine:
// this is the "preprocessor"
// 1. a line is read with ReadTextLine()
// 2. the line is tokenized with Tokenize(), Token_Count set
// 3. (text) macros are expanded by ExpandMacro()
// 4. "preprocessor" directives are executed

// for 100% MASM compatibility, step 2 and 3 must be exchanged!


int AsmLine( char *string )
/**************************/
{
#if defined( _STANDALONE_ )
    int i;

    if( ReadTextLine( string, MAX_LINE_LEN ) == NULL )
        return -1; // EOF

    // Token_Count is the number of tokens scanned
    Token_Count = Tokenize( string, 0);

    // if a comma is last token, concat lines ... with some exceptions

    if (Token_Count > 1 && AsmBuffer[Token_Count-1]->token == T_COMMA) {
        DebugMsg(("calling IsMultiLine()\n"));
        if (IsMultiLine()) {
            char * ptr = string;
            DebugMsg(("IsMultiLine()=TRUE\n"));
            while (AsmBuffer[Token_Count-1]->token == T_COMMA) {
                ptr = ptr + strlen(ptr);
                if (ReadTextLine(ptr, MAX_LINE_LEN) == NULL)
                    return -1;
                Token_Count = Tokenize( ptr, Token_Count );
                if (Token_Count == ERROR) {
                    return -1;
                }
            }
        }
    }

    DebugMsg(("AsmLine: >%s<\n", string));

    /* no expansion if current macro is skipped (EXITM does this) */
    /* then all what has to run are the conditional directives */
    /* (this could possibly be improved) */

    if (SkipMacroMode == FALSE)

     /* expand (text) macros. If expansion occured, rescan the line */

    while (Token_Count > 0 && ExpandMacro( string ) == STRING_EXPANDED) {
        DebugMsg(("AsmLine: expanded line is >%s<\n", string));
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

    if (AsmBuffer[i]->token == T_DIRECTIVE &&
        (AsmBuffer[i]->opcode & (OPCF_CONDDIR + OPCF_ERRDIR + OPCF_LOOPDIR))) {

        if (i > 1 && SkipMacroMode == FALSE) {
            if (AsmBuffer[i-2]->token != T_ID) {
                AsmError(SYNTAX_ERROR);
                return(0);
            }
            if (LabelCreate( AsmBuffer[i-2]->string_ptr, MT_NEAR, NULL, TRUE ) == ERROR)
                return(0);
        }

        if ( AsmBuffer[i]->opcode & OPCF_CONDDIR ) { /* conditional directive? */
            conditional_assembly_directive( i, AsmBuffer[i]->value );
        } else if ( AsmBuffer[i]->opcode & OPCF_ERRDIR ) { /* error directive? */
            if (SkipMacroMode == FALSE)
                conditional_error_directive( i );
        } else if ( AsmBuffer[i]->opcode & OPCF_LOOPDIR ) { /* loop directive? */
            if (SkipMacroMode == FALSE)
                LoopDirective ( i+1, AsmBuffer[i]->value );
        }
        return(0);
    }

    if (Options.preprocessor_stdout == TRUE && SkipMacroMode == FALSE)
        preprocessor_output(string);

    if (Token_Count > 1 &&  AsmBuffer[1]->token == T_DIRECTIVE && SkipMacroMode == FALSE) {
        switch (AsmBuffer[1]->value) {
        case T_MACRO:
            MacroDef ( 0 );
            return(0);
#if FASTPASS
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
#if 0 //def DEBUG_OUT
    else
        DebugMsg(("No scan for preprocessor cmds, reason: count=%u, skipmode=%u\n", Token_Count, SkipMacroMode));
#endif

    /* this has been moved from ParseItems() because inside proc_check
     there's an optional call of RunMacro(), which might be a problem if
     FASTPASS is on */
    if( DefineProc == TRUE && SkipMacroMode == FALSE) {
        if (proc_check() == EMPTY) {
            DebugMsg(("AsmLine: proc_check() skips line processing\n"));
            return( 0 );
        }
    }

    return (Token_Count);

#else
    // Token_Count is the number of tokens scanned
    Token_Count = Tokenize( string );
    if( Token_Count > 0 ) {
        ParseItems();
    }
#endif
}
