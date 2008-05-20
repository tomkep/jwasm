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


#include "asmglob.h"
#include <ctype.h>

#include "parser.h"
#include "asmdefs.h"

#if defined( _STANDALONE_ )

#include "directiv.h"
#include "memalloc.h"
#include "condasm.h"
#include "equate.h"
#include "macro.h"
#include "asminput.h"
#include "pathgrp.h"

extern int in_prologue;
extern int MacroLevel;
extern bool expansion;
extern bool SkipMacroMode;

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

extern void             heap( char * );

extern char             write_to_file;
extern uint_8           CheckSeg;
extern unsigned long    PassTotal;

uint_32                 BufSize;                // size of CodeBuffer

static input_queue      *line_queue  = NULL;    // line queue
static file_list        *file_stack  = NULL;    // top of included file stack
static char             *IncludePath = NULL;

#if defined(__UNIX__)
#define                 INCLUDE_PATH_DELIM  ":"
#define                 DIR_SEPARATOR       '/'
#define                 DIR_SEP_STRING      "/"
#else
#define                 INCLUDE_PATH_DELIM  ";"
#define                 DIR_SEPARATOR       '\\'
#define                 DIR_SEP_STRING      "\\"
#endif

#else

uint_32                 AsmCodeAddress;     // program counter

#endif

int                     Token_Count;        // number of tokens on line
unsigned char           *AsmCodeBuffer;     // code buffer for generated bytes

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
    char        *tmp;
    PGROUP      pg;

    _splitpath2( path, pg.buffer, &pg.drive, &pg.dir, &pg.fname, &pg.ext );
    /* if there is no absolute path given, first search in the directory
     of the current source file */
    if (*(pg.dir) != '\\' && *(pg.dir) != '/' && file_stack && file_stack->is_a_file == 1) {
        PGROUP pg2;
        _splitpath2( file_stack->srcfile->fullname, pg2.buffer, &pg2.drive, &pg2.dir, NULL, NULL );
        if (*(pg2.dir) == '\\' || *(pg2.dir) == '/') {
            _makepath( fullpath, pg2.drive, pg2.dir, pg.fname, pg.ext );
//            file = fopen( fullpath, "r" );
            file = fopen( fullpath, "rb" );
            tmp = fullpath;
            if (file)
                goto ffound;
        }
    }
    _makepath( fullpath, pg.drive, pg.dir, pg.fname, pg.ext );
//    file = fopen( fullpath, "r" );
    file = fopen( fullpath, "rb" );
    tmp = path;
    if( file == NULL &&
        IncludePath != NULL &&
        /* don't search include path if an absolute path is given */
        *(pg.dir) != '\\' && *(pg.dir) != '/') {
        tmp = pg.buffer;
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

void AsmCodeByte( unsigned char byte )
/************************************/
{
    if( CurrSeg != NULL ) {
        if( CurrSeg->seg->e.seginfo->iscode == SEGTYPE_UNDEF ) {
            CurrSeg->seg->e.seginfo->iscode = SEGTYPE_ISCODE;
        }
    }
    OutSelect( FALSE );
    AsmByte( byte );
}

void AsmDataByte( unsigned char byte )
/************************************/
{
    OutSelect( TRUE );
    AsmByte( byte );
}

static bool CheckHaveSeg( void )
/******************************/
{
    if( CurrSeg != NULL )
        return( TRUE );

    if( CheckSeg ) {
        AsmError( DATA_EMITTED_WITH_NO_SEGMENT );
        write_to_file = FALSE;
        CheckSeg = FALSE;
    }
    return( FALSE );
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

void FreeIncludePath( void )
/**************************/
{
    SrcFree( IncludePath );
}

// ???

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

static void preprocessor_output( void )
/****************************/
/* print out a simplied version of the source line
   after it is parsed and text is expanded */
{
    int             i;
    static bool PrintEmptyLine = TRUE;

    for (i = MacroLevel;i;i--)
        printf("*");

    for( i = 0; i < Token_Count; i++ ) {
        if (AsmBuffer[i]->token == T_STRING)
            if (*AsmBuffer[i]->string_ptr == '"' ||
                *AsmBuffer[i]->string_ptr == '\'' ||
                *AsmBuffer[i]->string_ptr == '{')
                printf("%s ", AsmBuffer[i]->string_ptr);
            else
                printf("<%s> ", AsmBuffer[i]->string_ptr);
        else
            printf("%s ", AsmBuffer[i]->string_ptr);
    }
    if (Token_Count > 0) {
        PrintEmptyLine = TRUE;
        printf("\n");
    } else if (PrintEmptyLine) {
        PrintEmptyLine = FALSE;
        printf("\n");
    }
}

#endif

void AsmByte( unsigned char byte )
/********************************/
/* Write a byte to the object file */
{
#if defined( _STANDALONE_ )
    if( CheckHaveSeg() ) {
        (CurrSeg->seg->e.seginfo->current_loc)++;
        if( CurrSeg->seg->e.seginfo->current_loc >=
            CurrSeg->seg->e.seginfo->segrec->d.segdef.seg_length ) {
            CurrSeg->seg->e.seginfo->segrec->d.segdef.seg_length = CurrSeg->seg->e.seginfo->current_loc;
        }
        if( Parse_Pass != PASS_1 && write_to_file ) {
            AsmCodeBuffer[BufSize] = byte;
            ++BufSize;
            if( BufSize >= MAX_LEDATA_THRESHOLD ) {
                FlushCurrSeg();
            }
        }
    }
    PassTotal++;
#else
    AsmCodeBuffer[AsmCodeAddress] = byte;
    ++AsmCodeAddress;
#endif
}

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
    if (AsmBuffer[i]->token == T_DIRECTIVE) {
        switch(AsmBuffer[i]->value) {
        case T_ELSE:
        case T_ELSEIF:
        case T_ELSEIFB:
        case T_ELSEIFDEF:
        case T_ELSEIFDIF:
        case T_ELSEIFDIFI:
        case T_ELSEIFE:
        case T_ELSEIFNB:
        case T_ELSEIFNDEF:
        case T_ELSEIFIDN:
        case T_ELSEIFIDNI:
        case T_ENDIF:
        case T_IF:
        case T_IF1:
        case T_IF2:
        case T_IFB:
        case T_IFDEF:
        case T_IFDIF:
        case T_IFDIFI:
        case T_IFE:
        case T_IFNB:
        case T_IFNDEF:
        case T_IFIDN:
        case T_IFIDNI:
            conditional_assembly_directive( i, AsmBuffer[i]->value );
            return (0);
#if 0
            /* these command are NOT handled by the preprocessor */
            /* if no macro is running, they will cause a "nesting" error, */
            /* else they are caught inside RunMacro() */
        case T_ENDM:
        case T_EXITM:
            if (SkipMacroMode == FALSE)
                EndMacro(i, AsmBuffer[i]->value);
            return (0);
#endif
        case T_FOR:
        case T_IRP:
        case T_FORC:
        case T_IRPC:
        case T_REPT:
        case T_REPEAT:
        case T_WHILE:
            if (SkipMacroMode == FALSE)
                ForDirective ( i+1, AsmBuffer[i]->value );
            return(0);
        }
    }

    if (Options.preprocessor_stdout == TRUE)
        preprocessor_output();

    if (Token_Count > 1 &&  AsmBuffer[1]->token == T_DIRECTIVE && SkipMacroMode == FALSE) {
        switch (AsmBuffer[1]->value) {
        case T_MACRO:
            MacroDef ( 0 );
            return(0);
        case T_EQU:
            DefineConstant( 0, FALSE );
            return(0);
        case T_EQU2:
            DefineConstant( 0, TRUE );
            return(0);
        case T_TEXTEQU:
            CatStrDef( 2, NULL );
            return(0);
        case T_CATSTR:
            CatStrDef( 2, NULL );
            return(0);
        case T_SUBSTR:
            SubStrDef( 1, string  );
            return(0);
        case T_SIZESTR:
            SizeStrDef( 1 );
            return(0);
        case T_INSTR:
            InStrDef( 1, string );
            return(0);
        }
    }
#if 0 //def DEBUG_OUT
    else
        DebugMsg(("No scan for preprocessor cmds, reason: count=%u, skipmode=%u\n", Token_Count, SkipMacroMode));
#endif

    return (Token_Count);

#else
    // Token_Count is the number of tokens scanned
    Token_Count = Tokenize( string );
    if( Token_Count > 0 ) {
        ParseItems();
    }
#endif
}
