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
* Description:  macro processing routines.
*               this code has been virtually rewritten for JWasm.
*
* functions:
* - ExpandText   handles expansion within strings
* - ExpandMacro  called by asmline to expand (text) macros
* - MacroDef     handle MACRO directive
* - EndMacro     handle ENDM/EXITM directives
* - SubStrDef    handle SUBSTR directive
* - SizeStrDef   handle SIZESTR directive
* - InStrDef     handle INSTR directive
* - PurgeMacro   handle PURGE directive
* - MacroInit    global macro initialization, set predefined macros
*
****************************************************************************/


#define     PLACEHOLDER_SIZE 3      /* for #dd - number sign, digit, digit */

#include "globals.h"
#include <ctype.h>

#include "memalloc.h"
#include "parser.h"
#include "expreval.h"
#include "equate.h"
#include "directiv.h"
#include "input.h"
#include "macro.h"
#include "condasm.h"
#include "fastpass.h"

#include "myassert.h"

#define  MAX_TEXTMACRO_NESTING  16

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

extern bool             GetQueueMacroHidden( void );

extern bool expansion;
extern int CurrIfState;
extern char EndDirectiveFound;

int MacroLocalVarCounter; // counter for temp. var names
#if FASTPASS
int saved_MacroLocalVarCounter;
#endif
int MacroLevel;
bool SkipMacroMode;

// make room (or delete items) in the token buffer

void AddTokens( int start, int count )
/************************************************************/
{
    int i;

    if ( count > 0) {
        for( i = Token_Count; i >= start; i-- ) {
            *AsmBuffer[i+count] = *AsmBuffer[i];
        }
    } else if (count < 0) {
        for( i = start - count; i <= Token_Count; ++i ) {
            *AsmBuffer[i+count] = *AsmBuffer[i];
        }
    }
    Token_Count += count;
}

static asmlines *asmline_insert( asmlines **head, void *data )
/************************************************************/
{
    asmlines *entry;
    asmlines **ptr;

    /* get a pointer to the last next ptr ( or Head if empty ) */
    for( ptr = head; *ptr; ptr = &((*ptr)->next) );

    entry = AsmAlloc( sizeof( asmlines ) );
    entry->next = NULL;
    entry->line = AsmAlloc( strlen( data ) + 1 );
    entry->parmcount = 0;
    strcpy( entry->line, data );
    *ptr = entry;
    return( entry );
}

static char *replace_parm( char *start, char len, asmlines *lstruct, mparm_list *parms , mlocal_list *locals )
/*************************************************************************************/
{
    /* search through parm list for word pointed at by start,
     * if you find it, set up the line string
     * this is similar to a printf format string
     * the placeholders are of the form #dd ( #, digit, digit )
     * this allows up to 100 parameters - lots :)
     * fixme - this max. should be docmented & checked for.
     */
    char            buffer[10];
    char            *label;
    char            *new_line;
    char            *old_line;
    char            before;             // length of text before placeholder
    char            count = 0;

//  DebugMsg(("replace_parm enter for %s, line=%s\n", start, lstruct->line));

    old_line = lstruct->line;
    for( ; parms || locals; ) {
        if (parms) {
            label = parms->label;
            parms = parms->next;
        } else {
            label = locals->label;
            locals = locals->next;
        }
        if( ( label != NULL ) && ( strlen( label ) == len ) &&
            ( strncmp( start, label, len ) == 0 ) ) {
            /* hey! it matches! */

            new_line = AsmAlloc( strlen(old_line) - len + PLACEHOLDER_SIZE +1 );
            before = start - old_line;
            if( *(start-1) == '&' ) before--;
            strncpy( new_line, old_line, before );
            *(new_line+before) = '\0';
            strcat( new_line, "#" );
            if( sprintf(buffer,"%2d", count ) != 2 ) {
                myassert( 0 );
            }
            if( buffer[0] == ' ' ) buffer[0]='0'; /* no spaces */
            strcat( new_line, buffer );
            if( *(start+len) == '&' ) len++;
            strcat( new_line, start+len );
            lstruct->line = new_line;
            lstruct->parmcount++;

            AsmFree( old_line );

            return( new_line + before + PLACEHOLDER_SIZE );  /* ptr to char after #dd */
        }
        count++;
    }
    return( start+len );
}

static void put_parm_placeholders_in_line( asmlines *linestruct, mparm_list *parms, mlocal_list *locals )
/*********************************************************************************/
{
    char *line;
    char *tmp;
    char *start;
    char quote = FALSE;
    char len;

    /* handle the substitution operator ( & ) */
    line = linestruct->line;
    for( tmp = line; *tmp != '\0'; ) {
        /* scan across the string for space, &, " - to start a word */
        line = tmp;
        for( ; *tmp != '\0'; tmp++ ) {
            if( is_valid_id_char( *tmp ) ) {
                if( tmp == line ) break; /* ok to start at beginning of line */
                continue;
            } else if( isspace( *tmp ) ) {
                /* find 1st non blank char */
                while( isspace( *tmp ) ) tmp++;
                break;
            } else if( *tmp == '"' ) {
                /* toggle the quote flag */
                quote = ( quote + 1 ) %2;
                tmp++;
                break;
            } else {
                /* some other garbage */
                tmp++;
                break;
            }
        }
        start = tmp;
        /* scan across the string for space, &, " - to end the word */
        for( ; *tmp != '\0'; tmp++ ) {
            if( is_valid_id_char( *tmp ) ) {
                continue;
            } else if( isspace( *tmp ) ) {
                break;
            } else if( *tmp == '"' ) {
                /* toggle the quote flag */
                quote = ( quote + 1 ) %2;
                break;
            } else {
                break;
            }
        }
        len = tmp - start;
        /* look for this word in the macro parms, and replace it if it is */
        /* this would change line - it will have to be reallocated */
        if( !quote || *start =='&' || *(start-1)=='&' || *(start+len+1)=='&' ) {
            if( *start != '\0' && len > 0 ) {
                tmp = replace_parm( start, len, linestruct, parms, locals);
            }
        }
    }
}

static bool lineis( char *string, char *substr, int len )
/********************************************/
{
    if( string[len] != '\0' && !isspace( string[len] ) ) {
        return( FALSE );
    }
    if( strnicmp( string, substr, len ) ) {
        return( FALSE );
    }
    return( TRUE );
}

// fill a macros param- and content list.
// macros can be redefined, so free any current values in there
// i = start index of macro params

int FillMacro( dir_node * macro, int i, bool store_data )
/****************************/
{
    macro_info          *info;
    char                *string;
    char                *token;
    mparm_list          *paranode;
    mparm_list          *paracurr;
    mlocal_list         *localnode;
    asmlines            *linestruct;
    char                buffer[ MAX_LINE_LEN ];
    uint                nesting_depth = 0;
    bool                locals_done;

    DebugMsg(("FillMacro(%s) enter\n", macro->sym.name));
    info = macro->e.macroinfo;

    /* go past "MACRO" */

    if( store_data ) {
        for( ; i < Token_Count ; ) {
            token = AsmBuffer[i]->string_ptr;
            paranode = AsmAlloc( sizeof( mparm_list ) );
            paranode->def = NULL;
            paranode->replace = NULL;
            paranode->required = FALSE;

            /* first get the parm. name */
            paranode->label = AsmAlloc( strlen( token ) + 1 );
            strcpy( paranode->label, token );
            i++;

            /* now see if it has a default value or is required */
            if( AsmBuffer[i]->token == T_COLON ) {
                i++;
                if( *AsmBuffer[i]->string_ptr == '=' ) {
                    i++;
                    if( AsmBuffer[i]->token != T_STRING ) {
                        AsmError( LITERAL_EXPECTED );
                        break; // return( ERROR );
                    }
                    token = AsmBuffer[i]->string_ptr;
                    paranode->def = AsmAlloc( strlen( token ) + 1 );
                    strcpy( paranode->def, token );
                    i++;
                } else if( stricmp( AsmBuffer[i]->string_ptr, "REQ" ) == 0 ) {
                    /* required parameter */
                    paranode->required = TRUE;
                    i++;
                } else if(( AsmBuffer[i]->token == T_RES_ID ) && (AsmBuffer[i]->value == T_VARARG)) {
                    /* more parameters can follow */
                    macro->sym.vararg = TRUE;
                    if (AsmBuffer[i+1]->token != T_FINAL) {
                        AsmError( VARARG_PARAMETER_MUST_BE_LAST);
                        break;
                    }
                    i++;
                }
            }
            DebugMsg(("FillMacro(%s): param=>%s< found\n", macro->sym.name, paranode->label));
            if( i< Token_Count && AsmBuffer[i]->token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                break; // return( ERROR );
            }
            /* go past comma */
            i++;

            /* add this parm node to the list */

            paranode->next = NULL;
            if( info->parmlist == NULL ) {
                info->parmlist = paranode;
            } else {
                for( paracurr = info->parmlist;; paracurr = paracurr->next ) {
                    if( paracurr->next == NULL ) break;
                }
                paracurr->next = paranode;
            }

        } /* looping through parameters */
    }
    DebugMsg(("FillMacro(%s): macro parameters done\n", macro->sym.name));

    locals_done = FALSE;
    /* now read in all the contents of the macro, and store them */
    for( ; ; ) {
        char *ptr;
        char *ptr2;

        string = ReadTextLine( buffer, sizeof(buffer) );
        if( string == NULL ) {
            AsmError( UNEXPECTED_END_OF_FILE );
            return( ERROR );
        }
        wipe_space( string );

        if (*string == '\0')
            continue;

        /* macro label? */
        if (*string == ':' && is_valid_id_char(*(string+1))) {
            ptr = string+2;
            while (is_valid_id_char(*ptr)) ptr++;
            /* the label must be the only item in the line */
            if (*ptr != NULLC) {
                /* make sure the label is zero-terminated */
                *ptr++ = NULLC;
                while (isspace(*ptr)) ptr++;
                if (*ptr) {
                    AsmError( SYNTAX_ERROR );
                }
            }
        } else if( locals_done == FALSE && lineis( string, "local", 5 ) ) {
            if( !store_data )
                continue;
            ptr = string+5;
            while( isspace( *ptr ) ) ptr++;
            for (;*ptr;) {
                if (!(is_valid_id_char(*ptr))) {
                    AsmError( SYNTAX_ERROR );
                    break;
                }
                localnode = AsmAlloc( sizeof( mlocal_list ) );
                for (ptr2=ptr;*ptr2;ptr2++)
                    if ((isspace(*ptr2)) || (*ptr2 == ','))
                        break;
                localnode->label = AsmAlloc((ptr2 - ptr) + 1);
                memcpy(localnode->label, ptr, ptr2 - ptr);
                *(localnode->label + (ptr2 - ptr)) = '\0';
                /* add this local node to the list */
                localnode->next = NULL;
                if( info->locallist == NULL ) {
                    info->locallist = localnode;
                } else {
                    mlocal_list *localcurr;
                    for( localcurr = info->locallist;localcurr->next; localcurr = localcurr->next );
                    localcurr->next = localnode;
                }
                DebugMsg(("FillMacro(%s): local=>%s< added\n", macro->sym.name, localnode->label));
                ptr = ptr2;
                while( isspace( *ptr ) ) ptr++;
                if (*ptr == ',') ptr++;
                while( isspace( *ptr ) ) ptr++;
            }
            continue;
        } else if( lineis( string, "exitm", 5 ) ) {
            if (nesting_depth == 0) {
                ptr = string+5;
                while( isspace( *ptr ) ) ptr++;
                if (*ptr)
                    macro->sym.isfunc = TRUE;
                macro->sym.runsync = TRUE;
            }
        } else if( lineis( string, "goto", 4 ) ) {
            if (nesting_depth == 0)
                macro->sym.runsync = TRUE;
        } else if( lineis( string, "endm", 4 ) ) {
            if( nesting_depth ) {
                nesting_depth--;
            } else {
                macro->sym.defined = TRUE;
                DebugMsg(("FillMacro(%s): endm found, exit, no error, func=%u\n", macro->sym.name, macro->sym.isfunc));
                return( NOT_ERROR );
            }
        }
        ptr = string;
        if (*ptr == '%') {
            ptr++;
            while(isspace(*ptr)) ptr++;
        }

        locals_done = TRUE;

        // skip a possible label
        if (is_valid_id_char(*ptr )) {
            ptr2 = ptr+1;
            while( is_valid_id_char(*ptr2 )) ptr2++;
            if (*ptr2 == ':') {
                ptr2++;
                if (*ptr2 == ':')
                    ptr2++;
                while (isspace(*ptr2)) ptr2++;
                ptr = ptr2;
            } else {
                while (isspace(*ptr2)) ptr2++;
                /* a "local" macro? */
                if( lineis( ptr2, "macro", 5 )) {
                    nesting_depth++;
                    goto check_done;
                }
            }
        }

        if( lineis( ptr, "for", 3 )
            || lineis( ptr, "forc", 4 )
            || lineis( ptr, "while", 5 )
            || lineis( ptr, "irp", 3 )
            || lineis( ptr, "irpc", 4 )
            || lineis( ptr, "rept", 4 )
            || lineis( ptr, "repeat", 6 ) ) {
            nesting_depth++;
        }
    check_done:
        if( store_data ) {
            linestruct = asmline_insert( &(info->data), string );
            /* make info->data point at the LAST line in the struct */
            put_parm_placeholders_in_line( linestruct, info->parmlist, info->locallist );
        }
    }
}

static int my_sprintf( char *dest, char *format, int argc, char *argv[] )
/***********************************************************************/
{
    /* just like sprintf, except take argv & argc for parms */
    /* so far it only handles string parms */

    char buffer[3];
    char *start;
    char *end;
    int  parmno = 0;

    *dest = '\0';
    start = format;
    for( end = start ;*end != '\0'; start = end + PLACEHOLDER_SIZE ) {
        /* scan till we hit a placeholdr ( #dd ) or the end of the string */
        for( end = start;
             !( ( *end == '#' ) && isdigit( *(end+1) ) && isdigit( *(end+2) ) )
             && ( *end != '\0' ); end++ );

        if( *end == '\0' ) {
            strncat( dest, start, end-start );
            return( strlen( dest ) );
        }
        /* we have a placeholder ( #dd ) */
        buffer[0] = *(end+1);
        buffer[1] = *(end+2);
        buffer[2] = '\0';
        parmno = atoi( buffer );
        strncat( dest, start, (end-start) );
        if ( parmno > argc ) {
            DebugMsg(("my_sprintf: parmno=%u, argc=%u, format=%s\n", parmno, argc, format));
            AsmError(SYNTAX_ERROR);
            return(0);
        }
        if( argv[parmno] != NULL ) {
            strcat( dest, argv[parmno++] );
        }
    }
    return( 1 );
//    return( strlen( dest ) );
}

#if 0
static void free_parmlist( mparm_list *head )
/******************************************/
{
    mparm_list *parm;

    for( parm = head; parm != NULL; parm = parm->next ) {
//        AsmFree( parm->replace );
        parm->replace = NULL;
    }
    return;
}
#endif

// skip macro execution until ENDM

int SkipMacro(void)
{
    char buffer[MAX_LINE_LEN];
    int lvl = 1;
    int i;

    SkipMacroMode = TRUE;
    while (lvl > 0) {
        i = AsmLine(buffer);
        if (i < 0) {
            AsmError( UNEXPECTED_END_OF_FILE );
            return(ERROR);
        }
        if (Token_Count > 0 &&  AsmBuffer[0]->token == T_DIRECTIVE) {
            if (AsmBuffer[0]->opcode & OPCF_LOOPDIR)
                lvl++;
            else if (AsmBuffer[0]->value == T_ENDM)
                lvl--;
        } else if (Token_Count > 1 &&
                 AsmBuffer[1]->token == T_DIRECTIVE &&
                 AsmBuffer[1]->value == T_MACRO)
            lvl++;
    }
    SkipMacroMode = FALSE;
    return(NOT_ERROR);
}

// run a macro
// dir: macro item
// params: parameter string (includes macro name!)
// prefix: line prefix which should be emitted "first"
// runit: emit a "ENDM" as last line and run it
// insert: call PushLineQueue()
// returns number of characters processed or ERROR

int RunMacro(dir_node * macro, char * params, char * prefix, bool runit, bool insert, bool addbrackets)
{
    char        buffer[MAX_LINE_LEN];
    char        line[MAX_LINE_LEN];
    char        *orgsrc = params;
    macro_info  *info;
    mparm_list  *parm;
    mlocal_list *local;
    asmlines    *lnode;
    dir_node    *dir;
    expr_list   opndx;
    int         funcname;
    int         count;
    int         i;
    int         localcnt = 0;
    char        expansion_flag;
    char        string_expansion;
    int         exp_start;
    bool        end_reached;
    int         bracket_level = -1;/* () level */
    char        parm_end_delim;   /* parameter end delimiter */
    char        *ptr;
    char        **parm_array;
    char        *local_array;

    DebugMsg(("RunMacro(%s) enter, src=>%s<, run=%u, insert=%u [local var cnt=%04u]\n", macro->sym.name, params, runit, insert, MacroLocalVarCounter));

    info = macro->e.macroinfo;

    /* skip the macro name - if there is one at all */
    if (*(macro->sym.name)) {
        if (*params == '.') /* assume OPTION DOTNAME */
            params++;
        while (is_valid_id_char(*params)) params++;
    }

    parm_end_delim = '\0';

    /* invokation of macro functions requires params enclosed in "()" */

    if (macro->sym.isfunc) {
        while (isspace(*params)) params++;
        if (*params == '(') {
            params++;
            parm_end_delim = ')';
            bracket_level = 1;
        }
    }

    /* now get all the parameters from the original src line
     they should alternate: parm /  comma.
     the parameters must be "expanded", that is, it is to be checked
     if an ID is a (text) macro and if yes, it is to be invoked!
     */

    DebugMsg(( "RunMacro(%s):  %s \n", macro->sym.name, params ));

    end_reached = FALSE;

    /* skip white spaces (in case there are no params at all) */
    while (isspace(*params)) params++;

    for( parm = info->parmlist; parm != NULL; parm = parm->next ) {

        buffer[0]='\0';
        expansion_flag = FALSE;

        while (isspace(*params)) params++;

        if (macro->sym.isfunc && *params == ')')
            end_reached = TRUE;

        if ((*params == '\0') || ( end_reached == TRUE) || ( *params == ',') ) {

            /* it's a blank parm */

            if( parm->required ) {
                DebugMsg(( "RunMacro(%s), parm %s required: >%s<\n", macro->sym.name, parm->label, params ));
                AsmError( PARM_REQUIRED );
                return( ERROR );
            }
            if( parm->def )
                strcpy( buffer, parm->def );

        } else { /* we have a parm! :) */

            char * startitem;
            int  str_level = 0;    /* <> level */
            char delim = ',';

            ptr = buffer + strlen(buffer);
            startitem = NULL;

            /* for VARARG parameter, ignore comma delimiter */
            if ((parm->next == NULL) && (macro->sym.vararg)) {
                delim = '\0';
            }

            if( *params == '%' ) {
                // *ptr++ = *params++;
                params++;
                expansion_flag = TRUE;
                if (*params == '<') {
                    string_expansion = TRUE;
                    delim = '>';
                    params++;
                } else {
                    string_expansion = FALSE;
                    Token_Count = Tokenize(params, 0);
                    DebugMsg(( "RunMacro(%s): parm >%s< tokenized, %u tokens\n", macro->sym.name, params, Token_Count));
                    // *ptr++ = '<';
                }
            }
#if 0
            if (*params == '<') {
                params++;
                str_level++;
                delim = '\0';  /* ignore commas */
            }
#endif
            while (*params != '\0') {
                dir_node * dir2;
                if  (*params == delim && str_level == 0)
                    break;
#if 1
                /* is this ok? what about EXITM <!SIGN?> ? */
                if (*params == '!' && *(params+1) != '\0') {
                    params++;
                    *ptr++ = *params++;
                    continue;
                }
#endif
                if (*params == '<') {
                    str_level++;
                    if (str_level == 1) {
                        params++;
                        continue;
                    }
                } else if (*params == '>') {
                    str_level--;
                    if (str_level == 0) {
                        params++;
                        continue;
                    }
                }
//                if (*params == '"' || *params == '\'') {
                if (str_level == 0 && (*params == '"' || *params == '\'')) {
                    char startc = *params;
                    *ptr++ = *params++;
                    while (*params) {
                        *ptr++ = *params++;
                        if (*(params-1) == startc)
                            if (*params == startc)
                                *ptr++ = *params++;
                            else
                                break;
                    }
                    startitem = NULL;
                    continue;
                }

                if (bracket_level > 0) {
                    if (*params == '(') {
                        bracket_level++;
                    } else if (*params == ')') {
                        bracket_level--;
                        if (bracket_level == 0)
                            break;
                    }
                }
                if (str_level == 0 && isspace(*params)) {
                    char * ptr2 = params+1;
                    while (isspace(*ptr2)) ptr2++;
                    if (*ptr2 == delim || *ptr2 == parm_end_delim) {
                        /* skip trailing spaces */
                        params++;
                        continue;
                    }
                }
                if (startitem == NULL && is_valid_id_char(*params) == TRUE)
                    startitem = ptr;

                if (expansion_flag == TRUE) {
                    i = 0;
                    if (AsmBuffer[0]->token == T_ID) {
                        dir = (dir_node *)SymSearch(AsmBuffer[0]->string_ptr);
                        if (dir && dir->sym.state == SYM_TMACRO && dir->sym.defined == TRUE) {
                            for (count = 0;count < MAX_TEXTMACRO_NESTING;count++) {
                                dir_node * tmpdir;
                                tmpdir = (dir_node *)SymSearch( dir->sym.string_ptr );
                                if (tmpdir &&
                                    tmpdir->sym.state == SYM_TMACRO &&
                                    tmpdir->sym.defined)
                                    dir = tmpdir;
                                else
                                    break;
                            }
                            strcpy(ptr, dir->sym.string_ptr);
                            ptr = ptr + strlen(ptr);
                            if (AsmBuffer[1]->token != T_FINAL)
                                params = AsmBuffer[1]->pos;
                            else
                                while (is_valid_id_char(*params)) params++;
                            continue;
                        }
                    }
                    if (EvalOperand(&i, Token_Count, &opndx, TRUE) == ERROR)
                        return(ERROR);
                    DebugMsg(( "RunMacro(%s): expansion, opndx.type=%u, value=%u\n", macro->sym.name, opndx.type, opndx.value));
                    if (opndx.type == EXPR_CONST && opndx.string == NULL) {
                        sprintf(ptr,"%d",opndx.value);
                        ptr = ptr + strlen(ptr);
                        if (AsmBuffer[i]->token != T_FINAL)
                            params = AsmBuffer[i]->pos;
                        else
                            params = params + strlen(params);
                        continue;
                    }
                }

                *ptr++ = *params++;

                /* check for (text) macros */

                if (str_level == 0 &&
                    is_valid_id_char(*params) == FALSE &&
                    startitem != NULL) {

                    *ptr = '\0';
                    dir = (dir_node *)SymSearch(startitem);
                    if (dir) {
                        if (dir->sym.state == SYM_MACRO &&
                            dir->sym.defined == TRUE &&
                            dir->sym.isfunc == TRUE) {
                            char * p = params;
                            while (isspace(*p)) p++;
                            /* no macro function invokation if the '(' is missing! */
                            if (*p == '(') {
                                params -= strlen(startitem);
                                ptr -= strlen(startitem);
                                *ptr = '\0';
                                line[0] = '\0';
                                i = RunMacro(dir, params, line, TRUE, TRUE, FALSE);
                                DebugMsg(("RunMacro(%s): back from RunMacro(%s), rc=%u, buffer=>%s<\n", macro->sym.name, dir->sym.name, i, line));
                                if (i != ERROR) {
                                    strcpy(ptr, line);
                                    ptr += strlen(line);
                                    params = params + i;
                                    startitem = NULL;
                                    continue;
                                } else
                                    return(ERROR);
                            }
                        }
                    }
                    startitem = NULL;
                }

            } /* end while */
            if (expansion_flag)
                if (string_expansion && *params && *params != parm_end_delim)
                    params++;

            *ptr = '\0';
        }
        parm->replace = AsmTmpAlloc(strlen(buffer)+1);
        strcpy( parm->replace, buffer );

        if (*params == ',') {
            params++;
        }
        DebugMsg(("RunMacro(%s): parm replacement: >%s< -> >%s<\n", macro->sym.name, parm->label, parm->replace));
    } /* end for  */

    if (bracket_level >= 0) {
        if (*params == '\0') {
            DebugMsg(("RunMacro(%s): missing ')'\n", macro->sym.name));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        } else if (*params != ')') {
            DebugMsg(("RunMacro(%s): expected ')', found >%s<\n", macro->sym.name, params));
            AsmError( TOO_MANY_ARGUMENTS_IN_MACRO_CALL);
            return( ERROR );
        } else
            params++;

        /* if macro name is "", it's a FOR/FORC macro.
         a check for a valid end must NOT be done then. */
    } else if (*params != '\0' && *macro->sym.name != NULLC) {
        DebugMsg(("RunMacro(%s): expected NULL, found >%s<\n", macro->sym.name, params));
        AsmError( TOO_MANY_ARGUMENTS_IN_MACRO_CALL);
        return( ERROR );
    }

    for( i = 0, parm = info->parmlist; parm != NULL; parm = parm->next )
        i ++;
    for( localcnt = 0, local= info->locallist; local != NULL; local = local->next ) {
        localcnt++;
    }
    i = i + localcnt;

    parm_array = AsmTmpAlloc( i * sizeof( char * ) );

    /* now actually fill in the parms */

    for( count = 0, parm = info->parmlist; parm != NULL; parm = parm->next ) {
        parm_array[count] = parm->replace;
        count++;
    }

    if (localcnt) {
        local_array = AsmTmpAlloc( localcnt * 8 ); /* 8 is max size of local label name */
        for( localcnt = 0, local = info->locallist; local != NULL; local = local->next, localcnt++ ) {
            parm_array[count] = local_array + localcnt * 8;
            sprintf(parm_array[count],"??%04u", MacroLocalVarCounter);
            DebugMsg(("RunMacro(%s): local %s replaced by %s, name index=%u\n", macro->sym.name, local->label, parm_array[count], count));
            MacroLocalVarCounter++;
            count++;
        }
    }

    /* a predefined macro func with a function address? */

    if (macro->sym.predefined == TRUE && macro->sym.func_ptr != NULL) {
        macro->sym.func_ptr(line, parm_array);
        strcat(prefix, line);
//        free_parmlist( info->parmlist );
        AsmBuffer[0]->value = T_EXITM;
        return( params - orgsrc );
    }

    /* emit the source lines */

    if (insert) {
        PushLineQueue();
    }

    if (macro->sym.isfunc == FALSE && prefix)
        InputQueueLine( prefix );

    for( lnode = info->data; lnode != NULL; lnode = lnode->next ) {
        // DebugMsg(("macro line: >%s<\n", lnode->line));
        if (*(lnode->line) != ':') {
            my_sprintf( line, lnode->line, count-1, parm_array );
            InputQueueLine( line );
        }
    }
    if (runit || macro->sym.isfunc)
        InputQueueLine( "endm" );

    if (*(macro->sym.name) && runit == FALSE) {
        /* put this macro into the file stack */
        PushMacro( (asm_sym *)macro, FALSE );
    }

    /* now free the parm replace strings */
//    free_parmlist( info->parmlist );

    if (runit || macro->sym.isfunc) {
        /* run the assembler until we hit EXITM, GOTO or ENDM */
        MacroLevel++;
        for(;;) {
            int i;
            while (0 == (i = AsmLine( buffer )));
            if (i < 0) {
                MacroLevel--;
                return(ERROR);
            }
            if (AsmBuffer[0]->token == T_DIRECTIVE)
                if (AsmBuffer[0]->value == T_EXITM) {
                    int pos;
                    i = 1;
                    DebugMsg(("RunMacro(%s): EXITM detected\n", macro->sym.name));
                    if (prefix) {
                        line[0] = '\0';
                        if (addbrackets) {
                            strcat(line,"<");
                            if (AsmBuffer[i]->token != T_FINAL)
                                strcat(line, AsmBuffer[i]->string_ptr);
                            strcat(line,">");
                        } else {
                            if (AsmBuffer[i]->token != T_FINAL) {
                                char *p = AsmBuffer[i]->string_ptr;
                                char *p2 = line;
                                while (*p) {
                                    if (*p == '!' &&
                                        (*(p+1) == '<' || *(p+1) == '>' || *(p+1) == '!'))
                                        p++;
                                    *p2++ = *p++;
                                }
                                *p2 = NULLC;
                            }
                        }
                        DebugMsg(("RunMacro(%s): prefix=%s, exitm-str=%s, suffix=%s\n", macro->sym.name, prefix, AsmBuffer[i]->string_ptr, params));
                        strcat(prefix, line);
//                        strcat(prefix, params);
                    }
                    MacroLevel--;
                    DebugMsg(("RunMacro(%s): calling SkipMacro()\n", macro->sym.name));
                    if (SkipMacro() == ERROR) {
                        DebugMsg(("RunMacro(%s): SkipMacro failed!\n", macro->sym.name));
                        return(ERROR);
                    }
                    DebugMsg(("RunMacro(%s): SkipMacro() done\n", macro->sym.name));
                    AsmBuffer[0]->value = T_EXITM;
                    DebugMsg(("RunMacro(%s): EXITM, MacroLevel=%u >%s<\n", macro->sym.name, MacroLevel, prefix));
                    break;
                } else if (AsmBuffer[0]->value == T_ENDM) {
                    DebugMsg(("RunMacro(%s): ENDM, MacroLevel=%u\n", macro->sym.name, MacroLevel));
                    MacroLevel--;
                    break;
                } else if (AsmBuffer[0]->value == T_GOTO) {
                    DebugMsg(("RunMacro(%s): GOTO, MacroLevel=%u\n", macro->sym.name, MacroLevel));
                    if (AsmBuffer[1]->token != T_FINAL && AsmBuffer[1]->token != T_NUM)
                        strcpy (line, AsmBuffer[1]->string_ptr);
                    else
                        line[0] = '\0';
                    SkipMacro();
                    for( lnode = info->data; lnode != NULL; lnode = lnode->next ) {
                        ptr = lnode->line;
                        if (*ptr == ':' &&  (stricmp(ptr+1,line) == 0)) {
                            lnode = lnode->next;
                            break;
                        }
                    }
                    if (lnode) {
                        DebugMsg(("RunMacro(%s): GOTO, found label >%s<\n", macro->sym.name, line));
                        PushLineQueue();
                        for( ; lnode != NULL; lnode = lnode->next ) {
                            my_sprintf( line, lnode->line, count-1, parm_array );
                            InputQueueLine( line );
                        }
                        InputQueueLine( "endm" );
                        continue;
                    } else {
                        break;
                    }
                }
#if FASTPASS
            if (StoreState)
                StoreLine( buffer );
#endif
            ParseItems();
            if (EndDirectiveFound) {
                AsmError( UNEXPECTED_END_OF_FILE );
                return(ERROR);
            }
        }
    }

    DebugMsg(("RunMacro(%s) exit\n", macro->sym.name));

    return( params - orgsrc );
}

// ExpandText() is called if
// - the evaluation operator '%' has been found as first
//   character of the line.
// - for SUBSTR, INSTR and SIZESTR
// Then do expansion within strings!
// if substitute is TRUE, scanning for the
// substitution character is active !

static int ExpandText(char * line, bool substitute)
{
    char * pSrc;
    char * pDst;
    char * pIdent;
    char * pStart;
    int len;
    int rc = NOT_ERROR;
    int count;
    bool expanded = TRUE;
    dir_node * dir;
    char srcline[MAX_LINE_LEN];
    char identifier[MAX_LINE_LEN];

    DebugMsg(("ExpandText(%s, %u) enter\n", line, substitute));
    for (count = 0;count < MAX_TEXTMACRO_NESTING && expanded;count++) {
        strcpy(srcline, line);
        expanded = FALSE;
        pSrc = srcline;
        pDst = line;
        for (pIdent = identifier;;) {
            if( is_valid_id_char( *pSrc ) )
                *pIdent++ = *pSrc++;
            else {
                len = pIdent - identifier;
                if (len) {
                    *pIdent = '\0';
                    pIdent = identifier;
                    dir = (dir_node *)SymSearch(identifier);
                    if (dir && dir->sym.state == SYM_TMACRO && dir->sym.defined == 1) {
                        if (substitute) {
                            if (*(pDst-1) == '&')
                                pDst--;
                            if (*pSrc == '&')
                                pSrc++;
                        }
                        DebugMsg(("ExpandText(): %s is to be replaced by >%s<\n", identifier, dir->sym.string_ptr));
                        pIdent = dir->sym.string_ptr;
                        rc = STRING_EXPANDED;
                        expanded = TRUE;
                    }
                    while (*pIdent) {
                        if (*pIdent == '!' && *(pIdent+1) != NULLC)
                            pIdent++;
                        *pDst++ = *pIdent++;
                    }
                }
                *pDst++ = *pSrc++;
                if (*(pSrc-1) == '\0')
                    break;
                pIdent = identifier;
            }
        }
    }
    if (count == MAX_TEXTMACRO_NESTING) {
        AsmError(NESTING_LEVEL_TOO_DEEP);
        return(ERROR);
    }
    DebugMsg(("ExpandText(%s) exit\n", line));
    return(rc);
}

// rebuild the source line
// adjust all "pos" values behind the current pos
// newstring = new value of item i
// i = position of item to replace
// string = source line to rebuild
// olelen = length of old item i

static void RebuildLine(char * newstring, int i, char * string, int oldlen, bool addbrackets)
{
    char buffer[MAX_LINE_LEN];
    char * dest = buffer;
    char * src;
    int newlen;
    int len;

    src = AsmBuffer[i]->pos;
    len = src - string;          /* get the length till the ID      */
    memcpy(dest, string, len);   /* copy things till start of ident */
    dest += len;
    newlen = strlen(newstring);
    if (addbrackets) {
        *dest++ = '<';
        memcpy(dest, newstring, newlen);
        dest += newlen;
        *dest++ = '>';
        newlen += 2;
    } else {
#if 0
        memcpy(dest, newstring, newlen);
        dest += newlen;
#else
        char * p = newstring;
        while (*p) {
            if (*p == '!' && *(p+1) != NULLC) {
                p++;
                newlen--;
            }
            *dest++ = *p++;
        }
#endif
    }
    /* len now has the length of the ID */
    strcpy(dest, src+oldlen);

    for (i++;i < Token_Count;i++) {
        if (AsmBuffer[i]->token != T_NUM)
            AsmBuffer[i]->pos = AsmBuffer[i]->pos - oldlen + newlen;
    }

    strcpy(string, buffer);
    return;
}

// replace text macros by their values, recursively
// dir: found text macro item
// pos: index AsmBuffer
// string: full line

static int ExpandTextMacro( dir_node * dir, int pos, char * string, int addbrackets)
/******************************************************************/
{
    int count;
    int rc;
#if 1
    int start;
    char * p;
    char buffer[MAX_LINE_LEN];
    char buffer2[MAX_LINE_LEN];
#endif

    DebugMsg(("ExpandTextMacro(pos=%u) enter (tokens=%u)\n", pos, Token_Count));

    count = 0;
    for (;;) {
        dir_node *tmpdir;
        /* expand text equates only! */
#if 1
        if (p = strstr(dir->sym.string_ptr,"(")) {
            rc = p - dir->sym.string_ptr;
            memcpy(buffer, dir->sym.string_ptr, rc);
            buffer[rc] = '\0';
            tmpdir = (dir_node *)SymSearch( buffer );
            if (tmpdir && tmpdir->sym.state == SYM_MACRO) {
                buffer[0] = '\0';
                /* size of original string */
                count = strlen(AsmBuffer[pos]->string_ptr);
                start = AsmBuffer[pos]->pos - string;
                strcpy(buffer2, string);
                rc = RunMacro(tmpdir, dir->sym.string_ptr, buffer, TRUE, TRUE, addbrackets);
                DebugMsg(("ExpandTextMacro: replace >%s< by >%s<\n", dir->sym.string_ptr, buffer));
                memcpy(string, buffer2, start);
                p = string + start;
                strcpy(p, buffer);
                p = p + strlen(p);
                strcpy(p, buffer2+start+count);
                Token_Count = 0;
                return(STRING_EXPANDED);
            } else
                break;
        } else
#endif
        tmpdir = (dir_node *)SymSearch( dir->sym.string_ptr );
//        if (tmpdir && tmpdir->sym.state == SYM_TMACRO && tmpdir->sym.defined == 1) {
        if (tmpdir && tmpdir->sym.state == SYM_TMACRO) {
            dir = tmpdir;
            count++;
            if (count >= MAX_TEXTMACRO_NESTING) {
                AsmError(NESTING_LEVEL_TOO_DEEP);
                return(ERROR);
            }
            continue;
        }
        break;
    }
    DebugMsg(("ExpandTextMacros: replace >%s< by >%s<\n", AsmBuffer[pos]->string_ptr, dir->sym.string_ptr));
    RebuildLine(dir->sym.string_ptr,
                pos, string,
                strlen(AsmBuffer[pos]->string_ptr), addbrackets);
    return (STRING_EXPANDED);
}

// expand one token
// count: position of token

int ExpandToken(int count, char * string, bool addbrackets, bool Equ_Mode)
{
    int pos;
    expr_list opndx;
    dir_node    *dir;
    int rc = NOT_ERROR;

    if (AsmBuffer[count]->token == T_PERCENT && Equ_Mode == FALSE) {
        pos = count+1;
        EvalOperand( &pos, Token_Count, &opndx, TRUE );
        if (opndx.type == EXPR_CONST) {
            char * p;
            sprintf(CurrStringEnd,"%u", opndx.value);
            p = AsmBuffer[count]->pos;
            AsmBuffer[count]->token = T_STRING;
            AsmBuffer[count]->string_ptr = CurrStringEnd;
            AddTokens( count+1, count+1 - pos );
            for (pos = count+1;AsmBuffer[pos]->token == T_NUM;pos++);
            if (AsmBuffer[pos]->token == T_FINAL)
                pos = strlen(p);
            else
                pos = AsmBuffer[pos]->pos - p;
            RebuildLine(CurrStringEnd, count, string, pos, addbrackets);
            return(STRING_EXPANDED);
        }
#if 1
    } else if (AsmBuffer[count]->token == T_STRING && *(AsmBuffer[count]->string_ptr) == '%') {
        char * p = AsmBuffer[count]->string_ptr + 1;
        dir = (dir_node *)SymSearch( p );
        /* it is either a text equate or a numeric expression */
        if (dir && dir->sym.state == SYM_TMACRO) {
            AsmBuffer[count]->pos--;
            /* skip the leading '%' */
            AsmBuffer[count]->string_ptr++;
            AsmBuffer[count]->value--;
            AsmBuffer[count]->token = T_ID;
            RebuildLine(p, count, string, AsmBuffer[count]->value+3, FALSE);
            rc = STRING_EXPANDED;
        } else {
            expr_list opndx;
            pos = Token_Count;
            Token_Count = Tokenize(p, Token_Count);
            if (EvalOperand(&pos, Token_Count, &opndx, TRUE) == ERROR)
                return(ERROR);
            sprintf(CurrStringEnd, "%u", opndx.value);
            RebuildLine(CurrStringEnd, count, string, AsmBuffer[count]->value, FALSE);
            return(STRING_EXPANDED);
        }
#endif
    }
    if( AsmBuffer[count]->token == T_ID ||
        (expansion == TRUE && AsmBuffer[count]->token == T_STRING))  {
        dir = (dir_node *)SymSearch( AsmBuffer[count]->string_ptr );
        if( dir != NULL) {
            if (dir->sym.state == SYM_MACRO &&
                dir->sym.defined == TRUE) {
                char buffer[MAX_LINE_LEN];
                char cmd[MAX_LINE_LEN];
                memcpy(buffer, string, AsmBuffer[count]->pos - string);
                buffer[AsmBuffer[count]->pos - string] = '\0';
                strcpy(cmd, AsmBuffer[count]->pos);
                if (dir->sym.isfunc == TRUE) {
                    int savedIfState;
                    /* ignore macro functions without a following '(' */
                    if (AsmBuffer[count+1]->token != T_OP_BRACKET)
                        return(NOT_ERROR);
                    savedIfState = CurrIfState;
                    CurrIfState = 0;
                    rc = RunMacro(dir, AsmBuffer[count]->pos, buffer, TRUE, TRUE, addbrackets);
                    CurrIfState = savedIfState;
                    if (rc != ERROR) {
                        DebugMsg(("ExpandToken: back from RunMacro(%s), string=>%s<\n", dir->sym.name, buffer));
                        strcat(buffer, cmd+rc);
                        strcpy(string, buffer);
                        rc = STRING_EXPANDED;
                    }
                } else {
                    if (count > 0 && AsmBuffer[count-1]->token != T_COLON) {
                        DebugMsg(("macro called without brackets at operand location\n"));
                        AsmError(SYNTAX_ERROR);
                        return(ERROR);
                    }
                    /* is runit=FALSE possible at all? */
                    rc = RunMacro(dir, AsmBuffer[count]->pos, buffer, dir->sym.runsync || MacroLevel > 0, TRUE, FALSE);
                    DebugMsg(("ExpandToken: back from RunMacro(%s) - called as Proc\n", dir->sym.name));
                    if (rc != ERROR)
                        if (AsmBuffer[0]->value == T_EXITM) {
                            strcat(buffer, cmd+rc);
                            strcpy(string, buffer);
                            rc = STRING_EXPANDED;
                        } else
                            rc = NOT_ERROR;
                }
                Token_Count = 0; /* AsmBuffer is destroyed, exit */
//            } else if( dir->sym.state == SYM_TMACRO && dir->sym.defined == TRUE) {
            } else if( dir->sym.state == SYM_TMACRO) {
                rc = ExpandTextMacro(dir, count, string, addbrackets);
            }
        }
    }
    return( rc);
}

// scan current line for (text) macros and expand them.
// [this is to be changed. Currently it runs AFTER Tokenize(),
// but it is to run BEFORE Tokenize()].

int ExpandMacro( char * string)
/*****************************/
{
    int count = 0;
    int addbrackets = FALSE;
    int Equ_Mode = FALSE;
    asm_sym *sym;
    int rc;
    int i;
    char buffer[MAX_LINE_LEN];

    /* filter certain conditions */
    /* addbrackets: for preprocessor directives which expect a string
     parameter, the string has to be enclosed in '<>' again.
     */
    if ( AsmBuffer[0]->token == T_DIRECTIVE ) {
        if ( AsmBuffer[0]->opcode & OPCF_STRPARM )
            addbrackets = TRUE;
        else if ( AsmBuffer[0]->opcode & OPCF_NOEXPAND ) {
            /* PURGE, IF[N]DEF, .ERR[N]DEF, ECHO, FOR[C]? */
            /* for these directives don't expand strings! */
            if (AsmBuffer[0]->value == T_PURGE || expansion == FALSE)
                return( NOT_ERROR );
        } else if ( AsmBuffer[0]->value == T_OPTION) {
            if ( AsmBuffer[1]->token == T_ID)
                if (strcmpi(AsmBuffer[1]->string_ptr, "PROLOGUE") == 0)
                    return( NOT_ERROR);
                if (strcmpi(AsmBuffer[1]->string_ptr, "EPILOGUE") == 0)
                    return( NOT_ERROR);
        }
    } else if ((Token_Count > 1) && ( AsmBuffer[1]->token == T_DIRECTIVE )) {
        switch (AsmBuffer[1]->value) {
        case T_TEXTEQU:
        case T_CATSTR:
            count = 2;
            addbrackets = TRUE;
            break; /* ok? was missing in previous versions! */
        case T_INSTR:
            rc = 0;
            for (i = 2, count = 0;i < Token_Count; i++)
                if (AsmBuffer[i]->token == T_COMMA) {
                    if (rc == 0)
                        rc = i+1;
                    count++;
                }
            if (count < 2)
                count = 2;
            else
                count = rc;
            /* fall thru */
        case T_SIZESTR:
        case T_SUBSTR: /* expand the source string as if % has been set */
            rc = NOT_ERROR;
            if (AsmBuffer[1]->value != T_INSTR)
                count = 2;
            if (AsmBuffer[count]->token == T_ID) {
                strcpy(buffer, AsmBuffer[count]->string_ptr);
                if (ExpandText(buffer, FALSE) == STRING_EXPANDED) {
                    RebuildLine(buffer, count, string,
                                strlen(AsmBuffer[count]->string_ptr), TRUE);
                    AsmBuffer[count]->token = T_STRING;
                    rc = STRING_EXPANDED;
                }
                count++;
            }
            count = 2;
            goto std_expansion;
        case T_MACRO:
            sym = SymSearch( AsmBuffer[0]->string_ptr );
            /* don't expand macro DEFINITIONs!
             the name is an exception, if it's not the macro itself
             */
            if (sym && sym->state != SYM_MACRO)
                return (ExpandToken(0, string, FALSE, FALSE));
            return(NOT_ERROR);
        case T_EQU:
            if (AsmBuffer[1]->opcode & 1) /* ignore '='! */
                break;
            /* EQU is usually not expanded ...  if it is a text and
             unless expansion operator is first character.
             */
            sym = SymSearch(AsmBuffer[0]->string_ptr);
            if (sym == NULL || sym->state == SYM_TMACRO) {
                if (expansion == FALSE)
                    return(NOT_ERROR);
                else {
                    /* expand text macros and macro functions */
                    /* ignore the % before items */
                    /* with "%" as first character, even the equate's name
                     is to be expanded!
                     */
                    Equ_Mode = TRUE;
                    count = 2;
                }
            }
        }
    } else if (Token_Count > 2 &&
               AsmBuffer[1]->token == T_COLON &&
               AsmBuffer[2]->token == T_DIRECTIVE ) {
        /* skip the code label */
        if ( AsmBuffer[2]->opcode & OPCF_STRPARM )
            addbrackets = TRUE;
        else if ( AsmBuffer[2]->opcode & OPCF_NOEXPAND )
            if (AsmBuffer[2]->value == T_PURGE || expansion == FALSE)
                return( NOT_ERROR );
    }

    if (expansion == TRUE) {
//        expansion = FALSE;
        if (STRING_EXPANDED == ExpandText(string, TRUE))
            return(STRING_EXPANDED);
    }
    // scan the line from left to right for (text) macros.
    // it's currently not quite correct. a macro proc should only
    // be evaluated in the following cases:
    // 1. it is the first token of a line
    // 2. it is the second token, and the first one is an ID
    // 3. it is the third token, the first one is an ID and
    //    the second is a ':' or '::'.
    rc = NOT_ERROR;
std_expansion:
    for(  ; count < Token_Count; count++ ) {
        if (ExpandToken(count, string, addbrackets, Equ_Mode) == STRING_EXPANDED)
            rc = STRING_EXPANDED;
    }
    return( rc );
}

// MACRO directive: define a macro

int MacroDef( int i)
/********************************/
{
    char                *name;
    bool                store_data;
    dir_node            *macro;

    DebugMsg(("MacroDef enter\n"));
    if( i < 0 ) {
        AsmError( PROC_MUST_HAVE_A_NAME );
        return( ERROR );
    }
    if (AsmBuffer[i]->token != T_ID) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    name = AsmBuffer[i]->string_ptr;
    DebugMsg(("Macro name=>%s<\n", name));
    macro = (dir_node *)SymSearch( name );
    if( macro == NULL ) {
        macro = dir_insert( name, TAB_MACRO );
        macro->e.macroinfo->srcfile = get_curr_srcfile();
    } else if( macro->sym.state != SYM_MACRO ) {
        if( Parse_Pass == PASS_1 )
            AsmErr( SYMBOL_ALREADY_DEFINED, name );
        return( ERROR );
    }

    if (( Parse_Pass == PASS_1) || (macro->sym.variable)) {
        /* is the macro redefined? */
        if ( macro->e.macroinfo->data != NULL) {
            dir_change( macro, TAB_MACRO);
            macro->sym.variable = TRUE;
        }
        store_data = TRUE;
    } else
        store_data = FALSE;

    return( FillMacro( macro, i+2, store_data ) );
}

/* get text macro value */

int GetTextMacroValue(char * p, char * buffer)
{
    char *dest = buffer;
    if (p)
        for (;*p;) {
            if (*p == '!' && *(p+1) != NULLC)
                p++;
            *dest++ = *p++;
        }
    *dest = NULLC;
    return(dest - buffer);
}

/*
 used by EQU if value is a text.
*/
asm_sym * SetTextMacro(asm_sym *sym, char *name, char * value)
{
    int count;

#if FASTPASS
    /* there's no need to set the value if FASTPASS is on, because
     the input are just preprocessed lines */
    if (Parse_Pass != PASS_1)
        return(sym);
#endif

    if (sym == NULL)
        sym = SymCreate( name, TRUE );

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;

    /*
     the original source is used, since the tokenizer has
     deleted some information
     */
    while (isspace(*value)) value++;
    count = strlen(value);
    if (count) {
        for (;count;count--)
            if (isspace(*(value+count-1)) == FALSE)
                break;
        if (count)
            if (*value == '<' && *(value+count-1) == '>') {
                value++;
                count = count - 2;
            }
    }
    if (sym->string_ptr)
        AsmFree(sym->string_ptr);
    sym->string_ptr = (char *)AsmAlloc( count + 1);
    memcpy(sym->string_ptr, value, count);
    *(sym->string_ptr+count) = '\0';

    DebugMsg(("SetTextMacro(%s): value is >%s<, exit\n", sym->name, sym->string_ptr));
    return( sym );
}

// CatStr()
// defines a text equate
// syntax <name> CATSTR [<string>,...]
// TEXTEQU is an alias for CatStr()

int CatStrDef( int start, asm_sym * *psym )
/********************/
{
    asm_sym *sym;
    int count;
    int i;
    char *string;
    expr_list opndx;
    char buffer2[32];
    char buffer[MAX_LINE_LEN];

    DebugMsg(("CatStrDef enter\n"));

    if (AsmBuffer[0]->token != T_ID) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    sym = SymSearch( AsmBuffer[0]->string_ptr );

    if( sym && sym->state != SYM_UNDEFINED && sym->state != SYM_TMACRO) {
        /* it is defined as something else, get out */
        DebugMsg(( "CatStrDef(%s) exit, error\n", sym->name));
        AsmErr( SYMBOL_REDEFINITION, sym->name );
        return( ERROR );
    }

    buffer[0] = '\0';
    for (i = start, count = 0;i < Token_Count;) {
        DebugMsg(("CatStrDef(%s): item=%s\n", AsmBuffer[0]->string_ptr, AsmBuffer[i]->string_ptr));
        if (AsmBuffer[i]->token == T_STRING) {
            string = AsmBuffer[i]->string_ptr;
            i++;
        } else {
            DebugMsg(("CatStrDef: bad item: token=%u\n", AsmBuffer[i]->token));
            AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        count = count + strlen(string);
        if (count >= MAX_LINE_LEN) {
            AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
            return( ERROR );
        }
        strcat(buffer, string);
        if ((AsmBuffer[i]->token != T_COMMA) &&
            (AsmBuffer[i]->token != T_FINAL)) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        i++;
    }

    if ( sym == NULL) {
        sym = SymCreate( AsmBuffer[0]->string_ptr, TRUE );
        DebugMsg(( "CatStrDef: new symbol %s created\n", sym->name));
    }

    if ( sym->string_ptr )
        AsmFree(sym->string_ptr);

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->string_ptr = (char *)AsmAlloc( count + 1);
    strcpy(sym->string_ptr, buffer);
    DebugMsg(("CatStrDef(%s) result: >%s<\n", sym->name, buffer));
    if (psym)
        *psym = sym;

    WriteLstFile(LSTTYPE_EQUATE, 0, sym);

    return( NOT_ERROR );
}

// SubStr()
// defines a text equate
// syntax <name> SUBSTR <string>, pos [, size]
// the parameters are NOT expanded

// if <string> is an ID, it will be expanded
// anything behind the ID is skipped until the comma

int SubStrDef( int i, char * string)
/********************/
{
    struct asm_sym      *sym;
    char                *name;
    char                *p;
    char                *newvalue;
    int                 pos;
    int                 size = MAX_LINE_LEN;
    int                 cnt;
    char                buffer[MAX_LINE_LEN];
    expr_list           opndx;

    DebugMsg(("SubStrDef entry\n"));

    // at least 5 items are needed
    // 0  1      2      3 4    5   6
    // ID SUBSTR SRC_ID , POS [, LENGTH]

    if ((i != 1) || (AsmBuffer[0]->token != T_ID)) {
        DebugMsg(("SubStrDef exit 1, error, i=%u, tokens=%u\n", i, Token_Count));
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    name = AsmBuffer[0]->string_ptr;
    i++;

    // third item must be a string

    if (AsmBuffer[i]->token != T_STRING) {
        DebugMsg(("SubStrDef exit 2, error\n"));
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    p = AsmBuffer[i]->string_ptr;
    i++;
    DebugMsg(("SubStrDef(%s): src=>%s<\n", name, p));

    if (AsmBuffer[i]->token != T_COMMA) {
        AsmError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;

    // get pos, must be a numeric value

    if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR) {
        DebugMsg(("SubStrDef(%s): invalid pos value\n", name));
        return(ERROR);
    }

    if (opndx.type != EXPR_CONST || opndx.string != NULL) {
        DebugMsg(("SubStrDef(%s): pos value is not a constant\n", name));
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }

    /* pos is expected to be 1-based, but 0 is treated as a 1 */
    pos = opndx.value;
    if (pos == 0)
        pos++;

    if (AsmBuffer[i]->token != T_FINAL) {
        if (AsmBuffer[i]->token != T_COMMA) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
        // get size, must be a numeric value
        if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR) {
            DebugMsg(("SubStrDef(%s): invalid size value\n", name));
            return(ERROR);
        }
        if (opndx.type != EXPR_CONST || opndx.string != NULL) {
            DebugMsg(("SubStrDef(%s): size value is not a constant\n", name));
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        size = opndx.value;
        if (AsmBuffer[i]->token != T_FINAL) {
            DebugMsg(("SubStrDef(%s): additional items found\n", name));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
    }

    sym = SymSearch( name );

    /* if we've never seen it before, put it in */
    if( sym == NULL ) {
        sym = SymCreate( name, TRUE );
    } else {
        /* was it referenced before definition (shouldn't happen anymore) */
        if( sym->state == SYM_UNDEFINED && sym->state != SYM_TMACRO ) {
            /* it is defined as something else, get out */
            DebugMsg(( "SubStrDef(%s) exit, error\n", sym->name));
            AsmErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }
    }

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;

    for (pos--;pos && *p;pos--, p++)
        if (*p == '!' && *(p+1) != NULLC)
            p++;

    if (*p == '!' && *(p+1) != NULLC)
        p++;

    for (newvalue = p, cnt = size; *p && cnt; cnt--, p++)
        if (*p == '!' && *(p+1) != NULLC)
            p++;

    size = p - newvalue;
    p = newvalue;

    newvalue = AsmAlloc (size + 1);
    memcpy(newvalue, p, size);
    *(newvalue+size) = '\0';
    DebugMsg(("SubStrDef(%s): result=>%s<\n", sym->name, newvalue));
    AsmFree(sym->string_ptr);
    sym->string_ptr = newvalue;

    WriteLstFile(LSTTYPE_EQUATE, 0, sym);

    return( NOT_ERROR );
}

// SizeStr()
// defines a numeric variable which contains size of a string

int SizeStrDef( int i )
{
    asm_sym *sym;
    int sizestr;
    char buffer[MAX_LINE_LEN];

    DebugMsg(("SizeStrDef entry\n"));

    if (i != 1 ||
        AsmBuffer[0]->token != T_ID ||
        Token_Count != 3 ||
        AsmBuffer[2]->token != T_STRING) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    sizestr = GetTextMacroValue(AsmBuffer[2]->string_ptr, buffer);

    if (sym = CreateConstant( AsmBuffer[0]->string_ptr, sizestr, -1, TRUE)) {
        DebugMsg(("SizeStrDef(%s) exit, value=%u\n", AsmBuffer[0]->string_ptr, sizestr));
        WriteLstFile(LSTTYPE_EQUATE, 0, sym);
        return(NOT_ERROR);
    } else
        return(ERROR);

}

// InStr()
// defines a numeric variable which contains position of substring
// syntax
// name INSTR [pos,]string,substr

int InStrDef( int i, char * string )
{
    asm_sym *sym;
    int l;
    int j;
    int commas;
    char *string1;
    char buffer1[MAX_LINE_LEN];
    char buffer2[MAX_LINE_LEN];
    expr_list opndx;
    int start;
    int strpos;

    DebugMsg(("InStrDef entry\n"));

    if ((i != 1) || (AsmBuffer[0]->token != T_ID)) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    i++;

    if (AsmBuffer[i]->token != T_STRING) {
        if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
            return(ERROR);
        if (opndx.type != EXPR_CONST || opndx.string != NULL) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        start = opndx.value;
        if (AsmBuffer[i]->token != T_COMMA) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        i++; /* skip comma */
    } else {
        start = 1;
    }

    if (AsmBuffer[i]->token != T_STRING) {
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    l = GetTextMacroValue(AsmBuffer[i]->string_ptr, buffer1);
#ifdef DEBUG_OUT
    DebugMsg(("InStrDef: first string >%s< \n", buffer1));
#endif
    i++;
    if (AsmBuffer[i]->token != T_COMMA) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    i++;

    if (AsmBuffer[i]->token != T_STRING) {
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    j = GetTextMacroValue(AsmBuffer[i]->string_ptr, buffer2);
#ifdef DEBUG_OUT
    DebugMsg(("InStrDef: second string >%s< \n", buffer2));
#endif
    i++;
    if (AsmBuffer[i]->token != T_FINAL) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    strpos = 0;
    if ((l >= j) && (string1 = strstr(buffer1, buffer2)))
        strpos = string1 - buffer1 + 1;

    if (sym = CreateConstant( AsmBuffer[0]->string_ptr,strpos, -1, TRUE)) {
        DebugMsg(("InStrDef(%s) exit, value=%u\n", AsmBuffer[0]->string_ptr, strpos));
        WriteLstFile(LSTTYPE_EQUATE, 0, sym);
        return (NOT_ERROR);
    }
    return(ERROR);
}

// PURGE implementation.
// this is not the way MASM does it, but I'm rather unsure
// whether PURGE works in MASM at all. At least I was unable
// to purge anything there.

int PurgeMacro( int i)
/*****************************/
{
    dir_node *dir;

    for (; i < Token_Count;) {
        if (AsmBuffer[i]->token != T_ID) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        dir = (dir_node *)SymSearch(AsmBuffer[i]->string_ptr);
        if ((dir == NULL) || (dir->sym.state != SYM_MACRO)) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        dir->sym.defined = FALSE;
        i++;
        if (AsmBuffer[i]->token == T_FINAL)
            break;
        if (AsmBuffer[i]->token != T_COMMA) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        i++;
    }

    return(NOT_ERROR);
}

// internal @Environ macro function

static int DoEnviron(char * buffer, char * *params)
{
    char * p = getenv( *params );
    if (p)
        strcpy(buffer, p);
    else
        buffer[0] = '\0';
    return(NOT_ERROR);
}

// macro initialization
// this proc is called once per pass

int MacroInit( int pass)
{
    static char *predefs[] = {"@CatStr","@InStr","@SizeStr","@SubStr", NULL};
    static char *precont[] = {
#if 1
     "@CatStr MACRO s0,s1,s2,s3,s4,s5,s6,s7,s8,s9",
     "local result",
     "result CatStr <s0>,<s1>,<s2>,<s3>,<s4>,<s5>,<s6>,<s7>,<s8>,<s9>",
     "exitm result",
     "endm",
#endif
     "@InStr MACRO start,s1,s2",
     "local pos",
     "ifnb <start>",
     "pos InStr start,<s1>,<s2>",
     "else",
     "pos InStr <s1>,<s2>",
     "endif",
     "exitm %pos",
     "endm",

     "@SizeStr MACRO s1",
     "local size",
     "size SizeStr <s1>",
     "exitm %size",
     "endm",

     "@SubStr MACRO s1,start,length",
     "local result",
     "ifnb <length>",
     "result SubStr <s1>,start,length",
     "else",
     "result SubStr <s1>,start",
     "endif",
     "exitm result",
     "endm",
     NULL};

    char **ptr;
    dir_node *macro;

    MacroLevel = 0;
    MacroLocalVarCounter = 0;
    SkipMacroMode = FALSE;

    if (pass == PASS_1) {
        for (ptr = predefs; *ptr; ptr++) {
            macro = dir_insert(*ptr , TAB_MACRO );
            macro->sym.defined = TRUE;
            macro->sym.predefined = TRUE;
        }

        PushLineQueue();
        for (ptr = precont; *ptr; ptr++)
            InputQueueLine(*ptr);

        // add @Environ() macro func

        macro = dir_insert("@Environ", TAB_MACRO );
        macro->sym.defined = TRUE;
        macro->sym.predefined = TRUE;
        macro->sym.func_ptr = DoEnviron;
        macro->sym.isfunc = TRUE;
        macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list));
        macro->e.macroinfo->parmlist->next = NULL;
        macro->e.macroinfo->parmlist->replace = NULL;
        macro->e.macroinfo->parmlist->def = NULL;
        macro->e.macroinfo->parmlist->label = "p";
        macro->e.macroinfo->parmlist->required = TRUE;
    }
    return(NOT_ERROR);
}
