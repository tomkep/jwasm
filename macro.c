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
* - ExpandText   handles expansion within strings (% operator)
* - SkipMacro    skip macro execution if EXITM/GOTO has been found
* - RunMacro     run a MACRO
* - ExpandMacro  called by AsmLine() to expand (text) macros
* - FillMacro    fill a macro's parameter/local/line list
* - MacroDef     handle MACRO directive
* - CatStrDef    handle CATSTR/TEXTEQU directive
* - SubStrDef    handle SUBSTR directive
* - SizeStrDef   handle SIZESTR directive
* - InStrDef     handle INSTR directive
* - PurgeMacro   handle PURGE directive
* - MacroInit    global macro initialization, set predefined macros
*
****************************************************************************/


#define  PLACEHOLDER_SIZE 3      /* for #dd - number sign, digit, digit */

#include "globals.h"
#include <ctype.h>

#include "memalloc.h"
#include "parser.h"
#include "expreval.h"
#include "equate.h"
#include "directiv.h"
#include "input.h"
#include "tokenize.h"
#include "fatal.h"
#include "macro.h"
#include "condasm.h"
#include "fastpass.h"
#include "listing.h"

#include "myassert.h"

#define  MAX_TEXTMACRO_NESTING  16

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

//extern bool GetQueueMacroHidden( void );

extern bool expansion;
extern int CurrIfState;
extern bool EndDirectiveFound;

static int MacroLocals; // counter for LOCAL names
int MacroLevel; /* macro nesting level */
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

static char *replace_parm( char *start, char len, asmlines *lstruct, int parmcnt, mparm_list *parms , mlocal_list *locals )
/*************************************************************************************/
{
    /* search through parm list for word pointed at by start,
     * if you find it, set up the line string
     * this is similar to a printf format string
     * the placeholders are of the form #dd ( #, digit, digit )
     * this allows up to 100 parameters + locals
     * fixme - this max. should be documented.
     */
    char            *label;
    char            *new_line;
    char            *old_line;
    unsigned int    before;             // length of text before placeholder
    unsigned int    count = 0;

//  DebugMsg(("replace_parm enter for %s, line=%s\n", start, lstruct->line));

    old_line = lstruct->line;
    for( ; parmcnt || locals; ) {
        if (parmcnt) {
            label = parms->label;
            parms++;
            parmcnt--;
        } else {
            label = locals->label;
            locals = locals->next;
        }
        if( ( label != NULL ) && ( strlen( label ) == len ) &&
            ( strncmp( start, label, len ) == 0 ) ) {

            /* hey! this word IS a macro parameter/local! */

            if ( count > 99 ) {
                AsmError( TOO_MANY_MACRO_PLACEHOLDERS );
                break;
            }

            before = start - old_line;

            if (before && *(start-1) == '&' )
                before--;

            /* alloc a new line if size of line will increase */
            if (len < PLACEHOLDER_SIZE) {
                new_line = AsmAlloc( strlen(old_line) - len + PLACEHOLDER_SIZE + 1 );
                memcpy( new_line, old_line, before );
            } else
                new_line = old_line;

            *(new_line + before) = '#';
            before++;
            *(new_line + before) = '0' + count / 10;
            before++;
            *(new_line + before) = '0' + count % 10;
            before++;

            /* skip a '&' located immediately following the placeholder */
            if( *(start+len) == '&' ) start++;

            strcpy( new_line+before , start+len );
            lstruct->parmcount++;

            if (len < PLACEHOLDER_SIZE) {
                lstruct->line = new_line;
                AsmFree( old_line );
            }

            return( new_line + before );  /* ptr to char after #dd */
        }
        count++;
    }
    return( start+len );
}

static void put_parm_placeholders_in_line( asmlines *linestruct, int parmcnt, mparm_list *parms, mlocal_list *locals )
/*********************************************************************************/
{
    char *tmp;
    char *start;
    bool quote = FALSE;
    int brlevel = 0;
    int qlevel;

    /* scan across the string for space, &, " - to start a word */

    for( tmp = linestruct->line; *tmp != NULLC; ) {
        if( quote == FALSE && is_valid_id_char( *tmp ) ) {
            start = tmp++;
            while ( is_valid_id_char( *tmp )) tmp++;
            /* look for this word in the macro parms, and replace it if it is */
            /* this would change line - it will have to be reallocated */
            tmp = replace_parm( start, tmp - start, linestruct, parmcnt, parms, locals);
        } else {
            /* skip !", !<, !> sequences */
            if( quote == FALSE && *tmp == '!' )
                tmp++;
            else if( *tmp == '<' )
                brlevel++;
            else if( *tmp == '>' )
                if (brlevel) {
                    if (qlevel == brlevel)
                        quote = FALSE;
                    brlevel--;
                }
            else if( *tmp == '"' ) {
                quote = quote ^ 1; /* toggle the quote flag */
                qlevel = brlevel;
            }
            tmp++;
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

// fill a macro's parameter, local and content list.
// i = start index of macro params

ret_code FillMacro( dir_node * macro, int i, bool store_data )
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
        int j;

        if ( i < Token_Count) {
            for ( j = i, info->parmcnt = 1; j < Token_Count; j++)
                if (AsmBuffer[j]->token == T_COMMA)
                    info->parmcnt++;
            info->parmlist = AsmAlloc( info->parmcnt * sizeof(mparm_list));
        } else {
            info->parmcnt = 0;
            info->parmlist = NULL;
        }

        for( paranode = info->parmlist ; i < Token_Count ; paranode++ ) {

            token = AsmBuffer[i]->string_ptr;
            /* Masm accepts reserved words and instructions as parameter
             names! So just check that the token is a valid id.
             */
            if ( !is_valid_id_char( *token ) ) {
                AsmErr( SYNTAX_ERROR_EX, token );
                break;
            }
            paranode->def = NULL;
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
                    paranode->def = AsmAlloc( AsmBuffer[i]->value + 1 );
                    strcpy( paranode->def, AsmBuffer[i]->string_ptr );
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
                } else {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                    break;
                }
            }
            DebugMsg(("FillMacro(%s): param=>%s< found\n", macro->sym.name, paranode->label));
            if( i< Token_Count && AsmBuffer[i]->token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                break; // return( ERROR );
            }
            /* go past comma */
            i++;

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
            while (is_valid_id_char(*ptr) || *string == '&') ptr++;
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
                DebugMsg(("FillMacro(%s, %u): local=>%s< added\n", macro->sym.name, nesting_depth, localnode->label));
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
            DebugMsg(("FillMacro(%s, %u): endm found\n", macro->sym.name, nesting_depth ));
            if( nesting_depth ) {
                nesting_depth--;
            } else {
                break; /* exit the for() loop */
            }
        }
        ptr = string;
        if (*ptr == '%') {
            ptr++;
            while(isspace(*ptr)) ptr++;
        }

        locals_done = TRUE;

        /* Skip a possible label. This isn't a trivial task, because
         the source might contain a & operator or a macro function call.
         */
        if (is_valid_id_char(*ptr )) {
            ptr2 = ptr+1;
            while( is_valid_id_char(*ptr2 ) || *ptr2 == '&' ) ptr2++;
            if (*ptr2 == ':') {
                ptr2++;
                if (*ptr2 == ':')
                    ptr2++;
                while (isspace(*ptr2)) ptr2++;
                ptr = ptr2;
            } else {
                while (isspace(*ptr2)) ptr2++;
                /* skip possible macro function parameter block enclosed
                 in '()' */
                if ( *ptr2 == '(' ) {
                    int brlevel = 1;
                    ptr2++;
                    for (;brlevel && *ptr2;ptr2++) {
                        if (*ptr2 == '(')
                            brlevel++;
                        else if (*ptr2 == ')')
                            brlevel--;
                    }
                    while (isspace(*ptr2)) ptr2++;
                }
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
            // DebugMsg(("FillMacro(%s): store line >%s<\n", macro->sym.name, string));
            /* make info->data point at the LAST line in the struct */
            put_parm_placeholders_in_line( linestruct, info->parmcnt, info->parmlist, info->locallist );
            DebugMsg(("FillMacro(%s, %u): store line >%s<, parmcnt=%u\n", macro->sym.name, nesting_depth, linestruct->line, linestruct->parmcount ));
        }
    }
    macro->sym.defined = TRUE;
    DebugMsg(("FillMacro(%s): exit, no error, isfunc=%u\n", macro->sym.name, macro->sym.isfunc));
    return( NOT_ERROR );
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
            DebugMsg(("my_sprintf, error: parmno=%u, argc=%u, format=%s\n", parmno, argc, format));
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

static void AddBrackets(char *dest, char *src)
{
    *dest++ = '<';
    while (*src) {
        if (*src == '<' || *src == '>' || *src == '!')
            *dest++ = '!';
        *dest++ = *src++;
    }
    *dest++ = '>';
    *dest = NULLC;
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
    int         parmidx;
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

    DebugMsg(("RunMacro(%s) enter, src=>%s<, run=%u, insert=%u [local var cnt=%04u]\n", macro->sym.name, params, runit, insert, MacroLocals));

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

    for( localcnt = 0, local= info->locallist; local != NULL; local = local->next ) {
        localcnt++;
    }
    parm_array = AsmTmpAlloc( (info->parmcnt + localcnt) * sizeof( char * ) );

    /* skip white spaces (in case there are no params at all) */
    while (isspace(*params)) params++;

    for( parmidx = 0, parm = info->parmlist; parmidx < info->parmcnt; parmidx++, parm++ ) {

        buffer[0]='\0';
        expansion_flag = FALSE;

        while (isspace(*params)) params++;

        if (macro->sym.isfunc && *params == ')')
            end_reached = TRUE;

        if ((*params == '\0') || ( end_reached == TRUE) || ( *params == ',') ) {

            /* it's a blank parm */

            if( parm->required ) {
                DebugMsg(( "RunMacro(%s.%s), parameter required, >%s<\n", macro->sym.name, parm->label, params ));
                AsmErr( REQUIRED_PARAMETER_MISSING, macro->sym.name, parm->label );
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

            DebugMsg(( "RunMacro(%s.%s), >%s<\n", macro->sym.name, parm->label, params ));

            /* for VARARG parameter, ignore comma delimiter */
            if ((parmidx == info->parmcnt - 1) && (macro->sym.vararg)) {
                delim = '\0';
            }

#if 1
            if (*params == '<') {
                str_level++;
                params++;
                // don't skip leading spaces if <> delimiter are used,
                // the parameter might be just a space (< >), nothing else!
                // while (isspace(*params)) params++;
            }
#endif
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
                    /* the evaluator can't handle a terminating '>',
                     so end the token chain if a string occurs */
                    for (i = 0; i < Token_Count; i++) {
                        if (AsmBuffer[i]->token == T_STRING) {
                            AsmBuffer[i]->token = T_FINAL;
                            Token_Count = i;
                            break;
                        }
                    }
                }
            }
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

                if (expansion_flag == TRUE && string_expansion == FALSE ) {
                    i = 0;
                    DebugMsg(( "RunMacro(%s.%s), expansion=1\n", macro->sym.name, parm->label ));
                    if (AsmBuffer[0]->token == T_ID) {
                        dir = (dir_node *)SymSearch(AsmBuffer[0]->string_ptr);
                        /* if it's a macro, just continue, will be evaluated below */
                        if (dir && dir->sym.state == SYM_MACRO) {
                            DebugMsg(( "RunMacro(%s.%s), expansion=1, macro found\n", macro->sym.name, parm->label ));
                            expansion_flag = FALSE;
                            goto skip_expansion;
                        }
                        if (dir && dir->sym.state == SYM_TMACRO && dir->sym.defined == TRUE) {
                            DebugMsg(( "RunMacro(%s.%s), expansion=1, text macro found\n", macro->sym.name, parm->label ));
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
#if 0
                            /* a plain copy is an error, since it might double '!' */
                            strcpy(ptr, dir->sym.string_ptr);
                            ptr = ptr + strlen(ptr);
#else
                            ptr += GetTextMacroValue( dir->sym.string_ptr, ptr );
#endif
                            params = AsmBuffer[1]->pos;
                            expansion_flag = FALSE;
                            continue;
                        }
                    }
                    if (EvalOperand(&i, Token_Count, &opndx, TRUE) == ERROR) {
                        Token_Count = 0;
                        return(ERROR);
                    }
                    DebugMsg(( "RunMacro(%s.%s): expansion, opndx.type=%d, value=%u\n", macro->sym.name, parm->label, opndx.type, opndx.value));
                    /* the expression evaluator accepts forward references
                     but the % operator won't accept them */
                    if (opndx.type != EXPR_CONST || opndx.string != NULL) {
                        if (opndx.sym && opndx.sym->state == SYM_UNDEFINED) {
                            AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
                        } else {
                            AsmError( EXPRESSION_NOT_A_CONSTANT );
                        }
                    } else {
                        // sprintf( ptr, "%d", opndx.value);
                        _ltoa( opndx.value, ptr, ModuleInfo.radix );
                        ptr = ptr + strlen(ptr);
                    }
                    params = AsmBuffer[i]->pos;
                    expansion_flag = FALSE;
                    continue;
                }
            skip_expansion:

                if (bracket_level > 0) {
                    if (*params == '(') {
                        bracket_level++;
                    } else if (*params == ')') {
                        bracket_level--;
                        if (bracket_level == 0)
                            break;
                    }
                }

                *ptr++ = *params++;

                /* check for (text) macros */

                if (str_level == 0 &&
                    is_valid_id_char(*params) == FALSE &&
                    startitem != NULL) {

                    *ptr = '\0';
                    dir = (dir_node *)SymSearch(startitem);
                    if (dir &&
                        dir->sym.state == SYM_MACRO &&
                        dir->sym.defined == TRUE &&
                        dir->sym.isfunc == TRUE) {
                        char * p = params;
                        while ( isspace(*p) ) p++;
                        /* no macro function invokation if the '(' is missing! */
                        if (*p == '(') {
                            params -= strlen(startitem);
                            ptr -= strlen(startitem);
                            *ptr = '\0';
                            line[0] = '\0';
                            i = RunMacro(dir, params, line, TRUE, TRUE, FALSE);
                            DebugMsg(("RunMacro(%s.%s): back from RunMacro(%s), rc=%u, buffer=>%s<\n", macro->sym.name, parm->label, dir->sym.name, i, line));
                            if ( i == ERROR )
                                return( ERROR );
                            strcpy(ptr, line);
                            ptr += strlen(line);
                            params = params + i;
                            startitem = NULL;
                            continue;
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
        if (buffer[0]) {
            parm_array[parmidx] = AsmTmpAlloc(strlen(buffer)+1);
            strcpy( parm_array[parmidx], buffer );
        } else
            parm_array[parmidx] = NULL;

        DebugMsg(("RunMacro(%s.%s): actual parameter value=>%s<\n", macro->sym.name, parm->label, buffer));

        if (*params == ',') {
            params++;
        }
    } /* end for  */

    if (bracket_level >= 0) {
        if (*params == '\0') {
            DebugMsg(("RunMacro(%s): missing ')'\n", macro->sym.name));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        } else if (*params != ')') {
            DebugMsg(("RunMacro(%s): expected ')', found >%s<\n", macro->sym.name, params));
            AsmErr( TOO_MANY_ARGUMENTS_IN_MACRO_CALL, macro->sym.name);
            return( ERROR );
        } else
            params++;

        /* if macro name is "", it's a FOR/FORC macro.
         a check for a valid end must NOT be done then. */
    } else if (*params != '\0' && *macro->sym.name != NULLC) {
        DebugMsg(("RunMacro(%s): expected NULL, found >%s<\n", macro->sym.name, params));
        AsmErr( TOO_MANY_ARGUMENTS_IN_MACRO_CALL, macro->sym.name);
        return( ERROR );
    }

    /* set the number of format params for my_sprinf() call */

    count = info->parmcnt;

#define SIZELOCALNAME 8 /* max size of local label name */

    if (localcnt) {
        local_array = AsmTmpAlloc( localcnt * SIZELOCALNAME );
        for( localcnt = 0, local = info->locallist; local != NULL; local = local->next, localcnt++ ) {
            parm_array[count] = local_array + localcnt * SIZELOCALNAME;
            sprintf(parm_array[count],"??%04X", MacroLocals);
            DebugMsg(("RunMacro(%s): local %s replaced by %s, name index=%u\n", macro->sym.name, local->label, parm_array[count], count));
            MacroLocals++;
            count++;
        }
    }

    /* a predefined macro func with a function address? */

    if (macro->sym.predefined == TRUE && macro->sym.func_ptr != NULL) {
        line[0] = NULLC;
        macro->sym.func_ptr(line, parm_array);
        if (addbrackets) {
            AddBrackets(prefix+strlen(prefix), line);
        } else
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
        // bugfix: lines which contain macro labels must be emitted as well.
        // The macro can contain local macros (FOR, REPEAT, WHILE constructs
        // ARE local macros), so the labels are needed!
        // if (*(lnode->line) != ':') {
        my_sprintf( line, lnode->line, count-1, parm_array );
        InputQueueLine( line );
        //}
        // DebugMsg(("macro line: >%s<\n", lnode->line));
    }
    if (runit || macro->sym.isfunc)
        InputQueueLine( "endm" );

#if 0
    if (*(macro->sym.name) && runit == FALSE) {
        /* put this macro into the file stack */
        PushMacro( (asm_sym *)macro, FALSE );
    }
#endif

    /* now free the parm replace strings */
//    free_parmlist( info->parmlist );

    if (runit || macro->sym.isfunc) {
        DebugMsg(("RunMacro(%s): enter assembly loop\n", macro->sym.name ));
        /* run the assembler until we hit EXITM, GOTO or ENDM */
        MacroLevel++;
        /* move the current line queue to the file stack */
        PushMacro( (asm_sym *)macro, *(macro->sym.name) == NULLC );
        for(;;) {
            int i;
            while (0 == (i = AsmLine( buffer )));
            if (i < 0) {
                MacroLevel--;
                return(ERROR);
            }
            /* skip macro label lines */
            if (AsmBuffer[0]->token == T_COLON)
                continue;

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
                        DebugMsg(("RunMacro(%s): GOTO, scan line >%s< for label >%s<\n", macro->sym.name, ptr, line));
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
                        DebugMsg(("RunMacro(%s): GOTO, label >%s< not found!\n", macro->sym.name, line));
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
        } /* end for */
    }

    DebugMsg(("RunMacro(%s) exit\n", macro->sym.name));

    return( params - orgsrc );
}

/*
 ExpandText() is called if
 - the evaluation operator '%' has been found as first char of the line.
 - for CATSTR, SUBSTR, SIZESTR (and INSTR?) parameters
 Then do expansion within strings!
 if substitute is TRUE, scanning for the substitution character is active!
 Both text macros and macro functions are expanded!
 */

static int ExpandText(char * line, bool substitute)
{
    char *pSrc;
    char *pDst;
    char *pIdent;
    char *pStart;
    unsigned char *pSave = NULL;
    int rc = NOT_ERROR;
    int count;
    bool expanded = TRUE;
    dir_node * dir;
    char srcline[MAX_LINE_LEN];

    DebugMsg(("ExpandText(line=>%s<, subst=%u) enter\n", line, substitute));
    for ( count = 0; count < MAX_TEXTMACRO_NESTING && expanded; count++ ) {
        strcpy(srcline, line);
        expanded = FALSE;
        pDst = line;
        for ( pSrc = srcline; *pSrc ; ) {
            if( is_valid_id_char( *pSrc ) ) {
                pIdent = pDst;
                do {
                    *pDst++ = *pSrc++;
                } while ( is_valid_id_char( *pSrc ));
                *pDst = NULLC;
                dir = (dir_node *)SymSearch( pIdent );
#ifdef DEBUG_OUT
                if (dir && (dir->sym.state == SYM_TMACRO || dir->sym.state == SYM_MACRO )) {
                    DebugMsg(( "ExpandText: symbol found: %s, state=%u, defined=%u, *pDst-1=%c\n", dir->sym.name, dir->sym.state, dir->sym.defined, *(pDst-1) ));
                }
#endif
                if ( dir &&
                     dir->sym.state == SYM_TMACRO &&
                     dir->sym.defined == TRUE) {
#if 1
                    /* if there's an evaluation operator (%) before the
                     text macro name, skip it. This is to be improved!
                     */
                    if (pIdent > line && *(pIdent-1) == '%')
                        pIdent--;
#endif
                    if (substitute) {
                        if (*(pIdent-1) == '&')
                            pIdent--;
                        if (*pSrc == '&')
                            pSrc++;
                    }
                    DebugMsg(("ExpandText(): %s is to be replaced by >%s<\n", pIdent, dir->sym.string_ptr));
                    strcpy( pIdent, dir->sym.string_ptr);
                    pDst = pIdent + strlen(pIdent);
                    rc = STRING_EXPANDED;
                    expanded = TRUE;
                    continue;
                }
#if 1
                /* expand macro functions. The token buffer has to be
                 saved/restored if RunMacro() is called */
                if ( dir &&
                     dir->sym.state == SYM_MACRO &&
                     dir->sym.defined == TRUE &&
                     dir->sym.isfunc == TRUE) {
                    char * p = pSrc;
                    int i;
                    while ( isspace(*p) ) p++;
                    /* no macro function invokation if the '(' is missing! */
                    if (*p == '(') {
                        pSrc -= strlen(pIdent);
                        *pIdent = NULLC;
                        if ( pSave == NULL )
                            pSave = AsmTmpAlloc( GetTokenStateSize() );
                        SaveTokenState( pSave );
                        i = RunMacro( dir, pSrc, pIdent, TRUE, TRUE, FALSE );
                        DebugMsg(( "ExpandText: back from RunMacro(%s), rc=%u, text returned=>%s<\n", dir->sym.name, i, pIdent ));
                        RestoreTokenState( pSave );
                        if ( i == ERROR )
                            return( ERROR );
                        pSrc += i;
                        pDst = pIdent + strlen(pIdent);
                        rc = STRING_EXPANDED;
                        expanded = TRUE;
                        continue;
                    }
                }
#endif
            } else {
                *pDst++ = *pSrc++;
            }
        } /* end for */
        *pDst = NULLC;
    } /* end for */
    if (count == MAX_TEXTMACRO_NESTING) {
        AsmError(NESTING_LEVEL_TOO_DEEP);
        return(ERROR);
    }
    DebugMsg(("ExpandText(line=>%s<) exit\n", line));
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
    if ((len + newlen + 1) > sizeof(buffer)) {
        Fatal( MSG_EXPANDED_LINE_TOO_LONG, string );
    }
    if (addbrackets) {
#ifdef DEBUG_OUT
        /* check buffer overflow */
        if ( (len + newlen + 2 + strlen(src+oldlen) + 1) > MAX_LINE_LEN ) {
            _asm int 3;
        }
#endif
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
#ifdef DEBUG_OUT
        /* check buffer overflow */
        if ( (len + newlen + strlen(src+oldlen) + 1) > MAX_LINE_LEN ) {
            _asm int 3;
        }
#endif
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

    DebugMsg(("ExpandTextMacro(pos=%u, addbr=%u) enter [tokens=%u]\n", pos, addbrackets, Token_Count));

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
// count: index of token in AsmBuffer
// string:

int ExpandToken(int count, char * string, bool addbrackets, bool Equ_Mode)
{
    int pos;
    expr_list opndx;
    dir_node    *dir;
    int rc = NOT_ERROR;

    if (AsmBuffer[count]->token == T_PERCENT && Equ_Mode == FALSE) {
        pos = count+1;
#if 1
        /* testcase:
         E1  EQU     @SizeStr("ab")
         TE1 TEXTEQU %E1
             %ECHO   TE1
         must display 4. The '%' is superfluous.
         */
        if (AsmBuffer[pos]->token == T_ID)
            if ((dir = (dir_node *)SymSearch(AsmBuffer[pos]->string_ptr)) &&
                dir->sym.state == SYM_TMACRO) {
                ExpandTextMacro(dir, pos, string, FALSE);
                Token_Count = 0; /* skip processing */
                return( STRING_EXPANDED );
            }
#endif
        if (EvalOperand( &pos, Token_Count, &opndx, TRUE ) == ERROR) {
            Token_Count = 0; /* ensure this line isn't processed further */
            return( ERROR );
        }
        if ( opndx.type == EXPR_CONST && opndx.string == NULL ) {
            char * p;
            //sprintf( StringBufferEnd, "%u", opndx.value );
            _ltoa( opndx.value, StringBufferEnd, ModuleInfo.radix );
            p = AsmBuffer[count]->pos;
            AsmBuffer[count]->token = T_STRING;
            AsmBuffer[count]->string_ptr = StringBufferEnd;
            AddTokens( count+1, count+1 - pos );
            for ( pos = count+1; AsmBuffer[pos]->token == T_NUM; pos++ );
            if (AsmBuffer[pos]->token == T_FINAL)
                pos = strlen(p);
            else
                pos = AsmBuffer[pos]->pos - p;
            RebuildLine( StringBufferEnd, count, string, pos, addbrackets);
            return(STRING_EXPANDED);
        } else {
            if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED ) {
                AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
            } else
                AsmError( SYNTAX_ERROR );
            Token_Count = 0; /* ensure this line isn't processed further */
            return( ERROR );
        }
#if 0
        /* this is to be done for macro params only! */
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
            if (EvalOperand(&pos, Token_Count, &opndx, TRUE) == ERROR) {
                Token_Count = 0;
                return(ERROR);
            }
            //sprintf( StringBufferEnd, "%u", opndx.value );
            _ltoa( opndx.value, StringBufferEnd, ModuleInfo.radix );
            RebuildLine( StringBufferEnd, count, string, AsmBuffer[count]->value, FALSE );
            return(STRING_EXPANDED);
        }
#endif
    }
    if( AsmBuffer[count]->token == T_ID ||
        (expansion == TRUE && AsmBuffer[count]->token == T_STRING))  {
        dir = (dir_node *)SymSearch( AsmBuffer[count]->string_ptr );
        if( dir != NULL) {
            if (dir->sym.state == SYM_MACRO &&
                dir->sym.defined == TRUE &&
                Equ_Mode == FALSE ) {
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
                    bool runit = dir->sym.runsync || MacroLevel > 0;
                    if (count > 0 && AsmBuffer[count-1]->token != T_COLON) {
                        DebugMsg(("macro called without brackets at operand location\n"));
                        AsmError(SYNTAX_ERROR);
                        Token_Count = 0;
                        return(ERROR);
                    }
                    /* is runit=FALSE possible at all? */
                    rc = RunMacro(dir, AsmBuffer[count]->pos, buffer, runit, TRUE, FALSE);
                    DebugMsg(("ExpandToken: back from RunMacro(%s) - called as Proc\n", dir->sym.name));
                    if ( rc != ERROR ) {
                        if ( AsmBuffer[0]->value == T_EXITM ) {
                            strcat(buffer, cmd+rc);
                            strcpy(string, buffer);
                            rc = STRING_EXPANDED;
                        } else {
                            /* if the macro didn't run yet, push the current
                             line queue - which contains the macro lines -
                             into the file stack */
                            if ( runit == FALSE )
                                PushMacro( (asm_sym *)dir, FALSE );
                            rc = NOT_ERROR;
                        }
                    }
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

// SIZESTR and INSTR subroutine

int FullExpandToken( int i, char *string)
{
    char buffer[MAX_LINE_LEN];

    if (AsmBuffer[i]->token == T_ID) {
        strcpy(buffer, AsmBuffer[i]->string_ptr);
        if (ExpandText(buffer, FALSE) == STRING_EXPANDED) {
            RebuildLine(buffer, i, string,
                        strlen(AsmBuffer[i]->string_ptr), TRUE);
            AsmBuffer[i]->token = T_STRING;
            return( STRING_EXPANDED );
        }
    }
    return( NOT_ERROR );
}

// scan current line for (text) macros and expand them.
// [this is to be changed. Currently it runs AFTER Tokenize(),
// but it is to run BEFORE Tokenize()].

ret_code ExpandMacro( char * string)
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
#if 1
            /* FullExpandToken won't expand macro functions, which
             makes a slight difference compared to Masm */
            for ( i = 2; i < Token_Count; i++) {
                if ( AsmBuffer[i]->token == T_PERCENT && AsmBuffer[i+1]->token == T_ID)
                    if ( ExpandToken( 2, string, TRUE, FALSE) == STRING_EXPANDED )
                        return( STRING_EXPANDED );
                if ( FullExpandToken( i, string ) == STRING_EXPANDED )
                    return( STRING_EXPANDED );
            }
#endif
            count = 2;
            addbrackets = TRUE;
            break;
        case T_SIZESTR:
            if (expansion == TRUE)
                break;
            if ( FullExpandToken( 2, string ) == STRING_EXPANDED)
                return( STRING_EXPANDED );
            count = 2;
            addbrackets = TRUE;
            break;
        case T_SUBSTR:
            rc = ExpandToken( 2, string, TRUE, FALSE);
            if (rc == STRING_EXPANDED) {
                AsmBuffer[2]->token = T_STRING;
            }
            count = 3;
            goto std_expansion;
        case T_INSTR:
            /* check if there's the optional first pos parameter */
            for (i = 2, count = 0, rc = 0;i < Token_Count; i++)
                if (AsmBuffer[i]->token == T_COMMA) {
                    if (rc == 0)
                        rc = i+1;
                    count++;
                }
            if (count < 2)
                count = 2;
            else {
                count = rc;
                rc = ExpandToken( 2, string, FALSE, FALSE);
            }
            if ( FullExpandToken(count, string) == STRING_EXPANDED)
                return( STRING_EXPANDED );
            addbrackets = TRUE;
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
            /* EQU is a special case. If the - expanded - expression is
             a number, then the value for EQU is numeric. Else the
             expression isn't expanded at all. This effectively makes it
             impossible to expand EQU lines here.
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
        if (ExpandToken(count, string, addbrackets, FALSE ) == STRING_EXPANDED)
            rc = STRING_EXPANDED;
    }
    return( rc );
}

// create a macro symbol

dir_node *CreateMacro( char *name )
{
    dir_node *macro;
    if (macro = (dir_node *)SymCreate( name, *name != NULLC )) {
        macro->sym.state = SYM_MACRO;
        macro->e.macroinfo = AsmAlloc( sizeof( macro_info ) );
        macro->e.macroinfo->parmlist = NULL;
        macro->e.macroinfo->locallist = NULL;
        macro->e.macroinfo->data = NULL;
        macro->e.macroinfo->srcfile = NULL;
        macro->sym.vararg = FALSE;
        macro->sym.isfunc = FALSE;
    }
    return( macro );
}

void ReleaseMacroData( dir_node *macro )
{
    int             i;
    mlocal_list     *localcurr;
    mlocal_list     *localnext;
    asmlines        *datacurr;
    asmlines        *datanext;

    /* free the parm list */
    for( i = 0 ; i < macro->e.macroinfo->parmcnt; i++ ) {
        AsmFree( macro->e.macroinfo->parmlist[i].label );
        AsmFree( macro->e.macroinfo->parmlist[i].def );
    }

    if( macro->e.macroinfo->parmlist ) {
        AsmFree( macro->e.macroinfo->parmlist );
        macro->e.macroinfo->parmcnt = 0;
        macro->e.macroinfo->parmlist = NULL;
    }

    /* free the local list */
    for( localcurr = macro->e.macroinfo->locallist ;localcurr; ) {
        localnext = localcurr->next;
        AsmFree( localcurr->label );
        AsmFree( localcurr );
        localcurr = localnext;
    }
    macro->e.macroinfo->locallist = NULL;

    /* free the lines list */
    for(datacurr = macro->e.macroinfo->data ;datacurr; ) {
        datanext = datacurr->next;
        AsmFree( datacurr->line );
        AsmFree( datacurr );
        datacurr = datanext;
    }
    macro->e.macroinfo->data = NULL;
    macro->e.macroinfo->srcfile = NULL;
    macro->sym.vararg = FALSE;
    macro->sym.isfunc = FALSE;
    return;
}

// MACRO directive: define a macro

ret_code MacroDef( int i)
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
    DebugMsg(("MacroDef(%s)\n", name));
    macro = (dir_node *)SymSearch( name );
    if( macro == NULL ) {
        macro = CreateMacro( name );
        macro->e.macroinfo->srcfile = get_curr_srcfile();
    } else if( macro->sym.state != SYM_MACRO ) {
        if( Parse_Pass == PASS_1 )
            AsmErr( SYMBOL_ALREADY_DEFINED, name );
        return( ERROR );
    }

    if (( Parse_Pass == PASS_1) || (macro->sym.variable)) {
        /* is the macro redefined? */
        if ( macro->e.macroinfo->data != NULL) {
            DebugMsg(("MacroDef(%s): macro already defined\n", name));
            ReleaseMacroData( macro );
            macro->sym.variable = TRUE;
        }
        store_data = TRUE;
    } else
        store_data = FALSE;

    return( FillMacro( macro, i+2, store_data ) );
}

/* get text macro value */
/* returns characters copied into buffer (without terminating 00) */

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
asm_sym * SetTextMacro(asm_sym *sym, char *name, char *value )
{
    int count;
    char buffer[MAX_LINE_LEN];

#if FASTPASS
    /* there's no need to set the value if FASTPASS is on, because
     the input are just preprocessed lines.
     */
    if (Parse_Pass != PASS_1)
        return(sym);
#endif

    if (sym == NULL)
        sym = SymCreate( name, TRUE );

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;

    if ( AsmBuffer[2]->token == T_STRING && AsmBuffer[2]->string_delim == '<' && AsmBuffer[3]->token == T_FINAL ) {
        value = AsmBuffer[2]->string_ptr;
        count = AsmBuffer[2]->value;
    } else {
        char *p = buffer;
        /*
         the original source is used, since the tokenizer has
         deleted some information. it's important to double '!' found inside
         the string.
         */
        while (isspace(*value)) value++;
        count = strlen(value);
        if (count) {
            for (;count;count--)
                if (isspace(*(value+count-1)) == FALSE)
                    break;
        }
        for (;count;count--) {
            if ( *value == '!' || *value == '<' || *value == '>' )
                *p++ = '!';
            *p++ = *value++;
        }
        *p = NULLC;
        value = buffer;
        count = p - buffer;
    }
    if (sym->string_ptr)
        AsmFree(sym->string_ptr);
    sym->string_ptr = (char *)AsmAlloc( count + 1);
    memcpy(sym->string_ptr, value, count);
    *(sym->string_ptr+count) = NULLC;

    DebugMsg(("SetTextMacro(%s): value is >%s<, exit\n", sym->name, sym->string_ptr ));
    return( sym );
}

// CatStr()
// defines a text equate
// syntax <name> CATSTR [<string>,...]
// TEXTEQU is an alias for CatStr()

ret_code CatStrDef( int start, asm_sym * *psym )
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
        if (AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<') {
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

    LstWriteFile(LSTTYPE_EQUATE, 0, sym);

    return( NOT_ERROR );
}

// SubStr()
// defines a text equate
// syntax: name SUBSTR <string>, pos [, size]

ret_code SubStrDef( int i, char * string)
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

    if (AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<') {
        DebugMsg(("SubStrDef: error, no text item\n"));
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

    // get pos, must be a numeric value and > 0

    if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR) {
        DebugMsg(("SubStrDef(%s): invalid pos value\n", name));
        return(ERROR);
    }

    if (opndx.type != EXPR_CONST || opndx.string != NULL) {
        DebugMsg(("SubStrDef(%s): pos value is not a constant\n", name));
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }

    /* pos is expected to be 1-based */
    pos = opndx.value;
    if (pos <= 0) {
        AsmError( POSITIVE_VALUE_EXPECTED );
        return( ERROR );
    }
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
        if (size < 0) {
            AsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
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

    cnt = pos;
    for ( pos--; pos > 0 && *p ; pos--, p++)
        if (*p == '!' && *(p+1) != NULLC)
            p++;

    if (*p == NULLC) {
        AsmErr( INDEX_PAST_END_OF_STRING, cnt );
        return( ERROR );
    }

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

    LstWriteFile(LSTTYPE_EQUATE, 0, sym);

    return( NOT_ERROR );
}

// SizeStr()
// defines a numeric variable which contains size of a string

ret_code SizeStrDef( int i )
{
    asm_sym *sym;
    int sizestr;
    char buffer[MAX_LINE_LEN];

    DebugMsg(("SizeStrDef entry\n"));

    if (i != 1 || AsmBuffer[0]->token != T_ID || Token_Count != 3 ) {
        DebugMsg(("SizeStrDef: syntax error, i=%u, Token_Count=%u\n", i, Token_Count));
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if ( AsmBuffer[2]->token != T_STRING || AsmBuffer[2]->string_delim != '<') {
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }

    sizestr = GetTextMacroValue(AsmBuffer[2]->string_ptr, buffer);

    if (sym = CreateConstant( AsmBuffer[0]->string_ptr, sizestr, -1, TRUE)) {
        DebugMsg(("SizeStrDef(%s) exit, value=%u\n", AsmBuffer[0]->string_ptr, sizestr));
        LstWriteFile(LSTTYPE_EQUATE, 0, sym);
        return(NOT_ERROR);
    } else
        return(ERROR);

}

// InStr()
// defines a numeric variable which contains position of substring
// syntax
// name INSTR [pos,]string,substr

ret_code InStrDef( int i, char * string )
{
    asm_sym *sym;
    int sizestr;
    int j;
    int commas;
    char *string1;
    char buffer1[MAX_LINE_LEN];
    char buffer2[MAX_LINE_LEN];
    expr_list opndx;
    int start = 1;
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
        if (start <= 0)
            start = 1;
        if (AsmBuffer[i]->token != T_COMMA) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        i++; /* skip comma */
    }

    if (AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<') {
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    sizestr = GetTextMacroValue(AsmBuffer[i]->string_ptr, buffer1);
#ifdef DEBUG_OUT
    DebugMsg(("InStrDef: first string >%s< \n", buffer1));
#endif
    if (start > sizestr) {
        AsmErr( INDEX_PAST_END_OF_STRING, start );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_COMMA) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    i++;

    if (AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<') {
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
    if ((sizestr >= j) && (string1 = strstr(buffer1+start-1, buffer2)))
        strpos = string1 - buffer1 + 1;

    if (sym = CreateConstant( AsmBuffer[0]->string_ptr,strpos, -1, TRUE)) {
        DebugMsg(("InStrDef(%s) exit, value=%u\n", AsmBuffer[0]->string_ptr, strpos));
        LstWriteFile(LSTTYPE_EQUATE, 0, sym);
        return (NOT_ERROR);
    }
    return(ERROR);
}

// PURGE implementation.
// this is not the way MASM does it, but I'm rather unsure
// whether PURGE works in MASM at all. At least I was unable
// to purge anything there.

ret_code PurgeMacro( int i)
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

static int EnvironFunc(char * buffer, char * *params)
{
    char * p = getenv( *params );
    if (p)
        strcpy(buffer, p);
    else
        buffer[0] = '\0';
    return(NOT_ERROR);
}

#define CATSTRMAX 20

// internal @CatStr macro function

static int CatStrFunc(char * buffer, char * *params)
{
    char **end = params + CATSTRMAX;

    DebugMsg(("CatStrFunc: params=%X, %X, %X\n", *(params+0), *(params+1), *(params+2)));

    for (; params != end; params++) {
        if (*params) {
            strcpy(buffer, *params);
            buffer += strlen(buffer);
        }
    }
    return(NOT_ERROR);
}

/* convert string to a number */

static int GetNumber(char * string, int * pi)
{
    expr_list opndx;
    int i;
    int last;

    last = Tokenize(string, Token_Count+1);
    i = Token_Count+1;
    if(EvalOperand(&i, last, &opndx, TRUE ) == ERROR) {
        return(ERROR);
    }
    if(opndx.type != EXPR_CONST || opndx.string != NULL || AsmBuffer[i]->token != T_FINAL) {
        AsmError( SYNTAX_ERROR );
        return(ERROR);
    }
    *pi = opndx.value;
    return( NOT_ERROR );
}

// internal @InStr macro function

static int InStrFunc(char * buffer, char * *params)
{
    int pos;
    char *p;
    int found;

    DebugMsg(("InStrFunc: params=%X, %X, %X\n", *(params+0), *(params+1), *(params+2)));

    /* init buffer with "0" */
    *buffer = '0';
    *(buffer+1) = NULLC;

    if ( *(params+0) ) {
        if (GetNumber( *(params+0), &pos ) == ERROR)
            return( ERROR );
        if (pos == 0)
            pos++;
    } else
        pos = 1;

    if (pos > strlen(*(params+1))) {
        AsmErr( INDEX_PAST_END_OF_STRING, pos );
        return( ERROR );
    } else {
        p = strstr(*(params+1)+pos-1, *(params+2));
        if (p)
            found = p - *(params+1) + 1;
        else
            found = 0;
    }

    sprintf(buffer, "%u", found );

    DebugMsg(( "InStrFunc returns >%s<\n", buffer ));

    return(NOT_ERROR);
}

// internal @SizeStr macro function

static int SizeStrFunc(char * buffer, char * *params)
{
    DebugMsg(("SizeStrFunc: param=%X\n", *params ));
    sprintf(buffer, "%u", strlen( *params ));
    return(NOT_ERROR);
}

// internal @SubStr macro function

static int SubStrFunc(char * buffer, char * *params)
{
    int pos;
    int size;
    char *src = *(params+0);

    DebugMsg(("SubStrFunc: params=%X, %X, %X\n", src, *(params+1), *(params+2)));

    if (GetNumber(*(params+1),&pos) == ERROR)
        return( ERROR );

    if (pos <= 0)
        pos = 1;

    size = strlen(src);
    if (pos > size) {
        AsmErr( INDEX_PAST_END_OF_STRING, pos );
        return( ERROR );
    }

    if ( *(params+2) ) {
        if (GetNumber(*(params+2), &size) == ERROR)
            return( ERROR );
        if (size < 0) {
            AsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
    } else {
        size = size - pos + 1;
    }

    for(src = src+pos-1 ; size && *src ; size--)
        *buffer++ = *src++;

    *buffer = NULLC;

    if (size) {
        AsmError( COUNT_VALUE_TOO_LARGE );
        return( ERROR );
    }

    return(NOT_ERROR);
}

#if 0
static char *predefs[] = {"@CatStr","@InStr","@SizeStr","@SubStr", NULL};
static char *precont[] = {
     "@CatStr MACRO s0,s1,s2,s3,s4,s5,s6,s7,s8,s9",
     "local result",
     "result CatStr <s0>,<s1>,<s2>,<s3>,<s4>,<s5>,<s6>,<s7>,<s8>,<s9>",
     "exitm result",
     "endm",

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
#endif

// generic parameter names. In case the parameter name is
// displayed in an error message ("required parameter %s missing")
static char * parmnames[] = {"p1","p2","p3"};

// macro initialization
// this proc is called once per pass

int MacroInit( int pass)
{

    int i;
    dir_node *macro;

    DebugMsg(( "MacroInit(%u)\n", pass ));
    MacroLevel = 0;
    MacroLocals = 0;
    SkipMacroMode = FALSE;

    if (pass == PASS_1) {
#if 0
        for (ptr = predefs; *ptr; ptr++) {
            macro = CreateMacro(*ptr );
            macro->sym.defined = TRUE;
            macro->sym.predefined = TRUE;
        }

        PushLineQueue();
        for (ptr = precont; *ptr; ptr++)
            InputQueueLine(*ptr);
#endif

        // add @CatStr() macro func

        macro = CreateMacro("@CatStr" );
        macro->sym.defined = TRUE;
        macro->sym.predefined = TRUE;
        macro->sym.func_ptr = CatStrFunc;
        macro->sym.isfunc = TRUE;
        macro->e.macroinfo->parmcnt = CATSTRMAX;
        macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list) * CATSTRMAX);
        for (i = 0; i < CATSTRMAX; i++) {
            macro->e.macroinfo->parmlist[i].def = NULL;
            macro->e.macroinfo->parmlist[i].label = "p";
            macro->e.macroinfo->parmlist[i].required = FALSE;
        }

        // add @InStr() macro func

        macro = CreateMacro("@InStr" );
        macro->sym.defined = TRUE;
        macro->sym.predefined = TRUE;
        macro->sym.func_ptr = InStrFunc;
        macro->sym.isfunc = TRUE;
        macro->e.macroinfo->parmcnt = 3;
        macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list) * 3);
        for (i = 0; i < 3; i++) {
            macro->e.macroinfo->parmlist[i].def = NULL;
            macro->e.macroinfo->parmlist[i].label = parmnames[i];
            macro->e.macroinfo->parmlist[i].required = (i != 0);
        }

        // add @SizeStr() macro func

        macro = CreateMacro("@SizeStr" );
        macro->sym.defined = TRUE;
        macro->sym.predefined = TRUE;
        macro->sym.func_ptr = SizeStrFunc;
        macro->sym.isfunc = TRUE;
        macro->e.macroinfo->parmcnt = 1;
        macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list));
        macro->e.macroinfo->parmlist[0].def = NULL;
        macro->e.macroinfo->parmlist[0].label = parmnames[0];
        macro->e.macroinfo->parmlist[0].required = TRUE;

        // add @SubStr() macro func

        macro = CreateMacro("@SubStr" );
        macro->sym.defined = TRUE;
        macro->sym.predefined = TRUE;
        macro->sym.func_ptr = SubStrFunc;
        macro->sym.isfunc = TRUE;
        macro->e.macroinfo->parmcnt = 3;
        macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list) * 3);
        for (i = 0; i < 3; i++) {
            macro->e.macroinfo->parmlist[i].def = NULL;
            macro->e.macroinfo->parmlist[i].label = parmnames[i];
            macro->e.macroinfo->parmlist[i].required = (i < 2);
        }

        // add @Environ() macro func

        macro = CreateMacro("@Environ" );
        macro->sym.defined = TRUE;
        macro->sym.predefined = TRUE;
        macro->sym.func_ptr = EnvironFunc;
        macro->sym.isfunc = TRUE;
        macro->e.macroinfo->parmcnt = 1;
        macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list));
        macro->e.macroinfo->parmlist->def = NULL;
        macro->e.macroinfo->parmlist->label = parmnames[0];
        macro->e.macroinfo->parmlist->required = TRUE;
    }
    return(NOT_ERROR);
}
