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
* Description:  MACRO, PURGE directives processing.
*               this code has been rewritten for JWasm.
*
* functions:
* - CreateMacro      create a macro item
* - ReleaseMacroData used to redefine/purge a macro
* - StoreMacro       store a macro's parameter/local/line list
* - MacroDef         handle MACRO directive
* - PurgeDef         handle PURGE directive
* - MacroInit        global macro initialization, set predefined macros
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "input.h"
#include "tokenize.h"
#include "macro.h"
#include "fastpass.h"
#include "listing.h"

/* a placeholder consists of escape char (0x0a) + index (1 byte).
 if this is to change, function fill_placeholders() in expans.c must
 be adjusted as well!
 */
#define PLACEHOLDER_SIZE 2

/* the list of macro local names is hold temporarily only.
 once the local names have been replaced by placeholders,
 the list of labels is superfluous. What's stored permanently
 in the macro item is the number of locals only.
 */
typedef struct mlocal_list {
    struct mlocal_list  *next;
    char                label[];         // name of local
} mlocal_list;

static int replace_parm( const char *line, char *start, int len, int parmcnt, mparm_list *parms , mlocal_list *locals )
/*********************************************************************************************************************/
{
    /* scan list of macro parameters/local if current word is found.
     - line: current line
     - start: start 'current word' in line
     - len: size current word
     * if found, the 'current word' is replaced by a placeholder.
     * format of placeholders is <placeholder_char><index>
     * <placeholder_char> is an escape character whose hex-code is
     * "impossible" to occur in a source line, <index> has type uint_8,
     * value 00 isn't used - this restricts the total of parameters
     * and locals for a macro to 255.
     */
    const char *label;
    char       *rest = start + len;
    uint       count;

//  DebugMsg(("replace_parm(%s) enter, len=%u\n", start, len ));

    for( count = 1; parmcnt || locals; count++ ) {
        if (parmcnt) {
            label = parms->label;
            parms++;
            parmcnt--;
        } else {
            label = locals->label;
            locals = locals->next;
        }
        if( label != NULL &&
           ( strlen( label ) == len ) &&
           ( memcmp( start, label, len ) == 0 ) ) {

            /* found a macro parameter/local! */

            if ( count > 0xFF ) {
                AsmError( TOO_MANY_MACRO_PLACEHOLDERS );
                break;
            }

            /* handle substitution operator '&' */
            if ( start != line && *(start-1) == '&' )
                start--;
            if (*rest == '&')
                rest++;

            *start = PLACEHOLDER_CHAR;
            start++;

            /* additional space needed for the placeholder? */
            if ( start >= rest ) {
                char *end = rest + strlen(rest);
                char *dst = end + 1;
                while (end >= rest )
                    *dst-- = *end--;
                *start = count;
            } else {
                *start++ = count;
                strcpy( start, rest );
            }
            return( 1 ); /* word has been replaced */
        }
    }
    return( 0 );
}

static int store_placeholders( char *line, int cnt, mparm_list *parms, mlocal_list *locals )
/******************************************************************************************/
{
    /* scan a macro source line for parameter and local names.
     - line: the source line
     - cnt: number of items in array of macro parameters
     - parms: array of macro parameters
     - locals: linked list of macro locals
     if a param/local is found, replace the name by a 2-byte placeholder.
     */
    char *p;
    char *start;
    char quote = NULLC;
    int brlevel = 0;
    int params = 0; /* number of replacements in this line */
    int qlevel;
    bool substprf;  /* substitution character before ID? */

    for( p = line; *p != NULLC; ) {
        if ( is_valid_id_first_char( *p ) ) {
            DebugMsg(("store_placeholders: found ID: %s\n", p));
            start = p++;
            while ( is_valid_id_char( *p )) p++;
            substprf = ( start != line && *(start-1) == '&');
            if ( quote == NULLC || substprf ) {
                /* look for this word in the macro parms, and replace it if it is */
                if ( replace_parm( line, start, p - start, cnt, parms, locals ) ) {
                    params++;
                    p = start + PLACEHOLDER_SIZE - (substprf ? 1 : 0);
                }
            }
        } else if ( isdigit( *p) ) {
            /* skip numbers (they may contain alphas) */
            while ( is_valid_id_char( *p )) p++;
        } else {
            switch (*p) {
            case '!':
                if ( quote == NULLC && *(p+1) != NULLC )
                /* v2.0: code below might be better - or evil. */
                //if ( quote == NULLC &&
                //    ( *(p+1) == '<' || *(p+1) == '>' || *(p+1) == '"' || *(p+1) == '\'' || *(p+1) == '!') )
                    p++;
                break;
            case '<':
                brlevel++;
                break;
            case '>':
                if (brlevel) {
                    if (qlevel == brlevel)
                        quote = NULLC;
                    brlevel--;
                }
                break;
            case '"':
            case '\'':
                if ( quote ) {
                    if ( quote == *p )
                        quote = NULLC;
                } else {
                    quote = *p;
                    qlevel = brlevel;
                }
            }
            p++;
        }
    }
    return( params );
}

/* check if <string> starts with <substr> */

static bool lineis( const char *string, const char *substr, int len )
/*******************************************************************/
{
    if( string[len] != '\0' && !isspace( string[len] ) ) {
        return( FALSE );
    }
    if( _strnicmp( string, substr, len ) ) {
        return( FALSE );
    }
    return( TRUE );
}

// store a macro's parameter, local and content list.
// i = start index of macro params

ret_code StoreMacro( dir_node * macro, int i, bool store_data )
/*************************************************************/
{
    macro_info          *info;
    char                *string;
    char                *token;
    mparm_list          *paranode;
    mlocal_list         *localfirst = NULL;
    mlocal_list         *localnode;
    asmlines            **nextline;
    uint                nesting_depth = 0;
    bool                locals_done;
    char                buffer[ MAX_LINE_LEN ];

    DebugMsg(("StoreMacro(%s, i=%u, store_data=%u) enter\n", macro->sym.name, i, store_data ));
    info = macro->e.macroinfo;

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
            if ( !is_valid_id_first_char( *token ) ) {
                AsmErr( SYNTAX_ERROR_EX, token );
                break;
            }
#if 0 /* v2.0: tokenizer handles dotnames ok */
            /* allow parameter names beginning with '.' */
            if ( AsmBuffer[i]->token == T_DOT ) {
                if ( ( AsmBuffer[i+1]->pos != (AsmBuffer[i]->pos + 1)) ||
                    !is_valid_id_char(*AsmBuffer[i+1]->string_ptr) ) {
                    AsmErr( SYNTAX_ERROR_EX, token );
                    break;
                }
                buffer[0] = '.';
                i++;
                strcpy( buffer+1, AsmBuffer[i]->string_ptr );
                token = buffer;
            }
#endif
            paranode->def = NULL;
            paranode->required = FALSE;

            /* first get the parm. name */
            paranode->label = AsmAlloc( strlen( token ) + 1 );
            strcpy( (char *)paranode->label, token );
            i++;

            /* now see if it has a default value or is required */
            if( AsmBuffer[i]->token == T_COLON ) {
                i++;
                if( *AsmBuffer[i]->string_ptr == '=' ) {
                    i++;
                    if( AsmBuffer[i]->token != T_STRING ) {
                        AsmError( LITERAL_EXPECTED_AFTER_EQ );
                        break; // return( ERROR );
                    }
                    paranode->def = AsmAlloc( AsmBuffer[i]->value + 1 );
                    strcpy( paranode->def, AsmBuffer[i]->string_ptr );
                    i++;
                } else if( _stricmp( AsmBuffer[i]->string_ptr, "REQ" ) == 0 ) {
                    /* required parameter */
                    paranode->required = TRUE;
                    i++;
                } else if( ( AsmBuffer[i]->token == T_RES_ID ) && ( AsmBuffer[i]->value == T_VARARG )) {
                    /* more parameters can follow */
                    macro->sym.vararg = TRUE;
                    if (AsmBuffer[i+1]->token != T_FINAL) {
                        AsmError( VARARG_PARAMETER_MUST_BE_LAST );
                        break;
                    }
                    i++;
#if MACROLABEL
                } else if( AsmBuffer[i]->token == T_DIRECTIVE &&
                          AsmBuffer[i]->value == T_LABEL &&
                          Options.strict_masm_compat == FALSE ) { /* parm:LABEL? */
                    /* must be first param */
                    if ( paranode != info->parmlist ) {
                        AsmError( LABEL_PARAMETER_MUST_BE_FIRST );
                        break;
                    }
                    macro->sym.label = TRUE;
                    i++;
#endif
                } else {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                    break;
                }
            }
            DebugMsg(("StoreMacro(%s): param=>%s< found\n", macro->sym.name, paranode->label));
            if( i< Token_Count && AsmBuffer[i]->token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                break; // return( ERROR );
            }
            /* go past comma */
            i++;

        } /* end for() */
        DebugMsg(("StoreMacro(%s): macro parameters done\n", macro->sym.name));
    }

    locals_done = FALSE;
    nextline = &info->data;
    /* now read in all the contents of the macro, and store them */
    for( ; ; ) {
        char *ptr;
        char *ptr2;

        string = GetTextLine( buffer, sizeof(buffer) );
        if( string == NULL ) {
            AsmError( UNMATCHED_MACRO_NESTING );
            ModuleInfo.EndDirectiveFound = TRUE; /* avoid error "END not found" */
            return( ERROR );
        }

        if ( ModuleInfo.list && store_data ) {
            char *oldsrc = CurrSource;
            CurrSource = buffer;
            LstWrite( LSTTYPE_MACROLINE, 0, NULL );
            CurrSource = oldsrc;
        }

        while ( isspace(*string) ) string++;
        //wipe_space( string );

        if ( *string == NULLC )
            continue;

        /* macro label? */
        if (*string == ':' && is_valid_id_char(*(string+1))) {
            ptr = string+2;
            while (is_valid_id_char(*ptr) || *string == '&') ptr++;
            /* the label must be the only item in the line */
            if (*ptr != NULLC) {
                /* make sure the label is zero-terminated */
                *ptr++ = NULLC;
                while ( isspace(*ptr) ) ptr++;
                if (*ptr) {
                    AsmError( SYNTAX_ERROR );
                }
            }
        } else if( locals_done == FALSE && lineis( string, "local", 5 ) ) {
            if( !store_data )
                continue;
            ptr = string+5; /* 5 = sizeof("local") */
            while( isspace( *ptr ) ) ptr++;
            for (;*ptr;) {
                if ( !( is_valid_id_first_char(*ptr) ) ) {
                    AsmErr( SYNTAX_ERROR_EX, ptr );
                    break;
                }
                for ( ptr2=ptr; *ptr2; ptr2++ )
                    if ( ( isspace( *ptr2 ) ) || ( *ptr2 == ',' ) )
                        break;
                localnode = AsmTmpAlloc( sizeof( mlocal_list ) + (ptr2 - ptr) + 1 );
                memcpy( localnode->label, ptr, ptr2 - ptr );
                *( localnode->label + (ptr2 - ptr) ) = NULLC;
                /* add this local node to the list */
                localnode->next = NULL;
                if( localfirst == NULL ) {
                    localfirst = localnode;
                } else {
                    mlocal_list *localcurr;
                    for( localcurr = localfirst; localcurr->next; localcurr = localcurr->next );
                    localcurr->next = localnode;
                }
                info->localcnt++;
                DebugMsg(("StoreMacro(%s, %u): local=>%s< added\n", macro->sym.name, nesting_depth, localnode->label ));
                ptr = ptr2;
                while( isspace( *ptr ) ) ptr++;
                if (*ptr == ',') ptr++;
                while( isspace( *ptr ) ) ptr++;
            }
            continue;
        // } else if( lineis( string, "exitm", 5 ) ) {
        /* allow "exitm<>" syntax */
        } else if( is_valid_id_char(*(string+5)) == FALSE && _memicmp( string, "exitm", 5 ) == 0 ) {
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
            DebugMsg(("StoreMacro(%s, %u): endm found\n", macro->sym.name, nesting_depth ));
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
        if ( is_valid_id_first_char(*ptr )) {
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
            uint_8 phs = 0;
            if ( info->parmcnt || localfirst )
                phs = store_placeholders( string, info->parmcnt, info->parmlist, localfirst );
            *nextline = AsmAlloc( sizeof( asmlines ) + strlen(string) + 1 );
            (*nextline)->next = NULL;
            (*nextline)->ph_count = phs;
            strcpy( (*nextline)->line, string );
            nextline = &(*nextline)->next;
#ifdef DEBUG_OUT
            /* the stored line cannot be displayed due to the format of
             the index field. for debugging, convert it to a readable format.
             */
            {
                char dbgbuff[MAX_LINE_LEN];
                char *src = string;
                char *dst;
                for ( dst = dbgbuff; *src; src++, dst++) {
                    if (*src == PLACEHOLDER_CHAR ) {
                        *dst++ = '#';
                        src++;
                        *dst = *src / 16 + '0';
                        if (*dst > '9')
                            *dst += 7;
                        dst++;
                        *dst = *src % 16 + '0';
                        if (*dst > '9')
                            *dst += 7;
                    } else
                        *dst = *src;
                }
                *dst = NULLC;
                DebugMsg(("StoreMacro(%s, %u): cnt=%u, line >%s<\n", macro->sym.name, nesting_depth, phs, dbgbuff ));
            }
#endif
        }
    }
    macro->sym.defined = TRUE;
    DebugMsg(("StoreMacro(%s): exit, no error, isfunc=%u\n", macro->sym.name, macro->sym.isfunc));
    return( NOT_ERROR );
}

// create a macro symbol

dir_node *CreateMacro( const char *name )
/***************************************/
{
    dir_node *macro;
    if (macro = (dir_node *)SymCreate( name, *name != NULLC )) {
        macro->sym.state = SYM_MACRO;
        macro->e.macroinfo = AsmAlloc( sizeof( macro_info ) );
        macro->e.macroinfo->parmcnt  = 0;
        macro->e.macroinfo->localcnt = 0;
        macro->e.macroinfo->parmlist = NULL;
        macro->e.macroinfo->data     = NULL;
        macro->e.macroinfo->srcfile  = 0;
        macro->sym.vararg = FALSE;
        macro->sym.isfunc = FALSE;
    }
    return( macro );
}

// clear macro data

void ReleaseMacroData( dir_node *macro )
/**************************************/
{
    int             i;
    asmlines        *datacurr;
    asmlines        *datanext;

    /* free the parm list */
    for( i = 0 ; i < macro->e.macroinfo->parmcnt; i++ ) {
        /*
         for predefined macros, don't free the param labels,
         the items are stored in static memory
         */
        if ( macro->sym.predefined == FALSE )
            AsmFree( (void *)macro->e.macroinfo->parmlist[i].label );
        AsmFree( macro->e.macroinfo->parmlist[i].def );
    }

    macro->e.macroinfo->parmcnt = 0;
    macro->e.macroinfo->localcnt = 0;

    if( macro->e.macroinfo->parmlist ) {
        AsmFree( macro->e.macroinfo->parmlist );
        macro->e.macroinfo->parmlist = NULL;
    }

    /* free the lines list */
    for(datacurr = macro->e.macroinfo->data ;datacurr; ) {
        datanext = datacurr->next;
        AsmFree( datacurr );
        datacurr = datanext;
    }
    macro->e.macroinfo->data = NULL;
    macro->e.macroinfo->srcfile = 0;
    macro->sym.vararg = FALSE;
    macro->sym.isfunc = FALSE;
    return;
}

// MACRO directive: define a macro
// i: index of macro name (is always 0)

ret_code MacroDef( int i )
/************************/
{
    char                *name;
    bool                store_data;
    dir_node            *macro;

    DebugMsg(("MacroDef(%u) enter, token=%s\n", i, AsmBuffer[i]->string_ptr ));

    if (AsmBuffer[i]->token != T_ID) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    name = AsmBuffer[i]->string_ptr;
    macro = (dir_node *)SymSearch( name );
    /* no need to care about SYM_UNDEFINED, a macro must always be
     defined BEFORE it's used. */
    if( macro == NULL ) {
        macro = CreateMacro( name );
        macro->e.macroinfo->srcfile = get_curr_srcfile();
    } else if( macro->sym.state != SYM_MACRO ) {
        AsmErr( SYMBOL_REDEFINITION, name );
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

    if ( ModuleInfo.list )
        LstWriteSrcLine();

    return( StoreMacro( macro, i+2, store_data ) );
}

/*
 PURGE directive implementation.
 Masm deletes the macro content, but the symbol name isn't released
 and cannot be used for something else.
 Text macros cannot be purged, because the PURGE arguments are expanded.
*/
ret_code PurgeDef( int i)
/***********************/
{
    dir_node *dir;

    do {
        if (AsmBuffer[i]->token != T_ID) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        dir = (dir_node *)SymSearch( AsmBuffer[i]->string_ptr );
        if ( dir == NULL ) {
            AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        if ( dir->sym.state != SYM_MACRO ) {
            AsmErr( EXPECTED, "macro name" );
            return( ERROR );
        }
        dir->sym.defined = FALSE;
        i++;
        if (AsmBuffer[i]->token == T_FINAL)
            break;
        if (AsmBuffer[i]->token != T_COMMA) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
    } while ( i < Token_Count );

    return( NOT_ERROR );
}

// internal @Environ macro function

static ret_code EnvironFunc(char * buffer, char * *params)
/********************************************************/
{
    char * p = getenv( *params );
    if (p)
        strcpy( buffer, p );
    else
        buffer[0] = '\0';
    return( NOT_ERROR );
}

// generic parameter names. In case the parameter name is
// displayed in an error message ("required parameter %s missing")

static char * parmnames[] = {"p1"};

// macro initialization
// this proc is called once per pass

ret_code MacroInit( int pass )
/****************************/
{
    dir_node *macro;

    DebugMsg(( "MacroInit(%u)\n", pass ));

    if (pass == PASS_1) {

        StringInit();

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
    return( NOT_ERROR );
}
#ifdef DEBUG_OUT
void MacroFini( void )
/********************/
{
    StringFini();
}
#endif
