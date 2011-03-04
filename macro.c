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
* - PurgeDirective   handle PURGE directive
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

/* store empty macro lines, to ensure correct line numbering
 */
#define STORE_EMPTY_LINES 1

/* 1="undefine" macros with PURGE - this isn't Masm-compatible,
 * and offers no real benefit because the name remains in the namespace.
 * The macro is marked "undefined" and cannot be invoked anymore.
 * 0=just delete the macro content.
 */
#define TRUEPERGE 0

extern int  MacroLocals;

/* the list of macro param + local names is hold temporarily only.
 * once the names have been replaced by placeholders,
 * the list is superfluous. What's stored permanently
 * in the macro item is the number of params and locals only.
 */
struct mname_list {
    struct mname_list  *next;
    uint_8              len;
    char                label[1];         /* name of param/local */
};

static int replace_parm( const char *line, char *start, int len, struct mname_list *mnames )
/******************************************************************************************/
{
    /* scan list of macro paras/locals if current word is found.
     * - line: current line
     * - start: start 'current word' in line
     * - len: size current word
     * - mnames: list of macro params+locals
     * if found, the 'current word' is replaced by a placeholder.
     * format of placeholders is <placeholder_char><index>
     * <placeholder_char> is an escape character whose hex-code is
     * "impossible" to occur in a source line, <index> has type uint_8,
     * value 00 isn't used - this restricts the total of parameters
     * and locals for a macro to 255.
     */
    char       *rest;
    uint       count;

//  DebugMsg(("replace_parm(%s) enter, len=%u\n", start, len ));

    for( count = 1; mnames; count++, mnames = mnames->next ) {
        if( mnames->len == len && SymCmpFunc( start, mnames->label, len ) == 0 ) {

            /* found a macro parameter/local! */

            if ( count > 0xFF ) {
                AsmError( TOO_MANY_MACRO_PLACEHOLDERS );
                break;
            }

            /* handle substitution operator '&' */
            rest = start + len;
            if ( start != line && *(start-1) == '&' )
                start--;
            if (*rest == '&')
                rest++;

            *start++ = PLACEHOLDER_CHAR;

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

static int store_placeholders( char *line, struct mname_list *mnames )
/********************************************************************/
{
    /* scan a macro source line for parameter and local names.
     * - line: the source line
     * - mnames: list of macro params + locals
     * if a param/local is found, replace the name by a 2-byte placeholder.
     */
    char *p;
    char *start;
    char quote = NULLC;
    int brlevel = 0;
    int params = 0; /* number of replacements in this line */
    int qlevel;
    bool substprf;  /* substitution character before ID? */

    for( p = line; *p != NULLC; ) {
        if ( isdigit( *p) ) {
            /* skip numbers (they may contain alphas) */
            while ( is_valid_id_char( *p )) p++;
        } else if ( ( is_valid_id_char( *p ) ) ||
                   ( *p == '.' &&
                    ModuleInfo.dotname &&
                    //is_valid_id_char(*(p+1)) && /* v2.05: masm allows a single dot as param/local name */
                    ( p == line ||
                     ( *(p-1) != ']' && ( is_valid_id_char( *(p-1) ) == FALSE ) ) ) ) ) {
            DebugMsg1(("store_placeholders: found ID: %s\n", p));
            start = p++;
            while ( is_valid_id_char( *p )) p++;
            substprf = ( start != line && *(start-1) == '&');
            if ( quote == NULLC || substprf ) {
                /* look for this word in the macro parms, and replace it if it is */
                if ( replace_parm( line, start, p - start, mnames ) ) {
                    params++;
                    p = start + PLACEHOLDER_SIZE - (substprf ? 1 : 0);
                }
            }
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

/*
 * store a macro's parameter, local and content list.
 * i = start index of macro params in AsmBuffer[].
 * this function destroys contents of AsmBuffer!
 */

ret_code StoreMacro( dir_node * macro, int i, bool store_data )
/*************************************************************/
{
    macro_info          *info;
    char                *string;
    char                *token;
    struct mparm_list   *paranode;
    struct mname_list   *mname_head = NULL;
    struct mname_list   *mname_tail;
    struct mname_list   *mname_node;
    struct asmlines     **nextline;
#ifdef DEBUG_OUT
    int lineno = 0;
#endif
    uint                nesting_depth = 0;
    bool                locals_done;
    struct line_status  ls;
    char                buffer[ MAX_LINE_LEN ];

    DebugMsg1(("StoreMacro(%s, i=%u, store_data=%u) enter\n", macro->sym.name, i, store_data ));
    info = macro->e.macroinfo;

    if( store_data ) {
        int j;

        if ( i < Token_Count ) {
            for ( j = i, info->parmcnt = 1; j < Token_Count; j++ )
                if ( AsmBuffer[j]->token == T_COMMA )
                    info->parmcnt++;
            info->parmlist = AsmAlloc( info->parmcnt * sizeof(struct mparm_list));
        } else {
            info->parmcnt = 0;
            info->parmlist = NULL;
        }

        for( paranode = info->parmlist ; i < Token_Count ; paranode++ ) {

            token = AsmBuffer[i]->string_ptr;
            /* Masm accepts reserved words and instructions as parameter
             * names! So just check that the token is a valid id.
             */
            if ( !is_valid_id_first_char( *token ) ) {
                AsmErr( SYNTAX_ERROR_EX, token );
                break;
            } else if ( AsmBuffer[i]->token != T_ID )
                AsmWarn( 4, PARAM_IS_RESERVED_WORD, AsmBuffer[i]->string_ptr );

            paranode->deflt = NULL;
            paranode->required = FALSE;

            /* first get the parm. name */
            j = strlen( token );
            mname_node = AsmTmpAlloc( sizeof( struct mname_list ) + j );
            memcpy( (char *)mname_node->label, token, j+1 );
            mname_node->next = NULL;
            mname_node->len = j;
            if( mname_head == NULL )
                mname_head = mname_node;
            else
                mname_tail->next = mname_node;
            mname_tail = mname_node;
            i++;

            /* now see if it has a default value or is required */
            if( AsmBuffer[i]->token == T_COLON ) {
                i++;
                if( *AsmBuffer[i]->string_ptr == '=' ) {
                    i++;
                    /* allowed syntax is parm:=<literal> */
                    if( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
                        AsmError( LITERAL_EXPECTED_AFTER_EQ );
                        break; // return( ERROR );
                    }
                    paranode->deflt = AsmAlloc( AsmBuffer[i]->value + 1 );
                    memcpy( paranode->deflt, AsmBuffer[i]->string_ptr, AsmBuffer[i]->value + 1 );
                    i++;
                } else if( _stricmp( AsmBuffer[i]->string_ptr, "REQ" ) == 0 ) {
                    /* required parameter */
                    paranode->required = TRUE;
                    i++;
                } else if( ( AsmBuffer[i]->token == T_RES_ID ) && ( AsmBuffer[i]->value == T_VARARG )) {
                    /* more parameters can follow */
                    macro->sym.mac_vararg = TRUE;
                    if ( AsmBuffer[i+1]->token != T_FINAL ) {
                        AsmError( VARARG_PARAMETER_MUST_BE_LAST );
                        break;
                    }
                    i++;
#if MACROLABEL
                } else if( AsmBuffer[i]->token == T_DIRECTIVE &&
                          AsmBuffer[i]->value == T_LABEL &&
                          Options.strict_masm_compat == FALSE ) { /* parm:LABEL? */
                    /* LABEL attribute for first param only! */
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
            DebugMsg1(("StoreMacro(%s): param=>%s< found\n", macro->sym.name, mname_node->label));
            if( i < Token_Count && AsmBuffer[i]->token != T_COMMA ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->tokpos );
                break; // return( ERROR );
            }
            /* go past comma */
            i++;

        } /* end for() */
        DebugMsg1(("StoreMacro(%s): macro parameters done\n", macro->sym.name));
    }

    locals_done = FALSE;
    nextline = &info->data;
    /* now read in the lines of the macro, and store them */
    for( ; ; ) {
        char *ptr;
        char *ptr2;

        string = GetTextLine( buffer, sizeof( buffer ) );
        if( string == NULL ) {
            AsmError( UNMATCHED_MACRO_NESTING );
            ModuleInfo.EndDirFound = TRUE; /* avoid error "END not found" */
            return( ERROR );
        }

        /* add the macro line to the listing file */
        if ( ModuleInfo.list && store_data ) {
            char *oldsrc = CurrSource;
            CurrSource = buffer;
            LstWrite( LSTTYPE_MACROLINE, 0, NULL );
            CurrSource = oldsrc;
        }

        while ( isspace(*string) ) string++;

        /* skip empty lines! */
        if ( *string == NULLC || *string == ';' ) {
#if STORE_EMPTY_LINES
            if( store_data ) {
                *nextline = AsmAlloc( sizeof( struct asmlines ) );
                (*nextline)->next = NULL;
                (*nextline)->ph_count = 0;
                (*nextline)->line[0] = NULLC;
                nextline = &(*nextline)->next;
            }
#endif
            continue;
        }

        /* v2.05: GetTextLine() doesn't concat lines anymore.
         * So if a backslash is found in the current source line,
         * tokenize it to get possible concatenated lines.
         */
        ls.input = string;
        ls.output = StringBufferEnd;
        AsmBuffer[0]->token = T_FINAL;
        GetToken( 0, &ls );
        if ( strchr( ls.input, '\\' ) ) {
            ptr = ls.input;
            while ( *ls.input && *ls.input != ';' ) {
                ls.is_concat = FALSE;
                GetToken( 1, &ls );
                if ( ls.is_concat && ModuleInfo.list && store_data ) {
                    char *oldsrc = CurrSource;
                    CurrSource = CurrSource + strlen( CurrSource ) + 1;
                    LstWrite( LSTTYPE_MACROLINE, 0, NULL );
                    CurrSource = oldsrc;
                }
                while( isspace( *ls.input ) ) ls.input++;
            }
            ls.input = ptr;
        }

        if( locals_done == FALSE && AsmBuffer[0]->token == T_DIRECTIVE && AsmBuffer[0]->value == T_LOCAL ) {
            if( !store_data )
                continue;
            while( isspace( *ls.input ) ) ls.input++;
            for ( ;*ls.input && *ls.input != ';'; ) { /* "no" locals are ok */
                int size;
                ls.output = StringBufferEnd;
                GetToken( 0, &ls );
                if ( !is_valid_id_first_char( *StringBufferEnd ) ) {
                    AsmErr( SYNTAX_ERROR_EX, StringBufferEnd );
                    break;
                } else if ( AsmBuffer[0]->token != T_ID )
                    AsmWarn( 4, PARAM_IS_RESERVED_WORD, StringBufferEnd );

                size = strlen( StringBufferEnd );
                mname_node = AsmTmpAlloc( sizeof( struct mname_list ) + size );
                memcpy( mname_node->label, StringBufferEnd, size + 1 );
                mname_node->next = NULL;
                mname_node->len = size;
                if( mname_head == NULL )
                    mname_head = mname_node;
                else
                    mname_tail->next = mname_node;
                mname_tail = mname_node;
                info->localcnt++;
                DebugMsg1(("StoreMacro(%s, %u): local=>%s< added\n", macro->sym.name, nesting_depth, mname_node->label ));
                while( isspace( *ls.input ) ) ls.input++;
                if ( *ls.input == ',' ) {
                    ls.input++;
                    while( isspace( *ls.input ) ) ls.input++;
                } else if ( is_valid_id_first_char( *ls.input ) ) {
                    AsmErr( SYNTAX_ERROR_EX, ls.input );
                    break;
                }
            }
            continue;
        }

        locals_done = TRUE;

        /* macro label? */
        if ( AsmBuffer[0]->token == T_COLON ) {
            ;/* just store the line */
        } else if( AsmBuffer[0]->token == T_DIRECTIVE ) {
            if ( AsmBuffer[0]->value == T_EXITM ) {
                DebugMsg1(("StoreMacro(%s, %u): exitm found, >%s<\n", macro->sym.name, nesting_depth, string+5 ));
                if ( nesting_depth == 0 ) {
                    ptr = ls.input;
                    while( isspace( *ptr ) ) ptr++;
                    if ( *ptr && *ptr != ';' )
                        macro->sym.isfunc = TRUE;
                    //macro->sym.runsync = TRUE;
                }
            } else if( AsmBuffer[0]->value == T_ENDM ) {
                DebugMsg1(("StoreMacro(%s, %u): endm found\n", macro->sym.name, nesting_depth ));
                if( nesting_depth ) {
                    nesting_depth--;
                } else {
                    break; /* exit the for() loop */
                }
            } else if( AsmBuffer[0]->dirtype == DRT_LOOPDIR ) {
                nesting_depth++; /* FOR[C], IRP[C], REP[EA]T, WHILE */
            }
        } else {
            ptr = string;
            if (*ptr == '%') {
                ptr++;
                while( isspace( *ptr ) ) ptr++;
            }

            /* Skip either
             * - an optional code label for the loop directives
             * - a label for the MACRO directive.
             * This isn't a trivial task, because the source might
             * contain '&' operators or a macro function call.
             */
            if ( is_valid_id_first_char( *ptr ) || *ptr == '&' ) {
                ptr2 = ptr+1;
                while( is_valid_id_char( *ptr2 ) || *ptr2 == '&' ) ptr2++;
                if (*ptr2 == ':') {
                    ptr2++;
                    if (*ptr2 == ':')
                        ptr2++;
                    while ( isspace(*ptr2) ) ptr2++;
                    ptr = ptr2;
                } else {
                    while ( isspace(*ptr2) ) ptr2++;
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
                        while ( isspace( *ptr2 ) ) ptr2++;
                    }
                    /* MACRO directive? */
                    ls.input = ptr2;
                    if( GetToken( 0, &ls ) == NOT_ERROR && AsmBuffer[0]->token == T_DIRECTIVE && AsmBuffer[0]->value == T_MACRO ) {
                        nesting_depth++;
                        goto check_done;
                    }
                }
            }
            ls.input = ptr;
            if ( GetToken( 0, &ls ) == NOT_ERROR && AsmBuffer[0]->token == T_DIRECTIVE && AsmBuffer[0]->dirtype == DRT_LOOPDIR )
                nesting_depth++;
        }
    check_done:
        if( store_data ) {
            int j;
            uint_8 phs = 0;
            if ( mname_head )
                phs = store_placeholders( string, mname_head );
            j = strlen( string );
            *nextline = AsmAlloc( sizeof( struct asmlines ) + j );
            (*nextline)->next = NULL;
            (*nextline)->ph_count = phs;
            memcpy( (*nextline)->line, string, j + 1 );
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
                DebugMsg1(("StoreMacro(%s, %u): cnt=%u, %u. line >%s<\n", macro->sym.name, nesting_depth, phs, ++lineno, dbgbuff ));
            }
#endif
        }
    } /* end for */
    macro->sym.isdefined = TRUE;
    DebugMsg1(("StoreMacro(%s): exit, no error, isfunc=%u\n", macro->sym.name, macro->sym.isfunc));
    return( NOT_ERROR );
}

/* create a macro symbol */

dir_node *CreateMacro( const char *name )
/***************************************/
{
    dir_node *macro;
    if ( macro = (dir_node *)SymCreate( name, *name != NULLC )) {
        macro->sym.state = SYM_MACRO;
        macro->e.macroinfo = AsmAlloc( sizeof( macro_info ) );
        macro->e.macroinfo->parmcnt  = 0;
        macro->e.macroinfo->localcnt = 0;
        macro->e.macroinfo->parmlist = NULL;
        macro->e.macroinfo->data     = NULL;
#ifdef DEBUG_OUT
        macro->e.macroinfo->count = 0;
#endif
        macro->e.macroinfo->srcfile  = 0;
        macro->sym.mac_vararg = FALSE;
        macro->sym.isfunc = FALSE;
    }
    return( macro );
}

/* clear macro data */

void ReleaseMacroData( dir_node *macro )
/**************************************/
{
    int             i;
    struct asmlines *datacurr;
    struct asmlines *datanext;

    DebugMsg1(("ReleaseMacroData(%s) enter\n", macro->sym.name));
    /* free the parm list */
    for( i = 0 ; i < macro->e.macroinfo->parmcnt; i++ ) {
        /*
         for predefined macros, don't free the param labels,
         the items are stored in static memory
         */
        //if ( macro->sym.predefined == FALSE )
        //    AsmFree( (void *)macro->e.macroinfo->parmlist[i].label );
        AsmFree( macro->e.macroinfo->parmlist[i].deflt );
    }

    macro->e.macroinfo->parmcnt = 0;
    macro->e.macroinfo->localcnt = 0;

    if( macro->e.macroinfo->parmlist ) {
        AsmFree( macro->e.macroinfo->parmlist );
        macro->e.macroinfo->parmlist = NULL;
    }

    /* free the lines list */
    for( datacurr = macro->e.macroinfo->data ;datacurr; ) {
        datanext = datacurr->next;
        AsmFree( datacurr );
        datacurr = datanext;
    }
    macro->e.macroinfo->data = NULL;
    macro->e.macroinfo->srcfile = 0;
    macro->sym.mac_vararg = FALSE;
    macro->sym.isfunc = FALSE;
    return;
}

/* MACRO directive: define a macro
 * i: directive token ( is to be 1 )
 */
ret_code MacroDef( int i )
/************************/
{
    char                *name;
    bool                store_data;
    dir_node            *macro;

    name = AsmBuffer[0]->string_ptr;
    DebugMsg1(("MacroDef(%s) enter, i=%u\n", name, i ));

    macro = (dir_node *)SymSearch( name );
    /* no need to care about SYM_UNDEFINED, a macro must always be
     defined BEFORE it's used. */
    if( macro == NULL ) {
        macro = CreateMacro( name );
        macro->e.macroinfo->srcfile = get_curr_srcfile();
#if 0 /* a macro MUST be defined before it's used! */
    } else if( macro->sym.state == SYM_UNDEFINED ) {
        dir_remove_table( &Tables[TAB_UNDEF], macro );
        macro->sym.state = SYM_MACRO;
        macro->e.macroinfo = AsmAlloc( sizeof( macro_info ) );
        memset( macro->e.macroinfo, 0, sizeof ( macro_info ) );
        macro->e.macroinfo->srcfile = get_curr_srcfile();
#endif
    } else if( macro->sym.state != SYM_MACRO ) {
        AsmErr( SYMBOL_REDEFINITION, name );
        return( ERROR );
    }

    if (( Parse_Pass == PASS_1 ) || (macro->sym.variable) ) {
        /* is the macro redefined? */
        if ( macro->e.macroinfo->data != NULL ) {
            DebugMsg(("MacroDef(%s): macro already defined\n", name));
            ReleaseMacroData( macro );
            macro->sym.variable = TRUE;
        }
        store_data = TRUE;
    } else
        store_data = FALSE;

    if ( ModuleInfo.list )
        LstWriteSrcLine();

    return( StoreMacro( macro, ++i, store_data ) );
}

/*
 * PURGE directive.
 * Masm deletes the macro content, but the symbol name isn't released
 * and cannot be used for something else.
 * Text macros cannot be purged, because the PURGE arguments are expanded.
 */
ret_code PurgeDirective( int i )
/******************************/
{
    dir_node *dir;

    i++; /* skip directive */
    do {
        if ( AsmBuffer[i]->token != T_ID ) {
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
#if TRUEPURGE
        dir->sym.defined = FALSE;
#else
        ReleaseMacroData( dir );
#endif
        i++;
        if ( i < Token_Count ) {
            if ( AsmBuffer[i]->token != T_COMMA || AsmBuffer[i+1]->token == T_FINAL ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->tokpos );
                return( ERROR );
            }
            i++;
        }
    } while ( i < Token_Count );

    return( NOT_ERROR );
}

/* internal @Environ macro function */

static ret_code EnvironFunc( char * buffer, char * *params )
/**********************************************************/
{
    char * p = getenv( *params );
    /* todo: ensure that variable isn't > MAX_LINE_LENGTH */
    if ( p )
        strcpy( buffer, p );
    else
        buffer[0] = '\0';
    return( NOT_ERROR );
}

/* macro initialization
 * this proc is called once per pass
 */
ret_code MacroInit( int pass )
/****************************/
{
    dir_node *macro;

    DebugMsg(( "MacroInit(%u)\n", pass ));

    MacroLevel = 0;
    MacroLocals = 0;
    if (pass == PASS_1) {

        StringInit();

        /* add @Environ() macro func */

        macro = CreateMacro( "@Environ" );
        macro->sym.isdefined = TRUE;
        macro->sym.predefined = TRUE;
        macro->sym.func_ptr = EnvironFunc;
        macro->sym.isfunc = TRUE;
        macro->e.macroinfo->parmcnt = 1;
        macro->e.macroinfo->parmlist = AsmAlloc(sizeof(struct mparm_list));
        macro->e.macroinfo->parmlist->deflt = NULL;
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
