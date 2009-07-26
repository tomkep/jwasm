/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  do macro expansion.
*
* functions:
* - RunMacro          run a macro
* - ExpandToken       expand one token
* - ExpandLine        expand a source line
* - GetTextMacroValue get contents of a text macro
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
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

#define QUOTE_PATCH /* PK!!! */

extern bool   expansion;
extern int    CurrIfState;

int   MacroLocals;  /* counter for LOCAL names */
bool  SkipMacroMode;

static const char __digits[] = "0123456789ABCDEF";

// C ltoa() isn't fully compatible since hex digits are lower case.
// for JWasm, it's ensured that 2 <= radix <= 16.

char *myltoa( int_32 value, char *buffer, int radix )
/***************************************************/
{
    char   *p;
    char   *dst = buffer;
    char   tmpbuf[34];

    tmpbuf[33] = '\0';
    if ( value < 0 ) {
        *dst++ = '-';
        value = 0 - value;
    } else if ( value == 0 ) {
        *dst++ = '0';
        *dst = NULLC;
        return( buffer );
    }
    for ( p = &tmpbuf[32]; value; value = value / radix )
        *p-- = __digits[value % radix];
    strcpy( dst, p + 1 );
    return( buffer );
}

// make room (or delete items) in the token buffer

static void AddTokens( int start, int count )
/*******************************************/
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

#ifdef __WATCOMC__
static _inline char HexDigit( char x )
#elif defined(_MSC_VER)
static _inline char HexDigit( char x )
#else
static char HexDigit( char x )
#endif
{
    x &= 0xF;
    return((x > 9) ? (x - 10 + 'A') : (x + '0'));
}

static void fill_placeholders( char *dst, char *src, uint argc, uint localstart, char *argv[] )
/***********************************************************************/
{
    /* fill placeholders in a stored macro source line with values of actual
     parameters and locals. a placeholder consists of escape char 0x0a,
     followed by a one-byte index field.
     */

    uint_32 i;
    char *p;
    uint parmno;

    /* scan the string, replace the placeholders #nn */
    for( p = src ;*p != NULLC; ) {
        if (*p == PLACEHOLDER_CHAR ) {
            p++;
            /* we found a placeholder, get the index part! */
            parmno = *(unsigned char *)p - 1; /* index is one-based! */
            p++;
            /* if parmno > argc, then it's a macro local */
            if ( parmno >= argc ) {
                *dst++ = '?';
                *dst++ = '?';
                i = localstart + parmno - argc;
                if ( i > 0xFFFF ) {
                    i = sprintf( dst, "%X", i );
                    dst += i;
                } else {
                    *dst++ = HexDigit( i >> 12 );
                    *dst++ = HexDigit( i >> 8 );
                    *dst++ = HexDigit( i >> 4 );
                    *dst++ = HexDigit( i );
                }
            } else if ( argv[parmno] ) {  /* actual parameter might be empty (=NULL) */
                i = strlen( argv[parmno] );
                memcpy( dst, argv[parmno], i );
                dst += i;
            }
        } else {
            *dst++ = *p++;
        }
    }
    *dst = NULLC;
    return;
}

// skip macro execution until ENDM
// this is to be improved. It's probably better just to quickly "read"
// to the end of the current "file_list" item in input.c. Thus there's
// no need to parse for ENDM in SkipMacro (which doesn't work perfectly).
// Problem with this approach is that the "conditional" status might get
// messed.

static ret_code SkipMacro( char * buffer )
/****************************************/
{
    int lvl = 1;
    int i;
    bool oldlist = ModuleInfo.list;

    DebugMsg((" SkipMacro() enter\n" ));
    SkipMacroMode = TRUE;

    ModuleInfo.list = FALSE; /* dont write a listing for these lines */

    buffer = buffer + strlen( buffer) + 1;

    while (lvl > 0) {
        i = GetPreprocessedLine( buffer );
        if (i < 0) {
            AsmError( UNEXPECTED_END_OF_FILE );
            break;
        }
        if ( Token_Count > 0 &&  AsmBuffer[0]->token == T_DIRECTIVE ) {
            if (AsmBuffer[0]->dirtype == DRT_LOOPDIR)
                lvl++;
            else if (AsmBuffer[0]->value == T_ENDM)
                lvl--;
        } else if (Token_Count > 1 &&
                 AsmBuffer[1]->token == T_DIRECTIVE &&
                 AsmBuffer[1]->value == T_MACRO)
            lvl++;
    }
    SkipMacroMode = FALSE;
    ModuleInfo.list = oldlist;
    DebugMsg((" SkipMacro() exit\n" ));
    return( NOT_ERROR );
}

static void AddBrackets(char *dest, char *src)
/********************************************/
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

/* rebuild a source line
 * adjust all "pos" values behind the current pos
 * - newstring = new value of item i
 * - i = position of item to replace
 * - string = source line to rebuild
 * - olelen = length of old item i
*/
static void RebuildLine(char * newstring, int i, char * string, int oldlen, bool addbrackets )
/********************************************************************************************/
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
        Fatal( FATAL_EXPANDED_LINE_TOO_LONG, string );
    }
    if (addbrackets) {
#ifdef DEBUG_OUT
#if !defined(__GNUC__) && !defined(__TINYC__)
        /* check buffer overflow */
        if ( (len + newlen + 2 + strlen(src+oldlen) + 1) > MAX_LINE_LEN ) {
            DebugMsg(("RebuildLine: buffer overflow error\n"));
            _asm int 3;
        }
#endif
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
#if !defined(__GNUC__) && !defined(__TINYC__)
        /* check buffer overflow */
        if ( (len + newlen + strlen(src+oldlen) + 1) > MAX_LINE_LEN ) {
            DebugMsg(("RebuildLine: buffer overflow error\n"));
            _asm int 3;
        }
#endif
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

// run a macro
// macro: macro item
// params: parameter string (includes macro name!)
// prefix: line prefix which should be emitted "first".
//         this is also the output buffer!
// runit: 1=emit a "ENDM" as last line and run it
// insert: 1=call PushLineQueue()
// returns number of characters processed or -1 on errors

int RunMacro( dir_node * macro, char * params, char * prefix, bool runit, bool insert, bool addbrackets )
/*******************************************************************************************************/
{
    char        *newline = CurrSource + strlen( CurrSource ) + 1;
    char        *orgsrc = params;
    macro_info  *info;
    asmlines    *lnode;
    dir_node    *dir;
    expr_list   opndx;
    int         count;
    int         i;
    int         parmidx;
    int         bracket_level = -1;/* () level */
    uint        localstart;
    char        delim;            /* parameter delimiter */
    char        expansion_flag;   /* % operator found */
    char        parm_end_delim;   /* parameter end delimiter */
    char        *ptr;
    char        **parm_array = NULL;
    char        line[MAX_LINE_LEN];

    DebugMsg(("%lu. RunMacro(%s) enter, src=>%s<, run=%u, insert=%u [MacroLocals=%04u]\n", LineNumber, macro->sym.name, params, runit, insert, MacroLocals ));

    info = macro->e.macroinfo;

#ifdef DEBUG_OUT
    info->count++;
#endif
    /* skip the macro name - if there is one at all */
    if ( *(macro->sym.name) ) {
        if (*params == '.') /* assume OPTION DOTNAME */
            params++;
        while (is_valid_id_char(*params)) params++;
    }

    parm_end_delim = NULLC;

    /* invokation of macro functions requires params enclosed in "()" */

    if ( macro->sym.isfunc ) {
        while ( isspace(*params) ) params++;
        if ( *params == '(' ) {
            params++;
            parm_end_delim = ')';
            bracket_level = 1;
        }
    }

    DebugMsg(( "%lu. RunMacro(%s): params=>%s< \n", LineNumber, macro->sym.name, params ));

    if ( info->parmcnt )
        parm_array = (char **)AsmTmpAlloc( info->parmcnt * sizeof( char * ) );

    /* skip white spaces (in case there are no params at all) */
    while ( isspace(*params) ) params++;

    /* now get all the parameters from the original src line.
     macro parameters are expanded if
     - it is a macro function call            or
     - the expansion operator (%) is found
     */

    delim = ',';

    for( parmidx = 0; parmidx < info->parmcnt; parmidx++ ) {

        /* for a VARARG parameter, ignore commas */
        if (( parmidx == info->parmcnt - 1 ) && (macro->sym.vararg) ) {
            delim = NULLC;
        }

        if ( *params == NULLC || *params == parm_end_delim || *params == delim ) {

            /* it's a blank parm */
            if( info->parmlist[parmidx].required ) {
                DebugMsg(( "RunMacro(%s.%s), parameter required, >%s<\n", macro->sym.name, info->parmlist[parmidx].label, params ));
                AsmErr( REQUIRED_PARAMETER_MISSING, macro->sym.name, info->parmlist[parmidx].label );
                return( -1 );
            }
            parm_array[parmidx] = info->parmlist[parmidx].def;

        } else {

            char * startitem = NULL;
            int  str_level = 0;    /* <> level */
#ifdef QUOTE_PATCH /* PK!!! */
            char quote_ch  = NULLC;
#endif
            *newline = NULLC;
            expansion_flag = FALSE;
            ptr = newline;

            DebugMsg(( "RunMacro(%s.%s), >%s<\n", macro->sym.name, info->parmlist[parmidx].label, params ));

            /* the % operator can be inside a string enclosed in <>! */
#ifdef QUOTE_PATCH /* PK!!! */
            if (*params == '<' && quote_ch == NULLC) {
                str_level++;
                params++;
            }
#else
            if (*params == '<') {
                str_level++;
                params++;
            }
#endif

            /* handle expansion of macro parameters.
             the expansion operator does
             - run a macro function
             - expand a text macro
             - store the result of an expression as text
             it is valid for the whole parameter (until a comma is found)
             */
            if( *params == '%' ) {
                int rep = 0;
                char  *savedCurrSource;
                char  *oldpos;
                expansion_flag = TRUE;
                *ptr = NULLC;
                strcpy( line, params );
                while ( 1 ) {
                    Token_Count = Tokenize( line, 1 );
                    if ( AsmBuffer[2]->token == T_ID ) {
                        dir = (dir_node *)SymSearch( AsmBuffer[2]->string_ptr );
                        if ( dir && dir->sym.state == SYM_MACRO &&
                            dir->sym.isfunc && dir->sym.defined &&
                            AsmBuffer[3]->token == T_OP_BRACKET ) {
                            int savedIfState;
                            savedCurrSource = CurrSource;
                            oldpos = AsmBuffer[2]->pos;
                            *ptr = NULLC;
                            CurrSource = newline;
                            if ( rep == 0 )
                                params += oldpos - line;
                            savedIfState = CurrIfState;
                            CurrIfState = 0;
                            i = RunMacro( dir, oldpos, ptr, TRUE, TRUE, FALSE );
                            DebugMsg(("RunMacro(%s.%s): back from RunMacro(%s), rc=%u, buffer=>%s<\n", macro->sym.name, info->parmlist[parmidx].label, dir->sym.name, i, line ));
                            CurrIfState = savedIfState;
                            CurrSource = savedCurrSource;
                            if ( rep == 0)
                                params = params + i - 1;
                        } else if (dir && dir->sym.state == SYM_TMACRO &&
                                   dir->sym.defined == TRUE) {
                            oldpos = AsmBuffer[2]->pos;
                            i = strlen(AsmBuffer[2]->string_ptr);
                            DebugMsg(( "RunMacro(%s.%s), expansion=1, text macro found\n", macro->sym.name, info->parmlist[parmidx].label ));
                            for ( count = 0; count < MAX_TEXTMACRO_NESTING; count++ ) {
                                dir_node * tmpdir;
                                tmpdir = (dir_node *)SymSearch( dir->sym.string_ptr );
                                if (tmpdir &&
                                    tmpdir->sym.state == SYM_TMACRO &&
                                    tmpdir->sym.defined)
                                    dir = tmpdir;
                                else
                                    break;
                            }
                            GetTextMacroValue( dir->sym.string_ptr, ptr );
                            if ( rep == 0 )
                                params += (AsmBuffer[3]->pos - line) - 1;
                        } else {
                            break;
                        }
                        rep++;
                        *line = '%';
                        if ( i < strlen(ptr) ) {
                            char temp[MAX_LINE_LEN];
                            strcpy( temp, ptr );
                            strcat( temp, oldpos+i );
                            strcpy( line+1, temp );
                        } else {
                            strcpy( line+1, ptr );
                            strcat( line, oldpos+i );
                        }
                        expansion_flag = FALSE;
                        continue;
                    }
                    break;
                }
                ptr += strlen( ptr );
                if ( expansion_flag && str_level ) {
                    /* remove the '>' from the token buffer */
                    for (i = 2; i < Token_Count; i++ )
                        if ( AsmBuffer[i]->token == T_STRING &&
                            *AsmBuffer[i]->string_ptr == '>') {
                            AsmBuffer[i]->token = T_FINAL;
                            Token_Count = i;
                        }
                }
                params++;

                if ( expansion_flag == TRUE ) {
                    i = 2;
                    if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
                        opndx.value = 0;
                    } else {
                        DebugMsg(( "RunMacro(%s.%s): expansion, opndx.type=%d, value=%u\n", macro->sym.name, info->parmlist[parmidx].label, opndx.type, opndx.value));
                        /* the expression evaluator accepts forward references
                         but the % operator won't accept them */
                        if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
                            if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED ) {
                                AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
                            } else {
                                AsmError( EXPRESSION_NOT_A_CONSTANT );
                            }
                            opndx.value = 0;
                        }
                    }
                    myltoa( opndx.value, ptr, ModuleInfo.radix );
                    ptr = ptr + strlen(ptr);
                    params += (AsmBuffer[i]->pos - line) - 1;
                }
            }

            while ( *params != '\0' ) {
                //dir_node * dir2;
                if  (*params == delim && str_level == 0)
                    break;
#ifndef QUOTE_PATCH
                /* is this ok? what about EXITM <!SIGN?> ? */
                if (*params == '!' && *(params+1) != '\0') {
                    params++;
                    *ptr++ = *params++;
                    continue;
                }
#endif
#ifdef QUOTE_PATCH /* PK!!! */
                if ( quote_ch == NULLC )
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
#ifdef QUOTE_PATCH /* PK!!! */
                } else if (*params == '!' && *(params+1) != '\0') {
                    params++;
                    *ptr++ = *params++;
                    continue;
#endif
                }
#ifdef QUOTE_PATCH /* PK!!! */
                if (*params == '"' || *params == '\'') {
                    if (quote_ch == NULLC) {
                        /* Open string */
                        quote_ch = *params;
                    }
                    else if (quote_ch == *params) {
                        /* Close string */
                        quote_ch = NULLC;
                    }
                }
#endif

                // if (*params == '"' || *params == '\'') {
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
                if ( startitem == NULL && is_valid_id_char(*params) == TRUE )
                    startitem = ptr;

                if ( bracket_level > 0 ) {
                    if ( *params == '(' ) {
                        bracket_level++;
                    } else if ( *params == ')' ) {
                        bracket_level--;
                        if ( bracket_level == 0 )
                            break;
                    }
                }

                *ptr++ = *params++;

                /* check for (text) macros */

                if (str_level == 0 &&
                    is_valid_id_char( *params ) == FALSE &&
                    startitem != NULL) {

                    *ptr = '\0';
                    dir = (dir_node *)SymSearch( startitem );
                    if (dir &&
                        dir->sym.state == SYM_MACRO &&
                        dir->sym.defined == TRUE &&
                        dir->sym.isfunc == TRUE) {
                        char * p = params;
                        while ( isspace(*p) ) p++;
                        /* no macro function invokation if the '(' is missing! */
                        if (*p == '(') {
                            /* RunMacro must be reentrant. To avoid too much
                             stack usage, it uses space behind the current
                             source line. Therefore variable CurrSource must
                             be adjusted, and the old value saved/restored.
                             */
                            char *savedCurrSource = CurrSource;
                            int savedIfState;
                            CurrSource = newline;
                            savedIfState = CurrIfState;
                            CurrIfState = 0;
                            params -= strlen( startitem );
                            ptr -= strlen( startitem );
                            *ptr = NULLC;
                            line[0] = NULLC;
                            i = RunMacro( dir, params, line, TRUE, TRUE, FALSE );
                            DebugMsg(("RunMacro(%s.%s): back from RunMacro(%s), rc=%u, buffer=>%s<\n", macro->sym.name, info->parmlist[parmidx].label, dir->sym.name, i, line));
                            CurrIfState = savedIfState;
                            CurrSource = savedCurrSource;
                            if ( i == -1 )
                                return( -1 );
                            strcpy( ptr, line );
                            ptr += strlen( ptr );
                            params = params + i;
                            startitem = NULL;
                            continue;
                        }
                    }
                    startitem = NULL;
                }

            } /* end while */

            *ptr = NULLC;

            if ( *newline ) {
                parm_array[parmidx] = (char *)AsmTmpAlloc( strlen( newline ) + 1 );
                strcpy( parm_array[parmidx], newline );
            } else
                parm_array[parmidx] = "";
        }

#ifdef DEBUG_OUT
        if ( parm_array[parmidx] )
            DebugMsg(("RunMacro(%s.%s): actual parameter value=>%s<\n", macro->sym.name, info->parmlist[parmidx].label, parm_array[parmidx] ));
        else
            DebugMsg(("RunMacro(%s.%s): actual parameter value=NULL\n", macro->sym.name, info->parmlist[parmidx].label ));
#endif
        if (*params == ',') {
            params++;
            while (isspace(*params)) params++;
        }
    } /* end for  */

    if ( bracket_level >= 0 ) {
        if ( *params == NULLC ) {
            DebugMsg(("RunMacro(%s): missing ')'\n", macro->sym.name));
            AsmError( MISSING_RIGHT_PARENTHESIS );
            return( -1 );
        } else if ( *params != ')' ) {
            DebugMsg(("RunMacro(%s): expected ')', found >%s<\n", macro->sym.name, params));
            AsmErr( TOO_MANY_ARGUMENTS_IN_MACRO_CALL, macro->sym.name);
            return( -1 );
        } else
            params++;

        /* if macro name is "", it's a FOR/FORC macro.
         a check for a valid end must NOT be done then. */
    } else if ( *params != '\0' && *macro->sym.name != NULLC ) {
        DebugMsg(("RunMacro(%s): expected NULL, found >%s<\n", macro->sym.name, params));
        AsmErr( TOO_MANY_ARGUMENTS_IN_MACRO_CALL, macro->sym.name );
        return( -1 );
    }

    /* a predefined macro func with a function address? */

    if ( macro->sym.predefined == TRUE && macro->sym.func_ptr != NULL ) {
        line[0] = NULLC;
        macro->sym.func_ptr( line, parm_array );
        if ( addbrackets ) {
            AddBrackets( prefix+strlen( prefix ), line );
        } else
            strcat( prefix, line );
        AsmBuffer[0]->value = T_EXITM;
        return( params - orgsrc );
    }

    if ( insert ) {
        PushLineQueue();
    }

    /* if there's a label before the macro, write it */
    if ( macro->sym.isfunc == FALSE && prefix ) {
        DebugMsg(("RunMacro(%s): isfunc=FALSE, prefix >%s< queued\n", macro->sym.name, prefix ));
        AddLineQueue( prefix );
        *prefix = NULLC; /* added v1.96 */
    }
    localstart = MacroLocals;
    MacroLocals += info->localcnt; /* adjust global variable MacroLocals */

    /* emit the source lines */
    for( lnode = info->data; lnode != NULL; lnode = lnode->next ) {
        /* if line contains placeholders, replace them by current values */
        if ( lnode->ph_count ) {
            fill_placeholders( line, lnode->line, info->parmcnt, localstart, parm_array );
            AddLineQueue( line );
        } else {
            AddLineQueue( lnode->line );
        }
//        line_queue->tail->macrolevel = MacroLevel+1;
    }

    if ( runit || macro->sym.isfunc ) {
        char *OldSource = CurrSource;
        AddLineQueue( "endm" );
//        line_queue->tail->macrolevel = MacroLevel+1;
        DebugMsg(("%lu. RunMacro(%s): enter assembly loop\n", LineNumber, macro->sym.name ));
        /*
         * move the current line queue to the file stack!
         * Also reset the current linenumber!
         */
        PushMacro( (asm_sym *)macro );
        /* run the assembler until we hit EXITM or ENDM. */
        /* also handle GOTO! */
        for(;;) {
            int i;
            while (0 == (i = GetPreprocessedLine( newline )));
            /* i < 0 probably cannot happen at all */
            if (i < 0) {
                DebugMsg(("%lu. RunMacro(%s): Internal error, i < 0 occured!\n", LineNumber, macro->sym.name ));
                CurrSource = OldSource;
                return( -1 );
            }
            /* skip macro label lines */
            if (AsmBuffer[0]->token == T_COLON)
                continue;

            if (AsmBuffer[0]->token == T_DIRECTIVE) {
                if (AsmBuffer[0]->value == T_EXITM) {
                    //int pos;
                    i = 1;
                    DebugMsg(("%lu. RunMacro(%s): lvl=%u, EXITM detected\n", LineNumber, macro->sym.name, MacroLevel ));
                    if ( ModuleInfo.list && ModuleInfo.list_macro == LM_LISTMACROALL )
                        LstWriteSrcLine();
                    if (prefix) {
                        line[0] = '\0';
                        if ( addbrackets ) {
#if 1
                            AddBrackets( line, AsmBuffer[i]->string_ptr );
#else
                            strcat( line, "<" );
                            if ( AsmBuffer[i]->token != T_FINAL )
                                strcat( line, AsmBuffer[i]->string_ptr );
                            strcat( line,">" );
#endif
                        } else {
                            if ( AsmBuffer[i]->token != T_FINAL ) {
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
                        DebugMsg(("%lu. RunMacro(%s): prefix=%s, exitm-str=%s, suffix=%s\n", LineNumber, macro->sym.name, prefix, AsmBuffer[i]->string_ptr, params));
                        strcat(prefix, line);
//                        strcat(prefix, params);
                    }
                    SkipMacro( newline );
                    MacroLevel--;
                    CurrSource = OldSource;
                    AsmBuffer[0]->value = T_EXITM;
                    DebugMsg(("%lu. RunMacro(%s): EXITM, new MacroLevel=%u >%s<\n", LineNumber, macro->sym.name, MacroLevel, prefix ));
                    break;
                } else if (AsmBuffer[0]->value == T_ENDM) {
                    MacroLevel--;
                    DebugMsg(("%lu. RunMacro(%s): ENDM, new MacroLevel=%u\n", LineNumber, macro->sym.name, MacroLevel));
                    CurrSource = OldSource;
                    break;
                } else if (AsmBuffer[0]->value == T_GOTO) {
                    DebugMsg(("RunMacro(%s): GOTO, MacroLevel=%u\n", macro->sym.name, MacroLevel ));
                    strcpy ( line, AsmBuffer[1]->string_ptr );
                    for( i = 1, lnode = info->data; lnode != NULL; lnode = lnode->next, i++ ) {
                        ptr = lnode->line;
                        //DebugMsg(("RunMacro(%s): GOTO, scan line >%s< for label >%s<\n", macro->sym.name, ptr, line));
                        if (*ptr == ':' &&  ( _stricmp( ptr+1, line ) == 0)) {
                            /* label found! */
                            lnode = lnode->next;
                            break;
                        }
                    }
                    SkipMacro( newline );
                    MacroLevel--; /* PushMacroGoto() will increase mlvl */
                    if (lnode) {
                        DebugMsg(("RunMacro(%s): GOTO, found label >%s<\n", macro->sym.name, line));
                        //PushLineQueue();
                        for( ; lnode != NULL; lnode = lnode->next ) {
                            if ( lnode->ph_count ) {
                                fill_placeholders( line, lnode->line, info->parmcnt, localstart, parm_array );
                                AddLineQueue( line );
                            } else
                                AddLineQueue( lnode->line );
                        }
                        AddLineQueue( "endm" );
                        PushMacroGoto( (asm_sym *)macro, i );
                        //PushMacro( (asm_sym *)macro );
                        continue;
                    } else {
                        DebugMsg(("RunMacro(%s): GOTO, label >%s< not found!\n", macro->sym.name, line));
                        break;
                    }
                }
            }
#if FASTPASS
            if ( StoreState && GeneratedCode == 0 )
                StoreLine( newline );
#endif
            ParseItems();
            if ( Options.preprocessor_stdout == TRUE )
                WritePreprocessedLine( newline );

            /* the macro might contain an END directive */
            if ( ModuleInfo.EndDirectiveFound ) {
                MacroLevel--;
                AsmBuffer[0]->token = T_EXITM; /* force loop exit */
                while ( GetTextLine( newline, sizeof( newline ) ) );
                break;
            }
        } /* end for */
        /* restore the line number now! If this isn't done here, the
         * assembler might display the "current" macro line number instead
         * of the "true" number if a macro function was executed.
         */
        RestoreLineNumber();
    }

    DebugMsg(("%lu. RunMacro(%s) exit\n", LineNumber, macro->sym.name));

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

static ret_code ExpandText(char * line, bool substitute, bool addbrackets )
/*************************************************************************/
{
    char *pSrc;
    char *pDst;
    char *pIdent;
    //char *pStart;
    unsigned char *pSave = NULL;
    ret_code rc = NOT_ERROR;
    int count;
    bool expanded = TRUE;
    dir_node * dir;
    char srcline[MAX_LINE_LEN];

    DebugMsg(("ExpandText(line=>%s<, subst=%u, addbrackets=%u) enter\n", line, substitute, addbrackets ));
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
                    //if (pIdent > line && *(pIdent-1) == '%')
                    /* see sample CatStr9.asm: %ifidn <abc>,<%TE1> */
                    if (pIdent > line && *(pIdent-1) == '%' && expansion == FALSE )
                        pIdent--;
#endif
                    if (substitute) {
                        if (*(pIdent-1) == '&')
                            pIdent--;
                        if (*pSrc == '&')
                            pSrc++;
                    }
                    DebugMsg(("ExpandText(): %s is to be replaced by >%s<\n", pIdent, dir->sym.string_ptr));
                    //strcpy( pIdent, dir->sym.string_ptr);
                    GetTextMacroValue( dir->sym.string_ptr, pIdent );
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
                        int savedIfState;
                        pSrc -= strlen(pIdent);
                        if (substitute) {
                            if (*(pIdent-1) == '&')
                                pIdent--;
                        }
                        *pIdent = NULLC;
                        if ( pSave == NULL )
                            pSave = (unsigned char *)AsmTmpAlloc( GetTokenStateSize() );
                        SaveTokenState( pSave );
                        savedIfState = CurrIfState;
                        CurrIfState = 0;
                        //i = RunMacro( dir, pSrc, pIdent, TRUE, TRUE, addbrackets );
                        i = RunMacro( dir, pSrc, pIdent, TRUE, TRUE, FALSE );
                        CurrIfState = savedIfState;
                        DebugMsg(( "ExpandText: back from RunMacro(%s), rc=%u, text returned=>%s<\n", dir->sym.name, i, pIdent ));
                        RestoreTokenState( pSave );
                        if ( i == -1 )
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
    if ( count == MAX_TEXTMACRO_NESTING ) {
        AsmError( MACRO_NESTING_LEVEL_TOO_DEEP );
        return( ERROR );
    }
    DebugMsg(("ExpandText(line=>%s<) exit\n", line));
    return( rc );
}

// replace text macros and macro functions by their values, recursively
// dir: found text macro/macro function item
// pos: index AsmBuffer
// string: full line

static ret_code ExpandTextMacro( dir_node * dir, int pos, char * string, int addbrackets)
/***************************************************************************************/
{
    int count;
    int i;
#if 1
    int start;
    char * p;
    char buffer[MAX_LINE_LEN];
    char buffer2[MAX_LINE_LEN];
    char buffer3[MAX_LINE_LEN];
#endif

    DebugMsg(("%lu. ExpandTextMacro(sym=%s, pos=%u, addbr=%u) enter [tokens=%u]\n", LineNumber, dir->sym.name, pos, addbrackets, Token_Count));

    count = 0;
    for (;;) {
        dir_node *tmpdir;
#if 1
        /* expand macro functions */
        if (p = strstr( dir->sym.string_ptr,"(") ) {
            i = p - dir->sym.string_ptr;
            memcpy( buffer, dir->sym.string_ptr, i );
            buffer[i] = NULLC;
            tmpdir = (dir_node *)SymSearch( buffer );
            if (tmpdir && tmpdir->sym.state == SYM_MACRO) {
                buffer[0] = NULLC;
                /* size of original string */
                count = strlen(AsmBuffer[pos]->string_ptr);
                start = AsmBuffer[pos]->pos - string;
                strcpy(buffer2, string);
                GetTextMacroValue( dir->sym.string_ptr, buffer3 );
                RunMacro(tmpdir, buffer3, buffer, TRUE, TRUE, addbrackets);
                DebugMsg(("ExpandTextMacro: replace >%s< by >%s<\n", dir->sym.string_ptr, buffer));
                memcpy(string, buffer2, start);
                p = string + start;
                strcpy(p, buffer);
                p = p + strlen(p);
                strcpy(p, buffer2+start+count);
                Token_Count = 0;
                return( STRING_EXPANDED );
            } else
                break;
        } else
#endif
        tmpdir = (dir_node *)SymSearch( dir->sym.string_ptr );
//        if (tmpdir && tmpdir->sym.state == SYM_TMACRO && tmpdir->sym.defined == 1) {
        if (tmpdir && tmpdir->sym.state == SYM_TMACRO) {
            dir = tmpdir;
            count++;
            if ( count >= MAX_TEXTMACRO_NESTING ) {
                AsmError( MACRO_NESTING_LEVEL_TOO_DEEP );
                return( ERROR );
            }
            continue;
        }
        break;
    }
    DebugMsg(("%lu. ExpandTextMacros: replace >%s< by >%s<\n", LineNumber, AsmBuffer[pos]->string_ptr, dir->sym.string_ptr));
    RebuildLine(dir->sym.string_ptr,
                pos, string,
                strlen(AsmBuffer[pos]->string_ptr), addbrackets);
    return ( STRING_EXPANDED );
}

// expand one token
// count: index of token in AsmBuffer
// string:

ret_code ExpandToken(int count, char * string, bool addbrackets, bool Equ_Mode)
/*****************************************************************************/
{
    int pos;
    int i;
    char * p;
    expr_list opndx;
    dir_node    *dir;
    ret_code rc = NOT_ERROR;

    if (AsmBuffer[count]->token == T_PERCENT && Equ_Mode == FALSE) {
        pos = count+1;
#if 1
        /* testcase:
         E1  EQU     @SizeStr("ab")
         TE1 TEXTEQU %E1
             %ECHO   TE1
         must display 4. The '%' is superfluous.
         */
        for (i = pos; i < Token_Count && AsmBuffer[i]->token != T_COMMA; i++ )
        if (AsmBuffer[i]->token == T_ID)
            if ( dir = (dir_node *)SymSearch( AsmBuffer[i]->string_ptr ) ) {
                if (dir->sym.defined == FALSE ) {
                    ;
                } else if ( dir->sym.state == SYM_TMACRO ) {
                    if (ExpandTextMacro( dir, i, string, FALSE ) == STRING_EXPANDED) {
                        Token_Count = 0; /* skip processing */
                        return( STRING_EXPANDED );
                    }
                } else if ( dir->sym.state == SYM_MACRO &&
                           AsmBuffer[i+1]->token == T_OP_BRACKET ) {
                    if (ExpandToken( i, string, FALSE, FALSE ) == STRING_EXPANDED ) {
                        Token_Count = 0; /* skip processing */
                        return( STRING_EXPANDED );
                    }
                }
            }
#endif
        if ( EvalOperand( &pos, Token_Count, &opndx, TRUE ) == ERROR ) {
            opndx.value = 0;
        } else if ( opndx.kind != EXPR_CONST ) {
            if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED )
                AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
            else
                AsmError( CONSTANT_EXPECTED );
            opndx.value = 0;
        }
        myltoa( opndx.value, StringBufferEnd, ModuleInfo.radix );
        p = AsmBuffer[count]->pos;
        AsmBuffer[count]->token = T_STRING;
        AsmBuffer[count]->string_ptr = StringBufferEnd;
        AddTokens( count+1, count+1 - pos );
        for ( pos = count+1; AsmBuffer[pos]->token == T_NUM; pos++ );
        if (AsmBuffer[pos]->token == T_FINAL)
            pos = strlen(p);
        else
            pos = AsmBuffer[pos]->pos - p;
        RebuildLine( StringBufferEnd, count, string, pos, addbrackets );
        return( STRING_EXPANDED );
    }

    if( AsmBuffer[count]->token == T_ID ||
        (expansion == TRUE && AsmBuffer[count]->token == T_STRING))  {
        DebugMsg(("%lu. ExpandToken: testing id >%s< equ_mode=%u\n", LineNumber, AsmBuffer[count]->string_ptr, Equ_Mode ));
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
                        return( NOT_ERROR );
                    savedIfState = CurrIfState;
                    CurrIfState = 0;
                    i = RunMacro(dir, AsmBuffer[count]->pos, buffer, TRUE, TRUE, addbrackets);
                    CurrIfState = savedIfState;
                    if ( i != -1 ) {
                        DebugMsg(("%lu. ExpandToken: back from RunMacro(%s), string=>%s<\n", LineNumber, dir->sym.name, buffer));
                        strcat(buffer, cmd + i);
                        strcpy(string, buffer);
                        rc = STRING_EXPANDED;
                    } else
                        rc = ERROR;
                } else {
                    //bool runit = dir->sym.runsync || MacroLevel > 0;
                    if ( count > 0 && AsmBuffer[count-1]->token != T_COLON ) {
                        DebugMsg(("ExpandToken: macro called without brackets at operand location\n"));
                        AsmError( SYNTAX_ERROR );
                        Token_Count = 0;
                        return( ERROR );
                    }
                    /* is runit=FALSE possible at all? Problem is correct
                     * update of global var MacroLevel.
                     */
                    //i = RunMacro( dir, AsmBuffer[count]->pos, buffer, runit, TRUE, FALSE );
                    i = RunMacro( dir, AsmBuffer[count]->pos, buffer, TRUE, TRUE, FALSE );
                    DebugMsg(("%lu. ExpandToken: back from RunMacro(%s) - called as Proc\n", LineNumber, dir->sym.name));
                    if ( i != -1 ) {
                        if ( AsmBuffer[0]->value == T_EXITM ) {
                            strcat(buffer, cmd + i);
                            strcpy(string, buffer);
                            rc = STRING_EXPANDED;
                        } else {
                            /* if the macro didn't run yet, push the current
                             line queue - which contains the macro lines -
                             onto the file stack */
                            //if ( runit == FALSE )
                            //    PushMacro( (asm_sym *)dir );
                            rc = NOT_ERROR;
                        }
                    } else
                        rc = ERROR;
                }
                Token_Count = 0; /* AsmBuffer is destroyed, exit */
//            } else if( dir->sym.state == SYM_TMACRO && dir->sym.defined == TRUE) {
            } else if( dir->sym.state == SYM_TMACRO) {
                DebugMsg(("%lu. ExpandToken: text macro %s to be expanded\n", LineNumber, dir->sym.name ));
                rc = ExpandTextMacro( dir, count, string, addbrackets );
            }
        }
    }
    return( rc );
}

// special handling for SIZESTR and INSTR

static ret_code FullExpandToken( int i, char *string)
/***************************************************/
{
    char buffer[MAX_LINE_LEN];

    if ( AsmBuffer[i]->token == T_ID ) {
        strcpy( buffer, AsmBuffer[i]->string_ptr );
        if (ExpandText( buffer, FALSE, TRUE ) == STRING_EXPANDED ) {
            RebuildLine( buffer, i, string,
                        strlen(AsmBuffer[i]->string_ptr), TRUE );
            AsmBuffer[i]->token = T_STRING;
            return( STRING_EXPANDED );
        }
    }
    return( NOT_ERROR );
}

// scan current line for (text) macros and expand them.

ret_code ExpandLine( char * string)
/*********************************/
{
    int count = 0;
    int addbrackets = FALSE;
    //int Equ_Mode = FALSE;
    asm_sym *sym;
    ret_code rc;
    int i,j;
    //char buffer[MAX_LINE_LEN];

    /* filter certain conditions */
    /* addbrackets: for preprocessor directives which expect a string
     parameter, the string has to be enclosed in '<>' again.
     */
    DebugMsg(( "%lu. ExpandLine(%s) enter\n", LineNumber, string ));
    if ( AsmBuffer[0]->token == T_DIRECTIVE ) {
        if ( AsmBuffer[0]->flags & DF_STRPARM )
            addbrackets = TRUE;
        else if ( AsmBuffer[0]->flags & DF_NOEXPAND ) {
            /* PURGE, IF[N]DEF, .ERR[N]DEF, ECHO, FOR[C]? */
            /* for these directives don't expand strings! */
            if (AsmBuffer[0]->value == T_PURGE || expansion == FALSE)
                return( NOT_ERROR );
        } else if ( AsmBuffer[0]->value == T_OPTION) {
            if ( AsmBuffer[1]->token == T_ID)
                //if ( _stricmp(AsmBuffer[1]->string_ptr, "PROLOGUE") == 0 )
                //    return( NOT_ERROR );
                if ( _stricmp(AsmBuffer[1]->string_ptr, "EPILOGUE") == 0 )
                    return( NOT_ERROR );
        }
    } else if ((Token_Count > 1) && ( AsmBuffer[1]->token == T_DIRECTIVE )) {
        switch (AsmBuffer[1]->value) {
        case T_TEXTEQU:
        case T_CATSTR:
#if 1
            for ( i = 2; i < Token_Count; i++) {
                if ( ExpandToken( i, string, TRUE, FALSE) == STRING_EXPANDED )
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
            for (i = 2, count = 0, j = 0;i < Token_Count; i++)
                if (AsmBuffer[i]->token == T_COMMA) {
                    if ( j == 0 )
                        j = i+1;
                    count++;
                }
            if (count < 2)
                count = 2;
            else {
                count = j;
                rc = ExpandToken( 2, string, FALSE, FALSE);
            }
            if ( FullExpandToken( count, string ) == STRING_EXPANDED)
                return( STRING_EXPANDED );
            addbrackets = TRUE;
            rc = NOT_ERROR;
            goto std_expansion;
        case T_MACRO:
            sym = SymSearch( AsmBuffer[0]->string_ptr );
            /* don't expand macro DEFINITIONs!
             the name is an exception, if it's not the macro itself
             */
            if (sym && sym->state != SYM_MACRO)
                return (ExpandToken( 0, string, FALSE, FALSE ));
            return( NOT_ERROR );
        case T_EQU:
            if ( AsmBuffer[1]->dirtype == DRT_EQUALSGN ) /* ignore '=' directive */
                break;
            /* EQU is a special case. If the - expanded - expression is
             a number, then the value for EQU is numeric. Else the
             expression isn't expanded at all. This effectively makes it
             impossible to expand EQU lines here.
             */
            sym = SymSearch(AsmBuffer[0]->string_ptr);
            if ( sym == NULL || sym->state == SYM_TMACRO ) {
                if (expansion == FALSE) {
                    DebugMsg(( "%lu. ExpandLine(%s) exit\n", LineNumber, string ));
                    return(NOT_ERROR);
                } else {
                    /* expand text macros and macro functions */
                    /* ignore the % before items */
                    /* with "%" as first character, even the equate's name
                     * is to be expanded!
                     */
                    //Equ_Mode = TRUE;
                    count = 2;
                }
            }
        }
    } else if (Token_Count > 2 &&
               AsmBuffer[1]->token == T_COLON &&
               AsmBuffer[2]->token == T_DIRECTIVE ) {
        /* skip the code label */
        if ( AsmBuffer[2]->flags & DF_STRPARM )
            addbrackets = TRUE;
        else if ( AsmBuffer[2]->flags & DF_NOEXPAND )
            if (AsmBuffer[2]->value == T_PURGE || expansion == FALSE) {
                DebugMsg(( "%lu. ExpandLine(%s) exit\n", LineNumber, string ));
                return( NOT_ERROR );
            }
    }

    if (expansion == TRUE) {
//        expansion = FALSE;
        if (STRING_EXPANDED == ExpandText(string, TRUE, addbrackets )) {
            DebugMsg(( "%lu. ExpandLine(%s) exit, STRING_EXPANDED\n", LineNumber, string ));
            return(STRING_EXPANDED);
        }
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
        if (ExpandToken( count, string, addbrackets, FALSE ) == STRING_EXPANDED )
            rc = STRING_EXPANDED;
    }
    DebugMsg(( "%lu. ExpandLine(%s) exit, rc=%u\n", LineNumber, string, rc ));
    return( rc );
}

/* get text macro value */
/* returns no of characters copied into buffer (without terminating 00) */

int GetTextMacroValue( char * p, char * buffer )
/**********************************************/
{
    char *dest = buffer;
    if (p)
        while ( *p ) {
            if (*p == '!' && *(p+1) != NULLC)
                p++;
            *dest++ = *p++;
        }
    *dest = NULLC;
    return( dest - buffer );
}

