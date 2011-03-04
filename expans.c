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
* - ExpandLine        expand a source line
* - GetLiteralValue   get contents of a literal
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "expreval.h"
#include "equate.h"
#include "input.h"
#include "tokenize.h"
#include "fatal.h"
#include "macro.h"
#include "condasm.h"
#include "listing.h"
#include "myassert.h"

#define NEWEXP 0

/* TEVALUE_UNSIGNED
 * 1 = the % operator used in an TEXTEQU expression is supposed to
 *     return an UNSIGNED value ( Masm-compatible ).
 */
#define TEVALUE_UNSIGNED 1

extern bool   expansion;
extern void TextItemError( int );

int           MacroLocals;     /* counter for LOCAL names */
uint_8        MacroLevel;      /* current macro nesting level */

static const char __digits[] = "0123456789ABCDEF";

/* C ltoa() isn't fully compatible since hex digits are lower case.
 * for JWasm, it's ensured that 2 <= radix <= 16.
 */
char *myltoa( uint_32 value, char *buffer, uint radix, bool sign, bool addzero )
/******************************************************************************/
{
    char   *p;
    char   *dst = buffer;
    char   tmpbuf[34];

#ifdef DEBUG_OUT
    uint_32 saved_value = value;
#endif
    tmpbuf[33] = '\0';
    if ( sign ) {
        *dst++ = '-';
         value = 0 - value;
    } else if ( value == 0 ) {
        *dst++ = '0';
        *dst = NULLC;
        return( buffer );
    }
    for ( p = &tmpbuf[32]; value; value = value / radix )
        *p-- = __digits[value % radix];
    if ( addzero && ( *(p+1) > '9') ) /* v2: add a leading '0' if first digit is alpha */
        *p-- = '0';
    strcpy( dst, p + 1 );
    DebugMsg1(("myltoa( value=%" FX32 "h, out=%s, radix=%u, sign=%u, %u)\n", saved_value, buffer, radix, sign, addzero ));
    return( buffer );
}

/* make room (or delete items) in the token buffer */

static void AddTokens( int start, int count )
/*******************************************/
{
    int i;

    if ( count > 0 ) {
        for( i = Token_Count; i >= start; i-- ) {
            *AsmBuffer[i+count] = *AsmBuffer[i];
        }
    } else if ( count < 0 ) {
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

/* Replace placeholders in a stored macro source line with values of actual
 * parameters and locals. A placeholder consists of escape char 0x0a,
 * followed by a one-byte index field.
 */
static void fill_placeholders( char *dst, const char *src, uint argc, uint localstart, char *argv[] )
/***************************************************************************************************/
{
    uint_32 i;
    const char *p;
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

/* Skip macro execution by reading the current queue until it's empty. */

static void SkipMacro( char * buffer )
/************************************/
{
    bool oldlist = ModuleInfo.list;

    DebugMsg1(("SkipMacro() enter\n" ));

    ModuleInfo.list = FALSE; /* suppress listing for these lines */

    buffer = buffer + strlen( buffer) + 1;

    /* when the queue's empty, -1 will be returned */
    while ( GetPreprocessedLine( buffer, TRUE ) >= 0 );

    ModuleInfo.list = oldlist;
    DebugMsg1(("SkipMacro() exit\n" ));
    return;
}
/*
 * Convert a string to a literal (enclosed by <>)
 */
static char * AddBrackets( char *dest, const char *src )
/******************************************************/
{
    *dest++ = '<';
    while (*src) {
        if (*src == '<' || *src == '>' || *src == '!')
            *dest++ = '!';
        *dest++ = *src++;
    }
    *dest++ = '>';
    *dest = NULLC;
    return( dest );
}

/* rebuild a source line
 * adjust all "pos" values behind the current pos
 * - newstring = new value of item i
 * - i = AsmBuffer index of item to replace
 * - outbuf = start of source line to rebuild
 * - oldlen = old length of item i
 * - pos_line = position of item in source line
*/
static ret_code RebuildLine( const char * newstring, int i, char * const outbuf, unsigned oldlen, unsigned pos_line, int addbrackets )
/************************************************************************************************************************************/
{
    char buffer[MAX_LINE_LEN];
    char *dest = buffer;
    unsigned  newlen;
    unsigned  rest = strlen( AsmBuffer[i]->tokpos + oldlen );

    if ( addbrackets ) {
        /* v2.05: changed, using AddBrackets() */
        //*dest++ = '<';
        //memcpy( dest, newstring, newlen );
        //dest += newlen;
        //*dest++ = '>';
        dest = AddBrackets( dest, newstring );
        newlen = dest - buffer;
        if ( newlen > oldlen )
            if ( ( pos_line + newlen - oldlen + rest ) >= MAX_LINE_LEN ) {
                AsmErr( EXPANDED_LINE_TOO_LONG, outbuf );
                return( ERROR );
            }
    } else {
        const char * p = newstring;
        newlen = strlen( newstring );
        if ( newlen > oldlen )
            if ( ( pos_line + newlen - oldlen + rest ) >= MAX_LINE_LEN ) {
                AsmErr( EXPANDED_LINE_TOO_LONG, outbuf );
                return( ERROR );
            }
        while (*p) {
            if ( *p == '!' && *(p+1) != NULLC ) {
                p++;
                newlen--;
            }
            *dest++ = *p++;
        }
    }
    strcpy( dest, AsmBuffer[i]->tokpos + oldlen ); /* concat rest of line */
    strcpy( outbuf, buffer );     /* and finally copy it back */

    /* v2.05: changed '<' to '<=' */
    for ( i++; i <= Token_Count; i++) {
        AsmBuffer[i]->tokpos = AsmBuffer[i]->tokpos - oldlen + newlen;
    }

    return( STRING_EXPANDED );
}

/*
 * ExpandText() is called if
 * - the evaluation operator '%' has been found as first char of the line.
 * - for CATSTR, SUBSTR, SIZESTR (and INSTR?) parameters
 * Then do expansion within strings!
 * if substitute is TRUE, scanning for the substitution character '&' is active!
 * Both text macros and macro functions are expanded!
 */

static ret_code ExpandText( char * line, int substitute )
/*******************************************************/
{
    char *pSrc;
    char *pDst;
    char *pIdent;
    bool is_exitm;
    //char *pStart;
    unsigned char *pSave = NULL;
    ret_code rc = NOT_ERROR;
    int count;
    bool expanded = TRUE;
    dir_node * dir;
    char srcline[MAX_LINE_LEN];

    DebugMsg(("ExpandText(line=>%s<, subst=%u ) enter\n", line, substitute ));
    for ( count = 0; count < MAX_TEXTMACRO_NESTING && expanded; count++ ) {
        strcpy( srcline, line );
        expanded = FALSE;
        pDst = line;
        for ( pSrc = srcline; *pSrc ; ) {
            if( is_valid_id_first_char( *pSrc ) ) {
                pIdent = pDst;
                do {
                    *pDst++ = *pSrc++;
                } while ( is_valid_id_char( *pSrc ));
                *pDst = NULLC;
                dir = (dir_node *)SymSearch( pIdent );
#ifdef DEBUG_OUT
                if ( dir && ( dir->sym.state == SYM_TMACRO || dir->sym.state == SYM_MACRO ) ) {
                    DebugMsg(( "ExpandText: symbol found: %s, state=%u, defined=%u, *pDst-1=%c\n", dir->sym.name, dir->sym.state, dir->sym.isdefined, *(pDst-1) ));
                }
#endif
                if ( dir &&
                     dir->sym.state == SYM_TMACRO &&
                     dir->sym.isdefined == TRUE ) {
#if 1
                    /* if there's an evaluation operator (%) before the
                     text macro name, skip it. This is to be improved!
                     */
                    //if (pIdent > line && *(pIdent-1) == '%')
                    /* see sample CatStr9.asm: %ifidn <abc>,<%TE1> */
                    if ( pIdent > line && *(pIdent-1) == '%' && expansion == FALSE )
                        pIdent--;
#endif
                    if ( substitute ) {
                        if ( *(pIdent-1) == '&' )
                            pIdent--;
                        if ( *pSrc == '&' )
                            pSrc++;
                    }
                    DebugMsg(("ExpandText(): %s is to be replaced by >%s<\n", pIdent, dir->sym.string_ptr));
                    //strcpy( pIdent, dir->sym.string_ptr);
                    GetLiteralValue( pIdent, dir->sym.string_ptr );
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
                     dir->sym.isdefined == TRUE &&
                     dir->sym.isfunc == TRUE) {
                    char * p = pSrc;
                    int i;
                    while ( isspace(*p) ) p++;
                    /* no macro function invokation if the '(' is missing! */
                    if (*p == '(') {
                        if_state savedIfState;
                        pSrc -= strlen(pIdent);
                        if ( substitute ) {
                            if ( *(pIdent-1) == '&' )
                                pIdent--;
                        }
                        *pIdent = NULLC;
                        if ( pSave == NULL )
                            pSave = (unsigned char *)AsmTmpAlloc( GetTokenStateSize() );
                        SaveTokenState( pSave );
                        savedIfState = CurrIfState;
                        CurrIfState = BLOCK_ACTIVE;
                        //i = RunMacro( dir, pSrc, pIdent, TRUE, addbrackets );
                        i = RunMacro( dir, pSrc, pIdent, TRUE, FALSE, &is_exitm );
                        CurrIfState = savedIfState;
                        DebugMsg(( "ExpandText: back from RunMacro(%s), rc=%u, text returned=>%s<\n", dir->sym.name, i, pIdent ));
                        RestoreTokenState( pSave );
                        if ( i == -1 )
                            return( ERROR );
                        pSrc += i;
                        pDst = pIdent + strlen( pIdent );
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

#define TMPSTACKALLOC 0

/* run a macro
 * macro: macro item
 * params: parameter string (includes macro name!)
 * prefix: line prefix which should be emitted "first".
 *         this is also the output buffer!
 * insert: 1=call PushLineQueue() (0 is obsolete!)
 * addbrackets: 1=enclose result string in <>
 * returns number of characters processed or -1 on errors
 */
int RunMacro( dir_node *macro, char *params, char *prefix, int insert, int addbrackets, bool *is_exitm )
/******************************************************************************************************/
{
    char        *newline;
    char        *orgsrc = params;
    macro_info  *info;
    struct asmlines *lnode;
    dir_node    *dir;
    expr_list   opndx;
    int         i;
    int         parmidx;
    int         varargcnt;
    int         bracket_level = -1;/* () level */
    uint        localstart;
    char        parm_end_delim;   /* parameter end delimiter */
    char        *ptr;
    char        **parm_array = NULL;
    char        line[MAX_LINE_LEN];

    DebugMsg1(("RunMacro(%s, src=>%s<, prf=>%s<, insert=%u) enter, lvl=%u, locals=%04u\n", macro->sym.name, params, prefix, insert, MacroLevel, MacroLocals ));

    /* v2.03: ensure the "next" source ptr is aligned */
    newline = CurrSource + (( strlen( CurrSource ) + 1 + 3 ) & ~3);

    info = macro->e.macroinfo;
#ifdef DEBUG_OUT
    info->count++;
#endif
    params += macro->sym.name_size; /* skip the macro name */
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

    DebugMsg1(( "RunMacro(%s): params=>%s< \n", macro->sym.name, params ));

    if ( info->parmcnt )
        parm_array = (char **)AsmTmpAlloc( info->parmcnt * sizeof( char * ) );

    /* skip white spaces (in case there are no params at all) */
    while ( isspace(*params) ) params++;

    /* now get all the parameters from the original src line.
     * macro parameters are expanded if
     * - it is a macro function call            or
     * - the expansion operator (%) is found
     */

    parmidx = 0;
#if MACROLABEL
    if ( macro->sym.label ) {
        i = strlen( prefix );
        parm_array[parmidx] = (char *)AsmTmpAlloc( i + 1 );
        memcpy( parm_array[0], prefix, i + 1 );
        ptr = parm_array[0];
        while ( i && isspace(*(ptr+i-1))) i--;
        *(ptr + i) = NULLC;
        parmidx++;
    }
#endif

    *is_exitm = FALSE;

    for( varargcnt = 0; parmidx < info->parmcnt; parmidx++ ) {

        if ( ( *params == NULLC || *params == parm_end_delim || *params == ',' ) && varargcnt == 0 ) {

            /* it's a blank parm */
            if( info->parmlist[parmidx].required ) {
                DebugMsg1(( "RunMacro(%s.%s), parameter %u required >%s<\n", macro->sym.name, parmidx, params ));
                AsmErr( MISSING_MACRO_ARGUMENT, macro->sym.name, parmidx + 1 );
                return( -1 );
            }
            parm_array[parmidx] = info->parmlist[parmidx].deflt;

        } else {

            char * startid = NULL;
            int  str_level = 0;    /* <> level */
            char  *dst;
            char  *tmpparams;
            uint_8 expansion_flag = FALSE;   /* % operator found */
            char *start_expr;
            int  numex;
            char quote_ch = NULLC;

            *newline = NULLC;
            ptr = newline;

            DebugMsg1(( "RunMacro(%s.%u), >%s<\n", macro->sym.name, parmidx, params ));

            /* the % operator can be inside a string enclosed in <>! */
            if (*params == '<') {
                str_level++;
                params++;
            }

            while ( *params != NULLC ) {
                if  ( *params == ',' && str_level == 0 )
                    break;

                if ( quote_ch == NULLC )
                    if ( *params == '<' ) {
                        str_level++;
                        if ( str_level == 1 ) {
                            params++;
                            continue;
                        }
                        //} else if ( *params == '>' ) { changed in v2.0
                    } else if ( *params == '>' && str_level ) {
                        str_level--;
                        if ( str_level == 0 ) {
                            params++;
                            continue;
                        }
                    } else if ( *params == '!' && *(params+1) != NULLC ) {
                        params++;
                        *ptr++ = *params++;
                        continue;
                    } else if ( *params == '%' ) {
                        /* handle expansion of macro parameters.
                         * the expansion operator does
                         * - run a macro function
                         * - expand a text macro
                         * - store the result of an expression as text
                         * it is valid for the whole parameter (until a comma is found)
                         */
                        expansion_flag = TRUE;
                        params++;
                        while ( isspace(*params) ) params++;
                        start_expr = ptr;
                        numex = 0;
                        continue;
                    }

                if ( *params == '"' || *params == '\'' ) {
                    if ( quote_ch == NULLC ) {
                        /* Open string */
                        quote_ch = *params;
                    }
                    else if ( quote_ch == *params ) {
                        /* Close string */
                        quote_ch = NULLC;
                    }
                }

                // if (*params == '"' || *params == '\'') {
                if ( str_level == 0 && ( *params == '"' || *params == '\'' ) ) {
                    char startc = *params;
                    *ptr++ = *params++;
                    while ( *params ) {
                        *ptr++ = *params++;
                        if ( *(params-1) == startc )
                            if ( *params == startc )
                                *ptr++ = *params++;
                            else
                                break;
                    }
                    startid = NULL;
                    continue;
                }

                /* skip argument's trailing spaces */
                if ( str_level == 0 && isspace( *params ) ) {
                    char * ptr2 = params+1;
                    while ( isspace(*ptr2) ) ptr2++;
                    if ( *ptr2 == ',' || *ptr2 == parm_end_delim ) {
                        params++;
                        continue;
                    }
                }

                if ( startid == NULL && is_valid_id_first_char(*params) == TRUE )
                    startid = ptr;

                if ( bracket_level > 0 ) {
                    if ( *params == '(' ) {
                        bracket_level++;
                    } else if ( *params == ')' ) {
                        bracket_level--;
                        if ( bracket_level == 0 )
                            break;
                    }
                }
                if ( expansion_flag && startid == NULL && isspace( *params ) == FALSE )
                    numex++;

                *ptr++ = *params++;

                /* check for (text) macros */

                if ( ( str_level == 0 || ( expansion_flag == TRUE && str_level <= 1 )) &&
                    is_valid_id_char( *params ) == FALSE &&
                    startid != NULL ) {

                    *ptr = NULLC;
                    if ( expansion_flag == TRUE ) {
                        /* v2.03: Source + IfState saved/restored */
                        char *savedCurrSource = CurrSource;
                        if_state savedIfState = CurrIfState;
                        CurrSource = newline;
                        CurrIfState = BLOCK_ACTIVE;
                        tmpparams = params;
                        strcpy( line, startid );
                        dst = line + strlen( line );
                        while ( isspace(*tmpparams) ) tmpparams++;
                        if ( *tmpparams == '(' ) {
                            int lvl = 1;
                            *dst++ = *tmpparams++;
                            while (lvl && *tmpparams ) {
                                if ( *tmpparams == '(' )
                                    lvl++;
                                else if ( *tmpparams == ')')
                                    lvl--;
                                *dst++ = *tmpparams++;
                            }
                        } else /* v2.04: don't swallow white spaces */
                            tmpparams = params;
                        *dst = NULLC;
                        if ( ExpandText( line, FALSE ) == STRING_EXPANDED ) {
                            DebugMsg1(( "RunMacro(%s.%u), ExpandText()=>%s<\n", macro->sym.name, parmidx, line ));
                            strcpy( startid, line );
                            ptr = startid + strlen( startid );
                            params = tmpparams;
                        } else {
                            numex++;
                            DebugMsg1(( "RunMacro(%s.%u), numex=%u, ExpandText()=>%s<\n", macro->sym.name, parmidx, numex, line ));
                        }
                        CurrIfState = savedIfState;
                        CurrSource = savedCurrSource;
                    } else {

                        dir = (dir_node *)SymSearch( startid );
                        if (dir &&
                            dir->sym.state == SYM_MACRO &&
                            dir->sym.isdefined == TRUE &&
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
                                if_state savedIfState = CurrIfState;
                                bool is_exitm2;
                                CurrSource = newline;
                                CurrIfState = BLOCK_ACTIVE;
                                params -= strlen( startid );
                                ptr -= strlen( startid );
                                *ptr = NULLC;
                                line[0] = NULLC;
                                i = RunMacro( dir, params, line, TRUE, FALSE, &is_exitm2 );
                                DebugMsg1(("RunMacro(%s.%u): back from RunMacro(%s), rc=%u, buffer=>%s<\n", macro->sym.name, parmidx, dir->sym.name, i, line));
                                CurrIfState = savedIfState;
                                CurrSource = savedCurrSource;
                                if ( i == -1 )
                                    return( -1 );
                                strcpy( ptr, line );
                                ptr += strlen( ptr );
                                params = params + i;
                            }
                        }
                    }
                    startid = NULL;
                } /* end if (text) macro */
            } /* end while */

            *ptr = NULLC;

            /* convert numeric expression into a string? */
            if ( expansion_flag && numex ) {
                DebugMsg1(( "RunMacro(%s.%u), num expansion: >%s<\n", macro->sym.name, parmidx, start_expr ));
                Token_Count = Tokenize( start_expr, 1, FALSE );

                i = 1;
                if ( EvalOperand( &i, Token_Count, &opndx, 0 ) != ERROR ) {
                    DebugMsg1(( "RunMacro(%s.%u): num expansion, opndx.type=%d, value=%d\n", macro->sym.name, parmidx, opndx.type, opndx.value ));
                    /* the expression evaluator accepts forward references
                     but the % operator won't accept them */
                    if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
                        if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED ) {
                            AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
                        } else {
                            AsmError( EXPRESSION_NOT_A_CONSTANT );
                        }
                    } else if ( AsmBuffer[i]->token != T_FINAL ) {
                        /* the evaluator was unable to evaluate the full expression */
                        DebugMsg1(( "RunMacro(%s.%u): num expansion, unexpected token=%s\n", macro->sym.name, parmidx, AsmBuffer[i]->string_ptr ));
                        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->tokpos );
                    } else
                        myltoa( opndx.uvalue, start_expr, ModuleInfo.radix, opndx.hvalue < 0, FALSE );
                }
            }

            if (  macro->sym.mac_vararg && ( parmidx == info->parmcnt - 1 ) ) {
#if TMPSTACKALLOC
                while ( *params != NULLC && *params != parm_end_delim && *params != ',' )
                    *ptr++ = *params++;
                if ( *params == ',' )
                    *ptr++ = *params;
                *ptr = NULLC;
                if ( varargcnt == 0 ) {
                    parm_array[parmidx] = (char *)AsmTmpAlloc( MAX_LINE_LEN + 1 );
                    strcpy( parm_array[parmidx], newline );
                } else
                    strcat( parm_array[parmidx], newline );
                varargcnt++;
#else
                while ( *params != NULLC && *params != parm_end_delim && *params != ',' )
                    *ptr++ = *params++;
                if ( *params == ',' )
                    *ptr++ = *params;
                *ptr = NULLC;
                if ( varargcnt == 0 )
                    parm_array[parmidx] = newline;
                varargcnt++;
                newline = ptr;
#endif
            } else if ( *newline ) {
#if TMPSTACKALLOC
                parm_array[parmidx] = (char *)AsmTmpAlloc( strlen( newline ) + 1 );
                strcpy( parm_array[parmidx],  newline );
#else
                parm_array[parmidx] = newline;
                newline = newline + (( strlen( newline ) + 1 + 3 ) & ~3 );
#endif
            } else
                parm_array[parmidx] = "";
        } /*end if */

#ifdef DEBUG_OUT
        if ( parm_array[parmidx] )
            DebugMsg1(("RunMacro(%s.%u): actual parameter value=>%s<\n", macro->sym.name, parmidx, parm_array[parmidx] ));
        else
            DebugMsg1(("RunMacro(%s.%u): actual parameter value=NULL\n", macro->sym.name, parmidx ));
#endif

        if ( *params == ',' ) {
            params++;
            while (isspace(*params)) params++;
            if ( macro->sym.mac_vararg && ( parmidx == info->parmcnt - 1 ) && *params )
                parmidx--;
        }
    } /* end for  */

    if ( bracket_level >= 0 ) {
        if ( *params == NULLC ) {
            DebugMsg1(("RunMacro(%s): missing ')'\n", macro->sym.name));
            AsmError( MISSING_RIGHT_PARENTHESIS );
            return( -1 );
        } else if ( *params != ')' ) {
            DebugMsg1(("RunMacro(%s): expected ')', found >%s<\n", macro->sym.name, params));
            AsmErr( TOO_MANY_ARGUMENTS_IN_MACRO_CALL, macro->sym.name);
            return( -1 );
        } else
            params++;

        /* if macro name is "", it's a FOR/FORC macro.
         a check for a valid end must NOT be done then. */
    } else if ( *params != '\0' && *macro->sym.name != NULLC ) {
        DebugMsg1(("RunMacro(%s): expected NULL, found >%s<\n", macro->sym.name, params));
        /* v2.05: changed to a warning. That's what Masm does */
        AsmWarn( 1, TOO_MANY_ARGUMENTS_IN_MACRO_CALL, macro->sym.name );
        //return( -1 );
    }

    /* a predefined macro func with a function address? */

    if ( macro->sym.predefined == TRUE && macro->sym.func_ptr != NULL ) {
        line[0] = NULLC;
        macro->sym.func_ptr( line, parm_array );
        if ( addbrackets ) {
            AddBrackets( prefix + strlen( prefix ), line );
        } else
            strcat( prefix, line );
        *is_exitm = TRUE;
        return( params - orgsrc );
    }

#if 0
    /* v2.0: no ENDM is pushed anymore,
     * but exit RunMacro() prematurely seems a bad idea!
     * See 'AddLineQueue( prefix )' below!
     */
    if ( info->data == NULL )
        return( params - orgsrc );
#endif

    if ( insert ) {
        PushLineQueue(); /* start a new line queue */
    }

    /* if there's a label before the macro, write it */
    if ( macro->sym.isfunc == FALSE && prefix ) {
        for ( ptr = prefix; *ptr && isspace(*ptr); ptr++ );
        if ( *ptr ) { /* v2.03: don't queue an empty prefix */
            DebugMsg1(("RunMacro(%s): isfunc=FALSE, prefix >%s< queued\n", macro->sym.name, prefix ));
#if MACROLABEL
            if ( macro->sym.label == FALSE )
#endif
                AddLineQueue( prefix );
            *prefix = NULLC; /* added v1.96 */
        }
    }
    localstart = MacroLocals;
    MacroLocals += info->localcnt; /* adjust global variable MacroLocals */

    /* emit the macro's source lines */
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

    /* v2.03: no processing if line queue is empty */
    if ( line_queue  ) {
        char *OldSource = CurrSource;
        int currlevel = queue_level;
//        line_queue->tail->macrolevel = MacroLevel+1;
        DebugMsg1(("RunMacro(%s): enter assembly loop, macro level=%u\n", macro->sym.name, MacroLevel+1 ));
        /* v2.04: this listing is too excessive */
        //if ( ModuleInfo.list && ( ModuleInfo.list_macro == LM_LISTMACROALL || MacroLevel == 0 ) )
        if ( MacroLevel == 0 && macro->sym.isfunc == FALSE && *macro->sym.name )
            LstWriteSrcLine();
        /*
         * move the current line queue to the file stack!
         * Also reset the current linenumber!
         */
        PushMacro( (asm_sym *)macro, 0 );
        MacroLevel++;
        /* Run the assembler until we hit EXITM or ENDM.
         * Also handle GOTO and macro label lines!
         */
        while ( queue_level >= currlevel ) {
            int i;
            while ( 0 == (i = GetPreprocessedLine( newline, FALSE )) );
            if ( i < 0 ) {
                break;
            }
            /* skip macro label lines */
            if ( AsmBuffer[0]->token == T_COLON ) {
                /* v2.05: emit the error msg here, not in StoreMacro() */
                if ( AsmBuffer[1]->token != T_ID )
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->tokpos );
                else if ( AsmBuffer[2]->token != T_FINAL )
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[2]->tokpos );
                continue;
            }

            if ( AsmBuffer[0]->token == T_DIRECTIVE ) {
                if ( AsmBuffer[0]->value == T_EXITM ) {
                    if ( ModuleInfo.list && ModuleInfo.list_macro == LM_LISTMACROALL )
                        LstWriteSrcLine();
                    /* v2.05: display error if there's more than 1 argument or
                     * the argument isn't a text item
                     */
                    if ( AsmBuffer[1]->token != T_FINAL ) {
                        if ( AsmBuffer[1]->token != T_STRING || AsmBuffer[1]->string_delim != '<' )
                            TextItemError( 1 );
                        else if ( Token_Count > 2 )
                            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[2]->tokpos );
                    }
                    if ( addbrackets ) {
                        AddBrackets( line, AsmBuffer[1]->string_ptr );
                    } else {
                        line[0] = NULLC;
                        if ( AsmBuffer[1]->token != T_FINAL ) {
                            char *p = AsmBuffer[1]->string_ptr;
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
                    DebugMsg1(("RunMacro(%s): EXITM, prefix=%s, exitm result=%s, suffix=%s\n",
                               macro->sym.name, prefix ? prefix : "NULL", line, params));
                    if ( prefix ) { /* prefix may be NULL ( loop directives ) */
                        strcat( prefix, line );
                    }
                    SkipMacro( newline );
                    *is_exitm = TRUE;
                    break;
#if 0 /* won't happen anymore */
                } else if ( AsmBuffer[0]->value == T_ENDM ) {
                    DebugMsg1(("RunMacro(%s): ENDM\n", macro->sym.name ));
                    break;
#endif
                } else if ( AsmBuffer[0]->value == T_GOTO ) {
                    if ( AsmBuffer[1]->token != T_FINAL ) {
                        int len = strlen( AsmBuffer[1]->string_ptr );
                        DebugMsg1(("RunMacro(%s): GOTO %s, MacroLevel=%u\n", macro->sym.name, AsmBuffer[1]->string_ptr, MacroLevel ));
                        for( i = 1, lnode = info->data; lnode != NULL; lnode = lnode->next, i++ ) {
                            ptr = lnode->line;
                            //DebugMsg(("RunMacro(%s): GOTO, scan line >%s< for label >%s<\n", macro->sym.name, ptr, line));
                            if ( *ptr == ':' ) {
                                if ( lnode->ph_count ) {
                                    fill_placeholders( line, lnode->line, info->parmcnt, localstart, parm_array );
                                    ptr = line;
                                }
                                ptr++;
                                while( isspace( *ptr )) ptr++;
                                DebugMsg1(("RunMacro(%s): GOTO, line=>%s<\n", macro->sym.name, ptr ));
                                /* macro labels are always case-insensitive! */
                                //if ( ( SymCmpFunc( ptr, AsmBuffer[1]->string_ptr, len ) == 0 ) &&
                                if ( ( _memicmp( ptr, AsmBuffer[1]->string_ptr, len ) == 0 ) &&
                                    ( is_valid_id_char(*(ptr+len) ) == FALSE ) ) {
                                    /* label found! */
                                    break;
                                }
                            }
                        }
                        if ( !lnode ) {
                            /* v2.05: display error msg BEFORE SkipMacro()! */
                            DebugMsg1(("RunMacro(%s): GOTO, label >%s< not found!\n", macro->sym.name, AsmBuffer[1]->string_ptr ));
                            AsmErr( MACRO_LABEL_NOT_DEFINED, AsmBuffer[1]->string_ptr );
                        }
                    } else {
                        lnode = NULL;
                        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->tokpos );
                    }
                    SkipMacro( newline );
                    /* v2.05: MacroLevel isn't touched anymore inside the loop */
                    //MacroLevel--;
                    if ( lnode && lnode->next ) {
                        DebugMsg1(("RunMacro(%s): GOTO, found label >%s<\n", macro->sym.name, line));
                        //PushLineQueue();
                        for( lnode = lnode->next; lnode != NULL; lnode = lnode->next ) {
                            if ( lnode->ph_count ) {
                                fill_placeholders( line, lnode->line, info->parmcnt, localstart, parm_array );
                                AddLineQueue( line );
                            } else
                                AddLineQueue( lnode->line );
                        }
                        PushMacro( (asm_sym *)macro, i );
                        continue;
                    }
                    break;
                }
            }
            ParseLine();
            if ( Options.preprocessor_stdout == TRUE )
                WritePreprocessedLine( newline );

            /* the macro might contain an END directive */
            if ( ModuleInfo.EndDirFound ) {
                while ( GetTextLine( newline, MAX_LINE_LEN ) );
                *is_exitm = TRUE; /* force loop exit */
                break;
            }
        } /* end while */
        MacroLevel--;
        CurrSource = OldSource;
    } /* end if */

    DebugMsg1(("RunMacro(%s) exit, MacroLevel=%u\n", macro->sym.name, MacroLevel ));

    return( params - orgsrc );
}

/* replace text macros and macro functions by their values, recursively
 * dir: found text macro/macro function item
 * pos: item's AsmBuffer index
 * outbuf: output buffer
 * equmode: if 1, don't expand macro functions
 */
static ret_code ExpandTMacro( dir_node * dir, int pos, char * const outbuf, int addbrackets, int equmode )
/********************************************************************************************************/
{
    int count;
    int i;
    //int len;
    bool is_exitm;
    dir_node *tmpdir;
    char * p;
    char *src;
    char *dst;
    char buffer[MAX_LINE_LEN];
    char buffer2[MAX_LINE_LEN];
    char buffer3[MAX_LINE_LEN];
    char buffer4[MAX_LINE_LEN];

    DebugMsg1(("ExpandTMacro(sym=%s, pos=%u, addbr=%u) enter [tokens=%u]\n", dir->sym.name, pos, addbrackets, Token_Count));

    count = 0;
    /* v2.05: using GetLiteralValue() now */
    if ( addbrackets ) {
        GetLiteralValue( buffer4, dir->sym.string_ptr );
        dst = buffer4;
    } else
        dst = dir->sym.string_ptr;
    for ( ;; ) {
        src = dst;
        if ( !is_valid_id_first_char( *src ) )
            break;
        p = buffer;
        do {
            *p++ = *src++;
        } while ( is_valid_id_char( *src ) );
        *p = NULLC;
        tmpdir = (dir_node *)SymSearch( buffer );
        if ( tmpdir == NULL )
            break;

        /* expand macro functions */
        if ( tmpdir->sym.state == SYM_MACRO && *src == '(' && equmode == FALSE ) {
            i = src - dst;
            buffer[0] = NULLC;
            /* size of original string */
            count = strlen( AsmBuffer[pos]->string_ptr );
            strcpy( buffer2, AsmBuffer[pos]->tokpos + count );

            GetLiteralValue( buffer3, dst );

            //i = RunMacro( tmpdir, buffer3, buffer, TRUE, addbrackets );
            i = RunMacro( tmpdir, buffer3, buffer, TRUE, FALSE, &is_exitm );
            DebugMsg1(("ExpandTMacro(+%s): replace >%s< by >%s<\n", tmpdir->sym.name, dir->sym.string_ptr, buffer));
            /* v2.01: don't forget possible chars in the text macro
             * string behind the ')'! */
            if ( i != -1 )
                strcat( buffer, buffer3 + i );
            if ( addbrackets )
                AddBrackets( outbuf, buffer );
            else
                strcpy( outbuf, buffer );
            strcat( outbuf, buffer2 );
            Token_Count = 0;
            return( STRING_EXPANDED );
        } else if ( tmpdir->sym.state == SYM_TMACRO ) {
            dir = tmpdir;
            count++;
            if ( count >= MAX_TEXTMACRO_NESTING ) {
                AsmError( MACRO_NESTING_LEVEL_TOO_DEEP );
                return( ERROR );
            }
            dst = tmpdir->sym.string_ptr;
            if ( *src ) { /* anything behind the text macro ID? */
                char tmpbuf[MAX_LINE_LEN];
                strcpy( tmpbuf, dst );
                strcat( tmpbuf, src );
                dst = tmpbuf;
            }
            /* v2.05: using GetLiteralValue() now */
            if ( addbrackets )
                GetLiteralValue( buffer4, dst );
            else
                strcpy( buffer4, dst );
            dst = buffer4;
            DebugMsg1(("ExpandTMacro(-%s): lvl=%u, current string=%s src=%s\n", tmpdir->sym.name, count, dst, src ));
            continue;
        }
        break;
    }
    DebugMsg1(("ExpandTMacro: replace >%s< by >%s<\n", AsmBuffer[pos]->string_ptr, dir->sym.string_ptr));
    return( RebuildLine( dst,
                pos,
                AsmBuffer[pos]->tokpos,
                strlen( AsmBuffer[pos]->string_ptr ),
                AsmBuffer[pos]->tokpos - outbuf,
                addbrackets ) );
}

/* expand one token
 * count: index of token in AsmBuffer
 * line: full source line
 * equmode: if 1, dont expand macro functions
 */
static ret_code ExpandToken( int count, char *line, int addbrackets, int equmode )
/********************************************************************************/
{
    int pos;
    int i;
    //char * p;
    bool is_exitm;
    expr_list opndx;
    dir_node    *dir;
    ret_code rc = NOT_ERROR;

    /* v2.05: the '%' should only be handled as an operator if addbrackets==TRUE,
     * which means that the current directive is a preprocessor directive and the
     * expected argument is a literal (or text macro).
     */
    //if ( AsmBuffer[count]->token == T_PERCENT && equmode == FALSE ) {
    if ( addbrackets && AsmBuffer[count]->token == T_PERCENT ) {
        pos = count+1;
        DebugMsg1(("ExpandToken: %% found, line=%s\n", AsmBuffer[pos]->tokpos ));

        /* if a text macro/macro function call is found, it has to be
         * evaluated BEFORE the expression evaluator is called!
         * testcase:
         * E1  EQU     @SizeStr("ab")
         * TE1 TEXTEQU %E1
         *     %ECHO   TE1
         * must display 4. The '%' is superfluous.
         */
        for ( i = pos; i < Token_Count && AsmBuffer[i]->token != T_COMMA; i++ )
            if ( AsmBuffer[i]->token == T_ID )
                if ( dir = (dir_node *)SymSearch( AsmBuffer[i]->string_ptr ) ) {
                    if (dir->sym.isdefined == FALSE ) {
                        ;
                    } else if ( dir->sym.state == SYM_TMACRO ) {
                        if ( ExpandTMacro( dir, i, AsmBuffer[i]->tokpos, FALSE, FALSE ) == STRING_EXPANDED ) {
                            Token_Count = 0; /* skip processing */
                            return( STRING_EXPANDED );
                        }
                    } else if ( dir->sym.state == SYM_MACRO &&
                               AsmBuffer[i+1]->token == T_OP_BRACKET ) {
                        if ( ExpandToken( i, line, FALSE, FALSE ) == STRING_EXPANDED ) {
                            Token_Count = 0; /* skip processing */
                            return( STRING_EXPANDED );
                        }
                    }
                }

        if ( EvalOperand( &pos, Token_Count, &opndx, 0 ) == ERROR ) {
            opndx.value = 0;
        } else if ( opndx.kind != EXPR_CONST ) {
            if ( opndx.sym && opndx.sym->state == SYM_UNDEFINED )
                AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
            else {
                DebugMsg(("ExpandToken: 'constant expected' error\n"));
                AsmError( CONSTANT_EXPECTED );
            }
            opndx.value = 0;
        }
#if TEVALUE_UNSIGNED
        /* v2.03: Masm compatible: returns an unsigned value */
        myltoa( opndx.value, StringBufferEnd, ModuleInfo.radix, FALSE, FALSE );
#else
        myltoa( opndx.value, StringBufferEnd, ModuleInfo.radix, opndx.hvalue < 0, FALSE );
#endif
        /* v2.05: get size of string to be "replaced" */
        i = AsmBuffer[pos]->tokpos - AsmBuffer[count]->tokpos;
        DebugMsg1(("ExpandToken: curr pos=%u, start expr=%u, expr size=%d\n", pos, count+1, i ));

        AsmBuffer[count]->token = T_STRING;
        AsmBuffer[count]->string_ptr = StringBufferEnd;
        AddTokens( count+1, count+1 - pos );

        return( RebuildLine( StringBufferEnd,
                    count,
                    AsmBuffer[count]->tokpos,
                    i,
                    AsmBuffer[count]->tokpos - line,
                    addbrackets ) );
    }

    //if( AsmBuffer[count]->token == T_ID ||
        //( expansion == TRUE && AsmBuffer[count]->token == T_STRING ) )  {
    if( AsmBuffer[count]->token == T_ID ) {
        DebugMsg1(("ExpandToken: testing id >%s< equmode=%u\n", AsmBuffer[count]->string_ptr, equmode ));
        dir = (dir_node *)SymSearch( AsmBuffer[count]->string_ptr );
        if( dir != NULL) {
            if (dir->sym.state == SYM_MACRO &&
                dir->sym.isdefined == TRUE &&
                equmode == FALSE ) {
                char buffer[MAX_LINE_LEN];
                char cmd[MAX_LINE_LEN];
                memcpy( buffer, line, AsmBuffer[count]->tokpos - line );
                buffer[AsmBuffer[count]->tokpos - line] = NULLC;
                strcpy( cmd, AsmBuffer[count]->tokpos );
                if ( dir->sym.isfunc == TRUE ) {
                    if_state savedIfState;
                    /* ignore macro functions without a following '(' */
                    if ( AsmBuffer[count+1]->token != T_OP_BRACKET ) {
                        DebugMsg1(("ExpandToken: macro function %s without () - not expanded!\n", dir->sym.name ));
                        return( NOT_ERROR );
                    }
                    DebugMsg1(("ExpandToken: macro function %s to be expanded\n", dir->sym.name ));
                    savedIfState = CurrIfState;
                    CurrIfState = BLOCK_ACTIVE;
                    i = RunMacro( dir, AsmBuffer[count]->tokpos, buffer, TRUE, addbrackets, &is_exitm );
                    CurrIfState = savedIfState;
                    if ( i != -1 ) {
                        DebugMsg1(("ExpandToken: back from RunMacro(%s), string=>%s<\n", dir->sym.name, buffer));
                        strcat( buffer, cmd + i );
                        strcpy( line, buffer );
                        rc = STRING_EXPANDED;
                    } else
                        rc = ERROR;
                } else {
                    /* v2.05: check for T_DIRECTIVE added (option epilogue) */
                    if ( AsmBuffer[0]->token == T_DIRECTIVE ||
                        ( count > 0 &&
#if MACROLABEL
                        dir->sym.label == FALSE &&
#endif
                        /* v2.05: regression in v2.04 */
                        //AsmBuffer[count-1]->token != T_COLON ) {
                        AsmBuffer[count-1]->token != T_COLON &&
                        AsmBuffer[count-1]->token != T_DBL_COLON )) {
                        DebugMsg1(("ExpandToken: macro proc %s at pos %u NOT expanded\n", dir->sym.name, count ));
#if 1 /* v2.03: no error, just don't expand! */
                        return( rc );
#else
                        AsmErr( SYNTAX_ERROR_EX, dir->sym.name );
                        Token_Count = 0;
                        return( ERROR );
#endif
                    }
                    DebugMsg1(("ExpandToken: macro proc %s to be expanded\n", dir->sym.name ));
                    i = RunMacro( dir, AsmBuffer[count]->tokpos, buffer, TRUE, FALSE, &is_exitm );
                    DebugMsg1(("ExpandToken: back from RunMacro(%s) - called as proc\n", dir->sym.name));
                    if ( i != -1 ) {
                        if ( is_exitm ) {
                            strcat( buffer, cmd + i );
                            strcpy( line, buffer );
                            rc = STRING_EXPANDED;
                        } else {
                            rc = NOT_ERROR;
                        }
                    } else
                        rc = ERROR;
                }
                Token_Count = 0; /* AsmBuffer is destroyed, exit */
//            } else if( dir->sym.state == SYM_TMACRO && dir->sym.defined == TRUE) {
            } else if( dir->sym.state == SYM_TMACRO ) {
                DebugMsg1(("ExpandToken: text macro %s to be expanded\n", dir->sym.name ));
                rc = ExpandTMacro( dir, count, AsmBuffer[count]->tokpos, addbrackets, equmode );
                DebugMsg1(("ExpandToken: after expansion: %s\n", AsmBuffer[count]->tokpos ));
            }
        }
    }
#if NEWEXP
    else if ( expansion == TRUE && AsmBuffer[count]->token == T_STRING ) {
        char line2[MAX_LINE_LEN];
        strcpy( line2, AsmBuffer[count]->string_ptr );
        if ( AsmBuffer[count]->string_delim == '"' ||
            AsmBuffer[count]->string_delim == '\'' ) {
            if ( strchr( line2, '&' ) == NULL )
                return( rc );
        }
        if ( ExpandText( line2, TRUE ) == STRING_EXPANDED ) {
            return( RebuildLine( line2,
                                count,
                                AsmBuffer[count]->tokpos,
                                strlen( AsmBuffer[count]->string_ptr ),
                                AsmBuffer[count]->tokpos - line,
                                addbrackets ) );
        }
    }
#endif
    return( rc );
}

/* special handling for
 * CATSTR
 * INSTR
 */

static ret_code FullExpandToken( int i, char *line )
/**************************************************/
{
    char buffer[MAX_LINE_LEN];

    if ( AsmBuffer[i]->token == T_ID ) {
        DebugMsg1(( "FullExpandToken(%u): sym=%s >%s<\n", i, AsmBuffer[i]->string_ptr, line ));
        strcpy( buffer, AsmBuffer[i]->string_ptr );
        if ( ExpandText( buffer, FALSE ) == STRING_EXPANDED ) {
            AsmBuffer[i]->token = T_STRING;
            return( RebuildLine( buffer, i,
                                AsmBuffer[i]->tokpos,
                                strlen( AsmBuffer[i]->string_ptr ),
                                AsmBuffer[i]->tokpos - line, TRUE ) );
        }
    }
    return( NOT_ERROR );
}

/* used by some directives which are flagged with DF_NOEXPAND
 * if they have to partially expand their arguments
 * equmode: 1=don't expand macro functions
 */

void ExpandLinePart( int i, char *line, int addbrackets, int equmode )
/********************************************************************/
{
    int k;
    ret_code rc;

    while ( 1 ) {
        rc = NOT_ERROR;
        for( k = i; k < Token_Count; k++ ) {
            if ( ExpandToken( k, line, addbrackets, equmode ) == STRING_EXPANDED )
                rc = STRING_EXPANDED;
        }
        /* if there was an expansion, the tokenizer must be called. */
        /* if Token_Count is 0, there was a macro function call and
         * the loop must continue (won't happen if equmode == TRUE!)
         */
        if ( rc == STRING_EXPANDED ) {
            k = Token_Count;
            Token_Count = Tokenize( line, i, FALSE );
            if ( k )
                break;
        } else
            break;
    }
}

/* scan current line for (text) macros and expand them. */

ret_code ExpandLine( char * string )
/**********************************/
{
    int count = 0;
    int addbrackets = FALSE;
    asm_sym *sym;
    ret_code rc;
    int flags;
    int i,j;
    //char buffer[MAX_LINE_LEN];

    /* filter certain conditions */
    /* addbrackets: for (preprocessor) directives which expect a literal
     * parameter, the string has to be enclosed in '<>' again.
     */
    DebugMsg1(( "ExpandLine(%s) enter\n", string ));
    i = ( Token_Count > 2 && ( AsmBuffer[1]->token == T_COLON || AsmBuffer[1]->token == T_DBL_COLON ) && AsmBuffer[2]->token == T_DIRECTIVE ) ? 2 : 0;
    if ( AsmBuffer[i]->token == T_DIRECTIVE ) {
        flags = GetValueSp( AsmBuffer[i]->value );
        if ( flags & DF_STRPARM )
            addbrackets = TRUE;
        else if ( flags & DF_NOEXPAND ) {
            /* [ELSE]IF[N]DEF, .ERR[N]DEF, ECHO, FOR[C]
             * .[NO|X]CREF, INCLUDE */
            /* don't expand arguments - unless a % was found as first char */
            if ( expansion == FALSE )
                return( NOT_ERROR );
        }
    } else if ( Token_Count > 1 && AsmBuffer[1]->token == T_DIRECTIVE ) {
        switch ( AsmBuffer[1]->dirtype ) {
        case DRT_CATSTR:
            for ( i = 2; i < Token_Count; i++ ) {
#if 0
                if ( ExpandToken( i, string, TRUE, FALSE ) == STRING_EXPANDED )
                    return( STRING_EXPANDED );
#else /* v2.0: expand text macros only. If a macro function is found, skip loop */
                if ( AsmBuffer[i]->token == T_ID ) {
                    sym = SymSearch( AsmBuffer[i]->string_ptr );
                    if ( sym && sym->state == SYM_TMACRO && sym->isdefined == TRUE ) {
                        DebugMsg1(( "ExpandLine, TEXTEQU: TMACRO %s found\n", AsmBuffer[i]->string_ptr ));
                        if ( ( i == 2 || AsmBuffer[i-1]->token == T_COMMA ) && ( AsmBuffer[i+1]->token == T_COMMA || AsmBuffer[i+1]->token == T_FINAL ) ) {
                            if ( FullExpandToken( i, string ) == STRING_EXPANDED ) {
                                DebugMsg1(( "ExpandLine, TEXTEQU, STRING_EXPANDED exit (%s)\n", string ));
                                return( STRING_EXPANDED );
                            }
                        } else {
                            if ( ExpandToken( i, string, FALSE, FALSE ) == STRING_EXPANDED )
                                return( STRING_EXPANDED );
                        }
                    } else if ( sym && sym->state == SYM_MACRO && sym->isdefined == TRUE )
                        break;
                }
#endif
            }
            count = 2;
            addbrackets = TRUE;
            break;
        case DRT_SUBSTR:
            rc = ExpandToken( 2, string, TRUE, FALSE );
            if ( rc == STRING_EXPANDED ) {
                AsmBuffer[2]->token = T_STRING;
            }
            count = 3;
            goto std_expansion;
        case DRT_SIZESTR:
            if ( expansion == TRUE )
                break;
            if ( ExpandToken( 0, string, FALSE, FALSE ) == STRING_EXPANDED )
                return( STRING_EXPANDED );
            count = 2;
            addbrackets = TRUE;
            break;
        case DRT_INSTR:
            /* INSTR format:
             * label INSTR [number,] literal, literal
             */
            for ( i = 2, count = 0, j = 0; i < Token_Count; i++ )
                if ( AsmBuffer[i]->token == T_COMMA ) {
                    if ( j == 0 )
                        j = i+1;
                    count++;
                }
            if ( count < 2 )
                count = 2;
            else {
                count = j;
                rc = ExpandToken( 2, string, FALSE, FALSE );
            }
            if ( FullExpandToken( count, string ) == STRING_EXPANDED )
                return( STRING_EXPANDED );
            addbrackets = TRUE;
            rc = NOT_ERROR;
            goto std_expansion;
        case DRT_MACRO:
            sym = SymSearch( AsmBuffer[0]->string_ptr );
            /* don't expand macro DEFINITIONs!
             * the name is an exception, if it's not the macro itself
             */
            if ( sym && sym->state != SYM_MACRO )
                return ( ExpandToken( 0, string, FALSE, FALSE ) );
            return( NOT_ERROR );
        case DRT_EQU:
            /* EQU is a special case. If the - expanded - expression is
             a number, then the value for EQU is numeric. Else the
             expression isn't expanded at all. This effectively makes it
             impossible to expand EQU lines here.
             */
            sym = SymSearch( AsmBuffer[0]->string_ptr );
            if ( sym == NULL || sym->state == SYM_TMACRO ) {
                if ( expansion == FALSE ) {
                    DebugMsg1(( "ExpandLine(%s) exit\n", string ));
                    return( NOT_ERROR );
                } else {
                    /* expand text macros and macro functions */
                    /* ignore the % before items */
                    /* with "%" as first character, even the equate's name
                     * is to be expanded!
                     */
                    count = 2;
                }
            }
        }
    }
#if NEWEXP==0
    if ( expansion == TRUE ) {
        if ( STRING_EXPANDED == ExpandText( string, TRUE ) ) {
            DebugMsg1(( "ExpandLine(%s) exit, STRING_EXPANDED\n", string ));
            return( STRING_EXPANDED );
        }
        /* probably better to exit here! */
        return( NOT_ERROR );
    }
#endif
    /* scan the line from left to right for (text) macros.
     * it's currently not quite correct. a macro proc should only
     * be evaluated in the following cases:
     * 1. it is the first token of a line
     * 2. it is the second token, and the first one is an ID
     * 3. it is the third token, the first one is an ID and
     *    the second is a ':' or '::'.
     */
    rc = NOT_ERROR;
std_expansion:
    for(  ; count < Token_Count; count++ ) {
        if ( ExpandToken( count, string, addbrackets, FALSE ) == STRING_EXPANDED )
            rc = STRING_EXPANDED;
    }
    DebugMsg1(( "ExpandLine(%s) exit, rc=%u, token_count=%u\n", string, rc, Token_Count ));
    return( rc );
}

/* get value of a literal, skip literal-character operators(!) */
/* returns no of characters copied into buffer (without terminating 00) */

int GetLiteralValue( char * buffer, const char * p )
/****************************************************/
{
    char *dest = buffer;
    if ( p )
        while ( *p ) {
            if ( *p == '!' && *(p+1) != NULLC )
                p++;
            *dest++ = *p++;
        }
    *dest = NULLC;
    return( dest - buffer );
}

