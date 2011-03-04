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
* Description:  tokenizer.
*
*  The tokenizer is called rather early, with the result that it has to be
*  called multiple times for a line if macro expansion occurs.
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "condasm.h"
#include "insthash.h"
#include "input.h"
#include "segment.h"
#include "listing.h"
#include "tokenize.h"
#include "fastpass.h"

#define CONCATID 0 /* 0=most compatible (see backsl.asm) */
#define MASMNUMBER 1 /* 1=Masm-compatible number scanning */
#define TOKSTRALIGN 1 /* 1=align token strings to uint_32 */

#if 0
typedef union {
        float   f;
        long    l;
} NUMBERFL;
#endif

#ifdef DEBUG_OUT
extern int cnttok0;
extern int cnttok1;
#endif
extern char     CurrComment[];

char   *CurrSource;      /* Current Input Line */
char   *StringBufferEnd; /* start free space in string buffer */

/* string buffer - token strings and other stuff are stored here.
 * must be a multiple of MAX_LINE_LEN since it is used for string expansion.
 */
#ifdef __I86__
static char *token_stringbuf;
#else
static char token_stringbuf[MAX_LINE_LEN*MAX_SYNC_MACRO_NESTING];
#endif

//static struct asm_tok   tokens[MAX_TOKEN];      /* token buffer */
static struct asm_tok   *tokens;      /* start token buffer */
struct asm_tok          *AsmBuffer[MAX_TOKEN];  /* token array */

static uint_8 g_flags; /* directive flags for current line */

bool expansion; /* TRUE if a % has been found as first line character */
char inside_comment;

#if !defined(__GNUC__) && !defined(__POCC__)
#define tolower(c) ((c >= 'A' && c <= 'Z') ? c | 0x20 : c )
#endif

/* initialize the token buffer array */

void InitTokenBuffer( void )
/**************************/
{
    int  count;
    struct asm_tok *curr;
#ifdef __I86__
    token_stringbuf = AsmAlloc( MAX_LINE_LEN*MAX_SYNC_MACRO_NESTING );
#endif
    tokens = AsmAlloc( sizeof( struct asm_tok) * MAX_TOKEN );
    for( count = 0, curr = tokens; count < MAX_TOKEN; count ++ ) {
        AsmBuffer[count] = curr++;
    }
}

void FreeTokenBuffer( void )
/**************************/
{
    AsmFree( tokens );
#ifdef __I86__
    AsmFree( token_stringbuf );
#endif
}

/* test line concatenation if last token is a comma.
 * dont concat EQU, macro invocations or ECHO [v2.0: + FORC/IRPC ] lines!
 * v2.05: don't concat if line's an instruction.
 */
static bool IsMultiLine( struct asm_tok *tokbuf[] )
/*************************************************/
{
    asm_sym *sym;
    int i = 0;

    if ( tokbuf[1]->token == T_DIRECTIVE && tokbuf[1]->value == T_EQU )
        return( FALSE );
    if ( tokbuf[1]->token == T_COLON )
        i = 2;
    if ( tokbuf[i]->token == T_ID ) {
        sym = SymSearch( tokbuf[i]->string_ptr );
        if ( sym && ( sym->state == SYM_MACRO ) )
            return( FALSE );
    } else if ( tokbuf[i]->token == T_INSTRUCTION ||
               ( tokbuf[i]->token == T_DIRECTIVE &&
               ( tokbuf[i]->value == T_ECHO ||
                tokbuf[i]->value == T_FORC ||
                tokbuf[i]->value == T_IRPC ) ) ) {
        return( FALSE );
    }
    return( TRUE );
}

static ret_code get_float( struct asm_tok *buf, struct line_status *p )
/*********************************************************************/
{
    /* valid floats look like:  (int)[.(int)][e(int)]
     * Masm also allows hex format, terminated by 'r' (3F800000r)
     */

    char    got_decimal = FALSE;
    char    got_e = FALSE;
    char    *ptr = p->input;

    for( ; *ptr != NULLC; ptr++ ) {
        char c = *ptr;
        if( isdigit( c ) ) {
            ;
        } else if ( c == '.' && got_decimal == FALSE ) {
            got_decimal = TRUE;
        } else if ( tolower( c ) == 'e' && got_e == FALSE ) {
            got_e = TRUE;
            /* accept e+2 / e-4 /etc. */
            if ( *(ptr+1) == '+' || *(ptr+1) == '-' )
                ptr++;
            /* it's accepted if there's no digit behind 'e' */
            //if ( !isdigit( *(ptr+1) ) )
            //    break;
        } else
            break;
    }

    buf->token = T_FLOAT;
    buf->floattype = NULLC;
    memcpy( p->output, p->input, ptr - p->input );
    p->output += ( ptr - p->input );
    *p->output++ = NULLC;
    p->input = ptr;

    /* the binary value isn't used currently */
    //*((float *)(&buf->value)) = atof( buf->string_ptr );

    return( NOT_ERROR );
}

static ret_code ConcatLine( char *src, int cnt )
/**********************************************/
{
    char *p = src+1;
    int max;

    while ( isspace(*p) ) p++;
    if ( *p == NULLC || *p == ';' ) {
        char *buffer = CurrSource + strlen( CurrSource ) + 1;
        if( GetTextLine( buffer, MAX_LINE_LEN ) ) {
            p = buffer;
            /* skip leading spaces */
            while ( isspace( *p ) ) p++;
            max = strlen( p );
            if ( max + cnt >= MAX_LINE_LEN ) {
                AsmError( LINE_TOO_LONG );
                max = MAX_LINE_LEN - (cnt + 1);
            }
            if ( cnt == 0 )
                *src++ = ' ';
            memcpy( src, p, max+1 );
            return( NOT_ERROR );
        }
    }
    return( EMPTY );
}

static ret_code get_string( struct asm_tok *buf, struct line_status *p )
/**********************************************************************/
{
    char    symbol_o;
    char    symbol_c;
    char    c;
    char    *iptr = p->input;
    char    *optr = p->output;
    int     count;
    int     level;

    /*
     * a string might span multiple lines if the last character of the line
     * is a comma! It must be enclosed in <> or {}.
     */

    symbol_o = *iptr;
    buf->string_delim = symbol_o;

    buf->token = T_STRING;
    switch( symbol_o ) {
    case '"':
    case '\'':
        symbol_c = 0;
        break;  /* end of string marker is the same */
    case '<':
        symbol_c = '>';
        break;
    case '{':
        symbol_c = '}';
        break;
    default:
        /* this is an undelimited string,
         * so just copy it until we hit something that looks like the end.
         * this format is used by the INCLUDE directive, but may also
         * occur inside the string macros!
         */
        /* v2.05: also stop if a ')' is found - see literal2.asm regression test */
        //for( count = 0 ; count < MAX_STRING_LEN && *iptr != NULLC && !isspace( *iptr ) && *iptr != ',' && *iptr != ';'; ) {
        for( count = 0 ; count < MAX_STRING_LEN && *iptr != NULLC && !isspace( *iptr ) && *iptr != ',' && *iptr != ';' && *iptr != ')'; ) {
            if ( *iptr == '\\' ) {
                if ( ConcatLine( iptr, count ) != EMPTY ) {
                    p->is_concat = TRUE;
                    if ( count )
                        continue;
                    return( EMPTY );
                }
            }
            *optr++ = *iptr++;
            count++;
        }
        if ( count >= MAX_STRING_LEN ) {
            AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
            return( ERROR );
        }
        *optr++ = NULLC;
        buf->value = count;
        p->input = iptr;
        p->output = optr;
        return( NOT_ERROR );
    }

    *optr++ = symbol_o;
    if ( symbol_o == '<' )   /* don't include delimiters for <> literals! */
        buf->string_ptr++;

    iptr++;
    /* v2.05: delimiters are no longer skipped */
    //buf->tokpos = iptr; /* tokpos points BEHIND the delimiter! */

    count = 0;
    level = 0;
    while( count < MAX_STRING_LEN ) {
        c = *iptr;
        if( c == symbol_o ) { /* "'<{ */
            if( symbol_c ) { /* literal? */
                level++;
                *optr++ = c; iptr++;
                count++;
            } else if( *( iptr + 1 ) == symbol_o ) {
                /* if we see "" in a " delimited string,
                 * treat it as a literal " */
                iptr++; /* skip the 1st one */
                *optr++ = c; iptr++; /* keep the 2nd one */
                count++;
            } else {
                *optr++ = c; /* store " or ' */
                iptr++;
                break; /* exit loop */
            }
        } else if( symbol_c && c == symbol_c ) { /* >} */
            if( level ) {
                level--;
                *optr++ = c; iptr++;
                count++;
            } else {
                /* store the string delimiter unless it is <> */
                if (symbol_o != '<')
                    *optr++ = c;
                iptr++;
                break; /* exit loop */
            }
#if 1
            /*
             a " or ' inside a <>/{} string? Since it's not a must that
             [double-]quotes are paired in a literal it must be done
             directive-dependant!
             see: IFIDN <">,<">
             */
        } else if( ( c == '"' || c == '\'' ) &&
                  symbol_c != 0 &&
                  ( g_flags & DF_STRPARM ) == 0 ) {
            char delim = c;
            char *toptr;
            char *tiptr;
            int tcount;
            *optr++ = c; iptr++;
            count++;
            toptr = optr;
            tiptr = iptr;
            tcount = count;
            while (*iptr != delim && *iptr != NULLC && count < MAX_STRING_LEN-1 ) {
                *optr++ = *iptr++;
                count++;
            }
            if ( *iptr == delim ) {
                *optr++ = *iptr++;
                count++;
                continue;
            } else {
                /* restore values */
                iptr = tiptr;
                optr = toptr;
                count = tcount;
            }
#endif
        } else if( c == '!' && symbol_o == '<' && *(iptr+1) != '\0') {
            /* handle literal-character operator '!'.
               it makes the next char to enter the literal uninterpreted.
            */
            *optr++ = c; iptr++;
            count++;
            if ( count == MAX_STRING_LEN )
                break;
            *optr++ = *iptr++;
            count++;
        } else if( c == '\\' &&
                  symbol_c && /* for <{ only */
                  ConcatLine( iptr, count ) != EMPTY ) { 
            /* with quoted strings, there's no line concatenation:
             * db "abc \
             *     def"
             * is supposed to give error "missing .. quote in string"
             */
            p->is_concat = TRUE;
        } else if( c == NULLC || ( c == ';' && symbol_c == '}' )) {
            if ( symbol_c && ((g_flags & DF_NOCONCAT) == 0) ) { /* <{ */
                /* if last nonspace character was a comma
                 * get next line and continue string scan
                 */
                char *tmp = optr-1;
                while ( isspace(*tmp) ) tmp--;
                if ( *tmp == ',' ) {
                    /* use optr as temp buffer */
                    tmp = optr;
                    if( GetTextLine( tmp, MAX_LINE_LEN ) ) {
                        /* skip leading spaces */
                        while ( isspace( *tmp ) ) tmp++;
                        /* this size check isn't fool-proved yet */
                        if ( strlen(tmp) + count >= MAX_LINE_LEN ) {
                            AsmError( LINE_TOO_LONG );
                            return( ERROR );
                        }
                        strcpy( iptr, tmp );
                        continue;
                    }
                }
                if ( symbol_o == '<' ) {
                    buf->string_ptr--;
                    count++;
                }
                /* the end delimiter '}' is missing, but don't
                 flag this as an error! */
                //buf->string_delim = NULLC;
            }
            /* v2.0: same for <> literals an "strings" */
            buf->string_delim = NULLC;
            break;
        } else {
            *optr++ = c; iptr++;
            count++;
        }
    }
    if ( count == MAX_STRING_LEN ) {
        AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
        return( ERROR );
    }
    *optr++ = NULLC;
    buf->value = count;
    p->input = iptr;
    p->output = optr;
    return( NOT_ERROR );
}

static ret_code get_special_symbol( struct asm_tok *buf, struct line_status *p )
/******************************************************************************/
{
    char    symbol;
    //int     i;

    symbol = *p->input;
    switch( symbol ) {
    case ':' : /* binary operator */
        if ( *(p->input+1) == ':' ) {
            p->input += 2;
            buf->token = T_DBL_COLON;
            *(p->output)++ = symbol;
            *(p->output)++ = symbol;
            break;
        }
    case '.' : /* binary operator */
    case ',' :
    case '+' : /* unary|binary operator */
    case '-' : /* unary|binary operator */
    case '*' : /* binary operator */
    case '/' : /* binary operator */
    case '[' : /* operator - needs a matching ']' */
    case ']' :
    case '(' : /* operator - needs a matching ')' */
    case ')' :
    case '%' :
        /* all of these are themselves a token */
        p->input++;
        buf->token = symbol;
        *(p->output)++ = symbol;
        break;
#if 0 /* this case is filtered in Tokenize() */
    case ';' :
        /* a '<' in the source will prevent comments to be removed
         * so they might appear here. Remove!
         */
        //while (*p->input) *(p->input)++;/* v1.96: replaced by next line */
        while (*p->input) p->input++;
        return( EMPTY );
#endif
    case '=' :
        if ( *(p->input+1) != '=' ) {
            buf->token = T_DIRECTIVE;
            buf->value = T_EQU;
            buf->dirtype = DRT_EQUALSGN; /* to make it differ from EQU directive */
            //buf->flags = DF_LABEL;
            *(p->output)++ = symbol;
            p->input++;
            break;
        }
        /* fall through */
    default:
        /* recognize C style operators.
         * DF_CEXPR is set if .IF, .WHILE, .ELSEIF or .UNTIL
         * has been detected in the current line.
         * will catch: '!', '<', '>', '&', '==', '!=', '<=', '>=', '&&', '||'
         * A single '|' will also be caught, although it isn't a valid
         * operator - it will cause a 'operator expected' error msg later.
         * the tokens are stored as one- or two-byte sized "strings".
         */
        if ( ( g_flags & DF_CEXPR ) && strchr( "=!<>&|", symbol ) ) {
            *(p->output)++ = symbol;
            p->input++;
            buf->value = 1;
            if ( symbol == '&' || symbol == '|' ) {
                if ( *p->input == symbol ) {
                    *(p->output)++ = symbol;
                    p->input++;
                    buf->value = 2;
                }
            } else if ( *p->input == '=' ) {
                *(p->output)++ = '=';
                p->input++;
                buf->value = 2;
            }
            buf->token = T_STRING;
            buf->string_delim = NULLC;
            break;
        }
        /* anything we don't recognise we will consider a string,
         * delimited by space characters, commas, newlines or nulls
         */
        return( get_string( buf, p ) );
    }
    *(p->output)++ = NULLC;
    return( NOT_ERROR );
}

#if 0
static void array_mul_add( unsigned char *buf, unsigned base, unsigned num, unsigned size )
/*****************************************************************************************/
{
    while( size-- > 0 ) {
        num += *buf * base;
        *(buf++) = num;
        num >>= 8;
    }
}
#endif

/* read in a number
 * check the number suffix:
 * b or y: base 2
 * d or t: base 10
 * h: base 16
 * o or q: base 8
 */
static ret_code get_number( struct asm_tok *buf, struct line_status *p )
/**********************************************************************/
{
    char                *ptr = p->input;
    char                *dig_start;
    char                *dig_end;
    unsigned            len;
    unsigned            base = 0;
    uint_32             digits_seen;
    uint_32             val;
    char                last_char;
    uint_64             numval[2];

#define VALID_BINARY    0x0003
#define VALID_OCTAL     0x00ff
#define VALID_DECIMAL   0x03ff
#define OK_NUM( t )     ((digits_seen & ~VALID_##t) == 0)

    digits_seen = 0;
#if CHEXPREFIX
    if( *ptr == '0' && (tolower( *(ptr+1) ) == 'x' ) ) {
        ptr += 2;
        base = 16;
    }
#endif
    dig_start = ptr;
    for( ;; ptr++ ) {
        if (*ptr >= '0' && *ptr <= '9')
            digits_seen |= 1 << (*ptr - '0');
        else {
            last_char = tolower( *ptr );
            if ( last_char >= 'a' && last_char <= 'f' )
                digits_seen |= 1 << ( last_char + 10 - 'a' );
            else
                break;
        }
    }
#if CHEXPREFIX
    if ( base != 0 ) {
        dig_end = ptr;
        if ( digits_seen == 0 )
            base = 0;
    } else
#endif
    switch( last_char ) {
    case 'r': /* a float with the "real number designator" */
        buf->token = T_FLOAT;
        buf->floattype = 'r';
        ptr++;
        goto number_done;
    case 'h':
        base = 16;
        dig_end = ptr;
        ptr++;
        break;
    //case 'b':
    case 'y':
        if( OK_NUM( BINARY ) ) {
            base = 2;
            dig_end = ptr;
            ptr++;
        }
        break;
    //case 'd':
    case 't':
        if( OK_NUM( DECIMAL ) ) {
            base = 10;
            dig_end = ptr;
            ptr++;
        }
        break;
    case 'q':
    case 'o':
        if( OK_NUM( OCTAL ) ) {
            base = 8;
            dig_end = ptr;
            ptr++;
        }
        break;
    case '.':
        /* note that a float MUST contain a dot.
         * 1234e78 is NOT a valid float
         */
        return( get_float( buf, p ) );
    default:
        last_char = tolower( *(ptr-1) );
        if ( ( last_char == 'b' || last_char == 'd' ) && digits_seen >= ( 1UL << ModuleInfo.radix ) ) {
            char *tmp = dig_start;
            char max = ( last_char == 'b' ? '1' : '9' );
            for ( dig_end = ptr-1; tmp < dig_end && *tmp <= max; tmp++ );
            if ( tmp == dig_end ) {
                base = ( last_char == 'b' ? 2 : 10 );
                break;
            }
        }
        dig_end = ptr;
#if COCTALS
        if( Options.allow_c_octals && *dig_start == '0' ) {
            if( OK_NUM( OCTAL ) ) {
                base = 8;
                break;
            }
        }
#endif
        /* radix      max. digits_seen
         -----------------------------------------------------------
         2            3      2^2-1  (0,1)
         8            255    2^8-1  (0,1,2,3,4,5,6,7)
         10           1023   2^10-1 (0,1,2,3,4,5,6,7,8,9)
         16           65535  2^16-1 (0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f)
         */
        if ( digits_seen < (1UL << ModuleInfo.radix) )
            base = ModuleInfo.radix;
        break;
    }

    numval[0] = 0;
    numval[1] = 0;
    //buf->value64 = 0;
    //buf->hivalue64 = 0;

#if MASMNUMBER
    /* Masm doesn't swallow alphanum chars which may follow the
     * number!
     */
    if ( base != 0 ) {
#else
    if ( base != 0 && is_valid_id_char( *ptr ) == FALSE ) {
#endif
        buf->token = T_NUM;
        while( dig_start < dig_end ) {
            uint_16 *px;
            if( *dig_start <= '9' ) {
                val = *dig_start - '0';
            } else {
                val = tolower( *dig_start ) - 'a' + 10;
            }
            /* v2: do the calculation inline and with 2 bytes at once */
            //array_mul_add( buf->bytes, base, val, sizeof( buf->bytes ) );
            px = (uint_16 *)&numval[0];
            for ( len = sizeof( numval ) >> 1; len; len-- ) {
                val += (uint_32)*px * base;
                *(px++) = val;
                val >>= 16;
            };
            ++dig_start;
        }
        buf->value64 = numval[0];
        if ( numval[1] == 0 )
            buf->hivalflg = HV_NULL;
        else {
            buf->hivalflg = HV_STORED;
            /* store the upper 8 bytes in the token buffer */
            *(uint_64 *)p->output = numval[1];
            p->output += sizeof( uint_64 );
            buf->string_ptr = p->output;
        }
        //DebugMsg(("get_number: inp=%s, value=%" FX32 "\n", p->input, buf->value64 ));
    } else {
        //buf->tokpos = p->input; /* restore input ptr for T_BAD_NUM */
        buf->token = T_BAD_NUM;
        DebugMsg(("get_number: BAD_NUMBER (%s), radix=%u, base=%u, ptr=>%s<, digits_seen=%Xh\n", dig_start, ModuleInfo.radix, base, ptr, digits_seen ));
        /* swallow remainder of token */
        while( is_valid_id_char( *ptr ) ) ++ptr;
        /* don't display an error here, it will cause
         * 'nondigit in number' later in expression evaluator
         */
        //AsmError( INVALID_NUMBER_DIGIT );
    }
number_done:
    len = ptr - p->input;
    memcpy( p->output, p->input, len );

    p->output += len;
    *p->output++ = NULLC;
    p->input = ptr;

    return( NOT_ERROR );
}

#if BACKQUOTES
static ret_code get_id_in_backquotes( struct asm_tok *buf, struct line_status *p )
/********************************************************************************/
{
    char *optr = p->output;
    buf->token = T_ID;
    buf->value = 0;

    p->input++;         /* strip off the backquotes */
    for( ; *p->input != '`'; ) {
        if( *p->input == NULLC || *p->input == ';' ) {
            *p->output = NULLC;
            AsmErr( BACKQUOTE_MISSING, p->output );
            return( ERROR );
        }
        *optr++ = *p->input++;
    }
    p->input++;         /* skip the terminating '`' */
    *optr++ = NULLC;
    p->output = optr;
    return( NOT_ERROR );
}
#endif

/* get an ID. will always return NOT_ERROR. */

static ret_code get_id( struct asm_tok *buf, struct line_status *p )
/******************************************************************/
{
    struct ReservedWord *resw;
    char *iptr = p->input;
    char *optr = p->output;
    int  index;
    int  size;

    buf->value = 0;

#if CONCATID
continue_scan:
#endif
    do {
        *optr++ = *iptr++;
    } while ( is_valid_id_char( *iptr ) );
#if CONCATID
    /* v2.05: in case there's a backslash right behind
     * the ID, check if a line concatenation is to occur.
     * If yes, and the first char of the concatenated line
     * is also a valid ID char, continue to scan the name.
     * Problem: it's ok for EQU, but less good for other directives.
     */
    if ( *iptr == '\\' ) {
        if ( ConcatLine( iptr, iptr - p->input ) != EMPTY ) {
            p->concat = TRUE;
            if ( is_valid_id_char( *iptr ) )
                goto continue_scan;
        }
    }
#endif
    /* v2.04: check added */
    size = optr - p->output;
    if ( size > MAX_ID_LEN ) {
        AsmErr( IDENTIFIER_TOO_LONG );
        optr = p->output + MAX_ID_LEN;
    }
    *optr++ = NULLC;

    /* now decide what to do with it */

    if( size == 1 && *p->output == '?' ) {
        p->input = iptr;
        p->output = optr;
        buf->token = T_QUESTION_MARK;
        return( NOT_ERROR );
    }
    resw = FindResWord( p->output );
    if( resw == NULL ) {
        /* if ID begins with a DOT, check for OPTION DOTNAME.
         * if not set, skip the token and return a T_DOT instead!
         */
        if ( *p->output == '.' && ModuleInfo.dotname == FALSE ) {
           buf->token = T_DOT;
           p->output++;
           *p->output++ = NULLC;
           p->input++;
           return( NOT_ERROR );
        }
        p->input = iptr;
        p->output = optr;
        buf->token = T_ID;
        return( NOT_ERROR );
    }
    p->input = iptr;
    p->output = optr;
    buf->value = resw - AsmResWord; /* is a enum asm_token value */
    if ( ! ( resw->flags & RWF_SPECIAL ) ) {

        //  DebugMsg(("found item >%s< in instruction table, rm=%X\n", buf->string_ptr, InstrTable[index].rm_byte));

        /* if -Zm is set, the following from the Masm docs is relevant:
         *
         * Reserved Keywords Dependent on CPU Mode with OPTION M510
         *
         * With OPTION M510, keywords and instructions not available in the
         * current CPU mode (such as ENTER under .8086) are not treated as
         * keywords. This also means the USE32, FLAT, FAR32, and NEAR32 segment
         * types and the 80386/486 registers are not keywords with a processor
         * selection less than .386.
         * If you remove OPTION M510, any reserved word used as an identifier
         * generates a syntax error. You can either rename the identifiers or
         * use OPTION NOKEYWORD. For more information on OPTION NOKEYWORD, see
         * OPTION NOKEYWORD, later in this appendix.
         *
         * The current implementation of this rule below is likely to be improved.
         */
        if ( ModuleInfo.m510 ) {
            /* checking the cpu won't give the expected results currently since
             * some instructions in the table (i.e. MOV) start with a 386 variant!
             */
            index = IndexFromToken( buf->value );
#if 0 /* changed for v1.96 */
            if (( InstrTable[index].cpu & P_EXT_MASK ) > ( ModuleInfo.curr_cpu & P_EXT_MASK )) {
#else
            if (( InstrTable[index].cpu & P_CPU_MASK ) > ( ModuleInfo.curr_cpu & P_CPU_MASK ) ||
                ( InstrTable[index].cpu & P_EXT_MASK ) > ( ModuleInfo.curr_cpu & P_EXT_MASK )) {
#endif
                buf->value = 0;
                buf->token = T_ID;
                return( NOT_ERROR );
            }
        }
        buf->token = T_INSTRUCTION;
        return( NOT_ERROR );
    }
    index = buf->value;

    /* for RWT_SPECIAL, field <value8> contains further infos:
     - RWT_REG:             register number (regnum)
     - RWT_DIRECTIVE:       type of directive (dirtype)
     - RWT_UNARY_OPERATOR:  operator precedence
     - RWT_BINARY_OPERATOR: operator precedence
     - RWT_STYPE:           index into SimpleType table
     - RWT_RES_ID:          for languages, LANG_xxx value
                            for the rest, unused.
     */
    buf->value8 = SpecialTable[index].value8;

    switch ( SpecialTable[index].type ) {
    case RWT_REG:
        buf->token = T_REG;
        break;
    case RWT_DIRECTIVE:
        buf->token = T_DIRECTIVE;
        if ( g_flags == 0 )
            g_flags = SpecialTable[index].value;
        break;
    case RWT_UNARY_OP: /* OFFSET, LOW, HIGH, LOWWORD, HIGHWORD, SHORT, ... */
        buf->token  = T_UNARY_OPERATOR;
        break;
    case RWT_BINARY_OP: /* GE, GT, LE, LT, EQ, NE, MOD */
        buf->token = T_BINARY_OPERATOR;
        break;
    case RWT_STYPE:  /* BYTE, WORD, FAR, NEAR, FAR16, NEAR32 ... */
        buf->token = T_STYPE;
        break;
    case RWT_RES_ID: /* DUP, PTR, ADDR, FLAT, VARARG, and languages */
        buf->token = T_RES_ID;
        break;
    default:
        DebugMsg(("get_id: found unknown type=%u\n", SpecialTable[index].type ));
        buf->token = T_ID; /* shouldn't happen */
        break;
    }
    return( NOT_ERROR );
}

/* get one token.
 * possible return values: NOT_ERROR, ERROR, EMPTY.
 *
 * names beginning with '.' are difficult to detect,
 * because the dot is a binary operator. The rules to
 * accept a "dotted" name are:
 * 1.- a valid ID char is to follow the dot
 * 2.- if buffer index is > 0, then the previous item
 *     must not be a reg, ), ] or an ID.
 * [bx.abc]    -> . is an operator
 * ([bx]).abc  -> . is an operator
 * [bx].abc    -> . is an operator
 * varname.abc -> . is an operator
 */

#define is_valid_id_start( ch )  ( isalpha(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

ret_code GetToken( unsigned int buf_index, struct line_status *p )
/****************************************************************/
{
    if( isdigit( *p->input ) ) {
        return( get_number( AsmBuffer[buf_index], p ) );
    } else if( is_valid_id_start( *p->input ) ) {
        return( get_id( AsmBuffer[buf_index], p ) );
    } else if( *p->input == '.' &&
               is_valid_id_char(*(p->input+1)) &&
               ( buf_index == 0 ||
                (AsmBuffer[buf_index-1]->token != T_REG &&
                 AsmBuffer[buf_index-1]->token != T_CL_BRACKET &&
                 AsmBuffer[buf_index-1]->token != T_CL_SQ_BRACKET &&
                 AsmBuffer[buf_index-1]->token != T_ID ) ) ) {
        return( get_id( AsmBuffer[buf_index], p ) );
#if BACKQUOTES
    } else if( *p->input == '`' && Options.strict_masm_compat == FALSE ) {
        return( get_id_in_backquotes( AsmBuffer[buf_index], p ) );
#endif
    }
    return( get_special_symbol( AsmBuffer[buf_index], p ) );
}

// fixme char *IfSymbol;        /* save symbols in IFDEF's so they don't get expanded */

static void StartComment( const char * p )
/****************************************/
{
    while ( isspace( *p ) ) p++;
    if ( *p == NULLC ) {
        AsmError( COMMENT_DELIMITER_EXPECTED );
        return;
    }
    inside_comment = *p++;
    if( strchr( p, inside_comment ) )
        inside_comment = NULLC;
    return;
}

int Tokenize( char *line, unsigned int start, int rescan )
/********************************************************/
/*
 * create tokens from a source line.
 * string: the line which is to tokenize
 * index: where to start in the token buffer. If index == 0,
 *        then some variables are additionally initialized.
 */
{
    unsigned int                index = start;
    int                         rc;
    struct line_status          p;

    if ( index == 0 ) {
#ifdef DEBUG_OUT
        cnttok0++;
#endif
        ModuleInfo.line_listed = FALSE;
        CurrSource = line;
        p.output = token_stringbuf;
        g_flags = 0;
        expansion = FALSE;
        p.input = line;
        if( inside_comment ) {
            DebugMsg(("COMMENT active, delim is >%c<, line is >%s<\n", inside_comment, line));
            if( strchr( line, inside_comment ) != NULL ) {
                DebugMsg(("COMMENT mode exited\n"));
                inside_comment = NULLC;
            }
            goto skipline;
        }
        while( isspace( *p.input )) p.input++;
        if (*p.input == '%') {
            *p.input++ = ' ';
            expansion = TRUE;
        }
    } else {
#ifdef DEBUG_OUT
        cnttok1++;
#endif
        p.output = StringBufferEnd;
        p.input = line;
    }

    for( ;; ) {

        while( isspace( *p.input ) ) p.input++;

#if TOKSTRALIGN
        if ( ( p.output - StringBufferEnd ) & (sizeof(uint_32)-1) )
            p.output += sizeof(uint_32) - ( ( p.output - StringBufferEnd ) & (sizeof(uint_32)-1) );
#endif

        AsmBuffer[index]->string_ptr = p.output;
        AsmBuffer[index]->tokpos = p.input;

        if( *p.input == NULLC || *p.input == ';' ) {
            if ( *p.input == ';' ) {
                strcpy( CurrComment, p.input );
                *p.input = NULLC;
            }
            /* if a comma is last token, concat lines ... with some exceptions
             * v2.05: moved from GetPreprocessedLine(). Moved because the
             * concatenation may be triggered by a comma AFTER expansion.
             */
            if ( index > 1 &&
                AsmBuffer[index-1]->token == T_COMMA &&
#if FASTPASS
                ( Parse_Pass == PASS_1 || UseSavedState == FALSE ) && /* is it an already preprocessed line? */
#endif
                start == 0 ) {
                DebugMsg1(("Tokenize: calling IsMultiLine()\n" ));
                if ( IsMultiLine( AsmBuffer ) ) {
                    DebugMsg1(("Tokenize: IsMultiLine(%s)=TRUE\n", line ));
                    if ( GetTextLine( p.output, MAX_LINE_LEN - ( p.input - line ) ) ) {
                        char *ptr = p.output;
                        while ( isspace( *ptr ) ) ptr++;
                        if ( *ptr ) {
                            strcpy( p.input, ptr );
                            DebugMsg1(("Tokenize: line concatenation, line=%s\n", line ));
                            continue;
                        }
                    }
                }
            }
            break;
        }
        rc = GetToken( index, &p );
        if ( rc == EMPTY )
            continue;
        if ( rc == ERROR ) {
            index = 0; /* skip this line */
            break;
        }
        /* v2.04: this has been moved here from condasm.c to
         * avoid problems with (conditional) listings. It also
         * avoids having to search for the first token twice.
         * Note: a conditional assembly directive within an
         *    inactive block and preceded by a label isn't detected!
         *    This is an exact copy of the Masm behavior, although
         *    it probably is just a bug!
         */
        if ( rescan == FALSE )
        if ( index == 0 || ( index == 2 && ( AsmBuffer[1]->token == T_COLON || AsmBuffer[1]->token == T_DBL_COLON) ) ) {
            if ( AsmBuffer[index]->token == T_DIRECTIVE &&
                AsmBuffer[index]->value8 == DRT_CONDDIR ) {
                if ( AsmBuffer[index]->value == T_COMMENT ) {
                    DebugMsg1(("tokenize: COMMENT starting, delim is >%c<\n", inside_comment));
                    StartComment( p.input );
                    break;
                }
                conditional_assembly_prepare( AsmBuffer[index]->value );
                if ( CurrIfState != BLOCK_ACTIVE ) {
                    index++;
                    break;
                }
            } else if( CurrIfState != BLOCK_ACTIVE ) {
                break;
            }
        }

        index++;
        if( index >= MAX_TOKEN ) {
            AsmError( TOO_MANY_TOKENS );
            index = 0;
            break;
        }
    }

skipline:

    AsmBuffer[index]->token = T_FINAL;
#if TOKSTRALIGN
    *p.output = NULLC;
    p.output += sizeof(uint_32);
#else
    *p.output++ = NULLC;
#endif
    StringBufferEnd = p.output;
    return( index );
}

/* get size of token buffer status */

int GetTokenStateSize( void )
/***************************/
{
    return( sizeof( int ) +
            (Token_Count+1) * sizeof( struct asm_tok ) +
            sizeof( char * ) +
            sizeof( int ) +
            ( StringBufferEnd - token_stringbuf ) );
}

/* save the token buffer status.
 This is
 - variable Token_Count
 - variable AsmBuffer[0..Token_Count]
 - variable CurrSource
 - variable token_stringbuf (StringBufferEnd points to end of buffer)
 */

void SaveTokenState( unsigned char * pSave )
/******************************************/
{
    int i;
    *(int *)pSave = Token_Count;
    pSave += sizeof( int );
    for (i = 0; i <= Token_Count; i++, pSave += sizeof( struct asm_tok ) )
        memcpy( pSave, AsmBuffer[i], sizeof( struct asm_tok ) );
    *(char * *)pSave = CurrSource;
    pSave += sizeof( char * );
    *(int *)pSave = StringBufferEnd - token_stringbuf;
    pSave += sizeof( int );
    memcpy( pSave, token_stringbuf, StringBufferEnd - token_stringbuf );
    return;
}

/* restore token state previously saved with SaveTokenState() */

void RestoreTokenState( unsigned char * pSave )
/*********************************************/
{
    int i;
    Token_Count = *(int *)pSave;
    pSave += sizeof( int );
    for (i = 0; i <= Token_Count; i++, pSave += sizeof( struct asm_tok ) )
        memcpy( AsmBuffer[i], pSave, sizeof( struct asm_tok ) );
    CurrSource = *(char * *)pSave;
    pSave += sizeof( char * );
    i = *(int *)pSave;
    pSave += sizeof( int );
    memcpy( token_stringbuf, pSave, i );
    StringBufferEnd = token_stringbuf + i;
    return;
}
