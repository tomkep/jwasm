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
*  Currently the tokenizer is called "too early", with the result that it
*  has to be called multiple times if macro expansion occurs. Furthermore,
*  the tokenizer is not fully compatible with macro parameter formats. The
*  correct thing to do is to run the tokenizer AFTER the macro expansion step.
*  However, this will require some work and intensive tests.
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "insthash.h"
#include "parser.h"
#include "condasm.h"
#include "directiv.h"
#include "input.h"

char                    *CurrSource;      // Current Input Line
char                    *StringBufferEnd; // start free space in string buffer

#ifdef DEBUG_OUT
extern int cnttok0;
extern int cnttok1;
#endif

// string buffer - token strings and other stuff are stored here.
// must be a multiple of MAX_LINE_LEN since it is used for string expansion.

static char             token_stringbuf[MAX_LINE_LEN*MAX_SYNC_MACRO_NESTING];


static uint_8 g_flags; /* directive flags for current line */

bool expansion; /* TRUE if a % has been found as first line character */

static struct asm_tok   tokens[MAX_TOKEN];      /* token buffer */
struct asm_tok          *AsmBuffer[MAX_TOKEN];  /* token array */

typedef struct {
    char *input;
    char *output;
} ioptrs;

#if !defined(__GNUC__) && !defined(__POCC__)
#define tolower(c) ((c >= 'A' && c <= 'Z') ? c | 0x20 : c )
#endif

// initialize the token buffer array

void InitTokenBuffer( void )
/**************************/
{
    int         count;
    for( count = 0; count < MAX_TOKEN; count ++ ) {
        AsmBuffer[count] = &tokens[count];
    }
}

typedef union {
        float   f;
        long    l;
} NUMBERFL;

static ret_code get_float( struct asm_tok *buf, ioptrs *p )
/*********************************************************/
{
    /* valid floats look like:  (int)[.(int)][e(int)] */

    //char    got_decimal = FALSE;
    char    got_e = FALSE;
    char    *ptr = p->input;

    for( ; *ptr != '\0'; ptr++ ) {
        if( isdigit( *ptr ) )
            continue;
        if( isspace( *ptr ) )
            break;
        switch( tolower( *ptr ) ) {
        case '.':
            //got_decimal = TRUE;
            continue;
        case 'r':
            *ptr=' ';
            goto done_scanning_float;
        case 'e':
            if( !got_e ) {
                got_e = TRUE;
                /* accept e+2 / e-4 /etc. */

                switch( *(ptr+1) ) {
                case '+':
                case '-':
                    ptr++;
                    /* fall through */
                default:
                    continue;
                }
            }
            /* fall through */
        default:
            goto done_scanning_float;
//            return( get_string( buf, p ) );
        }
    }

done_scanning_float:

    buf->token = T_FLOAT;
    /* copy the string, fix input & output pointers */
    strncpy( p->output, p->input, ptr - p->input );
    //buf->string_ptr = p->output;
    p->output += ( ptr - p->input );
    *p->output = '\0';
    p->output++;
    p->input = ptr;

    *((float *)(&buf->value)) = atof(buf->string_ptr);
    return( NOT_ERROR );
}

static ret_code get_string( struct asm_tok *buf, ioptrs *p )
/**********************************************************/
{
    char    symbol_o;
    char    symbol_c;
    char    *iptr = p->input;
    char    *optr = p->output;
    int     count;
    int     level;

    /*
     a string might span multiple lines if the last character of the line
     is a comma! It must be enclosed in <> or {}.
     */

    //buf->string_ptr = optr;

    symbol_o = *iptr;
    buf->string_delim = symbol_o;

    buf->token = T_STRING;
    switch( symbol_o ) {
    case '"':
    case '\'':
        symbol_c = 0;
        break;  // end of string marker is the same
    case '<':
        symbol_c = '>';
        break;
    case '{':
        symbol_c = '}';
        break;
    default:
        if (g_flags & DF_CEXPR) {
            /* a C expression is likely to occur. check for &&,||,... */
            char c = *iptr;
            *optr++ = *iptr++;
            count = 1;
            switch(c) {
            case '&':
            case '|':
                if (*iptr == c) {
                    *optr++ = *iptr++;
                    count++;
                }
                break;
            case '<':
            case '>':
            case '!':
            // case '=': /* not sure if a '=' makes it to this place */
                if (*iptr == '=') {
                    *optr++ = *iptr++;
                    count++;
                }
                break;
            default:
                goto std_string;
            }
        } else {
        std_string:
        /* this is an undelimited string,
         * so just copy it until we hit something that looks like the end.
         * this format is used by the INCLUDE directive, maybe others also?
         */
            for( count = 0 ;
                count < MAX_STRING_LEN &&
                *iptr != NULLC &&
                !isspace( *iptr ) &&
                *iptr != ',';
                 count++ ) {
                *optr++ = *iptr++;
            }
            if ( count > MAX_STRING_LEN ) {
                AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
                return( ERROR );
            }
        }
        *optr++ = NULLC;
        buf->value = count;
        p->input = iptr;
        p->output = optr;
        return( NOT_ERROR );
    }

    *optr++ = symbol_o;
    if (symbol_o == '<')   /* don't include delimiters for <> literals! */
        buf->string_ptr++;

    iptr++;
    buf->pos = iptr; /* pos points BEHIND the delimiter! */

    count = 0;
    level = 0;
    while( count < MAX_STRING_LEN ) {
        if( *iptr == symbol_o ) {
            if( symbol_c ) {
                level++;
                *optr++ = *iptr++;
                count++;
            } else if( *( iptr + 1 ) == symbol_o ) {
                /* if we see "" in a " delimited string,
                 * treat it as a literal " */
                iptr++; /* skip the 1st one */
                *optr++ = *iptr++; /* keep the 2nd one */
                count++;
            } else {
                if (symbol_o != '<')
                    *optr++ = *iptr;
                iptr++; /* skip the closing delimiter */
                break; /* exit loop */
            }
        } else if( symbol_c && *iptr == symbol_c ) {
            if( level ) {
                level--;
                *optr++ = *iptr++;
                count++;
            } else {
                /* store the string delimiter unless it is <> */
                if (symbol_o != '<')
                    *optr++ = *iptr;
                iptr++; /* skip the closing delimiter */
                break; /* exit loop */
            }
#if 1
            /*
             a " or ' inside a <>/{} string? Since it's not a must that
             [double-]quotes are paired in a literal it must be done
             directive-dependant!
             see: IFIDN <">,<">
             */
        } else if( ( *iptr == '"' || *iptr == '\'' ) &&
                  symbol_c != 0 &&
                  (g_flags & DF_STRPARM) == 0) {
            char delim = *iptr;
            char *toptr;
            char *tiptr;
            int tcount;
            *optr++ = *iptr++;
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
        } else if( *iptr == '!' && symbol_o == '<' && *(iptr+1) != '\0') {
            /* handle literal-character operator '!'.
               it makes the next char to enter the literal uninterpreted.
            */
            *optr++ = *iptr++;
            count++;
            if (count == MAX_STRING_LEN)
                break;
            *optr++ = *iptr++;
            count++;
        } else if( *iptr == '\0' || *iptr == '\n' ) {
            if ((symbol_o == '<') || (symbol_o == '{')) {
                /* if last nonspace character was a comma
                 * get next line and continue string scan
                 */
                char *tmp = optr-1;
                while ( isspace(*tmp) ) tmp--;
                if (*tmp == ',') {
                    /* use optr as temp buffer */
                    tmp = optr;
                    if( GetTextLine( tmp, MAX_LINE_LEN ) == NULL ) {
                        AsmError( UNEXPECTED_END_OF_FILE );
                        return( ERROR );
                    }
                    /* skip leading spaces */
                    while (isspace(*tmp)) tmp++;

                    /* this size check isn't fool-proved yet */
                    if ( strlen(tmp) + count >= MAX_LINE_LEN ) {
                        AsmError( LINE_TOO_LONG );
                        return( ERROR );
                    }
                    strcpy( iptr, tmp );
                    continue;
                }
                /* the end delimiter ( '>' or '}') is missing, but don't
                 flag this as an error! */
#if 0
                AsmError( MISSING_ANGLE_BRACKET_OR_BRACE_IN_LITERAL );
                return( ERROR );
#else
                if ( symbol_o == '<' ) {
                    buf->string_ptr--;
                    count++;
                }
                buf->string_delim = NULLC;
#endif
            }
            buf->string_delim = NULLC; /* v2.0: same for strings beginning with (dbl)quote */
            break;
        } else {
            *optr++ = *iptr++;
            count++;
        }
    }
    if (count == MAX_STRING_LEN) {
        AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
        return( ERROR );
    }
    *optr++ = NULLC;
    buf->value = count;
    p->input = iptr;
    p->output = optr;
    return( NOT_ERROR );
}

#if 0
static void array_mul_add( unsigned char *buf, unsigned base, unsigned num, unsigned size )
/**********************************************************************************************/
{
    while( size-- > 0 ) {
        num += *buf * base;
        *(buf++) = num;
        num >>= 8;
    }
}
#endif

// read in a number
// check the number suffix:
// b or y: base 2
// d or t: base 10
// h: base 16
// o or q: base 8

static ret_code get_number( struct asm_tok *buf, ioptrs *p )
/**********************************************************/
{
    char                *ptr = p->input;
    char                *dig_start;
    char                *dig_end;
    unsigned            len;
    unsigned            base = 0;
    unsigned            digits_seen;
    uint_32             val;
    char                last_char;

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
            if ( last_char >= 'a' &&
                last_char <= 'f' &&
                ( isalnum( *(ptr+1) ) ||
                 (last_char + 10 - 'a') < ModuleInfo.radix ) )
                digits_seen |= 1 << (last_char + 10 - 'a');
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
    case 'h':
        base = 16;
        dig_end = ptr;
        ptr++;
        break;
    case 'b':
    case 'y':
        if( OK_NUM( BINARY ) ) {
            base = 2;
            dig_end = ptr;
            ptr++;
        }
        break;
    case 'd':
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
    //case 'r': 
        /* note that a float MUST contain a dot
         * OR be ended with an "r" ('r' is not Masm compatible!)
         * 1234e78 is NOT a valid float
         */
        return( get_float( buf, p ) );
    default:
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
        if ( digits_seen < (1U << ModuleInfo.radix) )
            base = ModuleInfo.radix;
        break;
    }

    //buf->string_ptr = p->output;
    //memset( buf->bytes, 0, sizeof( buf->bytes ) );
    buf->value64 = 0;
    buf->hivalue64 = 0;

    if ( base != 0 && is_valid_id_char( *ptr ) == FALSE ) {
        buf->token = T_NUM;
        while( dig_start < dig_end ) {
            uint_16 *px;
            if( *dig_start <= '9' ) {
                val = *dig_start - '0';
            } else {
                val = tolower( *dig_start ) - 'a' + 10;
            }
            //v2: do the calculation inline and with 2 bytes at once
            //array_mul_add( buf->bytes, base, val, sizeof( buf->bytes ) );
            px = (uint_16 *)buf->bytes;
            for (len = sizeof( buf->bytes ) >> 1; len; len-- ) {
                val += (uint_32)*px * base;
                *(px++) = val;
                val >>= 16;
            };
            ++dig_start;
        }
        //DebugMsg(("get_number: inp=%s, value=%lX\n", p->input, buf->value64 ));
    } else {
        buf->pos = p->input; /* restore input ptr for T_BAD_NUM */
        buf->token = T_BAD_NUM;
        DebugMsg(("get_number: BAD_NUMBER %s, radix=%u, base=%u, ptr=>%s<, digits_seen=%Xh\n", dig_start, ModuleInfo.radix, base, ptr, digits_seen ));
        /* swallow remainder of token */
        while( is_valid_id_char( *ptr ) ) ++ptr;
        /* don't display an error here, it will cause
         * 'nondigit in number' later in expression evaluator
         */
        //AsmError( INVALID_NUMBER_DIGIT );
    }

    len = ptr - p->input;
    memcpy( p->output, p->input, len );

    p->output += len;
    *p->output++ = NULLC;
    p->input = ptr;

    return( NOT_ERROR );
}

#if BACKQUOTES
static ret_code get_id_in_backquotes( struct asm_tok *buf, ioptrs *p )
/********************************************************************/
{
    //buf->string_ptr = p->output;
    //buf->pos = p->input;
    buf->token = T_ID;
    buf->value = 0;

    p->input++;         /* strip off the backquotes */
    for( ; *p->input != '`'; ) {
        *p->output++ = *p->input++;
        if( *p->input == '\0' || *p->input == ';' ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
    }
    p->input++;         /* skip the terminating '`' */
    *p->output++ = '\0';
    return( NOT_ERROR );
}
#endif

// get an ID. will always return NOT_ERROR.

static ret_code get_id( struct asm_tok *buf, ioptrs *p )
/******************************************************/
{
    struct ReservedWord *resw;
    int  index;

    //buf->string_ptr = p->output;
    //buf->pos = p->input;
    buf->value = 0;

    do {
        *(p->output)++ = *(p->input)++;
    } while ( is_valid_id_char( *p->input ) );
    *(p->output)++ = '\0';

    /* now decide what to do with it */

    if( buf->string_ptr[0] == '?' && buf->string_ptr[1] == '\0' ) {
        buf->token = T_QUESTION_MARK;
        return( NOT_ERROR );
    }
    resw = FindResWord( buf->string_ptr );
    if( resw == NULL ) {
        /* if ID begins with a DOT, check for OPTION DOTNAME.
         * if not set, skip the token and return a T_DOT instead!
         */
        if ( buf->string_ptr[0] == '.' && ModuleInfo.dotname == FALSE ) {
           buf->token = T_DOT;
           buf->string_ptr[1] = NULLC;
           p->output = buf->string_ptr+2;
           p->input = buf->pos+1;
           return( NOT_ERROR );
        }
        buf->token = T_ID;
        return( NOT_ERROR );
    }
    buf->value = resw - AsmResWord; /* is a enum asm_token value */
    index = optable_idx[buf->value];
//  DebugMsg(("found item >%s< in instruction table, rm=%X\n", buf->string_ptr, AsmOpTable[index].rm_byte));

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
#if 0 /* changed for v1.96 */
        if (( AsmOpTable[index].cpu & P_EXT_MASK ) > ( ModuleInfo.curr_cpu & P_EXT_MASK )) {
#else
        if (( AsmOpTable[index].cpu & P_CPU_MASK ) > ( ModuleInfo.curr_cpu & P_CPU_MASK ) ||
            ( AsmOpTable[index].cpu & P_EXT_MASK ) > ( ModuleInfo.curr_cpu & P_EXT_MASK )) {
#endif
            buf->token = T_ID;
            return( NOT_ERROR );
        }
    }

    if( !( resw->flags & RWF_SPECIAL ) ) {
        buf->token = T_INSTRUCTION;
        return( NOT_ERROR );
    }

    /* for RWT_SPECIAL, field <opcode> contains further infos:
     - RWT_REGISTER:       register number (regnum)
     - RWT_DIRECTIVE:      type of directive (dirtype)
     - RWT_UNARY_OPERATOR: operator precedence
     - RWT_TYPE:           index into SimpleType table
     ...
     */
    buf->opcode = AsmOpTable[index].opcode;

    switch (AsmOpTable[index].specialtype) {
    case RWT_REGISTER:
        buf->token = T_REG;
        break;
    case RWT_RES_ID:
        /* DUP, PTR, ADDR, FLAT, VARARG */
        /* BASIC, C, FORTRAN, PASCAL, STDCALL, SYSCALL, FASTCALL */
        /* fall through */
    case RWT_TYPE:   /* BYTE, WORD, FAR, NEAR, FAR16, NEAR32 ... */
        buf->token = T_RES_ID;
        buf->rm_byte = AsmOpTable[index].rm_byte;
        break;
    case RWT_UNARY_OP: /* OFFSET, LOW, HIGH, LOWWORD, HIGHWORD, SHORT, ... */
        buf->token  = T_UNARY_OPERATOR;
        break;
    case RWT_DIRECTIVE:
        buf->token = T_DIRECTIVE;
        buf->flags = AsmOpTable[index].opnd_type[1];
        if ( g_flags == 0 )
            g_flags = buf->flags;
        break;
    case RWT_BINARY_OP: /* GE, GT, LE, LT, EQ, NE, MOD */
        buf->token = T_BINARY_OPERATOR;
        break;
    default:
        DebugMsg(("get_id: found unknown specialtype=%u\n", AsmOpTable[index].specialtype ));
        buf->token = T_ID; /* shouldn't happen */
        break;
    }
    return( NOT_ERROR );
}

static ret_code get_special_symbol( struct asm_tok *buf, ioptrs *p )
/******************************************************************/
{
    char    symbol;
//    int     i;

    //buf->string_ptr = p->output;
    //buf->pos = p->input;

    symbol = *p->input;
    switch( symbol ) {
    case '.' :
    case ',' :
    case '+' :
    case '-' :
    case '*' :
    case '/' :
    case '[' :
    case ']' :
    case '(' :
    case ')' :
    case ':' :
    case '%' :
        /* all of these are themselves a token */

        buf->token = symbol;
        *(p->output)++ = *(p->input)++;
        *(p->output)++ = '\0';
        break;
    case '=' :
        if (*(p->input+1) != '=') {
            buf->token = T_DIRECTIVE;
            buf->value = T_EQU;
            buf->dirtype = DRT_EQUALSGN; /* to make it differ from EQU directive */
            buf->flags = DF_LABEL;
            *(p->output)++ = *(p->input)++;
            *(p->output)++ = '\0';
            break;
        }
    case '!' :
    case '<' :
        /* a hack to make C style expressions possible */
        /* no_str_delim is TRUE if .IF, .WHILE, .ELSEIF or .UNTIL */
        /* has been detected in the current line */
        /* it will also store "<=" as a string, not as 2 tokens */
        if (g_flags & DF_CEXPR) {
            *(p->output)++ = *(p->input)++;
            buf->value = 1;
            if (*p->input == '=') {
                *(p->output)++ = *(p->input)++;
                buf->value++;
            }
            *(p->output)++ = '\0';
            buf->token = T_STRING;
            buf->bytes[8] = 0x00;
            return (NOT_ERROR);
        }
    case '\'' :
    case '"' :
    case '{' :
        /* string delimiters */
        /* fall through */
    default:
        /* a '<' in the source will prevent comments to be removed */
        /* so they might appear here. Remove! */
        if ((g_flags & DF_CEXPR) && symbol == ';') {
            //while (*p->input) *(p->input)++;// v1.96: replaced by next line
            while (*p->input) p->input++;
            return( EMPTY );
        }
        /* anything we don't recognise we will consider a string,
         * delimited by space characters, commas, newlines or nulls
         */
        return( get_string( buf, p ) );
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

static ret_code GetToken( unsigned int buf_index, ioptrs *p )
/***********************************************************/
{
    if( isdigit( *p->input ) ) {
        return( get_number( AsmBuffer[buf_index], p ) );
    } else if( is_valid_id_char( *p->input )) {
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

int Tokenize( char *string, int index )
/*************************************/
/*
 * create tokens from a source line.
 * string: the line which is to tokenize
 * index: where to start in the token buffer. If index == 0,
 *        then some variables are additionally initialized.
 */
{
    int                         rc;
    ioptrs                      p;
    unsigned int                buf_index;

    if (index == 0) {
#ifdef DEBUG_OUT
        cnttok0++;
#endif
        CurrSource = string;
        p.output = token_stringbuf;
        g_flags = 0;
        expansion = FALSE;
        p.input = string;
        while( isspace( *p.input )) p.input++;
        conditional_assembly_prepare( p.input );
        if (*p.input == '%') {
            *p.input++ = ' ';
            expansion = TRUE;
        }
    } else {
#ifdef DEBUG_OUT
        cnttok1++;
#endif
        p.output = StringBufferEnd;
        p.input = string;
    }

    buf_index = index;

    for( ;; ) {

        while( isspace( *p.input ) ) p.input++;

        AsmBuffer[buf_index]->string_ptr = p.output;
        AsmBuffer[buf_index]->pos = p.input;

        /* comments usually are filtered in lower levels */
        /* but a ';' might appear here as part of a text macro */
        if( *p.input == NULLC || *p.input == ';' ) {
            //*p.input = '\0';
            break;
        }
        rc = GetToken( buf_index, &p );
        if ( rc != NOT_ERROR ) {
            if ( rc == ERROR )
                buf_index = 0; /* skip this line */
            break;
        }
        buf_index++;
        if( buf_index >= MAX_TOKEN ) {
            AsmError( TOO_MANY_TOKENS );
            buf_index = 0;
            break;
        }
    }

    AsmBuffer[buf_index]->token = T_FINAL;
    *p.output++ = NULLC;
    StringBufferEnd = p.output;
    return( buf_index );
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
