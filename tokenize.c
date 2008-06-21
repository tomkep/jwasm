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


#include "globals.h"
#include <ctype.h>

#include "codegen.h"
#include "parser.h"
#include "condasm.h"
#include "directiv.h"

char                    *CurrString; // Current Input Line
char                    *CurrStringEnd; // free space in current line

extern int              get_instruction_position( char *string );

static bool no_str_delim;

bool expansion;

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

#if defined( _STANDALONE_ )

extern global_options   Options;

void GetInsString( enum asm_token token, char *string, int len )
/**************************************************************/
{
    int index;

    if( len > AsmOpcode[ token ].len ) {
        len = AsmOpcode[ token ].len;
        index = AsmOpcode[ token ].index;
        if( AsmChars[index]== '.' ) {
            index++;
            len--;
        }
        strncpy( string, &(AsmChars[ index ]), len );
        string[ len ] = '\0';
    } else {
        *string='\0';
    }
    return;
}
#endif

typedef union {
        float   f;
        long    l;
} NUMBERFL;

static int get_float( struct asm_tok *buf, char **input, char **output )
/**********************************************************************/
{
    /* valid floats look like:  (int)[.(int)][e(int)] */

    char    got_decimal = FALSE;
    char    got_e = FALSE;
    char    *ptr = *input;

    for( ; *ptr != '\0'; ptr++ ) {
        if( isdigit( *ptr ) )
            continue;
        if( isspace( *ptr ) )
            break;
        switch( tolower( *ptr ) ) {
        case '.':
            got_decimal = TRUE;
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
//            return( get_string( buf, input, output ) );
        }
    }

done_scanning_float:

    buf->token = T_FLOAT;
    /* copy the string, fix input & output pointers */
    strncpy( *output, *input, ptr - *input );
    buf->string_ptr = *output;
    *output += ( ptr - *input );
    **output = '\0';
    (*output)++;
    *input = ptr;

    *((float *)(&buf->value)) = atof(buf->string_ptr);
    return( NOT_ERROR );
}

static void array_mul_add( unsigned char *buf, unsigned base, unsigned long num, unsigned size )
{
    while( size-- > 0 ) {
        num += *buf * base;
        *(buf++) = num;
        num >>= 8;
    }
}

static int get_string( struct asm_tok *buf, char **input, char **output )
/***********************************************************************/
{
    char    symbol_o;
    char    symbol_c;
    char    *iptr = *input;
    char    *optr = *output;
    int     count;
    int     level;
    char    buffer[MAX_LINE_LEN];

    /*
     a string might span multiple lines if the last character of the line
     is a comma! It must be enclosed in <> or {}.
     */

    buf->string_ptr = optr;

    symbol_o = *iptr;

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
    case '>':
        if (no_str_delim) {
            *(optr)++ = *(iptr)++;
            buf->value = 1;
            if (*(iptr) == '=') {
                *(optr)++ = *(iptr)++;
                buf->value++;
            }
            *(optr)++ = '\0';
            *input = iptr;
            *output = optr;
            return( NOT_ERROR );
        }
    default:
        /* this is an undelimited string,
         * so just copy it until we hit something that looks like the end
         */

        for(count = 0 ; *iptr != '\0' && !isspace( *iptr ) && *iptr != ','; count++ ) {
            *(optr)++ = *(iptr)++;
        }
        *(optr)++ = '\0';
        buf->value = count;
        *input = iptr;
        *output = optr;
        return( NOT_ERROR );
    }
    if (symbol_o != '<')   /* skip '<>' delimiters, don't skip the others */
        *(optr)++ = symbol_o;

    (iptr)++;
    buf->pos = iptr;

    count = 0;
    level = 0;
    while( count < MAX_STRING_LEN ) {
        if( *iptr == symbol_o ) {
            if( symbol_c ) {
                level++;
                *(optr)++ = *(iptr)++;
                count++;
            } else if( *( iptr + 1 ) == symbol_o ) {
                /* if we see "" in a " delimited string,
                 * treat it as a literal " */
                (iptr)++; /* skip the 1st one */
                *(optr)++ = *(iptr)++; /* keep the 2nd one */
                count++;
            } else {
                if (symbol_o != '<')
                    *(optr)++ = *(iptr);
                *(optr)++ = '\0';
                (iptr)++; /* skip the closing delimiter */
                buf->value = count;
                break;
            }
        } else if( symbol_c && *iptr == symbol_c ) {
            if( level ) {
                level--;
                *(optr)++ = *(iptr)++;
                count++;
            } else {
                /* store the string delimiter unless it is <> */
                if (symbol_o != '<')
                    *(optr)++ = *(iptr);
                *(optr)++ = '\0';
                (iptr)++; /* skip the closing delimiter */
                buf->value = count;
                break;
            }
#if 0
            /* a "" inside a <>/{} string? Won't work, since there's no
              request that quotes must be paired in such an item */
        } else if( *iptr == '"') {
            *optr++ = *iptr++;
            count++;
            while (*iptr != '"' && *iptr != 0 && count < MAX_STRING_LEN-1) {
                *optr++ = *iptr++;
                count++;
            }
            *optr++ = *iptr++;
            count++;
            continue;
#endif
            /* handle literal-character operator "!" */
        } else if( *iptr == '!' && symbol_o == '<' && *(iptr+1) != '\0') {
            iptr++;
            *optr++ = *iptr++;
            count++;
        } else if( *iptr == '\0' || *iptr == '\n' ) {
            if ((symbol_o == '<') || (symbol_o == '{')) {
                /* test if last nonspace character was a comma */
                /* if yes, get next line and continue string scan */
                if (*(optr-1) == ',') {
                    if( ReadTextLine( buffer, MAX_LINE_LEN ) == NULL )
                        return( ERROR );
                    iptr = buffer;
                    /* skip leading spaces */
                    while (isspace(*(iptr))) (iptr)++;
                    continue;
                }
            }
            *optr = '\0';
            *input = iptr;
            *output = optr;
            DebugMsg(("get_string: string >%s<\n", buf->string_ptr));
            // AsmError( SYNTAX_ERROR );
            // return( ERROR );
            return( NOT_ERROR );
        } else {
            *optr++ = *iptr++;
            count++;
        }
    }
    if (count == MAX_STRING_LEN) {
        AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
        return( ERROR );
    }
    *input = iptr;
    *output = optr;
    return( NOT_ERROR );
}

// read in a number
// check the number suffix:
// b or y: base 2
// d or t: base 10
// h: base 16
// o or q: base 8

static int get_number( struct asm_tok *buf, char **input, char **output )
/***********************************************************************/
{
    char                *ptr = *input;
    char                *dig_start;
    char                first_char_0 = FALSE;
    unsigned            extra;
    unsigned            len;
    unsigned            base = 0;
    unsigned            digits_seen;
    unsigned long       val;

#define VALID_BINARY    0x0003
#define VALID_OCTAL     0x00ff
#define VALID_DECIMAL   0x03ff
#define OK_NUM( t )     ((digits_seen & ~VALID_##t) == 0)

    digits_seen = 0;

    extra = 0;
    if( *ptr == '0' ) {
        if( tolower( *(ptr+1) ) == 'x' ) {
            ptr+=2;
            base = 16;
        } else {
            ptr += 1;
            first_char_0 = TRUE;
        }
    }
    dig_start = ptr;
    for( ;; ) {
        if (*ptr >= '0' && *ptr <= '9')
            digits_seen |= 1 << (*ptr - '0');
        else {
            switch( tolower( *ptr ) ) {
            case 'a':
                digits_seen |= 1 << 10;
                break;
            case 'b':
                if( base == 0
                    && OK_NUM( BINARY )
                    && !isxdigit( ptr[1] )
                    && tolower( ptr[1] ) != 'h' ) {
                    base = 2;
                    extra = 1;
                    goto done_scan;
                }
                digits_seen |= 1 << 11;
                break;
            case 'c':
                digits_seen |= 1 << 12;
                break;
            case 'd':
                if( base == 0
                    && OK_NUM( DECIMAL )
                    && !isxdigit( ptr[1] )
                    && tolower( ptr[1] ) != 'h' ) {
                    if( !isalnum( ptr[1] ) && ptr[1] != '_' ) {
                        base = 10;
                        extra = 1;
                    }
                    goto done_scan;
                }
                digits_seen |= 1 << 13;
                break;
            case 'e': /* note that this could NOT be part of a float */
                digits_seen |= 1 << 14;
                break;
            case 'f':
                digits_seen |= 1U << 15;
                break;
            case 'y':
                base = 2;
                extra = 1;
                goto done_scan;
            case 'h':
                base = 16;
                extra = 1;
                goto done_scan;
            case 'q':
            case 'o':
                base = 8;
                extra = 1;
                goto done_scan;
            case '.':
            case 'r':
            /* note that a float MUST contain a dot
             * OR be ended with an "r"
             * 1234e78 is NOT a valid float */

                return( get_float( buf, input, output ) );
            case 't':
                base = 10;
                extra = 1;
                goto done_scan;
            default:
                goto done_scan;
            }
        }
        ++ptr;
    }
done_scan:
    if( digits_seen == 0 ) {
        if( !first_char_0 ) {
            return( get_string( buf, input, output ) );
        }
        digits_seen |= 1;
        first_char_0 = FALSE;
        dig_start = *input;
    }
#if defined( _STANDALONE_ )
    if( !Options.allow_c_octals ) {
        first_char_0 = FALSE;
    }
#endif
    buf->token = T_NUM;
    if( base == 0 ) {
        base = first_char_0 ? 8 : 10;
    }
    switch( base ) {
    case 10:
        if( OK_NUM( DECIMAL ) )
            break;
        /* fall through */
    case 8:
        if( OK_NUM( OCTAL ) )
            break;
        /* fall through */
    case 2:
        if( OK_NUM( BINARY ) )
            break;
        /* fall through */
        //AsmError( INVALID_NUMBER_DIGIT );
        /* swallow remainder of token */
        while( isalnum( *ptr )
            || *ptr == '_'
            || *ptr == '$'
            || *ptr == '@'
            || *ptr == '?' ) {
            ++ptr;
        }
        buf->token = T_BAD_NUM;
        break;
    }
    /* copy the string, fix input & output pointers */
    len = ptr - *input + extra;
    strncpy( *output, *input, len );
    buf->string_ptr = *output;
    *output += len;
    **output = '\0';
    (*output)++;
    *input = ptr + extra;
    memset(buf->bytes, 0, sizeof(buf->bytes));
    while( dig_start < ptr ) {
        if( isdigit( *dig_start ) ) {
            val = *dig_start - '0';
        } else {
            val = tolower( *dig_start ) - 'a' + 10;
        }
        array_mul_add( buf->bytes, base, val, sizeof( buf->bytes ) );
        ++dig_start;
    }
    return( NOT_ERROR );
} /* get_number */

static int get_id_in_backquotes( struct asm_tok *buf, char **input, char **output )
/*********************************************************************************/
{
    buf->string_ptr = *output;
    buf->token = T_ID;
    buf->pos = *input;
    buf->value = 0;

    /* copy char from input to output & inc both */
    (*input)++;             // strip off the backquotes
    for( ; **input != '`'; ) {
        *(*output)++ = *(*input)++;
        if( **input == '\0' || **input == ';' ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
    }
    (*input)++;         /* don't output the last '`' */
    *(*output)++ = '\0';
    return( NOT_ERROR );
}

static int get_id( unsigned int *buf_index, char **input, char **output )
/***********************************************************************/
/* get_id could change buf_index, if a COMMENT directive is found */
{
    struct asm_tok  *buf;
    char            cur_char;
    int             count;

    buf = AsmBuffer[ *buf_index ];

    buf->string_ptr = *output;
    buf->pos = *input;
    if( **input != '\\' ) {
        buf->token = T_ID;
    } else {
        buf->token = T_PATH;
    }
    buf->value = 0;

    *(*output)++ = *(*input)++;
    for( ; ; ) {
        cur_char = **input;
        /* if character is part of a valid name, add it */
        if( is_valid_id_char( cur_char )) {
            *(*output)++ = *(*input)++;
        } else if( cur_char == '\\' ) {
            *(*output)++ = *(*input)++;
            buf->token = T_PATH;
        } else  {
            break;
        }
    }
    *(*output)++ = '\0';

    /* now decide what to do with it */

    if( buf->token == T_PATH ) {
        return( NOT_ERROR );
    }
    count = get_instruction_position( buf->string_ptr );
    if( count == EMPTY ) {
        buf->token = T_ID;
        if( buf->string_ptr[1] == '\0' && buf->string_ptr[0] == '?' ) {
            buf->token = T_QUESTION_MARK;
        }
    } else {
//      DebugMsg(("found item >%s< in instruction table, rm=%X\n", buf->string_ptr, AsmOpTable[count].rm_byte));
        buf->value = AsmOpTable[count].token;
        // count = AsmOpcode[count].position;

        /* OP_SPECIAL might be:
         OP_REGISTER, OP_RES_ID, OP_UNARY_OPERATOR, OP_DIRECTIVE,
         OP_DIRECT_EXPR, OP_ARITHOP
         OP_PTR_MODIFIER might be combined with OP_DIRECTIVE (hack for .IF,...)
         or with OP_RES_ID (for DWORD, ...)
         */

        if( AsmOpTable[count].opnd_type[OPND1] == OP_SPECIAL ) {
            if( AsmOpTable[count].rm_byte & OP_REGISTER ) {
                buf->token = T_REG;
            } else if( AsmOpTable[count].rm_byte & OP_RES_ID ) {
                buf->token = T_RES_ID;
                buf->rm_byte = AsmOpTable[count].rm_byte;
                buf->opcode = AsmOpTable[count].opcode;
            } else if( AsmOpTable[count].rm_byte & OP_UNARY_OPERATOR ) {
                buf->token  = T_UNARY_OPERATOR;
                buf->opcode = AsmOpTable[count].opcode;
            } else if( AsmOpTable[count].rm_byte & OP_DIRECTIVE ) {
                buf->token = T_DIRECTIVE;
                buf->opcode = AsmOpTable[count].opcode;
                /* set flags specific to directive */
                /* bit 0: avoid '<' being used as string delimiter */
                if (AsmOpTable[count].opcode & 1) {
                    no_str_delim = TRUE;
                }
#if defined( _STANDALONE_ )
                switch( AsmOpTable[count].token ) {
                case T_COMMENT:
                    /* save the whole line .. we need to check
                     * if the delim. char shows up 2 times */
                    (*output)++;
                    *(*output) = '\0';
                    (*output)++;
                    (*buf_index)++;
                    buf = AsmBuffer[ *buf_index ];
                    buf->string_ptr = *output;
                    strcpy( buf->string_ptr, *input );
                    (*output) += strlen( *input );
                    *(*output) = '\0';
                    (*output)++;
                    (*input) += strlen( *input );
                    buf->token = T_STRING;
                    buf->value = 0;
                    break;
                } /* default do nothing */
#endif
            } else if( AsmOpTable[count].rm_byte & OP_DIRECT_EXPR ) {
                buf->token = T_DIRECT_EXPR;
            } else if( AsmOpTable[count].rm_byte & OP_ARITHOP ) {
                buf->token = T_INSTRUCTION;
            } else {
                buf->token = T_ID;
            }
        } else {
            buf->token = T_INSTRUCTION;
        }
    }
    return( NOT_ERROR );
}

static int get_special_symbol( struct asm_tok *buf,
                                char **input, char **output )
/***********************************************************/
{
    char    symbol;
    int     i;

    buf->string_ptr = *output;
    buf->pos = *input;

    symbol = **input;
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
        *(*output)++ = *(*input)++;
        *(*output)++ = '\0';
        break;
#if defined( _STANDALONE_ )
    case '=' :
        buf->token = T_DIRECTIVE;
        buf->value = T_EQU2;
        *(*output)++ = *(*input)++;
        *(*output)++ = '\0';
        break;
#endif
    case '!' :
    case '<' :
        /* a hack to make C style expressions possible */
        /* no_str_delim is TRUE if .IF, .WHILE, .ELSEIF or .UNTIL */
        /* has been detected in the current line */
        /* it will also store "<=" as a string, not as 2 tokens */
        if (no_str_delim) {
            *(*output)++ = *(*input)++;
            buf->value = 1;
            if (*(*input) == '=') {
                *(*output)++ = *(*input)++;
                buf->value++;
            }
            *(*output)++ = '\0';
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
        if (no_str_delim && symbol == ';') {
            while (**input) *(*input)++;
            return( EMPTY );
        }
        /* anything we don't recognise we will consider a string,
         * delimited by space characters, commas, newlines or nulls
         */
        return( get_string( buf, input, output ) );
        break;
    }
    return( NOT_ERROR );
}

#if defined( _STANDALONE_ )
static int get_inc_path( unsigned int *buf_index, char **input, char **output )
/*****************************************************************************/
{
    char symbol;

    AsmBuffer[*buf_index]->token = T_PATH;
    AsmBuffer[*buf_index]->value = 0;
    AsmBuffer[*buf_index]->string_ptr = *output;

    while( isspace( **input ) )
        (*input)++;

    symbol = **input;

    switch( symbol ) {
    case '\'' :
    case '"' :
    case '<' :
    case '{' :
        /* string delimiters -- just get the path as a string */
        if( get_special_symbol( AsmBuffer[*buf_index],input,output ) == ERROR ) {
            return( ERROR );
        }
        return( NOT_ERROR );
    default:
        /* otherwise, just copy whatever is here */
        while( **input && !isspace( **input )  ) {
            *(*output)++ = *(*input)++;
        }
        return( NOT_ERROR );
    }
}
#endif

// get one token

static int GetToken(unsigned int * pi, char ** input, char ** output)
{
    int rc;
    char * iptr = *input;
    char * optr = *output;
    unsigned int buf_index = *pi;

//  while( isspace( *iptr ) )  iptr++;

    if( isdigit( *iptr ) ) {
        if( get_number( AsmBuffer[buf_index], &iptr, &optr ) == ERROR ) {
            return( ERROR );
        }
    } else if( is_valid_id_char( *iptr ) || *iptr == '\\') {
        if( get_id( &buf_index, &iptr, &optr ) == ERROR ) {
            return( ERROR );
        }
        /* allow names at pos 0 beginning with '.' and also
         a hack to make ".type" not split in '.' and "type" */
    } else if( *iptr == '.' &&
               (buf_index == 0 ||
                (buf_index > 1 && AsmBuffer[buf_index-1]->token == T_COLON) ||
                ((0 == memicmp(iptr+1,"type",4) && is_valid_id_char(*(iptr+5)) == FALSE)))) {
        if( get_id( &buf_index, &iptr, &optr ) == ERROR ) {
            return( ERROR );
        }
    } else if( *iptr == '`' ) {
        if( get_id_in_backquotes( AsmBuffer[buf_index], &iptr, &optr ) == ERROR ) {
            return( ERROR );
        }
    } else {
        rc = get_special_symbol( AsmBuffer[buf_index], &iptr, &optr );
        if (rc == ERROR || rc == EMPTY) {
            return( rc );
        }
    }
    *pi = buf_index;
    *input  = iptr;
    *output = optr;
    return (NOT_ERROR);
}

int Tokenize( char *string, int index )
/******************************************/
/*
- perform syntax checking on scan line;
- pass back tokens for later use;
- string contains the WHOLE line to scan
- if -1 is returned, no object file will be written anymore
*/
{
    int                         rc;
    char                        *ptr;
    char                        *output_ptr;
    unsigned int                buf_index;
    // stringbuf - buffer in which to store strings
    // must be larger than a line since it is used for string expansion
    static char                 stringbuf[8192];

    if (index == 0) {
        CurrString = string;
        output_ptr = stringbuf;
        no_str_delim = FALSE;
        expansion = FALSE;
        ptr = string;
        while( isspace( *ptr )) ptr++;
        conditional_assembly_prepare( ptr );
        if (*ptr == '%') {
            *ptr++ = ' ';
            expansion = TRUE;
        }
    } else {
        output_ptr = CurrStringEnd;
        ptr = string;
    }

    buf_index = index;

    for( ;; ) {

        while( isspace( *ptr ) ) ptr++;

        AsmBuffer[buf_index]->string_ptr = output_ptr;

        /* comments usually are filtered in lower levels */
        /* but a ';' might appear here as part of a text macro */
        if( *ptr == NULLC || *ptr == ';' ) {
            *ptr = '\0';
            break;
        }
        rc = GetToken(&buf_index, &ptr, &output_ptr);
        if (rc == ERROR) {
            AsmError( SYNTAX_ERROR );
            return (ERROR);
        } else if (rc == EMPTY)
            break;

#if defined( _STANDALONE_ )
        // this mess allows include directives with undelimited file names
        if( AsmBuffer[buf_index]->token == T_DIRECTIVE &&
          ( AsmBuffer[buf_index]->value == T_INCLUDE ||
            AsmBuffer[buf_index]->value == T_INCLUDELIB ) ) {
            buf_index++;
            get_inc_path( &buf_index, &ptr, &output_ptr );
        }
#endif
        buf_index++;
        if( buf_index >= MAX_TOKEN ) {
            AsmError( TOO_MANY_TOKENS );
            buf_index = 0;
            break;
        }
    }

    AsmBuffer[buf_index]->token = T_FINAL;
    *output_ptr='\0';
    output_ptr++;
    CurrStringEnd = output_ptr;
    return( buf_index );
}
