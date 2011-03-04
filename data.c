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
* Description:  data definition. handles
*               - directives DB,DW,DD,...
*               - predefined types (BYTE, WORD, DWORD, ...)
*               - arbitrary types
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "expreval.h"
#include "input.h"
#include "tbyte.h"
#include "fixup.h"
#include "listing.h"
#include "segment.h"
#include "types.h"
#include "fastpass.h"
#include "tokenize.h"
#include "macro.h"
#include "omf.h"

#ifndef min
#define min(x,y) (((x) < (y)) ? (x) : (y))
#endif

extern ret_code segm_override( const expr_list *opndx, struct code_info * );
extern struct asm_sym *SegOverride;

/* initialize an array inside a structure
 * if there are no brackets, the next comma, '>' or '}' will terminate
 *
 * valid initialization are:
 * - an expression, might contain DUP or a string enclosed in quotes.
 * - a literal enclosed in <> or {} which then is supposed to contain
 *   single items.
 */
static ret_code InitializeArray( const field_list *f, int *pi )
/*************************************************************/
{
    //int  count;
    char *ptr;
    //char bArray = FALSE;
    int  oldofs;
    unsigned char *pSave;
    int  i = *pi;
    int  j;
    int  lvl;
    char buffer[MAX_LINE_LEN];

    DebugMsg1(("InitializeArray( %s ) enter, items=%lu, type=%s\n", f->sym->name, f->sym->total_length, f->initializer ));

    /* empty the line queue to update the current offset */
    pSave = (unsigned char *)AsmTmpAlloc( GetTokenStateSize() );
    SaveTokenState( pSave );
    if ( line_queue ) {
        RunLineQueueEx();
        RestoreTokenState( pSave ); /* v2.0: restore token buffer state! */
    }
    oldofs = GetCurrOffset();
    DebugMsg1(("InitializeArray(%s): current offset=%X\n", f->sym->name, oldofs ));

    /* If current item is a literal enclosed in <> or {}, just use this
     * item. Else, use all items until a comma or EOL is found.
     * Prior to v2.04, EvalOperand() was called in the latter case. This
     * was an error, because text macros might be found in the
     * expression - which makes the expression evaluator complain.
     */

    if ( AsmBuffer[i]->token != T_STRING ||
         ( AsmBuffer[i]->string_delim != '<' &&
           AsmBuffer[i]->string_delim != '{' )) {
        DebugMsg1(("InitializeArray( %s ): i=%u token=%s\n", f->sym->name, i, AsmBuffer[i]->string_ptr ));
        /* copy items until a comma or EOL is found. */
        j = i;
        for( lvl = 0; AsmBuffer[j]->token != T_FINAL; j++ ) {
            if ( AsmBuffer[j]->token == T_OP_BRACKET )
                lvl++;
            else if ( AsmBuffer[j]->token == T_CL_BRACKET )
                lvl--;
#if 0
            /* v2.05: a "single item" should cause an error.
             * however, this can't be detected here, because the item
             * may be a text macro. This is to be fixed yet.
             */
            else if ( AsmBuffer[j]->token == T_RES_ID && AsmBuffer[j]->value == T_DUP )
                bArray = TRUE;
            else if ( AsmBuffer[j]->token == T_STRING &&
                     ( AsmBuffer[j]->string_delim == '"' ||
                     AsmBuffer[j]->string_delim == '\'') &&
                     (f->sym->mem_type & MT_SPECIAL_MASK) == 0 )
                bArray = TRUE;
#endif
            else if ( lvl == 0 && AsmBuffer[j]->token == T_COMMA )
                break;
        }
        lvl = AsmBuffer[j]->tokpos - AsmBuffer[i]->tokpos;
        memcpy( buffer, AsmBuffer[i]->tokpos, lvl );
        buffer[lvl] = NULLC;
#if 0
        if ( bArray == FALSE )
            AsmErr( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM, buffer );
#endif
        *pi = j;
        AddLineQueueX( "%s %s", f->initializer, buffer );

    } else {

        /* initializer is a literal */

        (*pi)++;

        /* if the string is empty, use the default initializer */
        if ( AsmBuffer[i]->value == 0 ) {
            AddLineQueueX( "%s %s", f->initializer, f->value );
        } else {
            /* for literals enclosed in {} the brackets are part of the literal!
             * They have to be removed temporarily.
             */
            if ( AsmBuffer[i]->string_delim == '{' ) {
                ptr = AsmBuffer[i]->string_ptr + 1;
                *( ptr + AsmBuffer[i]->value ) = NULLC;
                AddLineQueueX("%s %s", f->initializer, ptr );
                *( ptr + AsmBuffer[i]->value ) = '}';
            } else
                AddLineQueueX( "%s %s", f->initializer, AsmBuffer[i]->string_ptr );
        }
    }

    RunLineQueueEx();

    /* the generated line has been assembled and the true size
     * of the array is known now.
     */

    j = GetCurrOffset() - oldofs ;
    DebugMsg1(("InitializeArray(%s): new offset=%X\n", f->sym->name, j + oldofs ));

    RestoreTokenState( pSave );

    if ( j > f->sym->total_size ) {
        AsmErr( TOO_MANY_INITIAL_VALUES_FOR_ARRAY, AsmBuffer[i]->tokpos );
    } else if (j < f->sym->total_size ) {
        DebugMsg1(("InitializeArray: remaining bytes=%lu\n", f->sym->total_size - j ));
        AddLineQueueX( "db %u dup (%s)",
                      f->sym->total_size - j,
                      CurrSeg && CurrSeg->e.seginfo->segtype != SEGTYPE_BSS ? "0" : "?" );
    }

    DebugMsg1(("InitializeArray(%s) exit, curr ofs=%X\n", f->sym->name, GetCurrOffset() ));
    return( NOT_ERROR );
}

#define EXPINIT 0

/* initialize a STRUCT/UNION/RECORD data item
 * symtype:  type of data item
 * index:    index of initializer string
 * embedded: is != NULL if proc is called recursively

 * currently this proc emits ASM lines with simple types
 * to actually "fill" the structure.
 */
static ret_code InitStructuredVar( const dir_node *symtype, int index, asm_sym *embedded )
/****************************************************************************************/
{
    char            *ptr;
#if EXPINIT
    char            *newline;
#endif
    field_list      *f;
    int             nextofs;
    int             i;
    char            *init = NULL;
    char            c;
    unsigned char   *pSave;
    input_queue     *old_line_queue;
    unsigned int    dwRecInit;
    bool            is_record_set;
    expr_list       opndx;
    char            buffer[MAX_LINE_LEN];

#ifdef DEBUG_OUT
#if FASTPASS
    if ( Parse_Pass > PASS_1 && UseSavedState ) {
        DebugMsg(("InitStructuredVar(%s): unexpectedly called in pass %u!!!!!\n", symtype->sym.name, Parse_Pass+1 ));
    }
#endif
    DebugMsg1(("InitStructuredVar(%s) enter, total=%u/%u, init=>%s<, embedded=%s, alignm=%u\n",
              symtype->sym.name, symtype->sym.total_size, symtype->sym.total_length, AsmBuffer[index]->string_ptr, embedded ? embedded->name : "NULL", symtype->e.structinfo->alignment ));
#endif

    if ( AsmBuffer[index]->token == T_STRING ) {
        ptr = AsmBuffer[index]->string_ptr;
        if ( AsmBuffer[index]->string_delim == '<' ) {
            ;
        } else if ( AsmBuffer[index]->string_delim == '{') {
            init = ptr + AsmBuffer[index]->value + 1;
            ptr++;
            c = *init;
            *init = NULLC;
        } else {
            AsmError( MISSING_ANGLE_BRACKET_OR_BRACE_IN_LITERAL );
            return( ERROR );
        }
    } else if ( embedded &&
                (AsmBuffer[index]->token == T_COMMA ||
                 AsmBuffer[index]->token == T_FINAL)) {
        ptr = "";
    } else {
        AsmErr( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM, embedded ? embedded->name : "" );
        return( ERROR );
    }

    /* handle the following code with great care!
     *
     * If embedded == NULL, save the old line queue and
     * create a new one. This is because InitStructuredVar() can
     * be reentered by the call of RunLineQueue() at the end of this
     * function and the current line queue must be preserved then!
     *
     * If embedded != NULL, InitStructuredVar is directly reentered
     * due to an embedded structure/union within the structure. Then
     * RunLineQueue() is NOT called and therefore the queue needn't be
     * saved.
     */
    if ( embedded == NULL ) {
        old_line_queue = line_queue;
        line_queue = NULL;
    }
    /*
     * The state of the token buffer has to be saved/restored in any
     * case, regardless what value parameter <embedded> does have.
     */
    pSave = (unsigned char *)AsmTmpAlloc( GetTokenStateSize() );
    SaveTokenState( pSave );
#if EXPINIT
    /* v2.05: expand the initialization string HERE!
     * this doesn't fully work yet, but should be changed eventually.
     */
    newline = CurrSource + (( strlen( CurrSource ) + 1 + 3 ) & ~3);
    strcpy( newline, ptr );
    ptr = newline;
    Token_Count = Tokenize( ptr, 1, FALSE );
    ExpandLinePart( 1, ptr, FALSE, FALSE );
    i = 1;
#else
    i = Token_Count + 1;
    Token_Count = Tokenize( ptr, i, FALSE );
#endif

    if ( init )
        *init = c;

    if ( symtype->e.structinfo->typekind == TYPE_RECORD ) {
        dwRecInit = 0;
        is_record_set = FALSE;
    }

    ModuleInfo.StructInit++;
    for( f = symtype->e.structinfo->head; f != NULL; f = f->next ) {

        DebugMsg1(("InitStructuredVar field=%s ofs=%u total_size=%lu total_length=%lu initializer=%s default=>%s<\n",
                  f->sym->name,
                  f->sym->offset,
                  f->sym->total_size,
                  f->sym->total_length,
                  f->initializer ? f->initializer : "NULL",
                  f->value ));

        /* is it a RECORD field? */
        if ( f->sym->mem_type == MT_BITS ) {
            if ( AsmBuffer[i]->token == T_COMMA || AsmBuffer[i]->token == T_FINAL ) {
                if ( f->value ) {
                    int j = Token_Count + 1;
                    int max_item = Tokenize( f->value, j, FALSE );
                    EvalOperand( &j, max_item, &opndx, 0 );
                    is_record_set = TRUE;
                } else {
                    opndx.value = 0;
                    opndx.kind = EXPR_CONST;
                    opndx.string = NULL;
                }
            } else {
                EvalOperand( &i, Token_Count, &opndx, 0 );
                is_record_set = TRUE;
            }
            if ( opndx.kind != EXPR_CONST || opndx.string != NULL )
                AsmError( CONSTANT_EXPECTED );
            if ( f->sym->total_size < 32 ) {
                unsigned long dwMax = (1 << f->sym->total_size);
                if ( opndx.value >= dwMax )
                    AsmError( INITIALIZER_MAGNITUDE_TOO_LARGE );
            }
            dwRecInit |= opndx.value << f->sym->offset;

        } else if ( f->initializer == NULL ) { /* embedded struct? */

            InitStructuredVar( (dir_node *)f->sym->type, i, f->sym );
            if ( AsmBuffer[i]->token == T_STRING )
                i++;

        } else if ( f->sym->isarray &&
                    AsmBuffer[i]->token != T_FINAL &&
                    AsmBuffer[i]->token != T_COMMA ) {
            InitializeArray( f, &i );

        } else {
            strcpy( buffer, f->initializer );
            ptr = buffer + strlen( buffer );
            *ptr++ = ' ';
            if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_COMMA ) {
                strcpy( ptr, f->value );
            } else {
                int lvl;
                int j = i;
                /* ignore commas enclosed in () */
                for ( lvl = 0; AsmBuffer[i]->token != T_FINAL; i++ ) {
                    if ( AsmBuffer[i]->token == T_OP_BRACKET )
                        lvl++;
                    else if ( AsmBuffer[i]->token == T_CL_BRACKET )
                        lvl--;
                    else if ( lvl == 0 && AsmBuffer[i]->token == T_COMMA )
                        break;
                }
                lvl = AsmBuffer[i]->tokpos - AsmBuffer[j]->tokpos;
                memcpy( ptr, AsmBuffer[j]->tokpos, lvl );
                *(ptr+lvl) = NULLC;
            }
            AddLineQueue( buffer );
        }

        /* Add padding bytes if necessary (never inside RECORDS!).
         * f->next == NULL : it's the last field of the struct/union/record
         */
        if ( symtype->e.structinfo->typekind != TYPE_RECORD ) {
            if ( f->next == NULL || symtype->e.structinfo->typekind == TYPE_UNION )
                nextofs = symtype->sym.total_size;
            else
                nextofs = f->next->sym->offset;

            if ( f->sym->offset + f->sym->total_size < nextofs ) {
                DebugMsg1(("InitStructuredVar: padding, field=%s ofs=%X total=%X nextofs=%X\n",
                          f->sym->name, f->sym->offset, f->sym->total_size, nextofs ));
                AddLineQueueX( "db %u dup (?) ;padding",
                        nextofs - (f->sym->offset + f->sym->total_size) );
            }
        }

        /* for a union, just the first field is initialized */
        if ( symtype->e.structinfo->typekind == TYPE_UNION )
            break;

        if ( f->next != NULL ) {

            if ( AsmBuffer[i]->token != T_FINAL )
                if ( AsmBuffer[i]->token == T_COMMA )
                    i++;
                else {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->tokpos );
                    while (AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA)
                        i++;
                }
        }
    }  /* end for */

    if ( symtype->e.structinfo->typekind == TYPE_RECORD ) {
        char * p;
        switch ( symtype->sym.mem_type ) {
        case MT_BYTE: p = "db"; break;
        case MT_WORD: p = "dw"; break;
        default:      p = "dd";
        }
        AddLineQueueX( is_record_set ? "%s 0%xh" : "%s ?", p, dwRecInit );
    }

    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( TOO_MANY_INITIAL_VALUES_FOR_STRUCTURE, AsmBuffer[i]->string_ptr );
    }

    /* now run the generated code if embedded is FALSE ... and
     if at least one line has been generated */

    if ( line_queue && ( embedded == NULL ))
        RunLineQueueEx();

    RestoreTokenState( pSave );

    if ( embedded == NULL ) {
        line_queue = old_line_queue;
    }
    ModuleInfo.StructInit--;

    DebugMsg1(("InitStructuredVar(%s) exit, current ofs=%X\n", symtype->sym.name, GetCurrOffset() ));

    return( NOT_ERROR );
}

/*
 * convert a string into little endian format - ( LSB 1st, LSW 1st ... etc ).
 * <size> is the TYPE, may be 2,4,6,8,10?,16
 */
static void little_endian( char *string, unsigned string_len )
/************************************************************/
{
    for( ; string_len > 1; string_len-- ) {
        char c = *string;
        string_len--;
        *string = *(string + string_len);
        *(string + string_len) = c;
        string++;
    }
    return;
}

#if 0 /* changed in v2.03 */
static void OutputDataBytes( const unsigned char *p, int len )
/************************************************************/
{
    for( ; len; len-- )
        OutputByte( *p++ );
}
#else
#define OutputDataBytes( x, y ) OutputBytes( x, y, NULL )
#endif

/* convert hex numbers with "real number designator" to binary.
 * there's more or less the same function in tokenize(), but
 * we shouldn't use the AsmBuffer <value> space for items of
 * type <float>
 */

static void hex2float( void *out, const char *inp, int size )
/***********************************************************/
{
    int val;
    int i;
    int j;
    char *p = out;
    const char *char_ptr = inp;

    memset( (char *)out, 0, size );
    for ( i = 0;
         *char_ptr != NULLC && *char_ptr != 'r';
          char_ptr++, i++ ) {
        if( *char_ptr <= '9' )
            val = *char_ptr - '0';
        else
            val = tolower( *char_ptr ) - 'a' + 10;
        for ( j = 0, p = out; j < size; j++ ) {
            val += *p * 16;
            *(p++) = val;
            val >>= 8;
        }
    }
}

void atofloat( void *out, int index, unsigned size, bool negative )
/*****************************************************************/
{
    const char *inp;
    double  double_value;
    float   float_value;

    inp = AsmBuffer[index]->string_ptr;
    /* v2.04: accept and handle 'real number designator' */
    if ( AsmBuffer[index]->floattype == 'r' ) {
        hex2float( out, inp, size );
    } else {
        switch ( size ) {
        case 4:
            double_value = strtod( inp, NULL );
            if( negative )
                double_value *= -1;
            float_value = double_value;
            *(float *)out = float_value;
            break;
        case 8:
            double_value = strtod( inp, NULL );
            if( negative )
                double_value *= -1;
            *(double *)out = double_value;
            break;
        case 10:
            strtotb( inp, (TB_LD *)out, negative );
            break;
        default:
            /* sizes != 4,8 or 10 aren't accepted.
             * Masm ignores silently, JWasm also unless -W4 is set.
             */
            if ( Parse_Pass == PASS_1 )
                AsmWarn( 4, FP_INITIALIZER_IGNORED );
            memset( (char *)out, 0, size );
        }
    }
    return;
}

static void output_float( struct expr_list *opndx, unsigned size )
/****************************************************************/
{
    char buffer[12];

    if ( opndx->mem_type != MT_EMPTY ) {
        memset( buffer, 0, sizeof( buffer ) );
        atofloat( buffer, opndx->base_reg, SizeFromMemtype( opndx->mem_type, USE_EMPTY, NULL ), opndx->negative );
    } else {
        atofloat( buffer, opndx->base_reg, size, opndx->negative );
    }
    OutputDataBytes( buffer, size );
    return;
}

/* update a symbols's total_length (operator LENGTHOF) and
 * first_length (operator LENGTH) fields.
 * Called in Pass 1 only.
 */

static void update_sizes( asm_sym *sym, bool first, uint_32 size )
/****************************************************************/
{
    sym->total_length++;
    sym->total_size += size;
    if( first ) {
        sym->first_length++;
        sym->first_size += size;
    }
}

/*
 initialize a data item or structure field;
 - sym: label for data items (may be NULL), field for STRUCT declarations.
 - struct_sym: type of label/field if item is a STRUCT/UNION/RECORD, otherwise NULL.
 - start_pos: pointer to index for AsmBuffer token array [in/out]
 - no_of_bytes: size of type
 - dup: array size if called by DUP operator, otherwise 1
*/

static ret_code data_item( asm_sym *sym, const dir_node *struct_sym, int *start_pos, unsigned no_of_bytes, uint_32 dup, bool struct_field, bool float_initializer, bool first )
/*****************************************************************************************************************************************************************************/
{
    int                 cur_pos;
    int                 string_len;
#if FASTPASS
    bool                firstitem = TRUE;
#endif
    bool                initwarn = FALSE;
    //unsigned int        count;
    uint_8              *char_ptr;
    char                tmp;
    enum fixup_types    fixup_type;
    struct fixup        *fixup;
    expr_list           opndx;

    DebugMsg1(("data_item(sym=%s, type=%X, pos=%d, size=%u, dup=%" FX32 "h) enter\n", sym ? sym->name : "NULL", struct_sym, *start_pos, no_of_bytes, dup));

    if ( sym )
        sym->isdata = TRUE;

    for ( ; dup; dup-- ) {
    cur_pos = *start_pos;
next_item:
    /* since v1.94, the expression evaluator won't handle strings
     * enclosed in <> or {}. That is, in previous versions syntax
     * "mov eax,<1>" was accepted, now it's rejected.
     */
    if ( AsmBuffer[cur_pos]->token == T_STRING &&
                ( AsmBuffer[cur_pos]->string_delim == '<' ||
                  AsmBuffer[cur_pos]->string_delim == '{' )) {
        if( struct_sym != NULL ) {
            DebugMsg1(("data_item(%s): literal found: >%s<, struct_field=%u, no_of_bytes=%u, curr_ofs=%X\n",
                       struct_sym, AsmBuffer[cur_pos]->string_ptr, struct_field, no_of_bytes, GetCurrOffset()));
            /* it's either a real data item - then struct_field is FALSE -
             or a structure FIELD of arbitrary type */
            if( struct_field == FALSE ) {
#if FASTPASS
                DebugMsg1(("data_item: calling InitStructuredVar('%s', %u), firstitem=%u\n", struct_sym->sym.name, cur_pos, firstitem ));
                if ( Parse_Pass == PASS_1 ) {
                    /* v2.05: severe error: ignore firstitem if first == 0! */
                    //if ( firstitem ) {
                    if ( first && firstitem ) {
                        /* "remove" the current line" */
                        firstitem = FALSE;
                        *LineStoreCurr->line = ';';
                        LstWriteSrcLine();
                        /* CurrSource holds the source line, which is not
                         to reach pass 2 */
                        *CurrSource = ';';
                    }
                    if ( sym && sym->first_length == 0 ) {
                        char buffer[MAX_LINE_LEN];
                        sprintf( buffer, "%s label %s", sym->name, struct_sym->sym.name );
                        StoreLine( buffer, list_pos );
                        if ( Options.preprocessor_stdout == TRUE )
                            WritePreprocessedLine( buffer );
                    }
                }
#endif
                if ( InitStructuredVar( struct_sym, cur_pos, NULL ) == ERROR )
                    return( ERROR );
            } else {
                UpdateStructSize( no_of_bytes );
            }
            if( sym && Parse_Pass == PASS_1 )
                update_sizes( sym, first, no_of_bytes );
            cur_pos++;
            goto item_done;
        } else {
            DebugMsg(("data_item: invalid initializer for structure >%s< \n", AsmBuffer[cur_pos]->string_ptr ));
            //AsmError( INITIALIZER_MUST_BE_A_STRING_OR_SINGLE_ITEM );
            AsmErr( UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION, AsmBuffer[cur_pos]->string_ptr );
            return( ERROR );
        }
    }

    if ( AsmBuffer[cur_pos]->token == T_QUESTION_MARK )
        opndx.kind = EXPR_EMPTY;
    else
        if ( EvalOperand( &cur_pos, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );

    //DebugMsg(("data_item, EvalOperand() returned, opndx.kind=%u\n", opndx.kind ));

    /* handle DUP operator */

    if ( AsmBuffer[cur_pos]->token == T_RES_ID && AsmBuffer[cur_pos]->value == T_DUP ) {
        /* v2.03: db 'AB' dup (0) is valid syntax! */
        //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
        if ( opndx.kind != EXPR_CONST ) {
            DebugMsg(("data_item, error, opndx.kind=%u, opndx.string=%X\n", opndx.kind, opndx.string));
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        /* max dup is 0x7fffffff */
        if ( opndx.value < 0 ) {
            AsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
        cur_pos++;
        if( AsmBuffer[cur_pos]->token != T_OP_BRACKET ) {
            DebugMsg(("data_item error, missing '('\n"));
            AsmErr( EXPECTED, "(" );
            return( ERROR );
        }
        cur_pos++;

        if ( sym )
            sym->isarray = TRUE;

        if ( opndx.value == 0 ) {
            int level = 1;
            for ( ; AsmBuffer[cur_pos]->token != T_FINAL; cur_pos++ ) {
                if ( AsmBuffer[cur_pos]->token == T_OP_BRACKET )
                    level++;
                else if ( AsmBuffer[cur_pos]->token == T_CL_BRACKET )
                    level--;
                if ( level == 0 )
                    break;
            }
        } else {
            DebugMsg1(("data_item(%s): op DUP, count=%" FX32 "h, calling data_item()\n", sym ? sym->name : "NULL", opndx.uvalue ));
            if ( data_item( sym, struct_sym, &cur_pos, no_of_bytes, opndx.uvalue, struct_field, float_initializer, first ) == ERROR ) {
                DebugMsg(("data_item(%s): op DUP, count=%" FX32 "h, returned with error\n", sym ? sym->name : "NULL", opndx.uvalue ));
                return( ERROR );
            }
        }
        if( AsmBuffer[cur_pos]->token != T_CL_BRACKET ) {
            DebugMsg(("data_item: error 'missing ')', exit\n"));
            AsmErr( EXPECTED, ")" );
            return( ERROR );
        }
        cur_pos++;
    } else {
        /* a STRUCT/UNION/RECORD data item needs a literal as initializer */
        if( struct_sym != NULL && struct_field == FALSE ) {
            AsmErr( STRUCTURE_IMPROPERLY_INITIALIZED, struct_sym->sym.name );
            return( ERROR );
        }

        /* handle '?' */
        if ( opndx.kind == EXPR_EMPTY && AsmBuffer[cur_pos]->token == T_QUESTION_MARK ) {
            DebugMsg1(("data_item: ? found, curr_ofs=%X\n", GetCurrOffset()));
            opndx.uvalue = no_of_bytes;
            /* tiny optimization for uninitialized arrays */
            if ( AsmBuffer[cur_pos+1]->token != T_COMMA && cur_pos == *start_pos ) {
                opndx.uvalue *= dup;
                if( sym && Parse_Pass == PASS_1 ) {
                    sym->total_length += dup;
                    sym->total_size += opndx.uvalue;
                    if( first ) {
                        sym->first_length += dup;
                        sym->first_size += opndx.uvalue;
                    }
                }
                dup = 1; /* force loop exit */
            } else {
                if( sym && Parse_Pass == PASS_1 )
                    update_sizes( sym, first, opndx.uvalue );
            }
            if( !struct_field ) {
                SetCurrOffset( CurrSeg, opndx.uvalue, TRUE, TRUE );
            } else {
                UpdateStructSize( opndx.uvalue );
            }
            cur_pos++;
            goto item_done;
        }

        /* warn about initialized data in BSS/AT segments */
        if ( Parse_Pass == PASS_2 &&
            struct_field == FALSE  &&
            // CurrSeg != NULL &&  /* this is already ensured to be true */
            (CurrSeg->e.seginfo->segtype == SEGTYPE_BSS ||
             CurrSeg->e.seginfo->segtype == SEGTYPE_ABS) &&
            initwarn == FALSE ) {
            AsmWarn( 2,
                    INITIALIZED_DATA_NOT_SUPPORTED_IN_SEGMENT,
                    (CurrSeg->e.seginfo->segtype == SEGTYPE_BSS) ? "BSS" : "AT" );
            initwarn = TRUE;
        };
        switch( opndx.kind ) {
        case EXPR_EMPTY:
            DebugMsg(("data_item.EMPTY: idx=%u, AsmBuffer->token=%X\n", cur_pos, AsmBuffer[cur_pos]->token));
            if ( AsmBuffer[cur_pos]->token != T_FINAL )
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[cur_pos]->string_ptr );
            else
                AsmError( SYNTAX_ERROR );
            return( ERROR );
        case EXPR_FLOAT:
            DebugMsg1(("data_item.FLOAT: >%s<, struct_field=%u, no_of_bytes=%u, curr_ofs=%X\n",
                       AsmBuffer[opndx.base_reg]->string_ptr, struct_field, no_of_bytes, GetCurrOffset()));
            if (!struct_field)
                output_float( &opndx, no_of_bytes );
            else {
                UpdateStructSize( no_of_bytes );
            }
            if( sym && Parse_Pass == PASS_1 ) {
                update_sizes( sym, first, no_of_bytes );
            }
            break;
        case EXPR_CONST:
            if ( float_initializer ) {
                AsmError( MUST_USE_FLOAT_INITIALIZER );
                return( ERROR );
            }

            /* a string returned by the evaluator (enclosed in quotes!)? */

            if ( opndx.string != NULL ) {
                DebugMsg1(("data_item.CONST: string found: >%s<, struct_field=%u, no_of_bytes=%u, curr_ofs=%X\n", opndx.string, struct_field, no_of_bytes, GetCurrOffset()));
                char_ptr = (uint_8 *)opndx.string;
                string_len = strlen( opndx.string );

                /* this shouldn't happen, but just to be safe */
                if ( string_len < 2 ) {
                    DebugMsg(("data_item.CONST: error, string len=%d\n", string_len));
                    AsmErr( SYNTAX_ERROR_EX, char_ptr );
                    return( ERROR );
                }
                string_len -= 2;
                char_ptr++;

                /* a string is only regarded as an array if item size is 1 */
                /* else it is regarded as ONE item */
                if( no_of_bytes != 1 ) {
                    if( string_len > no_of_bytes ) {
                        AsmError( INITIALIZER_OUT_OF_RANGE );
                        return( ERROR );
                    }
                } else if ( string_len == 0 ) {
                    if ( struct_field )
                        string_len = 1;
                    else {
                        AsmError( EMPTY_STRING ); /* MASM doesn't like "" */
                        return( ERROR );
                    }
                }

                /* anything bigger than a byte must be stored in little-endian
                 * format -- LSB first */
                if ( string_len > 1 && no_of_bytes > 1 )
                    little_endian( (char *)char_ptr, string_len );

                if( sym && Parse_Pass == PASS_1 ) {
                    update_sizes( sym, first, no_of_bytes );
                    if ( no_of_bytes == 1 && string_len > 1 ) {
                        int i;
                        for ( i = string_len-1; i; i-- )
                            update_sizes( sym, FALSE, no_of_bytes );
                    }
                }


                if( !struct_field ) {
                    OutputDataBytes( char_ptr, string_len );

                    if ( no_of_bytes > string_len )
                        FillDataBytes( 0, no_of_bytes - string_len );
                } else {
                    if( Parse_Pass == PASS_1 ) {
                        UpdateStructSize( no_of_bytes == 1 ? string_len : no_of_bytes );
                    }
                }
            } else {
                /* it's NOT a string */
                char_ptr = (uint_8 *)&opndx.value;
                DebugMsg1(("data_item.CONST: const found, value=%" FX32 "h, no_of_bytes=%u, curr_ofs=%" FX32 "\n", opndx.value, no_of_bytes, GetCurrOffset()));
                if( sym && Parse_Pass == PASS_1 ) {
                    update_sizes( sym, first, no_of_bytes );
                }
                if( !struct_field ) {
                    int count = 0;
                    /* the evaluator cannot handle types > 16 bytes */
                    /* so if a simple type is larger, clear anything
                     which is above */
                    if ( no_of_bytes > 16 ) {
                        OutputDataBytes( char_ptr, 16 );
                        tmp = 0;
                        if (opndx.value6479 < 0)
                            tmp = 0xFF;
                        FillDataBytes( tmp, no_of_bytes - 16 );
                    } else {
                        OutputDataBytes( char_ptr, no_of_bytes );
                    }
                    /* check that there's no significant data left
                     * which hasn't been emitted.
                     */
                    if ( Parse_Pass > PASS_1 ) {
                        count = no_of_bytes;
                        char_ptr += no_of_bytes;
                        while ( count < 4 ) {
                            if ( *(char_ptr) != 0 && *(char_ptr) != 0xFF ) {
                                DebugMsg(("data_item.CONST: error, CONST is %X\n", opndx.value));
                                AsmError( INITIALIZER_MAGNITUDE_TOO_LARGE );
                                return( ERROR );
                            }
                            char_ptr++;
                            count++;
                        }
                    }
                } else {
                    UpdateStructSize( no_of_bytes );
                }
            }
            break;
        case EXPR_ADDR:
            /* since a fixup will be created, 8 bytes is max */
            if ( no_of_bytes > sizeof(uint_64) ) {
                AsmError( INVALID_DATA_INITIALIZER );
                break;
            }
#if AMD64_SUPPORT
            if ( ModuleInfo.Ofssize != USE64 )
#endif
                if ( opndx.hvalue && ( opndx.hvalue != -1 || opndx.value >= 0 ) ) {
                    /* v2.05: compared to Masm, the above is too restrictive.
                     * the line below might be better.
                     */
                    //if ( opndx.hvalue != 0 && ( opndx.hvalue != -1 || opndx.value == 0 ) ) {
                    DebugMsg(("data_item.ADDR: displacement doesn't fit in 32 bits: %I64X\n", opndx.value64 ));
                    AsmErr( CONSTANT_VALUE_TOO_LARGE, opndx.value64 );
                    return( ERROR );
                }

            /* indirect addresses (incl. stack variables) are invalid */
            if ( opndx.indirect == TRUE ) {
                DebugMsg(("data_item.ADDR: error, indirect=%u, sym=%X\n", opndx.indirect, opndx.sym ));
                AsmError( INVALID_USE_OF_REGISTER );
                break;
            }
            if (float_initializer) {
                DebugMsg(("data_item.ADDR: error, float_initializer=%u\n", float_initializer ));
                AsmError( MUST_USE_FLOAT_INITIALIZER );
                break;
            }

            if( sym && Parse_Pass == PASS_1 ) {
                update_sizes( sym, first, no_of_bytes );
            }
            /* for STRUCT fields, just update size. Don't care about fixups */
            if ( struct_field ) {
                UpdateStructSize( no_of_bytes );
                break;
            }

            /* determine what type of fixup is to be created */

            switch (opndx.instr) {
            case T_SEG:
                if (no_of_bytes < 2) {
                    DebugMsg(("data_item.ADDR: error, a SEG wont fit in a BYTE\n" ));
                    AsmError( SIZE_TOO_LARGE );
                }
                fixup_type = FIX_SEG;
                break;
            case T_OFFSET:
                switch (no_of_bytes) {
                case 1:
                    DebugMsg(("data_item.ADDR: error, a offset wont fit in a BYTE\n" ));
                    AsmError( OFFSET_MAGNITUDE_TOO_LARGE );
                    fixup_type = FIX_OFF8;
                    break;
                case 2:
                    fixup_type = FIX_OFF16;
                    break;
#if AMD64_SUPPORT
                case 8:
                    if ( ModuleInfo.Ofssize == USE64 ) {
                        fixup_type = FIX_OFF64;
                        break;
                    }
#endif
                default:
                    if ( opndx.sym && ( GetSymOfssize(opndx.sym) == USE16 ) )
                        fixup_type = FIX_OFF16;
                    else
                        fixup_type = FIX_OFF32;
                    break;
                }
                break;
#if IMAGERELSUPP
            case T_IMAGEREL:
                if ( no_of_bytes != sizeof(uint_32) ) {
                    DebugMsg(("data_item.ADDR: IMAGEREL, error, size=%u (should be 4)\n", no_of_bytes ));
                    AsmError( OFFSET_MAGNITUDE_TOO_LARGE );
                }
                fixup_type = FIX_OFF32_IMGREL;
                break;
#endif
#if SECTIONRELSUPP
            case T_SECTIONREL:
                if ( no_of_bytes != sizeof(uint_32) ) {
                    DebugMsg(("data_item.ADDR: SECTIONREL, error, size=%u (should be 4)\n", no_of_bytes ));
                    AsmError( OFFSET_MAGNITUDE_TOO_LARGE );
                }
                fixup_type = FIX_OFF32_SECREL;
                break;
#endif
            case T_LOW:
                fixup_type = FIX_OFF8; /* OMF, BIN + GNU-ELF only */
                break;
            case T_HIGH:
                DebugMsg(("data_item.ADDR: HIGH detected\n"));
                fixup_type = FIX_HIBYTE; /* OMF only */
                break;
            case T_LOWWORD:
                fixup_type = FIX_OFF16;
                if ( no_of_bytes < 2 ) {
                    AsmError( SIZE_TOO_LARGE );
                    break;
                }
                break;
            case T_HIGHWORD:
                fixup_type = FIX_OFF16;
                AsmError( CONSTANT_EXPECTED );
                break;
#if LOHI32
            case T_LOW32:
                fixup_type = FIX_OFF32;
                if ( no_of_bytes < 4 ) {
                    AsmError( SIZE_TOO_LARGE );
                    break;
                }
                break;
            case T_HIGHWORD:
                fixup_type = FIX_OFF32;
                AsmError( CONSTANT_EXPECTED );
                break;
#endif
            default:
                /* size < 2 can work with T_LOW|T_HIGH operator only */
                if ( no_of_bytes < 2) {
                    /* forward reference? */
                    if ( Parse_Pass == PASS_1 && opndx.sym && opndx.sym->state == SYM_UNDEFINED)
                        ;
                    else {
                        DebugMsg(("data_item.ADDR: error, no of bytes=%u\n", no_of_bytes ));
                        AsmError( SIZE_TOO_LARGE );
                        fixup_type = FIX_OFF8;
                        break;
                    }
                }
                /* if the symbol references a segment or group,
                 then generate a segment fixup.
                 */
                if ( opndx.sym && (opndx.sym->state == SYM_SEG || opndx.sym->state == SYM_GRP )) {
                    fixup_type = FIX_SEG;
                    break;
                }

                switch (no_of_bytes) {
                case 2:
                    /* accept "near16" override, else complain
                     * if symbol's offset is 32bit */
                    if (opndx.explicit == TRUE && opndx.mem_type == MT_NEAR && opndx.Ofssize == USE16 )
                        ;
                    else if ( opndx.sym &&
                             opndx.sym->state != SYM_UNDEFINED &&
                             ( GetSymOfssize(opndx.sym) > USE16 ) ) {
                        DebugMsg(("data_item.ADDR: error, a 32bit offset (%s) wont fit in a WORD\n", opndx.sym->name));
                        AsmError( INITIALIZER_MAGNITUDE_TOO_LARGE );
                    }
                    fixup_type = FIX_OFF16;
                    break;
                case 4:
                    /* masm generates:
                     * off32 if curr segment is 32bit,
                     * ptr16 if curr segment is 16bit,
                     * and ignores type overrides.
                     * if it's a NEAR external, size is 16, and
                     * format isn't OMF, error 'symbol type conflict'
                     * is displayed
                     */
                    if ( opndx.explicit == TRUE ) {
                        if ( opndx.mem_type == MT_FAR ) {
                            if ( opndx.Ofssize > USE16 ) {
                                DebugMsg(("data_item.ADDR: error, FAR32 won't fit in a DWORD\n" ));
                                AsmError( INITIALIZER_MAGNITUDE_TOO_LARGE );
                            }
                            fixup_type = FIX_PTR16;
                        } else if ( opndx.mem_type == MT_NEAR ) {
                            if ( opndx.Ofssize == USE16 )
                                fixup_type = FIX_OFF16;
                            else if ( opndx.sym && ( GetSymOfssize( opndx.sym ) == USE16 ) )
                                fixup_type = FIX_OFF16;
                            else
                                fixup_type = FIX_OFF32;
                        }
                    } else {
                        /* what's done if code size is 16 is Masm-compatible.
                         * It's not very smart, however.
                         * A better strategy is to choose fixup type depending
                         * on the symbol's offset size.
                         */
                        //if ( opndx.sym && ( GetSymOfssize( opndx.sym ) == USE16 ) )
                        if ( ModuleInfo.Ofssize == USE16 )
                            if ( opndx.mem_type == MT_NEAR &&
                                ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
                                 || Options.output_format == OFORMAT_ELF
#endif
                                )) {
                                fixup_type = FIX_OFF16;
                                AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
                            } else
                                fixup_type = FIX_PTR16;
                        else
                            fixup_type = FIX_OFF32;
                    }
                    break;
                case 6:
                    /* Masm generates a PTR32 fixup in OMF!
                     * and a DIR32 fixup in COFF.
                     */
                    /* COFF/ELF has no far fixups */
                    if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
                        || Options.output_format == OFORMAT_ELF
#endif
                       ) {
                        fixup_type = FIX_OFF32;
                    } else {
                        fixup_type = FIX_PTR32;
                    }
                    break;
                default:
                    /* Masm generates
                     * off32 if curr segment is 32bit
                     * ptr16 if curr segment is 16bit
                     * JWasm additionally accepts a FAR32 PTR override
                     * and generates a ptr32 fixup then */
                    if ( opndx.explicit == TRUE && opndx.mem_type == MT_FAR && opndx.Ofssize == USE32 )
                        fixup_type = FIX_PTR32;
                    else if( ModuleInfo.Ofssize == USE32 )
                        fixup_type = FIX_OFF32;
#if AMD64_SUPPORT
                    else if( ModuleInfo.Ofssize == USE64 )
                        fixup_type = FIX_OFF64;
#endif
                    else
                        fixup_type = FIX_PTR16;
                }
                break;
            }
#if 1
            /* there might be a segment override.
             * A segment, a group or a segment register.
             */
            SegOverride = NULL;
            segm_override( &opndx, NULL );
#endif
            fixup = NULL;
            if ( write_to_file ) {
                /* set global vars Frame and Frame_Datum */
                /* opndx.sym may be NULL, then SegOverride is set. */
                if ( ModuleInfo.offsettype == OT_SEGMENT &&
                    ( opndx.instr == T_OFFSET || opndx.instr == T_SEG ))
                    find_frame2( opndx.sym );
                else
                    find_frame( opndx.sym );
                /* uses Frame and Frame_Datum  */
                fixup = AddFixup( opndx.sym, fixup_type, OPTJ_NONE );
                //store_fixup( fixup, &opndx.value ); /* may fail, but ignore error! */
            }
            OutputBytes( (unsigned char *)&opndx.value, no_of_bytes, fixup );
            break;
        case EXPR_REG:
            AsmError( INVALID_USE_OF_REGISTER );
            break;
        default: /* type != EXPR_REG, EXPR_ADDR, EXPR_CONST, EXPR_EMPTY? */
            DebugMsg(("data_item: error, opndx.kind=%u\n", opndx.kind ));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
    }
item_done:
    if( AsmBuffer[cur_pos]->token == T_COMMA ) {
        cur_pos++;
       if ( AsmBuffer[cur_pos]->token != T_FINAL &&
           AsmBuffer[cur_pos]->token != T_CL_BRACKET ) {
           first = FALSE;
           if ( sym )
               sym->isarray = TRUE;
           goto next_item;
       }
    }

    } /* end for */

    *start_pos = cur_pos;
    DebugMsg1(("data_item: exit, no error, cur_pos=%d\n", cur_pos));
    return( NOT_ERROR );
}

static ret_code checktypes( const asm_sym *sym, memtype mem_type, const dir_node *struct_sym)
/*******************************************************************************************/
{
    /* for EXTERNDEF, check type changes */
    if ( sym->mem_type != MT_EMPTY ) {
        memtype mem_type2 = sym->mem_type;
        const asm_sym *tmp;
        /* skip alias types */
        tmp = (asm_sym *)struct_sym;
        while ( mem_type == MT_TYPE ) {
            mem_type = tmp->mem_type;
            tmp = tmp->type;
        }
        tmp = sym;
        while ( mem_type2 == MT_TYPE ) {
            mem_type2 = tmp->mem_type;
            tmp = tmp->type;
        }
        if ( mem_type2 != mem_type ) {
            DebugMsg(("checktypes: memtype conflict: %u - %u\n", mem_type2, mem_type ));
            AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}
/*
 * parse data initialization assembly line:
 * [label] simple|arbitrary type initializer,...
 * i: pos of directive (DB,DW,...) or of type (predef or userdef)
 * type_sym: userdef type or NULL
 */

ret_code data_dir( int i, dir_node *type_sym )
/********************************************/
{
    unsigned            no_of_bytes;
    struct asm_sym      *sym = NULL;
    //dir_node            *dir;
    uint                old_offset;
    uint                currofs; /* for LST output */
    //int                 oldofs;
    memtype             mem_type;
    bool                float_initializer = FALSE;
    int                 type;
    int                 lbl_loc;

    DebugMsg1(("data_dir( i=%u, type=%s ) enter\n", i, type_sym ? type_sym->sym.name : "NULL" ));

    /* v2.05: the previous test in parser.c wasn't fool-proved */
    if ( i > 1 && ModuleInfo.m510 == FALSE ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    /* if i == 1, the data item has a label */
    lbl_loc = (( i == 1 ) ? 0 : EMPTY );
    if( CurrStruct == NULL && lbl_loc != EMPTY ) {
        /*
         * it's a labeled data item. get/create the label.
         */
        DebugMsg1(("data_dir: calling SymLookup(%s)\n", AsmBuffer[lbl_loc]->string_ptr));
        sym = SymLookup( AsmBuffer[lbl_loc]->string_ptr );
        if( sym == NULL ) {
            DebugMsg(("data_dir exit, error: invalid label name\n"));
            return( ERROR );
        }
    }

    if ( type_sym ) {
        /* if the parser found a TYPE id, type_sym is != NULL */
        //DebugMsg1(("data_dir: arbitrary type %s, calling SymSearch\n", type_sym->sym.name ));
        //type_sym = SymSearch( AsmBuffer[i]->string_ptr );
        mem_type = MT_TYPE;
        if ( sym )
            sym->type = (asm_sym *)type_sym;
        if ( ((dir_node *)type_sym)->e.structinfo->typekind == TYPE_STRUCT &&
             ((dir_node *)type_sym)->e.structinfo->OrgInside == TRUE) {
            AsmError( STRUCT_CANNOT_BE_INSTANCED );
            return( ERROR );
        }
        no_of_bytes = type_sym->sym.total_size;
        if ( no_of_bytes == 0 ) {
            DebugMsg(("data_dir: size of arbitrary type is 0!\n"));
            /* a void type is not valid */
            if ( ((dir_node *)type_sym)->e.structinfo->typekind == TYPE_TYPEDEF ) {
                AsmErr( INVALID_TYPE_FOR_DATA_DECLARATION, type_sym->sym.name );
                return( ERROR );
            }
        }
    } else {
        /* it's either a type or a data directive. For types, the index
         into the simpletype table is in <value8>, for data directives
         the index is found in the opnd_type[0] field.
         */

        if ( AsmBuffer[i]->token == T_STYPE ) {
            type = AsmBuffer[i]->value8;
        } else if ( AsmBuffer[i]->token == T_DIRECTIVE &&
                   (AsmBuffer[i]->dirtype == DRT_DATADIR )) {
            type = GetSflagsSp( AsmBuffer[i]->value );
        } else {
            AsmErr( INVALID_TYPE_FOR_DATA_DECLARATION, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        /* types NEAR[16|32], FAR[16|32] and PROC are invalid here */
        if ( ( SimpleType[type].mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        mem_type = SimpleType[type].mem_type;
        no_of_bytes = (SimpleType[type].mem_type & MT_SIZE_MASK) + 1;
        if ( mem_type & MT_FLOAT )
            float_initializer = TRUE;

    }
    if( AsmBuffer[i+1]->token == T_FINAL ) {
        DebugMsg(("data_dir: no initializer found\n"));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->tokpos );
        return( ERROR );
    }

    /* in a struct declaration? */
    if( CurrStruct != NULL ) {

        /* structure parsing is done in the first pass only */
        if( Parse_Pass == PASS_1 ) {

            /* current offset isn't necessarily the fields start offset */
            //currofs = CurrStruct->sym.offset;

            if (!(sym = AddFieldToStruct( lbl_loc, i, mem_type, type_sym, no_of_bytes ))) {
                return ( ERROR );
            }
#if FASTPASS
            if ( StoreState ) FStoreLine();
#endif
            currofs = sym->offset;
            DebugMsg1(("data_dir: %s, AddFieldToStruct called, ofs=%X\n", sym->name, sym->offset ));
        } else { /* v2.04: else branch added */
            sym = CurrStruct->e.structinfo->tail->sym;
            currofs = sym->offset;
            CurrStruct->e.structinfo->tail = CurrStruct->e.structinfo->tail->next;
        }
        i++;
        if( data_item( sym, type_sym, &i, no_of_bytes, 1, TRUE, float_initializer, TRUE ) == ERROR ) {
            DebugMsg(("data_dir: exit 4, data_item() returned with error\n"));
            return( ERROR );
        }

        //if ( ModuleInfo.list && Parse_Pass == PASS_1 )
        if ( ModuleInfo.list )
            LstWrite( LSTTYPE_STRUCT, currofs, sym->name );

        if ( AsmBuffer[i]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        DebugMsg1(("data_dir: exit, inside struct declaration (%s.%u), no error\n", CurrStruct->sym.name, CurrStruct->sym.offset ));
        return( NOT_ERROR );
    }

    if( CurrSeg == NULL ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }

    FStoreLine();

    if ( ModuleInfo.CommentDataInCode )
        omf_OutSelect( TRUE );

    if ( ModuleInfo.list ) {
        currofs = GetCurrOffset();
    }

    /* is a label declared or is it just a data definition? */
    if( lbl_loc != EMPTY ) {
        if( Parse_Pass == PASS_1 ) {
            /* if it's an EXTERNDEF, remove the external info */
            if( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
                //if ( checktypes( sym, mem_type, type_sym ) == ERROR )
                //    return( ERROR );
                /* v2.0: display error and continue! */
                checktypes( sym, mem_type, type_sym );
                dir_ext2int( (dir_node *)sym );
                sym->total_size = 0;
                sym->total_length = 0;
                sym->first_length = 0;
            } else if( sym->state == SYM_UNDEFINED ) {
                dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );
                sym->state = SYM_INTERNAL;
#if 1
                /* accept a symbol "redefinition" if addresses and types
                 * do match.
                 */
            } else if ( sym->state == SYM_INTERNAL &&
                       CurrSeg &&
                       sym->segment == (asm_sym *)CurrSeg &&
                       sym->offset == GetCurrOffset() ) {
                if ( checktypes( sym, mem_type, type_sym ) == ERROR )
                    return( ERROR );
                goto label_defined; /* don't relink the label */
#endif
            } else {
                DebugMsg(("data_dir: exit 5 with error\n"));
                AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
                return( ERROR );
            }
            /* add the label to the linked list attached to curr segment */
            /* this allows to reduce the number of passes (see Fixup.c) */
            ((dir_node *)sym)->next = (dir_node *)CurrSeg->e.seginfo->labels;
            CurrSeg->e.seginfo->labels = sym;

        } else {
            old_offset = sym->offset;
        }
    label_defined:
        SetSymSegOfs( sym );
        if( Parse_Pass != PASS_1 && sym->offset != old_offset ) {
#ifdef DEBUG_OUT
            if ( !ModuleInfo.PhaseError )
                DebugMsg(("data_dir: Phase error, pass %u, sym >%s< first time, %X != %X\n", Parse_Pass+1, sym->name, sym->offset, old_offset));
#endif
            ModuleInfo.PhaseError = TRUE;
        }
        sym->isdefined = TRUE;
        sym->mem_type = mem_type;
        /* backpatch for data items? Yes, if the item is defined
         * in a code segment then its offset may change!
         */
        BackPatch( sym );
    }

#if 0
    if( label_directive ) {
        DebugMsg(("data_dir: exit without error\n"));
        return( NOT_ERROR );
    }
#endif

    if ( type_sym ) {
        while ( type_sym->sym.mem_type == MT_TYPE )
            type_sym = (dir_node *)type_sym->sym.type;
        /* if it is just a type alias, skip the arbitrary type */
        if (((dir_node *)type_sym)->e.structinfo->typekind == TYPE_TYPEDEF )
            type_sym = NULL;
    }

    i++;
    if ( data_item( sym, type_sym, &i, no_of_bytes, 1, FALSE, float_initializer, TRUE ) == ERROR ) {
        DebugMsg(("data_dir: exit, error in data_item()\n"));
        return( ERROR );
    }

    if (AsmBuffer[i]->token != T_FINAL) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if ( ModuleInfo.list && ModuleInfo.line_listed == FALSE ) {
#ifdef DEBUG_OUT
        if ( Parse_Pass != PASS_1 || Options.max_passes < 2 ) {
#else
        if ( Parse_Pass != PASS_1 ) {
#endif
            LstWrite( LSTTYPE_DATA, currofs, NULL );
        } else
            LstWriteSrcLine();
    }

    DebugMsg1(("data_dir(ofs=%X): exit without error\n", CurrSeg->sym.offset ));
    return( NOT_ERROR );
}

