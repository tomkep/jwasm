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
* Description:  data definition directives DB,DW,DD,...
*               directives inside a structure definition will be
*               routed to types.c (AddFieldToStruct()).
*
****************************************************************************/


#include <ctype.h>

#include "globals.h"
#include "codegen.h"
#include "symbols.h"
#include "parser.h"
#include "expreval.h"
#include "input.h"
#include "tbyte.h"
#include "fixup.h"

#if defined( _STANDALONE_ )
  #include "directiv.h"
  #include "types.h"
#endif

#ifndef min
#define min(x,y) (((x) < (y)) ? (x) : (y))
#endif

#if defined( _STANDALONE_ )

/* static globals */
/* is this data element a field in a structure definition? */
static bool             struct_field;
/* is this the first initializer for this field? */
static bool             first;
/* must initializer be floats? */
static bool             float_initializer;
/* used for structured variables */
static bool             veryfirst;
static bool             initwarn;

#endif

/* data initialization stuff */

static void little_endian( char *string, unsigned no_of_bytes )
/*************************************************************/
/* convert a string into little endian format - ( LSB 1st, LSW 1st ... etc ) */
{
        char c;
        //        strrev( string );
        switch (no_of_bytes) {
        case 8:
            c = *string;
            *string = *(string+7);
            *(string+7) = c;
            string++;
        case 6:
            c = *string;
            *string = *(string+5);
            *(string+5) = c;
            string++;
        case 4:
            c = *string;
            *string = *(string+3);
            *(string+3) = c;
            string++;
        case 2:
            c = *string;
            *string = *(string+1);
            *(string+1) = c;
    }
    return;
}

static void output_float( unsigned char index, unsigned no_of_bytes, char negative )
/**********************************************************************************/
{
    double              double_value;
    float               float_value;
    char                *char_ptr;
    uint_8              count;
    TB_LD               tbyte;

    if( no_of_bytes == 10 ) {
        char_ptr = (char *)strtotb( AsmBuffer[index]->string_ptr, &tbyte, negative );
    } else {
        double_value = strtod( AsmBuffer[index]->string_ptr, NULL );
        if( negative )
            double_value *= -1;
        switch( no_of_bytes ) {
        case 1:
        case 2:
#if defined( _STANDALONE_ )
            AsmWarn( 4, FLOAT_OPERAND );
#endif
            char_ptr = (char *)&AsmBuffer[index]->value;
            break;
        case 4:
            float_value = double_value;
            char_ptr = (char *)&float_value;
            break;
        case 8:
            char_ptr = (char *)&double_value;
            break;
        }
    }

    count = 0;
    while( count < no_of_bytes ) {
        OutputDataByte( *char_ptr );
        char_ptr++;
        count++;
    }
    return;
}

#if defined( _STANDALONE_ )
static void update_sizes( asm_sym *sym, bool first, unsigned no_of_bytes )
/************************************************************************/
{
    sym->total_length++;
    sym->total_size += no_of_bytes;
    if( first ) {
        sym->first_length++;
        sym->first_size += no_of_bytes;
    }
}
#endif

/*
 initialize one item of an array;
 call by dup_array() only;
 sym: label (or field if inside a STRUCT)
 struct_sym: type of label/field (or NULL)
 no_of_bytes: size of type
*/

static int array_item( asm_sym *sym, asm_sym *struct_sym, int start_pos, unsigned no_of_bytes, unsigned int dup )
/************************************************************************************************/
{
    int                 cur_pos;
    int                 string_len;
    int                 returned_pos;
    unsigned int        count;
    char                *char_ptr;
    char                negative;
    char                tmp;
    expr_list           opndx;

    DebugMsg(("array_item(sym=%X, type=%X, pos=%d, size=%u, dup=%u) enter\n", sym, struct_sym, start_pos, no_of_bytes, dup));

    for (;dup;dup--) {
    cur_pos = start_pos;
next_item:
    if (AsmBuffer[cur_pos]->token == T_QUESTION_MARK) {
        DebugMsg(("array_item: ? found\n"));
        /* tiny optimization for uninitialized arrays */
        count = no_of_bytes;
        if (AsmBuffer[cur_pos+1]->token != T_COMMA && cur_pos == start_pos) {
            count *= dup;
            if( sym && Parse_Pass == PASS_1 ) {
                sym->total_length += dup;
                sym->total_size += count;
                if( first ) {
                    sym->first_length += dup;
                    sym->first_size += count;
                }
            }
            dup = 1;
        } else {
            if( sym && Parse_Pass == PASS_1 )
                update_sizes( sym, first, count );
        }
        if( !struct_field ) {
            SetCurrOffset( count, TRUE,
                                   ( ( CurrSeg != NULL ) && SEGISCODE( CurrSeg ) ) );
        } else {
            UpdateStructSize(count);
        }

        cur_pos++;
        goto item_done;
    }
    if (EvalOperand(&cur_pos, Token_Count, &opndx, TRUE) == ERROR)
        return(ERROR);
    if (AsmBuffer[cur_pos]->token == T_RES_ID && AsmBuffer[cur_pos]->value == T_DUP) {
        if (opndx.type != EXPR_CONST || opndx.string != NULL) {
            DebugMsg(("array_item, error, opndx.type=%u, opndx.string=%X\n", opndx.type, opndx.string));
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        cur_pos++;
        if( AsmBuffer[cur_pos]->token != T_OP_BRACKET ) {
            DebugMsg(("array_item exit 2, error\n"));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        cur_pos++;
        if (opndx.value == 0) {
            int level = 1;
            for (;AsmBuffer[cur_pos]->token != T_FINAL;cur_pos++) {
                if (AsmBuffer[cur_pos]->token == T_OP_BRACKET)
                    level++;
                else if (AsmBuffer[cur_pos]->token == T_CL_BRACKET)
                    level--;
                if (level == 0)
                    break;
            }
        } else {
            returned_pos = array_item( sym, struct_sym, cur_pos, no_of_bytes, opndx.value );
            if( returned_pos == ERROR ) {
                DebugMsg(("call array_item for DUP, count=%u, returned with error\n", opndx.value));
                return( ERROR );
            }
            cur_pos = returned_pos;
        }
        if( AsmBuffer[cur_pos]->token != T_CL_BRACKET ) {
            DebugMsg(("array_item exit 4, error\n"));
            AsmError( BRACKET_EXPECTED );
            return( ERROR );
        }
        cur_pos++;
    } else {
        switch( opndx.type ) {
        case EXPR_EMPTY:
            negative = FALSE;
            /* evaluator cannot handle '?' and FLOATS! */
            if (AsmBuffer[cur_pos]->token == '-' && AsmBuffer[cur_pos+1]->token == T_FLOAT) {
                cur_pos++;
                negative = TRUE;
            } else if (AsmBuffer[cur_pos]->token == '+' && AsmBuffer[cur_pos+1]->token == T_FLOAT)
                cur_pos++;

            if (AsmBuffer[cur_pos]->token == T_FLOAT) {
                if (!struct_field)
                    output_float( cur_pos, no_of_bytes, negative );
                else {
                    UpdateStructSize(no_of_bytes);
                }
                if( sym && Parse_Pass == PASS_1 ) {
                    update_sizes( sym, first, no_of_bytes );
                }
                cur_pos++;
            } else {
                DebugMsg(("array_item EXPR_EMPTY, idx=%u, AsmBuffer->token=%X\n", cur_pos, AsmBuffer[cur_pos]->token));
                AsmError(SYNTAX_ERROR);
                return(ERROR);
            }
            break;
        case EXPR_CONST:
            if (float_initializer) {
                AsmError( MUST_USE_FLOAT_INITIALIZER );
                return( ERROR );
            }
            if (opndx.string != NULL) {
                DebugMsg(("array_item: string found: >%s<, struct_field=%u, no_of_bytes=%u\n", AsmBuffer[cur_pos]->string_ptr, struct_field, no_of_bytes));
                /* check if a real data item of type STRUCT is to be initialized */
                if( struct_field == FALSE && struct_sym != NULL ) {
                    DebugMsg(("array_item: calling InitializeStructure\n"));
                    if (veryfirst == TRUE) {
                        PushLineQueue();
                        veryfirst = FALSE;
                    }
                    if (NULL == InitializeStructure( sym, struct_sym, opndx.string, TRUE ))
                        return(ERROR);
                    if( sym && Parse_Pass == PASS_1 )
                        update_sizes( sym, first, no_of_bytes );

                    break;
                }
                /* warn about initialized data in BSS */
                if (struct_field == FALSE  &&
                    CurrSeg != NULL &&
                    CurrSeg->seg->e.seginfo->segtype == SEGTYPE_BSS &&
                    Parse_Pass == PASS_2 &&
                    initwarn == FALSE) {
                    AsmWarn( 2, INITIALIZED_DATA_NOT_SUPPORTED_IN_BSS_SEGMENT);
                    initwarn = TRUE;
                };
                char_ptr = opndx.string;
                string_len = strlen(opndx.string);

                /* this code needs cleanup */
                /* a string is possible for a simple data item, but also
                 as a struct field initializer. */
                if (struct_field == FALSE) {
                    if ((*char_ptr != '"') && (*char_ptr != '\'')) {
                        AsmError(UNEXPECTED_LITERAL_FOUND_IN_EXPRESSION);
                        return( ERROR );
                    }
                    /* this shouldn't happen, but just to be safe */
                    if (string_len < 2) {
                        DebugMsg(("array_item: error, string len=%d\n", string_len));
                        AsmError(SYNTAX_ERROR);
                        return(ERROR);
                    }
                    string_len -= 2;
                    char_ptr++;
                } else if (struct_field == TRUE && struct_sym == NULL) {
                    /* it's a struct field initializer */
                    if ((*char_ptr == '"') || (*char_ptr == '\'')) {
                        /* this shouldn't happen, but just to be safe */
                        if (string_len < 2) {
                            DebugMsg(("array_item: error, string len=%d\n", string_len));
                            AsmError(SYNTAX_ERROR);
                            return(ERROR);
                        }
                        string_len -= 2;
                        char_ptr++;
                    }
                }

                /* a string is only regarded as an array if item size is BYTE */
                /* else it is regarded as ONE item */
                if( no_of_bytes != 1 ) {
                    if( string_len > no_of_bytes ) {
                        AsmError( INITIALIZER_OUT_OF_RANGE );
                        return( ERROR );
                    }
                }
                count = 0;

                /* anything bigger than a byte must be stored in little-endian
                 * format -- LSB first */
                if (string_len > 1)
                    little_endian( char_ptr, no_of_bytes );
                if( sym && Parse_Pass == PASS_1 ) {
                    if (no_of_bytes == 1) {
                        if (string_len == 0) {
                            if (struct_field)
                                string_len = 1;
                            else {
                                AsmError(EMPTY_STRING); /* MASM doesn't like "" */
                                return(ERROR);
                            }
                        }
                        update_sizes( sym, first, no_of_bytes );
                        if (string_len > 1) {
                            int i;
                            for (i = string_len-1;i;i--)
                                update_sizes( sym, FALSE, no_of_bytes );
                        }
                    } else
                        update_sizes( sym, first, no_of_bytes );
                }
                if( !struct_field ) {
                    while( count < string_len ) {
                        OutputDataByte( *char_ptr );
                        char_ptr++;
                    count++;
                    }
                    while( count < no_of_bytes ) {
                        OutputDataByte( 0 );
                        char_ptr++;
                        count++;
                    }
                } else {
                    if( Parse_Pass == PASS_1 ) {
                        if (no_of_bytes == 1)
                            count = string_len;
                        else
                            count = no_of_bytes;
                        UpdateStructSize(count);
                    }
                }
            } else {
                /* it's NOT a string */
                char_ptr = (char *)&opndx.value;
                count = 0;
                DebugMsg(("array_item: const (NOT string!) found, no_of_bytes=%u\n", no_of_bytes));
                if( sym && Parse_Pass == PASS_1 ) {
                    update_sizes( sym, first, no_of_bytes );
                }
                if( !struct_field ) {
                    /* warn about initialized data in BSS */
                    if (CurrSeg != NULL &&
                        CurrSeg->seg->e.seginfo->segtype == SEGTYPE_BSS &&
                        Parse_Pass == PASS_2 &&
                        initwarn == FALSE) {
                        AsmWarn( 2, INITIALIZED_DATA_NOT_SUPPORTED_IN_BSS_SEGMENT);
                        initwarn = TRUE;
                    };
                    /* the evaluator cannot handle types > 16 bytes */
                    /* so if a simple type is larger, clear anything
                     which is above */
                    if (no_of_bytes > 16) {
                        while( count < 16 ) {
                            OutputDataByte( *(char_ptr++) );
                            count++;
                        }
                        tmp = 0;
                        if (opndx.xvalue < 0)
                            tmp = 0xFF;
                        while( count < no_of_bytes ) {
                            OutputDataByte(tmp);
                            count++;
                        }
                    } else {
                        while( count < no_of_bytes ) {
                            OutputDataByte( *(char_ptr++) );
                            count++;
                        }
                    }
                    if (Parse_Pass > PASS_1) {
                        while (count < 4) {
                            if (*(char_ptr) != 0 && *(char_ptr) != 0xFF) {
                                DebugMsg(("array_item: error, CONST is %X\n", opndx.value));
                                AsmError(INITIALIZER_MAGNITUDE_TOO_LARGE);
                                return(ERROR);
                            }
                            char_ptr++;
                            count++;
                        }
                    }
                } else {
                    UpdateStructSize(no_of_bytes);
                }
            }
            break;
        case EXPR_ADDR:
            {
                int i;
                int fixup_type;
                char *ptr;
//                long data = 0;
                struct asmfixup     *fixup;

                if (opndx.indirect == TRUE || opndx.sym == NULL) {
                    DebugMsg(("array_item: EXPR_ADDR, error, indirect=%u, sym=%X\n", opndx.indirect, opndx.sym));
                    AsmError( SYNTAX_ERROR );
                    return( ERROR );
                }
                if (float_initializer) {
                    AsmError( MUST_USE_FLOAT_INITIALIZER );
                    return( ERROR );
                }

                if( sym && Parse_Pass == PASS_1 ) {
                    update_sizes( sym, first, no_of_bytes );
                }
                /* for STRUCT fields, just update size. Don't care about fixups */
                if (struct_field) {
                    UpdateStructSize(no_of_bytes);
                    break;
                }

                if (CurrSeg != NULL &&
                    CurrSeg->seg->e.seginfo->segtype == SEGTYPE_BSS &&
                    Parse_Pass == PASS_2 &&
                    initwarn == FALSE) {
                    AsmWarn( 2, INITIALIZED_DATA_NOT_SUPPORTED_IN_BSS_SEGMENT);
                    initwarn = TRUE;
                };

                /* determine what type of fixup is to be created */

                switch (opndx.instr) {
                case T_SEG:
                    if (no_of_bytes < 2) {
                        AsmError( SIZE_TOO_LARGE );
                        return( ERROR );
                    }
                    fixup_type = FIX_SEG;
                    break;
                case T_OFFSET:
                    switch (no_of_bytes) {
                    case 1:
                        AsmError( OFFSET_TOO_LARGE );
                        return( ERROR );
                    case 2:
                        fixup_type = FIX_OFF16;
                        break;
                    default:
                        if (opndx.sym && (SymIs32(opndx.sym) == FALSE))
                            fixup_type = FIX_OFF16;
                        else
                            fixup_type = FIX_OFF32;
                        break;
                    }
                    break;
#if IMAGERELSUPP
                case T_IMAGEREL:
                    if (no_of_bytes != 4) {
                        AsmError( OFFSET_TOO_LARGE );
                        return( ERROR );
                    }
                    fixup_type = FIX_OFF32_IMGREL;
                    break;
#endif
#if SECRELSUPP
                case T_SECTIONREL:
                    if (no_of_bytes != 4) {
                        AsmError( OFFSET_TOO_LARGE );
                        return( ERROR );
                    }
                    fixup_type = FIX_OFF32_SECREL;
                    break;
#endif
                case T_LOW:
                    fixup_type = FIX_LOBYTE;
                    break;
                case T_HIGH:
                    fixup_type = FIX_HIBYTE;
                    // AsmError( CONSTANT_EXPECTED );
                    // return( ERROR );
                    break;
                case T_LOWWORD:
                    if (no_of_bytes < 2) {
                        AsmError( SIZE_TOO_LARGE );
                        return( ERROR );
                    }
                    fixup_type = FIX_OFF16;
                    break;
                case T_HIGHWORD:
                    AsmError( CONSTANT_EXPECTED );
                    return( ERROR );
                default:
                    switch (no_of_bytes) {
                    case 1:
                        AsmError( SIZE_TOO_LARGE );
                        return( ERROR );
                    case 2:
                        // accept "near16" override, else complain
                        // if symbol's offset is 32bit
                        if (opndx.explicit == TRUE && opndx.mem_type == MT_NEAR && opndx.ofs_size == OFSSIZE_16)
                            ;
                        else if (opndx.sym && SymIs32(opndx.sym)) {
                            DebugMsg(("array_item: error, a 32bit offset (%s) wont fit in a WORD\n", opndx.sym->name));
                            AsmError(INITIALIZER_MAGNITUDE_TOO_LARGE);
                        }
                        fixup_type = FIX_OFF16;
                        break;
                    case 4:
                        // masm generates:
                        // off32 if curr segment is 32bit,
                        // ptr16 if curr segment is 16bit,
                        // and ignores type overrides
                        if (opndx.explicit == TRUE) {
                            if (opndx.mem_type == MT_FAR) {
                                if (opndx.ofs_size == OFSSIZE_32)
                                    AsmError(INITIALIZER_MAGNITUDE_TOO_LARGE);
                                fixup_type = FIX_PTR16;
                            } else if (opndx.mem_type == MT_NEAR) {
                                if (opndx.ofs_size == OFSSIZE_16)
                                    fixup_type = FIX_OFF16;
                                else if (opndx.sym && (SymIs32(opndx.sym) == FALSE))
                                    fixup_type = FIX_OFF16;
                                else
                                    fixup_type = FIX_OFF32;
                            }
                        } else {
                            if (CodeInfo->use32)
                                fixup_type = FIX_OFF32;
                            else
                                fixup_type = FIX_PTR16;
                        }
                        break;
                    case 6:
                        // Masm always generates a PTR32 fixup!
                        fixup_type = FIX_PTR32;
                        CodeInfo->info.opnd_type[OPND1] = OP_J48;
                        break;
                    default:
                        // Masm generates
                        // off32 if curr segment is 32bit
                        // ptr16 if curr segment is 16bit
                        // JWasm additionally accepts a FAR32 PTR override
                        // and generates a ptr32 fixup then
                        if (opndx.explicit == TRUE && opndx.mem_type == MT_FAR && opndx.ofs_size == OFSSIZE_32)
                            fixup_type = FIX_PTR32;
                        else if( CodeInfo->use32 )
                            fixup_type = FIX_OFF32;
                        else
                            fixup_type = FIX_PTR16;
                    }
                    break;
                }

                /* updates global vars Frame and Frame_Datum */
                find_frame( opndx.sym );

                /* uses Frame, Frame_Datum and Opnd_Count(=OPND1) */
                fixup = AddFixup( opndx.sym, fixup_type, OPTJ_NONE );
                InsFixups[OPND1] = fixup;

                // data += fixup->offset;
                fixup->offset += opndx.value;

                CodeInfo->data[OPND1] = fixup->offset;

                if( store_fixup( OPND1 ) == ERROR )
                    return( ERROR );

                /* now actually output the data */
                ptr = (char *)&CodeInfo->data[OPND1];

                /* only output up to 4 bytes of offset (segment is on fixup) */
                for( i = 0; i < min( no_of_bytes, 4 ); i++ ) {
                    OutputDataByte( *ptr );
                    ptr++;
                }
                /* leave space for segment */
                for( ; i < no_of_bytes; i++ ) {
                    OutputDataByte( 0 );
                }
            }
            break;
        case EXPR_REG:
            AsmError(INVALID_USE_OF_REGISTER);
            return(ERROR);
        default:
            DebugMsg(("array_item: error, opndx.type=%u\n", opndx.type));
            AsmError(SYNTAX_ERROR);
            return(ERROR);
        }
    }
item_done:
    if( AsmBuffer[cur_pos]->token == T_COMMA ) {
        first = FALSE;
        cur_pos++;
        goto next_item;
    }

    } /* end for */

    DebugMsg(("array_item: exit without error, cur_pos=%d\n", cur_pos));
    return( cur_pos );
}

/*
 parse data initialization assembly line:
 [label] simple|arbitrary type initializer,...
 sym_loc: label pos (or -1 if there is none)
 initializer_loc: type pos
*/

int data_init( int sym_loc, int initializer_loc)
/***********************************************/
{
    unsigned            no_of_bytes;
    memtype             mem_type;
    struct asm_sym      *sym = NULL;
    struct asm_sym      *struct_sym = NULL;
    dir_node            *dir;
    uint                old_offset;
    uint                currofs; /* for LST output */
    int                 returned_loc;
    int                 oldofs;
    char                label_directive = FALSE;

    struct_field = FALSE;
    first = TRUE;
    initwarn = FALSE;
    float_initializer = FALSE;

    DebugMsg(("data_init enter, sym_loc=%d, init_loc=%d\n", sym_loc, initializer_loc));
    if( (sym_loc >= 0) && (StructDef.struct_depth == 0) ) {
        /* get/create the label */
        /* it might be a code label if Masm v5.1 compatibility is enabled */
        DebugMsg(("data_init: calling SymLookup(%s)\n", AsmBuffer[sym_loc]->string_ptr));
        sym = SymLookup( AsmBuffer[sym_loc]->string_ptr );
        if( sym == NULL ) {
            DebugMsg(("data_init exit, error: invalid label name\n"));
            return( ERROR );
        }
    }

    switch( AsmBuffer[initializer_loc]->value ) {
    case T_DB:
    case T_BYTE:
        mem_type = MT_BYTE;
        no_of_bytes = 1;
        break;
    case T_SBYTE:
        mem_type = MT_SBYTE;
        no_of_bytes = 1;
        break;
    case T_DW:
    case T_WORD:
        mem_type = MT_WORD;
        no_of_bytes = 2;
        break;
    case T_SWORD:
        mem_type = MT_SWORD;
        no_of_bytes = 2;
        break;
    case T_REAL4:
        float_initializer = TRUE;
    case T_DD:
    case T_DWORD:
        mem_type = MT_DWORD;
        no_of_bytes = 4;
        break;
    case T_SDWORD:
        mem_type = MT_SDWORD;
        no_of_bytes = 4;
        break;
    case T_DF:
    case T_FWORD:
        mem_type = MT_FWORD;
        no_of_bytes = 6;
        break;
    case T_REAL8:
        float_initializer = TRUE;
    case T_DQ:
    case T_QWORD:
        mem_type = MT_QWORD;
        no_of_bytes = 8;
        break;
    case T_REAL10:
        float_initializer = TRUE;
    case T_DT:
    case T_TBYTE:
        mem_type = MT_TBYTE;
        no_of_bytes = 10;
        break;
    case T_OWORD:
        mem_type = MT_OWORD;
        no_of_bytes = 16;
        break;
    // case T_STRUC:
    case T_STRUCT:
        /* if the parser found a TYPE id, it puts T_STRUCT in AsmBuffer */
        DebugMsg(("data_init: arbitrary type, calling SymSearch\n"));
        struct_sym = SymSearch( AsmBuffer[initializer_loc]->string_ptr );
        mem_type = MT_TYPE;
        if (sym)
            sym->type = struct_sym;
        veryfirst = TRUE;
        if (((dir_node *)struct_sym)->e.structinfo->typekind == TYPE_STRUCT &&
            ((dir_node *)struct_sym)->e.structinfo->OrgInside == TRUE) {
            AsmError( STRUCT_CANNOT_BE_INSTANCED );
            return( ERROR );
        }
        no_of_bytes = struct_sym->total_size;
        if (no_of_bytes == 0) {
            dir_node * dir = (dir_node *)struct_sym;
            /* a void type is not valid */
            if ( dir->e.structinfo->typekind == TYPE_TYPEDEF ) {
                AsmErr( INVALID_TYPE_FOR_DATA_DECLARATION, AsmBuffer[initializer_loc]->string_ptr );
                return( ERROR );
            }
        }
        break;
    default:
        DebugMsg(("data_init: unknown label type\n"));
        AsmErr( INVALID_TYPE_FOR_DATA_DECLARATION, AsmBuffer[initializer_loc]->string_ptr );
        return( ERROR );
    }

    if( AsmBuffer[ initializer_loc + 1 ]->token == T_FINAL ) {
        DebugMsg(("data_init: no initializer found\n"));
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    if( sym_loc >= 0 && AsmBuffer[ sym_loc ]->value == T_LABEL ) {
        label_directive = TRUE;
        sym_loc--;
    }
    /* in a struct declaration? */
    if( StructDef.struct_depth != 0 ) {

        /* everything will be done in the first pass */
        /* problem: listing output! */
        if( Parse_Pass != PASS_1 )
            return( NOT_ERROR );

        if (!(sym = AddFieldToStruct( sym_loc, initializer_loc, mem_type, struct_sym, no_of_bytes ))) {
            return ( ERROR );
        }

        if (AsmFiles.file[LST])
            currofs = StructDef.curr_struct->sym.total_size;

        struct_field = TRUE;
        DebugMsg(("data_init: %s, AddFieldToStruct called, ofs=%X, struct=%s\n",
                  sym->name,
                  sym->offset,
                  StructDef.curr_struct->sym.name));
        if( array_item( sym, NULL, initializer_loc + 1, no_of_bytes, 1 ) == ERROR ) {
            DebugMsg(("data_init: exit 4, array_item returned with error\n"));
            return( ERROR );
        }

        if (AsmFiles.file[LST])
            WriteLstFile(LSTTYPE_STRUCT, currofs, sym->name);

        DebugMsg(("data_init: exit without error\n"));
        return( NOT_ERROR );
    }

    if (AsmFiles.file[LST])
        currofs = GetCurrOffset();

    /* is a label declared or is it just a data definition? */
    if( sym_loc >= 0 ) {
        if( Parse_Pass == PASS_1 ) {
            /* if it's an EXTERNDEF, remove the external info */
            if( sym->state == SYM_EXTERNAL && sym->weak == 1 ) {
                dir_free( (dir_node *)sym, TRUE);
            } else if( sym->state != SYM_UNDEFINED ) {
                DebugMsg(("data_init: exit 5 with error\n"));
                AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
                return( ERROR );
            }
            /* for EXTERNDEF, check type changes */
            if (sym->mem_type != MT_EMPTY) {
                if (sym->mem_type != mem_type) {
                    AsmErr( SYMBOL_TYPE_CONFLICT, sym->name );
                }
            }
            /* add the label to the linked list attached to curr segment */
            /* this allows to reduce the number of passes (see AsmFixup.c) */
            if (CurrSeg) {
                ((dir_node *)sym)->next = (dir_node *)CurrSeg->seg->e.seginfo->labels;
                CurrSeg->seg->e.seginfo->labels = sym;
            }
        } else {
            old_offset = sym->offset;
        }
        SetSymSegOfs( sym );
        if( Parse_Pass != PASS_1 && sym->offset != old_offset ) {
#ifdef DEBUG_OUT
            if (!PhaseError)
                DebugMsg(("data_init: Phase error, pass %u, sym >%s< first time, %X != %X\n", Parse_Pass+1, sym->name, sym->offset, old_offset));
#endif
            PhaseError = TRUE;
        }
        sym->state = SYM_INTERNAL;
        sym->defined = TRUE;
        sym->mem_type = mem_type;
        BackPatch( sym );
    }

    if( label_directive ) {
        DebugMsg(("data_init: exit without error\n"));
        return( NOT_ERROR );
    }

    if (struct_sym) {
        while (struct_sym->mem_type == MT_TYPE)
            struct_sym = struct_sym->type;
        /* if it is just a type alias, skip the arbitrary type */
        if (((dir_node *)struct_sym)->e.structinfo->typekind == TYPE_TYPEDEF )
            struct_sym = NULL;
    }

    returned_loc = array_item( sym, struct_sym, initializer_loc + 1, no_of_bytes, 1 );

    if (returned_loc == ERROR) {
        DebugMsg(("data_init: exit 7 with error\n"));
        return( ERROR );
    }

    if (AsmBuffer[returned_loc]->token != T_FINAL) {
        AsmError(SYNTAX_ERROR);
        return(ERROR);
    }

    if (AsmFiles.file[LST])
        WriteLstFile(LSTTYPE_LIDATA, currofs, NULL);

    DebugMsg(("data_init: exit without error\n"));
    return( NOT_ERROR );
}

