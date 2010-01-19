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
* Description:  Label directive, anonymous labels
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "fixup.h"
#include "labels.h"
#include "directiv.h"
#include "segment.h"
#include "proc.h"
#include "types.h"
#include "listing.h"

void LabelsInit( void )
/*********************/
{
    ModuleInfo.anonymous_label = 0;
}
char * GetCurrAnonLabel( char * buffer )
/**************************************/
{
    sprintf( buffer, "L&_%04u", ModuleInfo.anonymous_label );
    return( buffer );
}
char * GetNextAnonLabel( char * buffer )
/**************************************/
{
    sprintf( buffer, "L&_%04u", ModuleInfo.anonymous_label+1);
    return( buffer );
}

// define a label
// symbol_name: name of the label
// mem_type: its memory type
// vartype: arbitrary type if mem_type is MT_TYPE
// bLocal: local should be defined locally if possible

asm_sym *LabelCreate( const char *symbol_name, memtype mem_type, struct asm_sym *vartype, bool bLocal )
/*****************************************************************************************************/
{
    struct asm_sym      *sym;
    uint_32             addr;
    char                buffer[20];

    DebugMsg(("LabelCreate(%s, memtype=%Xh, %Xh, %u) enter\n", symbol_name, mem_type, vartype, bLocal));

    if ( CurrStruct ) {
#if 0
        /* LABEL directive in a STRUCT definition?  Masm doesn't allow this,
         * it might be a remnant of Wasm syntax, which doesn't know UNION.
         * Currently it doesn't work to "initialize" such a struct, so it
         * has been commented out.
         */
        if( Parse_Pass == PASS_1 ) {
            if (!(sym = AddFieldToStruct( 0, -1, mem_type, (dir_node *)vartype, 0 )))
                return( NULL );
        } else {
            sym = SymSearch( symbol_name );
        }
        return( sym );
#else
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
        return( NULL );
#endif
    }

    if( CurrSeg == NULL ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( NULL );
    }

    if( strcmp( symbol_name, "@@" ) == 0 ) {
        sprintf( buffer, "L&_%04u", ++ModuleInfo.anonymous_label );
        symbol_name = buffer;
    }

    sym = SymLookupLabel( symbol_name, bLocal );
    if( sym == NULL )
        return( sym );
    if( Parse_Pass == PASS_1 ) {
        if( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
            /* don't accept EXTERNDEF for a local label! */
            if (bLocal && CurrProc) {
                DebugMsg(("LabelCreate(%s): error, EXTERNDEF for local label\n", sym->name));
                AsmErr( SYMBOL_REDEFINITION, symbol_name );
                return( NULL );
            }
            /* ensure that type of symbol is compatible! */
            if ( sym->mem_type != MT_EMPTY &&
                 sym->mem_type != mem_type ) {
                AsmErr( SYMBOL_TYPE_CONFLICT, symbol_name );
            }
            dir_internal( (dir_node *)sym );
        } else if( sym->state == SYM_UNDEFINED ) {
            dir_remove_table( (dir_node *)sym );
            sym->state = SYM_INTERNAL;
        } else {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, symbol_name );
            return( NULL );
        }
        /* add the label to the linked list attached to curr segment */
        /* this allows to reduce the number of passes (see fixup.c) */
        if (CurrSeg) {
            ((dir_node *)sym)->next = (dir_node *)CurrSeg->e.seginfo->labels;
            CurrSeg->e.seginfo->labels = sym;
        }
        /* a possible language type set by EXTERNDEF must be kept! */
        if (sym->langtype == LANG_NONE)
            sym->langtype = ModuleInfo.langtype;
    } else {
        /* save old offset */
        addr = sym->offset;
    }

    sym->defined = TRUE;
    sym->mem_type = mem_type;
    sym->type = vartype; /* if mem_type is MT_TYPE */
    SetSymSegOfs( sym );
//  DebugMsg(("LabelCreate(%s): ofs=%X\n", sym->name, sym->offset));

    if( Parse_Pass != PASS_1 && sym->offset != addr ) {
#ifdef DEBUG_OUT
        if (!PhaseError)
            DebugMsg(("LabelCreate: Phase error, pass %u, sym >%s< first time, new=%lX - old=%lX\n", Parse_Pass+1, sym->name, sym->offset, addr));
#endif
        PhaseError = TRUE;
    }
    BackPatch( sym );
    return( sym );
}

/* LABEL directive.
 * syntax: <label_name> LABEL <type>
 * <type> can be a predefined or arbitrary type.
 * PTR and "PTR type" is also accepted.
 */

ret_code LabelDirective( int i )
/******************************/
{
    asm_sym *sym;
    asm_sym *vartype;
    int ptrtype;
    int type;
    int size;

    if( i != 1 ) {  /* LABEL must be preceded by an ID */
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    i++;

    switch ( AsmBuffer[i]->token ) {
    case T_ID:
        vartype = SymSearch( AsmBuffer[i]->string_ptr );
        if ( vartype == NULL ) {
            AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        if ( vartype->state == SYM_TYPE ) {
            /* the label type must be a single item */
            if ( AsmBuffer[i+1]->token != T_FINAL ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+1]->string_ptr );
                return( ERROR );
            }
            if ( ( sym = LabelCreate( AsmBuffer[0]->string_ptr, MT_TYPE, vartype, FALSE )) == NULL )
                return( ERROR );

            if ( ModuleInfo.list )
                LstWrite( LSTTYPE_LABEL, 0, NULL );

            return( NOT_ERROR );
        }
        break;
    case T_DIRECTIVE: /* label type PROC is a DIRECTIVE! */
    case T_RES_ID:
        ptrtype = -1;
        for ( type = -1; AsmBuffer[i]->token != T_FINAL; i++ ) {
            int last;
            if ( AsmBuffer[i]->token == T_DIRECTIVE && AsmBuffer[i]->value == T_PROC )
                AsmBuffer[i]->token = T_RES_ID;
            if ( AsmBuffer[i]->token != T_RES_ID ) {
                if ( AsmBuffer[i]->token == T_ID )
                    i++;
                break;
            }
            last = FindStdType( AsmBuffer[i]->value );
            if ( last == -1 )
                break;

            if ( type == -1 )
                type = last;
            else if ( ptrtype == -1 && ( ( SimpleType[last].mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS ) ) {
                ptrtype = last;
            } else if ( AsmBuffer[i]->value == T_PTR )
                if ( ( SimpleType[type].mem_type & MT_SPECIAL_MASK ) == MT_ADDRESS ) {
                    ptrtype = type;
                    type = last;
                }
            else {
                i++;
                break;
            }
        }
        if ( type == -1 )
            break;

        /* dont allow near16/far16/near32/far32 if size won't match */
        if ( ( ModuleInfo.Ofssize > USE16 && SimpleType[type].Ofssize == USE16 ) ||
            ( ModuleInfo.Ofssize == USE16 && SimpleType[type].Ofssize == USE32 )) {
            AsmError( OFFSET_SIZE_MISMATCH );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        if (( sym = LabelCreate( AsmBuffer[0]->string_ptr, SimpleType[type].mem_type, NULL, FALSE )) == NULL )
            return( ERROR );

        /* make sure that '<label> LABEL PTR' won't define a MT_PTR item */
        if ( SimpleType[type].mem_type == MT_PTR && ptrtype == -1 )
            ptrtype = type;

        if ( ptrtype != -1 ) {
            if ( SimpleType[ptrtype].Ofssize != USE_EMPTY )
                size = SizeFromMemtype( SimpleType[ptrtype].mem_type, SimpleType[ptrtype].Ofssize );
            else
                size = SizeFromMemtype( SimpleType[ptrtype].mem_type, ModuleInfo.Ofssize );
            switch ( size ) {
            case 2:
                sym->mem_type = MT_WORD;
                break;
            case 4:
                sym->mem_type = MT_DWORD;
                break;
            case 6:
                sym->mem_type = MT_FWORD;
                break;
#if AMD64_SUPPORT
            case 8:
                sym->mem_type = MT_QWORD;
                break;
#endif
            }
        }

        if ( ModuleInfo.list )
            LstWrite( LSTTYPE_LABEL, 0, NULL );

        return( NOT_ERROR );
    }
    AsmError( INVALID_LABEL_DEFINITION );
    return( ERROR );
}
