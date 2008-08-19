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

#if defined( _STANDALONE_ )

#include "directiv.h"

#include "types.h"

void LabelsInit( void )
/*************************/
{
    ModuleInfo.anonymous_label = 0;
}
char * GetCurrAnonLabel( char * buffer )
/*************************/
{
    sprintf( buffer, "L&_%04u", ModuleInfo.anonymous_label );
    return( buffer );
}
char * GetNextAnonLabel( char * buffer )
/*************************/
{
    sprintf( buffer, "L&_%04u", ModuleInfo.anonymous_label+1);
    return( buffer );
}

struct asm_sym * IsLabelType( char *name )
/******************************/
{
    asm_sym *sym;

    sym = SymSearch( name );
    if ( sym && (sym->state == SYM_TYPE ))
        return(sym);
    return(NULL);
}
#endif

// define a label
// symbol_name: name of the label
// mem_type: its memory type
// vartype: arbitrary type if mem_type is MT_TYPE
// bLocal: local should be defined locally if possible

int LabelCreate( char *symbol_name, memtype mem_type, struct asm_sym *vartype, bool bLocal )
/**********************************************/
{
    struct asm_sym      *sym;
    int                 addr;
    char                buffer[20];

    DebugMsg(("LabelCreate(%s, memtype=%u, %X, %u) enter\n", symbol_name, mem_type, vartype, bLocal));
    if( CurrSeg == NULL ) 
        AsmError( LABEL_OUTSIDE_SEGMENT );

    if( strcmp( symbol_name, "@@" ) == 0 ) {
        sprintf( buffer, "L&_%04u", ++ModuleInfo.anonymous_label );
        symbol_name = buffer;
    }

    /* inside a STRUCT definition? */
    if (StructDef.struct_depth) {
        if( Parse_Pass == PASS_1 ) {
            if (!(sym = AddFieldToStruct( 0, -1, MT_NEAR, 0, 0 )))
                return( ERROR );
        }
        return( NOT_ERROR );
    }

    sym = SymLookupLabel( symbol_name, bLocal );
    if( sym == NULL )
        return( ERROR );
    if( Parse_Pass == PASS_1 ) {
        if( sym->state == SYM_UNDEFINED )
            ;
        else if( sym->state == SYM_EXTERNAL && sym->weak == 1) {
            /* don't accept EXTERNDEF for a local label! */
            if (bLocal && CurrProc) {
                DebugMsg(("LabelCreate(%s): error, EXTERNDEF for local label\n", sym->name));
                AsmErr( SYMBOL_REDEFINITION, symbol_name );
                return( ERROR );
            }
            dir_free( (dir_node *)sym, TRUE );
        } else {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, symbol_name );
            return( ERROR );
        }
        /* add the label to the linked list attached to curr segment */
        /* this allows to reduce the number of passes (see AsmFixup.c) */
        if (CurrSeg) {
            ((dir_node *)sym)->next = (dir_node *)CurrSeg->seg->e.seginfo->labels;
            CurrSeg->seg->e.seginfo->labels = sym;
        }
        /* a possible language type set by EXTERNDEF must be kept! */
        if (sym->langtype == LANG_NONE)
            sym->langtype = ModuleInfo.langtype;
    } else {
        /* save old offset */
        addr = sym->offset;
    }

    sym->state = SYM_INTERNAL;
    sym->defined = TRUE;
    sym->mem_type = mem_type;
    sym->type = vartype; /* if mem_type is MT_TYPE */
    SetSymSegOfs( sym );
//  DebugMsg(("LabelCreate(%s): ofs=%X\n", sym->name, sym->offset));

    if( Parse_Pass != PASS_1 && sym->offset != addr ) {
#ifdef DEBUG_OUT
        if (!PhaseError)
            DebugMsg(("LabelCreate: Phase error, pass %u, sym >%s< first time, new=%X - old=%X\n", Parse_Pass+1, sym->name, sym->offset, addr));
#endif
        PhaseError = TRUE;
    }
    BackPatch( sym );
    return( NOT_ERROR );
}

#if defined( _STANDALONE_ )
int LabelDirective( int i )
/*************************/
{
    struct asm_sym *vartype;
    int idx;

    if( i != 1 ) {
        AsmError( INVALID_LABEL_DEFINITION );
        return( ERROR );
    }
    if( AsmBuffer[i+1]->token == T_ID ) {
        if( vartype = IsLabelType( AsmBuffer[i+1]->string_ptr ) ) {
            return( LabelCreate( AsmBuffer[i-1]->string_ptr, MT_TYPE, vartype, FALSE ) );
        }
    }
    /* label type PROC is a DIRECTIVE! */
    if( AsmBuffer[i+1]->token == T_RES_ID || AsmBuffer[i+1]->token == T_DIRECTIVE) {
        idx = FindSimpleType(AsmBuffer[i+1]->value);
        if (idx != ERROR) {
            return( LabelCreate( AsmBuffer[i-1]->string_ptr, SimpleType[idx].mem_type, NULL, FALSE ));
        }
    }
    AsmError( INVALID_LABEL_DEFINITION );
    return( ERROR );
}
#endif
