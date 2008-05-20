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


#include "asmglob.h"

#include "parser.h"
#include "asmdefs.h"
#include "asmfixup.h"
#include "labels.h"

#if defined( _STANDALONE_ )

#include "directiv.h"

#include "types.h"

static unsigned             AnonymousCounter = 0;

void PrepAnonLabels( void )
/*************************/
{
    struct asm_sym *sym;
    char buffer[20];

    sprintf( buffer, "L&_%d", AnonymousCounter );
    SymChangeName( "@B", buffer  );
    AnonymousCounter = 0;

    sym = SymSearch( "L&_0" );
    if( sym != NULL ) {
        SymChangeName( sym->name, "@F" );
    }

}

struct asm_sym * IsLabelStruct( char *name )
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

int MakeLabel( char *symbol_name, memtype mem_type, struct asm_sym *vartype, bool bLocal )
/**********************************************/
{
    struct asm_sym      *sym;
#if defined( _STANDALONE_ )
    int                     addr;
    char                    buffer[20];

    DebugMsg(("MakeLabel(%s, memtype=%u, %X, %u) enter\n", symbol_name, mem_type, vartype, bLocal));
    if( CurrSeg == NULL ) 
        AsmError( LABEL_OUTSIDE_SEGMENT );
    if( strcmp( symbol_name, "@@" ) == 0 ) {
        struct asm_sym          *newsym;
        /* anonymous label */

        /* find any references to @F and mark them to here as @B */

        /* find the old @B */
        sym = SymSearch( "@B" );
        if( sym != NULL ) {
            /* change it to some magical name */
            sprintf( buffer, "L&_%d", AnonymousCounter++ );
            SymChangeName( sym->name, buffer );
        }
        sym = SymLookup( "@B" );

        /* change all forward anon. references to this location */
        newsym = SymSearch( "@F" );
        if( newsym != NULL ) {
            sym->fixup = newsym->fixup;
            newsym->fixup = NULL;
        }
        SymTakeOut( "@F" );
        sym->state = SYM_INTERNAL;
        sym->defined = TRUE;
        sym->mem_type = mem_type;
        SetSymSegOfs( sym );
        BackPatch( sym );

        /* now point the @F marker at the next anon. label if we have one */
        sprintf( buffer, "L&_%d", AnonymousCounter+1 );
        sym = SymSearch( buffer );
        if( sym != NULL ) {
            SymChangeName( sym->name, "@F" );
        }
        return( NOT_ERROR );
    }
    if (!Definition.struct_depth) {
        sym = SymLookupLabel( symbol_name, bLocal );
        if( sym == NULL )
            return( ERROR );
        if( Parse_Pass == PASS_1 ) {
            if( sym->state == SYM_UNDEFINED )
                ;
            else if( sym->state == SYM_EXTERNAL &&
                     ((dir_node *)sym)->e.extinfo->weak == 1)
                ;
            else {
                AsmErr( SYMBOL_PREVIOUSLY_DEFINED, symbol_name );
                return( ERROR );
            }
        } else {
            /* save old offset */
            addr = sym->offset;
        }
    }
    /* inside a STRUCT definition? */
    if( Definition.struct_depth != 0 ) {
        if( Parse_Pass == PASS_1 ) {
            if (!(sym = AddFieldToStruct( 0, -1, MT_NEAR, 0, 0 )))
                return( ERROR );
        }
        return( NOT_ERROR );
    } else {
        sym->state = SYM_INTERNAL;
        sym->defined = TRUE;
        SetSymSegOfs( sym );
        DebugMsg(("MakeLabel(%s): ofs=%X\n", sym->name, sym->offset));
    }
    sym->mem_type = mem_type;
    sym->type = vartype; /* if mem_type is MT_TYPE */
    if( Parse_Pass != PASS_1 && sym->offset != addr ) {
#ifdef DEBUG_OUT
        if (!PhaseError)
            DebugMsg(("MakeLabel: Phase error, first time, %X != %X\n", sym->offset, addr));
#endif
        PhaseError = TRUE;
    }
#else
    sym = SymLookup( symbol_name );
    if( sym == NULL )
        return( ERROR );
    if( sym->state != SYM_UNDEFINED ) {
        AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
        return( ERROR );
    }
    sym->state = SYM_INTERNAL;
    sym->addr = AsmCodeAddress;
    sym->mem_type = mem_type;
#endif

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
        if( vartype = IsLabelStruct( AsmBuffer[i+1]->string_ptr ) ) {
            return( MakeLabel( AsmBuffer[i-1]->string_ptr, MT_TYPE, vartype, FALSE ) );
        }
    }
    /* label type PROC is a DIRECTIVE! */
    if( AsmBuffer[i+1]->token == T_RES_ID || AsmBuffer[i+1]->token == T_DIRECTIVE) {
        idx = FindSimpleType(AsmBuffer[i+1]->value);
        if (idx != ERROR) {
            return( MakeLabel( AsmBuffer[i-1]->string_ptr, SimpleType[idx].mem_type, NULL, FALSE ));
        }
    }
    AsmError( INVALID_LABEL_DEFINITION );
    return( ERROR );
}
#endif
