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
* Description:  handles ASSUME
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"
#include "segment.h"
#include "assume.h"
#include "queues.h"
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "fastpass.h"

/* prototypes */

extern dir_node         *flat_grp;

// table SegAssume is for the segment registers:

assume_info      SegAssumeTable[NUM_SEGREGS];

// table StdAssume is for the standard registers:
// (E)AX=0, (E)CX=1, (E)DX=2, (E)BX=3
// (E)SP=4, (E)BP=5, (E)SI=6, (E)DI=7

#if AMD64_SUPPORT
#define NUM_STDREGS 16
#else
#define NUM_STDREGS 8
#endif

static assume_info      StdAssumeTable[NUM_STDREGS];

#if FASTPASS
static assume_info saved_SegAssumeTable[NUM_SEGREGS];
static assume_info saved_StdAssumeTable[NUM_STDREGS];
#endif

/* order to use for assume searches */
static const enum assume_segreg searchtab[] = {
    ASSUME_DS, ASSUME_SS, ASSUME_ES, ASSUME_FS, ASSUME_GS, ASSUME_CS
};

void SetSegAssumeTable( void *savedstate )
/****************************************/
{
    memcpy( &SegAssumeTable, savedstate, sizeof(SegAssumeTable) );
}
void GetSegAssumeTable( void *savedstate )
/****************************************/
{
    memcpy( savedstate, &SegAssumeTable, sizeof(SegAssumeTable) );
}
void SetStdAssumeTable( void *savedstate )
/****************************************/
{
    memcpy( &StdAssumeTable, savedstate, sizeof(StdAssumeTable) );
}
void GetStdAssumeTable( void *savedstate )
/****************************************/
{
    memcpy( savedstate, &StdAssumeTable, sizeof(StdAssumeTable) );
}

#if FASTPASS
void AssumeSaveState( )
/*********************/
{
    GetSegAssumeTable( &saved_SegAssumeTable );
    GetStdAssumeTable( &saved_StdAssumeTable );
}
#endif

void AssumeInit( )
/****************/
{
    int reg;

    for( reg = 0; reg < NUM_SEGREGS; reg++ ) {
        SegAssumeTable[reg].symbol = NULL;
        SegAssumeTable[reg].error = FALSE;
        SegAssumeTable[reg].flat = FALSE;
    }
    for( reg = 0; reg < NUM_STDREGS; reg++ ) {
        StdAssumeTable[reg].symbol = NULL;
        StdAssumeTable[reg].error = FALSE;
    }
#if FASTPASS
    if (Parse_Pass != PASS_1) {
        SetSegAssumeTable( &saved_SegAssumeTable );
        SetStdAssumeTable( &saved_StdAssumeTable );
    }
#endif
}

// generate assume lines after .MODEL directive
// PushLineQueue() has already been called

void ModelAssumeInit( void )
/**************************/
{
    char *pCS;
    char buffer[ 128 ];

    /* Generates codes for assume */
    switch( ModuleInfo.model ) {
    case MOD_FLAT:
        AddLineQueue( "ASSUME CS:FLAT,DS:FLAT,SS:FLAT,ES:FLAT,FS:ERROR,GS:ERROR");
        break;
    case MOD_TINY:
    case MOD_SMALL:
    case MOD_COMPACT:
    case MOD_MEDIUM:
    case MOD_LARGE:
    case MOD_HUGE:
        if (ModuleInfo.model == MOD_TINY)
            pCS = "DGROUP";
        else
            pCS = GetCodeSegName();

        strcpy( buffer, "ASSUME CS:" );
        strcat( buffer, pCS);
        strcat( buffer, ", DS:DGROUP" );
        if (ModuleInfo.distance != STACK_FAR)
            strcat( buffer, ", SS:DGROUP" );
        AddLineQueue( buffer );
        break;
    }
}

struct asm_sym * GetStdAssume( int reg )
/**************************************/
{
    return (StdAssumeTable[reg].symbol);
}

ret_code AssumeDirective( int i )
/*******************************/
/* Handles ASSUME
 * syntax is :
 * assume segregister : seglocation [, segregister : seglocation ]
 * assume dataregister : qualified type [, dataregister : qualified type ]
 * assume register : ERROR | NOTHING | FLAT
 * assume NOTHING
 */
{
    int             segloc; /* lcoation of segment/type info */
    int             reg;
    int             j;
    int             type;
    uint_32         flags;
    int             indirection;
    assume_info     *info;
    struct asm_sym  *sym;
    bool            segtable;

    DebugMsg(( "AssumeDirective(%u) enter\n", Parse_Pass+1 ));

    for( i++; i < Token_Count; i++ ) {
        indirection = 0;
        if( ( AsmBuffer[i]->token == T_ID )
            && (0 == _stricmp(AsmBuffer[i]->string_ptr, "NOTHING" ))) {
            AssumeInit();
            continue;
        }

        if (AsmBuffer[i]->token != T_REG) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        reg = AsmBuffer[i]->value;

        /*---- get the info ptr for the register ----*/

        info = NULL;
        j = GetRegNo( reg );
        flags = GetOpndType( reg, 1 );
        if ( flags & OP_SR ) {
            info = &SegAssumeTable[j];
            segtable = TRUE;
        } else if ( flags & OP_RGT8 ) {
            info = &StdAssumeTable[j];
            segtable = FALSE;
        }
        if (info == NULL) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < GetRegCpu( reg ) ) {
            AsmError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
            return( ERROR );
        }

        i++; /* go past register */

        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;


        if ( segtable == TRUE ) {
            if( ( AsmBuffer[i]->token == T_UNARY_OPERATOR )
                && ( AsmBuffer[i]->value == T_SEG ) ) {
                i++;
            }
        } else if( ( AsmBuffer[i]->token == T_RES_ID )
                  && ( AsmBuffer[i]->value == T_PTR ) ) {
            i++;
            indirection++;
        }

        segloc = i;
        if( AsmBuffer[i]->token == T_FINAL ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        i++;

        /*---- Now store the information ----*/

        if( 0 == _stricmp( AsmBuffer[segloc]->string_ptr, "ERROR" )) {
            info->error = TRUE;
            info->flat = FALSE;
            info->symbol = NULL;
        } else if( 0 == _stricmp( AsmBuffer[segloc]->string_ptr, "FLAT" ) ) {
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
                AsmError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                return( ERROR );
            } else if (segtable == FALSE) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[segloc]->string_ptr );
                return( ERROR );
            };
            DefineFlatGroup();
            info->flat = TRUE;
            info->error = FALSE;
            info->symbol = NULL;
        } else if( 0 == _stricmp( AsmBuffer[segloc]->string_ptr, "NOTHING" )) {
            info->flat = FALSE;
            info->error = FALSE;
            info->symbol = NULL;
        } else {
            if (segtable == FALSE) {
                type = -1;
                sym = NULL;
                if (AsmBuffer[segloc]->token == T_RES_ID) {
                    type = FindStdType( AsmBuffer[segloc]->value );
                }
                if ( type == -1 ) {
                    sym = SymSearch( AsmBuffer[segloc]->string_ptr );
                    if ( sym == NULL ) {/* in pass 1 a forward reference is allowed */
                        sym = SymCreate( AsmBuffer[segloc]->string_ptr, TRUE );
#if FASTPASS
                        /* ensure that directive is rerun in pass 2
                         * so an error msg can be emitted.
                         */
                        if ( StoreState == FALSE )
                            SaveState();
#endif
                    }
                    if ( sym->state == SYM_UNDEFINED ) {
                        DebugMsg(("AssumeDirective: forward referenced symbol %s\n", sym->name ));
                        /* change symbol to a type.
                         * It still has type TYPE_NONE */
                        dir_settype( (dir_node *)sym, SYM_TYPE );
                    }
                    if ( sym->state != SYM_TYPE ||
                       ( Parse_Pass != PASS_1 && ((dir_node *)sym)->e.structinfo->typekind == TYPE_NONE )) {
                        DebugMsg(("AssumeDirective: error, sym %s: state=%u, typekind=%u\n", sym->name, sym->state, ((dir_node *)sym)->e.structinfo->typekind ));
                        if ( sym->state != SYM_TYPE ) /* v2.0: no error if undefined type */
                            AsmErr( QUALIFIED_TYPE_EXPECTED, segloc );
                        return( ERROR );
                    }
                    if ( indirection == 0 && sym->total_size > OperandSize( flags, NULL ) ) {
                        AsmError( TYPE_IS_WRONG_SIZE_FOR_REGISTER );
                        return( ERROR );
                    }
                }
            } else {
                sym = SymLookup( AsmBuffer[segloc]->string_ptr );
                if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
                    if ( Parse_Pass != PASS_1 ) {
                        AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[segloc]->string_ptr );
                    }
#if FASTPASS
                    /* ensure that directive is rerun in pass 2
                     * so an error msg can be emitted.
                     */
                    if ( StoreState == FALSE )
                        SaveState();
#endif
                }
            }
            info->symbol = sym;
            info->flat = FALSE;
            info->error = FALSE;
        }

        /* go past comma */
        if( ( i < Token_Count ) && ( AsmBuffer[i]->token != T_COMMA ) ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}

/* for a symbol, search segment register which holds segment
 * part of symbol's address in assume table.
 * - sym: segment of symbol for which to search segment register
 * - def: prefered default register (or ASSUME_NOTHING )
 * - search_grps: if TRUE, check groups as well
 *
 * for data items, Masm checks assumes in this order:
 *   DS, SS, ES, FS, GS, CS
 */

enum assume_segreg search_assume( struct asm_sym *sym,
                  enum assume_segreg def, bool search_grps )
/**********************************************************/
{
    asm_sym *grp;

    if( sym == NULL )
        return( ASSUME_NOTHING );

    grp = GetGrp( sym );

    /* first check the default segment register */

    if( def != ASSUME_NOTHING ) {
        if( SegAssumeTable[def].symbol == sym )
            return( def );
        if( search_grps && grp ) {
            if( SegAssumeTable[def].flat && grp == &flat_grp->sym )
                return( def );
            if( SegAssumeTable[def].symbol == grp )
                return( def );
        }
    }

    /* now check all segment registers */

    for( def = 0; def < NUM_SEGREGS; def++ ) {
        if( SegAssumeTable[searchtab[def]].symbol == sym ) {
            return( searchtab[def] );
        }
    }

    /* now check the groups */
    if( search_grps && grp )
        for( def = 0; def < NUM_SEGREGS; def++ ) {
            if( SegAssumeTable[searchtab[def]].flat && grp == &flat_grp->sym )
                return( searchtab[def] );
            if( SegAssumeTable[searchtab[def]].symbol == grp ) {
                return( searchtab[def] );
            }
        }

    return( ASSUME_NOTHING );
}

/*
 called by the parser's seg_override() function if
 a segment register override has been detected.
 - override: segment register override (0,1,2,3,4,5)
*/

asm_sym *GetOverrideAssume( enum assume_segreg override )
/*******************************************************/
{
    if( SegAssumeTable[override].flat ) {
        return( (asm_sym *)flat_grp );
    }
    return( SegAssumeTable[override].symbol);

}

/*
 * in:
 * override: SegOverride
 * sym: symbol in current memory operand
 * def: default segment assume value
 */

enum assume_segreg GetAssume( struct asm_sym *override, struct asm_sym *sym, enum assume_segreg def, asm_sym * *passume )
/***********************************************************************************************************************/
{
    enum assume_segreg  reg;

    if( ( def != ASSUME_NOTHING ) && SegAssumeTable[def].flat ) {
        *passume = (asm_sym *)flat_grp;
        return( def );
    }
    if( override != NULL ) {
        reg = search_assume( override, def, FALSE );
    } else {
        reg = search_assume( sym->segment, def, TRUE );
    }
    if( reg == ASSUME_NOTHING ) {
        if( sym && sym->state == SYM_EXTERNAL && sym->segment == NULL ) {
            reg = def;
        }
    }
    if( reg != ASSUME_NOTHING ) {
        *passume = SegAssumeTable[reg].symbol;
        return( reg );
    }
    *passume = NULL;
    return( ASSUME_NOTHING );
}

