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

#include "myassert.h"

/* prototypes */

extern asm_sym          *sym_CurSeg;
extern dir_node         *flat_grp;

// table SegAssume is for the segment registers:
// DS=0, ES=1, SS=2, FS=3, GS=4, CS=5

assume_info      SegAssumeTable[NUM_SEGREGS];

// table StdAssume is for the standard registers:
// (E)AX=0, (E)CX=1, (E)DX=2, (E)BX=3
// (E)SP=4, (E)BP=5, (E)SI=6, (E)DI=7

static assume_info      StdAssumeTable[NUM_STDREGS];

#if FASTPASS
static assume_info saved_SegAssumeTable[NUM_SEGREGS];
static assume_info saved_StdAssumeTable[NUM_STDREGS];
#endif

void SetSegAssumeTable( void *savedstate )
{
    memcpy( &SegAssumeTable, savedstate, sizeof(SegAssumeTable) );
}
void GetSegAssumeTable( void *savedstate )
{
    memcpy( savedstate, &SegAssumeTable, sizeof(SegAssumeTable) );
}
void SetStdAssumeTable( void *savedstate )
{
    memcpy( &StdAssumeTable, savedstate, sizeof(StdAssumeTable) );
}
void GetStdAssumeTable( void *savedstate )
{
    memcpy( savedstate, &StdAssumeTable, sizeof(StdAssumeTable) );
}

#if FASTPASS
void AssumeSaveState( )
{
    GetSegAssumeTable( &saved_SegAssumeTable );
    GetStdAssumeTable( &saved_StdAssumeTable );
}
#endif

void AssumeInit( )
/*********************/
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
/**********************************/
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
            pCS = Options.text_seg;

        strcpy( buffer, "ASSUME CS:" );
        strcat( buffer, pCS);
        strcat( buffer, ", DS:DGROUP" );
        if (ModuleInfo.distance != STACK_FAR)
            strcat( buffer, ", SS:DGROUP" );
        AddLineQueue( buffer );
        break;
    }
}

// convert a standard register token to an index.

static int GetRegisterIndex(int reg, bool * is32)
{
    int j;

    j = AsmOpTable[AsmResWord[reg].position].opnd_type[1];
    if ( j & OP_R1632 ) {
        *is32 = (j & OP_R32) ? TRUE : FALSE;
        return( AsmOpTable[AsmResWord[reg].position].opcode );
    }
    return( ERROR );
}

struct asm_sym * GetStdAssume(int reg)
{
    return (StdAssumeTable[reg].symbol);
}

ret_code AssumeDirective( int i )
/********************/
/* Handles ASSUME statement
 syntax is :
 assume segregister : seglocation [, segregister : seglocation ]
 assume dataregister : qualified type [, dataregister : qualified type ]
 assume register : ERROR | NOTHING | FLAT
 assume NOTHING
 */
{
    int             segloc; /* lcoation of segment/type info */
    int             reg;
    int             j;
    int             type;
    assume_info     *info;
    struct asm_sym  *sym;
    bool            segtable;
    bool            is32;
    static int segidx[NUM_SEGREGS] = {T_DS, T_ES, T_SS, T_FS, T_GS, T_CS};

    DebugMsg(("AssumeDirective enter\n"));

    for( i++; i < Token_Count; i++ ) {
        if( ( AsmBuffer[i]->token == T_ID )
            && (0 == stricmp(AsmBuffer[i]->string_ptr, "NOTHING" ))) {
            AssumeInit();
            continue;
        }

        if (AsmBuffer[i]->token != T_REG) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        reg = AsmBuffer[i]->value;

        i++;

        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        /*---- get the info ptr for the register ----*/

        info = NULL;
        for (j = 0; j < NUM_SEGREGS; j++)
            if (segidx[j] == reg) {
                info = &SegAssumeTable[j];
                if( ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 )
                    && ( ( reg == T_FS ) || ( reg == T_GS ) ) ) {
                    AsmError( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                    return( ERROR );
                }
                segtable = TRUE;
                break;
            }

        if (info == NULL) {
            segtable = FALSE;
            /* convert T_xxx to an index */
            j = GetRegisterIndex( reg, &is32 );
            if ( j != ERROR ) {
                if( is32 && ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
                    AsmError( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                    return( ERROR );
                }
                info = &StdAssumeTable[j];
            }
        }
        if (info == NULL) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }

        if (segtable == TRUE) {
            if( ( AsmBuffer[i]->token == T_UNARY_OPERATOR )
                && ( AsmBuffer[i]->value == T_SEG ) ) {
                i++;
            }
        } else if( ( AsmBuffer[i]->token == T_RES_ID )
                   && ( AsmBuffer[i]->value == T_PTR ) )
            i++;

        segloc = i;
        i++;
        if( AsmBuffer[segloc]->string_ptr == '\0' ) {
            AsmError( SEGLOCATION_EXPECTED );
            return( ERROR );
        }

        /*---- Now store the information ----*/

        if( 0 == stricmp( AsmBuffer[segloc]->string_ptr, "ERROR" )) {
            info->error = TRUE;
            info->flat = FALSE;
            info->symbol = NULL;
        } else if(0 == stricmp( AsmBuffer[segloc]->string_ptr, "FLAT" )) {
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
                AsmError( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                return( ERROR );
            } else if (segtable == FALSE) {
                AsmError( SYNTAX_ERROR );
                return( ERROR );
            };
            DefineFlatGroup();
            info->flat = TRUE;
            info->error = FALSE;
            info->symbol = NULL;
        } else if( 0 == stricmp( AsmBuffer[segloc]->string_ptr, "NOTHING" )) {
            info->flat = FALSE;
            info->error = FALSE;
            info->symbol = NULL;
        } else {
            if (segtable == FALSE) {
                type = -1;
                sym = NULL;
                if (AsmBuffer[segloc]->token == T_RES_ID) {
                    type = FindSimpleType(AsmBuffer[segloc]->value);
                }
                if ( type == -1 ) {
                    sym = IsLabelType(AsmBuffer[segloc]->string_ptr);
                    if (sym == NULL) {
                        AsmErr( QUALIFIED_TYPE_EXPECTED, segloc );
                        return( ERROR );
                    }
                }
            } else {
                sym = SymLookup( AsmBuffer[segloc]->string_ptr );
                if( sym == NULL)
                    return( ERROR );
                if ( ( Parse_Pass != PASS_1 ) && ( sym->state == SYM_UNDEFINED ) ) {
                    AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[segloc]->string_ptr );
                    return( ERROR );
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


/* set CS assume entry whenever current segment is changed */

ret_code SetAssumeCSCurrSeg( void )
/*************************************/
{
    assume_info     *info;

    info = &(SegAssumeTable[ ASSUME_CS ]);
    if( CurrSeg == NULL ) {
        info->symbol = NULL;
        info->flat = FALSE;
        info->error = TRUE;
        sym_CurSeg->string_ptr = "";
    } else {
        info->flat = FALSE;
        info->error = FALSE;
        if( CurrSeg->seg->e.seginfo->group != NULL ) {
            info->symbol = GetGrp( &CurrSeg->seg->sym );
            if ( info->symbol == &flat_grp->sym )
                info->flat = TRUE;
        } else {
            info->symbol = &CurrSeg->seg->sym;
        }
        sym_CurSeg->string_ptr = CurrSeg->seg->sym.name;
        DebugMsg(("SetAssumeCSCurrSeg: current segment=%s\n", CurrSeg->seg->sym.name));
    }
    return( NOT_ERROR );
}

/* for a symbol, search segment register which holds segment
 part of symbol's address in assume table.
 - sym: symbol for which to search segment register
 - def: prefered default register (or ASSUME_NOTHING )
 - search_grps: if TRUE, check groups as well
 */

enum assume_segreg search_assume( struct asm_sym *sym,
                         enum assume_segreg def, bool search_grps )
/**********************************************************/
{
    asm_sym *grp;

    if( sym == NULL )
        return( ASSUME_NOTHING );

    grp = GetGrp(sym);

    /* first check the default segment register */

    if( def != ASSUME_NOTHING ) {
        if( SegAssumeTable[def].symbol != NULL ) {
            if( SegAssumeTable[def].symbol == sym )
                return( def );
            if( search_grps && ( SegAssumeTable[def].symbol == grp ) ) {
                return( def );
            }
        }
    }

    /* now check all segment registers */

    for( def = 0; def < NUM_SEGREGS; def++ ) {
        if( SegAssumeTable[def].symbol == sym ) {
            return( def );
        }
    }
    if( !search_grps )
        return( ASSUME_NOTHING );

    /* now check the groups */
    for( def = 0; def < NUM_SEGREGS; def++ ) {
        if( SegAssumeTable[def].flat && grp == &flat_grp->sym )
            return( def );
        if( SegAssumeTable[def].symbol == NULL )
            continue;
        if( SegAssumeTable[def].symbol == grp ) {
            return( def );
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
/****************************************************************************/
{
    if( SegAssumeTable[override].flat ) {
        return( (asm_sym *)flat_grp );
    }
    return( SegAssumeTable[override].symbol);

}

/*
 in: CodeInfo->prefix.SegOverride = ???
 def: default segment assume value
 */

enum assume_segreg GetAssume( struct asm_sym *sym, enum assume_segreg def, asm_sym * *passume )
/*******************************************************************/
{
    enum assume_segreg  reg;

    if( ( def != ASSUME_NOTHING ) && SegAssumeTable[def].flat ) {
        *passume = (asm_sym *)flat_grp;
        return( def );
    }
    if( CodeInfo->prefix.SegOverride != NULL ) {
        reg = search_assume( CodeInfo->prefix.SegOverride, def, FALSE );
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

