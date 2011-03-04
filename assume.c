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
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "fastpass.h"
#include "types.h"
#include "tokenize.h"
#include "listing.h"

/* prototypes */

/* table SegAssume is for the segment registers: */

struct assume_info SegAssumeTable[NUM_SEGREGS];

/* table StdAssume is for the standard registers:
 * (E)AX=0, (E)CX=1, (E)DX=2, (E)BX=3
 * (E)SP=4, (E)BP=5, (E)SI=6, (E)DI=7
 */
#if AMD64_SUPPORT
#define NUM_STDREGS 16
#else
#define NUM_STDREGS 8
#endif

struct assume_info StdAssumeTable[NUM_STDREGS];

static struct asm_sym *stdsym[NUM_STDREGS];

#if FASTPASS
static struct assume_info saved_SegAssumeTable[NUM_SEGREGS];
static struct assume_info saved_StdAssumeTable[NUM_STDREGS];
/* v2.05: saved type info content */
static struct stdassume_typeinfo saved_StdTypeInfo[NUM_STDREGS];
#endif

/* order to use for assume searches */
static const enum assume_segreg searchtab[] = {
    ASSUME_DS, ASSUME_SS, ASSUME_ES, ASSUME_FS, ASSUME_GS, ASSUME_CS
};

static const char szError[]   = { "ERROR" };
static const char szNothing[] = { "NOTHING" };
const char szDgroup[]  = { "DGROUP" };

void SetSegAssumeTable( void *savedstate )
/****************************************/
{
    DebugMsg(("SetSegAssumeTable\n" ));
    memcpy( &SegAssumeTable, savedstate, sizeof(SegAssumeTable) );
}
void GetSegAssumeTable( void *savedstate )
/****************************************/
{
    DebugMsg(("GetSegAssumeTable\n" ));
    memcpy( savedstate, &SegAssumeTable, sizeof(SegAssumeTable) );
}

/* unlike the segment register assumes, the
 * register assumes need more work to save/restore the
 * current status, because they have their own
 * type symbols, which may be reused.
 */

void SetStdAssumeTable( void *savedstate, struct stdassume_typeinfo *ti )
/***********************************************************************/
{
    int i;
    DebugMsg(("SetStdAssumeTable\n" ));
    memcpy( &StdAssumeTable, savedstate, sizeof(StdAssumeTable) );
    for ( i = 0; i < NUM_STDREGS; i++, ti++ ) {
        if ( StdAssumeTable[i].symbol ) {
            StdAssumeTable[i].symbol->type        = ti->type;
            StdAssumeTable[i].symbol->target_type = ti->target_type;
            StdAssumeTable[i].symbol->mem_type    = ti->mem_type;
            StdAssumeTable[i].symbol->ptr_memtype = ti->ptr_memtype;
            StdAssumeTable[i].symbol->is_ptr      = ti->is_ptr;
        }
    }
}

void GetStdAssumeTable( void *savedstate, struct stdassume_typeinfo *ti )
/***********************************************************************/
{
    int i;
    DebugMsg(("GetStdAssumeTable\n" ));
    memcpy( savedstate, &StdAssumeTable, sizeof(StdAssumeTable) );
    for ( i = 0; i < NUM_STDREGS; i++, ti++ ) {
        if ( StdAssumeTable[i].symbol ) {
            ti->type        = StdAssumeTable[i].symbol->type;
            ti->target_type = StdAssumeTable[i].symbol->target_type;
            ti->mem_type    = StdAssumeTable[i].symbol->mem_type;
            ti->ptr_memtype = StdAssumeTable[i].symbol->ptr_memtype;
            ti->is_ptr      = StdAssumeTable[i].symbol->is_ptr;
        }
    }
}

#if FASTPASS
void AssumeSaveState( )
/*********************/
{
    GetSegAssumeTable( &saved_SegAssumeTable );
    GetStdAssumeTable( &saved_StdAssumeTable, saved_StdTypeInfo );
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
        StdAssumeTable[reg].error = 0;
        stdsym[reg] = NULL;
    }
#if FASTPASS
    if ( Parse_Pass != PASS_1 ) {
        SetSegAssumeTable( &saved_SegAssumeTable );
        SetStdAssumeTable( &saved_StdAssumeTable, saved_StdTypeInfo );
    }
#endif
}

/* generate assume lines after .MODEL directive
 * model is in ModuleInfo.model, it can't be MOD_NONE.
 * PushLineQueue() has already been called
 */
void ModelAssumeInit( void )
/**************************/
{
    const char *pCS;
    const char *pFSassume = szError;
    const char *pGSassume = szError;
    const char *pFmt;

    /* Generates codes for assume */
    switch( ModuleInfo.model ) {
    case MOD_FLAT:
#if AMD64_SUPPORT
        if ( Options.header_format == HFORMAT_WIN64 )
            pGSassume = szNothing;
#endif
        AddLineQueueX( "%r %r:%r,%r:%r,%r:%r,%r:%r,%r:%s,%r:%s",
                  T_ASSUME, T_CS, T_FLAT, T_DS, T_FLAT, T_SS, T_FLAT, T_ES, T_FLAT, T_FS, pFSassume, T_GS, pGSassume );
        break;
    case MOD_TINY:
    case MOD_SMALL:
    case MOD_COMPACT:
    case MOD_MEDIUM:
    case MOD_LARGE:
    case MOD_HUGE:
        /* v2.03: no DGROUP for COFF/ELF */
        if( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
           || Options.output_format == OFORMAT_ELF
#endif
          )
            break;

        if ( ModuleInfo.model == MOD_TINY )
            pCS = szDgroup;
        else
            pCS = GetCodeSegName();

        if ( ModuleInfo.distance != STACK_FAR )
            pFmt = "%r %r:%s,%r:%s,%r:%s";
        else
            pFmt = "%r %r:%s,%r:%s";
        AddLineQueueX( pFmt, T_ASSUME, T_CS, pCS, T_DS, szDgroup, T_SS, szDgroup );
        break;
    }
}

/* used by INVOKE directive */

struct asm_sym * GetStdAssume( int reg )
/**************************************/
{
    if ( StdAssumeTable[reg].symbol )
        if ( StdAssumeTable[reg].symbol->mem_type == MT_TYPE )
            return( StdAssumeTable[reg].symbol->type );
        else
            return( StdAssumeTable[reg].symbol->target_type );
    return ( NULL );
}

/* v2.05: new, used by
 * expression evaluator if a register is used for indirect addressing
 */

struct asm_sym * GetStdAssumeEx( int reg )
/****************************************/
{
    return( StdAssumeTable[reg].symbol );
}

ret_code AssumeDirective( int i )
/*******************************/
/* Handles ASSUME
 * syntax is :
 * - ASSUME
 * - ASSUME NOTHING
 * - ASSUME segregister : seglocation [, segregister : seglocation ]
 * - ASSUME dataregister : qualified type [, dataregister : qualified type ]
 * - ASSUME register : ERROR | NOTHING | FLAT
 */
{
    int             reg;
    int             j;
    int             size;
    uint_32         flags;
    struct assume_info *info;
    struct asm_sym  *sym;
    bool            segtable;
    struct qualified_type ti;

    DebugMsg1(( "AssumeDirective enter, pass=%u\n", Parse_Pass+1 ));

    for( i++; i < Token_Count; i++ ) {

        if( ( AsmBuffer[i]->token == T_ID )
            && (0 == _stricmp( AsmBuffer[i]->string_ptr, szNothing )) ) {
            AssumeInit();
            i++;
            break;
        }

        /*---- get the info ptr for the register ----*/

        info = NULL;
        if ( AsmBuffer[i]->token == T_REG ) {
            reg = AsmBuffer[i]->value;
            j = GetRegNo( reg );
            flags = GetValueSp( reg );
            if ( flags & OP_SR ) {
                info = &SegAssumeTable[j];
                segtable = TRUE;
            } else if ( flags & OP_R ) {
                info = &StdAssumeTable[j];
                segtable = FALSE;
            }
        }
        if ( info == NULL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }

        if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < GetCpuSp( reg ) ) {
            AsmError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
            return( ERROR );
        }

        i++; /* go past register */

        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        if( AsmBuffer[i]->token == T_FINAL ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }

        /* check for ERROR and NOTHING */

        if( 0 == _stricmp( AsmBuffer[i]->string_ptr, szError )) {
            if ( segtable ) {
                info->flat = FALSE;
                info->error = TRUE;
            } else
                info->error |= (( reg >= T_AH && reg <= T_BH ) ? RH_ERROR : ( flags & OP_R ));
            info->symbol = NULL;
            i++;
        } else if( 0 == _stricmp( AsmBuffer[i]->string_ptr, szNothing )) {
            if ( segtable ) {
                info->flat = FALSE;
                info->error = FALSE;
            } else
                info->error &= ~(( reg >= T_AH && reg <= T_BH ) ? RH_ERROR : ( flags & OP_R ));
            info->symbol = NULL;
            i++;
        } else if ( segtable == FALSE ) {

            /* v2.05: changed to use new GetQualifiedType() function */
            ti.size = 0;
            ti.is_ptr = 0;
            ti.is_far = FALSE;
            ti.mem_type = MT_EMPTY;
            ti.ptr_memtype = MT_EMPTY;
            ti.symtype = NULL;
            ti.Ofssize = ModuleInfo.Ofssize;
            if ( GetQualifiedType( &i, &ti ) == ERROR )
                return( ERROR );

            /* v2.04: check size of argument! */
            size = OperandSize( flags, NULL );
            if ( ( ti.is_ptr == 0 && size != ti.size ) ||
                ( ti.is_ptr > 0 && size < CurrWordSize ) ) {
                AsmError( TYPE_IS_WRONG_SIZE_FOR_REGISTER );
                return( ERROR );
            }
            info->error &= ~(( reg >= T_AH && reg <= T_BH ) ? RH_ERROR : ( flags & OP_R ));
            if ( stdsym[j] == NULL ) {
                stdsym[j] = CreateTypeSymbol( NULL, "", FALSE );
                ((dir_node *)stdsym[j])->e.structinfo->typekind = TYPE_TYPEDEF;
            }

            stdsym[j]->total_size = ti.size;
            stdsym[j]->mem_type   = ti.mem_type;
            stdsym[j]->is_ptr     = ti.is_ptr;
            stdsym[j]->isfar      = ti.is_far;
            stdsym[j]->Ofssize    = ti.Ofssize;
            stdsym[j]->ptr_memtype = ti.ptr_memtype; /* added v2.05 rc13 */
            if ( ti.mem_type == MT_TYPE )
                stdsym[j]->type = ti.symtype;
            else
                stdsym[j]->target_type = ti.symtype;

            info->symbol = stdsym[j];

        } else { /* segment register */
            if( AsmBuffer[i]->token == T_UNARY_OPERATOR &&
               AsmBuffer[i]->value == T_SEG ) {
                i++;
            }
            /* v2.04: check type of argument. Also allow argument
             * to be another segment register!
             */
            if ( AsmBuffer[i]->token == T_ID ) {
                sym = SymLookup( AsmBuffer[i]->string_ptr );
                if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
                    if ( Parse_Pass != PASS_1 ) {
                        AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
                    }
                    /* ensure that directive is rerun in pass 2
                     * so an error msg can be emitted.
                     */
                    FStoreLine();
                } else if ( sym->state != SYM_SEG &&
                           sym->state != SYM_GRP ) {
                    AsmError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
                    return( ERROR );
                }
                info->flat = FALSE;
            } else if ( AsmBuffer[i]->token == T_REG &&
                       ( GetValueSp( AsmBuffer[i]->value ) & OP_SR ) ) {
                sym = SegAssumeTable[ GetRegNo( AsmBuffer[i]->value ) ].symbol;
                info->flat = SegAssumeTable[ GetRegNo( AsmBuffer[i]->value ) ].flat;
            } else if( AsmBuffer[i]->token == T_RES_ID &&
                      AsmBuffer[i]->value == T_FLAT ) {
                if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) {
                    AsmError( INSTRUCTION_OR_REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                    return( ERROR );
                }
                DefineFlatGroup();
                info->flat = TRUE;
                sym = NULL;
            } else {
                AsmError( SEGMENT_GROUP_OR_SEGREG_EXPECTED );
                return( ERROR );
            }
            info->error = FALSE;
            info->symbol = sym;
            i++;
        }

        /* comma expected */
        if( i < Token_Count && AsmBuffer[i]->token != T_COMMA )
            break;
    }
    if ( i < Token_Count ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->tokpos );
        return( ERROR );
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

    grp = GetGroup( sym );

    /* first check the default segment register */

    if( def != ASSUME_NOTHING ) {
        if( SegAssumeTable[def].symbol == sym )
            return( def );
        if( search_grps && grp ) {
            if( SegAssumeTable[def].flat && grp == &ModuleInfo.flat_grp->sym )
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
            if( SegAssumeTable[searchtab[def]].flat && grp == &ModuleInfo.flat_grp->sym )
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
        return( (asm_sym *)ModuleInfo.flat_grp );
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
        *passume = (asm_sym *)ModuleInfo.flat_grp;
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

