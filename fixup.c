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
* Description:  handles fixups
*
****************************************************************************/

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "fixup.h"
#include "segment.h"

#define GNURELOCS 1

extern int_8           Frame;          /* Frame of current fixup */
extern uint_16         Frame_Datum;    /* Frame datum of current fixup */
extern struct format_options formatoptions[];

const char szNull[] = {"<NULL>"};

struct fixup *AddFixup( struct asm_sym *sym, enum fixup_types type, enum fixup_options option )
/*********************************************************************************************/
/*
 * called when an instruction operand or a data item is relocatable:
 * - Parser.idata_fixup()
 * - Parser.memory_operand()
 * - branch.process_branch()
 * - data.data_item()
 * - dbgcv()
 * - fpfixup()
 * creates a new fixup item and initializes it using symbol <sym>.
 * put the correct target offset into the link list when forward reference of
 * relocatable is resolved;
 * Global vars Frame and Frame_Datum must be set.
 */
{
#ifdef DEBUG_OUT
    static uint_32 cnt = 0;
#endif
    struct fixup     *fixup;

    fixup = AsmAlloc( sizeof( struct fixup ) );

    /* add the fixup to the symbol's linked list (used for backpatch)
     * this is done for pass 1 only.
     */
    if ( Parse_Pass == PASS_1 ) {
#ifdef DEBUG_OUT
        if ( Options.nobackpatch == FALSE )
#endif
        if ( sym ) { /* changed v1.96 */
            fixup->nextbp = sym->fixup;
            sym->fixup = fixup;
        }
        /* v2.03: in pass one, create a linked list of
         * fixup locations for a segment. This is to improve
         * backpatching, because it allows to adjust fixup locations
         * after a distance has changed from short to near
         */
#ifdef DEBUG_OUT
        if ( Options.nobackpatch == FALSE )
#endif
        if ( CurrSeg ) {
            fixup->nextrlc = CurrSeg->e.seginfo->FixupListHead;
            CurrSeg->e.seginfo->FixupListHead = fixup;
        }
    }
    fixup->offset = 0;
    /* initialize <location> value with current offset.
     * Used for backpatching only. It's not the correct location,
     * but sufficiently exact for this purpose.
     */
    fixup->location = GetCurrOffset();
    fixup->type = type;
    fixup->option = option;
    fixup->flags = 0;
    fixup->frame = Frame;               /* this is just a guess */
    fixup->frame_datum = Frame_Datum;
    fixup->def_seg = CurrSeg;           /* may be NULL (END directive) */
    fixup->sym = sym;

    DebugMsg1(("AddFixup(sym=%s type=%X, opt=%X) cnt=%" FX32 ": loc=%" FX32 "h\n", sym ? sym->name : "NULL", type, option, ++cnt, fixup->location ));
    //CodeInfo->InsFixup[Opnd_Count] = fixup; /* changed in v1.96 */
    return( fixup );
}

/* remove a fixup from the segment's fixup queue */

void FreeFixup( struct fixup *fixup )
/***********************************/
{
    dir_node *dir;
    struct fixup *fixup2;

    if ( Parse_Pass == PASS_1 ) {
        dir = fixup->def_seg;
        if ( dir ) {
            if ( fixup == dir->e.seginfo->FixupListHead ) {
                dir->e.seginfo->FixupListHead = fixup->nextrlc;
            } else {
                for ( fixup2 = dir->e.seginfo->FixupListHead; fixup2; fixup2 = fixup2->nextrlc ) {
                    if ( fixup2->nextrlc == fixup ) {
                        fixup2->nextrlc = fixup->nextrlc;
                        break;
                    }
                }
            }
        }
    }
    AsmFree( fixup );
}

/*
 * Store fixup information in current segment's fixup linked list.
 * please note: forward references for backpatching are written in PASS 1 -
 * they no longer exist when store_fixup() is called.
 */

ret_code store_fixup( struct fixup *fixup, int_32 * pdata )
/*********************************************************/
{
    //struct fixup     *fixup;

    //fixup = CodeInfo->InsFixup[index];

    //CodeInfo->data[index] = CodeInfo->data[index] - fixup->sym->offset;
    //fixup->offset = CodeInfo->data[index];
    fixup->offset = *pdata;

#ifdef DEBUG_OUT
    if ( fixup->sym )
        DebugMsg1(("store_fixup: type=%u, loc=%s.%" FX32 ", target=%s(%" FX32 "+% "FX32 ")\n",
                  fixup->type, CurrSeg->sym.name, fixup->location, fixup->sym->name, fixup->sym->offset, fixup->offset ));
    else
        DebugMsg1(("store_fixup: type=%u, loc=%s.%" FX32 ", target=%" FX32 "\n",
                  fixup->type, CurrSeg->sym.name, fixup->location, fixup->offset));
#endif

    fixup->nextrlc = NULL;
    if ( ( 1 << fixup->type ) & formatoptions[Options.output_format].invalid_fixup_type ) {
        AsmErr( UNSUPPORTED_FIXUP_TYPE,
               formatoptions[Options.output_format].formatname,
               fixup->sym ? fixup->sym->name : szNull );
        return( ERROR );
    }
    if ( Options.output_format == OFORMAT_OMF ) {

        /* for OMF, the target's offset is stored at the fixup's location. */
        if( fixup->type != FIX_SEG && fixup->sym ) {
            *pdata += fixup->sym->offset;
        }

    } else {

#if ELF_SUPPORT
        if ( Options.output_format == OFORMAT_ELF ) {
            if ( fixup->type == FIX_RELOFF32 )
                *pdata = -4;
#if GNURELOCS /* v2.04: added */
            else if ( fixup->type == FIX_RELOFF16 )
                *pdata = -2;
            else if ( fixup->type == FIX_RELOFF8 )
                *pdata = -1;
#endif
        }
#endif
#if DJGPP_SUPPORT
        /* Djgpp's COFF variant needs special handling for
         * - at least - relative and direct 32-bit offsets.
         */
        if ( fixup->sym && Options.header_format == HFORMAT_DJGPP ) {
            if ( fixup->type == FIX_RELOFF32 ) { /* probably also for 16-bit */
                *pdata -= ( fixup->location + 4 );
            } else if ( fixup->type == FIX_OFF32 ) {
                *pdata += fixup->sym->offset;
                fixup->offset += fixup->sym->offset; /* ok? */
                fixup->segment = fixup->sym->segment;/* ok? */
            }
        } else
#endif
        /* special handling for assembly time variables needed */
        if ( fixup->sym && fixup->sym->variable ) {
            /* add symbol's offset to the fixup location and fixup's offset */
            *pdata += fixup->sym->offset;
            fixup->offset         += fixup->sym->offset;
            /* and save symbol's segment in fixup */
            fixup->segment = fixup->sym->segment;
        }
#if 0   /* fixup without symbol: this is to be resolved internally! */
        else if ( fixup->sym == NULL && fixup->frame == EMPTY ) {
            DebugMsg(("store_fixup: fixup skipped, symbol=0, no frame\n" ));
            return( NOT_ERROR );
        }
#endif
    }
    if( CurrSeg->e.seginfo->FixupListHead == NULL ) {
        CurrSeg->e.seginfo->FixupListTail = CurrSeg->e.seginfo->FixupListHead = fixup;
    } else {
        CurrSeg->e.seginfo->FixupListTail->nextrlc = fixup;
        CurrSeg->e.seginfo->FixupListTail = fixup;
    }
    return( NOT_ERROR );
}

