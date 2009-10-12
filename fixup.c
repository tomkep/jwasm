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
#include "parser.h"
#include "memalloc.h"
#include "fixup.h"
#include "directiv.h"
#include "segment.h"
#include "omf.h"

extern int_8           Frame;          // Frame of current fixup
extern uint_16         Frame_Datum;    // Frame datum of current fixup
extern struct format_options formatoptions[];

const char szNull[] = {"<NULL>"};

struct asmfixup *AddFixup( struct asm_sym *sym, enum fixup_types type, enum fixup_options option )
/************************************************************************************************/
/*
 * called when an instruction operand or a data item is relocatable.
 * creates a new asmfixup item and initializes it using symbol <sym>.
 * put the correct target offset into the link list when forward reference of
 * relocatable is resolved;
 * Global vars Frame and Frame_Datum must be set.
 */
{
    struct asmfixup     *fixup;

    fixup = AsmAlloc( sizeof( struct asmfixup ) );
//    fixup->external = 0;
    fixup->sym = sym;
    fixup->offset = 0;
    /* initialize fixup_loc value with current offset.
     * Used for backpatching only. It's not the correct location,
     * but sufficiently exact for this purpose.
     */
    fixup->fixup_loc = GetCurrOffset();
    fixup->def_seg = CurrSeg;           /* may be NULL */
    fixup->frame = Frame;               /* this is just a guess */
    fixup->frame_datum = Frame_Datum;

    /* add the fixup to the symbol's linked list (used for backpatch)
     * this is done for pass 1 only.
     */
    //if ( sym ) {
    if ( sym && Parse_Pass == PASS_1 ) { // changed v1.96
        fixup->nextbp = sym->fixup;
        sym->fixup = fixup;
    }

    fixup->type = type;
    fixup->loader_resolved = FALSE;
    fixup->option = option;
    //CodeInfo->InsFixup[Opnd_Count] = fixup; // changed in v1.96
    return( fixup );
}

/*
 * Store asmfixup information in current segment's fixup linked list.
 * please note: forward references - which must be written in PASS 1 -
 * aren't handled here!
 */
ret_code store_fixup( struct asmfixup *fixup, int_32 * pdata )
/************************************************************/
{
    //struct asmfixup     *fixup;

    //fixup = CodeInfo->InsFixup[index];

    //CodeInfo->data[index] = CodeInfo->data[index] - fixup->sym->offset;
    //fixup->offset = CodeInfo->data[index];
    fixup->offset = *pdata;

#ifdef DEBUG_OUT
    if (fixup->sym)
        DebugMsg(("store_fixup: type=%u, loc=%s.%X, target=%s(%X+%X)\n",
                  fixup->type, CurrSeg->sym.name, fixup->fixup_loc, fixup->sym->name, fixup->sym->offset, fixup->offset ));
    else
        DebugMsg(("store_fixup: type=%u, loc=%s.%X, target=%X\n",
                  fixup->type, CurrSeg->sym.name, fixup->fixup_loc, fixup->offset));
#endif

    if (Options.output_format == OFORMAT_OMF) {
        struct fixup *fixomf;

        /* for OMF, the target's offset is stored at the fixup's location.
        */
        if( fixup->type != FIX_SEG && fixup->sym ) {
            *pdata += fixup->sym->offset;
        }

        /* convert asmfixup to OMF fixup.
         * This probably should be done later, when the record is written;
         * then the segment's fixup list will always contain asmfixup entries.
         */
        fixomf = omf_create_fixup( fixup );
        if( fixomf == NULL )
            return( ERROR );

        if( CurrSeg->e.seginfo->FixupListHead == NULL ) {
            CurrSeg->e.seginfo->FixupListTail = CurrSeg->e.seginfo->FixupListHead = fixomf;
        } else {
            CurrSeg->e.seginfo->FixupListTail->next = fixomf;
            CurrSeg->e.seginfo->FixupListTail = fixomf;
        }
    } else {
        uint disallowed;
        fixup->nextrlc = NULL;

        /* filter fixup types which aren't supported by the format */
        switch ( Options.output_format ) {
#if COFF_SUPPORT
        case OFORMAT_COFF: disallowed = COFF_DISALLOWED; break;
#endif
#if ELF_SUPPORT
        case OFORMAT_ELF:  disallowed = ELF_DISALLOWED;  break;
#endif
        case OFORMAT_BIN:  disallowed = BIN_DISALLOWED;  break;
        };
        if ( ( 1 << fixup->type ) & disallowed ) {
            AsmErr( UNSUPPORTED_FIXUP_TYPE,
                   formatoptions[Options.output_format].formatname,
                   fixup->sym ? fixup->sym->name : szNull );
            return( ERROR );
        }
#if ELF_SUPPORT
        if ( Options.output_format == OFORMAT_ELF ) {
            if (fixup->type == FIX_RELOFF32)
                *pdata = -4;
        }
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

        /* For COFF, just the difference to the target's
         symbol offset is stored an the fixup location!
        */

        /* for COFF/ELF/BIN, store the asmfixup records directly.
         * (this should be done for OMF as well!)
         */

        if( CurrSeg->e.seginfo->FixupListHeadGen == NULL ) {
            CurrSeg->e.seginfo->FixupListTailGen = CurrSeg->e.seginfo->FixupListHeadGen = fixup;
        } else {
            CurrSeg->e.seginfo->FixupListTailGen->nextrlc = fixup;
            CurrSeg->e.seginfo->FixupListTailGen = fixup;
        }
    }
    return( NOT_ERROR );
}

