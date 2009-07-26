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

extern int_8           Frame;          // Frame of current fixup
extern uint_16         Frame_Datum;    // Frame datum of current fixup
extern char *          oformat_strings[];

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
    if ( CurrSeg ) {
        fixup->fixup_loc = GetCurrOffset();
        fixup->def_seg = CurrSeg;
    } else {
        fixup->fixup_loc = 0;
        fixup->def_seg = NULL;
    }
    fixup->frame = Frame;                   // this is just a guess
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

static void FixupError( struct asmfixup *fixup )
{
    if ( fixup->sym )
        AsmErr( UNSUPPORTED_FIXUP_TYPE,
               oformat_strings[Options.output_format],
               fixup->sym->name );
    else
        AsmErr( UNSUPPORTED_FIXUP_TYPE,
               oformat_strings[Options.output_format],
               "<NULL>" );
}

struct fixup *CreateOmfFixupRec( struct asmfixup *fixup )
/***************************************/
/* Create a fixup record for OMF, translates "asmfixup" to "fixup";
   Note that if Modend is TRUE, it means the fixup is the starting address
   for the module.
*/
{
    //struct asmfixup     *fixup;         // fixup structure from JWasm
    struct fixup        *fixomf;          // fixup structure for OMF
    struct asm_sym      *sym;

    //fixup = CodeInfo->InsFixup[index];
    sym = fixup->sym; /* may be NULL! */
    fixomf = OmfFixNew();
    fixomf->next = NULL;
    fixomf->self_relative = FALSE;

    if( !Modend ) {
        fixomf->lr.is_secondary = TRUE;
        fixomf->lr.target_offset = 0;
        switch( fixup->type ) {
        case FIX_RELOFF8:
            fixomf->self_relative = TRUE;
        case FIX_LOBYTE:
            fixomf->loc_method = FIX_LO_BYTE;
            break;
        case FIX_HIBYTE:
            fixomf->loc_method = FIX_HI_BYTE;
            break;
        case FIX_RELOFF16:
            fixomf->self_relative = TRUE;
        case FIX_OFF16:
            fixomf->loc_method = FIX_OFFSET;
            break;
        case FIX_RELOFF32:
            fixomf->self_relative = TRUE;
        case FIX_OFF32:
            fixomf->loc_method = FIX_OFFSET386;
            break;
        case FIX_SEG:
            fixomf->loc_method = FIX_BASE;
            break;
        case FIX_PTR16:
            fixomf->loc_method = FIX_POINTER;
            break;
        case FIX_PTR32:
            fixomf->loc_method = FIX_POINTER386;
            break;
        default:
            FixupError( fixup );
            return( NULL );
        }
    } else {
        fixomf->lr.is_secondary = FALSE;
        fixomf->lr.target_offset = fixup->offset;
    }

#ifdef DEBUG_OUT
    if (sym)
        DebugMsg(("CreateOmfFixupRec(%X): sym=%s, state=%u, fixup->type=%u\n", fixup, sym->name, sym->state, fixup->type ));
    else
        DebugMsg(("CreateOmfFixupRec(%X): sym=NULL, fixup->type=%u\n", fixup, fixup->type ));
#endif

    /* loader_resolved is FALSE for OFFSET, TRUE for LROFFSET */
    fixomf->loader_resolved = fixup->loader_resolved;

    /* set the fixup's location in current LEDATA */
    /* CurrSeg->curr_loc - CurrSeg->start_loc */

    fixomf->loc_offset = GetCurrOffset() - GetCurrSegStart();

    /*------------------------------------*/
    /* Determine the Target and the Frame */
    /*------------------------------------*/

    if( sym == NULL ) {

        if ( fixup->frame == EMPTY ) /* v1.96: nothing to do without a frame */
            return( NULL );
        fixomf->lr.target = fixup->frame;
        fixomf->lr.target_datum = fixup->frame_datum;
        fixomf->lr.frame = FRAME_TARG;

    } else if( sym->state == SYM_UNDEFINED ) { /* shouldn't happen */
        DebugMsg(("CreateOmfFixupRec(%X): state of >%s< is SYM_UNDEFINED\n", fixup, sym->name));
        AsmErr( SYMBOL_NOT_DEFINED, sym->name );
        return( NULL );
    } else if( sym->state == SYM_GRP ) {

        DebugMsg(("CreateOmfFixupRec(%X): GROUP %s\n", fixup, sym->name));

        fixomf->lr.target = TARGET_GRP;
        fixomf->lr.target_datum = GetGrpIdx( sym );
        if( fixup->frame != EMPTY ) {
            fixomf->lr.frame = fixup->frame;
            fixomf->lr.frame_datum = fixup->frame_datum;
        } else {
            fixomf->lr.frame = FRAME_GRP;
            fixomf->lr.frame_datum = fixomf->lr.target_datum;
        }

    } else if( sym->state == SYM_SEG ) {

        DebugMsg(("CreateOmfFixupRec(%X): SEG %s\n", fixup, sym->name));

        fixomf->lr.target = TARGET_SEG;
        fixomf->lr.target_datum = GetSegIdx( sym );
        if( fixup->frame != EMPTY ) {
            fixomf->lr.frame = fixup->frame;
            fixomf->lr.frame_datum = fixup->frame_datum;
        } else {
            fixomf->lr.frame = FRAME_SEG;
            fixomf->lr.frame_datum = fixomf->lr.target_datum;
        }

    } else {

        /* symbol is a label */

        if( sym->state == SYM_EXTERNAL ) {
            DebugMsg(("CreateOmfFixupRec(%X): EXTERNAL %s\n", fixup, sym->name));
            if( Modend ) {
                fixomf->lr.target = TARGET_EXT & TARGET_WITH_DISPL;
            } else {
                fixomf->lr.target = TARGET_EXT;
            }

            fixomf->lr.target_datum = sym->idx;

            if( fixup->frame == FRAME_GRP && fixup->frame_datum == 0 ) {
                /* set the frame to the frame of the corresponding segment */
                fixup->frame_datum = GetGrpIdx( sym );
            }
        } else if ( sym->state == SYM_PROC && sym->isproc == FALSE ) {
            /* these are PROTOs without a segment reference */
            DebugMsg(("CreateOmfFixupRec(%X): PROTO %s\n", fixup, sym->name));
            fixomf->lr.target = TARGET_EXT;
            fixomf->lr.target_datum = sym->idx;
        } else {
            //asm_sym *grpsym;
            /* it's a SYM_INTERNAL/SYM_PROC */
            DebugMsg(("CreateOmfFixupRec(%X): fixup->frame, datum=%u.%u sym->name=%s state=%X segm=%X\n",
                      fixup, fixup->frame, fixup->frame_datum, sym->name, sym->state, sym->segment ));
            if ( sym->segment == NULL ) {
                AsmErr( SEGMENT_MISSING_FOR_FIXUP, sym->name );
                return (NULL);
            }

            if( Modend )
                fixomf->lr.target = TARGET_SEG & TARGET_WITH_DISPL;
            else
                fixomf->lr.target = TARGET_SEG;
#if 0
            if ( grpsym = GetGrp( sym ) ) {
                fixup->frame = FRAME_GRP;
                fixup->frame_datum = GetGrpIdx( grpsym );
            }
#endif
            fixomf->lr.target_datum = GetSegIdx( sym->segment );
        }

        if( fixup->frame != EMPTY ) {
            fixomf->lr.frame = (uint_8)fixup->frame;
        } else {
            fixomf->lr.frame = FRAME_TARG;
        }
        fixomf->lr.frame_datum = fixup->frame_datum;

        if( Modend ) {
            DebugMsg(( "CreateOmfFixupRec: ModEnd fixup=%X/%X/%X/%X\n", fixomf->lr.target, fixomf->lr.frame, fixomf->lr.frame_datum, fixomf->loc_offset ));
            return( fixomf );
        }
    }

    /*--------------------*/
    /* Optimize the fixup */
    /*--------------------*/

    if( fixomf->lr.frame == ( fixomf->lr.target - TARGET_SEG ) ) {
        fixomf->lr.frame = FRAME_TARG;
    }

    return( fixomf );
}

/*
 * Store asmfixup information of CodeInfo->InsFixup[index] in current
 * segment's fixup linked list.
 * please note: forward references - which must be written in PASS 1 -
 * aren't handled here!
 */
ret_code store_fixup( struct code_info *CodeInfo, int index )
/**************************/
{
    struct asmfixup     *fixup;

    fixup = CodeInfo->InsFixup[index];

//    CodeInfo->data[index] = CodeInfo->data[index] - fixup->sym->offset;
    fixup->offset = CodeInfo->data[index];

#ifdef DEBUG_OUT
    if (fixup->sym)
        DebugMsg(("store_fixup(%u): type=%u, loc=%s.%X, target=%s(%X+%X)\n",
                  index, fixup->type, CurrSeg->sym.name, fixup->fixup_loc, fixup->sym->name, fixup->sym->offset, fixup->offset ));
    else
        DebugMsg(("store_fixup(%u): type=%u, loc=%s.%X, target=%X\n",
                  index, fixup->type, CurrSeg->sym.name, fixup->fixup_loc, fixup->offset));
#endif

    if (Options.output_format == OFORMAT_OMF) {
        struct fixup *fixomf;

        /* for OMF, the target's offset is stored at the fixup's location.
        */
        if( fixup->type != FIX_SEG && fixup->sym ) {
            CodeInfo->data[index] += fixup->sym->offset;
        }

        /* convert asmfixup to OMF fixup.
         * This probably should be done later, when the record is written;
         * then the segment's fixup list will always contain asmfixup entries.
         */
        fixomf = CreateOmfFixupRec( fixup );
        if( fixomf == NULL )
            return( ERROR );

        if( CurrSeg->e.seginfo->FixupListHead == NULL ) {
            CurrSeg->e.seginfo->FixupListTail = CurrSeg->e.seginfo->FixupListHead = fixomf;
        } else {
            CurrSeg->e.seginfo->FixupListTail->next = fixomf;
            CurrSeg->e.seginfo->FixupListTail = fixomf;
        }
    } else {
        fixup->nextrlc = NULL;

        /* filter fixup types which aren't supported by the format */
        switch( fixup->type ) {
        case FIX_RELOFF8: /* shouldn't happen */
        case FIX_LOBYTE:
        case FIX_HIBYTE:
        case FIX_SEG:
        case FIX_PTR16:
        case FIX_PTR32:
            if (Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
                || Options.output_format == OFORMAT_ELF
#endif
               ) {
                FixupError( fixup );
                return( ERROR );
            }
            break;
        }
#if ELF_SUPPORT
        if ( Options.output_format == OFORMAT_ELF ) {
            if (fixup->type == FIX_RELOFF32)
                CodeInfo->data[index] = -4;
        }
#endif
        /* special handling for assembly time variables needed */
        if ( fixup->sym && fixup->sym->variable ) {
            /* add symbol's offset to the fixup location and fixup's offset */
            CodeInfo->data[index] += fixup->sym->offset;
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

