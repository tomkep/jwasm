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
* Description:  handles fixups and short forward jump optimization.
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "memalloc.h"
#include "fixup.h"
#include "directiv.h"
#include "segment.h"

// short jump label optimization
// if this is 0, there is just the simple "fixup backpatch",
// which cannot adjust any label offsets between the forward reference
// and the newly defined label, resulting in more passes to be needed.

#define LABELOPT 1

extern int_8           Frame;          // Frame of current fixup
extern uint_16         Frame_Datum;    // Frame datum of current fixup

struct asmfixup *AddFixup( struct asm_sym *sym, enum fixup_types type, enum fixup_options option )
/************************************************************************************************************/
/*
  creates a new asmfixup item and initializes it using symbol <sym>.
  put the correct target offset into the link list when forward reference of
  relocatable is resolved;
  Global vars Frame, Frame_Datum and Opnd_Count must be set.
*/
{
    struct asmfixup     *fixup;

    /* no fixups needed for string instructions and XLAT */
    if ( CodeInfo->token == T_XLAT ||
         AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REP ||
         AsmOpTable[AsmResWord[CodeInfo->token].position].allowed_prefix == AP_REPxx )
        return( NULL );

    fixup = AsmAlloc( sizeof( struct asmfixup ) );
//    fixup->external = 0;
    fixup->sym = sym;
    fixup->offset = 0;
    if ( CurrSeg ) {
        fixup->fixup_loc = GetCurrOffset();
        fixup->def_seg = CurrSeg->seg;
    } else {
        fixup->fixup_loc = 0;
        fixup->def_seg = NULL;
    }
    fixup->frame = Frame;                   // this is just a guess
    fixup->frame_datum = Frame_Datum;
    if ( sym ) {
        fixup->next1 = sym->fixup;
        sym->fixup = fixup;
    }
    fixup->type = type;
    fixup->loader_resolved = FALSE;
    fixup->option = option;
    CodeInfo->InsFixup[Opnd_Count] = fixup;
    return( fixup );
}

#define SkipFixup() \
    fixup->next1 = sym->fixup; \
    sym->fixup = fixup

static ret_code DoPatch( struct asm_sym *sym, struct asmfixup *fixup )
/***************************************************************/
{
    long                disp;
    long                max_disp;
    unsigned            size;
    asm_sym             *sym2;
    dir_node            *seg;

    // all relative fixups should occure only at first pass and they signal forward references
    // they must be removed after patching or skiped ( next processed as normal fixup )

    DebugMsg(("DoPatch(%s): sym=%s fixup->ofs,loc,opt,def_seg=%X,%X,%X,%X\n",
              sym->name, fixup->sym ? fixup->sym->name : "",
              fixup->offset, fixup->fixup_loc, fixup->option, fixup->def_seg ));
    seg = GetSeg( sym );
    if( seg == NULL || fixup->def_seg != seg ) {
        /* can't backpatch if fixup location is in diff seg than symbol */
        SkipFixup();
        return( NOT_ERROR );
    } else if( Parse_Pass != PASS_1 ) {
    } else if( sym->mem_type == MT_FAR && fixup->option == OPTJ_CALL ) {
        // convert far call to push cs + near call, only at first pass
        DebugMsg(("DoPatch: Phase error! caused by far call optimization\n"));
        PhaseError = TRUE;
        sym->offset++;  /* a PUSH CS will be added */
        OutputByte( 0 );
        AsmFree( fixup );
        return( NOT_ERROR );
    } else if( sym->mem_type == MT_NEAR ) {
        // near forward reference, only at first pass
        switch( fixup->type ) {
        case FIX_RELOFF32:
        case FIX_RELOFF16:
            AsmFree( fixup );
            return( NOT_ERROR );
        }
    }
    size = 0;
    switch( fixup->type ) {
    case FIX_RELOFF32:
        size = 2; /* will be 4 finally */
        /* fall through */
    case FIX_RELOFF16:
        size++; /* will be 2 finally */
        /* fall through */
    case FIX_RELOFF8:
        size++;
        // calculate the displacement
        // disp = fixup->offset + GetCurrOffset() - fixup->fixup_loc - size;
        disp = fixup->offset + fixup->sym->offset - fixup->fixup_loc - size - 1;
        max_disp = (1UL << ((size * 8)-1)) - 1;
        if( disp > max_disp || disp < (-max_disp-1) ) {
            DebugMsg(("DoPatch: Phase error, disp=%X, fixup=%s(%X), loc=%X!\n", disp, fixup->sym->name, fixup->sym->offset, fixup->fixup_loc ));
            PhaseError = TRUE;
            /* ok, the standard case is: there's a forward jump which
             * was assumed to be SHORT, but it must be NEAR instead.
             */
            switch( size ) {
            case 1:
                size = 0;
                switch( fixup->option ) {
#if 0 /* don't display the error at the destination line! */
                case OPTJ_EXPLICIT:
                    sym->fixup = NULL;
                    DebugMsg(("DoPatch: jump out of range, disp=%d\n", disp ));
                    AsmErr( JUMP_OUT_OF_RANGE, disp - max_disp );
                    return( ERROR );
#endif
                case OPTJ_EXTEND: /* Jxx for 8086 */
                    size++;       /* will be 3/5 finally */
                    /* fall through */
                case OPTJ_JXX: /* Jxx for 386 */
                    size++;
                    /* fall through */
                default: /* normal JMP */
                    if( CodeInfo->use32 )
                        size += 2; /* NEAR32 instead of NEAR16 */
                    size++;
                    sym->offset += size;
#if LABELOPT
                    /* scan the label list of the segment and adjust all
                     * labels which are > fixup target and < current sym.
                     * ( PROCs are NOT contained in this list because they
                     * use the <next>-field of dir_node already!)
                     */
                    DebugMsg(("DoPatch: sym %s, offset changed %X -> %X\n", sym->name, sym->offset - size, sym->offset));
                    for ( sym2 = seg->e.seginfo->labels; sym2; sym2 = (asm_sym *)((dir_node *)sym2)->next ) {
                        if ( sym2 == sym )
                            continue;
                        if ( sym2->offset < fixup->fixup_loc )
                            break;
                        sym2->offset += size;
                        DebugMsg(("sym %s, offset changed %X -> %X\n", sym2->name, sym2->offset - size, sym2->offset));
                    }
#endif
                    /*  it doesn't matter what's actually "written" */
                    for (;size;size--)
                        OutputByte( 0xCC );
                    break;
                }
                break;
            case 2:
            case 4:
                DebugMsg(("DoPatch: jump out of range, disp=%d\n", disp ));
                AsmWarn( 4, JUMP_OUT_OF_RANGE, disp - max_disp );
                break;
            }
        }
        AsmFree( fixup );
        break;
    default:
        SkipFixup();
        break;
    }
    return( NOT_ERROR );
}

ret_code BackPatch( struct asm_sym *sym )
/**********************************/
/*
- patching for forward reference labels in Jmp/Call instructions;
- called by LabelCreate(), ProcDef() and data_init(), that is, whenever
- a new label appears. The new label is the <sym> parameter.
- During the process, the label's offset might be changed!
*/
{
    struct asmfixup     *fixup;
    struct asmfixup     *next;

    DebugMsg(("BackPatch(%s) enter, offset=%X\n", sym->name, sym->offset ));
    fixup = sym->fixup;
    sym->fixup = NULL;
    for( ; fixup != NULL; fixup = next ) {
        next = fixup->next1;
        if( DoPatch( sym, fixup ) == ERROR ) {
            return( ERROR );
        }
    }
    DebugMsg(("BackPatch(%s) exit, offset=%X\n", sym->name, sym->offset ));
    return( NOT_ERROR );
}

struct fixup *CreateOmfFixupRec( int index )
/***************************************/
/* Create a fixup record for OMF, translates "asmfixup" to "fixup";
   Note that if Modend is TRUE, it means the fixup is the starting address
   for the module.
*/
{
    struct asmfixup     *fixup;         // fixup structure from JWasm
    struct fixup        *fixnode;       // fixup structure for OMF
    struct asm_sym      *sym;

    fixup = CodeInfo->InsFixup[index];
    sym = fixup->sym; /* may be NULL! */
    fixnode = FixNew();
    fixnode->next = NULL;
    fixnode->self_relative = FALSE;

    if( !Modend ) {
        fixnode->lr.is_secondary = TRUE;
        fixnode->lr.target_offset = 0;
        switch( fixup->type ) {
        case FIX_RELOFF8:
            fixnode->self_relative = TRUE;
        case FIX_LOBYTE:
            fixnode->loc_method = FIX_LO_BYTE;
            break;
        case FIX_HIBYTE:
            fixnode->loc_method = FIX_HI_BYTE;
            break;
        case FIX_RELOFF16:
            fixnode->self_relative = TRUE;
        case FIX_OFF16:
            fixnode->loc_method = FIX_OFFSET;
            break;
        case FIX_RELOFF32:
            fixnode->self_relative = TRUE;
        case FIX_OFF32:
            fixnode->loc_method = FIX_OFFSET386;
            break;
        case FIX_SEG:
            fixnode->loc_method = FIX_BASE;
            break;
        case FIX_PTR16:
            fixnode->loc_method = FIX_POINTER;
            break;
        case FIX_PTR32:
            fixnode->loc_method = FIX_POINTER386;
            break;
        }
    } else {
        fixnode->lr.is_secondary = FALSE;
        fixnode->lr.target_offset = fixup->offset;
    }

#ifdef DEBUG_OUT
    if (sym)
        DebugMsg(("CreateOmfFixupRec: sym=%s, state=%u\n", sym->name, sym->state));
    else
        DebugMsg(("CreateOmfFixupRec: sym=NULL\n" ));
#endif

    /* loader_resolved is FALSE for OFFSET, TRUE for LROFFSET */
    fixnode->loader_resolved = fixup->loader_resolved;

    /* set the fixup's location in current LEDATA */
    /* CurrSeg->curr_loc - CurrSeg->start_loc */

    fixnode->loc_offset = GetCurrOffset() - GetCurrSegStart();

    /*------------------------------------*/
    /* Determine the Target and the Frame */
    /*------------------------------------*/

    if( sym == NULL ) {

        fixnode->lr.target = fixup->frame;
        fixnode->lr.target_datum = fixup->frame_datum;
        fixnode->lr.frame = FRAME_TARG;

    } else if( sym->state == SYM_UNDEFINED ) {
        DebugMsg(("CreateOmfFixupRec: state of >%s< is SYM_UNDEFINED\n", sym->name));
        AsmErr( SYMBOL_NOT_DEFINED, sym->name );
        return( NULL );
    } else if( sym->state == SYM_GRP ) {

        DebugMsg(("CreateOmfFixupRec: GROUP %s\n", sym->name));

        fixnode->lr.target = TARGET_GRP;
        fixnode->lr.target_datum = GetGrpIdx( sym );
        if( fixup->frame != EMPTY ) {
            fixnode->lr.frame = fixup->frame;
            fixnode->lr.frame_datum = fixup->frame_datum;
        } else {
            fixnode->lr.frame = FRAME_GRP;
            fixnode->lr.frame_datum = fixnode->lr.target_datum;
        }

    } else if( sym->state == SYM_SEG ) {

        DebugMsg(("CreateOmfFixupRec: SEG %s\n", sym->name));

        fixnode->lr.target = TARGET_SEG;
        fixnode->lr.target_datum = GetSegIdx( sym );
        if( fixup->frame != EMPTY ) {
            fixnode->lr.frame = fixup->frame;
            fixnode->lr.frame_datum = fixup->frame_datum;
        } else {
            fixnode->lr.frame = FRAME_SEG;
            fixnode->lr.frame_datum = fixnode->lr.target_datum;
        }

    } else {

        /* symbol is a label */

        if( sym->state == SYM_EXTERNAL ) {
            DebugMsg(("CreateOmfFixupRec: EXTERNAL %s\n", sym->name));
            if( Modend ) {
                fixnode->lr.target = TARGET_EXT & TARGET_WITH_DISPL;
            } else {
                fixnode->lr.target = TARGET_EXT;
            }

            fixnode->lr.target_datum = sym->idx;

            if( fixup->frame == FRAME_GRP && fixup->frame_datum == 0 ) {
                /* set the frame to the frame of the corresponding segment */
                fixup->frame_datum = GetGrpIdx( sym );
            }
        } else if ( sym->state == SYM_PROC && sym->isproc == FALSE ) {
            /* these are PROTOs without a segment reference */
            DebugMsg(("CreateOmfFixupRec: PROTO %s\n", sym->name));
            fixnode->lr.target = TARGET_EXT;
            fixnode->lr.target_datum = sym->idx;
        } else {
            //asm_sym *grpsym;
            /* it's a SYM_INTERNAL/SYM_PROC */
            DebugMsg(("CreateOmfFixupRec: fixup->frame,datum=%u.%u sym->name=%s state=%X segm=%X\n", fixup->frame, fixup->frame_datum, sym->name, sym->state, sym->segment ));
            if ( sym->segment == NULL ) {
                AsmErr( SEGMENT_MISSING_FOR_FIXUP, sym->name );
                return (NULL);
            }

            if( Modend )
                fixnode->lr.target = TARGET_SEG & TARGET_WITH_DISPL;
            else
                fixnode->lr.target = TARGET_SEG;
#if 0
            if ( grpsym = GetGrp( sym ) ) {
                fixup->frame = FRAME_GRP;
                fixup->frame_datum = GetGrpIdx( grpsym );
            }
#endif
            fixnode->lr.target_datum = GetSegIdx( sym->segment );
        }

        if( fixup->frame != EMPTY ) {
            fixnode->lr.frame = (uint_8)fixup->frame;
        } else {
            fixnode->lr.frame = FRAME_TARG;
        }
        fixnode->lr.frame_datum = fixup->frame_datum;

        if( Modend ) {
            DebugMsg(( "CreateOmfFixupRec: ModEnd fixup=%X/%X/%X/%X\n", fixnode->lr.target, fixnode->lr.frame, fixnode->lr.frame_datum, fixnode->loc_offset ));
            return( fixnode );
        }
    }

    /*--------------------*/
    /* Optimize the fixup */
    /*--------------------*/

    if( fixnode->lr.frame == ( fixnode->lr.target - TARGET_SEG ) ) {
        fixnode->lr.frame = FRAME_TARG;
    }

    return( fixnode );
}

void mark_fixupp( OPNDTYPE determinant, int index )
/*************************************************/
/*
  set field <type> of CodeInfo->InsFixup[index]
*/
{
    struct asmfixup     *fixup;

    fixup = CodeInfo->InsFixup[index];
    if( fixup != NULL ) {

        switch( determinant ) {
        case OP_I16:
        case OP_J32:
            switch( fixup->type ) {
            case FIX_OFF32:
                fixup->type = FIX_OFF16;
                break;
            case FIX_PTR32:
                fixup->type = FIX_PTR16;
                break;
            }
            break;
        case OP_I32:
        case OP_J48:
            switch( fixup->type ) {
            case FIX_OFF16:
                fixup->type = FIX_OFF32;
                break;
            case FIX_PTR16:
                fixup->type = FIX_PTR32;
                break;
            }
            break;
        }
    }
}

/*
 * Store asmfixup information of CodeInfo->InsFixup[index] in current
 * segment's fixup linked list.
*/
ret_code store_fixup( int index )
/**************************/
{
    struct asmfixup     *fixup;

    /* please note: forward references - which must be written in PASS 1 -
     aren't handled here! */

    fixup = CodeInfo->InsFixup[index];

    if( write_to_file == FALSE || fixup == NULL )
        return( NOT_ERROR );

    fixup->fixup_loc = GetCurrOffset();

//    CodeInfo->data[index] = CodeInfo->data[index] - fixup->sym->offset;
    fixup->offset = CodeInfo->data[index];

#ifdef DEBUG_OUT
    if (fixup->sym)
        DebugMsg(("store_fixup: loc=%s.%X, target=%s(%X+%X)\n", CurrSeg->seg->sym.name, fixup->fixup_loc, fixup->sym->name, fixup->sym->offset, fixup->offset));
    else
        DebugMsg(("store_fixup: loc=%s.%X, target=%X\n", CurrSeg->seg->sym.name, fixup->fixup_loc, fixup->offset));
#endif

    if (Options.output_format == OFORMAT_OMF) {
        struct fixup *fixnode;

        /* for OMF, the target's offset is stored an the fixup's location.
        */
        if( fixup->type != FIX_SEG && fixup->sym ) {
            CodeInfo->data[index] += fixup->sym->offset;
        }

        /* convert asmfixup to OMF fixup.
         * This probably should be done later, when the record is written;
         * then the segment's fixup list will always contain asmfixup entries.
         */
        fixnode = CreateOmfFixupRec( index );
        if( fixnode == NULL )
            return( ERROR );

        if( CurrSeg->seg->e.seginfo->FixupListHead == NULL ) {
            CurrSeg->seg->e.seginfo->FixupListTail = CurrSeg->seg->e.seginfo->FixupListHead = fixnode;
        } else {
            CurrSeg->seg->e.seginfo->FixupListTail->next = fixnode;
            CurrSeg->seg->e.seginfo->FixupListTail = fixnode;
        }
    } else {
        fixup->next2 = NULL;

        /* filter fixup types which aren't supported by the format */
        switch( fixup->type ) {
        case FIX_RELOFF8:
        case FIX_LOBYTE:
        case FIX_HIBYTE:
        case FIX_SEG:
        case FIX_PTR16:
        case FIX_PTR32:
            if (Options.output_format == OFORMAT_COFF ||
                Options.output_format == OFORMAT_ELF) {
                if ( fixup->sym )
                    AsmErr( UNSUPPORTED_FIXUP_TYPE,
                            Options.output_format == OFORMAT_COFF ? "COFF" : "ELF",
                            fixup->sym->name );
                else
                    AsmErr( UNSUPPORTED_FIXUP_TYPE,
                            Options.output_format == OFORMAT_COFF ? "COFF" : "ELF",
                            "<NULL>" );
                return( ERROR );
            }
            break;
        }

        if (Options.output_format == OFORMAT_ELF) {
            if (fixup->type == FIX_RELOFF32)
                CodeInfo->data[index] = -4;
        }
        /* special handling for assembly time variables needed */
        if ( fixup->sym && fixup->sym->variable ) {
            /* add symbol's offset to the fixup location and fixup's offset */
            CodeInfo->data[index] += fixup->sym->offset;
            fixup->offset         += fixup->sym->offset;
            /* and save symbol's segment in fixup */
            fixup->segment = fixup->sym->segment;
        }

        /* For COFF, just the difference to the target's
         symbol offset is stored an the fixup location!
        */

        /* for COFF/ELF/BIN, store the asmfixup records directly.
         * (this should be done for OMF as well!)
         */

        if( CurrSeg->seg->e.seginfo->FixupListHeadGen == NULL ) {
            CurrSeg->seg->e.seginfo->FixupListTailGen = CurrSeg->seg->e.seginfo->FixupListHeadGen = fixup;
        } else {
            CurrSeg->seg->e.seginfo->FixupListTailGen->next2 = fixup;
            CurrSeg->seg->e.seginfo->FixupListTailGen = fixup;
        }
    }
    return( NOT_ERROR );
}

ret_code MakeFpFixup( struct asm_sym *sym )
/*************************************/
{
    int old_count;
    int_8 old_frame;

    old_count = Opnd_Count;
    old_frame = Frame; /* what is with Frame_Datum? */
    Opnd_Count = 2;
    Frame = FRAME_LOC;
    AddFixup( sym, FIX_OFF16, OPTJ_NONE );
    Frame = old_frame;
    Opnd_Count = old_count;
    CodeInfo->data[2] = 0;
    return ( store_fixup( 2 ) );
}

