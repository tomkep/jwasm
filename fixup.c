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
#include "myassert.h"

// short jump label optimization
// if this is 0, there is just the simple "fixup backpatch",
// which cannot adjust any label offsets between the forward reference
// and the newly defined label, resulting in more passes to be needed.

#define LABELOPT 1

extern struct asm_sym   *SegOverride;

struct asmfixup         *InsFixups[3];
int_8                   Frame;          // Frame of current fixup
uint_8                  Frame_Datum;    // Frame datum of current fixup

/* set global vars Frame and Frame_Datum */

void find_frame( struct asm_sym *sym )
/*******************************************/
{
    if( SegOverride != NULL ) {
        sym = SegOverride;
        if( sym->state == SYM_GRP ) {
            Frame = FRAME_GRP;
            Frame_Datum = GetGrpIdx( sym );
        } else if( sym->segment != NULL ) {
            Frame = FRAME_SEG;
            Frame_Datum = GetSegIdx( sym->segment );
        }
    } else {
        asm_sym *grp;
        switch( sym->state ) {
        case SYM_INTERNAL:
        case SYM_PROC:
        case SYM_EXTERNAL:
            if( sym->segment != NULL ) {
                if( grp = GetGrp( sym ) ) {
                    Frame = FRAME_GRP;
                    Frame_Datum = GetGrpIdx( grp );
                } else {
                    Frame = FRAME_SEG;
                    Frame_Datum = GetSegIdx( sym->segment );
                }
            }
            break;
        case SYM_GRP:
            Frame = FRAME_GRP;
            Frame_Datum = GetGrpIdx( sym );
            break;
        case SYM_SEG:
            Frame = FRAME_SEG;
            Frame_Datum = GetSegIdx( sym->segment );
            break;
        default:
            break;
        }
    }
}

void add_frame( void )
/********************/
/* add frame data to current fixup */
/* expects global variables Frame and Frame_Datum to be set */
{
    struct asmfixup     *fixup;

    if( Parse_Pass != PASS_1 ) {
        fixup = InsFixups[Opnd_Count];
        if( fixup == NULL )
            return;
        fixup->frame = Frame;
        fixup->frame_datum = Frame_Datum;
    }
}

struct asmfixup *AddFixup( struct asm_sym *sym, enum fixup_types fixup_type, enum fixup_options fixup_option )
/************************************************************************************************************/
/*
  creates a new asmfixup item and initializes it using symbol <sym>.
  put the correct target offset into the link list when forward reference of
  relocatable is resolved;
  Global vars Frame, Frame_Datum and Opnd_Count must be set.
*/
{
    struct asmfixup     *fixup;

    switch( CodeInfo->info.token ) {
    case T_CMPS:
    case T_LODS:
    case T_MOVS:
    case T_OUTS:
    case T_INS:
    case T_SCAS:
    case T_STOS:
    case T_XLAT:
        return( NULL );
    }

    fixup = AsmAlloc( sizeof( struct asmfixup ) );
//    fixup->external = 0;
    fixup->sym = sym;
    fixup->offset = 0;
    if (CurrSeg) {
        fixup->fixup_loc = GetCurrOffset();
        fixup->def_seg = CurrSeg->seg;
    } else {
        fixup->fixup_loc = 0;
        fixup->def_seg = NULL;
    }
    fixup->frame = Frame;                   // this is just a guess
    fixup->frame_datum = Frame_Datum;
    fixup->next1 = sym->fixup;
    sym->fixup = fixup;
    fixup->type = fixup_type;
    fixup->loader_resolved = FALSE;
    fixup->fixup_option = fixup_option;
    InsFixups[Opnd_Count] = fixup;
    return( fixup );
}

#define SkipFixup() \
    fixup->next1 = sym->fixup; \
    sym->fixup = fixup

static int DoPatch( struct asm_sym *sym, struct asmfixup *fixup )
/***************************************************************/
{
    long                disp;
    long                max_disp;
    unsigned            size;
    asm_sym             *sym2;
    dir_node            *seg;

    // all relative fixups should occure only at first pass and they signal forward references
    // they must be removed after patching or skiped ( next processed as normal fixup )
    DebugMsg(("DoPatch(%s): fixup->ofs=%X, fixup->loc=%X\n", sym->name, fixup->offset, fixup->fixup_loc));
    seg = GetSeg( sym );
    if( seg == NULL || fixup->def_seg != seg ) {
        /* can't backpatch if fixup location is in diff seg than symbol */
        SkipFixup();
        return( NOT_ERROR );
    } else if( Parse_Pass != PASS_1 ) {
    } else if( sym->mem_type == MT_FAR && fixup->fixup_option == OPTJ_CALL ) {
        // convert far call to push cs + near call, only at first pass
        DebugMsg(("DoPatch: Phase error!\n"));
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
        size = 2;
        /* fall through */
    case FIX_RELOFF16:
        size++;
        /* fall through */
    case FIX_RELOFF8:
        size++;
        // calculate the displacement
        // disp = fixup->offset + GetCurrOffset() - fixup->fixup_loc - size;
        disp = fixup->offset + fixup->sym->offset - fixup->fixup_loc - size - 1;
        max_disp = (1UL << ((size * 8)-1)) - 1;
        if( disp > max_disp || disp < (-max_disp-1) ) {
            DebugMsg(("DoPatch: Phase error, disp=%X, fixup=%s, loc=%X!\n", disp, fixup->sym->name, fixup->fixup_loc));
            PhaseError = TRUE;
            switch( size ) {
            case 1:
                size = 0;
                switch( fixup->fixup_option ) {
                case OPTJ_EXPLICIT:
                    AsmError( JUMP_OUT_OF_RANGE );
                    sym->fixup = NULL;
                    return( ERROR );
                case OPTJ_EXTEND:
                    size++;
                    /* fall through */
                case OPTJ_JXX:
                    size++;
                    /* fall through */
                default:
                    if( CodeInfo->use32 )
                        size += 2;
                    size++;
                    sym->offset += size;
#if LABELOPT
                    DebugMsg(("sym %s, offset changed %X -> %X\n", sym->name, sym->offset - size, sym->offset));
                    for (sym2 = seg->e.seginfo->labels;sym2;sym2 = (asm_sym *)((dir_node *)sym2)->next ) {
                        if (sym2 == sym)
                            continue;
                        if (sym2->offset < fixup->fixup_loc)
                            break;
                        sym2->offset += size;
                        DebugMsg(("sym %s, offset changed %X -> %X\n", sym2->name, sym2->offset - size, sym2->offset));
                    }
#endif
                    for (;size;size--)
                        OutputByte(0xCC);
                    break;
                }
                break;
            case 2:
            case 4:
                AsmWarn( 4, JUMP_OUT_OF_RANGE );
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

int BackPatch( struct asm_sym *sym )
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

    DebugMsg(("BackPatch(%s) enter\n", sym->name));
    fixup = sym->fixup;
    sym->fixup = NULL;
    for( ; fixup != NULL; fixup = next ) {
        next = fixup->next1;
        if( DoPatch( sym, fixup ) == ERROR ) {
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}

struct fixup *CreateFixupRec( int index )
/***************************************/
/* Create a fixup record for OMF, translates "asmfixup" to "fixup";
   Note that if Modend is TRUE, it means the fixup is the starting address
   for the module.
*/
{
    struct asmfixup     *fixup;         // fixup structure from JWasm
    struct fixup        *fixnode;       // fixup structure for OMF
    struct asm_sym      *sym;
    struct asm_sym      *grpsym;

    fixup = InsFixups[index];

    if( fixup == NULL )
        return( NULL );

    sym = fixup->sym;
    if(( sym == NULL ) || (sym->state == SYM_STACK))
        return( NULL );

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

    DebugMsg(("CreateFixupRec: sym=%s, state=%u\n", sym->name, sym->state));

    /* loader_resolved is FALSE for OFFSET, TRUE for LROFFSET */
    fixnode->loader_resolved = fixup->loader_resolved;

    /* set the fixup's location in current LEDATA */
    /* CurrSeg->curr_loc - CurrSeg->start_loc */

    fixnode->loc_offset = GetCurrOffset() - GetCurrSegStart();

    /*------------------------------------*/
    /* Determine the Target and the Frame */
    /*------------------------------------*/

    if( sym->state == SYM_UNDEFINED ) {
        DebugMsg(("CreateFixupRec: state of >%s< is SYM_UNDEFINED\n", sym->name));
        AsmErr( SYMBOL_NOT_DEFINED, sym->name );
        return( NULL );
    } else if( sym->state == SYM_GRP ) {

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
            DebugMsg(("CreateFixupRec: EXTERNAL %s\n", sym->name));
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
            DebugMsg(("CreateFixupRec: PROTO %s\n", sym->name));
            fixnode->lr.target = TARGET_EXT;
            fixnode->lr.target_datum = sym->idx;
        } else {
            /* it's a SYM_INTERNAL/SYM_PROC */
            DebugMsg(("CreateFixupRec: sym->name=%s state=%X segm=%X\n", sym->name, sym->state, sym->segment));
            if ( sym->segment == NULL ) {
                AsmErr( SEGMENT_MISSING_FOR_FIXUP, sym->name );
                return (NULL);
            }
            grpsym = GetGrp( sym );

            fixnode->lr.target = TARGET_SEG;

            if( Modend ) {
                fixnode->lr.target = TARGET_SEG & TARGET_WITH_DISPL;
                fixup->frame = FRAME_TARG;
                DebugMsg(("ModEnd sym=%s segm=%X, grp=%X\n", sym->name, sym->segment, ((dir_node *)sym->segment)->e.seginfo->group));
            }
            if (grpsym) {
                fixup->frame = FRAME_GRP;
                fixup->frame_datum = GetGrpIdx( grpsym );
            }
            fixnode->lr.target_datum = GetSegIdx( sym->segment );
        }

        if( fixup->frame != EMPTY ) {
            fixnode->lr.frame = (uint_8)fixup->frame;
        } else {
            fixnode->lr.frame = FRAME_TARG;
        }
        fixnode->lr.frame_datum = fixup->frame_datum;

        if( Modend ) {
            DebugMsg(("CreateFixupRec: ModEnd fixup=%X/%X/%X/%X\n", fixnode->lr.target, fixnode->lr.frame, fixnode->lr.frame_datum, fixnode->loc_offset));
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
  this routine marks the correct target offset and data record address for
  FIXUPP record;
*/
{
    struct asmfixup     *fixup;

    fixup = InsFixups[index];
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

/* Store asmfixup information of InsFixup[index] in current segment's
   fixup linked list.
*/

int store_fixup( int index )
/**************************/
{
    struct asmfixup     *fixup;

    /* please note: forward references - which must be written in PASS 1 -
     aren't handled here! */

    fixup = InsFixups[index];

    if( write_to_file == FALSE || fixup == NULL)
        return( NOT_ERROR );

    fixup->fixup_loc = GetCurrOffset();

//    CodeInfo->data[index] = CodeInfo->data[index] - fixup->sym->offset;
    fixup->offset = CodeInfo->data[index];

    DebugMsg(("store_fixup: loc=%s.%X, target=%s(%X+%X)\n", CurrSeg->seg->sym.name, fixup->fixup_loc, fixup->sym->name, fixup->sym->offset, fixup->offset));

    if (Options.output_format == OFORMAT_OMF) {
        struct fixup *fixnode;

        /* for OMF, the target's offset is stored an the fixup's location.
        */
        if( fixup->type != FIX_SEG ) {
            CodeInfo->data[index] += fixup->sym->offset;
        }

        /* convert asmfixup to OMF fixup! */
        fixnode = CreateFixupRec( index );
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

        if (Options.output_format == OFORMAT_ELF) {
            if (fixup->type == FIX_RELOFF32)
                CodeInfo->data[index] = -4;
        }
        /* special handling for assembly time variables needed */
        if (fixup->sym->variable) {
            /* add symbol's offset to the fixup location and fixup's offset */
            CodeInfo->data[index] += fixup->sym->offset;
            fixup->offset         += fixup->sym->offset;
            /* and save symbol's segment in fixup */
            fixup->segment = fixup->sym->segment;
        }

        /* For COFF, just the difference to the target's
         symbol offset is stored an the fixup location!
        */

        /* for COFF/ELF, store the asmfixup records directly! */

        if( CurrSeg->seg->e.seginfo->FixupListHeadCoff == NULL ) {
            CurrSeg->seg->e.seginfo->FixupListTailCoff = CurrSeg->seg->e.seginfo->FixupListHeadCoff = fixup;
        } else {
            CurrSeg->seg->e.seginfo->FixupListTailCoff->next2 = fixup;
            CurrSeg->seg->e.seginfo->FixupListTailCoff = fixup;
        }
    }
    return( NOT_ERROR );
}

int MakeFpFixup( struct asm_sym *sym )
/*************************************/
{
    int old_count;
    int_8 old_frame;

    old_count = Opnd_Count;
    old_frame = Frame;
    Opnd_Count = 2;
    Frame = FRAME_LOC;
    AddFixup( sym, FIX_OFF16, OPTJ_NONE );
    Frame = old_frame;
    Opnd_Count = old_count;
    CodeInfo->data[2] = 0;
    if( store_fixup( 2 ) == ERROR )
        return( ERROR ); // extra entry in insfixups
    return( NOT_ERROR );
}

