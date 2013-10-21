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
* Description:  handle OMF fixups
*
****************************************************************************/

#include <stddef.h>

#include "globals.h"
#include "parser.h"
#include "segment.h"
#include "fixup.h"
#include "omfint.h"
#include "omfspec.h"
#include "myassert.h"

extern const char szNull[];
extern uint omf_GetGrpIdx( struct asym *sym );

static uint_8 *putIndex( uint_8 *p, uint_16 index )
/*************************************************/
{
    if( index > 0x7f ) {
        *p++ = 0x80 | ( index >> 8 );
    }
    *p++ = index;
    return( p );
}

static uint_8 *put16( uint_8 *p, uint_16 value )
/**********************************************/
{
    WriteU16( p, value );
    return( p + sizeof( uint_16 ) );
}

static uint_8 *put32( uint_8 *p, uint_32 value )
/**********************************************/
{
    WriteU32( p, value );
    return( p + sizeof( uint_32 ) );
}

static uint_8 *putFrameDatum( uint_8 *p, uint_8 method, uint_16 datum )
/*********************************************************************/
{
    switch( method ) {
    case FRAME_SEG:
    case FRAME_GRP:
    case FRAME_EXT:
        return( putIndex( p, datum ) );
    case FRAME_ABS:
        return( put16( p, datum ) );
    }
    /* for FRAME_LOC, FRAME_TARG, and FRAME_NONE there is nothing to output */
    return( p );
}

static uint_8 *putTargetDatum( uint_8 *p, uint_8 method, uint_16 datum )
/**********************************************************************/
{
    if( ( method & 0x03 ) == TARGET_ABSWD ) {
        return( put16( p, datum ) );
    }
    return( putIndex( p, datum ) );
}

/* create fields
 * - Fix Data ( byte, type of frame and target ),
 * - Frame Datum (index field to a SEGDEF, GRPDEF or EXTDEF),
 * - Target Datum (index field to a SEGDEF, GRPDEF or EXTDEF),
 * - Target Displacement ( 2- or 4-byte )
 * of a FIXUP subrecord.
 *
 * type is FIX_GEN_INTEL or FIX_GEN_MS386
 */

uint OmfFixGenLogRef( const struct logref *lr, uint_8 *buf, enum fixgen_types type )
/**********************************************************************************/
{
    uint_8  *p;
    uint_8  target;

    /**/myassert( lr != NULL );
    /**/myassert( buf != NULL );
    /**/myassert( type == FIX_GEN_INTEL || type == FIX_GEN_MS386 );

    /*
        According to the discussion on p102 of the Intel OMF document, we
        cannot just arbitrarily write fixups without a displacment if their
        displacement field is 0.  So we use the is_secondary field.
    */
    target = lr->target;
    if( lr->target_offset == 0 && lr->is_secondary ) {
        target |= 0x04; /* bit 2=1 -> no displacement field */
    }
    p = buf;
    *p++ = ( lr->frame << 4 ) | ( target );
    p = putFrameDatum( p, lr->frame, lr->frame_datum );
    p = putTargetDatum( p, target, lr->target_datum );
    if( ( target & 0x04 ) == 0 ) {
        if( type == FIX_GEN_MS386 ) {
            p = put32( p, (uint_32)lr->target_offset );
        } else {
            p = put16( p, (uint_16)lr->target_offset );
        }
    }
    return( p - buf );
}

#if 0 /* v2.11: obsolete */

static uint OmfFixGenPhysRef( const struct physref *ref, uint_8 *buf, enum fixgen_types type )
/********************************************************************************************/
{
    uint_8  *p;

    /**/myassert( ref != NULL );
    /**/myassert( buf != NULL );
    /**/myassert( type == FIX_GEN_INTEL || type == FIX_GEN_MS386 );

    p = put16( buf, ref->frame );
    p = put16( p, ref->offset );
    return( p - buf );
}

/* used when the MODEND record is written.
 * is_logical is always 1 then.
 * v2.11: obsolete. OmfFixGetLogRef() is now called directly
 * by writeModend() in omfint.c
 */

uint OmfFixGenRef( const union logphys *ref, int is_logical, uint_8 *buf, enum fixgen_types type )
/************************************************************************************************/
{

    /**/myassert( ref != NULL );
    /**/myassert( buf != NULL );
    /**/myassert( type == FIX_GEN_INTEL || type == FIX_GEN_MS386 );

    if( is_logical ) {
        return( OmfFixGenLogRef( &ref->log, buf, type ) );
    }
    return( OmfFixGenPhysRef( &ref->phys, buf, type ) );
}

#endif

/* fill a logref from a fixup's info */

static int omf_fill_logref( const struct fixup *fixup, struct logref *lr )
/************************************************************************/
{
    struct asym      *sym;

    sym = fixup->sym; /* may be NULL! */

    DebugMsg1(("omf_fill_logref: sym=%s, state=%u, fixup->type=%u\n",
               sym ? sym->name : "NULL", sym ? sym->state : 0, fixup->type ));

    /*------------------------------------*/
    /* Determine the Target and the Frame */
    /*------------------------------------*/

    if( sym == NULL ) {

        DebugMsg(("omf_fill_logref: sym is NULL, frame_type=%u\n", fixup->frame_type ));
        if ( fixup->frame_type == FRAME_NONE ) /* v1.96: nothing to do without a frame */
            return( 0 );
        lr->target = fixup->frame_type;
        lr->target_datum = fixup->frame_datum;
        lr->frame = FRAME_TARG;

    } else if( sym->state == SYM_UNDEFINED ) { /* shouldn't happen */

        DebugMsg(("omf_fill_logref: sym->state is SYM_UNDEFINED\n" ));
        EmitErr( SYMBOL_NOT_DEFINED, sym->name );
        return( 0 );

    } else if( sym->state == SYM_GRP ) {

        DebugMsg1(("omf_fill_logref: sym->state is SYM_GRP\n" ));
        lr->target = TARGET_GRP;
        lr->target_datum = ((struct dsym *)sym)->e.grpinfo->grp_idx;
        if( fixup->frame_type != FRAME_NONE ) {
            lr->frame = fixup->frame_type;
            lr->frame_datum = fixup->frame_datum;
        } else {
            lr->frame = FRAME_GRP;
            lr->frame_datum = lr->target_datum;
        }

    } else if( sym->state == SYM_SEG ) {

        DebugMsg1(("omf_fill_logref: sym->state is SYM_SEG %s\n" ));
        lr->target = TARGET_SEG;
        lr->target_datum = GetSegIdx( sym );
        if( fixup->frame_type != FRAME_NONE ) {
            lr->frame = fixup->frame_type;
            lr->frame_datum = fixup->frame_datum;
        } else {
            lr->frame = FRAME_SEG;
            lr->frame_datum = lr->target_datum;
        }

    } else {

        /* symbol is a label */

        lr->frame_datum = fixup->frame_datum;
        if( sym->state == SYM_EXTERNAL ) {
            DebugMsg1(("omf_fill_logref: sym->state is SYM_EXTERNAL, fixup->frame_type/datum=%u/%u\n",
                      fixup->frame_type, fixup->frame_datum ));
            lr->target = TARGET_EXT;
            lr->target_datum = sym->ext_idx1;

            if( fixup->frame_type == FRAME_GRP && fixup->frame_datum == 0 ) {
                /* set the frame to the frame of the corresponding segment */
                lr->frame_datum = omf_GetGrpIdx( sym );
            }
        } else {
            /* must be SYM_INTERNAL */
            /**/myassert( sym->state == SYM_INTERNAL );
            DebugMsg1(("omf_fill_logref: sym->state is SYM_INTERNAL, sym->segment=%s, fixup->frame/datum=%u/%u\n",
                       sym->segment ? sym->segment->name : "NULL", fixup->frame_type, fixup->frame_datum ));
            /* v2.08: don't use info from assembly-time variables */
            if ( sym->variable ) {
                lr->target = ( fixup->frame_type == FRAME_GRP ? TARGET_GRP : TARGET_SEG);
                lr->target_datum = fixup->frame_datum;
            } else if ( sym->segment == NULL ) { /* shouldn't happen */
                EmitErr( SEGMENT_MISSING_FOR_FIXUP, sym->name );
                return ( 0 );
            } else {
                lr->target = TARGET_SEG;
                lr->target_datum = GetSegIdx( sym->segment );
            }
        }

        if( fixup->frame_type != FRAME_NONE ) {
            lr->frame = (uint_8)fixup->frame_type;
        } else {
            lr->frame = FRAME_TARG;
        }
    }

    /*--------------------*/
    /* Optimize the fixup */
    /*--------------------*/

    if( lr->frame == ( lr->target - TARGET_SEG ) ) {
        lr->frame = FRAME_TARG;
    }

    return( 1 );
}

/* translate a fixup into its binary representation,
 * which is a "FIXUPP subrecord" according to OMF docs.
 * structure:
 * - WORD, Locat: 1MLLLLDD DDDDDDDD, is
 *   1 = indicates FIXUPP, no THREAD
 *   M = mode: 1=segment relative, 0=self relative
 *   L = location, see LOC_ entries in omfspec.h
 *   D = data record offset, 10 bits for range 0-3FFh
 * - BYTE, Fix Data: FRRRTPGG, is
 *   F = 0=frame defined in fixup, 1=frame defined in frame thread
 *   R = Frame
 *   T = 0=target defined in fixup, 1=Target defined in target thread
 *   P = 0=target displacement is present
 *   G = lower bits of target method (F=0), target thread number (F=1)
 * - void/BYTE/WORD, Frame Datum
 * - BYTE/WORD, Target Datum
 * - WORD/DWORD, Target Displacement
 */

uint OmfFixGenFix( const struct fixup *fixup, uint_32 start_loc, uint_8 *buf, enum fixgen_types type )
/****************************************************************************************************/
{
    uint_8  locat1;
    uint_8  self_relative = FALSE;
    uint    data_rec_offset;
    struct logref lr;

    /**/myassert( fixup != NULL );
    /**/myassert( buf != NULL );
    /**/myassert( type == FIX_GEN_INTEL || type == FIX_GEN_MS386 );

    switch( fixup->type ) {
    case FIX_RELOFF8:
        self_relative = TRUE;
        /* no break */
    case FIX_OFF8:
        locat1 = ( LOC_OFFSET_LO << 2 );
        break;
    case FIX_RELOFF16:
        self_relative = TRUE;
        /* no break */
    case FIX_OFF16:
        locat1 = ( fixup->loader_resolved ? LOC_MS_LINK_OFFSET << 2 : LOC_OFFSET << 2 );
        break;
    case FIX_RELOFF32:
        self_relative = TRUE;
        /* no break */
    case FIX_OFF32:
        locat1 = ( fixup->loader_resolved ? LOC_MS_LINK_OFFSET_32 << 2 : LOC_MS_OFFSET_32 << 2 );
        break;
    case FIX_HIBYTE:
        locat1 = ( LOC_OFFSET_HI << 2 );
        break;
    case FIX_SEG:
        locat1 = ( LOC_BASE << 2 );
        break;
    case FIX_PTR16:
        locat1 = ( LOC_BASE_OFFSET << 2 );
        break;
    case FIX_PTR32:
        locat1 = ( LOC_MS_BASE_OFFSET_32 << 2 );
        break;
    default: /* shouldn't happen. Check for valid fixup has already happened */
        EmitErr( UNSUPPORTED_FIXUP_TYPE,
               ModuleInfo.fmtopt->formatname,
               fixup->sym ? fixup->sym->name : szNull );
        return( 0 );
    }
    locat1 |= self_relative ? 0x80 : 0xc0; /* explicit fixup */

    lr.is_secondary = TRUE;
    lr.target_offset = 0;

    if ( omf_fill_logref( fixup, &lr ) == 0 )
        return( 0 );

    /* magnitude of fixup's position is 10! */
    /**/myassert( fixup->location - start_loc < 1024 );

    /* calculate the fixup's position in current LEDATA */
    data_rec_offset = fixup->location - start_loc;

    locat1 |= data_rec_offset >> 8;
    *buf = locat1;
    *(buf+1) = (uint_8)data_rec_offset;
    return( 2 + OmfFixGenLogRef( &lr, buf+2, type ) );
}

