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
* Description:  segment related prototypes & externals
*
****************************************************************************/


#ifndef _SEGMENT_H_
#define _SEGMENT_H_

#define GetSeg( x )     (dir_node *)x->segment
#define SEGISCODE( x )  ( x->seg->e.seginfo->segtype == SEGTYPE_CODE )

extern seg_item         *CurrSeg;       // stack of open segments

extern uint_32          GetCurrSegStart( void ); /* Get offset of segment at the start of current LEDATA record */
extern void             SetSymSegOfs( struct asm_sym * ); /* Store location information about a symbol */
extern int              SymIs32( struct asm_sym * );
extern direct_idx       GetLnameIdx( char * );
extern uint_32          GetCurrOffset( void );  // Get offset from current segment
extern ret_code         SetCurrOffset( int_32, bool, bool );
extern dir_node         *GetCurrSeg( void );  /* Get current segment; NULL means none */
extern int              GetCurrClass( void ); /* get curr segment's class index */
extern uint             GetGrpIdx( struct asm_sym * );/* get symbol's group index, from the symbol itself or from the symbol's segment */
extern uint             GetSegIdx( struct asm_sym * );/* get symbol's segment index, from the symbol itself */
extern ret_code         GrpDef( int );          // GROUP directive
extern ret_code         SegDef( int );          // SEGMENT/ENDS directive
extern ret_code         SetCurrSeg( int );      // SEGMENT/ENDS directive if pass > 1
extern ret_code         SimplifiedSegDir( int ); // .CODE, .DATA, ... directive
extern void             SegmentInit( int );     // init segments
extern asm_sym          *GetGrp( struct asm_sym * );
extern uint_32          GetCurrSegAlign( void );
extern ret_code         SetUse32( void );
extern void             DefineFlatGroup( void );
extern ret_code         SegmentModulePrologue( int type );
extern ret_code         SegmentModuleEnd( void );

#endif
