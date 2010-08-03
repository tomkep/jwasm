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

extern dir_node         *CurrSeg;       // stack of open segments

extern void             SetSymSegOfs( struct asm_sym * ); /* Store location information about a symbol */
extern int              GetSymOfssize( struct asm_sym * );
//extern direct_idx       GetLnameIdx( const char * );
extern uint_32          GetCurrOffset( void );  // Get current segment's offset
extern ret_code         SetCurrOffset( uint_32, bool, bool );
//extern direct_idx       SetSegmentClass( struct asm_sym *, const char * );
extern asm_sym          *CreateSegment( const char *name, const char *classname, uint_8 alignment, uint_8 is32 );
//extern dir_node         *GetCurrSeg( void );  /* Get current segment; NULL means none */
//extern int              GetCurrClass( void ); /* get curr segment's class index */
//extern uint           GetGrpIdx( struct asm_sym * );/* get symbol's group index, from the symbol itself or from the symbol's segment */
extern uint             GetSegIdx( struct asm_sym * );/* get symbol's segment index, from the symbol itself */
extern ret_code         GrpDef( int );          // GROUP directive
extern ret_code         SegmentDir( int );      // SEGMENT directive
extern ret_code         EndsDir( int );         // ENDS directive
extern ret_code         SetCurrSeg( int );      // SEGMENT directive if pass > 1
extern void             SegmentInit( int );     // init segments
extern asm_sym          *GetGroup( struct asm_sym * );
extern uint_32          GetCurrSegAlign( void );
extern ret_code         SetOfssize( void );
extern void             DefineFlatGroup( void );
extern ret_code         SegmentModuleExit( void );

/* simplified segment functions */
extern char             *GetCodeSegName( void );
extern const char       *GetCodeClass( void );
extern ret_code         SimplifiedSegDir( int ); // .CODE, .DATA, ... directive
extern ret_code         ModelSimSegmInit( int type );
extern void             SetModelDefaultSegNames( void );

#endif
