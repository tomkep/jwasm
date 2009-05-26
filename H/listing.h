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
* Description:  listing interface.
*
****************************************************************************/


#ifndef _LISTING_H_INCLUDED
#define _LISTING_H_INCLUDED

enum lsttype {
 LSTTYPE_LIDATA    = 0,
 LSTTYPE_EQUATE    = 1,
 LSTTYPE_DIRECTIVE = 2,
 LSTTYPE_MACRO     = 3,
 LSTTYPE_STRUCT    = 4,
 LSTTYPE_LABEL     = 5,
 LSTTYPE_MACROLINE = 6
};

extern void LstOpenFile( void );
extern void LstCloseFile( void );
extern void LstWrite( enum lsttype, unsigned int ofs, void * sym );
extern void LstWriteSrcLine( void );
extern void LstWriteCRef( void );
extern void LstPrintf( const char *format, ... );
extern void LstNL( void );

extern uint_32 list_pos;        /* current LST file position */
extern uint_32 list_pos_start;  /* position of current line */

#endif
