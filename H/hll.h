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
* Description:  hll constructs .IF, ...
*
****************************************************************************/


#ifndef _HLL_H_
#define _HLL_H_

// item for .IF, .WHILE, .REPEAT, ...

typedef struct hll_list {
    char                *symfirst;      // symbol first (local) label
    char                *symtest;       // continue, test for exit
    char                *symexit;       // exit loop
    char                *condlines;     // for .WHILE: lines to add after test
    unsigned            isif:1;         // it is a .IF/.ELSEIF item
    unsigned            iswhile:1;      // it is a .WHILE item
    unsigned            isrepeat:1;     // it is a .REPEAT item
} hll_list;

/*---------------------------------------------------------------------------*/

extern void             PrepHllLabels( void );  // reset label counter for hll labels
extern int              StartHllDef( int );     // begin a .IF, .WHILE, .REPEAT
extern int              EndHllDef( int );       // end a .IF, .WHILE, .REPEAT
extern int              ExitHllDef( int );      // exit a .IF, .WHILE, .REPEAT

#endif
