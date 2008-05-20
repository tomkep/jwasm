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
* Description:  prototypes for macro stuff
*
****************************************************************************/

#ifndef _MACRO_H_
#define _MACRO_H_

extern void    AddTokens( ASM_TOK **buffer, int start, int count );
extern int     MacroDef( int );  // define a macro
extern int     FillMacro( dir_node *, int, bool );  // set macro content
extern int     PurgeMacro( int );      // remove a macro
extern int     ExpandMacro( char * );
extern int     RunMacro( dir_node *, char *, char *, bool, bool, bool );
extern int     EndMacro( int, int );
extern int     CatStrDef( int, asm_sym** ); // CatStr + TEXTEQU directive
extern int     SubStrDef( int, char *);     // SubStr directive
extern int     SizeStrDef( int );           // SizeStr directive
extern int     InStrDef( int, char *);      // InStr directive
extern int     ForDirective( int, int );

#define STRING_EXPANDED (NOT_ERROR+1)

#endif
