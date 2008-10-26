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

extern void     AddTokens( int start, int count );
extern dir_node *CreateMacro( char * );     // create a macro symbol
extern void     ReleaseMacroData( dir_node * );
extern ret_code MacroDef( int );             // define a macro
extern ret_code FillMacro( dir_node *, int, bool );  // set macro content
extern ret_code PurgeMacro( int );           // remove a macro
extern ret_code ExpandMacro( char * );
extern int      RunMacro( dir_node *, char *, char *, bool, bool, bool );
extern asm_sym  *SetTextMacro( asm_sym*, char *, char * );
extern int      GetTextMacroValue( char *, char *);
extern ret_code CatStrDef( int, asm_sym** ); // CatStr + TEXTEQU directive
extern ret_code SubStrDef( int, char *);     // SubStr directive
extern ret_code SizeStrDef( int );           // SizeStr directive
extern ret_code InStrDef( int, char *);      // InStr directive
extern ret_code LoopDirective( int, int );   // FOR,FORC,IRP,IRPC,REPT,...
extern int      MacroInit( int );

#endif
