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
* Description:  Open Watcom banner strings and version defines.
*
****************************************************************************/

#define DOBANSTR( p )   #p
#define BANSTR( p )     DOBANSTR( p )

#ifndef _BETASTR_
#define _BETASTR_ "beta"
#endif

#ifdef _BETAVER
#define _BETA_                  _BETASTR_ BANSTR( _BETAVER )
#else
#define _BETA_
#endif

#define CURR_YEAR       "2008"

#define banner1p1(p)  p
#define banner1p2(v)  "Version " v
#define banner1(p,v) banner1p1(p) " "
#define banner1w(p,v) "JWasm v" v ", based on Open Watcom " banner1p1(p) " (WASM) "

#define banner2p1(year) "Portions Copyright (c) " year "-2002 Sybase, Inc. All Rights Reserved"
#define banner2p2() ""
#define banner2(year) banner2p1(year) "."

#define banner2a() "Portions Copyright (c) 1984-2002 Sybase, Inc. All Rights Reserved."

#define banner3       "Source code is available under the Sybase Open Watcom Public License."
#define banner3a      ""

// the following macros define the delimeters used by the resource
// compiler when concatenating strings
#define _RC_DELIM_LEFT_         [
#define _RC_DELIM_RIGHT_        ]

#define BAN_VER_STR "1.7" _BETA_

#define _JWASM_VERSION_ BAN_VER_STR
#define _JWASM_VERSION_INT_ "170"

