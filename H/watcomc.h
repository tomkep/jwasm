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
*  Description: file for GNU (Posix) CRT. Included by globals.h
*               if __UNIX__ or __CYGWIN__ is defined.
****************************************************************************/

#define _stricmp strcasecmp
#define _strcmpi strcasecmp
#define _strnicmp strncasecmp
#define _memicmp strncasecmp

#define _ltoa   ltoa
#define _strupr strupr

char *_fullpath( char *, const char *, size_t );

#define _MAX_DRIVE      48      /*  maximum length of node name w/ '\0' */
#define _MAX_DIR        256     /*  maximum length of subdirectory      */
#define _MAX_FNAME      48      /*  maximum length of a file name       */
#define _MAX_EXT        48      /*  maximum length of a file extension  */

#ifndef _MAX_PATH
 #define _MAX_PATH      256     /*  maximum length of path name         */
#endif


