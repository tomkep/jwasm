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
* Description:  Fatal error messages
*
****************************************************************************/

fix( OUT_OF_MEMORY,          0, CloseFiles, 1 ),
fix( CANNOT_OPEN_FILE,       1, NULL,       1 ),
fix( CANNOT_CLOSE_FILE,      1, NULL,       1 ),
fix( FILE_WRITE_ERROR,       1, CloseFiles, 1 ),
fix( FILE_LSEEK_ERROR,       1, CloseFiles, 1 ),
fix( EXPANDED_LINE_TOO_LONG, 1, CloseFiles, 1 ),
fix( NESTING_LEVEL_TOO_DEEP, 0, CloseFiles, 1 ),