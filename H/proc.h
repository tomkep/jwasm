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
* Description:  interface to PROC.C routines
*
****************************************************************************/


#ifndef _PROC_H_
#define _PROC_H_

extern dir_node         *CurrProc;      // current procedure

/*---------------------------------------------------------------------------*/

extern ret_code         proc_check( void );
extern ret_code         ExamineProc( dir_node *, int , bool );  // common for PROC/PROTO
extern ret_code         LocalDef( int );        // LOCAL directive
extern asm_sym         *CreateProc( asm_sym *, const char *, unsigned char );
extern void             DeleteProc( dir_node * );
extern ret_code         ProcDef( int );         // PROC directive
extern ret_code         ProtoDef( int, const char * );// PROTO directive
extern ret_code         EndpDef( int );         // ENDP directive
#if AMD64_SUPPORT
extern ret_code         ExcFrameDirective( int );
#endif
extern ret_code         InvokeDirective( int );
extern ret_code         RetInstr( int, int );   // emit return statement from procedure
//extern int              WritePrologue( void );  // emit prologue statement after the
                                                // declaration of a procedure
extern void             ProcInit( void );

#endif
