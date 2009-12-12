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
*  Description: declarations for assume.c
*
****************************************************************************/

#ifndef _ASSUME_H_
#define _ASSUME_H_

typedef struct {
    asm_sym             *symbol;        /* segment, group or type that is to
                                           be associated with the register */
    unsigned            error:1;        // the register is assumed to ERROR
    unsigned            flat:1;         // the register is assumed to FLAT
} assume_info;

/* segment assume table is sorted by seg reg number: ES,CS,SS,DS,FS,GS */
extern assume_info SegAssumeTable[];

#define NUM_SEGREGS 6

extern void AssumeInit( void );     // init assume tables

extern enum assume_segreg search_assume( struct asm_sym *sym,
                                         enum assume_segreg def, bool search_grps );

extern enum assume_segreg  GetAssume( struct asm_sym *, struct asm_sym*, enum assume_segreg, asm_sym * * );
extern struct asm_sym   *GetOverrideAssume( enum assume_segreg );

extern struct asm_sym   *GetStdAssume( int );

extern ret_code         AssumeDirective( int );
extern void             ModelAssumeInit( void );
extern void             SetSegAssumeTable( void * );
extern void             SetStdAssumeTable( void * );
extern void             GetSegAssumeTable( void * );
extern void             GetStdAssumeTable( void * );

#endif
