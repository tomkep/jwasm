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
* Description:  Declarations for output queues
*
****************************************************************************/

#ifndef _QUEUES_H_
#define _QUEUES_H_

typedef struct line_num_info {
    uint_16 number;
    uint_32 offset;
    union {
        uint srcfile;
        asm_sym * sym;
    };
} line_num_info;

extern long     GetQueueItems(void *);
extern void     AddPublicData( asm_sym *sym );
extern void     AddLnameData( dir_node *data );
extern void     AddGlobalData( dir_node *data );
extern void     AddLinnumData( struct line_num_info *data );

extern direct_idx FindLnameIdx( char * );
extern char     *GetLname( direct_idx );

extern unsigned GetLnameData( char ** );
extern int      GetLinnumData( struct linnum_data **ldata, bool *need32 );
extern void     GetGlobalData( void );

extern void     QueueInit( void );
extern void     QueueFini( void );

#endif
