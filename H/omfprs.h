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
* Description:  was "object file parser", which is virtually not used
*               by JWasm. Is to be removed ASAP.
*
****************************************************************************/


#ifndef PRSOBJ_H
#define PRSOBJ_H    1
#include <stddef.h>
#include "omfrec.h"
#include "watcom.h"
#include "omfpc.h"
#include "queue.h"
#include "omfio.h"

typedef struct {
    uint_8      pass;           /* current pass number */
    OBJ_WFILE   *file_out;      /* output file pointer */
} pobj_state;

#define POBJ_ALL_PASSES     0   /* invoke filter on all passes          */
#define POBJ_READ_PASS      1   /* invoke filter on the read pass only  */
#define POBJ_WRITE_PASS     2   /* invoke filter on the write pass only */

typedef int (*pobj_filter)( obj_rec *objr, pobj_state *state );

/* for lists of filters... */
typedef struct {
    uint_8      command;
    uint_8      pass;
    pobj_filter func;
} pobj_list;

enum extra_commands {
        /* our effective minimum command (should be == 0(mod2)) */
    CMD_POBJ_MIN_CMD = ( CMD_MIN_CMD -             1  ) & ~1,
        /* the number of extra commands goes here ^^^ */
    CMD_LAST_DEFN = CMD_POBJ_MIN_CMD
        /* each extra command should be == 0 (mod2), so add 2 to previous */
};

extern void omf_RegList( const pobj_list *list, size_t len );
extern void omf_UnRegList( const pobj_list *list, size_t len );

#endif
