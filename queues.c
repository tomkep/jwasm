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
* Description:  queues routines
*
****************************************************************************/

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "queue.h"
#include "queues.h"
#include "fastpass.h"
#include "proc.h"

#define COFF_LINNUM 1

typedef struct queuenode {
    void *next;
    void *data;
} queuenode;

static qdesc   LnameQueue;  // LNAME items (segments, groups and classes)
static qdesc   PubQueue;    // PUBLIC items
static qdesc   GlobalQueue; // GLOBAL items ( externdefs )

// add a new node to a queue

static void QAddItem( qdesc *queue, void *data )
/***********************************************/
{
    struct queuenode    *node;

    node = AsmAlloc( sizeof( queuenode ) );
    node->data = data;
    QEnqueue( queue, node );
}

void AddPublicData( asm_sym *sym )
/*********************************/
{
    DebugMsg(("AddPublicData: %s\n", sym->name ));
    QAddItem( &PubQueue, sym );
}

// get (next) PUBLIC item

asm_sym * GetPublicData( void * *vp )
/******************************************/
{
    queuenode * *curr = (queuenode * *)vp;

    if ( PubQueue.head == NULL)
        return( NULL );

    if (*curr == NULL)
        *curr = PubQueue.head;
    else
        *curr = (*curr)->next;

    for ( ; *curr ; *curr = (*curr)->next ) {
        asm_sym *sym = (*curr)->data;
        if( sym->isproc ) {
            /* skip PROTOs without matching PROC */
            if( sym->state == SYM_EXTERNAL ) {
                continue;
            }
        } else if ( sym->state == SYM_EXTERNAL ) {
            /* silently skip EXTERNDEFs (and EXTERNs???) */
            //if (sym->weak == TRUE)
                continue;
        }
        if( sym->state != SYM_INTERNAL && sym->state != SYM_EXTERNAL ) {
            // v1.95: make a full second pass and emit error on PUBLIC
            //AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
#if FASTPASS
            SkipSavedState();
#endif
            continue;
        }
        return( sym );
    }
    return( NULL );
}

static void FreePubQueue( void )
/******************************/
{
    while( PubQueue.head != NULL ) {
#if FASTMEM /* for FASTMEM, AsmFree() will vanish, so avoid unused p var */
        QDequeue( &PubQueue );
#else
        void *p = QDequeue( &PubQueue );
        AsmFree( p );
#endif
    }
}

/* what's inserted into the LNAMES queue:
 * SYM_SEG: segment names
 * SYM_GRP: group names
 * SYM_CLASS_LNAME : class names
*/

void AddLnameData( asm_sym *sym )
/********************************/
{
    QAddItem( &LnameQueue, sym );
}

// find a class index.
// the classes aren't in the symbol table!

direct_idx FindLnameIdx( const char *name )
/*****************************************/
{
    queuenode           *node;
    asm_sym             *sym;

    for( node = LnameQueue.head; node != NULL; node = node->next ) {
        sym = (asm_sym *)node->data;
        if( sym->state != SYM_CLASS_LNAME )
            continue;
        if( _stricmp( sym->name, name ) == 0 ) {
            return( sym->idx );
        }
    }
    return( LNAME_NULL );
}

char *GetLname( direct_idx idx )
/******************************/
{
    queuenode           *node;
    asm_sym             *sym;

    for( node = LnameQueue.head; node != NULL; node = node->next ) {
        sym = (asm_sym *)node->data;
        if( sym->state != SYM_CLASS_LNAME )
            continue;
        if( sym->idx == idx ) {
            return( sym->name );
        }
    }
    return( NULL );
}

// called by OMF output format

void GetLnameData( void **data, asm_sym **psym )
/*********************************************/
{
    queuenode       *curr = *data;

    *psym = NULL;
    if ( curr == NULL ) {
        curr = LnameQueue.head;
    } else {
        curr = curr->next;
    }
    if ( curr )
        *psym = (asm_sym *)(curr->data);
    *data = curr;
    return;
}

static void FreeLnameQueue( void )
/********************************/
{
    struct asm_sym *sym;
    queuenode *node;

    while( LnameQueue.head != NULL ) {
        node = QDequeue( &LnameQueue );
        sym = (asm_sym *)node->data;
        if( sym->state == SYM_CLASS_LNAME ) {
            SymFree( sym );
        }
        AsmFree( node );
    }
    LnameQueue.head = NULL;
}

// Global Queue is used to store EXTERNDEFs

void AddGlobalData( dir_node *dir )
/*********************************/
{
    QAddItem( &GlobalQueue, dir );
}

/* EXTERNDEFs which have been defined in the module must be made
 * PUBLIC. This code runs after pass 1.
 */
void GetGlobalData( void )
/************************/
{
    queuenode           *curr;
    struct asm_sym      *sym;

    DebugMsg(("GetGlobalData enter, GlobalQueue=%X\n", GlobalQueue));
    while ( curr = (queuenode *)QDequeue( &GlobalQueue )) {
        sym = (asm_sym *)curr->data;
        DebugMsg(("GetGlobalData: %s state=%u used=%u public=%u\n", sym->name, sym->state, sym->used, sym->public ));
        if( sym->state == SYM_INTERNAL &&
           sym->isproc == FALSE && /* ignore PROCs! Masm does also */
           sym->public == FALSE ) {
            /* add it to the public queue */
            sym->public = TRUE;
            QEnqueue( &PubQueue, curr );
            continue; /* don't free this item! */
        }
        AsmFree( curr );
    }
    GlobalQueue.head = NULL;
}

void QueueInit( void )
/********************/
{
    QInit( &LnameQueue );
    QInit( &PubQueue );
    QInit( &GlobalQueue );
}

/* called once per module */

void QueueFini( void )
/********************/
{
    FreePubQueue();
    FreeLnameQueue();
}
