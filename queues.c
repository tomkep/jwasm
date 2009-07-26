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
#include "myassert.h"

#define COFF_LINNUM 0

typedef struct queuenode {
    void *next;
    void *data;
} queuenode;

static qdesc   *LnameQueue;  // queue of LNAME structs
static qdesc   *PubQueue;    // queue of pubdefs
static qdesc   *GlobalQueue; // queue of global / externdefs
static qdesc   *LinnumQueue; // queue of linnum_data structs

// add an existing node to a queue
// used to add - former - EXTERNDEF items to the PubQueue

static void QAddQItem( qdesc **queue, queuenode *node )
/*****************************************************/
{
    if( *queue == NULL ) {
        *queue = AsmAlloc( sizeof( qdesc ) );
        QInit( *queue );
    }
    QEnqueue( *queue, node );
}

// add a new node to a queue

static void QAddItem( qdesc **queue, void *data )
/***********************************************/
{
    struct queuenode    *node;

    node = AsmAlloc( sizeof( queuenode ) );
    node->data = data;
    if( *queue == NULL ) {
        *queue = AsmAlloc( sizeof( qdesc ) );
        QInit( *queue );
    }
    QEnqueue( *queue, node );
}

long GetQueueItems( void *q )
/****************************/
/* count the # of entries in the queue, if the retval is -ve we have an error */
{
    long        count = 0;
    queuenode   *node;

    if( q == NULL )
        return( 0 );
    for( node = ((qdesc *)q)->head; node != NULL; node = node->next ) {
        if( ++count < 0 ) {
            return( -1 );
        }
    }
    return( count );
}

void AddPublicData( asm_sym *sym )
/*********************************/
{
    QAddItem( &PubQueue, sym );
}

// get (next) PUBLIC item

dir_node * GetPublicData( queuenode * *curr)
/******************************************/
{
    if (PubQueue == NULL)
        return( NULL );

    if (*curr == NULL)
        *curr = PubQueue->head;
    else
        *curr = (*curr)->next;

    for ( ; *curr ; *curr = (*curr)->next ) {
        asm_sym *sym = (*curr)->data;
        if( sym->state == SYM_PROC ) {
            /* skip PROTOs without matching PROC */
            if( sym->isproc == FALSE ) {
                continue;
            }
        } else if ( sym->state == SYM_EXTERNAL ) {
            /* silently skip EXTERNDEFs (and EXTERNs???) */
            //if (sym->weak == TRUE)
                continue;
        }
        if( sym->state != SYM_INTERNAL && sym->state != SYM_PROC ) {
            // v1.95: make a full second pass and emit error on PUBLIC
            //AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
#if FASTPASS
            SkipSavedState();
#endif
            continue;
        }
        return((dir_node *)sym);
    }
    return( NULL );
}

static void FreePubQueue( void )
/******************************/
{
    if( PubQueue != NULL ) {
        while( PubQueue->head != NULL ) {
#if FASTMEM /* for FASTMEM, AsmFree() will vanish, so avoid unused p var */
            QDequeue( PubQueue );
#else
            void *p = QDequeue( PubQueue );
            AsmFree( p );
#endif
        }
        AsmFree( PubQueue );
        PubQueue = NULL;
    }
}

/* what's inserted into the LNAMES queue:
 * SYM_SEG: segment names
 * SYM_GRP: group names
 * SYM_CLASS_LNAME : class names
*/

void AddLnameData( dir_node *dir )
/********************************/
{
    QAddItem( &LnameQueue, dir );
}

// find a class index

direct_idx FindLnameIdx( char *name )
/***********************************/
{
    queuenode           *node;
    dir_node            *dir;

    if( LnameQueue == NULL )
        return( LNAME_NULL);

    for( node = LnameQueue->head; node != NULL; node = node->next ) {
        dir = (dir_node *)node->data;
        if( dir->sym.state != SYM_CLASS_LNAME )
            continue;
        if( _stricmp( dir->sym.name, name ) == 0 ) {
            return( dir->sym.idx );
        }
    }
    return( LNAME_NULL );
}

char *GetLname( direct_idx idx )
/******************************/
{
    queuenode           *node;
    dir_node            *dir;

    if( LnameQueue == NULL )
        return( NULL);

    for( node = LnameQueue->head; node != NULL; node = node->next ) {
        dir = (dir_node *)node->data;
        if( dir->sym.state != SYM_CLASS_LNAME )
            continue;
        if( dir->sym.idx == idx ) {
            return( dir->sym.name );
        }
    }
    return( NULL );
}

// called by OMF output format
// the first Lname entry is a null string!

unsigned GetLnameData( char **data )
/**********************************/
{
    char            *lname = NULL;
    unsigned        total_size;
    queuenode       *curr;
    dir_node        *dir;
    int             len;

    total_size = 1;
    if( LnameQueue )
        for( curr = LnameQueue->head; curr != NULL ; curr = curr->next ) {
            dir = (dir_node *)(curr->data);
            /**/myassert( dir != NULL );
            total_size += 1 + strlen( dir->sym.name );
        }

    lname = AsmAlloc( total_size * sizeof( char ) + 1 );
    /* add the NULL entry */
    *lname = NULLC;

    if( LnameQueue ) {
        int     i = 1;
        for( curr = LnameQueue->head; curr != NULL ; curr = curr->next ) {
            dir = (dir_node *)(curr->data);

            len = strlen( dir->sym.name );
            lname[i] = (char)len;
            i++;
            strcpy( lname+i, dir->sym.name );
            //if ( ModuleInfo.convert_uppercase )
            /* lnames are converted for casemaps ALL and NOTPUBLIC */
            if ( ModuleInfo.case_sensitive == FALSE )
                _strupr( lname+i );
            i += len; // overwrite the null char
        }
    }
    *data = lname;
    return( total_size );
}

static void FreeLnameQueue( void )
/********************************/
{
    struct asm_sym *sym;
    queuenode *node;

    if( LnameQueue != NULL ) {
        while( LnameQueue->head != NULL ) {
            node = QDequeue( LnameQueue );
            sym = (asm_sym *)node->data;
            if( sym->state == SYM_CLASS_LNAME ) {
                SymFree( sym );
            }
            AsmFree( node );
        }
        AsmFree( LnameQueue );
        LnameQueue = NULL;
    }
}

// Global Queue is used by EXTERNDEF

void AddGlobalData( dir_node *dir )
/*********************************/
{
    QAddItem( &GlobalQueue, dir );
}

void GetGlobalData( void )
/************************/
/* turn the EXTERNDEFs into either externs or publics as appropriate */
/* this runs just once, after pass 1 */
{
    queuenode           *curr;
    struct asm_sym      *sym;

    if( GlobalQueue == NULL )
        return;
    DebugMsg(("GetGlobalData enter, GlobalQueue=%X\n", GlobalQueue));
    while ( curr = (queuenode *)QDequeue( GlobalQueue )) {
        sym = (asm_sym *)curr->data;
        DebugMsg(("GetGlobalData: %s state=%u used=%u public=%u\n", sym->name, sym->state, sym->used, sym->public ));
        if( sym->state == SYM_EXTERNAL ) {
            if( sym->used == TRUE)
                sym->weak = FALSE;
        } else if ( sym->state != SYM_PROC && /* ignore PROCs! Masm does also */
                    sym->public == FALSE ) {
            /* make this record a pubdef */
            sym->public = TRUE;
            QAddQItem( &PubQueue, curr );
            continue; /* don't free this item! */
        }
        AsmFree( curr );
    }
    AsmFree( GlobalQueue );
    GlobalQueue = NULL;
}

void AddLinnumData( struct line_num_info *data )
/**********************************************/
{
    /* if output format is OMF, there's just a global
     queue of line number data. For other formats, the
     queue is stored in the section */
    if (Options.output_format == OFORMAT_OMF)
        QAddItem( &LinnumQueue, data );
    else {
#if COFF_LINNUM
        /* this isn't fully implemented yet */
        dir_node *seg = CurrSeg;
        if (seg) {
            /* COFF line numbers must be preceded by a function symbol table
               index.  */
            if (seg->e.seginfo->LinnumQueue == NULL && data->number != 0) {
                static line_num_info dummy;
                dummy.number = 0;
                if (CurrProc == NULL) {
                    dummy.sym = SymLookupLabel( "$$$00001", FALSE );
                    if (dummy.sym) {
                        SetSymSegOfs( dummy.sym );
                        dummy.sym->state = SYM_INTERNAL;
                        dummy.sym->defined = TRUE;
                    }
                } else
                    dummy.sym = (asm_sym *)CurrProc;
                DebugMsg(("addlinnumdata: &data=%X\n", &dummy));
                QAddItem( (qdesc **)&seg->e.seginfo->LinnumQueue, &dummy );
            }
            DebugMsg(("addlinnumdata: &data=%X\n", data));
            QAddItem( (qdesc **)&seg->e.seginfo->LinnumQueue, data );
        }
#endif
    }
}

// get line numbers

line_num_info * GetLinnumData2( qdesc *curr)
{
    queuenode  *node;
    if (curr && ( node = QDequeue( curr )))
        return( node->data );
    return(NULL);
}

int GetLinnumData( struct linnum_data **ldata, bool *need32 )
/***********************************************************/
{
    queuenode               *node;
    struct line_num_info    *next;
    int                     count, i;

    count = GetQueueItems( LinnumQueue );
    if( count == 0 )
        return( count );
    *need32 = FALSE;
    *ldata = AsmAlloc( count * sizeof( struct linnum_data ) );
    for( i = 0; i < count; i++ ) {
        node = QDequeue( LinnumQueue );
        next = (struct line_num_info *)(node->data);
        if( *ldata != NULL ) {
            (*ldata)[i].number = next->number;
            (*ldata)[i].offset = next->offset;
            if( next->offset > 0xffffUL ) {
                *need32 = TRUE;
            }
        }
        AsmFree( next );
        AsmFree( node );
    }
    AsmFree( LinnumQueue );
    LinnumQueue = NULL;
    return( count );
}

void QueueInit( void )
/************************/
{
    LnameQueue = NULL;
    PubQueue   = NULL;
    GlobalQueue= NULL;
    LinnumQueue= NULL;
}
void QueueFini( void )
/************************/
{
    FreePubQueue();
    FreeLnameQueue();
}
