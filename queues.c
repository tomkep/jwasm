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
#include "mangle.h"
#include "parser.h"
#include "directiv.h"
#include "queue.h"
#include "queues.h"
#include "fastpass.h"

#include "myassert.h"

#define COFF_LINNUM 0

typedef struct queuenode {
    void *next;
    void *data;
} queuenode;

#if FASTPASS
extern bool UseSavedState;
#endif

static qdesc   *LnameQueue;  // queue of LNAME structs
static qdesc   *PubQueue;    // queue of pubdefs
static qdesc   *GlobalQueue; // queue of global / externdefs
static qdesc   *LinnumQueue; // queue of linnum_data structs

//

static void QAddQItem( qdesc **queue, queuenode *node )
/*****************************************************/
{
    if( *queue == NULL ) {
        *queue = AsmAlloc( sizeof( qdesc ) );
        QInit( *queue );
    }
    QEnqueue( *queue, node );
}

// add a node to a queue

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

void AddPublicData( dir_node *dir )
/*********************************/
{
    QAddItem( &PubQueue, dir );
}

// gather names for publics
// they are written by the caller (write.c)
// PUBDEF are true publics, LPUBDEF (CMD_STATIC_PUBLIC) are "static"

dir_node * GetPublicData2( queuenode * *curr)
{
    if (PubQueue == NULL)
        return(NULL);
    if (*curr == NULL)
        *curr = PubQueue->head;
    else
        *curr = (*curr)->next;

    for (;*curr;*curr = (*curr)->next) {
        asm_sym *sym = (*curr)->data;
        if( sym->state == SYM_UNDEFINED ) {
#if FASTPASS
            /* do not use saved state, scan full source in second pass */
            ResetUseSavedState();
#endif
            // AsmErr( SYMBOL_NOT_DEFINED, sym->name );
            continue;
        } else if( sym->state == SYM_PROC ) {
            /* skip PROTOs without matching PROC */
            if(((dir_node *)sym)->e.procinfo->defined == FALSE) {
                continue;
            }
        } else if (sym->state == SYM_EXTERNAL) {
            /* skip EXTERNDEFs which aren't used */
            if (sym->weak == TRUE)
                continue;
        }
        if( sym->state != SYM_INTERNAL && sym->state != SYM_PROC) {
            AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
            continue;
        }
        return((dir_node *)sym);
    }
    return(NULL);
}

uint GetPublicData(
    uint *seg,
    uint *grp,
    char *cmd,
    char ***NameArray,
    struct pubdef_data **data,
    bool *need32,
    bool first )
/****************************/
{
    static struct queuenode    *start;
    struct queuenode           *curr;
    struct asm_sym             *sym;
    dir_node                   *pub;
    uint                       count;
    uint                       i;
    struct pubdef_data         *d;
    struct asm_sym             *curr_seg;
    bool bReset = TRUE;

    if( PubQueue == NULL )
        return( 0 );
    if( first )
        start = PubQueue->head;
    if( start == NULL )
        return( 0 );

// the first loop just calcs the number of entries (count)

    *need32 = FALSE;
    *cmd = CMD_PUBDEF;
    curr = start;
    for( count = 0; curr != NULL; curr = curr->next ) {
        sym = (asm_sym *)curr->data;
        DebugMsg(("GetPublicData: %s, lang=%u\n", sym->name, sym->langtype));
        if( sym->state == SYM_UNDEFINED ) {
            /* don't display an error here, it's confusing since it will
             be associated with the END directive. In the next pass, the error
             will be displayed when the PUBLIC directive is handled
             */
            // AsmErr( SYMBOL_NOT_DEFINED, sym->name );
#if FASTPASS
            /* do not use saved state, scan full source in second pass */
            ResetUseSavedState();
#endif
            continue;
        }
        if( sym->state == SYM_PROC ) {
            /* skip PROTOs without matching PROC */
            if(((dir_node *)sym)->e.procinfo->defined == FALSE) {
                DebugMsg(("GetPublicData: %s skipped\n", sym->name));
                continue;
            }
        } else if (sym->state == SYM_EXTERNAL) {
            /* skip EXTERNDEFs which aren't used */
            if (sym->weak == TRUE)
                continue;
        }
        if( sym->state != SYM_INTERNAL && sym->state != SYM_PROC) {
            AsmErr( CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name );
            continue;
        }

        if( count == MAX_PUB_SIZE )  // don't let the records get too big
            break;
        if (bReset) {
            bReset = FALSE;
            start = curr;
            curr_seg = sym->segment;
            if( curr_seg == NULL ) {      // absolute symbol ( without segment )
                *seg = 0;
                *grp = 0;
            } else {
                *seg = GetSegIdx( curr_seg );
                *grp = GetGrpIdx( GetGrp( curr_seg ) );
            }
        }
        if( sym->segment != curr_seg )
            break;
        DebugMsg(("GetPublicData: %s ok\n", sym->name));
        if( sym->state == SYM_PROC ) {
            if( !sym->public ) {
                if( *cmd == CMD_PUBDEF ) {
                    if( curr != start ) {
                        DebugMsg(("GetPublicData: break curr != start\n"));
                        break;
                    }
//                    *cmd = CMD_STATIC_PUBDEF;
                }
            } else {
                if( *cmd == CMD_STATIC_PUBDEF ) {
                    DebugMsg(("GetPublicData: break *cmd == CMD_STATIC_PUBDEF\n"));
                    break;
                }
            }
        }
        count++;
        /* if we don't get to here, this entry is part of the next pubdef */
    }
    DebugMsg(("GetPublicData: count=%u\n", count));
    if (!count)
        return (0);

    *NameArray = AsmAlloc( count * sizeof( char * ) );
    for( i = 0; i < count; i++ ) {
        (*NameArray)[i] = NULL;
    }

    *data = d = AsmAlloc( count * sizeof( struct pubdef_data ) );

    for( curr = start, i = 0; i < count; curr = curr->next ) {
        sym = (asm_sym *)curr->data;

        if ( sym->state == SYM_UNDEFINED )
            continue;
        else if( sym->state == SYM_PROC ) {
            /* skip PROTOs without matching PROC */
            if(((dir_node *)sym)->e.procinfo->defined == FALSE)
                continue;
        } else if ( sym->state == SYM_EXTERNAL )
            /* skip EXTERNDEFs which aren't used */
            if (sym->weak == TRUE)
                continue;

        if( sym->segment != curr_seg )
            break;

        if( sym->offset > 0xffffUL )
            *need32 = TRUE;

        (*NameArray)[i] = Mangle( sym, NULL );

        DebugMsg(("GetPublicData 2. loop: %s, mangled=%s\n", sym->name, (*NameArray)[i]));

        d[i].name = i;
        d[i].offset = sym->offset;
        d[i].type.idx = 0;
        i++;
    }
    start = curr;
    return( count );
}

static void FreePubQueue( void )
/******************************/
{
    if( PubQueue != NULL ) {
        while( PubQueue->head != NULL ) {
            AsmFree( QDequeue( PubQueue ) );
        }
        AsmFree( PubQueue );
        PubQueue = NULL;
    }
}

void AddLnameData( dir_node *dir )
/********************************/
{
    QAddItem( &LnameQueue, dir );
}

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
        if( stricmp( dir->sym.name, name ) == 0 ) {
            return( dir->e.lnameinfo->idx );
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
        if( dir->e.lnameinfo->idx == idx ) {
            return( dir->sym.name );
        }
    }
    return( NULL );
}

unsigned GetLnameData( char **data )
/**********************************/
{
    char            *lname = NULL;
    unsigned        total_size = 0;
    queuenode       *curr;
    dir_node        *dir;
    int             len;

    if( LnameQueue == NULL )
        return( 0 );

    for( curr = LnameQueue->head; curr != NULL ; curr = curr->next ) {
        dir = (dir_node *)(curr->data);
        myassert( dir != NULL );
        total_size += 1 + strlen( dir->sym.name );
    }

    if( total_size > 0 ) {
        int     i = 0;

        lname = AsmAlloc( total_size * sizeof( char ) + 1 );
        for( curr = LnameQueue->head; curr != NULL ; curr = curr->next ) {
            dir = (dir_node *)(curr->data);

            len = strlen( dir->sym.name );
            lname[i] = (char)len;
            i++;
            strcpy( lname+i, dir->sym.name );
            //For the Q folks... strupr( lname+i );
            i += len; // overwrite the null char
        }
    }
    *data = lname;
    return( total_size );
}

static void FreeLnameQueue( void )
/********************************/
{
    dir_node *dir;
    queuenode *node;

    if( LnameQueue != NULL ) {
        while( LnameQueue->head != NULL ) {
            node = QDequeue( LnameQueue );
            dir = (dir_node *)node->data;
            if( dir->sym.state == SYM_CLASS_LNAME ) {
                AsmFree( dir->e.lnameinfo );
                AsmFree( dir->sym.name );
                AsmFree( dir );
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
        DebugMsg(("GetGlobalData: %s, state=%u, lang=%u\n", sym->name, sym->state, sym->langtype));
        if( sym->state == SYM_EXTERNAL ) {
            // dir_change( (dir_node *)curr->data, TAB_EXT );
            if( sym->used == TRUE)
                sym->weak = FALSE;
            else {
                AsmFree( curr );
            }
        } else {
            if (sym->state == SYM_PROC)
                ;
            else if (sym->public == FALSE) {
                /* make this record a pubdef */
                sym->public = TRUE;
                QAddQItem( &PubQueue, curr );
            }
        }
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
        dir_node *seg = GetCurrSeg();
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
