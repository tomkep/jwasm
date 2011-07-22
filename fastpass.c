/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  implements the "fastpass" handling.
*               "fastpass" is an optimization which increases
*               assembly time speed by storing preprocessed lines
*               in memory during the first pass. In further passes,
*               those lines are "read" instead of the original assembly
*               source files.
*               Speed increase is significant if there's a large include
*               file at the top of an assembly source which contains
*               just equates and type definitions, because there's no need
*               to save such lines during pass one.
*
****************************************************************************/

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "input.h"
#include "segment.h"
#include "fastpass.h"

#if FASTPASS

struct mod_state modstate; /* struct to store assembly status */
static struct line_item *LineStoreHead;
static struct line_item *LineStoreTail;
struct line_item *LineStoreCurr;
bool StoreState;
bool UseSavedState;

/*
 * save the current status (happens in pass one only) and
 * switch to "save precompiled lines" mode.
 * the status is then restored in further passes,
 * and the precompiled lines are used for assembly then.
 */

static void SaveState( void )
/***************************/
{
    DebugMsg1(("SaveState enter\n" ));
    StoreState = TRUE;
    UseSavedState = TRUE;
    modstate.init = TRUE;
    modstate.EquHead = modstate.EquTail = NULL;

    memcpy( &modstate.modinfo, &ModuleInfo, sizeof( struct module_info ) );

    SegmentSaveState();
    AssumeSaveState();
    ContextSaveState(); /* save pushcontext/popcontext stack */

    DebugMsg(( "SaveState exit\n" ));
}

void StoreLine( char *string, uint_32 list_pos )
/**********************************************/
{
    int i;

    if ( GeneratedCode )
        return;
    if ( StoreState == FALSE )
        SaveState();

    i = strlen( string );
    LineStoreCurr = AsmAlloc( i + sizeof( struct line_item ) );
    DebugMsg1(("StoreLine(>%s<, listpos=%u): cur=%X\n", string, list_pos, LineStoreCurr ));
    LineStoreCurr->next = NULL;
    LineStoreCurr->lineno = LineNumber;
    if ( MacroLevel ) {
        LineStoreCurr->srcfile = 0xfff;
    } else {
        LineStoreCurr->srcfile = get_curr_srcfile();
    }
    LineStoreCurr->list_pos = list_pos;
    memcpy( LineStoreCurr->line, string, i + 1 );
#ifdef DEBUG_OUT
    if ( Options.print_linestore )
        printf("%s\n", LineStoreCurr->line );
#endif
    if ( LineStoreHead )
        LineStoreTail->next = LineStoreCurr;
    else
        LineStoreHead = LineStoreCurr;
    LineStoreTail = LineStoreCurr;
}

/* an error has been detected in pass one. it should be
 reported in pass 2, so ensure that a full source scan is done then
 */

void SkipSavedState( void )
/*************************/
{
    DebugMsg(("SkipSavedState enter\n"));
    UseSavedState = FALSE;
}

struct line_item *RestoreState( void )
/************************************/
{
    DebugMsg1(("RestoreState enter\n"));
    if ( modstate.init ) {
        struct equ_item *curr;
        /* restore values of assembly time variables */
        for ( curr = modstate.EquHead; curr; curr = curr->next ) {
            DebugMsg1(("RestoreState: sym >%s<, value=%Xh (sym_high=%X), defined=%u\n", curr->sym->name, curr->lvalue, curr->hvalue, curr->isdefined ));
            if ( curr->sym->mem_type == MT_ABS ) {
                curr->sym->value   = curr->lvalue;
                curr->sym->value3264 = curr->hvalue;
                curr->sym->isdefined = curr->isdefined;
            }
        }
        /* fields in struct "g" are not to be restored. */
        memcpy( &modstate.modinfo.g, &ModuleInfo.g, sizeof( ModuleInfo.g ) );
        memcpy( &ModuleInfo, &modstate.modinfo, sizeof( struct module_info ) );
        SetOfssize();
        SymSetCmpFunc();
    }

#if 0
    /* v2.05: AFAICS this can't happen anymore. */
    if ( LineStoreHead == NULL ) {
        struct line_item *endl = AsmAlloc( sizeof( struct line_item ) + 3 );
        endl->next = NULL;
        endl->srcfile = 0;
        endl->lineno = LineNumber;
        endl->list_pos = 0;
        strcpy( endl->line, "END");
        LineStoreHead = endl;
        DebugMsg(("RestoreState: LineStoreHead was NULL !!!\n" ));
    }
#endif
    return( LineStoreHead );
}

void FastpassInit( void )
/***********************/
{
    StoreState = FALSE;
    modstate.init = FALSE;
    LineStoreHead = NULL;
    LineStoreTail = NULL;
    UseSavedState = FALSE;
}

#endif
