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
* Description:  symbol table access
*
****************************************************************************/


#include "globals.h"
#include "symbols.h"
#include "memalloc.h"
#include "banner.h"
#include "directiv.h"
#include "queues.h"
#include "fixup.h"
#include "token.h"
#include "fastpass.h"
#include "myassert.h"
#include "macro.h"
#include "types.h"
#include "proc.h"

// reorganize the symbol table permanently, so the last found item
// is always first in a line. Gives no significant speed boost.
#define DYNREO 0

/* set size of hash table for symbol table searches. This affects
  assembly speed.
  */

//#define HASH_TABLE_SIZE 211
#define HASH_TABLE_SIZE 2003
//#define HASH_TABLE_SIZE 8009

// static struct asm_sym   *sym_table[ HASH_TABLE_SIZE ] = { NULL };
static struct asm_sym   *sym_table[ HASH_TABLE_SIZE ];

/* initialize the whole table to null pointers */

static unsigned         SymCount;    /* Number of symbols in table */

static char             szDate[12];
static char             szTime[12];

struct asm_sym symPC;

StrCmpFunc SymCmpFunc = stricmp;

/* use the same hash fcn */
static unsigned int hashpjw( const char *s )
/******************************************/
{
    unsigned h;
    unsigned g;

#if 1
    for( h = 0; *s; ++s ) {
        /* ( h & ~0x0fff ) == 0 is always true here */
        h = (h << 4) + (*s | ' ');
        g = h & ~0x0fff;
        h ^= g;
        h ^= g >> 12;
    }
#else
    for( h = 0; *s; ++s ) {
        h = (h << 6) + (*s & 0x3F);
        g = h & ~0xffffff;
        h ^= g;
        h ^= g >> 24;
    }
#endif
    return( h % HASH_TABLE_SIZE );
}

void SymSetCmpFunc(bool nocasemap)
{
    if (nocasemap == TRUE)
        SymCmpFunc = strcmp;
    else
        SymCmpFunc = stricmp;
    return;
}

static struct asm_sym *SymAlloc( const char *name )
/**************************************************/
{
    struct asm_sym      *sym;

    sym = AsmAlloc( sizeof( dir_node ) );
    //sym->next = NULL;
    memset(sym, 0, sizeof( dir_node ) );
    sym->name_size = strlen( name );
    sym->name = AsmAlloc( sym->name_size + 1 );
    strcpy( sym->name, name );

#if 0
    sym->segment = NULL;

    sym->offset = 0;
    sym->first_size = 0; /* also clears use32, comm, weak, isfar, isproc */
    sym->first_length = 0;
    sym->total_size = 0;
    sym->total_length = 0;
    sym->mangler = NULL;

    sym->used = FALSE;
    sym->defined = FALSE;
    sym->local = FALSE;
    sym->global = FALSE;
    sym->equate = FALSE;
    sym->predefined = FALSE;
    sym->variable = FALSE;
    sym->public = FALSE;
#endif
    sym->list = ModuleInfo.cref;
#if 0
    sym->included = FALSE; /* for COFF: is symbol in COFF symbol table? */
//    sym->saved = FALSE;

    sym->langtype = LANG_NONE;
    sym->state = SYM_UNDEFINED;
    sym->mem_type = MT_EMPTY;
    sym->type = NULL;
    sym->fixup = NULL;

    ((dir_node *)sym)->e.seginfo = NULL;
    ((dir_node *)sym)->line_num = 0;
    ((dir_node *)sym)->next = NULL;
    ((dir_node *)sym)->prev = NULL;
#endif
    return sym;
}

static struct asm_sym **SymFind( const char *name )
/*************************************************/
/* find a symbol in the symbol table, return NULL if not found */
{
    int i;
    int len;
    struct asm_sym      **sym;
    static struct asm_sym  *sym2;

    len = strlen(name);

    if (CurrProc) {
        label_list *paracurr;
        for (paracurr = CurrProc->e.procinfo->paralist;paracurr;paracurr = paracurr->next)
            if (len == paracurr->sym->name_size && SymCmpFunc( name, paracurr->sym->name) == 0 ) {
                DebugMsg(("SymFind: '%s' found in proc's param namespace\n", name));
                return( &paracurr->sym );
            }
        for (paracurr = CurrProc->e.procinfo->locallist;paracurr;paracurr = paracurr->next)
            if (len == paracurr->sym->name_size && SymCmpFunc( name, paracurr->sym->name ) == 0 ) {
                DebugMsg(("SymFind: '%s' found in proc's local namespace\n", name));
                return( &paracurr->sym );
            }
        for (sym2 = CurrProc->e.procinfo->labellist;sym2;sym2 = sym2->next)
            if (len == sym2->name_size && SymCmpFunc( name, sym2->name ) == 0 ) {
                DebugMsg(("SymFind: '%s' found in proc's label namespace\n", name));
                sym = &sym2;
                return( sym );
            }
    }

#if defined( _STANDALONE_ )
    i =hashpjw( name );
    sym = &sym_table[ i ];
#else
    sym = &AsmSymHead;
#endif
    for( ; *sym; sym = &((*sym)->next) ) {
        if (len == (*sym)->name_size && SymCmpFunc( name, (*sym)->name ) == 0 ) {
#if DYNREO
            if (sym != &sym_table [i]) {
                sym2 = *sym;
                *sym = sym2->next;
                sym2->next = sym_table [i];
                sym_table[ i ] = sym2;
            }
            DebugMsg(("SymFind: '%s' found\n", name));
            return( &sym_table [i] );
#else
            DebugMsg(("SymFind: '%s' found\n", name));
            return( sym );
#endif
        }
    }

    return( sym );
}

static void SetCurrPC( asm_sym *sym )
{
    if( StructDef.struct_depth != 0 ) {
        sym->segment = NULL;
        sym->offset = StructDef.curr_struct->sym.offset;
    } else
        SetSymSegOfs( sym );
}

struct asm_sym *SymLookup( const char *name )
/*******************************************/
{
    struct asm_sym      **sym_ptr;
    struct asm_sym      *sym;

    sym_ptr = SymFind( name );
    sym = *sym_ptr;
    if( sym == NULL ) {
        if( strlen( name ) > MAX_ID_LEN ) {
            AsmError( LABEL_TOO_LONG );
            return NULL;
        }
        sym = SymAlloc( name );
        DebugMsg(("SymLookup: created new symbol: '%s'\n", name));
        sym->next = *sym_ptr;
        *sym_ptr = sym;
        ++SymCount;
    }

    if( sym == &symPC )
        SetCurrPC( sym );

    DebugMsg(("SymLookup: symbol %s found, state=%u, defined=%u\n", name, sym->state, sym->defined));

    return( sym );
}

// this function also creates a label if it isn't defined yet,
// but the label is preferably created in the local namespace
// of the current procedure if bLocal==TRUE.

struct asm_sym *SymLookupLabel( const char *name, int bLocal)
/*******************************************/
{
    struct asm_sym      **sym_ptr;
    struct asm_sym      *sym;

    sym_ptr = SymFind( name );
    sym = *sym_ptr;
    if (sym == NULL) {
        if( strlen( name ) > MAX_ID_LEN ) {
            AsmError( LABEL_TOO_LONG );
            return NULL;
        }
        sym = SymAlloc( name );
        if (CurrProc && bLocal && ModuleInfo.scoped) {
            sym->local = TRUE;
            for (sym_ptr = &CurrProc->e.procinfo->labellist;*sym_ptr;sym_ptr = &((*sym_ptr)->next))
                ;
            DebugMsg(("SymLookupLabel: local symbol created in %s: >%s<\n", CurrProc->sym.name, name));
        } else {
            DebugMsg(("SymLookupLabel: global symbol created: >%s<\n", name));
            ++SymCount;
        }
        sym->next = *sym_ptr;
        *sym_ptr = sym;
    } else if( sym->state == SYM_UNDEFINED &&
               sym->local == FALSE && CurrProc && bLocal && ModuleInfo.scoped) {
        /* move the label to the current proc's local label list
         if it isn't defined yet */
        *sym_ptr = sym->next;
        SymCount--;
        for (sym_ptr = &CurrProc->e.procinfo->labellist;*sym_ptr;sym_ptr = &((*sym_ptr)->next))
            ;
        sym->local = TRUE;
        sym->next = *sym_ptr;
        *sym_ptr = sym;
        DebugMsg(("SymLookupLabel: label moved into local namespace\n"));
    }
    if( sym == &symPC ) {
        SetCurrPC( sym );
    }
    DebugMsg(("SymLookupLabel: symbol %s found, state=%u, defined=%u\n", name, sym->state, sym->defined));
    return( sym );
}

static void FreeASym( struct asm_sym *sym )
/*****************************************/
{
#if defined( _STANDALONE_ )
    struct asmfixup     *curr;
    struct asmfixup     *next;

//    DebugMsg(("FreeASym: delete %s, state=%X\n", sym->name, sym->state));
    for(curr = sym->fixup ;curr; ) {
        next = curr->next1;
        AsmFree( curr );
        curr = next;
    }
#endif
    AsmFree( sym->name );
    AsmFree( sym );
}

#if defined( _STANDALONE_ )

#if 0

int SymChangeName( const char *old, const char *new )
/***************************************************/
{
    struct asm_sym      **sym_ptr;
    struct asm_sym      *sym;

    sym_ptr = SymFind( old );
    if( *sym_ptr != NULL ) {
        sym = *sym_ptr;
        *sym_ptr = sym->next;
        AsmFree( sym->name );
        sym->name_size = strlen( new );
        sym->name = AsmAlloc( sym->name_size + 1 );
        strcpy( sym->name, new );

        sym_ptr = SymFind( new );
        if( *sym_ptr != NULL )
            return( ERROR );

        sym->next = *sym_ptr;
        *sym_ptr = sym;
    }
    return( NOT_ERROR );
}

#endif

void SymTakeOut( const char *name )
/*********************************/
{
    struct asm_sym      *sym;
    struct asm_sym      **sym_ptr;

    DebugMsg(("SymTakeOut %s\n", name));
    sym_ptr = SymFind( name );
    if( *sym_ptr != NULL ) {
        /* found it -- so take it out */
        sym = *sym_ptr;
        *sym_ptr = sym->next;
        dir_free( (dir_node *)sym, FALSE );
        FreeASym( sym );
        --SymCount;
    }
    return;
}

// free a symbol directly without a try to find it first
// (it's not in global namespace)

void SymFree( struct asm_sym *sym)
/*********************************/
{
//    DebugMsg(("SymFree: free %X, name=%s, state=%X\n", sym, sym->name, sym->state));
    dir_free( (dir_node *)sym, FALSE );
    FreeASym( sym );
    return;
}

// set a symbol's name directly without a try to find it first -
// it's not in global namespace. The symbol's address may change,
// the changed symbol is returned.

struct asm_sym *SymSetName( struct asm_sym *sym, const char * name)
/*********************************/
{
    AsmFree( sym->name );
    sym->name_size = strlen( name );
    sym->name = AsmAlloc( sym->name_size + 1 );
    strcpy( sym->name, name );
    return( sym );
}

static struct asm_sym *SymAddToTable( struct asm_sym *sym )
/*****************************************************/
{
    struct asm_sym  **location;

    location = SymFind( sym->name );

    if( *location != NULL ) {
        /* we already have this symbol */
        AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
        return( *location );
    }

    sym->next = NULL;
    *location = sym;
    ++SymCount;
    return( sym );
}

struct asm_sym *SymCreate( const char *name, bool add_symbol )
/***********************************************************/
/* Create symbol and insert it into the symbol table */
{
    struct asm_sym      *new;

    new = SymAlloc( name );

    /* add it into the symbol table */
    if( add_symbol )
        return( SymAddToTable( new ) );

    return( new );
}

struct asm_sym *SymSearch( const char *name )
/**********************************************/
{
    struct asm_sym  **sym_ptr;

    sym_ptr = SymFind( name );
    if( *sym_ptr == &symPC )
        SetCurrPC( *sym_ptr );
    return( *sym_ptr );
}

#endif

void DumpSymbols( void );

void SymFini( void )
/*********************/
{
    struct asm_sym      *sym;
    dir_node            *dir;
    unsigned            i;

#if defined( DEBUG_OUT )
    DumpSymbols();
#endif

    /* free the symbol table */
#if FASTMEM==0
    for( i = 0; i < HASH_TABLE_SIZE; i++ ) {
        struct asm_sym  *next;
        next = sym_table[i];
        for( ;; ) {
            sym = next;
            if( sym == NULL )
                break;
            dir = (dir_node *)sym;
            next = sym->next;
            dir_free( dir, FALSE );
            FreeASym( sym );
            --SymCount;
        }
    }
    myassert( SymCount == 0 );
#else
#endif

}
void SymInit( )
/*********************/
{
    asm_sym * sym;

    SymCount = 0;

    memset(sym_table, 0, sizeof(sym_table));

    sym = SymCreate( "__JWASM__", TRUE );
    sym->state = SYM_INTERNAL;
    sym->mem_type = MT_ABS;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->offset = _JWASM_VERSION_INT_;

    /* @Version contains the Masm compatible version */
    sym = SymCreate( "@Version", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = "615";

    /* @Date and @Time */
    sym = SymCreate( "@Date", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = _strdate(szDate);
    sym = SymCreate( "@Time", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = _strtime(szTime);

    /* @FileName and @FileCur */
    sym = SymCreate( "@FileName", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = ModuleInfo.name;
    sym = SymCreate( "@FileCur", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = ModuleInfo.name;

    /* add $ symbol */
#if 0
    sym = SymCreate( "$", TRUE );
    sym->state = SYM_INTERNAL;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->mem_type = MT_NEAR;
    sym->list = FALSE; /* don't display the '$' symbol in symbol list */
#else
    symPC.name = "$";
    symPC.state = SYM_INTERNAL;
    symPC.defined = TRUE;
    symPC.predefined = TRUE;
    symPC.mem_type = MT_NEAR;
    symPC.name_size = 1;
    symPC.list = FALSE; /* don't display the '$' symbol in symbol list */
    SymAddToTable(&symPC);
#endif

    /* add @Line numeric equate */
    LineItem.mem_type = MT_ABS;
    LineItem.state = SYM_INTERNAL;
    LineItem.defined = TRUE;
    LineItem.predefined = TRUE;
    LineItem.variable = TRUE;
    LineItem.name_size = strlen(LineItem.name);
    SymAddToTable(&LineItem);

    /* add @WordSize numeric equate */
    WordSize.mem_type = MT_ABS;
    WordSize.state = SYM_INTERNAL;
    WordSize.defined = TRUE;
    WordSize.predefined = TRUE;
    WordSize.variable = TRUE;
    WordSize.name_size = strlen(WordSize.name);
    SymAddToTable(&WordSize);

    return;

}

void SymPassInit( int pass )
/*********************/
{
    dir_node            *dir;
    unsigned            i;

    if (pass == PASS_1)
        return;

#if FASTPASS
#else
    /* mark as "undefined":
     - text macros (SYM_TMACRO)
     - macros (SYM_MACRO)
     - internal labels (SYM_INTERNAL)
      */

    for( i = 0; i < HASH_TABLE_SIZE; i++ ) {
        for(dir = (dir_node *)sym_table[i];dir;dir = (dir_node *)dir->sym.next ) {
            if (dir->sym.predefined == FALSE) {
                if (dir->sym.state == SYM_TMACRO ||
                    dir->sym.state == SYM_MACRO  ||
                    dir->sym.state == SYM_INTERNAL) {
                    dir->sym.defined = FALSE;
                }
            }
        }
    }
#endif
}

#if defined( _STANDALONE_ )

static int compare_fn( const void *p1, const void *p2 )
/*****************************************************/
{
#ifdef __WATCOMC__
    struct asm_sym * const *sym1 = p1;
    struct asm_sym * const *sym2 = p2;
#else
    const asm_sym * *sym1 = p1;
    const asm_sym * *sym2 = p2;
#endif

    return( strcmp( (*sym1)->name, (*sym2)->name ) );
}

/* get a sorted list of all symbols */
/* possible speedup: filter the symbols with list=false */

struct asm_sym **SymSort( unsigned int *count )
/*****************************************/
{
    struct asm_sym      **syms;
    struct asm_sym      *sym;
    unsigned            i, j;

    *count = SymCount;
    syms = MemAlloc( SymCount * sizeof( struct asm_sym * ) );
    if( syms ) {
        /* copy symbols to table */
        for( i = j = 0; i < HASH_TABLE_SIZE; i++ ) {
            for(sym = sym_table[i]; sym; sym = sym->next) {
                syms[j++] = sym;
            }
        }
        /* sort 'em */
        qsort( syms, SymCount, sizeof( struct asm_sym * ), compare_fn );
    }
    return( syms );
}

#if defined( DEBUG_OUT )

#if 0
static void DumpSymbol( struct asm_sym *sym )
/*******************************************/
{
    dir_node    *dir;
    char        *type;
    char        value[512];
    const char  *langtype;
    char        *public;

    dir = (dir_node *)sym;
    *value = 0;
    switch( sym->state ) {
    case SYM_SEG:
        type = "SEGMENT";
        break;
    case SYM_GRP:
        type = "GROUP";
        break;
    case SYM_EXTERNAL:
        if (sym->comm)
            type = "COMMUNAL";
        else
            type = "EXTERNAL";
        break;
    case SYM_TMACRO:
        type = "TEXT";
        break;
    case SYM_PROC:
        if ( dir->sym.isproc )
            type = "PROCEDURE";
        else
            type = "PROTOTYPE";
        break;
    case SYM_MACRO:
        type = "MACRO";
        break;
    case SYM_TYPE:
        switch ( dir->e.structinfo->typekind ) {
        case TYPE_UNION:
            type = "UNION";
            break;
        case TYPE_TYPEDEF:
            type = "TYPEDEF";
            break;
        case TYPE_RECORD:
            type = "RECORD";
            break;
        default:
            type = "STRUCTURE";
        }
        break;
    case SYM_CLASS_LNAME:
        type = "CLASS";
        break;
    case SYM_STRUCT_FIELD:
        type = "STRUCTURE FIELD";
        break;
    case SYM_UNDEFINED:
        type = "UNDEFINED";
        break;
    case SYM_INTERNAL:
        if (dir->sym.mem_type == MT_ABS) {
            type = "NUMBER";
            sprintf(value, "%Xh", dir->sym.offset);
        } else
            type = "INTERNAL";
        break;
    default:
        type = "UNKNOWN";
        break;
    }
    if( sym->public ) {
        public = "PUBLIC ";
    } else {
        public = "";
    }
    langtype = get_sym_lang( sym );
    DoDebugMsg( "%-30s\t%s\t%s%s\t%8X\t%s\n", sym->name, type, public, langtype, sym->offset, value );
}
#endif

void DumpSymbols( void )
/*******************/
{
    struct asm_sym      *sym;
    unsigned            i;
    unsigned            count = 0;
    unsigned            max = 0;
    unsigned            curr = 0;
    unsigned            empty = 0;

    printf( "\n" );
    for( i = 0; i < HASH_TABLE_SIZE; i++ ) {
        struct asm_sym  *next;
        next = sym_table[i];
        if (next == NULL)
            empty++;
        for(curr = 0 ;; ) {
            sym = next;
            if( sym == NULL )
                break;
            count++;
            curr++;
            next = sym->next;
            //DumpSymbol( sym );
            //flushall(); // make sure everything is dumped out
        }
        if (max < curr)
            max = curr;
    }
    printf( "%u items in symbol table, expected %u\n", count, SymCount );
    printf( "%u items max in one line, %u lines empty\n", max, empty );
}
#endif

#endif
