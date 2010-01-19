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
#include "directiv.h"
#include "segment.h"
#include "queues.h"
#include "fixup.h"
#include "token.h"
#include "fastpass.h"
#include "myassert.h"
#include "macro.h"
#include "types.h"
#include "proc.h"
#include "input.h"

#if defined(__WATCOMC__) && !defined(__FLAT__)
#define HASH_MAGNITUDE 12  /* for 16bit model */
#else
#define HASH_MAGNITUDE 15  /* is 15 since v1.94, previously 12 */
#endif

/* size of global hash table for symbol table searches. This affects
 * assembly speed.
 */
#if HASH_MAGNITUDE==12
#define GHASH_TABLE_SIZE 2003
#else
#define GHASH_TABLE_SIZE 8009
#endif

/* size of local hash table */
#define LHASH_TABLE_SIZE 127

#define USEFIXSTRCMP 0 /* 1=don't use a function pointer for string compare */

#if USEFIXSTRCMP
//#define STRCMP( x, y, z ) ( ModuleInfo.case_sensitive ? strcmp( x, y ) : _stricmp( x, y ) )
#define STRCMP( x, y, z ) ( ModuleInfo.case_sensitive ? memcmp( x, y, z ) : _memicmp( x, y, z ) )
#else
#define STRCMP( x, y, z ) SymCmpFunc( x, y )
#endif

extern struct asm_sym FileCur;  /* @FileCur symbol */
extern struct asm_sym LineCur;  /* @Line symbol    */
extern uint_32 GetLineNumber( struct asm_sym * );

struct asm_sym *sym_CurSeg;  /* the @CurSeg symbol */
struct asm_sym symPC;        /* the '$' symbol     */

StrCmpFunc SymCmpFunc;

static struct asm_sym   **lsym;      /* pointer into local hash table */
static unsigned         SymCount;    /* Number of symbols in global table */
static char             szDate[12];  /* value of @Date symbol */
static char             szTime[12];  /* value of @Time symbol */
static struct asm_sym   *gsym_table[ GHASH_TABLE_SIZE ];
static struct asm_sym   *lsym_table[ LHASH_TABLE_SIZE ];

static unsigned int hashpjw( const char *s )
/******************************************/
{
    unsigned h;
    unsigned g;

#if HASH_MAGNITUDE==12
    for( h = 0; *s; ++s ) {
        h = (h << 4) + (*s | ' ');
        g = h & ~0x0fff;
        h ^= g;
        h ^= g >> 12;
    }
#else
    for( h = 0; *s; ++s ) {
        h = (h << 5) + (*s | ' ');
        g = h & ~0x7fff;
        h ^= g;
        h ^= g >> 15;
    }
#endif
    return( h );
}

void SymSetCmpFunc( void )
/************************/
{
    if ( ModuleInfo.case_sensitive == TRUE )
        SymCmpFunc = strcmp;
    else
        SymCmpFunc = _stricmp;
    return;
}

/* reset local hash table */

void SymClearLocal( void )
/************************/
{
    memset( &lsym_table, 0, sizeof( lsym_table ) );
    return;
}

/* store local hash table in proc's list of local symbols */

void SymGetLocal( asm_sym *proc )
/*******************************/
{
    int i;
    dir_node  **l = &((dir_node *)proc)->e.procinfo->labellist;

    for ( i = 0; i < LHASH_TABLE_SIZE; i++ ) {
        if ( lsym_table[i] ) {
            *l = (dir_node *)lsym_table[i];
            l = &(*l)->nextll;
        }
    }
    *l = NULL;

    return;
}

/* restore local hash table.
 * - proc: procedure which will become active
 */

void SymSetLocal( asm_sym *proc )
/*******************************/
{
    int i;
    dir_node  *l;

    SymClearLocal();
    for ( l = ((dir_node *)proc)->e.procinfo->labellist; l; l = l->nextll ) {
        DebugMsg(("SymSetLocal: label=%s\n", l->sym.name ));
        i = hashpjw( l->sym.name ) % LHASH_TABLE_SIZE;
        lsym_table[i] = &l->sym;
    }
    return;
}

static struct asm_sym *SymAlloc( const char *name )
/*************************************************/
{
    struct asm_sym *sym;

    sym = AsmAlloc( sizeof( dir_node ) );
    memset( sym, 0, sizeof( dir_node ) );
    sym->name_size = strlen( name );
    sym->list = ModuleInfo.cref;
    sym->mem_type = MT_EMPTY;
    sym->name = AsmAlloc( sym->name_size + 1 );
    memcpy( sym->name, name, sym->name_size + 1 );
    return( sym );
}

static struct asm_sym **SymFind( const char *name )
/*************************************************/
/* find a symbol in the local/global symbol table,
 * return ptr to next free entry in global table if not found.
 */
{
    int i;
    int len;
    struct asm_sym  **gsym;

    len = strlen(name);
    i = hashpjw( name );

    if (CurrProc) {
        for( lsym = &lsym_table[ i % LHASH_TABLE_SIZE ]; *lsym; lsym = &((*lsym)->next) ) {
            if ( len == (*lsym)->name_size && STRCMP( name, (*lsym)->name, len ) == 0 ) {
                DebugMsg(("SymFind: '%s' found in local table, state=%u, local=%u\n", name, (*lsym)->state, (*lsym)->scoped ));
                return( lsym );
            }
        }
    }

    for( gsym = &gsym_table[ i % GHASH_TABLE_SIZE ]; *gsym; gsym = &((*gsym)->next) ) {
        if ( len == (*gsym)->name_size && STRCMP( name, (*gsym)->name, len ) == 0 ) {
            DebugMsg(("SymFind: '%s' found, state=%u\n", name, (*gsym)->state ));
            return( gsym );
        }
    }

    return( gsym );
}

static void SymSetCurrPC( void )
/******************************/
{
    if( CurrStruct ) {
        //symPC.segment = NULL;
        symPC.mem_type = MT_ABS;
        symPC.offset = CurrStruct->sym.offset + (CurrStruct->next ? CurrStruct->next->sym.offset : 0);
    } else {
        symPC.mem_type = MT_NEAR;
        symPC.offset = GetCurrOffset();
    }
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
            return( NULL );
        }
        sym = SymAlloc( name );
        DebugMsg(("SymLookup: created new symbol: '%s', CurrProc=%s\n", name, CurrProc ? CurrProc->sym.name : "NULL" ));
        sym->next = *sym_ptr;
        *sym_ptr = sym;
        ++SymCount;
    }

    if( sym == &symPC )
        SymSetCurrPC();

    DebugMsg(("SymLookup: symbol %s found, state=%u, defined=%u\n", name, sym->state, sym->defined));

    return( sym );
}

// this function also creates a label if it isn't defined yet,
// but the label is preferably created in the local namespace
// of the current procedure if bLocal==TRUE.

struct asm_sym *SymLookupLabel( const char *name, int bLocal )
/************************************************************/
{
    struct asm_sym      **sym_ptr;
    struct asm_sym      *sym;

    sym_ptr = SymFind( name );
    sym = *sym_ptr;
    if (sym == NULL) {
        if( strlen( name ) > MAX_ID_LEN ) {
            AsmError( LABEL_TOO_LONG );
            return( NULL );
        }
        sym = SymAlloc( name );
        if ( CurrProc && bLocal && ModuleInfo.scoped ) {
            sym->scoped = TRUE;
            /* add the label to the local hash table */
            sym->next = *lsym;
            *lsym = sym;
            DebugMsg(("SymLookupLabel: local symbol created in %s: >%s<\n", CurrProc->sym.name, name));
        } else {
            DebugMsg(("SymLookupLabel: global symbol created: >%s<\n", name));
            ++SymCount;
            sym->next = *sym_ptr;
            *sym_ptr = sym;
        }
    } else if( sym->state == SYM_UNDEFINED &&
               sym->scoped == FALSE && CurrProc && bLocal && ModuleInfo.scoped) {
        sym->scoped = TRUE;
        /* remove the label from the global hash table */
        *sym_ptr = sym->next;
        SymCount--;
        /* add the label to the local hash table */
        sym->next = *lsym;
        *lsym = sym;
        DebugMsg(("SymLookupLabel: label %s moved into %s's local namespace\n", sym->name, CurrProc->sym.name ));
    }
    if( sym == &symPC )
        SymSetCurrPC();

    DebugMsg(("SymLookupLabel: symbol %s found, state=%u, defined=%u\n", name, sym->state, sym->defined));
    return( sym );
}

static void FreeASym( struct asm_sym *sym )
/*****************************************/
{
    struct asmfixup     *curr;
    struct asmfixup     *next;

//    DebugMsg(("FreeASym: delete %s, state=%X\n", sym->name, sym->state));
    for( curr = sym->fixup ; curr; ) {
        next = curr->nextbp;
        AsmFree( curr );
        curr = next;
    }
    AsmFree( sym->name );
    AsmFree( sym );
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

// set a symbol's name
// the previous name was "", the symbol wasn't in a symbol table.
// this function is called for symbols which are to be moved
// to the local namespace ( PROC parameters ).

void SymSetName( struct asm_sym *sym, const char * name )
/*******************************************************/
{
    struct asm_sym  **location;

    location = SymFind( name );

    if( *location != NULL ) { /* shouldn't happen */
        AsmErr( SYMBOL_ALREADY_DEFINED, name );
        return;
    }
    AsmFree( sym->name );
    sym->name_size = strlen( name );
    sym->name = AsmAlloc( sym->name_size + 1 );
    memcpy( sym->name, name, sym->name_size + 1 );
    sym->next = NULL;
    if ( CurrProc ) /* should always be set */
        *lsym = sym;
    else
        *location = sym;
    return;
}

/* add a symbol to the global symbol table */

static struct asm_sym *SymAddToTable( struct asm_sym *sym )
/*********************************************************/
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

struct asm_sym *SymCreate( const char *name, bool add_table )
/***********************************************************/
/* Create symbol and insert it into the symbol table */
{
    struct asm_sym      *new;

    new = SymAlloc( name );

    /* add it to the global symbol table */
    if( add_table )
        return( SymAddToTable( new ) );

    return( new );
}

struct asm_sym *SymLCreate( const char *name )
/********************************************/
/* Create symbol and insert it into the local symbol table */
{
    struct asm_sym  *sym;
    struct asm_sym  **location;

    location = SymFind( name );

    if( *location != NULL ) {
        /* we already have this symbol */
        AsmErr( SYMBOL_ALREADY_DEFINED, name );
        return( NULL );
    }
    sym = SymAlloc( name );
    sym->next = NULL;
    *lsym = sym;
    return( sym );
}

struct asm_sym *SymSearch( const char *name )
/*******************************************/
{
    struct asm_sym  **sym_ptr;

    sym_ptr = SymFind( name );

    if( *sym_ptr == &symPC )
        SymSetCurrPC();

    return( *sym_ptr );
}

struct asm_sym * SymIsType( const char *name )
/********************************************/
{
    struct asm_sym *sym;

    sym = SymSearch( name );
    if ( sym && (sym->state == SYM_TYPE ) )
        return( sym );
    return( NULL );
}

void SymMakeAllSymbolsPublic( void )
/**********************************/
{
    int i;
    struct asm_sym  *sym;

    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        for( sym = gsym_table[i]; sym; sym = sym->next ) {
            if ( sym->state == SYM_INTERNAL &&
                 sym->mem_type != MT_ABS &&  /* no symbolic constants */
                 sym->predefined == FALSE && /* no predefined symbols ($) */
                 sym->public == FALSE ) {
                sym->public = TRUE;
                AddPublicData( sym );
            }
        }
    }
}

#ifdef DEBUG_OUT
static void DumpSymbols( void );
#endif

void SymFini( void )
/******************/
{
#if FASTMEM==0
    unsigned i;
#endif

#if defined( DEBUG_OUT )
    DumpSymbols();
#endif

#if FASTMEM==0
    /* free the symbol table */
    /* there are some items which are located in static memory! */
    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        struct asm_sym  *sym;
        struct asm_sym  *next;
        sym = gsym_table[i];
        for( ; sym; ) {
            next = sym->next;
            dir_free( (dir_node *)sym, FALSE );
            if ( sym->staticmem == FALSE )
                FreeASym( sym );
            SymCount--;
            sym = next;
        }
    }
    /**/myassert( SymCount == 0 );
#endif

}

/* initialize global symbol table */

void SymInit( )
/*************/
{
    asm_sym * sym;
    time_t    time_of_day;
    struct tm *now;

    DebugMsg(("SymInit() enter\n"));
    SymCount = 0;

    memset( gsym_table, 0, sizeof(gsym_table) );

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
    time_of_day = time( NULL );
    now = localtime( &time_of_day );
    sym = SymCreate( "@Date", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    strftime( szDate, 9, "%D", now );
    sym->string_ptr = szDate;
    sym = SymCreate( "@Time", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    strftime( szTime, 9, "%T", now );
    sym->string_ptr = szTime;

    /* @FileName */
    sym = SymCreate( "@FileName", TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = ModuleInfo.name;

    /* @FileCur */
    //filecur = SymCreate( "@FileCur", TRUE );
    FileCur.state = SYM_TMACRO;
    FileCur.defined = TRUE;
    FileCur.predefined = TRUE;
#if FASTMEM==0
    FileCur.staticmem = TRUE;
#endif
    FileCur.mem_type = MT_EMPTY;
    FileCur.string_ptr = ModuleInfo.name;
    FileCur.name_size = 8; /* sizeof("@FileCur") */
    SymAddToTable( &FileCur );

    /* add $ symbol */
    symPC.name = "$";
    symPC.state = SYM_INTERNAL;
    symPC.defined = TRUE;
    symPC.predefined = TRUE;
    symPC.variable = TRUE; /* added v1.96. Important for fixup creation */
#if FASTMEM==0
    symPC.staticmem = TRUE;
#endif
    symPC.mem_type = MT_NEAR;
    symPC.name_size = 1;
    symPC.list = FALSE; /* don't display the '$' symbol in symbol list */
    SymAddToTable(&symPC);

    /* add @Line numeric equate */
    LineCur.sfunc_ptr = &GetLineNumber;
    LineCur.mem_type = MT_ABS;
    LineCur.state = SYM_INTERNAL;
    LineCur.defined = TRUE;
    LineCur.predefined = TRUE;
#if FASTMEM==0
    LineCur.staticmem = TRUE;
#endif
    LineCur.variable = TRUE; /* ??? */
    LineCur.name_size = strlen( LineCur.name ); /* sizeof @Line */
    SymAddToTable( &LineCur );

    /* add @WordSize numeric equate */
    WordSize.mem_type = MT_ABS;
    WordSize.state = SYM_INTERNAL;
    WordSize.defined = TRUE;
    WordSize.predefined = TRUE;
#if FASTMEM==0
    WordSize.staticmem = TRUE;
#endif
    WordSize.variable = TRUE; /* ??? */
    WordSize.name_size = strlen( WordSize.name );
    SymAddToTable( &WordSize );

    sym_CurSeg = SymCreate("@CurSeg", TRUE );
    sym_CurSeg->state = SYM_TMACRO;
    sym_CurSeg->defined = TRUE;
    sym_CurSeg->predefined = TRUE;

    DebugMsg(("SymInit() exit\n"));
    return;

}

void SymPassInit( int pass )
/**************************/
{
    unsigned            i;

    if (pass == PASS_1)
        return;

#if FASTPASS
    if ( UseSavedState )
        return;
#endif
    /* mark as "undefined":
     - text macros (SYM_TMACRO)
     - macros (SYM_MACRO)
     - internal labels (SYM_INTERNAL)
      */

    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        asm_sym *sym;
        for( sym = gsym_table[i]; sym; sym = sym->next ) {
            if ( sym->predefined == FALSE ) {
                if ( sym->state == SYM_TMACRO ||
                    sym->state == SYM_MACRO  ||
                    sym->state == SYM_INTERNAL) {
                    sym->defined = FALSE;
                }
            }
        }
    }
}

static int compare_fn( const void *p1, const void *p2 )
/*****************************************************/
{
#if defined(__WATCOMC__) || defined(__GNUC__) || defined(__POCC__) || defined(__DMC__)
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
/*********************************************/
{
    struct asm_sym      **syms;
    struct asm_sym      *sym;
    unsigned            i, j;

    *count = SymCount;
    syms = MemAlloc( SymCount * sizeof( struct asm_sym * ) );
    if( syms ) {
        /* copy symbols to table */
        for( i = j = 0; i < GHASH_TABLE_SIZE; i++ ) {
            for(sym = gsym_table[i]; sym; sym = sym->next) {
                syms[j++] = sym;
            }
        }
        /* sort 'em */
        qsort( syms, SymCount, sizeof( struct asm_sym * ), compare_fn );
    }
    return( syms );
}

// enum all symbols in global hash table

int SymEnum( struct asm_sym * *psym, int *pi )
/********************************************/
{
    unsigned            i;
    struct asm_sym      *sym;

    if ( *psym == NULL ) {
        i = 0;
        sym = gsym_table[i];
    } else {
        i = *pi;
        sym = *psym;
        sym = sym->next;
    }

    for( ; sym == NULL && i < GHASH_TABLE_SIZE; i++ )
        sym = gsym_table[i];
    *psym = sym;
    *pi = i;
    return( sym != NULL );
}

#if defined( DEBUG_OUT )

#ifdef TRMEM
static void DumpSymbol( struct asm_sym *sym )
/*******************************************/
{
    dir_node    *dir;
    char        *type;
    //const char  *langtype;
    char        *visibility;

    dir = (dir_node *)sym;

    switch( sym->state ) {
    case SYM_SEG:
        type = "SEGMENT";
        break;
    case SYM_GRP:
        type = "GROUP";
        break;
    case SYM_EXTERNAL:
        if ( dir->sym.isproc )
            type = "PROTO";
        else if ( sym->comm )
            type = "COMMUNAL";
        else
            type = "EXTERNAL";
        break;
    case SYM_TMACRO:
        type = "TEXT";
        break;
    case SYM_MACRO:
        type = "MACRO";
        break;
    case SYM_TYPE:
        switch ( dir->e.structinfo->typekind ) {
        case TYPE_UNION:   type = "UNION";     break;
        case TYPE_TYPEDEF: type = "TYPEDEF";   break;
        case TYPE_RECORD:  type = "RECORD";    break;
        default:           type = "STRUCTURE"; break;
        }
        break;
    //case SYM_CLASS_LNAME:  /* this type is never stored in the symbol table */
    //    type = "CLASS";
    //    break;
    case SYM_STRUCT_FIELD:
        type = "STRUCT FIELD";
        break;
    case SYM_UNDEFINED:
        type = "UNDEFINED";
        break;
    case SYM_INTERNAL:
        if ( dir->sym.isproc )
            type = "PROCEDURE";
        else if ( dir->sym.mem_type == MT_ABS )
            type = "NUMBER";
        else
            type = "INTERNAL";
        break;
    default:
        type = "UNKNOWN";
        break;
    }
    if( sym->public ) {
        visibility = "PUBLIC ";
    } else {
        visibility = "";
    }
    DebugMsg(( "%8X: %-30s %-12s  %8X  %8X %8X %s\n", sym, sym->name, type, sym->offset, dir->e, sym->name, visibility ));
}
#endif

static void DumpSymbols( void )
/*****************************/
{
    struct asm_sym      *sym;
    unsigned            i;
    unsigned            count = 0;
    unsigned            max = 0;
    unsigned            num0 = 0;
    unsigned            num1 = 0;
    unsigned            num5 = 0;
    unsigned            num10 = 0;
    unsigned            curr = 0;

    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        for( sym = gsym_table[i], curr = 0; sym; sym = sym->next ) {
            curr++;
#ifdef TRMEM
            DumpSymbol( sym );
#endif
        }
        count += curr;
        if ( curr == 0 )
            num0++;
        else if ( curr == 1 )
            num1++;
        else if ( curr <= 5 )
            num5++;
        else if ( curr <= 10 )
            num10++;
        if (max < curr)
            max = curr;
    }
    if ( Options.quiet == FALSE ) {
        printf( "%u items in symbol table, expected %u\n", count, SymCount );
        printf( "max items in a line=%u, lines with 0/1/<=5/<=10 items=%u/%u/%u/%u, \n", max, num0, num1, num5, num10 );
    }
}
#endif

