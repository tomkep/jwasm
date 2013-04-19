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
#include "memalloc.h"
#include "parser.h"
#include "segment.h"
#include "extern.h"
#include "fixup.h"
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

/* use memcpy()/memcmpi() directly?
 * this may speed-up things, but not with OW.
 * MSVC is a bit faster then.
 */
#define USEFIXSYMCMP 0 /* 1=don't use a function pointer for string compare */

#if USEFIXSYMCMP
#define SYMCMP( x, y, z ) ( ModuleInfo.case_sensitive ? memcmp( x, y, z ) : _memicmp( x, y, z ) )
#else
#define SYMCMP( x, y, z ) SymCmpFunc( x, y, z )
#endif

#define DUMPSYMBOLS 0 /* for debug version only*/

extern struct asym *FileCur;  /* @FileCur symbol    */
extern struct asym *LineCur;  /* @Line symbol       */
extern struct asym *symCurSeg;/* @CurSeg symbol     */

extern void   UpdateLineNumber( struct asym *, void * );
extern void   UpdateWordSize( struct asym *, void * );
extern void   UpdateCurPC( struct asym *sym, void *p );

static struct asym   *gsym_table[ GHASH_TABLE_SIZE ];
static struct asym   *lsym_table[ LHASH_TABLE_SIZE ];

StrCmpFunc SymCmpFunc;

static struct asym   **gsym;      /* pointer into global hash table */
static struct asym   **lsym;      /* pointer into local hash table */
static unsigned      SymCount;    /* Number of symbols in global table */
static char          szDate[12];  /* value of @Date symbol */
static char          szTime[12];  /* value of @Time symbol */

#if defined(__WATCOMC__) || defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)
static const char szDateFmt[] = "%D"; /* POSIX date (mm/dd/yy) */
static const char szTimeFmt[] = "%T"; /* POSIX time (HH:MM:SS) */
#else
/* v2.04: MS VC won't understand POSIX formats */
static const char szDateFmt[] = "%x"; /* locale's date */
static const char szTimeFmt[] = "%X"; /* locale's time */
#endif

static struct asym  *symPC = NULL;

struct tmitem {
    const char *name;
    char *value;
    struct asym **store;
};

/* table of predefined text macros */
static const struct tmitem tmtab[] = {
    /* @Version contains the Masm compatible version */
    /* v2.06: value of @Version changed to 800 */
    //{"@Version",  "615", NULL },
    {"@Version",  "800", NULL },
    {"@Date",     szDate, NULL },
    {"@Time",     szTime, NULL },
    {"@FileName", ModuleInfo.name, NULL },
    {"@FileCur",  NULL, &FileCur },
    /* v2.09: @CurSeg value is never set if no segment is ever opened.
     * this may have caused an access error if a listing was written.
     */
    {"@CurSeg",   "", &symCurSeg }
};

struct eqitem {
    const char *name;
    uint_32 value;
    internal_func sfunc_ptr;
    struct asym **store;
};

/* table of predefined numeric equates */
static const struct eqitem eqtab[] = {
    { "__JWASM__", _JWASM_VERSION_INT_, NULL, NULL },
    { "$",         0,                   UpdateCurPC, &symPC },
    { "@Line",     0,                   UpdateLineNumber, &LineCur },
    { "@WordSize", 0,                   UpdateWordSize, NULL }, /* must be last (see SymInit()) */
};

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
        SymCmpFunc = memcmp;
    else
        SymCmpFunc = _memicmp;
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

void SymGetLocal( struct asym *proc )
/***********************************/
{
    int i;
    struct dsym  **l = &((struct dsym *)proc)->e.procinfo->labellist;

    for ( i = 0; i < LHASH_TABLE_SIZE; i++ ) {
        if ( lsym_table[i] ) {
            *l = (struct dsym *)lsym_table[i];
            l = &(*l)->e.nextll;
        }
    }
    *l = NULL;

    return;
}

/* restore local hash table.
 * - proc: procedure which will become active.
 * fixme: It might be necessary to reset the "defined" flag
 * for local labels (not for params and locals!). Low priority!
 */

void SymSetLocal( struct asym *proc )
/***********************************/
{
    int i;
    struct dsym *l;

    SymClearLocal();
    for ( l = ((struct dsym *)proc)->e.procinfo->labellist; l; l = l->e.nextll ) {
        DebugMsg1(("SymSetLocal(%s): label=%s\n", proc->name, l->sym.name ));
        i = hashpjw( l->sym.name ) % LHASH_TABLE_SIZE;
        lsym_table[i] = &l->sym;
    }
    return;
}

struct asym *SymAlloc( const char *name )
/***************************************/
{
    int len = strlen( name );
    struct asym *sym;

    sym = LclAlloc( sizeof( struct dsym ) );
    memset( sym, 0, sizeof( struct dsym ) );
    if( len > MAX_ID_LEN ) {
        EmitError( IDENTIFIER_TOO_LONG );
        len = MAX_ID_LEN;
    }
    sym->name_size = len;
    sym->list = ModuleInfo.cref;
    sym->mem_type = MT_EMPTY;
    if ( len ) {
        sym->name = LclAlloc( len + 1 );
        memcpy( sym->name, name, len );
        sym->name[len] = NULLC;
    } else
        sym->name = "";
    return( sym );
}

struct asym *SymFind( const char *name )
/**************************************/
/* find a symbol in the local/global symbol table,
 * return ptr to next free entry in global table if not found.
 * Note: lsym must be global, thus if the symbol isn't
 * found and is to be added to the local table, there's no
 * second scan necessary.
 */
{
    int i;
    int len;

    len = strlen( name );
    i = hashpjw( name );

    if ( CurrProc ) {
        for( lsym = &lsym_table[ i % LHASH_TABLE_SIZE ]; *lsym; lsym = &((*lsym)->next) ) {
            if ( len == (*lsym)->name_size && SYMCMP( name, (*lsym)->name, len ) == 0 ) {
                DebugMsg1(("SymFind(%s): found in local table, state=%u, local=%u\n", name, (*lsym)->state, (*lsym)->scoped ));
                return( *lsym );
            }
        }
    }

    for( gsym = &gsym_table[ i % GHASH_TABLE_SIZE ]; *gsym; gsym = &((*gsym)->next) ) {
        if ( len == (*gsym)->name_size && SYMCMP( name, (*gsym)->name, len ) == 0 ) {
            DebugMsg1(("SymFind(%s): found, state=%u memtype=%X lang=%u\n", name, (*gsym)->state, (*gsym)->mem_type, (*gsym)->langtype ));
            return( *gsym );
        }
    }

    return( NULL );
}

#if 0
/* Search a symbol */

struct asym *SymSearch( const char *name )
/****************************************/
{
    return( *SymFind( name ) );
}
#endif

/* SymLookup() creates a global label if it isn't defined yet */

struct asym *SymLookup( const char *name )
/****************************************/
{
    struct asym      *sym;

    sym = SymFind( name );
    if( sym == NULL ) {
        sym = SymAlloc( name );
        DebugMsg1(("SymLookup(%s): created new symbol, CurrProc=%s\n", name, CurrProc ? CurrProc->sym.name : "NULL" ));
        //sym->next = *gsym;
        *gsym = sym;
        ++SymCount;
    }

    DebugMsg1(("SymLookup(%s): found, state=%u, defined=%u\n", name, sym->state, sym->isdefined));

    return( sym );
}

/* this function also creates a label if it isn't defined yet,
 * but the label is preferably created in the local namespace
 * of the current procedure if bLocal==TRUE.
 * called by LabelCreate() [see labels.c]
 */
struct asym *SymLookupLabel( const char *name, int bLocal )
/*********************************************************/
{
    //struct asym      **sym_ptr;
    struct asym      *sym;

    sym = SymFind( name );
    if ( sym == NULL ) {
        sym = SymAlloc( name );
        if ( CurrProc && bLocal && ModuleInfo.scoped ) {
            sym->scoped = TRUE;
            /* add the label to the local hash table */
            //sym->next = *lsym;
            *lsym = sym;
            DebugMsg1(("SymLookupLabel(%s): local symbol created in %s\n", name, CurrProc->sym.name));
        } else {
            DebugMsg1(("SymLookupLabel(%s): global symbol created\n", name));
            //sym->next = *gsym;
            *gsym = sym;
            SymCount++;
        }
    } else if( sym->state == SYM_UNDEFINED &&
               sym->scoped == FALSE && CurrProc && bLocal && ModuleInfo.scoped ) {
        /* the label was defined due to a FORWARD reference.
         * Its scope is to be changed from global to local.
         */
        sym->scoped = TRUE;
        /* remove the label from the global hash table */
        *gsym = sym->next;
        SymCount--;
        /* add the label to the local hash table */
        //sym->next = *lsym;
        sym->next = NULL;
        *lsym = sym;
        DebugMsg1(("SymLookupLabel(%s): label moved into %s's local namespace\n", sym->name, CurrProc->sym.name ));
    }

    DebugMsg1(("SymLookupLabel(%s): found, state=%u, defined=%u\n", name, sym->state, sym->isdefined));
    return( sym );
}

static void FreeASym( struct asym *sym )
/**************************************/
{
    //DebugMsg(("FreeASym: delete %s, state=%X\n", sym->name, sym->state));
#if FASTPASS==0 /* v2.09: why is this FASTPASS and not FASTMEM? */
    struct fixup     *curr;
    struct fixup     *next;

    if ( Parse_Pass == PASS_1 ) /* why pass one only? */
        for( curr = sym->bp_fixup ; curr; ) {
            next = curr->nextbp;
            LclFree( curr );
            curr = next;
        }
#endif
#if FASTMEM==0
    if ( sym->name_size ) LclFree( sym->name );
#endif
    LclFree( sym );
}

/* free state-specific info of a symbol */

static void free_ext( struct asym *sym )
/**************************************/
{
    DebugMsg(("free_ext: item=%p name=%s state=%u\n", sym, sym->name, sym->state ));
    switch( sym->state ) {
    case SYM_INTERNAL:
        if ( sym->isproc )
            DeleteProc( (struct dsym *)sym );
        break;
    case SYM_EXTERNAL:
        if ( sym->isproc )
            DeleteProc( (struct dsym *)sym );
        sym->first_size = 0;
        /* The altname field may contain a symbol (if weak == FALSE).
         * However, this is an independant item and must not be released here
         */
#ifdef DEBUG_OUT /* to be removed, this can't happen anymore. */
        if ( sym->mem_type == MT_TYPE && *sym->type->name == NULLC ) {
            DebugMsg(( "free_ext: external with private type: %s\n", sym->name ));
            SymFree( sym->type );
        }
#endif
        break;
    case SYM_SEG:
        if ( ((struct dsym *)sym)->e.seginfo->internal )
            LclFree( ((struct dsym *)sym)->e.seginfo->CodeBuffer );
        LclFree( ((struct dsym *)sym)->e.seginfo );
        break;
    case SYM_GRP:
        DeleteGroup( (struct dsym *)sym );
        break;
    case SYM_TYPE:
        DeleteType( (struct dsym *)sym );
        break;
    case SYM_MACRO:
        ReleaseMacroData( (struct dsym *)sym );
        LclFree( ((struct dsym *)sym)->e.macroinfo );
        break;
    case SYM_TMACRO:
        if ( sym->predefined == FALSE )
            LclFree( sym->string_ptr );
        break;
#ifdef DEBUG_OUT 
    case SYM_STACK: /* to be removed, this can't happen anymore. */
        if ( sym->mem_type == MT_TYPE && *sym->type->name == NULLC ) {
            DebugMsg(( "free_ext: case SYM_STACK, sym=%s with private type\n", sym->name ));
            /* symbol has a "private" type */
            SymFree( sym->type );
        }
        break;
#endif
    }
}

/* free a symbol directly without a try to find it first
 * (it's not in global namespace)
 */
void SymFree( struct asym *sym )
/******************************/
{
    //DebugMsg(("SymFree: free %X, name=%s, state=%X\n", sym, sym->name, sym->state));
    free_ext( sym );
#if FASTMEM==0
    /* there are some items which are located in static memory! */
    /* v2.09: obsolete */
    //if ( sym->isstatic == FALSE )
#endif
        FreeASym( sym );
    return;
}

/* add a symbol to local table and set the symbol's name.
 * the previous name was "", the symbol wasn't in a symbol table.
 * this function is called for PROC parameters ( see proc.c ).
 */
struct asym *SymAddLocal( struct asym *sym, const char *name )
/************************************************************/
{
    struct asym *sym2;
    /* v2.10: ignore symbols with state SYM_UNDEFINED! */
    //if( SymFind( name ) ) {
    if( ( sym2 = SymFind( name ) ) && sym2->state != SYM_UNDEFINED ) {
        /* shouldn't happen */
        EmitErr( SYMBOL_ALREADY_DEFINED, name );
        return( NULL );
    }
#if FASTMEM==0
    if ( sym->name_size ) LclFree( sym->name );
#endif
    sym->name_size = strlen( name );
    sym->name = LclAlloc( sym->name_size + 1 );
    memcpy( sym->name, name, sym->name_size + 1 );
    sym->next = NULL;
    *lsym = sym;
    return( sym );
}

#if 1
/* add a symbol to the global symbol table */

struct asym *SymAddGlobal( struct asym *sym )
/*******************************************/
{
    if( SymFind( sym->name ) ) {
        EmitErr( SYMBOL_ALREADY_DEFINED, sym->name );
        return( NULL );
    }
    sym->next = NULL;
    *gsym = sym;
    SymCount++;
    return( sym );
}
#endif

struct asym *SymCreate( const char *name )
/****************************************/
/* Create symbol and optionally insert it into the symbol table */
{
    struct asym *sym;

    if( SymFind( name ) ) {
        EmitErr( SYMBOL_ALREADY_DEFINED, name );
        return( NULL );
    }
    sym = SymAlloc( name );
    *gsym = sym;
    SymCount++;
    return( sym );
}

struct asym *SymLCreate( const char *name )
/*****************************************/
/* Create symbol and insert it into the local symbol table.
 * This function is called by LocalDir() and ParseParams()
 * in proc.c ( for LOCAL directive and PROC parameters ).
 */
{
    struct asym *sym;

    /* v2.10: ignore symbols with state SYM_UNDEFINED */
    //if( SymFind( name ) ) {
    if( ( sym = SymFind( name ) ) && sym->state != SYM_UNDEFINED ) {
        EmitErr( SYMBOL_ALREADY_DEFINED, name );
        return( NULL );
    }
    sym = SymAlloc( name );
    *lsym = sym;
    return( sym );
}

void SymMakeAllSymbolsPublic( void )
/**********************************/
{
    int i;
    struct asym  *sym;

    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        for( sym = gsym_table[i]; sym; sym = sym->next ) {
            if ( sym->state == SYM_INTERNAL &&
                /* v2.07: MT_ABS is obsolete */
                //sym->mem_type != MT_ABS &&  /* no EQU or '=' constants */
                sym->isequate == FALSE &&     /* no EQU or '=' constants */
                sym->predefined == FALSE && /* no predefined symbols ($) */
                sym->included == FALSE && /* v2.09: symbol already added to public queue? */
                //sym->scoped == FALSE && /* v2.09: no procs that are marked as "private" */
                sym->name[1] != '&' && /* v2.10: no @@ code labels */
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
#if FASTMEM==0 || defined( DEBUG_OUT )
    unsigned i;
#endif

#if defined( DEBUG_OUT )
    DumpSymbols();
#endif

#if FASTMEM==0 || defined( DEBUG_OUT )
    /* free the symbol table */
    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        struct asym  *sym;
        struct asym  *next;
        for( sym = gsym_table[i]; sym; ) {
            next = sym->next;
            SymFree( sym );
            SymCount--;
            sym = next;
        }
    }
    /**/myassert( SymCount == 0 );
#endif

}

/* initialize global symbol table */

void SymInit( void )
/******************/
{
    struct asym *sym;
    int i;
    time_t    time_of_day;
    struct tm *now;

    DebugMsg(("SymInit() enter\n"));
    SymCount = 0;

    memset( gsym_table, 0, sizeof(gsym_table) );

    time_of_day = time( NULL );
    now = localtime( &time_of_day );
    strftime( szDate, 9, szDateFmt, now );
    strftime( szTime, 9, szTimeFmt, now );

    for( i = 0; i < sizeof(tmtab) / sizeof(tmtab[0]); i++ ) {
        sym = SymCreate( tmtab[i].name );
        sym->state = SYM_TMACRO;
        sym->isdefined = TRUE;
        sym->predefined = TRUE;
        sym->string_ptr = tmtab[i].value;
        if ( tmtab[i].store )
            *tmtab[i].store = sym;
    }

    for( i = 0; i < sizeof(eqtab) / sizeof(eqtab[0]); i++ ) {
        sym = SymCreate( eqtab[i].name );
        sym->state = SYM_INTERNAL;
        /* v2.07: MT_ABS is obsolete */
        //sym->mem_type = MT_ABS;
        sym->isdefined = TRUE;
        sym->predefined = TRUE;
        sym->offset = eqtab[i].value;
        sym->sfunc_ptr = eqtab[i].sfunc_ptr;
        //sym->variable = TRUE; /* if fixup must be created */
        if ( eqtab[i].store )
            *eqtab[i].store = sym;
    }
    sym->list   = FALSE; /* @WordSize should not be listed */
    /* $ is an address (usually). Also, don't add it to the list */
    symPC->variable = TRUE;
    symPC->list     = FALSE;
    LineCur->list   = FALSE;

    DebugMsg(("SymInit() exit\n"));
    return;

}

void SymPassInit( int pass )
/**************************/
{
    unsigned            i;

    if ( pass == PASS_1 )
        return;

#if FASTPASS
    /* No need to reset the "defined" flag if FASTPASS is on.
     * Because then the source lines will come from the line store,
     * where inactive conditional lines are NOT contained.
     */
    if ( UseSavedState )
        return;
#endif
    /* mark as "undefined":
     * - SYM_INTERNAL - internals
     * - SYM_MACRO - macros
     * - SYM_TMACRO - text macros
     */
    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        struct asym *sym;
        for( sym = gsym_table[i]; sym; sym = sym->next ) {
            if ( sym->predefined == FALSE ) {
                /* v2.04: all symbol's "defined" flag is now reset. */
                // if ( sym->state == SYM_TMACRO ||
                //    sym->state == SYM_MACRO  ||
                //    sym->state == SYM_INTERNAL ) {
                    sym->isdefined = FALSE;
                //}
            }
        }
    }
}

uint_32 SymGetCount( void )
/*************************/
{
    return( SymCount );
}

/* get all symbols in global hash table */

void SymGetAll( struct asym **syms )
/**********************************/
{
    struct asym         *sym;
    unsigned            i, j;

    /* copy symbols to table */
    for( i = j = 0; i < GHASH_TABLE_SIZE; i++ ) {
        for( sym = gsym_table[i]; sym; sym = sym->next ) {
            syms[j++] = sym;
        }
    }
    return;
}

/* enum symbols in global hash table.
 * used for codeview symbolic debug output.
 */

struct asym *SymEnum( struct asym *sym, int *pi )
/***********************************************/
{
    if ( sym == NULL ) {
        *pi = 0;
        sym = gsym_table[*pi];
    } else {
        sym = sym->next;
    }

    /* v2.10: changed from for() to while() */
    while( sym == NULL && *pi < GHASH_TABLE_SIZE - 1 )
        sym = gsym_table[++(*pi)];

    //printf("sym=%X, i=%u\n", sym, *pi );
    return( sym );
}

#if defined( DEBUG_OUT )

#if DUMPSYMBOLS
static void DumpSymbol( struct asym *sym )
/****************************************/
{
    struct dsym *dir;
    char        *type;
    //const char  *langtype;

    dir = (struct dsym *)sym;

    switch( sym->state ) {
    case SYM_UNDEFINED:
        type = "UNDEFINED";
        break;
    case SYM_INTERNAL:
        if ( sym->isproc )
            type = "PROCEDURE";
        //else if ( sym->mem_type == MT_ABS )
        else if ( sym->segment == NULL )
            type = "NUMBER";
        else
            type = "INTERNAL";
        break;
    case SYM_EXTERNAL:
        if ( sym->isproc )
            type = "PROTO";
        else if ( sym->comm )
            type = "COMMUNAL";
        else
            type = "EXTERNAL";
        break;
    case SYM_SEG:
        type = "SEGMENT";
        break;
    case SYM_GRP:
        type = "GROUP";
        break;
    case SYM_STACK: /* should never be found in global table */
        type = "STACK VAR";
        break;
    case SYM_STRUCT_FIELD: /* record bitfields are in global namespace! */
        type = "STRUCT FIELD";
        break;
    case SYM_TYPE:
        switch ( sym->typekind ) {
        case TYPE_UNION:   type = "UNION";     break;
        case TYPE_TYPEDEF: type = "TYPEDEF";   break;
        case TYPE_RECORD:  type = "RECORD";    break;
        default:           type = "STRUCTURE"; break;
        }
        break;
    case SYM_ALIAS:
        type = "ALIAS";
        break;
    case SYM_MACRO:
        type = "MACRO";
        break;
    case SYM_TMACRO:
        type = "TEXT";
        break;
    //case SYM_CLASS_LNAME: /* never stored in global or local table */
    //    type = "CLASS";
    //    break;
    default:
        type = "UNKNOWN";
        break;
    }
    DebugMsg(( "%8p: %-30s %-12s  %8" FX32 "  %8p %8p %8p %s\n", sym, sym->name, type, sym->offset, dir->e, sym->name, sym->next, sym->public ? "PUBLIC " : "" ));
}
#endif

static void DumpSymbols( void )
/*****************************/
{
    struct asym         *sym;
    unsigned            i;
    unsigned            count = 0;
    unsigned            max = 0;
    unsigned            num0 = 0;
    unsigned            num1 = 0;
    unsigned            num5 = 0;
    unsigned            num10 = 0;
    unsigned            curr = 0;

    DebugMsg(("DumpSymbols enter\n"));
    for( i = 0; i < GHASH_TABLE_SIZE; i++ ) {
        for( sym = gsym_table[i], curr = 0; sym; sym = sym->next ) {
            curr++;
#if DUMPSYMBOLS
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
        if ( max < curr )
            max = curr;
    }
    if ( Options.quiet == FALSE ) {
        printf( "%u items in symbol table, expected %u\n", count, SymCount );
        printf( "max items in a line=%u, lines with 0/1/<=5/<=10 items=%u/%u/%u/%u, \n", max, num0, num1, num5, num10 );
    }
}
#endif

