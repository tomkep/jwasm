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

static unsigned         AsmSymCount;    /* Number of symbols in table */

static char             szDate[12];
static char             szTime[12];

#define DOTSMAX 32

static char             dots[] = " . . . . . . . . . . . . . . . .";

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
    sym->next = NULL;
    sym->name_size = strlen( name );
    sym->name = AsmAlloc( sym->name_size + 1 );
    strcpy( sym->name, name );

    sym->segment = NULL;

    sym->offset = 0;
    sym->first_size = 0;
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
    sym->list = ModuleInfo.cref;
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
        ++AsmSymCount;
    }

    if( IS_SYM_COUNTER( name ))
        SetSymSegOfs( sym );

    DebugMsg(("SymLookup: symbol %s found, state=%u\n", name, sym->state));

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
            ++AsmSymCount;
        }
        sym->next = *sym_ptr;
        *sym_ptr = sym;
    } else if( sym->state == SYM_UNDEFINED &&
               sym->local == FALSE && CurrProc && bLocal && ModuleInfo.scoped) {
        /* move the label to the current proc's local label list
         if it isn't defined yet */
        *sym_ptr = sym->next;
        AsmSymCount--;
        for (sym_ptr = &CurrProc->e.procinfo->labellist;*sym_ptr;sym_ptr = &((*sym_ptr)->next))
            ;
        sym->local = TRUE;
        sym->next = *sym_ptr;
        *sym_ptr = sym;
        DebugMsg(("SymLookupLabel: label moved into local namespace\n"));
    }
    if( IS_SYM_COUNTER( name ) ) {
        SetSymSegOfs( sym );
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
        next = curr->next;
        AsmFree( curr );
        curr = next;
    }
#endif
    AsmFree( sym->name );
    AsmFree( sym );
}

#if defined( _STANDALONE_ )

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
        --AsmSymCount;
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

// set a symbol's name directly without a try to find it first
// (it's not in global namespace)

void SymSetName( struct asm_sym *sym, const char * name)
/*********************************/
{
    AsmFree( sym->name );
    sym->name_size = strlen( name );
    sym->name = AsmAlloc( sym->name_size + 1 );
    strcpy( sym->name, name );
    return;
}

static struct asm_sym *SymAddToTable( struct asm_sym *sym )
/*****************************************************/
{
    struct asm_sym  **location;

    location = SymFind( sym->name );

    if( *location != NULL ) {
        /* we already have one */
        AsmErr( SYMBOL_ALREADY_DEFINED, sym->name );
        return( NULL );
    }

    sym->next = *location;
    *location = sym;
    ++AsmSymCount;
    return( sym );
}

struct asm_sym *SymCreate( const char *name, int add_symbol )
/***********************************************************/
/* Create directive symbol and insert it into the symbol table */
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
    if( ( *sym_ptr != NULL ) && IS_SYM_COUNTER( name ) )
        SetSymSegOfs( *sym_ptr );
    return( *sym_ptr );
}

#endif

void DumpASym( void );

void SymFini( void )
/*********************/
{
    struct asm_sym      *sym;
    dir_node            *dir;
    unsigned            i;

#if defined( DEBUG_OUT )
    DumpASym();
#endif

    /* free the symbol table */
#if 0
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
            --AsmSymCount;
        }
    }
    myassert( AsmSymCount == 0 );
#else
    AsmSymCount = 0;
#endif

}
void SymInit( )
/*********************/
{
    asm_sym * sym;

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

    /* $ */
    sym = SymCreate( "$", TRUE );  // create "$" symbol for current segment counter
    sym->state = SYM_INTERNAL;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->mem_type = MT_NEAR;
    sym->list = FALSE; /* don't display the '$' symbol in symbol list */

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

static struct asm_sym **SortAsmSyms( void )
/*****************************************/
{
    struct asm_sym      **syms;
    struct asm_sym      *sym;
    unsigned            i, j;

    syms = MemAlloc( AsmSymCount * sizeof( struct asm_sym * ) );
    if( syms ) {
        /* copy symbols to table */
        for( i = j = 0; i < HASH_TABLE_SIZE; i++ ) {
            for(sym = sym_table[i]; sym; sym = sym->next) {
                syms[j++] = sym;
            }
        }
        /* sort 'em */
        qsort( syms, AsmSymCount, sizeof( struct asm_sym * ), compare_fn );
    }
    return( syms );
}

static const char *get_seg_align( seg_info *seg )
/****************************************/
{
    switch( seg->segrec->d.segdef.align ) {
    case ALIGN_ABS:
    case ALIGN_BYTE:
        return( "Byte " );
    case ALIGN_WORD:
        return( "Word " );
    case ALIGN_DWORD:
        return( "DWord" );
    case ALIGN_PARA:
        return( "Para " );
    case ALIGN_PAGE:
        return( "Page " );
    case ALIGN_4KPAGE:
        return( "4K   " );
    default:
        return( "?    " );
    }
}

static const char *get_seg_combine( seg_info *seg )
/*************************************************/
{
    switch( seg->segrec->d.segdef.combine ) {
    case COMB_INVALID:
        return( "Private " );
    case COMB_STACK:
        return( "Stack   " );
    case COMB_ADDOFF:
        return( "Public  " );
    default:
        return( "?       " );
    }
}

static void log_segment( struct asm_sym *sym, struct asm_sym *group )
/*******************************************************************/
{
    if( sym->state == SYM_SEG ) {
        dir_node    *dir = (dir_node *)sym;
        seg_info    *seg = dir->e.seginfo;

        if( seg->group == group ) {
            int i = strlen( sym->name );
            char *pdots = dots + i + 1;
            if (i >= DOTSMAX)
                pdots = "";
            LstMsg( "%s %s        ", sym->name, pdots );
            if( seg->Use32 ) {
                LstMsg( "32 Bit   %08lX ", seg->current_loc );
            } else {
                LstMsg( "16 Bit   %04lX     ", seg->current_loc );
            }
            LstMsg( "%s   %s", get_seg_align( seg ), get_seg_combine( seg ) );
            LstMsg( "'%s'\n", GetLname( seg->segrec->d.segdef.class_name_idx ) );
        }
    }
}

static void log_macro( struct asm_sym *sym )
{
    int i = strlen ( sym->name);
    char *pdots = dots + i + 1;
    char *type = (sym->isfunc) ? "Func" : "Proc";

    if (i >= DOTSMAX)
        pdots = "";
    LstMsg( "%s %s        %s\n", sym->name, pdots ,type);
    return;
}

static char * GetMemTypeString(asm_sym * sym, char * buffer)
{
    switch (sym->mem_type) {
    case MT_BYTE:
    case MT_SBYTE:
        return( "Byte");
    case MT_WORD:
    case MT_SWORD:
        return( "Word");
    case MT_DWORD:
    case MT_SDWORD:
        return( "Dword");
    case MT_FWORD:
        return( "Fword");
    case MT_QWORD:
        return( "Qword");
    case MT_TBYTE:
        return( "TByte");
    case MT_OWORD:
        return( "Oword");
    case MT_PTR:
    case MT_FAR:
    case MT_NEAR:
        if (sym->type && buffer) {
            strcat(buffer,"Ptr ");
            strcat(buffer, sym->type->name);
            return(buffer);
        }
        return( "Ptr");
    case MT_PROC:
        if (sym->type && buffer) { /* typedef PROTO? */
            strcat(buffer,"Proc ");
            strcat(buffer, sym->type->name);
            return(buffer);
        }
        return( "Proc");
    case MT_TYPE:
        if (sym->type)  /* it must be set, but to be safe ... */
            return(sym->type->name);
    }
    return("?");
}

// a struct either has fields or a size of 0

static void log_struct( char * name, struct asm_sym *sym, int ofs )
/*****************************************************************/
{
    unsigned        i;
    dir_node    *dir = (dir_node *)sym;
    char *pdots;
    struct_info  *si = dir->e.structinfo;
    field_list *f;
    static int prefix = 0;

    if(( sym->state == SYM_TYPE ) && (si->isRecord == FALSE) && (si->isTypedef == FALSE)) {
        if (!name)
            name = sym->name;
        i = strlen ( name);
        pdots = dots + i + prefix + 1;
        if (i >= DOTSMAX)
            pdots = "";
        for (i=0;i<prefix;i++)
            LstMsg(" ");
        if (prefix == 0)
            LstMsg( "%s %s        %8X\n", name, pdots, sym->total_size );
        else
            LstMsg( "%s %s        %8X\n", name, pdots, sym->offset + ofs);
        prefix += 2;
        for( f = si->head; f; f = f->next ) {
            /* recursion if an embedded struct occurs */
            if (f->sym->mem_type == MT_TYPE && f->initializer == NULL) {
                log_struct( f->sym->name, f->sym->type, f->sym->offset + ofs);
            } else {
                /* don't list unstructured fields without name */
                /* but do list them if they are structured */
                if (*(f->sym->name) || (f->sym->mem_type == MT_TYPE)) {
                    i = strlen ( f->sym->name ) + prefix;
                    pdots = dots + i + 1 ;
                    if (i >= DOTSMAX)
                        pdots = "";
                    for (i=0;i<prefix;i++)
                        LstMsg(" ");
                    LstMsg( "%s %s        %8X   ", f->sym->name, pdots, f->sym->offset + sym->offset + ofs);
                    LstMsg("%s", GetMemTypeString(f->sym, NULL));
                    if (f->sym->total_length > 1)
                        LstMsg("[%u]",f->sym->total_length);
                    LstMsg( "\n");
                }
            }
        }
        prefix -= 2;
    }
}

static void log_record( struct asm_sym **syms, struct asm_sym *sym )
/*****************************************************************/
{
    unsigned        i;
    unsigned        mask;
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    field_list *f;

    if(( sym->state == SYM_TYPE ) && (si->isRecord == TRUE)) {
        int i = strlen ( sym->name);
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        for( i = 0,f = si->head; f; f = f->next,i++ );
        LstMsg( "%s %s      %6X  %7X\n", sym->name, pdots, sym->total_size*8, i );
        for( f = si->head; f; f = f->next ) {
            i = strlen ( f->sym->name ) + 2;
            pdots = dots + i + 1 ;
            if (i >= DOTSMAX)
                pdots = "";
            for (i=f->sym->offset, mask=0;i < f->sym->offset+f->sym->total_size;i++)
                mask |= 1 << i;
            LstMsg( "  %s %s      %6X  %7X  %08X\n", f->sym->name, pdots, f->sym->offset, f->sym->total_size, mask);
        }
    }
}

// a typedef is a simple struct with no fields. Size might be 0.

static void log_typedef( struct asm_sym **syms, struct asm_sym *sym )
{
    unsigned        i;
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    field_list *f;
    static int prefix = 2;

    if(( sym->state == SYM_TYPE ) && (si->isTypedef == TRUE)) {
        char buffer[128];
        char * attr;
        int i = strlen ( sym->name);
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        buffer[0] = '\0';
        LstMsg( "%s %s    %8u %s\n", sym->name, pdots, sym->total_size, GetMemTypeString(sym, buffer));
#if 0
        for( f = si->head; f; f = f->next ) {
            i = strlen ( f->sym->name ) + prefix;
            pdots = dots + i + 1 ;
            if (i >= DOTSMAX)
                pdots = "";
            LstMsg( "%s %s        %8u\n", f->sym->name, pdots, f->sym->offset);
        }
#endif
    }
}

static void log_group( struct asm_sym **syms, struct asm_sym *sym )
/*****************************************************************/
{
    unsigned        i;

    if( sym->state == SYM_GRP ) {
        int i = strlen ( sym->name);
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        LstMsg( "%s %s        ", sym->name, pdots );
        LstMsg( "GROUP\n" );
        for( i = 0; i < AsmSymCount; ++i ) {
            log_segment( syms[i], sym );
        }
    }
}

static const char *get_sym_type( struct asm_sym *sym )
/****************************************************/
{
    switch( sym->mem_type ) {
    case MT_BYTE:
    case MT_SBYTE:
        return( "Byte" );
    case MT_WORD:
    case MT_SWORD:
        return( "Word" );
    case MT_DWORD:
    case MT_SDWORD:
        return( "DWord" );
    case MT_FWORD:
        return( "FWord" );
    case MT_QWORD:
        return( "QWord" );
    case MT_OWORD:
        return( "OWord" );
    case MT_NEAR:
        return( "L Near" );
    case MT_FAR:
        return( "L Far" );
    case MT_PTR:
        return( "Ptr" );
    case MT_PROC:
        return( "Proc" );
    case MT_ABS:
        return( "Number" );
    case MT_TYPE:
        if (*(sym->type->name))
            return( sym->type->name );
        return( "Ptr");
    default:
        return( "?" );
    }
}

static const char *get_proc_type( struct asm_sym *sym )
/*****************************************************/
{
    switch( sym->mem_type ) {
    case MT_NEAR:
        return( "Near " );
    case MT_FAR:
        return( "Far  " );
    default:
        return( "     " );
    }
}

static const char *get_sym_lang( struct asm_sym *sym )
/****************************************************/
{
    switch( sym->langtype ) {
    case LANG_C:
        return( "C" );
    case LANG_BASIC:
        return( "BASIC" );
    case LANG_FORTRAN:
        return( "FORTRAN" );
    case LANG_PASCAL:
        return( "PASCAL" );
    case LANG_WATCOM_C:
        return( "WATCOM_C" );
    case LANG_STDCALL:
        return( "STDCALL" );
    case LANG_SYSCALL:
        return( "SYSCALL" );
    default:
        return( "" );
    }
}

static const char *get_sym_seg_name( struct asm_sym *sym )
/********************************************************/
{
    if( sym->segment ) {
        return( sym->segment->name );
    } else {
        return( "No Seg" );
    }
}

/* list symbols */

static void log_symbol( struct asm_sym *sym )
/*******************************************/
{
    int i = strlen( sym->name );
    char *pdots = dots + i + 1;
    char buffer[MAX_LINE_LEN];

    if (i >= DOTSMAX)
        pdots = "";

    switch (sym->state) {
    case SYM_INTERNAL:
    case SYM_EXTERNAL:
        LstMsg( "%s %s        ", sym->name, pdots );

        if (sym->total_length > 1) {
            i = sprintf( buffer, "%s[%u]", get_sym_type( sym ), sym->total_length );
            LstMsg( "%-10s ", buffer );
        } else
            LstMsg( "%-10s ", get_sym_type( sym ) );

        LstMsg( "%8X  ", sym->offset );

        if (sym->mem_type == MT_ABS)
            ;
        else
            LstMsg( "%s\t", get_sym_seg_name( sym ) );

        if( sym->public )
            LstMsg( "Public " );

        if (sym->state == SYM_EXTERNAL) {

            if (sym->weak == 1)
                LstMsg( "*External " );
            else
                LstMsg( "External  " );
        }

        LstMsg( "%s\n", get_sym_lang( sym ) );
        break;
    case SYM_TMACRO:
        GetTextMacroValue(sym->string_ptr, buffer);
        LstMsg( "%s %s        Text   %s\n", sym->name, pdots, buffer );
        break;
    case SYM_ALIAS:
        LstMsg( "%s %s        Alias  %s\n", sym->name, pdots, sym->string_ptr );
        break;
    }
}

/* list Procedures and Prototypes */

static void log_proc( struct asm_sym *sym )
/*****************************************/
{
    if( sym->state == SYM_PROC ) {
        label_list *f;
        struct asm_sym *l;
        char * p;
        dir_node *dir = (dir_node *)sym;
        int i = strlen ( sym->name);
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        if (sym->use32)
            p = "%s %s        P %s  %08X %s\t";
        else
            p = "%s %s        P %s  %04X     %s\t";
        LstMsg( p,
                sym->name,
                pdots,
                get_proc_type( sym ),
                sym->offset,
                get_sym_seg_name( sym ));
        if (sym->use32)
            LstMsg( "Length= %08X ", sym->total_size );
        else
            LstMsg( "Length= %04X ", sym->total_size );
        if( sym->public ) {
            LstMsg( "Public " );
        } else if (dir->e.procinfo->defined) {
            LstMsg( "Private " );
        } else
            LstMsg( "External " );

        LstMsg( "%s", get_sym_lang( sym ) );
        LstMsg( "\n" );
        if (dir->e.procinfo->defined) {
            if (dir->sym.langtype == LANG_C ||
                dir->sym.langtype == LANG_SYSCALL ||
                dir->sym.langtype == LANG_STDCALL) {
                int cnt;
                /* position f2 to last param */
                for (cnt = 0,f = dir->e.procinfo->paralist;f;f = f->next)
                    cnt++;
                for (;cnt;cnt--) {
                    int curr;
                    for (curr = 1,f = dir->e.procinfo->paralist;curr < cnt;f = f->next,curr++);
                    i = strlen ( f->sym->name);
                    pdots = dots + i + 1 + 2;
                    if (i >= DOTSMAX)
                        pdots = "";
                    LstMsg( "  %s %s        %-17s bp +%04X\n", f->sym->name, pdots, get_sym_type(f->sym), f->sym->offset);
                }
            } else {
                for (f = dir->e.procinfo->paralist;f;f = f->next) {
                    i = strlen ( f->sym->name);
                    pdots = dots + i + 1 + 2;
                    if (i >= DOTSMAX)
                        pdots = "";
                    LstMsg( "  %s %s        %-17s bp +%04X\n", f->sym->name, pdots, get_sym_type(f->sym), f->sym->offset);
                }
            }
            for (f = dir->e.procinfo->locallist;f;f = f->next) {
                i = strlen ( f->sym->name);
                pdots = dots + i + 1 + 2;
                if (i >= DOTSMAX)
                    pdots = "";
                LstMsg( "  %s %s        %-17s bp -%04X\n", f->sym->name, pdots, get_sym_type(f->sym), -f->sym->offset);
            }
            for (l = dir->e.procinfo->labellist;l;l = l->next) {
                i = strlen ( l->name);
                pdots = dots + i + 1 + 2;
                if (i >= DOTSMAX)
                    pdots = "";
                if (sym->use32)
                    p = "  %s %s        L %s  %08X %s\n";
                else
                    p = "  %s %s        L %s  %04X     %s\n";
                LstMsg( p,
                        l->name,
                        pdots,
                        get_proc_type( l ),
                        l->offset,
                        get_sym_seg_name( l ));
            }
        }
    }
}

/* output the symbol table listing */

void SymWriteCRef( void )
/***********************/
{
    struct asm_sym  **syms;
    struct_info     *si;
    unsigned        i;
    unsigned cntStructs = 0;
    unsigned cntRecords = 0;
    unsigned cntTypedefs = 0;
    unsigned cntProcs = 0;
    unsigned cntMacros = 0;

    if( AsmFiles.file[LST] == NULL || Options.no_symbol_listing == TRUE) {
        return; // no point going through the motions if lst file isn't open
    }

    DebugMsg(("SymWriteCRef: calling SortAsmSyms\n"));

    syms = SortAsmSyms();
    if( syms ) {
        for( i = 0; i < AsmSymCount; ++i ) {
            if (syms[i]->list == FALSE)
                continue;
            switch (syms[i]->state) {
            case SYM_TYPE:
                si = ((dir_node *)syms[i])->e.structinfo;
                if (si->isRecord == TRUE)
                    cntRecords++;
                else if (si->isTypedef == TRUE)
                    cntTypedefs++;
                else
                    cntStructs++;
                break;
            case SYM_PROC:
                cntProcs++;
                break;
            case SYM_MACRO:
                cntMacros++;
                break;
            }
        }
        if (cntMacros) {
            LstMsg( "\n\nMacros:\n\n" );
            LstMsg( "                N a m e                 Type\n\n" );
            /* write out structures */
            for( i = 0; i < AsmSymCount; ++i ) {
                if (syms[i]->list == TRUE && syms[i]->state == SYM_MACRO)
                    log_macro( syms[i] );
            }
        }
        if (cntStructs) {
            LstMsg( "\n\nStructures and Unions:\n\n" );
            LstMsg( "                N a m e                 Size/Ofs   Type\n\n" );
            /* write out structures */
            for( i = 0; i < AsmSymCount; ++i ) {
                if (syms[i]->list == TRUE)
                    log_struct( NULL, syms[i], 0 );
            }
        }
        if (cntRecords) {
            LstMsg( "\n\nRecords:\n\n" );
            LstMsg( "                N a m e                 Width   # fields\n" );
            LstMsg( "                                        Shift   Width    Mask   Initial\n\n" );
            for( i = 0; i < AsmSymCount; ++i ) {
                if (syms[i]->list == TRUE)
                    log_record( syms, syms[i] );
            }
        }
        if (cntTypedefs) {
            LstMsg( "\n\nTypes:\n\n" );
            LstMsg( "                N a m e              Size    Attr\n\n" );
            for( i = 0; i < AsmSymCount; ++i ) {
                if (syms[i]->list == TRUE)
                    log_typedef( syms, syms[i] );
            }
        }
        /* write out the segments */
        LstMsg( "\n\nSegments and Groups:\n\n" );
        LstMsg( "                N a m e                 Size" );
        LstMsg( "     Length   Align   Combine Class\n\n" );
        /* write out groups with associated segments */
        for( i = 0; i < AsmSymCount; ++i ) {
            log_group( syms, syms[i] );
        }
        /* write out remaining segments, outside any group */
        for( i = 0; i < AsmSymCount; ++i ) {
            log_segment( syms[i], NULL );
        }
        LstMsg( "\n" );

        if (cntProcs) {
            /* next write out procedures and stuff */
            LstMsg( "\n\nProcedures, parameters and locals:\n\n" );
            LstMsg( "                N a m e                 Type" );
            LstMsg( "     Value    Attr\n\n" );
            for( i = 0; i < AsmSymCount; ++i ) {
                if (syms[i]->list == TRUE)
                    log_proc( syms[i] );
            }
            LstMsg( "\n" );
        }

        /* next write out symbols */
        LstMsg( "\n\nSymbols:\n\n" );
        LstMsg( "                N a m e                 Type" );
        LstMsg( "       Value     Attr\n\n" );
        for( i = 0; i < AsmSymCount; ++i ) {
            if (syms[i]->list == TRUE)
                log_symbol( syms[i] );
        }
        LstMsg( "\n" );

        DebugMsg(("SymWriteCRef: free sorted symbols\n"));

        /* free the sorted symbols */
        MemFree( syms );
    }
}

#if defined( DEBUG_OUT )

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
        if (dir->e.procinfo->defined)
            type = "PROCEDURE";
        else
            type = "PROTOTYPE";
        break;
    case SYM_MACRO:
        type = "MACRO";
        break;
    case SYM_LNAME:
        type = "LNAME";
        break;
    case SYM_TYPE:
        if (dir->e.structinfo->isUnion)
            type = "UNION";
        else if (dir->e.structinfo->isTypedef)
            type = "TYPEDEF";
        else if (dir->e.structinfo->isRecord)
            type = "RECORD";
        else
            type = "STRUCTURE";
        break;
    case SYM_CLASS_LNAME:
        type = "CLASS";
        break;
    case SYM_STRUCT_FIELD:
        type = "STRUCTURE FIELD";
        break;
    case SYM_LIB:
        type = "LIBRARY";
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

void DumpASym( void )
/*******************/
{
    struct asm_sym      *sym;
    unsigned            i;
    unsigned            count = 0;
    unsigned            max = 0;
    unsigned            curr = 0;
    unsigned            empty = 0;

    LstMsg( "\n" );
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
//            DumpSymbol( sym );
            flushall(); // make sure everything is dumped out
        }
        if (max < curr)
            max = curr;
    }
    printf("%u items in symbol table, ", count);
    printf("%u items max in one line, %u lines empty\n", max, empty);
}
#endif

#endif
