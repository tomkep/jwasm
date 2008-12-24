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
* Description:  listing support
*
****************************************************************************/


#include <stdarg.h>
#include <ctype.h>

#include "globals.h"
#include "symbols.h"
#include "memalloc.h"
#include "directiv.h"
#include "segment.h"
#include "tokenize.h"
#include "symbols.h"
#include "macro.h"
#include "queue.h"
#include "queues.h"
#include "fatal.h"
#include "fastpass.h"
#include "listing.h"
#include "input.h"
#include "msgtext.h"

#define CODEBYTES 8
#define OFSSIZE 8
#define PREFFMT OFSSIZE + 2 + 2 * CODEBYTES + 1
#define PREFFMTSTR "23"  /* OFSSIZE + 2 * CODEBYTES - 3 */

#ifdef __UNIX__
#define NLSIZ 1
#define NLSTR "\n"
#else
#define NLSIZ 2
#define NLSTR "\r\n"
#endif

extern uint_32  LastCodeBufSize;
extern bool     line_listed;

uint_32 list_pos; /* current pos in LST file */
uint_32 list_pos_start;

static unsigned         SymCount;

#define DOTSMAX 32
static char  dots[] = " . . . . . . . . . . . . . . . .";
static char  stdprefix[] = "%08X                  ";

void LstWrite( enum lsttype type, unsigned int oldofs, void * value )
{
    unsigned int newofs;
    asm_sym * sym = value;
    int len;
    int idx;
    char * p;
    char * p2;
    char buffer[128];

    if (ModuleInfo.list == FALSE || FileInfo.file[LST] == NULL)
        return;
    if ( GeneratedCode && ( ModuleInfo.list_generated_code == FALSE ) )
        return;
    if ( MacroLevel ) {
        switch ( ModuleInfo.list_macro ) {
        case LM_NOLISTMACRO:
            return;
        case LM_LISTMACRO:
            /* todo: filter certain macro lines */
            break;
        }
    }

    line_listed = TRUE;

    DebugMsg(("LstWrite: enter, lineno=%u, pos=%u\n", LineNumber, list_pos ));
#if FASTPASS
    if ( ( Parse_Pass > PASS_1 ) && UseSavedState ) {
        if ( GeneratedCode == 0 ) {
            list_pos = LineStoreCurr->list_pos;
            DebugMsg(("LstWrite: Pass=%u, pos=%u\n", Parse_Pass+1, list_pos ));
        }
        fseek( FileInfo.file[LST], list_pos, SEEK_SET );
    }
#endif

    p = buffer + OFSSIZE+2*CODEBYTES+2;

    switch ( type ) {
    case LSTTYPE_LIDATA:
        newofs = GetCurrOffset();
        len = sprintf( buffer, stdprefix, oldofs );
        p = buffer + len;

        if (CurrSeg == NULL)
            break;
        if ( write_to_file == FALSE )
            break;

        len = CODEBYTES;
        p2 = buffer + 8 + 2;

        if ( CurrSeg->seg->e.seginfo->segtype == SEGTYPE_BSS ) {
            while (oldofs < newofs && len) {
                *p2++ = '0';
                *p2++ = '0';
                oldofs++;
                len--;
            }
            break;
        }

        /* OMF hold just a small buffer for one LEDATA record */
        /* if it has been flushed, use LastCodeBufSize */
        idx = (CurrSeg->seg->e.seginfo->current_loc - CurrSeg->seg->e.seginfo->start_loc)
            - (newofs - oldofs);
        if (Options.output_format == OFORMAT_OMF) {
            while (idx < 0 && len) {
                sprintf( p2, "%02X", CurrSeg->seg->e.seginfo->CodeBuffer[idx+LastCodeBufSize] );
                p2 += 2;
                idx++;
                oldofs++;
                len--;
            }
        } else if (idx < 0)
            idx = 0;

        while ( oldofs < newofs && len ) {
            sprintf( p2, "%02X", CurrSeg->seg->e.seginfo->CodeBuffer[idx] );
            p2 += 2;
            idx++;
            oldofs++;
            len--;
        }
        *p2 = ' ';
        break;
    case LSTTYPE_EQUATE:
        if ( sym->state == SYM_INTERNAL ) {
            sprintf( buffer, " = %-" PREFFMTSTR "X", sym->value );
            p = buffer + strlen(buffer);
        } else if ( sym->state == SYM_TMACRO ) {
            char buffer2[MAX_LINE_LEN];
            GetTextMacroValue( sym->string_ptr, buffer2 );
            sprintf( buffer, " = %-" PREFFMTSTR ".80s", buffer2 );
            p = buffer + strlen( buffer );
        }
        break;
    case LSTTYPE_STRUCT:
        len = sprintf(buffer, stdprefix, oldofs);
        break;
    case LSTTYPE_LABEL:
        oldofs = GetCurrOffset();
    default:
        if ( type != LSTTYPE_MACRO && (CurrSeg || value) )
            len = sprintf(buffer, stdprefix, oldofs);
        else
            memset( buffer, ' ', OFSSIZE+2*CODEBYTES+2 );
    }

    *p = ' ';
    p++;
    fwrite( buffer, 1, p - buffer, FileInfo.file[LST] );
#ifdef DEBUG_OUT
    *p = NULLC;
    DebugMsg(("LstWrite: writing >%s<\n", buffer ));
#endif

    list_pos += p - buffer;

    p = CurrSource;
    //while( isspace( *p ) ) p++;
    len = strlen(p);

    /* calc new list position */
    if ( MacroLevel ) {
        list_pos += sprintf( buffer, "%u ", MacroLevel );
    }
    if ( GeneratedCode )
        list_pos++;

    list_pos += len + NLSIZ;

#if FASTPASS
    if ( (Parse_Pass > PASS_1) && UseSavedState ) {
        DebugMsg(("LstWrite: new pos=%u\n", list_pos ));
        return;
    }
#endif
    /* if it's macro code, write the macro level before the line */
    if ( MacroLevel )
        fwrite( buffer, 1, strlen(buffer), FileInfo.file[LST] );

    /* if it's generated code, write a '*' before the line */
    if ( GeneratedCode )
        fwrite( "*", 1, 1, FileInfo.file[LST] );

    fwrite( p, 1, len, FileInfo.file[LST] );
    fwrite( NLSTR, 1, NLSIZ, FileInfo.file[LST] );

    DebugMsg(("LstWrite: writing >%s<, new pos=%u\n", p, list_pos ));
    return;
}

void LstWriteSrcLine( void )
{
    LstWrite( LSTTYPE_MACRO, 0, NULL );
}

void LstPrintf( const char *format, ... )
/************************************/
{
    va_list     args;

    if( FileInfo.file[LST] ) {
        va_start( args, format );
        list_pos += vfprintf( FileInfo.file[LST], format, args );
        va_end( args );
    }
}
void LstNL( void )
/************************************/
{
    if( FileInfo.file[LST] ) {
        fwrite( NLSTR, 1, NLSIZ, FileInfo.file[LST] );
        list_pos += NLSIZ;
    }
}

static char *get_seg_align( seg_info *seg, char * buffer )
/****************************************/
{
    switch( seg->alignment ) {
    case 0:
        return( "Byte " );
    case 1:
        return( "Word " );
    case 2:
        return( "DWord" );
    case 3:
        return( "QWord" );
    case 4:
        return( "Para " );
    case 8:
        return( "Page " );
    default:
        sprintf( buffer, "%.5u", 1 << seg->alignment );
        return( buffer );
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

static void log_macro( struct asm_sym *sym )
{
    int i = strlen ( sym->name);
    char *pdots = dots + i + 1;
    char *type = (sym->isfunc) ? "Func" : "Proc";

    if (i >= DOTSMAX)
        pdots = "";
    LstPrintf( "%s %s        %s", sym->name, pdots ,type);
    LstNL();
    return;
}

// called by log_struct and log_typedef
// that is, the symbol is ensured to be a TYPE!

static char * GetMemtypeString(asm_sym * sym, char * buffer)
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
        if ( buffer ) {
            if ( sym->isfar )
                strcat(buffer,"Far");
            else
                strcat(buffer,"Near");
            if ( sym->use32 )
                strcat(buffer,"32");
            else
                strcat(buffer,"16");
            strcat(buffer," Ptr");
            return(buffer);
        }
        return( "Ptr");
    case MT_FAR:
    is_far:
        if ( sym->segment )
            return( "L Far");
        if ( SymIs32( sym ) )
            return( "L Far32");
        return( "L Far16");
    case MT_PROC:
        if ( ModuleInfo.model == MOD_MEDIUM ||
             ModuleInfo.model == MOD_LARGE ||
             ModuleInfo.model == MOD_HUGE)
            goto is_far;
        /* fall through */
    case MT_NEAR:
        if ( sym->segment )
            return( "L Near");
        if ( SymIs32( sym ) )
            return( "L Near32");
        return( "L Near16");
    case MT_TYPE:
        if (*(sym->type->name))  /* it must be set, but to be safe ... */
            return(sym->type->name);
        return( "Ptr");
    case MT_ABS:
    case MT_EMPTY: /* relocatable number, assigned with '=' directive */
        return( "Number" );
    }
    return("?");
}

static const char *GetLanguage( struct asm_sym *sym )
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
    case LANG_STDCALL:
        return( "STDCALL" );
    case LANG_SYSCALL:
        return( "SYSCALL" );
    case LANG_WATCOM_C:
        return( "WATCOM_C" );
    default:
        return( "" );
    }
}

// display STRUCTs and UNIONs

static void log_struct( char * name, struct asm_sym *sym, int ofs )
/*****************************************************************/
{
    unsigned      i;
    dir_node      *dir;
    char          *pdots;
    struct_info   *si;
    field_list    *f;
    static int    prefix = 0;

    dir = (dir_node *)sym;

    /* filter typedefs and records */
    if ( dir->e.structinfo->typekind != TYPE_STRUCT &&
         dir->e.structinfo->typekind != TYPE_UNION )
        return;

    si = dir->e.structinfo;

    if (!name)
        name = sym->name;
    i = strlen ( name);
    pdots = dots + i + prefix + 1;
    if (i >= DOTSMAX)
        pdots = "";
    for (i=0;i<prefix;i++)
        LstPrintf(" ");
    if (prefix == 0)
        LstPrintf( "%s %s        %8X", name, pdots, sym->total_size );
    else
        LstPrintf( "%s %s        %8X", name, pdots, sym->offset + ofs);
    LstNL();
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
                    LstPrintf(" ");
                LstPrintf( "%s %s        %8X   ", f->sym->name, pdots, f->sym->offset + sym->offset + ofs);
                LstPrintf("%s", GetMemtypeString(f->sym, NULL));
                if ( f->sym->isarray )
                    LstPrintf( "[%u]",f->sym->total_length );
                LstNL();
            }
        }
    }
    prefix -= 2;
}

static void log_record( struct asm_sym **syms, struct asm_sym *sym )
/*****************************************************************/
{
    unsigned        mask;
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    field_list *f;

    if( si->typekind == TYPE_RECORD ) {
        int i = strlen ( sym->name);
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        for( i = 0,f = si->head; f; f = f->next,i++ );
        LstPrintf( "%s %s      %6X  %7X", sym->name, pdots, sym->total_size*8, i );
        LstNL();
        for( f = si->head; f; f = f->next ) {
            i = strlen ( f->sym->name ) + 2;
            pdots = dots + i + 1 ;
            if (i >= DOTSMAX)
                pdots = "";
            for (i=f->sym->offset, mask=0;i < f->sym->offset+f->sym->total_size;i++)
                mask |= 1 << i;
            LstPrintf( "  %s %s      %6X  %7X  %08X", f->sym->name, pdots, f->sym->offset, f->sym->total_size, mask);
            LstNL();
        }
    }
}

// a typedef is a simple struct with no fields. Size might be 0.

static void log_typedef( struct asm_sym **syms, struct asm_sym *sym )
{
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    char *p;
    char buffer[256];

    if( si->typekind == TYPE_TYPEDEF ) {
        int i = strlen ( sym->name );
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        buffer[0] = '\0';
        if (sym->mem_type == MT_PROC && si->target ) { /* typedef proto? */
            strcat(buffer,"Proc ");
            if ( *si->target->name ) {  /* the name may be "" */
                strcat(buffer, si->target->name);
                strcat(buffer," ");
            }
            strcat(buffer, GetMemtypeString( si->target, NULL ) );
            strcat(buffer," ");
            strcat(buffer, GetLanguage( si->target ) );
            p = buffer;
        } else
            p = GetMemtypeString(sym, buffer);
        LstPrintf( "%s %s    %8u %s", sym->name, pdots, sym->total_size, p );
        LstNL();
    }
}

static void log_segment( struct asm_sym *sym, struct asm_sym *group )
/*******************************************************************/
{
    char buffer[32];

    seg_info *seg = ((dir_node *)sym)->e.seginfo;

    if( seg->group == group ) {
        int i = strlen( sym->name );
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        LstPrintf( "%s %s        ", sym->name, pdots );
        if( seg->Use32 ) {
            //LstPrintf( "32 Bit   %08lX ", seg->current_loc );
            LstPrintf( "32 Bit   %08lX ", seg->segrec->d.segdef.seg_length );
        } else {
            //LstPrintf( "16 Bit   %04lX     ", seg->current_loc );
            LstPrintf( "16 Bit   %04lX     ", seg->segrec->d.segdef.seg_length );
        }
        LstPrintf( "%s   %s", get_seg_align( seg, buffer ), get_seg_combine( seg ) );
        LstPrintf( "'%s'", GetLname( seg->segrec->d.segdef.class_name_idx ) );
#if 0
        if ( group != NULL )
            LstPrintf( " %s", group->name );
#endif
        LstNL();
    }
}

static void log_group( struct asm_sym *grp )
/*****************************************************************/
{
    unsigned i;
    char *pdots;

    i = strlen ( grp->name);
    pdots = dots + i + 1;
    if (i >= DOTSMAX)
        pdots = "";
    LstPrintf( "%s %s        GROUP", grp->name, pdots );
    LstNL();
}

static const char *get_proc_type( struct asm_sym *sym )
/*****************************************************/
{
    /* if there's no segment associated with the symbol,
     add the symbol's offset size to the distance */
    switch( sym->mem_type ) {
    case MT_NEAR:
        if ( sym->segment == NULL )
            return( SymIs32( sym ) ? "Near32" : "Near16");
        return( "Near  " );
    case MT_FAR:
        if ( sym->segment == NULL )
            return( SymIs32( sym ) ? "Far32 " : "Far16 ");
        return( "Far   " );
    default:
        return( "      " );
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
    case SYM_UNDEFINED:
    case SYM_INTERNAL:
    case SYM_EXTERNAL:
        LstPrintf( "%s %s        ", sym->name, pdots );

        if ( sym->isarray ) {
            i = sprintf( buffer, "%s[%u]", GetMemtypeString( sym, NULL ), sym->total_length );
            LstPrintf( "%-10s ", buffer );
        } else
            LstPrintf( "%-10s ", GetMemtypeString( sym, NULL ) );

        LstPrintf( "%8X  ", sym->offset );

        if (sym->mem_type == MT_ABS)
            ;
        else
            LstPrintf( "%s\t", get_sym_seg_name( sym ) );

        if( sym->public )
            LstPrintf( "Public " );

        if ( sym->state == SYM_EXTERNAL ) {

            if (sym->weak == 1)
                LstPrintf( "*External " );
            else
                LstPrintf( "External  " );
        } else if ( sym->state == SYM_UNDEFINED ) {
            LstPrintf( "Undefined! " );
        }

        LstPrintf( "%s", GetLanguage( sym ) );
        LstNL();
        break;
    case SYM_TMACRO:
        GetTextMacroValue(sym->string_ptr, buffer);
        LstPrintf( "%s %s        Text   %s", sym->name, pdots, buffer );
        LstNL();
        break;
    case SYM_ALIAS:
        LstPrintf( "%s %s        Alias  %s", sym->name, pdots, sym->string_ptr );
        LstNL();
        break;
    }
}

/* list Procedures and Prototypes */

static void log_proc( struct asm_sym *sym )
/*****************************************/
{
    local_sym *f;
    struct asm_sym *l;
    char * p;
    dir_node *dir = (dir_node *)sym;
    int i = strlen( sym->name );
    char *pdots = dots + i + 1;

    if (i >= DOTSMAX)
        pdots = "";
    if (sym->use32)
        p = "%s %s        P %s %08X %-8s ";
    else
        p = "%s %s        P %s %04X     %-8s ";
    LstPrintf( p,
            sym->name,
            pdots,
            get_proc_type( sym ),
            sym->offset,
            get_sym_seg_name( sym ));
    if (sym->use32)
        LstPrintf( "%08X ", sym->total_size );
    else
        LstPrintf( "%04X     ", sym->total_size );
    if( sym->public ) {
        LstPrintf( "Public " );
    } else if ( dir->sym.isproc == TRUE ) {
        LstPrintf( "Private " );
    } else
        LstPrintf( "External " );

    LstPrintf( "%s", GetLanguage( sym ) );
    LstNL();
    if ( dir->sym.isproc == TRUE ) {
        if (dir->sym.langtype == LANG_C ||
            dir->sym.langtype == LANG_SYSCALL ||
            dir->sym.langtype == LANG_STDCALL) {
            int cnt;
            /* position f2 to last param */
            for (cnt = 0,f = dir->e.procinfo->paralist; f; f = (local_sym *)f->sym.next)
                cnt++;
            for (;cnt;cnt--) {
                int curr;
                for (curr = 1,f = dir->e.procinfo->paralist; curr < cnt;f = (local_sym *)f->sym.next,curr++);
                i = strlen ( f->sym.name);
                pdots = dots + i + 1 + 2;
                if (i >= DOTSMAX)
                    pdots = "";
                LstPrintf( "  %s %s        %-17s bp +%04X", f->sym.name, pdots, GetMemtypeString( &f->sym, NULL), f->sym.offset);
                LstNL();
            }
        } else {
            for ( f = dir->e.procinfo->paralist; f; f = (local_sym *)f->sym.next) {
                i = strlen ( f->sym.name);
                pdots = dots + i + 1 + 2;
                if (i >= DOTSMAX)
                    pdots = "";
                LstPrintf( "  %s %s        %-17s bp +%04X", f->sym.name, pdots, GetMemtypeString( &f->sym, NULL), f->sym.offset);
                LstNL();
            }
        }
        for ( f = dir->e.procinfo->locallist; f; f = (local_sym *)f->sym.next) {
            i = strlen ( f->sym.name);
            pdots = dots + i + 1 + 2;
            if (i >= DOTSMAX)
                pdots = "";
            LstPrintf( "  %s %s        %-17s bp -%04X", f->sym.name, pdots, GetMemtypeString(&f->sym, NULL), - f->sym.offset);
            LstNL();
        }
        for (l = dir->e.procinfo->labellist;l;l = l->next) {
            i = strlen ( l->name);
            pdots = dots + i + 1 + 2;
            if (i >= DOTSMAX)
                pdots = "";
            if (sym->use32)
                p = "  %s %s        L %s %08X %s";
            else
                p = "  %s %s        L %s %04X     %s";
            LstPrintf( p,
                    l->name,
                    pdots,
                    get_proc_type( l ),
                    l->offset,
                    get_sym_seg_name( l ));
            LstNL();
        }
    }
}

static void LstCaption( char *caption, int prefNL )
{
    for (; prefNL; prefNL--)
        LstNL();
    LstPrintf( caption );
    LstNL();
    LstNL();
}

/* write symbol table listing */
/* todo: move the text strings to msgdef.h */

void LstWriteCRef( void )
/***********************/
{
    struct asm_sym  **syms;
    dir_node        *dir;
    struct_info     *si;
    unsigned        i;
    qdesc           Structs  = { NULL, NULL };
    qdesc           Records  = { NULL, NULL };
    qdesc           Typedefs = { NULL, NULL };
    qdesc           Procs    = { NULL, NULL };
    qdesc           Macros   = { NULL, NULL };
    qdesc           Segs     = { NULL, NULL };
    qdesc           Grps     = { NULL, NULL };

    if( FileInfo.file[LST] == NULL || Options.no_symbol_listing == TRUE) {
        return; // no point going through the motions if lst file isn't open
    }

    /* go to EOF */
    fseek( FileInfo.file[LST], 0, SEEK_END );

    DebugMsg(("LstWriteCRef: calling SymSort\n"));

    syms = SymSort( &SymCount );
    if( !syms )
        return;

    for( i = 0; i < SymCount; ++i ) {
        qdesc *q;
        if (syms[i]->list == FALSE)
            continue;
        switch (syms[i]->state) {
        case SYM_TYPE:
            si = ((dir_node *)syms[i])->e.structinfo;
            if (si->typekind == TYPE_RECORD)
                q = &Records;
            else if (si->typekind == TYPE_TYPEDEF)
                q = &Typedefs;
            else
                q = &Structs;
            break;
        case SYM_PROC:
            q = &Procs;
            break;
        case SYM_MACRO:
            q = &Macros;
            break;
        case SYM_SEG:
            q = &Segs;
            break;
        case SYM_GRP:
            q = &Grps;
            break;
        default:
            continue;
        }
        if( q->head == NULL ) {
            q->head = syms[i];
        } else {
            ((dir_node *)q->tail)->next = (dir_node *)syms[i];
        }
        q->tail = syms[i];
        ((dir_node *)syms[i])->next = NULL;
    }
    if ( Macros.head ) {
        LstCaption( "Macros:", 2 );
        LstCaption( "                N a m e                 Type", 0 );
        /* write out macros */
        for( dir = Macros.head; dir ; dir = dir->next ) {
            log_macro( &dir->sym );
        }
    }
    if ( Structs.head ) {
        LstCaption( "Structures and Unions:", 2 );
        LstCaption( "                N a m e                 Size/Ofs   Type", 0 );
        /* write out structures */
        for( dir = Structs.head; dir; dir = dir->next ) {
            log_struct( NULL, &dir->sym, 0 );
        }
    }
    if ( Records.head ) {
        LstCaption( "Records:", 2 );
        LstPrintf( "                N a m e                 Width   # fields" );
        LstNL();
        LstCaption( "                                        Shift   Width    Mask   Initial", 0 );
        for( dir = Records.head; dir; dir = dir->next ) {
            log_record( syms, &dir->sym );
        }
    }
    if ( Typedefs.head ) {
        LstCaption( "Types:", 2 );
        LstCaption( "                N a m e              Size    Attr", 0 );
        for( dir = Typedefs.head; dir; dir = dir->next ) {
            log_typedef( syms, &dir->sym );
        }
    }
    /* write out segments & groups */
    LstCaption( "Segments and Groups:", 2 );
    LstCaption( "                N a m e                 Size     Length   Align   Combine Class", 0 );
    /* write out segments not within a group */
    for( dir = Segs.head; dir; dir = dir->next ) {
        log_segment( &dir->sym, NULL );
    }
    /* write out groups with associated segments */
    for( dir = Grps.head; dir; dir = dir->next ) {
        dir_node *dir2;
        log_group( &dir->sym );
        for( dir2 = Segs.head; dir2; dir2 = dir2->next ) {
            log_segment( &dir2->sym, &dir->sym );
        }
    }
    LstNL();

    if ( Procs.head ) {
        /* write out procedures and stuff */
        LstCaption( "Procedures, parameters and locals:", 2 );
        LstCaption( "                N a m e                 Type     Value    Segment  Length", 0 );
        for( dir = Procs.head; dir; dir = dir->next ) {
            log_proc( &dir->sym );
        }
        LstNL();
    }

    /* write out symbols */
    LstCaption( "Symbols:", 2 );
    LstCaption( "                N a m e                 Type       Value     Attr", 0 );
    for( i = 0; i < SymCount; ++i ) {
        if ( syms[i]->list == TRUE )
            log_symbol( syms[i] );
    }
    LstNL();

    /* free the sorted symbols */
    DebugMsg(("LstWriteCRef: free sorted symbols\n"));
    MemFree( syms );
}

void LstOpenFile( void )
/**********************/
{
    char buffer[128];

    list_pos = 0;
    if( FileInfo.fname[LST] != NULL && Options.write_listing ) {
        int namelen;
        FileInfo.file[LST] = fopen( FileInfo.fname[LST], "wb" );
        if ( FileInfo.file[LST] == NULL )
            Fatal( MSG_CANNOT_OPEN_FILE, FileInfo.fname[LST] );

        MsgGetJWasmName( buffer );
        list_pos = strlen( buffer );
        fwrite( buffer, 1, list_pos, FileInfo.file[LST] );
        LstNL();
        namelen = strlen( ModuleInfo.srcfile->name );
        fwrite( ModuleInfo.srcfile->name, 1, namelen, FileInfo.file[LST] );
        list_pos += namelen;
        LstNL();
    }

}

void LstCloseFile( void )
/**********************/
{
    if( FileInfo.file[LST] != NULL ) {
        fclose( FileInfo.file[LST] );
        FileInfo.file[LST] = NULL;
    }
}

