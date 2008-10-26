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
#include "tokenize.h"
#include "symbols.h"
#include "macro.h"
#include "queues.h"
#include "fatal.h"
#include "listing.h"

#define CODEBYTES 8
#define OFSSIZE 8
#define PREFFMTSTR "23"  /* OFSSIZE + 2 * CODEBYTES - 3 */

extern uint_32          LastCodeBufSize;

#define DOTSMAX 32

static unsigned         SymCount;

static char             dots[] = " . . . . . . . . . . . . . . . .";

void LstOpenFile( void )
/**********************/
{
    if( AsmFiles.fname[LST] != NULL && Options.write_listing ) {
        AsmFiles.file[LST] = fopen( AsmFiles.fname[LST], "w" );
        if ( AsmFiles.file[LST] == NULL )
            Fatal( MSG_CANNOT_OPEN_FILE, AsmFiles.fname[LST] );
    }
}

void LstCloseFile( void )
/**********************/
{
    if( AsmFiles.file[LST] != NULL ) {
        fclose( AsmFiles.file[LST] );
        AsmFiles.file[LST] = NULL;
    }
}

void LstWriteFile( int type, unsigned int oldofs, void * value )
{
    unsigned int newofs;
    asm_sym * sym = value;
    int len;
    int idx;
    char * p;
    char buffer[128];

    if (ModuleInfo.list == FALSE || AsmFiles.file[LST] == NULL)
        return;
    switch ( type ) {
    case LSTTYPE_LIDATA:
        newofs = GetCurrOffset();
        len = sprintf( buffer, "%08X: ", oldofs );
        fwrite( buffer, 1, len, AsmFiles.file[LST] );

        len = CODEBYTES;

        if (CurrSeg == NULL)
            return;

        if ( CurrSeg->seg->e.seginfo->segtype == SEGTYPE_BSS ) {
            while (oldofs < newofs && len) {
                sprintf( buffer, "%02X", 0 );
                // sprintf(buffer, "??");
                fwrite( buffer, 1, 2, AsmFiles.file[LST] );
                oldofs++;
                len--;
            }
            goto nodump;
        }

        if ( write_to_file == FALSE )
            goto nodump;

        /* OMF hold just a small buffer for one LEDATA record */
        /* if it has been flushed, use LastCodeBufSize */
        idx = (CurrSeg->seg->e.seginfo->current_loc - CurrSeg->seg->e.seginfo->start_loc)
            - (newofs - oldofs);
        if (Options.output_format == OFORMAT_OMF) {
            while (idx < 0 && len) {
                sprintf( buffer, "%02X", CurrSeg->seg->e.seginfo->CodeBuffer[idx+LastCodeBufSize] );
                fwrite( buffer, 1, 2, AsmFiles.file[LST] );
                idx++;
                oldofs++;
                len--;
            }
        } else if (idx < 0)
            idx = 0;

        while ( oldofs < newofs && len ) {
            sprintf( buffer, "%02X", CurrSeg->seg->e.seginfo->CodeBuffer[idx] );
            fwrite( buffer, 1, 2, AsmFiles.file[LST] );
            idx++;
            oldofs++;
            len--;
        }
    nodump:
        for ( ; len ; len-- )
            fwrite( "  ", 1, 2, AsmFiles.file[LST] );
        break;
    case LSTTYPE_EQUATE:
        if ( sym->state == SYM_INTERNAL ) {
            sprintf( buffer, " = %-" PREFFMTSTR "X", sym->value );
            fwrite( buffer, 1, strlen(buffer), AsmFiles.file[LST] );
        } else if ( sym->state == SYM_TMACRO ) {
            char buffer2[MAX_LINE_LEN];
            GetTextMacroValue( sym->string_ptr, buffer2 );
            sprintf( buffer, " = %-" PREFFMTSTR ".80s", buffer2 );
            fwrite( buffer, 1, strlen(buffer), AsmFiles.file[LST] );
        }
        break;
    default:
        if ( type != LSTTYPE_MACRO && (CurrSeg || value) )
            len = sprintf(buffer, "%08X:                 ", oldofs);
        else
            memset( buffer, ' ', OFSSIZE+2*CODEBYTES+2 );
        fwrite( buffer, 1, OFSSIZE+2*CODEBYTES+2, AsmFiles.file[LST] );
    }

    fwrite( " ", 1, 1, AsmFiles.file[LST] );
    p = CurrSource;
    while( isspace( *p ) ) p++;
    fwrite( p, 1, strlen(p), AsmFiles.file[LST] );
    fwrite( "\n", 1, 1, AsmFiles.file[LST] );
    return;
}

static void LstMsg( const char *format, ... )
/************************************/
{
    va_list     args;

    if( AsmFiles.file[LST] ) {
        va_start( args, format );
        vfprintf( AsmFiles.file[LST], format, args );
        va_end( args );
    }
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
    seg_info *seg = ((dir_node *)sym)->e.seginfo;

    if( seg->group == group ) {
        int i = strlen( sym->name );
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        LstMsg( "%s %s        ", sym->name, pdots );
        if( seg->Use32 ) {
            //LstMsg( "32 Bit   %08lX ", seg->current_loc );
            LstMsg( "32 Bit   %08lX ", seg->segrec->d.segdef.seg_length );
        } else {
            //LstMsg( "16 Bit   %04lX     ", seg->current_loc );
            LstMsg( "16 Bit   %04lX     ", seg->segrec->d.segdef.seg_length );
        }
        LstMsg( "%s   %s", get_seg_align( seg ), get_seg_combine( seg ) );
        LstMsg( "'%s'", GetLname( seg->segrec->d.segdef.class_name_idx ) );
#if 0
        if ( group != NULL )
            LstMsg( " %s", group->name );
#endif
        LstMsg( "\n" );
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
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    char buffer[128];

    if( si->typekind == TYPE_TYPEDEF ) {
        int i = strlen ( sym->name );
        char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        buffer[0] = '\0';
        LstMsg( "%s %s    %8u %s\n", sym->name, pdots, sym->total_size, GetMemTypeString(sym, buffer));
    }
}

static void log_group( struct asm_sym **syms, struct asm_sym *grp )
/*****************************************************************/
{
    unsigned i;
    char *pdots;

    i = strlen ( grp->name);
    pdots = dots + i + 1;
    if (i >= DOTSMAX)
        pdots = "";
    LstMsg( "%s %s        ", grp->name, pdots );
    LstMsg( "GROUP\n" );
    for( i = 0; i < SymCount; ++i ) {
        if( syms[i]->state == SYM_SEG )
            log_segment( syms[i], grp );
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
    case MT_EMPTY: /* relocatable number, assigned with '=' directive */
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
    label_list *f;
    struct asm_sym *l;
    char * p;
    dir_node *dir = (dir_node *)sym;
    int i = strlen( sym->name );
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
    } else if ( dir->sym.isproc == TRUE ) {
        LstMsg( "Private " );
    } else
        LstMsg( "External " );

    LstMsg( "%s", get_sym_lang( sym ) );
    LstMsg( "\n" );
    if ( dir->sym.isproc == TRUE ) {
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

/* output the symbol table listing */

void LstWriteCRef( void )
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

    DebugMsg(("LstWriteCRef: calling SymSort\n"));

    syms = SymSort( &SymCount );
    if( syms ) {
        for( i = 0; i < SymCount; ++i ) {
            if (syms[i]->list == FALSE)
                continue;
            switch (syms[i]->state) {
            case SYM_TYPE:
                si = ((dir_node *)syms[i])->e.structinfo;
                if (si->typekind == TYPE_RECORD)
                    cntRecords++;
                else if (si->typekind == TYPE_TYPEDEF)
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
            for( i = 0; i < SymCount; ++i ) {
                if ( syms[i]->list == TRUE && syms[i]->state == SYM_MACRO )
                    log_macro( syms[i] );
            }
        }
        if (cntStructs) {
            LstMsg( "\n\nStructures and Unions:\n\n" );
            LstMsg( "                N a m e                 Size/Ofs   Type\n\n" );
            /* write out structures */
            for( i = 0; i < SymCount; ++i ) {
                if ( syms[i]->list == TRUE && syms[i]->state == SYM_TYPE )
                    log_struct( NULL, syms[i], 0 );
            }
        }
        if (cntRecords) {
            LstMsg( "\n\nRecords:\n\n" );
            LstMsg( "                N a m e                 Width   # fields\n" );
            LstMsg( "                                        Shift   Width    Mask   Initial\n\n" );
            for( i = 0; i < SymCount; ++i ) {
                if ( syms[i]->list == TRUE && syms[i]->state == SYM_TYPE )
                    log_record( syms, syms[i] );
            }
        }
        if (cntTypedefs) {
            LstMsg( "\n\nTypes:\n\n" );
            LstMsg( "                N a m e              Size    Attr\n\n" );
            for( i = 0; i < SymCount; ++i ) {
                if ( syms[i]->list == TRUE && syms[i]->state == SYM_TYPE )
                    log_typedef( syms, syms[i] );
            }
        }
        /* write out the segments */
        LstMsg( "\n\nSegments and Groups:\n\n" );
        LstMsg( "                N a m e                 Size" );
        LstMsg( "     Length   Align   Combine Class\n\n" );
        /* write out groups with associated segments */
        for( i = 0; i < SymCount; ++i ) {
            if( syms[i]->state == SYM_GRP )
                log_group( syms, syms[i] );
        }
        /* write out remaining segments, outside any group */
        for( i = 0; i < SymCount; ++i ) {
            if( syms[i]->state == SYM_SEG )
                log_segment( syms[i], NULL );
        }
        LstMsg( "\n" );

        if (cntProcs) {
            /* next write out procedures and stuff */
            LstMsg( "\n\nProcedures, parameters and locals:\n\n" );
            LstMsg( "                N a m e                 Type" );
            LstMsg( "     Value    Attr\n\n" );
            for( i = 0; i < SymCount; ++i ) {
                if (syms[i]->list == TRUE && syms[i]->state == SYM_PROC )
                    log_proc( syms[i] );
            }
            LstMsg( "\n" );
        }

        /* next write out symbols */
        LstMsg( "\n\nSymbols:\n\n" );
        LstMsg( "                N a m e                 Type" );
        LstMsg( "       Value     Attr\n\n" );
        for( i = 0; i < SymCount; ++i ) {
            if (syms[i]->list == TRUE)
                log_symbol( syms[i] );
        }
        LstMsg( "\n" );

        DebugMsg(("LstWriteCRef: free sorted symbols\n"));

        /* free the sorted symbols */
        MemFree( syms );
    }
}

