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
extern int      StructInit;     // see data.c

uint_32 list_pos; /* current pos in LST file */
uint_32 list_pos_start;

static unsigned         SymCount;

#define DOTSMAX 32
static const char  dots[] = " . . . . . . . . . . . . . . . .";
static const char  stdprefix[] = "%08X                  ";

enum typestring {
    TS_BYTE,   TS_WORD,   TS_DWORD,   TS_FWORD,
    TS_QWORD,  TS_TBYTE,  TS_PARA,    TS_OWORD,
    TS_PAGE,
    TS_NEAR,   TS_FAR,    TS_NEAR16,  TS_FAR16,  TS_NEAR32,  TS_FAR32,
    TS_LNEAR,  TS_LFAR,   TS_LNEAR16, TS_LFAR16, TS_LNEAR32, TS_LFAR32,
    TS_PTR,    TS_PROC,   TS_FUNC,    TS_NUMBER,
    TS_PRIVATE,TS_STACK,  TS_PUBLIC,  TS_EXTERNAL, TS_UNDEFINED,
    TS_GROUP,  TS_NOSEG,  TS_TEXT,    TS_ALIAS,  TS_ABS
};

static const char * const typestr[] = {
    "Byte", "Word", "DWord", "FWord",
    "QWord", "TByte", "Para", "XmmWord",
    "Page",
    "Near", "Far", "Near16", "Far16", "Near32", "Far32",
    "L Near", "L Far", "L Near16", "L Far16", "L Near32", "L Far32",
    "Ptr", "Proc", "Func", "Number",
    "Private", "Stack", "Public", "External", "Undefined",
    "GROUP", "No Seg", "Text", "Alias", "Abs"
};

/* don't change this order, must match enum lang_type in globals.h */
static const char * const langstr[] = {
    "", "C", "SYSCALL", "STDCALL", "PASCAL", "FORTRAN", "BASIC", "FASTCALL"
};

static const char basereg[] = {' ', 'e', 'r' };

void LstWrite( enum lsttype type, unsigned int oldofs, void * value )
/*******************************************************************/
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

    p = buffer + OFSSIZE + 2*CODEBYTES + 2;
    memset( buffer, ' ', OFSSIZE+ 2*CODEBYTES + 2 );

    switch ( type ) {
    case LSTTYPE_LIDATA:
        newofs = GetCurrOffset();
        len = sprintf( buffer, stdprefix, oldofs );
        p = buffer + len;

        if (CurrSeg == NULL)
            break;
        //if ( write_to_file == FALSE )
#ifdef DEBUG_OUT
        if ( Options.max_passes == 1 )
            ; // write a listing in pass 1
        else
#endif
        if ( Parse_Pass == PASS_1 )  /* changed v1.96 */
            break;

        len = CODEBYTES;
        p2 = buffer + 8 + 2;

        if ( CurrSeg->e.seginfo->CodeBuffer == NULL ) {
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
        idx = (CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc)
            - (newofs - oldofs);
        if (Options.output_format == OFORMAT_OMF) {
            while (idx < 0 && len) {
                sprintf( p2, "%02X", CurrSeg->e.seginfo->CodeBuffer[idx+LastCodeBufSize] );
                p2 += 2;
                idx++;
                oldofs++;
                len--;
            }
        } else if (idx < 0)
            idx = 0;

        while ( oldofs < newofs && len ) {
            sprintf( p2, "%02X", CurrSeg->e.seginfo->CodeBuffer[idx] );
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
            GetLiteralValue( buffer2, sym->string_ptr );
            sprintf( buffer, " = %-" PREFFMTSTR ".80s", buffer2 );
            p = buffer + strlen( buffer );
        }
        break;
    case LSTTYPE_MACROLINE:
        buffer[1] = '>';
        break;
    case LSTTYPE_STRUCT:
        len = sprintf( buffer, stdprefix, oldofs );
        break;
    case LSTTYPE_LABEL:
        oldofs = GetCurrOffset();
    default:
        if ( type != LSTTYPE_MACRO && (CurrSeg || value) )
            len = sprintf(buffer, stdprefix, oldofs);
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
    if ( !p )
        p = "";
    //while( isspace( *p ) ) p++;
    len = strlen( p );

    /* calc new list position */
    if ( MacroLevel ) {
        list_pos += sprintf( buffer, "%u ", MacroLevel );
    }
    if ( GeneratedCode || StructInit )
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
        fwrite( buffer, 1, strlen( buffer ), FileInfo.file[LST] );

    /* if it's generated code, write a '*' before the line */
    if ( GeneratedCode || StructInit )
        fwrite( "*", 1, 1, FileInfo.file[LST] );

    fwrite( p, 1, len, FileInfo.file[LST] );
    fwrite( NLSTR, 1, NLSIZ, FileInfo.file[LST] );

    DebugMsg(("LstWrite: writing >%s<, new pos=%u\n", p, list_pos ));
    return;
}

void LstWriteSrcLine( void )
/**************************/
{
    LstWrite( LSTTYPE_MACRO, 0, NULL );
}

void LstPrintf( const char *format, ... )
/***************************************/
{
    va_list     args;

    if( FileInfo.file[LST] ) {
        va_start( args, format );
        list_pos += vfprintf( FileInfo.file[LST], format, args );
        va_end( args );
    }
}
void LstNL( void )
/****************/
{
    if( FileInfo.file[LST] ) {
        fwrite( NLSTR, 1, NLSIZ, FileInfo.file[LST] );
        list_pos += NLSIZ;
    }
}

static const char *get_seg_align( seg_info *seg, char * buffer )
/**************************************************************/
{
    switch( seg->alignment ) {
    case 0:    return( typestr[TS_BYTE]  );
    case 1:    return( typestr[TS_WORD]  );
    case 2:    return( typestr[TS_DWORD] );
    case 3:    return( typestr[TS_QWORD] );
    case 4:    return( typestr[TS_PARA]  );
    case 8:    return( typestr[TS_PAGE]  );
    case MAX_SEGALIGNMENT:
               return( typestr[TS_ABS]   );
    default:
        sprintf( buffer, "%u", 1 << seg->alignment );
        return( buffer );
    }
}

static const char *get_seg_combine( seg_info *seg )
/*************************************************/
{
    switch( seg->segrec->d.segdef.combine ) {
    case COMB_INVALID:    return( typestr[TS_PRIVATE] );
    case COMB_STACK:      return( typestr[TS_STACK]   );
    case COMB_ADDOFF:     return( typestr[TS_PUBLIC]  );
    }
    return( "?" );
}

static void log_macro( struct asm_sym *sym )
/******************************************/
{
    int i = strlen ( sym->name);
    const char *pdots = dots + i + 1;
    const char *type = (sym->isfunc) ? typestr[TS_FUNC] : typestr[TS_PROC];

    if (i >= DOTSMAX)
        pdots = "";
    LstPrintf( "%s %s        %s", sym->name, pdots ,type);
#ifdef DEBUG_OUT
    LstPrintf( " %5u", ((dir_node *)sym)->e.macroinfo->count );
#endif
    LstNL();
    return;
}

// called by log_struct and log_typedef
// that is, the symbol is ensured to be a TYPE!

static const char * GetMemtypeString(asm_sym * sym, char * buffer)
/****************************************************************/
{
    if ( (sym->mem_type & MT_SPECIAL) == 0 ) {
        int size = (sym->mem_type & MT_SIZE_MASK) + 1;
        switch ( size ) {
        case 1: return( typestr[TS_BYTE] );
        case 2: return( typestr[TS_WORD] );
        case 4: return( typestr[TS_DWORD] );
        case 6: return( typestr[TS_FWORD] );
        case 8: return( typestr[TS_QWORD] );
        case 10:return( typestr[TS_TBYTE] );
        case 16:return( typestr[TS_OWORD] );
        }
    }
    switch ( sym->mem_type ) {
    case MT_PTR:
        if ( buffer ) {
#if AMD64_SUPPORT
            if ( sym->Ofssize == USE64 )
                strcat( buffer, typestr[TS_NEAR] );
            else
#endif
            if ( sym->isfar )
                if ( sym->Ofssize )
                    strcat( buffer, typestr[TS_FAR32] );
                else
                    strcat( buffer, typestr[TS_FAR16] );
            else
                if ( sym->Ofssize )
                    strcat( buffer, typestr[TS_NEAR32] );
                else
                    strcat( buffer, typestr[TS_NEAR16] );
            strcat(buffer," ");
            strcat(buffer, typestr[TS_PTR] );
            return(buffer);
        }
        return( typestr[TS_PTR] );
    case MT_FAR:
    is_far:
        if ( sym->segment )
            return( typestr[TS_LFAR] );
        if ( GetSymOfssize( sym ) > USE16 )
            return( typestr[TS_LFAR32] );
        return( typestr[TS_LFAR16] );
    case MT_PROC:
        if ( ModuleInfo.model == MOD_MEDIUM ||
             ModuleInfo.model == MOD_LARGE ||
             ModuleInfo.model == MOD_HUGE)
            goto is_far;
        /* fall through */
    case MT_NEAR:
        if ( sym->segment )
            return( typestr[TS_LNEAR] );
        if ( GetSymOfssize( sym ) > USE16 )
            return( typestr[TS_LNEAR32] );
        return( typestr[TS_LNEAR16] );
    case MT_TYPE:
        if (*(sym->type->name))  /* it must be set, but to be safe ... */
            return(sym->type->name);
        return( typestr[TS_PTR] );
    case MT_ABS:
    case MT_EMPTY: /* relocatable number, assigned with '=' directive */
        return( typestr[TS_NUMBER] );
    }
    return("?");
}

static const char *GetLanguage( struct asm_sym *sym )
/***************************************************/
{
    if (sym->langtype <= 7 )
        return( langstr[sym->langtype] );
    return( "?" );
}

// display STRUCTs and UNIONs

static void log_struct( char * name, struct asm_sym *sym, int ofs )
/*****************************************************************/
{
    unsigned      i;
    dir_node      *dir;
    const char    *pdots;
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
        if ( dir->e.structinfo->alignment > 1)
            LstPrintf( "%s %s        %8X (%u)", name, pdots, sym->total_size, si->alignment );
        else
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
/******************************************************************/
{
    unsigned        mask;
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    field_list *f;

    if( si->typekind == TYPE_RECORD ) {
        int i = strlen ( sym->name);
        const char *pdots = dots + i + 1;
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
/*******************************************************************/
{
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    const char *p;
    char buffer[256];

    if( si->typekind == TYPE_TYPEDEF ) {
        int i = strlen ( sym->name );
        const char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        buffer[0] = '\0';
        if (sym->mem_type == MT_PROC && si->target ) { /* typedef proto? */
            strcat(buffer, typestr[TS_PROC] );
            strcat(buffer, " " );
            if ( *si->target->name ) {  /* the name may be "" */
                strcat( buffer, si->target->name );
                strcat( buffer," ");
            }
            strcat(buffer, GetMemtypeString( si->target, NULL ) );
            strcat(buffer," ");
            strcat(buffer, GetLanguage( si->target ) );
            p = buffer;
        } else
            p = GetMemtypeString( sym, buffer);
        LstPrintf( "%s %s    %8u  %s", sym->name, pdots, sym->total_size, p );
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
        const char *pdots = dots + i + 1;
        if (i >= DOTSMAX)
            pdots = "";
        LstPrintf( "%s %s        ", sym->name, pdots );
        if( seg->Ofssize == USE32 ) {
            //LstPrintf( "32 Bit   %08lX ", seg->current_loc );
            LstPrintf( "32 Bit   %08lX ", sym->max_offset );
#if AMD64_SUPPORT
        } else if( seg->Ofssize == USE64 ) {
            LstPrintf( "64 Bit   %08lX ", sym->max_offset );
#endif
        } else {
            //LstPrintf( "16 Bit   %04lX     ", seg->current_loc );
            LstPrintf( "16 Bit   %04lX     ", sym->max_offset );
        }
        LstPrintf( "%-7s %-8s", get_seg_align( seg, buffer ), get_seg_combine( seg ) );
        LstPrintf( "'%s'", GetLname( seg->segrec->d.segdef.class_name_idx ) );
#if 0
        if ( group != NULL )
            LstPrintf( " %s", group->name );
#endif
        LstNL();
    }
}

static void log_group( struct asm_sym *grp )
/******************************************/
{
    unsigned i;
    const char *pdots;

    i = strlen ( grp->name);
    pdots = dots + i + 1;
    if (i >= DOTSMAX)
        pdots = "";
    LstPrintf( "%s %s        %s", grp->name, pdots, typestr[TS_GROUP] );
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
            return( GetSymOfssize( sym ) ? typestr[TS_NEAR32] : typestr[TS_NEAR16] );
        return( typestr[TS_NEAR] );
    case MT_FAR:
        if ( sym->segment == NULL )
            return( GetSymOfssize( sym ) ? typestr[TS_FAR32] : typestr[TS_FAR16] );
        return( typestr[TS_FAR] );
    }
    return( " " );
}

static const char *get_sym_seg_name( struct asm_sym *sym )
/********************************************************/
{
    if( sym->segment ) {
        return( sym->segment->name );
    } else {
        return( typestr[TS_NOSEG] );
    }
}

/* list symbols */

static void log_symbol( struct asm_sym *sym )
/*******************************************/
{
    int i = strlen( sym->name );
    const char *pdots = dots + i + 1;
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

        if ( ( sym->mem_type == MT_ABS ) && sym->sign )
            LstPrintf( "-%08Xh ", 0 - sym->uvalue );
        else
            LstPrintf( " %8Xh ", sym->offset );

        if (sym->mem_type == MT_ABS)
            ;
        else
            LstPrintf( "%s ", get_sym_seg_name( sym ) );

        if( sym->public )
            LstPrintf( "%s ", typestr[TS_PUBLIC] );

        if ( sym->state == SYM_EXTERNAL ) {
            if (sym->weak == 1)
                LstPrintf( "*%s ", typestr[TS_EXTERNAL] );
            else
                LstPrintf( "%s ", typestr[TS_EXTERNAL] );
        } else if ( sym->state == SYM_UNDEFINED ) {
            LstPrintf( "%s ", typestr[TS_UNDEFINED] );
        }

        LstPrintf( "%s", GetLanguage( sym ) );
        LstNL();
        break;
    case SYM_TMACRO:
        GetLiteralValue( buffer, sym->string_ptr );
        LstPrintf( "%s %s        %s   %s", sym->name, pdots, typestr[TS_TEXT], buffer );
        LstNL();
        break;
    case SYM_ALIAS:
        LstPrintf( "%s %s        %s  %s", sym->name, pdots, typestr[TS_ALIAS], sym->string_ptr );
        LstNL();
        break;
    }
}

/* list Procedures and Prototypes */

static void log_proc( struct asm_sym *sym )
/*****************************************/
{
    dir_node *f;
    struct dir_node *l;
    const char * p;
    dir_node *dir = (dir_node *)sym;
    int i = strlen( sym->name );
    const char *pdots = dots + i + 1;

    if ( i >= DOTSMAX )
        pdots = "";
    if ( sym->Ofssize )
        p = "%s %s        P %-6s %08X %-8s ";
    else
        p = "%s %s        P %-6s %04X     %-8s ";
    LstPrintf( p,
            sym->name,
            pdots,
            get_proc_type( sym ),
            sym->offset,
            get_sym_seg_name( sym ));
    if ( sym->Ofssize )
        LstPrintf( "%08X ", sym->total_size );
    else
        LstPrintf( "%04X     ", sym->total_size );

    if( sym->public ) {
        LstPrintf( "%-9s", typestr[TS_PUBLIC] );
    } else if ( dir->sym.isproc == TRUE ) {
        LstPrintf( "%-9s", typestr[TS_PRIVATE] );
    } else
        LstPrintf( "%-9s", typestr[TS_EXTERNAL] );

    LstPrintf( "%s", GetLanguage( sym ) );
    LstNL();
    if ( dir->sym.isproc == TRUE ) {
        if (dir->sym.langtype == LANG_C ||
            dir->sym.langtype == LANG_SYSCALL ||
            dir->sym.langtype == LANG_STDCALL ||
            dir->sym.langtype == LANG_FASTCALL) {
            int cnt;
            /* position f2 to last param */
            for ( cnt = 0, f = dir->e.procinfo->paralist; f; f = f->nextparam )
                cnt++;
            for (;cnt;cnt--) {
                int curr;
                for (curr = 1,f = dir->e.procinfo->paralist; curr < cnt;f = f->nextparam, curr++ );
                i = strlen ( f->sym.name);
                pdots = dots + i + 1 + 2;
                if (i >= DOTSMAX)
                    pdots = "";
                /* FASTCALL: parameter may be a text macro (=register name) */
                if ( f->sym.state == SYM_TMACRO )
                    LstPrintf( "  %s %s        %-17s %s", f->sym.name, pdots, GetMemtypeString( &f->sym, NULL), f->sym.string_ptr);
                else
                    LstPrintf( "  %s %s        %-17s %cbp +%04X", f->sym.name, pdots, GetMemtypeString( &f->sym, NULL), basereg[dir->sym.Ofssize], f->sym.offset);
                LstNL();
            }
        } else {
            for ( f = dir->e.procinfo->paralist; f; f = f->nextparam ) {
                i = strlen ( f->sym.name);
                pdots = dots + i + 1 + 2;
                if (i >= DOTSMAX)
                    pdots = "";
                LstPrintf( "  %s %s        %-17s %cbp +%04X", f->sym.name, pdots, GetMemtypeString( &f->sym, NULL), basereg[dir->sym.Ofssize], f->sym.offset);
                LstNL();
            }
        }
        for ( l = dir->e.procinfo->locallist; l; l = l->nextlocal ) {
            char buffer[32];
            i = strlen ( l->sym.name);
            pdots = dots + i + 1 + 2;
            if (i >= DOTSMAX)
                pdots = "";
            if ( l->sym.isarray )
                sprintf( buffer, "%s[%u]", GetMemtypeString(&l->sym, NULL), l->sym.total_length );
            else
                strcpy( buffer, GetMemtypeString(&l->sym, NULL) );
            LstPrintf( "  %s %s        %-17s %cbp -%04X", l->sym.name, pdots, buffer, basereg[dir->sym.Ofssize], - l->sym.offset);
            LstNL();
        }
        for ( l = dir->e.procinfo->labellist; l ; l = l->nextll ) {
            dir_node *l2;
            for ( l2 = l; l2; l2 = (dir_node *)l2->sym.next ) {
                /* filter params and locals! */
                if ( l2->sym.state == SYM_STACK || l2->sym.state == SYM_TMACRO)
                    continue;
                i = strlen ( l2->sym.name);
                pdots = dots + i + 1 + 2;
                if (i >= DOTSMAX)
                    pdots = "";
                if ( sym->Ofssize )
                    p = "  %s %s        L %-6s %08X %s";
                else
                    p = "  %s %s        L %-6s %04X     %s";
                LstPrintf( p,
                          l2->sym.name,
                          pdots,
                          get_proc_type( &l2->sym ),
                          l2->sym.offset,
                          get_sym_seg_name( &l2->sym ));
                LstNL();
            }
        }
    }
}

static void LstCaption( char *caption, int prefNL )
/*************************************************/
{
    for (; prefNL; prefNL--)
        LstNL();
    LstPrintf( caption );
    LstNL();
    LstNL();
}

/* write symbol table listing */

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
        case SYM_MACRO:
            q = &Macros;
            break;
        case SYM_SEG:
            q = &Segs;
            break;
        case SYM_GRP:
            q = &Grps;
            break;
        case SYM_INTERNAL:
            if ( syms[i]->isproc ) {
                q = &Procs;
                break;
            }
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
        LstCaption( MsgGetEx( TXT_MACROS ), 2 );
        LstCaption( MsgGetEx( TXT_MACROCAP ), 0 );
        /* write out macros */
        for( dir = Macros.head; dir ; dir = dir->next ) {
            log_macro( &dir->sym );
        }
    }
    if ( Structs.head ) {
        LstCaption( MsgGetEx( TXT_STRUCTS ), 2 );
        LstCaption( MsgGetEx( TXT_STRUCTCAP ), 0 );
        /* write out structures */
        for( dir = Structs.head; dir; dir = dir->next ) {
            log_struct( NULL, &dir->sym, 0 );
        }
    }
    if ( Records.head ) {
        LstCaption( MsgGetEx( TXT_RECORDS ), 2 );
        LstPrintf( MsgGetEx( TXT_RECORDCAP1 ) );
        LstNL();
        LstCaption( MsgGetEx( TXT_RECORDCAP2 ), 0 );
        for( dir = Records.head; dir; dir = dir->next ) {
            log_record( syms, &dir->sym );
        }
    }
    if ( Typedefs.head ) {
        LstCaption( MsgGetEx( TXT_TYPEDEFS ), 2 );
        LstCaption( MsgGetEx( TXT_TYPEDEFCAP ), 0 );
        for( dir = Typedefs.head; dir; dir = dir->next ) {
            log_typedef( syms, &dir->sym );
        }
    }
    /* write out segments & groups */
    LstCaption( MsgGetEx( TXT_SEGS ), 2 );
    LstCaption( MsgGetEx( TXT_SEGCAP ), 0 );
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
        LstCaption( MsgGetEx( TXT_PROCS) , 2 );
        LstCaption( MsgGetEx( TXT_PROCCAP), 0 );
        for( dir = Procs.head; dir; dir = dir->next ) {
            log_proc( &dir->sym );
        }
        LstNL();
    }

    /* write out symbols */
    LstCaption( MsgGetEx( TXT_SYMBOLS ), 2 );
    LstCaption( MsgGetEx( TXT_SYMCAP ), 0 );
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
    const FNAME *fn;
    char buffer[128];

    list_pos = 0;
    if( FileInfo.fname[LST] != NULL && Options.write_listing ) {
        int namelen;
        FileInfo.file[LST] = fopen( FileInfo.fname[LST], "wb" );
        if ( FileInfo.file[LST] == NULL )
            Fatal( FATAL_CANNOT_OPEN_FILE, FileInfo.fname[LST], errno );

        MsgGetJWasmName( buffer );
        list_pos = strlen( buffer );
        fwrite( buffer, 1, list_pos, FileInfo.file[LST] );
        LstNL();
        fn = GetFName( ModuleInfo.srcfile );
        namelen = strlen( fn->name );
        fwrite( fn->name, 1, namelen, FileInfo.file[LST] );
        list_pos += namelen;
        LstNL();
    }

}

void LstCloseFile( void )
/***********************/
{
    if( FileInfo.file[LST] != NULL ) {
        fclose( FileInfo.file[LST] );
        FileInfo.file[LST] = NULL;
    }
}

