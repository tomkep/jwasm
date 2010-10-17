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
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "tokenize.h"
#include "symbols.h"
#include "macro.h"
#include "fatal.h"
#include "fastpass.h"
#include "listing.h"
#include "input.h"
#include "msgtext.h"
#include "types.h"
#include "omfspec.h"

#define CODEBYTES 10
#define OFSSIZE 8
#define PREFFMT OFSSIZE + 2 + 2 * CODEBYTES + 1
#define PREFFMTSTR "27"  /* OFSSIZE + 2 * CODEBYTES - 3 */

#ifdef __UNIX__
#define NLSIZ 1
#define NLSTR "\n"
#else
#define NLSIZ 2
#define NLSTR "\r\n"
#endif

extern uint_32  LastCodeBufSize;
extern int      StructInit;     /* see data.c */
extern char     CurrComment[];

uint_32 list_pos; /* current pos in LST file */
uint_32 list_pos_start;

#define DOTSMAX 32
static const char  dots[] = " . . . . . . . . . . . . . . . .";

/* spaces behind %08X must be 2 + CODEBYTES*2 */
static const char  stdprefix[] = "%08" FX32 "                      ";

static const char  szFmtProcStk[] = "  %s %s        %-17s %s %c %04" FX32;

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

static const char szCOMM[] = "COMM";
static const char szVararg[] ="VARARG";
static const char szCount[] = "count";

/* don't change this order, must match enum lang_type in globals.h */
static const char * const langstr[] = {
    "", "C", "SYSCALL", "STDCALL", "PASCAL", "FORTRAN", "BASIC", "FASTCALL"
};

static const short basereg[] = { T_BP, T_EBP,
#if AMD64_SUPPORT
T_RBP
#endif
};

/* cref definitions */

enum list_queues {
    LQ_MACROS,
    LQ_STRUCTS,
    LQ_RECORDS,
    LQ_TYPEDEFS,
    LQ_SEGS,
    LQ_GRPS,
    LQ_PROCS,
    LQ_LAST
};

enum pr_flags {
    PRF_ADDSEG = 0x01
};

struct print_item {
    short type;
    short flags;
    const short *capitems;
    void (*function)();
};


static const short maccap[] = { TXT_MACROS,  TXT_MACROCAP  ,0 };
static const short strcap[] = { TXT_STRUCTS, TXT_STRUCTCAP, 0 };
static const short reccap[] = { TXT_RECORDS, TXT_RECORDCAP1, -1, TXT_RECORDCAP2, 0 };
static const short tdcap[]  = { TXT_TYPEDEFS,TXT_TYPEDEFCAP, 0 };
static const short segcap[] = { TXT_SEGS,    TXT_SEGCAP, 0 };
static const short prccap[] = { TXT_PROCS,   TXT_PROCCAP, 0 };

static void log_macro(   struct asm_sym * );
static void log_struct(  struct asm_sym *, char *name, int );
static void log_record(  struct asm_sym * );
static void log_typedef( struct asm_sym * );
static void log_segment( struct asm_sym *, struct asm_sym *group );
static void log_group(   struct asm_sym *, dir_node * );
static void log_proc(    struct asm_sym * );

static const struct print_item cr[] = {
    { LQ_MACROS,          0, maccap, log_macro   },
    { LQ_STRUCTS,         0, strcap, log_struct  },
    { LQ_RECORDS,         0, reccap, log_record  },
    { LQ_TYPEDEFS,        0, tdcap,  log_typedef },
    { LQ_SEGS,            0, segcap, log_segment },
    { LQ_GRPS,   PRF_ADDSEG, NULL,   log_group   },
    { LQ_PROCS,           0, prccap, log_proc    },
};


void LstWrite( enum lsttype type, uint_32 oldofs, void * value )
/**************************************************************/
{
    uint_32 newofs;
    asm_sym * sym = value;
    int len;
    int len2;
    int idx;
    char * p;
    char * p2;
    char buffer[128];

    if ( ModuleInfo.list == FALSE || FileInfo.file[LST] == NULL )
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

    ModuleInfo.line_listed = TRUE;

    DebugMsg1(("LstWrite: enter, pos=%" FU32 "\n", list_pos ));
#if FASTPASS
    if ( ( Parse_Pass > PASS_1 ) && UseSavedState ) {
        if ( GeneratedCode == 0 ) {
            list_pos = LineStoreCurr->list_pos;
            DebugMsg1(("LstWrite: Pass=%u, stored pos=%" FU32 "\n", Parse_Pass+1, list_pos ));
        }
        fseek( FileInfo.file[LST], list_pos, SEEK_SET );
    }
#endif

    p = buffer + OFSSIZE + 2*CODEBYTES + 2;
    memset( buffer, ' ', OFSSIZE+ 2*CODEBYTES + 2 );

    switch ( type ) {
    case LSTTYPE_CODE:
    case LSTTYPE_DATA:
        newofs = GetCurrOffset();
        len = sprintf( buffer, stdprefix, oldofs );
        p = buffer + len;

        if ( CurrSeg == NULL )
            break;
        //if ( write_to_file == FALSE )
#ifdef DEBUG_OUT
        if ( Options.max_passes == 1 )
            ; /* write a listing in pass 1 */
        else
#endif
        if ( Parse_Pass == PASS_1 )  /* changed v1.96 */
            break;

        len = CODEBYTES;
        p2 = buffer + OFSSIZE + 2;

        if ( CurrSeg->e.seginfo->CodeBuffer == NULL ||
            CurrSeg->e.seginfo->written == FALSE ) {
            while ( oldofs < newofs && len ) {
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
        if ( Options.output_format == OFORMAT_OMF ) {
            while ( idx < 0 && len ) {
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
#if AMD64_SUPPORT
            if ( sym->value3264 != 0 && ( sym->value3264 != -1 || sym->value >= 0 ) )
                sprintf( buffer, " = %-" PREFFMTSTR "I64X", sym->value, sym->value3264 );
            else
#endif
                sprintf( buffer, " = %-" PREFFMTSTR FX32, sym->value );
            p = buffer + strlen( buffer );
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
            len = sprintf( buffer, stdprefix, oldofs );
    }

    *p = ' ';
    p++;
    fwrite( buffer, 1, p - buffer, FileInfo.file[LST] );
#ifdef DEBUG_OUT
    *p = NULLC;
    DebugMsg1(("LstWrite: writing >%s<\n", buffer ));
#endif

    list_pos += p - buffer;

    p = CurrSource;
    if ( !p )
        p = "";
    //while( isspace( *p ) ) p++;
    len = strlen( p );
    if ( *CurrComment )
        len2 = strlen( CurrComment );
    else
        len2 = 0;

    /* calc new list position */
    if ( MacroLevel ) {
        list_pos += sprintf( buffer, "%u ", MacroLevel );
    }
    if ( GeneratedCode || StructInit )
        list_pos++;

    list_pos += len + len2 + NLSIZ;

#if FASTPASS
    if ( (Parse_Pass > PASS_1) && UseSavedState ) {
        DebugMsg1(("LstWrite: new pos=%" FU32 "\n", list_pos ));
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
    if ( len2 ) {
        fwrite( CurrComment, 1, len2, FileInfo.file[LST] );
//        *CurrComment = NULLC;
    }
    fwrite( NLSTR, 1, NLSIZ, FileInfo.file[LST] );

    DebugMsg1(("LstWrite: writing >%s<, new pos=%" FU32 "\n", p, list_pos ));
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
    switch( seg->combine ) {
    case COMB_INVALID:    return( typestr[TS_PRIVATE] );
    case COMB_STACK:      return( typestr[TS_STACK]   );
    case COMB_ADDOFF:     return( typestr[TS_PUBLIC]  );
    }
    return( "?" );
}

static void log_macro( struct asm_sym *sym )
/******************************************/
{
    int i = sym->name_size;
    const char *pdots;
    const char *type = (sym->isfunc) ? typestr[TS_FUNC] : typestr[TS_PROC];

    pdots = ((i >= DOTSMAX) ? "" : dots + i + 1 );
    LstPrintf( "%s %s        %s", sym->name, pdots ,type);
#ifdef DEBUG_OUT
    LstPrintf( " %5lu", ((dir_node *)sym)->e.macroinfo->count );
#endif
    LstNL();
    return;
}

/* called by log_struct and log_typedef
 * that is, the symbol is ensured to be a TYPE!
 */

static const char * GetMemtypeString( asm_sym * sym, char * buffer )
/******************************************************************/
{
    const char *p;

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
#if AMD64_SUPPORT
        if ( sym->Ofssize == USE64 )
            p = typestr[TS_NEAR];
        else
#endif
            if ( sym->isfar )
                p = sym->Ofssize ? typestr[TS_FAR32] : typestr[TS_FAR16];
            else
                p = sym->Ofssize ? typestr[TS_NEAR32] : typestr[TS_NEAR16];

        if ( buffer ) {
            strcat( buffer, p );
            strcat( buffer, " ");
            strcat( buffer, typestr[TS_PTR] );
            return( buffer );
        }
        return( p );
    case MT_FAR:
    is_far:
        if ( sym->segment )
            return( typestr[TS_LFAR] );
        if ( GetSymOfssize( sym ) > USE16 )
            return( typestr[TS_LFAR32] );
        return( typestr[TS_LFAR16] );
    case MT_PROC:
        if ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) )
            goto is_far;
        /* fall through */
    case MT_NEAR:
        if ( sym->segment )
            return( typestr[TS_LNEAR] );
        if ( GetSymOfssize( sym ) > USE16 )
            return( typestr[TS_LNEAR32] );
        return( typestr[TS_LNEAR16] );
    case MT_TYPE:
        if ( *(sym->type->name) )  /* there are a lot of unnamed types */
            return( sym->type->name );
        /* v2.04: changed */
        //return( typestr[TS_PTR] );
        return( GetMemtypeString( sym->type, buffer ) );
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

/* display STRUCTs and UNIONs */

static void log_struct( struct asm_sym *sym, char *name, int ofs )
/****************************************************************/
{
    unsigned      i;
    dir_node      *dir;
    const char    *pdots;
    struct_info   *si;
    field_list    *f;
    static int    prefix = 0;

    dir = (dir_node *)sym;

    /* filter typedefs and records */
    //if ( dir->e.structinfo->typekind != TYPE_STRUCT &&
    //     dir->e.structinfo->typekind != TYPE_UNION )
    //    return;

    si = dir->e.structinfo;

    if ( !name )
        name = sym->name;
    i = strlen ( name );
    pdots = (( (i+prefix) >= DOTSMAX) ? "" : dots + i + prefix + 1 );
    for ( i = 0; i < prefix; i++ )
        LstPrintf(" ");
    if ( prefix == 0 )
        if ( dir->e.structinfo->alignment > 1)
            LstPrintf( "%s %s        %8" FX32 " (%u)", name, pdots, sym->total_size, si->alignment );
        else
            LstPrintf( "%s %s        %8" FX32, name, pdots, sym->total_size );
    else
        LstPrintf( "%s %s        %8" FX32, name, pdots, sym->offset + ofs);
    LstNL();
    prefix += 2;
    for( f = si->head; f; f = f->next ) {
        /* recursion if an embedded struct occurs */
        if ( f->sym->mem_type == MT_TYPE && f->initializer == NULL ) {
            log_struct( f->sym->type, f->sym->name, f->sym->offset + ofs);
        } else {
            /* don't list unstructured fields without name */
            /* but do list them if they are structured */
            if (*(f->sym->name) || (f->sym->mem_type == MT_TYPE)) {
                i = f->sym->name_size + prefix;
                pdots = ((i >= DOTSMAX) ? "" : dots + i + 1 );
                for ( i = 0; i < prefix; i++ )
                    LstPrintf(" ");
                LstPrintf( "%s %s        %8" FX32 "   ", f->sym->name, pdots, f->sym->offset + sym->offset + ofs);
                LstPrintf( "%s", GetMemtypeString( f->sym, NULL ) );
                if ( f->sym->isarray )
                    LstPrintf( "[%u]",f->sym->total_length );
                LstNL();
            }
        }
    }
    prefix -= 2;
}

static void log_record( struct asm_sym *sym )
/*******************************************/
{
    unsigned        mask;
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    field_list *f;
    int i = sym->name_size;
    const char *pdots;

    pdots = ((i >= DOTSMAX) ? "" : dots + i + 1 );
    for( i = 0,f = si->head; f; f = f->next,i++ );
    LstPrintf( "%s %s      %6" FX32 "  %7X", sym->name, pdots, sym->total_size*8, i );
    LstNL();
    for( f = si->head; f; f = f->next ) {
        i = f->sym->name_size + 2;
        pdots = ((i >= DOTSMAX) ? "" : dots + i + 1 );
        for ( i = f->sym->offset, mask=0; i < f->sym->offset+f->sym->total_size; i++ )
            mask |= 1 << i;
        LstPrintf( "  %s %s      %6" FX32 "  %7" FX32 "  %08" FX32, f->sym->name, pdots, f->sym->offset, f->sym->total_size, mask );
        LstNL();
    }
}

/* a typedef is a simple struct with no fields. Size might be 0. */

static void log_typedef( struct asm_sym *sym )
/********************************************/
{
    dir_node    *dir = (dir_node *)sym;
    struct_info  *si = dir->e.structinfo;
    const char *p;
    char buffer[256];
    int i = sym->name_size;
    const char *pdots;

    pdots = (( i >= DOTSMAX ) ? "" : dots + i + 1 );
    buffer[0] = NULLC;
    if ( sym->mem_type == MT_PROC && si->target ) { /* typedef proto? */
        strcat(buffer, typestr[TS_PROC] );
        strcat(buffer, " " );
        if ( *si->target->name ) {  /* the name may be "" */
            strcat( buffer, si->target->name );
            strcat( buffer," ");
        }
        strcat( buffer, GetMemtypeString( si->target, NULL ) );
        strcat( buffer," " );
        strcat( buffer, GetLanguage( si->target ) );
        p = buffer;
    } else
        p = GetMemtypeString( sym, buffer );
    LstPrintf( "%s %s    %8" FU32 "  %s", sym->name, pdots, sym->total_size, p );
    LstNL();
}

static void log_segment( struct asm_sym *sym, struct asm_sym *group )
/*******************************************************************/
{
    char buffer[32];

    seg_info *seg = ((dir_node *)sym)->e.seginfo;

    if( seg->group == group ) {
        int i = sym->name_size;
        const char *pdots;
        pdots = (( i >= DOTSMAX ) ? "" : dots + i + 1);
        LstPrintf( "%s %s        ", sym->name, pdots );
        if( seg->Ofssize == USE32 ) {
            //LstPrintf( "32 Bit   %08" FX32 " ", seg->current_loc );
            LstPrintf( "32 Bit   %08" FX32 " ", sym->max_offset );
#if AMD64_SUPPORT
        } else if( seg->Ofssize == USE64 ) {
            LstPrintf( "64 Bit   %08" FX32 " ", sym->max_offset );
#endif
        } else {
            //LstPrintf( "16 Bit   %04" FX32 "     ", seg->current_loc );
            LstPrintf( "16 Bit   %04" FX32 "     ", sym->max_offset );
        }
        LstPrintf( "%-7s %-8s", get_seg_align( seg, buffer ), get_seg_combine( seg ) );
        LstPrintf( "'%s'", GetLname( seg->class_name_idx ) );
#if 0
        if ( group != NULL )
            LstPrintf( " %s", group->name );
#endif
        LstNL();
    }
}

static void log_group( struct asm_sym *grp, dir_node *segs )
/**********************************************************/
{
    unsigned i;
    const char *pdots;
    seg_item *curr;

    i = grp->name_size;
    pdots = (( i >= DOTSMAX ) ? "" : dots + i + 1);
    LstPrintf( "%s %s        %s", grp->name, pdots, typestr[TS_GROUP] );
    LstNL();

    /* the FLAT groups is always empty */
    if ( grp == (asm_sym *)ModuleInfo.flat_grp ) {
        for( ; segs; segs = segs->next ) {
            log_segment( (asm_sym *)segs, grp );
        }
    } else
        for( curr = ((dir_node *)grp)->e.grpinfo->seglist; curr; curr = curr->next ) {
            log_segment( (asm_sym *)curr->seg, grp );
        }
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

/* list Procedures and Prototypes */

static void log_proc( struct asm_sym *sym )
/*****************************************/
{
    dir_node *f;
    struct dir_node *l;
    const char * p;
    dir_node *dir = (dir_node *)sym;
    int i = sym->name_size;
    const char *pdots;

    pdots = (( i >= DOTSMAX ) ? "" : dots + i + 1 );
    if ( sym->Ofssize )
        p = "%s %s        P %-6s %08" FX32 " %-8s ";
    else
        p = "%s %s        P %-6s %04" FX32 "     %-8s ";
    LstPrintf( p,
            sym->name,
            pdots,
            get_proc_type( sym ),
            sym->offset,
            get_sym_seg_name( sym ));
    if ( sym->Ofssize )
        LstPrintf( "%08" FX32 " ", sym->total_size );
    else
        LstPrintf( "%04" FX32 "     ", sym->total_size );

#ifdef DEBUG_OUT
    if ( sym->forward )
        LstPrintf( "(F) " );
#endif
    if( sym->public ) {
        LstPrintf( "%-9s", typestr[TS_PUBLIC] );
    } else if ( dir->sym.state == SYM_INTERNAL ) {
        LstPrintf( "%-9s", typestr[TS_PRIVATE] );
    } else
        LstPrintf( sym->weak ? "*%-8s " : "%-9s ", typestr[TS_EXTERNAL] );

    LstPrintf( "%s", GetLanguage( sym ) );
    LstNL();
    /* for PROTOs, list optional altname */
    if ( dir->sym.state == SYM_EXTERNAL && dir->sym.altname ) {
        struct asm_sym *sym2 = dir->sym.altname;
        LstPrintf( "  ");
        LstPrintf( p,
                  sym2->name,
                  pdots+2,
                  get_proc_type( sym2 ),
                  sym2->offset,
                  get_sym_seg_name( sym2 ));
        LstNL();
    }
    /* for PROCs, list parameters and locals */
    if ( dir->sym.state == SYM_INTERNAL ) {
        if (dir->sym.langtype == LANG_C ||
            dir->sym.langtype == LANG_SYSCALL ||
            dir->sym.langtype == LANG_STDCALL ||
            dir->sym.langtype == LANG_FASTCALL) {
            int cnt;
            /* position f2 to last param */
            for ( cnt = 0, f = dir->e.procinfo->paralist; f; f = f->nextparam )
                cnt++;
            for ( ; cnt; cnt-- ) {
                int curr;
                for ( curr = 1,f = dir->e.procinfo->paralist; curr < cnt;f = f->nextparam, curr++ );
                i = f->sym.name_size;
                pdots = (( i >= DOTSMAX-2 ) ? "" : dots + i + 1 + 2 );
                /* FASTCALL: parameter may be a text macro (=register name) */
                if ( f->sym.state == SYM_TMACRO )
                    LstPrintf( "  %s %s        %-17s %s", f->sym.name, pdots, GetMemtypeString( &f->sym, NULL ), f->sym.string_ptr );
                else
                    LstPrintf( szFmtProcStk, f->sym.name, pdots,
                            f->is_vararg ? szVararg : GetMemtypeString( &f->sym, NULL ),
                            GetResWName( basereg[dir->sym.Ofssize], NULL ),
                            '+', f->sym.offset );
                LstNL();
            }
        } else {
            for ( f = dir->e.procinfo->paralist; f; f = f->nextparam ) {
                i = f->sym.name_size;
                pdots = (( i >= DOTSMAX-2 ) ? "" : dots + i + 1 + 2);
                LstPrintf( szFmtProcStk, f->sym.name, pdots, GetMemtypeString( &f->sym, NULL ), GetResWName( basereg[dir->sym.Ofssize], NULL ), '+', f->sym.offset );
                LstNL();
            }
        }
        for ( l = dir->e.procinfo->locallist; l; l = l->nextlocal ) {
            char buffer[32];
            i = l->sym.name_size;
            pdots = (( i >= DOTSMAX-2 ) ? "" : dots + i + 1 + 2);
            if ( l->sym.isarray )
                sprintf( buffer, "%s[%lu]", GetMemtypeString(&l->sym, NULL), l->sym.total_length );
            else
                strcpy( buffer, GetMemtypeString( &l->sym, NULL ) );
            LstPrintf( szFmtProcStk, l->sym.name, pdots, buffer, GetResWName( basereg[dir->sym.Ofssize], NULL ), '-', - l->sym.offset );
            LstNL();
        }
        for ( l = dir->e.procinfo->labellist; l ; l = l->nextll ) {
            dir_node *l2;
            for ( l2 = l; l2; l2 = (dir_node *)l2->sym.next ) {
                /* filter params and locals! */
                if ( l2->sym.state == SYM_STACK || l2->sym.state == SYM_TMACRO)
                    continue;
                i = l2->sym.name_size;
                pdots = (( i >= DOTSMAX-2 ) ? "" : dots + i + 1 + 2);
                if ( sym->Ofssize )
                    p = "  %s %s        L %-6s %08" FX32 " %s";
                else
                    p = "  %s %s        L %-6s %04" FX32 "     %s";
                LstPrintf( p,
                          l2->sym.name,
                          pdots,
                          get_proc_type( &l2->sym ),
                          l2->sym.offset,
                          get_sym_seg_name( &l2->sym ));
#ifdef DEBUG_OUT
                if ( l2->sym.forward )
                    LstPrintf( " (F)" );
#endif
                LstNL();
            }
        }
    }
}

/* list symbols */

static void log_symbol( struct asm_sym *sym )
/*******************************************/
{
    int i = sym->name_size;
    const char *pdots;
    char buffer[MAX_LINE_LEN];

    pdots = ((i >= DOTSMAX) ? "" : dots + i + 1 );

    switch ( sym->state ) {
    case SYM_UNDEFINED:
    case SYM_INTERNAL:
    case SYM_EXTERNAL:
        LstPrintf( "%s %s        ", sym->name, pdots );

        if ( sym->isarray ) {
            i = sprintf( buffer, "%s[%u]", GetMemtypeString( sym, NULL ), sym->total_length );
            LstPrintf( "%-10s ", buffer );
        } else if ( sym->state == SYM_EXTERNAL && sym->comm == TRUE ) {
            LstPrintf( "%-10s ", szCOMM );
        } else
            LstPrintf( "%-10s ", GetMemtypeString( sym, NULL ) );

        /* print value */
        if ( sym->mem_type == MT_ABS )
            if ( sym->value3264 != 0 && sym->value3264 != -1 )
                LstPrintf( " %I64Xh ", sym->uvalue, sym->value3264 );
            else if ( sym->value3264 < 0 )
                LstPrintf( "-%08" FX32 "h ", 0 - sym->uvalue );
            else
                LstPrintf( " %8" FX32 "h ", sym->offset );
        else if ( sym->state == SYM_EXTERNAL && sym->comm == TRUE )
            LstPrintf( " %8" FX32 "h ", sym->total_size / sym->total_length );
        else
            LstPrintf( " %8" FX32 "h ", sym->offset );

        /* print segment */
        if ( sym->mem_type == MT_ABS || sym->state == SYM_UNDEFINED )
            ;
        else
            LstPrintf( "%s ", get_sym_seg_name( sym ) );

#ifdef DEBUG_OUT
        if ( sym->forward )
            LstPrintf( "(F) " );
#endif
        if ( sym->state == SYM_EXTERNAL && sym->comm == TRUE )
            LstPrintf( "%s=%u ", szCount, sym->total_length );

        if( sym->public )
            LstPrintf( "%s ", typestr[TS_PUBLIC] );

        if ( sym->state == SYM_EXTERNAL ) {
            LstPrintf( sym->weak ? "*%s " : "%s ", typestr[TS_EXTERNAL] );
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
        LstPrintf( "%s %s        %s  %s", sym->name, pdots, typestr[TS_ALIAS], sym->substitute->name );
        LstNL();
        break;
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
    int             idx;
    uint_32         i;
    uint_32         SymCount;
    qdesc           queues[LQ_LAST];

    /* no point going through the motions if lst file isn't open */
    if( FileInfo.file[LST] == NULL || Options.no_symbol_listing == TRUE ) {
        return;
    }

    /* go to EOF */
    fseek( FileInfo.file[LST], 0, SEEK_END );

    DebugMsg(("LstWriteCRef: calling SymSort\n"));

    syms = SymSort( &SymCount );
    if( !syms )
        return;

    memset( queues, 0, sizeof( queues ) );
    for( i = 0; i < SymCount; ++i ) {
        qdesc *q;
        if ( syms[i]->list == FALSE )
            continue;
        switch (syms[i]->state) {
        case SYM_TYPE:
            si = ((dir_node *)syms[i])->e.structinfo;
            if ( si->typekind == TYPE_RECORD )
                idx = LQ_RECORDS;
            else if ( si->typekind == TYPE_TYPEDEF )
                idx = LQ_TYPEDEFS;
            else
                idx = LQ_STRUCTS;
            break;
        case SYM_MACRO:
            idx = LQ_MACROS;
            break;
        case SYM_SEG:
            idx = LQ_SEGS;
            break;
        case SYM_GRP:
            idx = LQ_GRPS;
            break;
        case SYM_INTERNAL:
        case SYM_EXTERNAL: /* v2.04: added, since PROTOs are now externals */
            if ( syms[i]->isproc ) {
                idx = LQ_PROCS;
                break;
            }
        default:
            continue;
        }
        q = &queues[idx];
        if( q->head == NULL ) {
            q->head = syms[i];
        } else {
            ((dir_node *)q->tail)->next = (dir_node *)syms[i];
        }
        q->tail = syms[i];
        ((dir_node *)syms[i])->next = NULL;
    }
    for ( idx = 0; idx < ( sizeof( cr ) / sizeof (cr[0] ) ); idx++ ) {
        if ( queues[cr[idx].type].head ) {
            if ( cr[idx].capitems ) {
                const short *ps;
                for ( ps = cr[idx].capitems; *ps; ps++ ) {
                    if ( *ps == -1 )
                        LstNL();
                    else
                        LstCaption( MsgGetEx( *ps ), ps == cr[idx].capitems ? 2 : 0 );
                }
            }
            for( dir = queues[cr[idx].type].head; dir ; dir = dir->next ) {
                cr[idx].function( &dir->sym, ( cr[idx].flags & PRF_ADDSEG ) ? queues[LQ_SEGS].head : NULL, 0 );
            }
        }
    }

    /* write out symbols */
    LstCaption( MsgGetEx( TXT_SYMBOLS ), 2 );
    LstCaption( MsgGetEx( TXT_SYMCAP ), 0 );
    for( i = 0; i < SymCount; ++i ) {
        if ( syms[i]->list == TRUE && syms[i]->isproc == FALSE )
            log_symbol( syms[i] );
    }
    LstNL();

    /* free the sorted symbols */
    DebugMsg(("LstWriteCRef: free sorted symbols\n"));
    MemFree( syms );
}

/* .[NO|X]LIST, .[NO|X]CREF, .[NO]LISTIF, .[NO]LISTMACRO
 * .LISTALL, .LISTMACROALL, .[LF|SF|TF]COND, .[X|L|S]ALL
 * PAGE, TITLE, SUBTITLE, SUBTTL directives
 */
ret_code ListingDirective( int i )
/********************************/
{
    int directive = AsmBuffer[i]->value;
    i++;

    switch ( directive ) {
    case T_DOT_LIST:
        if ( FileInfo.file[LST] )
            ModuleInfo.list = TRUE;
        break;
    case T_DOT_CREF:
        ModuleInfo.cref = TRUE;
        break;
    case T_DOT_NOLIST:
    case T_DOT_XLIST:
        ModuleInfo.list = FALSE;
        break;
    case T_DOT_NOCREF:
    case T_DOT_XCREF:
        if ( AsmBuffer[i]->token == T_FINAL ) {
            ModuleInfo.cref = FALSE;
        } else {
            asm_sym *sym;
            while ( AsmBuffer[i]->token != T_FINAL ) {
                if ( AsmBuffer[i]->token != T_ID ) {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                    return( ERROR );
                }
                if ( sym = SymLookup( AsmBuffer[i]->string_ptr )) {
                    sym->list = FALSE;
                }
                i++;
                if ( AsmBuffer[i]->token != T_FINAL ) {
                    if ( AsmBuffer[i]->token == T_COMMA ) {
                        if ( (i + 1) < Token_Count )
                            i++;
                    } else {
                        AsmError( EXPECTING_COMMA );
                        return( ERROR );
                    }
                }
            }
        }
        break;
    case T_DOT_LISTALL: /* list false conditionals and generated code */
        if ( FileInfo.file[LST] )
            ModuleInfo.list = TRUE;
        ModuleInfo.list_generated_code = TRUE;
        /* fall through */
    case T_DOT_LISTIF:
    case T_DOT_LFCOND: /* .LFCOND is synonym for .LISTIF */
        ModuleInfo.listif = TRUE;
        break;
    case T_DOT_NOLISTIF:
    case T_DOT_SFCOND: /* .SFCOND is synonym for .NOLISTIF */
        ModuleInfo.listif = FALSE;
        break;
    case T_DOT_TFCOND: /* .TFCOND toggles .LFCOND, .SFCOND */
        ModuleInfo.listif = !ModuleInfo.listif;
        break;
    case T_DOT_LISTMACRO:
    case T_DOT_XALL:   /* .XALL is synonym for .LISTMACRO */
        ModuleInfo.list_macro = LM_LISTMACRO;
        break;
    case T_DOT_LISTMACROALL:
    case T_DOT_LALL:   /* .LALL is synonym for .LISTMACROALL */
        ModuleInfo.list_macro = LM_LISTMACROALL;
        break;
    case T_DOT_NOLISTMACRO:
    case T_DOT_SALL:   /* .SALL is synonym for .NOLISTMACRO */
        ModuleInfo.list_macro = LM_NOLISTMACRO;
        break;
    case T_PAGE:
    default: /* TITLE, SUBTITLE, SUBTTL */
        /* tiny checks to ensure that these directives
         aren't used as code labels or struct fields */
        if ( AsmBuffer[i]->token == T_COLON )
            break;
        /* this isn't really Masm-compatible, but ensures we don't get
         * struct fields with names page, title, subtitle, subttl.
         */
        if( CurrStruct ) {
            AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
            return( ERROR );
        }
        if ( Parse_Pass == PASS_1 )
            AsmWarn( 4, DIRECTIVE_IGNORED, AsmBuffer[i-1]->string_ptr );
        while (AsmBuffer[i]->token != T_FINAL) i++;
    }

    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    return( NOT_ERROR );
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

