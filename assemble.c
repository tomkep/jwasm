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
* Description:  assemble a module.
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "symbols.h"
#include "memalloc.h"
#include "input.h"
#include "tokenize.h"
#include "condasm.h"
#include "directiv.h"
#include "segment.h"
#include "assume.h"
#include "proc.h"
#include "expreval.h"
#include "hll.h"
#include "context.h"
#include "labels.h"
#include "macro.h"
#include "parser.h"
#include "queues.h"
#include "types.h"
#include "fixup.h"
#include "omf.h"
#include "omfgenms.h"
#include "fastpass.h"
#include "listing.h"
#include "msgtext.h"
#include "fatal.h"
#include "myassert.h"

#if COFF_SUPPORT
#include "coff.h"
#endif
#if ELF_SUPPORT
#include "elf.h"
#endif
#if BIN_SUPPORT
#include "bin.h"
#endif

extern void             CheckProcOpen( void );
extern void             SortSegments( void );

extern symbol_queue     Tables[];       // tables of definitions
extern void             *SafeSEHStack;
extern obj_rec          *ModendRec;     // Record for Modend (OMF)
extern asm_sym          *start_label;   // symbol for Modend (COFF)
extern int              StructInit;     // see data.c
extern uint_32          LastCodeBufSize;
extern int              procidx;

/* fields: next, name, segment, offset/value */
struct asm_sym WordSize = {NULL,"@WordSize", NULL, 0};

/* names for output formats. order must match enum oformat */
const struct format_options formatoptions[] = {
    { "OMF", NULL },
    { "COFF", NULL },
#if ELF_SUPPORT
    { "ELF", elf_init },
#endif
    { "BIN", NULL }
};

module_info             ModuleInfo;
unsigned int            Parse_Pass;     // phase of parsing

global_vars             GlobalVars;     // used for OMF comment records

static unsigned long    lastLineNumber;
unsigned int            GeneratedCode;

static asm_sym          *dmyproc;
qdesc                   LinnumQueue;    // queue of line_num_info items

// buffer for source lines
// since the lines are sometimes concatenated
// the buffer must be a multiple of MAX_LINE_LEN

static char srclinebuffer[ MAX_LINE_LEN * MAX_SYNC_MACRO_NESTING ];

#if FASTPASS

mod_state modstate;
static line_item *LineStoreHead;
static line_item *LineStoreTail;
line_item *LineStoreCurr;
bool StoreState;
bool UseSavedState;

#endif

bool            write_to_file;  // write object module
bool            Modend;         // end of module is reached
bool            PhaseError;     // phase error occured
bool            CommentDataInCode;

static void AddLinnumData( struct line_num_info *data )
/*****************************************************/
{
    qdesc *q;
    if ( Options.output_format == OFORMAT_COFF ) {
        q = (qdesc *)CurrSeg->e.seginfo->LinnumQueue;
        if ( q == NULL ) {
            q = AsmAlloc( sizeof( qdesc ) );
            CurrSeg->e.seginfo->LinnumQueue = q;
            q->head = NULL;
        }
    } else
        q = &LinnumQueue;

    data->next = NULL;
    if ( q->head == NULL)
        q->head = q->tail = data;
    else {
        ((line_num_info *)q->tail)->next = data;
        q->tail = data;
    }
}

void AddLinnumDataRef( unsigned line_num )
/****************************************/
/* store a reference for the current line at the current address */
{
    struct line_num_info    *curr;

    /* COFF line number info is related to functions/procedures. Since
     * assembly allows code lines outside of procs, "dummy" procs must
     * be generated. A dummy proc last until a true PROC is detected or
     * the source file changes.
     */
    if ( Options.output_format == OFORMAT_COFF &&
        CurrProc == NULL &&
        ( dmyproc == NULL ||
        dmyproc->debuginfo->file != get_curr_srcfile() ) ) {
        char procname[10];
        if ( dmyproc ) {
            myassert( dmyproc->segment );
            dmyproc->total_size =
                ((dir_node *)dmyproc->segment)->e.seginfo->current_loc -
                dmyproc->offset;
        }
        sprintf( procname, "$$$%05u", procidx );
        DebugMsg(("AddLinnumDataRef: generating proc=%s\n", procname ));
        dmyproc = SymSearch( procname );

        /* in pass 1, create the proc */
        if ( dmyproc == NULL ) {
            dmyproc = CreateProc( NULL, procname, TRUE );
            DebugMsg(("AddLinnumDataRef: new proc %s created\n", procname ));
            dmyproc->isproc = TRUE; /* fixme: should be set inside CreateProc */
            dmyproc->included = TRUE;
            AddPublicData( dmyproc );
        } else
            procidx++; /* for passes > 1, adjust procidx */

        /* it the symbols isn't a PROC, the symbol name has been used
         * by the user - bad! A warning should be displayed */
        if ( dmyproc->isproc == TRUE ) {
            SetSymSegOfs( dmyproc );
            dmyproc->Ofssize = ModuleInfo.Ofssize;
            dmyproc->langtype = ModuleInfo.langtype;
            if ( write_to_file == TRUE ) {
                curr = AsmAlloc( sizeof( struct line_num_info ) );
                curr->sym = dmyproc;
                curr->line_number = LineNumber;
                curr->file = get_curr_srcfile();
                curr->number = 0;
                DebugMsg(("AddLinnumDataRef: sym=%s (#=%u.%u)\n", curr->sym->name, curr->file, curr->line_number ));
                AddLinnumData( curr );
            }
        }
    }

    if(  line_num && ( write_to_file == FALSE || lastLineNumber == line_num )) {
#ifdef DEBUG_OUT
        if ( write_to_file == TRUE )
            DebugMsg(("AddLinnumDataRef(#=%u) line skipped, lastline=%u\n", line_num, lastLineNumber ));
#endif
        return;
    }
    DebugMsg(("AddLinnumDataRef(#=%u) enter, currofs=%Xh, CurrProc=%s, GeneratedCode=%u\n", line_num, GetCurrOffset(), CurrProc ? CurrProc->sym.name : "NULL", GeneratedCode ));
    curr = AsmAlloc( sizeof( struct line_num_info ) );
    curr->number = line_num;
    if ( line_num == 0 ) { /* happens for COFF only */
        if ( Options.output_format == OFORMAT_COFF && CurrProc->sym.public == FALSE ) {
            CurrProc->sym.included = TRUE;
            AddPublicData( (asm_sym *)CurrProc );
        }
        curr->sym = (asm_sym *)CurrProc;
        curr->line_number = LineNumber;
        curr->file        = get_curr_srcfile();
        /* set the function's size! */
        if ( dmyproc ) {
            myassert( dmyproc->segment );
            dmyproc->total_size =
                ((dir_node *)dmyproc->segment)->e.seginfo->current_loc -
                dmyproc->offset;
        }
        dmyproc = NULL;
    } else {
        curr->offset = GetCurrOffset();
        curr->srcfile = get_curr_srcfile();
    }
    lastLineNumber = line_num;
    AddLinnumData( curr );

    return;
}

static void QueueDeleteLinnum( qdesc * queue )
/********************************************/
{
    struct line_num_info    *curr;
    struct line_num_info    *next;

    if ( queue == NULL )
        return;
    curr = queue->head;
    for( ; curr ; curr = next ) {
        next = curr->next;
        AsmFree( curr );
    }
    return;
}


#if FASTPASS

void StoreLine( char * string )
/*****************************/
{
    int i;

    DebugMsg(("StoreLine: Line=%u, listpos=%u\n", LineNumber, list_pos ));
    i = strlen(string) + 1;
    LineStoreCurr = AsmAlloc( i + sizeof(line_item) );
    LineStoreCurr->next = NULL;
    LineStoreCurr->lineno = LineNumber;
    if ( MacroLevel ) {
        LineStoreCurr->srcfile = 0xfff;
    } else {
        LineStoreCurr->srcfile = get_curr_srcfile();
    }
    LineStoreCurr->list_pos = list_pos;
    memcpy( LineStoreCurr->line, string, i );
#ifdef DEBUG_OUT
    if ( Options.print_linestore )
        printf("%s\n", LineStoreCurr->line );
#endif
    LineStoreTail->next = LineStoreCurr;
    LineStoreTail = LineStoreCurr;
}

/*
 * save the current status (happens in pass one only) and
 * switch to "save precompiled lines" mode.
 * the status is then restored in further passes,
 * and the precompiled lines are used for assembly then.
 */

void SaveState( void )
/********************/
{
    int i;

    DebugMsg(("%u. SaveState enter\n", LineNumber ));
    StoreState = TRUE;
    UseSavedState = TRUE;
    modstate.init = TRUE;
    modstate.EquHead = modstate.EquTail = NULL;

    memcpy(&modstate.modinfo, &ModuleInfo, sizeof(module_info));

    SegmentSaveState();
    AssumeSaveState();
    ContextSaveState(); /* save pushcontext/popcontext stack */

    i = strlen( CurrSource ) + 1;
    LineStoreCurr = AsmAlloc( i + sizeof(line_item) );
    LineStoreCurr->next = NULL;
    LineStoreCurr->lineno = LineNumber;
    if ( MacroLevel ) {
        LineStoreCurr->srcfile = 0xfff;
    } else {
        LineStoreCurr->srcfile = get_curr_srcfile();
    }
    LineStoreCurr->list_pos = list_pos_start;
    memcpy( LineStoreCurr->line, CurrSource, i );
#ifdef DEBUG_OUT
    if ( Options.print_linestore )
        printf("%s\n", LineStoreCurr->line );
#endif
    LineStoreHead = LineStoreTail = LineStoreCurr;
    DebugMsg(( "SaveState: curr line=>%s<, list pos=%u\n", CurrSource, list_pos ));
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

static void RestoreState( void )
/******************************/
{
#if !defined(__POCC__)
    static line_item endl = { NULL, 0, 0, 0, "END" };
#endif

    DebugMsg(("RestoreState enter\n"));
    if (modstate.init) {
        equ_item *curr;
        int i;
        for (curr = modstate.EquHead; curr; curr = curr->next) {
//            printf("RestoreState: sym >%s<, value=%u, defined=%u\n", curr->sym->name, curr->value, curr->defined);
            if (curr->sym->mem_type == MT_ABS) {
                curr->sym->value   = curr->value;
                curr->sym->defined = curr->defined;
            }
        }
        i = ModuleInfo.warning_count; /* don't restore warning count! */
        memcpy(&ModuleInfo, &modstate.modinfo, sizeof(module_info));
        SetOfssize();
        ModuleInfo.warning_count = i;
        SymSetCmpFunc();
    }

    if (LineStoreHead == NULL) {
#ifdef __POCC__
        line_item *endl = AsmAlloc( sizeof( line_item ) + 4 );
        endl->next = NULL;
        endl->srcfile = 0;
        endl->lineno = LineNumber;
        endl->list_pos = 0;
        strcpy( endl->line, "END");
        LineStoreHead = endl;
#else
        endl.lineno = LineNumber;
        LineStoreHead = &endl;
#endif
    }
    return;
}

#endif

/* Write a byte to the segment buffer.
 * in OMF, the segment buffer is flushed when the max. record size is reached.
 */

void OutputByte( unsigned char byte )
/***********************************/
{
    CurrSeg->e.seginfo->current_loc++;
    CurrSeg->e.seginfo->bytes_written++;
    if( CurrSeg->e.seginfo->current_loc >= CurrSeg->sym.max_offset )
        CurrSeg->sym.max_offset = CurrSeg->e.seginfo->current_loc;
    if( write_to_file == TRUE ) {
        uint_32 idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc - 1;
        CurrSeg->e.seginfo->CodeBuffer[idx] = byte;
        if( Options.output_format == OFORMAT_OMF && idx >= MAX_LEDATA_THRESHOLD ) {
            omf_FlushCurrSeg();
        }
    }
}

/* this is to output small amounts ( <= 8) of bytes which must
 * not be separated ( for omf, because of fixups )
 */
void OutputBytes( unsigned char *pbytes, int len )
/************************************************/
{
    if( write_to_file == TRUE ) {
        uint_32 idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
        if( Options.output_format == OFORMAT_OMF && ((idx + len) >= MAX_LEDATA ) ) {
            omf_FlushCurrSeg();
            idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
        }
        memcpy( &CurrSeg->e.seginfo->CodeBuffer[idx], pbytes, len );
    }
    CurrSeg->e.seginfo->current_loc += len;
    CurrSeg->e.seginfo->bytes_written += len;
    if( CurrSeg->e.seginfo->current_loc >= CurrSeg->sym.max_offset )
        CurrSeg->sym.max_offset = CurrSeg->e.seginfo->current_loc;
}

/* new for v2.02. this function ensures that data and fixup
 * is never separated ( required for OMF ). in v2.00-v2.01, this
 * might have happened.
 */

void OutputBytesAndFixup( struct asmfixup *fixup, unsigned char *pbytes, int len )
/********************************************************************************/
{
    if( write_to_file == TRUE ) {
        uint_32 idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
        if( Options.output_format == OFORMAT_OMF && ((idx + len) >= MAX_LEDATA ) ) {
            omf_FlushCurrSeg();
            idx = CurrSeg->e.seginfo->current_loc - CurrSeg->e.seginfo->start_loc;
        }
        store_fixup( fixup, (int_32 *)pbytes );
        memcpy( &CurrSeg->e.seginfo->CodeBuffer[idx], pbytes, len );
    }
    CurrSeg->e.seginfo->current_loc += len;
    CurrSeg->e.seginfo->bytes_written += len;
    if( CurrSeg->e.seginfo->current_loc >= CurrSeg->sym.max_offset )
        CurrSeg->sym.max_offset = CurrSeg->e.seginfo->current_loc;
}

void OutputCodeByte( unsigned char byte )
/***************************************/
{
    if ( CommentDataInCode )
        omf_OutSelect( FALSE );
    OutputByte( byte );
}

void FillDataBytes( unsigned char byte, int len )
/***********************************************/
{
    if ( CommentDataInCode )
        omf_OutSelect( TRUE );
    for( ; len; len-- )
        OutputByte( byte );
}

// set current position in current segment without to write anything

ret_code SetCurrOffset( int_32 value, bool relative, bool select_data )
/*********************************************************************/
{
    if( CurrSeg == NULL ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }

    if( relative ) {
        value += GetCurrOffset();
    }

    if ( write_to_file && ( Options.output_format == OFORMAT_OMF ) )
        omf_FlushCurrSeg( );

    /* for -bin, if there's an ORG (relative==false) and no initialized data
     * has been set yet, set start_loc!
     * v1.96: this is now also done for COFF and ELF
     */
    //else if ( Options.output_format == OFORMAT_BIN && relative == FALSE ) {
    else if ( relative == FALSE ) {
        if ( CurrSeg->e.seginfo->bytes_written == 0 ) {
            CurrSeg->e.seginfo->start_loc = value;
        }
    }

    if( select_data )
        if ( CommentDataInCode )
            omf_OutSelect( TRUE );
    CurrSeg->e.seginfo->current_loc = value;
    if ( Options.output_format == OFORMAT_OMF ) {
        CurrSeg->e.seginfo->start_loc = value;
        LastCodeBufSize = value;
    }

    if( CurrSeg->e.seginfo->current_loc >=
        CurrSeg->sym.max_offset ) {
        CurrSeg->sym.max_offset = CurrSeg->e.seginfo->current_loc;
    }

    return( NOT_ERROR );
}

// finish module writes
// for OMF, just write the MODEND record
// for COFF,ELF and BIN, write the section data and symbol table

static ret_code WriteContent( void )
/**********************************/
{
    DebugMsg(("WriteContent enter\n"));
    switch (Options.output_format) {
    case OFORMAT_OMF:
        if( ModendRec == NULL ) {
            AsmError( UNEXPECTED_END_OF_FILE );
            return ERROR;
        }
        /* -if Zi is set, write symbols and types */
        if ( Options.debug_symbols )
            omf_write_debug_tables();
        omf_write_modend();
        break;
#if COFF_SUPPORT
    case OFORMAT_COFF:
        coff_write_data( &ModuleInfo );
        coff_write_symbols( &ModuleInfo );
        break;
#endif
#if ELF_SUPPORT
    case OFORMAT_ELF:
        elf_write_data( &ModuleInfo );
        break;
#endif
#if BIN_SUPPORT
    case OFORMAT_BIN:
        bin_write_data( &ModuleInfo );
        break;
#endif
    }
    DebugMsg(("WriteContent exit\n"));
    return NOT_ERROR;
}

/*
 * write the OMF/COFF/ELF header
 * for OMF, this is called twice, once after Pass 1 is done
 * and then again after assembly has finished without errors.
 * for COFF/ELF/BIN, it's just called once.
 */
static ret_code WriteHeader( bool initial )
/*****************************************/
{
    dir_node    *curr;

    DebugMsg(("WriteHeader(%u) enter\n", initial));

    /* calc the number of segments/sections */
    ModuleInfo.total_segs = 0;
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if( ( curr->sym.segment == NULL )
          && ( curr->e.seginfo->group == NULL ) )
            AsmErr( SEG_NOT_DEFINED, curr->sym.name );
        else if ( curr->e.seginfo->Ofssize == USE16 && curr->sym.max_offset > 0x10000 ) {
            if ( Options.output_format == OFORMAT_OMF )
                AsmErr( SEGMENT_EXCEEDS_64K_LIMIT, curr->sym.name );
            else
                AsmWarn( 2, SEGMENT_EXCEEDS_64K_LIMIT, curr->sym.name );
        }
        ModuleInfo.total_segs++;
    }

    switch ( Options.output_format ) {
    case OFORMAT_OMF:
        if ( initial == TRUE ) {
            omf_write_header();
            //if( Options.no_dependencies == FALSE )
            if( Options.line_numbers )
                omf_write_autodep();
            if( ModuleInfo.segorder == SEGORDER_DOSSEG )
                omf_write_dosseg();
            else if( ModuleInfo.segorder == SEGORDER_ALPHA )
                SortSegments();
            omf_write_lib();
            omf_write_lnames();
        }
        omf_write_seg( initial );
        if ( initial == TRUE ) {
            omf_write_grp();
            omf_write_extdef();
            omf_write_comdef();
            omf_write_alias();
        }
        omf_write_public( initial );
        if ( initial == TRUE ) {
            omf_write_export();
            omf_end_of_pass1();
        }
        break;
#if COFF_SUPPORT
    case OFORMAT_COFF:
        coff_write_header( &ModuleInfo );
        coff_write_section_table( &ModuleInfo );
        break;
#endif
#if ELF_SUPPORT
    case OFORMAT_ELF:
        elf_write_header( &ModuleInfo );
        break;
#endif
#if BIN_SUPPORT
    case OFORMAT_BIN:
        /* check if PROTOs or externals are used */
#if 0 /* v2.01: PROTOs are now in TAB_EXT */
        for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next )
            if( curr->sym.used == TRUE && curr->sym.isproc == FALSE ) {
                DebugMsg(("WriteHeader: error, %s isproc=%u\n", curr->sym.name, curr->sym.isproc ));
                AsmErr( FORMAT_DOESNT_SUPPORT_EXTERNALS, curr->sym.name );
                return( ERROR );
            }
#endif
        for ( curr = Tables[TAB_EXT].head; curr != NULL ; curr = curr->next )
            if( curr->sym.weak == FALSE || curr->sym.used == TRUE ) {
                DebugMsg(("WriteHeader: error, %s weak=%u\n", curr->sym.name, curr->sym.weak ));
                AsmErr( FORMAT_DOESNT_SUPPORT_EXTERNALS, curr->sym.name );
                return( ERROR );
            }
        break;
#endif
#ifdef DEBUG_OUT
    default:
        /* this shouldn't happen */
        printf("unknown output format: %u\n", Options.output_format);
#endif
    }
    DebugMsg(("WriteHeader exit\n"));
    return( NOT_ERROR );
}

#define is_valid_first_char( ch )  ( isalpha(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' || ch=='.' )

static int is_valid_identifier( char *id )
/****************************************/
{
    /* special handling of first char of an id: it can't be a digit,
     but can be a dot (don't care about ModuleInfo.dotname!). */

    if( is_valid_first_char( *id ) == 0 )
        return( ERROR );
    id++;
    for( ; *id != NULLC; id++ ) {
        if ( is_valid_id_char( *id ) == FALSE )
            return( ERROR );
    }
    /* don't allow a single dot! */
    if ( *(id-1) == '.' )
        return( ERROR );

    return( NOT_ERROR );
}

/* add text macros defined with the -D cmdline switch */

static void add_cmdline_tmacros( void )
/****************************************/
{
    struct qitem *p;
    char *name;
    char *value;
    int len;
    struct asm_sym *sym;

    DebugMsg(("add_cmdline_tmacros enter\n"));
    for ( p = Options.SymQueue; p; p = p->next ) {
        DebugMsg(("add_cmdline_tmacros: found >%s<\n", p->value));
        name = p->value;
        value = strchr( name, '=' );
        if( value == NULL ) {
            value = "";
        } else {
            len = value - name;
            name = (char *)AsmTmpAlloc( len + 1 );
            memcpy( name, p->value, len );
            *(name + len) = NULLC;
            value++;
        }

        if( is_valid_identifier( name ) == ERROR ) {
            DebugMsg(("add_cmdline_tmacros: name >%s< invalid\n", name ));
            AsmErr( SYNTAX_ERROR_EX, name );
        } else {
            sym = SymSearch( name );
            if (sym == NULL) {
                sym = SymCreate( name, TRUE);
                sym->state = SYM_TMACRO;
            }
            if (sym->state == SYM_TMACRO) {
                sym->defined = TRUE;
                sym->predefined = TRUE;
                sym->string_ptr = value;
            } else
                AsmErr( SYMBOL_ALREADY_DEFINED, name );
        }
    }
    return;
}

/* add the include paths set by -I option */

static void add_incpaths( void )
/*************************/
{
    struct qitem *p;
    DebugMsg(("add_incpaths: enter\n"));
    for ( p = Options.IncQueue; p; p = p->next ) {
        AddStringToIncludePath( p->value );
    }
}

static void set_cpu_parameters( void )
/************************************/
{
    DebugMsg(("set_cpu_parameters enter, Options.cpu=%X\n", Options.cpu ));
    SetCPU( Options.cpu );
    return;
}

// this is called for every pass.
// symbol table and ModuleInfo are initialized.

static void CmdlParamsInit( int pass )
/************************************/
{
    DebugMsg(("CmdlParamsInit(%u) enter\n", pass));

#if BUILD_TARGET
    if (pass == PASS_1) {
        asm_sym *sym;
        char * p;

        _strupr( Options.build_target );
        tmp = AsmTmpAlloc( strlen( Options.build_target ) + 5 ); // null + 4 uscores
        strcpy( tmp, uscores );
        strcat( tmp, Options.build_target );
        strcat( tmp, uscores );

        /* define target */
        sym = SymCreate( tmp, TRUE );
        sym->state = SYM_INTERNAL;
        sym->mem_type = MT_ABS;
        sym->defined = TRUE;
        sym->predefined = TRUE;

        p = NULL;
        if( stricmp( Options.build_target, "DOS" ) == 0 ) {
            p = "__MSDOS__";
        } else if( stricmp( Options.build_target, "NETWARE" ) == 0 ) {
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                p = "__NETWARE_386__";
            } else {
                /* do nothing ... __NETWARE__ already defined */
            }
        } else if( stricmp( Options.build_target, "WINDOWS" ) == 0 ) {
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                p = "__WINDOWS_386__";
            } else {
                /* do nothing ... __WINDOWS__ already defined */
            }
        } else if( stricmp( Options.build_target, "QNX" ) == 0 ) {
            p = "__UNIX__";
        } else if( stricmp( Options.build_target, "LINUX" ) == 0 ) {
            p = "__UNIX__";
        }
        if (p) {
            sym = SymCreate( p, TRUE );
            sym->state = SYM_INTERNAL;
            sym->mem_type = MT_ABS;
            sym->defined = TRUE;
            sym->predefined = TRUE;
        }
    }
#endif

    if( Options.ForceInclude != NULL )
        InputQueueFile( Options.ForceInclude, NULL );

    if (pass == PASS_1) {
        char *env;
        set_cpu_parameters();
        add_cmdline_tmacros();
        add_incpaths();
        if ( Options.ignore_include == FALSE )
            if ( env = getenv( "INCLUDE" ) )
                AddStringToIncludePath( env );
    }
    DebugMsg(("CmdlParamsInit exit\n"));
    return;
}

void WritePreprocessedLine( const char *string )
/**********************************************/
/* print out preprocessed source lines
 */
{
    static bool PrintEmptyLine = TRUE;

    /* filter some macro specific directives */
    if ( AsmBuffer[0]->token == T_DIRECTIVE &&
         ( AsmBuffer[0]->value == T_ENDM ||
           AsmBuffer[0]->value == T_EXITM))
        return;

    /* don't print generated code - with one exception:
     if the code was generated as a result of structure initialization,
     then do!
     */
    if ( GeneratedCode )
        return;

    if (Token_Count > 0) {
        PrintEmptyLine = TRUE;
        printf("%s\n", string);
    } else if (PrintEmptyLine) {
        PrintEmptyLine = FALSE;
        printf("\n");
    }
}

// set Masm v5.1 compatibility options

void SetMasm510( bool value )
/***************************/
{
    ModuleInfo.m510 = value;
    ModuleInfo.oldstructs = value;
    //ModuleInfo.oldmacros = value; not implemented yet
    ModuleInfo.dotname = value;
    ModuleInfo.setif2 = value;

    if ( value ) {
        if ( ModuleInfo.model == MOD_NONE ) {
            /* if no model is specified, set OFFSET:SEGMENT */
            ModuleInfo.offsettype = OT_SEGMENT;
            if ( ModuleInfo.langtype == LANG_NONE ) {
                ModuleInfo.scoped = FALSE;
                ModuleInfo.procs_private = TRUE;
            }
        }
    }
    return;
}

/* memory model has been defined by cmdline option */

static void SetModelCmdline( void )
/*********************************/
{
    char *model;
    char buffer[20];

    DebugMsg(( "SetModelCmdline() enter\n" ));
    switch( Options.model ) {
    case MOD_FLAT:
        model = "FLAT";
        if (Options.cpu < 3) /* ensure that a 386 cpu is set */
            Options.cpu = 3;
        break;
    case MOD_COMPACT:  model = "COMPACT";   break;
    case MOD_HUGE:     model = "HUGE";      break;
    case MOD_LARGE:    model = "LARGE";     break;
    case MOD_MEDIUM:   model = "MEDIUM";    break;
    case MOD_SMALL:    model = "SMALL";     break;
    case MOD_TINY:     model = "TINY";      break;
    default: return;
    }
    strcpy( buffer, ".MODEL " );
    strcat( buffer, model );
    PushLineQueue();
    AddLineQueue( buffer );
}

// called for each pass

static void ModulePassInit( void )
/********************************/
{
    /* set default values not affected by the masm 5.1 compat switch */
    ModuleInfo.langtype = Options.langtype;
    ModuleInfo.procs_private = FALSE;
    ModuleInfo.procs_export = FALSE;
    ModuleInfo.offsettype = OT_GROUP;
    ModuleInfo.scoped = TRUE;

    if ( Options.model != MOD_NONE )
        SetModelCmdline(); /* if memory model has been set with -m{s|f|...} */

    SetMasm510( Options.masm51_compat );
    ModuleInfo.defOfssize = USE16;
    ModuleInfo.ljmp     = TRUE;

    ModuleInfo.list   = Options.write_listing;
    ModuleInfo.cref   = TRUE;
    ModuleInfo.listif = Options.listif;
    ModuleInfo.list_generated_code = Options.list_generated_code;
    ModuleInfo.list_macro = Options.list_macro;

    ModuleInfo.case_sensitive = Options.case_sensitive;
    ModuleInfo.convert_uppercase = Options.convert_uppercase;
    SymSetCmpFunc();

    ModuleInfo.segorder = SEGORDER_SEQ;
    ModuleInfo.radix = 10;
    ModuleInfo.fieldalign = Options.fieldalign;
#if PROCALIGN
    ModuleInfo.procalign = 0;
#endif
}

void RunLineQueue( void )
/***********************/
{
    char *OldCurrSource = CurrSource;
    bool old_line_listed = line_listed;
    int currlevel = queue_level;
    int i;

    DebugMsg(( "%lu. RunLineQueue() enter\n", LineNumber ));
    CurrSource += strlen( CurrSource ) + 1;
    GeneratedCode++;
    while ( queue_level >= currlevel ) {
        while (0 == (i = GetPreprocessedLine( CurrSource )));
        if ( i > 0 )
            ParseItems();
    }
#ifdef DEBUG_OUT
    if ( ModuleInfo.EndDirectiveFound == TRUE ) {
        DebugMsg(("!!!!! Error: End directive found in generated-code parser loop!\n"));
    }
#endif
    GeneratedCode--;
    CurrSource = OldCurrSource;
    line_listed = old_line_listed;

    DebugMsg(( "%lu. RunLineQueue() exit\n", LineNumber ));
    return;
}

// this is called by InitializeStructure(), which is a special case

void RunLineQueueEx( void )
/*************************/
{
    char *OldCurrSource = CurrSource;
    bool old_line_listed = line_listed;
    int currlevel = queue_level;
    int old_list_pos;
    int i;

    DebugMsg(( "RunLineQueueEx() enter\n" ));
    CurrSource += strlen( CurrSource ) + 1;
    while ( queue_level >= currlevel ) {
        while (0 == (i = GetPreprocessedLine( CurrSource )));
        if ( i > 0 ) {
            old_list_pos = list_pos;
            ParseItems();
            /* handle special case 'structure initialization' */
#if FASTPASS
            StoreLine( CurrSource );
            LineStoreCurr->list_pos = old_list_pos;
#endif
            if ( Options.preprocessor_stdout == TRUE )
                WritePreprocessedLine( CurrSource );
        }
    }
#ifdef DEBUG_OUT
    if ( ModuleInfo.EndDirectiveFound == TRUE ) {
        DebugMsg(("!!!!! Error: End directive found in generated-code parser loop!\n"));
    }
#endif
    CurrSource = OldCurrSource;
    line_listed = old_line_listed;

    DebugMsg(( "RunLineQueueEx() exit\n" ));
    return;
}

// do ONE assembly pass
// the FASTPASS variant (which is default now) doesn't scan the full source
// for each pass. For this to work, the following things are implemented:
// 1. in pass one, save state if the first byte is to be emitted.
//    <state> is the segment stack, moduleinfo state, ...
// 2. once the state is saved, all preprocessed lines must be stored.
//    this can be done here, in OnePass, the line is in <string>.
// 3. for subsequent passes do
//    - restore the state
//    - read preprocessed lines and feed ParseItems() with it

static unsigned long OnePass( void )
/**********************************/
{
    int i;

    InputPassInit();
    ModulePassInit();
    SymPassInit( Parse_Pass );
    LabelsInit();
    SegmentInit( Parse_Pass );
    ContextInit( Parse_Pass );
    ProcInit();
    TypesInit();
    HllInit();
    MacroInit( Parse_Pass ); /* insert predefined macros */
    AssumeInit();
    CmdlParamsInit( Parse_Pass );

    ModuleInfo.EndDirectiveFound = FALSE;
    PhaseError = FALSE;
    Modend = FALSE;
    //LineNumber = 0;
    lastLineNumber = 0;
    dmyproc = NULL;
    GlobalVars.code_seg = FALSE;
    GlobalVars.data_in_code = FALSE;

#if FASTPASS
    StoreState = FALSE;
    if ( Parse_Pass > PASS_1 && UseSavedState == TRUE ) {
        /* the functions above might have written something to the line_queue */
        if ( line_queue )
            RunLineQueue();
        RestoreState();
#ifdef DEBUG_OUT
        DebugMsg(("OnePass(%u) segment sizes:\n", Parse_Pass + 1));
        {
            dir_node *dir;
            for( dir = Tables[TAB_SEG].head; dir; dir = dir->next ) {
                if( ( dir->sym.state != SYM_SEG ) || ( dir->sym.segment == NULL ) )
                    continue;
                DebugMsg(("OnePass(%u): segm=%-8s size=%8X\n", Parse_Pass + 1, dir->sym.name, dir->sym.max_offset ));
            }
        }
#endif
        LineStoreCurr = LineStoreHead;
        while ( LineStoreCurr && ModuleInfo.EndDirectiveFound == FALSE ) {
            strcpy( srclinebuffer, LineStoreCurr->line );
            if ( LineStoreCurr->srcfile == 0xFFF ) {
                MacroLevel = 1;
            } else {
                set_curr_srcfile( LineStoreCurr->srcfile, LineStoreCurr->lineno );
                MacroLevel = 0;
            }
            DebugMsg(("%u. OnePass(%u) mlvl=%u: >%s<\n", LineNumber, Parse_Pass+1, MacroLevel, srclinebuffer ));
            if ( Token_Count = Tokenize( srclinebuffer, 0 ) )
                ParseItems();
            LineStoreCurr = LineStoreCurr->next;
        }
    } else
#endif
    while ( ModuleInfo.EndDirectiveFound == FALSE ) {
        while (0 == (i = GetPreprocessedLine( srclinebuffer )));
        if (i < 0)
            break;
#if FASTPASS
        if (StoreState)
            StoreLine( srclinebuffer );
#endif
        ParseItems();

        if ( Options.preprocessor_stdout == TRUE && Parse_Pass == PASS_1 )
            WritePreprocessedLine( srclinebuffer );

    }
    /* if -Zd is set and there is trailing code not inside
     * a function, set the dummy function's length now.
     */
    if ( dmyproc ) {
        dmyproc->total_size =
            ((dir_node *)dmyproc->segment)->e.seginfo->current_loc -
            dmyproc->offset;
        DebugMsg(("OnePass: last dummy proc size=%Xh\n"));
    }
    if ( Parse_Pass == PASS_1 ) {
        CheckProcOpen();
        HllCheckOpen();

        if( ModuleInfo.EndDirectiveFound == FALSE )
            AsmError( END_DIRECTIVE_REQUIRED );
    }
    ClearFileStack();

    return( 1 );
}

static void scan_global( void )
/*****************************/
{
    /* make all symbols of type SYM_INTERNAL, which aren't
     a constant, public.  */
    if ( Options.all_symbols_public )
        SymMakeAllSymbolsPublic();

    /* turn EXTERNDEFs into either EXTERNs or PUBLICs as appropriate */
    GetGlobalData();
}

/*
 set index field for EXTERN/PROTO/COMM.
 This is called after PASS 1 has been finished.
 */

static void set_ext_idx( void )
/*****************************/
{
    dir_node    *curr;
    uint        index = 0;

    // scan the EXTERN/EXTERNDEF items

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* v2.01: externdefs which have been "used" become "strong" */
        if ( curr->sym.used )
            curr->sym.weak = FALSE;
        /* skip COMM and unused EXTERNDEF/PROTO items */
        if (( curr->sym.comm == TRUE ) || ( curr->sym.weak == TRUE ))
            continue;
        index++;
        curr->sym.idx = index;
#if FASTPASS
        if ( curr->sym.altname &&
            curr->sym.altname->state != SYM_INTERNAL ) {
            /* do not use saved state, scan full source in second pass */
            SkipSavedState();
        }
#endif
    }

#if 0 /* v2.01: PROTOs are now in TAB_EXT */
    // now scan the PROTO items
    for(curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        /* the item must be USED and PROTO */
        if( curr->sym.used && (curr->sym.isproc == FALSE )) {
            index++;
            curr->sym.idx = index;
        }
    }
#endif

    // now scan the COMM items

    for( curr = Tables[TAB_EXT].head; curr != NULL; curr = curr->next ) {
        if ( curr->sym.comm == FALSE )
            continue;
        index++;
        curr->sym.idx = index;
    }
    return;
}

#if BUILD_TARGET
/*
 * from WASM : get os-specific xxx_INCLUDE environment variable.
 *             if set, add string to include path.
 */

static void get_os_include( void )
/********************************/
{
    char *env;
    char *tmp;

    /* add OS_include to the include path */

    tmp = AsmTmpAlloc( strlen( Options.build_target ) + 10 );
    strcpy( tmp, Options.build_target );
    strcat( tmp, "_INCLUDE" );

    env = getenv( tmp );
    if( env != NULL ) {
        AddStringToIncludePath( env );
    }
}

#endif

static void get_module_name( void )
/*********************************/
{
    char dummy[_MAX_EXT];
    char        *p;

    _splitpath( FileInfo.fname[ASM], NULL, NULL, ModuleInfo.name, dummy );
    _strupr( ModuleInfo.name );
    for( p = ModuleInfo.name; *p != '\0'; ++p ) {
        if( !( isalnum( *p ) || ( *p == '_' ) || ( *p == '$' )
            || ( *p == '@' ) || ( *p == '?') ) ) {
            /* it's not a legal character for a symbol name */
            *p = '_';
        }
    }
    /* first character can't be a number either */
    if( isdigit( ModuleInfo.name[0] ) ) {
        ModuleInfo.name[0] = '_';
    }
}

// called by AssembleInit(), once per source module
// symbol table has been initialized

static void ModuleInit( void )
/****************************/
{
    ModuleInfo.error_count = 0;
    ModuleInfo.warning_count = 0;
    ModuleInfo.model = MOD_NONE;
    //ModuleInfo.distance = STACK_NONE;
    ModuleInfo.ostype = OPSYS_DOS;
    ModuleInfo.emulator = (Options.floating_point == FPO_EMULATION);
    ModuleInfo.flatgrp_idx = 0;

    get_module_name(); /* set ModuleInfo.name */

    SimpleType[ST_PROC].mem_type = MT_NEAR;

    memset(Tables, 0, sizeof(Tables[0]) * TAB_LAST );
    SafeSEHStack = NULL;
    if ( formatoptions[Options.output_format].init )
        formatoptions[Options.output_format].init( &ModuleInfo );
    return;
}

static void ReswTableInit( void )
/*******************************/
{
    /* initialize reserved word hash table.
     * this must be called for each source module.
     */
    if ( ParseInit() == ERROR )
        exit( 1 );        // tables wrong, internal error

    /* this must be done AFTER ParseInit has been called */
    if ( Options.output_format == OFORMAT_OMF ) {
        //DebugMsg(("InitAsm: disable IMAGEREL+SECTIONREL\n"));
        /* for OMF, IMAGEREL and SECTIONREL make no sense */
#if IMAGERELSUPP
        DisableKeyword( T_IMAGEREL );
#endif
#if SECTIONRELSUPP
        DisableKeyword( T_SECTIONREL );
#endif
    }

    if ( Options.strict_masm_compat == TRUE ) {
        DebugMsg(("InitAsm: disable INCBIN + FASTCALL keywords\n"));
        DisableKeyword( T_INCBIN );
        DisableKeyword( T_FASTCALL );
    }

    return;
}

static void open_files( void )
/****************************/
{
    /* open ASM file */
    DebugMsg(("open_files() enter\n" ));

//    FileInfo.file[ASM] = fopen( FileInfo.fname[ASM], "r" );
    FileInfo.file[ASM] = fopen( FileInfo.fname[ASM], "rb" );

    if( FileInfo.file[ASM] == NULL ) {
        DebugMsg(("open_files(): fopen(%s) failed\n", FileInfo.fname[ASM] ));
        Fatal( FATAL_CANNOT_OPEN_FILE, FileInfo.fname[ASM], errno );
    }

    /* open OBJ file */
    if ( Options.syntax_check_only == FALSE ) {
        FileInfo.file[OBJ] = fopen( FileInfo.fname[OBJ], "wb" );
        if( FileInfo.file[OBJ] == NULL ) {
            DebugMsg(("open_files(): fopen(%s) failed\n", FileInfo.fname[OBJ] ));
            Fatal( FATAL_CANNOT_OPEN_FILE, FileInfo.fname[OBJ], errno );
        }
        DebugMsg(("open_files(): output, fopen(%s) ok\n", FileInfo.fname[OBJ] ));
    }

    if ( Options.output_format == OFORMAT_OMF )
        omf_init( &ModuleInfo );

    /* delete any existing ERR file */
    InitErrFile();
}

void close_files( void )
/**********************/
{
    /* close ASM file */
    if( FileInfo.file[ASM] != NULL ) {
        if( fclose( FileInfo.file[ASM] ) != 0 )
            Fatal( FATAL_CANNOT_CLOSE_FILE, FileInfo.fname[ASM], errno  );
        FileInfo.file[ASM] = NULL;
    }

    if ( Options.output_format == OFORMAT_OMF )
        omf_fini();

    /* close OBJ file */
    if ( FileInfo.file[OBJ] != NULL ) {
        if ( fclose( FileInfo.file[OBJ] ) != 0 )
            Fatal( FATAL_CANNOT_CLOSE_FILE, FileInfo.fname[OBJ], errno  );
        FileInfo.file[OBJ] = NULL;
    }

    LstCloseFile();

    if( ModuleInfo.error_count > 0 ) {
        if ( FileInfo.file[ERR] != NULL )
            fclose( FileInfo.file[ERR] );
        FileInfo.file[ERR] = NULL;
        /* delete the object module if errors occured */
        remove( FileInfo.fname[OBJ] );
    }
}

// init assembler. called once per module

static void AssembleInit( void )
/******************************/
{
    DebugMsg(("AssembleInit() enter\n"));

    memset( &ModuleInfo, 0, sizeof(ModuleInfo));
#ifdef DEBUG_OUT
    ModuleInfo.cref = TRUE; /* don't suppress debug displays */
#endif
    ModendRec     = NULL; /* OMF */
    start_label   = NULL; /* COFF */
    write_to_file = FALSE;
    StructInit = 0;
    GeneratedCode = 0;
    LinnumQueue.head = NULL;
#if FASTPASS
    modstate.init = FALSE;
    LineStoreHead = NULL;
    LineStoreTail = NULL;
    UseSavedState = FALSE;
#endif
    CommentDataInCode = (Options.output_format == OFORMAT_OMF &&
                         Options.no_comment_data_in_code_records == FALSE);

    open_files();
#if BUILD_TARGET
    get_os_include();
#endif
    ReswTableInit();
    InputInit();

    OmfRecInit(); /* needed for all formats because of objrec in seg_info */
    SymInit();
    QueueInit();
    ModuleInit();
    CondInit();
    ExprEvalInit();
    OmfFixInit();
    DebugMsg(("AssembleInit() exit\n"));
}

#ifdef DEBUG_OUT
    void DumpInstrStats( void );
#endif

// called once per module. AssembleModule() cleanup

static void AssembleFini( void )
/******************************/
{
    SymFini();
#ifdef DEBUG_OUT
    DumpInstrStats();
    MacroFini();
#endif
    QueueFini();
    OmfFixFini();
    OmfRecFini(); /* needed for all formats because of objrec in seg_info */
    close_files();
    InputFini();
}

// AssembleModule() assembles the source and writes the object module

void AssembleModule( void )
/*************************/
{
    unsigned long       prev_total = -1;
    unsigned long       curr_total;
    int                 starttime;
    int                 endtime;
    dir_node            *dir;

    DebugMsg(("AssembleModule enter\n"));

    AssembleInit();

    LstOpenFile();

#if 0 //ndef __UNIX__
    starttime = GetTickCount();
#else
    starttime = clock();
#endif

    for( Parse_Pass = PASS_1; ; Parse_Pass++ ) {

        DebugMsg(( "*************\npass %u\n*************\n", Parse_Pass + 1 ));
        OnePass();

        if ( Parse_Pass == PASS_1 && ModuleInfo.error_count == 0 ) {
            DebugMsg(("AssembleModule(%u): pass 1 actions\n", Parse_Pass + 1));
            if ( ERROR == CheckForOpenConditionals() )
                break;
            if ( Options.syntax_check_only == FALSE )
                write_to_file = TRUE;
            /* convert EXTERNDEFs into EXTERNs, PUBLICs or nothing */
            scan_global();
            /* set external index field in EXTERNal/PROTO/COMM */
            set_ext_idx();
#if FASTPASS
            if ( Tables[TAB_UNDEF].head ) {
                DebugMsg(("AssembleModule: undefined symbols after pass 1:\n"));
#ifdef DEBUG_OUT
                for ( dir = Tables[TAB_UNDEF].head; dir; dir = dir->next ) {
                    DebugMsg(("AssembleModule: %s\n", dir->sym.name ));
                }
#endif
                /* to force a full second pass in case of missing symbols,
                 * activate the next line. It was implemented to have proper
                 * error displays if a forward reference wasn't found.
                 * However, v1.95 final won't need this anymore, because both
                 * filename + lineno for every line is known now in pass 2.
                 */
                // SkipSavedState();
            }
#endif

            if ( write_to_file && ( Options.output_format == OFORMAT_OMF ) ) {
                WriteHeader( TRUE );
            }
#ifdef DEBUG_OUT
            DebugMsg(("AssembleModule forward references:\n"));
            for( dir = Tables[TAB_SEG].head; dir; dir = dir->next ) {
                int i;
                int j;
                asm_sym * sym;
                struct asmfixup * fix;
                if( ( dir->sym.state != SYM_SEG ) || ( dir->sym.segment == NULL ) )
                    continue;
                for (i = 0, j = 0, sym = dir->e.seginfo->labels;sym;sym = (asm_sym *)((dir_node *)sym)->next) {
                    i++;
                    for ( fix = sym->fixup; fix ; fix = fix->nextbp, j++ );
                }
                DebugMsg(("AssembleModule: segm=%s, labels=%u forward refs=%u\n", dir->sym.name, i, j));
            }
#endif
        }

        DebugMsg(("AssembleModule(%u): errorcnt=%u\n", Parse_Pass + 1, ModuleInfo.error_count ));
        if( ModuleInfo.error_count > 0 )
            break;

        for ( curr_total = 0, dir = Tables[TAB_SEG].head; dir ; dir = dir->next ) {
            curr_total += dir->e.seginfo->bytes_written;
        }
        DebugMsg(("AssembleModule(%u): PhaseError=%u, prev_total=%lX, curr_total=%lX\n", Parse_Pass + 1, PhaseError, prev_total, curr_total));
        if( !PhaseError && prev_total == curr_total ) {
            uint_32 tmp = LineNumber;
            if ( write_to_file == FALSE )
                break;
            set_curr_srcfile( 0, 0 ); /* no line reference for errors/warnings */
            if ( Options.output_format == OFORMAT_OMF ) {
                WriteContent();
                WriteHeader( FALSE );
            } else {
                WriteHeader( TRUE );
                WriteContent();
            }
            set_curr_srcfile( 0, tmp );
            break;
        }
#ifdef DEBUG_OUT
        DebugMsg(("AssembleModule(%u) segment sizes:\n", Parse_Pass + 1));
        for( dir = Tables[TAB_SEG].head; dir; dir = dir->next ) {
            if( ( dir->sym.state != SYM_SEG ) || ( dir->sym.segment == NULL ) )
                continue;
            DebugMsg(("AssembleModule(%u): segm=%-8s size=%8X\n", Parse_Pass + 1, dir->sym.name, dir->sym.max_offset ));
        }
#endif
        DebugMsg(("AssembleModule(%u): prepare for next pass\n", Parse_Pass + 1));

        prev_total = curr_total;

        if ( Parse_Pass % 10000 == 9999 )
            AsmWarn( 2, ASSEMBLY_PASSES, Parse_Pass+1);
#ifdef DEBUG_OUT
        if ( Options.max_passes && Parse_Pass == (Options.max_passes - 1) )
            break;
#endif
        if ( Options.line_numbers ) {
            if ( Options.output_format == OFORMAT_COFF ) {
                for( dir = Tables[TAB_SEG].head; dir; dir = dir->next ) {
                    if ( dir->e.seginfo->LinnumQueue )
                        QueueDeleteLinnum( dir->e.seginfo->LinnumQueue );
                    dir->e.seginfo->LinnumQueue = NULL;
                }
            } else {
                QueueDeleteLinnum( &LinnumQueue );
                LinnumQueue.head = NULL;
            }
        }

        /* set file position of ASM and LST files for next pass */

        rewind( FileInfo.file[ASM] );
        if ( Options.output_format == OFORMAT_OMF )
            omf_set_filepos();

#if FASTPASS
        if ( UseSavedState == FALSE && FileInfo.file[LST] ) {
#else
        if ( FileInfo.file[LST] ) {
#endif
            LstCloseFile();
            LstOpenFile();
        }
    }

    DebugMsg(("AssembleModule: finished, cleanup\n"));

    /* Write a symbol listing file (if requested) */
    LstWriteCRef();

#if 0 //ndef __UNIX__
    endtime = GetTickCount();
#else
    endtime = clock(); // is in ms already
#endif

    sprintf( srclinebuffer, MsgGet( MSG_ASSEMBLY_RESULTS, NULL ),
             GetFName( ModuleInfo.srcfile )->name,
             LineNumber,
             Parse_Pass + 1,
             endtime - starttime,
             ModuleInfo.warning_count,
             ModuleInfo.error_count);
    if ( Options.quiet == FALSE )
        printf( "%s\n", srclinebuffer );

    if ( FileInfo.file[LST] ) {
        LstPrintf( srclinebuffer );
        LstNL();
        LstCloseFile();
    }
    AssembleFini();
    DebugMsg(("AssembleModule exit\n"));
}
