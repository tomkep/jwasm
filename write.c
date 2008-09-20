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
* Description:  Write translated object module.
*
****************************************************************************/


#include "globals.h"
#include <errno.h>
#include <ctype.h>
#include <time.h>
#ifdef __WATCOMC__
#include <unistd.h>
#else
#include <io.h>
#endif

#include "symbols.h"
#include "memalloc.h"
#include "input.h"
#include "condasm.h"
#include "directiv.h"
#include "assume.h"
#include "proc.h"
#include "expreval.h"
#include "hll.h"
#include "labels.h"
#include "macro.h"
#include "parser.h"
#include "queues.h"
#include "types.h"
#include "fixup.h"
#include "omfprs.h"
#include "omf.h"
#include "fastpass.h"
#include "listing.h"

#if COFF_SUPPORT
#include "coff.h"
#endif
#if ELF_SUPPORT
#include "elf.h"
#endif
#if BIN_SUPPORT
#include "bin.h"
#endif

extern int              CheckForOpenConditionals( void );
extern void             set_cpu_parameters( void );
extern void             set_fpu_parameters( void );
extern void             CheckProcOpen( void );
extern void             CmdlParamsInit( int );
extern void             SortSegments( void );

#ifndef __UNIX__
#include "win32.h"
#endif

extern symbol_queue     Tables[];       // tables of definitions
extern obj_rec          *ModendRec;     // Record for Modend (OMF)
extern asm_sym          *start_label;   // symbol for Modend (COFF)
extern uint_8           CheckSeg;
extern pobj_state       pobjState;      // object file information

/* fields: next, name, segment, offset/value */
struct asm_sym LineItem = {NULL,"@Line", NULL, 0};
struct asm_sym WordSize = {NULL,"@WordSize", NULL, 0};

static uint_32          BytesTotal;     // total of ledata bytes generated
unsigned int            Parse_Pass;     // phase of parsing

global_vars             GlobalVars;

static unsigned long    lastLineNumber;
static unsigned         seg_pos;        // file pos of SEGDEF record(s)
static unsigned         public_pos;     // file pos of PUBDEF record(s)
unsigned                total_segs;

#if FASTPASS

void preprocessor_output( void );

mod_state modstate;
line_item *LineStoreHead;
line_item *LineStoreTail;
line_item *LineStoreCurr;
bool StoreState;
bool UseSavedState;
uint_32 list_pos;

#endif

bool                    write_to_file;  // write object module
bool                    Modend;         // end of module is reached
bool                    PhaseError;     // phase error occured
bool                    EndDirectiveFound;

void AddLinnumDataRef( void )
/***************************/
/* store a reference for the current line at the current address */
{
    struct line_num_info    *curr;
    unsigned long           line_num;

    line_num = LineNumber;
    if( line_num < 0x8000 )  {
        if( lastLineNumber != line_num ) {
            curr = AsmAlloc( sizeof( struct line_num_info ) );
            curr->number = line_num;
            curr->offset = GetCurrOffset();
            curr->srcfile = get_curr_srcfile();

            AddLinnumData( curr );
            lastLineNumber = line_num;
        }
    }
}

#if FASTPASS
void SaveState( void )
{
    int i;
    DebugMsg(("SaveState enter\n"));
    StoreState = TRUE;
    UseSavedState = TRUE;
    modstate.init = TRUE;
    modstate.EquHead = modstate.EquTail = NULL;

    memcpy(&modstate.modinfo, &ModuleInfo, sizeof(module_info));

    SegmentSaveState();
    AssumeSaveState();

    i = strlen(CurrString);
    LineStoreCurr = AsmAlloc(i+1+sizeof(line_item));
    LineStoreCurr->next = NULL;
    strcpy(LineStoreCurr->line, CurrString);

    if (AsmFiles.file[LST])
        list_pos = ftell(AsmFiles.file[LST]);

    LineStoreHead = LineStoreTail = LineStoreCurr;
    DebugMsg(("SaveState: curr line=>%s<\n", CurrString));
}

/* an error has been detected in pass one. it should be
 reported in pass 2, so ensure that a full source scan is done then
 */

void SkipSavedState( void )
{
    if ( UseSavedState == TRUE ) {
        UseSavedState = FALSE;
        if (AsmFiles.file[LST]) {
            rewind( AsmFiles.file[LST] );
        }
    }
}

static void RestoreState( void )
{
    static line_item endl = {NULL, 0, "END"};

    DebugMsg(("RestoreState enter\n"));
    if (modstate.init) {
        equ_item *curr;
        for (curr = modstate.EquHead; curr; curr = curr->next) {
//            printf("RestoreState: sym >%s<, value=%u, defined=%u\n", curr->sym->name, curr->value, curr->defined);
            if (curr->sym->mem_type == MT_ABS) {
                curr->sym->value   = curr->value;
                curr->sym->defined = curr->defined;
            }
        }
        memcpy(&ModuleInfo, &modstate.modinfo, sizeof(module_info));
    }

    if (AsmFiles.file[LST])
        fseek(AsmFiles.file[LST], list_pos, SEEK_SET);

    if (LineStoreHead == NULL) {
        endl.lineno = LineNumber;
        LineStoreHead = &endl;
    }
    return;
}

void StoreLine( char * string )
{
    int i;
    i = strlen(string);
    LineStoreCurr = AsmAlloc(i+1+sizeof(line_item));
    LineStoreCurr->next = NULL;
    LineStoreCurr->lineno = LineNumber;
    strcpy(LineStoreCurr->line, string);
    LineStoreTail->next = LineStoreCurr;
    LineStoreTail = LineStoreCurr;
}

void InputQueueLineEx( char *buffer, bool index )
{
    int i = strlen(buffer);
    line_item *p;

    DebugMsg(( "InputQueueLineEx(%u): >%s<\n", Parse_Pass+1, buffer ));

    /* special handling only if the saved state is used! */
    if ( Parse_Pass == PASS_1 || UseSavedState == FALSE ) {
        if (index == 0 && StoreState) {
            *(LineStoreCurr->line) = ';';
        }
        InputQueueLine( buffer );
        return;
    }

    p = AsmAlloc(i+1+sizeof(line_item));
    p->lineno = LineNumber;
    strcpy(p->line, buffer);
    if (index == 0) {
        p->next = LineStoreCurr->next;
        LineStoreCurr->next = p;
        *(LineStoreCurr->line) = ';';
    } else {
        p->next = LineStoreCurr->next->next;
        LineStoreCurr->next->next = p;
    }
}
#endif

void OutputByte( unsigned char byte )
/********************************/
/* Write a byte to the object file */
{
    if( CurrSeg == NULL) {
        if (CheckSeg == TRUE) {
            AsmError( DATA_EMITTED_WITH_NO_SEGMENT );
            CheckSeg = FALSE;
        }
        return;
    }

    CurrSeg->seg->e.seginfo->current_loc++;
    if( CurrSeg->seg->e.seginfo->current_loc >=
        CurrSeg->seg->e.seginfo->segrec->d.segdef.seg_length ) {
        CurrSeg->seg->e.seginfo->segrec->d.segdef.seg_length = CurrSeg->seg->e.seginfo->current_loc;
    }

    if( write_to_file == TRUE) {
        uint_32 idx = CurrSeg->seg->e.seginfo->current_loc - CurrSeg->seg->e.seginfo->start_loc - 1;
        CurrSeg->seg->e.seginfo->CodeBuffer[idx] = byte;
        if( Options.output_format == OFORMAT_OMF && idx >= MAX_LEDATA_THRESHOLD) {
            omf_FlushCurrSeg();
        }
    }
#if FASTPASS
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif
    BytesTotal++;
}

void OutputCodeByte( unsigned char byte )
/************************************/
{
    if( CurrSeg != NULL ) {
        if( CurrSeg->seg->e.seginfo->segtype == SEGTYPE_UNDEF ) {
            CurrSeg->seg->e.seginfo->segtype = SEGTYPE_CODE;
        }
    }
    if (Options.no_comment_data_in_code_records == FALSE)
        omf_OutSelect( FALSE );
    OutputByte( byte );
}


void OutputDataByte( unsigned char byte )
/************************************/
{
    if (Options.no_comment_data_in_code_records == FALSE)
        omf_OutSelect( TRUE );
    OutputByte( byte );
}

// set current position in current segment without to write anything

int SetCurrOffset( int_32 value, bool relative, bool select_data )
/************************************************************************/
{
    if( CurrSeg == NULL ) {
        AsmError(MUST_BE_IN_SEGMENT_BLOCK);
        return( ERROR );
    }

    if( relative ) {
        value += GetCurrOffset();
    }

    if (Options.output_format == OFORMAT_OMF)
        omf_FlushCurrSeg( );
#if BIN_SUPPORT
    else if (Options.output_format == OFORMAT_BIN) {
        if (CurrSeg->seg->e.seginfo->current_loc == 0 &&
            CurrSeg->seg->e.seginfo->initial == FALSE) {
            CurrSeg->seg->e.seginfo->start_loc = value;
            CurrSeg->seg->e.seginfo->initial = TRUE;
        }
    }
#endif
    if( select_data )
        if (Options.no_comment_data_in_code_records == FALSE)
            omf_OutSelect( TRUE );
    CurrSeg->seg->e.seginfo->current_loc = value;
    if (Options.output_format == OFORMAT_OMF) {
        CurrSeg->seg->e.seginfo->start_loc = value;
    }

    if( CurrSeg->seg->e.seginfo->current_loc >=
        CurrSeg->seg->e.seginfo->segrec->d.segdef.seg_length ) {
        CurrSeg->seg->e.seginfo->segrec->d.segdef.seg_length = CurrSeg->seg->e.seginfo->current_loc;
    }

#if FASTPASS
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif
    return( NOT_ERROR );
}

// finish module writes
// for OMF, just write the MODEND record
// for COFF,ELF and BIN, write the section data and symbol table

static int WriteContent( void )
/*****************************/
{
    DebugMsg(("WriteContent enter\n"));
    switch (Options.output_format) {
    case OFORMAT_OMF:
        if( ModendRec == NULL ) {
            AsmError( UNEXPECTED_END_OF_FILE );
            return ERROR;
        }
        omf_write_record( ModendRec, TRUE );
        break;
#if COFF_SUPPORT
    case OFORMAT_COFF:
        coff_write_data(pobjState.file_out->fh);
        coff_write_symbols(pobjState.file_out->fh);
        break;
#endif
#if ELF_SUPPORT
    case OFORMAT_ELF:
        elf_write_data(pobjState.file_out->fh);
        break;
#endif
#if BIN_SUPPORT
    case OFORMAT_BIN:
        bin_write_data(pobjState.file_out->fh);
        break;
#endif
    }
    DebugMsg(("WriteContent exit\n"));
    return NOT_ERROR;
}

/*
 write the OMF/COFF header
 for OMF, this is called twice, once after Pass 1 is done
 and then again after assembly has finished without errors.
 for COFF/ELF/BIN, it's just called once.
*/
static int WriteHeader( bool initial )
/*********************************/
{
    dir_node    *curr;

    DebugMsg(("WriteHeader(%u) enter\n", initial));

    /* calc the number of sections */
    total_segs = 0;
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if( ( curr->sym.segment == NULL )
          && ( curr->e.seginfo->group == NULL ) )
            AsmErr( SEG_NOT_DEFINED, curr->sym.name );
        total_segs++;
    }

    switch (Options.output_format) {
    case OFORMAT_OMF:
        if (initial == TRUE) {
            omf_write_header();
            if( Options.no_dependencies == FALSE)
                omf_write_autodep();
            if( ModuleInfo.segorder == SEGORDER_DOSSEG )
                omf_write_dosseg();
            else if( ModuleInfo.segorder == SEGORDER_ALPHA )
                SortSegments();
            omf_write_lib();
            omf_write_lnames();
            seg_pos = tell(pobjState.file_out->fh);
        } else {
            lseek(pobjState.file_out->fh, seg_pos, SEEK_SET);
        }

        omf_write_seg();
        if (initial == TRUE) {
            omf_write_grp();
            omf_write_extdef();
            omf_write_comdef();
            omf_write_alias();
            public_pos = tell(pobjState.file_out->fh);
        } else {
            lseek(pobjState.file_out->fh, public_pos, SEEK_SET);
        }
        omf_write_pub();
        if (initial == TRUE) {
            omf_write_export();
            omf_write_end_of_pass1();
        }
        break;
#if COFF_SUPPORT
    case OFORMAT_COFF:
        coff_write_header(pobjState.file_out->fh);
        coff_write_section_table(pobjState.file_out->fh);
        break;
#endif
#if ELF_SUPPORT
    case OFORMAT_ELF:
        elf_write_header(pobjState.file_out->fh);
        break;
#endif
#if BIN_SUPPORT
    case OFORMAT_BIN:
        /* check if PROTOs or externals are used */
        for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next )
            if( curr->sym.used == TRUE && curr->sym.isproc == FALSE )
                break;
        if (curr || Tables[TAB_EXT].head) {
            AsmError(FORMAT_DOESNT_SUPPORT_EXTERNALS);
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
    return(NOT_ERROR);
}

// do ONE assembly pass
// the FASTPASS variant (which is default now) doesn't scan the full source
// for each pass. For this to work, the following things are implemented:
// 1. in pass one, save state if the first byte is to be emitted
//    state is the segment stack, moduleinfo state, ...
// 2. once the state is saved, all preprocessed lines must be stored.
//    this can be done here, in OnePass, the line is in <string>.
// 3. for subsequent passes do
//    - restore the state
//    - read preprocessed lines and feed ParseItems() with it

static unsigned long OnePass( char *string )
/******************************************/
{
    int i;

    SymPassInit( Parse_Pass );
    LabelsInit();
    SegmentInit( Parse_Pass );
    ProcInit();
    HllInit();
    MacroInit( Parse_Pass ); /* insert predefined macros */
    AssumeInit();
    CmdlParamsInit( Parse_Pass );

    EndDirectiveFound = FALSE;
    PhaseError = FALSE;
    Modend = FALSE;
    BytesTotal = 0;
    LineNumber = 0;
    lastLineNumber = 0;
    GlobalVars.data_in_code = FALSE;

#if FASTPASS
    StoreState = FALSE;
    if (Parse_Pass > PASS_1 && UseSavedState == TRUE) {
        RestoreState();
        LineStoreCurr = LineStoreHead;
        while (LineStoreCurr && EndDirectiveFound == FALSE) {
            strcpy(string, LineStoreCurr->line);
            DebugMsg(("AsmLine(%u): >%s<\n", Parse_Pass+1, string));
            LineNumber = LineStoreCurr->lineno;
            if (Token_Count = Tokenize(string, 0))
                ParseItems();
            LineStoreCurr = LineStoreCurr->next;
        }
        return( BytesTotal);
    }
#endif
    while (EndDirectiveFound == FALSE) {
        while (0 == (i = AsmLine( string )));
        if (i < 0)
            break;
#if FASTPASS
        if (StoreState)
            StoreLine(string);
#endif
        ParseItems();
    }
    CheckProcOpen();
    return( BytesTotal );
}

static void scan_global( void )
/******************************/
/* turn the globals into either externs or publics as appropriate */
{
    GetGlobalData();
}

/* set index field for EXTERN/PROTO/COMM
 */

static void set_ext_idx( )
/**********************************************/
{
    dir_node    *curr;
    uint        i;
    uint        index = 0;

    // first scan the EXTERN/EXTERNDEF items

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if ((curr->sym.comm == 1) || (curr->sym.weak == 1))
            continue;
        index++;
        curr->sym.idx = index;
    }

    // now scan the PROTO items

    for(curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        /* the item must be USED and PROTO */
        if( curr->sym.used && (curr->sym.isproc == FALSE )) {
            index++;
            curr->sym.idx = index;
        }
    }

    // now scan the COMM items

    for( curr = Tables[TAB_EXT].head; curr != NULL; curr = curr->next ) {
        if (curr->sym.comm == 0)
            continue;
        index++;
        curr->sym.idx = index;
    }
    return;
}

// init assembler. called once per module

static void AssembleInit( void )
/****************************/
{
    ModendRec     = NULL; /* OMF */
    start_label   = NULL; /* COFF */
    write_to_file = FALSE;
    GlobalVars.sel_idx = 0;
    GlobalVars.sel_start = 0;
    GlobalVars.code_seg = FALSE;
#if FASTPASS
    modstate.init = FALSE;
    LineStoreHead = NULL;
    LineStoreTail = NULL;
    UseSavedState = FALSE;
#endif

    SymInit();
    QueueInit();
    ModuleInit();
    CondInit();
    ExprEvalInit();
    TypesInit();
    FixInit();
}

// called once per module. AssembleModule() cleanup

static void AssembleFini( void )
/****************************/
{
    FixFini();
}

// AssembleModule() assembles the source and writes the object module

void AssembleModule( void )
/**************************/
{
    char                string[ MAX_LINE_LEN ];
    char                *p;
    unsigned long       prev_total = -1;
    unsigned long       curr_total;
    unsigned long       end_of_header;
    dir_node            *dir;
    int starttime;
    int endtime;

    DebugMsg(("AssembleModule enter\n"));
//    CodeBuffer = codebuf;

    AssembleInit();

    OpenLstFile();

#ifndef __UNIX__
    starttime = GetTickCount();
#else
    starttime = clock();
#endif

    for(Parse_Pass = PASS_1;;Parse_Pass++) {

        DebugMsg(( "*************\npass %u\n*************\n", Parse_Pass + 1 ));
        curr_total = OnePass( string );

        if( !EndDirectiveFound ) {
            AsmError( END_DIRECTIVE_REQUIRED );
            break;
        }
        while( PopLineQueue() ) {};

        DebugMsg(("AssembleModule(%u): errorcnt=%u\n", Parse_Pass + 1, ModuleInfo.error_count));
        if( ModuleInfo.error_count > 0 )
            break;

        if (Parse_Pass == PASS_1) {
            DebugMsg(("AssembleModule(%u): pass 1 actions\n", Parse_Pass + 1));
            if (ERROR == CheckForOpenConditionals())
                break;
            write_to_file = TRUE;
            /* put EXTERNDEFs into EXTERNs, PUBLICs or nothing */
            scan_global();
            /* set external index field in EXTERNal/PROTO/COMM */
            set_ext_idx();
            if (Options.output_format == OFORMAT_OMF) {
                WriteHeader(TRUE);
                end_of_header = tell(pobjState.file_out->fh);
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
                    for ( fix = sym->fixup; fix ; fix = fix->next1, j++ );
                }
                DebugMsg(("AssembleModule: segm=%s, labels=%u forward refs=%u\n", dir->sym.name, i, j));
            }
#endif
        }

        DebugMsg(("AssembleModule(%u): PhaseError=%u, prev_total=%X, curr_total=%X\n", Parse_Pass + 1, PhaseError, prev_total, curr_total));
        if( !PhaseError && prev_total == curr_total ) {
            if (Options.output_format == OFORMAT_OMF) {
                WriteContent();
                WriteHeader(FALSE);
            } else {
                WriteHeader(TRUE);
                WriteContent();
            }
            break;
        }
#ifdef DEBUG_OUT
        DebugMsg(("AssembleModule(%u) segment sizes:\n", Parse_Pass + 1));
        for( dir = Tables[TAB_SEG].head; dir; dir = dir->next ) {
            if( ( dir->sym.state != SYM_SEG ) || ( dir->sym.segment == NULL ) )
                continue;
            DebugMsg(("AssembleModule(%u): segm=%s size=%8X:\n", Parse_Pass + 1, dir->sym.name, dir->e.seginfo->segrec->d.segdef.seg_length));
        }
#endif
        DebugMsg(("AssembleModule(%u): prepare for next pass\n", Parse_Pass + 1));

        prev_total = curr_total;

        /* set file position of OBJ, ASM, LST files for next pass */

        if (Options.output_format == OFORMAT_OMF)
            lseek(pobjState.file_out->fh, end_of_header, SEEK_SET);

        rewind( AsmFiles.file[ASM] );
#if FASTPASS==0
        if (AsmFiles.file[LST])
            rewind( AsmFiles.file[LST] );
#endif
    }

    DebugMsg(("AssembleModule: finished, cleanup\n"));

    /* Write a symbol listing file (if requested) */
    WriteCRef();

#ifndef __UNIX__
    endtime = GetTickCount();
#else
    endtime = clock(); // is in ms already
#endif
    if (Parse_Pass == PASS_1)
        p = "";
    else
        p = "es";

    if (Options.quiet == FALSE) {
        sprintf( string, "%s: %lu lines, %u pass%s, time: %u ms, %u warnings, %u errors \n",
                 ModuleInfo.srcfile->name,
                 LineNumber,
                 Parse_Pass + 1,
                 p,
                 endtime - starttime,
                 ModuleInfo.warning_count,
                 ModuleInfo.error_count);
        printf(string);
    }
    if (AsmFiles.file[LST]) {
        fwrite(string, 1, strlen(string), AsmFiles.file[LST]);
        CloseLstFile();
    }

    SymFini();
    QueueFini();
    InputFini();
    AssembleFini();
}
