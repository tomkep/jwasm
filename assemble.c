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
#include "omfprs.h"
#include "omf.h"
#include "fastpass.h"
#include "listing.h"
#include "msgtext.h"

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

#ifndef __UNIX__
#include "win32.h"
#endif

extern symbol_queue     Tables[];       // tables of definitions
extern obj_rec          *ModendRec;     // Record for Modend (OMF)
extern asm_sym          *start_label;   // symbol for Modend (COFF)
extern pobj_state       pobjState;      // object file information
extern int              StructInit;     // see data.c


/* fields: next, name, segment, offset/value */
struct asm_sym LineItem = {NULL,"@Line", NULL, 0};
struct asm_sym WordSize = {NULL,"@WordSize", NULL, 0};

static uint_32          BytesTotal;     // total of ledata bytes generated
unsigned int            Parse_Pass;     // phase of parsing

global_vars             GlobalVars;

static unsigned long    lastLineNumber;
static unsigned         seg_pos;        // file pos of SEGDEF record(s)
static unsigned         public_pos;     // file pos of PUBDEF record(s)
unsigned int            total_segs;
unsigned int            GeneratedCode;

// buffer for source lines
// since the lines are sometimes concatenated
// the buffer must be a multiple of MAX_LINE_LEN

char srclinebuffer[ MAX_LINE_LEN * MAX_SYNC_MACRO_NESTING ];

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
bool            EndDirectiveFound;
bool            CheckSeg;       // restricts "data emitted wo segment" errors

void AddLinnumDataRef( void )
/***************************/
/* store a reference for the current line at the current address */
{
    struct line_num_info    *curr;
    unsigned long           line_num;

    DebugMsg(("AddLinnumDataRef enter, LineNumber=%u\n", LineNumber));
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

void StoreLine( char * string )
{
    int i;

    DebugMsg(("StoreLine: Line=%u, listpos=%u, MacroLevel=%u\n", LineNumber, list_pos, MacroLevel ));
    i = strlen(string) + 1;
    LineStoreCurr = AsmAlloc( i + sizeof(line_item) );
    LineStoreCurr->next = NULL;
    LineStoreCurr->lineno = LineNumber;
    LineStoreCurr->list_pos = list_pos;
    LineStoreCurr->macrolevel = MacroLevel;
    memcpy( LineStoreCurr->line, string, i );
    LineStoreTail->next = LineStoreCurr;
    LineStoreTail = LineStoreCurr;
}

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
    ContextSaveState(); /* save pushcontext/popcontext stack */

    i = strlen( CurrSource ) + 1;
    LineStoreCurr = AsmAlloc( i + sizeof(line_item) );
    LineStoreCurr->next = NULL;
    LineStoreCurr->lineno = LineNumber;
    LineStoreCurr->list_pos = list_pos_start;
    LineStoreCurr->macrolevel = MacroLevel;
    memcpy( LineStoreCurr->line, CurrSource, i );

    LineStoreHead = LineStoreTail = LineStoreCurr;
    DebugMsg(( "SaveState: curr line=>%s<, list pos=%u\n", CurrSource, list_pos ));
}

/* an error has been detected in pass one. it should be
 reported in pass 2, so ensure that a full source scan is done then
 */

void SkipSavedState( void )
{
    UseSavedState = FALSE;
}

static void RestoreState( void )
{
    static line_item endl = {NULL, 0, 0, 0, "END"};

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
        ModuleInfo.warning_count = i;
        SymSetCmpFunc();
    }

    if (LineStoreHead == NULL) {
        endl.lineno = LineNumber;
        LineStoreHead = &endl;
    }
    return;
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

extern uint_32 LastCodeBufSize;

// set current position in current segment without to write anything

ret_code SetCurrOffset( int_32 value, bool relative, bool select_data )
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
        LastCodeBufSize = value;
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

static ret_code WriteContent( void )
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
 write the OMF/COFF/ELF header
 for OMF, this is called twice, once after Pass 1 is done
 and then again after assembly has finished without errors.
 for COFF/ELF/BIN, it's just called once.
*/
static ret_code WriteHeader( bool initial )
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

#define is_valid_first_char( ch )  ( isalpha(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' || ch=='.' )

static int is_valid_identifier( char *id )
/*********************************/
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

static void add_predefined_tmacros(  )
/**************************************/
{
    struct qitem *p;
    char *name;
    char *value;
    int len;
    struct asm_sym *sym;

    DebugMsg(("add_predefined_tmacros enter\n"));
    for (p = Options.SymQueue;p;p = p->next) {
        DebugMsg(("add_predefined_tmacros: found >%s<\n", p->value));
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
            DebugMsg(("add_predefined_tmacros: name >%s< invalid\n", name));
            AsmError( SYNTAX_ERROR ); // fixme
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

static void add_incpaths( )
/**************************************/
{
    struct qitem *p;
    DebugMsg(("add_incpaths: enter\n"));
    for ( p = Options.IncQueue; p; p = p->next ) {
        AddStringToIncludePath( p->value );
    }
}

static void set_cpu_parameters( void )
/*****************************/
{
    int token;

    DebugMsg(("set_cpu_parameters enter\n"));
#if OWREGCONV
    // set naming convention
    if( Options.register_conventions || ( Options.cpu < 3 ) ) {
        Options.naming_convention = NC_ADD_USCORES;
    } else {
        Options.naming_convention = NC_DO_NOTHING;
    }
    // set parameters passing convention
    if( Options.cpu >= 3 ) {
        if( Options.register_conventions ) {
            add_constant( "__REGISTER__" );
        } else {
            add_constant( "__STACK__" );
        }
    }
#endif
    switch( Options.cpu ) {
    case 1:
        token = T_DOT_186;
        break;
    case 2:
        token =  Options.privileged_mode ? T_DOT_286P : T_DOT_286;
        break;
    case 3:
        token =  Options.privileged_mode ? T_DOT_386P : T_DOT_386;
        break;
    case 4:
        token =  Options.privileged_mode ? T_DOT_486P : T_DOT_486;
        break;
    case 5:
        token =  Options.privileged_mode ? T_DOT_586P : T_DOT_586;
        break;
    case 6:
        token =  Options.privileged_mode ? T_DOT_686P : T_DOT_686;
        break;
    default:
        token = T_DOT_8086;
        break;
    }
    cpu_directive( token );
    return;
}

static void set_fpu_parameters( void )
/*****************************/
{
    switch( Options.floating_point ) {
    case FPO_EMULATION:
//        add_constant( "__FPI__" );
        break;
    case FPO_NO_EMULATION:
//        add_constant( "__FPI87__" );
        break;
    case FPO_DISABLED:
//        add_constant( "__FPC__" );
        cpu_directive( T_DOT_NO87 );
        return;
    }
    switch( Options.fpu ) {
    case 0:
    case 1:
        cpu_directive( T_DOT_8087 );
        break;
    case 2:
        cpu_directive( T_DOT_287 );
        break;
    case 3:
    case 5:
    case 6:
        cpu_directive( T_DOT_387 );
        break;
    default: // unspecified FPU
        if ( Options.cpu < 2 )
            cpu_directive( T_DOT_8087 );
        else if ( Options.cpu == 2 )
            cpu_directive( T_DOT_287 );
        else
            cpu_directive( T_DOT_387 );
        break;
    }
    return;
}

// this is called for every pass.
// symbol table and ModuleInfo are initialized.

static void CmdlParamsInit( int pass )
/*************************/
{
    DebugMsg(("CmdlParamsInit(%u) enter\n", pass));

#if BUILD_TARGET
    if (pass == PASS_1) {
        asm_sym *sym;
        char * p;

        strupr( Options.build_target );
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
            if( (CodeInfo->info.cpu&P_CPU_MASK) >= P_386 ) {
                p = "__NETWARE_386__";
            } else {
                /* do nothing ... __NETWARE__ already defined */
            }
        } else if( stricmp( Options.build_target, "WINDOWS" ) == 0 ) {
            if( (CodeInfo->info.cpu&P_CPU_MASK) >= P_386 ) {
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
        set_fpu_parameters();
        add_predefined_tmacros();
        add_incpaths();
        if ( env = getenv( "INCLUDE" ) )
            AddStringToIncludePath( env );
    }
    DebugMsg(("CmdlParamsInit exit\n"));
    return;
}

void WritePreprocessedLine( char *string )
/***************************************/
/* print out preprocessed source lines
 */
{
    int             i;
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
    if ( GeneratedCode && (StructInit == 0))
        return;

    if (Token_Count > 0) {
        PrintEmptyLine = TRUE;
        printf("%s\n", string);
    } else if (PrintEmptyLine) {
        PrintEmptyLine = FALSE;
        printf("\n");
    }
}

// called for each pass

void ModulePassInit( void )
{
    ModuleInfo.cmdline  = FALSE;
    ModuleInfo.defUse32 = FALSE;
    ModuleInfo.dotname  = FALSE;
    ModuleInfo.ljmp     = TRUE;

    ModuleInfo.list   = Options.write_listing;
    ModuleInfo.cref   = TRUE;
    ModuleInfo.listif = Options.listif;
    ModuleInfo.list_generated_code = Options.list_generated_code;
    ModuleInfo.list_macro = LM_LISTMACRO;

    ModuleInfo.case_sensitive = Options.case_sensitive;
    ModuleInfo.convert_uppercase = Options.convert_uppercase;
    SymSetCmpFunc();

    ModuleInfo.segorder = SEGORDER_SEQ;
    ModuleInfo.langtype = Options.langtype;
    ModuleInfo.radix = 10;
    ModuleInfo.fieldalign = Options.alignment_default;
#if PROCALIGN
    ModuleInfo.procalign = 0;
#endif
}

void RunLineQueue( void )
{
    char *OldCurrSource = CurrSource;
    bool old_line_listed = line_listed;
    int currlevel = queue_level;

    DebugMsg(( "RunLineQueue() enter\n" ));
    CurrSource += strlen( CurrSource ) + 1;
    GeneratedCode++;
    while ( queue_level >= currlevel ) {
        int i;
        while (0 == (i = GetPreprocessedLine( CurrSource )));
        ParseItems();
        if ( StructInit ) {
            StoreLine( CurrSource );
            if ( Options.preprocessor_stdout == TRUE )
                WritePreprocessedLine( CurrSource );
        }
#ifdef DEBUG_OUT
        if ( EndDirectiveFound == TRUE ) {
            DebugMsg(("!!!!! Error: End directive found in generated-code parser loop!\n"));
            break;
        }
#endif
    }
    GeneratedCode--;
    CurrSource = OldCurrSource;
    line_listed = old_line_listed;

    DebugMsg(( "RunLineQueue() exit\n" ));
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
/******************************************/
{
    int i;

    ModulePassInit();
    SymPassInit( Parse_Pass );
    LabelsInit();
    SegmentInit( Parse_Pass );
    ContextInit( Parse_Pass );
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
#ifdef DEBUG_OUT
        DebugMsg(("OnePass(%u) segment sizes:\n", Parse_Pass + 1));
        {
            dir_node *dir;
            for( dir = Tables[TAB_SEG].head; dir; dir = dir->next ) {
                if( ( dir->sym.state != SYM_SEG ) || ( dir->sym.segment == NULL ) )
                    continue;
                DebugMsg(("OnePass(%u): segm=%s size=%8X:\n", Parse_Pass + 1, dir->sym.name, dir->e.seginfo->segrec->d.segdef.seg_length ));
            }
        }
#endif
        LineStoreCurr = LineStoreHead;
        while ( LineStoreCurr && EndDirectiveFound == FALSE ) {
            strcpy( srclinebuffer, LineStoreCurr->line );
            LineNumber = LineStoreCurr->lineno;
            MacroLevel = LineStoreCurr->macrolevel;
            DebugMsg(("OnePass(%u) #=%u mlvl=%u: >%s<\n", Parse_Pass+1, LineNumber, MacroLevel, srclinebuffer ));
            if ( Token_Count = Tokenize( srclinebuffer, 0 ) )
                ParseItems();
            LineStoreCurr = LineStoreCurr->next;
        }
        return( BytesTotal);
    }
#endif
    while ( EndDirectiveFound == FALSE ) {
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
    CheckProcOpen();
    HllCheckOpen();

    if( EndDirectiveFound == FALSE ) {
        AsmError( END_DIRECTIVE_REQUIRED );
    }
    ClearFileStack();

    return( BytesTotal );
}

static void scan_global( void )
/******************************/
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

static void set_ext_idx( )
/**********************************************/
{
    dir_node    *curr;
    uint        i;
    uint        index = 0;

    // first scan the EXTERN/EXTERNDEF items

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip COMM and EXTERNDEF items */
        if ((curr->sym.comm == 1) || (curr->sym.weak == 1))
            continue;
        index++;
        curr->sym.idx = index;
#if FASTPASS
        if ( curr->sym.altname &&
             curr->sym.altname->state != SYM_INTERNAL &&
             curr->sym.altname->state != SYM_PROC ) {
            /* do not use saved state, scan full source in second pass */
            SkipSavedState();
        }
#endif
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

static void SetMemoryModel( void )
/********************************/
{
    char *model;
    char buffer[20];

    DebugMsg(( "SetMemoryModel() enter\n" ));
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
    StructInit = 0;
    GeneratedCode = 0;
#if FASTPASS
    modstate.init = FALSE;
    LineStoreHead = NULL;
    LineStoreTail = NULL;
    UseSavedState = FALSE;
#endif


    if ( Options.model != MOD_NONE )
        SetMemoryModel(); /* if memory model has been set with -m{s|f|...} */

    SymInit();
    QueueInit();
    ModuleInit();
    CondInit();
    ExprEvalInit();
    TypesInit();
    FixInit();
}

#ifdef DEBUG_OUT
    void DumpInstrStats( void );
#endif

// called once per module. AssembleModule() cleanup

static void AssembleFini( void )
/****************************/
{
    SymFini();
#ifdef DEBUG_OUT
    DumpInstrStats();
#endif
    QueueFini();
    InputFini();
    FixFini();
}

// AssembleModule() assembles the source and writes the object module

void AssembleModule( void )
/**************************/
{
    unsigned long       prev_total = -1;
    unsigned long       curr_total;
    unsigned long       end_of_header;
    int                 starttime;
    int                 endtime;
#ifdef DEBUG_OUT
    dir_node            *dir;
#endif

    DebugMsg(("AssembleModule enter\n"));

    AssembleInit();

    LstOpenFile();

#ifndef __UNIX__
    starttime = GetTickCount();
#else
    starttime = clock();
#endif

    for( Parse_Pass = PASS_1; ; Parse_Pass++ ) {

        DebugMsg(( "*************\npass %u\n*************\n", Parse_Pass + 1 ));
        curr_total = OnePass();

        if ( Parse_Pass == PASS_1 && ModuleInfo.error_count == 0 ) {
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

        DebugMsg(("AssembleModule(%u): errorcnt=%u\n", Parse_Pass + 1, ModuleInfo.error_count ));
        if( ModuleInfo.error_count > 0 )
            break;

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

        if ( Parse_Pass % 10000 == 9999 )
            AsmWarn( 2, ASSEMBLY_PASSES, Parse_Pass+1);
#ifdef DEBUG_OUT
        if ( Options.max_passes && Parse_Pass == (Options.max_passes - 1) )
            break;
#endif

        rewind( FileInfo.file[ASM] );
#if FASTPASS
        if ( UseSavedState == FALSE && FileInfo.file[LST] )
            rewind( FileInfo.file[LST] );
#else
        if ( FileInfo.file[LST] )
            rewind( FileInfo.file[LST] );
#endif
    }

    DebugMsg(("AssembleModule: finished, cleanup\n"));

    /* Write a symbol listing file (if requested) */
    LstWriteCRef();

#ifndef __UNIX__
    endtime = GetTickCount();
#else
    endtime = clock(); // is in ms already
#endif

    sprintf( srclinebuffer, MsgGet( MSG_ASSEMBLY_RESULTS, NULL ),
             ModuleInfo.srcfile->name,
             LineNumber,
             Parse_Pass + 1,
             endtime - starttime,
             ModuleInfo.warning_count,
             ModuleInfo.error_count);
    if ( Options.quiet == FALSE )
        printf( srclinebuffer );

    LstPrintf ( srclinebuffer );

    LstCloseFile();

    AssembleFini();
}
