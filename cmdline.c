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
* Description:  command line argument parser
*
****************************************************************************/

#include <stdarg.h>
#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "fatal.h"
#include "msgtext.h"
#include "dbgcv.h"
#include "cmdline.h"

//#ifdef __OSI__
//  #include "ostype.h"
//#endif

#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)

#define HANDLECTRLZ 0
#define HANDLESWITCHCHAR 0

#else

#define HANDLECTRLZ 1
#define HANDLESWITCHCHAR 1

#endif

#ifdef __I86__
#define OPTQUAL __near
#else
#define OPTQUAL
#endif

struct  cmdloption {
    const char  *name;
    unsigned    value;
    void OPTQUAL (*function)( void );
};

extern char     banner_printed;

struct global_options Options = {
    /* quiet            */          FALSE,
    /* line_numbers     */          FALSE,
    /* debug_symbols    */          0,
    /* floating_point   */          FPO_NO_EMULATION,

    /* error_limit      */          50,
    /* no error display */          FALSE,
    /* warning_level    */          2,
    /* warning_error    */          FALSE,
#ifdef DEBUG_OUT
    /* debug            */          FALSE,
    /* nofastpass       */          FALSE,
    /* nobackpatch      */          FALSE,
    /* print_linestore  */          FALSE,
    /* max_passes       */          0,
#endif
    /* names            */          {
        NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
#if BUILD_TARGET
        NULL,
#endif
#if MANGLERSUPP
        NULL
#endif
    },
    /* queues           */          { NULL, NULL, NULL },
#if COCTALS
    /* allow_c_octals        */     FALSE,
#endif
    /* no_comment_data_in_code_records */   FALSE,
    /* no_opt_farcall        */     FALSE,
    /* no_dependencies       */ //    FALSE,
    /* no_file_entry         */     FALSE,
    /* no_section_aux_entry  */     FALSE,
    /* no_cdecl_decoration   */     FALSE,
    /* stdcall_decoration    */     STDCALL_FULL,
    /* no_export_decoration  */     FALSE,
    /* entry_decorated       */     FALSE,
    /* write_listing         */     FALSE,
    /* case_sensitive        */     FALSE,
    /* convert_uppercase     */     FALSE,
    /* preprocessor_stdout   */     FALSE,
    /* masm51_compat         */     FALSE,
    /* strict_masm_compat    */     FALSE,
    /* masm_compat_gencode   */     FALSE,
    /* masm8_proc_visibility */     FALSE,

    /* listif                */     FALSE,
    /* list_generated_code   */     FALSE,
    /* list_macro            */     LM_LISTMACRO,
    /* no_symbol_listing     */     FALSE,

    /* all_symbols_public    */     FALSE,
    /* safeseh               */     FALSE,
    /* ignore_include        */     FALSE,
    /* output_format         */     OFORMAT_OMF,
    /* header_format         */     HFORMAT_NONE,
    /* alignment_default     */     0,
    /* langtype              */     LANG_NONE,
    /* model                 */     MOD_NONE,
    /* cpu                   */     P_86,
    /* fastcall type         */     FCT_MS32,
    /* syntax check only     */     FALSE,
#if MANGLERSUPP
    /* naming_convention*/          NC_DO_NOTHING,
#endif
};

char                    *FileDir[NUM_FILE_TYPES] = { NULL, NULL, NULL, NULL };

#define MAX_RSP_NESTING 15  /* nesting of response files */

static unsigned         OptValue;  /* value of option's numeric argument  */
static char             *OptName;  /* value of option's name argument     */
static const char       *cmdsave[MAX_RSP_NESTING]; /* response files */
static const char       *cmdbuffers[MAX_RSP_NESTING]; /* response files */
static int              rspidx = 0; /* response file level */
#if HANDLESWITCHCHAR
static unsigned char    SwitchChar = '/';
#endif

/* array for options -0 ... -10 */
static const unsigned cpuoption[] = {
     P_86, P_186, P_286, P_386,  /* 0-3 */
    P_486, P_586, P_686, P_686 | P_MMX, /* 4-7 */
    P_686 | P_MMX | P_SSE1,  /* 8 */
    P_686 | P_MMX | P_SSE1 | P_SSE2,  /* 9 */
#if AMD64_SUPPORT
    P_64, /* 10 */
#endif
};

static void StripQuotes( char *fname )
/************************************/
{
    char *s;
    char *d;

    if( *fname == '"' ) {
        /* string will shrink so we can reduce in place */
        d = fname;
        for( s = d + 1; *s && *s != '"'; ++s ) {
            /* collapse double backslashes, only then look for escaped quotes */
            if( s[0] == '\\' && s[1] == '\\' ) {
                ++s;
            } else if( s[0] == '\\' && s[1] == '"' ) {
                ++s;
            }
            *d++ = *s;
        }
        *d = '\0';
    }
}

static char *GetAFileName( void )
/*******************************/
{
    StripQuotes( OptName );
    return( OptName );
}

#if BUILD_TARGET
static void SetTargName( char *name, unsigned len )
/*************************************************/
{
    if( Options.names[OPTN_BUILD_TARGET] != NULL ) {
        MemFree( Options.names[OPTN_BUILD_TARGET] );
        Options.names[OPTN_BUILD_TARGET] = NULL;
    }
    if( name == NULL || len == 0 )
        return;
    Options.names[OPTN_BUILD_TARGET] = MemAlloc( len + 1 );
    strcpy( Options.names[OPTN_BUILD_TARGET], name );
    _strupr( Options.names[OPTN_BUILD_TARGET] );
}
#endif

/* called by -0, -1, ... argument */

static void SetCpuCmdline( unsigned value, const char *parm )
/***********************************************************/
{

    Options.cpu &= ~(P_CPU_MASK | P_EXT_MASK | P_PM);
    Options.cpu |= value;
#if 0 //AMD64_SUPPORT
    /* implicitely set model flat if cpu is set to x86-64 via
     * commandline. This is deactive, because it's intransparent.
     */
    if ( Options.cpu == P_64 )
        if ( Options.model == MOD_NONE )
            Options.model = MOD_FLAT;
#endif
    for( ; *parm ; parm++ ) {
        if( *parm == 'p' && Options.cpu >= P_286 ) {
            Options.cpu |= P_PM;      /* set privileged mode */
#if MANGLERSUPP
        } else if( *parm == '"' ) {       /* set default mangler */
            char *dest;
            parm++;
            dest = strchr( parm, '"' );
            if( Options.names[OPTN_DEFNAME_MANGLER] != NULL ) {
                MemFree( Options.names[OPTN_DEFNAME_MANGLER );
            }
            Options.names[OPTN_DEFNAME_MANGLER = MemAlloc( dest - parm + 1 );
            dest = Options.names[OPTN_DEFNAME_MANGLER];
            for( ; *parm != '"'; dest++, parm++ ) {
                *dest = *parm;
            }
            *dest = NULLC;
#endif
        } else {
            AsmWarn( 1, CPU_OPTION_INVALID, parm );
            break;
        }
    }
}

/* queue a text macro, include path or "forced" include files.
   this is called for cmdline options -D, -I and -Fi
 */

static void queue_item( int i, const char *string )
/*************************************************/
{
    struct qitem *p;
    struct qitem *q;

    DebugMsg(("queue_item(%u, %s) enter\n", i, string));
    p = MemAlloc( sizeof(struct qitem) + strlen( string ) );
    p->next = NULL;
    strcpy( p->value, string );
    q = Options.queues[i];
    if ( q ) {
        for ( ; q->next; q = q->next );
        q->next = p;
    } else
        Options.queues[i] = p;
    return;
}

static void get_fname( int type, const char *token )
/**************************************************/
/*
 * called by -Fo, -Fr or -Fl (for .OBJ, .ERR or .LST filenames ).
 */
{
    char        *pExt;
    char        name [ _MAX_PATH ];
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    //char        msgbuf[MAXMSGSIZE];

    _splitpath( token, drive, dir, fname, ext );
    /*
     * If name's ending with a '\' (or '/' in Unix), it's supposed
     * to be a directory name only.
     */
    if( fname[0] == NULLC ) {
        DebugMsg(("get_fname(%s, %u) name is a directory\n", token, type ));
        if ( FileDir[type] )
            MemFree( FileDir[type]);
        FileDir[type] = MemAlloc( strlen( token ) + 1 );
        strcpy( FileDir[type], token );
        return;
    }
    if ( drive[0] == NULLC && dir[0] == NULLC && FileDir[type] ) {
        DebugMsg(("get_fname: default drive+dir used: %s\n" ));
        _splitpath( FileDir[type], drive, dir, NULL, NULL );
    }
    pExt = ext;
    if( ext[0] == NULLC ) {
        switch( type ) {
        case ERR:   pExt = ERR_EXT;  break;
        case LST:   pExt = LST_EXT;  break;
        case OBJ:   pExt = OBJ_EXT;  break;
        }
    }
    _makepath( name, drive, dir, fname, pExt );
    if( Options.names[type] != NULL ) {
        MemFree( Options.names[type] );
    }
    Options.names[type] = MemAlloc( strlen( name ) + 1 );
    strcpy( Options.names[type], name );
}

static void set_option_n_name( int idx, const char *name )
/*************************************šš*****************/
/* option -n: set name of
 * - nd: data seg
 * - nm: module name
 * - nt: text seg
 * - nc: code class
 */
{
    if ( *name != '.' && !is_valid_id_char( *name ) ) {
        AsmError( N_OPTION_NEEDS_A_NAME_PARAMETER );
        return;
    }

    if( Options.names[idx] != NULL ) {
        MemFree( Options.names[idx] );
    }
    Options.names[idx] = MemAlloc( strlen( name ) + 1 );
    strcpy( Options.names[idx], name );
}

static void usagex_msg( void )
/****************************/
{
    MsgPrintUsage();
#ifndef __SW_BD
    exit(1);
#endif
}

//static void OPTQUAL Ignore( void ) {};

#if BUILD_TARGET
static void OPTQUAL Set_bt( void ) { SetTargName( OptName,  strlen(OptName) ); }
#endif

static void OPTQUAL Set_c( void ) { }

#if COFF_SUPPORT
static void OPTQUAL Set_coff( void ) {
    Options.output_format = OFORMAT_COFF;
}
#if DJGPP_SUPPORT
static void OPTQUAL Set_djgpp( void ) {
    Set_coff();
    Options.header_format = HFORMAT_DJGPP;
}
#endif
#endif
#if ELF_SUPPORT
static void OPTQUAL Set_elf( void ) {
    Options.output_format = OFORMAT_ELF;
}
#if AMD64_SUPPORT
static void OPTQUAL Set_elf64( void ) {
    Options.output_format = OFORMAT_ELF;
    Options.header_format = HFORMAT_ELF64;
    if ( Options.model == MOD_NONE )  /* default to model FLAT */
        Options.model = MOD_FLAT;
    //if ( Options.langtype == LANG_NONE ) /* default to language SYSCALL */
    //    Options.langtype = LANG_FASTCALL;
    //Options.fastcall = FCT_WIN64;     /* FASTCALL is Win64 register ABI */
    /* set CPU if output format is for 64bit */
    if ( (Options.cpu & P_CPU_MASK) < P_64 ) {
        SetCpuCmdline( P_64, "" );
    }
}
#endif
#endif
#if BIN_SUPPORT
static void OPTQUAL Set_bin( void ) {
    Options.output_format = OFORMAT_BIN;
    Options.header_format = HFORMAT_NONE;
}
#if MZ_SUPPORT
static void OPTQUAL Set_mz( void ) {
    Options.output_format = OFORMAT_BIN;
    Options.header_format = HFORMAT_MZ;
}
#endif
#endif
static void OPTQUAL Set_Cp( void ) { Options.case_sensitive = TRUE;   Options.convert_uppercase = FALSE; }
static void OPTQUAL Set_Cu( void ) { Options.case_sensitive = FALSE;  Options.convert_uppercase = TRUE;  }
static void OPTQUAL Set_Cx( void ) { Options.case_sensitive = FALSE;  Options.convert_uppercase = FALSE; }

static void OPTQUAL Set_Zd( void ) { Options.line_numbers = TRUE; }
static void OPTQUAL Set_Zi( void ) { Set_Zd(); Options.debug_symbols = CV4_SIGNATURE; }

static void OPTQUAL Set_Zm( void ) { Options.masm51_compat = TRUE; }

static void OPTQUAL Set_Zne( void ) { Options.strict_masm_compat = TRUE; }

static void OPTQUAL Set_Zg( void ) { Options.masm_compat_gencode = TRUE; }

static void OPTQUAL Set_Zf( void ) { Options.all_symbols_public = TRUE; }

static void OPTQUAL Set_Zp( void ) {
    uint_8 power;
    for ( power = 0; (1 << power) <= MAX_STRUCT_ALIGN; power++ )
        if ( ( 1 << power ) == OptValue ) {
            Options.fieldalign = power;
            return;
        }
    AsmWarn( 1, INVALID_CMDLINE_VALUE, "-Zp" );
    return;
}

static void OPTQUAL Set_D( void )  { queue_item( OPTQ_MACRO,    GetAFileName() ); }
static void OPTQUAL Set_Fi( void ) { queue_item( OPTQ_FINCLUDE, GetAFileName() ); }
static void OPTQUAL Set_I( void )  { queue_item( OPTQ_INCPATH,  GetAFileName() ); }

static void OPTQUAL Set_e( void ) { Options.error_limit = OptValue; }
static void OPTQUAL Set_eq( void ) { Options.no_error_disp = TRUE; }

static void OPTQUAL Set_nologo( void ) { banner_printed = TRUE; }
static void OPTQUAL Set_q( void )      { Set_nologo(); Options.quiet = TRUE; }
static void OPTQUAL Set_EP( void ) { Options.preprocessor_stdout = TRUE; Set_q(); }


static void OPTQUAL Set_Fr( void ) { get_fname( ERR, GetAFileName() ); }
static void OPTQUAL Set_Fl( void ) { get_fname( LST, GetAFileName() ); Options.write_listing = TRUE;}
static void OPTQUAL Set_Fo( void ) { get_fname( OBJ, GetAFileName() ); }

static void OPTQUAL Set_fp( void ) { Options.cpu &= ~P_FPU_MASK; Options.cpu = OptValue; }
static void OPTQUAL Set_FPx( void ) { Options.floating_point = OptValue; }
static void OPTQUAL Set_G( void ) { Options.langtype = OptValue; }


#ifdef DEBUG_OUT
static void OPTQUAL Set_ls( void ) { Options.print_linestore = TRUE; };
#endif

static void OPTQUAL Set_Sa( void )
{
    Options.listif = TRUE;
    Options.list_generated_code = TRUE;
    Options.list_macro = LM_LISTMACROALL;
}
static void OPTQUAL Set_Sg( void ) { Options.list_generated_code = TRUE; }
static void OPTQUAL Set_Sn( void ) { Options.no_symbol_listing = TRUE; }
static void OPTQUAL Set_Sx( void ) { Options.listif = TRUE; }
#if COFF_SUPPORT
static void OPTQUAL Set_safeseh( void ) { Options.safeseh = TRUE; }
#endif
static void OPTQUAL Set_m( void ) { Options.model = OptValue; }
static void OPTQUAL Set_n( void ) { set_option_n_name( OptValue, OptName ); }

#if COCTALS
static void OPTQUAL Set_o( void ) { Options.allow_c_octals = TRUE; }
#endif
#ifdef DEBUG_OUT
static void OPTQUAL Set_pm( void ) { Options.max_passes = OptValue; };
#endif
static void OPTQUAL Set_omf( void ) { Options.output_format = OFORMAT_OMF;}

static void OPTQUAL Set_WX( void ) { Options.warning_error = TRUE; }

static void OPTQUAL Set_w( void ) { Set_WX(); Options.warning_level = 0; }

static void OPTQUAL Set_W( void )
{
    if ( OptValue <= 4 )
        Options.warning_level = OptValue;
    else
        AsmWarn( 1, INVALID_CMDLINE_VALUE, "/W" );
}

#if AMD64_SUPPORT
static void OPTQUAL Set_win64( void )
{
    Options.output_format = OFORMAT_COFF;
    Options.header_format = HFORMAT_WIN64;
    if ( Options.model == MOD_NONE )  /* default to model FLAT */
        Options.model = MOD_FLAT;
    if ( Options.langtype == LANG_NONE ) /* default to language SYSCALL */
        Options.langtype = LANG_FASTCALL;
    Options.fastcall = FCT_WIN64;     /* FASTCALL is Win64 register ABI */
    /* set CPU if output format is for 64bit */
    if ( (Options.cpu & P_CPU_MASK) < P_64 ) {
        SetCpuCmdline( P_64, "" );
    }
}
#endif
static void OPTQUAL Set_X( void )   { Options.ignore_include = TRUE; }
static void OPTQUAL Set_zcm( void ) { Options.no_cdecl_decoration = FALSE; }
static void OPTQUAL Set_zcw( void ) { Options.no_cdecl_decoration = TRUE; }
#if OWFC_SUPPORT
static void OPTQUAL Set_zf( void )  { Options.fastcall = OptValue; }
#endif

static void OPTQUAL Set_zlc( void ) { Options.no_comment_data_in_code_records = TRUE; }
static void OPTQUAL Set_zld( void ) { Options.no_opt_farcall = TRUE; }
#if COFF_SUPPORT
static void OPTQUAL Set_zlf( void ) { Options.no_file_entry = TRUE; }
static void OPTQUAL Set_zls( void ) { Options.no_section_aux_entry = TRUE; }
#endif

static void OPTQUAL Set_Zs( void ) { Options.syntax_check_only = TRUE; }
static void OPTQUAL Set_Zv8( void ) { Options.masm8_proc_visibility = TRUE; }
static void OPTQUAL Set_zt( void ) { Options.stdcall_decoration = OptValue; }
static void OPTQUAL Set_zze( void ) { Options.no_export_decoration = TRUE; }
#if COFF_SUPPORT
static void OPTQUAL Set_zzs( void ) { Options.entry_decorated = TRUE; }
#endif

static void OPTQUAL Set_h( void ) { usagex_msg();}

#ifdef DEBUG_OUT
static void OPTQUAL Set_d6( void )
{
    Options.debug = TRUE;
    DebugMsg(( "debugging output on\n" ));
}
static void OPTQUAL Set_d7( void )
{
    Options.nofastpass = TRUE;
    DebugMsg(( "FASTPASS disabled\n" ));
}
static void OPTQUAL Set_d8( void )
{
    Options.nobackpatch = TRUE;
    DebugMsg(( "backpatching disabled\n" ));
}
#endif

static struct cmdloption const cmdl_options[] = {
    { "?",      0,        Set_h },
#if BIN_SUPPORT
    { "bin",    0,        Set_bin },
#endif
#if BUILD_TARGET
    { "bt=$",   0,        Set_bt },
#endif
    { "Cp",     0,        Set_Cp },
    { "Cu",     0,        Set_Cu },
    { "Cx",     0,        Set_Cx },
#if COFF_SUPPORT
    { "coff",   0,        Set_coff },
#endif
    { "c",      0,        Set_c },
#ifdef DEBUG_OUT
    { "d6",     6,        Set_d6 },
    { "d7",     7,        Set_d7 },
    { "d8",     8,        Set_d8 },
#endif
#if COFF_SUPPORT && DJGPP_SUPPORT
    { "djgpp",  0,        Set_djgpp },
#endif
    { "D^$",    0,        Set_D },
#if ELF_SUPPORT
#if AMD64_SUPPORT
    { "elf64",  0,        Set_elf64 },
#endif
    { "elf",    0,        Set_elf },
#endif
    { "EP",     0,        Set_EP },
    { "eq",     0,        Set_eq },
    { "e=#",    0,        Set_e },
    { "Fi=^@",  0,        Set_Fi },
    { "Fl=@",   0,        Set_Fl },
    { "Fo=^@",  0,        Set_Fo },
    { "FPi87",  FPO_NO_EMULATION, Set_FPx },
    { "FPi",    FPO_EMULATION,    Set_FPx },
    { "fp0",    P_87,     Set_fp },
    { "fp2",    P_287,    Set_fp },
    { "fp3",    P_387,    Set_fp },
#if 0
    { "fp5",    P_387,    Set_fp },
    { "fp6",    P_387,    Set_fp },
#endif
    { "fpc",    P_NO87,   Set_fp },
    { "Fr=^@",  0,        Set_Fr },
    { "Gc",     LANG_PASCAL,  Set_G },
    { "Gd",     LANG_C,       Set_G },
    { "Gz",     LANG_STDCALL, Set_G },
    { "h",      0,        Set_h },
    { "I=^@",   0,        Set_I },
#ifdef DEBUG_OUT
    { "ls",     0,        Set_ls },
#endif
    { "mc",     MOD_COMPACT, Set_m },
    { "mf",     MOD_FLAT,    Set_m },
    { "mh",     MOD_HUGE,    Set_m },
    { "ml",     MOD_LARGE,   Set_m },
    { "mm",     MOD_MEDIUM,  Set_m },
    { "ms",     MOD_SMALL,   Set_m },
    { "mt",     MOD_TINY,    Set_m },
#if BIN_SUPPORT
#if MZ_SUPPORT
    { "mz",     0,        Set_mz },
#endif
#endif
    { "nc=$",   OPTN_CODE_CLASS,    Set_n },
    { "nd=$",   OPTN_DATA_SEG,      Set_n },
    { "nm=$",   OPTN_MODULE_NAME,   Set_n },
    { "nt=$",   OPTN_TEXT_SEG,      Set_n },
    { "nologo", 0,        Set_nologo },
    { "omf",    0,        Set_omf },
#if COCTALS
    { "o",      0,        Set_o },
#endif
#ifdef DEBUG_OUT
    { "pm=#",   0,        Set_pm },
#endif
    { "q",      0,        Set_q },
    { "Sa",     0,        Set_Sa },
    { "Sg",     0,        Set_Sg },
    { "Sn",     0,        Set_Sn },
    { "Sx",     0,        Set_Sx },
#if COFF_SUPPORT
    { "safeseh",0,        Set_safeseh },
#endif
    { "WX",     0,        Set_WX },
    { "W=#",    0,        Set_W },
#if AMD64_SUPPORT
    { "win64",  0,        Set_win64 },
#endif
    { "w",      0,        Set_w },
    { "X",      0,        Set_X },
    { "Zd",     0,        Set_Zd },
    { "Zf",     0,        Set_Zf },
    { "Zg",     0,        Set_Zg },
    { "Zi",     0,        Set_Zi },
    { "Zm",     0,        Set_Zm },
    { "Zne",    0,        Set_Zne },
    { "Zp=#",   0,        Set_Zp },
    { "zcm",    0,        Set_zcm },
    { "zcw",    0,        Set_zcw },
#if OWFC_SUPPORT
    { "zf0",    FCT_MS32,    Set_zf },
    { "zf1",    FCT_WATCOMC, Set_zf },
#endif
    { "zlc",    0,        Set_zlc },
    { "zld",    0,        Set_zld },
#if COFF_SUPPORT
    { "zlf",    0,        Set_zlf },
    { "zls",    0,        Set_zls },
#endif
    { "Zs",     0,        Set_Zs },
    { "zt0",    STDCALL_NONE, Set_zt },
    { "zt1",    STDCALL_HALF, Set_zt },
    { "zt2",    STDCALL_FULL, Set_zt },
    { "Zv8",    0,        Set_Zv8 },
    { "zze",    0,        Set_zze },
#if COFF_SUPPORT
    { "zzs",    0,        Set_zzs },
#endif
    { NULL,     0,        0 }
};

static int OptionDelimiter( char c )
/**********************************/
{
#if HANDLESWITCHCHAR
    if( c == ' ' || c == '-' || c == NULLC || c == '\t' || c == SwitchChar ) {
#else
    if( c == ' ' || c == '-' || c == NULLC || c == '\t' ) {
#endif
        return( 1 );
    }
    return( 0 );
}

/*
 * get a "filename" or "macro" name
 */
static const char *GetNameToken( char *dst, const char *str, int max, bool isequate )
/***********************************************************************************/
{
    bool equatefound = FALSE;

    //while( isspace( *str ) ) ++str;  /* no spaces allowed! */
is_quote:
    if( *str == '"' ) {
        ++str;
        for( ;max; max-- ) {
            if ( *str == '"' ) {
                ++str;
                break;
            }
            if ( *str == NULLC )
                break;
            if ( *str == '\\' && *(str+1) == '"' ) {
                ++str;
            }
            *dst++ = *str++;
        }
    } else {
        for( ; max; max-- ) {
            if ( *str == NULLC || *str == ' ' || *str == '\t' )
                break;
#if HANDLESWITCHCHAR
            if ( (*str == SwitchChar || *str == '-') && isequate == FALSE )
                break;
#else
            if ( *str == '-' && isequate == FALSE )
                break;
#endif
            if ( *str == '=' && isequate == TRUE && equatefound == FALSE ) {
                equatefound = TRUE;
                *dst++ = *str++;
                if (*str == '"')
                    goto is_quote;
            }
            *dst++ = *str++;
        }
    }
    *dst = NULLC;
    return( str );
}

/*
 * A '@' was found in the cmdline. It's not an environment variable,
 * so check if it is a file and, if yes, read it.
 */

static char *ReadParamFile( const char *name )
/********************************************/
{
    char        *env;
    char        *str;
    FILE        *file;
    int         len;
    char        ch;

    env = NULL;
    file = fopen( name, "rb" );
    if( file == NULL ) {
#ifdef __SW_BD
        /* no fatal error if jwasm is loaded as a library */
        AsmErr( CANNOT_OPEN_FILE, name, errno );
#else
        Fatal( FATAL_CANNOT_OPEN_FILE, name, errno );
#endif
        return( NULL );
    }
    len = 0;
    if ( fseek( file, 0, SEEK_END ) == 0 ) {
        len = ftell( file );
        rewind( file );
        env = MemAlloc( len + 1 );
        fread( env, 1, len, file );
        env[len] = NULLC;
    }
    fclose( file );
    if ( len == 0)
        return( NULL );
    /* zip through characters changing \r, \n etc into ' ' */
    str = env;
    while( *str ) {
        ch = *str;
        if( ch == '\r' || ch == '\n' ) {
            *str = ' ';
        }
#if HANDLECTRLZ
        if( ch == 0x1A ) {      /* if end of file */
            *str = '\0';        /* - mark end of str */
            break;
        }
#endif
        ++str;
    }
    return( env );
}

/* current cmdline string is done, get the next one! */

static const char *getnextcmdstring( const char **cmdline )
/*********************************************************/
{
    const char **src;
    const char **dst;

    /* something onto the response file stack? */
    if ( rspidx ) {
        rspidx--;
        if ( cmdbuffers[rspidx] )
            MemFree( (void *)cmdbuffers[rspidx] );
        return( cmdsave[rspidx] );
    }
    for ( dst = cmdline, src = cmdline+1; *src; )
        *dst++ = *src++;
    *dst = *src;
    return( *cmdline );
}

static const char * GetNumber( const char * p )
/*********************************************/
{
    OptValue = 0;
    for( ;*p >= '0' && *p <= '9'; p++ )
        OptValue = OptValue * 10 + *p - '0';
    return( p );
}

/* scan option table and if option is known, process it */

static void ProcessOption( const char **cmdline, char *buffer )
/*************************************************************/
{
    int   i;
    int   j;
    const char *p = *cmdline;
    const char *opt;
    //char  c;

    DebugMsg(("ProcessOption(%s)\n", p ));

    /* numeric option (-0, -1, ... ) handled separately since
     * the value can be >= 10.
     */
    if ( *p >= '0' && *p <= '9' ) {
        p = GetNumber( p );
        if ( OptValue < sizeof(cpuoption)/sizeof(cpuoption[0]) ) {
            p = GetNameToken( buffer, p, 16, FALSE ); /* get optional 'p' */
            *cmdline = p;
            SetCpuCmdline( cpuoption[OptValue], buffer );
            return;
        }
    }
    for( i = 0; cmdl_options[i].name; i++ ) {
        opt = cmdl_options[i].name;
        //DebugMsg(("ProcessOption(%s): %s\n", p, opt ));
        if( *p == *opt ) {
            for ( opt++, j = 1 ; isalnum(*opt) && *opt == p[j]; opt++, j++ );
            /* make sure end of option is reached */
            if ( isalnum(*opt) )
                continue;
            p += j;
            OptValue = cmdl_options[i].value;
            //DebugMsg(("ProcessOption(%s): Option found\n", p ));
            for( ;; opt++) {
                switch ( *opt ) {
                //case '*': /* don't know what this is supposed to do? */
                case NULLC:
                    if ( !OptionDelimiter( *p ) )
                        goto opt_error_exit;
                    *cmdline = p;
                    cmdl_options[i].function();
                    return; /* option processed successfully */
                    break;
                case '#':             /* collect a number */
                    if( *p >= '0' && *p <= '9' )
                        p = GetNumber( p );
                    break;
                case '$':      /* collect an identifer+value */
                case '@':      /* collect a filename */
                    OptName = buffer;
#if 0 /* v2.05: removed */
                    if ( rspidx )
                        p = GetNameToken( buffer, p, _MAX_PATH - 1, *opt == '$' );
                    else {
                        j = strlen( p );
                        memcpy( buffer, p, (j >= _MAX_PATH) ? _MAX_PATH : j+1 );
                        p += j;
                    }
#else
                    p = GetNameToken( buffer, p, _MAX_PATH - 1, *opt == '$' );
#endif
                    break;
                case '=':    /* collect an optional '=' */
                    if ( *p == '=' || *p == '#' )
                        p++;
                    break;
                case '^':    /* skip spaces before argument */
                    while ( isspace(*p) ) p++;
                    if ( *p == NULLC ) {
                        p = getnextcmdstring( cmdline );
                        if ( p == NULL ) {
                            AsmWarn( 1, MISSING_ARGUMENT_FOR_CMDLINE_OPTION );
                            return;
                        }
                    }
                    break;
                default:
                    /* internal error: unknown format of option item! */
                    DebugMsg(( "ProcessOption: unknown option specifier: %s\n", opt ));
                    break;
                }
            }
        }
    }
opt_error_exit:
    AsmWarn( 1, INVALID_CMDLINE_OPTION, *cmdline - 1 );
    *cmdline = "";
    return;
}

#if BUILD_TARGET

#define MAX_OS_NAME_SIZE 7

static void set_default_build_target( void )
/******************************************/
{

    if( Options.names[OPTN_BUILD_TARGET] == NULL ) {
        Options.names[OPTN_BUILD_TARGET] = MemAlloc( MAX_OS_NAME_SIZE + 1 );
#if defined(__OSI__)
        if( __OS == OS_DOS ) {
            strcpy( Options.names[OPTN_BUILD_TARGET], "DOS" );
        } else if( __OS == OS_OS2 ) {
            strcpy( Options.names[OPTN_BUILD_TARGET], "OS2" );
        } else if( __OS == OS_NT ) {
            strcpy( Options.names[OPTN_BUILD_TARGET], "NT" );
        } else if( __OS == OS_WIN ) {
            strcpy( Options.names[OPTN_BUILD_TARGET], "WINDOWS" );
        } else {
            strcpy( Options.names[OPTN_BUILD_TARGET], "XXX" );
        }
#elif defined(__QNX__)
        strcpy( Options.names[OPTN_BUILD_TARGET], "QNX" );
#elif defined(__LINUX__)
        strcpy( Options.names[OPTN_BUILD_TARGET], "LINUX" );
#elif defined(__BSD__)
        strcpy( Options.names[OPTN_BUILD_TARGET], "BSD" );
#elif defined(__OSX__) || defined(__APPLE__)
        strcpy( Options.names[OPTN_BUILD_TARGET], "OSX" );
#elif defined(__DOS__)
        strcpy( Options.names[OPTN_BUILD_TARGET], "DOS" );
#elif defined(__OS2__)
        strcpy( Options.names[OPTN_BUILD_TARGET], "OS2" );
#elif defined(__NT__)
        strcpy( Options.names[OPTN_BUILD_TARGET], "NT" );
#else
        #error unknown host OS
#endif
    }
    return;
}
#endif

/* parse cmdline:
 * - process cmdline options
 * - get filename argument
 * - handle (nested) response files
 */

char * EXPQUAL ParseCmdline( const char **cmdline, int *pCntArgs )
/****************************************************************/
{
    int i;
    const char *str = *cmdline;
    char paramfile[_MAX_PATH];

    for ( i = 0; i < NUM_FILE_TYPES; i++ )
        if ( Options.names[i] != NULL ) {
            MemFree( Options.names[i] );
            Options.names[i] = NULL;
        }

    for( ; str; ) {
        while( *str == ' ' || *str == '\t' )
            ++str;
        if( *str == '@' ) {
            if ( rspidx >= MAX_RSP_NESTING )
                Fatal( FATAL_NESTING_LEVEL_TOO_DEEP );
            str++;
#if 0 /* v2.05: removed */
            if ( rspidx ) {
                cmdsave[rspidx] = GetNameToken( paramfile, str, sizeof( paramfile ) - 1, FALSE );
            } else {
                strcpy( paramfile, str );
                cmdsave[rspidx] = str + strlen(str);
            }
#else
            cmdsave[rspidx] = GetNameToken( paramfile, str, sizeof( paramfile ) - 1, FALSE );
#endif
            cmdbuffers[rspidx] = NULL;
            str = NULL;
            if ( paramfile[0] )
                str = getenv( paramfile );
            if( str == NULL ) {
                str = ReadParamFile( paramfile );
                cmdbuffers[rspidx] = str;
                if ( str == NULL ) {
                    str = cmdsave[rspidx];
                    continue;
                }
            }
            rspidx++;
            continue;
        }
        if( *str == NULLC ) {
            str = getnextcmdstring( cmdline );
            continue;
        }
#if HANDLESWITCHCHAR
        if( *str == '-'  ||  *str == SwitchChar ) {
#else
        if( *str == '-' ) {
#endif
            str++;
            *cmdline = str;
            ProcessOption( cmdline, paramfile );
            (*pCntArgs)++;
            str = *cmdline;
        } else {  /* collect  file name */
            //int len;
#if BUILD_TARGET
            set_default_build_target();
#endif
#if 0 /* v2.05: removed */
            if ( rspidx ) {
                str = GetNameToken( paramfile, str, sizeof( paramfile ) - 1, FALSE );
                get_fname( ASM, paramfile );
            } else {
                len = strlen( str );
                get_fname( ASM, str );
                str += len;
            }
#else
            str = GetNameToken( paramfile, str, sizeof( paramfile ) - 1, FALSE );
            Options.names[ASM] = MemAlloc( strlen( paramfile ) + 1 );
            strcpy( Options.names[ASM], paramfile );
#endif
            (*pCntArgs)++;
            *cmdline = str;
            return( Options.names[ASM] );
        }
    }
    *cmdline = str;
    return( NULL );
}

void EXPQUAL CmdlineFini( void )
/******************************/
/* Free resources allocated by cmdline options */
{
    int i;
    DebugMsg(("CmdLineFini enter\n" ));
    for ( i = 0; i < NUM_FILE_TYPES; i++ ) {
        if ( FileDir[i] != NULL ) {
            MemFree( FileDir[i] );
            FileDir[i] = NULL;
        }
    }
    for ( i = 0; i < OPTN_LAST; i++ )
        if ( Options.names[i] != NULL ) {
            MemFree( Options.names[i] );
            Options.names[i] = NULL;
        }
    for ( i = 0; i < OPTQ_LAST; i++ ) {
        struct qitem *p;
        struct qitem *q;
        for ( q = Options.queues[i]; q; ) {
            p = q->next;
            MemFree( q );
            q = p;
        }
        Options.queues[i] = NULL;
    }
    DebugMsg(("CmdLineFini exit\n" ));
    return;
}

