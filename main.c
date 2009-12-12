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
* Description:  JWasm top level module + command line parser
*
****************************************************************************/

#include <stdarg.h>
#include <ctype.h>
#include <signal.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "fatal.h"
#include "errmsg.h"
#include "msgtext.h"

//#ifdef __OSI__
//  #include "ostype.h"
//#endif

#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)

#define WILDCARDS 0
#define CATCHBREAK 0
#define HANDLECTRLZ 0
#define HANDLESWITCHCHAR 0

#else

#define WILDCARDS 1
#ifdef __POCC__
#define CATCHBREAK 0
#else
#define CATCHBREAK 1
#endif
#define HANDLECTRLZ 1
#define HANDLESWITCHCHAR 1

#endif

#if WILDCARDS
#ifdef __UNIX__
 #include <unistd.h>
#else
 #include <io.h>
#endif
#endif

extern char     banner_printed;

File_Info       FileInfo;       // files information
static int      numArgs = 0;
static int      numFiles = 0;

struct  cmdloption {
    char        *name;
    unsigned    value;
    void        (*function)( void );
};

#define MAX_RSP_NESTING 15  /* nesting of response files */
#define BUF_SIZE 512

static unsigned         OptValue;  /* value of option's numeric argument  */
static char             *OptParm;
static char             *FileDir[NUM_FILE_TYPES];
static char             ParamBuf[ BUF_SIZE ];
static const char       *cmdsave[MAX_RSP_NESTING]; /* response files */
static const char       *cmdbuffers[MAX_RSP_NESTING]; /* response files */
static int              rspidx = 0; /* response file level */
#if HANDLESWITCHCHAR
static unsigned char    SwitchChar = '/';
#endif

global_options Options = {
    /* quiet            */          FALSE,
    /* line_numbers     */          FALSE,
    /* debug_symbols    */          FALSE,
    /* floating_point   */          FPO_NO_EMULATION,

    /* error_limit      */          50,
    /* warning_level    */          2,
    /* warning_error    */          FALSE,
#ifdef DEBUG_OUT
    /* debug            */          FALSE,
    /* print_linestore  */          FALSE,
    /* max_passes       */          0,
#endif
#if BUILD_TARGET
    /* build_target     */          NULL,
#endif
    /* code_class       */          NULL,
    /* data_seg         */          NULL,
    /* text_seg         */          NULL,
    /* module_name      */          NULL,
    /* forced include   */          NULL,
    /* symbol queue     */          NULL,
    /* include path queue */        NULL,
#if COCTALS
    /* allow_c_octals        */     FALSE,
#endif
    /* no_comment_data_in_code_records */   FALSE,
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
    /* default_name_mangler  */     NULL,
    /* naming_convention*/          NC_DO_NOTHING,
#endif
};

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

static char *CopyOfParm( void )
/*****************************/
{
    //unsigned    len;

    strcpy( ParamBuf, OptParm );
    return( ParamBuf );
}

static void StripQuotes( char *fname )
/************************************/
{
    char *s;
    char *d;

    if( *fname == '"' ) {
        // string will shrink so we can reduce in place
        d = fname;
        for( s = d + 1; *s && *s != '"'; ++s ) {
            // collapse double backslashes, only then look for escaped quotes
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
    char *fname;
    fname = CopyOfParm();
    StripQuotes( fname );
    return( fname );
}

#if BUILD_TARGET
static void SetTargName( char *name, unsigned len )
/*************************************************/
{
    if( Options.build_target != NULL ) {
        MemFree( Options.build_target );
        Options.build_target = NULL;
    }
    if( name == NULL || len == 0 )
        return;
    Options.build_target = MemAlloc( len + 1 );
    strcpy( Options.build_target, name );
    _strupr( Options.build_target );
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
            Options.cpu |= P_PM;      // set privileged mode
#if MANGLERSUPP
        } else if( *parm == '"' ) {       // set default mangler
            char *dest;
            parm++;
            dest = strchr( parm, '"' );
            if( Options.default_name_mangler != NULL ) {
                AsmFree( Options.default_name_mangler );
            }
            Options.default_name_mangler = MemAlloc( dest - parm + 1 );
            dest = Options.default_name_mangler;
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

/* queue a text macro or an include path.
   this is called for cmdline options -D and -I
 */

static void queue_item( struct qitem * *start, const char *string )
/*****************************************************************/
{
    struct qitem *p;
    struct qitem *q;

    DebugMsg(("queue_item(%s) enter\n", string));
    p = MemAlloc( sizeof(struct qitem) + strlen( string) + 1 );
    p->next = NULL;
    strcpy( p->value, string );
    q = *start;
    if (q) {
        for (;q->next;q = q->next);
        q->next = p;
    } else
        *start = p;
    return;
}

// get default file extension for error, object and listing files

static char *GetExt( int type )
/*****************************/
{
    switch (type) {
    case ERR:
        return( ERR_EXT );
    case OBJ:
#if BIN_SUPPORT
        if ( Options.output_format == OFORMAT_BIN )
#if MZ_SUPPORT
            if ( Options.header_format == HFORMAT_MZ )
                return("EXE");
            else
#endif
                return("BIN");
#endif
        return( OBJ_EXT );
    case LST:
        return( LST_EXT );
    }
    return( NULL );
}

static void GetDefaultFilenames( const char *name )
/*************************************************/
{
    int i;
    char fnamesrc[_MAX_FNAME];
    char drive[_MAX_DRIVE];
    char dir[_MAX_DIR];
    char fname[_MAX_FNAME];
    char ext[_MAX_EXT];
    char path[ _MAX_PATH  ];

    DebugMsg(("GetDefaultFilenames(%s) enter\n", name ));
    _splitpath( name, NULL, NULL, fnamesrc, NULL );
    for (i = ERR; i < NUM_FILE_TYPES; i++ ) {
        if( FileInfo.fname[i] == NULL ) {
            drive[0] = NULLC;
            dir[0] = NULLC;
            if ( FileDir[i])
                _splitpath( FileDir[i], drive, dir, NULL, NULL );
            _makepath( path, drive, dir, fnamesrc, GetExt( i ) );
        } else {
            _splitpath( FileInfo.fname[i], drive, dir, fname, ext );
            if( fname[0] != NULLC && ext[0] != NULLC )
                continue;
            if( fname[0] == NULLC )
                strcpy( fname, fnamesrc );
            if( ext[0] == NULLC )
                strcpy( ext, GetExt( i ) );

            _makepath( path, drive, dir, fname, ext );
            AsmFree( FileInfo.fname[i] );
        }
        FileInfo.fname[i] = AsmAlloc( strlen( path ) + 1 );
        strcpy( FileInfo.fname[i], path );
    }
    return;
}

static void get_fname( const char *token, int type )
/**************************************************/
/*
 * called for .OBJ, .ERR, .LST and .ASM filenames.
 * for ASM, figure out the source file name & store it in FileInfo
 * and fill in default object file name if it is null.
 */
{
    char        name [ _MAX_PATH  ];
    char        *pExt;
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    //char        msgbuf[MAXMSGSIZE];

    /* get filename for source file */

    if( type == ASM ) {

        FileInfo.fname[ASM] = AsmAlloc( strlen( token ) + 1 );
        strcpy( FileInfo.fname[ASM], token );

    } else {
        /* get filename for object, error, or listing file.
         * If it's ending with a '\' (or '/' in Unix), it's supposed
         * to be a directory name only.
         */
        _splitpath( token, drive, dir, fname, ext );
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
        if( FileInfo.fname[type] != NULL ) {
            AsmFree( FileInfo.fname[type] );
        }
        FileInfo.fname[type] = AsmAlloc( strlen( name ) + 1 );
        strcpy( FileInfo.fname[type], name );
    }
}

static void set_some_kinda_name( char token, const char *name )
/*************************************************************/
/* option -n: set name of
 * - nd: data seg
 * - nm: module name
 * - nt: text seg
 * - nc: code class
 */
{
    char **tmp;

    if ( *name != '.' && !is_valid_id_char( *name ) ) {
        AsmError( N_OPTION_NEEDS_A_NAME_PARAMETER );
        return;
    }

    switch( token ) {
    case 'c':
        tmp = &Options.code_class;
        break;
    case 'd':
        tmp = &Options.data_seg;
        break;
    case 'm':
        tmp = &Options.module_name;
        break;
    case 't':
        tmp = &Options.text_seg;
        break;
    default:
        return;
    }

    if( *tmp != NULL ) {
        MemFree(*tmp);
    }
    *tmp = MemAlloc( strlen( name ) + 1 );
    strcpy( *tmp, name );
}

static void usagex_msg( void )
/****************************/
{
    MsgPrintUsage();
    exit(1);
}

//static void Ignore( void ) {};

#if BUILD_TARGET
static void Set_bt( void ) { SetTargName( OptParm,  strlen(OptParm) ); }
#endif

static void Set_c( void ) { }

#if COFF_SUPPORT
static void Set_coff( void ) {
    Options.output_format = OFORMAT_COFF;
}
#endif
#if ELF_SUPPORT
static void Set_elf( void ) {
    Options.output_format = OFORMAT_ELF;
}
#if AMD64_SUPPORT
static void Set_elf64( void ) {
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
static void Set_bin( void ) {
    Options.output_format = OFORMAT_BIN;
    Options.header_format = HFORMAT_NONE;
}
#if MZ_SUPPORT
static void Set_mz( void ) {
    Options.output_format = OFORMAT_BIN;
    Options.header_format = HFORMAT_MZ;
}
#endif
#endif
static void Set_Cp( void ) { Options.case_sensitive = TRUE;   Options.convert_uppercase = FALSE; }
static void Set_Cu( void ) { Options.case_sensitive = FALSE;  Options.convert_uppercase = TRUE;  }
static void Set_Cx( void ) { Options.case_sensitive = FALSE;  Options.convert_uppercase = FALSE; }

static void Set_Zd( void ) { Options.line_numbers = TRUE; }
static void Set_Zi( void ) { Set_Zd(); Options.debug_symbols = TRUE; }

static void Set_Zm( void ) { Options.masm51_compat = TRUE; }

static void Set_Zne( void ) { Options.strict_masm_compat = TRUE; }

static void Set_Zg( void ) { Options.masm_compat_gencode = TRUE; }

static void Set_Zf( void ) { Options.all_symbols_public = TRUE; }

static void Set_Zp( void ) {
    uint_8 power;
    for ( power = 0; (1 << power) <= MAX_STRUCT_ALIGN; power++ )
        if ( ( 1 << power ) == OptValue ) {
            Options.fieldalign = power;
            return;
        }
    AsmWarn( 1, INVALID_CMDLINE_VALUE, "-Zp" );
    return;
}

//static void DefineMacro( void ) { queue_item( &Options.SymQueue, CopyOfParm() ); }
static void DefineMacro( void ) { queue_item( &Options.SymQueue, GetAFileName() ); }

static void SetErrorLimit( void ) { Options.error_limit = OptValue; }

static void Set_nologo( void ) { banner_printed = TRUE; }
static void Set_q( void )      { Set_nologo(); Options.quiet = TRUE; }
static void Set_EP( void ) { Options.preprocessor_stdout = TRUE; Set_q(); }

static void Set_Fr( void ) { get_fname( GetAFileName(), ERR ); }

static void Set_Fi( void ) { Options.ForceInclude = GetAFileName(); }

static void Set_Fl( void ) { get_fname( GetAFileName(), LST ); Options.write_listing = TRUE;}

static void Set_Fo( void ) { get_fname( GetAFileName(), OBJ ); }
static void Set_fp( void ) { Options.cpu &= ~P_FPU_MASK; Options.cpu = OptValue; }
static void Set_FPx( void ) { Options.floating_point = OptValue; }
static void Set_G( void ) { Options.langtype = OptValue; }

static void SetInclude( void ) { queue_item( &Options.IncQueue, GetAFileName() ); }

#ifdef DEBUG_OUT
static void Set_ls( void ) { Options.print_linestore = TRUE; };
#endif

static void Set_Sa( void )
{
    Options.listif = TRUE;
    Options.list_generated_code = TRUE;
    Options.list_macro = LM_LISTMACROALL;
}
static void Set_Sg( void ) { Options.list_generated_code = TRUE; }
static void Set_Sn( void ) { Options.no_symbol_listing = TRUE; }
static void Set_Sx( void ) { Options.listif = TRUE; }
static void Set_safeseh( void ) { Options.safeseh = TRUE; }

static void Set_m( void ) { Options.model = OptValue; }
static void Set_n( void ) { set_some_kinda_name( OptValue, CopyOfParm() ); }

#if COCTALS
static void Set_o( void ) { Options.allow_c_octals = TRUE; }
#endif
#ifdef DEBUG_OUT
static void Set_pm( void ) { Options.max_passes = OptValue; };
#endif
static void Set_omf( void ) { Options.output_format = OFORMAT_OMF;}

static void Set_WX( void ) { Options.warning_error = TRUE; }

static void Set_w( void ) { Set_WX(); Options.warning_level = 0; }

static void SetWarningLevel( void )
{
    if ( OptValue <= 4 )
        Options.warning_level = OptValue;
    else
        AsmWarn( 1, INVALID_CMDLINE_VALUE, "/W" );
}

static void Set_X( void ) { Options.ignore_include = TRUE; }
#if AMD64_SUPPORT
static void Set_win64( void )
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
static void Set_zcm( void ) { Options.no_cdecl_decoration = FALSE; }
static void Set_zcw( void ) { Options.no_cdecl_decoration = TRUE; }
static void Set_zf( void ) { Options.fastcall = OptValue; }


static void Set_zlc( void ) { Options.no_comment_data_in_code_records = TRUE; }
//static void Set_zld( void ) { Options.no_dependencies       = TRUE; }
#if COFF_SUPPORT
static void Set_zlf( void ) { Options.no_file_entry         = TRUE; }
static void Set_zls( void ) { Options.no_section_aux_entry  = TRUE; }
#endif

static void Set_Zs( void ) { Options.syntax_check_only    = TRUE; }
static void Set_zt( void ) { Options.stdcall_decoration    = OptValue; }
static void Set_zze( void ) { Options.no_export_decoration  = TRUE; }
#if COFF_SUPPORT
static void Set_zzs( void ) { Options.entry_decorated       = TRUE; }
#endif

static void HelpUsage( void ) { usagex_msg();}

#ifdef DEBUG_OUT
static void Set_d6( void )
{
    Options.debug = TRUE;
    DebugMsg(( "debugging output on\n" ));
}
#endif

static struct cmdloption const cmdl_options[] = {
    { "?",      0,        HelpUsage },
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
#endif
    { "D^$",    0,        DefineMacro },
#if ELF_SUPPORT
#if AMD64_SUPPORT
    { "elf64",  0,        Set_elf64 },
#endif
    { "elf",    0,        Set_elf },
#endif
    { "EP",     0,        Set_EP },
    { "e=#",    0,        SetErrorLimit },
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
    { "h",      0,        HelpUsage },
    { "I=^@",   0,        SetInclude },
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
    { "nc=$",   'c',      Set_n },
    { "nd=$",   'd',      Set_n },
    { "nm=$",   'm',      Set_n },
    { "nt=$",   't',      Set_n },
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
    { "safeseh",0,        Set_safeseh },
    { "WX",     0,        Set_WX },
    { "W=#",    0,        SetWarningLevel },
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
    { "zf0",    FCT_MS32,    Set_zf },
    { "zf1",    FCT_WATCOMC, Set_zf },
    { "zlc",    0,        Set_zlc },
//    { "zld",    0,        Set_zld },
#if COFF_SUPPORT
    { "zlf",    0,        Set_zlf },
    { "zls",    0,        Set_zls },
#endif
    { "Zs",     0,        Set_Zs },
    { "zt0",    STDCALL_NONE, Set_zt },
    { "zt1",    STDCALL_HALF, Set_zt },
    { "zt2",    STDCALL_FULL, Set_zt },
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

static void free_strings( void )
/******************************/
/* Free strings set by cmdline options */
{
#if BUILD_TARGET
    if( Options.build_target != NULL )
        MemFree( Options.build_target );
#endif
    if( Options.code_class != NULL )
        MemFree( Options.code_class );
    if( Options.data_seg != NULL )
        MemFree( Options.data_seg );
    if( Options.module_name != NULL )
        MemFree( Options.module_name );
    if( Options.text_seg != NULL )
        MemFree( Options.text_seg );
}

#if BUILD_TARGET
#define MAX_OS_NAME_SIZE 7

static void set_default_build_target( void )
/******************************************/
{

    if( Options.build_target == NULL ) {
        Options.build_target = MemAlloc( MAX_OS_NAME_SIZE + 1 );
#if defined(__OSI__)
        if( __OS == OS_DOS ) {
            strcpy( Options.build_target, "DOS" );
        } else if( __OS == OS_OS2 ) {
            strcpy( Options.build_target, "OS2" );
        } else if( __OS == OS_NT ) {
            strcpy( Options.build_target, "NT" );
        } else if( __OS == OS_WIN ) {
            strcpy( Options.build_target, "WINDOWS" );
        } else {
            strcpy( Options.build_target, "XXX" );
        }
#elif defined(__QNX__)
        strcpy( Options.build_target, "QNX" );
#elif defined(__LINUX__)
        strcpy( Options.build_target, "LINUX" );
#elif defined(__BSD__)
        strcpy( Options.build_target, "BSD" );
#elif defined(__OSX__) || defined(__APPLE__)
        strcpy( Options.build_target, "OSX" );
#elif defined(__DOS__)
        strcpy( Options.build_target, "DOS" );
#elif defined(__OS2__)
        strcpy( Options.build_target, "OS2" );
#elif defined(__NT__)
        strcpy( Options.build_target, "NT" );
#else
        #error unknown host OS
#endif
    }
    return;
}
#endif

/*
 * get a "filename" or "macro"
 */
static const char *GetToken( char *dst, const char *str, int max, bool isequate )
/*******************************************************************************/
{
    bool equatefound = FALSE;

    //while( isspace( *str ) ) ++str;  // no spaces allowed!
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
        for( ;max; max-- ) {
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
        Fatal( FATAL_CANNOT_OPEN_FILE, name, errno );
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
    // zip through characters changing \r, \n etc into ' '
    str = env;
    while( *str ) {
        ch = *str;
        if( ch == '\r' || ch == '\n' ) {
            *str = ' ';
        }
#if HANDLECTRLZ
        if( ch == 0x1A ) {      // if end of file
            *str = '\0';        // - mark end of str
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
            AsmFree( (void *)cmdbuffers[rspidx] );
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
    char  *opt;
    //char  c;

    numArgs++;
    DebugMsg(("ProcessOption(%s)\n", p ));

    /* numeric option (-0, -1, ... ) handled separately since
     * the value can be >= 10.
     */
    if ( *p >= '0' && *p <= '9' ) {
        p = GetNumber( p );
        if ( OptValue < sizeof(cpuoption)/sizeof(cpuoption[0]) ) {
            p = GetToken( buffer, p, 16, FALSE ); /* get optional 'p' */
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
            // make sure end of option is reached
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
                case '#':             // collect a number
                    if( *p >= '0' && *p <= '9' )
                        p = GetNumber( p );
                    break;
                case '$':      // collect an identifer+value
                case '@':      // collect a filename
                    OptParm = buffer;
                    if ( rspidx )
                        p = GetToken( buffer, p, _MAX_PATH - 1, *opt == '$' );
                    else {
                        j = strlen( p );
                        memcpy( buffer, p, (j >= _MAX_PATH) ? _MAX_PATH : j+1 );
                        p += j;
                    }
                    break;
                case '=':    // collect an optional '='
                    if ( *p == '=' || *p == '#' )
                        p++;
                    break;
                case '^':    // skip spaces before argument
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
    AsmWarn( 1, INVALID_CMDLINE_OPTION, *cmdline );
    *cmdline = "";
    return;
}

/* parse cmdline:
 * - handle (nested) response files
 * - process options
 * - get filename argument
 */

static void parse_cmdline( const char **cmdline )
/***********************************************/
{
    const char *str = *cmdline;
    char paramfile[_MAX_PATH];

    for( ; str; ) {
        while( *str == ' ' || *str == '\t' )
            ++str;
        if( *str == '@' ) {
            if ( rspidx >= MAX_RSP_NESTING )
                Fatal( FATAL_NESTING_LEVEL_TOO_DEEP );
            str++;
            if ( rspidx ) {
                cmdsave[rspidx] = GetToken( paramfile, str, sizeof( paramfile ) - 1, FALSE );
            } else {
                strcpy( paramfile, str );
                cmdsave[rspidx] = str + strlen(str);
            }
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
            str = *cmdline;
        } else {  /* collect  file name */
            int len;
            if ( rspidx ) {
                str = GetToken( paramfile, str, sizeof( paramfile ) - 1, FALSE );
                get_fname( paramfile, ASM );
            } else {
                len = strlen( str );
                get_fname( str, ASM ); /* try to open .asm file */
                str += len;
            }
            numArgs++;
            numFiles++;
            break;
        }
    }
    *cmdline = str;
    return;
}

static void genfailure(int signo)
/*******************************/
{
#if CATCHBREAK
    if (signo != SIGBREAK)
#else
    if (signo != SIGTERM)
#endif
        AsmError( GENERAL_FAILURE );
    close_files();
    exit ( EXIT_FAILURE );
}

/* main_init is called once per module */

static void main_init( void )
/***************************/
{
    int i;

    MemInit();
    /* v2.01: ModuleInfo now initialized in AssembleModule() */
    //memset( &ModuleInfo, 0, sizeof(ModuleInfo));
    for( i = 0; i < NUM_FILE_TYPES; i++ ) {
        FileInfo.file[i] = NULL;
        FileInfo.fname[i] = NULL;
    }
}

static void main_fini( void )
/***************************/
{
    int i;
    for( i = 0; i < NUM_FILE_TYPES; i++ ) {
        AsmFree( FileInfo.fname[i] );
    }
    MemFini();
}

int main( int argc, char **argv )
/*******************************/
{
    char       *pEnv;

#if WILDCARDS
    long        fh; /* _findfirst/next/close() handle, must be long! */
    static struct _finddata_t finfo;
    static char drv[_MAX_DRIVE];
    static char dir[_MAX_DIR];
#endif

#if 0 //def DEBUG_OUT    // DebugMsg() cannot be used that early
    int i;
    for (i = 1; i < argc; i++ ) {
        printf("argv[%u]=>%s<\n", i, argv[i] );
    }
#endif

    pEnv = getenv( "JWASM" );
    if ( pEnv == NULL )
        pEnv = "";
    argv[0] = pEnv;

#ifndef DEBUG_OUT
    signal(SIGSEGV, genfailure);
#endif

#if CATCHBREAK
    signal(SIGBREAK, genfailure);
#else
    signal(SIGTERM, genfailure);
#endif

    MsgInit();

    FileDir[ERR] = NULL;
    FileDir[OBJ] = NULL;
    FileDir[LST] = NULL;

    while ( 1 ) {
        main_init();
#ifdef DEBUG_OUT
        ModuleInfo.cref = TRUE; /* don't suppress debug displays */
#endif
        parse_cmdline( (const char **)argv );
        if( FileInfo.fname[ASM] == NULL ) /* source file name supplied? */
            break;
        trademark();
#if BUILD_TARGET
        set_default_build_target();
#endif
#if WILDCARDS
        if ((fh = _findfirst( FileInfo.fname[ASM], &finfo )) == -1 ) {
            DebugMsg(("main: _findfirst(%s) failed\n", FileInfo.fname[ASM] ));
            AsmErr( CANNOT_OPEN_FILE, FileInfo.fname[ASM], errno );
            break;
        }
        _splitpath( FileInfo.fname[ASM], drv, dir, NULL, NULL );
        AsmFree( FileInfo.fname[ASM]);
        while ( 1 ) {
            FileInfo.fname[ASM] = AsmAlloc( strlen(drv) + strlen(dir) + strlen(finfo.name) + 3 );
            _makepath( FileInfo.fname[ASM], drv, dir, finfo.name, NULL );
#endif
            GetDefaultFilenames( FileInfo.fname[ASM] );
            AssembleModule();     // assemble source module
            main_fini();
#if WILDCARDS
            if ( _findnext( fh, &finfo ) == -1 ) {
                break;
            }
            main_init();
        }
        _findclose( fh );
#endif
    };
    free_strings();
    if ( numArgs == 0 ) {
        MsgPrintf( MSG_USAGE );
    } else if ( numFiles == 0 )
        AsmError( NO_FILENAME_SPECIFIED );

    MsgFini();
    return( ModuleInfo.error_count != 0 ); /* zero if no errors */
}

