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
#include <fcntl.h>
#include <ctype.h>
#include <signal.h>
#include <sys/stat.h>   /* _S_IREAD ... */

#include "globals.h"
#include "myunistd.h"
#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "fatal.h"
#include "equate.h"
#include "omf.h"
#include "input.h"
#include "errmsg.h"
#include "macro.h"
#include "listing.h"
#include "msgtext.h"

#ifdef __OSI__
  #include "ostype.h"
#endif

#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__)
#define WILDCARDS 0
#define CATCHBREAK 0
#define HANDLECTRLZ 0
#define HANDLESWITCHCHAR 0
#ifndef __DJGPP
#define OPENBINARY 0
#else
#define OPENBINARY 1
#endif
#else
#define WILDCARDS 1
#ifdef __POCC__
#define CATCHBREAK 0
#else
#define CATCHBREAK 1
#endif
#define HANDLECTRLZ 1
#define HANDLESWITCHCHAR 1
#define OPENBINARY 1
#endif

#if OPENBINARY

#define OP_MODE         (O_RDWR | O_CREAT | O_TRUNC | O_BINARY)
#ifdef __WATCOMC__
#define OP_PERM         (S_IREAD | S_IWRITE)
#else
 #ifdef __DJGPP__
#define OP_PERM         (S_IRUSR | S_IWUSR)
 #else
#define OP_PERM         (_S_IREAD | _S_IWRITE)
 #endif
#endif

#else

#define OP_MODE         (O_RDWR | O_CREAT | O_TRUNC)
#define OP_PERM         (0666)

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
static char             *cmdsave[MAX_RSP_NESTING]; /* response files */
static char             *cmdbuffers[MAX_RSP_NESTING]; /* response files */
static int              rspidx = 0; /* response file level */
#if HANDLESWITCHCHAR
static unsigned char    SwitchChar = '/';
#endif

global_options Options = {
    /* sign_value       */          FALSE,
    /* quiet            */          FALSE,
    /* line_numbers     */          FALSE,
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
    /* forced include  */           NULL,
    /* symbol queue  */             NULL,
    /* include path queue */        NULL,
#if COCTALS
    /* allow_c_octals        */     FALSE,
#endif
    /* no_comment_data_in_code_records */   FALSE,
    /* no_dependencies       */     FALSE,
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
    /* ignore_include        */     FALSE,
    /* output_format         */     OFORMAT_OMF,
    /* header_format         */     HFORMAT_NONE,
    /* alignment_default     */     0,
    /* langtype              */     LANG_NONE,
    /* model                 */     MOD_NONE,
    /* cpu                   */     P_86,
    /* fastcall type         */     FCT_MS32,
#if MANGLERSUPP
    /* default_name_mangler  */     NULL,
    /* naming_convention*/          NC_DO_NOTHING,
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
    char        *p;

    if( Options.build_target != NULL ) {
        AsmFree( Options.build_target );
        Options.build_target = NULL;
    }
    if( name == NULL || len == 0 )
        return;
    Options.build_target = MemAlloc( len + 1 );
    p = Options.build_target;
    while( len != 0 ) {
        *p++ = toupper( *name++ );
        --len;
    }
    *p++ = '\0';
}
#endif

/* called by -0, -1, ... -6 argument */

static void SetCPUCmdline( void )
/*******************************/
{
    char  *tmp;

    Options.cpu &= ~(P_CPU_MASK | P_PM);
    Options.cpu |= OptValue;
#if 0 //AMD64_SUPPORT
    /* implicitely set model flat if cpu is set to x86-64 via
     * commandline. This is deactive, because it's intransparent.
     */
    if ( Options.cpu == P_64 )
        if ( Options.model == MOD_NONE )
            Options.model = MOD_FLAT;
#endif
    for( tmp = OptParm; *tmp ; tmp++ ) {
        if( *tmp == 'p' ) {
            if( Options.cpu >= P_286 ) {         // set privileged mode
                Options.cpu |= P_PM;
            } else {
                AsmErr( CPU_OPTION_INVALID, CopyOfParm() );
            }
#if MANGLERSUPP
        } else if( *tmp == '"' ) {       // set default mangler
            char *dest;
            tmp++;
            dest = strchr(tmp, '"');
            if( Options.default_name_mangler != NULL ) {
                AsmFree( Options.default_name_mangler );
            }
            Options.default_name_mangler = MemAlloc( dest - tmp + 1 );
            dest = Options.default_name_mangler;
            for( ; *tmp != '"'; dest++, tmp++ ) {
                *dest = *tmp;
            }
            *dest = NULLC;
#endif
        } else {
            AsmWarn( 1, INVALID_CMDLINE_OPTION, CopyOfParm() );
            break;
        }
    }
}

/* queue a text macro or an include path.
   this is called for cmdline options -D and -I
 */

static void queue_item( struct qitem * *start, char *string )
/***********************************************************/
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

static void GetDefaultFilenames( char *name )
/*******************************************/
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

static void get_fname( char *token, int type )
/********************************************/
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

static void set_some_kinda_name( char token, char *name )
/*******************************************************/
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

    /* these params are valid for ONE module only */

    if( *tmp != NULL ) {
        AsmFree(*tmp);
    }
    *tmp = AsmAlloc( strlen( name ) + 1 );
    strcpy( *tmp, name );
}

static void usagex_msg( void )
/***************************/
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

static void Set_Zm( void ) { Options.masm51_compat = TRUE; }

static void Set_Zne( void ) { Options.strict_masm_compat = TRUE; }

static void Set_Zg( void ) { Options.masm_compat_gencode = TRUE; }

static void Set_Zf( void ) { Options.all_symbols_public = TRUE; }

static void Set_Zp( void ) {
    uint_8 power;
    for (power = 1;power < OptValue && power < MAX_STRUCT_ALIGN; power = power << 1);
    if (power == OptValue)
        Options.alignment_default = OptValue;
    else {
        AsmWarn(1, INVALID_CMDLINE_VALUE, "/Zp");
    }
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

static void Set_Gc( void ) { Options.langtype = LANG_PASCAL; }
static void Set_Gd( void ) { Options.langtype = LANG_C; }
static void Set_Gz( void ) { Options.langtype = LANG_STDCALL; }

static void SetInclude( void ) { queue_item( &Options.IncQueue, GetAFileName() ); }

static void Set_j( void ) { Options.sign_value = TRUE; }

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

static void Set_mt( void ) { Options.model = MOD_TINY; }
static void Set_ms( void ) { Options.model = MOD_SMALL; }
static void Set_mm( void ) { Options.model = MOD_MEDIUM; }
static void Set_mc( void ) { Options.model = MOD_COMPACT; }
static void Set_ml( void ) { Options.model = MOD_LARGE; }
static void Set_mh( void ) { Options.model = MOD_HUGE; }
static void Set_mf( void ) { Options.model = MOD_FLAT; }

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

static void SetWarningLevel( void ) { Options.warning_level = OptValue; }

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
        OptValue = P_64;
        OptParm = "";
        SetCPUCmdline();
    }
}
#endif
static void Set_zcm( void ) { Options.no_cdecl_decoration = FALSE; }
static void Set_zcw( void ) { Options.no_cdecl_decoration = TRUE; }
static void Set_zf0( void ) { Options.fastcall = FCT_MS32; }
static void Set_zf1( void ) { Options.fastcall = FCT_WATCOMC; }

static void Set_Zi( void ) { }

static void Set_zlc( void ) { Options.no_comment_data_in_code_records = TRUE; }
static void Set_zld( void ) { Options.no_dependencies       = TRUE; }
static void Set_zlf( void ) { Options.no_file_entry         = TRUE; }
static void Set_zls( void ) { Options.no_section_aux_entry  = TRUE; }

static void Set_zs0( void ) { Options.stdcall_decoration    = STDCALL_NONE; }
static void Set_zs1( void ) { Options.stdcall_decoration    = STDCALL_HALF; }
static void Set_zs2( void ) { Options.stdcall_decoration    = STDCALL_FULL; }
static void Set_zze( void ) { Options.no_export_decoration  = TRUE; }
static void Set_zzs( void ) { Options.entry_decorated       = TRUE; }

static void HelpUsage( void ) { usagex_msg();}

#ifdef DEBUG_OUT
static void Set_d6( void )
{
    Options.debug = TRUE;
    DebugMsg(( "debugging output on\n" ));
}
#endif

static struct cmdloption const cmdl_options[] = {
    { "0$",     P_86,     SetCPUCmdline },
    { "1$",     P_186,    SetCPUCmdline },
    { "2$",     P_286,    SetCPUCmdline },
    { "3$",     P_386,    SetCPUCmdline },
    { "4$",     P_486,    SetCPUCmdline },
    { "5$",     P_586,    SetCPUCmdline },
    { "6$",     P_686,    SetCPUCmdline },
#if AMD64_SUPPORT
    { "7$",     P_64,     SetCPUCmdline },
#endif
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
    { "Gc",     0,        Set_Gc },
    { "Gd",     0,        Set_Gd },
    { "Gz",     0,        Set_Gz },
    { "h",      0,        HelpUsage },
    { "I=^@",   0,        SetInclude },
    { "j",      0,        Set_j },
#ifdef DEBUG_OUT
    { "ls",     0,        Set_ls },
#endif
    { "mc",     0,        Set_mc },
    { "mf",     0,        Set_mf },
    { "mh",     0,        Set_mh },
    { "ml",     0,        Set_ml },
    { "mm",     0,        Set_mm },
    { "ms",     0,        Set_ms },
    { "mt",     0,        Set_mt },
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
    { "zf0",    0,        Set_zf0 },
    { "zf1",    0,        Set_zf1 },
    { "zlc",    0,        Set_zlc },
    { "zld",    0,        Set_zld },
    { "zlf",    0,        Set_zlf },
    { "zls",    0,        Set_zls },
    { "zs0",    0,        Set_zs0 },
    { "zs1",    0,        Set_zs1 },
    { "zs2",    0,        Set_zs2 },
    { "zze",    0,        Set_zze },
    { "zzs",    0,        Set_zzs },
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

static void free_names( void )
/****************************/
/* Free names set as cmdline options */
{
#if BUILD_TARGET
    if( Options.build_target != NULL ) {
        AsmFree( Options.build_target );
        Options.build_target = NULL;
    }
#endif
    if( Options.code_class != NULL ) {
        AsmFree( Options.code_class );
        Options.code_class = NULL;
    }
    if( Options.data_seg != NULL ) {
        AsmFree( Options.data_seg );
        Options.data_seg = NULL;
    }
    if( Options.module_name != NULL ) {
        AsmFree( Options.module_name );
        Options.module_name = NULL;
    }
    if( Options.text_seg != NULL ) {
        AsmFree( Options.text_seg );
        Options.text_seg = NULL;
    }
}

#if BUILD_TARGET
#define MAX_OS_NAME_SIZE 7

static void set_default_build_target( void )
/******************************************/
{

    if( Options.build_target == NULL ) {
        Options.build_target = AsmAlloc( MAX_OS_NAME_SIZE + 1 );
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

void CloseFiles( void )
/**********************/
{
    /* close ASM file */
    if( FileInfo.file[ASM] != NULL ) {
        if( fclose( FileInfo.file[ASM] ) != 0 ) {
            FileInfo.file[ASM] = NULL;
            Fatal( FATAL_CANNOT_CLOSE_FILE, FileInfo.fname[ASM] );
        }
    }
    LstCloseFile();

    if ( Options.output_format == OFORMAT_OMF )
        omf_fini();

    /* close OBJ file */
    if ( ModuleInfo.obj_fh != -1 ) {
        _close( ModuleInfo.obj_fh );
        ModuleInfo.obj_fh = -1;
    }

    if( ModuleInfo.error_count > 0 ) {
        remove( FileInfo.fname[OBJ] );
    }
    AsmFree( FileInfo.fname[ASM] );
    AsmFree( FileInfo.fname[ERR] );
    AsmFree( FileInfo.fname[LST] );
    AsmFree( FileInfo.fname[OBJ] );
    MemFini();
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
#ifdef __BORLANDC__
    /* borland known both open() and _open()! */
    ModuleInfo.obj_fh = open( FileInfo.fname[OBJ], OP_MODE, OP_PERM );
#else
    ModuleInfo.obj_fh = _open( FileInfo.fname[OBJ], OP_MODE, OP_PERM );
#endif
    if( ModuleInfo.obj_fh < 0 ) {
        DebugMsg(("open_files(): fopen(%s) failed\n", FileInfo.fname[OBJ] ));
        Fatal( FATAL_CANNOT_OPEN_FILE, FileInfo.fname[OBJ], errno );
    }

    if ( Options.output_format == OFORMAT_OMF )
        omf_init( ModuleInfo.obj_fh );

    /* delete any existing ERR file */
    InitErrFile();
}

/*
 * get a "filename" or "macro"
 */
static char *GetToken( char *dst, char *str, int max, bool isequate )
/*******************************************************************/
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

static char *ReadParamFile( char *name )
/**************************************/
{
    char        *env;
    char        *str;
    FILE        *fh;
    int         len;
    char        ch;

    env = NULL;
    fh = fopen( name, "rb" );
    if( fh == NULL ) {
        Fatal( FATAL_CANNOT_OPEN_FILE, name, errno );
        return( NULL );
    }
    len = 0;
    if ( fseek( fh, 0, SEEK_END ) == 0 ) {
        len = ftell( fh );
        rewind( fh );
        env = MemAlloc( len + 1 );
        fread( env, 1, len, fh );
        env[len] = NULLC;
    }
    fclose( fh );
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

static char *getnextcmdstring( char **cmdline )
/*********************************************/
{
    char **src;
    char **dst;

    /* something onto the response file stack? */
    if ( rspidx ) {
        rspidx--;
        if ( cmdbuffers[rspidx] )
            AsmFree( cmdbuffers[rspidx] );
        return( cmdsave[rspidx] );
    }
    for ( dst = cmdline, src = cmdline+1; *src; )
        *dst++ = *src++;
    *dst = *src;
    return( *cmdline );
}

/* scan option table and if option is known, process it */

static void ProcessOption( char **cmdline, char *buffer )
/*******************************************************/
{
    int   i;
    int   j;
    char  *p = *cmdline;
    char  *opt;
    char  c;

    numArgs++;
    DebugMsg(("ProcessOption(%s)\n", p ));
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
                    if( *p >= '0' && *p <= '9' ) {
                        OptValue = 0;
                        for( ;; ) {
                            c = *p;
                            if( c < '0' || c > '9' )
                                break;
                            OptValue = OptValue * 10 + c - '0';
                            p++;
                        }
                    }
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
                    break;
                }
            }
        }
    }
opt_error_exit:
    AsmWarn( 1, INVALID_CMDLINE_OPTION, *cmdline );
    **cmdline = NULLC;
    return;
}

/* parse cmdline:
 * - handle (nested) response files
 * - process options
 * - get filename argument
 */

static void parse_cmdline( char **cmdline )
/*****************************************/
{
    char *str = *cmdline;
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
/************************/
{
#if CATCHBREAK
    if (signo != SIGBREAK)
#else
    if (signo != SIGTERM)
#endif
        AsmError( GENERAL_FAILURE );
    CloseFiles();
    exit ( EXIT_FAILURE );
}

/* main_init is called once per module */

static void main_init( void )
/***************************/
{
    int         i;

    MemInit();
    memset( &ModuleInfo, 0, sizeof(ModuleInfo));
    ModuleInfo.obj_fh = -1;
    for( i = 0; i < NUM_FILE_TYPES; i++ ) {
        FileInfo.file[i] = NULL;
        FileInfo.fname[i] = NULL;
    }
}

static void main_fini( void )
/***************************/
{
    free_names();
    CloseFiles();
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
        ModuleInfo.cref = TRUE; /* enable debug displays */
#endif
        parse_cmdline( argv );
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
            open_files();
            AssembleModule();     // main body: parse the source file
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
    if ( numArgs == 0 ) {
        MsgPrintf( MSG_USAGE );
    } else if ( numFiles == 0 )
        AsmError( NO_FILENAME_SPECIFIED );

    MsgFini();
    return( ModuleInfo.error_count != 0); /* zero if no errors */
}

