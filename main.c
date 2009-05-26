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

#if defined(__UNIX__) || defined(__CYGWIN__)
#define WILDCARDS 0
#define CATCHBREAK 0
#define HANDLECTRLZ 0
#define HANDLESWITCHCHAR 0
#define USECRTARGC 1
#define OPENBINARY 0
#else
#define CATCHBREAK 1
#define HANDLECTRLZ 1
#define HANDLESWITCHCHAR 1
#define WILDCARDS 1
#ifdef __FLAT__
 #ifdef __OS2__
  #define USECRTARGC 1
 #else
  #define USECRTARGC 0 /* Win32, DOS32 */
 #endif
#else
 #define USECRTARGC 1 /* 16bit */
#endif
#define OPENBINARY 1
#endif

#if OPENBINARY

#define OP_MODE         (O_RDWR | O_CREAT | O_TRUNC | O_BINARY)
#ifdef __WATCOMC__
#define OP_PERM         (S_IREAD | S_IWRITE)
#else
#define OP_PERM         (_S_IREAD | _S_IWRITE)
#endif

#else

#define OP_MODE         (O_RDWR | O_CREAT | O_TRUNC)
#define OP_PERM         (0666)

#endif

extern char     banner_printed;

File_Info       FileInfo;       // files information
int             obj_fh = -1;    // object file handle
static int      numArgs = 0;
static int      numFiles = 0;

struct  option {
    char        *option;
    unsigned    value;
    void        (*function)( void );
};

#define MAX_NESTING 15
#define BUF_SIZE 512

#define DisableKeyword( x )  AsmOpTable[AsmResWord[ x ].position].disabled = TRUE;

static char             ParamBuf[ BUF_SIZE ];
static unsigned char    SwitchChar;
static unsigned         OptValue;
static char             *OptScanPtr;
static char             *OptParm;

global_options Options = {
    /* sign_value       */          FALSE,
    /* quiet            */          FALSE,
    /* line_numbers     */          FALSE,
    /* naming_convention*/          NC_DO_NOTHING,
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
#if MANGLERSUPP
    /* default_name_mangler  */     NULL,
#endif
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
    /* Watcom C name mangler */     FALSE,
    /* no_stdcall_decoration */     FALSE,
    /* no_stdcall_export_decoration */ FALSE,
    /* no_stdcall_suffix     */     FALSE,
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
    /* alignment_default     */     0,
    /* langtype              */     LANG_NONE,
    /* model                 */     MOD_NONE,
    /* cpu                   */     0,
    /* fpu                   */     -1,
#if OWREGCONV
    /* open watcom reg conv  */     FALSE,
#endif
    /* privileged mode       */     FALSE
};

#if defined(__WATCOMC__) && !defined(__UNIX__)

#ifdef __FLAT__

typedef union cu {
    int c;
    char * p;
} cu;

// this is an emulation of the Win32 function which is called
// by the OW runtime. It's the only USER32 function used.
// By defining it here the binary will just need KERNEL32 to load.

char * _stdcall CharUpperA(char * lpsz)
{
    cu p;
    p.p = lpsz;

    if (p.c < 0x10000)
        if (p.c >= 'a')
            return((char *)p.c - 0x20);
        else
            return((char *)p.c);
    else
        for (;*p.p;p.p++)
            if (*p.p >= 'a')
                *p.p = *p.p - 0x20;
    return(lpsz);
}
#endif
#endif

static char *CopyOfParm( void )
/*****************************/
{
    unsigned    len;

    len = OptScanPtr - OptParm;
    memcpy( ParamBuf, OptParm, len );
    ParamBuf[ len ] = NULLC;
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

static void SetCPUCmdline( void )
/************************/
{
    char  *tmp;

    Options.cpu = OptValue;

    for( tmp = OptParm; tmp < OptScanPtr; tmp++ ) {
        if( *tmp == 'p' ) {
            if( Options.cpu >= 2 ) {         // set privileged mode
                Options.privileged_mode = TRUE;
            } else {
                AsmErr( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
#if OWREGCONV
        } else if( *tmp == 'r' ) {
            if( Options.cpu >= 3 ) {  // set register calling convention
                Options.register_conventions = TRUE;
            } else {
                AsmErr( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
        } else if( *tmp == 's' ) {
            if( Options.cpu >= 3 ) {  // set stack calling convention
                Options.register_conventions = FALSE;
            } else {
                AsmErr( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
#endif
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
            AsmErr( INVALID_CMDLINE_OPTION, CopyOfParm() );
            exit( 1 );
        }
    }
#if OWREGCONV
    if( Options.cpu < 2 ) {
        Options.privileged_mode = FALSE;
        Options.register_conventions = TRUE;
    } else if( Options.cpu < 3 ) {
        Options.register_conventions = TRUE;
    }
#endif
}

static void SetFPUCmdline( void )
/************************/
{
    switch( OptValue ) {
    case 'i':
        Options.floating_point = FPO_EMULATION;
        break;
    case '7':
        Options.floating_point = FPO_NO_EMULATION;
        break;
    case 'c':
        Options.floating_point = FPO_DISABLED;
        break;
    case 0:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
        Options.fpu = OptValue;
        break;
    }
}

/* queue a text macro or an include path.
   this is called for cmdline options -D and -I
 */

static void queue_item( struct qitem * *start, char *string )
/**************************************/
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

static void GetDefaultFilenames( char *name )
{
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    char        path[ _MAX_PATH  ];

    DebugMsg(("GetDefaultFilenames(%s) enter\n", name ));
    if( FileInfo.fname[OBJ] == NULL ) {
        _splitpath( name, drive,
                    dir, fname, ext );
#if BIN_SUPPORT
        if ( Options.output_format == OFORMAT_BIN )
            _makepath( path, NULL, NULL, fname, "BIN" );
        else
#endif
            _makepath( path, NULL, NULL, fname, OBJ_EXT );
    } else {
        _splitpath( FileInfo.fname[OBJ], drive,
                    dir, fname, ext );
        if( fname[0] == NULLC )
            _splitpath( name, NULL, NULL, fname, NULL );
        if( ext[0] == NULLC )
            strcpy(ext, OBJ_EXT);

        _makepath( path, drive, dir, fname, ext );
        AsmFree( FileInfo.fname[OBJ] );
    }
    FileInfo.fname[OBJ] = AsmAlloc( strlen( path ) + 1 );
    strcpy( FileInfo.fname[OBJ], path );

    if( FileInfo.fname[ERR] == NULL ) {
        _makepath( path, NULL, NULL, fname, ERR_EXT );
    } else {
        _splitpath( FileInfo.fname[ERR], drive,
                    dir, fname, ext );
        if( fname[0] == NULLC )
            _splitpath( name, NULL, NULL, fname, NULL );
        if( ext[0] == NULLC )
            strcpy(ext, ERR_EXT);

        _makepath( path, drive, dir, fname, ext );
        AsmFree( FileInfo.fname[ERR] );
    }
    FileInfo.fname[ERR] = AsmAlloc( strlen( path ) + 1 );
    strcpy( FileInfo.fname[ERR], path );

    if( FileInfo.fname[LST] == NULL ) {
        _makepath( path, NULL, NULL, fname, LST_EXT );
    } else {
        _splitpath( FileInfo.fname[LST], drive,
                    dir, fname, ext );
        if( fname[0] == NULLC )
            _splitpath( name, NULL, NULL, fname, NULL );
        if( ext[0] == NULLC )
            strcpy(ext, LST_EXT);

        _makepath( path, drive, dir, fname, ext );
        AsmFree( FileInfo.fname[LST] );
    }
    FileInfo.fname[LST] = AsmAlloc( strlen( path ) + 1 );
    strcpy( FileInfo.fname[LST], path );
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
    char        drive2[_MAX_DRIVE];
    char        dir2[_MAX_DIR];
    char        fname2[_MAX_FNAME];
    char        ext2[_MAX_EXT];
    //char        msgbuf[MAXMSGSIZE];

    /* get filename for source file */

    if( type == ASM ) {

        FileInfo.fname[ASM] = AsmAlloc( strlen( token ) + 1 );
        strcpy( FileInfo.fname[ASM], token );

    } else {
        /* get filename for object, error, or listing file */
        _splitpath( token, drive, dir, fname, ext );
        if( FileInfo.fname[ASM] != NULL ) {
            _splitpath( FileInfo.fname[ASM], drive2, dir2, fname2, ext2 );
            if( fname[0] == NULLC ) {
                strcpy( fname, fname2 );
            }
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
        exit( 1 );
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
static void Set_bt( void ) { SetTargName( OptParm,  OptScanPtr - OptParm ); }
#endif

static void Set_c( void ) { }

static void Set_coff( void ) {
    Options.output_format = OFORMAT_COFF;
}
#if ELF_SUPPORT
static void Set_elf( void ) {
    Options.output_format = OFORMAT_ELF;
}
#endif
#if BIN_SUPPORT
static void Set_bin( void ) {
    Options.output_format = OFORMAT_BIN;
}
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

static void Set_zcm( void ) { Options.watcom_c_mangler = FALSE; }

static void Set_zcw( void ) { Options.watcom_c_mangler = TRUE; }

static void Set_Zi( void ) { }

static void Set_zlc( void ) { Options.no_comment_data_in_code_records = TRUE; }
static void Set_zld( void ) { Options.no_dependencies       = TRUE; }
static void Set_zlf( void ) { Options.no_file_entry         = TRUE; }
static void Set_zls( void ) { Options.no_section_aux_entry  = TRUE; }

static void Set_zze( void ) { Options.no_stdcall_export_decoration = TRUE; }
static void Set_zzo( void ) { Options.no_stdcall_decoration = TRUE; }
static void Set_zzp( void ) { Options.no_stdcall_suffix     = TRUE; }
static void Set_zzs( void ) { Options.entry_decorated       = TRUE; }

static void HelpUsage( void ) { usagex_msg();}

#ifdef DEBUG_OUT
static void Set_d6( void )
{
    Options.debug = TRUE;
    DebugMsg(( "debugging output on\n" ));
}
#endif

static struct option const cmdl_options[] = {
    { "0$",     0,        SetCPUCmdline },
    { "1$",     1,        SetCPUCmdline },
    { "2$",     2,        SetCPUCmdline },
    { "3$",     3,        SetCPUCmdline },
    { "4$",     4,        SetCPUCmdline },
    { "5$",     5,        SetCPUCmdline },
    { "6$",     6,        SetCPUCmdline },
    { "7",      7,        SetFPUCmdline },
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
    { "coff",   0,        Set_coff },
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
    { "FPi87",  '7',      SetFPUCmdline },
    { "FPi",    'i',      SetFPUCmdline },
    { "fp0",    0,        SetFPUCmdline },
    { "fp2",    2,        SetFPUCmdline },
    { "fp3",    3,        SetFPUCmdline },
    { "fp5",    5,        SetFPUCmdline },
    { "fp6",    6,        SetFPUCmdline },
    { "fpc",    'c',      SetFPUCmdline },
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
    { "zlc",    0,        Set_zlc },
    { "zld",    0,        Set_zld },
    { "zlf",    0,        Set_zlf },
    { "zls",    0,        Set_zls },
    { "zze",    0,        Set_zze },
    { "zzo",    0,        Set_zzo },
    { "zzp",    0,        Set_zzp },
    { "zzs",    0,        Set_zzs },
    { NULL,     0,        0 }
};

static int OptionDelimiter( char c )
/**********************************/
{
    if( c == ' ' || c == '-' || c == NULLC || c == '\t' || c == SwitchChar ) {
        return( 1 );
    }
    return( 0 );
}

#if BUILD_TARGET
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

void CloseFiles( void )
/**********************/
{
    /* close ASM file */
    if( FileInfo.file[ASM] != NULL ) {
        if( fclose( FileInfo.file[ASM] ) != 0 ) {
            FileInfo.file[ASM] = NULL;
            Fatal( MSG_CANNOT_CLOSE_FILE, FileInfo.fname[ASM] );
        }
    }
    LstCloseFile();

    if ( Options.output_format == OFORMAT_OMF )
        omf_fini();

    /* close OBJ file */
    if ( obj_fh != -1 ) {
        _close( obj_fh );
        obj_fh = -1;
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
        Fatal( MSG_CANNOT_OPEN_FILE, FileInfo.fname[ASM], errno );
    }

    /* open OBJ file */
    obj_fh = _open( FileInfo.fname[OBJ], OP_MODE, OP_PERM );
    if( obj_fh < 0 ) {
        DebugMsg(("open_files(): fopen(%s) failed\n", FileInfo.fname[OBJ] ));
        Fatal( MSG_CANNOT_OPEN_FILE, FileInfo.fname[OBJ], errno );
    }

    if ( Options.output_format == OFORMAT_OMF )
        omf_init( obj_fh );

    /* delete any existing ERR file */
    InitErrFile();
}

static char *CollectEnvOrFileName( char *str, char *buffer )
/********************************************/
{
    char        ch;

    while( *str == ' ' || *str == '\t' )
        ++str;
    for( ;; ) {
        ch = *str;
        if( ch == '\0' )
            break;
        ++str;
        if( ch == ' ' )
            break;
        if( ch == '\t' )
            break;
#if HANDLESWITCHCHAR
        if( ch == '-' )
            break;
        if( ch == SwitchChar )
            break;
#endif
        *buffer++ = ch;
    }
    *buffer = NULLC;
    return( str );
}

/*
 A '@' was found in the cmdline. It's not an environment variable,
 so check if it is a file and, if yes, read it.
 */

static char *ReadParamFile( char *name )
/***********************************/
{
    char        *env;
    char        *str;
    int         handle;
    int         len;
    char        ch;

    env = NULL;
#if OPENBINARY
    handle = _open( name, O_RDONLY | O_BINARY );
#else
    handle = _open( name, O_RDONLY );
#endif
    if( handle == -1 ) {
        Fatal( MSG_CANNOT_OPEN_FILE, name, errno );
        return( NULL );
    }
    len = _filelength( handle );
    env = MemAlloc( len + 1 );
    _read( handle, env, len );
    env[len] = NULLC;
    _close( handle );
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

/* process one option
 * - p: option
 */

static char *ProcessOption( char *p, char *option_start )
/*******************************************************/
{
    int         i;
    int         j;
    char        *opt;
    char        ident_value;
    char        c;

    numArgs++;
    for( i = 0; cmdl_options[i].option; i++ ) {
        opt = cmdl_options[i].option;
        if( *p == *opt ) {
            for (opt++, j = 1 ; isalnum(*opt) && *opt == p[j]; opt++, j++ );
            // make sure end of option is reached
            if ( isalnum(*opt))
                continue;
            OptValue = cmdl_options[i].value;
            for( ;; opt++) {
                switch ( *opt ) {
                //case '*': /* don't know what this is supposed to do? */
                case NULLC:
                    if ( !OptionDelimiter(p[j]) )
                        goto opt_exit;
                    OptScanPtr = p + j;
                    cmdl_options[i].function();
                    return( OptScanPtr );
                    break;
                case '#':             // collect a number
                    if( p[j] >= '0' && p[j] <= '9' ) {
                        OptValue = 0;
                        for( ;; ) {
                            c = p[j];
                            if( c < '0' || c > '9' )
                                break;
                            OptValue = OptValue * 10 + c - '0';
                            ++j;
                        }
                    }
                    break;
                case '$':      // collect an identifer+value
                    ident_value = FALSE;
                    OptParm = &p[j];
                    if ( p[j] != '"') {
                        for( ;; ) {
                            c = p[j];
                            if( c == NULLC || isspace(c))
                                break;
                            if( ident_value == FALSE && (c == '-' || c == SwitchChar))
                                break;
                            if( c == '=')
                                ident_value = TRUE;
                            ++j;
                        }
                        break;
                    } /* fall trough if name argument is enclosed in "" */
                case '@':      // collect a filename
                    OptParm = &p[j];
                    c = p[j];
                    if( c == '"' ) { // "filename"
                        for( ;; ) {
                            c = p[++j];
                            if( c == '"' ) {
                                ++j;
                                break;
                            }
                            if( c == NULLC )
                                break;
                            if( c == '\\' ) {
                                ++j;
                            }
                        }
                    } else {
                        for( ;; ) {
                            c = p[j];
                            if( c == NULLC || isspace(c) )
                                break;
#if HANDLESWITCHCHAR
                            if( c == SwitchChar )
                                break;
#endif
                            ++j;
                        }
                    }
                    break;
                case '=':    // collect an optional '='
                    if( p[j] == '=' || p[j] == '#' )
                        ++j;
                    break;
                case '^':    // get a (file)name, skip spaces
                    while (isspace(p[j])) j++;
                    break;
                default:
                    /* internal error: unknown format of option item! */
                    break;
                }
            }
        }
    }
opt_exit:
    while (isalnum(*p)) p++;
    *p = NULLC;
    AsmErr( INVALID_CMDLINE_OPTION, option_start );
    exit( 1 );
    return( NULL );
}

#define MAX_OS_NAME_SIZE 7

#if BUILD_TARGET
static int set_build_target( void )
/*********************************/
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
    return( NOT_ERROR );
}
#endif


static char * ProcessOptions( char *str, int *level )
/*********************************************/
{
    char *save[MAX_NESTING];
    char *buffers[MAX_NESTING];
    char paramfile[_MAX_PATH];

    for( ; str; ) {
        while( *str == ' ' || *str == '\t' )
            ++str;
        if( *str == '@' && *level < MAX_NESTING ) {
            save[(*level)++] = CollectEnvOrFileName( str + 1, paramfile );
            buffers[*level] = NULL;
            str = getenv( paramfile );
            if( str == NULL ) {
                str = ReadParamFile( paramfile );
                buffers[*level] = str;
            }
            if( str != NULL )
                continue;
            str = save[--(*level)];
        }
        if( *str == NULLC ) {
            if( *level == 0 )
                break;
            if( buffers[*level] != NULL ) {
                AsmFree( buffers[*level] );
                buffers[*level] = NULL;
            }
            str = save[--(*level)];
            continue;
        }
        if( *str == '-'  ||  *str == SwitchChar ) {
            str = ProcessOption(str+1, str);
        } else {  /* collect  file name */
            char *beg, *p;
            int len;

            beg = str;
            if( *str == '"' ) {
                for( ;; ) {
                    ++str;
                    if( *str == '"' ) {
                        ++str;
                        break;
                    }
                    if( *str == '\0' )
                        break;
                    if( *str == '\\' ) {
                        ++str;
                    }
                }
            } else {
                for( ;; ) {
                    if( *str == '\0' )
                        break;
                    if( *str == ' ' )
                        break;
                    if( *str == '\t' )
                        break;
#if HANDLESWITCHCHAR
                    if( *str == SwitchChar )
                        break;
#endif
                    ++str;
                }
            }
            len = str-beg;
            p = (char *) AsmAlloc( len + 1 );
            memcpy( p, beg, len );
            p[ len ] = '\0';
            StripQuotes( p );
            get_fname( p, ASM ); /* try to open .asm file */
            AsmFree(p);
            numArgs++;
            numFiles++;
            break;
        }
    }
    return( str );
}

static bool parse_cmdline( char **cmdline )
/*****************************************/
{
    int  level = 0;

    for( ; *cmdline != NULL && FileInfo.fname[ASM] == NULL; ++cmdline ) {
        *cmdline = ProcessOptions( *cmdline, &level );
    }
    if( FileInfo.fname[ASM] == NULL ) {
        return( FALSE );
    }
    return( TRUE );
}

/* called once for each source module */

static bool do_init_stuff( char **cmdline, bool parse )
/*****************************************/
{
#ifdef DEBUG_OUT
    ModuleInfo.cref = TRUE; /* enable debug displays */
#endif

    /* no debug displays possible before cmdline has been parsed! */
    if ( parse && parse_cmdline( cmdline ) == FALSE )
        return( FALSE );

    if (ParseInit() == ERROR)   // initialize hash table
        exit( -1 );             // tables wrong, internal error

    trademark();

    /* for OMF, IMAGEREL and SECTIONREL make no sense */
    /* this must be done AFTER ParseInit */
    if ( Options.output_format == OFORMAT_OMF ) {
        //DebugMsg(("do_init_stuff: disable IMAGEREL+SECTIONREL\n"));
#if IMAGERELSUPP
        DisableKeyword( T_IMAGEREL );
#endif
#if SECRELSUPP
        DisableKeyword( T_SECTIONREL );
#endif
    }

    if ( Options.strict_masm_compat == TRUE ) {
        DebugMsg(("do_init_stuff: disable INCBIN + WATCOM_C keywords\n"));
        DisableKeyword( T_INCBIN );
        DisableKeyword( T_WATCOM_C );
    }

#if BUILD_TARGET
    set_build_target();
    /* search for <os>_INCLUDE and add it to the include search path */
    get_os_include();
#endif

    return( TRUE );
}

void genfailure(int signo)
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
    for( i = 0; i < NUM_FILE_TYPES; i++ ) {
        FileInfo.file[i] = NULL;
        FileInfo.fname[i] = NULL;
    }
    ModuleInfo.srcfile = 0;
}

static void main_fini( void )
/***************************/
{
    free_names();
    CloseFiles();
}

#if USECRTARGC

int main( int argc, char **argv )
/*******************************/
{

#else

#include "win32.h"

int main( void )
/**************/
{
    char       *argv[3];
#endif
#if USECRTARGC==0
    char       *pCmd;
#endif
    char       *pEnv;
#if WILDCARDS
    long        fh; /* _findfirst/next/close() handle, must be long! */
    static struct _finddata_t finfo;
    static char drv[_MAX_DRIVE];
    static char dir[_MAX_DIR];
#endif

    pEnv = getenv( "JWASM" );
    if ( pEnv == NULL )
        pEnv = "";
    argv[0] = pEnv;
#ifndef __UNIX__
    SwitchChar = '/';
#endif
#if USECRTARGC==0
//    SwitchChar = _dos_switch_char();
    pCmd = (char *)GetCommandLineA();
    /* skip the JWasm program name */
    if (*pCmd == '"') {
        pCmd++;
        while ( *pCmd != NULLC && *pCmd != '"' )
            pCmd++;
        if ( *pCmd == '"' )
            pCmd++;
    } else {
        while ( *pCmd && !isspace(*pCmd) ) pCmd++;
    }
    argv[1] = pCmd;
    argv[2] = NULL;
#endif

#ifndef DEBUG_OUT
    signal(SIGSEGV, genfailure);
#endif

#if CATCHBREAK
    signal(SIGBREAK, genfailure);
#else
    signal(SIGTERM, genfailure);
#endif

    MsgInit();

    while ( 1 ) {
        main_init();
        if ( do_init_stuff( argv, TRUE ) == 0 )
            break;
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
            do_init_stuff( argv, FALSE );
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

