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
#include "globals.h"
#include <fcntl.h>
#ifdef __WATCOMC__
  #include <unistd.h>
  #include <process.h>
#else
  #include <io.h>
#endif
#include <ctype.h>
#include <signal.h>

#include "memalloc.h"
#include "parser.h"
#include "directiv.h"
#include "fatal.h"
#include "equate.h"
#include "omfprs.h"
#include "omfgenms.h"
#include "input.h"
#include "errmsg.h"
#include "macro.h"
#include "listing.h"
#include "msgtext.h"

#ifdef __OSI__
  #include "ostype.h"
#endif

#if !defined(__UNIX__)
#define WILDCARDS 1
#else
#define WILDCARDS 0
#endif

extern char     banner_printed;

File_Info       FileInfo;       // files information
pobj_state      pobjState;      // object file information
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
    /* no_stdcall_suffix     */     FALSE,
    /* entry_decorated       */     FALSE,
    /* write_listing         */     FALSE,
    /* case_sensitive        */     FALSE,
    /* convert_uppercase     */     FALSE,
    /* preprocessor_stdout   */     FALSE,
    /* masm51_compat         */     FALSE,
    /* strict_masm_compat    */     FALSE,

    /* listif                */     FALSE,
    /* list_generated_code   */     FALSE,
    /* no_symbol_listing     */     FALSE,

    /* all_symbols_public    */     FALSE,
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

#if !defined(__UNIX__)
#ifdef __WATCOMC__

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
    ParamBuf[ len ] = '\0';
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

static void SetCPU( void )
/************************/
{
    char  *tmp;

    Options.cpu = OptValue;

    for( tmp = OptParm; tmp < OptScanPtr; tmp++ ) {
        if( *tmp == 'p' ) {
            if( Options.cpu >= 2 ) {         // set privileged mode
                Options.privileged_mode = TRUE;
            } else {
                MsgPrintf1( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
#if OWREGCONV
        } else if( *tmp == 'r' ) {
            if( Options.cpu >= 3 ) {  // set register calling convention
                Options.register_conventions = TRUE;
            } else {
                MsgPrintf1( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
        } else if( *tmp == 's' ) {
            if( Options.cpu >= 3 ) {  // set stack calling convention
                Options.register_conventions = FALSE;
            } else {
                MsgPrintf1( MSG_CPU_OPTION_INVALID, CopyOfParm() );
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
            MsgPrintf1( MSG_UNKNOWN_OPTION, CopyOfParm() );
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

static void SetFPU( void )
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

static void GetDefaultFnames( char *name )
{
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    char        path[ _MAX_PATH  ];

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
        MemFree( FileInfo.fname[OBJ] );
    }
    FileInfo.fname[OBJ] = MemAlloc( strlen( path ) + 1 );
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
        MemFree( FileInfo.fname[ERR] );
    }
    FileInfo.fname[ERR] = MemAlloc( strlen( path ) + 1 );
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
        MemFree( FileInfo.fname[LST] );
    }
    FileInfo.fname[LST] = MemAlloc( strlen( path ) + 1 );
    strcpy( FileInfo.fname[LST], path );
    return;
}


static void get_fname( char *token, int type )
/********************************************/
/*
 * called for .OBJ, .ERR, .LST and .ASM filenames.
 * for ASM, figure out the source file name & store it in FileInfo
 * and fill in default object file name if it is null.
 * AddStringToIncludePath() is called for ASM files.
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
    char        msgbuf[MAXMSGSIZE];

    /* get filename for source file */

    if( type == ASM ) {
        if( token == NULL ) {
            MsgGet( SOURCE_FILE, msgbuf );
            Fatal( MSG_CANNOT_OPEN_FILE, msgbuf );
        }
        _splitpath( token, drive, dir, fname, ext );

        _makepath( name, drive, dir, NULL, NULL );
        /* add the source path to the include path */
        AddStringToIncludePath( name );

        _makepath( name, drive, dir, fname, ext );
        FileInfo.fname[ASM] = MemAlloc( strlen( name ) + 1 );
        strcpy( FileInfo.fname[ASM], name );

        /* set up default OBF, LST and ERR filenames */

    } else {
        /* get filename for object, error, or listing file */
        _splitpath( token, drive, dir, fname, ext );
        if( FileInfo.fname[ASM] != NULL ) {
            _splitpath( FileInfo.fname[ASM], drive2,
                         dir2, fname2, ext2 );
            if( fname[0] == NULLC ) {
                strcpy(fname, fname2);
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
            MemFree( FileInfo.fname[type] );
        }
        FileInfo.fname[type] = MemAlloc( strlen( name ) + 1 );
        strcpy( FileInfo.fname[type], name );
    }
}

static void set_some_kinda_name( char token, char *name )
/*******************************************************/
/* set:  code class / data seg. / module name / text seg */
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

static void Ignore( void ) {};

#if BUILD_TARGET
static void Set_BT( void ) { SetTargName( OptParm,  OptScanPtr - OptParm ); }
#endif

static void Set_C( void ) { }

static void Set_COFF( void ) {
    Options.output_format = OFORMAT_COFF;
}
#if ELF_SUPPORT
static void Set_ELF( void ) {
    Options.output_format = OFORMAT_ELF;
}
#endif
#if BIN_SUPPORT
static void Set_BIN( void ) {
    Options.output_format = OFORMAT_BIN;
}
#endif
static void Set_Cp( void ) { Options.case_sensitive = TRUE;   Options.convert_uppercase = FALSE; }
static void Set_Cu( void ) { Options.case_sensitive = FALSE;  Options.convert_uppercase = TRUE;  }
static void Set_Cx( void ) { Options.case_sensitive = FALSE;  Options.convert_uppercase = FALSE; }

static void Set_ZD( void ) { Options.line_numbers = TRUE; }

static void Set_ZM( void ) { Options.masm51_compat = TRUE; }

static void Set_ZNE( void ) { Options.strict_masm_compat = TRUE; }

static void Set_ZF( void ) { Options.all_symbols_public = TRUE; }

static void Set_ZP( void ) {
    uint_8 power;
    for (power = 1;power < OptValue && power < MAX_STRUCT_ALIGN; power = power << 1);
    if (power == OptValue)
        Options.alignment_default = OptValue;
    else {
        AsmWarn(1, INVALID_CMDLINE_VALUE, "/Zp");
    }
}

static void DefineMacro( void ) { queue_item( &Options.SymQueue, CopyOfParm() ); }

static void SetErrorLimit( void ) { Options.error_limit = OptValue; }

static void Set_NOLOGO( void ) { banner_printed = TRUE; }
static void Set_Q( void )      { Set_NOLOGO(); Options.quiet = TRUE; }
static void Set_EP( void ) { Options.preprocessor_stdout = TRUE; Set_Q(); }

static void Set_FR( void ) { get_fname( GetAFileName(), ERR ); }

static void Set_FI( void ) { Options.ForceInclude = GetAFileName(); }

static void Set_FL( void ) { get_fname( GetAFileName(), LST ); Options.write_listing = TRUE;}

static void Set_FO( void ) { get_fname( GetAFileName(), OBJ ); }

static void Set_Gc( void ) { Options.langtype = LANG_PASCAL; }
static void Set_Gd( void ) { Options.langtype = LANG_C; }
static void Set_Gz( void ) { Options.langtype = LANG_STDCALL; }

static void SetInclude( void ) { queue_item( &Options.IncQueue, GetAFileName() ); }

static void Set_J( void ) { Options.sign_value = TRUE; }

static void Set_Sa( void ) { Options.listif = TRUE; Options.list_generated_code = TRUE; }
static void Set_Sg( void ) { Options.list_generated_code = TRUE; }
static void Set_Sn( void ) { Options.no_symbol_listing = TRUE; }
static void Set_Sx( void ) { Options.listif = TRUE; }

static void Set_Mt( void ) { Options.model = MOD_TINY; }
static void Set_Ms( void ) { Options.model = MOD_SMALL; }
static void Set_Mm( void ) { Options.model = MOD_MEDIUM; }
static void Set_Mc( void ) { Options.model = MOD_COMPACT; }
static void Set_Ml( void ) { Options.model = MOD_LARGE; }
static void Set_Mh( void ) { Options.model = MOD_HUGE; }
static void Set_Mf( void ) { Options.model = MOD_FLAT; }

static void Set_N( void ) { set_some_kinda_name( OptValue, CopyOfParm() ); }

#if COCTALS
static void Set_O( void ) { Options.allow_c_octals = TRUE; }
#endif
#ifdef DEBUG_OUT
static void Set_PM( void ) { Options.max_passes = OptValue; };
#endif
static void Set_OMF( void ) { Options.output_format = OFORMAT_OMF;}

static void Set_WX( void ) { Options.warning_error = TRUE; }

static void Set_w( void ) { Set_WX(); Options.warning_level = 0; }

static void SetWarningLevel( void ) { Options.warning_level = OptValue; }

static void Set_ZCM( void ) { Options.watcom_c_mangler = FALSE; }

static void Set_ZCW( void ) { Options.watcom_c_mangler = TRUE; }

static void Set_ZI( void ) { }

static void Set_ZLC( void ) { Options.no_comment_data_in_code_records = TRUE; }
static void Set_ZLD( void ) { Options.no_dependencies       = TRUE; }
static void Set_ZLF( void ) { Options.no_file_entry         = TRUE; }
static void Set_ZLS( void ) { Options.no_section_aux_entry  = TRUE; }

static void Set_ZZO( void ) { Options.no_stdcall_decoration = TRUE; }
static void Set_ZZP( void ) { Options.no_stdcall_suffix     = TRUE; }
static void Set_ZZS( void ) { Options.entry_decorated       = TRUE; }

static void HelpUsage( void ) { usagex_msg();}

#ifdef DEBUG_OUT
static void Set_D6( void )
{
    Options.debug = TRUE;
    DebugMsg(( "debugging output on\n" ));
}
#endif

static struct option const cmdl_options[] = {
    { "0$",     0,        SetCPU },
    { "1$",     1,        SetCPU },
    { "2$",     2,        SetCPU },
    { "3$",     3,        SetCPU },
    { "4$",     4,        SetCPU },
    { "5$",     5,        SetCPU },
    { "6$",     6,        SetCPU },
    { "7",      7,        SetFPU },
    { "?",      0,        HelpUsage },
#if BIN_SUPPORT
    { "bin",    0,        Set_BIN },
#endif
#if BUILD_TARGET
    { "bt=$",   0,        Set_BT },
#endif
    { "c",      0,        Set_C },
    { "coff",   0,        Set_COFF },
    { "Cp",     0,        Set_Cp },
    { "Cu",     0,        Set_Cu },
    { "Cx",     0,        Set_Cx },
#ifdef DEBUG_OUT
    { "d6",     6,        Set_D6 },
#endif
    { "d+",     0,        Ignore },
    { "D$",     0,        DefineMacro },
    { "e=#",    0,        SetErrorLimit },
#if ELF_SUPPORT
    { "elf",    0,        Set_ELF },
#endif
    { "EP",     0,        Set_EP },
    { "Fi=^@",  0,        Set_FI },
    { "Fl=@",   0,        Set_FL },
    { "Fo=^@",  0,        Set_FO },
    { "fp0",    0,        SetFPU },
    { "fp2",    2,        SetFPU },
    { "fp3",    3,        SetFPU },
    { "fp5",    5,        SetFPU },
    { "fp6",    6,        SetFPU },
    { "fpc",    'c',      SetFPU },
    { "FPi87",  '7',      SetFPU },
    { "FPi",    'i',      SetFPU },
    { "Fr=^@",  0,        Set_FR },
    { "Gc",     0,        Set_Gc },
    { "Gd",     0,        Set_Gd },
    { "Gz",     0,        Set_Gz },
    { "h",      0,        HelpUsage },
    { "hc",     'c',      Ignore },
    { "hd",     'd',      Ignore },
    { "hw",     'w',      Ignore },
    { "I=^@",   0,        SetInclude },
    { "j",      0,        Set_J },
    { "mc",     0,        Set_Mc },
    { "mf",     0,        Set_Mf },
    { "mh",     0,        Set_Mh },
    { "ml",     0,        Set_Ml },
    { "mm",     0,        Set_Mm },
    { "ms",     0,        Set_Ms },
    { "mt",     0,        Set_Mt },
    { "nc=$",   'c',      Set_N },
    { "nd=$",   'd',      Set_N },
    { "nm=$",   'm',      Set_N },
    { "nt=$",   't',      Set_N },
    { "nologo", 0,        Set_NOLOGO },
#if COCTALS
    { "o",      0,        Set_O },
#endif
    { "omf",    0,        Set_OMF },
#ifdef DEBUG_OUT
    { "pm=#",   0,        Set_PM },
#endif
    { "q",      0,        Set_Q },
    { "Sa",     0,        Set_Sa },
    { "Sg",     0,        Set_Sg },
    { "Sn",     0,        Set_Sn },
    { "Sx",     0,        Set_Sx },
    { "u",      0,        Ignore },
    { "w",      0,        Set_w },
    { "WX",     0,        Set_WX },
    { "W=#",    0,        SetWarningLevel },
    { "zcm",    0,        Set_ZCM },
    { "zcw",    0,        Set_ZCW },
    { "Zd",     0,        Set_ZD },
    { "Zf",     0,        Set_ZF },
    { "Zi",     0,        Set_ZI },
    { "zlc",    0,        Set_ZLC },
    { "zld",    0,        Set_ZLD },
    { "zlf",    0,        Set_ZLF },
    { "zls",    0,        Set_ZLS },
    { "Zm",     0,        Set_ZM },
    { "Zne",    0,        Set_ZNE },
    { "Zp=#",   0,        Set_ZP },
    { "zzo",    0,        Set_ZZO },
    { "zzp",    0,        Set_ZZP },
    { "zzs",    0,        Set_ZZS },
    { 0,        0,        0 }
};

static int OptionDelimiter( char c )
/**********************************/
{
    if( c == ' ' || c == '-' || c == '\0' || c == '\t' || c == SwitchChar ) {
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

    /* close OBJ file */
    if ( pobjState.file_out != NULL )
        ObjWriteClose( pobjState.file_out );

    OmfRecFini();
    if( ModuleInfo.error_count > 0 ) {
        remove( FileInfo.fname[OBJ] );
    }
    MemFree( FileInfo.fname[ASM] );
    MemFree( FileInfo.fname[ERR] );
    MemFree( FileInfo.fname[LST] );
    MemFree( FileInfo.fname[OBJ] );
    MemFini();
}

static void open_files( void )
/****************************/
{
    /* open ASM file */
//    FileInfo.file[ASM] = fopen( FileInfo.fname[ASM], "r" );
    FileInfo.file[ASM] = fopen( FileInfo.fname[ASM], "rb" );

    if( FileInfo.file[ASM] == NULL ) {
        Fatal( MSG_CANNOT_OPEN_FILE, FileInfo.fname[ASM] );
    }

    /* open OBJ file */
    pobjState.file_out = ObjWriteOpen( FileInfo.fname[OBJ] );
    if( pobjState.file_out == NULL ) {
        Fatal( MSG_CANNOT_OPEN_FILE, FileInfo.fname[OBJ] );
    }
    pobjState.pass = POBJ_WRITE_PASS;

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
#if !defined(__UNIX__)
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
    handle = open( name, O_RDONLY | O_BINARY );
    if( handle == -1 ) {
        Fatal( MSG_CANNOT_OPEN_FILE, name );
        return( NULL );
    }
    len = filelength( handle );
    env = MemAlloc( len + 1 );
    read( handle, env, len );
    env[len] = NULLC;
    close( handle );
    // zip through characters changing \r, \n etc into ' '
    str = env;
    while( *str ) {
        ch = *str;
        if( ch == '\r' || ch == '\n' ) {
            *str = ' ';
        }
#if !defined(__UNIX__)
        if( ch == 0x1A ) {      // if end of file
            *str = '\0';        // - mark end of str
            break;
        }
#endif
        ++str;
    }
    return( env );
}

// process one option

static char *ProcessOption( char *p, char *option_start )
/*******************************************************/
{
    int         i;
    int         j;
    char        *opt;
    char        ident_value;
    char        c;

    numArgs++;
    for( i = 0; ; i++ ) {
        opt = cmdl_options[i].option;
        if( opt == NULL )
            break;
//        c = tolower( *p );
        c = *p;
        if( c == *opt ) {
            OptValue = cmdl_options[i].value;
            j = 1;
            for( ;; ) {
                ++opt;
                if( *opt == '\0' || *opt == '*' ) {
                    if( *opt == '\0' ) {
                        if( p - option_start == 1 ) {
                            // make sure end of option
                            if( !OptionDelimiter( p[j] ) ) {
                                break;
                            }
                        }
                    }
                    OptScanPtr = p + j;
                    cmdl_options[i].function();
                    return( OptScanPtr );
                }
                if( *opt == '#' ) {             // collect a number
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
                } else if( *opt == '$' ) {      // collect an identifer
                    ident_value = FALSE;
                    OptParm = &p[j];
                    for( ;; ) {
                        c = p[j];
                        if( c == '\0' || isspace(c))
                            break;
                        if( ident_value == FALSE && (c == '-' || c == SwitchChar))
                            break;
                        if( c == '=')
                            ident_value = TRUE;
                        ++j;
                    }
                } else if( *opt == '@' ) {      // collect a filename
                    OptParm = &p[j];
                    c = p[j];
                    if( c == '"' ) { // "filename"
                        for( ;; ) {
                            c = p[++j];
                            if( c == '"' ) {
                                ++j;
                                break;
                            }
                            if( c == '\0' )
                                break;
                            if( c == '\\' ) {
                                ++j;
                            }
                        }
                    } else {
                        for( ;; ) {
                            c = p[j];
                            if( c == '\0' )
                                break;
                            if( c == ' ' )
                                break;
                            if( c == '\t' )
                                break;
#if !defined(__UNIX__)
                            if( c == SwitchChar )
                                break;
#endif
                            ++j;
                        }
                    }
                } else if( *opt == '=' ) {      // collect an optional '='
                    if( p[j] == '=' || p[j] == '#' ) ++j;
                } else if( *opt == '^' ) {      // get a filename, skip spaces
                    while (isspace(p[j])) j++;
                } else {
//                    c = tolower( p[j] );
                    c = p[j];
                    if( *opt != c ) {
                        if( *opt < 'A' || *opt > 'Z' )
                            break;
                        if( *opt != p[j] ) {
                            break;
                        }
                    }
                    ++j;
                }
            }
        }
    }
    MsgPrintf1( MSG_UNKNOWN_OPTION, option_start );
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
#if !defined(__UNIX__)
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

static int parse_cmdline( char **cmdline )
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

static int do_init_stuff( char **cmdline, bool parse )
/*****************************************/
{
#ifdef DEBUG_OUT
    ModuleInfo.cref = TRUE; /* enable debug displays */
#endif

    /* InputInit() resets the list of include paths! This must be done
     before parse_cmdline() is called! Otherwise one risks a GPF if more
     than one file is to be assembled.
     */
    InputInit();

    /* no debug displays possible before cmdline has been parsed! */
    if ( parse && parse_cmdline( cmdline ) == 0 )
        return( FALSE );

    if (ParseInit() == ERROR)   // initialize hash table
        exit( -1 );             // tables wrong, internal error

    trademark();

    /* for OMF, IMAGEREL and SECTIONREL make no sense */
    /* this must be done AFTER ParseInit */
    if ( Options.output_format == OFORMAT_OMF ) {
#if IMAGERELSUPP
        DisableKeyword( T_IMAGEREL );
#endif
#if SECRELSUPP
        DisableKeyword( T_SECTIONREL );
#endif
    }

    if ( Options.strict_masm_compat == TRUE ) {
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
#ifdef __UNIX__
    if (signo != SIGTERM)
#else
    if (signo != SIGBREAK)
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
    ModuleInfo.srcfile = NULL; /* remove any old value, to be sure */
}

static void main_fini( void )
/***************************/
{
    free_names();
    CloseFiles();
}

#ifdef __UNIX__

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
    char       *pCmd;
    char       *pEnv;
#if WILDCARDS
    int        fh;
    struct     _finddata_t finfo;
    char       drv[_MAX_DRIVE];
    char       dir[_MAX_DIR];
#endif

    pEnv = getenv( "JWASM" );
    if ( pEnv == NULL )
        pEnv = "";
    argv[0] = pEnv;
#ifndef __UNIX__
//    SwitchChar = _dos_switch_char();
    SwitchChar = '/';
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
#ifndef __UNIX__
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
        finfo.name[0] = NULLC;
        while ( 1 ) {
            if ( finfo.name[0] == NULLC ) {
                if ((fh = _findfirst( FileInfo.fname[ASM], &finfo )) == -1 ) {
                    AsmErr( CANNOT_OPEN_FILE, FileInfo.fname[ASM] );
                    break;
                }
                _splitpath( FileInfo.fname[ASM], drv, dir, NULL, NULL );
                MemFree( FileInfo.fname[ASM]);
            } else {
                main_init();
                do_init_stuff( argv, FALSE );
                if ( _findnext( fh, &finfo ) == -1 ) {
                    _findclose( fh );
                    break;
                }
            }
            FileInfo.fname[ASM] = MemAlloc( strlen(drv) + strlen(dir) + strlen(finfo.name) + 3 );
            _makepath( FileInfo.fname[ASM], drv, dir, finfo.name, NULL );
#endif
            GetDefaultFnames( FileInfo.fname[ASM] );
            open_files();
            OmfRecInit();
            omf_GenMSInit();
            AssembleModule();     // main body: parse the source file
            omf_GenMSFini();
            main_fini();
#if WILDCARDS
        }
#endif
    };

    if ( numArgs == 0 ) {
        MsgPrintf( MSG_USAGE );
    } else if ( numFiles == 0 )
        AsmError( NO_FILENAME_SPECIFIED );

    MsgFini();
    return( ModuleInfo.error_count != 0); /* zero if no errors */
}

