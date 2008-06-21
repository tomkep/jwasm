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


#include "globals.h"
#include <fcntl.h>
#ifdef __WATCOMC__
  #include <unistd.h>
  #include <process.h>
#endif
#include <ctype.h>
#include <signal.h>

#include "memalloc.h"
#include "banner.h"
#include "parser.h"
#include "fatal.h"
#include "equate.h"
#include "omfprs.h"
#include "omfgenms.h"
#include "directiv.h"
#include "input.h"
#include "errmsg.h"
#include "macro.h"

#ifdef __OSI__
  #include "ostype.h"
#endif

#define BUILD_TARGET 0

extern void             Fatal( unsigned msg, ... );
extern void             ObjRecInit( void );
extern void             InitErrFile( void );
//extern void             PrintfUsage( int first_ln );
extern void             MsgPrintf1( int resourceid, char *token );

extern const char       *FingerMsg[];

File_Info               AsmFiles;       // files information
pobj_state              pobjState;      // object file information

struct  option {
    char        *option;
    unsigned    value;
    void        (*function)( void );
};


static struct SWData {
    bool register_conventions;
    bool privileged_mode;
    int cpu;
    int fpu;
} SWData = {
    FALSE, // register conventions
    FALSE, // privileged mode CPU instructions set
    0,     // default CPU (=8086)
    -1     // unspecified FPU
};

#define MAX_NESTING 15
#define BUF_SIZE 512

static char             ParamBuf[ BUF_SIZE ];
static unsigned char    SwitchChar;
static unsigned         OptValue;
static char             *OptScanPtr;
static char             *OptParm;
static char             *ForceInclude = NULL;
char                    banner_printed = FALSE;

global_options Options = {
    /* sign_value       */          FALSE,
    /* quiet            */          FALSE,
    /* line_numbers     */          FALSE,
    /* naming_convention*/          DO_NOTHING,
    /* floating_point   */          NO_FP_EMULATION,

    /* error_limit      */          50,
    /* warning_level    */          2,
    /* warning_error    */          FALSE,
#ifdef DEBUG_OUT
    /* debug            */          FALSE,
#endif
    /* build_target     */          NULL,

    /* code_class       */          NULL,
    /* data_seg         */          NULL,
    /* text_seg         */          NULL,
    /* module_name      */          NULL,

    /* default_name_mangler  */     NULL,
    /* allow_c_octals        */     FALSE,
    /* no_comment_data_in_code_records */   FALSE,
    /* no_dependencies       */     FALSE,
    /* no_file_entry         */     FALSE,
    /* no_section_aux_entry  */     FALSE,
    /* Watcom C name mangler */     FALSE,
    /* no_stdcall_decoration */     FALSE,
    /* no_stdcall_suffix     */     FALSE,
    /* entry_decorated       */     FALSE,
    /* write_listing         */     FALSE,
    /* nocasemap             */     FALSE,
    /* preprocessor_stdout   */     FALSE,
    /* masm51_compat         */     FALSE,
    /* no_symbol_listing     */     FALSE,
    /* list_generated_code   */     TRUE,
    /* output_format         */     OFORMAT_OMF
};

struct qitem {
    void * next;
    char * value;
};

struct qitem * SymQueue = NULL;
struct qitem * IncQueue = NULL;

static char usage[] = {
"\nusage: JWasm [options] asm-file [options] [asm-file] ... [@env_var]\n"
"Run \"JWasm /?\" or \"JWasm /h\" for more info\n"
};

static char usagex[] = {
#include "usage.h"
};

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

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

static void SetCPUPMC( void )
/***************************/
{
    char                *tmp;

    for( tmp=OptParm; tmp < OptScanPtr; tmp++ ) {
        if( *tmp == 'p' ) {
            if( SWData.cpu >= 2 ) {         // set privileged mode
                SWData.privileged_mode = TRUE;
            } else {
                MsgPrintf1( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
#if 0
        } else if( *tmp == 'r' ) {
            if( SWData.cpu >= 3 ) {  // set register calling convention
                SWData.register_conventions = TRUE;
            } else {
                MsgPrintf1( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
        } else if( *tmp == 's' ) {
            if( SWData.cpu >= 3 ) {  // set stack calling convention
                SWData.register_conventions = FALSE;
            } else {
                MsgPrintf1( MSG_CPU_OPTION_INVALID, CopyOfParm() );
            }
#endif
        } else if( *tmp == '"' ) {                             // set default mangler
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
        } else {
            MsgPrintf1( MSG_UNKNOWN_OPTION, CopyOfParm() );
            exit( 1 );
        }
    }
#if 0
    if( SWData.cpu < 2 ) {
        SWData.privileged_mode = FALSE;
        SWData.register_conventions = TRUE;
    } else if( SWData.cpu < 3 ) {
        SWData.register_conventions = TRUE;
    }
#endif
}

static void SetCPU( void )
/************************/
{
    SWData.cpu = OptValue;
    SetCPUPMC();
}

static void SetFPU( void )
/************************/
{
    switch( OptValue ) {
    case 'i':
        Options.floating_point = DO_FP_EMULATION;
        break;
    case '7':
        Options.floating_point = NO_FP_EMULATION;
        break;
    case 'c':
        Options.floating_point = NO_FP_ALLOWED;
        break;
    case 0:
    case 2:
    case 3:
    case 4:
    case 5:
    case 6:
    case 7:
        SWData.fpu = OptValue;
        break;
    }
}

static char memory_model = 0;

static void SetMM( void )
/***********************/
{
    char buffer[20];

    switch( OptValue ) {
    case 'c':
    case 'f':
    case 'h':
    case 'l':
    case 'm':
    case 's':
    case 't':
        break;
    default:
        strcpy( buffer, "/m" );
        strcat( buffer, (char *)&OptValue );
        MsgPrintf1( MSG_UNKNOWN_OPTION, buffer );
        exit( 1 );
    }

    memory_model = OptValue;
}

static void SetMemoryModel( void )
/********************************/
{
    char buffer[20];
    char *model;

    switch( memory_model ) {
    case 'c':
        model = "COMPACT";
        break;
    case 'f':
        model = "FLAT";
        break;
    case 'h':
        model = "HUGE";
        break;
    case 'l':
        model = "LARGE";
        break;
    case 'm':
        model = "MEDIUM";
        break;
    case 's':
        model = "SMALL";
        break;
    case 't':
        model = "TINY";
        break;
    default:
        return;
    }

    strcpy( buffer, ".MODEL " );
    strcat( buffer, model );
    InputQueueLine( buffer );
}

static int is_valid_identifier( char *id )
/*********************************/
{
    if( isdigit( *id ) )
        return( ERROR ); /* can't start with a number */
    if (*id == '.') id++; /* allow name to start with a '.' */
    for( ; *id != '\0'; id++ ) {
        if (is_valid_id_char(*id) == 0)
            return( ERROR );
    }
    return( NOT_ERROR );
}

/* define a numeric constant with value 1.
 */

#if 0
static struct asm_sym * add_constant( char *string )
/**************************************/
{
    asm_sym *sym;

    DebugMsg(("add_constant(%s) enter\n", string));

    if( is_valid_identifier( string ) == ERROR ) {
        AsmError( SYNTAX_ERROR );
        return(NULL);
    }
    if (sym = CreateConstant( string, 1, -1, FALSE))
        sym->predefined = TRUE;

    return(sym);
}
#endif

/* queue a text macro.
   this is called for cmdline option -D
 */

static void queue_item( struct qitem * *start, char *string )
/**************************************/
{
    struct qitem *p;
    struct qitem *q;

    DebugMsg(("queue_item(%s) enter\n", string));
    p = MemAlloc(sizeof(struct qitem));
    p->value = MemAlloc(strlen(string)+1);
    strcpy(p->value, string);
    p->next = NULL;
    q = *start;
    if (q) {
        for (;q->next;q = q->next);
        q->next = p;
    } else
        *start = p;
    return;
}

static void add_predefined_tmacros(  )
/**************************************/
{
    struct qitem *p;
    char * name;
    char * value;
    int len;
    struct asm_sym *sym;

    DebugMsg(("add_predefined_tmacros enter\n"));
    for (p = SymQueue;p;p = p->next) {
        DebugMsg(("add_predefined_tmacros: found >%s<\n", p->value));
        name = p->value;
        value = strchr( name, '=' );
        if( value == NULL ) {
            value = "";
        } else {
            len = value - name;
            name = AsmTmpAlloc(len + 1);
            memcpy(name, p->value, len);
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

static void add_incpaths( )
/**************************************/
{
    struct qitem *p;
    DebugMsg(("add_incpaths enter\n"));
    for (p = IncQueue;p;p = p->next) {
        AddStringToIncludePath(p->value);
    }
}

static void get_fname( char *token, int type )
/********************************************/
/*
 * figure out the source file name & store it in AsmFiles
 * fill in default object file name if it is null
 */
{
    char        name [ _MAX_PATH  ];
    char        msgbuf[80];
    char        *pExt;
    char        drive[_MAX_DRIVE];
    char        dir[_MAX_DIR];
    char        fname[_MAX_FNAME];
    char        ext[_MAX_EXT];
    char        drive2[_MAX_DRIVE];
    char        dir2[_MAX_DIR];
    char        fname2[_MAX_FNAME];
    char        ext2[_MAX_EXT];

    /* get filename for source file */

    if( type == ASM ) {
        if( token == NULL ) {
            MsgGet( SOURCE_FILE, msgbuf );
            Fatal( MSG_CANNOT_OPEN_FILE, msgbuf );
        }
        _splitpath( token, drive, dir, fname, ext );
        if( ext[0] == '\0' ) {
            strcpy(ext, ASM_EXT);
        }
        _makepath( name, drive, dir, fname, ext );
        AsmFiles.fname[ASM] = MemAlloc( strlen( name ) + 1 );
        strcpy( AsmFiles.fname[ASM], name );

        _makepath( name, drive, dir, NULL, NULL );
        /* add the source path to the include path */
        AddStringToIncludePath( name );

        /* set up default object and error filename */

        if( AsmFiles.fname[OBJ] == NULL ) {
            _makepath( name, NULL, NULL, fname, OBJ_EXT );
        } else {
            _splitpath( AsmFiles.fname[OBJ], drive2,
                         dir2, fname2, ext2 );
            if( fname2[0] == NULLC )
                strcpy(fname2, fname);
            if( ext2[0] == NULLC )
                strcpy(ext2, OBJ_EXT);

            _makepath( name, drive2, dir2, fname2, ext2 );
            MemFree( AsmFiles.fname[OBJ] );
        }
        AsmFiles.fname[OBJ] = MemAlloc( strlen( name ) + 1 );
        strcpy( AsmFiles.fname[OBJ], name );

        if( AsmFiles.fname[ERR] == NULL ) {
            _makepath( name, NULL, NULL, fname, ERR_EXT );
        } else {
            _splitpath( AsmFiles.fname[ERR], drive2,
                         dir2, fname2, ext2 );
            if( fname2[0] == NULLC )
                strcpy(fname2, fname);
            if( ext2[0] == NULLC )
                strcpy(ext2, ERR_EXT);

            _makepath( name, drive2, dir2, fname2, ext2 );
            MemFree( AsmFiles.fname[ERR] );
        }
        AsmFiles.fname[ERR] = MemAlloc( strlen( name ) + 1 );
        strcpy( AsmFiles.fname[ERR], name );

        if( AsmFiles.fname[LST] == NULL ) {
            _makepath( name, NULL, NULL, fname, LST_EXT );
        } else {
            _splitpath( AsmFiles.fname[LST], drive2,
                         dir2, fname2, ext2 );
            if( fname2[0] == NULLC )
                strcpy(fname2, fname);
            if( ext2[0] == NULLC )
                strcpy(ext2, LST_EXT);
            _makepath( name, drive2, dir2, fname2, ext2 );
            MemFree( AsmFiles.fname[LST] );
        }
        AsmFiles.fname[LST] = MemAlloc( strlen( name ) + 1 );
        strcpy( AsmFiles.fname[LST], name );

    } else {
        /* get filename for object, error, or listing file */
        _splitpath( token, drive, dir, fname, ext );
        if( AsmFiles.fname[ASM] != NULL ) {
            _splitpath( AsmFiles.fname[ASM], drive2,
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
        if( AsmFiles.fname[type] != NULL ) {
            MemFree( AsmFiles.fname[type] );
        }
        AsmFiles.fname[type] = MemAlloc( strlen( name ) + 1 );
        strcpy( AsmFiles.fname[type], name );
    }
}

static void set_some_kinda_name( char token, char *name )
/*******************************************************/
/* set:  code class / data seg. / module name / text seg */
{
    int len;
    char **tmp;

    len = strlen( name ) + 1;
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
        AsmFree(*tmp);
    }
    *tmp = AsmAlloc( len );
    strcpy( *tmp, name );
}

static void usage_msg( void )
/***************************/
{
    //    PrintfUsage( MSG_USE_BASE );
    trademark();
    printf("%s", usage);
    exit(1);
}

static void usagex_msg( void )
/***************************/
{
    printf("%s", usagex);
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

static void Set_CP( void ) { Options.nocasemap = TRUE; }

static void Set_ZD( void ) { Options.line_numbers = TRUE; }

static void Set_ZM( void ) { Options.masm51_compat = TRUE; }

static void DefineMacro( void ) { queue_item( &SymQueue, CopyOfParm() ); }

static void SetErrorLimit( void ) { Options.error_limit = OptValue; }

static void Set_EP( void ) { Options.preprocessor_stdout = TRUE; }

static void Set_FR( void ) { get_fname( GetAFileName(), ERR ); }

static void Set_FI( void ) { ForceInclude = GetAFileName(); }

static void Set_FL( void ) { get_fname( GetAFileName(), LST ); Options.write_listing = TRUE;}

static void Set_FO( void ) { get_fname( GetAFileName(), OBJ ); }

static void SetInclude( void ) { queue_item( &IncQueue, GetAFileName() ); }

static void Set_J( void ) { Options.sign_value = TRUE; }

static void Set_SG( void ) { }

static void Set_SN( void ) { Options.no_symbol_listing = TRUE; }

static void Set_N( void ) { set_some_kinda_name( OptValue, CopyOfParm() ); }

static void Set_O( void ) { Options.allow_c_octals = TRUE; }

static void Set_OMF( void ) { Options.output_format = OFORMAT_OMF;}

static void Set_WX( void ) { Options.warning_error = TRUE; }

static void Set_w( void ) { Set_WX(); Options.warning_level = 0; }

static void SetWarningLevel( void ) { Options.warning_level = OptValue; }

static void Set_ZCM( void ) { Options.watcom_c_mangler = FALSE; }

static void Set_ZCW( void ) { Options.watcom_c_mangler = TRUE; }

static void Set_ZLC( void ) { Options.no_comment_data_in_code_records = TRUE; }
static void Set_ZLD( void ) { Options.no_dependencies       = TRUE; }
static void Set_ZLF( void ) { Options.no_file_entry         = TRUE; }
static void Set_ZLS( void ) { Options.no_section_aux_entry  = TRUE; }

static void Set_NOLOGO( void ) { Options.quiet = 1; }
static void Set_Q( void )      { Options.quiet = 2; }

static void Set_ZZO( void ) { Options.no_stdcall_decoration = TRUE; }
static void Set_ZZP( void ) { Options.no_stdcall_suffix     = TRUE; }
static void Set_ZZS( void ) { Options.entry_decorated       = TRUE; }

static void HelpUsage( void ) { usagex_msg();}

#ifdef DEBUG_OUT
static void Set_D6( void )
{
    Options.debug = TRUE;
    DebugMsg(( "debugging output on \n" ));
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
#if BUILD_TARGET
    { "bt=$",   0,        Set_BT },
#endif
    { "c",      0,        Set_C },
    { "coff",   0,        Set_COFF },
    { "Cp",     0,        Set_CP },
#ifdef DEBUG_OUT
    { "d6",     6,        Set_D6 },
#endif
    { "d+",     0,        Ignore },
    { "D$",     0,        DefineMacro },
    { "e=#",    0,        SetErrorLimit },
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
    { "h",      0,        HelpUsage },
    { "hc",     'c',      Ignore },
    { "hd",     'd',      Ignore },
    { "hw",     'w',      Ignore },
    { "I=^@",   0,        SetInclude },
    { "j",      0,        Set_J },
    { "mc",     'c',      SetMM },
    { "mf",     'f',      SetMM },
    { "mh",     'h',      SetMM },
    { "ml",     'l',      SetMM },
    { "mm",     'm',      SetMM },
    { "ms",     's',      SetMM },
    { "mt",     't',      SetMM },
    { "nc=$",   'c',      Set_N },
    { "nd=$",   'd',      Set_N },
    { "nm=$",   'm',      Set_N },
    { "nt=$",   't',      Set_N },
    { "nologo", 0,        Set_NOLOGO },
    { "o",      0,        Set_O },
    { "omf",    0,        Set_OMF },
    { "q",      0,        Set_Q },
    { "Sg",     0,        Set_SG },
    { "Sn",     0,        Set_SN },
    { "u",      0,        Ignore },
    { "w",      0,        Set_w },
    { "WX",     0,        Set_WX },
    { "W=#",    0,        SetWarningLevel },
    { "Zd",     0,        Set_ZD },
    { "zcm",    0,        Set_ZCM },
    { "zcw",    0,        Set_ZCW },
    { "zlc",    0,        Set_ZLC },
    { "zld",    0,        Set_ZLD },
    { "zlf",    0,        Set_ZLF },
    { "zls",    0,        Set_ZLS },
    { "Zm",     0,        Set_ZM },
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

int trademark( void )
/*******************/
{
    int         count = 0;

    if( !Options.quiet ) {
        while( FingerMsg[count] != NULL ) {
            printf( "%s\n", FingerMsg[count++] );
        }
    }
    return( count );
}

static void free_names( void )
/****************************/
/* Free names set as cmdline options */
{
#if BUILD_TARGET
    if( Options.build_target != NULL ) {
        AsmFree( Options.build_target );
    }
#endif
    if( Options.code_class != NULL ) {
        AsmFree( Options.code_class );
    }
    if( Options.data_seg != NULL ) {
        AsmFree( Options.data_seg );
    }
    if( Options.module_name != NULL ) {
        AsmFree( Options.module_name );
    }
    if( Options.text_seg != NULL ) {
        AsmFree( Options.text_seg );
    }
#if BUILD_TARGET
    Options.build_target = NULL;
#endif
    Options.code_class = NULL;
    Options.data_seg = NULL;
    Options.module_name = NULL;
    Options.text_seg = NULL;
}

static void main_init( void )
/***************************/
{
    int         i;

    MemInit();
    for( i = 0; i <= FILE_TYPES; i++ ) {
        AsmFiles.file[i] = NULL;
        AsmFiles.fname[i] = NULL;
    }
    ObjRecInit();
    omf_GenMSInit();
}

static void main_fini( void )
/***************************/
{
    free_names();
    omf_GenMSFini();
    AsmShutDown();
}

static void open_files( void )
/****************************/
{
    /* open ASM file */
//    AsmFiles.file[ASM] = fopen( AsmFiles.fname[ASM], "r" );
    AsmFiles.file[ASM] = fopen( AsmFiles.fname[ASM], "rb" );

    if( AsmFiles.file[ASM] == NULL ) {
        Fatal( MSG_CANNOT_OPEN_FILE, AsmFiles.fname[ASM] );
    }

    /* open OBJ file */
    pobjState.file_out = ObjWriteOpen( AsmFiles.fname[OBJ] );
    if( pobjState.file_out == NULL ) {
        Fatal( MSG_CANNOT_OPEN_FILE, AsmFiles.fname[OBJ] );
    }
    pobjState.pass = POBJ_WRITE_PASS;

    /* delete any existing ERR file */
    InitErrFile();
}

static char *CollectEnvOrFileName( char *str )
/********************************************/
{
    char        *env;
    char        ch;

    while( *str == ' ' || *str == '\t' )
        ++str;
    env = ParamBuf;
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
        *env++ = ch;
    }
    *env = '\0';
    return( str );
}

static char *ReadIndirectFile( void )
/***********************************/
{
    char        *env;
    char        *str;
    int         handle;
    int         len;
    char        ch;

    env = NULL;
    handle = open( ParamBuf, O_RDONLY | O_BINARY );
    if( handle != -1 ) {
        len = filelength( handle );
        env = AsmAlloc( len + 1 );
        read( handle, env, len );
        env[len] = '\0';
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

static char * ProcessOptions( char *str, int *level )
/*********************************************/
{
    char *save[MAX_NESTING];
    char *buffers[MAX_NESTING];

    if( str != NULL ) {
        for( ;; ) {
            while( *str == ' ' || *str == '\t' )
                ++str;
            if( *str == '@' && *level < MAX_NESTING ) {
                save[(*level)++] = CollectEnvOrFileName( str + 1 );
                buffers[*level] = NULL;
                str = getenv( ParamBuf );
                if( str == NULL ) {
                    str = ReadIndirectFile();
                    buffers[*level] = str;
                }
                if( str != NULL )
                    continue;
                str = save[--(*level)];
            }
            if( *str == '\0' ) {
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
                get_fname( p, ASM );
                AsmFree(p);
                break;
            }
        }
    }
    return( str );
}

/* get cmdline options from environment variable JWASM */

static void do_envvar_cmdline( char *envvar )
/*******************************************/
{
    char *cmdline;
    int  level = 0;

    cmdline = getenv( envvar );
    if( cmdline != NULL ) {
        ProcessOptions( cmdline, &level );
    }
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

static void parse_cmdline( char **cmdline )
/*****************************************/
{
    char msgbuf[80];
    int  level = 0;

    if( cmdline == NULL || *cmdline == NULL || **cmdline == 0 ) {
        usage_msg();
    }
    for( ;*cmdline != NULL; ++cmdline ) {
        *cmdline = ProcessOptions( *cmdline, &level );
    }
    if( AsmFiles.fname[ASM] == NULL ) {
        MsgGet( NO_FILENAME_SPECIFIED, msgbuf );
        Fatal( MSG_CANNOT_OPEN_FILE, msgbuf );
    }
}

static void do_init_stuff( char **cmdline, bool first )
/*****************************************/
{
    char        *env;

    if (first) {
        MsgInit();
        ParseInit( -1, -1, -1, -1 );       // initialize hash table
    }

    InputInit();
#if 0
    ForceInclude = getenv( "FORCE" );
#endif
    do_envvar_cmdline( "JWASM" );
    parse_cmdline( cmdline );
#if BUILD_TARGET
    set_build_target();
    /* search for <os>_INCLUDE and add it to the include search path */
    get_os_include();
#endif

    /* search for INCLUDE and add it to the include search path */
    env = getenv( "INCLUDE" );
    if( env != NULL )
        AddStringToIncludePath( env );

    if (first) {
        if( !Options.quiet && !banner_printed ) {
            banner_printed = TRUE;
            trademark();
        }
    }

    open_files();
    PushLineQueue();

}

void genfailure(int signo)
{
    if (signo != SIGBREAK)
        AsmError(GENERAL_FAILURE);
    AsmShutDown();
    exit (1);
}

#ifdef __UNIX__

int main( int argc, char **argv )
/*******************************/
{
    argc = argc;
#ifndef __WATCOMC__
    _argv = argv;
#endif

#else

char * _stdcall GetCommandLineA(void);

int main( void )
/**************/
{
    char       *argv[2];
    int        len;
    bool       first = TRUE;
    char       *buff;
    char       *pCmd;
#endif

    main_init();
//    SwitchChar = _dos_switch_char();
    SwitchChar = '/';
    pCmd = (char *)GetCommandLineA();
    if (*pCmd == '"') {
        pCmd++;
        while (*pCmd != NULLC && *pCmd != '"')
            pCmd++;
        if (*pCmd == '"')
            pCmd++;
    } else {
        while (!isspace(*pCmd)) pCmd++;
    }
    while (isspace(*pCmd)) pCmd++;

    len = strlen(pCmd) +1;
    buff = malloc( len );
    if( buff != NULL ) {
        argv[0] = buff;
        argv[1] = NULL;
        strcpy( buff, pCmd );
    } else {
        return( -1 );
    }
#ifndef DEBUG_OUT
    signal(SIGSEGV, genfailure);
#endif
    signal(SIGBREAK, genfailure);

    while (1) {
        do_init_stuff( argv , first);
        SetMemoryModel();

        WriteObjModule();           // main body: parse the source file
        MsgFini();
        main_fini();
//        if (ModuleInfo.error_count > 0)
//            break;

        pCmd = argv[0];
        while (isspace(*pCmd)) pCmd++;
        if (*pCmd == 0)
            break;
        first = FALSE;
        main_init();
    };
#ifndef __UNIX__
    free( buff );
#endif
    return( ModuleInfo.error_count != 0); /* zero if no errors */
}

void set_cpu_parameters( void )
/*****************************/
{
    int token;

    DebugMsg(("set_cpu_parameters enter\n"));
#if 0
    // set naming convention
    if( SWData.register_conventions || ( SWData.cpu < 3 ) ) {
        Options.naming_convention = ADD_USCORES;
    } else {
        Options.naming_convention = DO_NOTHING;
    }
    // set parameters passing convention
    if( SWData.cpu >= 3 ) {
        if( SWData.register_conventions ) {
            add_constant( "__REGISTER__" );
        } else {
            add_constant( "__STACK__" );
        }
    }
#endif
    switch( SWData.cpu ) {
    case 0:
        token = T_DOT_8086;
        break;
    case 1:
        token = T_DOT_186;
        break;
    case 2:
        token =  SWData.privileged_mode ? T_DOT_286P : T_DOT_286;
        break;
    case 3:
        token =  SWData.privileged_mode ? T_DOT_386P : T_DOT_386;
        break;
    case 4:
        token =  SWData.privileged_mode ? T_DOT_486P : T_DOT_486;
        break;
    case 5:
        token =  SWData.privileged_mode ? T_DOT_586P : T_DOT_586;
        break;
    case 6:
        token =  SWData.privileged_mode ? T_DOT_686P : T_DOT_686;
        break;
    }
    cpu_directive( token );
}

void set_fpu_parameters( void )
/*****************************/
{
    switch( Options.floating_point ) {
    case DO_FP_EMULATION:
//        add_constant( "__FPI__" );
        break;
    case NO_FP_EMULATION:
//        add_constant( "__FPI87__" );
        break;
    case NO_FP_ALLOWED:
//        add_constant( "__FPC__" );
        cpu_directive( T_DOT_NO87 );
        return;
    }
    switch( SWData.fpu ) {
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
    case 7:
    default: // unspecified FPU
        switch( SWData.cpu ) {
        case 0:
        case 1:
            cpu_directive( T_DOT_8087 );
            break;
        case 2:
            cpu_directive( T_DOT_287 );
            break;
        case 3:
        case 4:
        case 5:
        case 6:
            cpu_directive( T_DOT_387 );
            break;
        }
        break;
    }
}

// called by write.c
// this is called for every pass.
// symbol table and ModuleInfo are initialized.

void CmdlParamsInit( int pass )
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

    if( ForceInclude != NULL )
        InputQueueFile( ForceInclude );

    if (pass == PASS_1) {
        set_cpu_parameters();
        set_fpu_parameters();
        add_predefined_tmacros();
        add_incpaths();
    }
    DebugMsg(("CmdlParamsInit exit\n"));

}
