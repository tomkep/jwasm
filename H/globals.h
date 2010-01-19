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
* Description:  JWasm globals and limits.
*
****************************************************************************/


#ifndef _GLOBALS_H_INCLUDED
#define _GLOBALS_H_INCLUDED

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <time.h>
#include <errno.h>
#include "inttype.h"
#include "bool.h"

#if defined(__UNIX__) || defined(__CYGWIN__) || defined(__DJGPP__) /* avoid for MinGW! */
#include "watcomc.h"  /* for POSIX based C runtimes */
#elif defined(__POCC__)
#define _MAX_PATH FILENAME_MAX
#define _MAX_FNAME FILENAME_MAX
#define _MAX_EXT FILENAME_MAX
#define _MAX_DIR FILENAME_MAX
#define _MAX_DRIVE 4
#pragma warn(disable:2030) /* disable '=' used in a conditional expression */
#elif defined(__BORLANDC__)
#define _stricmp  stricmp
#define _strnicmp strnicmp
#define _strupr   strupr
#define _memicmp  memicmp
#endif

#define MAX_LINE_LEN            600     // no restriction for this number
#define MAX_TOKEN               MAX_LINE_LEN / 4  // max tokens in one line
#define MAX_STRING_LEN          MAX_LINE_LEN - 32 // must be < MAX_LINE_LEN
#define MAX_ID_LEN              247
#define MAX_STRUCT_ALIGN        32
#define MAX_RESW_LEN            20 /* max size of reserved words */

#define MAX_IF_NESTING          20 /* IFxx block nesting         */
#define MAX_TEXTMACRO_NESTING   20
#define MAX_SEG_NESTING         20 /* limit for segment nesting  */
#define MAX_QUEUE_NESTING       40 /* "async" macro call nesting */
#define MAX_SYNC_MACRO_NESTING  20 /* "sync" macro call nesting  */
#define MAX_STRUCT_NESTING      32 /* limit for "anonymous structs" only */

#define MAX_LEDATA              (1024 - 6) /* OMF */
#define MAX_LEDATA_THRESHOLD    1024 - 12 /* OMF: - 6 for header, -6 for fixups     */
#define MAX_PUB_LENGTH          1024 /* OMF: max length of pubdef record */
#define MAX_EXT_LENGTH          1024 /* OMF: max length ( in chars ) of extdef */

#ifndef AMD64_SUPPORT
#define AMD64_SUPPORT 1 /* 1=support 64bit */
#endif

#ifndef COFF_SUPPORT
#define COFF_SUPPORT 1 /* support COFF output format             */
#endif
#ifndef ELF_SUPPORT
#define ELF_SUPPORT  1 /* support ELF output format              */
#endif
#define BIN_SUPPORT  1 /* support BIN output format              */
#define MZ_SUPPORT   1 /* support DOS MZ output format           */
#define IMAGERELSUPP 1 /* support IMAGEREL operator (not for OMF) */
#define SECTIONRELSUPP 1 /* support SECTIONREL operator (not for OMF) */
#define K3DSUPP      1 /* support K3D instruction set            */
#define SSE3SUPP     1 /* support SSE3 instruction set           */
#define SSSE3SUPP    1 /* support SSSE3 instruction set          */
#ifndef SSE4SUPP
#define SSE4SUPP     1 /* support SSE4 instruction set           */
#endif
#define FIELDALIGN   1 /* support OPTION FIELDALIGN:<const>      */
#define PROCALIGN    1 /* support OPTION PROCALIGN:<const>       */
#define LOHI32       0 /* support LOW32/HIGH32 operators         */
#define XMMWORD      1 /* support MMWORD and XMMWORD types       */
#define RENAMEKEY    1 /* support OPTION RENAMEKEYWORD:<old>,new */
#define MACROLABEL   1 /* support LABEL qualifier for macro arg  */
#define BUILD_TARGET 0 /* support "build target" (obsolete)      */
#define INVOKE_WC    0 /* support watcom_c for INVOKE (not impl) */

#ifndef FASTPASS
#define FASTPASS     1 /* don't scan full source if pass > 1     */
#endif

#ifndef FASTMEM
#define FASTMEM      1 /* fast memory allocation */
#endif

/* JWasm extensions */
#define BACKQUOTES   1 /* allow IDs enclosed in `             */
#define FPIMMEDIATE  1 /* allow float immediates: mov eax,1.0 */
#define INCLUDEBIN   1 /* support INCBIN directive            */
#define COCTALS      0 /* allow C form of octals              */
#define CHEXPREFIX   0 /* accept "0x" as hex number prefix    */
#define MANGLERSUPP  0 /* support Wasm's "mangler" extension  */

/* JWasm version info */

#define _BETA_
#define _JWASM_VERSION_ "2.02" _BETA_
#define _JWASM_VERSION_INT_ 201

#include "errmsg.h"

#ifndef PATH_MAX
#define PATH_MAX 259
#endif

#define NULLC  '\0'
//#define NULLS  ""

//#define is_valid_id_char( ch )  ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )
#define is_valid_id_char( ch )  ( isalnum(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

#define is_valid_id_first_char( ch )  ( isalpha(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' || (ch == '.' && ModuleInfo.dotname == TRUE ))

extern struct asm_sym WordSize;
#define CurrWordSize WordSize.value

/* function return values */

typedef enum {
 EMPTY = -2,
 ERROR = -1,
 NOT_ERROR = 0,
 STRING_EXPANDED = 1
} ret_code;

enum {
    PASS_1 = 0,
    PASS_2
};

enum {
    ASM,
    ERR,
    OBJ,
    LST
};

#define NUM_FILE_TYPES 4

typedef struct {
    FILE        *file[NUM_FILE_TYPES];      // ASM, ERR, OBJ and LST
    char        *fname[NUM_FILE_TYPES];
} File_Info;

// Information about source, object, listing and error files
extern File_Info        FileInfo;

#define ASM_EXT "asm"
#define ERR_EXT "err"
#define LST_EXT "lst"

#ifdef __UNIX__
#define OBJ_EXT "o"
#else
#define OBJ_EXT "obj"
#endif

// enumerations

enum oformat {
    OFORMAT_OMF,
    OFORMAT_COFF,
#if ELF_SUPPORT
    OFORMAT_ELF,
#endif
    OFORMAT_BIN /* this is also for MZ and PE formats */
};

enum hformat {
    HFORMAT_NONE,
    HFORMAT_MZ,
    HFORMAT_PE,  /* not supported yet */
#if AMD64_SUPPORT
    HFORMAT_WIN64, /* COFF for 64bit */
    HFORMAT_ELF64  /* ELF for 64bit  */
#endif
};

enum fpo {
    FPO_NO_EMULATION,  /* -FPi87 (default) */
    FPO_EMULATION      /* -FPi */
};

/* language vaules.
 * the order cannot be changed, it's
 * returned by OPATTR and used in user-defined prologue/epilogue.
 */
typedef enum {
    LANG_NONE     = 0,
    LANG_C        = 1,
    LANG_SYSCALL  = 2,
    LANG_STDCALL  = 3,
    LANG_PASCAL   = 4,
    LANG_FORTRAN  = 5,
    LANG_BASIC    = 6,
    LANG_FASTCALL = 7
} lang_type;

/* Memory model type.
 * the order cannot be changed, it's
 * the value of the predefined @Model symbol.
 */
typedef enum {
    MOD_NONE    = 0,
    MOD_TINY    = 1,
    MOD_SMALL   = 2,
    MOD_COMPACT = 3,
    MOD_MEDIUM  = 4,
    MOD_LARGE   = 5,
    MOD_HUGE    = 6,
    MOD_FLAT    = 7,
} mod_type;

typedef enum {
    SEGORDER_SEQ = 0,  /* .SEQ (default) */
    SEGORDER_DOSSEG,   /* .DOSSEG */
    SEGORDER_ALPHA     /* .ALPHA */
} seg_order;

/* .NOLISTMACRO, .LISTMACRO and .LISTMACROALL directives setting */
enum listmacro {
    LM_NOLISTMACRO,
    LM_LISTMACRO,
    LM_LISTMACROALL
};

/* assume values are used as index in codegen.c! */
enum assume_segreg {
    ASSUME_NOTHING = EMPTY,
    ASSUME_ES = 0,
    ASSUME_CS,
    ASSUME_SS,
    ASSUME_DS,
    ASSUME_FS,
    ASSUME_GS
};

enum asm_cpu {
        /* bit count from left:
           bit 0-2:   Math coprocessor
           bit 3:     priviledged?
           bit 4-6:   cpu type
           bit 7-11;  extension set */

        P_NO87  = 0x0001,         /* no FPU */
        P_87    = 0x0002,         /* 8087 */
        P_287   = 0x0003,         /* 80287 */
        P_387   = 0x0004,         /* 80387 */

        P_PM    = 0x0008,         /* privileged opcode */

        P_86    = 0x0000,         /* 8086, default */
        P_186   = 0x0010,         /* 80186 */
        P_286   = 0x0020,         /* 80286 */
        P_386   = 0x0030,         /* 80386 */
        P_486   = 0x0040,         /* 80486 */
        P_586   = 0x0050,         /* pentium */
        P_686   = 0x0060,         /* ppro */
#if AMD64_SUPPORT
        P_64    = 0x0070,         /* x64 cpu */
#endif

        P_286p  = P_286 | P_PM,   /* 286, priv mode */
        P_386p  = P_386 | P_PM,   /* 386, priv mode */
        P_486p  = P_486 | P_PM,   /* 486, priv mode */
        P_586p  = P_586 | P_PM,   /* 586, priv mode */
        P_686p  = P_686 | P_PM,   /* 686, priv mode */
#if AMD64_SUPPORT
        P_64p   = P_64 | P_PM,    /* x64, priv mode */
#endif

        P_MMX   = 0x0100,         /* MMX extension instructions */
#if K3DSUPP
        P_K3D   = 0x0200,         /* 3DNow extension instructions */
#endif
        P_SSE1  = 0x0400,         /* SSE1 extension instructions */
        P_SSE2  = 0x0800,         /* SSE2 extension instructions */
        P_SSE3  = 0x1000,         /* SSE3 extension instructions */
#if SSSE3SUPP
        P_SSSE3 = 0x2000,         /* SSSE3 extension instructions */
#if SSE4SUPP
        P_SSE4  = 0x4000,         /* SSE4 extension instructions */
#endif
#endif
        /* all SSE extension instructions */
#if SSSE3SUPP
 #if SSE4SUPP
        P_SSEALL = P_SSE1 | P_SSE2 | P_SSE3 | P_SSSE3 | P_SSE4,
 #else
        P_SSEALL = P_SSE1 | P_SSE2 | P_SSE3 | P_SSSE3,
 #endif
#else
        P_SSEALL = P_SSE1 | P_SSE2 | P_SSE3,
#endif
        NO_OPPRFX = P_MMX | P_SSEALL,

        P_FPU_MASK = 0x0007,
        P_CPU_MASK = 0x00F0,

#if K3DSUPP
        P_EXT_MASK = P_MMX | P_K3D | P_SSEALL,
        P_EXT_ALL  = P_MMX | P_K3D | P_SSEALL
#else
        P_EXT_MASK = P_MMX | P_SSEALL,
        P_EXT_ALL  = P_MMX | P_SSEALL
#endif
};

/* the MASM compatible @CPU value flags: */
enum M_CPU {
    M_8086 = 0x0001, /* 8086 */
    M_186  = 0x0002, /* 186 */
    M_286  = 0x0004, /* 286 */
    M_386  = 0x0008, /* 386 */
    M_486  = 0x0010, /* 486 */
    M_586  = 0x0020, /* Pentium */
    M_686  = 0x0040, /* PPro */
    M_PROT = 0x0080, /* protected instructions ok */
    M_8087 = 0x0100, /* 8087 */
    M_287  = 0x0400, /* 287 */
    M_387  = 0x0800  /* 387 */
};

#if MANGLERSUPP
enum naming_types {
    NC_DO_NOTHING,
    /*  put uscores on the front of labels & the back of procedures.
     this is what the OW compiler does with /3r
     */
    NC_ADD_USCORES,
    /* assume that the user manually put uscores as described above
     into the assembly file and take them off
     */
    NC_REMOVE_USCORES
};
#endif

enum asm_token {
#define  res(token, string, len, rm_byte, op2, opcode, flags, cpu, prefix) T_ ## token ,
#define  ins(token, string, len, op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix ) T_ ## token ,
#define insx(token, string, len, op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix,flgs ) T_ ## token ,
#define insn(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#define insm(tok,suffix,op1,byte1_info,op2,op3,op_dir,rm_info,opcode,rm_byte,cpu,prefix)
#include "special.h"
#include "instruct.h"
#undef insm
#undef insn
#undef insx
#undef ins
#undef res
    T_NULL
};

enum segofssize {
    USE_EMPTY = 0xFE,
    USE16 = 0,
    USE32 = 1,
    USE64 = 2
};

/* fastcall types. if order is to be changed or entries
 * added, also adjust tables in proc.c, mangle.c and probably invoke.c!
 */
enum fastcall_type {
    FCT_MS32,       // MS 32bit fastcall (ecx,edx)
    FCT_WATCOMC,    // OW register calling convention (eax, ebx, ecx, edx)
    FCT_WIN64       // Win64 fastcall convention (rcx, rdx, r8, r9)
};
enum stdcall_decoration {
    STDCALL_FULL,
    STDCALL_NONE,
    STDCALL_HALF
};

struct qitem {
    void * next;
    char value[];
};

typedef struct global_options {
    bool        quiet;                 /* -q option */
    bool        line_numbers;          /* -Zd option */
    bool        debug_symbols;         /* -Zi option */
    enum fpo    floating_point;        /* -FPi, -FPi87 */

    /* error handling stuff */
    int         error_limit;             /* -e option  */
    uint_8      warning_level;           /* -Wn option */
    bool        warning_error;           /* -WX option */
#ifdef DEBUG_OUT
    bool        debug;                   /* -d6 option */
    bool        print_linestore;         /* -ls option */
    uint_16     max_passes;              /* -pm option */
#endif
#if BUILD_TARGET
    char        *build_target;           /* -bt option */
#endif
    char        *code_class;             /* -nc option */
    char        *data_seg;               /* -nd option */
    char        *text_seg;               /* -nt option */
    char        *module_name;            /* -nm option */
    char        *ForceInclude;           /* -FI option */

    struct qitem *SymQueue;              /* list of symbols set by -D */
    struct qitem *IncQueue;              /* list of include paths set by -I */

#if COCTALS
    bool        allow_c_octals;          /* -o option */
#endif
    bool        no_comment_data_in_code_records; /* -zlc option */
//    bool        no_dependencies;         /* -zld option */
    bool        no_file_entry;           /* -zlf option */
    bool        no_section_aux_entry;    /* -zls option  */
    bool        no_cdecl_decoration;     /* -zcw & -zcm option */
    uint_8      stdcall_decoration;      /* -zt<0|1|2> option */
    bool        no_export_decoration;    /* -zze option */
    bool        entry_decorated;         /* -zzs option  */
    bool        write_listing;           /* -Fl option  */
    bool        case_sensitive;          /* -Cp,-Cx,-Cu options */
    bool        convert_uppercase;       /* -Cp,-Cx,-Cu options */
    bool        preprocessor_stdout;     /* -EP option  */
    bool        masm51_compat;           /* -Zm option  */
    bool        strict_masm_compat;      /* -Zne option  */
    bool        masm_compat_gencode;     /* -Zg option  */
    bool        listif;                  /* -Sx, -Sa option  */
    bool        list_generated_code;     /* -Sg, -Sa option  */
    enum listmacro list_macro;           /* -Sa option  */
    bool        no_symbol_listing;       /* -Sn option  */
    bool        all_symbols_public;      /* -Zf option  */
    bool        safeseh;                 /* -safeseh option */
    uint_8      ignore_include;          /* -X option */
    enum oformat output_format;          /* -omf, -coff, -elf options */
    enum hformat header_format;          /* -mz, -pe options */
    uint_8      fieldalign;              /* -Zp option  */
    lang_type   langtype;                /* -Gc|d|z option */
    mod_type    model;                   /* -mt|s|m|c|l|h|f option */
    enum asm_cpu cpu;                    /* -0|1|2|3|4|5|6 & -fp{0|2|3|5|6|c} option */
    unsigned char fastcall;              /* -zf0 & -zf1 option */
    bool        syntax_check_only;       /* -Zs option */
#if MANGLERSUPP
    char        *default_name_mangler;   /* OW peculiarity */
    enum naming_types naming_convention; /* OW naming peculiarities */
#endif
} global_options;

extern global_options Options;

typedef struct global_vars{
    uint    sel_idx;              /* used for OMF comment record */
    uint_32 sel_start;            /* used for OMF comment record */
    unsigned int code_seg:1;      /* is current segment CODE? */
    unsigned int data_in_code:1;  /* have we just written data to a code seg */
} global_vars;

extern global_vars GlobalVars;

typedef struct asm_tok ASM_TOK;

typedef struct  fname_list {
        char    *name;
        char    *fullname;
        time_t  mtime;
} FNAME;

#if MZ_SUPPORT
/* if the structure changes, option.c, SetMZ() might need adjustment! */
struct MZDATA {
    uint_16 ofs_fixups; /* offset start fixups */
    uint_16 alignment; /* header alignment: 16,32,64,128,256,512 */
    uint_16 heapmin;
    uint_16 heapmax;
};
#endif

/* global variables */
extern struct asm_tok   *AsmBuffer[];   // token buffer
extern unsigned int     Parse_Pass;     // assembly pass
// v1.96: Opnd_Count removed
//extern unsigned int     Opnd_Count;     // operand count of current instr
extern int              Token_Count;    // number of tokens in current line
extern bool             Modend;         // end of module is reached
extern unsigned int     GeneratedCode;  // nesting level generated code
extern uint_8           MacroLevel;     // macro nesting level

extern bool             PhaseError;     // phase error occured during pass
extern bool             write_to_file;  // 1=write the object module

// functions in assemble.c

extern void             OutputByte( unsigned char );
extern void             OutputBytes( unsigned char *, int len );
extern void             OutputBytesAndFixup( struct asmfixup *, unsigned char *, int len );
extern void             FillDataBytes( unsigned char, int len );
extern void             OutputCodeByte( unsigned char );
extern void             OutSelect( bool );
extern void             AssembleModule( void );
extern void             AddLinnumDataRef( unsigned );
extern void             RunLineQueue( void );
extern void             RunLineQueueEx( void );
extern void             WritePreprocessedLine( const char * );
extern void             SetMasm510( bool );
extern void             close_files( void );

typedef struct line_num_info {
    struct line_num_info *next;
    uint_16 number;
    union {
        uint_32 offset;
        /* the next struct is set if sym is != NULL */
        struct {
            uint_32 line_number:20,
                    file:12;
        };
    };
    union {
        uint srcfile;
        struct asm_sym * sym; /* used if number is 0 */
    };
} line_num_info;

#endif
