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
#include "watcom.h"
#include "bool.h"

/* max_ledata_threshold = 1024 - 6 for the header, -6 for space for fixups */

#define MAX_LINE_LEN            512     // there is no restriction for this number
#define MAX_TOKEN               MAX_LINE_LEN / 4  // there is no restriction for this number
#define MAX_STRING_LEN          MAX_LINE_LEN - 32 // must be < MAX_LINE_LEN
#define MAX_ID_LEN              247
#define MAX_LEDATA_THRESHOLD    1024 - 12
#define MAX_PUB_SIZE            100     // max # of entries in pubdef record
#define MAX_EXT_LENGTH          1024    // max length ( in chars ) of extdef
#define MAX_STRUCT_ALIGN        32

#define COFF_SUPPORT 1 /* support COFF output format             */
#define ELF_SUPPORT  1 /* support ELF output format              */
#define BIN_SUPPORT  1 /* support BIN output format              */
#define IMAGERELSUPP 1 /* support IMAGEREL operator (COFF+ELF)   */
#define SECRELSUPP   1 /* support SECTIONREL operator (COFF+ELF) */
#define SSSE3SUPP    1 /* support SSSE3 instruction set          */
#define BUILD_TARGET 0 /* support "build target" (obsolete)      */


#define FASTMEM      1 /* fast memory allocation */
#define OWREGCONV    0 /* support 'r' and 's' suffix for cpu */
#define COCTALS      0 /* allow C form of octals */
#define CHEXPREFIX   0 /* accept "0x" as hex number prefix */

#include "errmsg.h"

#ifndef PATH_MAX
#define PATH_MAX 259
#endif

#define NULLC                   '\0'
#define NULLS                   "\0"

#define BIT_012                 0x07
#define BIT_345                 0x38
#define BIT_67                  0xC0
#define NOT_BIT_012             0xF8
#define NOT_BIT_345             0xC7
#define NOT_BIT_67              0x3F

/* function return values */

typedef enum {
 EMPTY = -2,
 ERROR = -1,
 NOT_ERROR = 0,
 STRING_EXPANDED = 1,
 SCRAP_INSTRUCTION = 2, /* used by jmp() */
 INDIRECT_JUMP = 3      /* used by jmp() */
} ret_code;

enum naming_conventions {
    DO_NOTHING,
    ADD_USCORES,            /*  put uscores on the front of labels
                             *  & the back of procedures
                             *  this is what the OW compiler does with /3r
                             */
    REMOVE_USCORES,         /*
                             * assume that the user manually put uscores
                             * as described above into the assembly file
                             * and take them off
                             */
};

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
} File_Info;    // Information about the source and object files

extern File_Info        AsmFiles;   // files information



#define ASM_EXT "asm"
#define ERR_EXT "err"
#define LST_EXT "lst"

#ifdef __UNIX__
#define OBJ_EXT "o"
#else
#define OBJ_EXT "obj"
#endif

enum oformat {
    OFORMAT_OMF,
    OFORMAT_COFF,
    OFORMAT_ELF,
    OFORMAT_BIN
};

enum fpe {
    DO_FP_EMULATION,
    NO_FP_EMULATION,
    NO_FP_ALLOWED
};

typedef enum {
    LANG_NONE     = 0,
    LANG_C        = 1,
    LANG_SYSCALL  = 2,
    LANG_STDCALL  = 3,
    LANG_PASCAL   = 4,
    LANG_FORTRAN  = 5,
    LANG_BASIC    = 6,
    LANG_WATCOM_C = 7
} lang_type;

/* Paul Edwards
   Note that there is code that is dependent on the ordering
   of these model types. */
typedef enum {
    MOD_NONE    = 0,
    MOD_TINY    = 1,
    MOD_SMALL   = 2,
    MOD_COMPACT = 3,
    MOD_MEDIUM  = 4,
    MOD_LARGE   = 5,
    MOD_HUGE    = 6,
    MOD_FLAT    = 7,
} mod_type;             // Memory model type

enum asm_cpu {
        /* bit count from left:
           bit 0-2:   Math coprocessor
           bit 3:     priviledged?
           bit 4-6:   cpu type
           bit 7-11;  extension set */

        P_NO87  = 0x0000,         /* no FPU */
        P_87    = 0x0001,         /* 8087 */
        P_287   = 0x0002,         /* 80287 */
        P_387   = 0x0004,         /* 80387 */

        P_PM    = 0x0008,         /* privileged opcode */

        P_86    = 0x0000,         /* 8086, default */
        P_186   = 0x0010,         /* 80186 */
        P_286   = 0x0020,         /* 80286 */
        P_286p  = P_286 | P_PM,   /* 80286, protected mode */
        P_386   = 0x0030,         /* 80386 */
        P_386p  = P_386 | P_PM,   /* 80386, protected mode */
        P_486   = 0x0040,         /* 80486 */
        P_486p  = P_486 | P_PM,   /* 80486, protected mode */
        P_586   = 0x0050,         /* pentium */
        P_586p  = P_586 | P_PM,   /* pentium, protected mode */
        P_686   = 0x0060,         /* pentium */
        P_686p  = P_686 | P_PM,   /* pentium, protected mode */

        P_MMX   = 0x0080,         /* MMX extension instructions */
        P_K3D   = 0x0100,         /* 3DNow extension instructions */
        P_SSE1  = 0x0200,         /* SSE1 extension instructions */
        P_SSE2  = 0x0400,         /* SSE2 extension instructions */
        P_SSE3  = 0x0800,         /* SSE3 extension instructions */
#if SSSE3SUPP
        P_SSSE3 = 0x1000,         /* SSSE3 extension instructions */
#endif
        /* all SSE extension instructions */
#if SSSE3SUPP
        P_SSEALL = P_SSE1 | P_SSE2 | P_SSE3 | P_SSSE3,
#else
        P_SSEALL = P_SSE1 | P_SSE2 | P_SSE3,
#endif
        NO_OPPRFX  = P_MMX | P_SSEALL,

        P_FPU_MASK = P_87 | P_287 | P_387,
        P_CPU_MASK = 0x0070,
        P_EXT_MASK = P_MMX | P_K3D | P_SSEALL
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

typedef struct global_options {
    bool        sign_value;            /* -j, -s options */
    bool        quiet;                 /* -q option */
    bool        line_numbers;          /* -Zd option */
    char        naming_convention;     /* obsolete (OW specific) */
    enum fpe    floating_point;        /* -FPi, -FPi87, -FPc */

    /* error handling stuff */
    int         error_limit;             /* -e option  */
    char        warning_level;           /* -Wn option */
    char        warning_error;           /* -WX option */
#ifdef DEBUG_OUT
    char        debug;                   /* -d6 option */
#endif
#if BUILD_TARGET
    char        *build_target;           /* -bt option */
#endif
    char        *code_class;             /* -nc option */
    char        *data_seg;               /* -nd option */
    char        *text_seg;               /* -nt option */
    char        *module_name;            /* -nm option */

    char        *default_name_mangler;   /* obsolete */
#if COCTALS
    bool        allow_c_octals;          /* -o option */
#endif
    bool        no_comment_data_in_code_records; /* -zlc option */
    bool        no_dependencies;         /* -zld option */
    bool        no_file_entry;           /* -zlf option */
    bool        no_section_aux_entry;    /* -zls option  */
    bool        watcom_c_mangler;        /* -zcw option */
    bool        no_stdcall_decoration;   /* -zzo option */
    bool        no_stdcall_suffix;       /* -zzp option  */
    bool        entry_decorated;         /* -zzs option  */
    bool        write_listing;           /* -Fl option  */
    bool        nocasemap;               /* -Cp option  */
    bool        preprocessor_stdout;     /* -EP option  */
    bool        masm51_compat;           /* -Zm option  */
    bool        no_symbol_listing;       /* -Sn option  */
    bool        list_generated_code;     /* -Sg option  */
    enum oformat output_format;          /* -omf, -coff, -elf options */
    uint_8      alignment_default;       /* -Zp option  */
    lang_type   langtype;                /* -Gc|d|z option */
    mod_type    model;                   /* -mt|s|m|c|l|h|f option */
    uint_8      cpu;                     /* -0|1|2|3|4|5|6 option */
    uint_8      fpu;                     /* -fp{0|2|3|5|6|c} option */
#if OWREGCONV
    bool        register_convention;     /* open watcom reg conv  */
#endif
    bool        privileged_mode;         /* p suffix for cpu? */
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

typedef enum {
    OFSSIZE_EMPTY = 0,
    OFSSIZE_16,
    OFSSIZE_32
} ofssize;

typedef enum {
    SEGORDER_SEQ = 0,
    SEGORDER_DOSSEG,
    SEGORDER_ALPHA
} seg_order;

/* global variables */
extern struct asm_tok   *AsmBuffer[];   // token buffer
extern struct asm_code  *CodeInfo;      // input for codegen
extern unsigned int     Parse_Pass;     // assembly pass
extern unsigned char    Opnd_Count;     // operand count of current instr
extern bool             Modend;         // end of module is reached
extern bool             Use32;          // if current segment is 32-bit
extern int              Token_Count;    // number of tokens in current line

// defined in assemble.c

extern struct asm_sym LineItem;
#define LineNumber LineItem.offset
extern struct asm_sym WordSize;
#define CurrWordSize WordSize.offset

extern bool             PhaseError;
extern bool             write_to_file;

// functions in assemble.c

extern void             OutputCodeByte( unsigned char );
extern void             OutputDataByte( unsigned char );
extern void             OutputByte( unsigned char );
extern void             OutSelect( bool );
extern void             AssembleModule( void );
extern void             AddLinnumDataRef( void );

// main.c

extern void             CloseFiles( void );

#endif
