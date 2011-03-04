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
* Description:  Assembler symbols internal structures and definitions.
*
****************************************************************************/


#ifndef _SYMBOLS_H_
#define _SYMBOLS_H_

/*
 * SYM_LIB  - library paths are no longer added to the symbol table
 * SYM_LNAME has been removed.
 * It was used for the null-entry in the LNAME table only
 * v2.01: SYM_PROC has been removed.
 * v2.01: SYM_LIB has been removed.
 */
enum sym_state {
        SYM_UNDEFINED,
        SYM_INTERNAL,       /*  1 internal label */
        SYM_EXTERNAL,       /*  2 external       */
        SYM_SEG,            /*  3 segment        */
        SYM_GRP,            /*  4 group          */
        SYM_STACK,          /*  5 stack variable */
        SYM_STRUCT_FIELD,   /*  6 struct member  */
        SYM_TYPE,           /*  7 structure, union, typedef, record */
        SYM_ALIAS,          /*  8 alias name     */
        SYM_MACRO,          /*  9 macro          */
        SYM_TMACRO,         /* 10 text macro     */
        SYM_CLASS_LNAME     /* 11 lname item for segm class - not in symbol table */
};

/* v2.04: MT_SHORT removed */

typedef enum {
        MT_BYTE  = 0x00,
        MT_SBYTE = 0x40,
        MT_WORD  = 0x01,
        MT_SWORD = 0x41,
        MT_DWORD = 0x03,
        MT_REAL4 = 0x23,
        MT_SDWORD= 0x43,
        MT_FWORD = 0x05,
        MT_QWORD = 0x07,
        MT_SQWORD= 0x47,
        MT_REAL8 = 0x27,
        MT_TBYTE = 0x09,
        MT_REAL10= 0x29,
        MT_OWORD = 0x0F,
        MT_PROC  = 0x80,
        MT_NEAR  = 0x81,
        MT_FAR   = 0x82,
        MT_EMPTY = 0xC0,
        MT_BITS  = 0xC1,
        MT_ABS   = 0xC2,
        MT_PTR   = 0xC3,   /* v2.05: changed, old value 0x83 */
        MT_TYPE  = 0xC4,
        MT_FLOAT   = 0x20, /* bit 5 */
        MT_SPECIAL = 0x80, /* bit 7 */
        MT_SPECIAL_MASK = 0xC0, /* bit 6+7 */
        MT_SIGNED  = 0x40, /* bit 6=1, bit 7 = 0 */
        MT_ADDRESS = 0x80, /* bit 7=1, bit 6 = 0 */
        MT_SIZE_MASK = 0x1F
} memtype;

#define IS_SIGNED(x)  (((x) & MT_SPECIAL_MASK) == MT_SIGNED)

/* symbols can be
 * - "labels" (data or code, internal, external, stack)
 *   which have mem_type MT_BYTE..MT_OWORD, MT_NEAR, MT_FAR, MT_PTR
 * - constants (EQU) or assembly time variables (defined by "="),
 *   mem_type is MT_ABS.
 * - types (STRUCT, UNION, TYPEDEF, RECORD) (mem_type = MT_TYPE)
 * - preprocessor items (macros and text macros), which have no
 *   mem_type (MT_EMPTY).
 */
typedef ret_code (* macro_func)( char *, char * * );
typedef void (* internal_func)( struct asm_sym * );

struct debug_info {
    uint_32 start_line;  /* procs's start line */
    uint_32 end_line;    /* procs's last line */
    uint_32 ln_fileofs;  /* file offset to line numbers */
    uint_16 line_numbers;/* line numbers in function */
    uint_16 file;        /* proc's start file */
    uint next_proc;      /* index next proc */
    uint next_file;      /* index next file */
};


typedef struct asm_sym {
        struct asm_sym  *next;
        char            *name;

        struct asm_sym  *segment;      /* used by SYM_INTERNAL, SYM_EXTERNAL */
        union {
            int_32          offset;    /* used by SYM_INTERNAL */
            int_32          value;     /* used by MT_ABS */
            uint_32         uvalue;    /* v2.01: equates */
            char *          string_ptr;/* used by SYM_TMACRO */
            struct asm_sym *substitute;/* v2.04b: used by SYM_ALIAS */
            macro_func      func_ptr;  /* used by SYM_MACRO */
            int_32          max_offset;/* used by SYM_SEG */
            int_32          class_lname_idx;/* used by SYM_CLASS_LNAME */
        };
        union {
            /* for SYM_INTERNAL (memtype != NEAR|FAR|ABS), SYM_STRUCT_FIELD */
            uint_32         first_size;   /* size of 1st initializer in bytes */
            /* for SYM_INTERNAL (memtype == NEAR|FAR),
             * SYM_GRP (Ofssize),
             * SYM_EXTERNAL (Ofssize, comm, weak, isfar, is_ptr, ptr_memtype),
             * SYM_STACK (Ofssize, isfar, is_vararg, is_ptr, ptr_memtype ),
             * SYM_TYPE, TYPE_TYPEDEF (Ofssize, isfar, is_ptr, ptr_memtype )
             */
            struct {
                unsigned char   Ofssize;   /* offset size (USE16, USE32) */
                unsigned char   is_ptr;    /* PTR indirection */
                union {
                    unsigned char ptr_memtype;/* pointer target type */
                    unsigned char asmpass;    /* SYM_INTERNAL (mem_type NEAR|FAR) */
                };
                unsigned char   seg_ofssize:2;    /* SYM_EXTERNAL only */
                unsigned char   comm:1;    /* is communal */
                unsigned char   weak:1;    /* 1 if an unused "externdef" */
                unsigned char   isfar:1;   /* SYM_EXTERNAL, SYM_TYPE, SYM_STACK */
                unsigned char   is_vararg:1;/* SYM_STACK, VARARG param */
            };
            /* for SYM_MACRO */
            struct {
                unsigned char   mac_vararg:1;/* accept additional params */
                unsigned char   isfunc:1;   /* it's a macro function */
#if MACROLABEL
                unsigned char   label:1;    /* macro is "label-aware" */
#endif
                //unsigned char   runsync:1;  /* run macro synchronous */
            };
        };
        union {
            /* first_length is used for data items only
             * for SYM_INTERNAL (except "labels"), SYM_STRUCT_FIELD */
            uint_32         first_length; /* size of 1st initializer--elts. dup'd */
            /* SYM_TYPE (TYPEKIND_STRUCT or TYPEKIND_UNION) */
            uint_32         max_mbr_size; /* max size members */
            /* SYM_STACK, SYM_TYPE (TYPEKIND_TYPEDEF), SYM_EXTERNAL, SYM_INTERNAL (labels) */
            struct asm_sym  *target_type; /* set if ptr_memtype is MT_TYPE */
        };
        union {
            /* for SYM_INTERNAL, SYM_STRUCT_FIELD,
             * SYM_TYPE, SYM_STACK,
             * SYM_EXTERNAL (comm=1)
             */
            uint_32         total_size;   /* total number of bytes (sizeof) */
            /* for SYM_INTERNAL, MT_ABS (numeric equates) */
            int_32          value3264;    /* high bits for equates */
        };
        union {
            /* SYM_INTERNAL, SYM_STRUCT_FIELD,
             * SYM_STACK, SYM_EXTERNAL (comm==1):
             * total number of elements (LENGTHOF)
             */
            uint_32        total_length;
            struct asm_sym *altname;     /* SYM_EXTERNAL (comm==0): alternative name */
            struct debug_info *debuginfo;/* SYM_INTERNAL (isproc==1): debug info (COFF) */
            internal_func  sfunc_ptr;    /* SYM_INTERNAL+predefined */
            /* SYM_TYPE: codeview type index (used after assembly steps)
             * v2.04: moved from first_length, were it didn't work anymore
             * since the addition of field max_mbr_size.
             */
            uint_16        cv_typeref;
        };
        unsigned short  used:1;       /* symbol has been referenced */
        unsigned short  isdefined:1;  /* symbol is "defined" in this pass */
        unsigned short  scoped:1;     /* symbol is local label or LOCAL */
        unsigned short  global:1;     /* symbol has been added to the globals queue */
        unsigned short  equate:1;     /* symbol has been defined with EQU */
        unsigned short  predefined:1; /* symbol is predefined */
        unsigned short  variable:1;   /* symbol is variable ('=' directive) */
        unsigned short  public:1;     /* symbol has been added to the publics queue */
        unsigned short  list:1;       /* symbol is to be listed */
        unsigned short  isarray:1;    /* symbol is an array (total_length is valid) */
        unsigned short  included:1;   /* COFF: static symbol added to public queue. ELF:symbol added to symbol table */
        unsigned short  saved:1;      /* assembly time variables only: symbol has been saved ("fast pass") */
        unsigned short  isproc:1;     /* symbol is PROC or PROTO */
        unsigned short  isdata:1;     /* field first_size is valid */
#if FASTMEM==0
        unsigned short  staticmem:1;  /* symbol stored in static memory */
#endif
#ifdef DEBUG_OUT
        unsigned short  forward:1;    /* symbol was forward referenced */
#endif
        enum lang_type  langtype;
        enum sym_state  state;
        memtype         mem_type;
#if (MAX_ID_LEN <= 255)
        uint_8          name_size;
#else
        uint_16         name_size;
#endif
        struct asm_sym  *type;        /* set if memtype is MT_TYPE */
        union {
            struct fixup *fixup;      /* SYM_INTERNAL, SYM_UNDEFINED, SYM_EXTERNAL? only */
            /* for SYM_EXTERNAL */
            uint         ext_idx;     /* (external definition) index */
        };
} asm_sym;

extern  struct asm_sym  *SymLookup( const char *name );
extern  struct asm_sym  *SymLookupLabel( const char *name, int bDefine );
extern  struct asm_sym  *SymSearch( const char *name );
extern  void            SymSetCmpFunc( void );

//extern  void            SymTakeOut( const char *name );
extern  void            SymFree( struct asm_sym *sym);
//extern  int             SymChangeName( const char *old, const char *new );
extern  void            SymSetName( struct asm_sym * sym, const char *name );
extern  struct asm_sym *SymAddToTable( struct asm_sym *sym );
extern  void            SymInit( void );
extern  void            SymFini( void );
extern  void            SymPassInit( int pass );
extern  struct asm_sym **SymSort( uint_32 * );
extern  void            SymMakeAllSymbolsPublic( void );
extern  int             SymEnum( struct asm_sym * *, int * );
extern  struct asm_sym  *SymIsType( const char * );

#ifdef __WATCOMC__
typedef int (__watcall * StrCmpFunc)(const void *, const void *, unsigned );
#else
typedef int (* StrCmpFunc)(const void *, const void *, unsigned );
#endif
extern StrCmpFunc SymCmpFunc;

extern  struct asm_sym  *SymCreate( const char *, bool );
extern  struct asm_sym  *SymLCreate( const char * );
extern  void             SymClearLocal( void );
extern  void             SymSetLocal( asm_sym * );
extern  void             SymGetLocal( asm_sym * );

#endif
