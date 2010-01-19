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

// SYM_LIB  - library paths are no longer added to the symbol table
// SYM_LNAME has been removed.
// It was used for the null-entry in the LNAME table only
// v2.01: SYM_PROC has been removed.
// v2.01: SYM_LIB has been removed.

enum sym_state {
        SYM_UNDEFINED,
        SYM_INTERNAL,       /*  1 internal label */
        SYM_EXTERNAL,       /*  2 external       */
        SYM_STACK,          /*  3 stack variable */
        SYM_SEG,            /*  4 segment        */
        SYM_GRP,            /*  5 group          */
        SYM_CLASS_LNAME,    /*  6 lname entry for segment class ... not in symbol table */
        SYM_STRUCT_FIELD,   /*  7 field defined in some structure   */
        SYM_TYPE,           /*  8 structure, union, typedef, record */
        SYM_ALIAS,          /*  9 alias name    */
        SYM_MACRO,          /* 10 macro         */
        SYM_TMACRO          /* 11 text macro    */
};

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
        MT_PTR   = 0x83,
        MT_EMPTY = 0xC0,
        MT_BITS  = 0xC1,
        MT_ABS   = 0xC2,
        MT_SHORT = 0xC3,
        MT_TYPE  = 0xC4,
        MT_FLOAT   = 0x20, /* bit 5 */
        MT_SPECIAL = 0x80, /* bit 7 */
        MT_SPECIAL_MASK = 0xC0, /* bit 6+7 */
        MT_SIGNED  = 0x40, /* bit 6=1, bit 7 = 0 */
        MT_ADDRESS = 0x80, /* bit 7=1, bit 6 = 0 */
        MT_SIZE_MASK = 0x1F
} memtype;

#define IS_SIGNED(x)  (((x) & MT_SPECIAL_MASK) == MT_SIGNED)

// symbols can be
// - "labels" (data or code, internal, external, stack)
//   which have mem_type MT_BYTE..MT_OWORD, MT_NEAR, MT_FAR, MT_PTR
// - constants (EQU) or assembly time variables (defined by "="),
//   mem_type is MT_ABS.
// - types (STRUCT, UNION, TYPEDEF, RECORD) (mem_type = MT_TYPE)
// - preprocessor items (macros and text macros), which have no
//   mem_type (MT_EMPTY).

typedef ret_code (* macro_func)( char *, char * * );
typedef uint_32 (* internal_func)( struct asm_sym * );

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

        struct asm_sym  *segment;      /* for SYM_INTERNAL, SYM_EXTERNAL */
        union {
            int_32          offset;    /* used by SYM_INTERNAL */
            int_32          value;     /* used by MT_ABS */
            uint_32         uvalue;    /* v2.01: equates */
            char *          string_ptr;/* used by SYM_TMACRO */
            macro_func      func_ptr;  /* used by SYM_MACRO */
            int_32          max_offset;/* used by SYM_SEG */
        };
        union {
            /* for SYM_INTERNAL (if isproc=0), SYM_STRUCT_FIELD */
            uint_32         first_size;   /* size of 1st initializer in bytes */
            /* for SYM_INTERNAL (if isproc=1), SYM_EXTERNAL, ( + SYM_TYPE (typedefs) ) */
            struct {
                unsigned char   Ofssize;   /* also for SYM_TYPE + SYM_GRP */
                unsigned char   comm:1;    /* is communal */
                unsigned char   weak:1;    /* 1 if an unused "externdef" */
                unsigned char   isfar:1;   /* for communal + SYM_TYPE */
            };
            /* for SYM_MACRO */
            struct {
                unsigned char   vararg:1;   // accept additional params
                unsigned char   isfunc:1;   // it's a macro function
                unsigned char   runsync:1;  // run macro synchronous
#if MACROLABEL
                unsigned char   label:1;    // macro is "label-aware"
#endif
            };
        };
        /* first_length is used for data items only
         (SYM_INTERNAL & SYM_STRUCT_FIELD).
         idx is used by SYM_EXTERNAL
         */
        union {
            /* for SYM_INTERNAL, SYM_STRUCT_FIELD */
            uint_32         first_length; /* size of 1st initializer--elts. dup'd */
            /* for SYM_EXTERNAL, SYM_CLASS_LNAME */
            uint            idx;          /* (external definition) index */
            /* for SYM_TYPE ( used after assembly steps) */
            uint_16         cv_typeref;   /* codeview type index */
            uint_32         max_mbr_size; /* SYM_TYPE: max size members */
        };
        /* for SYM_INTERNAL, SYM_STRUCT_FIELD, SYM_TYPE */
        uint_32         total_size;   /* total number of bytes (sizeof) */
        union {
            uint_32        total_length; /* SYM_INTERNAL, SYM_STRUCT_FIELD: total number of elements (LENGTHOF) */
            struct asm_sym *altname;     /* SYM_EXTERNAL: alternative name */
            struct debug_info *debuginfo;/* SYM_INTERNAL (isproc): debug info (COFF) */
            internal_func  sfunc_ptr;    /* SYM_INTERNAL+predefined */
        };
        unsigned short  used:1;       /* symbol has been referenced */
        unsigned short  defined:1;    /* symbol has been defined */
        unsigned short  scoped:1;     /* symbol is local label or LOCAL */
        unsigned short  global:1;     /* symbol is global */
        unsigned short  equate:1;     /* symbol has been defined with EQU */
        unsigned short  predefined:1; /* symbol is predefined */
        unsigned short  variable:1;   /* symbol is variable (redef) */
        unsigned short  public:1;     /* symbol is to make public */
        unsigned short  list:1;       /* symbol is to be listed */
        unsigned short  isarray:1;    /* symbol is an array */
        unsigned short  included:1;   /* symbol is in COFF symbol table */
        unsigned short  saved:1;      /* symbol has been saved ("fast pass") */
        unsigned char   isproc:1;     /* symbol is PROC or PROTO */
#if FASTMEM==0
        unsigned short  staticmem:1;  /* symbol stored in static memory */
#endif
        unsigned short  sign:1;       /* for equates allow full 32-bit value */
        lang_type       langtype;
        enum sym_state  state;
        memtype         mem_type;
        uint_8          name_size;
        struct asm_sym  *type;        /* set if memtype is MT_TYPE */
        struct asmfixup *fixup;
#if 0 //MANGLERSUPP /* obsolete */
        char            *(*mangler)( struct asm_sym *sym, char *buffer );
#endif
} asm_sym;

extern  struct asm_sym  *SymLookup( const char *name );
extern  struct asm_sym  *SymLookupLabel( const char *name, int bDefine );
extern  struct asm_sym  *SymSearch( const char *name );
extern  void            SymSetCmpFunc( void );

//extern  void            SymTakeOut( const char *name );
extern  void            SymFree( struct asm_sym *sym);
//extern  int             SymChangeName( const char *old, const char *new );
extern  void            SymSetName( struct asm_sym * sym, const char *name );
extern  void            SymInit( void );
extern  void            SymFini( void );
extern  void            SymPassInit( int pass );
extern  struct asm_sym **SymSort( unsigned int * );
extern  void            SymMakeAllSymbolsPublic( void );
//extern  void            SymSetCurrPC( void );
extern  int             SymEnum( struct asm_sym * *, int * );
extern  struct asm_sym  *SymIsType( const char * );

#ifdef __WATCOMC__
typedef int (__watcall * StrCmpFunc)(const char *, const char * );
#else
typedef int (* StrCmpFunc)(const char *, const char * );
#endif
extern StrCmpFunc SymCmpFunc;

extern  struct asm_sym  *SymCreate( const char *, bool );
extern  struct asm_sym  *SymLCreate( const char * );
extern  void             SymClearLocal( void );
extern  void             SymSetLocal( asm_sym * );
extern  void             SymGetLocal( asm_sym * );

#endif
