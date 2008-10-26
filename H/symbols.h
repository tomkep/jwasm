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

// SYM_LIB has been removed since v1.92,
// library paths are no longer added to the symbol table
// SYM_LNAME is also removed.
// It was used for the null-entry in the LNAME table only
// SYM_PROC is to be removed yet!

enum sym_state {
        SYM_UNDEFINED,
        SYM_INTERNAL,       /* 1 internal var   */
        SYM_EXTERNAL,       /* 2 external       */
        SYM_STACK,          /* 3 stack variable */
        SYM_PROC,           /* 4 procedure      */
        SYM_SEG,            /* 5 segment        */
        SYM_GRP,            /* 6 group          */
        SYM_CLASS_LNAME,    /* 7 lname entry for segment class ... not in symbol table */
        SYM_STRUCT_FIELD,   /* 8  field defined in some structure   */
        SYM_TYPE,           /* 9  structure, union, typedef, record */
        SYM_ALIAS,          /* 10 alias name    */
        SYM_MACRO,          /* 11 macro         */
        SYM_TMACRO          /* 12 text macro    */
};

typedef enum {
        MT_EMPTY,
        MT_BYTE,            /* 1 */
        MT_WORD,            /* 2 */
        MT_DWORD,           /* 3 */
        MT_QWORD,           /* 4 */
        MT_FWORD,           /* 5 */
        MT_TBYTE,           /* 6 */
        MT_OWORD,           /* 7 */
        MT_SHORT,           /* 8 */
        MT_NEAR,            /* 9 */
        MT_FAR,             /* 10 */
        MT_PTR,             /* 11 */
        MT_SBYTE,           /* 12 */
        MT_SWORD,           /* 13 */
        MT_SDWORD,          /* 14 */
        MT_TYPE,            /* 15 field <type> specifies type */
        MT_PROC,            /* 16 a PROTO type (no name) */
        MT_ABS,             /* 17 */
        MT_BITS,            /* 18 */
        MT_ERROR = -1
} memtype;

// symbols can be
// - "labels" (data or code, internal, external, stack)
//   which have mem_type MT_BYTE..MT_OWORD, MT_NEAR, MT_FAR, MT_PTR
// - constants
//   either numbers, defined by EQU or "=", then mem_type is MT_ABS
//   or text equates, then mem_type is MT_EMPTY (to be removed)
// - types (STRUCT, UNION, TYPEDEF, RECORD) (mem_type = MT_TYPE)

typedef int (* macro_func)(char *, char * *);

typedef struct asm_sym {
        struct asm_sym  *next;
        char            *name;

        /* for SYM_INTERNAL, SYM_EXTERNAL */
        struct asm_sym  *segment;
        union {
            int_32          offset;
            int_32          value;     /* used by MT_ABS */
            char *          string_ptr;/* used by SYM_TMACRO */
            macro_func      func_ptr;  /* used by SYM_MACRO */
        };
        /* first_size is used for data items (SYM_INTERNAL & SYM_STRUCT_FIELD).
         */
        union {
            /* for SYM_INTERNAL */
            uint_32         first_size;   /* size of 1st initializer in bytes */
            /* for SYM_EXTERNAL, SYM_PROC */
            struct {
                unsigned    use32:1;
                unsigned    comm:1;    /* is communal */
                unsigned    weak:1;    /* 1 if an unused "externdef" */
                unsigned    isfar:1;   /* for communal only */
                unsigned    isproc:1;  /* PROTO=0, PROC=1 */
            };
            /* for SYM_MACRO */
            struct {
                unsigned    vararg:1;   // accept additional params
                unsigned    isfunc:1;   // it's a macro function
                unsigned    runsync:1;  // run macro synchronous
            };
        };
        /* first_length is used for data items only
         (SYM_INTERNAL & SYM_STRUCT_FIELD).
         idx is used by SYM_EXTERNAL, SYM_PROC (proto)
         */
        union {
            /* for SYM_INTERNAL */
            uint_32         first_length; /* size of 1st initializer--elts. dup'd */
            /* for SYM_EXTERNAL, SYM_LNAME */
            uint            idx;      /* (external definition) index */
        };
        /* for SYM_INTERNAL, SYM_STRUCT_FIELD, SYM_TYPE */
        uint_32         total_size;   /* total number of bytes (sizeof) */
        /* for SYM_INTERNAL, SYM_STRUCT_FIELD */
        uint_32         total_length; /* total number of elements (lengthof) */
        char            *(*mangler)( struct asm_sym *sym, char *buffer );
        unsigned        used:1;       /* symbol has been referenced */
        unsigned        defined:1;    /* symbol has been defined */
        unsigned        local:1;      /* symbol is local label or LOCAL */
        unsigned        global:1;     /* symbol is global */
        unsigned        equate:1;     /* symbol has been defined with EQU */
        unsigned        predefined:1; /* symbol is predefined */
        unsigned        variable:1;   /* symbol is variable (redef) */
        unsigned        public:1;     /* symbol is to make public */
        unsigned        list:1;       /* symbol is to be listed */
        unsigned        included:1;   /* symbol is in COFF symbol table */
        unsigned        saved:1;      /* symbol has been saved ("fast pass") */
        lang_type       langtype;
        enum sym_state  state;
        memtype         mem_type;
        uint_8          name_size;
        struct asm_sym  *type;        /* set if memtype is MT_TYPE */
        struct asmfixup *fixup;
} asm_sym;

extern  struct asm_sym  *SymLookup( const char *name );
extern  struct asm_sym  *SymLookupLabel( const char *name, int bDefine );
extern  struct asm_sym  *SymSearch( const char *name );
extern  void            SymSetCmpFunc( bool nocasemap );

extern  void            SymTakeOut( const char *name );
extern  void            SymFree( struct asm_sym *sym);
//extern  int             SymChangeName( const char *old, const char *new );
extern  struct asm_sym  *SymSetName( struct asm_sym * sym, const char *name );
extern  void            SymInit( void );
extern  void            SymFini( void );
extern  void            SymPassInit( int pass );
extern  struct asm_sym **SymSort( unsigned int * );

#ifdef __WATCOMC__
typedef int (__watcall * StrCmpFunc)(const char *, const char * );
#else
typedef int (* StrCmpFunc)(const char *, const char * );
#endif
extern StrCmpFunc SymCmpFunc;

extern  struct asm_sym  *SymCreate( const char *, bool );

#endif
