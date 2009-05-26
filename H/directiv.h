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
* Description:  directives declarations
*
****************************************************************************/


#ifndef _DIRECTIV_H_
#define _DIRECTIV_H_

#include "omfrec.h" /* needed because of obj_rec member in seg_info */

#define MAX_LNAME       255
#define LNAME_NULL      0

typedef int     direct_idx;     // directive index, such as segment index,
                                // group index or lname index, etc.

typedef enum {
    STACK_NONE,
    STACK_NEAR,
    STACK_FAR,
} dist_type;            // Stack distance
#define NUM_STACKTYPE 3

typedef enum {
    OPSYS_DOS,
    OPSYS_OS2
} os_type;              // Type of operating system
#define NUM_OS 2

enum {
    TAB_UNDEF = 0,
    TAB_EXT,
    TAB_SEG,
    TAB_GRP,
    TAB_LIB,
    TAB_PROC,
    TAB_ALIAS,
    TAB_LAST,
};

typedef enum {
    SEGTYPE_UNDEF,
    SEGTYPE_CODE,
    SEGTYPE_DATA,
    SEGTYPE_BSS,
    SEGTYPE_ABS
} seg_type;

typedef enum {
    OT_GROUP = 0,  /* OFFSET:GROUP (default, must be 0) */
    OT_FLAT,       /* OFFSET:FLAT    */
    OT_SEGMENT     /* OFFSET:SEGMENT */
} offset_type;

/*---------------------------------------------------------------------------*/

typedef struct stacknode {
    void    *next;
    void    *elt;
} stacknode;

enum stypeflags {
    VOID_TYPE = 0,
    INT_TYPE,
    UINT_TYPE,
    FLOAT_TYPE,
    ADDR_TYPE
};

typedef struct {
    enum asm_token token;
    memtype mem_type;
    int size;
    enum stypeflags type;
    ofssize ofs_size;
} simpletype;

extern simpletype SimpleType[];

typedef struct {
      char      *string;        // the token string
      uint_16   value;          // value assigned to the token
      uint_16   init;           // kind of token
} typeinfo;

/*---------------------------------------------------------------------------*/
/* Structures for grpdef, segdef, externdef, pubdef, included library,       */
/* procedure and symbolic integer constants.                                 */
/*---------------------------------------------------------------------------*/

typedef struct seg_item {
    struct seg_item     *next;
    struct dir_node     *seg;
} seg_item;

typedef struct {
    seg_item            *seglist;       // list of segments in the group
    direct_idx          idx;            // its group index
    direct_idx          lname_idx;      // LNAME index (OMF only)
    uint                numseg;         // number of segments in the group
} grp_info;

/* todo: remove the OMF segment record and store the info directly
   in seg_info */

typedef struct {
    obj_rec             *segrec;        /* OMF segment record */
    struct asm_sym      *group;         // its group
    uint_32             start_loc;      // starting offset of current ledata or lidata
    union {
        uint_32         current_loc;    // current offset in current ledata or lidata
        uint_32         reloc_offset;   // used by ELF to store reloc file offset
        uint_32         offset;         // used by BIN to store start offset
    };
    uint_8              *CodeBuffer;
    asm_sym             *labels;        // linked list of labels in this seg
    union {
        struct fixup        *FixupListHead; /* for OMF */
        struct asmfixup     *FixupListHeadGen; /* for other formats */
    };
    union {
        struct fixup        *FixupListTail;    /* for OMF */
        struct asmfixup     *FixupListTailGen; /* for other formats */
    };
    void                *LinnumQueue;   // for COFF line numbers
    union {
        uint_32         num_relocs;     // used by COFF/ELF
        uint_32         fileoffset;     // used by BIN
    };
    seg_type            segtype;        // segment is belonging to "CODE" or 'DATA' class
    direct_idx          lname_idx;      // LNAME index (OMF only)
    unsigned            alignment:4;    // is value 2^x
    unsigned            characteristics:8;
    unsigned            readonly:1;     // if the segment is readonly
    unsigned            Use32:1;
#if BIN_SUPPORT
    unsigned            initial:1;      // for BIN output
#endif
} seg_info;

#define MAX_SEGALIGNMENT 0x0F

typedef struct regs_list {
    struct regs_list    *next;
    char                *reg;
} regs_list;

// PROC item

typedef struct {
    regs_list           *regslist;      // PROC: list of registers to be saved
    struct dir_node     *paralist;      // list of parameters
    struct dir_node     *locallist;     // PROC: list of local variables
    struct dir_node     *labellist;     // PROC: list of local labels
    int                 parasize;       // total no. of bytes used by parameters
    int                 localsize;      // PROC: total no. of bytes used by local variables
    char                *prologuearg;   // PROC: prologuearg attribute
    uint_32             list_pos;       // PROC: prologue list pos
    unsigned            is_vararg:1;    // if it has a vararg
    unsigned            pe_type:1;      // prolog/epilog code type 0:8086/186 1:286 and above
    unsigned            export:1;       // EXPORT procedure
    unsigned            init:1;         // has ExamineProc() been called?
    unsigned            forceframe:1;   // FORCEFRAME prologuearg?
    unsigned            loadds:1;       // LOADDS prologuearg?
} proc_info;

// macro parameter

typedef struct mparm_list {
    char                *label;         // name of parameter
    char                *def;           // optional default parm
    unsigned int        required:1;     // is parm required (REQ)
} mparm_list;

// macro line

typedef struct asmlines {
    struct asmlines     *next;
    uint_8              ph_count; /* placeholders contained in this line */
    char                line[];
} asmlines;

// macro item

typedef struct {
    uint_16             parmcnt;    /* no of params */
    uint_16             localcnt;   /* no of locals */
    mparm_list          *parmlist;  /* array of parameter items */
    asmlines            *data;      /* prepared macro source lines */
#ifdef DEBUG_OUT
    uint_32             count;      /* no of times the macro was invoked */
#endif
    uint                srcfile;    /* sourcefile index */
} macro_info;

// STRUCT field item

typedef struct field_list {
    struct field_list   *next;
    char                *initializer;
    char                *value;
    struct asm_sym      *sym;
} field_list;

typedef enum {
    TYPE_NONE,
    TYPE_STRUCT,
    TYPE_UNION,
    TYPE_TYPEDEF,
    TYPE_RECORD
} type_kind;

typedef struct {
    field_list          *head;       /* is NULL for TYPEDEF */
    union {
        field_list          *tail;   /* for STRUCT, UNION, RECORD */
        struct asm_sym      *target; /* if TYPEDEF is a PTR, specifies the target TYPE */
    };
    union {
        uint_8          alignment;   /* STRUCT: 1,2,4,8,16 or 32 */
        uint_8          indirection; /* TYPEDEF: level of indirection for PTR */
    };
    type_kind           typekind;
    union {
        uint_8          flags;
        struct {
            unsigned    isInline:1;  /* STRUCT: inline */
            unsigned    isOpen:1;    /* STRUCT: set until the matching ENDS is found */
            unsigned    OrgInside:1; /* STRUCT: struct contains an ORG */
        };
    };
} struct_info;

union entry {
    seg_info            *seginfo;       // SEGMENT definition
    grp_info            *grpinfo;       // GROUP definition
    proc_info           *procinfo;      // PROC definition
    struct_info         *structinfo;    // STRUCT, UNION, TYPEDEF, RECORD def
    macro_info          *macroinfo;     // MACRO definition
};

/* dir_node originally was a "directive_node"
 * However, currently all symbols are allocated as a dir_node
 * the additional 3 fields are used differently depending on symbol's type.
 */

typedef struct dir_node {
    struct asm_sym sym;
    union {
        /* additional fields, used by seg, grp, proc, type, macro */
        union entry e;
        /* used to save the local hash table (contains PROC locals: params,
         locals, labels). Details see SymGetLocal(), SymSetLocal() in symbols.c */
        struct dir_node *nextll;
    };
    union {
        /* for SYM_SEG, SYM_GRP, SYM_EXTERNAL, SYM_PROC, SYM_ALIAS, SYM_LIB:
         * linked list of this type of symbol.
         * for SYM_INTERNAL:
         * linked list of labels for current segment (used for BackPatch)
         */
        struct dir_node *next;
        /* used by PROC params */
        struct {
            unsigned            is_vararg:1;    // if it is a VARARG param
            unsigned            is_ptr:1;       // if it is a PTR type
            unsigned            is_far:1;       // if ptr is FAR
            unsigned            is32:1;         // if offset is 32 bit (ptr only)
        };
    };
    union {
        /* for SYM_SEG, SYM_GRP, SYM_EXTERNAL, SYM_PROC, SYM_ALIAS, SYM_LIB:
         * linked list of this type of symbol, to allow fast removes.
         * Actually, the only symbols which have a "chance" to be
         * removed are those of type SYM_EXTERNAL.
         */
        struct dir_node *prev;
        /* used by PROC locals (SYM_STACK) for linked list */
        struct dir_node *nextlocal;
        /* used by PROC params (SYM_STACK) for linked list */
        struct dir_node *nextparam;
    };
} dir_node;

typedef struct {
    dir_node            *head;
    dir_node            *tail;
} symbol_queue;     // tables array - queues of symbols of 1 type ie: segments
                    // the data are actually part of the symbol table


/*---------------------------------------------------------------------------*/

extern uint                     LnamesIdx;      // Number of LNAMES definition

typedef struct {
    unsigned            error_count;     // total of errors so far
    unsigned            warning_count;   // total of warnings so far
    char                *proc_prologue;  // current OPTION PROLOGUE value
    char                *proc_epilogue;  // current OPTION EPILOGUE value
    unsigned            anonymous_label; // "anonymous label" counter
    unsigned            hll_label;       // hll directive label counter
    bool                Use32;           // current wordsize
    dist_type           distance;        // stack distance;
    mod_type            model;           // memory model;
    lang_type           langtype;        // language;
    os_type             ostype;          // operating system;
    seg_order           segorder;        // .alpha, .seq, .dosseg
    offset_type         offsettype;      // OFFSET:GROUP|FLAT|SEGMENT
    short               cpu;             // cpu setting (value @cpu symbol);
    enum asm_cpu        curr_cpu;        // cpu setting (OW stylex);
    unsigned char       radix;           // current .RADIX setting
    unsigned char       fieldalign;      // -Zp, OPTION:FIELDALIGN setting
#if PROCALIGN
    unsigned char       procalign;       // current OPTION:PROCALIGN setting
#endif
    enum listmacro      list_macro;      // current .LISTMACRO setting
    unsigned            cmdline:1;       // memory model set by cmdline opt?
    unsigned            defUse32:1;      // default segment size 32-bit
    unsigned            case_sensitive:1;     // option casemap
    unsigned            convert_uppercase:1;  // option casemap
    unsigned            procs_private:1; // option proc:private
    unsigned            procs_export:1;  // option proc:export
    unsigned            dotname:1;       // option dotname
    unsigned            ljmp:1;          // option ljmp
    unsigned            m510:1;          // option m510
    unsigned            scoped:1;        // option scoped
    unsigned            oldstructs:1;    // option oldstructs
    unsigned            emulator:1;      // option emulator
    unsigned            setif2:1;        // option setif2
    unsigned            list:1;          // .list/.nolist
    unsigned            cref:1;          // .cref/.nocref
    unsigned            listif:1;        // .listif/.nolistif
    unsigned            list_generated_code:1; // .listall, -Sa, -Sg

    unsigned            flatgrp_idx;     // index of FLAT group
    char                name[_MAX_FNAME];// name of module
    uint                srcfile;
} module_info;                           // Information about the module

extern module_info      ModuleInfo;

/*---------------------------------------------------------------------------*/

extern dir_node         *dir_insert( const char *, int );
extern dir_node         *dir_insert_ex( const char *, int );
extern void             dir_settype( dir_node *, int );
extern void             dir_internal( dir_node * );
extern void             dir_free( dir_node *, bool );
extern void             dir_add_table( dir_node * );
extern void             dir_remove_table( dir_node * );

extern int              SizeFromMemtype( memtype, bool );
extern ret_code         MemtypeFromSize( int, memtype * );
extern ret_code         GetLangType( int *, lang_type * );

//extern uint             GetExtIdx( struct asm_sym * ); /* Get the index of an extrn defn */

extern typeinfo         *FindToken( char *token, typeinfo *, int );
extern int              FindSimpleType( int );  // find simple type
//extern int              RegisterValueToIndex( int, bool *);
extern int              SizeFromRegister( int );
extern ret_code         EchoDef( int );         // handle ECHO directive
extern ret_code         OptionDirective( int ); // handle OPTION directive

extern void             pushitem( void *stack, void *elt );
extern void             *popitem( void *stack );
//extern void             *peekitem( void *stack, int );
//extern ret_code         cpu_directive( int i );
extern ret_code         SetCPU( enum asm_token );
extern ret_code         directive( int , long );
extern void             ModuleInit( void ); /* Initializes ModuleInfo structure */


// this isn't in parser.h because the dir_node type isn't known yet there
extern ret_code         data_init( int, int, dir_node * );

#endif
