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
    //STACK_NONE,
    STACK_NEAR,
    STACK_FAR,
} dist_type;            // Stack distance

typedef enum {
    OPSYS_DOS,
    OPSYS_OS2
} os_type;              // Type of operating system

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
    SEGTYPE_STACK,
    SEGTYPE_ABS,
    SEGTYPE_ERROR
} seg_type;

typedef enum {
    OT_GROUP = 0,  /* OFFSET:GROUP (default, must be 0) */
    OT_FLAT,       /* OFFSET:FLAT    */
    OT_SEGMENT     /* OFFSET:SEGMENT */
} offset_type;

/*---------------------------------------------------------------------------*/

typedef struct {
    enum asm_token token;
    memtype mem_type;
    uint_8 Ofssize;
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

/* todo: remove field segrec, which is OMF specific, and store the info directly
   in seg_info */

typedef struct {
    obj_rec             *segrec;        /* OMF segment record */
    struct asm_sym      *group;         // segment's group or NULL
    uint_32             start_loc;      // starting offset of current ledata or lidata
    union {
        uint_32         current_loc;    // current offset in current ledata or lidata
        uint_32         reloc_offset;   // ELF: reloc file offset
        uint_32         start_offset;   // BIN: start offset in group
    };
#ifdef __I86__
    uint_8 huge         *CodeBuffer;
#else
    uint_8              *CodeBuffer;
#endif
    uint_32             bytes_written;  // initialized bytes in segment
    asm_sym             *labels;        // linked list of labels in this seg
    union {
        struct fixup        *FixupListHead; /* for OMF */
        struct asmfixup     *FixupListHeadGen; /* for other formats */
    };
    union {
        struct fixup        *FixupListTail;    /* for OMF */
        struct asmfixup     *FixupListTailGen; /* for other formats */
    };
    union {
        void            *LinnumQueue;   // for COFF line numbers
        uint_32         fileoffset;     // used by BIN + ELF
        uint_32         num_linnums;    // used by COFF (after LinnumQueue has been read)
    };
    uint_32             num_relocs;     // used by COFF/ELF
    seg_type            segtype;        // segment's type (code, data, ...)
    direct_idx          lname_idx;      // LNAME index (OMF only)
    unsigned char       Ofssize;        // segment's offset size
    unsigned char       characteristics;// used by COFF
    unsigned char       alignment:4;    // is value 2^x
    unsigned char       readonly:1;     // if the segment is readonly
    unsigned char       force32:1;      // force 32bit segdef (OMF only)
} seg_info;

#define MAX_SEGALIGNMENT 0x0F

typedef struct regs_list {
    struct regs_list    *next;
    uint_16             idx;
    char                reg[1];
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
#if AMD64_SUPPORT
    struct asm_sym      *exc_handler;   // PROC: exc handler set by FRAME
#endif
    uint_32             list_pos;       // PROC: prologue list pos
    union {
        unsigned char   flags;
        struct {
            unsigned char       is_vararg:1;    // if it has a vararg
            unsigned char       pe_type:1;      // epilog code, 1=use LEAVE
            unsigned char       export:1;       // EXPORT procedure
            unsigned char       init:1;         // has ExamineProc() been called?
            unsigned char       forceframe:1;   // FORCEFRAME prologuearg?
            unsigned char       loadds:1;       // LOADDS prologuearg?
            unsigned char       stackparam:1;   // for FASTCALL: 1=a stack param exists
#if AMD64_SUPPORT
            unsigned char       isframe:1;      // FRAME set?
#endif
        };
    };
} proc_info;

// macro parameter

typedef struct mparm_list {
    const char          *label;         // name of parameter
    char                *def;           // optional default parm
    unsigned char       required:1;     // is parm required (REQ)
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
            unsigned char   isInline:1;  /* STRUCT: inline */
            unsigned char   isOpen:1;    /* STRUCT: set until the matching ENDS is found */
            unsigned char   OrgInside:1; /* STRUCT: struct contains an ORG */
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
        /* additional fields, used by
         SYM_SEG, SYM_GRP, SYM_INTERNAL(procs), SYM_TYPE, SYM_MACRO */
        union entry e;
        /* used to save the local hash table (contains PROC locals: params,
         locals, labels). Details see SymGetLocal(), SymSetLocal() in symbols.c */
        struct dir_node *nextll;
    };
    union {
        /* for SYM_UNDEFINED, SYM_SEG, SYM_GRP, SYM_EXTERNAL, SYM_INTERNAL(procs), SYM_ALIAS:
         * linked list of this type of symbol.
         * for SYM_INTERNAL:
         * linked list of labels for current segment (used for BackPatch)
         */
        struct dir_node *next;
        /* used by PROC params ( SYM_STACK ) */
        struct {
            unsigned char       is_vararg:1;  // if it is a VARARG param
            unsigned char       is_ptr:1;     // if it is a PTR type
            unsigned char       is_far:1;     // if ptr is FAR
            unsigned char       is32:2;       // offset size (0|1|2, ptr only)
        };
    };
    union {
        /* for SYM_SEG, SYM_GRP, SYM_EXTERNAL, SYM_INTERNAL(procs), SYM_ALIAS:
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

typedef struct {
    unsigned            error_count;     // total of errors so far
    unsigned            warning_count;   // total of warnings so far
    char                *proc_prologue;  // current OPTION PROLOGUE value
    char                *proc_epilogue;  // current OPTION EPILOGUE value
    char                *code_class;     // either "CODE" or value of -nc
    unsigned            anonymous_label; // "anonymous label" counter
    unsigned            hll_label;       // hll directive label counter
    unsigned            total_segs;      // number of segments in module
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
    unsigned char       Ofssize;         // current offset size (16,32,64)
    unsigned char       defOfssize;      // default segment offset size (16,32 [,64]-bit)
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
    unsigned            StartupDirectiveFound:1;
    unsigned            EndDirectiveFound:1;
    unsigned            frame_auto:1;

    unsigned            flatgrp_idx;     // index of FLAT group
    union {
        uint_8          osabi;           // for ELF
    };
    uint                srcfile;         // is a file stack index
    char                name[_MAX_FNAME];// name of module
} module_info;    // Information about the module

extern module_info      ModuleInfo;

struct format_options {
    const char *formatname;
    void (*init)( module_info * );
};

/*---------------------------------------------------------------------------*/

extern dir_node         *dir_insert( const char *, int );
extern dir_node         *dir_insert_ex( const char *, int );
extern void             dir_settype( dir_node *, int );
extern void             dir_internal( dir_node * );
extern void             dir_free( dir_node *, bool );
extern void             dir_add_table( dir_node * );
extern void             dir_remove_table( dir_node * );

extern int              SizeFromMemtype( memtype, int );
extern ret_code         MemtypeFromSize( int, memtype * );
extern ret_code         GetLangType( int *, lang_type * );

//extern uint             GetExtIdx( struct asm_sym * ); /* Get the index of an extrn defn */

extern const typeinfo   *FindToken( const char *token, const typeinfo *, int );
extern int              FindStdType( int );  // find a predefined type
//extern int              RegisterValueToIndex( int, bool *);
extern int              SizeFromRegister( int );
extern ret_code         EchoDef( int );         // handle ECHO directive
extern ret_code         OptionDirective( int ); // handle OPTION directive

extern void             pushitem( void *stack, void *elt );
extern void             *popitem( void *stack );
//extern void             *peekitem( void *stack, int );
//extern ret_code         cpu_directive( int i );
extern ret_code         SetCPU( enum asm_cpu );

/* make a forward declaration for struct code_info. */
struct code_info;

extern ret_code         directive( int, struct code_info * );

#endif
