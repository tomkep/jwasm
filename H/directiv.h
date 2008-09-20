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

#include "omfrec.h"

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
    TAB_FIRST = 0,
    TAB_SEG = TAB_FIRST,  // order seg, grp is important
    TAB_GRP,
    TAB_PUB,
    TAB_LIB,
    TAB_EXT,
    TAB_PROC,
    TAB_ALIAS,
    TAB_LAST,
    TAB_TYPE,
    TAB_GLOBAL,  // TAB_GLOBAL and TAB_COMM are in fact TAB_EXT items
    TAB_COMM
};           

typedef enum {
    SEGTYPE_UNDEF,
    SEGTYPE_CODE,
    SEGTYPE_DATA,
    SEGTYPE_BSS,
    SEGTYPE_ABS
} seg_type;

/*---------------------------------------------------------------------------*/

typedef struct stacknode {
    void    *next;
    void    *elt;
} stacknode;

typedef struct {
    int token;
    memtype mem_type;
    int size;
    ofssize ofs_size;
} simpletype;

extern simpletype SimpleType[];

typedef struct {
      char      *string;        // the token string
      uint      value;          // value connected to this token
      uint      init;           // explained in direct.c ( look at SegDef() )
} typeinfo;

extern typeinfo TypeInfo[];

/*---------------------------------------------------------------------------*/
/* Structures for grpdef, segdef, externdef, pubdef, included library,       */
/* procedure and symbolic integer constants.                                 */
/*---------------------------------------------------------------------------*/

typedef struct seg_item {
    struct seg_item     *next;
    struct dir_node     *seg;
} seg_item;

typedef struct {
    direct_idx          idx;            // its group index
    seg_item            *seglist;       // list of segments in the group
    uint                numseg;         // number of segments in the group
    direct_idx          lname_idx;      // LNAME index (OMF only)
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
        struct fixup        *FixupListHead; // head of list of fixups
        struct asmfixup     *FixupListHeadCoff;
        struct asmfixup     *FixupListHeadElf;
    };
    union {
        struct fixup        *FixupListTail;
        struct asmfixup     *FixupListTailCoff;
        struct asmfixup     *FixupListTailElf;
    };
    void                *LinnumQueue;   // for COFF line numbers
    union {
        uint_32         num_relocs;     // used by COFF/ELF
        uint_32         fileoffset;     // used by BIN
    };
    seg_type            segtype;        // segment is belonging to "CODE" or 'DATA' class
    direct_idx          lname_idx;      // LNAME index (OMF only)
    unsigned            readonly:1;     // if the segment is readonly
    unsigned            Use32:1;
#if BIN_SUPPORT
    unsigned            initial:1;      // for BIN output
#endif
} seg_info;

typedef struct regs_list {
    struct regs_list    *next;
    char                *reg;
} regs_list;

typedef struct label_list {
    struct label_list   *next;
    asm_sym             *sym;           // symbol for this param/local
    unsigned            is_vararg:1;    // if it is a VARARG param
    unsigned            is_ptr:1;       // if it is a PTR type
    unsigned            is_far:1;       // if ptr is FAR
    unsigned            is32:1;         // if offset is 32 bit (ptr only)
} label_list;

typedef struct {
    regs_list           *regslist;      // list of registers to be saved
    label_list          *paralist;      // list of parameters
    label_list          *locallist;     // list of local variables
    struct asm_sym      *labellist;     // list of local labels
    int                 parasize;       // total no. of bytes used by parameters
    int                 localsize;      // total no. of bytes used by local variables
    unsigned            is_vararg:1;    // if it has a vararg
    unsigned            pe_type:1;      // prolog/epilog code type 0:8086/186 1:286 and above
    unsigned            export:1;       // EXPORT procedure
    unsigned            init:1;         // has ExamineProc() been called?
} proc_info;

// macro parameter

typedef struct mparm_list {
    char                *label;         // name of parameter
    char                *def;           // optional default parm
    unsigned int        required:1;     // is parm required (REQ)
} mparm_list;

// macro local

typedef struct mlocal_list {
    struct mlocal_list  *next;
    char                *label;         // name of local
} mlocal_list;

typedef struct asmlines {
    struct asmlines     *next;
    char                *line;
    char                parmcount;
} asmlines;

typedef struct  fname_list {
        struct  fname_list *next;
        time_t  mtime;
        char    *name;
        char    *fullname;
} FNAME;

typedef struct {
    uint_32             parmcnt;    /* no of params */
    mparm_list          *parmlist;  /* array of parameter items */
    mlocal_list         *locallist; // list of locals
    asmlines            *data;      // the guts of the macro - LL of strings
    const FNAME         *srcfile;
} macro_info;


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
    unsigned            isInline:1;
    unsigned            isOpen:1;    /* set until the matching ENDS is found */
    unsigned            OrgInside:1; /* struct contains an ORG */
} struct_info;

union entry {
    seg_info            *seginfo;       // SEGMENT definition
    grp_info            *grpinfo;       // GROUP definition
    proc_info           *procinfo;      // PROC definition
    struct_info         *structinfo;    // STRUCT, UNION, TYPEDEF, RECORD def
    macro_info          *macroinfo;     // MACRO definition
};

typedef struct dir_node {
    struct asm_sym      sym;
    union entry         e;
    unsigned long       line_num;     // line number of the directive in source file
    struct dir_node     *next, *prev; // linked list of this type of symbol
} dir_node;         // List of grpdef, segdef, pubdef, externdef, included lib
                    // and symbolic integer constants.

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
    dist_type           distance;        // stack distance;
    mod_type            model;           // memory model;
    lang_type           langtype;        // language;
    os_type             ostype;          // operating system;
    seg_order           segorder;        // .alpha, .seq, .dosseg
    short               cpu;             // cpu setting;
    unsigned            use32:1;         // If 32-bit segment is used
    unsigned            cmdline:1;       // memory model set by cmdline opt?
    unsigned            defUse32:1;      // default segment size 32-bit
    unsigned            mseg:1;          // mixed segments (16/32-bit)
    unsigned            nocasemap:1;     // option casemap:none
    unsigned            procs_private:1; // option proc:private
    unsigned            procs_export:1;  // option proc:export
    unsigned            ljmp:1;          // option ljmp
    unsigned            m510:1;          // option m510
    unsigned            scoped:1;        // option scoped
    unsigned            oldstructs:1;    // option oldstructs
    unsigned            emulator:1;      // option emulator
    unsigned            list:1;          // .list/.nolist
    unsigned            cref:1;          // .cref/.nocref
    unsigned            setif2:1;        // option setif2
    unsigned            flat_idx;        // index of FLAT group
    char                name[_MAX_FNAME];// name of module
    const FNAME         *srcfile;
} module_info;                           // Information about the module

#define MAGIC_FLAT_GROUP        ModuleInfo.flat_idx

extern module_info      ModuleInfo;

#define IS_PROC_FAR()   ( ModuleInfo.model == MOD_MEDIUM || ModuleInfo.model == MOD_LARGE || ModuleInfo.model == MOD_HUGE )

/*---------------------------------------------------------------------------*/

extern dir_node         *dir_insert( const char *, int );
extern dir_node         *dir_insert_ex( const char *, int );
extern void             dir_change( dir_node *, int );
extern void             dir_free( dir_node *, bool );

extern int              SizeFromMemtype(memtype, bool );

extern uint             GetExtIdx( struct asm_sym * );
/* Get the index of an extrn defn */

extern int              token_cmp( char *token, int start, int end );
extern int              FindSimpleType( int );  // find simple type
//extern int              RegisterValueToIndex( int, bool *);
extern int              SizeFromRegister( int );
extern struct asm_sym   *MakeExtern( char *name, memtype type, struct asm_sym * vartype, struct asm_sym *, bool );
extern int              EchoDef( int );         // handle ECHO directive
extern int              OptionDirective( int ); // handle OPTION directive

extern int              SetModel( int );        // handle .MODEL statement

extern void             ModuleInit( void );
/* Initializes the information about the module, which are contained in
   ModuleInfo */

extern int              ModuleEnd( int );       // handle END statement
extern int              FixOverride( int );
/* Get the correct frame and frame_datum for a label when there is a segment
   or group override. */
extern void             push( void *stack, void *elt );
extern void             *pop( void *stack );
extern void             *peek( void *stack, int );
extern void             wipe_space( char *token );
extern void             SetMasm510( bool );

/*---------------------------------------------------------------------------
 *   included from segment.c
 *---------------------------------------------------------------------------*/

extern seg_item         *CurrSeg;       // points to stack of opened segments

extern int              directive( int , long );
extern uint_32          GetCurrSegStart(void);
/* Get offset of segment at the start of current LEDATA record */

#define GetSeg( x )     (dir_node *)x->segment

#define SEGISCODE( x )  ( x->seg->e.seginfo->segtype == SEGTYPE_CODE )

extern void             SetSymSegOfs( struct asm_sym * );
/* Store location information about a symbol */
extern int              SymIs32( struct asm_sym * );
extern int              SimSeg( int );          // handle simplified segment


extern direct_idx       GetLnameIdx( char * );

extern uint_32          GetCurrOffset( void );  // Get offset from current segment
extern int              SetCurrOffset( int_32, bool, bool );

extern dir_node         *GetCurrSeg( void );
/* Get current segment; NULL means none */

extern int              GetCurrClass( void ); /* get curr segment's class index */

extern uint             GetGrpIdx( struct asm_sym * );
/* get symbol's group index, from the symbol itself or from the symbol's segment */

extern uint             GetSegIdx( struct asm_sym * );
/* get symbol's segment index, from the symbol itself */


extern int              GrpDef( int );          // define a group
extern int              SegDef( int );          // open or close a segment
extern int              SetCurrSeg( int );      // open or close a segment in
                                                // the second pass
extern void             SegmentInit( int );     // init segments
extern struct asm_sym   *GetGrp( struct asm_sym * );

extern uint_32          GetCurrSegAlign( void );
extern int              SetUse32Def( bool );

// input.c

extern const FNAME      *get_curr_srcfile( void );

#endif
