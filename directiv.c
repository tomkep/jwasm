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
* Description:  Processing of assembly directives.
*
****************************************************************************/


#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"
#include "queues.h"
#include "equate.h"
#include "fixup.h"
#include "mangle.h"
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "types.h"
#include "condasm.h"
#include "hll.h"
#include "macro.h"
#include "proc.h"
#include "fastpass.h"

#include "myassert.h"

#define ONEXMM 1

/* prototypes */
extern int              OrgDirective( int );
extern int              AlignDirective( uint_16, int );
extern const FNAME      *AddFlist( char const *filename );

extern int              SegmentModulePrologue(int type);
extern int              SegmentModuleEnd(void);
extern void             DefineFlatGroup( void );
extern void             set_def_seg_name( void );

#define INIT_ALIGN      0x1
#define INIT_COMBINE    0x2
#define INIT_USE        0x4
#define INIT_CLASS      0x8

#define INIT_MODEL      0x10
#define INIT_STACK      0x20

enum {
#undef fix
#define fix( tok, str, val, init )              tok

#include "dirtoken.h"
};

extern typeinfo TypeInfo[] = {

#undef fix
#define fix( tok, string, value, init_val )     { string, value, init_val }

#include "dirtoken.h"
};

extern simpletype SimpleType[] = {
    { T_FINAL,   MT_EMPTY,       0  , OFSSIZE_EMPTY},
    { T_BYTE,    MT_BYTE,        1  , OFSSIZE_EMPTY},
    { T_SBYTE,   MT_SBYTE,       1  , OFSSIZE_EMPTY},
    { T_WORD,    MT_WORD,        2  , OFSSIZE_EMPTY},
    { T_SWORD,   MT_SWORD,       2  , OFSSIZE_EMPTY},
    { T_DWORD,   MT_DWORD,       4  , OFSSIZE_EMPTY},
    { T_SDWORD,  MT_SDWORD,      4  , OFSSIZE_EMPTY},
    { T_REAL4,   MT_DWORD,       4  , OFSSIZE_EMPTY},
    { T_FWORD,   MT_FWORD,       6  , OFSSIZE_EMPTY},
    { T_QWORD,   MT_QWORD,       8  , OFSSIZE_EMPTY},
    { T_REAL8,   MT_QWORD,       8  , OFSSIZE_EMPTY},
    { T_TBYTE,   MT_TBYTE,      10  , OFSSIZE_EMPTY},
    { T_REAL10,  MT_TBYTE,      10  , OFSSIZE_EMPTY},
    { T_OWORD,   MT_OWORD,      16  , OFSSIZE_EMPTY},
    { T_PTR,     MT_PTR,        -1  , OFSSIZE_EMPTY},
    { T_NEAR,    MT_NEAR,       -1  , OFSSIZE_EMPTY},
    { T_NEAR16,  MT_NEAR,        2  , OFSSIZE_16   },
    { T_NEAR32,  MT_NEAR,        4  , OFSSIZE_32   },
    { T_FAR,     MT_FAR,        -1  , OFSSIZE_EMPTY},
    { T_FAR16,   MT_FAR,         4  , OFSSIZE_16   },
    { T_FAR32,   MT_FAR,         6  , OFSSIZE_32   },
    { T_PROC,    MT_PROC,       -1  , OFSSIZE_EMPTY}
//    { T_ABS,     MT_ABS,         0  }
};

static char             *Check4Mangler( int *i );
static void             ModelAssumeInit( void );

extern bool             EndDirectiveFound;
extern struct asm_sym   *SegOverride;

obj_rec                 *ModendRec;     // Record for Modend (OMF)
asm_sym                 *start_label;   // for COFF

// this is for the segment registers:
// DS=0, ES=1, SS=2, FS=3, GS=4, CS=5

assume_info      SegAssumeTable[NUM_SEGREGS];

// this is for the standard registers:
// (E)AX=0, (E)CX=1, (E)DX=2, (E)BX=3
// (E)SP=4, (E)BP=5, (E)SI=6, (E)DI=7

static assume_info      StdAssumeTable[NUM_STDREGS];

#if FASTPASS
static assume_info saved_SegAssumeTable[NUM_SEGREGS];
static assume_info saved_StdAssumeTable[NUM_STDREGS];
#endif

symbol_queue            Tables[TAB_LAST];// tables of definitions
module_info             ModuleInfo;

/* startup code for 8086 */

static char *StartupDosNear0[] = {
        " mov     dx,DGROUP",
        " mov     ds,dx",
        " mov     bx,ss",
        " sub     bx,dx",
        " shl     bx,1",
        " shl     bx,1",
        " shl     bx,1",
        " shl     bx,1",
        " cli     ",
        " mov     ss,dx",
        " add     sp,bx",
        " sti     ",
        NULL
};

/* startup code for 80186+ */

static char *StartupDosNear1[] = {
        " mov     ax,DGROUP",
        " mov     ds,ax",
        " mov     bx,ss",
        " sub     bx,ax",
        " shl     bx,4",
        " mov     ss,ax",
        " add     sp,bx",
        NULL
};

static char *StartupDosFar[] = {
        " mov     dx,DGROUP",
        " mov     ds,dx",
        NULL
};
static char *ExitOS2[] = { /* mov al, retval  followed by: */
        " xor ah, ah",
        " push 01h",
        " push ax",
        " call far DOSEXIT",
        NULL
};
static char *ExitDos[] = {
        " mov     ah,4ch",
        " int     21h",
        NULL
};

static char *StartAddr = "@Startup";
static char StartupDirectiveFound;

typedef struct _context {
    uint_32 type;
    char data[];
} context;

static context          *ContextStack;  // for PUSHCONTEXT/POPCONTEXT

       uint             segdefidx;      // Number of Segment definition
static uint             grpdefidx;      // Number of Group definition

static asm_sym *sym_CodeSize  = { NULL }; /* numeric */
static asm_sym *sym_DataSize  = { NULL }; /* numeric */
static asm_sym *sym_Model     = { NULL }; /* numeric */
       asm_sym *sym_Interface = { NULL }; /* numeric */
       asm_sym *sym_Cpu       = { NULL }; /* numeric */
       asm_sym *sym_CurSeg    = { NULL }; /* text    */


#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

#ifdef DEBUG_OUT
#ifdef __WATCOMC__
/* the C heap isn't really used anymore due to speed issues. */
/* so this heap check function has become pretty useless */
void heap( char *func )
/*********************/
{
    switch(_heapchk()) {
    case _HEAPBADNODE:
    case _HEAPBADBEGIN:
    DebugMsg(("Function : %s - ", func ));
        DebugMsg(("ERROR - heap is damaged\n"));
        exit(1);
        break;
    default:
        break;
    }
}
#endif
#endif

void IdxInit( void )
/******************/
{
    DebugMsg(("IdxInit: lnames, segment+group idx reset to 0\n"));
    LnamesIdx   = 0;
    segdefidx   = 0;
    grpdefidx   = 0;

    memset(Tables, 0, sizeof(Tables));

}

void push( void *stk, void *elt )
/*******************************/
{
    void        **stack = stk;
    stacknode   *node;

    node = AsmAlloc( sizeof( stacknode ));
    node->next = *stack;
    node->elt = elt;
    *stack = node;
}

void *pop( void *stk )
/********************/
{
    void        **stack = stk;
    stacknode   *node;
    void        *elt;

    node = (stacknode *)(*stack);
    *stack = node->next;
    elt = node->elt;
    AsmFree( node );
    return( elt );
}

void *peek( void *stk, int level )
/********************/
{
    stacknode   *node = (stacknode *)(stk);

    for (;node && level;level--) {
        node = node->next;
    }

    if (node)
        return( node->elt );
    else
        return( NULL );
}

static void dir_add( dir_node *new, int tab )
/*******************************************/
{
    /* note: this is only for those above which do NOT return right away */
    /* put the new entry into the queue for its type of symbol */
    if( Tables[tab].head == NULL ) {
        Tables[tab].head = Tables[tab].tail = new;
        new->next = new->prev = NULL;
    } else {
        new->prev = Tables[tab].tail;
        Tables[tab].tail->next = new;
        Tables[tab].tail = new;
        new->next = NULL;
    }
}

static void dir_init( dir_node *dir, int tab )
/********************************************/
/* Change node and insert it into the table specified by tab */
{
    struct asm_sym      *sym;

    sym = &dir->sym;

    dir->line_num = LineNumber;
    dir->next = dir->prev = NULL;

    switch( tab ) {
    case TAB_SEG:
        sym->state = SYM_SEG;
        dir->e.seginfo = AsmAlloc( sizeof( seg_info ) );
        dir->e.seginfo->segrec = ObjNewRec( CMD_SEGDEF );
        dir->e.seginfo->group = NULL;
        dir->e.seginfo->num_relocs = 0;
        dir->e.seginfo->lname_idx = 0;
        dir->e.seginfo->labels = NULL;
        dir->e.seginfo->Use32 = ModuleInfo.defUse32;
        dir->e.seginfo->readonly = FALSE;
        dir->e.seginfo->segtype = SEGTYPE_UNDEF;
        dir->e.seginfo->LinnumQueue = NULL;
        dir->e.seginfo->FixupListHead = NULL;
        dir->e.seginfo->FixupListTail = NULL;
        dir->e.seginfo->CodeBuffer = NULL;
        dir->e.seginfo->start_loc = 0;
        dir->e.seginfo->current_loc = 0;
        dir->e.seginfo->segrec->d.segdef.align = ALIGN_PARA;
        dir->e.seginfo->segrec->d.segdef.combine = COMB_INVALID;
        /* null class name, in case none is mentioned */
        dir->e.seginfo->segrec->d.segdef.class_name_idx = 1;
        dir->e.seginfo->segrec->d.segdef.seg_length = 0;
        dir->e.seginfo->segrec->d.segdef.access_valid = FALSE;
        break;
    case TAB_GRP:
        sym->state = SYM_GRP;
        dir->e.grpinfo = AsmAlloc( sizeof( grp_info ) );
        dir->e.grpinfo->idx = ++grpdefidx;
        dir->e.grpinfo->seglist = NULL;
        dir->e.grpinfo->numseg = 0;
        dir->e.grpinfo->lname_idx = 0;
        break;
    case TAB_GLOBAL: /* EXTERNDEF */
        sym->state = SYM_EXTERNAL;
        sym->use32 = Use32;
        sym->comm = 0;
        sym->weak = 1;
        tab = TAB_EXT;
        break;
    case TAB_EXT: /* EXTERN, EXTRN */
        sym->state = SYM_EXTERNAL;
        sym->use32 = Use32;
        sym->comm = 0;
        sym->weak = 0;
        break;
    case TAB_COMM: /* COMM */
        sym->state = SYM_EXTERNAL;
        sym->use32 = Use32;
        sym->comm = 1;
        sym->weak = 0;
        sym->isfar = 0;
        tab = TAB_EXT;
        break;
    case TAB_PROC:
        sym->state = SYM_PROC;
        dir->e.procinfo = AsmAlloc( sizeof( proc_info ) );
        dir->e.procinfo->regslist = NULL;
        dir->e.procinfo->paralist = NULL;
        dir->e.procinfo->locallist = NULL;
        dir->e.procinfo->labellist = NULL;
        dir->e.procinfo->parasize = 0;
        dir->e.procinfo->localsize = 0;
        dir->e.procinfo->is_vararg = FALSE;
        dir->e.procinfo->pe_type = FALSE;
        dir->e.procinfo->defined = FALSE;
        dir->e.procinfo->export = FALSE;
        dir->e.procinfo->init = FALSE;
        break;
    case TAB_MACRO:
        sym->state = SYM_MACRO;
        dir->e.macroinfo = AsmAlloc( sizeof( macro_info ) );
        dir->e.macroinfo->parmlist = NULL;
        dir->e.macroinfo->locallist = NULL;
        dir->e.macroinfo->data = NULL;
        dir->e.macroinfo->srcfile = NULL;
        sym->vararg = FALSE;
        sym->isfunc = FALSE;
        return;
    case TAB_CLASS_LNAME:
    case TAB_LNAME:
        sym->state = ( tab == TAB_LNAME ) ? SYM_LNAME : SYM_CLASS_LNAME;
        dir->e.lnameinfo = AsmAlloc( sizeof( lname_info ) );
        dir->e.lnameinfo->idx = ++LnamesIdx;
        // fixme
        return;
    case TAB_PUB:
        sym->public = TRUE;
        return;
    case TAB_TYPE:
        sym->state = SYM_TYPE;
        dir->e.structinfo = AsmAlloc( sizeof( struct_info ) );
        dir->e.structinfo->head = NULL;
        dir->e.structinfo->tail = NULL;
        dir->e.structinfo->alignment = 0;
        dir->e.structinfo->typekind = TYPE_NONE;
        dir->e.structinfo->isInline = FALSE;
        dir->e.structinfo->isOpen = FALSE;
        dir->e.structinfo->OrgInside = FALSE;
        return;
    case TAB_LIB:
        break;
    case TAB_ALIAS:
        sym->state = SYM_ALIAS;
        break;
    default:
        // unknown table
        /**/myassert( 0 );
        break;
    }
    dir_add( dir, tab );
    return;
}

static void RemoveFromTable( dir_node *dir )
/******************************************/
{
    int tab;

    if( dir->prev)
        dir->prev->next = dir->next;
    if( dir->next )
        dir->next->prev = dir->prev;

    switch( dir->sym.state ) {
    case SYM_EXTERNAL:
        tab = TAB_EXT;
        break;
    case SYM_SEG:
        tab = TAB_SEG;
        break;
    case SYM_GRP:
        tab = TAB_GRP;
        break;
    case SYM_PROC:
        tab = TAB_PROC;
        break;
    default:
        dir->next = NULL;
        dir->prev = NULL;
        return;
    }

    if (dir->next == NULL)
        dir->next = dir->prev;

    if (Tables[tab].head == dir)
        Tables[tab].head = dir->next;
    if (Tables[tab].tail == dir)
        Tables[tab].tail = dir->next;

    dir->prev = NULL;
    dir->next = NULL;
}

void dir_change( dir_node *dir, int tab )
/***************************************/
/* Change node type and insert it into the table specified by tab */
{
    dir_free( dir, TRUE );
    dir_init( dir, tab );
}

dir_node *dir_insert( const char *name, int tab )
/***********************************************/
/* Insert a node into the table specified by tab */
{
    dir_node            *new;

    /**/myassert( name != NULL );

    if (*name)
        new = (dir_node *)SymCreate( name, TRUE );
    else
        new = (dir_node *)SymCreate( name, FALSE );

    if( new != NULL )
        dir_init( new, tab );

    return( new );
}

// alloc a dir_node and do NOT put it into the symbol table
// used for (temporary) externals created for PROTO items,
// nested STRUCTs, class names, ...

dir_node *dir_insert_ex( const char *name, int tab )
{
    dir_node            *new;

    /**/myassert( name != NULL );

    new = (dir_node *)SymCreate( name, FALSE );
    if( new != NULL )
        dir_init( new, tab );

    return( new );
}

// free a dir_node

void dir_free( dir_node *dir, bool remove_from_table )
/****************************/
{
    int i;

    switch( dir->sym.state ) {
    case SYM_GRP:
        {
            seg_item    *segcurr;
            seg_item    *segnext;

            segcurr = dir->e.grpinfo->seglist;
            if( segcurr != NULL ) {
                for( ;; ) {
                    segnext = segcurr->next;
                    AsmFree( segcurr );
                    if( segnext == NULL )
                        break;
                    segcurr = segnext;
                }
            }
            AsmFree( dir->e.grpinfo );
        }
        break;
    case SYM_SEG:
        if( dir->e.seginfo->segrec != NULL )
            ObjKillRec( dir->e.seginfo->segrec );
        AsmFree( dir->e.seginfo );
        break;
    case SYM_EXTERNAL:
        dir->sym.first_size = 0;
        break;
    case SYM_LNAME:
    case SYM_CLASS_LNAME:
        AsmFree( dir->e.lnameinfo );
        break;
    case SYM_PROC:
        {
            label_list  *labelcurr;
            label_list  *labelnext;
            regs_list   *regcurr;
            regs_list   *regnext;
            asm_sym     *symcurr;
            asm_sym     *symnext;

            labelcurr = dir->e.procinfo->paralist;
            if( labelcurr != NULL ) {
                for( ;labelcurr; ) {
                    labelnext = labelcurr->next;
                    SymFree( labelcurr->sym );
                    /* AsmFree( labelcurr->replace ); */
                    AsmFree( labelcurr );
                    labelcurr = labelnext;
                }
            }

            labelcurr = dir->e.procinfo->locallist;
            if( labelcurr != NULL ) {
                for( ;labelcurr; ) {
                    labelnext = labelcurr->next;
                    SymFree( labelcurr->sym );
                    /* AsmFree( labelcurr->replace ); */
                    AsmFree( labelcurr );
                    labelcurr = labelnext;
                }
            }

            for(symcurr = dir->e.procinfo->labellist;
                symcurr; symcurr = symnext) {
                symnext = symcurr->next;
                SymFree( symcurr );
            }

            regcurr = dir->e.procinfo->regslist;
            if( regcurr != NULL ) {
                for( ;regcurr; ) {
                    regnext = regcurr->next;
                    AsmFree( regcurr->reg );
                    AsmFree( regcurr );
                    regcurr = regnext;
                }
            }
            AsmFree( dir->e.procinfo );
        }
        break;
    case SYM_MACRO:
        {
            mlocal_list     *localcurr;
            mlocal_list     *localnext;
            asmlines        *datacurr;
            asmlines        *datanext;

            /* free the parm list */
            ;
            for( i = 0 ; i < dir->e.macroinfo->parmcnt; i++ ) {
                AsmFree( dir->e.macroinfo->parmlist[i].label );
                AsmFree( dir->e.macroinfo->parmlist[i].def );
            }
            if( dir->e.macroinfo->parmlist )
                AsmFree( dir->e.macroinfo->parmlist );

            /* free the local list */
            ;
            for( localcurr = dir->e.macroinfo->locallist ;localcurr; ) {
                localnext = localcurr->next;
                AsmFree( localcurr->label );
                AsmFree( localcurr );
                localcurr = localnext;
            }

            /* free the lines list */
            for(datacurr = dir->e.macroinfo->data ;datacurr; ) {
                datanext = datacurr->next;
                AsmFree( datacurr->line );
                AsmFree( datacurr );
                datacurr = datanext;
            }
            AsmFree( dir->e.macroinfo );
        }
        break;
    case SYM_TMACRO:
        if (dir->sym.predefined == FALSE) {
            AsmFree(dir->sym.string_ptr);
            dir->sym.string_ptr = NULL;
        }
        break;
    case SYM_TYPE:
        {
            field_list      *ptr;
            field_list      *next;

            for( ptr = dir->e.structinfo->head; ptr != NULL; ptr = next ) {
                /* bitfields field names are global, don't free them here! */
                if ( dir->e.structinfo->typekind != TYPE_RECORD )
                    SymFree( ptr->sym );
                AsmFree( ptr->initializer );
                AsmFree( ptr->value );
                next = ptr->next;
                AsmFree( ptr );
            }
            AsmFree( dir->e.structinfo );
        }
        break;
    default:
        break;
    }
    if (remove_from_table == TRUE)
        RemoveFromTable( dir);
}


void wipe_space( char *token )
/****************************/
/* wipe out the spaces at the beginning of a token */
{
    char        *start;

    if( token == NULL )
        return;
    if( strlen( token ) == 0 )
        return;

    for( start = token;; start++ ){
        if( *start != ' ' && *start != '\t' ) {
            break;
        }
    }
    if( start == token )
        return;

    memmove( token, start, strlen( start ) + 1 );
}

uint checkword( char **token )
/***********************************/
/* wipes out prceding and tailing spaces, and make sure token contains only
   one word */
{
    char        *ptrhead;
    char        *ptrend;

    /* strip the space in the front */
    for( ptrhead = *token; ; ptrhead++ ) {
        if( ( *ptrhead != ' ' ) && ( *ptrhead != '\t' ) ) {
            break;
        }
    }

    /* Then search for the first ending space */
    ptrend = strchr( ptrhead, ' ' );
    if( ptrend == NULL ) {
        ptrend = strchr( ptrhead, '\t' );
    }
    if( ptrend == NULL ) {
        ptrend = strchr( ptrhead, '\n' );
    }

    /* Check if there is any letters following that ending space */
    if( ptrend != NULL ) {
        *ptrend = '\0';
        ptrend++;
        while( *ptrend != '\0' ) {
            if( ( *ptrend != ' ' ) && ( *ptrend != '\t' ) && ( *ptrend != '\n' ) ) {
                return( ERROR );
            }
            ptrend++;
        }
    }

    *token = ptrhead;
    return( NOT_ERROR );
}

static int GetLangType( int *i )
/******************************/
{
    if( AsmBuffer[*i]->token == T_RES_ID) {
        switch( AsmBuffer[(*i)++]->value ) {
        case T_C:
            return( LANG_C );
        case T_BASIC:
            return( LANG_BASIC );
        case T_FORTRAN:
            return( LANG_FORTRAN );
        case T_PASCAL:
            return( LANG_PASCAL );
        case T_WATCOM_C:
            return( LANG_WATCOM_C );
        case T_STDCALL:
            return( LANG_STDCALL );
        case T_SYSCALL:
            return( LANG_SYSCALL );
        default:
            (*i)--;
            break;
        }
    }
    return( ModuleInfo.langtype );
}

// returns an index into SimpleType table

int FindSimpleType(int token)
{
    int i;
    for( i = 0; i <= sizeof(SimpleType)/sizeof(simpletype); i++ ) {
        if( token == SimpleType[i].token )
            return( i );
    }
    return(ERROR);
}

// get size from memory type
// is32 param used only for MT_NEAR/MT_FAR

int SizeFromMemtype( memtype type, bool is32 )
/******************************/
{
    int  size;

    switch( type ) {
    case MT_BYTE:
    case MT_SBYTE:
        return( 1 );
    case MT_WORD:
    case MT_SWORD:
        return( 2 );
    case MT_DWORD:
    case MT_SDWORD:
        return( 4 );
    case MT_FWORD:
        return( 6 );
    case MT_QWORD:
        return( 8 );
    case MT_TBYTE:
        return( 10 );
    case MT_OWORD:
        return( 16 );
    case MT_NEAR:
        return (is32 ? 4 : 2);
    case MT_FAR:
        return ((is32 ? 4 : 2) + 2);
    case MT_PROC:
        return(0);
    case MT_PTR:
        /* first determine offset size */
        size = ( Use32 ?  4 : 2);
        if( (ModuleInfo.model == MOD_COMPACT)
         || (ModuleInfo.model == MOD_LARGE)
         || (ModuleInfo.model == MOD_HUGE) ) {
            size += 2;      /* add segment for far data pointers */
        }
        return( size );
    default:
        return( ERROR );
    }
}

// externdef [ attr ] symbol:type
// called during Pass 1 only

static int ExterndefDirective( int i )
/********************/
{
    char                *token;
    char                *mangle_type = NULL;
    char                *typetoken;
    memtype             mem_type;
    int                 index;
    bool                is32 = Use32;
    int                 size;
    struct asm_sym      *sym;
    struct asm_sym      *symtype = NULL;
    dir_node            *dir;
    int                 lang_type;

    DebugMsg(("ExterndefDirective entry\n"));

    mangle_type = Check4Mangler( &i );
    for( ; i < Token_Count; ) {

        /* get the symbol language type if present */
        lang_type = GetLangType( &i );

        /* get the symbol name */
        token = AsmBuffer[i++]->string_ptr;

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        typetoken = AsmBuffer[i]->string_ptr;
        mem_type = MT_ERROR;
        if (AsmBuffer[i]->token == T_ID) {
            if (0 == stricmp(AsmBuffer[i]->string_ptr, "ABS"))
                mem_type = MT_ABS;
        } else if (AsmBuffer[i]->token == T_RES_ID ||
                   /* T_PROC/T_PROTO are directives */
                   AsmBuffer[i]->token == T_DIRECTIVE) {
            index = FindSimpleType( AsmBuffer[i]->value);
            if (index != ERROR) {
                if (SimpleType[index].ofs_size != OFSSIZE_EMPTY)
                    is32 = (SimpleType[index].ofs_size == OFSSIZE_32);
                mem_type = SimpleType[index].mem_type;
            } else if (AsmBuffer[i]->value == T_PROTO) {
                /* dont scan this line further */
                return (ProtoDef(i, token));
            }
        }

        if( mem_type == MT_ERROR ) {
            if( !(symtype = IsLabelType( AsmBuffer[i]->string_ptr ) ) ) {
                AsmError( INVALID_QUALIFIED_TYPE );
                return( ERROR );
            }
            mem_type = MT_TYPE;
        }
        /* if it is a pointer to something (not VOID),
         an anonymous TYPEDEF has to be created
         */
        if (mem_type == MT_PTR && AsmBuffer[i+1]->token != T_FINAL) {
            if ((symtype = CreateTypeDef("", &i)) == NULL)
                return (ERROR);
            DebugMsg(("ExterndefDirective(%s): CreateTypeDef()=%X\n", token, symtype));
            mem_type = MT_TYPE;
        }

        while (AsmBuffer[i]->token == T_ID || AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE)
            i++;

        sym = SymSearch( token );
        if (!sym) {
            dir = dir_insert( token, TAB_GLOBAL);
            sym = &dir->sym;
            DebugMsg(("ExterndefDirective(%s): new symbol\n", token));
        } else {
            dir = (dir_node *)sym;
            if (sym->state == SYM_UNDEFINED)
                dir_change( dir, TAB_GLOBAL);
            DebugMsg(("ExterndefDirective(%s): symbol exists, state=%u\n", token, sym->state));
        }

        sym->defined = TRUE;

        /* ensure that the type of the symbol won't change */

        if (sym->state == SYM_EXTERNAL && sym->mem_type == MT_EMPTY) {
            DebugMsg(("ExterndefDirective: type set for >%s<\n", sym->name));
            if (mem_type != MT_ABS)
                SetSymSegOfs(sym);
            else if (sym->weak == TRUE)
                sym->equate = TRUE; /* allow redefinition by EQU, = */
            sym->offset = 0;
            sym->use32 = is32;
            sym->mem_type = mem_type;
            if (mem_type == MT_TYPE) {
                sym->type = symtype;
                sym->total_size = symtype->total_size;
            } else {
                size = SizeFromMemtype(mem_type, is32);
                if (size != ERROR)
                    sym->total_size = size;
            }
        } else if (sym->mem_type != mem_type) {
            /* if the symbol is already defined (as SYM_INTERNAL), Masm
             won't display an error. The other way, first externdef and
             then the definition, will make Masm complain, however */
            DebugMsg(("ExterndefDirective: type conflict for %s. mem_types: %u - %u ; %u - %u\n", sym->name, sym->mem_type, mem_type));
            AsmWarn( 1, SYMBOL_TYPE_CONFLICT, sym->name );
        } else if (sym->mem_type == MT_TYPE && sym->type != symtype) {
            asm_sym *sym2 = sym;
            /* skip alias types and compare the base types */
            DebugMsg(("ExterndefDirective(%s): types differ: %X (%s) - %X (%s)\n", sym->name, sym->type, sym->type->name, symtype, symtype->name));
            while (sym2->type)
                sym2 = sym2->type;
            while (symtype->type)
                symtype = symtype->type;
            if (sym2 != symtype) {
                DebugMsg(("ExterndefDirective(%s): type conflict: %X (%s) - %X (%s)\n", sym->name, sym2, sym2->name, symtype, symtype->name));
                AsmWarn( 1, SYMBOL_TYPE_CONFLICT, sym->name );
            }
        }

        // FIXME !! symbol can have different language type
        SetMangler( sym, mangle_type, lang_type );

        /* write a global entry if none has been written yet */
        if (sym->state == SYM_EXTERNAL && sym->weak == FALSE)
            ;// skip EXTERNDEF if a real EXTERN/COMM was done
        else if (sym->global == FALSE) {
            sym->global = TRUE;
            DebugMsg(("writing a global entry for %s\n", sym->name));
            AddGlobalData( dir );
        }

        if (AsmBuffer[i]->token != T_FINAL && AsmBuffer[i]->token != T_COMMA) {
            AsmError(EXPECTING_COMMA);
            return(ERROR);
        }
        i++;

    } /* end for */
    return( NOT_ERROR );
}

// handle ECHO
// displays text on the console

int EchoDef( int i )
{
#if 0
    for (;i < Token_Count;i++)
        printf("%s ", AsmBuffer[i]->string_ptr);
    printf("\n");
#else
    printf("%s\n", AsmBuffer[i-1]->pos + 5);
#endif
    return( NOT_ERROR );
}

// set Masm v5.1 compatibility options

void SetMasm510(bool value)
{
    ModuleInfo.m510 = value;
    ModuleInfo.scoped = !value;
    ModuleInfo.oldstructs = value;
    ModuleInfo.setif2 = value;
    return;
}

// helper for EXTERN directive

asm_sym *MakeExtern( char *name, memtype mem_type, struct asm_sym * vartype, asm_sym * sym, bool is32)
/********************************************************************/
{
    dir_node        *ext;

    if (sym != NULL) {
        ext = (dir_node *)sym;
        dir_change( ext, TAB_EXT );
    } else {
        ext = dir_insert( name, TAB_EXT );
        if( ext == NULL ) {
            return( NULL );
        }
        sym = &ext->sym;
    }
    if (mem_type != MT_ABS)
        SetSymSegOfs( sym );

    sym->offset = 0;
    sym->defined = TRUE;
    sym->mem_type = mem_type;
    if (mem_type != MT_TYPE) {
        int size = SizeFromMemtype(mem_type, is32);
        sym->use32 = is32;
        if (size != ERROR)
            sym->total_size = size;
    } else
        sym->total_size = vartype->total_size;
    sym->type = vartype;
    return( sym );
}

static int ExternDef( int i )
/*****************/
{
    char                *token;
    char                *mangle_type = NULL;
    char                *typetoken;
    int                 type;
    memtype             mem_type;
    bool                is32 = Use32;
    struct asm_sym      *sym;
    struct asm_sym      *symtype = NULL;
    int                 lang_type;

    mangle_type = Check4Mangler( &i );
    for( ; i < Token_Count; i++ ) {

        /* get the symbol language type if present */
        lang_type = GetLangType( &i );

        /* get the symbol name */
        token = AsmBuffer[i++]->string_ptr;

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        type = ERROR;
        typetoken = AsmBuffer[i]->string_ptr;
        if (AsmBuffer[i]->token == T_ID) {
            if (0 == stricmp(AsmBuffer[i]->string_ptr, "ABS")) {
                type = 0;
                mem_type = MT_ABS;
            }
            /* T_PROC is a directive */
        } else if (AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE) {
            type = FindSimpleType( AsmBuffer[i]->value );
            if (type != ERROR) {
                mem_type = SimpleType[type].mem_type;
                if (SimpleType[type].ofs_size != OFSSIZE_EMPTY)
                    is32 = (SimpleType[type].ofs_size == OFSSIZE_32);
            }
        }

        if( type == ERROR ) {
            if( !(symtype = IsLabelType( typetoken ) ) ) {
                AsmError( INVALID_QUALIFIED_TYPE );
                return( ERROR );
            }
            mem_type = MT_TYPE;
        }

        for( ; i< Token_Count && AsmBuffer[i]->token != T_COMMA; i++ );

        sym = SymSearch( token );

        if( sym == NULL || sym->state == SYM_UNDEFINED) {
            if (sym && sym->public == TRUE) {
                AsmErr(CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name);
                return( ERROR );
            }
            if(( sym = MakeExtern( token, mem_type, symtype, sym, is32 )) == NULL )
                return( ERROR );
        } else if (sym->state != SYM_EXTERNAL) {
            DebugMsg(("ExternDef: symbol redefinition\n"));
            AsmErr( SYMBOL_REDEFINITION, token );
            return( ERROR );
        } else if( sym->mem_type != mem_type ) {
            AsmError( EXT_DEF_DIFF );
            return( ERROR );
        }

        SetMangler( sym, mangle_type, lang_type );
    }
    return( NOT_ERROR );
}

static char *Check4Mangler( int *i )
/**********************************/
{
    char *mangle_type = NULL;

    if( AsmBuffer[*i]->token == T_STRING ) {
        mangle_type = AsmBuffer[*i]->string_ptr;
        (*i)++;
        if( AsmBuffer[*i]->token != T_COMMA ) {
            AsmWarn( 2, EXPECTING_COMMA );
        } else {
            (*i)++;
        }
    }
    return( mangle_type );
}

static int PublicDirective( int i )
/*****************/
{
    char                *mangle_type = NULL;
    char                *token;
    struct asm_sym      *sym;
    dir_node            *dir;
    int                 lang_type;

    mangle_type = Check4Mangler( &i );
    for( ; i < Token_Count; i+=2 ) {

        /* get the symbol language type if present */
        lang_type = GetLangType( &i );

        /* get the symbol name */
        token = AsmBuffer[i]->string_ptr;

        /* Add the public name */
        if( checkword( &token ) == ERROR ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }

        sym = SymSearch( token );
        if (Parse_Pass != PASS_1 && (sym == NULL || sym->state == SYM_UNDEFINED)) {
            AsmErr( SYMBOL_NOT_DEFINED, sym->name );
            return(ERROR);
        }
        if( sym != NULL ) {
            dir = (dir_node *)sym;
            if (sym->state != SYM_UNDEFINED &&
                sym->state != SYM_INTERNAL &&
                sym->state != SYM_EXTERNAL &&
                sym->state != SYM_PROC) {
                AsmErr(CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name);
                return(ERROR);
            }
            if (sym->state == SYM_INTERNAL && sym->local == TRUE) {
                AsmErr( CANNOT_DECLARE_SCOPED_CODE_LABEL_AS_PUBLIC, sym->name );
                return(ERROR);
            }
            if (sym->state == SYM_EXTERNAL && sym->weak != TRUE) {
                DebugMsg(("PublicDirective: symbol redefinition\n"));
                AsmErr( SYMBOL_REDEFINITION, sym->name );
                return(ERROR);
            }
            if( Parse_Pass == PASS_1 && sym->public == FALSE) {
                /* put it into the pub table */
                sym->public = TRUE;
                AddPublicData( dir );
            }
        } else if (Parse_Pass == PASS_1) {
            dir = dir_insert( token, TAB_PUB );
            AddPublicData( dir );
            sym = &dir->sym;
        }
        if (Parse_Pass == PASS_1)
            SetMangler( sym, mangle_type, lang_type );
    }
    return( NOT_ERROR );
}

int token_cmp( char *token, int start, int end )
/******************************************************/
/* compare token against those specified in TypeInfo[ start...end ] */
{
    int         i;
    char        *tkn;

    for( i = start; i <= end; i++ ) {
        tkn = TypeInfo[i].string;
        if( tkn == NULL )
            continue;
        if( stricmp( tkn, token ) == 0 ) {
            // type is found
            return( i );
        }
    }
    return( ERROR );        // No type is found
}

// INCLUDE directive
// if a full path is specified, the directory where the included file
// is located becomes the "source" directory, that is, it is searched
// FIRST if further INCLUDE directives are found inside the included file.

static int IncludeDirective( int i )
/******************/
{
    switch( AsmBuffer[i]->token ) {
    case T_ID:
    case T_STRING:
    case T_PATH:
        return( InputQueueFile( AsmBuffer[i]->string_ptr ) );
    default:
        AsmError( EXPECTED_FILE_NAME );
        return( ERROR );
    }
}

// called during pass 1

static int IncludeLibDirective( int i )
/*********************/
{
    char *name;
    struct asm_sym *sym;

    name = AsmBuffer[i]->string_ptr;
    if( name == NULL ) {
        AsmError( LIBRARY_NAME_MISSING );
        return( ERROR );
    }

    /* good idea to have the libs in the symbol namespace? Doubt it. */

    sym = SymSearch( name );
    if( sym == NULL ) {
        // fixme
        if( dir_insert( name, TAB_LIB ) == NULL ) {
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}

// .STARTUP and .EXIT

static int Startup( int i )
/******************/
{
    /* handles .STARTUP directive */

    int         count;
    char        buffer[ MAX_LINE_LEN ];

    if( ModuleInfo.model == MOD_NONE ) {
        AsmError( MODEL_IS_NOT_DECLARED );
        return( ERROR );
    }
    ModuleInfo.cmdline = FALSE;

    PushLineQueue();

    switch( AsmBuffer[i]->value ) {
    case T_DOT_STARTUP:
        count = 0;
        if (ModuleInfo.model == MOD_TINY)
            InputQueueLine( "org 100h" );
        strcpy( buffer, StartAddr );
        strcat( buffer, "::" );
        InputQueueLine( buffer );
        if( ModuleInfo.ostype == OPSYS_DOS ) {
            char **p;
            if (ModuleInfo.model == MOD_TINY)
                ;
            else if( ModuleInfo.distance == STACK_NEAR ) {
                if ( (ModuleInfo.cpu & 0x7F) <= 1)
                    p = StartupDosNear0;
                else
                    p = StartupDosNear1;
                while( *p != NULL ) {
                    InputQueueLine( *p );
                    p++;
                }
            } else {
                while( StartupDosFar[count] != NULL ) {
                    InputQueueLine( StartupDosFar[count++] );
                }
            }
        }
        StartupDirectiveFound = TRUE;
        break;
    case T_DOT_EXIT:
        i++;
        count = 0;
        if( ( AsmBuffer[i]->string_ptr != NULL )
            && ( *(AsmBuffer[i]->string_ptr) != '\0' ) ) {
            if( ModuleInfo.ostype == OPSYS_DOS ) {
                sprintf( buffer, "mov ax,4C00h+(%s and 0FFh)", AsmBuffer[i]->string_ptr);
            } else
                sprintf( buffer, "mov ax, %s", AsmBuffer[i]->string_ptr);
            InputQueueLine( buffer );
            count++;
        }
        if( ModuleInfo.ostype == OPSYS_DOS ) {
            while( ExitDos[count] != NULL ) {
                InputQueueLine( ExitDos[count++] );
            }
        } else {
            while( ExitOS2[count] != NULL ) {
                InputQueueLine( ExitOS2[count++] );
            }
        }
        break;
    default:
        break;
    }
    return( NOT_ERROR );
}

static asm_sym * AddPredefinedConstant( char *name, int value)
/*******************************************************/
{
    asm_sym * sym = CreateConstant(name, value, -1, TRUE);
    if (sym)
        sym->predefined = TRUE;
    return(sym);
}

static void AddPredefinedText( char *name, char *value)
{
    asm_sym *sym;

    sym = SymSearch( name );
    if (sym == NULL)
        sym = SymCreate( name, TRUE );
    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->predefined = TRUE;
    sym->string_ptr = value;
}

// called by ModelDirective(), in pass one only!

static void module_prologue( int type )
/*************************************/
/* Generates codes for .MODEL; based on optasm pg.142-146 */
{
    int         value;
    char        *textvalue;
    asm_sym     *sym;

    PushLineQueue();
    SegmentModulePrologue(type);
    ModelAssumeInit();

    if (Parse_Pass != PASS_1)
        return;

    /* Set @CodeSize */
    switch( type ) {
    case MOD_MEDIUM:
    case MOD_LARGE:
    case MOD_HUGE:
        value = 1;
        break;
    default:
        value = 0;
        break;
    }
    sym_CodeSize = AddPredefinedConstant( "@CodeSize", value );
    AddPredefinedText( "@code", Options.text_seg);

    /* Set @DataSize */
    switch( type ) {
    case MOD_COMPACT:
    case MOD_LARGE:
        value = 1;
        break;
    case MOD_HUGE:
        value = 2;
        break;
    default:
        value = 0;
        break;
    }
    sym_DataSize = AddPredefinedConstant( "@DataSize", value );

    if (type == MOD_FLAT)
        textvalue = "FLAT";
    else
        textvalue = "DGROUP";

    AddPredefinedText( "@data", textvalue );

    if (ModuleInfo.distance == STACK_FAR)
        textvalue = "STACK";
    AddPredefinedText( "@stack", textvalue );

    /* Set @Model and @Interface */

    sym_Model     = AddPredefinedConstant( "@Model", ModuleInfo.model );
    sym_Interface = AddPredefinedConstant( "@Interface", ModuleInfo.langtype );
}

// called by write_init(), once per source module
// symbol table has been initialized

void ModuleInit( void )
/*********************/
{
    ModuleInfo.error_count = 0;
    ModuleInfo.warning_count = 0;
    ModuleInfo.model = MOD_NONE;
    ModuleInfo.distance = STACK_NONE;
    ModuleInfo.langtype = Options.langtype;
    ModuleInfo.ostype = OPSYS_DOS;
    ModuleInfo.use32 = FALSE;
    ModuleInfo.defUse32 = FALSE;
    ModuleInfo.cmdline = FALSE;
    ModuleInfo.mseg = FALSE;
    // these values are initialized for each pass in ProcInit()
    // ModuleInfo.procs_private = FALSE;
    // ModuleInfo.procs_export = FALSE;
    ModuleInfo.nocasemap = Options.nocasemap;
    ModuleInfo.ljmp = TRUE;
    ModuleInfo.emulator = (Options.floating_point == DO_FP_EMULATION);
    SymSetCmpFunc(ModuleInfo.nocasemap);
    SetMasm510(Options.masm51_compat);
    ModuleInfo.list = Options.write_listing;
    ModuleInfo.cref = TRUE;
    ModuleInfo.segorder = SEGORDER_SEQ;
    ModuleInfo.flat_idx = 0;
    ModuleInfo.name[0] = NULLC;
    // add source file to autodependency list
    ModuleInfo.srcfile = AddFlist( AsmFiles.fname[ASM] );

    StartupDirectiveFound = FALSE;

    ContextStack = NULL;

    sym_CurSeg = SymCreate("@CurSeg", TRUE );
    sym_CurSeg->state = SYM_TMACRO;
    sym_CurSeg->defined = TRUE;
    sym_CurSeg->predefined = TRUE;
}

static void get_module_name( void )
/*********************************/
{
    char dummy[_MAX_EXT];
    char        *p;

    /**/myassert( AsmFiles.fname[ASM] != NULL );
    _splitpath( AsmFiles.fname[ASM], NULL, NULL, ModuleInfo.name, dummy );
    for( p = ModuleInfo.name; *p != '\0'; ++p ) {
        if( !( isalnum( *p ) || ( *p == '_' ) || ( *p == '$' )
            || ( *p == '@' ) || ( *p == '?') ) ) {
            /* it's not a legal character for a symbol name */
            *p = '_';
        }
    }
    /* first character can't be a number either */
    if( isdigit( ModuleInfo.name[0] ) ) {
        ModuleInfo.name[0] = '_';
    }
}

// handle .model directive

static int ModelDirective( int i )
/****************/
{
    char        *token;
    int         initstate = 0;
    uint        type;           // type of option

    if( Parse_Pass != PASS_1 ) {
        module_prologue( ModuleInfo.model );
        return( NOT_ERROR );
    }

    if( ModuleInfo.model != MOD_NONE && !ModuleInfo.cmdline ) {
        AsmError( MODEL_DECLARED_ALREADY );
        return( ERROR );
    }
    ModuleInfo.cmdline = FALSE;

    get_module_name();

    for( i++; i < Token_Count; i++ ) {

        token = AsmBuffer[i]->string_ptr;
        wipe_space( token );
        /* Add the public name */

        // look up the type of token
        type = token_cmp( token, TOK_TINY, TOK_FARSTACK );
        if( type == ERROR ) {
            type = token_cmp( token, TOK_PROC_BASIC, TOK_PROC_SYSCALL );
            if( type == ERROR ) {
                type = token_cmp( token, TOK_OS_OS2, TOK_OS_DOS );
                if( type == ERROR ) {
                    AsmError( UNDEFINED_MODEL_OPTION );
                    return( ERROR );
                }
            }
        }
#if 0
        /* this is NOT Masm compatible */
        MakeConstantUnderscored( AsmBuffer[i]->value );
#endif
        if( initstate & TypeInfo[type].init ) {
            AsmError( MODEL_PARA_DEFINED ); // initialized already
            return( ERROR );
        } else {
            initstate |= TypeInfo[type].init; // mark it initialized
        }
        switch( type ) {
        case TOK_FLAT:
            DefineFlatGroup();
            SetUse32Def( TRUE );
            // fall through
        case TOK_TINY:
        case TOK_SMALL:   /* near code, near data */
        case TOK_MEDIUM:  /* far code, near data */
            ModuleInfo.distance = STACK_NEAR;
        case TOK_COMPACT: /* near data, far code */
        case TOK_LARGE:
        case TOK_HUGE:
            ModuleInfo.model = TypeInfo[type].value;
            set_def_seg_name();
            break;
        case TOK_FARSTACK:
            if (ModuleInfo.model == MOD_FLAT) {
                AsmError(INVALID_MODEL_PARAM_FOR_FLAT);
                break;
            }
        case TOK_NEARSTACK:
            ModuleInfo.distance = TypeInfo[type].value;
            break;
        case TOK_PROC_BASIC:
        case TOK_PROC_FORTRAN:
        case TOK_PROC_PASCAL:
        case TOK_PROC_C:
        case TOK_PROC_WATCOM_C:
        case TOK_PROC_STDCALL:
        case TOK_PROC_SYSCALL:
            ModuleInfo.langtype = TypeInfo[type].value;
            break;
        case TOK_OS_DOS:
        case TOK_OS_OS2:
            ModuleInfo.ostype = TypeInfo[type].value;
            break;
        }
        i++;

        /* go past comma */
        if( ( i < Token_Count ) && ( AsmBuffer[i]->token != T_COMMA ) ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
    }

    if( ( initstate & INIT_MODEL ) == 0 ) {
        AsmError( NO_MEMORY_MODEL_FOUND );
        return( ERROR );
    }

    module_prologue( ModuleInfo.model );
    ModuleInfo.cmdline = (LineNumber == 0);
    return(NOT_ERROR);
}

void AssumeInit( )
/*********************/
{
    int reg;

    for( reg = 0; reg < NUM_SEGREGS; reg++ ) {
        SegAssumeTable[reg].symbol = NULL;
        SegAssumeTable[reg].error = FALSE;
        SegAssumeTable[reg].flat = FALSE;
    }
    for( reg = 0; reg < NUM_STDREGS; reg++ ) {
        StdAssumeTable[reg].symbol = NULL;
        StdAssumeTable[reg].error = FALSE;
    }
#if FASTPASS
    if (Parse_Pass != PASS_1) {
        memcpy(&SegAssumeTable, &saved_SegAssumeTable, sizeof(SegAssumeTable));
        memcpy(&StdAssumeTable, &saved_StdAssumeTable, sizeof(StdAssumeTable));
    }
#endif
}
#if FASTPASS
void AssumeSaveState( )
{
    memcpy(&saved_SegAssumeTable, &SegAssumeTable, sizeof(SegAssumeTable));
    memcpy(&saved_StdAssumeTable, &StdAssumeTable, sizeof(StdAssumeTable));
}
#endif
static void ModelAssumeInit( void )
/**********************************/
{
    char *pCS;
    char buffer[ MAX_LINE_LEN ];

    /* Generates codes for assume */
    switch( ModuleInfo.model ) {
    case MOD_FLAT:
        InputQueueLine( "ASSUME CS:FLAT,DS:FLAT,SS:FLAT,ES:FLAT,FS:ERROR,GS:ERROR");
        break;
    case MOD_TINY:
    case MOD_SMALL:
    case MOD_COMPACT:
    case MOD_MEDIUM:
    case MOD_LARGE:
    case MOD_HUGE:
        if (ModuleInfo.model == MOD_TINY)
            pCS = "DGROUP";
        else
            pCS = Options.text_seg;

        strcpy( buffer, "ASSUME CS:" );
        strcat( buffer, pCS);
        strcat( buffer, ", DS:DGROUP" );
        if (ModuleInfo.distance != STACK_FAR)
            strcat( buffer, ", SS:DGROUP" );
        InputQueueLine( buffer );
        break;
    }
}

// convert a standard register token to an index.

static int RegisterValueToIndex(int reg, bool * is32)
{
    int j;
    static int std32idx[NUM_STDREGS] = {T_EAX, T_ECX, T_EDX, T_EBX, T_ESP, T_EBP, T_ESI, T_EDI};
    static int std16idx[NUM_STDREGS] = {T_AX, T_CX, T_DX, T_BX, T_SP, T_BP, T_SI, T_DI};

    for (j = 0; j < NUM_STDREGS; j++)
        if (std32idx[j] == reg) {
            *is32 = TRUE;
            return(j);
        } else if (std16idx[j] == reg) {
            *is32 = FALSE;
            return(j);
        }
    return(ERROR);
}

struct asm_sym * GetStdAssume(int reg)
{
    return (StdAssumeTable[reg].symbol);
}

static int AssumeDirective( int i )
/********************/
/* Handles ASSUME statement
 syntax is :
 assume segregister : seglocation [, segregister : seglocation ]
 assume dataregister : qualified type [, dataregister : qualified type ]
 assume register : ERROR | NOTHING | FLAT
 assume NOTHING
 */
{
    int             segloc; /* lcoation of segment/type info */
    int             reg;
    int             j;
    int             type;
    assume_info     *info;
    struct asm_sym  *sym;
    bool            segtable;
    bool            is32;
    static int segidx[NUM_SEGREGS] = {T_DS, T_ES, T_SS, T_FS, T_GS, T_CS};

    for( i++; i < Token_Count; i++ ) {
        if( ( AsmBuffer[i]->token == T_ID )
            && (0 == stricmp(AsmBuffer[i]->string_ptr, "NOTHING" ))) {
            AssumeInit();
            continue;
        }

        if (AsmBuffer[i]->token != T_REG) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        reg = AsmBuffer[i]->value;

        i++;

        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        /*---- get the info ptr for the register ----*/

        info = NULL;
        for (j = 0; j < NUM_SEGREGS; j++)
            if (segidx[j] == reg) {
                info = &SegAssumeTable[j];
                if( ( ( curr_cpu & P_CPU_MASK ) < P_386 )
                    && ( ( reg == T_FS ) || ( reg == T_GS ) ) ) {
                    AsmError( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                    return( ERROR );
                }
                segtable = TRUE;
                break;
            }

        if (info == NULL) {
            segtable = FALSE;
            /* convert T_xxx to an index */
            j = RegisterValueToIndex(reg, &is32);
            if (j != ERROR) {
                if(is32 == TRUE && ( curr_cpu & P_CPU_MASK ) < P_386 ) {
                    AsmError( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                    return( ERROR );
                }
                info = &StdAssumeTable[j];
            }
        }
        if (info == NULL) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }

        if (segtable == TRUE) {
            if( ( AsmBuffer[i]->token == T_UNARY_OPERATOR )
                && ( AsmBuffer[i]->value == T_SEG ) ) {
                i++;
            }
        } else if( ( AsmBuffer[i]->token == T_RES_ID )
                   && ( AsmBuffer[i]->value == T_PTR ) )
            i++;

        segloc = i;
        i++;
        if( AsmBuffer[segloc]->string_ptr == '\0' ) {
            AsmError( SEGLOCATION_EXPECTED );
            return( ERROR );
        }

        /*---- Now store the information ----*/

        if( 0 == stricmp( AsmBuffer[segloc]->string_ptr, "ERROR" )) {
            info->error = TRUE;
            info->flat = FALSE;
            info->symbol = NULL;
        } else if(0 == stricmp( AsmBuffer[segloc]->string_ptr, "FLAT" )) {
            if( ( curr_cpu & P_CPU_MASK ) < P_386 ) {
                AsmError( REGISTER_NOT_ACCEPTED_IN_CURRENT_CPU_MODE );
                return( ERROR );
            } else if (segtable == FALSE) {
                AsmError( SYNTAX_ERROR );
                return( ERROR );
            };
            DefineFlatGroup();
            info->flat = TRUE;
            info->error = FALSE;
            info->symbol = NULL;
        } else if( 0 == stricmp( AsmBuffer[segloc]->string_ptr, "NOTHING" )) {
            info->flat = FALSE;
            info->error = FALSE;
            info->symbol = NULL;
        } else {
            if (segtable == FALSE) {
                type = ERROR;
                sym = NULL;
                if (AsmBuffer[segloc]->token == T_RES_ID) {
                    type = FindSimpleType(AsmBuffer[segloc]->value);
                }
                if (type == ERROR) {
                    sym = IsLabelType(AsmBuffer[segloc]->string_ptr);
                    if (sym == NULL) {
                        AsmErr( QUALIFIED_TYPE_EXPECTED, segloc );
                        return( ERROR );
                    }
                }
            } else {
                sym = SymLookup( AsmBuffer[segloc]->string_ptr );
                if( sym == NULL)
                    return( ERROR );
                if ( ( Parse_Pass != PASS_1 ) && ( sym->state == SYM_UNDEFINED ) ) {
                    AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[segloc]->string_ptr );
                    return( ERROR );
                }
            }
            info->symbol = sym;
            info->flat = FALSE;
            info->error = FALSE;
        }

        /* go past comma */
        if( ( i < Token_Count ) && ( AsmBuffer[i]->token != T_COMMA ) ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}

static enum assume_segreg search_assume( struct asm_sym *sym,
                         enum assume_segreg def, int override )
/**********************************************************/
{
    if( sym == NULL )
        return( ASSUME_NOTHING );
    if( def != ASSUME_NOTHING ) {
        if( SegAssumeTable[def].symbol != NULL ) {
            if( SegAssumeTable[def].symbol == sym )
                return( def );
            if( !override && ( SegAssumeTable[def].symbol == GetGrp( sym ) ) ) {
                return( def );
            }
        }
    }
    for( def = 0; def < NUM_SEGREGS; def++ ) {
        if( SegAssumeTable[def].symbol == sym ) {
            return( def );
        }
    }
    if( override )
        return( ASSUME_NOTHING );

    for( def = 0; def < NUM_SEGREGS; def++ ) {
        if( SegAssumeTable[def].symbol == NULL )
            continue;
        if( SegAssumeTable[def].symbol == GetGrp( sym ) ) {
            return( def );
        }
    }

    return( ASSUME_NOTHING );
}

// if count == 1, there's NO start address
// if count > 1, there's one.

int process_address(expr_list *);

static int EndDirective( int i )
/************************/
{
    struct fixup        *fixup;
    expr_list           opndx;
    struct asmfixup     *fix;
    asm_sym             *sym;

    /* close open segments */
    SegmentModuleEnd();

    if( StartupDirectiveFound ) {
        if( i > 1 ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        strcat( CurrString, " " );
        strcat( CurrString, StartAddr );
        InputQueueLine( CurrString );
        StartupDirectiveFound = FALSE;
        return( NOT_ERROR );
    }

    EndDirectiveFound = TRUE;

    if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
        return( ERROR );
    }
    if( AsmBuffer[i]->token != T_FINAL) {
        AsmError(SYNTAX_ERROR);
        return( ERROR );
    }

    if( opndx.empty)
        ;
    else if ( opndx.type == EXPR_ADDR ) {
        Modend = TRUE;
        process_address( &opndx );
        fix = InsFixups[0];
        if (fix)
            sym = fix->sym;
        if (fix == NULL || sym == NULL) {
            AsmError( INVALID_START_ADDRESS );
            return( ERROR );
        } else if (sym->state == SYM_INTERNAL || sym->state == SYM_EXTERNAL || sym->state == SYM_PROC) {
            if (sym->mem_type == MT_NEAR || sym->mem_type == MT_FAR || sym->mem_type == MT_PROC)
                ;
            else {
                AsmError( MUST_BE_ASSOCIATED_WITH_CODE );
                return( ERROR );
            }
        } else {
            AsmError( INVALID_START_ADDRESS );
            return( ERROR );
        }
    } else {
        AsmError( INVALID_START_ADDRESS );
        return( ERROR );
    }

    if (Options.output_format == OFORMAT_OMF) {
        ModendRec = ObjNewRec( CMD_MODEND );
        ModendRec->d.modend.main_module = FALSE;

        if( opndx.empty == TRUE ) {
            ModendRec->d.modend.start_addrs = FALSE;
            return( NOT_ERROR );
        }

        ModendRec->d.modend.start_addrs = TRUE;
        ModendRec->d.modend.is_logical = TRUE;
        ModendRec->d.modend.main_module = TRUE;

        fix->offset = sym->offset + opndx.value;

        ModendRec->is_32 = SymIs32(sym);

        fixup = CreateFixupRec( 0 );
        if( fixup == NULL ) {
            return( ERROR );
        }
        ModendRec->d.modend.ref.log = fixup->lr;
    } else {

        if (opndx.empty == TRUE )
            return( NOT_ERROR );

        if (sym->state != SYM_EXTERNAL && sym->public == FALSE) {
            AddPublicData((dir_node *)sym);
            sym->public = TRUE;
        }
        start_label = sym;
    }
    return( NOT_ERROR );
}

/*
 OW Wasm assumed the syntax is 'alias_name ALIAS substitute_name'
 but MASM accepts 'ALIAS <alias_name> = <actual_name>' only!

 <actual_name> is the name which is used in the source (it still must
 be defined somewhere, internal or external!).
 <alias_name> must NOT be defined elsewhere in the source!
*/

static int AliasDirective( int i )
/*******************/
{
    char *tmp;
    asm_sym *sym;

    /* check syntax. note that T_EQU is '=' */
    if (( i != 0 ) ||
        (Token_Count < 4) ||
        (AsmBuffer[i+2]->token != T_DIRECTIVE) ||
        (AsmBuffer[i+2]->value != T_EQU) ||
        (AsmBuffer[i+2]->opcode != 1)) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    i++;

    if ((AsmBuffer[i]->token != T_STRING) &&
        (AsmBuffer[i]->token != T_ID) &&
        (AsmBuffer[i+2]->token != T_STRING) &&
        (AsmBuffer[i+2]->token != T_ID)) {
        AsmError( SYNTAX_ERROR);
        return( ERROR );
    }

    /* make sure <alias_name> isn't defined elsewhere */
    sym = SymSearch(AsmBuffer[i]->string_ptr);
    if (sym != NULL) {
        if (sym->state != SYM_ALIAS || (strcmp(sym->string_ptr, AsmBuffer[i+2]->string_ptr) != 0)) {
            DebugMsg(("AliasDirective: symbol redefinition\n"));
            AsmErr(SYMBOL_REDEFINITION, AsmBuffer[i]->string_ptr);
            return(ERROR);
        }
        /* ignore multiple definitions */
        return(NOT_ERROR);
    }

    if (Parse_Pass != PASS_1) {
        /* for COFF, make sure <actual_name> is defined elsewhere */
        if (Parse_Pass == PASS_2 && Options.output_format == OFORMAT_COFF) {
            sym = SymSearch(AsmBuffer[i+2]->string_ptr);
            if (sym == NULL ||
                (sym->state != SYM_INTERNAL && sym->state != SYM_EXTERNAL && sym->state != SYM_PROC)) {
                AsmErr(SYMBOL_NOT_DEFINED, AsmBuffer[i+2]->string_ptr);
                return(ERROR);
            }
        }
    } else {
        sym = (asm_sym *)dir_insert(AsmBuffer[i]->string_ptr, TAB_ALIAS);
        sym->string_ptr = AsmAlloc( strlen( AsmBuffer[i+2]->string_ptr ) + 1 );
        strcpy( sym->string_ptr, AsmBuffer[i+2]->string_ptr );
    }
    return( NOT_ERROR );
}

// the NAME directive is ignored in Masm v6

static int NameDirective( int i )
/************************/
{
    if( Options.module_name != NULL )
        return( NOT_ERROR );
    i++;

    /* improper use of NAME is difficult to see since it is a nop
     therefore some syntax checks are implemented:
     - no 'name' structs, unions, records, typedefs!
     - no 'name' struct fields!
     - no 'name' segments!
     - no 'name:' label!
     */
    if (StructDef.struct_depth != 0 ||
        (AsmBuffer[i]->token == T_DIRECTIVE &&
         (AsmBuffer[i]->value == T_SEGMENT ||
          AsmBuffer[i]->value == T_STRUCT  ||
          AsmBuffer[i]->value == T_STRUC   ||
          AsmBuffer[i]->value == T_UNION   ||
          AsmBuffer[i]->value == T_TYPEDEF ||
          AsmBuffer[i]->value == T_RECORD)) ||
         AsmBuffer[i]->token == T_COLON) {
        AsmError(SYNTAX_ERROR);
        return( ERROR );
    }

#if 0
    Options.module_name = AsmAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1 );
    strcpy( Options.module_name, AsmBuffer[i]->string_ptr );
    DebugMsg(("NameDirective: set name to >%s<\n", Options.module_name));
#endif
    return( NOT_ERROR );
}

// .RADIX directive
// currently only 10 is accepted

static int RadixDirective( int i )
{
    int start = i;
    expr_list       opndx;

    if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
        return( ERROR );
    if (opndx.type != EXPR_CONST || opndx.string != NULL) {
        AsmError(CONSTANT_EXPECTED);
        return( ERROR );
    }
    if (opndx.value > 16 || opndx.value < 2) {
        AsmError( INVALID_RADIX_TAG );
        return( ERROR );
    }
    /* todo: accept radix other than 10 */
    if (opndx.value != 10) {
        AsmErr( NOT_SUPPORTED, AsmBuffer[start-1]->pos);
        return( ERROR );
    }
    if (AsmBuffer[i]->token != T_FINAL) {
        AsmError(SYNTAX_ERROR);
        return( ERROR );
    }
    return(NOT_ERROR);
}

static int comp_opt( uint direct )
/********************************/
/*
  Compare function for CPU directive
*/
{
    // follow Microsoft MASM
    switch( direct ) {
    case T_DOT_NO87:
        return( P_NO87 );
    case T_DOT_8086:
        return( P_86 );
    case T_DOT_8087:
        return( P_87 );
    case T_DOT_186:
        return( P_186 );
    case T_DOT_286:
        return( P_286 );
    case T_DOT_287:
        return( P_287 );
    case T_DOT_286P:
        return( P_286p );
    case T_DOT_386:
        return( P_386 );
    case T_DOT_387:
        return( P_387 );
    case T_DOT_386P:
        return( P_386p );
    case T_DOT_486:
        return( P_486 );
    case T_DOT_486P:
        return( P_486p );
    case T_DOT_586:
        return( P_586 );
    case T_DOT_586P:
        return( P_586p );
    case T_DOT_686:
        return( P_686 );
    case T_DOT_686P:
        return( P_686p );
    case T_DOT_MMX:
        return( P_MMX );
    case T_DOT_K3D:
        return( P_MMX | P_K3D );
#if ONEXMM
    case T_DOT_XMM:
        return( P_686 | P_MMX | P_SSE | P_SSE2 | P_SSE3 );
#else
    case T_DOT_XMM:
        return( P_MMX | P_SSE );
    case T_DOT_XMM2:
        return( P_MMX | P_SSE | P_SSE2 );
    case T_DOT_XMM3:
        return( P_MMX | P_SSE | P_SSE2 | P_SSE3 );
#endif
    default:
        // not found
        return( EMPTY );
    }
}

static int def_fpu( uint direct )
/********************************/
/*
  get FPU from CPU directive
*/
{
    switch( direct ) {
    case T_DOT_8086:
    case T_DOT_186:
        return( P_87 );
    case T_DOT_286:
    case T_DOT_286P:
        return( P_287 );
    default:
        return( P_387 );
    }
}

#if defined( _STANDALONE_ )
static void MakeCPUConstant( int i )
/**********************************/
{
#if 0
    MakeConstantUnderscored( i );

    switch( i ) {
    // fall right through
    case T_DOT_686P:
    case T_DOT_686:
        MakeConstantUnderscored( T_DOT_686 );
    case T_DOT_586P:
    case T_DOT_586:
        MakeConstantUnderscored( T_DOT_586 );
    case T_DOT_486P:
    case T_DOT_486:
        MakeConstantUnderscored( T_DOT_486 );
    case T_DOT_386P:
    case T_DOT_386:
        MakeConstantUnderscored( T_DOT_386 );
        break;
    case T_DOT_286P:
    case T_DOT_286:
        MakeConstantUnderscored( T_DOT_286 );
    }
#endif
    return;
}
#endif

/* set CPU, FPU parameter in ModuleInfo + curr_cpu */

int cpu_directive( int i )
/************************/
// handles .8086, .80386
{
    int                 temp;

    if( i == T_DOT_NO87 ) {
        curr_cpu &= ~P_FPU_MASK;  // turn off FPU bits
    } else {
        if( ( temp = comp_opt( i ) ) == EMPTY ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        if( temp & P_FPU_MASK ) {
            curr_cpu &= ~P_FPU_MASK;
            curr_cpu |= temp & P_FPU_MASK;           // setup FPU bits
        }

        if (temp & P_CPU_MASK) {
            if (temp & P_EXT_MASK)
                curr_cpu &= ~( P_CPU_MASK );
            else
                curr_cpu &= ~( P_CPU_MASK | P_PM );

            curr_cpu |= temp & ( P_CPU_MASK | P_PM );// setup CPU bits
            curr_cpu &= ~P_FPU_MASK;
            curr_cpu |= def_fpu( i ) & P_FPU_MASK;   // setup FPU bits
        }

        if( temp & P_EXT_MASK ) {
            curr_cpu &= ~P_EXT_MASK;
            curr_cpu |= temp & P_EXT_MASK;
        }
    }
    /* set the MASM compatible @CPU value
     0001: 86
     0002: 186
     0004: 286
     0008: 386
     0010: 486
     0020: Pentium
     0040: PPro
     0080: protected instructions ok
     0100: 87
     0400: 287
     0800: 387
     */

    temp = curr_cpu & P_CPU_MASK;
    switch (temp) {
    case P_186:
        ModuleInfo.cpu = 0x3;
        break;
    case P_286:
        ModuleInfo.cpu = 0x7;
        break;
    case P_386:
        ModuleInfo.cpu = 0xF;
        break;
    case P_486:
        ModuleInfo.cpu = 0x1F;
        break;
    case P_586:
        ModuleInfo.cpu = 0x3F;
        break;
    case P_686:
        ModuleInfo.cpu = 0x5F;
        break;
    default:
        ModuleInfo.cpu = 1;
        break;
    }
    if ( curr_cpu & P_PM )
        ModuleInfo.cpu = ModuleInfo.cpu | 0x80;

    temp = curr_cpu & P_FPU_MASK;
    switch (temp) {
    case P_87:
        ModuleInfo.cpu = ModuleInfo.cpu | 0x100;
        break;
    case P_287:
        ModuleInfo.cpu = ModuleInfo.cpu | 0x500;
        break;
    case P_387:
        ModuleInfo.cpu = ModuleInfo.cpu | 0xD00;
        break;
    }

#if defined( _STANDALONE_ )

    MakeCPUConstant( i );
    if (ModuleInfo.cpu & 0x78)
        SetUse32Def( TRUE );
    else
        SetUse32Def( FALSE );

    /* Set @Cpu */
    /* differs from Codeinfo cpu setting */

    DebugMsg(("cpu_directive: curr_cpu=%X, @Cpu=%X\n", curr_cpu, ModuleInfo.cpu));
    sym_Cpu = CreateConstant( "@Cpu", ModuleInfo.cpu, -1, TRUE);

#endif

    return( NOT_ERROR );
}

static int MakeComm(
    char *name,
    bool already_defd,
    int size,
    int count,
    bool isfar )
/********************/
{
    int i;
    struct asm_sym *sym;

    sym = SymSearch( name );
    if( ( sym != NULL ) && already_defd ) {
        dir_change( (dir_node *)sym, TAB_COMM );
    } else {
        sym = (asm_sym *)dir_insert( name, TAB_COMM );
        if( sym == NULL )
            return( ERROR );
    }
    sym->total_length = count;
    sym->isfar = isfar;

    SetSymSegOfs( sym );
    sym->offset = 0;

    for( i = 0; i <= sizeof(SimpleType)/sizeof(simpletype); i++ ) {
        if( SimpleType[i].size == size ) {
            sym->mem_type = SimpleType[i].mem_type;
            break;
        }
    }
    sym->total_size = count * size;

    return( NOT_ERROR );
}

/* define "communal" items
 syntax:
 COMM [langtype] [NEAR|FAR] label:type[:count]
 */

static int CommDirective( int i )
/******************/
{
    char            *token;
    char            *mangle_type = NULL;
    bool            isfar;
    int             distance;
    int             tmp;
    int             size;
    int             count;
    struct asm_sym  *sym;
    expr_list       opndx;
    int             lang_type;

    mangle_type = Check4Mangler( &i );
    for( ; i < Token_Count; i++ ) {
        count = 1;

        /* get the distance ( near or far ) */
        isfar = FALSE;
        if (AsmBuffer[i]->token == T_RES_ID)
            switch (AsmBuffer[i]->value) {
            case T_FAR:
            case T_FAR16:
            case T_FAR32:
                isfar = TRUE;
                if (ModuleInfo.model == MOD_FLAT) {
                    AsmError(FAR_NOT_ALLOWED_IN_FLAT_MODEL_COMM_VARIABLES);
                    return( ERROR );
                }
            case T_NEAR:
            case T_NEAR16:
            case T_NEAR32:
                i++;
            }

        /* get the symbol language type if present */
        lang_type = GetLangType( &i );

        /* get the symbol name */
        token = AsmBuffer[i++]->string_ptr;

        /* go past the colon */
        if( AsmBuffer[i]->token != T_COLON ) {
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;
        /* the evaluator cannot handle a ':' so scan for one first */
        for (tmp = i; tmp < Token_Count;tmp++)
            if (AsmBuffer[tmp]->token == T_COLON)
                break;
        if (EvalOperand( &i, tmp, &opndx, TRUE ) == ERROR)
            return( ERROR );
        if (opndx.type != EXPR_CONST || opndx.string != NULL) {
            AsmError(CONSTANT_EXPECTED);
            return( ERROR );
        }
        if (opndx.value == 0) {
            AsmError(POSITIVE_VALUE_EXPECTED);
            return( ERROR );
        }
        size = opndx.value;

        if( AsmBuffer[i]->token == T_COLON ) {
            i++;
            /* count */
            if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
                return( ERROR );
            if (opndx.type != EXPR_CONST || opndx.string != NULL) {
                AsmError(CONSTANT_EXPECTED);
                return( ERROR );
            }
            if (opndx.value == 0) {
                AsmError(POSITIVE_VALUE_EXPECTED);
                return( ERROR );
            }
            count = opndx.value;
        }

        sym = SymSearch( token );
        if( sym != NULL ) {
            if( sym->state == SYM_UNDEFINED ) {
                if (sym->public) {
                    AsmErr(CANNOT_DEFINE_AS_PUBLIC_OR_EXTERNAL, sym->name);
                    return( ERROR );
                }
                if( MakeComm( token, TRUE, size, count, isfar) == ERROR )
                    return( ERROR );
            } else {
                if (sym->total_length == 0)
                    tmp = sym->total_size;
                else
                    tmp = sym->total_size / sym->total_length;
                if( size != tmp) {
                    AsmError( EXT_DEF_DIFF );
                    return( ERROR );
                }
            }
        } else {
            if( MakeComm( token, FALSE, size, count, isfar ) == ERROR )
                return( ERROR );
            sym = SymSearch( token );
        }
        SetMangler( sym, mangle_type, lang_type );
    }
    return( NOT_ERROR );
}

int ListingDirective( int directive, int i)
{
    i++;
    switch ( directive ) {
    case T_DOT_LIST:
        if (AsmFiles.file[LST])
            ModuleInfo.list = TRUE;
        break;
    case T_DOT_CREF:
        ModuleInfo.cref = TRUE;
        break;
    case T_DOT_NOLIST:
    case T_DOT_XLIST:
        ModuleInfo.list = FALSE;
        break;
    case T_DOT_NOCREF:
    case T_DOT_XCREF:
        ModuleInfo.cref = FALSE;
        break;
    case T_DOT_LISTALL:
    case T_DOT_LISTIF:
    case T_DOT_LFCOND: /* .LFCOND is synonym for .LISTIF */
    case T_DOT_LISTMACRO:
    case T_DOT_XALL:   /* .XALL is synonym for .LISTMACRO */
    case T_DOT_LISTMACROALL:
    case T_DOT_LALL:   /* .LALL is synonym for .LISTMACROALL */
    case T_DOT_NOLISTIF:
    case T_DOT_SFCOND: /* .SFCOND is synonym for .NOLISTIF */
    case T_DOT_NOLISTMACRO:
    case T_DOT_SALL:   /* .SALL is synonym for .NOLISTMACRO */
    case T_DOT_TFCOND: /* .TFCOND toggles .LFCOND, .SFCOND */
        if (Parse_Pass == PASS_1)
            AsmWarn( 4, IGNORING_DIRECTIVE, AsmBuffer[i]->string_ptr );
        break;
    case T_PAGE:
    default: /* TITLE, SUBTITLE, SUBTTL */
        /* tiny syntax check to ensure that these directives
         aren't used as code labels */
        if (AsmBuffer[i]->token == T_COLON) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        while (AsmBuffer[i]->token != T_FINAL) i++;
    }

    if (AsmBuffer[i]->token != T_FINAL) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    return( NOT_ERROR );
}

// PUSHCONTEXT and POPCONTEXT directives
// RADIX context is dummy

#define CONT_ASSUMES 0
#define CONT_RADIX   1
#define CONT_LISTING 2
#define CONT_CPU     3
#define CONT_ALL     4

typedef struct _assumes_context {
    assume_info SegAssumeTable[NUM_SEGREGS];
    assume_info StdAssumeTable[NUM_STDREGS];
} assumes_context;

typedef struct _listing_context {
    unsigned list:1;
    unsigned cref:1;
} listing_context;

typedef struct _cpu_context {
    short cpu;          /* saved ModuleInfo.cpu */
    enum asm_cpu cicpu; /* saved CodeInfo.cpu   */
} cpu_context;

typedef struct _radix_context {
    uint_8 radix; /* there's no radix to save/restore yet */
} radix_context;

typedef struct _all_context {
    assumes_context ac;
    radix_context   rc;
    listing_context lc;
    cpu_context     cc;
} all_context;

static int ContextDirective( int directive, int i )
{
    int type;
    context *pcontext;
    assumes_context *acontext = NULL;
    listing_context *lcontext = NULL;
    cpu_context     *ccontext = NULL;
    radix_context   *rcontext = NULL;
    all_context     *alcontext;

    static char *context[] = { "ASSUMES", "RADIX", "LISTING", "CPU", "ALL", NULL };

    i++;
    if (AsmBuffer[i]->token == T_ID) {
        char **p;
        for (p = context, type = CONT_ASSUMES; *p ; p++, type++) {
            if (stricmp(*p, AsmBuffer[i]->string_ptr) == 0) {
                i++;
                if (AsmBuffer[i]->token == T_FINAL) {
                    if (directive == T_POPCONTEXT) {
                        /* for POPCONTEXT, check if the proper item is pushed */
                        pcontext = peek( ContextStack, 0 );
                        if (pcontext == NULL || pcontext->type != type) {
                            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i-2]->pos);
                            return( ERROR );
                        }
                        // get the item
                        pcontext = pop( &ContextStack );

                        // restore the values
                        switch (type) {
                        case CONT_ASSUMES:
                            acontext = (assumes_context *)&pcontext->data;
                            break;
                        case CONT_RADIX:
                            rcontext = (radix_context *)&pcontext->data;
                            break;
                        case CONT_LISTING:
                            lcontext = (listing_context *)&pcontext->data;
                            break;
                        case CONT_CPU:
                            ccontext = (cpu_context *)&pcontext->data;
                            break;
                        case CONT_ALL:
                            alcontext = (all_context *)&pcontext->data;
                            acontext = &alcontext->ac;
                            rcontext = &alcontext->rc;
                            lcontext = &alcontext->lc;
                            ccontext = &alcontext->cc;
                        }
                        if (acontext) {
                            memcpy(SegAssumeTable, acontext->SegAssumeTable, sizeof(SegAssumeTable));
                            memcpy(StdAssumeTable, acontext->StdAssumeTable, sizeof(StdAssumeTable));
                        }
                        if (rcontext) {
                        }
                        if (lcontext) {
                            ModuleInfo.list = lcontext->list;
                            ModuleInfo.cref = lcontext->cref;
                        }
                        if (ccontext) {
                            ModuleInfo.cpu     = ccontext->cpu;
                            if (sym_Cpu)
                                sym_Cpu->value = ccontext->cpu;
                            curr_cpu = ccontext->cicpu;
                        }
                        // remove the item
                        AsmFree(pcontext);
                    } else {
                        // setup a context item
                        switch (type) {
                        case CONT_ASSUMES:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(assumes_context));
                            acontext = (assumes_context *)&pcontext->data;
                            break;
                        case CONT_RADIX:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(radix_context));
                            rcontext = (radix_context *)&pcontext->data;
                            break;
                        case CONT_LISTING:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(listing_context));
                            lcontext = (listing_context *)&pcontext->data;
                            break;
                        case CONT_CPU:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(cpu_context));
                            ccontext = (cpu_context *)&pcontext->data;
                            break;
                        case CONT_ALL:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(all_context));
                            alcontext = (all_context *)pcontext->data;
                            acontext = &alcontext->ac;
                            rcontext = &alcontext->rc;
                            lcontext = &alcontext->lc;
                            ccontext = &alcontext->cc;
                            break;
                        }

                        pcontext->type = type;

                        if (acontext) {
                            memcpy(acontext->SegAssumeTable, SegAssumeTable, sizeof(SegAssumeTable));
                            memcpy(acontext->StdAssumeTable, StdAssumeTable, sizeof(StdAssumeTable));
                        }
                        if (rcontext) {
                        }
                        if (lcontext) {
                            lcontext->list = ModuleInfo.list;
                            lcontext->cref = ModuleInfo.cref;
                        }
                        if (ccontext) {
                            ccontext->cpu   = ModuleInfo.cpu;
                            ccontext->cicpu = curr_cpu;
                        }
                        push( &ContextStack, pcontext );
                    }
                    return( NOT_ERROR );
                }
                break;
            }
        }
    }
    AsmError( SYNTAX_ERROR );
    return( ERROR );
}
static int SegOrderDirective(int directive, int i)
{
    if (Options.output_format != OFORMAT_OMF &&
        Options.output_format != OFORMAT_BIN) {
        if (Parse_Pass == PASS_1)
            AsmWarn( 2, DIRECTIVE_IGNORED_FOR_COFF, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    switch( directive ) {
    case T_DOT_DOSSEG:
    case T_DOSSEG:
        ModuleInfo.segorder = SEGORDER_DOSSEG;
        break;
    case T_DOT_ALPHA:
        ModuleInfo.segorder = SEGORDER_ALPHA;
        break;
    default:
        ModuleInfo.segorder = SEGORDER_SEQ;
        break;
    }
    if (AsmBuffer[i+1]->token != T_FINAL) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    return( NOT_ERROR );
}

// dispatcher for directives

int directive( int i, long direct )
{
//    int ret;

    switch( direct ) {
#if FASTPASS
    case T_EQU:
        /* bit 0 of opcode is set if it's the '=' directive */
        DefineConstant( AsmBuffer[1]->opcode & 1 );
        return( NOT_ERROR );
    case T_SIZESTR:
        SizeStrDef( 1 );
        directive_listed = TRUE;
        return( NOT_ERROR );
    case T_INSTR:
        InStrDef( 1, CurrString );
        directive_listed = TRUE;
        return( NOT_ERROR );
#endif
    case T_PUBLIC:
        return( PublicDirective(i+1) );
    case T_ENDS:
        if( StructDef.struct_depth != 0 ) {
            return( StructDirective( i ) );
        }
        // else fall through to T_SEGMENT
    case T_SEGMENT:
        return( Parse_Pass == PASS_1 ? SegDef(i-1) : SetCurrSeg(i-1) );
    case T_GROUP:
        return( GrpDef(i-1) );
    case T_PROTO:
        return( ProtoDef(i-1, NULL ));
    case T_PROC:
        return( ProcDef(i-1) );
    case T_ENDP:
        return( ProcEnd(i-1) );
    case T_DOT_CODE:
    case T_DOT_STACK:
    case T_DOT_DATA:
    case T_DOT_DATA_UN:
    case T_DOT_FARDATA:
    case T_DOT_FARDATA_UN:
    case T_DOT_CONST:
        return( SimSeg(i) );
    case T_OPTION:
        return( OptionDirective(i+1) );
    case T_TYPEDEF:
        return( Parse_Pass == PASS_1 ? TypeDef(i-1) : NOT_ERROR );
    case T_INVOKE:
        return( InvokeDef(i+1) );
    case T_DOT_IF:
    case T_DOT_WHILE:
    case T_DOT_REPEAT:
        return( HllStartDef(i) );
    case T_DOT_BREAK:
    case T_DOT_CONTINUE:
    case T_DOT_ELSEIF:
    case T_DOT_ELSE:
        return( HllExitDef(i) );
    case T_DOT_ENDIF:
    case T_DOT_ENDW:
    case T_DOT_UNTIL:
    case T_DOT_UNTILCXZ:
        return( HllEndDef(i) );
    case T_PURGE:
        return( PurgeMacro(i+1) );
    case T_ALIAS:
        return( AliasDirective( i ) );
    case T_EXTERN:
    case T_EXTRN:
        return( Parse_Pass == PASS_1 ? ExternDef(i+1) : NOT_ERROR );
    case T_COMM:
        return( Parse_Pass == PASS_1 ? CommDirective(i+1) : NOT_ERROR );
    case T_EXTERNDEF:
        return( Parse_Pass == PASS_1 ? ExterndefDirective(i+1) : NOT_ERROR );
    case T_INCLUDE:
        return( IncludeDirective(i+1) );
    case T_INCLUDELIB:
        return( Parse_Pass == PASS_1 ? IncludeLibDirective(i+1) : NOT_ERROR );
    case T_ASSUME:
        return( AssumeDirective(i) );
    case T_LOCAL:
        return( Parse_Pass == PASS_1 ? LocalDef(i) : NOT_ERROR );
//    case T_COMMENT:
//        return( CommentDirective( START_COMMENT, AsmBuffer[i]->pos + 7 ) );
    case T_STRUC:
    case T_STRUCT:
    case T_UNION:
        return( StructDirective( i ) );
    case T_RECORD:
        return( Parse_Pass == PASS_1 ? RecordDef( i ) : NOT_ERROR );
    case T_NAME:
        return( Parse_Pass == PASS_1 ? NameDirective(i) : NOT_ERROR );
    case T_LABEL:
        return( LabelDirective( i ) );
    case T_ORG:
        return( OrgDirective( i ) );
    case T_END:
        return( EndDirective(i+1) );
    case T_ALIGN:
    case T_EVEN:
        return( AlignDirective( direct, i ) );
    case T_DOT_MODEL:
        return( ModelDirective(i) );
    case T_POPCONTEXT:
    case T_PUSHCONTEXT:
        return( ContextDirective(direct, i ) );
    case T_DOT_SEQ: /* this is default */
    case T_DOT_DOSSEG:
    case T_DOSSEG:
    case T_DOT_ALPHA:
        return( SegOrderDirective(direct, i ) );
    case T_DOT_RADIX:
        return( RadixDirective(i+1));
    case T_ECHO:
        return(Parse_Pass == PASS_1 ? EchoDef(i+1) : NOT_ERROR );
    case T_DOT_286C:
        direct = T_DOT_286;
    case T_DOT_8086:
    case T_DOT_186:
    case T_DOT_286:
    case T_DOT_286P:
    case T_DOT_386:
    case T_DOT_386P:
    case T_DOT_486:
    case T_DOT_486P:
    case T_DOT_586:
    case T_DOT_586P:
    case T_DOT_686:
    case T_DOT_686P:
    case T_DOT_8087:
    case T_DOT_287:
    case T_DOT_387:
    case T_DOT_NO87:
    case T_DOT_K3D:
    case T_DOT_MMX:
    case T_DOT_XMM:
#if ONEXMM==0
    case T_DOT_XMM2:
    case T_DOT_XMM3:
#endif
        if (AsmBuffer[i+1]->token != T_FINAL) {
            AsmError(SYNTAX_ERROR);
            return( ERROR );
        }
        return( cpu_directive(direct) );
    case T_DOT_LIST:
    case T_DOT_CREF:
    case T_DOT_NOLIST:
    case T_DOT_XLIST:
    case T_DOT_NOCREF:
    case T_DOT_XCREF:
    case T_DOT_LISTALL:
    case T_DOT_LISTIF:
    case T_DOT_LFCOND: /* .LFCOND is synonym for .LISTIF */
    case T_DOT_LISTMACRO:
    case T_DOT_XALL:   /* .XALL is synonym for .LISTMACRO */
    case T_DOT_LISTMACROALL:
    case T_DOT_LALL:   /* .LALL is synonym for .LISTMACROALL */
    case T_DOT_NOLISTIF:
    case T_DOT_SFCOND: /* .SFCOND is synonym for .NOLISTIF */
    case T_DOT_NOLISTMACRO:
    case T_DOT_SALL:   /* .SALL is synonym for .NOLISTMACRO */
    case T_DOT_TFCOND: /* .TFCOND toggles .LFCOND, .SFCOND */
    case T_PAGE:
    case T_TITLE:
    case T_SUBTITLE:
    case T_SUBTTL:
        return( ListingDirective( direct, i ) );
    case T_DOT_STARTUP:
    case T_DOT_EXIT:
        return( Startup ( i ) );
    case T_ENDM:
    case T_EXITM:
        /* these directives should never be seen here */
        AsmError(UNMATCHED_MACRO_NESTING);
        return( ERROR );
    case T_GOTO:
        AsmError( DIRECTIVE_MUST_APPEAR_INSIDE_A_MACRO );
        return( ERROR );
    default:
        DebugMsg(("directive: i=%u, string=%s\n", i, AsmBuffer[i]->string_ptr));
        AsmErr( UNKNOWN_DIRECTIVE, AsmBuffer[i]->string_ptr);
        return( ERROR );
    }
    return( NOT_ERROR );
}