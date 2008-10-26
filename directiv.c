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
#include "assume.h"
#include "queues.h"
#include "equate.h"
#include "fixup.h"
#include "mangle.h"
#include "labels.h"
#include "input.h"
#include "tokenize.h"
#include "expreval.h"
#include "types.h"
#include "condasm.h"
#include "hll.h"
#include "context.h"
#include "macro.h"
#include "proc.h"
#include "fastpass.h"
#include "listing.h"

#include "myassert.h"

#define ONEXMM 1

/* prototypes */
extern int              OrgDirective( int );
extern int              AlignDirective( int, int );
extern const FNAME      *AddFlist( char const *filename );

extern int              SegmentModulePrologue(int type);
extern int              SegmentModuleEnd(void);
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

extern bool             EndDirectiveFound;
extern struct asm_sym   *SegOverride;

obj_rec                 *ModendRec;     // Record for Modend (OMF)
asm_sym                 *start_label;   // for COFF

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
        " sti     "
};

/* startup code for 80186+ */

static char *StartupDosNear1[] = {
        " mov     ax,DGROUP",
        " mov     ds,ax",
        " mov     bx,ss",
        " sub     bx,ax",
        " shl     bx,4",
        " mov     ss,ax",
        " add     sp,bx"
};

static char *StartupDosFar[] = {
        " mov     dx,DGROUP",
        " mov     ds,dx"
};
static char *ExitOS2[] = { /* mov al, retval  followed by: */
        " xor ah, ah",
        " push 01h",
        " push ax",
        " call far DOSEXIT"
};
static char *ExitDos[] = {
        " mov     ah,4ch",
        " int     21h"
};

static char *StartAddr = "@Startup";
static char StartupDirectiveFound;

static asm_sym *sym_CodeSize  ; /* numeric */
static asm_sym *sym_DataSize  ; /* numeric */
static asm_sym *sym_Model     ; /* numeric */
       asm_sym *sym_Interface ; /* numeric */
       asm_sym *sym_Cpu       ; /* numeric */


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
        dir->e.seginfo->segrec = OmfNewRec( CMD_SEGDEF );
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
        dir->e.grpinfo->idx = 0;
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
        dir->e.procinfo->export = FALSE;
        dir->e.procinfo->init = FALSE;
        break;
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
        /* libraries aren't part of the symbol namespace, no
         need to set a symbol type */
        // sym->state = SYM_LIB;
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
    dir_node *dir;

    /**/myassert( name != NULL );

    if (dir = (dir_node *)SymCreate( name, *name != NULLC ))
        dir_init( dir, tab );

    return( dir );
}

// alloc a dir_node and do NOT insert it into the symbol table.
// used for (temporary) externals created for PROTO items,
// nested STRUCTs, class names, ...

dir_node *dir_insert_ex( const char *name, int tab )
{
    dir_node *dir;

    /**/myassert( name != NULL );

    if (dir = (dir_node *)SymCreate( name, FALSE ))
        dir_init( dir, tab );

    return( dir );
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
            OmfKillRec( dir->e.seginfo->segrec );
        AsmFree( dir->e.seginfo );
        break;
    case SYM_EXTERNAL:
        dir->sym.first_size = 0;
        break;
    case SYM_CLASS_LNAME:
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
        ReleaseMacroData( dir );
        AsmFree( dir->e.macroinfo );
        dir->e.macroinfo = NULL;
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

ret_code checkword( char **token )
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

static lang_type GetLangType( int *i )
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
    return( ERROR );
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

static ret_code ExterndefDirective( int i )
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

ret_code EchoDef( int i )
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

// EXTERN xxx:yyy directive

static ret_code ExternDirective( int i )
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
        } else {
            if( sym->mem_type != mem_type ||
                ( lang_type != LANG_NONE && sym->langtype != LANG_NONE && sym->langtype != lang_type )) {
                AsmError( EXT_DEF_DIFF );
                return( ERROR );
            }
            /* if EXTERN is placed BEHIND the symbol definition, it's
             ignored by Masm! */
            if (sym->state != SYM_EXTERNAL) {
                DebugMsg(("ExternDirective: symbol %s redefinition\n", token ));
                AsmWarn( 3, SYMBOL_REDEFINITION, token );
            }
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

static ret_code PublicDirective( int i )
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

static ret_code IncludeDirective( int i )
/******************/
{
    int size;

    DebugMsg(("IncludeDirective enter\n"));

    if (AsmFiles.file[LST]) {
        directive_listed = TRUE;
        LstWriteFile(LSTTYPE_DIRECTIVE, 0, NULL );
    }

    if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM) {
        AsmError( EXPECTED_FILE_NAME );
        return( ERROR );
    }

    if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<') {
        if (AsmBuffer[i+1]->token != T_FINAL) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        return( InputQueueFile( AsmBuffer[i]->string_ptr ) );
    } else {
        size = strlen(AsmBuffer[i]->pos);
        while (size && isspace(AsmBuffer[i]->pos[size-1])) {
            size--;
            AsmBuffer[i]->pos[size] = NULLC;
        }
        return( InputQueueFile( AsmBuffer[i]->pos ) );
    }
}

// called during pass 1 only

static ret_code IncludeLibDirective( int i )
/*********************/
{
    char *name;
    dir_node *dir;
    struct asm_sym *sym;

    if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM) {
        AsmError( LIBRARY_NAME_MISSING );
        return( ERROR );
    }

    if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<') {
        if (AsmBuffer[i+1]->token != T_FINAL) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        name = AsmBuffer[i]->string_ptr;
    } else {
        int size;
        size = strlen(AsmBuffer[i]->pos);
        while (size && isspace(AsmBuffer[i]->pos[size-1])) {
            size--;
            AsmBuffer[i]->pos[size] = NULLC;
        }
        name = AsmBuffer[i]->pos;
    }

    /* old approach, <= 1.91: add lib name to global namespace */
    /* new approach, >= 1.92: check lib table, if entry is missing, add it */

#if 1
    /* Masm doesn't map cases for the paths. So if there is
     includelib <kernel32.lib>
     includelib <KERNEL32.LIB>
     then 2 defaultlib entries are added. If this is to be changed for
     JWasm, activate the stricmp() below.
     */
    for (dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
        //if ( stricmp( dir->sym.name, name) == 0)
        if ( strcmp( dir->sym.name, name) == 0)
            return( NOT_ERROR );
    }
    if (dir_insert_ex( name, TAB_LIB ) == NULL)
        return( ERROR );
#else
    /* add the lib name to global namespace */
    sym = SymSearch( name );
    if( sym == NULL ) {
        if( dir_insert( name, TAB_LIB ) == NULL ) {
            return( ERROR );
        }
    }
#endif
    return( NOT_ERROR );
}

/* handles .STARTUP and .EXIT directives */

static ret_code StartupExitDirective( int i )
/******************/
{
    int         count;
    char        **p;
    char        buffer[ MAX_LINE_LEN ];

    if( ModuleInfo.model == MOD_NONE ) {
        AsmError( MODEL_IS_NOT_DECLARED );
        return( ERROR );
    }
    if ( Use32) {
        AsmErr( DOES_NOT_WORK_WITH_32BIT_SEGMENTS, AsmBuffer[i]->string_ptr );
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
            if (ModuleInfo.model == MOD_TINY)
                ;
            else {
                if( ModuleInfo.distance == STACK_NEAR ) {
                    if ( (ModuleInfo.cpu & 0x7F) <= 1) {
                        p = StartupDosNear0;
                        count = sizeof(StartupDosNear0) / sizeof(char *);
                    } else {
                        p = StartupDosNear1;
                        count = sizeof(StartupDosNear1) / sizeof(char *);
                    }
                } else {
                    p = StartupDosFar;
                    count = sizeof(StartupDosFar) / sizeof(char *);
                }
                for ( ; count ; count--, p++ )
                    InputQueueLine( *p );
            }
        }
        StartupDirectiveFound = TRUE;
        break;
    case T_DOT_EXIT:
        if( ModuleInfo.ostype == OPSYS_DOS ) {
            p = ExitDos;
            count = sizeof( ExitDos) / sizeof( char * );
        } else {
            p = ExitOS2;
            count = sizeof( ExitOS2) / sizeof( char * );
        }
        i++;
        if( ( AsmBuffer[i]->string_ptr != NULL )
            && ( *(AsmBuffer[i]->string_ptr) != '\0' ) ) {
            if( ModuleInfo.ostype == OPSYS_DOS ) {
                sprintf( buffer, "mov ax,4C00h+(%s and 0FFh)", AsmBuffer[i]->string_ptr);
            } else {
                sprintf( buffer, "mov ax, %s", AsmBuffer[i]->string_ptr);
            }
            InputQueueLine( buffer );
            p++;
            count--;
        }
        for( ; count ; count--, p++ ) {
            InputQueueLine( *p );
        }
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

// called by AssembleInit(), once per source module
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
    ModuleInfo.nocasemap = Options.nocasemap;
    ModuleInfo.ljmp = TRUE;
    ModuleInfo.emulator = (Options.floating_point == DO_FP_EMULATION);
    SymSetCmpFunc( ModuleInfo.nocasemap );
    SetMasm510( Options.masm51_compat );
    ModuleInfo.list = Options.write_listing;
    ModuleInfo.cref = TRUE;
    ModuleInfo.segorder = SEGORDER_SEQ;
    ModuleInfo.flatgrp_idx = 0;
    ModuleInfo.name[0] = NULLC;
    // add source file to autodependency list
    ModuleInfo.srcfile = AddFlist( AsmFiles.fname[ASM] );

    StartupDirectiveFound = FALSE;

    memset(Tables, 0, sizeof(Tables));

}

static void get_module_name( void )
/*********************************/
{
    char dummy[_MAX_EXT];
    char        *p;

    /**/myassert( AsmFiles.fname[ASM] != NULL );
    _splitpath( AsmFiles.fname[ASM], NULL, NULL, ModuleInfo.name, dummy );
    strupr( ModuleInfo.name );
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

static ret_code ModelDirective( int i )
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
                    AsmErr( UNKNOWN_MODEL_OPTION, token );
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
    return( NOT_ERROR );
}

// if count == 1, there's NO start address
// if count > 1, there's one.

int process_address(expr_list *);

static ret_code EndDirective( int i )
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
        strcat( CurrSource, " " );
        strcat( CurrSource, StartAddr );
        InputQueueLine( CurrSource );
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

    if( opndx.type == EXPR_EMPTY )
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
        ModendRec = OmfNewRec( CMD_MODEND );
        ModendRec->d.modend.main_module = FALSE;

        if( opndx.type == EXPR_EMPTY ) {
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

        if (opndx.type == EXPR_EMPTY )
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

static ret_code AliasDirective( int i )
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

static ret_code NameDirective( int i )
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

static ret_code RadixDirective( int i )
{
    expr_list       opndx;

    if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
        return( ERROR );
    if (opndx.type != EXPR_CONST || opndx.string != NULL) {
        AsmError(CONSTANT_EXPECTED);
        return( ERROR );
    }
    if ( opndx.value > 16 || opndx.value < 2 ) {
        AsmError( INVALID_RADIX_TAG );
        return( ERROR );
    }
#if 0
    if ( opndx.value != 10 && opndx.value != 16 ) {
        AsmErr( NOT_SUPPORTED, AsmBuffer[start-1]->pos);
        return( ERROR );
    }
#endif
    if (AsmBuffer[i]->token != T_FINAL) {
        AsmError(SYNTAX_ERROR);
        return( ERROR );
    }

    ModuleInfo.radix = opndx.value;

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
//    case T_DOT_NO87:
//        return( P_NO87 );
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
        return( P_MMX | P_SSEALL );
#else
    case T_DOT_XMM:
        return( P_MMX | P_SSE1 );
    case T_DOT_XMM2:
        return( P_MMX | P_SSE1 | P_SSE2 );
    case T_DOT_XMM3:
        return( P_MMX | P_SSE1 | P_SSE2 | P_SSE3 | P_SSSE3 );
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

/* handles
 .8086,
 .[1|2|3|4|5|6]86[p],
 .8087,
 .[2|3]87,
 .NO87, .MMX, .K3D, .XMM directives.
 set CPU and FPU parameter in ModuleInfo.cpu + ModuleInfo.curr_cpu.
 ModuleInfo.cpu is the value of Masm's @CPU symbol.
 ModuleInfo.curr_cpu is the old OW Wasm value.
 additional notes:
 .[1|2|3|4|5|6]86 will reset .MMX, .K3D and .XMM,
 OTOH, .MMX/.XMM won't automatically enable .586/.686
*/

ret_code cpu_directive( int i )
{
    int temp;

    if( i == T_DOT_NO87 ) {
        ModuleInfo.curr_cpu &= ~P_FPU_MASK;  // turn off FPU bits
    } else {
        if( ( temp = comp_opt( i ) ) == EMPTY ) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        if (temp & P_CPU_MASK) {
            /* reset CPU, FPU and EXT bits */
            ModuleInfo.curr_cpu &= ~( P_CPU_MASK | P_FPU_MASK | P_EXT_MASK | P_PM );

            /* set CPU bits */
            ModuleInfo.curr_cpu |= temp & ( P_CPU_MASK | P_PM );

            /* set default FPU bits */
            ModuleInfo.curr_cpu |= def_fpu( i ) & P_FPU_MASK;
        }

        if( temp & P_FPU_MASK ) {
            ModuleInfo.curr_cpu &= ~P_FPU_MASK;
            ModuleInfo.curr_cpu |= temp & P_FPU_MASK;
        }

        if( temp & P_EXT_MASK ) {
            ModuleInfo.curr_cpu &= ~P_EXT_MASK;
            ModuleInfo.curr_cpu |= temp & P_EXT_MASK;
        }
    }

    temp = ModuleInfo.curr_cpu & P_CPU_MASK;
    switch ( temp ) {
    case P_186:
        ModuleInfo.cpu = M_8086 | M_186;
        break;
    case P_286:
        ModuleInfo.cpu = M_8086 | M_186 | M_286;
        break;
    case P_386:
        ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386;
        break;
    case P_486:
        ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486;
        break;
    case P_586:
        ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486 | M_586;
        break;
    case P_686:
        // ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486 | M_586 | M_686;
        /* Masm's .686 directive doesn't set the Pentium flag! A bug? */
        ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486 | M_686;
        break;
    default:
        ModuleInfo.cpu = M_8086;
        break;
    }
    if ( ModuleInfo.curr_cpu & P_PM )
        ModuleInfo.cpu = ModuleInfo.cpu | M_PROT;

    temp = ModuleInfo.curr_cpu & P_FPU_MASK;
    switch (temp) {
    case P_87:
        ModuleInfo.cpu = ModuleInfo.cpu | M_8087;
        break;
    case P_287:
        ModuleInfo.cpu = ModuleInfo.cpu | M_8087 | M_287;
        break;
    case P_387:
        ModuleInfo.cpu = ModuleInfo.cpu | M_8087 | M_287 | M_387;
        break;
    }

    DebugMsg(("cpu_directive: curr_cpu=%X, @Cpu=%X\n", ModuleInfo.curr_cpu, ModuleInfo.cpu ));

    MakeCPUConstant( i );
    if (ModuleInfo.cpu & ( M_386 | M_486 | M_586 | M_686 ) )
        SetUse32Def( TRUE );
    else
        SetUse32Def( FALSE );

    /* Set @Cpu */
    /* differs from Codeinfo cpu setting */

    sym_Cpu = CreateConstant( "@Cpu", ModuleInfo.cpu, -1, TRUE);

    return( NOT_ERROR );
}

static ret_code MakeComm(
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

static ret_code CommDirective( int i )
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

// .[NO|X]LIST, .[NO|X]CREF, .[NO]LISTIF, .[NO]LISTMACRO
// .LISTALL, .LISTMACROALL, .[LF|SF|TF]COND, .[X|L|S]ALL
// PAGE, TITLE, SUBTITLE, SUBTTL directives

static ret_code ListingDirective( int directive, int i)
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

// DOSSEG, .DOSSEG, .ALPHA, .SEQ directives

static ret_code SegOrderDirective(int directive, int i)
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

ret_code directive( int i, long direct )
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
        InStrDef( 1, CurrSource );
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
        return( EndpDef(i-1) );
    case T_DOT_CODE:
    case T_DOT_STACK:
    case T_DOT_DATA:
    case T_DOT_DATA_UN:
    case T_DOT_FARDATA:
    case T_DOT_FARDATA_UN:
    case T_DOT_CONST:
        return( SimplifiedSegDir( i ) );
    case T_OPTION:
        return( OptionDirective( i+1 ) );
    case T_TYPEDEF:
        return( Parse_Pass == PASS_1 ? TypeDef(i-1) : NOT_ERROR );
    case T_INVOKE:
        return( InvokeDirective( i+1 ) );
    case T_DOT_IF:
    case T_DOT_WHILE:
    case T_DOT_REPEAT:
        return( HllStartDef( i ) );
    case T_DOT_BREAK:
    case T_DOT_CONTINUE:
    case T_DOT_ELSEIF:
    case T_DOT_ELSE:
        return( HllExitDef( i ) );
    case T_DOT_ENDIF:
    case T_DOT_ENDW:
    case T_DOT_UNTIL:
    case T_DOT_UNTILCXZ:
        return( HllEndDef( i ) );
    case T_PURGE:
        return( PurgeMacro( i+1 ) );
    case T_ALIAS:
        return( AliasDirective( i ) );
    case T_EXTERN:
    case T_EXTRN:
        return( Parse_Pass == PASS_1 ? ExternDirective(i+1) : NOT_ERROR );
    case T_COMM:
        return( Parse_Pass == PASS_1 ? CommDirective(i+1) : NOT_ERROR );
    case T_EXTERNDEF:
        return( Parse_Pass == PASS_1 ? ExterndefDirective(i+1) : NOT_ERROR );
    case T_INCLUDE:
        return( IncludeDirective( i+1 ) );
    case T_INCLUDELIB:
        return( Parse_Pass == PASS_1 ? IncludeLibDirective(i+1) : NOT_ERROR );
    case T_ASSUME:
        return( AssumeDirective( i ) );
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
        return( ContextDirective( direct, i ) );
    case T_DOT_SEQ: /* this is default */
    case T_DOT_DOSSEG:
    case T_DOSSEG:
    case T_DOT_ALPHA:
        return( SegOrderDirective(direct, i ) );
    case T_DOT_RADIX:
        return( RadixDirective( i+1 ) );
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
        return( StartupExitDirective( i ) );
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
