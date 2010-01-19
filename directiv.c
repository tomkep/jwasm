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
* Description:  processes directives:
*               NameDirective()     - NAME
*               RadixDirective()    - .RADIX
*               SegOrderDirective() - .DOSSEG, .SEQ, .ALPHA
*               ListingDirective()  - .LIST, .CREF, ...
*               cpu_directive()     - .8086, .186, ... , .MMX, .XMM
*               AliasDirective()    - ALIAS
*               EndDirective()      - END
*               ModelDirective()    - .MODEL
*               StartupExitDirective() - .STARTUP, .EXIT
*               IncBinDirective()   - INCBIN
*               IncludeLibDirective() - INCLUDELIB
*               EchoDirective()     - ECHO
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"
#include "segment.h"
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
#include "extern.h"
#include "posndir.h"
#include "omf.h"

#include "myassert.h"

#define ONEXMM 1

/* prototypes */
extern ret_code         process_address( struct code_info *, expr_list * );

typedef struct stacknode {
    void    *next;
    void    *elt;
} stacknode;

#define INIT_MODEL      0x1
#define INIT_LANG       0x2
#define INIT_STACK      0x4
#define INIT_OS         0x8

static const typeinfo ModelInfo[] = {
    { "TINY",         MOD_TINY,       INIT_MODEL      },
    { "SMALL",        MOD_SMALL,      INIT_MODEL      },
    { "COMPACT",      MOD_COMPACT,    INIT_MODEL      },
    { "MEDIUM",       MOD_MEDIUM,     INIT_MODEL      },
    { "LARGE",        MOD_LARGE,      INIT_MODEL      },
    { "HUGE",         MOD_HUGE,       INIT_MODEL      },
    { "FLAT",         MOD_FLAT,       INIT_MODEL      }
};
#if 0
// not needed. see GetLangType()
static const typeinfo LangInfo[] = {
    { "BASIC",        LANG_BASIC,     INIT_LANG       },
    { "FORTRAN",      LANG_FORTRAN,   INIT_LANG       },
    { "PASCAL",       LANG_PASCAL,    INIT_LANG       },
    { "C",            LANG_C,         INIT_LANG       },
    { "FASTCALL",     LANG_FASTCALL,  INIT_LANG       },
    { "STDCALL",      LANG_STDCALL,   INIT_LANG       },
    { "SYSCALL",      LANG_SYSCALL,   INIT_LANG       }
};
#else
static const typeinfo dmyLang = { NULL, 0, INIT_LANG };
#endif
static const typeinfo StackInfo[] = {
    { "NEARSTACK",    STACK_NEAR,     INIT_STACK      },
    { "FARSTACK",     STACK_FAR,      INIT_STACK      }
};
static const typeinfo OsInfo[] = {
    { "OS_OS2",       OPSYS_OS2,      INIT_OS         },
    { "OS_DOS",       OPSYS_DOS,      INIT_OS         }
};

simpletype SimpleType[] = {
#undef pick
#define pick( name, memtype, ofssize ) { T_ ## name , memtype, ofssize },
#include "stypes.h"
//    { T_PTR,     MT_PTR,      USE_EMPTY }
};

obj_rec                 *ModendRec;     // Record for Modend (OMF)
asm_sym                 *start_label;   // for COFF, ELF, BIN

symbol_queue            Tables[TAB_LAST];// tables of definitions

stacknode               *SafeSEHStack;

/* startup code for 8086 */

static const char * const StartupDosNear0[] = {
        "mov\tdx,DGROUP",
        "mov\tds,dx",
        "mov\tbx,ss",
        "sub\tbx,dx",
        "shl\tbx,1",
        "shl\tbx,1",
        "shl\tbx,1",
        "shl\tbx,1",
        "cli\t",
        "mov\tss,dx",
        "add\tsp,bx",
        "sti\t"
};

/* startup code for 80186+ */

static const char * const StartupDosNear1[] = {
        "mov\tax,DGROUP",
        "mov\tds,ax",
        "mov\tbx,ss",
        "sub\tbx,ax",
        "shl\tbx,4",
        "mov\tss,ax",
        "add\tsp,bx"
};

static const char * const StartupDosFar[] = {
        "mov\tdx,DGROUP",
        "mov\tds,dx"
};
static const char * const ExitOS2[] = { /* mov al, retval  followed by: */
        "mov\tah,0",
        "push\t01h",
        "push\tax",
        "call\tDOSEXIT"
};
static const char * const ExitDos[] = {
        "mov\tah,4ch",
        "int\t21h"
};

static char *StartAddr = "@Startup";

static asm_sym *sym_CodeSize  ; /* numeric */
static asm_sym *sym_DataSize  ; /* numeric */
static asm_sym *sym_Model     ; /* numeric */
       asm_sym *sym_Interface ; /* numeric */
       asm_sym *sym_Cpu       ; /* numeric */

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

#if 0 //def DEBUG_OUT
#ifdef __WATCOMC__
/* the C heap is used for line queues only - due to speed issues. */
/* so this heap check function has become less useful. */
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

void pushitem( void *stk, void *elt )
/***********************************/
{
    void        **stack = stk;
    stacknode   *node;

    node = AsmAlloc( sizeof( stacknode ));
    node->next = *stack;
    node->elt = elt;
    *stack = node;
}

void *popitem( void *stk )
/************************/
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

#if 0
void *peekitem( void *stk, int level )
/************************************/
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
#endif

void dir_add_table( dir_node *new )
/*********************************/
{
    /* put the new entry into the queue for its type of symbol */
    int tab;

    switch( new->sym.state ) {
    case SYM_UNDEFINED:tab = TAB_UNDEF;break;
    case SYM_SEG:     tab = TAB_SEG;   break;
    case SYM_GRP:     tab = TAB_GRP;   break;
    case SYM_EXTERNAL:tab = TAB_EXT;   break;
    case SYM_INTERNAL:tab = TAB_PROC;  break;
    case SYM_ALIAS:   tab = TAB_ALIAS; break;
    default:
        return;
    }
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

void dir_remove_table( dir_node *dir )
/************************************/
{
    int tab;

    switch( dir->sym.state ) {
    case SYM_UNDEFINED:  tab = TAB_UNDEF; break;
    case SYM_EXTERNAL:   tab = TAB_EXT;   break;
    case SYM_INTERNAL:   tab = TAB_PROC;  break; /* shouldn't happen */
    case SYM_SEG:        tab = TAB_SEG;   break; /* shouldn't happen */
    case SYM_GRP:        tab = TAB_GRP;   break; /* shouldn't happen */
    case SYM_ALIAS:      tab = TAB_ALIAS; break; /* shouldn't happen */
    default:
        return;
    }

    if( dir->prev )
        dir->prev->next = dir->next;
    if( dir->next )
        dir->next->prev = dir->prev;

    if ( dir->next == NULL )
        dir->next = dir->prev;

    if ( Tables[tab].head == dir )
        Tables[tab].head = dir->next;
    if ( Tables[tab].tail == dir )
        Tables[tab].tail = dir->next;

    dir->prev = NULL;
    dir->next = NULL;
}


static void dir_init( dir_node *dir, int type )
/*********************************************/
/* Change node and insert it into the table specified by tab */
{
    switch( type ) {
    case SYM_SEG:
        dir->sym.state = SYM_SEG;
        dir->e.seginfo = AsmAlloc( sizeof( seg_info ) );
        memset( dir->e.seginfo, 0, sizeof( seg_info ) );
        dir->e.seginfo->segrec = OmfNewRec( CMD_SEGDEF );
#if 0
        dir->e.seginfo->group = NULL;
        dir->e.seginfo->start_loc = 0;
        dir->e.seginfo->current_loc = 0;
        dir->e.seginfo->CodeBuffer = NULL;
        dir->e.seginfo->labels = NULL;
        dir->e.seginfo->FixupListHead = NULL;
        dir->e.seginfo->FixupListTail = NULL;
        dir->e.seginfo->LinnumQueue = NULL;
        dir->e.seginfo->num_relocs = 0;
        dir->e.seginfo->segtype = SEGTYPE_UNDEF;
        dir->e.seginfo->lname_idx = 0;
        dir->e.seginfo->alignment = 4;
        dir->e.seginfo->readonly = FALSE;
#endif
        dir->e.seginfo->Ofssize = ModuleInfo.defOfssize;
        dir->e.seginfo->alignment = 4; /* this is PARA */
        dir->e.seginfo->segrec->d.segdef.combine = COMB_INVALID;
        /* null class name, in case none is mentioned */
        dir->e.seginfo->segrec->d.segdef.class_name_idx = 1;
        //dir->sym.max_offset = 0;
        dir->e.seginfo->segrec->d.segdef.access_valid = FALSE;
        break;
    case SYM_GRP:
        dir->sym.state = SYM_GRP;
        dir->e.grpinfo = AsmAlloc( sizeof( grp_info ) );
        dir->e.grpinfo->seglist = NULL;
        dir->e.grpinfo->idx = 0;
        dir->e.grpinfo->lname_idx = 0;
        dir->e.grpinfo->numseg = 0;
        break;
    case SYM_TYPE:
        dir->sym.state = SYM_TYPE;
        dir->e.structinfo = AsmAlloc( sizeof( struct_info ) );
        dir->e.structinfo->head = NULL;
        dir->e.structinfo->tail = NULL;
        dir->e.structinfo->alignment = 0;
        dir->e.structinfo->typekind = TYPE_NONE;
        dir->e.structinfo->flags = 0;
        return;
    default:
        // unknown symbol type
        /**/myassert( 0 );
        return;
    }
    dir_add_table( dir );
    return;
}

dir_node *dir_insert( const char *name, int type )
/************************************************/
/* Insert a node into the table specified by tab */
{
    dir_node *dir;

    /**/myassert( name != NULL );

    if ( dir = (dir_node *)SymCreate( name, *name != NULLC ) )
        dir_init( dir, type );

    return( dir );
}

// alloc a dir_node and do NOT insert it into the symbol table.
// used for (temporary) externals created for PROTO items,
// nested STRUCTs, class names, ...

dir_node *dir_insert_ex( const char *name, int tab )
/**************************************************/
{
    dir_node *dir;

    /**/myassert( name != NULL );

    if ( dir = (dir_node *)SymCreate( name, FALSE ) )
        dir_init( dir, tab );

    return( dir );
}

// free a dir_node

void dir_free( dir_node *dir, bool remove )
/*****************************************/
{
//    int i;

    switch( dir->sym.state ) {
    case SYM_GRP:
        {
            seg_item    *segcurr;
            seg_item    *segnext;

            segcurr = dir->e.grpinfo->seglist;
            for( ; segcurr; ) {
                segnext = segcurr->next;
                DebugMsg(("dir_free: GROUP=%X, segm=%X\n", dir, segcurr ));
                AsmFree( segcurr );
                segcurr = segnext;
            }
            DebugMsg(("dir_free: GROUP=%X, extension=%X\n", dir, dir->e.grpinfo ));
            AsmFree( dir->e.grpinfo );
        }
        break;
    case SYM_SEG:
        DebugMsg(("dir_free: SEG=%X, extension=%X, OMF ref=%X\n", dir, dir->e.seginfo, dir->e.seginfo->segrec ));
        if( dir->e.seginfo->segrec != NULL )
            OmfKillRec( dir->e.seginfo->segrec );
        AsmFree( dir->e.seginfo );
        break;
    case SYM_EXTERNAL:
        if ( dir->sym.isproc )
            DeleteProc( dir );
        dir->sym.first_size = 0;
        /* for EXTERN, free the optional alternative name */
        if ( dir->sym.weak == FALSE && dir->sym.altname )
            AsmFree( dir->sym.altname );
        break;
    //case SYM_CLASS_LNAME:
    //    break;
    case SYM_INTERNAL:
        if ( dir->sym.isproc )
            DeleteProc( dir );
        break;
    case SYM_MACRO:
        ReleaseMacroData( dir );
        AsmFree( dir->e.macroinfo );
        dir->e.macroinfo = NULL;
        break;
    case SYM_TMACRO:
        if ( dir->sym.predefined == FALSE ) {
            AsmFree( dir->sym.string_ptr );
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
    if ( remove == TRUE)
        dir_remove_table( dir );
}

void dir_internal( dir_node *dir )
/********************************/
/* Change node type from SYM_EXTERNAL + sym->weak=1 to SYM_INTERNAL */
{
    dir_remove_table( dir );
    if ( dir->sym.isproc == FALSE ) /* v2.01: don't clear flags for PROTO */
        dir->sym.first_size = 0;
    dir->sym.state = SYM_INTERNAL;
}

void dir_settype( dir_node *dir, int type )
/*****************************************/
/* Change node type from SYM_UNDEFINED to anything else */
{
    dir_remove_table( dir );
    dir_init( dir, type );
}

ret_code GetLangType( int *i, lang_type *plang )
/**********************************************/
{
    if( AsmBuffer[*i]->token == T_RES_ID ) {
        switch( AsmBuffer[(*i)]->value ) {
        case T_C:        *plang = LANG_C;        break;
        case T_SYSCALL:  *plang = LANG_SYSCALL;  break;
        case T_STDCALL:  *plang = LANG_STDCALL;  break;
        case T_PASCAL:   *plang = LANG_PASCAL;   break;
        case T_FORTRAN:  *plang = LANG_FORTRAN;  break;
        case T_BASIC:    *plang = LANG_BASIC;    break;
        case T_FASTCALL: *plang = LANG_FASTCALL; break;
        default:
            return( ERROR );
        }
        (*i)++;
        return( NOT_ERROR );
    }
    return( ERROR );
}

/* returns an index into SimpleType table.
 * this is actually a ST_xxx value.
 */

int FindStdType( int token )
/***************************/
{
#if 1
    /* v2.01: avoid to scan the SimpleType table!
     * It is to be removed.
     */
    switch ( token ) {
    case T_PROC: return( ST_PROC );
    case T_PTR:  return( ST_PTR );
    default:
        if ( AsmOpTable[token].rm_byte == RWT_TYPE ) {
            return( AsmOpTable[token].opcode );
        }
    }
#else
    int i;
    for( i = 0; i <= sizeof(SimpleType)/sizeof(simpletype); i++ ) {
        if( token == SimpleType[i].token )
            return( i );
    }
#endif
    return( -1 );
}

// get size from memory type
// is32 param used only for MT_NEAR/MT_FAR

int SizeFromMemtype( memtype type, int Ofssize )
/**********************************************/
{
    int  size;

    if ( ( type & MT_SPECIAL) == 0 )
        return ( (type & MT_SIZE_MASK) + 1 );

    DebugMsg(("SizeFromMemtype: memtype=%Xh, Ofssize=%u\n", type, Ofssize ));
    switch ( type ) {
    case MT_NEAR:
        return ( 2 << Ofssize );
    case MT_FAR:
        return ( ( 2 << Ofssize ) + 2 );
    case MT_PROC:
        return( ( 2 << ModuleInfo.Ofssize ) +
                ( SimpleType[ST_PROC].mem_type == MT_FAR ? 2 : 0 ) );
    case MT_PTR:
        /* first determine offset size */
        size = ( 2 << ModuleInfo.Ofssize );
        if( (ModuleInfo.model == MOD_COMPACT)
         || (ModuleInfo.model == MOD_LARGE)
         || (ModuleInfo.model == MOD_HUGE) ) {
            size += 2;      /* add segment for far data pointers */
        }
        return( size );
    default:
        return( 0 );
    }
}

// get memory type from size

ret_code MemtypeFromSize( int size, memtype *ptype )
/**************************************************/
{
#if 0
    int i;
    for( i = 0; i <= sizeof(SimpleType)/sizeof(simpletype); i++ ) {
        if( (SimpleType[i].mem_type & MT_SPECIAL) == 0 ) {
            /* the size is encoded 0-based in field mem_type */
            if( ( ( SimpleType[i].mem_type & MT_SIZE_MASK) + 1 ) == size ) {
                *ptype = SimpleType[i].mem_type;
                return( NOT_ERROR );
            }
        }
    }
#else
    const struct asm_ins *ins;
    //for ( ins = &AsmOpTable[optable_idx[T_BYTE]]; ins->rm_byte == RWT_TYPE; ins++ ) {
    for ( ins = &AsmOpTable[T_BYTE]; ins->rm_byte == RWT_TYPE; ins++ ) {
        if( ( ins->opnd_type[1] & MT_SPECIAL) == 0 ) {
            /* the size is encoded 0-based in field mem_type */
            if( ( ( ins->opnd_type[1] & MT_SIZE_MASK) + 1 ) == size ) {
                *ptype = ins->opnd_type[1];
                return( NOT_ERROR );
            }
        }
    }
#endif
    return( ERROR );
}

// handle ECHO
// displays text on the console

static ret_code EchoDirective( int i )
/************************************/
{
    /* don't print to stdout if -EP is on! */
    if ( Options.preprocessor_stdout == FALSE ) {
        char *p = AsmBuffer[i]->pos + 4; /* 4 = sizeof("echo") */
        while ( isspace( *p ) ) p++;
        printf( "%s\n", p );
    }
    return( NOT_ERROR );
}

/*
 find token in a 'typeinfo' table
 */

const typeinfo *FindToken( const char *token, const typeinfo *table, int size )
/*****************************************************************************/
{
    for( ; size; size--, table++ ) {
        if( _stricmp( table->string, token ) == 0 ) {
            return( table );
        }
    }
    return( NULL );  // Not found
}

// called during pass 1 only

static ret_code IncludeLibDirective( int i )
/******************************************/
{
    char *name;
    dir_node *dir;
    struct asm_sym *sym;

    if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM ) {
        AsmError( LIBRARY_NAME_MISSING );
        return( ERROR );
    }

    if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
        if ( AsmBuffer[i+1]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+1]->string_ptr );
            return( ERROR );
        }
        name = AsmBuffer[i]->string_ptr;
    } else {
        int size;
        size = strlen( AsmBuffer[i]->pos );
        while ( size && isspace( AsmBuffer[i]->pos[size-1] ) ) {
            size--;
            AsmBuffer[i]->pos[size] = NULLC;
        }
        name = AsmBuffer[i]->pos;
    }

    /* old approach, <= 1.91: add lib name to global namespace */
    /* new approach, >= 1.92: check lib table, if entry is missing, add it */

    /* Masm doesn't map cases for the paths. So if there is
     includelib <kernel32.lib>
     includelib <KERNEL32.LIB>
     then 2 defaultlib entries are added. If this is to be changed for
     JWasm, activate the _stricmp() below.
     */
    for ( dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
        //if ( _stricmp( dir->sym.name, name) == 0)
        if ( strcmp( dir->sym.name, name ) == 0 )
            return( NOT_ERROR );
    }
    sym = SymCreate( name, FALSE );
    //if ( sym == NULL )  /* only error possible is "out of memory" */
    //    return( ERROR );
    if( Tables[TAB_LIB].head == NULL ) {
        Tables[TAB_LIB].head = Tables[TAB_LIB].tail = (dir_node *)sym;
    } else {
        Tables[TAB_LIB].tail->next = (dir_node *)sym;
        Tables[TAB_LIB].tail = (dir_node *)sym;
    }

    return( NOT_ERROR );
}

#if INCLUDEBIN
// INCBIN directive

static ret_code IncBinDirective( int i )
/**************************************/
{
    FILE *file;
    //int size;
    uint fileoffset = 0;
    uint sizemax = -1;
    expr_list opndx;
    char filename[_MAX_PATH];

    DebugMsg(("IncBinDirective enter\n"));

    if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM ) {
        AsmError( EXPECTED_FILE_NAME );
        return( ERROR );
    }
#if FASTPASS
    /* make sure the directive is stored */
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif

    if ( AsmBuffer[i]->token == T_STRING ) {
        if ( AsmBuffer[i]->string_delim == '"' ||
             AsmBuffer[i]->string_delim == '\'') {
            strncpy( filename, AsmBuffer[i]->string_ptr+1, AsmBuffer[i]->value );
            filename[ AsmBuffer[i]->value ] = NULLC;
        } else if ( AsmBuffer[i]->string_delim == '<' ) {
            strncpy( filename, AsmBuffer[i]->string_ptr, sizeof( filename ) );
        } else {
            AsmError( FILENAME_MUST_BE_ENCLOSED_IN_QUOTES_OR_BRACKETS );
            return( ERROR );
        }
    } else {
        AsmError( FILENAME_MUST_BE_ENCLOSED_IN_QUOTES_OR_BRACKETS );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token == T_COMMA ) {
        i++;
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if ( opndx.kind == EXPR_CONST && opndx.string == NULL ) {
            fileoffset = opndx.value;
        } else if ( opndx.kind != EXPR_EMPTY ) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                return( ERROR );
            if ( opndx.kind == EXPR_CONST && opndx.string == NULL ) {
                sizemax = opndx.value;
            } else if ( opndx.kind != EXPR_EMPTY ) {
                AsmError( CONSTANT_EXPECTED );
                return( ERROR );
            }
        }
    }
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if( CurrSeg == NULL ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }

    /* try to open the file */
    DebugMsg(("IncBinDirective: filename=%s, offset=%u, size=%u\n", filename, fileoffset, sizemax ));
    if ( InputQueueFile( filename, &file ) == NOT_ERROR ) {
        /* now transfer file content to the current segment. If no
         segment is open, function OutputByte() will fail and display
         an error.
         */
        if ( fileoffset )
            fseek( file, fileoffset, SEEK_SET );
        for( ; sizemax; sizemax--) {
            int ch = fgetc( file );
            if ((ch == EOF) && feof( file ) )
                break;
            OutputByte( ch );
        }
        fclose( file );
    }

    return( NOT_ERROR );
}
#endif

/* handles .STARTUP and .EXIT directives */

static ret_code StartupExitDirective( int i )
/*******************************************/
{
    int         count;
    int         j;
    const char  * const *p;
    char        *buffer = StringBufferEnd;
    expr_list   opndx;

    if ( ModuleInfo.list )
        LstWriteSrcLine();

    if( ModuleInfo.model == MOD_NONE ) {
        AsmError( MODEL_IS_NOT_DECLARED );
        return( ERROR );
    }
    if ( ModuleInfo.Ofssize > USE16 ) {
        AsmErr( DOES_NOT_WORK_WITH_32BIT_SEGMENTS, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    PushLineQueue();

    switch( AsmBuffer[i]->value ) {
    case T_DOT_STARTUP:
        count = 0;
        if (ModuleInfo.model == MOD_TINY)
            AddLineQueue( "org 100h" );
        strcpy( buffer, StartAddr );
        strcat( buffer, "::" );
        AddLineQueue( buffer );
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
                    AddLineQueue( (char *)*p );
            }
        }
        ModuleInfo.StartupDirectiveFound = TRUE;
        break;
    case T_DOT_EXIT:
        if( ModuleInfo.ostype == OPSYS_DOS ) {
            p = ExitDos;
            count = sizeof( ExitDos) / sizeof( char * );
        } else {
            p = ExitOS2;
            count = sizeof( ExitOS2) / sizeof( char * );
        }
        j = i;
        i++;
        if ( AsmBuffer[i]->token != T_FINAL ) {
            if( ModuleInfo.ostype == OPSYS_OS2 ) {
                sprintf( buffer, "mov ax,%s", AsmBuffer[j]->pos + 5);
            } else {
                if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                    return( ERROR );
                if ( opndx.kind == EXPR_CONST && opndx.value < 0x100 && AsmBuffer[i]->token == T_FINAL ) {
                    sprintf( buffer, "mov ax,4C%02Xh", opndx.value );
                } else {
                    sprintf( buffer, "mov al,%s", AsmBuffer[j]->pos + 5);
                    AddLineQueue( buffer );
                    strcpy( buffer, "mov ah,4Ch" );
                }
            }
            AddLineQueue( buffer );
            p++;
            count--;
        }
        for( ; count ; count--, p++ ) {
            AddLineQueue( (char *)*p );
        }
        break;
    }

    RunLineQueue();

    return( NOT_ERROR );
}

static asm_sym * AddPredefinedConstant( const char *name, int value )
/*******************************************************************/
{
    asm_sym * sym = CreateConstantEx( name, value );
    if (sym)
        sym->predefined = TRUE;
    return(sym);
}

static void AddPredefinedText( const char *name, char *value )
/************************************************************/
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

/* Generates codes for .MODEL; based on optasm pg.142-146.
 * called by ModelDirective()
 */
static void SetModel( void )
/***************************/
{
    int         value;
    char        *textvalue;
    //asm_sym     *sym;

    DebugMsg(("SetModel() enter (model=%u)\n", ModuleInfo.model ));
    /* if model is set, it disables OT_SEGMENT of -Zm switch */
    if ( ModuleInfo.model == MOD_FLAT )
        ModuleInfo.offsettype = OT_FLAT;
    else
        ModuleInfo.offsettype = OT_GROUP;

    PushLineQueue();
    ModelSegmentInit( ModuleInfo.model ); /* create segments in first pass */
    ModelAssumeInit();

    if ( ModuleInfo.list )
        LstWriteSrcLine();

    RunLineQueue();

    if (Parse_Pass != PASS_1)
        return;

    /* Set @CodeSize */
    switch( ModuleInfo.model ) {
    case MOD_MEDIUM:
    case MOD_LARGE:
    case MOD_HUGE:
        value = 1;
        SimpleType[ST_PROC].mem_type = MT_FAR;
        break;
    default:
        value = 0;
        // SimpleType[ST_PROC].mem_type = MT_NEAR; /* this is default */
        break;
    }
    sym_CodeSize = AddPredefinedConstant( "@CodeSize", value );
    AddPredefinedText( "@code", GetCodeSegName() );

    /* Set @DataSize */
    switch( ModuleInfo.model ) {
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

    if ( ModuleInfo.model == MOD_FLAT )
        textvalue = "FLAT";
    else
        textvalue = "DGROUP";

    AddPredefinedText( "@data", textvalue );

    if ( ModuleInfo.distance == STACK_FAR )
        textvalue = "STACK";
    AddPredefinedText( "@stack", textvalue );

    /* Set @Model and @Interface */

    sym_Model     = AddPredefinedConstant( "@Model", ModuleInfo.model );
    sym_Interface = AddPredefinedConstant( "@Interface", ModuleInfo.langtype );
}

#if 0

// get instruction string

void GetInsString( enum asm_token token, char *string, int len )
/**************************************************************/
{
    const char *name;

    if( len > AsmResWord[ token ].len ) {
        len = AsmResWord[ token ].len;
        name = AsmResWord[ token ].name;
        if( *name == '.' ) {
            name++;
            len--;
        }
        strncpy( string, name, len );
        string[ len ] = NULLC;
    } else {
        *string= NULLC;
    }
    return;
}

// this is to define values like __386__, __486__, ...
// if directives like .386, .486, ... are found.
// it isn't Masm-compatible.

void MakeConstantUnderscored( int token )
/***************************************/
{
    char buffer[23];

    /* define a macro */

    strcpy( buffer, "__" );
    GetInsString( (enum asm_token)token, buffer+2, 18 );
    strcat( buffer, "__" );
    _strupr( buffer );
    CreateConstantEx( buffer, 1 );
    return;
}
#endif

// set default wordsize for segment definitions

static ret_code SetDefaultOfssize( int size )
/*******************************************/
{
    /* outside any segments? */
    if( CurrSeg == NULL ) {
        ModuleInfo.defOfssize = size;
    }
    return( SetOfssize() );
}

// handle .model directive
// syntax: .MODEL <FLAT|TINY|SMALL...> [,<C|PASCAL|STDCALL...>][,<NEARSTACK|FARSTACK>][,<OS_DOS|OS_OS2>]

static ret_code ModelDirective( int i )
/*************************************/
{
    const typeinfo *type;           // type of option
    mod_type model;
    lang_type language;
    dist_type distance;
    os_type ostype;
    uint_16 init;

    DebugMsg(("ModelDirective enter\n"));
    if( Parse_Pass != PASS_1 ) {
        SetModel();
        return( NOT_ERROR );
    }

    i++;
    if ( AsmBuffer[i]->token == T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->string_ptr );
        return( ERROR );
    }
    /* get the model argument */
    if( type = FindToken( AsmBuffer[i]->string_ptr, ModelInfo, sizeof(ModelInfo)/sizeof(typeinfo) )) {
        if( ModuleInfo.model != MOD_NONE ) {
            AsmWarn( 2, MODEL_DECLARED_ALREADY );
            return( NOT_ERROR );
        }
        model = type->value;
        i++;
    } else {
        AsmError( EXPECTED_MEMORY_MODEL );
        return( ERROR );
    }

    /* get the optional arguments: language, stack distance, os */
    init = 0;
    while ( i < ( Token_Count - 1 ) && AsmBuffer[i]->token == T_COMMA ) {
        i++;
        if ( AsmBuffer[i]->token != T_COMMA ) {
            if ( GetLangType( &i, &language ) == NOT_ERROR ) {
                type = &dmyLang;
            } else if ( type = FindToken( AsmBuffer[i]->string_ptr, StackInfo, sizeof(StackInfo)/sizeof(typeinfo) ) ) {
                if ( model == MOD_FLAT ) {
                    AsmError( INVALID_MODEL_PARAM_FOR_FLAT );
                    return( ERROR );
                }
                i++;
                distance = type->value;
            } else if ( type = FindToken( AsmBuffer[i]->string_ptr, OsInfo, sizeof(OsInfo)/sizeof(typeinfo) ) ) {
                i++;
                ostype = type->value;
            } else {
                break;
            }
            if ( type->init & init ) {
                i--;
                break;
            }
            init |= type->init;
        }
    }
    /* everything parsed successfully? */
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    ModuleInfo.model = model;
    if ( init & INIT_LANG )
        ModuleInfo.langtype = language;
    if ( init & INIT_STACK )
        ModuleInfo.distance = distance;
    if ( init & INIT_OS )
        ModuleInfo.ostype = ostype;

    if ( model == MOD_FLAT ) {
        DefineFlatGroup();
#if AMD64_SUPPORT
        SetDefaultOfssize( ((ModuleInfo.curr_cpu & P_CPU_MASK) >= P_64 ) ? USE64 : USE32 );
#else
        SetDefaultOfssize( USE32 );
#endif
    }
    SetModelDefaultSegNames();
    SetModel();

    return( NOT_ERROR );
}

static ret_code EndDirective( int i, struct code_info *CodeInfo )
/***************************************************************/
{
    struct fixup        *fixup;
    expr_list           opndx;
    struct asmfixup     *fix;
    asm_sym             *sym;

    DebugMsg(("EndDirective enter\n"));
#if FASTPASS
    /* if there's no code or data emitted, init the line store now */
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif
    if ( CurrStruct ) {
        while ( CurrStruct->next )
            CurrStruct = CurrStruct->next;
        AsmErr( UNMATCHED_BLOCK_NESTING, CurrStruct->sym.name );
    }
    /* close open segments */
    SegmentModuleExit();

    if( ModuleInfo.StartupDirectiveFound ) {
        /* start label behind END ignored if .STARTUP has been found */
        if( i < Token_Count && Parse_Pass == PASS_1 ) {
            AsmWarn( 2, START_ADDRESS_IGNORED );
        }
        AsmBuffer[i]->token = T_ID;
        AsmBuffer[i]->string_ptr = StartAddr;
        AsmBuffer[i+1]->token = T_FINAL;
        AsmBuffer[i+1]->string_ptr = "";
        Token_Count = i+1;
    }

    ModuleInfo.EndDirectiveFound = TRUE;

    if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
        return( ERROR );
    }
    if( AsmBuffer[i]->token != T_FINAL) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if( opndx.kind == EXPR_EMPTY )
        ;
    else if ( opndx.kind == EXPR_ADDR && opndx.indirect == FALSE ) {
        Modend = TRUE;
        process_address( CodeInfo, &opndx );
        fix = CodeInfo->InsFixup[0];
        if ( fix )
            sym = fix->sym;
        if ( fix == NULL || sym == NULL ) {
            AsmError( INVALID_START_ADDRESS );
            return( ERROR );
        } else if ( sym->state == SYM_INTERNAL || sym->state == SYM_EXTERNAL ) {
            if ( sym->mem_type == MT_NEAR || sym->mem_type == MT_FAR || sym->mem_type == MT_PROC )
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

    if ( Options.output_format == OFORMAT_OMF ) {
        ModendRec = OmfNewRec( CMD_MODEND );
        ModendRec->d.modend.main_module = FALSE;

        if( opndx.kind == EXPR_EMPTY ) {
            ModendRec->d.modend.start_addrs = FALSE;
            return( NOT_ERROR );
        }

        ModendRec->d.modend.start_addrs = TRUE;
        ModendRec->d.modend.is_logical = TRUE;
        ModendRec->d.modend.main_module = TRUE;
        ModendRec->is_32 = GetSymOfssize(sym); /* USE16 or USE32 */

        fix->offset = sym->offset + opndx.value;

        if ( CodeInfo->InsFixup[0] ) {
            if ( fixup = omf_create_fixup( CodeInfo->InsFixup[0] ) )
                ModendRec->d.modend.ref.log = fixup->lr;
        }
    } else {
        // Masm silently ignores start for -coff if an offset was given
        //if ( opndx.kind == EXPR_EMPTY || opndx.value )
        if ( opndx.kind == EXPR_EMPTY )
            return( NOT_ERROR );

        if ( sym->state != SYM_EXTERNAL && sym->public == FALSE ) {
            sym->public = TRUE;
            AddPublicData( sym );
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
/*************************************/
{
    //char *tmp;
    asm_sym *sym;

    i++; /* go past ALIAS */

    if ( AsmBuffer[i]->token != T_STRING ||
        AsmBuffer[i]->string_delim != '<' ) {
        DebugMsg(("AliasDirective: text item not found: %s\n", AsmBuffer[i]->string_ptr ));
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }

    /* check syntax. note that '=' is T_DIRECTIVE && T_EQU && DRT_EQUALSGN */
    if ( AsmBuffer[i+1]->token != T_DIRECTIVE ||
        AsmBuffer[i+1]->value != T_EQU ||
        AsmBuffer[i+1]->dirtype != DRT_EQUALSGN ) {
        DebugMsg(("AliasDirective: syntax error: %s\n", AsmBuffer[i+1]->string_ptr ));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+1]->string_ptr );
        return( ERROR );
    }

    if ( AsmBuffer[i+2]->token != T_STRING ||
        AsmBuffer[i+2]->string_delim != '<' )  {
        DebugMsg(("AliasDirective: text item not found: %s\n", AsmBuffer[i+2]->string_ptr ));
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }

    if ( AsmBuffer[i+3]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+3]->string_ptr );
        return( ERROR );
    }

    /* make sure <alias_name> isn't defined elsewhere */
    sym = SymSearch( AsmBuffer[i]->string_ptr );
    if (sym != NULL) {
        if ( sym->state != SYM_ALIAS || (strcmp( sym->string_ptr, AsmBuffer[i+2]->string_ptr ) != 0)) {
            DebugMsg(("AliasDirective: symbol redefinition\n"));
            AsmErr( SYMBOL_REDEFINITION, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        /* ignore multiple definitions */
        return( NOT_ERROR );
    }

    if ( Parse_Pass != PASS_1 ) {
        /* for COFF, make sure <actual_name> is defined elsewhere */
        /* fixme: what about ELF format? */
        if ( Parse_Pass == PASS_2 && Options.output_format == OFORMAT_COFF ) {
            sym = SymSearch( AsmBuffer[i+2]->string_ptr );
            if (sym == NULL ||
                ( sym->state != SYM_INTERNAL && sym->state != SYM_EXTERNAL )) {
                AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i+2]->string_ptr );
                return( ERROR );
            }
        }
    } else {
        sym = (asm_sym *)SymCreate( AsmBuffer[i]->string_ptr, TRUE );
        if ( sym == NULL )
            return( ERROR );
        sym->state = SYM_ALIAS;
        sym->string_ptr = AsmAlloc( strlen( AsmBuffer[i+2]->string_ptr ) + 1 );
        strcpy( sym->string_ptr, AsmBuffer[i+2]->string_ptr );
        dir_add_table( (dir_node *)sym );
    }
    return( NOT_ERROR );
}

// the NAME directive is ignored in Masm v6

static ret_code NameDirective( int i )
/************************************/
{
    /* if a module name is set with -nm, ignore NAME directive! */
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
    if ( CurrStruct != NULL ||
        (AsmBuffer[i]->token == T_DIRECTIVE &&
         (AsmBuffer[i]->value == T_SEGMENT ||
          AsmBuffer[i]->value == T_STRUCT  ||
          AsmBuffer[i]->value == T_STRUC   ||
          AsmBuffer[i]->value == T_UNION   ||
          AsmBuffer[i]->value == T_TYPEDEF ||
          AsmBuffer[i]->value == T_RECORD)) ||
         AsmBuffer[i]->token == T_COLON ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->pos );
        return( ERROR );
    }

#if 0 /* don't touch Option fields! ( ModuleInfo.name probably? ) */
    Options.module_name = AsmAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1 );
    strcpy( Options.module_name, AsmBuffer[i]->string_ptr );
    DebugMsg(("NameDirective: set name to >%s<\n", Options.module_name));
#endif
    return( NOT_ERROR );
}

// .RADIX directive, value must be between 2 .. 16

static ret_code RadixDirective( int i )
/*************************************/
{
    uint_8          oldradix;
    expr_list       opndx;

    /* to get the .radix parameter, enforce radix 10 and retokenize! */
    oldradix = ModuleInfo.radix;
    ModuleInfo.radix = 10;
    Tokenize( AsmBuffer[i]->pos, i );
    ModuleInfo.radix = oldradix;
    i++;
    if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
        return( ERROR );
    }

    if ( opndx.kind != EXPR_CONST ) {
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }
    if ( opndx.value > 16 || opndx.value < 2 || opndx.hvalue != 0 ) {
        AsmError( INVALID_RADIX_TAG );
        return( ERROR );
    }
    if (AsmBuffer[i]->token != T_FINAL) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    ModuleInfo.radix = opndx.value;
    DebugMsg(("RadixDirective: new radix=%u\n", ModuleInfo.radix ));

    return( NOT_ERROR );
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

ret_code SetCPU( enum asm_cpu newcpu )
/************************************/
{
    int temp;

    DebugMsg(("SetCPU(%X) enter\n", newcpu ));
    if ( newcpu == P_86 || ( newcpu & P_CPU_MASK ) ) {
        /* reset CPU and EXT bits */
        ModuleInfo.curr_cpu &= ~( P_CPU_MASK | P_EXT_MASK | P_PM );

        /* set CPU bits */
        ModuleInfo.curr_cpu |= newcpu & ( P_CPU_MASK | P_PM );

        /* set default FPU bits if nothing is given and .NO87 not active */
        if ( (ModuleInfo.curr_cpu & P_FPU_MASK) != P_NO87 &&
            ( newcpu & P_FPU_MASK ) == 0 ) {
            ModuleInfo.curr_cpu &= ~P_FPU_MASK;
            if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_286 )
                ModuleInfo.curr_cpu |= P_87;
            else if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 )
                ModuleInfo.curr_cpu |= P_287;
            else
                ModuleInfo.curr_cpu |= P_387;
        }

    }
    if( newcpu & P_FPU_MASK ) {
        ModuleInfo.curr_cpu &= ~P_FPU_MASK;
        ModuleInfo.curr_cpu |= (newcpu & P_FPU_MASK);
    }
#if AMD64_SUPPORT
    /* enable MMX, K3D, SSEx for 64bit cpus */
    if ( ( newcpu & P_CPU_MASK ) == P_64 )
        ModuleInfo.curr_cpu |= P_EXT_ALL;
#endif
    if( newcpu & P_EXT_MASK ) {
        ModuleInfo.curr_cpu &= ~P_EXT_MASK;
        ModuleInfo.curr_cpu |= (newcpu & P_EXT_MASK);
    }

    /* set the Masm compatible @Cpu value */

    temp = ModuleInfo.curr_cpu & P_CPU_MASK;
    switch ( temp ) {
    case P_186: ModuleInfo.cpu = M_8086 | M_186; break;
    case P_286: ModuleInfo.cpu = M_8086 | M_186 | M_286; break;
    case P_386: ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386; break;
    case P_486: ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486; break;
    case P_586: ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486 | M_586; break;
    /* Masm's .686 directive doesn't set the Pentium flag! A bug? */
    //case P_686: ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486 | M_586 | M_686; break;
#if AMD64_SUPPORT
    case P_64:
#endif
    case P_686: ModuleInfo.cpu = M_8086 | M_186 | M_286 | M_386 | M_486 | M_686; break;
    default: ModuleInfo.cpu = M_8086; break;
    }
    if ( ModuleInfo.curr_cpu & P_PM )
        ModuleInfo.cpu = ModuleInfo.cpu | M_PROT;

    temp = ModuleInfo.curr_cpu & P_FPU_MASK;
    switch (temp) {
    case P_87:  ModuleInfo.cpu = ModuleInfo.cpu | M_8087;     break;
    case P_287: ModuleInfo.cpu = ModuleInfo.cpu | M_8087 | M_287; break;
    case P_387: ModuleInfo.cpu = ModuleInfo.cpu | M_8087 | M_287 | M_387; break;
    }

    DebugMsg(("SetCPU: ModuleInfo.curr_cpu=%X, @Cpu=%X\n", ModuleInfo.curr_cpu, ModuleInfo.cpu ));

    //MakeCPUConstant( newcpu );
    if ( ModuleInfo.model == MOD_NONE )
#if AMD64_SUPPORT
        if ( ( ModuleInfo.curr_cpu & P_CPU_MASK) >= P_64 ) {
            SetDefaultOfssize( USE64 );
        } else
#endif
            SetDefaultOfssize( ((ModuleInfo.curr_cpu & P_CPU_MASK) >= P_386) ? USE32 : USE16 );

    /* Set @Cpu */
    /* differs from Codeinfo cpu setting */

    sym_Cpu = CreateConstantEx( "@Cpu", ModuleInfo.cpu );

    return( NOT_ERROR );
}

#if 0
static int comp_opt( uint direct )
/********************************/
/*
  Compare function for CPU directive
*/
{
    switch( direct ) {
    case T_DOT_NO87:        return( P_NO87 );
    case T_DOT_8086:        return( P_86 );
    case T_DOT_8087:        return( P_87 );
    case T_DOT_186:         return( P_186 );
    case T_DOT_286C:
    case T_DOT_286:         return( P_286 );
    case T_DOT_286P:        return( P_286p );
    case T_DOT_287:         return( P_287 );
    case T_DOT_386C:
    case T_DOT_386:         return( P_386 );
    case T_DOT_386P:        return( P_386p );
    case T_DOT_387:         return( P_387 );
    case T_DOT_486:         return( P_486 );
    case T_DOT_486P:        return( P_486p );
    case T_DOT_586:         return( P_586 );
    case T_DOT_586P:        return( P_586p );
    case T_DOT_686:         return( P_686 );
    case T_DOT_686P:        return( P_686p );
#if AMD64_SUPPORT
    case T_DOT_X64:         return( P_64 );
    case T_DOT_X64P:        return( P_64p );
#endif
    case T_DOT_MMX:         return( P_MMX );
    case T_DOT_K3D:         return( P_MMX | P_K3D );
#if ONEXMM
    case T_DOT_XMM:         return( P_MMX | P_SSEALL );
#else
    case T_DOT_XMM:         return( P_MMX | P_SSE1 );
    case T_DOT_XMM2:        return( P_MMX | P_SSE1 | P_SSE2 );
    case T_DOT_XMM3:        return( P_MMX | P_SSE1 | P_SSE2 | P_SSE3 | P_SSSE3 );
#endif
    }
    return( EMPTY );
}
#endif

static ret_code cpu_directive( int i )
/************************************/
{
    enum asm_cpu newcpu;

    //newcpu = comp_opt( AsmBuffer[i]->value );
    //newcpu = AsmOpTable[optable_idx[AsmBuffer[i]->value]].opnd_type[0];
    newcpu = GetOpndType( AsmBuffer[i]->value, 0 );

    if ( SetCPU( newcpu ) == NOT_ERROR ) {
        i++;
        if ( AsmBuffer[i]->token == T_FINAL )
            return( NOT_ERROR );
    }
    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
    return( ERROR );
}

// .[NO|X]LIST, .[NO|X]CREF, .[NO]LISTIF, .[NO]LISTMACRO
// .LISTALL, .LISTMACROALL, .[LF|SF|TF]COND, .[X|L|S]ALL
// PAGE, TITLE, SUBTITLE, SUBTTL directives

static ret_code ListingDirective( int i )
/***************************************/
{
    int directive = AsmBuffer[i]->value;
    i++;

    switch ( directive ) {
    case T_DOT_LIST:
        if ( FileInfo.file[LST] )
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
        if ( AsmBuffer[i]->token == T_FINAL ) {
            ModuleInfo.cref = FALSE;
        } else {
            asm_sym *sym;
            while ( AsmBuffer[i]->token != T_FINAL ) {
                if ( AsmBuffer[i]->token != T_ID ) {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                    return( ERROR );
                }
                if ( sym = SymLookup( AsmBuffer[i]->string_ptr )) {
                    sym->list = FALSE;
                }
                i++;
                if ( AsmBuffer[i]->token != T_FINAL ) {
                    if ( AsmBuffer[i]->token == T_COMMA ) {
                        if ( (i + 1) < Token_Count )
                            i++;
                    } else {
                        AsmError( EXPECTING_COMMA );
                        return( ERROR );
                    }
                }
            }
        }
        break;
    case T_DOT_LISTALL: /* list false conditionals and generated code */
        if ( FileInfo.file[LST] )
            ModuleInfo.list = TRUE;
        ModuleInfo.list_generated_code = TRUE;
        /* fall through */
    case T_DOT_LISTIF:
    case T_DOT_LFCOND: /* .LFCOND is synonym for .LISTIF */
        ModuleInfo.listif = TRUE;
        break;
    case T_DOT_NOLISTIF:
    case T_DOT_SFCOND: /* .SFCOND is synonym for .NOLISTIF */
        ModuleInfo.listif = FALSE;
        break;
    case T_DOT_TFCOND: /* .TFCOND toggles .LFCOND, .SFCOND */
        ModuleInfo.listif = !ModuleInfo.listif;
        break;
    case T_DOT_LISTMACRO:
    case T_DOT_XALL:   /* .XALL is synonym for .LISTMACRO */
        ModuleInfo.list_macro = LM_LISTMACRO;
        break;
    case T_DOT_LISTMACROALL:
    case T_DOT_LALL:   /* .LALL is synonym for .LISTMACROALL */
        ModuleInfo.list_macro = LM_LISTMACROALL;
        break;
    case T_DOT_NOLISTMACRO:
    case T_DOT_SALL:   /* .SALL is synonym for .NOLISTMACRO */
        ModuleInfo.list_macro = LM_NOLISTMACRO;
        break;
    case T_PAGE:
    default: /* TITLE, SUBTITLE, SUBTTL */
        /* tiny checks to ensure that these directives
         aren't used as code labels or struct fields */
        if ( AsmBuffer[i]->token == T_COLON )
            break;
        if( CurrStruct ) {
            AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
            return( ERROR );
        }
        if (Parse_Pass == PASS_1)
            AsmWarn( 4, DIRECTIVE_IGNORED, AsmBuffer[i-1]->string_ptr );
        while (AsmBuffer[i]->token != T_FINAL) i++;
    }

    if (AsmBuffer[i]->token != T_FINAL) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    return( NOT_ERROR );
}

// DOSSEG, .DOSSEG, .ALPHA, .SEQ directives

static ret_code SegOrderDirective( int i )
/****************************************/
{
    if ( Options.output_format != OFORMAT_OMF &&
        Options.output_format != OFORMAT_BIN ) {
        if ( Parse_Pass == PASS_1 )
            AsmWarn( 2, DIRECTIVE_IGNORED_FOR_COFF, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    switch( AsmBuffer[i]->value ) {
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
    i++;
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

static ret_code SafeSEHDirective( int i )
/***************************************/
{
    struct asm_sym *sym;
    stacknode   *node;

    if ( Options.output_format != OFORMAT_COFF ) {
        if ( Parse_Pass == PASS_1)
            AsmWarn( 2, DIRECTIVE_IGNORED_WITHOUT_X, "coff" );
        return( NOT_ERROR );
    }
    if ( Options.safeseh == FALSE ) {
        if ( Parse_Pass == PASS_1)
            AsmWarn( 2, DIRECTIVE_IGNORED_WITHOUT_X, "safeseh" );
        return( NOT_ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    sym = SymSearch( AsmBuffer[i]->string_ptr );
    if ( Parse_Pass == PASS_2 &&
        ( sym == NULL || sym->state == SYM_UNDEFINED )) {
        AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
        return( ERROR );
    } else if ( sym && sym->state != SYM_UNDEFINED && sym->isproc == FALSE ) {
        AsmErr( SAFESEH_ARGUMENT_MUST_BE_A_PROC, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if ( Parse_Pass == PASS_1 ) {
        if ( sym ) {
            for ( node = (stacknode *)SafeSEHStack; node; node = node->next )
                if ( node->elt == sym )
                    break;
        } else {
            sym = SymCreate( AsmBuffer[i]->string_ptr, TRUE );
            node = NULL;
        }
        if ( node == NULL )
            pushitem( &SafeSEHStack, sym );
    }
    i++;
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    return( NOT_ERROR );
}

// dispatcher for all directives (exception: data directives DB, DW, ...)

ret_code directive( int i, struct code_info *CodeInfo )
/*****************************************************/
{
    int direct = AsmBuffer[i]->value;

    if ( direct >= T_DOT_8086 && direct <= T_DOT_NO87 )
        return( cpu_directive( i ) );
    else if (direct >= T_DOT_CREF && direct <= T_TITLE )
        return( ListingDirective( i ) );

    switch( direct ) {
#if FASTPASS
    case T_EQU:
        return( DefineConstant( AsmBuffer[1]->dirtype == DRT_EQUALSGN ) );
    case T_SIZESTR:
        return( SizeStrDef( i ) );
    case T_INSTR:
        return( InStrDef( i ) );
#endif
    case T_ENDS:
        if( CurrStruct != NULL ) {
            return( EndstructDirective( i ) );
        }
        // else fall through to T_SEGMENT
    case T_SEGMENT:
        return( Parse_Pass == PASS_1 ? SegDef( i ) : SetCurrSeg( i ) );
    case T_GROUP:
        return( GrpDef( i ) );
    case T_PROTO:
        return( ProtoDef( i, NULL ));
    case T_PROC:
        return( ProcDef( i ) );
    case T_ENDP:
        return( EndpDef( i ) );
    case T_LOCAL:
        return( Parse_Pass == PASS_1 ? LocalDef( i ) : NOT_ERROR );
    case T_INVOKE:
        return( InvokeDirective( i+1 ) );
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
    case T_EXTERN:
    case T_EXTRN:
        return( Parse_Pass == PASS_1 ? ExternDirective(i+1) : ExternDirective2(i+1) );
    case T_COMM:
        return( Parse_Pass == PASS_1 ? CommDirective(i+1) : NOT_ERROR );
    case T_EXTERNDEF:
        return( Parse_Pass == PASS_1 ? ExterndefDirective(i+1) : NOT_ERROR );
    case T_PUBLIC:
        return( PublicDirective(i+1) );
    case T_INCLUDELIB:
        return( Parse_Pass == PASS_1 ? IncludeLibDirective(i+1) : NOT_ERROR );
#if INCLUDEBIN
    case T_INCBIN:
        return( IncBinDirective(i+1) );
#endif
    case T_ASSUME:
        return( AssumeDirective( i ) );
    case T_STRUC:
    case T_STRUCT:
    case T_UNION:
        return( StructDirective( i ) );
    case T_TYPEDEF:
        return( Parse_Pass == PASS_1 ? TypeDirective( i ) : NOT_ERROR );
    case T_RECORD:
        return( Parse_Pass == PASS_1 ? RecordDef( i ) : NOT_ERROR );
    case T_LABEL:
        return( LabelDirective( i ) );
    case T_ORG:
        return( OrgDirective( i ) );
    case T_END:
        return( EndDirective( i+1, CodeInfo ) );
    case T_ALIGN:
    case T_EVEN:
        return( AlignDirective( direct, i ) );
    case T_DOT_MODEL:
        return( ModelDirective( i ) );
    case T_POPCONTEXT:
    case T_PUSHCONTEXT:
        return( ContextDirective( direct, i ) );
    case T_DOT_SEQ: /* this is default */
    case T_DOT_DOSSEG:
    case T_DOSSEG:
    case T_DOT_ALPHA:
        return( SegOrderDirective( i ) );
    case T_DOT_RADIX:
        return( RadixDirective( i ) );
    case T_PURGE:
        return( PurgeDef( i+1 ) );
    case T_ALIAS:
        return( AliasDirective( i ) );
    case T_ECHO:
        return( Parse_Pass == PASS_1 ? EchoDirective( i ) : NOT_ERROR );
    case T_NAME:
        return( Parse_Pass == PASS_1 ? NameDirective( i ) : NOT_ERROR );
    case T_DOT_STARTUP:
    case T_DOT_EXIT:
        return( StartupExitDirective( i ) );
    case T_ENDM:
    case T_EXITM:
        /* these directives should never be seen here */
        AsmError( UNMATCHED_MACRO_NESTING );
        return( ERROR );
    case T_GOTO:
        AsmError( DIRECTIVE_MUST_APPEAR_INSIDE_A_MACRO );
        return( ERROR );
    case T_DOT_SAFESEH:
        return( SafeSEHDirective( i ) );
#if AMD64_SUPPORT
    case T_DOT_ALLOCSTACK:
    case T_DOT_ENDPROLOG:
    case T_DOT_PUSHFRAME:
    case T_DOT_PUSHREG:
    case T_DOT_SAVEREG:
    case T_DOT_SAVEXMM128:
    case T_DOT_SETFRAME:
        return( ExcFrameDirective( i ) );
#endif
    default:
        /* this error may happen if
         * CATSTR, SUBSTR, MACRO, ...
         * aren't at pos 1
         */
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
}
