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
*               AliasDirective()    - ALIAS
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

#include "myassert.h"

/* prototypes */

typedef struct stacknode {
    void    *next;
    void    *elt;
} stacknode;

simpletype SimpleType[] = {
#undef pick
#define pick( name, memtype, ofssize ) { T_ ## name , memtype, ofssize },
#include "stypes.h"
//    { T_PTR,     MT_PTR,      USE_EMPTY }
};

symbol_queue            Tables[TAB_LAST];/* tables of definitions */

stacknode               *SafeSEHStack;

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
        DebugMsg(("dir_add_table: unhandled symbol '%s', state=%X\n", new->sym.name, new->sym.state ));
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
        dir->e.seginfo->alignment = 4; /* this is PARA (2^4) */
        dir->e.seginfo->segrec->d.segdef.combine = COMB_INVALID;
        /* null class name, in case none is mentioned */
        dir->e.seginfo->segrec->d.segdef.class_name_idx = 1;
        //dir->sym.max_offset = 0;
        //dir->e.seginfo->segrec->d.segdef.access_valid = FALSE;
        break;
    case SYM_GRP:
        dir->sym.state = SYM_GRP;
        dir->e.grpinfo = AsmAlloc( sizeof( grp_info ) );
        dir->e.grpinfo->seglist = NULL;
        //dir->e.grpinfo->grp_idx = 0;
        //dir->e.grpinfo->lname_idx = 0;
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
        /* unknown symbol type */
        DebugMsg(("dir_init: unhandled symbol '%s', type=%X\n", dir->sym.name, type ));
        /**/myassert( 0 );
        return;
    }
    dir_add_table( dir ); /* add SEG | GRP */
    return;
}

dir_node *dir_create( const char *name, int type )
/************************************************/
/* Insert a node into the table specified by tab */
{
    dir_node *dir;

    /**/myassert( name != NULL );

    if ( dir = (dir_node *)SymCreate( name, *name != NULLC ) )
        dir_init( dir, type );

    return( dir );
}

/* alloc a dir_node and do NOT insert it into the symbol table.
 * used for (temporary) externals created for PROTO items,
 * nested STRUCTs, ...
 */
dir_node *dir_create_ex( const char *name, int tab )
/**************************************************/
{
    dir_node *dir;

    /**/myassert( name != NULL );

    if ( dir = (dir_node *)SymCreate( name, FALSE ) )
        dir_init( dir, tab );

    return( dir );
}

/* free a dir_node */

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
    if ( remove == TRUE )
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
#if 1 /* v2.03: simplified */
        if ( AsmBuffer[(*i)]->value >= T_C &&
            AsmBuffer[(*i)]->value <= T_FASTCALL ) {
            *plang = AsmBuffer[(*i)]->opcode;
            (*i)++;
            return( NOT_ERROR );
        }
#else
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
#endif
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

/* get size from memory type
 * is32 param used only for MT_NEAR/MT_FAR
 */
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

/* get memory type from size */

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

/*
 * find token in a 'typeinfo' table
 */

const typeinfo *FindToken( const char *token, const typeinfo *table, int size )
/*****************************************************************************/
{
    for( ; size; size--, table++ ) {
        if( _stricmp( table->string, token ) == 0 ) {
            return( table );
        }
    }
    return( NULL );  /* Not found */
}

/* handle ECHO
 * displays text on the console
 */
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

/* called during pass 1 only */

static ret_code IncludeLibDirective( int i )
/******************************************/
{
    char *name;
    dir_node *dir;
    struct asm_sym *sym;

    i++; /* skip the directive */
    /* v2.03: library name may be just a "number" */
    //if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM ) {
    if ( AsmBuffer[i]->token == T_FINAL ) {
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
        char *p;
        /* regard "everything" behind the INCLUDELIB as the library name,
         * just skip preceding/trailing white spaces.
         *
         * the scan has to be started from the INCLUDEBIN token (i-1),
         * because this token is guaranteed to have a valid pos field.
         */
        name = AsmBuffer[i-1]->pos + 1;
        while ( is_valid_id_char( *name ) ) name++;
        while ( isspace( *name ) ) name++;
        for ( p = name; *p; p++ );
        for ( p--; p > name && isspace(*p); *p = NULLC, p-- );
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

/* INCBIN directive */

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

    i++; /* skip the directive */
    /* v2.03: library name may be just a "number" */
    //if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM ) {
    if ( AsmBuffer[i]->token == T_FINAL ) {
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
        for( ; sizemax; sizemax-- ) {
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
        /* for COFF+ELF, make sure <actual_name> is defined elsewhere */
        //if ( Parse_Pass == PASS_2 && Options.output_format == OFORMAT_COFF ) {
        if ( Parse_Pass == PASS_2 &&
            ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
             || Options.output_format == OFORMAT_ELF
#endif
            ) ) {
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
        dir_add_table( (dir_node *)sym ); /* add ALIAS */
    }
    return( NOT_ERROR );
}

/* the NAME directive is ignored in Masm v6 */

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

/* .RADIX directive, value must be between 2 .. 16 */

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

/* DOSSEG, .DOSSEG, .ALPHA, .SEQ directives */

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

/* dispatcher for all directives (exception: data directives DB, DW, ...) */

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
        return( EndsDir( i ) );
    case T_SEGMENT:
        return( Parse_Pass == PASS_1 ? SegmentDir( i ) : SetCurrSeg( i ) );
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
        return( InvokeDirective( i ) );
    case T_DOT_CODE:
    case T_DOT_STACK:
    case T_DOT_DATA:
    case T_DOT_DATA_UN:
    case T_DOT_FARDATA:
    case T_DOT_FARDATA_UN:
    case T_DOT_CONST:
        return( SimplifiedSegDir( i ) );
    case T_OPTION:
        return( OptionDirective( i ) );
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
        return( Parse_Pass == PASS_1 ? ExternDirective( i ) : ExternDirective2( i ) );
    case T_COMM:
        return( Parse_Pass == PASS_1 ? CommDirective( i ) : NOT_ERROR );
    case T_EXTERNDEF:
        return( Parse_Pass == PASS_1 ? ExterndefDirective( i ) : NOT_ERROR );
    case T_PUBLIC:
        return( PublicDirective( i ) );
    case T_INCLUDELIB:
        return( Parse_Pass == PASS_1 ? IncludeLibDirective( i ) : NOT_ERROR );
#if INCLUDEBIN
    case T_INCBIN:
        return( IncBinDirective( i ) );
#endif
    case T_ASSUME:
        return( AssumeDirective( i ) );
    case T_STRUC:
    case T_STRUCT:
    case T_UNION:
        return( StructDirective( i ) );
    case T_TYPEDEF:
        return( Parse_Pass == PASS_1 ? TypedefDirective( i ) : NOT_ERROR );
    case T_RECORD:
        return( Parse_Pass == PASS_1 ? RecordDef( i ) : NOT_ERROR );
    case T_LABEL:
        return( LabelDirective( i ) );
    case T_ORG:
        return( OrgDirective( i ) );
    case T_END:
        return( EndDirective( i, CodeInfo ) );
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
        return( PurgeDef( i ) );
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
