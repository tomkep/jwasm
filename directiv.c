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
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "assume.h"
#include "equate.h"
#include "fixup.h"
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

/* prototypes */

simpletype SimpleType[] = {
#undef pick
#define pick( name, memtype, ofssize ) { T_ ## name , memtype, ofssize },
#include "stypes.h"
//    { T_PTR,     MT_PTR,      USE_EMPTY }
};

symbol_queue            Tables[TAB_LAST];/* tables of definitions */

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

void pushitem( void *stk, void *elmt )
/************************************/
{
    void      **stack = stk;
    qnode     *node;

    node = AsmAlloc( sizeof( qnode ));
    node->next = *stack;
    node->elmt = elmt;
    *stack = node;
}

void *popitem( void *stk )
/************************/
{
    void        **stack = stk;
    qnode       *node;
    void        *elmt;

    node = (qnode *)(*stack);
    *stack = node->next;
    elmt = node->elmt;
    AsmFree( node );
    return( elmt );
}

#if 0
void *peekitem( void *stk, int level )
/************************************/
{
    qnode  *node = (qnode *)(stk);

    for (;node && level;level--) {
        node = node->next;
    }

    if (node)
        return( node->elt );
    else
        return( NULL );
}
#endif

void dir_add_table( symbol_queue *queue, dir_node *item )
/******************************************************/
{
#ifdef DEBUG_OUT
    if ( queue == &Tables[TAB_UNDEF] )
        item->sym.forward = TRUE;
#endif
    /* put the new entry into the queue for its type of symbol */
    if( queue->head == NULL ) {
        queue->head = queue->tail = item;
        item->next = item->prev = NULL;
    } else {
        item->prev = queue->tail;
        queue->tail->next = item;
        queue->tail = item;
        item->next = NULL;
    }
}

void dir_remove_table( symbol_queue *queue, dir_node *item )
/**********************************************************/
{
    /* unlink the node */
    if( item->prev )
        item->prev->next = item->next;
    if( item->next )
        item->next->prev = item->prev;

    //if ( dir->next == NULL )
    //    dir->next = dir->prev;

    if ( queue->head == item )
        queue->head = item->next;
    if ( queue->tail == item )
        queue->tail = item->prev;

    item->prev = NULL;
    item->next = NULL;
}

void dir_ext2int( dir_node *dir )
/********************************/
/* Change symbol type from SYM_EXTERNAL to SYM_INTERNAL
 * this is for PROTOs (isproc=1) and EXTERNDEFs (weak=1)
 */
{
    dir_remove_table( &Tables[TAB_EXT], dir );
    if ( dir->sym.isproc == FALSE ) /* v2.01: don't clear flags for PROTO */
        dir->sym.first_size = 0;
    dir->sym.state = SYM_INTERNAL;
}

ret_code GetLangType( int *i, lang_type *plang )
/**********************************************/
{
    if( AsmBuffer[*i]->token == T_RES_ID ) {
#if 1 /* v2.03: simplified */
        if ( AsmBuffer[(*i)]->value >= T_C &&
            AsmBuffer[(*i)]->value <= T_FASTCALL ) {
            *plang = AsmBuffer[(*i)]->value8;
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
        if ( SpecialTable[token].type == RWT_TYPE ) {
            return( SpecialTable[token].value8 );
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

/* get size of a register */

int SizeFromRegister( int registertoken )
/***************************************/
{
    int flags = GetValueSp( registertoken );
    if ( flags & OP_R32 )
        return( 4 );
#if AMD64_SUPPORT
    else if ( flags & OP_R64 )
        return( 8 );
#endif
    else if ( flags & OP_R16 )
        return( 2 );
    else if ( flags & OP_R8 )
        return( 1 );
    else if ( flags & OP_SR )
        return( CurrWordSize );
    else if ( flags & OP_ST )
        return( 10 );
    else if ( flags & OP_MMX )
        return( 8 );
    else if ( flags & OP_XMM )
        return( 16 ); /* masm v6x and v7 return 10, v8 returns 16 */
    else {/* CRx, DRx, TRx */
#if AMD64_SUPPORT
        return( ModuleInfo.Ofssize == USE64 ? 8 : 4 );
#else
        return( 4 );
#endif
    }
}

/* get size from memory type
 * is32 param used only for MT_NEAR/MT_FAR
 */
int SizeFromMemtype( memtype mem_type, int Ofssize, asm_sym *type )
/*****************************************************************/
{
    int  size;

    if ( ( mem_type & MT_SPECIAL) == 0 )
        return ( (mem_type & MT_SIZE_MASK) + 1 );

    if ( Ofssize == USE_EMPTY )
        Ofssize = ModuleInfo.Ofssize;

    switch ( mem_type ) {
    case MT_NEAR:
        DebugMsg1(("SizeFromMemtype( MT_NEAR, Ofssize=%u )=%u\n", Ofssize, 2 << Ofssize ));
        return ( 2 << Ofssize );
    case MT_FAR:
        DebugMsg1(("SizeFromMemtype( MT_FAR, Ofssize=%u )=%u\n", Ofssize, 2 + 2 << Ofssize ));
        return ( ( 2 << Ofssize ) + 2 );
    case MT_PROC:
        DebugMsg1(("SizeFromMemtype( MT_PROC, Ofssize=%u )=%u\n", Ofssize, 2 << Ofssize + ( SimpleType[ST_PROC].mem_type == MT_FAR ? 2 : 0 )));
        return( ( 2 << Ofssize ) +
                ( SimpleType[ST_PROC].mem_type == MT_FAR ? 2 : 0 ) );
    case MT_PTR:
        /* first determine offset size */
        size = ( 2 << Ofssize );
        if ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) )
            size += 2;      /* add segment for far data pointers */

        DebugMsg1(("SizeFromMemtype( MT_PTR, Ofssize=%u )=%u\n", Ofssize, size ));
        return( size );
    case MT_TYPE:
        if ( type )
            return( type->total_size );
    default:
        DebugMsg1(("SizeFromMemtype( memtype=%Xh, Ofssize=%u )=%u\n", mem_type, Ofssize, 0 ));
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
    const struct asm_special *curr;
    for ( curr = &SpecialTable[T_BYTE]; curr->type == RWT_TYPE; curr++ ) {
        if( ( curr->value & MT_SPECIAL ) == 0 ) {
            /* the size is encoded 0-based in field mem_type */
            if( ( ( curr->value & MT_SIZE_MASK) + 1 ) == size ) {
                *ptype = curr->value;
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
    char *node;
    qnode *q;
    //struct asm_sym *sym;

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
         * the scan has to be started from the INCLUDELIB token (i-1),
         * because this token is guaranteed to have a valid pos field.
         */
        name = AsmBuffer[i-1]->pos + 1;
        while ( is_valid_id_char( *name ) ) name++;
        while ( isspace( *name ) ) name++;
        for ( p = name; *p; p++ );
        /* remove trailing white spaces */
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
    for ( q = ModuleInfo.g.LibQueue.head; q ; q = q->next ) {
        //if ( _stricmp( dir->sym.name, name) == 0)
        if ( strcmp( q->elmt, name ) == 0 )
            return( NOT_ERROR );
    }
    node = AsmAlloc( strlen( name ) + 1 );
    strcpy( node, name );
    QAddItem( &ModuleInfo.g.LibQueue, node );

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
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
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
        if ( opndx.kind == EXPR_CONST ) {
            fileoffset = opndx.value;
        } else if ( opndx.kind != EXPR_EMPTY ) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                return( ERROR );
            if ( opndx.kind == EXPR_CONST ) {
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

    /* v2.04: tell assembler that data is emitted */
    if ( ModuleInfo.CommentDataInCode )
        omf_OutSelect( TRUE );

    /* try to open the file */
    DebugMsg(("IncBinDirective: filename=%s, offset=%u, size=%u\n", filename, fileoffset, sizemax ));
    if ( InputQueueFile( filename, &file ) == NOT_ERROR ) {
        /* transfer file content to the current segment. */
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

/* Alias directive.
 * Masm syntax is:
 *   'ALIAS <alias_name> = <substitute_name>'
 * which looks somewhat strange if compared to other Masm directives.
 * (OW Wasm syntax is 'alias_name ALIAS substitute_name', which is
 * what one might have expected for Masm as well).
 *
 * <alias_name> is a global name and must be unique (that is, NOT be
 * defined elsewhere in the source!
 * <substitute_name> is the name which is defined in the source.
 * For COFF and ELF, this name MUST be defined somewhere as
 * external or public!
 */

static ret_code AliasDirective( int i )
/*************************************/
{
    //char *tmp;
    asm_sym *sym;
    char *subst;

    i++; /* go past ALIAS */

    if ( AsmBuffer[i]->token != T_STRING ||
        AsmBuffer[i]->string_delim != '<' ) {
        DebugMsg(("AliasDirective: first argument is not a literal: %s\n", AsmBuffer[i]->string_ptr ));
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
        DebugMsg(("AliasDirective: second argument is not a literal: %s\n", AsmBuffer[i+2]->string_ptr ));
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    subst = AsmBuffer[i+2]->string_ptr;

    if ( AsmBuffer[i+3]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+3]->string_ptr );
        return( ERROR );
    }

    /* make sure <alias_name> isn't defined elsewhere */
    sym = SymSearch( AsmBuffer[i]->string_ptr );
    if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
        asm_sym *sym2;
        /* v2.04b: adjusted to new field <substitute> */
        sym2 = SymSearch( subst );
        if ( sym2 == NULL ) {
            sym2 = SymCreate( subst, TRUE );
            sym2->state = SYM_UNDEFINED;
            dir_add_table( &Tables[TAB_UNDEF], (dir_node *)sym2 );
        } else if ( sym2->state != SYM_UNDEFINED &&
                   sym2->state != SYM_INTERNAL &&
                   sym2->state != SYM_EXTERNAL ) {
            AsmErr( MUST_BE_PUBLIC_OR_EXTERNAL, subst );
            return( ERROR );
        }
        if ( sym == NULL )
            sym = (asm_sym *)SymCreate( AsmBuffer[i]->string_ptr, TRUE );
        else
            dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );

        sym->state = SYM_ALIAS;
        sym->substitute = sym2;
        dir_add_table( &Tables[TAB_ALIAS], (dir_node *)sym ); /* add ALIAS */
        return( NOT_ERROR );
    }
    if ( sym->state != SYM_ALIAS || ( strcmp( sym->substitute->name, subst ) != 0 )) {
        DebugMsg(("AliasDirective: symbol redefinition\n"));
        AsmErr( SYMBOL_REDEFINITION, sym->name );
        return( ERROR );
    }
#if COFF_SUPPORT || ELF_SUPPORT
    /* for COFF+ELF, make sure <actual_name> is "global" (EXTERNAL or
     * public INTERNAL). For OMF, there's no check at all. */
    if ( Parse_Pass != PASS_1 ) {
        if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
             || Options.output_format == OFORMAT_ELF
#endif
           ) {
            if ( sym->substitute->state == SYM_UNDEFINED ) {
                AsmErr( SYMBOL_NOT_DEFINED, subst );
                return( ERROR );
            } else if ( sym->substitute->state != SYM_EXTERNAL &&
                       ( sym->substitute->state != SYM_INTERNAL || sym->substitute->public == FALSE ) ) {
                AsmErr( MUST_BE_PUBLIC_OR_EXTERNAL, subst );
                return( ERROR );
            }
        }
    }
#endif
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
    Tokenize( AsmBuffer[i]->pos, i, FALSE );
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
    if ( AsmBuffer[i]->token != T_FINAL ) {
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
        return( ProtoDef( i ));
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
        return( ExternDirective( i ) );
    case T_COMM:
        return( CommDirective( i ) );
    case T_EXTERNDEF:
        return( ExterndefDirective( i ) );
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
#if COFF_SUPPORT
    case T_DOT_SAFESEH:
        return( SafeSEHDirective( i ) );
#endif
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
