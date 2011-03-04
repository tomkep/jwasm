/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  processes directives:
*               EchoDirective()     - ECHO
*               IncludeLibDirective() - INCLUDELIB
*               IncBinDirective()   - INCBIN
*               AliasDirective()    - ALIAS
*               NameDirective()     - NAME
*               RadixDirective()    - .RADIX
*               SegOrderDirective() - .DOSSEG, .SEQ, .ALPHA
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "input.h"
#include "tokenize.h"
#include "expreval.h"
#include "types.h"
#include "fastpass.h"
#include "listing.h"
#include "omf.h"

#include "myassert.h"

/* handle ECHO directive.
 * displays text on the console
 */
ret_code EchoDirective( int i )
/*****************************/
{
    if ( Parse_Pass == PASS_1 ) /* display in pass 1 only */
        if ( Options.preprocessor_stdout == FALSE ) { /* don't print to stdout if -EP is on! */
            printf( "%s\n", AsmBuffer[i+1]->tokpos );
        }
    return( NOT_ERROR );
}

/* directive INCLUDELIB */

ret_code IncludeLibDirective( int i )
/***********************************/
{
    char *name;
    char *node;
    qnode *q;
    //struct asm_sym *sym;

    if ( Parse_Pass != PASS_1 ) /* do all work in pass 1 */
        return( NOT_ERROR );
    i++; /* skip the directive */
    /* v2.03: library name may be just a "number" */
    //if ( AsmBuffer[i]->token == T_FINAL || AsmBuffer[i]->token == T_NUM ) {
    if ( AsmBuffer[i]->token == T_FINAL ) {
        /* v2.05: Masm doesn't complain if there's no name, so emit a warning only! */
        //AsmError( LIBRARY_NAME_MISSING );
        //return( ERROR );
        AsmWarn( 2, LIBRARY_NAME_MISSING );
    }

    if ( AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
        if ( AsmBuffer[i+1]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+1]->string_ptr );
            return( ERROR );
        }
        name = AsmBuffer[i]->string_ptr;
    } else {
        char *p;
        /* regard "everything" behind the INCLUDELIB as the library name, */
        name = AsmBuffer[i]->tokpos;
        for ( p = name; *p; p++ );
        /* remove trailing white spaces */
        for ( p--; p > name && isspace( *p ); *p = NULLC, p-- );
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

ret_code IncBinDirective( int i )
/*******************************/
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
        if ( EvalOperand( &i, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );
        if ( opndx.kind == EXPR_CONST ) {
            fileoffset = opndx.value;
        } else if ( opndx.kind != EXPR_EMPTY ) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA ) {
            i++;
            if ( EvalOperand( &i, Token_Count, &opndx, 0 ) == ERROR )
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

ret_code AliasDirective( int i )
/******************************/
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

ret_code NameDirective( int i )
/*****************************/
{
    if( Parse_Pass != PASS_1 )
        return( NOT_ERROR );
    /* if a module name is set with -nm, ignore NAME directive! */
    if( Options.names[OPTN_MODULE_NAME] != NULL )
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
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->tokpos );
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

ret_code RadixDirective( int i )
/******************************/
{
    uint_8          oldradix;
    expr_list       opndx;

    /* to get the .radix parameter, enforce radix 10 and retokenize! */
    oldradix = ModuleInfo.radix;
    ModuleInfo.radix = 10;
    Tokenize( AsmBuffer[i]->tokpos, i, FALSE );
    ModuleInfo.radix = oldradix;
    i++;
    if ( EvalOperand( &i, Token_Count, &opndx, 0 ) == ERROR ) {
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

ret_code SegOrderDirective( int i )
/*********************************/
{
    if ( AsmBuffer[i+1]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i+1]->string_ptr );
        return( ERROR );
    }

    if ( Options.output_format != OFORMAT_OMF &&
        Options.output_format != OFORMAT_BIN ) {
        if ( Parse_Pass == PASS_1 )
            AsmWarn( 2, DIRECTIVE_IGNORED_FOR_COFF, AsmBuffer[i]->string_ptr );
    } else {
#if 1 /* v2.05 */
        ModuleInfo.segorder = GetSflagsSp( AsmBuffer[i]->value );
#else
        switch( AsmBuffer[i]->value ) {
        case T_DOT_DOSSEG:
        case T_DOSSEG:    ModuleInfo.segorder = SEGORDER_DOSSEG;  break;
        case T_DOT_ALPHA: ModuleInfo.segorder = SEGORDER_ALPHA;   break;
        default:          ModuleInfo.segorder = SEGORDER_SEQ;     break;
        }
#endif
    }
    return( NOT_ERROR );
}
