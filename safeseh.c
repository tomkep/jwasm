/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  processes directive .SAFESEH
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"

#include "myassert.h"

#if COFF_SUPPORT

/* .SAFESEH works for coff format only.
 * syntax is: .SAFESEH handler
 * <handler> must be a PROC or PROTO
 */

ret_code SafeSEHDirective( int i )
/********************************/
{
    struct asm_sym *sym;
    qnode   *node;

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

    /* make sure the argument is a true PROC */
    if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
        if ( Parse_Pass != PASS_1 ) {
            AsmErr( SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
    } else if ( sym->isproc == FALSE ) {
        AsmErr( SAFESEH_ARGUMENT_MUST_BE_A_PROC, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if ( Parse_Pass == PASS_1 ) {
        if ( sym ) {
            for ( node = ModuleInfo.g.SafeSEHList.head; node; node = node->next )
                if ( node->elmt == sym )
                    break;
        } else {
            sym = SymCreate( AsmBuffer[i]->string_ptr, TRUE );
            node = NULL;
        }
        if ( node == NULL ) {
            sym->used = TRUE; /* make sure an external reference will become strong */
            node = AsmAlloc( sizeof( qnode ) );
            node->elmt = sym;
            node->next = NULL;
            if ( ModuleInfo.g.SafeSEHList.head == 0 )
                ModuleInfo.g.SafeSEHList.head = ModuleInfo.g.SafeSEHList.tail = node;
            else {
                ((qnode *)ModuleInfo.g.SafeSEHList.tail)->next = node;
                ModuleInfo.g.SafeSEHList.tail = node;
            }
        }
    }
    i++;
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    return( NOT_ERROR );
}
#endif

