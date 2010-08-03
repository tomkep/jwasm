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
* Description:  directives END, .STARTUP and .EXIT
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"
#include "segment.h"
#include "queues.h"
#include "fixup.h"
#include "input.h"
#include "tokenize.h"
#include "expreval.h"
#include "types.h"
#include "fastpass.h"
#include "listing.h"
#include "omf.h"

#include "myassert.h"

/* prototypes */
extern ret_code         process_address( struct code_info *, expr_list * );

extern asm_sym          *start_label;   /* start address (COFF, ELF, BIN) */

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
        "cli",
        "mov\tss,dx",
        "add\tsp,bx",
        "sti"
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

/* .STARTUP and .EXIT directives */

ret_code StartupExitDirective( int i )
/************************************/
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

/* END directive */

ret_code EndDirective( int i, struct code_info *CodeInfo )
/********************************************************/
{
    expr_list           opndx;
    struct genfixup     *fix;
    asm_sym             *sym;

    DebugMsg(("EndDirective enter\n"));
#if FASTPASS
    /* if there's no code or data emitted, init the line store now */
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
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

    i++; /* skip END directive */

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

    ModuleInfo.EndDirFound = TRUE;

    if( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
        return( ERROR );
    }
    if( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if( opndx.kind == EXPR_EMPTY )
        ;
    else if ( opndx.kind == EXPR_ADDR && opndx.indirect == FALSE ) {
        process_address( CodeInfo, &opndx );
        fix = CodeInfo->InsFixup[0];
        if ( fix )
            sym = fix->sym;
        if ( fix == NULL || sym == NULL ) {
            DebugMsg(("EndDirective: start address invalid, fix=%X, sym=%X\n", fix, sym ));
            AsmError( INVALID_START_ADDRESS );
            return( ERROR );
        } else if ( sym->state == SYM_INTERNAL || sym->state == SYM_EXTERNAL ) {
            if ( sym->mem_type == MT_NEAR || sym->mem_type == MT_FAR || sym->mem_type == MT_PROC )
                ;
            else {
                DebugMsg(("EndDirective: start address not a code label, mem_type=%Xh\n", sym->mem_type ));
                AsmError( MUST_BE_ASSOCIATED_WITH_CODE );
                return( ERROR );
            }
        } else {
            DebugMsg(("EndDirective: start address invalid, sym->state=%X\n", sym->state ));
            if ( sym->state == SYM_UNDEFINED )
                AsmErr( SYMBOL_NOT_DEFINED, sym->name );
            else
                AsmError( INVALID_START_ADDRESS );
            return( ERROR );
        }
    } else {
        DebugMsg(("EndDirective: start address invalid, opndx.kind=%X\n", opndx.kind ));
        AsmError( INVALID_START_ADDRESS );
        return( ERROR );
    }

    if ( Options.output_format == OFORMAT_OMF ) {
        if ( write_to_file )
            omf_create_modend( CodeInfo->InsFixup[0], opndx.value );
    } else {
        /* Masm silently ignores start for -coff if an offset was given */
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

