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
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "extern.h"
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
extern ret_code      process_address( struct code_info *, expr_list * );

struct code_line {
    const char *src;
    const short r1;
    const short r2;
};

/* startup code for 8086 */

static const struct code_line StartupDosNear0[] = {
    { "mov %r,DGROUP", T_DX, T_NULL },
    { "mov %r,%r", T_DS, T_DX       },
    { "mov %r,%r", T_BX, T_SS       },
    { "sub %r,%r", T_BX, T_DX       },
    { "shl %r,1",  T_BX, T_NULL     },
    { "shl %r,1",  T_BX, T_NULL     },
    { "shl %r,1",  T_BX, T_NULL     },
    { "shl %r,1",  T_BX, T_NULL     },
    { "cli",       T_NULL, T_NULL   },
    { "mov %r,%r", T_SS, T_DX       },
    { "add %r,%r", T_SP, T_BX       },
    { "sti",       T_NULL, T_NULL   },
};

/* startup code for 80186+ */

static const struct code_line StartupDosNear1[] = {
    { "mov %r,DGROUP", T_AX, T_NULL },
    { "mov %r,%r",     T_DS, T_AX   },
    { "mov %r,%r",     T_BX, T_SS   },
    { "sub %r,%r",     T_BX, T_AX   },
    { "shl %r,4",      T_BX, T_NULL },
    { "mov %r,%r",     T_SS, T_AX   },
    { "add %r,%r",     T_SP, T_BX   },
};

static const struct code_line StartupDosFar[] = {
    { "mov %r,DGROUP", T_DX, T_NULL },
    { "mov %r,%r"    , T_DS, T_DX   },
};

static const struct code_line ExitOS2[] = { /* mov al, retval  followed by: */
    { "mov %r,0",     T_AH,  T_NULL  },
    { "push 1",       T_NULL,T_NULL  },
    { "push %r",      T_AX,  T_NULL  },
    { "call DOSEXIT", T_NULL,T_NULL  },
};

static const struct code_line ExitDos[] = {
    { "mov %r,4ch",  T_AH,  T_NULL  },
    { "int 21h",     T_NULL,T_NULL  },
};

static const char StartAddr[] = {"@Startup"};

/* .STARTUP and .EXIT directives */

ret_code StartupExitDirective( int i )
/************************************/
{
    int         count;
    int         j;
    const struct code_line *p;
    expr_list   opndx;

#if FASTPASS
    /* make sure the directive is stored */
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
        SaveState();
    }
#endif
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
        if ( ModuleInfo.model == MOD_TINY )
            AddLineQueue( "org 100h" );
        AddLineQueueX( "%s::", StartAddr );
        if( ModuleInfo.ostype == OPSYS_DOS ) {
            if (ModuleInfo.model == MOD_TINY)
                ;
            else {
                if( ModuleInfo.distance == STACK_NEAR ) {
                    if ( (ModuleInfo.cpu & 0x7F) <= 1) {
                        p = StartupDosNear0;
                        count = sizeof(StartupDosNear0) / sizeof(StartupDosNear0[0]);
                    } else {
                        p = StartupDosNear1;
                        count = sizeof(StartupDosNear1) / sizeof(StartupDosNear1[0]);
                    }
                } else {
                    p = StartupDosFar;
                    count = sizeof(StartupDosFar) / sizeof(StartupDosFar[0]);
                }
                for ( ; count ; count--, p++ )
                    AddLineQueueX( (char *)p->src, p->r1, p->r2 );
            }
        }
        ModuleInfo.StartupDirectiveFound = TRUE;
        break;
    case T_DOT_EXIT:
        if( ModuleInfo.ostype == OPSYS_DOS ) {
            p = ExitDos;
            count = sizeof( ExitDos ) / sizeof( ExitDos[0] );
        } else {
            p = ExitOS2;
            count = sizeof( ExitOS2 ) / sizeof( ExitOS2[0] );
        }
        j = i;
        i++;
        if ( AsmBuffer[i]->token != T_FINAL ) {
            if( ModuleInfo.ostype == OPSYS_OS2 ) {
                AddLineQueueX( "mov %r,%s", T_AX, AsmBuffer[j]->pos + 5 );
            } else {
                if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                    return( ERROR );
                if ( opndx.kind == EXPR_CONST && opndx.value < 0x100 && AsmBuffer[i]->token == T_FINAL ) {
                    AddLineQueueX( "mov %r,4C00h + %u", T_AX, opndx.value );
                } else {
                    AddLineQueueX( "mov %r,%s", T_AL, AsmBuffer[j]->pos + 5 );
                    AddLineQueueX( "mov %r,4Ch", T_AH );
                }
            }
            p++;
            count--;
        }
        for( ; count ; count--, p++ ) {
            AddLineQueueX( (char *)p->src, p->r1, p->r2 );
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
    struct fixup        *fix;
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
        AsmBuffer[i]->string_ptr = (char *)StartAddr;
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

    if( opndx.kind == EXPR_EMPTY ) {
        ;
    } else if ( opndx.sym && ( opndx.sym->state == SYM_UNDEFINED ) ) {
        AsmErr( SYMBOL_NOT_DEFINED, opndx.sym->name );
        return( ERROR );
    } else {
        char error = TRUE;
        if ( opndx.kind == EXPR_ADDR && opndx.indirect == FALSE ) {
            CodeInfo->pcurr = &InstrTable[IndexFromToken( CodeInfo->token )];
            process_address( CodeInfo, &opndx );
            fix = CodeInfo->InsFixup[0];
            if ( fix )
                sym = fix->sym;
            if ( fix == NULL || sym == NULL ) {
                DebugMsg(("EndDirective: start address invalid, fix=%p, sym=%p\n", fix, sym ));
            } else if ( sym->state == SYM_INTERNAL || sym->state == SYM_EXTERNAL ) {
                if ( opndx.mem_type == MT_NEAR || opndx.mem_type == MT_FAR || opndx.mem_type == MT_PROC )
                    error = FALSE;
                else {
                    DebugMsg(("EndDirective: start address not a code label, mem_type=%Xh\n", opndx.mem_type ));
                }
            } else {
                DebugMsg(("EndDirective: start address invalid, sym->state=%X\n", sym->state ));
            }
        } else {
            DebugMsg(("EndDirective: start address invalid, opndx.kind=%X\n", opndx.kind ));
        }
        if ( error ) {
            AsmError( CONSTANT_OR_RELOCATABLE_LABEL_EXPECTED );
            return( ERROR );
        }
    }

    if ( Options.output_format == OFORMAT_OMF ) {
        ModuleInfo.start_fixup = CodeInfo->InsFixup[0];
        ModuleInfo.start_displ = opndx.value;
    } else {
        /* Masm silently ignores start for -coff if an offset was given */
        //if ( opndx.kind == EXPR_EMPTY || opndx.value )
        if ( opndx.kind == EXPR_EMPTY )
            return( NOT_ERROR );

        if ( sym->state != SYM_EXTERNAL && sym->public == FALSE ) {
            sym->public = TRUE;
            AddPublicData( sym );
        }
        DebugMsg(("EndDirective: start label=%p\n", sym ));
        ModuleInfo.start_label = sym;
    }
    return( NOT_ERROR );
}

