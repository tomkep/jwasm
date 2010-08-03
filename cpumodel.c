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
* Description:  processes .MODEL and cpu directives
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
#include "input.h"
#include "tokenize.h"
#include "expreval.h"
#include "fastpass.h"
#include "listing.h"

#include "myassert.h"

#define ONEXMM 1

/* prototypes */

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
/* not needed. see GetLangType() */
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

static asm_sym *sym_CodeSize  ; /* numeric. requires model */
static asm_sym *sym_DataSize  ; /* numeric. requires model */
static asm_sym *sym_Model     ; /* numeric. requires model */
       asm_sym *sym_Interface ; /* numeric. requires model */
       asm_sym *sym_Cpu       ; /* numeric. This is ALWAYS set */


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

/* set memory model, called by ModelDirective()
 */
static void SetModel( void )
/**************************/
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
    ModelSimSegmInit( ModuleInfo.model ); /* create segments in first pass */
    ModelAssumeInit();

    if ( ModuleInfo.list )
        LstWriteSrcLine();

    RunLineQueue();

    if ( Parse_Pass != PASS_1 )
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

/* set default wordsize for segment definitions */

static ret_code SetDefaultOfssize( int size )
/*******************************************/
{
    /* outside any segments? */
    if( CurrSeg == NULL ) {
        ModuleInfo.defOfssize = size;
    }
    return( SetOfssize() );
}

/* handle .model directive
 * syntax: .MODEL <FLAT|TINY|SMALL...> [,<C|PASCAL|STDCALL...>][,<NEARSTACK|FARSTACK>][,<OS_DOS|OS_OS2>]
 * sets fields
 * - ModuleInfo.model
 * - ModuleInfo.language
 * - ModuleInfo.distance
 * - ModuleInfo.ostype
 * if model is FLAT, defines FLAT pseudo-group
 * set default segment names for code and data
 */
ret_code ModelDirective( int i )
/******************************/
{
    const typeinfo *type;           /* type of option */
    mod_type model;
    lang_type language;
    dist_type distance;
    os_type ostype;
    uint_16 init;

    DebugMsg(("ModelDirective enter\n"));
    /* v2.03: it may occur that "code" is defined BEFORE the MODEL
     * directive (i.e. DB directives in AT-segments). For FASTPASS,
     * this may have caused errors because contents of the ModuleInfo
     * structure was saved before the .MODEL directive.
     */
    //if( Parse_Pass != PASS_1 ) {
    if( Parse_Pass != PASS_1 && ModuleInfo.model != MOD_NONE ) {
        /* just set the model with SetModel() if pass is != 1.
         * This won't set the language ( which can be modified by
         * OPTION LANGUAGE directive ), but the language in ModuleInfo
         * isn't needed anymore once pass one is done.
         */
        SetModel();
        return( NOT_ERROR );
    }

    i++;
    if ( AsmBuffer[i]->token == T_FINAL ) {
        AsmError( EXPECTED_MEMORY_MODEL );
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
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
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
    if ( init & INIT_LANG ) {
        ModuleInfo.langtype = language;
#if AMD64_SUPPORT
        /* v2.03: set the fastcall type if x64 is active */
    if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_64 )
        if ( language == LANG_FASTCALL && Options.output_format != OFORMAT_ELF ) {
            DebugMsg(("ModelDirective: FASTCALL type set to WIN64\n"));
            Options.fastcall = FCT_WIN64;
        }
#endif
    }
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

ret_code cpu_directive( int i )
/*****************************/
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

