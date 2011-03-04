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
* Description:  Processing of PROC/ENDP/LOCAL directives.
*               rewritten for JWasm.
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
#include "equate.h"
#include "fixup.h"
#include "labels.h"
#include "input.h"
#include "tokenize.h"
#include "expreval.h"
#include "types.h"
#include "condasm.h"
#include "macro.h"
#include "proc.h"
#include "fastpass.h"
#include "listing.h"
#include "posndir.h"
#include "myassert.h"
#if AMD64_SUPPORT
#include "insthash.h"
#include "win64seh.h"
#endif

#ifdef __I86__
#define NUMQUAL (long)
#else
#define NUMQUAL
#endif

extern const char szDgroup[];

/*
 * Masm allows nested procedures
 * but they must NOT have params or locals
 */

/*
 * calling convention FASTCALL supports:
 * - Watcom C: registers e/ax,e/dx,e/bx,e/cx
 * - MS fastcall: registers e/cx,e/dx  (default for 32bit)
 * - Win64: registers rcx, rdx, r8, r9 (default for 64bit)
 */

dir_node                *CurrProc;      /* current procedure */
int                     procidx;        /* procedure index */

static proc_info        *ProcStack;

bool                    in_epilogue;
bool                    DefineProc;     /* TRUE if definition of procedure
                                         * hasn't ended yet */
#if AMD64_SUPPORT
static bool             endprolog_found;
static uint_8           unw_segs_defined;
static UNWIND_INFO      unw_info = {UNW_VERSION, 0, 0, 0, 0, 0 };
static UNWIND_CODE      unw_code[128];
#endif

/* tables for FASTCALL support */

static const enum asm_token ms32_regs16[] = { T_CX, T_DX };
static const enum asm_token ms32_regs32[] = { T_ECX,T_EDX };
#if OWFC_SUPPORT
static const enum asm_token watc_regs8[] = {T_AL, T_DL, T_BL, T_CL };
static const enum asm_token watc_regs16[] = {T_AX, T_DX, T_BX, T_CX };
static const enum asm_token watc_regs32[] = {T_EAX, T_EDX, T_EBX, T_ECX };
static const enum asm_token watc_regs_qw[] = {T_AX, T_BX, T_CX, T_DX };
#endif
#if AMD64_SUPPORT
static const enum asm_token ms64_regs[] = {T_RCX, T_RDX, T_R8, T_R9 };
/* win64 non-volatile GPRs:
 * T_RBX, T_RBP, T_RSI, T_RDI, T_R12, T_R13, T_R14, T_R15
 */
static const uint_16 win64_nvgpr = 0xF0E8;
/* win64 non-volatile XMM regs: XMM6-XMM15 */
static const uint_16 win64_nvxmm = 0xFFC0;
#endif

struct fastcall_conv {
    int (* paramcheck)( dir_node *, dir_node *, int * );
    void (* handlereturn)( dir_node *, char *buffer );
};

static  int ms32_pcheck( dir_node *, dir_node *, int * );
static void ms32_return( dir_node *, char * );
#if OWFC_SUPPORT
static  int watc_pcheck( dir_node *, dir_node *, int * );
static void watc_return( dir_node *, char * );
#endif
#if AMD64_SUPPORT
static  int ms64_pcheck( dir_node *, dir_node *, int * );
static void ms64_return( dir_node *, char * );
#endif

/* table of fastcall types.
 * must match order of enum fastcall_type!
 * also see table in mangle.c!
 */

static const struct fastcall_conv fastcall_tab[] = {
    { ms32_pcheck, ms32_return },  /* FCT_MS32 */
#if OWFC_SUPPORT
    { watc_pcheck, watc_return },  /* FCT_WATCOMC */
#endif
#if AMD64_SUPPORT
    { ms64_pcheck, ms64_return }   /* FCT_WIN64 */
#endif
};

static const enum asm_token basereg[] = { T_BP, T_EBP,
#if AMD64_SUPPORT
T_RBP
#endif
};
static const enum asm_token stackreg[] = { T_SP, T_ESP,
#if AMD64_SUPPORT
T_RSP
#endif
};

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

#if OWFC_SUPPORT
/* register usage for OW fastcall (register calling convention).
 * registers are used for parameter size 1,2,4,8.
 * if a parameter doesn't fit in a register, a register pair is used.
 * however, valid register pairs are e/dx:e/ax and e/cx:e/bx only!
 * if a parameter doesn't fit in a register pair, registers
 * are used ax:bx:cx:dx!!!
 * stack cleanup for OW fastcall: if the proc is VARARG, the caller
 * will do the cleanup, else the called proc does it.
 * in VARARG procs, all parameters are pushed onto the stack!
 */

static int watc_pcheck( dir_node *proc, dir_node *paranode, int *used )
/*********************************************************************/
{
    static char regname[64];
    static char regist[32];
    int newflg;
    int shift;
    int firstreg;
    uint_8 Ofssize = GetSymOfssize( &proc->sym );
    int size = SizeFromMemtype( paranode->sym.mem_type, paranode->sym.Ofssize, paranode->sym.type );

    /* v2.05: VARARG procs don't have register params */
    if ( proc->e.procinfo->is_vararg )
        return( 0 );

    if ( size != 1 && size != 2 && size != 4 && size != 8 )
        return( 0 );

    /* v2.05: rewritten. The old code didn't allow to "fill holes" */
    if ( size == 8 ) {
        newflg = Ofssize ? 3 : 15;
        shift = Ofssize ? 2 : 4;
    } else if ( size == 4 && Ofssize == USE16 ) {
        newflg = 3;
        shift = 2;
    } else {
        newflg = 1;
        shift = 1;
    }

    /* scan if there's a free register (pair/quadrupel) */
    for ( firstreg = 0; firstreg < 4 && (newflg & *used ); newflg <<= shift, firstreg += shift );
    if ( firstreg >= 4 ) /* exit if nothing is free */
        return( 0 );

    paranode->sym.state = SYM_TMACRO;
    switch ( size ) {
    case 1:
        GetResWName( watc_regs8[firstreg], regname );
        break;
    case 2:
        GetResWName( watc_regs16[firstreg], regname );
        break;
    case 4:
        if ( Ofssize ) {
            GetResWName( watc_regs32[firstreg], regname );
        } else {
            sprintf( regname, "%s::%s",
                    GetResWName( watc_regs16[firstreg+1], regist ),
                    GetResWName( watc_regs16[firstreg], NULL ) );
        }
        break;
    case 8:
        if ( Ofssize ) {
            sprintf( regname, "%s::%s",
                    GetResWName( watc_regs32[firstreg+1], regist ),
                    GetResWName( watc_regs32[firstreg], NULL ) );
        } else {
            /* the AX:BX:CX:DX sequence is for 16-bit only */
            for( firstreg = 0, regname[0] = NULLC; firstreg < 4; firstreg++ ) {
                GetResWName( watc_regs_qw[firstreg], regname + strlen( regname ) );
                if ( firstreg != 3 )
                    strcat( regname, "::");
            }
        }
    }
    *used |= newflg;
    paranode->sym.string_ptr = AsmAlloc( strlen( regname ) + 1 );
    strcpy( paranode->sym.string_ptr, regname );
    DebugMsg(("watc_pcheck(%s.%s): size=%u ptr=%u far=%u reg=%s\n", proc->sym.name, paranode->sym.name, size, paranode->sym.is_ptr, paranode->sym.isfar, regname ));
    return( 1 );
}

static void watc_return( dir_node *proc, char *buffer )
/*****************************************************/
{
    int value;
    value = 4 * CurrWordSize;
    if( proc->e.procinfo->is_vararg == FALSE && proc->e.procinfo->parasize > value )
        sprintf( buffer + strlen( buffer ), " %d", proc->e.procinfo->parasize - value );
    return;
}
#endif

/* the MS Win32 fastcall ABI is simple: register ecx and edx are used,
 * if the parameter's value fits into the register.
 * there is no space reserved on the stack for a register backup.
 */

static int ms32_pcheck( dir_node *proc, dir_node *paranode, int *used )
/*********************************************************************/
{
    char regname[32];
    int size = SizeFromMemtype( paranode->sym.mem_type, paranode->sym.Ofssize, paranode->sym.type );

    if ( size > CurrWordSize || *used >= 2 )
        return( 0 );
    paranode->sym.state = SYM_TMACRO;
    GetResWName( ModuleInfo.Ofssize ? ms32_regs32[*used] : ms32_regs16[*used], regname );
    paranode->sym.string_ptr = AsmAlloc( strlen( regname ) + 1 );
    strcpy( paranode->sym.string_ptr, regname );
    (*used)++;
    return( 1 );
}

static void ms32_return( dir_node *proc, char *buffer )
/*****************************************************/
{
    if( proc->e.procinfo->parasize > ( 2 * CurrWordSize ) )
        sprintf( buffer + strlen( buffer ), " %d", proc->e.procinfo->parasize - (2 * CurrWordSize) );
    return;
}

#if AMD64_SUPPORT

/* the MS Win64 fastcall ABI is strict: the first four parameters are
 * passed in registers. If a parameter's value doesn't fit in a register,
 * it's address is used instead. parameter 1 is stored in rcx/xmm0,
 * then comes rdx/xmm1, r8/xmm2, r9/xmm3. The xmm regs are used if the
 * param is a float/double (but not long double!).
 * Additionally, there's space for the registers reserved by the caller on,
 * the stack. On a function's entry it's located at [esp+8] for param 1, 
 * [esp+16] for param 2,... The parameter names refer to those stack
 * locations, not to the register names.
 */

static int ms64_pcheck( dir_node *proc, dir_node *paranode, int *used )
/*********************************************************************/
{
    /* since the parameter names refer the stack-backup locations,
     * there's nothing to do here!
     * That is, if a parameter's size is > 8, it has to be changed
     * to a pointer. This is to be done yet.
     */
    return( 0 );
}

static void ms64_return( dir_node *proc, char *buffer )
/*****************************************************/
{
    /* nothing to do, the caller cleans the stack */
    return;
}
#endif

static void push_proc( dir_node *proc )
/*************************************/
{
    if ( Parse_Pass == PASS_1 ) /* get the locals stored so far */
        SymGetLocal( (asm_sym *)proc );
    pushitem( &ProcStack, proc );
    return;
}

static dir_node *pop_proc( void )
/*******************************/
{
    if( ProcStack == NULL )
        return( NULL );
    return( (dir_node *)popitem( &ProcStack ) );
}

/* LOCAL directive. Called on Pass 1 only */

ret_code LocalDef( int i )
/************************/
{
    char        *name;
    dir_node    *local;
    dir_node    *curr;
    proc_info   *info;
    //int         size;
    //int         idx;
#if AMD64_SUPPORT
    int         displ;
#endif
    struct qualified_type ti;
    int         align = CurrWordSize;

/*

    LOCAL symbol[,symbol]...
    symbol:name [[count]] [:[type]]
    count: number of array elements, default is 1
    type:  Simple Type, structured type, ptr to simple/structured type

 */
    if ( Parse_Pass != PASS_1 )
        return( NOT_ERROR );

    DebugMsg1(("LocalDef(%u) entry\n", i));

    if( DefineProc == FALSE || CurrProc == NULL ) {
        AsmError( PROC_MACRO_MUST_PRECEDE_LOCAL );
        return( ERROR );
    }

    info = CurrProc->e.procinfo;

    i++; /* go past LOCAL */
#if AMD64_SUPPORT
    /* adjust start displacement for Win64 FRAME procs */
    if ( info->isframe )
        if ( info->regslist )
            displ = *info->regslist * sizeof(uint_64);
        else
            displ = 0;
#endif

    do  {
        if( AsmBuffer[i]->token != T_ID ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        name = AsmBuffer[i]->string_ptr;

        DebugMsg1(("LocalDef(%s)\n", name ));

        ti.symtype = NULL;
        ti.is_ptr = 0;
        ti.ptr_memtype = MT_EMPTY;
        if ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) )
            ti.is_far = TRUE;
        else
            ti.is_far = FALSE;
        ti.Ofssize = ModuleInfo.Ofssize;

#if 0
        /* since v1.95 a local hash table is used. No need to search the
         * symbol before SymLCreate() is called. SymLCreate() will display
         * an error if the symbol is already defined.
         */
        if ((local = (dir_node *)SymSearch( name )) && local->sym.state != SYM_UNDEFINED ) {
            AsmErr( SYMBOL_ALREADY_DEFINED, name );
            return( ERROR );
        }
#endif
        local = (dir_node *)SymLCreate( name );
        if( !local ) { /* if it failed, an error msg has been written already */
            DebugMsg(("LocalDef: SymLCreate( %s ) failed\n", name ));
            return( ERROR );
        }

        local->sym.state = SYM_STACK;
        local->sym.isdefined = TRUE;
        local->sym.total_length = 1; /* v2.04: added */
        switch ( ti.Ofssize ) {
#if AMD64_SUPPORT
        case USE64: local->sym.mem_type = MT_QWORD; break;
#endif
        case USE32: local->sym.mem_type = MT_DWORD; break;
        default: local->sym.mem_type = MT_WORD;
        }
        ti.size = align;

        i++; /* go past name */

        /* get an optional index factor: local name[xx]:... */
        if( AsmBuffer[i]->token == T_OP_SQ_BRACKET ) {
            int j;
            expr_list opndx;
            i++; /* go past '[' */
            /* scan for comma or colon. this isn't really necessary,
             * but will prevent the expression evaluator from emitting
             * confusing error messages.
             */
            for ( j = i; j < Token_Count; j++ )
                if (AsmBuffer[j]->token == T_COMMA ||
                    AsmBuffer[j]->token == T_COLON)
                    break;
            if ( ERROR == EvalOperand( &i, j, &opndx, 0 ) )
                return( ERROR );
            if ( opndx.kind != EXPR_CONST ) {
                AsmError( CONSTANT_EXPECTED );
                opndx.value = 1;
            }
            // local->factor = AsmBuffer[i++]->value;
            /* zero is allowed as value! */
            local->sym.total_length = opndx.value;
            local->sym.isarray = TRUE;
            if( AsmBuffer[i]->token == T_CL_SQ_BRACKET ) {
                i++; /* go past ']' */
            } else {
                AsmError( EXPECTED_CL_SQ_BRACKET );
            }
        }

        /* get the optional type: local name[xx]:type  */
        if( AsmBuffer[i]->token == T_COLON ) {
            DebugMsg1(("LocalDef(%s): i=%u, token=%X\n", name, i, AsmBuffer[i]->token ));
            i++;

            if ( GetQualifiedType( &i, &ti ) == ERROR )
                return( ERROR );

            local->sym.mem_type = ti.mem_type;
            if ( ti.mem_type == MT_TYPE ) {
                local->sym.type = ti.symtype;
            } else {
                local->sym.target_type = ti.symtype;
            }
            DebugMsg1(("LocalDef: memtype=%X, type=%s, size=%u (curr localsize=%X)\n",
                       local->sym.mem_type,
                       ti.symtype ? ti.symtype->name : "NULL",
                       ti.size, info->localsize ));
        }
        local->sym.is_ptr  = ti.is_ptr;
        local->sym.isfar   = ti.is_far;
        local->sym.Ofssize = ti.Ofssize;
        local->sym.ptr_memtype = ti.ptr_memtype;
        local->sym.total_size = ti.size * local->sym.total_length;

        info->localsize += local->sym.total_size;

        if ( ti.size > align )
            info->localsize = ROUND_UP( info->localsize, align );
        else if ( ti.size ) /* v2.04: skip if size == 0 */
            info->localsize = ROUND_UP( info->localsize, ti.size );
        DebugMsg1(("LocalDef(%s): aligned local total=%X\n", name, info->localsize));

#if AMD64_SUPPORT
        if ( info->isframe )
            local->sym.offset = - ( info->localsize + displ );
        else
#endif
        local->sym.offset = - info->localsize;
        DebugMsg1(("LocalDef(%s): symbol offset=%d\n", name, local->sym.offset));

        if( info->locallist == NULL ) {
            info->locallist = local;
        } else {
            for( curr = info->locallist; curr->nextlocal ; curr = curr->nextlocal );
            curr->nextlocal = local;
        }

        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA ) {
                if ( (i + 1) < Token_Count )
                    i++;
            } else {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }

    } while ( i < Token_Count );

    return( NOT_ERROR );
}

/* parse parameters of a PROC/PROTO
 * i=token buffer index
 */

static ret_code ParseParams( dir_node *proc, int i, bool IsPROC )
/***************************************************************/
{
    char            *name;
    struct asm_sym  *sym;
    int             cntParam;
    int             offset;
    //int             size;
    int             fcint = 0;
    struct qualified_type ti;
    bool            is_vararg;
    dir_node        *paranode;
    dir_node        *paracurr;

    /* parse PROC parms */
    /* it's important to remember that params are stored in "push" order! */

    if (proc->sym.langtype == LANG_C ||
        proc->sym.langtype == LANG_SYSCALL ||
#if AMD64_SUPPORT
        ( proc->sym.langtype == LANG_FASTCALL && ModuleInfo.Ofssize != USE64 ) ||
#else
        proc->sym.langtype == LANG_FASTCALL ||
#endif
        proc->sym.langtype == LANG_STDCALL)
        for (paracurr = proc->e.procinfo->paralist; paracurr && paracurr->nextparam; paracurr = paracurr->nextparam );
    else
        paracurr = proc->e.procinfo->paralist;

    for( cntParam = 0 ; AsmBuffer[i]->token != T_FINAL ; cntParam++ ) {

        if ( AsmBuffer[i]->token == T_ID ) {
            name = AsmBuffer[i++]->string_ptr;
        } else if ( IsPROC == FALSE && AsmBuffer[i]->token == T_COLON ) {
            if ( paracurr )
                name = paracurr->sym.name;
            else
                name = "";
        } else {
            /* PROC needs a parameter name, PROTO accepts <void> also */
            DebugMsg(("ParseParams: name missing/invalid for parameter %u, i=%u\n", cntParam+1, i));
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }

        ti.symtype = NULL;
        ti.is_ptr = 0;
        ti.ptr_memtype = MT_EMPTY;
        /* v2.02: init is_far depending on memory model */
        //ti.is_far = FALSE;
        if ( SIZE_DATAPTR & ( 1 << ModuleInfo.model ) )
            ti.is_far = TRUE;
        else
            ti.is_far = FALSE;
        ti.Ofssize = ModuleInfo.Ofssize;
        ti.size = CurrWordSize;

        is_vararg = FALSE;

        /* read colon. It's optional for PROC.
         * Masm also allows a missing colon for PROTO - if there's
         * just one parameter. Probably a Masm bug.
         * JWasm always require a colon for PROTO.
         */
        if( AsmBuffer[i]->token != T_COLON ) {
            if ( IsPROC == FALSE ) {
                AsmError( COLON_EXPECTED );
                return( ERROR );
            }
            switch ( ti.Ofssize ) {
#if AMD64_SUPPORT
            case USE64: ti.mem_type = MT_QWORD; break;
#endif
            case USE32: ti.mem_type = MT_DWORD; break;
            default: ti.mem_type = MT_WORD;
            }
        } else {
            i++;
            if (( AsmBuffer[i]->token == T_RES_ID ) && ( AsmBuffer[i]->value == T_VARARG )) {
                switch( proc->sym.langtype ) {
                case LANG_NONE:
                case LANG_BASIC:
                case LANG_FORTRAN:
                case LANG_PASCAL:
                case LANG_STDCALL:
                    AsmError( VARARG_REQUIRES_C_CALLING_CONVENTION );
                    return( ERROR );
                }
                /* v2.05: added check */
                if ( AsmBuffer[i+1]->token != T_FINAL )
                    AsmError( VARARG_PARAMETER_MUST_BE_LAST );
                else
                    is_vararg = TRUE;
                ti.mem_type = MT_EMPTY;
                ti.size = 0;
                i++;
            } else {
                if ( GetQualifiedType( &i, &ti ) == ERROR )
                    return( ERROR );
            }
        }

        /* check if parameter name is defined already */
        if (( IsPROC ) && ( sym = SymSearch( name ) ) && sym->state != SYM_UNDEFINED ) {
            DebugMsg(("ParseParams: %s defined already, state=%u, local=%u\n", sym->name, sym->state, sym->scoped ));
            AsmErr( SYMBOL_REDEFINITION, name );
            return( ERROR );
        }

        /* redefinition? */
        if ( paracurr ) {
#if 0 /* was active till v2.04 */
            int newsize = ti.size;
            int oldsize;

            /* check size only (so UINT <-> DWORD wont cause an error) */
            if ( paracurr->sym.type )
                oldsize = paracurr->sym.total_size;
            else if ( paracurr->sym.mem_type == MT_EMPTY )
                oldsize = 0;
            else if ( paracurr->sym.mem_type == MT_PTR )
                oldsize = SizeFromMemtype( paracurr->sym.isfar ? MT_FAR : MT_NEAR, paracurr->sym.Ofssize, NULL );
            else
                oldsize = SizeFromMemtype( paracurr->sym.mem_type, paracurr->sym.Ofssize, paracurr->sym.type );
            if ( oldsize != newsize ) {
                DebugMsg(("ParseParams: old memtype=%u, new memtype=%u\n", paracurr->sym.mem_type, ti.mem_type));
                AsmErr( CONFLICTING_PARAMETER_DEFINITION, name );
                //return( ERROR );
            }
            /* the parameter type used in PROC has highest priority! */
            if ( IsPROC ) {
                if ( ti.symtype ) {
                    paracurr->sym.type = ti.symtype;
                    paracurr->sym.mem_type = MT_TYPE;
                } else
                    paracurr->sym.mem_type = ti.mem_type;
            }
#else
            asm_sym *to;
            asm_sym *tn;
            char oo;
            char on;
            for( tn = ti.symtype; tn && tn->type; tn = tn->type );
            to = ( paracurr->sym.mem_type == MT_TYPE ) ? paracurr->sym.type : paracurr->sym.target_type;
            for( ; to && to->type; to = to->type );
            oo = ( paracurr->sym.Ofssize != USE_EMPTY ) ? paracurr->sym.Ofssize : ModuleInfo.Ofssize;
            on = ( ti.Ofssize != USE_EMPTY ) ? ti.Ofssize : ModuleInfo.Ofssize;
            if ( ti.mem_type != paracurr->sym.mem_type ||
                ( ti.mem_type == MT_TYPE && tn != to ) ||
                ( ti.mem_type == MT_PTR &&
                 ( ti.is_far != paracurr->sym.isfar ||
                  on != oo ||
                  ti.ptr_memtype != paracurr->sym.ptr_memtype ||
                  tn != to ))) {
                DebugMsg(("ParseParams: old-new memtype=%X-%X type=%X(%s)-%X(%s) far=%u-%u ind=%u-%u ofss=%d-%d pmt=%X-%X\n",
                          paracurr->sym.mem_type, ti.mem_type, 
                          (paracurr->sym.mem_type == MT_TYPE) ? paracurr->sym.type : paracurr->sym.target_type,
                          (paracurr->sym.mem_type == MT_TYPE) ? paracurr->sym.type->name : paracurr->sym.target_type ? paracurr->sym.target_type->name : "",
                          ti.symtype, ti.symtype ? ti.symtype->name : "",
                          paracurr->sym.isfar, ti.is_far,
                          paracurr->sym.is_ptr, ti.is_ptr,
                          paracurr->sym.Ofssize, ti.Ofssize,
                          paracurr->sym.ptr_memtype, ti.ptr_memtype ));
                AsmErr( CONFLICTING_PARAMETER_DEFINITION, name );
                //return( ERROR );
            }
#endif
            if ( IsPROC ) {
                DebugMsg(("ParseParams: calling SymSetName(%s, %s)\n", paracurr->sym.name, name ));
                SymSetName( &paracurr->sym, name );
            }
            /* set paracurr to next parameter */
            if ( proc->sym.langtype == LANG_C ||
                proc->sym.langtype == LANG_SYSCALL ||
#if AMD64_SUPPORT
                ( proc->sym.langtype == LANG_FASTCALL && ti.Ofssize != USE64 ) ||
#else
                proc->sym.langtype == LANG_FASTCALL ||
#endif
                proc->sym.langtype == LANG_STDCALL) {
                dir_node *l;
                for (l = proc->e.procinfo->paralist;
                     l && ( l->nextparam != paracurr );
                     l = l->nextparam );
                paracurr = l;
            } else
                paracurr = paracurr->nextparam;

        } else if ( proc->e.procinfo->init == TRUE ) {
            /* second definition has more parameters than first */
            DebugMsg(("ParseParams: different param count\n"));
            AsmErr( CONFLICTING_PARAMETER_DEFINITION, "" );
            return( ERROR );
        } else {
            if ( IsPROC ) {
                paranode = (dir_node *)SymLCreate( name );
            } else
                paranode = (dir_node *)SymCreate( "", FALSE );/* for PROTO, no param name needed */

            if( paranode == NULL ) { /* error msg has been displayed already */
                DebugMsg(("ParseParams: SymLCreate(%s) failed\n", name ));
                return( ERROR );
            }
            paranode->sym.isdefined = TRUE;
            paranode->sym.mem_type = ti.mem_type;
            if ( ti.mem_type == MT_TYPE ) {
                paranode->sym.type = ti.symtype;
            } else {
                paranode->sym.target_type = ti.symtype;
            }

            /* v2.05: moved BEFORE fastcall_tab() */
            paranode->sym.isfar   = ti.is_far;
            paranode->sym.Ofssize = ti.Ofssize;
            paranode->sym.is_ptr  = ti.is_ptr;
            paranode->sym.ptr_memtype = ti.ptr_memtype;
            paranode->sym.is_vararg = is_vararg;
            if ( proc->sym.langtype == LANG_FASTCALL &&
                fastcall_tab[Options.fastcall].paramcheck( proc, paranode, &fcint ) ) {
            } else {
                paranode->sym.state = SYM_STACK;
            }

            paranode->sym.total_length = 1; /* v2.04: added */
            paranode->sym.total_size = ti.size;

            if( paranode->sym.is_vararg == FALSE )
                proc->e.procinfo->parasize += ROUND_UP( ti.size, CurrWordSize );

            /* v2.05: the PROC's vararg flag has been set already */
            //proc->e.procinfo->is_vararg |= paranode->sym.is_vararg;

            /* Parameters usually are stored in "push" order.
             * However, for Win64, it's better to store them
             * the "natural" way from left to right, since the
             * arguments aren't "pushed".
             */

            switch( proc->sym.langtype ) {
            case LANG_BASIC:
            case LANG_FORTRAN:
            case LANG_PASCAL:
#if AMD64_SUPPORT
            left_to_right:
#endif
                paranode->nextparam = NULL;
                if( proc->e.procinfo->paralist == NULL ) {
                    proc->e.procinfo->paralist = paranode;
                } else {
                    for( paracurr = proc->e.procinfo->paralist;; paracurr = paracurr->nextparam ) {
                        if( paracurr->nextparam == NULL ) {
                            break;
                        }
                    }
                    paracurr->nextparam = paranode;
                    paracurr = NULL;
                }
                break;
#if AMD64_SUPPORT
            case LANG_FASTCALL:
                if ( ti.Ofssize == USE64 )
                    goto left_to_right;
#endif
            default:
                paranode->nextparam = proc->e.procinfo->paralist;
                proc->e.procinfo->paralist = paranode;
                break;
            }
        }
        if ( AsmBuffer[i]->token != T_FINAL ) {
            if( AsmBuffer[i]->token != T_COMMA ) {
                DebugMsg(("ParseParams: error, cntParam=%u, found %s\n", cntParam, AsmBuffer[i]->tokpos ));
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;    /* go past comma */
        }
    } /* end for */

    if ( proc->e.procinfo->init == TRUE ) {
        if ( paracurr ) {
            /* first definition has more parameters than second */
            DebugMsg(("ParseParams: a param is left over, cntParam=%u\n", cntParam));
            AsmErr( CONFLICTING_PARAMETER_DEFINITION, "" );
            return( ERROR );
        }
    } else {
        int curr;

        //if( proc->e.procinfo->mem_type == MT_NEAR ) {
        if( proc->sym.mem_type == MT_NEAR ) {
            offset = 4 << ModuleInfo.Ofssize; /* offset from [E]BP : return addr + old [E]BP */
        } else {
            offset = 6 << ModuleInfo.Ofssize; /* 3 * sizeof(uint_16) */
        }

        /* now calculate the (E)BP offsets */

#if AMD64_SUPPORT
        if ( ModuleInfo.Ofssize == USE64 && proc->sym.langtype == LANG_FASTCALL ) {
            for ( paranode = proc->e.procinfo->paralist; paranode ;paranode = paranode->nextparam )
                if ( paranode->sym.state == SYM_TMACRO ) /* register param */
                    ;
                else {
                    paranode->sym.offset = offset;
                    proc->e.procinfo->stackparam = TRUE;
                    offset += ROUND_UP( paranode->sym.total_size, CurrWordSize );
                }
        } else
#endif
        for ( ; cntParam; cntParam-- ) {
            for ( curr = 1, paranode = proc->e.procinfo->paralist; curr < cntParam;paranode = paranode->nextparam, curr++ );
            DebugMsg1(("ParseParams: parm=%s, ofs=%u, size=%d\n", paranode->sym.name, offset, paranode->sym.total_size));
            if ( paranode->sym.state == SYM_TMACRO ) /* register param? */
                ;
            else {
                paranode->sym.offset = offset;
                proc->e.procinfo->stackparam = TRUE;
                offset += ROUND_UP( paranode->sym.total_size, CurrWordSize );
            }
        }
    }
    return ( NOT_ERROR );
}

/*
 * create a PROC type
 * i = position of attributes
 * strategy to set default value for "offset size" (16/32):
 * 1. if current model is FLAT, use 32, else
 * 2. use the current segment's attribute
 * 3. if no segment is set, use cpu setting
 */

ret_code ExamineProc( dir_node *proc, int i, bool IsPROC )
/********************************************************/
{
    char            *token;
    uint_16         *regist;
    int             type;
    enum lang_type  langtype;
    memtype         newmemtype;
    uint_8          newofssize;
#if FASTPASS
    bool            oldpublic = proc->sym.public;
#endif

    /* set some default values */

    proc->sym.isdefined = TRUE;

    if ( IsPROC ) {
        proc->e.procinfo->export = ModuleInfo.procs_export;
        /* don't overwrite a PUBLIC directive for this symbol! */
        if ( ModuleInfo.procs_private == FALSE )
            proc->sym.public = TRUE;
#if AMD64_SUPPORT
        if ( Options.masm_compat_gencode )
            /* use LEAVE for cpu >= .286 */
            proc->e.procinfo->pe_type = ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_286 );
        else
            /* use LEAVE for 286, 386 and x64 */
            proc->e.procinfo->pe_type = ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_286 ) ||
                ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_386 ) ||
                ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_64 );
#else
        /* use LEAVE for 286 and 386 */
        proc->e.procinfo->pe_type = ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_286 ) || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_386 );
#endif
    }

#if MANGLERSUPP
    /* OW name mangling */
    if( AsmBuffer[i]->token == T_STRING && IsPROC ) {
        /* SetMangler() will ignore LANG_NONE */
        SetMangler( &proc->sym, LANG_NONE, AsmBuffer[i]->string_ptr );
        i++;
    }
#endif

    /* 1. attribute is <distance> */
    if ( AsmBuffer[i]->token == T_STYPE &&
        AsmBuffer[i]->value >= T_NEAR && AsmBuffer[i]->value <= T_FAR32 ) {
        /* v2.05: FindStdType() is obsolete */
        type = AsmBuffer[i]->value8;
        //type = FindStdType(AsmBuffer[i]->value);
        if ( IsPROC ) {
            if (( ModuleInfo.Ofssize >= USE32 && SimpleType[type].Ofssize == USE16 ) ||
                ( ModuleInfo.Ofssize == USE16 && SimpleType[type].Ofssize == USE32 )) {
                AsmError( DISTANCE_INVALID );
            }
        }
        newmemtype = SimpleType[type].mem_type;
        if ( SimpleType[type].Ofssize != USE_EMPTY )
            newofssize = SimpleType[type].Ofssize;
        else
            newofssize = ModuleInfo.Ofssize;

        i++;
    } else {
        newmemtype = SimpleType[ST_PROC].mem_type;
        newofssize = ModuleInfo.Ofssize;
    }

    /* did the distance attribute change? */
    if ( proc->sym.mem_type != MT_EMPTY &&
        ( proc->sym.mem_type != newmemtype ||
         GetSymOfssize( &proc->sym ) != newofssize ) ) {
        DebugMsg(("ExamineProc: error, memtype changed, old-new memtype=%X-%X, ofssize=%X-%X\n", proc->sym.mem_type, newmemtype, proc->sym.Ofssize, newofssize));
        if ( proc->sym.mem_type == MT_NEAR || proc->sym.mem_type == MT_FAR )
            AsmError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
        else {
            AsmErr( SYMBOL_REDEFINITION, proc->sym.name );
            return( ERROR );
        }
    } else {
        proc->sym.mem_type = newmemtype;
        if ( IsPROC == FALSE )
            proc->sym.seg_ofssize = newofssize;
    }

    /* 2. attribute is <langtype> */
    langtype = ModuleInfo.langtype; /* set the default value */
    GetLangType( &i, &langtype ); /* optionally overwrite the value */
    /* has language changed? */
    if ( proc->sym.langtype != LANG_NONE && proc->sym.langtype != langtype ) {
        DebugMsg(("ExamineProc: error, language changed, %u - %u\n", proc->sym.langtype, langtype ));
        AsmError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
    } else
        proc->sym.langtype = langtype;

    /* 3. attribute is <visibility> */
    /* note that reserved word PUBLIC is a directive! */
    /* PROTO does NOT accept PUBLIC!
     * PROTO accepts PRIVATE, but this attribute is ignored then!
     */

    if ( AsmBuffer[i]->token == T_ID || AsmBuffer[i]->token == T_DIRECTIVE ) {
        token = AsmBuffer[i]->string_ptr;
        if ( _stricmp( token, "PRIVATE") == 0 ) {
            proc->sym.public = FALSE;
#if FASTPASS
            /* error if there was a PUBLIC directive! */
            proc->sym.scoped = TRUE;
            if ( oldpublic ) {
                SkipSavedState(); /* do a full pass-2 scan */
            }
#endif
            proc->e.procinfo->export = FALSE;
            i++;
        } else if ( IsPROC && (_stricmp(token, "PUBLIC") == 0 ) ) {
            proc->sym.public = TRUE;
            proc->e.procinfo->export = FALSE;
            i++;
        } else if ( _stricmp(token, "EXPORT") == 0 ) {
            proc->sym.public = TRUE;
            proc->e.procinfo->export = TRUE;
            i++;
        }
    }

    /* 4. attribute is <prologuearg>, for PROC only.
     it must be enclosed in <> */
    if ( IsPROC && AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
        int idx = Token_Count + 1;
        int max;
        if ( ModuleInfo.prologuemode == PEM_NONE )
            ; /* no prologue at all */
        else if ( ModuleInfo.prologuemode == PEM_MACRO ) {
            proc->e.procinfo->prologuearg = AsmAlloc( AsmBuffer[i]->value + 1 );
            strcpy( proc->e.procinfo->prologuearg, AsmBuffer[i]->string_ptr );
        } else {
            /* check the argument. The default prologue
             understands FORCEFRAME and LOADDS only
             */
            max = Tokenize( AsmBuffer[i]->string_ptr, idx, FALSE );
            for ( ; idx < max; idx++ ) {
                if ( AsmBuffer[idx]->token == T_ID ) {
                    if ( _stricmp( AsmBuffer[idx]->string_ptr, "FORCEFRAME") == 0 ) {
                        proc->e.procinfo->forceframe = TRUE;
#if AMD64_SUPPORT
                    } else if ( ModuleInfo.Ofssize != USE64 && (_stricmp( AsmBuffer[idx]->string_ptr, "LOADDS") == 0 ) ) {
#else
                    } else if ( _stricmp( AsmBuffer[idx]->string_ptr, "LOADDS") == 0 ) {
#endif
                        if ( ModuleInfo.model == MOD_FLAT && Parse_Pass == PASS_1 ) {
                            AsmWarn( 2, LOADDS_IGNORED_IN_FLAT_MODEL );
                        } else
                            proc->e.procinfo->loadds = TRUE;
                    } else {
                        AsmErr( UNKNOWN_DEFAULT_PROLOGUE_ARGUMENT, AsmBuffer[idx]->string_ptr );
                        return( ERROR );
                    }
                    if ( AsmBuffer[idx+1]->token == T_COMMA && AsmBuffer[idx+2]->token != T_FINAL)
                        idx++;
                } else {
                    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[idx]->string_ptr );
                    return( ERROR );
                }
            }
        }
        i++;
    }

#if AMD64_SUPPORT
    /* check for optional FRAME[:exc_proc] */
    if ( ModuleInfo.Ofssize == USE64 &&
        IsPROC &&
        AsmBuffer[i]->token == T_RES_ID &&
        AsmBuffer[i]->value == T_FRAME ) {
        /* v2.05: don't accept FRAME for ELF */
        if ( Options.output_format != OFORMAT_COFF ) {
            AsmErr( NOT_SUPPORTED_WITH_CURR_FORMAT, GetResWName( T_FRAME, NULL ) );
            return( ERROR );
        }
        i++;
        if( AsmBuffer[i]->token == T_COLON ) {
            asm_sym *sym;
            i++;
            if ( AsmBuffer[i]->token != T_ID ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
            sym = SymSearch( AsmBuffer[i]->string_ptr );
            if ( sym == NULL ) {
                sym = SymCreate( AsmBuffer[i]->string_ptr, TRUE );
                sym->state = SYM_UNDEFINED;
                sym->used = TRUE;
                dir_add_table( &Tables[TAB_UNDEF], (dir_node *)sym ); /* add UNDEFINED */
            } else if ( sym->state != SYM_UNDEFINED &&
                       sym->state != SYM_INTERNAL &&
                       sym->state != SYM_EXTERNAL ) {
                AsmErr( SYMBOL_REDEFINITION, sym->name );
                return( ERROR );
            }
            proc->e.procinfo->exc_handler = sym;
            i++;
        } else
            proc->e.procinfo->exc_handler = NULL;
        proc->e.procinfo->isframe = TRUE;
    }
#endif
    /* check for USES */
    if ( AsmBuffer[i]->token == T_ID && _stricmp(AsmBuffer[i]->string_ptr, "USES") == 0 ) {
        int cnt;
        int j;
        if ( !IsPROC ) {/* not for PROTO! */
            DebugMsg(("ExamineProc: USES found in PROTO\n"));
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        }
        i++;
        /* count register names which follow */
        for ( cnt = 0, j = i; AsmBuffer[j]->token == T_REG; j++, cnt++ );

        if ( cnt == 0 ) {
            DebugMsg(("ExamineProc: no registers for regslist\n"));
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->tokpos );
        } else {
            regist = AsmAlloc( (cnt + 1) * sizeof( uint_16 ) );
            proc->e.procinfo->regslist = regist;
            *regist++ = cnt;
            /* read in registers */
            for( ; AsmBuffer[i]->token == T_REG; i++ ) {
                if ( SizeFromRegister( AsmBuffer[i]->value ) == 1 ) {
                    AsmError( INVALID_USE_OF_REGISTER );
                }
                *regist++ = AsmBuffer[i]->value;
            }
        }
    }

    /* the parameters must follow */
    if ( AsmBuffer[i]->token == T_STYPE || AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE )
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );

    /* skip optional comma */
    if ( AsmBuffer[i]->token == T_COMMA )
        i++;

    DebugMsg1(("ExamineProc(%s): i=%u, Token_Count=%u, CurrWordSize=%u\n", proc->sym.name, i, Token_Count, CurrWordSize ));

    /* are there parameters at all? */
    if( i >= Token_Count ) {
        if ( proc->e.procinfo->init == TRUE && proc->e.procinfo->paralist != NULL )
            AsmErr( CONFLICTING_PARAMETER_DEFINITION, "" );
    } else if( proc->sym.langtype == LANG_NONE ) {
        AsmError( LANG_MUST_BE_SPECIFIED );
        return ( ERROR );
    } else  {
        /* v2.05: set PROC's vararg flag BEFORE params are scanned! */
        if ( AsmBuffer[Token_Count - 1]->token == T_RES_ID &&
            AsmBuffer[Token_Count - 1]->value == T_VARARG )
            proc->e.procinfo->is_vararg = TRUE;
        /* v2.04: removed, comma is checked above already */
        //if( AsmBuffer[i]->token == T_COMMA )
        //    i++;
        if ( ERROR == ParseParams( proc, i, IsPROC ) )
            /* do proceed if the parameter scan returns an error */
            ;//return( ERROR );
    }

    proc->e.procinfo->init = TRUE;
    DebugMsg1(("ExamineProc(%s): memtype=%Xh parasize=%u\n", proc->sym.name, proc->sym.mem_type, proc->e.procinfo->parasize));

    return( NOT_ERROR );
}

/* create a proc item.
 * sym is either NULL, or has type SYM_UNDEFINED or SYM_EXTERNAL */

asm_sym *CreateProc( asm_sym *sym, const char *name, unsigned char IsPROC )
/*************************************************************************/
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    else
        dir_remove_table( ( sym->state == SYM_UNDEFINED ) ? &Tables[TAB_UNDEF] : &Tables[TAB_EXT], (dir_node *)sym );

    if ( sym ) {
        proc_info *info;
        if ( IsPROC )
            sym->state = SYM_INTERNAL;
        else {
            sym->seg_ofssize = ModuleInfo.Ofssize;
            sym->state = SYM_EXTERNAL;
            sym->weak = TRUE;
        }
        info = AsmAlloc( sizeof( proc_info ) );
        ((dir_node *)sym)->e.procinfo = info;
        info->regslist = NULL;
        info->paralist = NULL;
        info->locallist = NULL;
        info->labellist = NULL;
        info->parasize = 0;
        info->localsize = 0;
        info->prologuearg = NULL;
        info->flags = 0;
        if ( *(sym->name) )
            if ( sym->state == SYM_INTERNAL )
                /* v2.04: don't use dir_add_table() and thus
                 * free the <next> member field!
                 */
                if ( Tables[TAB_PROC].head == NULL )
                    Tables[TAB_PROC].head = Tables[TAB_PROC].tail = (dir_node *)sym;
                else {
                    Tables[TAB_PROC].tail->nextproc = (dir_node *)sym;
                    Tables[TAB_PROC].tail = (dir_node *)sym;
                }
            else
                dir_add_table( &Tables[TAB_EXT], (dir_node *)sym ); /* EXTERNAL */

        if ( IsPROC ) {
            procidx++;
            if ( Options.line_numbers ) {
                sym->debuginfo = AsmAlloc( sizeof( struct debug_info ) );
                sym->debuginfo->file = get_curr_srcfile();
            }
        }
    }
    return( sym );
}

/* delete a PROC item */

void DeleteProc( dir_node *dir )
/******************************/
{
    dir_node    *symcurr;
    dir_node    *symnext;

    DebugMsg1(("DeleteProc(%s) enter\n", dir->sym.name ));
    /* delete all local symbols ( params, locals, labels ) */
    if ( dir->sym.state == SYM_INTERNAL ) {

        for( symcurr = dir->e.procinfo->labellist; symcurr; ) {
            symnext = symcurr->nextll;
            DebugMsg(("DeleteProc(%s): free %s\n", dir->sym.name, symcurr->sym.name ));
            SymFree( &symcurr->sym );
            symcurr = symnext;
        }

        if ( dir->e.procinfo->regslist )
            AsmFree( dir->e.procinfo->regslist );

        if ( dir->e.procinfo->prologuearg )
            AsmFree( dir->e.procinfo->prologuearg );

        if ( Options.line_numbers && dir->sym.state == SYM_INTERNAL )
            AsmFree( dir->sym.debuginfo );
    } else {
        /* PROTOs have just a parameter list, usually without names */
        for( symcurr = dir->e.procinfo->paralist; symcurr; ) {
            symnext = symcurr->nextparam;
            DebugMsg(("DeleteProc(%s): free %p (%s)\n", dir->sym.name, symcurr, symcurr->sym.name ));
            SymFree( &symcurr->sym );
            symcurr = symnext;
        }
    }
    AsmFree( dir->e.procinfo );
    return;
}

/* PROC directive. */

ret_code ProcDef( int i )
/***********************/
{
    struct asm_sym      *sym;
    dir_node            *dir;
    unsigned int        ofs;
    char                *name;
    bool                oldpubstate;
    bool                is_global;

    DebugMsg1(("ProcDef enter, curr ofs=%X\n", GetCurrOffset() ));
    if( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    /* v2.04b: check was missing */
    if( CurrSeg == NULL ) {
        AsmError( MUST_BE_IN_SEGMENT_BLOCK );
        return( ERROR );
    }

    name = AsmBuffer[0]->string_ptr;

    if( CurrProc != NULL ) {

        /* this is not needed for JWasm, but Masm will reject nested
         * procs if there are params, locals or used registers.
         */
        if ( CurrProc->e.procinfo->paralist ||
#if AMD64_SUPPORT
            CurrProc->e.procinfo->isframe ||
#endif
            CurrProc->e.procinfo->locallist ||
            CurrProc->e.procinfo->regslist ) {
            AsmErr( CANNOT_NEST_PROCEDURES, name );
            return( ERROR );
        }
        /* nested procs ... push currproc on a stack */
        push_proc( CurrProc );
    }


    if ( ModuleInfo.procalign ) {
        AlignCurrOffset( ModuleInfo.procalign );
    }

    i++; /* go past PROC */

    sym = SymSearch( name );

    if( Parse_Pass == PASS_1 ) {

        oldpubstate = sym ? sym->public : FALSE;

        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = CreateProc( sym, name, TRUE );
            is_global = FALSE;
        } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
            /* PROTO or EXTERNDEF item */
            is_global = TRUE;
            if ( sym->isproc == TRUE  ) {
                /* don't create the procinfo extension; it exists already */
                procidx++; /* v2.04: added */
                if ( Options.line_numbers ) {
                    sym->debuginfo = AsmAlloc( sizeof( struct debug_info ) );
                    sym->debuginfo->file = get_curr_srcfile();
                }
            } else {
                /* it's a simple EXTERNDEF. Create a PROC item!
                 * this will be SYM_INTERNAL */
                /* v2.03: don't call dir_free(), it'll clear field Ofssize */
                //dir_free( (dir_node *)sym );
                sym = CreateProc( sym, name, TRUE );
            }
        } else {
            AsmErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }
        dir = (dir_node *)sym;

        SetSymSegOfs( sym );

        SymClearLocal();

        CurrProc = (dir_node *)sym;

        if( ExamineProc( dir, i, TRUE ) == ERROR ) {
            CurrProc = NULL;
            return( ERROR );
        }

        /* v2.04: added */
        if ( is_global && Options.masm8_proc_visibility )
            sym->public = TRUE;

        /* if there was a PROTO (or EXTERNDEF name:PROTO ...),
         * change symbol to SYM_INTERNAL! */
        if ( sym->state == SYM_EXTERNAL && sym->isproc == TRUE ) {
            dir_ext2int( dir );
        }

        sym->isproc = TRUE;

        if( sym->public == TRUE && oldpubstate == FALSE )
            AddPublicData( sym );

        /* v2.04: add the proc to the list of labels attached to curr segment.
         * this allows to reduce the number of passes (see fixup.c)
         */
        ((dir_node *)sym)->next = (dir_node *)CurrSeg->e.seginfo->labels;
        CurrSeg->e.seginfo->labels = sym;

    } else {
        /**/myassert( sym != NULL );

        procidx++;
        sym->isdefined = TRUE;

        SymSetLocal( sym );

        /* it's necessary to check for a phase error here
         as it is done in LabelCreate() and data_dir()!
         */
        ofs = GetCurrOffset();

        if ( ofs != sym->offset) {
            DebugMsg(("ProcDef(%s): %spass %u, old ofs=%" FX32 ", new ofs=%" FX32 "\n",
                    sym->name,
                    ModuleInfo.PhaseError ? "" : "phase error ",
                    Parse_Pass+1, sym->offset, ofs ));
            sym->offset = ofs;
            ModuleInfo.PhaseError = TRUE;
        }
        CurrProc = (dir_node *)sym;
#if AMD64_SUPPORT
        /* check if the exception handler set by FRAME is defined */
        if ( CurrProc->e.procinfo->isframe &&
            CurrProc->e.procinfo->exc_handler &&
            CurrProc->e.procinfo->exc_handler->state == SYM_UNDEFINED ) {
            AsmErr( SYMBOL_NOT_DEFINED, CurrProc->e.procinfo->exc_handler->name );
        }
#endif
    }

    DefineProc = TRUE;
#if AMD64_SUPPORT
    if ( CurrProc->e.procinfo->isframe ) {
        endprolog_found = FALSE;
        if ( CurrProc->e.procinfo->exc_handler )
            unw_info.Flags = UNW_FLAG_FHANDLER;
        else
            unw_info.Flags = 0;
        unw_info.SizeOfProlog = 0;
        unw_info.CountOfCodes = 0;
    }
#endif

    sym->asmpass = Parse_Pass;
    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    if( Options.line_numbers ) {
        if ( Options.output_format == OFORMAT_COFF )
            AddLinnumDataRef( 0 );
        else
            AddLinnumDataRef( LineNumber );
    }

    BackPatch( sym );
    return( NOT_ERROR );
}

ret_code CopyPrototype( dir_node *proc, dir_node *src )
/*****************************************************/
{
    dir_node *curr;
    dir_node *newl;
    dir_node *oldl;

    if ( src->sym.isproc == FALSE )
        return( ERROR );
    memcpy(proc->e.procinfo, src->e.procinfo, sizeof(proc_info));
    proc->sym.mem_type = src->sym.mem_type;
    proc->sym.langtype = src->sym.langtype;
#if MANGLERSUPP
    proc->sym.mangler  = src->sym.mangler;
#endif
    proc->sym.public   = src->sym.public;
    /* we use the PROTO part, not the TYPE part */
    //dir->sym.seg_ofssize = src->sym.Ofssize;
    proc->sym.seg_ofssize = src->sym.seg_ofssize;
    proc->sym.isproc = TRUE;
    proc->e.procinfo->paralist = NULL;
    for ( curr = src->e.procinfo->paralist; curr; curr = curr->nextparam ) {
        newl = AsmAlloc( sizeof(dir_node) );
        memcpy( newl, curr, sizeof(dir_node) );
        newl->nextparam = NULL;
        if ( proc->e.procinfo->paralist == NULL)
            proc->e.procinfo->paralist = newl;
        else {
            for ( oldl = proc->e.procinfo->paralist; oldl->nextparam; oldl = oldl->nextparam );
            oldl->nextparam = newl;
        }
    }
    DebugMsg1(("CopyPrototype(%s,src=%s): ofssize=%u\n",
               proc->sym.name, src->sym.name, src->sym.seg_ofssize ));
    return( NOT_ERROR );
}

#if AMD64_SUPPORT

/* for FRAME procs, write .pdata and .xdata SEH unwind information */

static void WriteSEHData( dir_node *proc )
/****************************************/
{
    dir_node *xdata;
    char *segname = ".xdata";
    int i;
    uint_8 olddotname;
    uint_32 xdataofs = 0;
    char buffer[128];

    if ( endprolog_found == FALSE ) {
        AsmErr( MISSING_ENDPROLOG, proc->sym.name );
    }
    PushLineQueue();
    if ( unw_segs_defined )
        AddLineQueueX("%s %r", segname, T_SEGMENT );
    else {
        AddLineQueueX("%s %r align(%u) flat readonly 'DATA'", segname, T_SEGMENT, 8 );
        AddLineQueue("$xdatasym label near");
    }
    xdataofs = 0;
    xdata = (dir_node *)SymSearch( segname );
    if ( xdata )
        xdataofs = xdata->sym.offset;
    /* write the .xdata stuff (a UNWIND_INFO entry ) */
    AddLineQueueX( "db 0%xh + (0%xh shl 3), %u, %u, 0%xh + (0%xh shl 4)",
            unw_info.Version, unw_info.Flags, unw_info.SizeOfProlog,
            unw_info.CountOfCodes, unw_info.FrameRegister, unw_info.FrameOffset );
    if ( unw_info.CountOfCodes ) {
        char *pfx = "dw";
        buffer[0] = NULLC;
        /* write the codes from right to left */
        for ( i = unw_info.CountOfCodes; i ; i-- ) {
            sprintf( buffer + strlen( buffer ), "%s 0%xh", pfx, unw_code[i-1] );
            pfx = ",";
            if ( i == 1 || strlen( buffer ) > 72 ) {
                AddLineQueue( buffer );
                buffer[0] = NULLC;
                pfx = "dw";
            }
        }
    }
    AddLineQueueX( "%r 4", T_ALIGN );
    if ( proc->e.procinfo->exc_handler ) {
        AddLineQueueX( "dd %r %s", T_IMAGEREL, proc->e.procinfo->exc_handler->name );
        AddLineQueueX( "%r 8", T_ALIGN );
    }
    AddLineQueueX( "%s %r", segname, T_ENDS );

    segname = ".pdata";
    if ( unw_segs_defined )
        AddLineQueueX( "%s %r", segname, T_SEGMENT );
    else
        AddLineQueueX( "%s %r align(%u) flat readonly 'DATA'", segname, T_SEGMENT, 4 );
    unw_segs_defined = 1;
    /* write the .pdata stuff ( type IMAGE_RUNTIME_FUNCTION_ENTRY )*/
    AddLineQueueX( "dd %r %s, %r %s+0%xh, %r $xdatasym+0%xh",
                  T_IMAGEREL, proc->sym.name,
                  T_IMAGEREL, proc->sym.name, proc->sym.total_size,
                  T_IMAGEREL, xdataofs );
    AddLineQueueX("%s %r", segname, T_ENDS );
    olddotname = ModuleInfo.dotname;
    ModuleInfo.dotname = TRUE; /* set OPTION DOTNAME because .pdata and .xdata */
    RunLineQueue();
    ModuleInfo.dotname = olddotname;
    return;
}
#endif

/* close a PROC
 */

static void ProcFini( dir_node *proc )
/************************************/
{
    proc->sym.total_size = GetCurrOffset() - proc->sym.offset;

    /* v2.03: for W3+, check for unused params and locals */
    if ( Options.warning_level > 2 ) {
        dir_node *curr;
        for ( curr = proc->e.procinfo->paralist; curr; curr = curr->nextparam ) {
            if ( curr->sym.used == FALSE && Parse_Pass == PASS_1 )
                AsmWarn( 3, PROCEDURE_ARGUMENT_OR_LOCAL_NOT_REFERENCED, curr->sym.name );
        }
        for ( curr = proc->e.procinfo->locallist; curr; curr = curr->nextlocal ) {
            if ( curr->sym.used == FALSE && Parse_Pass == PASS_1 )
                AsmWarn( 3, PROCEDURE_ARGUMENT_OR_LOCAL_NOT_REFERENCED, curr->sym.name );
        }
    }
#if AMD64_SUPPORT
    /* create the .pdata and .xdata stuff */
    if ( proc->e.procinfo->isframe )
        WriteSEHData( proc );
#endif
    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    /* create the list of locals */
    if ( Parse_Pass == PASS_1 )
        SymGetLocal( (asm_sym *)CurrProc );

    CurrProc = pop_proc();
    if ( CurrProc )
        SymSetLocal( (asm_sym *)CurrProc );  /* restore local symbol table */

    DefineProc = FALSE; /* in case there was an empty PROC/ENDP pair */
}

/* ENDP directive */

ret_code EndpDef( int i )
/***********************/
{
    if( i != 1 || AsmBuffer[2]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if( CurrProc && ( SymCmpFunc(CurrProc->sym.name, AsmBuffer[0]->string_ptr, CurrProc->sym.name_size ) == 0 ) ) {
        ProcFini( CurrProc );
    } else {
        AsmErr( UNMATCHED_BLOCK_NESTING, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

#if AMD64_SUPPORT

/* handles win64 directives
 * .allocstack
 * .endprolog
 * .pushframe
 * .pushreg
 * .savereg
 * .savexmm128
 * .setframe
 */

ret_code ExcFrameDirective( int i )
/*********************************/
{
    expr_list opndx;
    int token;
    unsigned int size;
    uint_8 reg;
    uint_8 ofs;
    UNWIND_CODE *puc;

    DebugMsg(("ExcFrameDirective(%s) enter\n", AsmBuffer[i]->string_ptr ));
    /* v2.05: accept directives for windows only */
    if ( Options.output_format != OFORMAT_COFF ) {
        AsmErr( NOT_SUPPORTED_WITH_CURR_FORMAT, GetResWName( AsmBuffer[i]->value, NULL ) );
        return( ERROR );
    }
    if ( CurrProc == NULL || endprolog_found == TRUE ) {
        AsmError( ENDPROLOG_FOUND_BEFORE_EH_DIRECTIVES );
        return( ERROR );
    }
    if ( CurrProc->e.procinfo->isframe == FALSE ) {
        AsmError( MISSING_FRAME_IN_PROC );
        return( ERROR );
    }
    puc = &unw_code[unw_info.CountOfCodes];
    ofs = GetCurrOffset() - CurrProc->sym.offset;
    token = AsmBuffer[i]->value;
    switch ( token ) {
    case T_DOT_ALLOCSTACK: /* syntax: .ALLOCSTACK size */
        i++;
        if ( ERROR == EvalOperand( &i, Token_Count, &opndx, 0 ) )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( opndx.value & 7 ) {
            AsmError( BAD_ALIGNMENT_FOR_OFFSET_IN_UNWIND_CODE );
            return( ERROR );
        }
        if ( opndx.value == 0 ) {
            AsmError( NONZERO_VALUE_EXPECTED );
            return( ERROR );
        }
        opndx.value -= 8;
        if ( opndx.value > 16*8 ) {
            if ( opndx.value > 65536 * 8 ) {
                puc->FrameOffset = ( opndx.value >> 19 );
                puc++;
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                unw_info.CountOfCodes += 2;
                puc->OpInfo = 1;
            } else {
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                unw_info.CountOfCodes++;
                puc->OpInfo = 0;
            }
            puc->UnwindOp = UWOP_ALLOC_LARGE;
        } else {
            puc->UnwindOp = UWOP_ALLOC_SMALL;
            puc->OpInfo = ( opndx.value >> 3 );
        }
        puc->CodeOffset = ofs;
        unw_info.CountOfCodes++;
        break;
    case T_DOT_ENDPROLOG: /* syntax: .ENDPROLOG */
        opndx.value = GetCurrOffset() - CurrProc->sym.offset;
        if ( opndx.uvalue > 255 ) {
            AsmError( SIZE_OF_PROLOG_TOO_BIG );
            return( ERROR );
        }
        unw_info.SizeOfProlog = (uint_8)opndx.uvalue;
        endprolog_found = TRUE;
        i++;
        break;
    case T_DOT_PUSHFRAME: /* syntax: .PUSHFRAME [code] */
        i++;
        puc->CodeOffset = ofs;
        puc->UnwindOp = UWOP_PUSH_MACHFRAME;
        puc->OpInfo = 0;
        if ( AsmBuffer[i]->token == T_ID && (_stricmp( AsmBuffer[i]->string_ptr, "CODE") == 0 ) ) {
            puc->OpInfo = 1;
            i++;
        }
        unw_info.CountOfCodes++;
        break;
    case T_DOT_PUSHREG: /* syntax: .PUSHREG r64 */
        i++;
        if ( AsmBuffer[i]->token != T_REG || !( GetValueSp( AsmBuffer[i]->value ) & OP_R64) ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        puc->CodeOffset = ofs;
        puc->UnwindOp = UWOP_PUSH_NONVOL;
        puc->OpInfo = GetRegNo( AsmBuffer[i]->value );
        unw_info.CountOfCodes++;
        i++;
        break;
    case T_DOT_SAVEREG:    /* syntax: .SAVEREG r64, offset       */
    case T_DOT_SAVEXMM128: /* syntax: .SAVEXMM128 xmmreg, offset */
    case T_DOT_SETFRAME:   /* syntax: .SETFRAME r64, offset      */
        i++;
        if ( AsmBuffer[i]->token != T_REG ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        if ( token == T_DOT_SAVEXMM128 ) {
            if ( !( GetValueSp( AsmBuffer[i]->value ) & OP_XMM ) ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
        } else {
            if ( !( GetValueSp( AsmBuffer[i]->value ) & OP_R64 ) ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
        }
        reg = GetRegNo( AsmBuffer[i]->value );

        if ( token == T_DOT_SAVEREG )
            size = 8;
        else
            size = 16;

        i++;
        if ( AsmBuffer[i]->token != T_COMMA ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        i++;
        if ( ERROR == EvalOperand( &i, Token_Count, &opndx, 0 ) )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( opndx.value & (size - 1) ) {
            AsmError( BAD_ALIGNMENT_FOR_OFFSET_IN_UNWIND_CODE );
            return( ERROR );
        }
        switch ( token ) {
        case T_DOT_SAVEREG:
            puc->OpInfo = reg;
            if ( opndx.value > 65536 * size ) {
                puc->FrameOffset = ( opndx.value >> 19 );
                puc++;
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_NONVOL_FAR;
                unw_info.CountOfCodes += 3;
            } else {
                puc->FrameOffset = ( opndx.value >> 3 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_NONVOL;
                unw_info.CountOfCodes += 2;
            }
            puc->CodeOffset = ofs;
            puc->OpInfo = reg;
            break;
        case T_DOT_SAVEXMM128:
            if ( opndx.value > 65536 * size ) {
                puc->FrameOffset = ( opndx.value >> 20 );
                puc++;
                puc->FrameOffset = ( opndx.value >> 4 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_XMM128_FAR;
                unw_info.CountOfCodes += 3;
            } else {
                puc->FrameOffset = ( opndx.value >> 4 );
                puc++;
                puc->UnwindOp = UWOP_SAVE_XMM128;
                unw_info.CountOfCodes += 2;
            }
            puc->CodeOffset = ofs;
            puc->OpInfo = reg;
            break;
        case T_DOT_SETFRAME:
            if ( opndx.uvalue > 240 ) {
                AsmErr( CONSTANT_VALUE_TOO_LARGE, opndx.value64 );
                return( ERROR );
            }
            unw_info.FrameRegister = reg;
            unw_info.FrameOffset = ( opndx.uvalue >> 4 );
            puc->CodeOffset = ofs;
            puc->UnwindOp = UWOP_SET_FPREG;
            //puc->OpInfo = ( opndx.uvalue >> 4 );
            puc->OpInfo = reg;
            unw_info.CountOfCodes++;
            break;
        }
        break;
    }
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    DebugMsg(("ExcFrameDirective() exit, ok\n" ));
    return( NOT_ERROR );
}
#endif

/* see if there are open procedures.
 * called when the END directive has been found.
 */
void ProcCheckOpen( void )
/************************/
{
    while( CurrProc != NULL ) {
        AsmErr( UNMATCHED_BLOCK_NESTING, CurrProc->sym.name );
        ProcFini( CurrProc );
    }
}

static ret_code write_userdef_prologue( void )
/********************************************/
{
    int                 len;
    proc_info           *info;
    char                *p;
    bool                is_exitm;
    dir_node            *dir;
    //int                 align = CurrWordSize;
    int                 flags = CurrProc->sym.langtype; /* set bits 0-2 */
    uint_16             *regs;
    char                buffer[128+128];
    char                reglst[128];
    char                retvalue[MAX_LINE_LEN];

#if FASTPASS
    if ( Parse_Pass > PASS_1 && UseSavedState )
        return( NOT_ERROR );
#endif

    info = CurrProc->e.procinfo;
    info->localsize = ROUND_UP( info->localsize, CurrWordSize );

    /* set bit 4 if the caller restores (E)SP */
    if ( CurrProc->sym.langtype == LANG_C ||
        CurrProc->sym.langtype == LANG_SYSCALL ||
        CurrProc->sym.langtype == LANG_FASTCALL )
        flags |= 0x10;

    if ( CurrProc->sym.mem_type == MT_FAR )
        flags |= 0x20;

    if ( CurrProc->sym.public == FALSE )
        flags |= 0x40;

    //flags |= CurrProc->sym.export ? 0 : 0x80; /* bit 7: 1 if export */

    p = reglst;
    if ( info->regslist ) {
        regs = info->regslist;
        for ( len = *regs++; len; len--, regs++ ) {
            GetResWName( *regs, p );
            p += strlen( p );
            if ( len > 1 )
                *p++ = ',';
        }
    }
    *p = NULLC;

    dir = (dir_node *)SymSearch( ModuleInfo.proc_prologue );
    if ( dir == NULL || dir->sym.state != SYM_MACRO || dir->sym.isfunc != TRUE ) {
        AsmError( PROLOGUE_MUST_BE_MACRO_FUNC );
        return( ERROR );
    }

    /* if -EP is on, emit "prologue: none" */
    if ( Options.preprocessor_stdout )
        printf( "option prologue:none\n" );

    sprintf( buffer,"%s(%s, %u, %u, %u, <<%s>>, <%s>)", ModuleInfo.proc_prologue,
             CurrProc->sym.name, flags, info->parasize, info->localsize,
             reglst, info->prologuearg ? info->prologuearg : "" );

    retvalue[0] = NULLC;
    RunMacro( dir, buffer, retvalue, TRUE, FALSE, &is_exitm );
    DebugMsg(("write_userdef_prologue: macro %s returned >%s<\n", ModuleInfo.proc_prologue, retvalue));

    if ( Parse_Pass == PASS_1 ) {
        dir_node *curr;
        len = atoi(retvalue) - info->localsize;
        for ( curr = info->locallist; curr; curr = curr->nextlocal ) {
            curr->sym.offset -= len;
        }
    }

    Token_Count = Tokenize( CurrSource, 0, FALSE );

    return ( NOT_ERROR );
}

#if AMD64_SUPPORT

/* save up to 4 register parameters for WIN64 fastcall */

static void win64_SaveRegParams( proc_info *info )
/************************************************/
{
    int i;
    dir_node *param;

    for ( i = 0, param = info->paralist; param && ( i < 4 ); i++ ) {
        /* v2.05: save XMMx if type is float/double */
        if ( param->sym.is_vararg == FALSE ) {
            if ( param->sym.mem_type & MT_FLOAT )
                AddLineQueueX( "movq [%r+%u], %r", T_RSP, 8 + i * 8, T_XMM0 + i );
            else
                AddLineQueueX( "mov [%r+%u], %r", T_RSP, 8 + i * 8, ms64_regs[i] );
            param = param->nextparam;
        }
    }
    return;
}

/* win64 default prologue when PROC FRAME and
 * OPTION FRAME:AUTO is set */

static ret_code write_win64_default_prologue( proc_info *info )
/*************************************************************/
{
    uint_16             *regist;
    int                 sizestd = 0;
    int                 sizexmm = 0;

    DebugMsg1(("write_win64_default_prologue enter\n"));
    PushLineQueue();

    if ( ModuleInfo.win64_saveparams )
        win64_SaveRegParams( info );
    /*
     * PUSH RBP
     * .PUSHREG RBP
     * MOV RBP, RSP
     * .SETFRAME RBP, 0
     */
    AddLineQueueX( "push %r", T_RBP );
    AddLineQueueX( "%r %r", T_DOT_PUSHREG, T_RBP );
    AddLineQueueX( "mov %r, %r", T_RBP, T_RSP );
    AddLineQueueX( "%r %r, 0", T_DOT_SETFRAME, T_RBP );

    /* after the "push rbp", the stack is xmmword aligned */

    /* Push the registers */
    if( info->regslist ) {
        int cnt;
        regist = info->regslist;
        for( cnt = *regist++; cnt; cnt--, regist++ ) {
            //int i;
            if ( GetValueSp( *regist ) & OP_XMM ) {
                sizexmm += 16;
            } else {
                sizestd += 8;
                AddLineQueueX( "push %r", *regist );
                if ( ( 1 << GetRegNo( *regist ) ) & win64_nvgpr ) {
                    AddLineQueueX( "%r %r", T_DOT_PUSHREG, *regist );
                }
            }
        } /* end for */

        DebugMsg1(("write_win64_default_prologue: sizestd=%u, sizexmm=%u\n", sizestd, sizexmm ));
        sizestd &= 0xF; /* result will be 8 or 0. Just this amount is needed below */
#if 1
        /* save xmm registers */
        if ( sizexmm ) {
            int i;
            AddLineQueueX( "sub %r, %d", T_RSP, NUMQUAL sizexmm + sizestd );
            AddLineQueueX( "%r %d", T_DOT_ALLOCSTACK, NUMQUAL sizexmm + sizestd );
            sizestd = 0; /* stack is aligned now. Don't use sizestd anymore */
            regist = info->regslist;
            for( cnt = *regist++, i = 0; cnt; cnt--, regist++ ) {
                if ( GetValueSp( *regist ) & OP_XMM ) {
                    AddLineQueueX( "movdqa [%r+%u], %r", T_RSP, NUMQUAL i, *regist );
                    if ( ( 1 << GetRegNo( *regist ) ) & win64_nvxmm )  {
                        AddLineQueueX( "%r %r, %u", T_DOT_SAVEXMM128, *regist, NUMQUAL i );
                    }
                    i += 16;
                }
            }
        }
#endif
    }
    info->localsize = ROUND_UP( info->localsize, CurrWordSize );

    /* alloc space for local variables and align the stack. */
    if( info->localsize + sizestd ) {

        /* align the stack if necessary. */
        if ( ( sizestd && (!(info->localsize & 0xF ) ) ) ||
            ( sizestd == 0 && (info->localsize & 0xF ) ) )
            info->localsize += 8;
        DebugMsg1(("write_win64_default_prologue: localsize=%u, sizestd=%u\n", info->localsize, sizestd ));

        /*
         * SUB  RSP, localsize
         * .ALLOCSTACK localsize
         */
        AddLineQueueX( "sub %r, %d", T_RSP, NUMQUAL info->localsize );
        AddLineQueueX( "%r %d", T_DOT_ALLOCSTACK, NUMQUAL info->localsize );
    }

    AddLineQueueX( "%r", T_DOT_ENDPROLOG );

#if FASTPASS
    /* special case: generated code runs BEFORE the line */
    if ( ModuleInfo.list && UseSavedState )
        if ( Parse_Pass == PASS_1 )
            info->list_pos = list_pos;
        else
            list_pos = info->list_pos;
#endif
    RunLineQueue();

#if FASTPASS
    if ( ModuleInfo.list && UseSavedState && (Parse_Pass > PASS_1))
         LineStoreCurr->list_pos = list_pos;
#endif

    Token_Count = Tokenize( CurrSource, 0, FALSE );

    return( NOT_ERROR );
}
#endif

/* write PROC prologue
 * this is to be done after the LOCAL directives
 * and *before* any real instruction
 */
/* prolog code timings

                                                  best result
               size  86  286  386  486  P     86  286  386  486  P
 push bp       2     11  3    2    1    1
 mov bp,sp     2     2   2    2    1    1
 sub sp,immed  4     4   3    2    1    1
              -----------------------------
               8     17  8    6    3    3     x   x    x    x    x

 push ebp      2     -   -    2    1    1
 mov ebp,esp   2     -   -    2    1    1
 sub esp,immed 6     -   -    2    1    1
              -----------------------------
               10    -   -    6    3    3              x    x    x

 enter imm,0   4     -   11   10   14   11

 write prolog code
*/

static ret_code write_default_prologue( void )
/********************************************/
{
    proc_info           *info;
    uint_8              oldlinenumbers;
    //int                 align = CurrWordSize;

    info = CurrProc->e.procinfo;

#if AMD64_SUPPORT
    if ( info->isframe ) {
        //DebugMsg(("write_default_prologue: isframe\n"));
        if ( ModuleInfo.frame_auto )
            return( write_win64_default_prologue( info ) );
        return( NOT_ERROR );
    }
#endif
    /* default processing. if no params/locals are defined, continue */
    if( info->forceframe == FALSE &&
        info->localsize == 0 &&
        info->stackparam == FALSE &&
        info->is_vararg == FALSE &&
        info->regslist == NULL )
        return( NOT_ERROR );

    info->localsize = ROUND_UP( info->localsize, CurrWordSize );
    PushLineQueue();

#if AMD64_SUPPORT
    /* initialize shadow space for register params */
    if ( ModuleInfo.Ofssize == USE64 &&
        CurrProc->sym.langtype == LANG_FASTCALL &&
        Options.fastcall == FCT_WIN64 &&
        ModuleInfo.win64_saveparams )
        win64_SaveRegParams( info );
#endif
    if( ( info->localsize != 0 ) || info->stackparam || info->is_vararg || info->forceframe ) {

        /* write 80386 prolog code
         * PUSH [E|R]BP
         * MOV  [E|R]BP, [E|R]SP
         * SUB  [E|R]SP, localsize
         */
        AddLineQueueX( "push %r", basereg[ModuleInfo.Ofssize] );
        AddLineQueueX( "mov %r, %r", basereg[ModuleInfo.Ofssize], stackreg[ModuleInfo.Ofssize] );

        if( info->localsize != 0 ) {
            /* using ADD and the 2-complement has one advantage:
             * it will generate short instructions up to a size of 128.
             * with SUB, short instructions work up to 127 only
             */
            if ( Options.masm_compat_gencode || info->localsize == 128 )
                AddLineQueueX( "add %r, %d", stackreg[ModuleInfo.Ofssize], NUMQUAL - info->localsize );
            else
                AddLineQueueX( "sub %r, %d", stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize );
        }
    }

    if ( info->loadds ) {
        AddLineQueueX( "push %r", T_DS );
        AddLineQueueX( "mov %r, %s", T_AX, szDgroup );
        AddLineQueueX( "mov %r, %r", T_DS, ModuleInfo.Ofssize ? T_EAX : T_AX );
    }

    /* Push the USES registers */
    if ( info->regslist ) {
        uint_16 *regist = info->regslist;
        int cnt;
        for( cnt = *regist++; cnt; cnt--, regist++ ) {
            AddLineQueueX( "push %r", *regist );
        }
    }
#if FASTPASS
    /* special case: generated code runs BEFORE the line.*/
    if ( ModuleInfo.list && UseSavedState )
        if ( Parse_Pass == PASS_1 )
            info->list_pos = list_pos;
        else
            list_pos = info->list_pos;
#endif
    /* line number debug info also needs special treatment
     * because current line number is the first true src line
     * IN the proc.
     */
    oldlinenumbers = Options.line_numbers;
    Options.line_numbers = FALSE; /* temporarily disable line numbers */
    RunLineQueue();
    Options.line_numbers = oldlinenumbers;

#if FASTPASS
    if ( ModuleInfo.list && UseSavedState && (Parse_Pass > PASS_1))
         LineStoreCurr->list_pos = list_pos;
#endif

    Token_Count = Tokenize( CurrSource, 0, TRUE );

    return( NOT_ERROR );
}

void write_prologue( void )
/*************************/
{
    DefineProc = FALSE;

    /* there are 3 cases:
     * option prologue:NONE           proc_prologue == NULL
     * option prologue:default        *proc_prologue == NULLC
     * option prologue:usermacro      *proc_prologue != NULLC
     */
    if ( ModuleInfo.prologuemode == PEM_DEFAULT ) {
        DebugMsg1(("write_prologue: default prologue\n" ));
        write_default_prologue();
    } else if ( ModuleInfo.prologuemode == PEM_NONE ) {
        DebugMsg1(("write_prologue: prologue is NULL\n" ));
    } else {
        DebugMsg1(("write_prologue: userdefined prologue %s\n", ModuleInfo.proc_prologue ));
        write_userdef_prologue();
    }
    return;
}

static void pop_register( uint_16 *regist )
/*****************************************/
/* Pop the register when a procedure ends */
{
    int cnt;
    if( regist == NULL )
        return;
    cnt = *regist;
    regist += cnt;
    for ( ; cnt; cnt--, regist-- ) {
        /* don't "pop" xmm registers */
        if ( GetValueSp( *regist ) & OP_XMM )
            continue;
        AddLineQueueX( "pop %r", *regist );
    }
}

#if AMD64_SUPPORT

/* Win64 default epilogue if PROC FRAME and OPTION FRAME:AUTO is set
 */

static void write_win64_default_epilogue( proc_info *info )
/*********************************************************/
{
    uint sizexmm;
    uint sizestd;

#if 1
    /* restore non-volatile xmm registers */
    sizexmm = 0;
    sizestd = 0;
    if ( info->regslist ) {
        uint_16 *regist = info->regslist;
        int cnt;
        for( cnt = *regist++; cnt; cnt--, regist++ ) {
            if ( GetValueSp( *regist ) & OP_XMM ) {
                AddLineQueueX( "movdqa %r, [%r+%u]", *regist, stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize + sizexmm );
                sizexmm += 16;
            }
            //else
            //    sizestd += 8;
        }
    }
    sizestd &= 0xF;
#endif

    //sprintf( buffer, "add %s, %d", GetResWName( stackreg[ModuleInfo.Ofssize], NULL ), info->localsize + sizexmm + sizestd );
    AddLineQueueX( "add %r, %d", stackreg[ModuleInfo.Ofssize], NUMQUAL info->localsize + sizexmm );
    pop_register( CurrProc->e.procinfo->regslist );
    AddLineQueueX( "pop %r", basereg[ModuleInfo.Ofssize] );
    return;
}
#endif

/* write default epilogue code
 * if a RET/IRET instruction has been found inside a PROC.

 * epilog code timings
 *
 *                                                  best result
 *              size  86  286  386  486  P      86  286  386  486  P
 * mov sp,bp    2     2   2    2    1    1
 * pop bp       2     8   5    4    4    1
 *             -----------------------------
 *              4     10  7    6    5    2      x             x    x
 *
 * mov esp,ebp  2     -   -    2    1    1
 * pop ebp      2     -   -    4    4    1
 *             -----------------------------
 *              4     -   -    6    5    2                    x    x
 *
 * leave        1     -   5    4    5    3          x    x    x
 *
 * !!!! DECISION !!!!
 *
 * leave will be used for .286 and .386
 * .286 code will be best working on 286,386 and 486 processors
 * .386 code will be best working on 386 and 486 processors
 * .486 code will be best working on 486 and above processors
 *
 *   without LEAVE
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   -2   -2   0    +1
 *  .386   -   -    -2   0    +1
 *  .486   -   -    -    0    +1
 *
 *   LEAVE 286 only
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   0    +2   0    -1
 *  .386   -   -    -2   0    +1
 *  .486   -   -    -    0    +1
 *
 *   LEAVE 286 and 386
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   0    +2   0    -1
 *  .386   -   -    0    0    -1
 *  .486   -   -    -    0    +1
 *
 *   LEAVE 286, 386 and 486
 *
 *         86  286  386  486  P
 *  .8086  0   -2   -2   0    +1
 *  .286   -   0    +2   0    -1
 *  .386   -   -    0    0    -1
 *  .486   -   -    -    0    -1
 */

static void write_default_epilogue( void )
/****************************************/
{
    proc_info   *info;

    info = CurrProc->e.procinfo;

#if AMD64_SUPPORT
    if ( info->isframe ) {
        if ( ModuleInfo.frame_auto )
            write_win64_default_epilogue( info );
        return;
    }
#endif

    /* Pop the registers */
    pop_register( CurrProc->e.procinfo->regslist );

    if ( info->loadds ) {
        AddLineQueueX( "pop %r", T_DS );
    }

    if( ( info->localsize == 0 ) && info->stackparam == FALSE && info->is_vararg == FALSE && info->forceframe == FALSE )
        return;

    if( info->pe_type  ) {
        /* write 80286 and 80386 epilog code */
        /* masm always uses LEAVE if cpu is >= .286 */
        AddLineQueue( "leave" );
    } else  {
        /*
         MOV [E|R]SP, [E|R]BP
         POP [E|R]BP
         */
        if( info->localsize != 0 ) {
            AddLineQueueX( "mov %r, %r", stackreg[ModuleInfo.Ofssize], basereg[ModuleInfo.Ofssize] );
        }
        AddLineQueueX( "pop %r", basereg[ModuleInfo.Ofssize] );
    }
}

/* write userdefined epilogue code
 * if a RET/IRET instruction has been found inside a PROC.
 */
static ret_code write_userdef_epilogue( bool flag_iret )
/******************************************************/
{
    uint_16 *regs;
    char *p;
    bool is_exitm;
    proc_info   *info;
    int flags = CurrProc->sym.langtype; /* set bits 0-2 */
    dir_node * dir;
    char reglst[128];
    char buffer[MAX_LINE_LEN];

    dir = (dir_node *)SymSearch( ModuleInfo.proc_epilogue );
    if (dir == NULL ||
        dir->sym.state != SYM_MACRO ||
        dir->sym.isfunc == TRUE ) {
        AsmErr( EPILOGUE_MUST_BE_MACRO_PROC, ModuleInfo.proc_epilogue );
        return( ERROR );
    }

    info = CurrProc->e.procinfo;

    if ( CurrProc->sym.langtype == LANG_C ||
         CurrProc->sym.langtype == LANG_SYSCALL ||
         CurrProc->sym.langtype == LANG_FASTCALL)
        flags |= 0x10;

    if ( CurrProc->sym.mem_type == MT_FAR)
        flags |= 0x20;

    if ( CurrProc->sym.public == FALSE )
        flags |= 0x40;

    //flags |= CurrProc->sym.export ? 0 : 0x80; /* bit 7: 1 if export */
    flags |= flag_iret ? 0x100 : 0;           /* bit 8: 1 if IRET    */

    p = reglst;
    if ( info->regslist ) {
        int cnt = *info->regslist;
        regs = info->regslist + cnt;
        for ( ; cnt; regs--, cnt-- ) {
            GetResWName( *regs, p );
            p += strlen( p );
            if ( cnt != 1 )
                *p++ = ',';
        }
    }
    *p = NULLC;
    //strcat( reglst, ">" );

    sprintf( buffer,"%s %s, %02XH, %02XH, %02XH, <<%s>>, <%s>", ModuleInfo.proc_epilogue,
             CurrProc->sym.name, flags, info->parasize, info->localsize,
             reglst, info->prologuearg ? info->prologuearg : "" );

    /* if -EP is on, emit "epilogue: none" */
    if ( Options.preprocessor_stdout )
        printf( "option epilogue:none\n" );

    RunMacro( dir, buffer, NULL, TRUE, FALSE, &is_exitm );
    return( NOT_ERROR );
}

/* a RET <nnnn> or IRET/IRETD has occured inside a PROC.
 * count = number of tokens in buffer (=Token_Count)
 * it's ensured already that ModuleInfo.proc_epilogue isn't NULL.
 */
ret_code RetInstr( int i, int count )
/***********************************/
{
    proc_info   *info;
    bool        flag_iret = FALSE;
    char        *p;
#ifdef DEBUG_OUT
    ret_code    rc;
#endif
    char        buffer[MAX_LINE_LEN];

    DebugMsg1(( "RetInstr() enter\n" ));

#if AMD64_SUPPORT
    if( AsmBuffer[i]->value == T_IRET || AsmBuffer[i]->value == T_IRETD || AsmBuffer[i]->value == T_IRETQ )
#else
    if( AsmBuffer[i]->value == T_IRET || AsmBuffer[i]->value == T_IRETD )
#endif
        flag_iret = TRUE;

    if ( ModuleInfo.epiloguemode == PEM_MACRO ) {
#if FASTPASS
        /* don't run userdefined epilogue macro if pass > 1 */
        if ( UseSavedState ) {
            if ( Parse_Pass > PASS_1 ) {
                DebugMsg(( "RetInstr() exit\n" ));
                //return( NOT_ERROR );
                return( ParseLine() );
            }
            /* handle the current line as if it is REPLACED by the macro content */
            *(LineStoreCurr->line) = ';';
        }
#endif
#ifdef DEBUG_OUT
        rc = write_userdef_epilogue( flag_iret );
        DebugMsg(( "RetInstr() exit\n" ));
        return( rc );
#else
        return( write_userdef_epilogue( flag_iret ) );
#endif
    }

    if ( ModuleInfo.list ) {
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );
    }

    strcpy( buffer, AsmBuffer[i]->string_ptr );

    PushLineQueue();

    write_default_epilogue();

    info = CurrProc->e.procinfo;

    /* skip this part for IRET */
    if( flag_iret == FALSE ) {
        if ( CurrProc->sym.mem_type == MT_FAR )
            buffer[3] = 'f';   /* ret -> retf */
        else
            buffer[3] = 'n';     /* ret -> retn */
        buffer[4] = NULLC;
    }
    /* RET without argument? Then calculate the value */
    if( flag_iret == FALSE && count == i + 1 ) {
        if ( ModuleInfo.epiloguemode != PEM_NONE ) {
            switch( CurrProc->sym.langtype ) {
            case LANG_BASIC:
            case LANG_FORTRAN:
            case LANG_PASCAL:
                if( info->parasize != 0 ) {
                    sprintf( buffer + strlen( buffer ), " %d", info->parasize );
                }
                break;
            case LANG_FASTCALL:
                fastcall_tab[Options.fastcall].handlereturn( CurrProc, buffer );
                break;
            case LANG_STDCALL:
                if( !info->is_vararg && info->parasize != 0 ) {
                    sprintf( buffer + strlen( buffer ), " %d", info->parasize );
                }
                break;
            }
        }
    } else {
        /* v2.04: changed. Now works for both RET nn and IRETx */
        p = AsmBuffer[i]->tokpos+1;
        while ( is_valid_id_char( *p ) ) p++;
        strcat( buffer, p );
    }
    AddLineQueue( buffer );
    RunLineQueue();

    DebugMsg1(( "RetInstr() exit\n" ));

    return( NOT_ERROR );
}

/* init this module. called for every pass. */

void ProcInit()
/*************/
{
    ProcStack = NULL;
    CurrProc  = NULL;
    procidx = 1;
    DefineProc = FALSE;
#if AMD64_SUPPORT
    unw_segs_defined = 0;
#endif
}
