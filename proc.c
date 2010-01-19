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
* Description:  Processing of PROC/ENDP/LOCAL/PROTO directives.
*               rewritten for JWasm.
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

/*
 * Masm allows nested procedures
 * but they must NOT have params or locals
 */

/*
 * calling convention FASTCALL supports:
 * - Watcom C: registers e/ax,e/bx,e/cx,e/dx
 * - MS fastcall: registers e/cx,e/dx  (default for 32bit)
 * - Win64: registers rcx, rdx, r8, r9 (default for 64bit)
 */

dir_node                *CurrProc;      // current procedure
int                     procidx;        // procedure index

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

static const char reg_prefix[] = { ' ', 'e', 'r' };
static const char * const watc_regs[] = {"ax", "dx", "bx", "cx" };
static const char * const ms32_regs[] = {"cx", "dx" };
#if AMD64_SUPPORT
//static const char * ms64_regs[] = {"rcx", "rdx", "r8", "r9" };;
static const short NVR[] = {T_RBX, T_RBP, T_RSI, T_RDI, T_R12, T_R13, T_R14, T_R15 };
#endif

struct fastcall_conv {
    int (* paramcheck)( dir_node *, dir_node *, int * );
    void (* handlereturn)( dir_node *, char *buffer );
};

static int watc_pcheck( dir_node *, dir_node *, int * );
static int ms32_pcheck( dir_node *, dir_node *, int * );
static void watc_return( dir_node *, char * );
static void ms32_return( dir_node *, char * );

#if AMD64_SUPPORT
static int ms64_pcheck( dir_node *, dir_node *, int * );
static void ms64_return( dir_node *, char * );
#endif

/* table of fastcall types.
 * must match order of enum fastcall_type!
 * also see table in mangle.c!
 */

static struct fastcall_conv fastcall_tab[] = {
    { ms32_pcheck, ms32_return },
    { watc_pcheck, watc_return },
#if AMD64_SUPPORT
    { ms64_pcheck, ms64_return }
#endif
};

static const char * const basereg[] = {"bp", "ebp", "rbp"};
static const char * const stackreg[] = {"sp", "esp", "rsp"};

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))


/* register usage for OW fastcall (register calling convention).
 * registers are used for parameter size 1,2,4,8.
 * if a parameter doesn't fit in a register, a register pair is used.
 * however, valid register pairs are e/dx:e/ax and e/cx:e/bx only!
 * if a parameter doesn't fit in a register pair, registers
 * are used ax:bx:cx:dx!!!
 * stack cleanup for OW fastcall: if the proc is VARARG, the caller
 * will do the cleanup, else the called proc does it.
 */

static int watc_pcheck( dir_node *proc, dir_node *paranode, int *used )
/*********************************************************************/
{
    if ( ( paranode->sym.first_size == 1 || paranode->sym.first_size == 2 ||
        paranode->sym.first_size == 4 || paranode->sym.first_size == 8) &&
        (proc->e.procinfo->parasize + paranode->sym.first_size) <= (4 * (proc->sym.Ofssize ? 4 : 2 ))) {
        int shft = proc->e.procinfo->parasize >> (1 + proc->sym.Ofssize );
        paranode->sym.state = SYM_TMACRO;
        paranode->sym.string_ptr = AsmAlloc( 16 );
        switch ( paranode->sym.first_size ) {
        case 1:
            strcpy(paranode->sym.string_ptr, watc_regs[shft]);
            *(paranode->sym.string_ptr+1) = 'l';
            break;
        case 2:
            strcpy(paranode->sym.string_ptr, watc_regs[shft]);
            break;
        case 4:
            if ( proc->sym.Ofssize ) {
                *paranode->sym.string_ptr = 'e'; /* use 32bit regs */
                strcpy(paranode->sym.string_ptr+1, watc_regs[shft]);
            } else {
                strcpy(paranode->sym.string_ptr, watc_regs[shft+1]);
                strcat(paranode->sym.string_ptr, "::");
                strcat(paranode->sym.string_ptr, watc_regs[shft]);
            }
            break;
        case 8:
            if ( proc->sym.Ofssize ) {
                *paranode->sym.string_ptr = 'e';
                strcpy(paranode->sym.string_ptr+1, watc_regs[shft+1]);
                strcat(paranode->sym.string_ptr, "::");
                *(paranode->sym.string_ptr+5) = 'e';
                strcpy(paranode->sym.string_ptr+6, watc_regs[shft]);
            } else {
                strcpy(paranode->sym.string_ptr, "ax::bx::cx::dx");
            }
        }
        return(1);
    }
    return(0);
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

/* the MS Win32 fastcall ABI is simple: register ecx and edx are used,
 * if the parameter's value fits into the register.
 * there is no space reserved on the stack for a register backup.
 */

static int ms32_pcheck( dir_node *proc, dir_node *paranode, int *used )
/*********************************************************************/
{
    if ( paranode->sym.first_size > CurrWordSize || *used >= 2 )
        return(0);
    paranode->sym.state = SYM_TMACRO;
    paranode->sym.string_ptr = AsmAlloc( 4 );
    sprintf( paranode->sym.string_ptr, "%c%s", reg_prefix[ModuleInfo.Ofssize], ms32_regs[*used] );
    (*used)++;
    return(1);
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
    return(0);
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

// LOCAL directive. Called on Pass 1 only

ret_code LocalDef( int i )
/************************/
{
    char        *name;
    int         type;
    dir_node    *local;
    dir_node    *curr;
    proc_info   *info;
    //int         size;
    int         idx;
#if AMD64_SUPPORT
    int         displ;
    regs_list   *reg;
#endif
    asm_sym     *symtype;
    uint_8      Ofssize;
    int         align = CurrWordSize;

/*

    LOCAL symbol[,symbol]...
    symbol:name [[count]] [:[type]]
    count: number of array elements, default is 1
    type:  Simple Type, structured type, ptr to simple/structured type

 */
    DebugMsg(("LocalDef(%u) entry\n", i));

    if( DefineProc == FALSE || CurrProc == NULL) {
        AsmError( PROC_MACRO_MUST_PRECEDE_LOCAL );
        return( ERROR );
    }

    info = CurrProc->e.procinfo;
    Ofssize = ModuleInfo.Ofssize;

    i++; /* go past LOCAL */
#if AMD64_SUPPORT
    if ( info->isframe )
        for( reg = info->regslist, displ = 0; reg; reg = reg->next, displ += sizeof(uint_64) );
#endif

    do  {
        if( AsmBuffer[i]->token != T_ID ) {
            AsmError( LABEL_EXPECTED );
            return( ERROR );
        }
        name = AsmBuffer[i]->string_ptr;

        DebugMsg(("LocalDef: %s\n", name ));
#if 0
        /* since v1.95 a local hash table is used. No need to search the
         * symbol before SymLCreate() is called. SymLCreate() will display
         * an error if the symbol is already defined.
         */
        if ((local = (dir_node *)SymSearch( name )) && local->sym.state != SYM_UNDEFINED ) {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, name );
            return( ERROR );
        }
#endif
        local = (dir_node *)SymLCreate( name );
        if( !local ) { /* if it failed, an error msg has been written already */
            DebugMsg(("LocalDef: SymLCreate( %s ) failed\n", name ));
            return( ERROR );
        }

        local->sym.state = SYM_STACK;
        local->sym.defined = TRUE;
        switch ( Ofssize ) {
#if AMD64_SUPPORT
        case USE64: local->sym.mem_type = MT_QWORD; break;
#endif
        case USE32: local->sym.mem_type = MT_DWORD; break;
        default: local->sym.mem_type = MT_WORD;
        }
        local->sym.first_size = align;

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
            for (j = i; j < Token_Count; j++)
                if (AsmBuffer[j]->token == T_COMMA ||
                    AsmBuffer[j]->token == T_COLON)
                    break;
            if ( ERROR == EvalOperand( &i, j, &opndx, TRUE ) )
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
            DebugMsg(("LocalDef: i=%u, token=%X\n", i, AsmBuffer[i]->token ));
            i++;

            type = ERROR;
            if ( AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE ) {
                if (( idx = FindStdType( AsmBuffer[i]->value)) != -1 ) {
                    type = SimpleType[idx].mem_type;
                    if ( SimpleType[idx].Ofssize != USE_EMPTY )
                        Ofssize = SimpleType[idx].Ofssize;
                }
            }
            if( type == ERROR ) {
                if( !(symtype = SymIsType( AsmBuffer[i]->string_ptr ) ) ) {
                    AsmError( INVALID_QUALIFIED_TYPE );
                    return( ERROR );
                }
                type = MT_TYPE;
            }
            /* if a pointer to an arbitrary type is given, an
             anonymous type has to be created
             */
            if ( AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_PTR ) {
                if ( AsmBuffer[i+1]->token != T_FINAL && AsmBuffer[i+1]->token != T_COMMA ) {
                    if (( symtype = CreateTypeDef( "", &i) ) == NULL )
                        return (ERROR);
                    type = MT_TYPE;
                    i--;
                }
                while ( AsmBuffer[i+1]->token != T_FINAL && AsmBuffer[i+1]->token != T_COMMA )
                    i++;
            }

            i++;
            if (type != MT_TYPE) {
                local->sym.mem_type = type;
                local->sym.first_size = SizeFromMemtype( local->sym.mem_type, Ofssize );
            } else {
                local->sym.mem_type = MT_TYPE;
                local->sym.type = symtype;
                local->sym.first_size = symtype->total_size;
                DebugMsg(("LocalDef: type=%s, total_size=%X (curr localsize=%X)\n", symtype->name, symtype->total_size, info->localsize));
            }
        }
        if ( local->sym.isarray )
            local->sym.total_size = local->sym.first_size * local->sym.total_length;
        else
            local->sym.total_size = local->sym.first_size;

        info->localsize += local->sym.total_size;

        if ( local->sym.first_size > align )
            info->localsize = ROUND_UP(info->localsize, align);
        else
            info->localsize = ROUND_UP(info->localsize, local->sym.first_size);
        DebugMsg(("LocalDef: aligned local total=%X\n", info->localsize));

#if AMD64_SUPPORT
        if ( info->isframe )
            local->sym.offset = - ( info->localsize + displ );
        else
#endif
        local->sym.offset = - info->localsize;
        DebugMsg(("LocalDef: symbol offset=%d\n", local->sym.offset));

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

static ret_code ParseParams( dir_node *proc, int i, bool IsInternal )
/*******************************************************************/
{
    char            *token;
#ifdef DEBUG_OUT
    char            *typetoken;
#endif
    unsigned char   Ofssize;
    int             type;
    struct asm_sym  *sym;
    struct asm_sym  *symtype;
    int             cntParam;
    int             offset;
    int             newsize;
    int             oldsize;
    int             ptrpos;
    int             fcint = 0;
    bool            is_ptr;
    bool            is_far;
    bool            is_vararg;
    memtype         mem_type;
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

        symtype = NULL;
        /* read symbol */
        if ( IsInternal ) {
            if (AsmBuffer[i]->token != T_ID) {
                DebugMsg(("ParseParams: name missing/invalid for parameter %u, i=%u\n", cntParam+1, i));
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr ); /* for PROC, parameter needs a name */
                return( ERROR );
            }
            token = AsmBuffer[i++]->string_ptr;

        } else {
            /* for PROTO, a parameter name is optional */
            if( AsmBuffer[i]->token == T_COLON )
                if (paracurr)
                    token = paracurr->sym.name;
                else
                    token = "";
            else {
                token = AsmBuffer[i]->string_ptr;
                i++;
            }
        }

        is_ptr = FALSE;
        /* v2.02: init is_far depending on memory model */
        //is_far = FALSE;
        if ( ModuleInfo.model == MOD_COMPACT ||
            ModuleInfo.model == MOD_LARGE ||
            ModuleInfo.model == MOD_HUGE )
            is_far = TRUE;
        else
            is_far = FALSE;
        is_vararg = FALSE;
        Ofssize = ModuleInfo.Ofssize;
        ptrpos = EMPTY;

        /* read colon (optional for PROC!) */
        if( AsmBuffer[i]->token != T_COLON ) {
            if ( IsInternal ) {
                switch ( ModuleInfo.Ofssize ) {
#if AMD64_SUPPORT
                case USE64: mem_type = MT_QWORD; break;
#endif
                case USE32: mem_type = MT_DWORD; break;
                default: mem_type = MT_WORD;
                }
                i--;
                goto type_is_set;
            }
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        /* allow NEARxx | FARxx [PTR] [<type>] param types */
        if ( AsmBuffer[i]->token == T_RES_ID &&
            AsmBuffer[i]->rm_byte == RWT_TYPE ) {
            if ( SimpleType[AsmBuffer[i]->opcode].mem_type == MT_FAR ) {
                is_far = TRUE;
                ptrpos = i++;
            } else if ( SimpleType[AsmBuffer[i]->opcode].mem_type == MT_NEAR ) {
                is_far = FALSE;
                ptrpos = i++;
            }
        }

        /* now read qualified type */
#ifdef DEBUG_OUT
        typetoken = AsmBuffer[i]->string_ptr;
#endif
        type = ERROR;
        if ( AsmBuffer[i]->token == T_RES_ID )
            if ( AsmBuffer[i]->value == T_PTR && ptrpos != EMPTY )
                type = FindStdType( AsmBuffer[ptrpos]->value );
            else
                type = FindStdType( AsmBuffer[i]->value );
        if (( AsmBuffer[i]->token == T_RES_ID) && (AsmBuffer[i]->value == T_PTR )) {
            is_ptr = TRUE;
            /* a valid syntax is 'name:ptr near' */
            if ( AsmBuffer[i+1]->token == T_RES_ID &&
                ptrpos == EMPTY &&
                AsmBuffer[i+1]->rm_byte == RWT_TYPE ) {
                if ( SimpleType[AsmBuffer[i+1]->opcode].mem_type == MT_FAR ) {
                    type = AsmBuffer[i+1]->opcode;
                    is_far = TRUE;
                    ptrpos = i++;
                    goto no_arbitrary;
                } else if ( SimpleType[AsmBuffer[i+1]->opcode].mem_type == MT_NEAR ) {
                    type = AsmBuffer[i+1]->opcode;
                    is_far = FALSE;
                    ptrpos = i++;
                    goto no_arbitrary;
                }
            }
            /* if a pointer to an arbitrary type is given, an
             anonymous type has to be created
             */
            if (AsmBuffer[i+1]->token != T_FINAL && AsmBuffer[i+1]->token != T_COMMA) {
                if (ptrpos != EMPTY)
                    i = ptrpos;
                if (( IsInternal == TRUE && (symtype = CreateTypeDef("", &i)))) {
                    is_ptr = FALSE;
                    mem_type = symtype->mem_type;
                    i--;
                }
#ifdef DEBUG_OUT
                if ( IsInternal && (symtype == NULL )) {
                    DebugMsg(("ParseParams: CreateTypeDef() failed!\n"));
                }
#endif
                while (AsmBuffer[i+1]->token != T_FINAL && AsmBuffer[i+1]->token != T_COMMA)
                    i++;
                if (symtype)
                    goto type_is_set;
            }
        }
    no_arbitrary:
        DebugMsg(("ParseParams: cntParam=%u, i=%u, token=%s, type=%s\n", cntParam, i, token, typetoken));

        if( type == ERROR ) {
            if ((AsmBuffer[i]->token == T_RES_ID) && (AsmBuffer[i]->value == T_VARARG)) {
                switch( proc->sym.langtype ) {
                case LANG_NONE:
                case LANG_BASIC:
                case LANG_FORTRAN:
                case LANG_PASCAL:
                case LANG_STDCALL:
                    AsmError( VARARG_REQUIRES_C_CALLING_CONVENTION );
                    return( ERROR );
                default:
                    break;
                }
                is_vararg = TRUE;
                mem_type = MT_EMPTY;
            } else {
                if( !(symtype = SymIsType( AsmBuffer[i]->string_ptr ) ) ) {
                    DebugMsg(("ParseParams: type invalid for parameter %u: %s\n", cntParam+1, AsmBuffer[i]->string_ptr ));
                    AsmError( INVALID_QUALIFIED_TYPE );
                    return( ERROR );
                }
//                mem_type = MT_TYPE;
                mem_type = symtype->mem_type;
            }
        } else {
            mem_type = SimpleType[type].mem_type;
            if (SimpleType[type].Ofssize != USE_EMPTY)
                Ofssize = SimpleType[type].Ofssize;
        }
    type_is_set:

        /* check if parameter name is defined already */
        if (( IsInternal ) && (sym = SymSearch( token )) && sym->state != SYM_UNDEFINED) {
            DebugMsg(("ParseParams: %s defined already, state=%u, local=%u\n", sym->name, sym->state, sym->scoped ));
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, token );
            return( ERROR );
        }

        if (symtype)
            newsize = symtype->total_size;
        else if (mem_type == MT_EMPTY) /* ignore VARARG */
            newsize = 0;
        else
            newsize = SizeFromMemtype( mem_type, Ofssize );

        if (paracurr) {
#if 1
            /* check size only (so UINT <-> DWORD wont cause an error) */
            if (paracurr->sym.type)
                oldsize = paracurr->sym.total_size;
            else if (paracurr->sym.mem_type == MT_EMPTY)
                oldsize = 0;
            else
                oldsize = SizeFromMemtype( paracurr->sym.mem_type, Ofssize );
            if (oldsize != newsize) {
                DebugMsg(("ParseParams: old memtype=%u, new memtype=%u\n", paracurr->sym.mem_type, mem_type));
                AsmErr( CONFLICTING_PARAMETER_DEFINITION, token );
                //return( ERROR );
            }
            /* the parameter type used in PROC has highest priority! */
            if ( IsInternal ) {
                if (symtype) {
                    paracurr->sym.type = symtype;
                    paracurr->sym.mem_type = MT_TYPE;
                } else
                    paracurr->sym.mem_type = mem_type;
            }
#else
            if (paracurr->sym->mem_type != mem_type) {
                DebugMsg(("ParseParams: old memtype=%u, new memtype=%u\n", paracurr->sym->mem_type, mem_type));
                AsmErr( CONFLICTING_PARAMETER_DEFINITION, token );
                //return( ERROR );
            }
            if (symtype != NULL)
                if (paracurr->sym->type != symtype) {
                    DebugMsg(("ParseParams: struct param type=%X, symtype=%X\n", paracurr->sym->type, symtype));
                    AsmErr( CONFLICTING_PARAMETER_DEFINITION, token );
                    //return( ERROR );
                }
#endif
            if ( IsInternal ) {
                DebugMsg(("ParseParams: calling SymSetName(%s, %s)\n", paracurr->sym.name, token ));
                SymSetName( &paracurr->sym, token );
            }
            /* set paracurr to next parameter */
            if ( proc->sym.langtype == LANG_C ||
                proc->sym.langtype == LANG_SYSCALL ||
#if AMD64_SUPPORT
                ( proc->sym.langtype == LANG_FASTCALL && Ofssize != USE64 ) ||
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
            if ( IsInternal ) {
                paranode = (dir_node *)SymLCreate( token );
            } else
                paranode = (dir_node *)SymCreate("", FALSE );/* for PROTO, no param name needed */

            if( paranode == NULL ) { /* error msg has been displayed already */
                DebugMsg(("ParseParams: SymLCreate(%s) failed\n", token ));
                return( ERROR );
            }
            paranode->sym.defined = TRUE;
            if ( symtype ) {
                paranode->sym.type = symtype;
                paranode->sym.mem_type = MT_TYPE;
                paranode->sym.first_size = symtype->total_size;
            } else {
                paranode->sym.mem_type = mem_type;
                if (mem_type == MT_EMPTY) /* ignore VARARG */
                    paranode->sym.first_size = 0;
                else
                    paranode->sym.first_size = SizeFromMemtype( mem_type, Ofssize );
            }

            if ( proc->sym.langtype == LANG_FASTCALL &&
                fastcall_tab[Options.fastcall].paramcheck( proc, paranode, &fcint ) ) {
            } else {
                paranode->sym.state = SYM_STACK;
            }
            paranode->is_ptr = is_ptr;
            paranode->is_far = is_far;
            paranode->is_vararg = is_vararg;
            paranode->is32 = Ofssize;

            paranode->sym.total_size = paranode->sym.first_size;

            if( paranode->is_vararg == FALSE )
                proc->e.procinfo->parasize += ROUND_UP( paranode->sym.first_size, CurrWordSize );

            proc->e.procinfo->is_vararg |= paranode->is_vararg;

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
                if ( Ofssize == USE64 )
                    goto left_to_right;
#endif
            default:
                paranode->nextparam = proc->e.procinfo->paralist;
                proc->e.procinfo->paralist = paranode;
                break;
            }
        }
        /* go past type */
        i++;
        if (AsmBuffer[i]->token != T_FINAL) {
            if( AsmBuffer[i]->token != T_COMMA ) {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }
            i++;    /* go past comma */
        }
    } /* end for */

    if ( proc->e.procinfo->init == TRUE ) {
        if (paracurr) {
            /* first definition has more parameters than second */
            DebugMsg(("ParseParams: a param is left over, cntParam=%u\n", cntParam));
            AsmErr( CONFLICTING_PARAMETER_DEFINITION, "" );
            return( ERROR );
        }
    } else {
        int curr;

//        if( proc->e.procinfo->mem_type == MT_NEAR ) {
        if( proc->sym.mem_type == MT_NEAR ) {
            offset = 4;         // offset from BP : return addr + old BP
        } else {
            offset = 6; /* 3 * sizeof(uint_16) */
        }

        if( ModuleInfo.Ofssize == USE32 )
            offset *= 2;
#if AMD64_SUPPORT
        else if( ModuleInfo.Ofssize == USE64 )
            offset *= 4;
#endif

        /* now calculate the (E)BP offsets */

#if AMD64_SUPPORT
        if ( Ofssize == USE64 && proc->sym.langtype == LANG_FASTCALL ) {
            for ( paranode = proc->e.procinfo->paralist; paranode ;paranode = paranode->nextparam )
                if (paranode->sym.state == SYM_TMACRO)
                    ;
                else {
                    paranode->sym.offset = offset;
                    proc->e.procinfo->stackparam = TRUE;
                    offset += ROUND_UP( paranode->sym.first_size, CurrWordSize );
                }
        } else
#endif
        for (;cntParam ;cntParam--) {
            for (curr = 1, paranode = proc->e.procinfo->paralist; curr < cntParam;paranode = paranode->nextparam, curr++ );
            DebugMsg(("ParseParams: parm=%s, ofs=%u, size=%d\n", paranode->sym.name, offset, paranode->sym.first_size));
            if (paranode->sym.state == SYM_TMACRO)
                ;
            else {
                paranode->sym.offset = offset;
                proc->e.procinfo->stackparam = TRUE;
                offset += ROUND_UP( paranode->sym.first_size, CurrWordSize );
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

ret_code ExamineProc( dir_node *proc, int i, bool IsInternal )
/************************************************************/
{
    char            *token;
    regs_list       *regist;
    int             type;
    lang_type       langtype;
    unsigned char   Ofssize = ModuleInfo.Ofssize;
#if FASTPASS
    bool            oldpublic = proc->sym.public;
#endif

    // set some default values

    if ( proc->e.procinfo->init == FALSE ) {
        proc->sym.mem_type = SimpleType[ST_PROC].mem_type;
        proc->sym.Ofssize = Ofssize;
    }

    proc->sym.defined = TRUE;

    if ( IsInternal ) {
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
    if( AsmBuffer[i]->token == T_STRING && IsInternal ) {
        SetMangler( &proc->sym, AsmBuffer[i]->string_ptr, LANG_NONE );
        i++;
    }
#endif

    /* 1. attribute is <distance> */
    if (AsmBuffer[i]->token == T_RES_ID) {
        switch (AsmBuffer[i]->value) {
        case T_FAR:
        case T_NEAR:
        case T_FAR16:
        case T_FAR32:
        case T_NEAR16:
        case T_NEAR32:
            type = FindStdType(AsmBuffer[i]->value);
            if ( IsInternal ) {
                if (( ModuleInfo.Ofssize >= USE32 && SimpleType[type].Ofssize == USE16 ) ||
                    ( ModuleInfo.Ofssize == USE16 && SimpleType[type].Ofssize == USE32 )) {
                    AsmError( DISTANCE_INVALID );
                }
            }
            if (SimpleType[type].Ofssize != USE_EMPTY)
                Ofssize = SimpleType[type].Ofssize;

            if ( proc->e.procinfo->init == TRUE )
                if ( proc->sym.mem_type != SimpleType[type].mem_type ||
                    proc->sym.Ofssize != Ofssize ) {
                    AsmError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
                    break;
                }
            proc->sym.mem_type = SimpleType[type].mem_type;
            proc->sym.Ofssize = Ofssize;
            i++;
            break;
        }
    }

    /* 2. attribute is <langtype> */
    if ( GetLangType( &i, &langtype ) == NOT_ERROR ) {
        if (proc->sym.langtype != LANG_NONE && proc->sym.langtype != langtype )
            AsmError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
        proc->sym.langtype = langtype;
    }

    /* 3. attribute is <visibility> */
    /* note that reserved word PUBLIC is a directive! */
    /* PROTO does NOT accept PUBLIC! */

    if ( AsmBuffer[i]->token == T_ID || AsmBuffer[i]->token == T_DIRECTIVE ) {
        token = AsmBuffer[i]->string_ptr;
        if ( _stricmp(token, "PRIVATE") == 0 ) {
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
        } else if ( IsInternal && (_stricmp(token, "PUBLIC") == 0 )) {
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
    if ( IsInternal && AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
        int idx = Token_Count + 1;
        int max;
        if ( ModuleInfo.proc_prologue == NULL)
            ; // no prologue at all
        else if ( *ModuleInfo.proc_prologue != NULLC ) {
            proc->e.procinfo->prologuearg = AsmAlloc( AsmBuffer[i]->value + 1 );
            strcpy( proc->e.procinfo->prologuearg, AsmBuffer[i]->string_ptr );
        } else {
            /* check the argument. The default prologue
             understands FORCEFRAME and LOADDS only
             */
            max = Tokenize( AsmBuffer[i]->string_ptr, idx );
            for ( ; idx < max; idx++ ) {
                if ( AsmBuffer[idx]->token == T_ID ) {
                    if ( _stricmp( AsmBuffer[idx]->string_ptr, "FORCEFRAME") == 0 ) {
                        proc->e.procinfo->forceframe = TRUE;
                    } else if ( Ofssize != USE64 && (_stricmp( AsmBuffer[idx]->string_ptr, "LOADDS") == 0 ) ) {
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
        IsInternal &&
        AsmBuffer[i]->token == T_RES_ID &&
        AsmBuffer[i]->value == T_FRAME ) {
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
                dir_add_table( (dir_node *)sym );
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
    if ( AsmBuffer[i]->token == T_ID ) {
        if ( _stricmp(AsmBuffer[i]->string_ptr, "USES") == 0) {
            regs_list *lastreg = NULL;
            if ( !IsInternal ) {/* not for PROTO! */
                DebugMsg(("ExamineProc: USES found in PROTO\n"));
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            }
            i++;
             /* at least 1 register name must be present*/
            if ( AsmBuffer[i]->token != T_REG ) {
                DebugMsg(("ExamineProc: no registers for regslist\n"));
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->pos );
            }
            /* read in register names */
            for( ; ( i < Token_Count ) && ( AsmBuffer[i]->token == T_REG ); i++ ) {
                if ( SizeFromRegister(AsmBuffer[i]->value) == 1 ) {
                    AsmError( INVALID_USE_OF_REGISTER );
                }
                regist = AsmAlloc( sizeof( regs_list ) + strlen(AsmBuffer[i]->string_ptr) );
                regist->next = NULL;
                regist->idx = AsmBuffer[i]->value;
                strcpy( regist->reg, AsmBuffer[i]->string_ptr );
                if( lastreg == NULL ) {
                    proc->e.procinfo->regslist = lastreg = regist;
                } else {
                    lastreg->next = regist;
                    lastreg = regist;
                }
            }
        }
    }
    if ( AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE )
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );

    if (AsmBuffer[i]->token == T_COMMA)
        i++;

    /* if no lang type has been set for PROC, use the default one */
    if (proc->sym.langtype == LANG_NONE)
        proc->sym.langtype = ModuleInfo.langtype;

    DebugMsg(("ExamineProc: i=%u, Token_Count=%u, CurrWordSize=%u\n", i, Token_Count, CurrWordSize ));

    /* are there parameters at all? */
    if( i >= Token_Count ) {
        if ( proc->e.procinfo->init == TRUE && proc->e.procinfo->paralist != NULL )
            AsmErr( CONFLICTING_PARAMETER_DEFINITION, "" );
    } else if( proc->sym.langtype == LANG_NONE ) {
        AsmError( LANG_MUST_BE_SPECIFIED );
        return (ERROR);
    } else  {
        if( AsmBuffer[i]->token == T_COMMA )
            i++;
        if ( ERROR == ParseParams(proc, i, IsInternal ) )
            /* do proceed if the parameter scan returns an error */
            ;//return( ERROR );
    }

    proc->e.procinfo->init = TRUE;
    DebugMsg(("ExamineProc: parasize=%u\n", proc->e.procinfo->parasize));

    return( NOT_ERROR );
}

// create a proc item. sym is either NULL or has type SYM_UNDEFINED

asm_sym *CreateProc( asm_sym *sym, const char *name, unsigned char isinternal )
/*****************************************************************************/
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    else
        dir_remove_table( (dir_node *)sym );

    if ( sym ) {
        proc_info *info;
        if ( isinternal )
            sym->state = SYM_INTERNAL;
        else {
            sym->Ofssize = ModuleInfo.Ofssize;
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
            dir_add_table( (dir_node *)sym );
        if ( isinternal ) {
            procidx++;
            if ( Options.line_numbers ) {
                sym->debuginfo = AsmAlloc( sizeof( struct debug_info ) );
                sym->debuginfo->file = get_curr_srcfile();
            }
        }
    }
    return( sym );
}

// delete a PROC item

void DeleteProc( dir_node *dir )
/******************************/
{
    regs_list   *regcurr;
    regs_list   *regnext;
    dir_node    *symcurr;
    dir_node    *symnext;

    /* delete all local symbols ( params, locals, labels ) */
    for( symcurr = dir->e.procinfo->labellist; symcurr; ) {
        asm_sym *symnext2;
        symnext = symcurr->next;
        for ( ; symcurr; ) {
            symnext2 = symcurr->sym.next;
            SymFree( &symcurr->sym );
            symcurr = (dir_node *)symnext2;
        }
        symcurr = symnext;
    }

    for( regcurr = dir->e.procinfo->regslist; regcurr; regcurr = regnext ) {
        regnext = regcurr->next;
        //AsmFree( regcurr->reg );
        AsmFree( regcurr );
    }

    if ( dir->e.procinfo->prologuearg )
        AsmFree( dir->e.procinfo->prologuearg );

    if ( Options.line_numbers && dir->sym.state == SYM_INTERNAL )
        AsmFree( dir->sym.debuginfo );

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

    DebugMsg(("%u. ProcDef enter, curr ofs=%X\n", LineNumber, GetCurrOffset() ));
    if( CurrStruct ) {
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
        return( ERROR );
    }
    if( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
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

#if FASTPASS
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
        SaveState();
    }
#endif

    if ( ModuleInfo.procalign ) {
        AlignCurrOffset( ModuleInfo.procalign );
    }

    i++; /* go past PROC */

    sym = SymSearch( name );

    if( Parse_Pass == PASS_1 ) {

        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = CreateProc( sym, name, TRUE );
        } else if ( sym->state == SYM_EXTERNAL && sym->isproc == TRUE ) {
            if ( Options.line_numbers ) {
                sym->debuginfo = AsmAlloc( sizeof( struct debug_info ) );
                sym->debuginfo->file = get_curr_srcfile();
            }
        } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
            /* additional checks (language type? mem type?) */
            dir_free( (dir_node *)sym, TRUE );
            sym = CreateProc( sym, name, TRUE );
        } else {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, sym->name );
            return( ERROR );
        }
        dir = (dir_node *)sym;

        SetSymSegOfs( sym );

        oldpubstate = sym->public;

        SymClearLocal();

        CurrProc = (dir_node *)sym;

        if( ExamineProc( dir, i, TRUE ) == ERROR ) {
            CurrProc = NULL;
            return( ERROR );
        }

        /* if proto, change to SYM_INTERNAL! */
        if ( sym->state == SYM_EXTERNAL && sym->isproc == TRUE ) {
            dir_internal( dir );
        }

        sym->isproc = TRUE;

        if( sym->public == TRUE && oldpubstate == FALSE )
            AddPublicData( sym );
    } else {
        /**/myassert( sym != NULL );

        procidx++;

        SymSetLocal( sym );

        /* it's necessary to check for a phase error here
         as it is done in LabelCreate() and data_init()!
         */
        ofs = GetCurrOffset();

        if ( ofs != sym->offset) {
#ifdef DEBUG_OUT
            if (!PhaseError)
                DebugMsg(("ProcDef: phase error, pass %u, sym >%s<, old ofs=%X, new ofs=%X\n", Parse_Pass+1, sym->name, sym->offset, ofs ));
#endif
            sym->offset = ofs;
            PhaseError = TRUE;
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

/* PROTO directive.
 * PROTO is virtually an EXTERNDEF for a PROC.
 * there is no segment associated with it, however.
 */

ret_code ProtoDef( int i, const char * name )
/*******************************************/
{
    struct asm_sym      *sym;
    dir_node            *dir;

    /* nothing to do if pass isn't the first one */
    if( Parse_Pass != PASS_1 )
        return( NOT_ERROR );

    /* if name is != NULL, ProtoDef() is called by
     * ExterndefDirective() */
    if ( name == NULL ) {
        if( i != 1 ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        name = AsmBuffer[0]->string_ptr;
    }

    sym = SymSearch( name );

    /* for PROTO, the symbol must be undefined or
     of type PROTO, an external is not allowed */
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        sym = CreateProc( sym, name, FALSE );
    } else if ( sym->isproc == FALSE ) {
        AsmErr( SYMBOL_PREVIOUSLY_DEFINED, sym->name );
        return( ERROR );
    }
    dir = (dir_node *)sym;

    i++; /* go past PROTO */

    /* a PROTO type may be used */
    if ( AsmBuffer[i]->token == T_ID ) {
        dir_node * dir2;
        dir2 = (dir_node *)SymSearch( AsmBuffer[i]->string_ptr );
        if (dir2 && dir2->sym.state == SYM_TYPE && dir2->sym.mem_type == MT_PROC ) {
            dir_node *curr;
            dir_node *newl;
            dir_node *oldl;
            dir2 = (dir_node *)(dir2->e.structinfo->target);
            memcpy(dir->e.procinfo, dir2->e.procinfo, sizeof(proc_info));
            dir->sym.mem_type = dir2->sym.mem_type;
            dir->sym.langtype = dir2->sym.langtype;
#if MANGLERSUPP
            dir->sym.mangler  = dir2->sym.mangler;
#endif
            dir->sym.public   = dir2->sym.public;
            dir->sym.isproc   = TRUE;
            dir->e.procinfo->paralist = NULL;
            for ( curr = dir2->e.procinfo->paralist; curr; curr = curr->nextparam ) {
                newl = AsmAlloc( sizeof(dir_node) );
                memcpy( newl, curr, sizeof(dir_node) );
                newl->nextparam = NULL;
                if (dir->e.procinfo->paralist == NULL)
                    dir->e.procinfo->paralist = newl;
                else {
                    for ( oldl = dir->e.procinfo->paralist; oldl->nextparam; oldl = oldl->nextparam );
                    oldl->nextparam = newl;
                }
            }
            return( NOT_ERROR );
        }
    }
    sym->isproc = TRUE;

    return( ExamineProc( dir, i, FALSE ) );
}

#if AMD64_SUPPORT

/* for FRAME procs, write .pdata and .xdata SEH unwind information */

static void WriteSEHData( dir_node *proc )
/****************************************/
{
    dir_node *xdata;
    int i;
    uint_8 olddotname;
    uint_32 xdataofs = 0;
    char buffer[128];

    if ( endprolog_found == FALSE ) {
        AsmErr( MISSING_ENDPROLOG, proc->sym.name );
    }
    PushLineQueue();
    if ( unw_segs_defined )
        AddLineQueue(".xdata segment");
    else {
        AddLineQueue(".xdata segment align(8) flat readonly 'DATA'");
        AddLineQueue("$xdatasym label near");
    }
    xdataofs = 0;
    xdata = (dir_node *)SymSearch(".xdata");
    if ( xdata )
        xdataofs = xdata->sym.offset;
    /* write the .xdata stuff (a UNWIND_INFO entry ) */
    sprintf( buffer, "db 0%xh + (0%xh shl 3), %u, %u, 0%xh + (0%xh shl 4)",
            unw_info.Version, unw_info.Flags, unw_info.SizeOfProlog,
            unw_info.CountOfCodes, unw_info.FrameRegister, unw_info.FrameOffset );
    AddLineQueue( buffer );
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
    AddLineQueue("align 4");
    if ( proc->e.procinfo->exc_handler ) {
        sprintf( buffer, "dd IMAGEREL %s", proc->e.procinfo->exc_handler->name );
        AddLineQueue( buffer );
        AddLineQueue("align 8");
    }
    AddLineQueue(".xdata ends");
    if ( unw_segs_defined )
        AddLineQueue(".pdata segment");
    else
        AddLineQueue(".pdata segment align(4) flat readonly 'DATA'");
    unw_segs_defined = 1;
    /* write the .pdata stuff ( type IMAGE_RUNTIME_FUNCTION_ENTRY )*/
    sprintf( buffer, "dd IMAGEREL %s, IMAGEREL %s+0%xh, IMAGEREL $xdatasym+0%xh", proc->sym.name, proc->sym.name, proc->sym.total_size, xdataofs );
    AddLineQueue( buffer );
    AddLineQueue(".pdata ends");
    olddotname = ModuleInfo.dotname;
    ModuleInfo.dotname = TRUE; /* set OPTION DOTNAME because .pdata and .xdata */
    RunLineQueue();
    ModuleInfo.dotname = olddotname;
    return;
}
#endif

static void ProcFini( dir_node *proc )
/************************************/
{
    proc->sym.total_size = GetCurrOffset() - proc->sym.offset;
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

// ENDP directive

ret_code EndpDef( int i )
/***********************/
{
    if( CurrStruct ) {
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
        return( ERROR );
    }
    if( i != 1 || AsmBuffer[2]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    if( CurrProc && ( SymCmpFunc(CurrProc->sym.name, AsmBuffer[0]->string_ptr ) == 0 ) ) {
        ProcFini( CurrProc );
    } else {
        AsmErr( UNMATCHED_BLOCK_NESTING, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

#if AMD64_SUPPORT

/* handles directives
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
        if ( ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ) )
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
        if ( AsmBuffer[i]->token != T_REG || !( GetOpndType( AsmBuffer[i]->value, 1 ) & OP_R64) ) {
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
            if ( !( GetOpndType( AsmBuffer[i]->value, 1 ) & OP_XMM ) ) {
                AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
                return( ERROR );
            }
        } else {
            if ( !( GetOpndType( AsmBuffer[i]->value, 1 ) & OP_R64 ) ) {
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
        if ( ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ) )
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
                AsmError( CONSTANT_VALUE_TOO_LARGE );
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

void CheckProcOpen( void )
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
    //regs_list           *regist;
    int                 len;
    proc_info           *info;
    dir_node            *dir;
    //int                 align = CurrWordSize;
    int                 flags = CurrProc->sym.langtype; /* set bits 0-2 */
    regs_list           *regs;
    char                buffer[80];
    char                reglst[64];
    char                retvalue[MAX_LINE_LEN];

#if FASTPASS
    if ( Parse_Pass > PASS_1 && UseSavedState )
        return( NOT_ERROR );
#endif

    info = CurrProc->e.procinfo;
    info->localsize = ROUND_UP( info->localsize, CurrWordSize );

    /* set bit 4 if the caller restores (E)SP */
    if (CurrProc->sym.langtype == LANG_C ||
        CurrProc->sym.langtype == LANG_SYSCALL ||
        CurrProc->sym.langtype == LANG_FASTCALL)
        flags |= 0x10;

    if (CurrProc->sym.mem_type == MT_FAR)
        flags |= 0x20;

    if (CurrProc->sym.public == FALSE)
        flags |= 0x40;

    //flags |= CurrProc->sym.export ? 0 : 0x80; /* bit 7: 1 if export */

    reglst[0] = NULLC;
    for (regs = info->regslist;regs;regs = regs->next) {
        strcat(reglst,regs->reg);
        if (regs->next)
            strcat(reglst,",");
    }

    dir = (dir_node *)SymSearch(ModuleInfo.proc_prologue);
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
    RunMacro(dir, buffer, retvalue, TRUE, TRUE, FALSE);
    DebugMsg(("write_userdef_prologue: macro %s returned >%s<\n", ModuleInfo.proc_prologue, retvalue));

    if (Parse_Pass == PASS_1) {
        dir_node *curr;
        len = atoi(retvalue) - info->localsize;
        for ( curr = info->locallist; curr; curr = curr->nextlocal ) {
            curr->sym.offset -= len;
        }
    }

    Token_Count = Tokenize( CurrSource, 0 );

    return ( NOT_ERROR );
}

#if AMD64_SUPPORT
static ret_code write_win64_default_prologue( proc_info *info )
/*************************************************************/
{
    regs_list           *regist;
    int                 sizestd = 0;
    int                 sizexmm = 0;
    char                buffer[80];

    DebugMsg(("write_win64_default_prologue enter\n"));
    PushLineQueue();

    /*
     * PUSH RBP
     * .PUSHREG RBP
     * MOV  RBP, RSP
     * .SETFRAME RBP,0
     */
    AddLineQueue( "push rbp" );
    AddLineQueue( ".pushreg rbp" );
    AddLineQueue( "mov rbp, rsp" );
    AddLineQueue( ".setframe rbp,0" );

    /* Push the registers */
    if( info->regslist ) {
        for( regist = info->regslist; regist; regist = regist->next ) {
            int i;
            if ( GetOpndType( regist->idx, 1 ) & OP_XMM ) {
                sizexmm += 16;
            } else {
                sizestd += 8;
                strcpy( buffer, "push " );
                strcpy( buffer + 5, regist->reg );
                AddLineQueue( buffer );
                for ( i = 0; i < 8; i++) {
                    if ( regist->idx == NVR[i]) {
                        strcpy( buffer, ".pushreg " );
                        strcpy( buffer + 9, regist->reg );
                        AddLineQueue( buffer );
                        break;
                    }
                }
            }
        } /* end for */
        sizestd &= 0xF;
#if 1
        /* save xmm registers */
        if ( sizexmm ) {
            int i;
            sprintf( buffer, "sub rsp, %d", sizexmm + sizestd );
            AddLineQueue( buffer );
            sprintf( buffer, ".allocstack %d", sizexmm + sizestd );
            AddLineQueue( buffer );
            sizestd = 0;
            for( regist = info->regslist, i = 0; regist; regist = regist->next ) {
                if ( GetOpndType( regist->idx, 1 ) & OP_XMM ) {
                    sprintf( buffer, "movdqa [rsp+%u], %s", i, regist->reg );
                    AddLineQueue( buffer );
                    if ( AsmOpTable[regist->idx].opcode >= 6 )  {
                        sprintf( buffer, ".savexmm128 %s, %u", regist->reg, i );
                        AddLineQueue( buffer );
                    }
                    i += 16;
                }
            }
        }
#endif
    }
    info->localsize = ROUND_UP( info->localsize, CurrWordSize );

    if( info->localsize + sizestd ) {
        /*
         * SUB  RSP, localsize
         * .ALLOCSTACK localsize
         */
        sprintf( buffer, "sub rsp, %d", info->localsize + sizestd );
        AddLineQueue( buffer );
        sprintf( buffer, ".allocstack %d", info->localsize + sizestd );
        AddLineQueue( buffer );
    }

    AddLineQueue( ".endprolog" );

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

    Token_Count = Tokenize( CurrSource, 0 );

    return( NOT_ERROR );
}
#endif

// write PROC prologue
// this is to be done after the LOCAL directives
// and *before* any real instruction

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
    regs_list           *regist;
    int                 len;
    proc_info           *info;
    uint_8              oldlinenumbers;
    //int                 align = CurrWordSize;
    char                buffer[80];

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
        info->regslist == NULL)
        return( NOT_ERROR );

    info->localsize = ROUND_UP( info->localsize, CurrWordSize );
    PushLineQueue();

    if( ( info->localsize != 0 ) || info->stackparam || info->is_vararg || info->forceframe ) {

        /* write 80386 prolog code
         * PUSH [E|R]BP
         * MOV  [E|R]BP, [E|R]SP
         * SUB  [E|R]SP, localsize
         */
        strcpy( buffer, "push ");
        strcpy( buffer+5, basereg[ModuleInfo.Ofssize]);
        AddLineQueue( buffer );

        strcpy( buffer, "mov ");
        strcpy( buffer+4, basereg[ModuleInfo.Ofssize]);
        strcat( buffer, ", ");
        strcat( buffer, stackreg[ModuleInfo.Ofssize]);
        AddLineQueue( buffer );

        if( info->localsize != 0 ) {
            if ( Options.masm_compat_gencode )
                sprintf( buffer, "add %s, %d", stackreg[ModuleInfo.Ofssize], - info->localsize );
            else
                sprintf( buffer, "sub %s, %d", stackreg[ModuleInfo.Ofssize], info->localsize );
            AddLineQueue( buffer );
        }
    }

    if ( info->loadds ) {
        AddLineQueue( "push ds" );
        AddLineQueue( "mov ax, DGROUP" );
        if ( ModuleInfo.Ofssize )
            AddLineQueue( "mov ds, eax" );
        else
            AddLineQueue( "mov ds, ax" );
    }

    /* Push the registers */
    if( info->regslist ) {
        strcpy( buffer, "push " );
        len = strlen( buffer );
        for( regist = info->regslist; regist; regist = regist->next ) {
            strcpy( buffer + len, regist->reg );
            AddLineQueue( buffer );
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

    Token_Count = Tokenize( CurrSource, 0 );

    return( NOT_ERROR );
}

/* proc_check() checks if the prologue code generation is to be triggered.
 * it might return NOT_ERROR or ERROR.
 */
ret_code proc_check( void )
/*************************/
{
    /* some directives are ignored (LOCAL, ORG, ALIGN, LABEL, ENDP!). */
    /* others - i.e. SEGMENT/ENDS - will trigger the code generation */

    if (AsmBuffer[0]->token == T_DIRECTIVE &&
        (AsmBuffer[0]->value == T_LOCAL ||
         AsmBuffer[0]->value == T_EVEN ||
         AsmBuffer[0]->value == T_ALIGN ||
         AsmBuffer[0]->value == T_ORG ||
         AsmBuffer[0]->value == T_ENDM || /* yes! */
         AsmBuffer[0]->value == T_OPTION))
        return( NOT_ERROR );

    if (AsmBuffer[0]->token == T_ID &&
        AsmBuffer[1]->token == T_DIRECTIVE &&
        (AsmBuffer[1]->value == T_LABEL ||
#if FASTPASS
         AsmBuffer[1]->value == T_EQU ||
         AsmBuffer[1]->value == T_SIZESTR ||
         AsmBuffer[1]->value == T_INSTR ||
#endif
         AsmBuffer[1]->value == T_ENDP))
        return( NOT_ERROR );

    DefineProc = FALSE;

    /* masm does also allow data definition directives to occur BEFORE
     the prologue code is inserted. This might be a bug, however, and
     therefore it's not copied.
     */

    /* there are 3 cases:
     option prologue:NONE -> exit quickly
     option prologue:userdefined macro function -> RunMacro()
     option prologue:default
     */
    if ( ModuleInfo.proc_prologue == NULL ) {
        DebugMsg(("proc_check: prologue is NULL\n" ));
        return( NOT_ERROR );
    }
    if (*ModuleInfo.proc_prologue == NULLC ) {
        DebugMsg(("proc_check: default prologue\n" ));
        return( write_default_prologue() ) ;
    }
    DebugMsg(("proc_check: userdefined prologue\n" ));
    return( write_userdef_prologue() );
}

static void pop_register( regs_list *regist )
/*******************************************/
/* Pop the register when a procedure ends */
{
    char        buffer[20];

    if( regist == NULL )
        return;
    pop_register( regist->next );
    /* don't "pop" xmm registers */
    if ( GetOpndType( regist->idx, 1 ) & OP_XMM )
        return;
    strcpy( buffer, "pop " );
    strcpy( buffer + strlen( buffer ), regist->reg );
    AddLineQueue( buffer );
}

#if AMD64_SUPPORT
static void write_win64_default_epilogue( proc_info *info )
/*********************************************************/
{
    regs_list *regist;
    uint sizexmm;
    uint sizestd;
    char buffer[80];

#if 1
    /* restore non-volatile xmm registers */
    sizexmm = 0;
    sizestd = 0;
    for( regist = info->regslist; regist; regist = regist->next ) {
        if ( GetOpndType( regist->idx, 1 ) & OP_XMM ) {
            sprintf( buffer, "movdqa %s, [%s+%u]", regist->reg, stackreg[ModuleInfo.Ofssize], info->localsize + sizexmm );
            AddLineQueue( buffer );
            sizexmm += 16;
        } else
            sizestd += 8;
    }
    sizestd &= 0xF;
#endif

    sprintf( buffer, "add %s, %d", stackreg[ModuleInfo.Ofssize], info->localsize + sizexmm + sizestd );
    AddLineQueue( buffer );
    pop_register( CurrProc->e.procinfo->regslist );
    strcpy( buffer, "pop ");
    strcpy( buffer+4, basereg[ModuleInfo.Ofssize] );
    AddLineQueue( buffer );
    return;
}
#endif

// write default epilogue code
// if a RET/IRET instruction has been found inside a PROC.

// epilog code timmings
//
//                                                  best result
//              size  86  286  386  486  P      86  286  386  486  P
// mov sp,bp    2     2   2    2    1    1
// pop bp       2     8   5    4    4    1
//             -----------------------------
//              4     10  7    6    5    2      x             x    x
//
// mov esp,ebp  2     -   -    2    1    1
// pop ebp      2     -   -    4    4    1
//             -----------------------------
//              4     -   -    6    5    2                    x    x
//
// leave        1     -   5    4    5    3          x    x    x
//
// !!!! DECISION !!!!
//
// leave will be used for .286 and .386
// .286 code will be best working on 286,386 and 486 processors
// .386 code will be best working on 386 and 486 processors
// .486 code will be best working on 486 and above processors
//
//   without LEAVE
//
//         86  286  386  486  P
//  .8086  0   -2   -2   0    +1
//  .286   -   -2   -2   0    +1
//  .386   -   -    -2   0    +1
//  .486   -   -    -    0    +1
//
//   LEAVE 286 only
//
//         86  286  386  486  P
//  .8086  0   -2   -2   0    +1
//  .286   -   0    +2   0    -1
//  .386   -   -    -2   0    +1
//  .486   -   -    -    0    +1
//
//   LEAVE 286 and 386
//
//         86  286  386  486  P
//  .8086  0   -2   -2   0    +1
//  .286   -   0    +2   0    -1
//  .386   -   -    0    0    -1
//  .486   -   -    -    0    +1
//
//   LEAVE 286, 386 and 486
//
//         86  286  386  486  P
//  .8086  0   -2   -2   0    +1
//  .286   -   0    +2   0    -1
//  .386   -   -    0    0    -1
//  .486   -   -    -    0    -1
//

static void write_default_epilogue( void )
/****************************************/
{
    proc_info   *info;
    char        buffer[80];

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
        AddLineQueue( "pop ds" );
    }

    if( ( info->localsize == 0 ) && info->stackparam == FALSE && info->is_vararg == FALSE && info->forceframe == FALSE )
        return;

    if( info->pe_type  ) {
        /* write 80286 and 80386 epilog code */
        /* masm always uses LEAVE if cpu is >= .286 */
        strcpy( buffer, "leave" );
    } else  {
        /*
         MOV [E|R]SP, [E|R]BP
         POP [E|R]BP
         */
        if( info->localsize != 0 ) {
            sprintf( buffer, "mov %s, %s", stackreg[ModuleInfo.Ofssize], basereg[ModuleInfo.Ofssize] );
            AddLineQueue( buffer );
        }
        strcpy( buffer, "pop ");
        strcpy( buffer+4, basereg[ModuleInfo.Ofssize] );
    }
    AddLineQueue( buffer );
}

// write userdefined epilogue code
// if a RET/IRET instruction has been found inside a PROC.

static ret_code write_userdef_epilogue( bool flag_iret )
/******************************************************/
{
    regs_list * regs;
    proc_info   *info;
    int flags = CurrProc->sym.langtype; /* set bits 0-2 */
    dir_node * dir;
    char reglst[64];
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

    reglst[0] = NULLC;
    if ( info->regslist ) {
        for ( regs = info->regslist; regs->next; regs = regs->next );
        while (1) {
            regs_list * regs2;
            strcat(reglst, regs->reg);
            if (regs == info->regslist)
                break;
            strcat( reglst, "," );
            for ( regs2 = info->regslist;regs2->next != regs; regs2 = regs2->next );
            regs = regs2;
        }
    }
    //strcat( reglst, ">" );

    sprintf( buffer,"%s %s, %02XH, %02XH, %02XH, <<%s>>, <%s>", ModuleInfo.proc_epilogue,
             CurrProc->sym.name, flags, info->parasize, info->localsize,
             reglst, info->prologuearg ? info->prologuearg : "" );

    /* if -EP is on, emit "epilogue: none" */
    if ( Options.preprocessor_stdout )
        printf( "option epilogue:none\n" );

    RunMacro(dir, buffer, NULL, TRUE, TRUE, FALSE);
    return( NOT_ERROR );
}

// a RET <nnnn> or IRET/IRETD has occured inside a PROC.
// count = number of tokens in buffer (=Token_Count)
// it's ensured already that ModuleInfo.proc_epilogue isn't NULL.

ret_code RetInstr( int i, int count )
/***********************************/
{
    proc_info   *info;
    expr_list   opndx;
    bool        flag_iret = FALSE;
#ifdef DEBUG_OUT
    ret_code    rc;
#endif
    char        buffer[MAX_LINE_LEN];

    DebugMsg(( "RetInstr() enter\n" ));

#if AMD64_SUPPORT
    if( AsmBuffer[i]->value == T_IRET || AsmBuffer[i]->value == T_IRETD || AsmBuffer[i]->value == T_IRETQ )
#else
    if( AsmBuffer[i]->value == T_IRET || AsmBuffer[i]->value == T_IRETD )
#endif
        flag_iret = TRUE;

    if ( *ModuleInfo.proc_epilogue != NULLC ) {
#if FASTPASS
        /* don't run userdefined epilogue macro if pass > 1 */
        if ( UseSavedState ) {
            if ( Parse_Pass > PASS_1 ) {
                DebugMsg(( "RetInstr() exit\n" ));
                //return( NOT_ERROR );
                return( ParseItems() );
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

        if ( CurrProc->sym.mem_type == MT_FAR)
            strcpy( buffer+3, "f" );  /* ret -> retf */
        else
            strcpy( buffer+3, "n" );  /* ret -> retn */

        /* is there an argument for RET? */
        if( count == i + 1 ) {
            if ( ModuleInfo.proc_epilogue ) {
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
                default:
                    break;
                }
            }
        } else {
            ++i;
            /* get the numeric RET xx argument */
            if( EvalOperand( &i, count, &opndx, TRUE ) == ERROR)
                opndx.value = 0;
            if (opndx.kind != EXPR_CONST || opndx.string != NULL ) {
                AsmError( CONSTANT_EXPECTED );
                opndx.value = 0;
            }
            sprintf( buffer + strlen( buffer ), " %d", opndx.value );
        }
    }
    AddLineQueue( buffer );
    RunLineQueue();

    DebugMsg(( "RetInstr() exit\n" ));

    return( NOT_ERROR );
}

// init this module. called for every pass.

void ProcInit()
/*************/
{
    ProcStack = NULL;
    CurrProc  = NULL;
    procidx = 1;
    DefineProc = FALSE;
    ModuleInfo.proc_prologue = "";
    ModuleInfo.proc_epilogue = "";
#if AMD64_SUPPORT
    unw_segs_defined = 0;
#endif
}
