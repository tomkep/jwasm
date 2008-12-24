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
* rewritten for JWasm.
*
****************************************************************************/


#include "globals.h"
#include <ctype.h>

#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"
#include "segment.h"
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
#include "macro.h"
#include "proc.h"
#include "fastpass.h"
#include "listing.h"
#include "posndir.h"

#include "myassert.h"

/*
 Masm allows nested procedures
 but they must NOT have params or locals
 */

dir_node                *CurrProc;      // current procedure
static proc_info        *ProcStack;

bool                    in_epilogue;
bool                    DefineProc;     // TRUE if the definition of procedure
                                        // has not ended
static char * watc_regs[] = {"eax", "ebx", "ecx", "edx"};

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

static void push_proc( dir_node *proc )
/*************************************/
{
    push( &ProcStack, proc );
    return;
}

static dir_node *pop_proc( void )
/*******************************/
{
    if( ProcStack == NULL )
        return( NULL );
    return( (dir_node *)pop( &ProcStack ) );
}

// LOCAL directive. Called on Pass 1 only

ret_code LocalDef( int i )
/*******************/
{
    char        *string;
    int         type;
    local_sym   *local;
    local_sym   *curr;
    proc_info   *info;
    int         size;
    int         idx;
    //struct asm_sym      *sym;
    struct asm_sym      *symtype;
    int         align = CurrWordSize;

/*

    LOCAL symbol[,symbol]...
    symbol:name [[count]] [:[type]]
    count: number of array elements, default is 1
    type:  Simple Type, structured type, ptr to simple/structured type

 */
    DebugMsg(("LocalDef(%u) entry\n", i));

    if( DefineProc == FALSE || CurrProc == NULL) {
        AsmError( LOCAL_VAR_MUST_FOLLOW_PROC );
        return( ERROR );
    }

    info = CurrProc->e.procinfo;

    for( i++; i< Token_Count; i++ ) {
        string = AsmBuffer[i]->string_ptr;

        if( AsmBuffer[i]->token != T_ID ) {
            AsmError( LABEL_EXPECTED );
            return( ERROR );
        }
        DebugMsg(("LocalDef: %s\n", string));

        if ((local = (local_sym *)SymSearch( string )) && local->sym.state != SYM_UNDEFINED ) {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        local = (local_sym *)SymCreate(string, FALSE);
        if( !local )
            return( ERROR );

        local->sym.state = SYM_STACK;
        local->sym.defined = TRUE;
        local->sym.mem_type = Use32 ? MT_DWORD : MT_WORD;

        i++;

        local->sym.first_size = align;
        local->sym.next = NULL;
        local->is_ptr = 0;
        local->is_far = 0;

        /* get an optional index factor: local name[xx]:... */
        if(( i < Token_Count ) && (AsmBuffer[i]->token == T_OP_SQ_BRACKET)) {
            expr_list expr;
            i++;
            if (ERROR == EvalOperand( &i, Token_Count, &expr, TRUE ))
                return(ERROR);
            if (expr.type != EXPR_CONST || expr.string != NULL) {
                AsmError( CONSTANT_EXPECTED );
                return(ERROR);
            }
            // local->factor = AsmBuffer[i++]->value;
            /* zero is allowed as value! */
//            local->sym.count = expr.value;
            local->sym.total_length = expr.value;
            if( ( AsmBuffer[i]->token != T_CL_SQ_BRACKET ) || ( i >= Token_Count ) ) {
                AsmError( EXPECTED_CL_SQ_BRACKET );
                return( ERROR );
            }
            i++;
        }

        /* get the optional type: local name[xx]:type  */
        if( (i < Token_Count) && (AsmBuffer[i]->token == T_COLON)) {
            DebugMsg(("LocalDef: i=%u, token=%X\n", i, AsmBuffer[i]->token));
            i++;

            type = ERROR;
            if (AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE) {
                if (( idx = FindSimpleType( AsmBuffer[i]->value)) != -1 )
                    type = SimpleType[idx].mem_type;
            }
            if( type == ERROR ) {
                if( !(symtype = IsLabelType( AsmBuffer[i]->string_ptr ) ) ) {
                    AsmError( INVALID_QUALIFIED_TYPE );
                    return( ERROR );
                }
                type = MT_TYPE;
            }
            /* if a pointer to an arbitrary type is given, an
             anonymous type has to be created
             */
            if (AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_PTR) {
                if (AsmBuffer[i+1]->token != T_FINAL && AsmBuffer[i+1]->token != T_COMMA) {
                    if ((symtype = CreateTypeDef("", &i)) == NULL)
                        return (ERROR);
                    type = MT_TYPE;
                    i--;
                }
                while (AsmBuffer[i+1]->token != T_FINAL && AsmBuffer[i+1]->token != T_COMMA)
                    i++;
            }

            i++;
            if (type != MT_TYPE) {
                local->sym.mem_type = type;
                local->sym.first_size = SizeFromMemtype( local->sym.mem_type, Use32 );
            } else {
                local->sym.mem_type = MT_TYPE;
                local->sym.type = symtype;
                local->sym.first_size = symtype->total_size;
                DebugMsg(("LocalDef: type=%s, total_size=%X (curr localsize=%X)\n", symtype->name, symtype->total_size, info->localsize));
            }
        }
        if (local->sym.total_length)
            local->sym.total_size = local->sym.first_size * local->sym.total_length;
        else
            local->sym.total_size = local->sym.first_size;

        info->localsize += local->sym.total_size;

        if (local->sym.first_size > align)
            info->localsize = ROUND_UP(info->localsize, align);
        else
            info->localsize = ROUND_UP(info->localsize, local->sym.first_size);
        DebugMsg(("LocalDef: aligned local total=%X\n", info->localsize));

        local->sym.offset = - info->localsize;
        DebugMsg(("LocalDef: symbol offset=%d\n", local->sym.offset));

        if( info->locallist == NULL ) {
            info->locallist = local;
        } else {
            for( curr = info->locallist;; curr = (local_sym *)curr->sym.next ) {
                if( curr->sym.next == NULL ) {
                    break;
                }
            }
            curr->sym.next = (asm_sym *)local;
        }

        if( ( i < Token_Count ) && ( AsmBuffer[i]->token != T_COMMA ) ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }

    }
    return( NOT_ERROR );
}

// parse parameters of a PROC/PROTO

static ret_code ParseParams(dir_node *proc, int i, bool bDefine)
{
    char            *token;
    char            *typetoken;
    int             type;
    struct asm_sym  *sym;
    struct asm_sym  *symtype;
    int             cntParam;
    int             offset;
    int             newsize;
    int             oldsize;
    int             ptrpos;
    bool            is_ptr;
    bool            is_far;
    bool            is32;
    bool            is_vararg;
    memtype         mem_type;
    local_sym       *paranode;
    local_sym       *paracurr;

    /* parse PROC parms */
    /* it's important to remember that params are stored in "push" order! */

    if (proc->sym.langtype == LANG_C ||
        proc->sym.langtype == LANG_SYSCALL ||
        proc->sym.langtype == LANG_WATCOM_C ||
        proc->sym.langtype == LANG_STDCALL)
        for (paracurr = proc->e.procinfo->paralist; paracurr && paracurr->sym.next; paracurr = (local_sym *)paracurr->sym.next );
    else
        paracurr = proc->e.procinfo->paralist;

    for( cntParam = 0 ; AsmBuffer[i]->token != T_FINAL ; cntParam++ ) {

        symtype = NULL;
        /* read symbol */
        if (bDefine) {
            if (AsmBuffer[i]->token != T_ID) {
                DebugMsg(("ParseParams: name missing/invalid for parameter %u, i=%u\n", cntParam+1, i));
                AsmError( SYNTAX_ERROR ); /* for PROC, parameter needs a name */
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
        is_far = FALSE;
        is_vararg = FALSE;
        is32 = Use32;
        ptrpos = EMPTY;

        /* read colon (optional for PROC!) */
        if( AsmBuffer[i]->token != T_COLON ) {
            if (bDefine) {
                mem_type = Use32 ? MT_DWORD : MT_WORD;
                i--;
                goto type_is_set;
            }
            AsmError( COLON_EXPECTED );
            return( ERROR );
        }
        i++;

        /* allow NEARxx | FARxx [PTR] [<type>] param types */
        if (AsmBuffer[i]->token == T_RES_ID)
            switch(AsmBuffer[i]->value) {
            case T_FAR:
            case T_FAR16:
            case T_FAR32:
                is_far = TRUE;
            case T_NEAR:
            case T_NEAR16:
            case T_NEAR32:
                ptrpos = i++;
            }

        /* now read qualified type */
        typetoken = AsmBuffer[i]->string_ptr;
        type = ERROR;
        if (AsmBuffer[i]->token == T_RES_ID)
            if (AsmBuffer[i]->value == T_PTR && ptrpos != EMPTY)
                type = FindSimpleType( AsmBuffer[ptrpos]->value );
            else
                type = FindSimpleType( AsmBuffer[i]->value );
        if (( AsmBuffer[i]->token == T_RES_ID) && (AsmBuffer[i]->value == T_PTR )) {
            is_ptr = TRUE;
            /* a valid syntax is 'name:ptr near' */
            if (AsmBuffer[i+1]->token == T_RES_ID && ptrpos == EMPTY) {
                switch (AsmBuffer[i+1]->value) {
                case T_FAR:
                case T_FAR16:
                case T_FAR32:
                    is_far = TRUE;
                case T_NEAR:
                case T_NEAR16:
                case T_NEAR32:
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
                if ((bDefine == TRUE && (symtype = CreateTypeDef("", &i)))) {
                    is_ptr = FALSE;
                    mem_type = symtype->mem_type;
                    i--;
                }
                while (AsmBuffer[i+1]->token != T_FINAL && AsmBuffer[i+1]->token != T_COMMA)
                    i++;
                if (symtype)
                    goto type_is_set;
            }
        }
    no_arbitrary:
        DebugMsg(("ParseParams: cnpParam=%u, i=%u, token=%s, type=%s\n", cntParam, i, token, typetoken));

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
                if( !(symtype = IsLabelType( AsmBuffer[i]->string_ptr ) ) ) {
                    DebugMsg(("ParseParams: type invalid for parameter %u\n", cntParam+1));
                    AsmError( INVALID_QUALIFIED_TYPE );
                    return( ERROR );
                }
//                mem_type = MT_TYPE;
                mem_type = symtype->mem_type;
            }
        } else {
            mem_type = SimpleType[type].mem_type;
            if (SimpleType[type].ofs_size != OFSSIZE_EMPTY)
                is32 = (SimpleType[type].ofs_size == OFSSIZE_32);
        }
    type_is_set:

        /* check if parameter name is defined already */
        if ((bDefine) && (sym = SymSearch( token )) && sym->state != SYM_UNDEFINED) {
             AsmErr( SYMBOL_PREVIOUSLY_DEFINED, token );
             return( ERROR );
        }

        if (symtype)
            newsize = symtype->total_size;
        else if (mem_type == MT_EMPTY) /* ignore VARARG */
            newsize = 0;
        else
            newsize = SizeFromMemtype( mem_type, Use32);

        if (paracurr) {
#if 1
            /* check size only (so UINT <-> DWORD wont cause an error) */
            if (paracurr->sym.type)
                oldsize = paracurr->sym.total_size;
            else if (paracurr->sym.mem_type == MT_EMPTY)
                oldsize = 0;
            else
                oldsize = SizeFromMemtype( paracurr->sym.mem_type, Use32 );
            if (oldsize != newsize) {
                DebugMsg(("ParseParams: old memtype=%u, new memtype=%u\n", paracurr->sym.mem_type, mem_type));
                AsmErr( CONFLICTING_PARAMETER_DEFINITION, token );
                //return( ERROR );
            }
            /* the parameter type used in PROC has highest priority! */
            if (bDefine) {
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
            if (bDefine) {
                SymSetName( &paracurr->sym, token );
            }
            /* set paracurr to next parameter */
            if (proc->sym.langtype == LANG_C ||
                proc->sym.langtype == LANG_SYSCALL ||
                proc->sym.langtype == LANG_WATCOM_C ||
                proc->sym.langtype == LANG_STDCALL) {
                local_sym *l;
                for (l = proc->e.procinfo->paralist;
                     l && ((local_sym *)l->sym.next != paracurr);
                     l = (local_sym *)l->sym.next);
                paracurr = l;
            } else
                paracurr = (local_sym *)paracurr->sym.next;

        } else if (proc->e.procinfo->init == TRUE) {
            /* second definition has more parameters than first */
            DebugMsg(("ParseParams: different param count\n"));
            AsmErr( CONFLICTING_PARAMETER_DEFINITION, "" );
            return( ERROR );
        } else {
            if (bDefine)
                paranode = (local_sym *)SymCreate(token, FALSE);
            else
                paranode = (local_sym *)SymCreate("", FALSE);/* for PROTO, no param name needed */

            if( paranode == NULL )
                return( ERROR ); /* shouldn't happen */

            paranode->sym.defined = TRUE;
            if (proc->sym.langtype == LANG_WATCOM_C && proc->e.procinfo->parasize < 16) {
                paranode->sym.state = SYM_TMACRO;
                paranode->sym.string_ptr = AsmAlloc(4);
                strcpy( paranode->sym.string_ptr, watc_regs[proc->e.procinfo->parasize >> 2]);
            } else {
                paranode->sym.state = SYM_STACK;
                if (symtype) {
                    paranode->sym.type = symtype;
                    paranode->sym.mem_type = MT_TYPE;
                } else
                    paranode->sym.mem_type = mem_type;
            }
            paranode->is_ptr = is_ptr;
            paranode->is_far = is_far;
            paranode->is_vararg = is_vararg;
            paranode->is32 = is32;

            if (symtype)
                paranode->sym.first_size = symtype->total_size;
            else if (mem_type == MT_EMPTY) /* ignore VARARG */
                paranode->sym.first_size = 0;
            else
                paranode->sym.first_size = SizeFromMemtype( mem_type, Use32 );

            paranode->sym.total_size = paranode->sym.first_size;

            if( paranode->is_vararg )
                ;
            else if( Use32 ) {
                proc->e.procinfo->parasize += ROUND_UP( paranode->sym.first_size, 4 );
            } else {
                proc->e.procinfo->parasize += ROUND_UP( paranode->sym.first_size, 2 );
            }

            proc->e.procinfo->is_vararg |= paranode->is_vararg;

            /* Parameters are stored in "push" order */

            switch( proc->sym.langtype ) {
            case LANG_BASIC:
            case LANG_FORTRAN:
            case LANG_PASCAL:
                paranode->sym.next = NULL;
                if( proc->e.procinfo->paralist == NULL ) {
                    proc->e.procinfo->paralist = paranode;
                } else {
                    for( paracurr = proc->e.procinfo->paralist;; paracurr = (local_sym *)paracurr->sym.next ) {
                        if( paracurr->sym.next == NULL ) {
                            break;
                        }
                    }
                    paracurr->sym.next = (asm_sym *)paranode;
                    paracurr = NULL;
                }
                break;
            default:
                paranode->sym.next = (asm_sym *)proc->e.procinfo->paralist;
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

    if (proc->e.procinfo->init == TRUE) {
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
            offset = 6;
        }

        if( Use32 )
            offset *= 2;

        /* now calculate the (E)BP offsets */

        for (;cntParam ;cntParam--) {
            for (curr = 1,paranode = proc->e.procinfo->paralist;curr < cntParam;paranode = (local_sym *)paranode->sym.next, curr++);
            DebugMsg(("ParseParams: parm=%s, ofs=%u, size=%d\n", paranode->sym.name, offset, paranode->sym.first_size));
            if (paranode->sym.state == SYM_TMACRO)
                ;
            else {
                paranode->sym.offset = offset;
                offset += ROUND_UP( paranode->sym.first_size, CurrWordSize );
            }
        }
    }
    return (NOT_ERROR);
}

/*
 create a PROC type
 i = position of attributes
 bDefine = TRUE for PROC, FALSE for PROTO
 strategy to set default value for "offset size" (16/32):
 1. if current model is FLAT, use 32, else
 2. use the current segment's attribute
 3. if no segment is set, use cpu setting
 */

ret_code ExamineProc( dir_node *proc, int i, bool IsProc )
/*******************************************/
{
    char            *token;
    regs_list       *regist;
    regs_list       *temp_regist;
    int             type;
    int             type_comp;
    lang_type       langtype;
    bool            is32 = Use32;
    bool            visibility=FALSE;

    /* ignore current segment for PROTOs and model FLAT */
    if (ModuleInfo.model == MOD_FLAT && IsProc == FALSE)
        is32 = TRUE;

    // set some default values

    if (proc->e.procinfo->init == FALSE) {
        proc->sym.mem_type = IS_PROC_FAR() ? MT_FAR : MT_NEAR;
        proc->sym.use32 = is32;
    }

    proc->sym.defined = TRUE;

    if ( IsProc ) {
        proc->e.procinfo->export = ModuleInfo.procs_export;
        proc->sym.public = ~ModuleInfo.procs_private;
        proc->e.procinfo->pe_type = ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_286 ) || ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) == P_386 );
    }

#if MANGLERSUPP
    /* OW name mangling */
    if( AsmBuffer[i]->token == T_STRING && IsProc ) {
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
            type = FindSimpleType(AsmBuffer[i]->value);
            if ( IsProc ) {
                if ((Use32 == TRUE  && SimpleType[type].ofs_size == OFSSIZE_16) ||
                    (Use32 == FALSE && SimpleType[type].ofs_size == OFSSIZE_32)) {
                    AsmError(DISTANCE_INVALID);
                }
            }
            if (SimpleType[type].ofs_size != OFSSIZE_EMPTY)
                is32 = (SimpleType[type].ofs_size == OFSSIZE_32);

            if (proc->e.procinfo->init == TRUE)
                if (proc->sym.mem_type != SimpleType[type].mem_type ||
                    proc->sym.use32 != is32) {
                    AsmError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
                    break;
                }
            proc->sym.mem_type = SimpleType[type].mem_type;
            proc->sym.use32 = is32;
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

    if ( AsmBuffer[i]->token == T_ID || AsmBuffer[i]->token == T_DIRECTIVE ) {
        token = AsmBuffer[i]->string_ptr;
        if (stricmp(token, "PRIVATE") == 0) {
            proc->sym.public = FALSE;
            proc->e.procinfo->export = FALSE;
            i++;
        } else if (stricmp(token, "PUBLIC") == 0) {
            proc->sym.public = TRUE;
            proc->e.procinfo->export = FALSE;
            i++;
        } else if (stricmp(token, "EXPORT") == 0) {
            proc->sym.public = TRUE;
            proc->e.procinfo->export = TRUE;
            i++;
        }
    }

    /* 4. attribute is <prologuearg>, for PROC only.
     it must be enclosed in <> */
    if ( IsProc && AsmBuffer[i]->token == T_STRING && AsmBuffer[i]->string_delim == '<' ) {
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
                    if ( stricmp( AsmBuffer[idx]->string_ptr, "FORCEFRAME") == 0 ) {
                        proc->e.procinfo->forceframe = TRUE;
                    } else if ( stricmp( AsmBuffer[idx]->string_ptr, "LOADDS") == 0 ) {
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

    if (AsmBuffer[i]->token == T_ID) {
        if (stricmp(AsmBuffer[i]->string_ptr, "USES") == 0) {
            if ( !IsProc ) {/* not for PROTO! */
                DebugMsg(("ExamineProc: USES found in PROTO\n"));
                AsmError( SYNTAX_ERROR );
            }
            /* check for register name */
            for( i++; ( i < Token_Count ) && ( AsmBuffer[i]->token == T_REG ); i++ ) {
                if ( SizeFromRegister(AsmBuffer[i]->value) == 1 ) {
                    AsmError( INVALID_USE_OF_REGISTER );
                }
                token = AsmBuffer[i]->string_ptr;
                regist = AsmAlloc( sizeof( regs_list ));
                regist->next = NULL;
                regist->reg = AsmAlloc( strlen(token) + 1 );
                strcpy( regist->reg, token );
                if( proc->e.procinfo->regslist == NULL ) {
                    proc->e.procinfo->regslist = regist;
                } else {
                    for( temp_regist = proc->e.procinfo->regslist;;
                         temp_regist = temp_regist->next ) {
                        if( temp_regist->next == NULL ) {
                            break;
                        }
                    }
                    temp_regist->next = regist;
                }
            }
            if (proc->e.procinfo->regslist == NULL) {
                DebugMsg(("ExamineProc: regslist is NULL\n"));
                AsmError( SYNTAX_ERROR );
            }
        }
    }
    if (AsmBuffer[i]->token == T_RES_ID || AsmBuffer[i]->token == T_DIRECTIVE)
        AsmError(SYNTAX_ERROR);

    if (AsmBuffer[i]->token == T_COMMA)
        i++;

    /* if no lang type has been set for PROC, use the default one */
    if (proc->sym.langtype == LANG_NONE)
        proc->sym.langtype = ModuleInfo.langtype;

    DebugMsg(("ExamineProc: i=%u, Token_Count=%u\n", i, Token_Count));

    /* are there parameters at all? */
    if( i >= Token_Count ) {
        if (proc->e.procinfo->init == TRUE && proc->e.procinfo->paralist != NULL)
            AsmErr( CONFLICTING_PARAMETER_DEFINITION, "" );
    } else if( proc->sym.langtype == LANG_NONE ) {
        AsmError( LANG_MUST_BE_SPECIFIED );
        return (ERROR);
    } else  {
        if( AsmBuffer[i]->token == T_COMMA )
            i++;
        if (ERROR == ParseParams(proc, i, IsProc ))
            /* do proceed if the parameter scan returns an error */
            ;//return(ERROR);
    }

    proc->e.procinfo->init = TRUE;
    DebugMsg(("ExamineProc: parasize=%u\n", proc->e.procinfo->parasize));

    return( NOT_ERROR );
}

// create a proc item

asm_sym *CreateProc( asm_sym *sym, char *name )
{
    if ( sym == NULL )
        sym = SymCreate( name, *name != NULLC );
    if ( sym ) {
        proc_info *info;
        sym->state = SYM_PROC;
        info = AsmAlloc( sizeof( proc_info ) );
        ((dir_node *)sym)->e.procinfo = info;
        info->regslist = NULL;
        info->paralist = NULL;
        info->locallist = NULL;
        info->labellist = NULL;
        info->parasize = 0;
        info->localsize = 0;
        info->prologuearg = NULL;
        info->is_vararg = FALSE;
        info->pe_type = FALSE;
        info->export = FALSE;
        info->init = FALSE;
        info->forceframe = FALSE;
        info->loadds = FALSE;
        if ( *(sym->name) )
            dir_add( (dir_node *)sym );
    }
    return( sym );
}

// delete a proc item

void DeleteProc( dir_node *dir )
{
    local_sym   *localcurr;
    local_sym   *localnext;
    regs_list   *regcurr;
    regs_list   *regnext;
    asm_sym     *symcurr;
    asm_sym     *symnext;

    for( localcurr = dir->e.procinfo->paralist ;localcurr; ) {
        localnext = (local_sym *)localcurr->sym.next;
        SymFree( &localcurr->sym );
        localcurr = localnext;
    }

    for( localcurr = dir->e.procinfo->locallist ;localcurr; ) {
        localnext = (local_sym *)localcurr->sym.next;
        SymFree( &localcurr->sym );
        localcurr = localnext;
    }

    for( symcurr = dir->e.procinfo->labellist; symcurr; ) {
        symnext = symcurr->next;
        SymFree( symcurr );
        symcurr = symnext;
    }

    regcurr = dir->e.procinfo->regslist;
    if( regcurr != NULL ) {
        for( ;regcurr; ) {
            regnext = regcurr->next;
            AsmFree( regcurr->reg );
            AsmFree( regcurr );
            regcurr = regnext;
        }
    }

    if ( dir->e.procinfo->prologuearg )
        AsmFree( dir->e.procinfo->prologuearg );

    AsmFree( dir->e.procinfo );
    return;
}

// PROC directive

ret_code ProcDef( int i )
/******************/
{
    struct asm_sym      *sym;
    dir_node            *dir;
    unsigned int        addr;
    char                *name;
    bool                oldpubstate;

    if( StructDef.struct_depth > 0 ) {
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
        return( ERROR );
    }

    if( i < 0 ) {
        AsmError( PROC_MUST_HAVE_A_NAME );
        return( ERROR );
    }

    if( CurrProc != NULL ) {
        if ( CurrProc->e.procinfo->paralist ||
             CurrProc->e.procinfo->locallist ||
             CurrProc->e.procinfo->regslist ) {
            AsmErr( CANNOT_NEST_PROCEDURES, AsmBuffer[i]->string_ptr);
            return( ERROR );
        }
        /* nested procs ... push currproc on a stack */
        push_proc( CurrProc );
    }

#if FASTPASS
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif

    if ( ModuleInfo.procalign ) {
        AlignCurrOffset( ModuleInfo.procalign );
    }

    name = AsmBuffer[i++]->string_ptr;
    sym = SymSearch( name );

    if( Parse_Pass == PASS_1 ) {

        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = CreateProc( sym, name );
        } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
            /* additional checks (language type? mem type?) */
            dir_free( (dir_node *)sym, TRUE );
            sym = CreateProc( sym, name );
        } else if ( sym->state == SYM_PROC && sym->isproc == FALSE ) {
        } else {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, sym->name );
            return( ERROR );
        }
        dir = (dir_node *)sym;

        SetSymSegOfs( sym );

        oldpubstate = sym->public;

        sym->isproc = TRUE;

        if( ExamineProc( dir, i+1, TRUE ) == ERROR ) {
            return( ERROR );
        }

        if( sym->public == TRUE && oldpubstate == FALSE )
            AddPublicData( sym );
    } else {
        /**/myassert( sym != NULL );

        /* it's necessary to check for a phase error here
         as it is done in LabelCreate() and data_init()!
         */
        addr = GetCurrOffset();

        if (addr != sym->offset) {
            sym->offset = addr;
#ifdef DEBUG_OUT
            if (!PhaseError)
                DebugMsg(("ProcDef: phase error, pass %u, sym >%s<\n", Parse_Pass+1, sym->name));
#endif
            PhaseError = TRUE;
        }
    }

    CurrProc = (dir_node *)sym;
    DefineProc = TRUE;

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    if( Options.line_numbers && write_to_file == TRUE ) {
        AddLinnumDataRef();
    }

    BackPatch( sym );
    return( NOT_ERROR );
}

/* PROTO directive.
 PROTO is virtually an EXTERNDEF for a PROC.
 there is no segment associated with it, however.
 */

ret_code ProtoDef( int i, char * name )
/******************/
{
    struct asm_sym      *sym;
    dir_node            *dir;

    if( Parse_Pass == PASS_1 ) {
        if( i < 0 && name == NULL) {
            AsmError( PROC_MUST_HAVE_A_NAME );
            return( ERROR );
        }
        if (name == NULL)
            name = AsmBuffer[i++]->string_ptr;

        sym = SymSearch( name );

        /* for PROTO, the symbol must be undefined or
           of type PROTO, an external is not allowed */
        if( sym == NULL || sym->state == SYM_UNDEFINED ) {
            sym = CreateProc( sym, name );
        } else if ( sym->state != SYM_PROC ) {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, sym->name );
            return( ERROR );
        }
        dir = (dir_node *)sym;

        i++;
        /* a PROTO type may be used */
        if (AsmBuffer[i]->token == T_ID) {
            dir_node * dir2;
            dir2 = (dir_node *)SymSearch(AsmBuffer[i]->string_ptr);
            if (dir2 && dir2->sym.state == SYM_TYPE && dir2->sym.mem_type == MT_PROC) {
                local_sym *curr;
                local_sym *newl;
                local_sym *oldl;
                dir2 = (dir_node *)(dir2->e.structinfo->target);
                memcpy(dir->e.procinfo, dir2->e.procinfo, sizeof(proc_info));
                dir->sym.mem_type = dir2->sym.mem_type;
                dir->sym.langtype = dir2->sym.langtype;
                dir->sym.mangler  = dir2->sym.mangler;
                dir->sym.public   = dir2->sym.public;
                dir->e.procinfo->paralist = NULL;
                for ( curr = dir2->e.procinfo->paralist; curr; curr = (local_sym *)curr->sym.next) {
                    newl = AsmAlloc( sizeof(local_sym) );
                    memcpy( newl, curr, sizeof(local_sym) );
                    newl->sym.next = NULL;
                    if (dir->e.procinfo->paralist == NULL)
                        dir->e.procinfo->paralist = newl;
                    else {
                        for (oldl = dir->e.procinfo->paralist;oldl->sym.next;oldl = (local_sym *)oldl->sym.next);
                        oldl->sym.next = (asm_sym *)newl;
                    }
                }
                return(NOT_ERROR);
            }
        }

        if( ExamineProc( dir, i, FALSE ) == ERROR )
            return( ERROR );
    } else {
#if 0
        if (sym = SymSearch( AsmBuffer[i]->string_ptr )) {
            dir = (dir_node *)sym;
            /* if there is no matching PROC, set segment value */
            if (dir->e.procinfo->defined == FALSE)
                sym->segment = &GetCurrSeg()->sym;
        }
#endif
    }
    return( NOT_ERROR );
}

static void ProcFini( void )
/**************************/
{
    CurrProc->sym.total_size = GetCurrOffset() - CurrProc->sym.offset;

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    CurrProc = pop_proc();

    DefineProc = FALSE; /* in case there was an empty PROC/ENDP pair */
}

// ENDP directive

ret_code EndpDef( int i )
/******************/
{
    if( StructDef.struct_depth > 0 ) {
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
    } else if( i < 0 ) {
        AsmError( PROC_MUST_HAVE_A_NAME );
    } else if( CurrProc && (SymCmpFunc(CurrProc->sym.name, AsmBuffer[i]->string_ptr ) == 0 )) {
        ProcFini();
        return( NOT_ERROR );
    } else {
        AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
    }
    return( ERROR );
}

void CheckProcOpen( void )
/************************/
{
    while( CurrProc != NULL ) {
        AsmErr( PROC_IS_NOT_CLOSED, CurrProc->sym.name );
        ProcFini();
    }
}

static ret_code write_userdef_prologue( void )
/***********************/
{
    regs_list           *regist;
    int                 len;
    proc_info           *info;
    dir_node            *dir;
    int                 align = CurrWordSize;
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
    info->localsize = ROUND_UP( info->localsize, align );

    /* set bit 4 if the caller restores (E)SP */
    if (CurrProc->sym.langtype == LANG_C ||
        CurrProc->sym.langtype == LANG_SYSCALL ||
        CurrProc->sym.langtype == LANG_WATCOM_C)
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
    DebugMsg(("write_prologue: macro %s returned >%s<\n", ModuleInfo.proc_prologue, retvalue));

    if (Parse_Pass == PASS_1) {
        local_sym *curr;
        len = atoi(retvalue) - info->localsize;
        for (curr = info->locallist; curr; curr = (local_sym *)curr->sym.next) {
            curr->sym.offset -= len;
        }
    }

    Token_Count = Tokenize( CurrSource, 0 );

    return ( NOT_ERROR );
}

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
/***********************/
{
    regs_list           *regist;
    int                 len;
    proc_info           *info;
    int                 align = CurrWordSize;
    char                buffer[80];

    info = CurrProc->e.procinfo;
    info->localsize = ROUND_UP( info->localsize, align );

    /* default processing. if no params/locals are defined, continue */
    if( info->forceframe == FALSE &&
        info->localsize == 0 &&
        info->parasize == 0 &&
        info->is_vararg == FALSE &&
        info->regslist == NULL)
        return( NOT_ERROR );

    PushLineQueue();

    if( ( info->localsize != 0 ) || ( info->parasize != 0 ) || info->is_vararg || info->forceframe ) {

        if( Use32 ) {
            /* write 80386 prolog code
             PUSH EBP
             MOV  EBP, ESP
             SUB  ESP, localsize
            */
            AddLineQueue( "push ebp" );
            AddLineQueue( "mov ebp, esp" );
            if( info->localsize != 0 ) {
                sprintf( buffer, "sub esp, %d", info->localsize );
                AddLineQueue( buffer );
            }
        } else {
            /* write 8086 prolog code
             PUSH BP
             MOV  BP, SP
             SUB  SP, localsize
             */
            AddLineQueue( "push bp" );
            AddLineQueue( "mov bp, sp" );
            if( info->localsize != 0 ) {
                sprintf( buffer, "sub sp, %d", info->localsize );
                AddLineQueue( buffer );
            }
        }
    }

    if ( info->loadds ) {
        AddLineQueue( "push ds" );
        AddLineQueue( "mov ax, DGROUP" );
        if ( Use32 )
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

/* proc_check() checks if the prologue code generation is to be triggered
 it might return NOT_ERROR or ERROR.
*/
ret_code proc_check( void )
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
    if ( ModuleInfo.proc_prologue == NULL )
        return( NOT_ERROR );
    if (*ModuleInfo.proc_prologue == NULLC )
        return( write_default_prologue() ) ;

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
    strcpy( buffer, "pop " );
    strcpy( buffer + strlen( buffer ), regist->reg );
    AddLineQueue( buffer );
}

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
/********************************/
{
    proc_info   *info;
    char        buffer[80];

    info = CurrProc->e.procinfo;

    /* Pop the registers */
    pop_register( CurrProc->e.procinfo->regslist );

    if ( info->loadds ) {
        AddLineQueue( "pop ds" );
    }

    if( ( info->localsize == 0 ) && ( info->parasize == 0 ) && info->is_vararg == FALSE && info->forceframe == FALSE )
        return;

    if( info->pe_type ) {
        /* write 80286 and 80386 epilog code */
        strcpy( buffer, "leave" );
    } else  {
        /*
         MOV [E]SP, [E]BP
         POP [E]BP
         */
        if( info->localsize != 0 ) {
            if( Use32 )
                strcpy( buffer, "mov esp, ebp" );
            else
                strcpy( buffer, "mov sp, bp" );
            AddLineQueue( buffer );
        }
        if( Use32 )
            strcpy( buffer, "pop ebp" );
        else
            strcpy( buffer, "pop bp" );
    }
    AddLineQueue( buffer );
}

// write userdefined epilogue code
// if a RET/IRET instruction has been found inside a PROC.

static ret_code write_userdef_epilogue( bool flag_iret )
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
         CurrProc->sym.langtype == LANG_WATCOM_C)
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
/****************************************/
{
    proc_info   *info;
    expr_list   opndx;
    bool        flag_iret = FALSE;
    char        buffer[MAX_LINE_LEN];

    DebugMsg(( "RetInstr() enter\n" ));

    if ( FileInfo.file[LST] ) {
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );
    }

    if( AsmBuffer[i]->value == T_IRET || AsmBuffer[i]->value == T_IRETD )
        flag_iret = TRUE;

    if ( *ModuleInfo.proc_epilogue != NULLC ) {
#if FASTPASS
        if ( Parse_Pass > PASS_1 && UseSavedState )
            return( NOT_ERROR );
#endif
        return( write_userdef_epilogue( flag_iret ) );
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
            if (opndx.type != EXPR_CONST || opndx.string != NULL ) {
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

// init this module.

void ProcInit()
{
    ProcStack = NULL;
    CurrProc  = NULL;
    DefineProc = FALSE;
    ModuleInfo.proc_prologue = "";
    ModuleInfo.proc_epilogue = "";
    ModuleInfo.procs_private = FALSE;
    ModuleInfo.procs_export = FALSE;
}
