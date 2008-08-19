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
#include "queues.h"
#include "equate.h"
#include "fixup.h"
#include "mangle.h"
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "types.h"
#include "condasm.h"
#include "hll.h"
#include "macro.h"
#include "proc.h"
#include "fastpass.h"

#include "myassert.h"

#define NESTEDPROCS 0

dir_node                *CurrProc;      // current procedure
#if NESTEDPROCS
static proc_info        *ProcStack;
#endif

bool                    in_prologue;
bool                    in_epilogue;
bool                    DefineProc;     // TRUE if the definition of procedure
                                        // has not ended

enum {
#undef fix
#define fix( tok, str, val, init )              tok

#include "dirtoken.h"
};

static char * watc_regs[] = {"eax", "ebx", "ecx", "edx"};

static lang_type lt[] = {LANG_C, LANG_SYSCALL, LANG_STDCALL, LANG_PASCAL, LANG_FORTRAN, LANG_BASIC, LANG_WATCOM_C};
static int  ltok[]    = {   T_C,    T_SYSCALL,    T_STDCALL,    T_PASCAL,    T_FORTRAN,    T_BASIC,    T_WATCOM_C};

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

#if NESTEDPROCS
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
#endif

#if 0
static int get_watcom_argument_string( char *buffer, uint_8 size, uint_8 *parm_number )
/*************************************************************************************/
/* get the register for parms 0 to 3,
 * using the watcom register parm passing conventions ( A D B C ) */
{
    int parm = *parm_number;

    if( parm > 3 )
        return( FALSE );
    switch( size ) {
    case 1:
        sprintf( buffer, parm_reg[A_BYTE][parm] );
        break;
    case 2:
        sprintf( buffer, parm_reg[A_WORD][parm] );
        break;
    case 4:
        if( Use32 ) {
            sprintf( buffer, parm_reg[A_DWORD][parm] );
            break;
        } else {
            switch( parm ) {
            case 0:
                sprintf( buffer, " [DX AX]" );
                buffer[0] = 0;
                *parm_number = 1; // take up 0 and 1
                break;
            case 1:
            case 2:
                sprintf( buffer, " [CX BX]" );
                buffer[0] = 0;
                *parm_number = 3; // take up 2 and 3
                break;
            default:
                // passed on stack ... it's ok
                return( FALSE );
            }
            return( TRUE );
        }
    case 10:
        AsmErr( TBYTE_NOT_SUPPORTED );
        return( ERROR );
    case 6:
        if( Use32 ) {
            switch( parm ) {
            case 0:
                sprintf( buffer, " [DX EAX]" );
                buffer[0]=0;
                *parm_number = 1; // take up 0 and 1
                break;
            case 1:
            case 2:
                sprintf( buffer, " [CX EBX]" );
                buffer[0]=0;
                *parm_number = 3; // take up 2 and 3
                break;
            default:
                // passed on stack ... it's ok
                return( FALSE );
            }
            return( TRUE );
        }
        // fall through for 16 bit to default
    case 8:
        if( Use32 ) {
            switch( parm ) {
            case 0:
                sprintf( buffer, " [EDX EAX]" );
                buffer[0]=0;
                *parm_number = 1; // take up 0 and 1
                break;
            case 1:
            case 2:
                sprintf( buffer, " [ECX EBX]" );
                buffer[0]=0;
                *parm_number = 3; // take up 2 and 3
                break;
            default:
                // passed on stack ... it's ok
                return( FALSE );
            }
            return( TRUE );
        }
        // fall through for 16 bit to default
    default:
        // something wierd
        AsmError( STRANGE_PARM_TYPE );
        return( ERROR );
    }
    return( TRUE );
}
#endif

// LOCAL directive. Called on Pass 1 only

int LocalDef( int i )
/*******************/
{
    char        *string;
    int         type;
    label_list  *local;
    label_list  *curr;
    proc_info   *info;
    int         size;
    int         idx;
    struct asm_sym      *sym;
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
            AsmError( LABEL_IS_EXPECTED );
            return( ERROR );
        }
        DebugMsg(("LocalDef: %s\n", string));

        if ((sym = SymSearch( string )) && sym->state != SYM_UNDEFINED) {
            AsmErr( SYMBOL_PREVIOUSLY_DEFINED, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        sym = SymCreate(string, FALSE);
        if( !sym)
            return( ERROR );

        sym->state = SYM_STACK;
        sym->defined = TRUE;
//        sym->local = TRUE;
        sym->mem_type = Use32 ? MT_DWORD : MT_WORD;

        i++;

        local = AsmAlloc( sizeof( label_list ) );
        local->sym = sym;
//        local->size = align;
        local->sym->first_size = align;
//        local->factor = 1;
        local->next = NULL;
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
//            local->sym->count = expr.value;
            local->sym->total_length = expr.value;
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
                if ((idx = FindSimpleType( AsmBuffer[i]->value)) != ERROR)
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
                sym->mem_type = type;
                local->sym->first_size = SizeFromMemtype( sym->mem_type, Use32 );
            } else {
                sym->mem_type = MT_TYPE;
//                sym->mem_type = symtype->mem_type;
                sym->type = symtype;
                local->sym->first_size = symtype->total_size;
                DebugMsg(("LocalDef: type=%s, total_size=%X (curr localsize=%X)\n", symtype->name, symtype->total_size, info->localsize));
            }
        }
        if (local->sym->total_length)
            local->sym->total_size = local->sym->first_size * local->sym->total_length;
        else
            local->sym->total_size = local->sym->first_size;

        info->localsize += local->sym->total_size;

        if (local->sym->first_size > align)
            info->localsize = ROUND_UP(info->localsize, align);
        else
            info->localsize = ROUND_UP(info->localsize, local->sym->first_size);
        DebugMsg(("LocalDef: aligned local total=%X\n", info->localsize));

        sym->offset = - info->localsize;
        DebugMsg(("LocalDef: symbol offset=%d\n", sym->offset));

        if( info->locallist == NULL ) {
            info->locallist = local;
        } else {
            for( curr = info->locallist;; curr = curr->next ) {
                if( curr->next == NULL ) {
                    break;
                }
            }
            curr->next = local;
        }

        if( ( i < Token_Count ) && ( AsmBuffer[i]->token != T_COMMA ) ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }

    }
    return( NOT_ERROR );
}

// parse parameters of a PROC/PROTO

static int ParseProcParams(dir_node *proc, int i, bool bDefine)
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
    label_list      *paranode;
    label_list      *paracurr;
    char buffer[MAX_LINE_LEN];

    /* parse PROC parms */
    /* it's important to remember that params are stored in "push" order! */

    if (proc->sym.langtype == LANG_C ||
        proc->sym.langtype == LANG_SYSCALL ||
        proc->sym.langtype == LANG_WATCOM_C ||
        proc->sym.langtype == LANG_STDCALL)
        for (paracurr = proc->e.procinfo->paralist;paracurr && paracurr->next;paracurr = paracurr->next);
    else
        paracurr = proc->e.procinfo->paralist;

    for(cntParam = 0 ; AsmBuffer[i]->token != T_FINAL ; cntParam++ ) {

        symtype = NULL;
        /* read symbol */
        if (bDefine) {
            if (AsmBuffer[i]->token != T_ID) {
                DebugMsg(("ParseProcParams: name missing/invalid for parameter %u, i=%u\n", cntParam+1, i));
                AsmError( SYNTAX_ERROR ); /* for PROC, parameter needs a name */
                return( ERROR );
            }
            token = AsmBuffer[i++]->string_ptr;
        } else {
            /* for PROTO, a parameter name is optional */
            if( AsmBuffer[i]->token == T_COLON )
                if (paracurr)
                    token = paracurr->sym->name;
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
        if ((AsmBuffer[i]->token == T_RES_ID) && (AsmBuffer[i]->value == T_PTR)) {
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
        DebugMsg(("ParseProcParams: cnpParam=%u, i=%u, token=%s, type=%s\n", cntParam, i, token, typetoken));

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
                    DebugMsg(("ParseProcParams: type invalid for parameter %u\n", cntParam+1));
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
            if (paracurr->sym->type)
                oldsize = paracurr->sym->total_size;
            else if (paracurr->sym->mem_type == MT_EMPTY)
                oldsize = 0;
            else
                oldsize = SizeFromMemtype( paracurr->sym->mem_type, Use32 );
            if (oldsize != newsize) {
                DebugMsg(("ParseProcParams: old memtype=%u, new memtype=%u\n", paracurr->sym->mem_type, mem_type));
                AsmErr( SYMBOL_TYPE_CONFLICT, token );
                return( ERROR );
            }
            /* the parameter type used in PROC has highest priority! */
            if (bDefine) {
                if (symtype) {
                    paracurr->sym->type = symtype;
                    paracurr->sym->mem_type = MT_TYPE;
                } else
                    paracurr->sym->mem_type = mem_type;
            }
#else
            if (paracurr->sym->mem_type != mem_type) {
                DebugMsg(("ParseProcParams: old memtype=%u, new memtype=%u\n", paracurr->sym->mem_type, mem_type));
                AsmErr( SYMBOL_TYPE_CONFLICT, token );
                return( ERROR );
            }
            if (symtype != NULL)
                if (paracurr->sym->type != symtype) {
                    DebugMsg(("ParseProcParams: struct param type=%X, symtype=%X\n", paracurr->sym->type, symtype));
                    AsmErr( SYMBOL_TYPE_CONFLICT, token );
                    return( ERROR );
                }
#endif
            if (bDefine) {
                paracurr->sym = SymSetName(paracurr->sym, token);
            }
            /* set paracurr to next parameter */
            if (proc->sym.langtype == LANG_C ||
                proc->sym.langtype == LANG_SYSCALL ||
                proc->sym.langtype == LANG_WATCOM_C ||
                proc->sym.langtype == LANG_STDCALL) {
                label_list *l;
                for (l = proc->e.procinfo->paralist; l && (l->next != paracurr); l = l->next);
                paracurr = l;
            } else
                paracurr = paracurr->next;

        } else if (proc->e.procinfo->init == TRUE) {
            /* second definition has more parameters than first */
            DebugMsg(("ParseProcParams: different param count\n"));
            AsmError( CONFLICTING_PARAMETER_DEFINITION );
            return( ERROR );
        } else {
            if (bDefine)
                sym = SymCreate(token, FALSE);
            else
                sym = SymCreate("", FALSE);/* for PROTO, no param name needed */
            if( sym == NULL )
                return( ERROR );
            sym->defined = TRUE;
            if (proc->sym.langtype == LANG_WATCOM_C && proc->e.procinfo->parasize < 16) {
                sym->state = SYM_TMACRO;
                sym->string_ptr = AsmAlloc(4);
                strcpy(sym->string_ptr, watc_regs[proc->e.procinfo->parasize >> 2]);
            } else {
                sym->state = SYM_STACK;
                if (symtype) {
                    sym->type = symtype;
                    sym->mem_type = MT_TYPE;
                } else
                    sym->mem_type = mem_type;
            }
            paranode = AsmAlloc( sizeof( label_list ) );

            paranode->is_ptr = is_ptr;
            paranode->is_far = is_far;
            paranode->is_vararg = is_vararg;
            paranode->is32 = is32;

            paranode->sym = sym;

            if (symtype)
                paranode->sym->first_size = symtype->total_size;
            else if (mem_type == MT_EMPTY) /* ignore VARARG */
                paranode->sym->first_size = 0;
            else
                paranode->sym->first_size = SizeFromMemtype( mem_type, Use32 );

            paranode->sym->total_size = paranode->sym->first_size;

            if( paranode->is_vararg )
                ;
            else if( Use32 ) {
                proc->e.procinfo->parasize += ROUND_UP( paranode->sym->first_size, 4 );
            } else {
                proc->e.procinfo->parasize += ROUND_UP( paranode->sym->first_size, 2 );
            }

            proc->e.procinfo->is_vararg |= paranode->is_vararg;

            /* Parameters are stored in "push" order */

            switch( proc->sym.langtype ) {
            case LANG_BASIC:
            case LANG_FORTRAN:
            case LANG_PASCAL:
                paranode->next = NULL;
                if( proc->e.procinfo->paralist == NULL ) {
                    proc->e.procinfo->paralist = paranode;
                } else {
                    for( paracurr = proc->e.procinfo->paralist;; paracurr = paracurr->next ) {
                        if( paracurr->next == NULL ) {
                        break;
                        }
                    }
                    paracurr->next = paranode;
                    paracurr = NULL;
                }
                break;
            default:
                paranode->next = proc->e.procinfo->paralist;
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
            DebugMsg(("ParseProcParams: a param is left over, cntParam=%u\n", cntParam));
            AsmError( CONFLICTING_PARAMETER_DEFINITION );
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
            for (curr = 1,paranode = proc->e.procinfo->paralist;curr < cntParam;paranode = paranode->next, curr++);
            DebugMsg(("ParseProcParams: parm=%s, ofs=%u, size=%d\n", paranode->sym->name, offset, paranode->sym->first_size));
            if (paranode->sym->state == SYM_TMACRO)
                ;
            else {
                paranode->sym->offset = offset;
                offset += ROUND_UP( paranode->sym->first_size, CurrWordSize );
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

int ExamineProc( dir_node *proc, int i, bool bDefine )
/*******************************************/
{
    char            *token;
    regs_list       *regist;
    regs_list       *temp_regist;
    int             type;
    int             type_comp;
    bool            is32 = Use32;
    bool            visibility=FALSE;

    /* ignore current segment for PROTOs and model FLAT */
    if (ModuleInfo.model == MOD_FLAT && bDefine == FALSE)
        is32 = TRUE;

    // set some default values

    if (proc->e.procinfo->init == FALSE) {
        proc->sym.mem_type = IS_PROC_FAR() ? MT_FAR : MT_NEAR;
        proc->sym.use32 = is32;
    }

    proc->sym.defined = TRUE;
    if (bDefine) {
        proc->e.procinfo->export = ModuleInfo.procs_export;
        proc->sym.public = ~ModuleInfo.procs_private;
        proc->e.procinfo->pe_type = ( ( curr_cpu & P_CPU_MASK ) == P_286 ) || ( ( curr_cpu & P_CPU_MASK ) == P_386 );
    }

    if( AsmBuffer[i]->token == T_STRING ) {
        /* name mangling */
        if (bDefine)
            SetMangler( &proc->sym, AsmBuffer[i]->string_ptr, LANG_NONE );
        i++;
    }

    /* distance comes first */
    if (AsmBuffer[i]->token == T_RES_ID) {
        switch (AsmBuffer[i]->value) {
        case T_FAR:
        case T_NEAR:
        case T_FAR16:
        case T_FAR32:
        case T_NEAR16:
        case T_NEAR32:
            type = FindSimpleType(AsmBuffer[i]->value);
            if (bDefine) {
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
    /* second is langtype */
    if (AsmBuffer[i]->token == T_RES_ID) {
        switch (AsmBuffer[i]->value) {
        case T_BASIC:
        case T_FORTRAN:
        case T_PASCAL:
        case T_C:
        case T_WATCOM_C:
        case T_STDCALL:
        case T_SYSCALL:
            for (type = 0; type < 7; type++) {
                if (ltok[type] == AsmBuffer[i]->value) {
                    if (proc->sym.langtype != LANG_NONE && proc->sym.langtype != lt[type])
                        AsmError( PROC_AND_PROTO_CALLING_CONV_CONFLICT );
                    proc->sym.langtype = lt[type];
                    break;
                }
            }
            i++;
        }
    }
    /* third is visibility */
    /* PUBLIC is a directive! */

    if (AsmBuffer[i]->token == T_ID || AsmBuffer[i]->token == T_DIRECTIVE) {
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

    if (AsmBuffer[i]->token == T_ID) {
        if (stricmp(AsmBuffer[i]->string_ptr, "USES") == 0) {
            if (!bDefine) {/* not for PROTO! */
                DebugMsg(("ExamineProc: USES found in PROTO\n"));
                AsmError( SYNTAX_ERROR );
            }
            /* check for register name */
            for( i++; ( i < Token_Count ) && ( AsmBuffer[i]->token == T_REG ); i++ ) {
                if (SizeFromRegister(AsmBuffer[i]->value) == 1) {
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
            AsmError( CONFLICTING_PARAMETER_DEFINITION );
    } else if( proc->sym.langtype == LANG_NONE ) {
        AsmError( LANG_MUST_BE_SPECIFIED );
        return (ERROR);
    } else  {
        if( AsmBuffer[i]->token == T_COMMA )
            i++;
        if (ERROR == ParseProcParams(proc, i, bDefine))
            /* do proceed if the parameter scan returns an error */
            ;//return(ERROR);
    }

    proc->e.procinfo->init = TRUE;
    DebugMsg(("ExamineProc: parasize=%u\n", proc->e.procinfo->parasize));
    if (bDefine == TRUE) {
        proc->e.procinfo->defined = TRUE;
        CurrProc = proc;
        DefineProc = TRUE;
    }

    return( NOT_ERROR );
}

int ProcDef( int i )
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
#if NESTEDPROCS
        /* nested procs ... push currproc on a stack */
        push_proc( CurrProc );
#else
        AsmErr( CANNOT_NEST_PROCEDURES, AsmBuffer[i]->string_ptr);
        return( ERROR );
#endif
    }

#if FASTPASS
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif

    name = AsmBuffer[i++]->string_ptr;
    sym = SymSearch( name );

    if( Parse_Pass == PASS_1 ) {

        if( sym == NULL ) {
            dir = dir_insert( name, TAB_PROC );
            sym = &dir->sym;
        } else {
            dir = (dir_node *)sym;
            if( sym->state == SYM_UNDEFINED)
                dir_change( dir, TAB_PROC );
            else if (sym->state == SYM_EXTERNAL && sym->weak == TRUE) {
                /* additional check for matching language type? */
                dir_change( dir, TAB_PROC );
            } else {
                /* does a PROTO exist? */
                if ((sym->state == SYM_PROC) && (!(dir->e.procinfo->defined)))
                    ;
                else {
                    AsmErr( SYMBOL_PREVIOUSLY_DEFINED, sym->name );
                    return( ERROR );
                }
            }
        }

        SetSymSegOfs( sym );

        oldpubstate = dir->sym.public;

        if( ExamineProc( dir, i+1, TRUE ) == ERROR ) {
            return( ERROR );
        }
        if( dir->sym.public == TRUE && oldpubstate == FALSE)
            AddPublicData( dir );
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
        CurrProc = (dir_node *)sym;
        DefineProc = TRUE;
    }
    BackPatch( sym );
    return( NOT_ERROR );
}

/* a PROTO is virtually an EXTERNDEF for a PROC.
 there is no segment associated with it, however.
 */

int ProtoDef( int i, char * name )
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

        if( sym == NULL ) {
            dir = dir_insert( name, TAB_PROC );
            sym = &dir->sym;
        } else {
            dir = (dir_node *)sym;
            if ( sym->state == SYM_UNDEFINED ) {
                dir_change( dir, TAB_PROC );
            } else if( sym->state != SYM_PROC ) {
                AsmErr( SYMBOL_PREVIOUSLY_DEFINED, sym->name );
                return( ERROR );
            }
        }

        i++;
        /* a PROTO type may be used */
        if (AsmBuffer[i]->token == T_ID) {
            dir_node * dir2;
            dir2 = (dir_node *)SymSearch(AsmBuffer[i]->string_ptr);
            if (dir2 && dir2->sym.state == SYM_TYPE && dir2->sym.mem_type == MT_PROC) {
                label_list *curr;
                label_list *newl;
                label_list *oldl;
                dir2 = (dir_node *)(dir2->e.structinfo->target);
                memcpy(dir->e.procinfo, dir2->e.procinfo, sizeof(proc_info));
                dir->sym.mem_type = dir2->sym.mem_type;
                dir->sym.langtype = dir2->sym.langtype;
                dir->sym.mangler  = dir2->sym.mangler;
                dir->sym.public   = dir2->sym.public;
                dir->e.procinfo->paralist = NULL;
                for (curr = dir2->e.procinfo->paralist;curr;curr = curr->next) {
                    newl = AsmAlloc(sizeof(label_list));
                    memcpy(newl, curr, sizeof(label_list));
                    newl->next = NULL;
                    if (dir->e.procinfo->paralist == NULL)
                        dir->e.procinfo->paralist = newl;
                    else {
                        for (oldl = dir->e.procinfo->paralist;oldl->next;oldl = oldl->next);
                        oldl->next = newl;
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
#if NESTEDPROCS
    CurrProc = pop_proc();
#else
    CurrProc = NULL;
#endif
    DefineProc = FALSE; /* in case there was an empty PROC/ENDP pair */
}

// ENDP detected

int ProcEnd( int i )
/******************/
{
    if( StructDef.struct_depth > 0 ) {
        AsmError( STATEMENT_NOT_ALLOWED_INSIDE_STRUCTURE_DEFINITION );
        return( ERROR );
    } else if( i < 0 ) {
        AsmError( PROC_MUST_HAVE_A_NAME );
        // ProcFini();
        return( ERROR );
    } else if( CurrProc == NULL ) {
        DebugMsg(("ProcEnd: nesting error, CurrProc is NULL\n"));
        AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
        return( ERROR );
    // } else if( (dir_node *)SymSearch( AsmBuffer[i]->string_ptr ) == CurrProc ) {
    } else if( SymCmpFunc(CurrProc->sym.name, AsmBuffer[i]->string_ptr ) == 0 ) {
        ProcFini();
        return( NOT_ERROR );
    } else {
        AsmError( PROC_NAME_DOES_NOT_MATCH );
        // ProcFini();
        return( ERROR );
    }
}

void CheckProcOpen( void )
/************************/
{
    while( CurrProc != NULL ) {
        if( Parse_Pass == PASS_1 )
            AsmErr( PROC_IS_NOT_CLOSED, CurrProc->sym.name );
        ProcFini();
    }
}

// write PROC prologue
// this is to be done after the LOCAL directives
// and *before* any real instruction

static int WritePrologue( void )
/***********************/
{
    char                buffer[80];
    regs_list           *regist;
    int                 len;
    proc_info           *info;
    dir_node            *dir;
    int                 align = CurrWordSize;

    info = CurrProc->e.procinfo;
    info->localsize = ROUND_UP( info->localsize, align );

    DefineProc = FALSE;

    /* there are 3 cases:
     option prologue:NONE -> exit quickly
     option prologue:userdefined macro function -> RunMacro()
     option prologue:default
     */
    if (ModuleInfo.proc_prologue == NULL)
        return(NOT_ERROR);  /* continue processing */
    else if (*ModuleInfo.proc_prologue != '\0') {
        regs_list * regs;
        char reglst[64];
        int flags = CurrProc->sym.langtype; /* set bits 0-2 */

        /* set bit 4 if the caller restores (E)SP */
        if (CurrProc->sym.langtype == LANG_C ||
            CurrProc->sym.langtype == LANG_WATCOM_C)
            flags |= 0x10;
//        if (info->mem_type == MT_FAR)
        if (CurrProc->sym.mem_type == MT_FAR)
            flags |= 0x20;
        if (CurrProc->sym.public == FALSE)
            flags |= 0x40;

        strcpy(reglst,"<");
        for (regs = info->regslist;regs;regs = regs->next) {
            strcat(reglst,regs->reg);
            if (regs->next)
                strcat(reglst,",");
        }
        strcat(reglst,">");

        dir = (dir_node *)SymSearch(ModuleInfo.proc_prologue);
        if (dir) {
            char retvalue[MAX_LINE_LEN];
            char currline[MAX_LINE_LEN];
            label_list *curr;
            strcpy( currline, CurrString );
            sprintf(buffer,"%s(%s, %u, %u, %u, <%s>, %s)", ModuleInfo.proc_prologue,
                    CurrProc->sym.name, flags, info->parasize, info->localsize,
                    reglst, "");
            in_prologue = TRUE;
            retvalue[0] = NULLC;
            RunMacro(dir, buffer, retvalue, TRUE, TRUE, FALSE);
            DebugMsg(("WritePrologue: macro %s returned >%s<\n", ModuleInfo.proc_prologue, retvalue));
            if (Parse_Pass == PASS_1) {
                len = atoi(retvalue) - info->localsize;
                for (curr = info->locallist;curr;curr = curr->next) {
                    curr->sym->offset -= len;
                }
            }
            in_prologue = FALSE;
            PushLineQueue();
            InputQueueLine( currline );
            return (EMPTY);  /* skip further processing of line */
        }
    }

    /* default processing. if no params/locals are defined, continue */
    if( info->localsize == 0 &&
        info->parasize == 0 &&
        info->is_vararg == FALSE &&
        info->regslist == NULL)
        return( NOT_ERROR );

    PushLineQueue();
    in_prologue = TRUE;

    if( ( info->localsize != 0 ) || ( info->parasize != 0 ) || info->is_vararg ) {
        // prolog code timmings
        //
        //                                                   best result
        //               size  86  286  386  486  P     86  286  386  486  P
        // push bp       2     11  3    2    1    1
        // mov bp,sp     2     2   2    2    1    1
        // sub sp,immed  4     4   3    2    1    1
        //              -----------------------------
        //               8     17  8    6    3    3     x   x    x    x    x
        //
        // push ebp      2     -   -    2    1    1
        // mov ebp,esp   2     -   -    2    1    1
        // sub esp,immed 6     -   -    2    1    1
        //              -----------------------------
        //               10    -   -    6    3    3              x    x    x
        //
        // enter imm,0   4     -   11   10   14   11
        //
        // write prolog code
        if( Use32 ) {
            // write 80386 prolog code
            // PUSH EBP
            // MOV  EBP, ESP
            // SUB  ESP, the number of localbytes
            InputQueueLine( "push ebp" );
            InputQueueLine( "mov ebp, esp" );
            if( info->localsize != 0 ) {
                sprintf( buffer, "sub esp, %d", info->localsize );
                InputQueueLine( buffer );
            }
        } else {
            // write 8086 prolog code
            // PUSH BP
            // MOV  BP, SP
            // SUB  SP, the number of localbytes
            InputQueueLine( "push bp" );
            InputQueueLine( "mov bp, sp" );
            if( info->localsize != 0 ) {
                sprintf( buffer, "sub sp, %d", info->localsize );
                InputQueueLine( buffer );
            }
        }
    }
    /* Push the registers */
    if( info->regslist ) {
        strcpy( buffer, "push " );
        len = strlen( buffer );
        for( regist = info->regslist; regist; regist = regist->next ) {
            strcpy( buffer + len, regist->reg );
            InputQueueLine( buffer );
        }
    }
    InputQueueLine( CurrString );

    return( EMPTY );
}

/* proc_check() checks if the prologue code generation is to be triggered
 it might return NOT_ERROR, ERROR or EMPTY! if EMPTY is returned, further
 processing of current line is skipped.
*/
int proc_check( void )
{
    /* some directives are ignored (LOCAL, ORG, ALIGN, LABEL, ENDP!). */
    /* others - i.e. SEGMENT/ENDS - will trigger the code generation */

    if (AsmBuffer[0]->token == T_DIRECTIVE &&
        (AsmBuffer[0]->value == T_LOCAL ||
         AsmBuffer[0]->value == T_EVEN ||
         AsmBuffer[0]->value == T_ALIGN ||
         AsmBuffer[0]->value == T_ORG ||
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

    /* masm does also allow data definition directives to occur BEFORE
     the prologue code is inserted. This might be a bug, however, and
     therefore it's not copied.
     */

    return( WritePrologue());
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
    InputQueueLine( buffer );
}

static void write_epilogue( void )
/********************************/
{
    char        buffer[80];
    proc_info   *info;

    /**/myassert( CurrProc != NULL );
    info = CurrProc->e.procinfo;

    if (ModuleInfo.proc_epilogue == NULL)
        return;

#if 1
    /* is a true macro set for prologue creation? */
    if (*ModuleInfo.proc_epilogue != '\0') {
        regs_list * regs;
        char reglst[64];
        int flags = CurrProc->sym.langtype; /* set bits 0-2 */

        if (CurrProc->sym.langtype == LANG_C)
            flags |= 0x10;
//        if (info->mem_type == MT_FAR)
        if (CurrProc->sym.mem_type == MT_FAR)
            flags |= 0x20;
        if (CurrProc->sym.public == FALSE)
            flags |= 0x40;

        strcpy(reglst,"<");
        if (info->regslist) {
            for (regs = info->regslist;regs->next;regs = regs->next);
            while (1) {
                regs_list * regs2;
                strcat(reglst, regs->reg);
                if (regs == info->regslist)
                    break;
                strcat(reglst,",");
                for (regs2 = info->regslist;regs2->next != regs; regs2 = regs2->next);
                regs = regs2;
            }
        }
        strcat(reglst,">");

        sprintf(buffer," %s %s, %02XH, %02XH, %02XH,<%s>, %s", ModuleInfo.proc_epilogue,
                CurrProc->sym.name, flags, info->parasize, info->localsize,
                reglst, "");
        InputQueueLine( buffer );
        return;
    }
#endif
    /* Pop the registers */
    pop_register( CurrProc->e.procinfo->regslist );

    if( ( info->localsize == 0 ) && ( info->parasize == 0 ) && !info->is_vararg )
        return;
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
    // write epilog code
    if( info->pe_type ) {
        // write 80286 and 80386 epilog code
        // LEAVE
        strcpy( buffer, "leave" );
    } else if( Use32 ) {
        // write 32-bit 80486 or P epilog code
        // Mov ESP, EBP
        // POP EBP
        if( info->localsize != 0 ) {
            strcpy( buffer, "mov esp, ebp" );
            InputQueueLine( buffer );
        }
        strcpy( buffer, "pop ebp" );
    } else {
        // write 16-bit 8086 or 80486 or P epilog code
        // Mov SP, BP
        // POP BP
        if( info->localsize != 0 ) {
            strcpy( buffer, "mov sp, bp" );
            InputQueueLine( buffer );
        }
        strcpy( buffer, "pop bp" );
    }
    InputQueueLine( buffer );
}

// a RET has occured inside a PROC.

int RetInstr( int i, int count, int flag_iret )
/****************************************/
{
    char        buffer[20];
    proc_info   *info;
    expr_list   opndx;

    info = CurrProc->e.procinfo;

    if( flag_iret ) {
        if( AsmBuffer[i]->value == T_IRET ) {
            strcpy( buffer, "iretf" );
        } else {
            strcpy( buffer, "iretdf" );
        }
    } else {
//        if( info->mem_type == MT_NEAR ) {
        if( CurrProc->sym.mem_type == MT_NEAR) {
            strcpy( buffer, "retn " );
        } else {
            strcpy( buffer, "retf " );
        }
    }

    write_epilogue();

    if( !flag_iret ) {
        /* the RET is part of the EPILOGUE! Don't write it if an epilogue
         macro is set.
         */
        if (ModuleInfo.proc_epilogue && (*ModuleInfo.proc_epilogue != '\0'))
            return (NOT_ERROR);

        if( count == i + 1 ) {
            switch( CurrProc->sym.langtype ) {
            case LANG_BASIC:
            case LANG_FORTRAN:
            case LANG_PASCAL:
                if( info->parasize != 0 ) {
                    sprintf( buffer + strlen( buffer ), "%d", info->parasize );
                }
                break;
            case LANG_STDCALL:
                if( !info->is_vararg && info->parasize != 0 ) {
                    sprintf( buffer + strlen( buffer ), "%d", info->parasize );
                }
                break;
            default:
                break;
            }
        } else {
            ++i;
            if( EvalOperand( &i, count, &opndx, TRUE ) == ERROR)
                return( ERROR );
            if (opndx.type != EXPR_CONST || opndx.string != NULL ) {
                AsmError( CONSTANT_EXPECTED );
                return( ERROR );
            }
            sprintf( buffer + strlen( buffer ), "%d", opndx.value );
        }
    }

    InputQueueLine( buffer );

    return( NOT_ERROR );
}

// init this module.

void ProcInit()
{
#if NESTEDPROCS
    ProcStack = NULL;
#endif
    CurrProc  = NULL;
    DefineProc = FALSE;
    in_prologue = FALSE;
    in_epilogue = FALSE;
    ModuleInfo.proc_prologue = "";
    ModuleInfo.proc_epilogue = "";
    ModuleInfo.procs_private = FALSE;
    ModuleInfo.procs_export = FALSE;
}
