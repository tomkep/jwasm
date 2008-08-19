/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  Processing of INVOKE directive.
*
****************************************************************************/


#include "globals.h"
#include <ctype.h>

#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "expreval.h"
#include "directiv.h"
#include "input.h"
#include "queues.h"
#include "equate.h"
#include "mangle.h"

#include "myassert.h"

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

static int size_vararg;

// get size from register (for PUSH)

int SizeFromRegister( int registername )
/******************************/
{
    switch( registername ) {
    case T_EAX:
    case T_EBX:
    case T_ECX:
    case T_EDX:
    case T_ESI:
    case T_EDI:
    case T_EBP:
    case T_ESP:
        return( 4 );
    case T_AX:
    case T_BX:
    case T_CX:
    case T_DX:
    case T_SI:
    case T_DI:
    case T_BP:
    case T_SP:
        return( 2 );
    case T_AL:
    case T_AH:
    case T_BL:
    case T_BH:
    case T_CL:
    case T_CH:
    case T_DL:
    case T_DH:
        return( 1 );
    case T_CS:
    case T_SS:
    case T_DS:
    case T_ES:
    case T_FS:
    case T_GS:
        return( 0 );
    case T_ST:
        return( 10 );
    case T_MM0:
    case T_MM1:
    case T_MM2:
    case T_MM3:
    case T_MM4:
    case T_MM6:
    case T_MM7:
        return( 8 );
    case T_XMM0:
    case T_XMM1:
    case T_XMM2:
    case T_XMM3:
    case T_XMM4:
    case T_XMM6:
    case T_XMM7:
        return( 16 ); /* masm v6x and v7 return 10, v8 returns 16 */
    default: /* CRx, DRx, TRx ... */
        return( 4 );
    }
}


/*
  push one parameter of a procedure called with INVOKE onto the stack
  this is an internal macro which works with text only!
  curr    : the parameter of the PROC to call
  i       : the AsmBuffer index of the start of the parameter list
  reqParam: the index of the parameter which is to be pushed
*/

void SkipTypecast(char * fullparam, int i)
{
    int j;
    fullparam[0] = NULLC;
    for (j = i;;j++) {
        if ((AsmBuffer[j]->token == T_COMMA) || (AsmBuffer[j]->token == T_FINAL))
            break;
        if ((AsmBuffer[j+1]->token == T_RES_ID) && (AsmBuffer[j+1]->value == T_PTR))
            j = j + 1;
        else {
            if (fullparam[0] != NULLC)
                strcat(fullparam," ");
            strcat(fullparam, AsmBuffer[j]->string_ptr);
        }
    }
}

int PushInvokeParam(label_list * curr, int i, int reqParam, bool * eaxused)
{
    int  currParm;
    int psize;
    char fullparam[MAX_LINE_LEN];
    char buffer[MAX_LINE_LEN];

    DebugMsg(("PushInvokeParam(param=%X, i=%u, reqParam=%u) enter\n", curr, i, reqParam));
    for (currParm = 0;currParm <= reqParam;) {
        if (AsmBuffer[i]->token == T_FINAL) { /* this is no real error! */
            DebugMsg(("PushInvokeParam: T_FINAL token, i=%u\n", i));
            return ERROR;
        }
        if (AsmBuffer[i]->token == T_COMMA) {
            currParm++;
        }
        i++;
    }
    /* if curr is NULL this call is just a parameter check */
    if (!curr) return (NOT_ERROR);

    /* set psize (size of parameter) */
    if (curr->is_ptr) {
        psize = curr->is32 ? 4 : 2;
        if (curr->is_far)
            psize += 2;
    } else if (curr->sym->mem_type != MT_TYPE)
        psize = SizeFromMemtype(curr->sym->mem_type, curr->is32);
    else
        psize = curr->sym->type->total_size;

    /* ADDR: the argument's address is to be pushed? */

    if (AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_ADDR) {
        asm_sym * sym;
        expr_list opndx;
        int j;

        i++;

        fullparam[0] = 0;
        for (j = i;;j++) {
            if ((AsmBuffer[j]->token == T_COMMA) || (AsmBuffer[j]->token == T_FINAL))
                break;
            if (j != i)
                strcat(fullparam," ");
            strcat(fullparam, AsmBuffer[j]->string_ptr);
        }
        j = i;
        if (EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR)
            return(ERROR);

        if (opndx.type == EXPR_REG || opndx.indirect == TRUE) {
            if (curr->is_far) {
                strcpy(buffer, " push ");
#if 0
                /* this is useless, as it is ignored by the assembler */
                if (curr->is32 != Use32) {
                    if (curr->is32)
                        strcat(buffer, "dword ptr ");
                    else
                        strcat(buffer, "word ptr ");
                }
#endif
                if (opndx.stackbased)
                    strcat(buffer, "ss");
                else if (opndx.override != EMPTY)
                    strcat(buffer, AsmBuffer[opndx.override]->string_ptr);
                else
                    strcat(buffer,"ds");
                InputQueueLine( buffer );
            }
            sprintf(buffer, " lea %s, %s", Use32 ? "eax" : "ax", fullparam);
            *eaxused = TRUE;
//          DebugMsg(("PushInvokeParam: %s\n", buffer));
            InputQueueLine( buffer );
            sprintf(buffer, " push %s", Use32 ? "eax" : "ax");
        } else {
            if (curr->is_far) {
                strcpy(buffer, " push ");
#if 0
                /* this is useless, as it is ignored by the assembler */
                if (curr->is32 != Use32) {
                    if (curr->is32)
                        strcat(buffer, "dword ptr ");
                    else
                        strcat(buffer, "word ptr ");
                }
#endif
                if (opndx.override != EMPTY) {
                    strcat(buffer, AsmBuffer[opndx.override]->string_ptr);
                } else if (opndx.sym != NULL && opndx.sym->segment != NULL) {
                    dir_node *dir = (dir_node *)opndx.sym->segment;
                    if (dir->e.seginfo->segtype == SEGTYPE_DATA ||
                        dir->e.seginfo->segtype == SEGTYPE_BSS)
                        strcat(buffer,"ds");
                    else
                        strcat(buffer,"cs");
                } else {
                    strcat(buffer,"seg ");
                    strcat(buffer, fullparam);
                }
                InputQueueLine(buffer);
            }
#if 0
            if (Use32 != curr->is32) {
                if (Use32)
                    sprintf(buffer, " push LOWWORD(offset %s)", fullparam);
                else
                    sprintf(buffer, " push DWORD PTR offset %s", fullparam);
            } else
#endif
                sprintf(buffer, " push offset %s", fullparam);
        }
        if (curr->is_vararg) {
            size_vararg = size_vararg + CurrWordSize;
            if (curr->is_far)
                size_vararg = size_vararg + CurrWordSize;
        }
    } else {
        struct asm_sym * sym;
        int asize;
        int pushsize;
        int j;
        bool useaddr = FALSE;
        expr_list opndx;

        for (j = i;;j++) {
            if ((AsmBuffer[j]->token == T_COMMA) || (AsmBuffer[j]->token == T_FINAL))
                break;
            if ((AsmBuffer[j]->token == T_UNARY_OPERATOR) && (AsmBuffer[j]->value == T_OFFSET))
                useaddr = TRUE;
            if (j == i)
                strcpy(fullparam,AsmBuffer[j]->string_ptr);
            else {
                strcat(fullparam," ");
                strcat(fullparam, AsmBuffer[j]->string_ptr);
            }
            //          DebugMsg(("PushInvokeParam: partial string[%u]=%s\n", j, AsmBuffer[j]->string_ptr));
#if 0
            if (AsmBuffer[j]->token == T_ID)
                i = j;
#endif
        }
        j = i;
        /* handle the <reg>::<reg> case here, the evaluator wont handle it */
        if (AsmBuffer[j]->token == T_REG &&
            AsmBuffer[j+1]->token == T_COLON &&
            AsmBuffer[j+2]->token == T_COLON &&
            AsmBuffer[j+3]->token == T_REG) {
            asize = SizeFromRegister(AsmBuffer[j]->value);
            if (asize == 0)
                asize = CurrWordSize;
            asize += SizeFromRegister(AsmBuffer[j+3]->value);
            sprintf(buffer, " push %s", AsmBuffer[j]->string_ptr);
            InputQueueLine( buffer );
            strcpy(fullparam, AsmBuffer[j+3]->string_ptr);
            opndx.type = EXPR_REG;
            opndx.indirect = FALSE;
            opndx.sym = NULL;
        } else {
            if (EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR) {
                return(ERROR);
            }
            /* for a simple register, get its size */
            if (opndx.type == EXPR_REG && opndx.indirect == FALSE) {
                asize = SizeFromRegister(AsmBuffer[opndx.base_reg]->value);
                if (asize == 0) /* segment register? */
                    asize = CurrWordSize;
            }
            else if (opndx.mem_type == MT_EMPTY)
                asize = psize;
            else if (opndx.mem_type != MT_TYPE)
                asize = SizeFromMemtype(opndx.mem_type, Use32);
            else
                asize = opndx.sym->type->total_size;
        }

        if (curr->is_vararg == TRUE)
            psize = asize;

        if (asize > psize) { /* argument's size too big */
            DebugMsg(("PushInvokeParm: error, arg size=%u, parm size=%u\n", asize, psize));
            AsmErr(INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1);
            return (NOT_ERROR);
        }

        sym = opndx.sym;

#ifdef DEBUG_OUT
        if (sym)
            DebugMsg(("PushInvokeParam: arg name=%s, asize=%u, psize=%u\n", sym->name, asize, psize));
        else
            DebugMsg(("PushInvokeParam: arg no name, asize=%u, psize=%u\n", asize, psize));
#endif
        pushsize = CurrWordSize;

        if ((opndx.type == EXPR_ADDR && opndx.instr != T_OFFSET) ||
            (opndx.type == EXPR_REG && opndx.indirect == TRUE)) {

            if (asize > pushsize) {
                char dw = ' ';
                if (( curr_cpu & P_CPU_MASK ) >= P_386 ) {
                    pushsize = 4;
                    dw = 'd';
                }
                if (curr->is_vararg)
                    size_vararg += asize;

                /* in params like "qword ptr [eax]" the typecast */
                /* has to be removed */
                if (opndx.explicit) {
                    SkipTypecast(fullparam, i);
                    opndx.explicit = FALSE;
                }

                while (asize > 0) {
#if 0
                    if (opndx.explicit) {
                        sprintf(buffer, " push %s", fullparam);
                        asize = 0;
                    } else if (asize & 2) {
#else
                    if (asize & 2) {
#endif
                        /* ensure the stack remains dword-aligned in 32bit */
                        if (pushsize == 4)
                            InputQueueLine( "sub esp,2" );

                        sprintf(buffer, " push word ptr %s+%u", fullparam, asize-2);
                        asize -= 2;
                    } else {
                        sprintf(buffer, " push %cword ptr %s+%u", dw, fullparam, asize-pushsize);
                        asize -= pushsize;
                    }
                    InputQueueLine( buffer );
                }
                return NOT_ERROR;
            } else if (asize < pushsize) {
                if (curr->is_vararg)
                    size_vararg += pushsize;
                //                    switch (sym->mem_type) {
                switch (opndx.mem_type) {
                case MT_BYTE:
                    if (pushsize == 2) {
                        sprintf(buffer, " mov al, %s", fullparam);
                        InputQueueLine( buffer );
                        strcpy(buffer, " mov ah, 0");
                        InputQueueLine( buffer );
                        strcpy(buffer, " push ax");
                    } else {
                        sprintf(buffer, " movzx eax, %s", fullparam);
                        InputQueueLine( buffer );
                        strcpy(buffer, " push eax");
                    }
                    *eaxused = TRUE;
                    break;
                case MT_SBYTE:
                    if (pushsize == 2) {
                        sprintf(buffer, " mov al, %s", fullparam);
                        InputQueueLine( buffer );
                        strcpy(buffer, " cbw");
                        InputQueueLine( buffer );
                        strcpy(buffer, " push ax");
                    } else {
                        sprintf(buffer, " movsx eax, %s", fullparam);
                        InputQueueLine( buffer );
                        strcpy(buffer, " push eax");
                    }
                    *eaxused = TRUE;
                    break;
                case MT_WORD:
                    sprintf(buffer, " movzx eax, %s", fullparam);
                    InputQueueLine( buffer );
                    strcpy(buffer, " push eax");
                    *eaxused = TRUE;
                    break;
                case MT_SWORD:
                    sprintf(buffer, " movsx eax, %s", fullparam);
                    InputQueueLine( buffer );
                    strcpy(buffer, " push eax");
                    *eaxused = TRUE;
                    break;
                default:
                    sprintf(buffer, " push %s", fullparam);
                }
            } else {
                if ((pushsize == 2) || (( curr_cpu & P_CPU_MASK ) >= P_386 ))
                    sprintf(buffer, " push %s", fullparam);
                else {
                    sprintf(buffer, " push word ptr %s+2", AsmBuffer[i]->string_ptr);
                    InputQueueLine( buffer );
                    sprintf(buffer, " push word ptr %s", AsmBuffer[i]->string_ptr);
                }
                if (curr->is_vararg)
                    size_vararg += pushsize;
            }
        } else { /* the parameter is a register or constant value! */
            char * qual = "";
            if (opndx.type == EXPR_REG) {
                if (asize != psize)
                    AsmErr(INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1);
                if (*eaxused)
                    if (AsmBuffer[opndx.base_reg]->value == T_EAX) {
                        AsmErr(REGISTER_VALUE_OVERWRITTEN_BY_INVOKE);
                        *eaxused = FALSE;
                    }
            } else {
                asize = CurrWordSize;
                if (psize < pushsize)  /* ensure that the default pushsize is met */
                    psize = pushsize;
                if (asize != psize)
                    if (psize == 2)
                        qual = "word ptr ";
                    else if (( curr_cpu & P_CPU_MASK ) >= P_386 )
                        qual = "dword ptr ";
                    else {
                        sprintf(buffer, " push HIGHWORD %s", fullparam);
                        InputQueueLine( buffer );
                        qual = "LOWWORD ";
                    }
            }
            sprintf(buffer, " push %s%s", qual, fullparam);
            if (curr->is_vararg)
                size_vararg += psize;
        }
    }
    InputQueueLine( buffer );
    return NOT_ERROR;
}

// generate a call for a prototyped procedure

int InvokeDef( int i )
/******************/
{
    struct asm_sym      *sym;
    dir_node            *dir;
    char                *name;
    char                *param;
    int                 numParam;
    int                 namepos = i;
    bool                eaxused = FALSE;
    bool                uselabel = FALSE;
    proc_info   *info;
    label_list  *curr;
    expr_list   opndx;
    char        buffer[MAX_LINE_LEN];

    /* the call address might be an expression! */

    DebugMsg(("InvokeDef enter\n"));

    /* if there is more than 1 item describing the invoke target,
     use the expression evaluator to get it
     */
    if (AsmBuffer[i+1]->token != T_COMMA && AsmBuffer[i+1]->token != T_FINAL) {
        if (ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ))
            return (ERROR);
        DebugMsg(("InvokeDef: target is expression, opndx->sym=%X, opndx->mbr=%X, opndx->assume=%X\n", opndx.sym, opndx.mbr, opndx.assume));
        if (opndx.mbr != NULL) {
            sym = opndx.mbr;
            // it may be a typecast. then the mbr member contains the explicit type
            // and sym->state is SYM_TYPE
            if (sym->state == SYM_TYPE) {
                dir = (dir_node *)sym;
                if (opndx.label != EMPTY)
                    uselabel = TRUE;
                goto isfnptr;
            }
        } else if (opndx.type == EXPR_ADDR &&
                   opndx.sym != NULL &&
                   opndx.sym->mem_type == MT_TYPE &&
                   opndx.sym->type->mem_type == MT_PTR) {
            sym = opndx.sym;
        } else {
            AsmErr( INVOKE_REQUIRES_PROTOTYPE );
            return (ERROR);
        }
    } else {
        name = AsmBuffer[i++]->string_ptr;
        sym = SymSearch( name );
    }

    if( sym == NULL ) {
        AsmErr( SYMBOL_NOT_DEFINED, name );
        return( ERROR );
    }
    if( sym->state == SYM_PROC )  /* the most simple case: symbol is a PROC */
        ;
    else if ((sym->mem_type == MT_TYPE) && (sym->type->mem_type == MT_PTR)) {
        /* second case: symbol is a (function?) pointer */
        dir = (dir_node *)sym->type;
        /* get the pointer target */
    isfnptr:
        dir = (dir_node *)dir->e.structinfo->target;
        /* pointer target must be a PROTO typedef */
        if (dir == NULL || dir->sym.mem_type != MT_PROC) {
            DebugMsg(("InvokeDef: sym->name=%s, sym->type=%X\n", sym->name, sym->type));
            AsmErr( INVOKE_REQUIRES_PROTOTYPE );
            return( ERROR );
        }
        sym = dir->e.structinfo->target;

    } else {
#ifdef DEBUG_OUT
        if (sym->mem_type == MT_TYPE)
            DebugMsg(("InvokeDef: symbol name=%s, state=%u, type=%s, memtype=%u, [type memtype=%u]\n", sym->name, sym->state, sym->type->name, sym->mem_type, sym->type->mem_type));
        else
            DebugMsg(("InvokeDef: symbol name=%s, state=%u, type=%X, memtype=%u\n", sym->name, sym->state, sym->type, sym->mem_type));
#endif
        AsmErr( INVOKE_REQUIRES_PROTOTYPE );
        return( ERROR );
    }
    dir = (dir_node *)sym;
    info = dir->e.procinfo;

    if (dir->sym.langtype == LANG_WATCOM_C) {
        AsmError(LANG_CONV_NOT_SUPPORTED);
        return(ERROR);
    }
    /* get the number of parameters */

    for (curr = info->paralist, numParam = 0 ; curr ; curr = curr->next, numParam++);
    DebugMsg(("PushInvokeParam: numparams=%u\n", numParam));

    curr = info->paralist;

    PushLineQueue();

    if (!(info->is_vararg)) {
        /* check if there is a superfluous parameter in the INVOKE call */
        if (PushInvokeParam( NULL, i, numParam, &eaxused) != ERROR) {
            DebugMsg(("PushInvokeParam: superfluous argument, i=%u\n", i));
            AsmErr(TOO_MANY_ARGUMENTS_TO_INVOKE);
            return( ERROR );
        }
    } else {
        int j = (Token_Count - i) / 2;
        /* for VARARG procs, just push the additional params with
         the VARARG descriptor
        */
        numParam--;
        size_vararg = 0; /* reset the VARARG parameter size count */
        DebugMsg(("PushInvokeParam: VARARG proc, numparams=%u, actual (max) params=%u, parasize=%u\n", numParam, j, info->parasize));
        for (;j >= numParam; j--)
            PushInvokeParam( curr, i, j, &eaxused);
        /* VARARG procs have at least 1 param, the VARARG param */
        curr = curr->next;
    }

    /* the parameters are always stored in "push" order */

    if (sym->langtype == LANG_STDCALL ||
        sym->langtype == LANG_C ||
        sym->langtype == LANG_WATCOM_C ||
        sym->langtype == LANG_SYSCALL) {
        for ( ; curr ; curr = curr->next) {
            numParam--;
            if (PushInvokeParam( curr, i, numParam, &eaxused) == ERROR) {
                DebugMsg(("PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                AsmErr(TOO_FEW_ARGUMENTS_TO_INVOKE);
            }
        }
    } else {
        for (numParam = 0 ; curr ; curr = curr->next, numParam++) {
            if ( PushInvokeParam( curr, i, numParam, &eaxused) == ERROR) {
                DebugMsg(("PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                AsmErr(TOO_FEW_ARGUMENTS_TO_INVOKE);
            }
        }
    }

    strcpy(buffer, " call ");
    if (uselabel)
        strcat(buffer, AsmBuffer[opndx.label]->string_ptr);
    else
        for (;(AsmBuffer[namepos]->token != T_COMMA) && (AsmBuffer[namepos]->token != T_FINAL);namepos++) {
            if (buffer[6] != '\0' && is_valid_id_char(*(AsmBuffer[namepos]->string_ptr)))
                strcat(buffer," ");
            strcat(buffer, AsmBuffer[namepos]->string_ptr);
        }
    InputQueueLine( buffer );

    if ((sym->langtype == LANG_C || sym->langtype == LANG_SYSCALL) &&
        (info->parasize || (info->is_vararg && size_vararg))) {
        if (info->is_vararg) {
            DebugMsg(("PushInvokeParam: size of fix args=%u, var args=%u\n", info->parasize, size_vararg));
            sprintf(buffer, " add %s, %u", Use32 ? "esp" : "sp", info->parasize + size_vararg);
        } else
            sprintf(buffer, " add %s, %u", Use32 ? "esp" : "sp", info->parasize);
        InputQueueLine( buffer );
    }

    return( NOT_ERROR );
}

