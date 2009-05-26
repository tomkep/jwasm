/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  Processing of INVOKE directive.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "expreval.h"
#include "directiv.h"
#include "input.h"
#include "queues.h"
#include "equate.h"
#include "mangle.h"
#include "assume.h"
#include "segment.h"
#include "fastpass.h"
#include "listing.h"

static int size_vararg;

#if INVOKE_WC

// watcom_c calling convention doesn't work yet with INVOKE!

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
        if( ModuleInfo.Use32 ) {
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
        if( ModuleInfo.Use32 ) {
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
        if( ModuleInfo.Use32 ) {
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

// get size from register (for PUSH)

int SizeFromRegister( int registertoken )
/******************************/
{
    int flags = AsmOpTable[AsmResWord[registertoken].position].opnd_type[1];
    if ( flags & OP_R32 )
        return( 4 );
    else if ( flags & OP_R16 )
        return( 2 );
    else if ( flags & OP_R8 )
        return( 1 );
    else if ( flags & OP_SR )
        return( 0 );
    else if ( flags & OP_ST )
        return( 10 );
    else if ( flags & OP_MMX )
        return( 8 );
    else if ( flags & OP_XMM )
        return( 16 ); /* masm v6x and v7 return 10, v8 returns 16 */
    else /* CRx, DRx, TRx ... */
        return( 4 );
}

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

/*
  push one parameter of a procedure called with INVOKE onto the stack
  this is an internal macro which works with text only!
  curr    : the parameter of the PROC to call
  i       : the AsmBuffer index of the start of the parameter list
  reqParam: the index of the parameter which is to be pushed
  psize,asize: size of parameter/argument in bytes.
*/

static int PushInvokeParam( dir_node *curr, int i, int reqParam, bool procuse32, bool * eaxused)
{
    int currParm;
    int psize;
    int asize;
    int pushsize;
    int j;
    int fptrsize;
    bool addr = FALSE; /* ADDR operator found */
    struct asm_sym * sym;
    expr_list opndx;
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
    if ( curr->is_ptr ) {
        psize = curr->is32 ? 4 : 2;
        if ( curr->is_far )
            psize += 2;
    } else if ( curr->sym.mem_type != MT_TYPE )
        psize = SizeFromMemtype(curr->sym.mem_type, curr->is32);
    else
        psize = curr->sym.type->total_size;

    DebugMsg(("PushInvokeParam(%u): is_ptr=%u, memtype=%X, psize=%u\n", reqParam, curr->is_ptr, curr->sym.mem_type, psize ));

    /* ADDR: the argument's address is to be pushed? */
    if (AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_ADDR) {
        addr = TRUE;
        i++;
    }

    /* get the full parameter */
    fullparam[0] = NULLC;
    for ( j = i;
          AsmBuffer[j]->token != T_COMMA && AsmBuffer[j]->token != T_FINAL;
          j++ ) {
        if (j != i)
            strcat( fullparam," " );
        strcat( fullparam, AsmBuffer[j]->string_ptr );
    }

    j = i;
    fptrsize = procuse32 ? 6 : 4;

    if ( addr ) {
        if (EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR)
            return(ERROR);

        /* DWORD (16bit) and FWORD(32bit) are treated like FAR ptrs */
        if ( psize > fptrsize ) {
            /* QWORD is NOT accepted as a FAR ptr */
            AsmErr(INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1);
            return(NOT_ERROR);
        }

        if (opndx.kind == EXPR_REG || opndx.indirect ) {
            if (curr->is_far || psize == fptrsize ) {
                strcpy(buffer, " push ");
#if 0
                /* this is useless, as it is ignored by the assembler */
                if (curr->is32 != ModuleInfo.Use32) {
                    if (curr->is32)
                        strcat(buffer, "dword ptr ");
                    else
                        strcat(buffer, "word ptr ");
                }
#endif
                if ( opndx.sym && opndx.sym->state == SYM_STACK )
                    strcat(buffer, "ss");
                else if (opndx.override != EMPTY)
                    strcat(buffer, AsmBuffer[opndx.override]->string_ptr);
                else
                    strcat(buffer,"ds");
                AddLineQueue( buffer );
            }
            sprintf(buffer, " lea %s, %s", ModuleInfo.Use32 ? "eax" : "ax", fullparam);
            *eaxused = TRUE;
//          DebugMsg(("PushInvokeParam: %s\n", buffer));
            AddLineQueue( buffer );
            sprintf(buffer, " push %s", ModuleInfo.Use32 ? "eax" : "ax");
        } else {
        push_address:
            if (curr->is_far || psize == fptrsize ) {
                strcpy(buffer, " push ");

                if (opndx.override != EMPTY) {
                    strcat(buffer, AsmBuffer[opndx.override]->string_ptr);
                } else if (opndx.sym != NULL && opndx.sym->segment != NULL) {
                    dir_node *dir = GetSeg(opndx.sym);
                    enum assume_segreg as;
                    if (dir->e.seginfo->segtype == SEGTYPE_DATA ||
                        dir->e.seginfo->segtype == SEGTYPE_BSS)
                        as = search_assume( (asm_sym *)dir, ASSUME_DS, TRUE );
                    else
                        as = search_assume( (asm_sym *)dir, ASSUME_CS, TRUE );
                    if ( as != ASSUME_NOTHING ) {
                        char *p;
                        switch (as) {
                        case ASSUME_DS:
                            p = "ds";
                            break;
                        case ASSUME_CS:
                            p = "cs";
                            break;
                        case ASSUME_SS:
                            p = "ss";
                            break;
                        case ASSUME_ES:
                            p = "es";
                            break;
                        case ASSUME_FS:
                            p = "fs";
                            break;
                        default:
                            p = "gs";
                            break;
                        }
                        strcat( buffer, p );
                    } else {
                        struct asm_sym *seg;
                        seg = GetGrp( opndx.sym );
                        if (seg == NULL)
                            seg = &dir->sym;
                        if ( seg )
                            strcat( buffer, seg->name );
                        else {
                            strcat( buffer, "seg ");
                            strcat(buffer, fullparam);
                        }
                    }
                } else {
                    strcat(buffer,"seg ");
                    strcat(buffer, fullparam);
                }
                AddLineQueue(buffer);
            }
#if 0
            if ( ModuleInfo.Use32 != curr->is32 ) {
                if ( ModuleInfo.Use32 )
                    /* better use PUSHW/PUSHD! */
                    sprintf(buffer, " push LOWWORD(offset %s)", fullparam);
                else
                    sprintf(buffer, " push DWORD PTR offset %s", fullparam);
            } else
#endif
                sprintf(buffer, " push offset %s", fullparam);
        }
        if ( curr->is_vararg ) {
            size_vararg = size_vararg + CurrWordSize;
            if ( curr->is_far )
                size_vararg = size_vararg + CurrWordSize;
        }
    } else { /* ! ADDR branch */

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
            AddLineQueue( buffer );
            strcpy(fullparam, AsmBuffer[j+3]->string_ptr);
            opndx.kind = EXPR_REG;
            opndx.indirect = FALSE;
            opndx.sym = NULL;
            opndx.base_reg = j+3; /* for error msg 'eax overwritten...' */
        } else {
            if ( EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR) {
                return( ERROR );
            }
            /* for a simple register, get its size */
            if ( opndx.kind == EXPR_REG && opndx.indirect == FALSE ) {
                asize = SizeFromRegister( AsmBuffer[opndx.base_reg]->value );
                if ( asize == 0 ) /* segment register? */
                    asize = CurrWordSize;
            } else if ( opndx.mem_type == MT_EMPTY )
                asize = psize;
            else if ( opndx.mem_type != MT_TYPE ) {
                if ( opndx.kind == EXPR_ADDR &&
                     opndx.indirect == FALSE &&
                     opndx.sym &&
                     opndx.instr == EMPTY &&
                     (opndx.mem_type == MT_PROC ||
                      opndx.mem_type == MT_FAR ||
                      opndx.mem_type == MT_NEAR))
                    goto push_address;
                asize = SizeFromMemtype( opndx.mem_type, ModuleInfo.Use32 );
            } else {
                if ( opndx.sym != NULL )
                    asize = opndx.sym->type->total_size;
                else
                    asize = opndx.mbr->type->total_size;
            }
        }

        if (curr->is_vararg == TRUE)
            psize = asize;

        if (asize > psize) { /* argument's size too big */
            DebugMsg(("PushInvokeParm(%u): error, arg size=%u, parm size=%u\n", reqParam, asize, psize));
            AsmErr(INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1);
            return (NOT_ERROR);
        }

        sym = opndx.sym;

#ifdef DEBUG_OUT
        if (sym)
            DebugMsg(("PushInvokeParam(%u): arg name=%s, asize=%u, psize=%u\n", reqParam, sym->name, asize, psize));
        else
            DebugMsg(("PushInvokeParam(%u): arg no name, asize=%u, psize=%u\n", reqParam, asize, psize));
#endif
        pushsize = CurrWordSize;

        if ((opndx.kind == EXPR_ADDR && opndx.instr != T_OFFSET) ||
            (opndx.kind == EXPR_REG && opndx.indirect == TRUE)) {

            if (asize > pushsize) {
                char dw = ' ';
                if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
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
                            AddLineQueue( "sub esp,2" );

                        sprintf(buffer, " push word ptr %s+%u", fullparam, asize-2);
                        asize -= 2;
                    } else {
                        sprintf(buffer, " push %cword ptr %s+%u", dw, fullparam, asize-pushsize);
                        asize -= pushsize;
                    }
                    AddLineQueue( buffer );
                }
                return NOT_ERROR;
            } else if (asize < pushsize) {
                if (curr->is_vararg)
                    size_vararg += pushsize;
                if ( psize > 4 )
                    AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                //switch (sym->mem_type) {
                switch ( opndx.mem_type ) {
                case MT_BYTE:
                    if (pushsize == 2) {
                        if ( psize == 4 )
                            AddLineQueue( "push 0" );
                        sprintf(buffer, " mov al, %s", fullparam);
                        AddLineQueue( buffer );
                        strcpy(buffer, " mov ah, 0");
                        AddLineQueue( buffer );
                        strcpy(buffer, " push ax");
                    } else {
                        sprintf(buffer, " movzx eax, %s", fullparam);
                        AddLineQueue( buffer );
                        strcpy(buffer, " push eax");
                    }
                    *eaxused = TRUE;
                    break;
                case MT_SBYTE:
                    if (pushsize == 2) {
                        if ( psize == 4 )
                            AddLineQueue( "push 0" );
                        sprintf(buffer, " mov al, %s", fullparam);
                        AddLineQueue( buffer );
                        strcpy(buffer, " cbw");
                        AddLineQueue( buffer );
                        strcpy(buffer, " push ax");
                    } else {
                        sprintf(buffer, " movsx eax, %s", fullparam);
                        AddLineQueue( buffer );
                        strcpy(buffer, " push eax");
                    }
                    *eaxused = TRUE;
                    break;
                case MT_WORD:
                    sprintf(buffer, " movzx eax, %s", fullparam);
                    AddLineQueue( buffer );
                    strcpy(buffer, " push eax");
                    *eaxused = TRUE;
                    break;
                case MT_SWORD:
                    sprintf(buffer, " movsx eax, %s", fullparam);
                    AddLineQueue( buffer );
                    strcpy(buffer, " push eax");
                    *eaxused = TRUE;
                    break;
                default:
                    sprintf(buffer, " push %s", fullparam);
                }
            } else { /* asize == pushsize */
                if ((pushsize == 2) || (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ))
                    sprintf(buffer, " push %s", fullparam);
                else {
                    sprintf(buffer, " push word ptr %s+2", AsmBuffer[i]->string_ptr);
                    AddLineQueue( buffer );
                    sprintf(buffer, " push word ptr %s", AsmBuffer[i]->string_ptr);
                }
                if (curr->is_vararg)
                    size_vararg += pushsize;
            }
        } else { /* the parameter is a register or constant value! */
            char * qual = "";
            if ( opndx.kind == EXPR_REG ) {
                if (asize != psize || asize < pushsize) {
                    /* if argument is a 16bit register, just push the full 32bit */
                    if (asize == 2 && (psize == 4 || pushsize == 4)) {
                        qual = "e";
                    } else if ( asize == 1 && (psize <= 4) ) {
                        /* the argument is an 8bit register */
                        /* add a movzx if psize is > 1 */
                        if ( psize <= 2 && pushsize == 2 ) {
                            if (*(fullparam+1) == 'h' || *(fullparam+1) == 'H') {
                                if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386) {
                                    sprintf( buffer, " movzx %cx, %ch", *fullparam, *fullparam );
                                } else {
                                    sprintf( buffer, " mov %cl, %ch", *fullparam, *fullparam );
                                    AddLineQueue( buffer );
                                    sprintf( buffer, " mov %ch, 0", *fullparam );
                                }
                                AddLineQueue( buffer );
                            }
                            sprintf( fullparam, "%cx", *fullparam);
                        } else {
                            if ( psize > 1 ) {
                                sprintf(buffer, " movzx e%cx, %s", *fullparam, fullparam);
                                AddLineQueue( buffer );
                            }
                            sprintf( fullparam, "e%cx", *fullparam);
                        }
                    } else {
                        AsmErr(INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1);
                    }
                }
                if ( *eaxused )
                    if (AsmBuffer[opndx.base_reg]->value == T_EAX ||
                        AsmBuffer[opndx.base_reg]->value == T_AX) {
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
                    else if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 )
                        qual = "dword ptr ";
                    else {
                        sprintf(buffer, " push HIGHWORD (%s)", fullparam);
                        AddLineQueue( buffer );
                        qual = "LOWWORD (";
                        strcat( fullparam, ")");
                    }
            }
            sprintf(buffer, " push %s%s", qual, fullparam);
            if (curr->is_vararg)
                size_vararg += psize;
        }
    }
    AddLineQueue( buffer );
    return NOT_ERROR;
}

// generate a call for a prototyped procedure

ret_code InvokeDirective( int i )
/******************/
{
    struct asm_sym      *sym;
    dir_node            *dir;
    char                *name;
    //char                *param;
    int                 numParam;
    int                 namepos = i;
    bool                eaxused = FALSE;
    bool                uselabel = FALSE;
    bool                procuse32;
    proc_info   *info;
    dir_node    *curr;
    expr_list   opndx;
    char        buffer[MAX_LINE_LEN];

    /* the call address might be an expression! */

    DebugMsg(("InvokeDef(%s) enter\n", AsmBuffer[i]->pos ));

#if FASTPASS
    /* make sure the directive is stored */
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif
    /* if there is more than 1 item describing the invoke target,
     use the expression evaluator to get it
     */
    if (AsmBuffer[i+1]->token != T_COMMA && AsmBuffer[i+1]->token != T_FINAL) {
        if (ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ))
            return (ERROR);
        DebugMsg(("InvokeDef: target is expression, opndx->sym=%X, opndx->mbr=%X, opndx->type=%X\n", opndx.sym, opndx.mbr, opndx.type ));
#if 1
        /* a typecast with PTR? Since v1.95, this has highest priority */
        if (opndx.explicit == TRUE && opndx.type != NULL && opndx.type->state == SYM_TYPE ) {
            sym = opndx.type;
            dir = (dir_node *)sym;
            if (opndx.label != EMPTY)
                uselabel = TRUE;
            if ( dir->sym.mem_type == MT_PROC )  /* added for v1.95 */
                goto isfnproto;
            goto isfnptr;
#endif
        } else if (opndx.mbr != NULL) {
            sym = opndx.mbr;
            // it may be a typecast. then the mbr member contains the explicit type
            // and sym->state is SYM_TYPE
            if (sym->state == SYM_TYPE) {
                dir = (dir_node *)sym;
                if (opndx.label != EMPTY)
                    uselabel = TRUE;
                goto isfnptr;
            }
        } else if (opndx.kind == EXPR_ADDR &&
                   opndx.sym != NULL &&
                   (opndx.sym->state == SYM_PROC ||
                   (opndx.sym->mem_type == MT_TYPE &&
                   opndx.sym->type->mem_type == MT_PTR))) {
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
    isfnproto:
        if (dir == NULL || dir->sym.mem_type != MT_PROC) {
#ifdef DEBUG_OUT
            if ( sym )
                DebugMsg(("InvokeDef: sym->name=%s, sym->type=%X\n", sym->name, sym->type));
            else
                DebugMsg(("InvokeDef: sym=NULL, dir=%X\n", dir ));
#endif
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
    procuse32 = sym->use32;

    if (dir->sym.langtype == LANG_WATCOM_C) {
        AsmError(LANG_CONV_NOT_SUPPORTED);
        return(ERROR);
    }
    /* get the number of parameters */

    for (curr = info->paralist, numParam = 0 ; curr ; curr = curr->nextparam, numParam++);
    DebugMsg(("PushInvokeParam: numparams=%u\n", numParam));

    curr = info->paralist;

    PushLineQueue();

    if (!(info->is_vararg)) {
        /* check if there is a superfluous parameter in the INVOKE call */
        if (PushInvokeParam( NULL, i, numParam, procuse32, &eaxused) != ERROR) {
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
            PushInvokeParam( curr, i, j, procuse32, &eaxused);
        /* VARARG procs have at least 1 param, the VARARG param */
        curr = curr->nextparam;
    }

    /* the parameters are always stored in "push" order */

    if (sym->langtype == LANG_STDCALL ||
        sym->langtype == LANG_C ||
        sym->langtype == LANG_WATCOM_C ||
        sym->langtype == LANG_SYSCALL) {
        for ( ; curr ; curr = curr->nextparam ) {
            numParam--;
            if (PushInvokeParam( curr, i, numParam, procuse32, &eaxused) == ERROR) {
                DebugMsg(("PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                AsmErr( TOO_FEW_ARGUMENTS_TO_INVOKE, sym->name );
            }
        }
    } else {
        for ( numParam = 0 ; curr ; curr = curr->nextparam, numParam++ ) {
            if ( PushInvokeParam( curr, i, numParam, procuse32, &eaxused ) == ERROR) {
                DebugMsg(("PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                AsmErr( TOO_FEW_ARGUMENTS_TO_INVOKE, sym->name );
            }
        }
    }

    strcpy( buffer, " call " );
    if ( uselabel )
        strcat( buffer, AsmBuffer[opndx.label]->string_ptr );
    else
        for ( ; (AsmBuffer[namepos]->token != T_COMMA) && (AsmBuffer[namepos]->token != T_FINAL); namepos++ ) {
            if ( buffer[6] != '\0' && is_valid_id_char( *(AsmBuffer[namepos]->string_ptr ) ) )
                strcat( buffer," " );
            strcat( buffer, AsmBuffer[namepos]->string_ptr );
        }
    AddLineQueue( buffer );

    if (( sym->langtype == LANG_C || sym->langtype == LANG_SYSCALL ) &&
        ( info->parasize || ( info->is_vararg && size_vararg ) )) {
        if (info->is_vararg) {
            DebugMsg(("PushInvokeParam: size of fix args=%u, var args=%u\n", info->parasize, size_vararg));
            sprintf( buffer, " add %s, %u", ModuleInfo.Use32 ? "esp" : "sp", info->parasize + size_vararg );
        } else
            sprintf( buffer, " add %s, %u", ModuleInfo.Use32 ? "esp" : "sp", info->parasize );
        AddLineQueue( buffer );
    }

    LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    RunLineQueue();

    return( NOT_ERROR );
}

