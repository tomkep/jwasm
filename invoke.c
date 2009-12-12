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
#include "assume.h"
#include "segment.h"
#include "fastpass.h"
#include "listing.h"

static int size_vararg;
static int fcscratch;

struct fastcall_conv {
    void (* invokestart)( dir_node *, int, char * );
    void (* invokeend)( dir_node *, int, char * );
    int  (* handleparam)( dir_node *, int, dir_node *, bool, expr_list *, char *, char *, bool * );
};

static void ms32_fcstart( dir_node *, int, char * );
static void ms32_fcend( dir_node *, int, char * );
static  int ms32_param( dir_node *, int, dir_node *, bool, expr_list *, char *, char *, bool * );
#if AMD64_SUPPORT
static void ms64_fcstart( dir_node *, int, char * );
static void ms64_fcend( dir_node *, int, char * );
static  int ms64_param( dir_node *, int, dir_node *, bool, expr_list *, char *, char *, bool * );
#endif

static const struct fastcall_conv fastcall_tab[] = {
 { ms32_fcstart, ms32_fcend , ms32_param },
#if INVOKE_WC
 { watc_fcstart, watc_fcend , watc_param },
#else
 { NULL, NULL , NULL },
#endif
#if AMD64_SUPPORT
    { ms64_fcstart, ms64_fcend , ms64_param }
#endif
};

static const char reg_prefix[] = { ' ', 'e', 'r' };
static const char * const ms32_regs[] = {"cx", "dx" };
#if AMD64_SUPPORT
static const char * const ms64_regs[] = {
 "rcx", "rdx",  "r8",  "r9",
 "ecx", "edx", "r8d", "r9d",
  "cx",  "dx", "r8w", "r9w",
  "cl",  "dl", "r8b", "r9b"
};
#endif

// get size of a register (for PUSH)

int SizeFromRegister( int registertoken )
/***************************************/
{
    int flags = GetOpndType( registertoken, 1 );
    if ( flags & OP_R32 )
        return( 4 );
#if AMD64_SUPPORT
    else if ( flags & OP_R64 )
        return( 8 );
#endif
    else if ( flags & OP_R16 )
        return( 2 );
    else if ( flags & OP_R8 )
        return( 1 );
    else if ( flags & OP_SR )
        return( CurrWordSize );
    else if ( flags & OP_ST )
        return( 10 );
    else if ( flags & OP_MMX )
        return( 8 );
    else if ( flags & OP_XMM )
        return( 16 ); /* masm v6x and v7 return 10, v8 returns 16 */
    else {/* CRx, DRx, TRx */
#if AMD64_SUPPORT
        if ( ModuleInfo.Ofssize == USE64 )
            return( 8 );
#endif
        return( 4 );
    }
}

static void ms32_fcstart( dir_node *proc, int numparams, char *buffer )
/*********************************************************************/
{
    /* nothing to do */
    fcscratch = 2;
    return;
}

static void ms32_fcend( dir_node *proc, int numparams, char *buffer )
/*******************************************************************/
{
    /* nothing to do */
    return;
}

static int ms32_param( dir_node *proc, int index, dir_node *param, bool addr, expr_list *opndx, char *buffer, char *paramvalue, bool *r0used )
/********************************************************************************************************************************************/
{
    if ( param->sym.state != SYM_TMACRO || fcscratch == 0 )
        return( 0 );
    fcscratch--;
    if ( addr )
        sprintf( buffer, " lea %c%s, %s", reg_prefix[ModuleInfo.Ofssize], ms32_regs[fcscratch], paramvalue );
    else
        sprintf( buffer, " mov %c%s, %s", reg_prefix[ModuleInfo.Ofssize], ms32_regs[fcscratch], paramvalue );
    AddLineQueue( buffer );
    return( 1 );
}

#if AMD64_SUPPORT
static void ms64_fcstart( dir_node *proc, int numparams, char *buffer )
/*********************************************************************/
{
    if ( numparams < 4 )
        numparams = 4;
    else if ( numparams & 1 )
        numparams++;
    sprintf( buffer, " sub rsp, %d", numparams * 8 );
    AddLineQueue( buffer );
    return;
}

static void ms64_fcend( dir_node *proc, int numparams, char *buffer )
/*******************************************************************/
{
    if ( numparams < 4 )
        numparams = 4;
    else if ( numparams & 1 )
        numparams++;
    sprintf( buffer, " add rsp, %d", numparams * 8 );
    AddLineQueue( buffer );
    return;
}

/*
 * parameter for Win64 FASTCALL.
 * the first 4 parameters are hold in registers: rcx, rdx, r8, r9 for non-float arguments,
 * xmm0, xmm1, xmm2, xmm3 for float arguments. If parameter size is > 8, the address of
 * the argument is used instead of the value.
 */

static int ms64_param( dir_node *proc, int index, dir_node *param, bool addr, expr_list *opndx, char *buffer, char *paramvalue, bool *r0used )
/********************************************************************************************************************************************/
{
    int size;
    int i;
    char *pstr;
    char regname[16];

    if ( index >= 4 ) {
        if ( opndx->mem_type == MT_EMPTY && addr == FALSE ) {
            switch ( SizeFromMemtype( param->sym.mem_type, USE64 ) ) {
            case 1:   pstr = "byte"; break;
            case 2:   pstr = "word"; break;
            case 4:   pstr = "dword"; break;
            default:  pstr = "qword"; break;
            }
            sprintf( buffer, " mov %s ptr [rsp+%u], %s", pstr, index*8, paramvalue );
        } else {
            if ( addr == FALSE ) {
                if ( opndx->indirect == FALSE && opndx->base_reg != EMPTY ) {
                    i = AsmBuffer[opndx->base_reg]->value;
                    pstr = regname;
                    memcpy( pstr, AsmResWord[i].name, AsmResWord[i].len );
                    *(pstr+AsmResWord[i].len) = NULLC;
                } else {
                    size = SizeFromMemtype( opndx->mem_type, USE64 );
                    switch ( size ) {
                    case 1:  pstr =  "al"; break;
                    case 2:  pstr =  "ax"; break;
                    case 4:  pstr = "eax"; break;
                    default: pstr = "rax"; break;
                    }
                    if ( size <= 8 )
                        sprintf( buffer, " mov %s, %s", pstr, paramvalue );
                    else
                        sprintf( buffer, " lea rax, %s", paramvalue );
                    *r0used = TRUE;
                }
            } else {
                sprintf( buffer, " lea rax, %s", paramvalue );
                *r0used = TRUE;
            }
            AddLineQueue( buffer );
            sprintf( buffer, " mov [rsp+%u], %s", index*8, pstr );
        }
    } else if ( addr || param->sym.total_size > 8 ) {
        sprintf( buffer, " lea %s, %s", ms64_regs[index], paramvalue );
    } else if ( param->sym.mem_type == MT_REAL4  ) {
        sprintf( buffer, " movd xmm%u, %s", index, paramvalue );
    } else if ( param->sym.mem_type == MT_REAL8 ) {
        sprintf( buffer, " movq xmm%u, %s", index, paramvalue );
    } else {
        if ( opndx->indirect == FALSE && opndx->base_reg != EMPTY ) {
            i = AsmBuffer[opndx->base_reg]->value;
            size = SizeFromRegister( i );
        } else {
            if ( opndx->mem_type != MT_EMPTY )
                size = SizeFromMemtype( opndx->mem_type, USE64 );
            else {
                size = SizeFromMemtype( param->sym.mem_type, USE64 );
            }
        }
        DebugMsg(("ms64_param(%s, param=%u): size=%u\n", proc->sym.name, index, size ));
        switch ( size ) {
        case 1: index += 4;
        case 2: index += 4;
        case 4: index += 4;
        default:
            if ( _stricmp( ms64_regs[index], paramvalue ) == 0 )
                return ( 1 );
            sprintf( buffer, " mov %s, %s", ms64_regs[index], paramvalue );
        }
    }
    AddLineQueue( buffer );
    return( 1 );
}

#endif

#if INVOKE_WC

// FASTCALL for OW register calling convention doesn't work yet with INVOKE!

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
        if( ModuleInfo.Ofssize ) {
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
        if( ModuleInfo.Ofssize ) {
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
        if( ModuleInfo.Ofssize ) {
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

static void SkipTypecast(char * fullparam, int i)
/***********************************************/
{
    int j;
    fullparam[0] = NULLC;
    for (j = i;;j++) {
        if (( AsmBuffer[j]->token == T_COMMA ) || ( AsmBuffer[j]->token == T_FINAL ) )
            break;
        if (( AsmBuffer[j+1]->token == T_RES_ID ) && ( AsmBuffer[j+1]->value == T_PTR ) )
            j = j + 1;
        else {
            if ( fullparam[0] != NULLC )
                strcat( fullparam," " );
            strcat( fullparam, AsmBuffer[j]->string_ptr );
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

static int PushInvokeParam( dir_node *proc, dir_node *curr, int i, int reqParam, uint_8 Ofssize, bool * r0used)
/*************************************************************************************************************/
{
    int currParm;
    int psize;
    int asize;
    int pushsize;
    int j;
    int fptrsize;
    bool addr = FALSE; /* ADDR operator found */
    //struct asm_sym * sym;
    expr_list opndx;
    char fullparam[MAX_LINE_LEN];
    char buffer[MAX_LINE_LEN];

    DebugMsg(("PushInvokeParam(%s, param=%s:%u, i=%u ) enter\n", proc->sym.name, curr ? curr->sym.name : "NULL", reqParam, i ));
    for ( currParm = 0; currParm <= reqParam; ) {
        if ( AsmBuffer[i]->token == T_FINAL ) { /* this is no real error! */
            DebugMsg(("PushInvokeParam(%s): T_FINAL token, i=%u\n", proc->sym.name, i));
            return ERROR;
        }
        if ( AsmBuffer[i]->token == T_COMMA ) {
            currParm++;
        }
        i++;
    }
    /* if curr is NULL this call is just a parameter check */
    if (!curr) return (NOT_ERROR);

    /* set psize (size of parameter) */
    if ( curr->is_ptr ) {
        psize = 2 << curr->is32;
        if ( curr->is_far )
            psize += 2;
    } else if ( curr->sym.mem_type != MT_TYPE )
        psize = SizeFromMemtype( curr->sym.mem_type, curr->is32 );
    else
        psize = curr->sym.type->total_size;

    DebugMsg(("PushInvokeParam(%s,%u): is_ptr=%u, memtype=%X, psize=%u\n", proc->sym.name, reqParam, curr->is_ptr, curr->sym.mem_type, psize ));

    /* ADDR: the argument's address is to be pushed? */
    if ( AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_ADDR ) {
        addr = TRUE;
        i++;
    }

    /* get the full parameter */
    fullparam[0] = NULLC;
    for ( j = i;
          AsmBuffer[j]->token != T_COMMA && AsmBuffer[j]->token != T_FINAL;
          j++ ) {
        if ( j != i )
            strcat( fullparam," " );
        strcat( fullparam, AsmBuffer[j]->string_ptr );
    }

    j = i;
    fptrsize = 2 + ( 2 << Ofssize );

    if ( addr ) {
        if ( EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );

        /* DWORD (16bit) and FWORD(32bit) are treated like FAR ptrs */
        if ( psize > fptrsize ) {
            /* QWORD is NOT accepted as a FAR ptr */
            DebugMsg(("PushInvokeParm(%u): error, psize=%u, fptrsize=%u\n",
                      reqParam, psize, fptrsize));
            AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
            return( NOT_ERROR );
        }

        if ( proc->sym.langtype == LANG_FASTCALL )
            if ( fastcall_tab[Options.fastcall].handleparam( proc, reqParam, curr, addr, &opndx, buffer, fullparam, r0used ) )
                return( NOT_ERROR );

        if ( opndx.kind == EXPR_REG || opndx.indirect ) {
            if ( curr->is_far || psize == fptrsize ) {
                DebugMsg(("PushInvokeParam: far ptr, %s isfar=%u, psize=%u, fptrsize=%u\n", curr->sym.name, curr->is_far, psize, fptrsize ));
                strcpy( buffer, " push " );
                if ( opndx.sym && opndx.sym->state == SYM_STACK )
                    strcat( buffer, "ss" );
                else if ( opndx.override != EMPTY )
                    strcat( buffer, AsmBuffer[opndx.override]->string_ptr );
                else
                    strcat( buffer,"ds" );
                AddLineQueue( buffer );
            }
            sprintf( buffer, " lea %cax, %s", reg_prefix[ModuleInfo.Ofssize], fullparam );
            *r0used = TRUE;
//          DebugMsg(("PushInvokeParam: %s\n", buffer));
            AddLineQueue( buffer );
            sprintf( buffer, " push %cax", reg_prefix[ModuleInfo.Ofssize] );
        } else {
        push_address:
            if ( curr->is_far || psize == fptrsize ) {
                strcpy(buffer, " push ");

                if ( opndx.override != EMPTY ) {
                    strcat( buffer, AsmBuffer[opndx.override]->string_ptr );
                } else if ( opndx.sym != NULL && opndx.sym->segment != NULL ) {
                    dir_node *dir = GetSeg( opndx.sym );
                    enum assume_segreg as;
                    if (dir->e.seginfo->segtype == SEGTYPE_DATA ||
                        dir->e.seginfo->segtype == SEGTYPE_BSS)
                        as = search_assume( (asm_sym *)dir, ASSUME_DS, TRUE );
                    else
                        as = search_assume( (asm_sym *)dir, ASSUME_CS, TRUE );
                    if ( as != ASSUME_NOTHING ) {
                        char *p;
                        switch ( as ) {
                        case ASSUME_ES:  p = "es";  break;
                        case ASSUME_CS:  p = "cs";  break;
                        case ASSUME_SS:  p = "ss";  break;
                        case ASSUME_DS:  p = "ds";  break;
                        case ASSUME_FS:  p = "fs";  break;
                        default:         p = "gs";  break;
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
                            strcat( buffer, "seg " );
                            strcat( buffer, fullparam );
                        }
                    }
                } else {
                    strcat( buffer,"seg " );
                    strcat( buffer, fullparam );
                }
                AddLineQueue( buffer );
            }
#if 0
            if ( ModuleInfo.Ofssize != curr->is32 ) {
                if ( ModuleInfo.Ofssize )
                    /* better use PUSHW/PUSHD! */
                    sprintf( buffer, " push LOWWORD(offset %s)", fullparam );
                else
                    sprintf( buffer, " push DWORD PTR offset %s", fullparam );
            } else
#endif
                if ( (ModuleInfo.curr_cpu & P_CPU_MASK ) < P_186 ) {
                    sprintf( buffer, " mov ax, offset %s", fullparam );
                    *r0used = TRUE;
                    AddLineQueue( buffer );
                    sprintf( buffer, " push ax" );
                } else
                    sprintf( buffer, " push offset %s", fullparam );
        }
        if ( curr->is_vararg ) {
            size_vararg = size_vararg + CurrWordSize;
            if ( curr->is_far )
                size_vararg = size_vararg + CurrWordSize;
        }
    } else { /* ! ADDR branch */

        /* handle the <reg>::<reg> case here, the evaluator wont handle it */
        if ( AsmBuffer[j]->token == T_REG &&
            AsmBuffer[j+1]->token == T_COLON &&
            AsmBuffer[j+2]->token == T_COLON &&
            AsmBuffer[j+3]->token == T_REG ) {
            asize = SizeFromRegister( AsmBuffer[j]->value );
            asize += SizeFromRegister( AsmBuffer[j+3]->value );
            sprintf( buffer, " push %s", AsmBuffer[j]->string_ptr );
            AddLineQueue( buffer );
            strcpy( fullparam, AsmBuffer[j+3]->string_ptr );
            opndx.kind = EXPR_REG;
            opndx.indirect = FALSE;
            opndx.sym = NULL;
            opndx.base_reg = j+3; /* for error msg 'eax overwritten...' */
        } else {
            if ( EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR ) {
                return( ERROR );
            }
            /* for a simple register, get its size */
            if ( opndx.kind == EXPR_REG && opndx.indirect == FALSE ) {
                asize = SizeFromRegister( AsmBuffer[opndx.base_reg]->value );
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
                if ( opndx.Ofssize == USE_EMPTY )
                    opndx.Ofssize = ModuleInfo.Ofssize;
                asize = SizeFromMemtype( opndx.mem_type, opndx.Ofssize );
            } else {
                if ( opndx.sym != NULL )
                    asize = opndx.sym->type->total_size;
                else
                    asize = opndx.mbr->type->total_size;
            }
        }

        if ( curr->is_vararg == TRUE )
            psize = asize;

        if ( asize > psize ) { /* argument's size too big */
            DebugMsg(("PushInvokeParm(%u): error, arg size=%u, parm size=%u\n", reqParam, asize, psize));
            AsmErr(INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1);
            return (NOT_ERROR);
        }

        //sym = opndx.sym;

#ifdef DEBUG_OUT
        if ( opndx.sym )
            DebugMsg(("PushInvokeParam(%s, %u): arg name=%s, asize=%u, psize=%u\n", proc->sym.name, reqParam, opndx.sym->name, asize, psize));
        else
            DebugMsg(("PushInvokeParam(%s, %u): arg no name, asize=%u, psize=%u\n", proc->sym.name, reqParam, asize, psize));
#endif
        pushsize = CurrWordSize;

        if ( proc->sym.langtype == LANG_FASTCALL )
            if ( fastcall_tab[Options.fastcall].handleparam( proc, reqParam, curr, addr, &opndx, buffer, fullparam, r0used ) )
                return( NOT_ERROR );

        if ( ( opndx.kind == EXPR_ADDR && opndx.instr != T_OFFSET ) ||
            ( opndx.kind == EXPR_REG && opndx.indirect == TRUE ) ) {

            if ( asize > pushsize ) {
                char dw = ' ';
                if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                    pushsize = 4;
                    dw = 'd';
                }
                if ( curr->is_vararg )
                    size_vararg += asize;

                /* in params like "qword ptr [eax]" the typecast
                 * has to be removed */
                if ( opndx.explicit ) {
                    SkipTypecast( fullparam, i );
                    opndx.explicit = FALSE;
                }

                while ( asize > 0 ) {
#if 0
                    if ( opndx.explicit ) {
                        sprintf( buffer, " push %s", fullparam );
                        asize = 0;
                    } else if ( asize & 2 ) {
#else
                    if ( asize & 2 ) {
#endif
                        /* ensure the stack remains dword-aligned in 32bit */
                        if ( pushsize == 4 )
                            AddLineQueue( "sub esp,2" );

                        sprintf( buffer, " push word ptr %s+%u", fullparam, asize-2 );
                        asize -= 2;
                    } else {
                        sprintf( buffer, " push %cword ptr %s+%u", dw, fullparam, asize-pushsize );
                        asize -= pushsize;
                    }
                    AddLineQueue( buffer );
                }
                return NOT_ERROR;
            } else if (asize < pushsize) {
                if ( curr->is_vararg )
                    size_vararg += pushsize;
                if ( psize > 4 ) {
                    DebugMsg(("PushInvokeParm(%u): error, psize=%u, is > 4\n",
                              reqParam, psize ));
                    AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                }
                //switch (sym->mem_type) {
                switch ( opndx.mem_type ) {
                case MT_BYTE:
                    if ( pushsize == 2 ) {
                        if ( psize == 4 )
                            AddLineQueue( "push 0" );
                        sprintf( buffer, " mov al, %s", fullparam );
                        AddLineQueue( buffer );
                        strcpy( buffer, " mov ah, 0" );
                        AddLineQueue( buffer );
                        strcpy( buffer, " push ax" );
                    } else {
                        sprintf( buffer, " movzx eax, %s", fullparam );
                        AddLineQueue( buffer );
                        strcpy( buffer, " push eax" );
                    }
                    *r0used = TRUE;
                    break;
                case MT_SBYTE:
                    if ( pushsize == 2 ) {
                        if ( psize == 4 )
                            AddLineQueue( "push 0" );
                        sprintf( buffer, " mov al, %s", fullparam );
                        AddLineQueue( buffer );
                        strcpy( buffer, " cbw" );
                        AddLineQueue( buffer );
                        strcpy( buffer, " push ax" );
                    } else {
                        sprintf( buffer, " movsx eax, %s", fullparam );
                        AddLineQueue( buffer );
                        strcpy( buffer, " push eax" );
                    }
                    *r0used = TRUE;
                    break;
                case MT_WORD:
                    if ( Options.masm_compat_gencode ) {
                        strcpy( buffer, " sub esp, 2");
                        AddLineQueue( buffer );
                        sprintf( buffer, "push %s", fullparam );
                    } else {
                        sprintf( buffer, " movzx eax, %s", fullparam );
                        AddLineQueue( buffer );
                        strcpy( buffer, " push eax" );
                        *r0used = TRUE;
                    }
                    break;
                case MT_SWORD:
                    if ( Options.masm_compat_gencode ) {
                        strcpy( buffer, " sub esp, 2");
                        AddLineQueue( buffer );
                        sprintf( buffer, "push %s", fullparam );
                    } else {
                        sprintf( buffer, " movsx eax, %s", fullparam );
                        AddLineQueue( buffer );
                        strcpy(buffer, " push eax");
                        *r0used = TRUE;
                    }
                    break;
                default:
                    sprintf( buffer, " push %s", fullparam );
                }
            } else { /* asize == pushsize */
                if (( pushsize == 2 ) || (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ))
                    sprintf( buffer, " push %s", fullparam );
                else {
                    sprintf( buffer, " push word ptr %s+2", AsmBuffer[i]->string_ptr );
                    AddLineQueue( buffer );
                    sprintf( buffer, " push word ptr %s", AsmBuffer[i]->string_ptr );
                }
                if (curr->is_vararg)
                    size_vararg += pushsize;
            }
        } else { /* the parameter is a register or constant value! */
            char *suffix = "";
            char *qual = "";
            if ( opndx.kind == EXPR_REG ) {
                if ( asize != psize || asize < pushsize ) {
                    /* if argument is a 16bit register, just push the full 32bit */
                    if ( asize == 2 && ( psize == 4 || pushsize == 4 ) ) {
                        qual = "e";
                    } else if ( asize == 1 && ( psize <= 4 ) ) {
                        /* the argument is an 8bit register */
                        /* add a movzx if psize is > 1 */
                        if ( psize <= 2 && pushsize == 2 ) {
                            if ( *(fullparam+1) == 'h' || *(fullparam+1) == 'H' ) {
                                if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386) {
                                    sprintf( buffer, " movzx %cx, %ch", *fullparam, *fullparam );
                                } else {
                                    sprintf( buffer, " mov %cl, %ch", *fullparam, *fullparam );
                                    AddLineQueue( buffer );
                                    sprintf( buffer, " mov %ch, 0", *fullparam );
                                }
                                AddLineQueue( buffer );
                            }
                            sprintf( fullparam, "%cx", *fullparam );
                        } else {
                            if ( psize > 1 ) {
                                sprintf( buffer, " movzx e%cx, %s", *fullparam, fullparam );
                                AddLineQueue( buffer );
                            }
                            sprintf( fullparam, "e%cx", *fullparam );
                        }
                    } else {
                        DebugMsg(("PushInvokeParm(%u): error, asize=%u, psize=%u, pushsize=%u\n",
                                  reqParam, asize, psize, pushsize ));
                        AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    }
                }
                if ( *r0used )
                    if ( AsmBuffer[opndx.base_reg]->value == T_EAX ||
                        AsmBuffer[opndx.base_reg]->value == T_AX ) {
                        AsmErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                        *r0used = FALSE;
                    }
            } else { /* constant value */
                asize = CurrWordSize;
                if ( psize < pushsize )  /* ensure that the default pushsize is met */
                    psize = pushsize;
                if (( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_186) {
                    *r0used = TRUE;
                    switch ( psize ) {
                    case 2:
                        if ( opndx.value != 0 || opndx.kind == EXPR_ADDR )
                            sprintf( buffer, " mov ax, %s", fullparam );
                        else
                            strcpy( buffer, " xor ax, ax" );
                        AddLineQueue( buffer );
                        strcpy( fullparam, "ax" );
                        break;
                    case 4:
                        if ( opndx.uvalue <= 0xFFFF )
                            strcpy( buffer, " xor ax, ax" );
                        else
                            sprintf( buffer, " mov ax, HIGHWORD (%s)", fullparam );
                        AddLineQueue( buffer );
                        AddLineQueue( " push ax" );
                        if ( opndx.uvalue != 0 || opndx.kind == EXPR_ADDR ) {
                            sprintf( buffer, " mov ax, LOWWORD (%s)", fullparam );
                            AddLineQueue( buffer );
                        }
                        strcpy( fullparam, "ax" );
                        break;
                    default:
                        DebugMsg(("PushInvokeParm(%u): error, asize=%u, psize=%u, pushsize=%u\n",
                                  reqParam, asize, psize, pushsize ));
                        AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    }
                } else {
                    if ( asize != psize ) {
                        switch ( psize ) {
                        case 2:
                            suffix = "w";
                            break;
                        case 4:
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 )
                                suffix = "d";
                            else {
                                sprintf( buffer, " pushw HIGHWORD (%s)", fullparam );
                                AddLineQueue( buffer );
                                suffix = "w LOWWORD (";
                                strcat( fullparam, ")");
                            }
                            break;
                        case 8:
#if AMD64_SUPPORT
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_64 )
                                break;
#endif
                        default:
                            DebugMsg(("PushInvokeParm(%u): error, asize=%u, psize=%u, pushsize=%u\n",
                                      reqParam, asize, psize, pushsize ));
                            AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                        }
                    }
                }
            }
            sprintf( buffer, " push%s %s%s", suffix, qual, fullparam );
            if ( curr->is_vararg )
                size_vararg += psize;
        }
    }
    AddLineQueue( buffer );
    return( NOT_ERROR );
}

// generate a call for a prototyped procedure

ret_code InvokeDirective( int i )
/*******************************/
{
    struct asm_sym      *sym;
    dir_node            *proc;
    char                *name;
    //char                *param;
    int                 numParam;
    int                 namepos = i;
    bool                r0used = FALSE;
    bool                uselabel = FALSE;
    uint_8              procofssize;
    proc_info   *info;
    dir_node    *curr;
    expr_list   opndx;
    char        buffer[MAX_LINE_LEN];

    /* the call address might be an expression! */

    DebugMsg(("InvokeDef(%s) enter\n", AsmBuffer[i]->pos ));

#if FASTPASS
    /* make sure the directive is stored */
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
        SaveState();
    }
#endif
    /* if there is more than 1 item describing the invoke target,
     use the expression evaluator to get it
     */
    if ( AsmBuffer[i+1]->token != T_COMMA && AsmBuffer[i+1]->token != T_FINAL ) {
        if ( ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ) )
            return ( ERROR );
        DebugMsg(("InvokeDef: target is expression, opndx->sym=%X, opndx->mbr=%X, opndx->type=%X\n", opndx.sym, opndx.mbr, opndx.type ));
#if 1
        /* a typecast with PTR? Since v1.95, this has highest priority */
        //if (opndx.explicit == TRUE && opndx.type != NULL && opndx.type->state == SYM_TYPE ) {
        // v1.96: removed opndx.explicit!!!
        if ( opndx.type != NULL && opndx.type->state == SYM_TYPE ) {
            sym = opndx.type;
            proc = (dir_node *)sym;
            if ( opndx.label != EMPTY )
                uselabel = TRUE;
            if ( proc->sym.mem_type == MT_PROC )  /* added for v1.95 */
                goto isfnproto;
            goto isfnptr;
#endif
        } else if ( opndx.mbr != NULL ) {
            sym = opndx.mbr;
            // it may be a typecast. then the mbr member contains the explicit type
            // and sym->state is SYM_TYPE
            // v1.96: the sentence above describes an obsolete feature.
            // Not sure if <mbr> can contain a type anymore.
            // this code is to be removed/modified!
            if ( sym->state == SYM_TYPE ) {
                proc = (dir_node *)sym;
                if ( opndx.label != EMPTY )
                    uselabel = TRUE;
                goto isfnptr;
            }
        } else if ( opndx.kind == EXPR_ADDR &&
                   opndx.sym != NULL &&
                   ( opndx.sym->isproc ||
                   ( opndx.sym->mem_type == MT_TYPE &&
                   opndx.sym->type->mem_type == MT_PTR ) ) ) {
            sym = opndx.sym;
        } else {
            AsmErr( INVOKE_REQUIRES_PROTOTYPE );
            return ( ERROR );
        }
    } else {
        name = AsmBuffer[i]->string_ptr;
        sym = NULL;
        if ( AsmBuffer[i]->token == T_ID )
            sym = SymSearch( name );
        else if ( AsmBuffer[i]->token == T_REG ) {
            if ( GetOpndType( AsmBuffer[i]->value, 1 ) & OP_RGT8 ) {
                sym = GetStdAssume( GetRegNo( AsmBuffer[i]->value ) );
                proc = (dir_node *)sym;
                if ( proc->sym.mem_type == MT_PROC )  /* added for v1.95 */
                    goto isfnproto;
                goto isfnptr;
            }
        }
        i++;
    }

    if( sym == NULL ) {
        AsmErr( SYMBOL_NOT_DEFINED, name );
        return( ERROR );
    }
    if( sym->isproc )  /* the most simple case: symbol is a PROC */
        ;
    else if ( ( sym->mem_type == MT_TYPE ) && ( sym->type->mem_type == MT_PTR ) ) {
        /* second case: symbol is a (function?) pointer */
        proc = (dir_node *)sym->type;
        /* get the pointer target */
    isfnptr:
        proc = (dir_node *)proc->e.structinfo->target;
        /* pointer target must be a PROTO typedef */
    isfnproto:
        if ( proc == NULL || proc->sym.mem_type != MT_PROC) {
#ifdef DEBUG_OUT
            if ( sym )
                DebugMsg(("InvokeDef: sym->name=%s, sym->type=%X\n", sym->name, sym->type));
            else
                DebugMsg(("InvokeDef: sym=NULL, dir=%X\n", proc ));
#endif
            AsmErr( INVOKE_REQUIRES_PROTOTYPE );
            return( ERROR );
        }
        sym = proc->e.structinfo->target;
    } else {
#ifdef DEBUG_OUT
        if (sym->mem_type == MT_TYPE)
            DebugMsg(("InvokeDef: symbol name=%s, state=%u, type=%s, memtype=%Xh, [type memtype=%u]\n", sym->name, sym->state, sym->type->name, sym->mem_type, sym->type->mem_type));
        else
            DebugMsg(("InvokeDef: symbol name=%s, state=%u, type=%X, memtype=%Xh\n", sym->name, sym->state, sym->type, sym->mem_type));
#endif
        AsmErr( INVOKE_REQUIRES_PROTOTYPE );
        return( ERROR );
    }
    proc = (dir_node *)sym;
    info = proc->e.procinfo;
    procofssize = sym->Ofssize;

    /* does FASTCALL variant support INVOKE? */

    if ( proc->sym.langtype == LANG_FASTCALL && fastcall_tab[Options.fastcall].invokestart == NULL ) {
        AsmError( FASTCALL_VARIANT_NOT_SUPPORTED );
        return( ERROR );
    }

    /* get the number of parameters */

    for ( curr = info->paralist, numParam = 0 ; curr ; curr = curr->nextparam, numParam++ );
    DebugMsg(("InvokeDef: numparams=%u\n", numParam ));

    curr = info->paralist;

    PushLineQueue();

    if ( proc->sym.langtype == LANG_FASTCALL ) {
        fcscratch = 0;
        fastcall_tab[Options.fastcall].invokestart( proc, numParam, buffer );
    }

    if (!(info->is_vararg)) {
        /* check if there is a superfluous parameter in the INVOKE call */
        if ( PushInvokeParam( proc, NULL, i, numParam, procofssize, &r0used) != ERROR ) {
            DebugMsg(("InvokeDef: superfluous argument, i=%u\n", i));
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
        DebugMsg(("InvokeDef: VARARG proc, numparams=%u, actual (max) params=%u, parasize=%u\n", numParam, j, info->parasize));
        for (;j >= numParam; j--)
            PushInvokeParam( proc, curr, i, j, procofssize, &r0used);
        /* VARARG procs have at least 1 param, the VARARG param */
        curr = curr->nextparam;
    }

    /* the parameters are usually stored in "push" order */

    if (sym->langtype == LANG_STDCALL ||
        sym->langtype == LANG_C ||
#if AMD64_SUPPORT
        /* since Win64 fastcall doesn't push, it's a better/faster strategy to 
         * handle the arguments the other way
         */
        (sym->langtype == LANG_FASTCALL && procofssize != USE64 ) ||
#else
        sym->langtype == LANG_FASTCALL ||
#endif
        sym->langtype == LANG_SYSCALL) {
        for ( ; curr ; curr = curr->nextparam ) {
            numParam--;
            if (PushInvokeParam( proc, curr, i, numParam, procofssize, &r0used) == ERROR) {
                DebugMsg(("PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                AsmErr( TOO_FEW_ARGUMENTS_TO_INVOKE, sym->name );
            }
        }
    } else {
        for ( numParam = 0 ; curr ; curr = curr->nextparam, numParam++ ) {
            if ( PushInvokeParam( proc, curr, i, numParam, procofssize, &r0used ) == ERROR) {
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
        if ( info->is_vararg ) {
            DebugMsg(("InvokeDef: size of fix args=%u, var args=%u\n", info->parasize, size_vararg));
            sprintf( buffer, " add %csp, %u", reg_prefix[ModuleInfo.Ofssize], info->parasize + size_vararg );
        } else
            sprintf( buffer, " add %csp, %u", reg_prefix[ModuleInfo.Ofssize], info->parasize );
        AddLineQueue( buffer );
    } else if ( sym->langtype == LANG_FASTCALL ) {
        fastcall_tab[Options.fastcall].invokeend( proc, numParam, buffer );
    }

    LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    RunLineQueue();

    return( NOT_ERROR );
}

