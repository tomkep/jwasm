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
#include "directiv.h"
#include "expreval.h"
#include "input.h"
#include "equate.h"
#include "assume.h"
#include "segment.h"
#include "fastpass.h"
#include "listing.h"

#ifdef __I86__
#define NUMQUAL (long)
#else
#define NUMQUAL
#endif

enum reg_used_flags {
    R0_USED       = 0x01, /* register contents of AX/EAX/RAX is destroyed */
    R0_H_CLEARED  = 0x02, /* 16bit: high byte of R0 (=AH) has been set to 0 */
    R0_X_CLEARED  = 0x04, /* 16bit: register R0 (=AX) has been set to 0 */
#if AMD64_SUPPORT
    RCX_USED      = 0x08, /* win64: register contents of CL/CX/ECX/RCX is destroyed */
    RDX_USED      = 0x10, /* win64: register contents of DL/DX/EDX/RDX is destroyed */
    R8_USED       = 0x20, /* win64: register contents of R8B/R8W/R8D/R8 is destroyed */
    R9_USED       = 0x40, /* win64: register contents of R9B/R9W/R9D/R9 is destroyed */
#define RPAR_START 3 /* Win64: RCX first param start at bit 3 */
#endif
#ifdef INVWC_SUPPORT
    ROW_AX_USED   = 0x08, /* watc: register contents of AL/AX/EAX is destroyed */
    ROW_DX_USED   = 0x10, /* watc: register contents of DL/DX/EDX is destroyed */
    ROW_BX_USED   = 0x20, /* watc: register contents of BL/BX/EBX is destroyed */
    ROW_CX_USED   = 0x40, /* watc: register contents of CL/CX/ECX is destroyed */
#define ROW_START 3 /* watc: irst param start at bit 3 */
#endif
};

static int size_vararg;
static int fcscratch;

struct fastcall_conv {
    void (* invokestart)( dir_node *, int, int, int * );
    void (* invokeend)( dir_node *, int, int );
    int  (* handleparam)( dir_node *, int, dir_node *, bool, expr_list *, char *, uint_8 * );
};

static void ms32_fcstart( dir_node *, int, int, int * );
static void ms32_fcend( dir_node *, int, int );
static  int ms32_param( dir_node *, int, dir_node *, bool, expr_list *, char *, uint_8 * );
#if INVWC_SUPPORT
static void watc_fcstart( dir_node *, int, int, int * );
static void watc_fcend( dir_node *, int, int );
static  int watc_param( dir_node *, int, dir_node *, bool, expr_list *, char *, uint_8 * );
#endif
#if AMD64_SUPPORT
static void ms64_fcstart( dir_node *, int, int, int * );
static void ms64_fcend( dir_node *, int, int );
static  int ms64_param( dir_node *, int, dir_node *, bool, expr_list *, char *, uint_8 * );
#define REGPAR_WIN64 0x0306 /* regs 1, 2, 8 and 9 */
#endif

static const struct fastcall_conv fastcall_tab[] = {
 { ms32_fcstart, ms32_fcend , ms32_param }, /* FCT_MS32 */
#if INVWC_SUPPORT
 { watc_fcstart, watc_fcend , watc_param }, /* FCT_WATCOMC */
#else
 { NULL, NULL , NULL },
#endif
#if AMD64_SUPPORT
 { ms64_fcstart, ms64_fcend , ms64_param } /* FCT_WIN64 */
#endif
};

static const enum asm_token regsp[] = { T_SP, T_ESP,
#if AMD64_SUPPORT
T_RSP
#endif
};
static const enum asm_token regax[] = { T_AX, T_EAX,
#if AMD64_SUPPORT
T_RAX
#endif
};

static const enum asm_token ms32_regs[] = {
    T_CX, T_DX,
    T_ECX, T_EDX
};

#if 0 //INVWC_SUPPORT
/* the watcomm fastcall variant is somewhat peculiar:
 * 16-bit:
 * - for BYTE/WORD arguments, there are 4 registers: AX,DX,BX,CX
 * - for DWORD arguments, there are 2 register pairs: DX::AX and CX::BX
 * - there is a "usage" flag for each register. Thus the prototype:
 *   sample proto :WORD, :DWORD, :WORD
 *   will assign AX to the first param, CX::BX to the second, and DX to
 *   the third!
 */
#define FCW_BYTE 0
#define FCW_WORD 4
#define FCW_DWORD 8
static const enum asm_token watc_regs[] = {
    T_AL,  T_DL,  T_BL,  T_CL,
    T_AX,  T_DX,  T_BX,  T_CX,
    T_EAX, T_EDX, T_EBX, T_ECX
};
#endif

#if AMD64_SUPPORT
static const enum asm_token ms64_regs[] = {
 T_CL,  T_DL,  T_R8B, T_R9B,
 T_CX,  T_DX,  T_R8W, T_R9W,
 T_ECX, T_EDX, T_R8D, T_R9D,
 T_RCX, T_RDX, T_R8,  T_R9
};
#endif

/* segment register names, order must match ASSUME_ enum */
static const enum asm_token segreg_tab[] = {
    T_ES, T_CS, T_SS, T_DS, T_FS, T_GS };

static void ms32_fcstart( dir_node *proc, int numparams, int start, int *value )
/******************************************************************************/
{
    fcscratch = 2;
    return;
}

static void ms32_fcend( dir_node *proc, int numparams, int value )
/****************************************************************/
{
    /* nothing to do */
    return;
}

static int ms32_param( dir_node *proc, int index, dir_node *param, bool addr, expr_list *opndx, char *paramvalue, uint_8 *r0used )
/********************************************************************************************************************************/
{
    if ( param->sym.state != SYM_TMACRO || fcscratch == 0 )
        return( 0 );
    fcscratch--;
    if ( addr )
        AddLineQueueX( " lea %r, %s", ms32_regs[ModuleInfo.Ofssize * 2 + fcscratch], paramvalue );
    else
        AddLineQueueX( " mov %r, %s", ms32_regs[ModuleInfo.Ofssize * 2 + fcscratch], paramvalue );
    return( 1 );
}

#if AMD64_SUPPORT
static void ms64_fcstart( dir_node *proc, int numparams, int start, int *value )
/******************************************************************************/
{
    DebugMsg1(("ms64_fcstart(%s, numparams=%u)\n", proc->sym.name, numparams ));
    /* v2.04: VARARG didn't work */
    if ( proc->e.procinfo->is_vararg ) {
        for ( numparams = 0; AsmBuffer[start]->token != T_FINAL; start++ )
            if ( AsmBuffer[start]->token == T_COMMA )
                numparams++;
    }
    if ( numparams < 4 )
        numparams = 4;
    else if ( numparams & 1 )
        numparams++;
    *value = numparams;
    AddLineQueueX( " sub %r, %d", T_RSP, numparams * 8 );
    return;
}

static void ms64_fcend( dir_node *proc, int numparams, int value )
/****************************************************************/
{
    /* use <value>, which has been set by ms64_fcstart() */
    AddLineQueueX( " add %r, %d", T_RSP, value * 8 );
    return;
}

/* macro to convert register number to param number:
 * 1 -> 0 (rCX)
 * 2 -> 1 (rDX)
 * 8 -> 2 (r8)
 * 9 -> 3 (r9)
 */
#define GetParmIndex( x)  ( ( (x) >= 8 ) ? (x) - 6 : (x) - 1 )

/*
 * parameter for Win64 FASTCALL.
 * the first 4 parameters are hold in registers: rcx, rdx, r8, r9 for non-float arguments,
 * xmm0, xmm1, xmm2, xmm3 for float arguments. If parameter size is > 8, the address of
 * the argument is used instead of the value.
 */

static int ms64_param( dir_node *proc, int index, dir_node *param, bool addr, expr_list *opndx, char *paramvalue, uint_8 *regs_used )
/***********************************************************************************************************************************/
{
    uint_32 size;
    uint_32 psize;
    int reg;
    int reg2;
    int i;
    int base;
    bool destroyed = FALSE;

    psize = SizeFromMemtype( param->sym.mem_type, USE64, param->sym.type );
    if ( index >= 4 ) {
        /* check for register overwrites */
        if ( opndx->base_reg != EMPTY ) {
            reg = AsmBuffer[opndx->base_reg]->value;
            if ( GetValueSp( reg ) & OP_R ) {
                i = GetRegNo( reg );
                if ( REGPAR_WIN64 & ( 1 << i ) ) {
                    base = GetParmIndex( i );
                    if ( *regs_used & ( 1 << ( base + RPAR_START ) ) )
                        destroyed = TRUE;
                } else if ( (*regs_used & R0_USED ) && ( ( GetValueSp( reg ) & OP_A ) || reg == T_AH ) ) {
                    destroyed = TRUE;
                }
            }
        }
        if ( opndx->idx_reg != EMPTY ) {
            reg2 = AsmBuffer[opndx->idx_reg]->value;
            if ( GetValueSp( reg2 ) & OP_R ) {
                i = GetRegNo( reg2 );
                if ( REGPAR_WIN64 & ( 1 << i ) ) {
                    base = GetParmIndex( i );
                    if ( *regs_used & ( 1 << ( base + RPAR_START ) ) )
                        destroyed = TRUE;
                } else if ( (*regs_used & R0_USED ) && ( ( GetValueSp( reg2 ) & OP_A ) || reg2 == T_AH ) ) {
                    destroyed = TRUE;
                }
            }
        }
        if ( destroyed ) {
            AsmErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
            *regs_used = 0;
        }
        if ( opndx->indirect == FALSE && opndx->base_reg != EMPTY ) {
            i = reg;
            size = SizeFromRegister( reg );
            if ( size != psize && param->is_vararg == FALSE ) {
                DebugMsg(("ms64_param(%s, param=%u): type error size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
                AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
                psize = size;
            }
        }
        if ( opndx->mem_type == MT_EMPTY && addr == FALSE ) {
            switch ( psize ) {
            case 1:   i = T_BYTE; break;
            case 2:   i = T_WORD; break;
            case 4:   i = T_DWORD; break;
            default:  i = T_QWORD; break;
            }
            AddLineQueueX( " mov %r ptr [%r+%u], %s", i, T_RSP, NUMQUAL index*8, paramvalue );
            DebugMsg(("ms64_param(%s, param=%u): MT_EMPTY size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
        } else {
            if ( addr == FALSE ) {
                if ( opndx->indirect == FALSE && opndx->base_reg != EMPTY ) {
                    DebugMsg(("ms64_param(%s, param=%u): REG size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
                } else {
                    DebugMsg(("ms64_param(%s, param=%u): MEM size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
                    size = SizeFromMemtype( opndx->mem_type, USE64, opndx->type );
                    if ( size != psize && param->is_vararg == FALSE )
                        AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
                    switch ( size ) {
                    case 1:  i = T_AL;  break;
                    case 2:  i = T_AX;  break;
                    case 4:  i = T_EAX; break;
                    default: i = T_RAX; break;
                    }
                    *regs_used |= R0_USED;
                    if ( size <= 8 )
                        AddLineQueueX( " mov %r, %s", i, paramvalue );
                    else {
                        AddLineQueueX( " lea %r, %s", i, paramvalue );
                    }
                }
            } else {
                if ( param->is_vararg == TRUE )
                    psize = 8;
                switch ( psize ) {
                case 4: i = T_EAX;   break;
                case 8: i = T_RAX;   break;
                default:
                    AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
                    i = T_RAX;
                }
                *regs_used |= R0_USED;
                AddLineQueueX( " lea %r, %s", i, paramvalue );
                DebugMsg(("ms64_param(%s, param=%u): ADDR flags=%X\n", proc->sym.name, index, *regs_used ));
            }
            AddLineQueueX( " mov [%r+%u], %r", T_RSP, NUMQUAL index*8, i );
        }
    } else if ( param->sym.mem_type == MT_REAL4 ||
               param->sym.mem_type == MT_REAL8 ) {
        /* v2.04: check if argument is the correct XMM register already */
        if ( opndx->indirect == FALSE && opndx->base_reg != EMPTY ) {
            reg = AsmBuffer[opndx->base_reg]->value;
            if ( GetValueSp( reg ) & OP_XMM ) {
                if ( reg == T_XMM0 + index )
                    DebugMsg(("ms64_param(%s, param=%u): argument optimized\n", proc->sym.name, index ));
                else
                    AddLineQueueX( " movq %r, %s", T_XMM0 + index, paramvalue );
                return( 1 );
            }
        }
        if ( param->sym.mem_type == MT_REAL4 )
            AddLineQueueX( " movd %r, %s", T_XMM0 + index, paramvalue );
        else
            AddLineQueueX( " movq %r, %s", T_XMM0 + index, paramvalue );
    } else {
        if ( addr || psize > 8 ) { /* psize > 8 shouldn't happen! */
            if ( psize == 4 )
                AddLineQueueX( " lea %r, %s", ms64_regs[index+2*4], paramvalue );
            else if ( psize > 4 )
                AddLineQueueX( " lea %r, %s", ms64_regs[index+3*4], paramvalue );
            else
                AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
            *regs_used |= ( 1 << ( index + RPAR_START ) );
            return( 1 );
        }
        /* register argument? */
        if ( opndx->indirect == FALSE && opndx->base_reg != EMPTY ) {
            reg = AsmBuffer[opndx->base_reg]->value;
            size = SizeFromRegister( reg );
        } else {
            if ( opndx->mem_type != MT_EMPTY )
                size = SizeFromMemtype( opndx->mem_type, USE64, opndx->type );
            else {
                size = psize;
            }
        }
        if ( size != psize && param->is_vararg == FALSE ) {
            DebugMsg(("ms64_param(%s, param=%u): type error size.p/a=%u/%u flags=%X\n", proc->sym.name, index, psize, size, *regs_used ));
            AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, index+1 );
        }
        switch ( size ) {
        case 1: base =  0*4; break;
        case 2: base =  1*4; break;
        case 4: base =  2*4; break;
        default:base =  3*4; break;
        }
        /* optimization if the register holds the value already */
        if ( opndx->indirect == FALSE && opndx->base_reg != EMPTY ) {
            if ( GetValueSp( reg ) & OP_R ) {
                if ( ms64_regs[index+base] == reg ) {
                    DebugMsg(("ms64_param(%s, param=%u): argument optimized\n", proc->sym.name, index ));
                    return( 1 );
                }
                i = GetRegNo( reg );
                if ( REGPAR_WIN64 & ( 1 << i ) ) {
                    i = GetParmIndex( i );
                    if ( *regs_used & ( 1 << ( i + RPAR_START ) ) )
                        AsmErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                }
            }
        }
        AddLineQueueX( " mov %r, %s", ms64_regs[index+base], paramvalue );
        *regs_used |= ( 1 << ( index + RPAR_START ) );
        DebugMsg(("ms64_param(%s, param=%u): size=%u flags=%X\n", proc->sym.name, index, size, *regs_used ));
    }
    return( 1 );
}

#endif

#if INVWC_SUPPORT

static void watc_fcstart( dir_node *proc, int numparams, int start, int *value )
/******************************************************************************/
{
    return;
}

static void watc_fcend( dir_node *proc, int numparams, int value )
/****************************************************************/
{
    /* nothing to do */
    return;
}

/* get the register for parms 0 to 3,
 * using the watcom register parm passing conventions ( A D B C )
 */
static int watc_param( dir_node *proc, int index, dir_node *param, bool addr, expr_list *opndx, char *paramvalue, uint_8 *r0used )
/********************************************************************************************************************************/
{
    int opc;
    int qual;
    int i;
    char regs[64];
    char *reg[4];
    char *p;
    int psize = SizeFromMemtype( param->sym.mem_type, USE_EMPTY, param->sym.type );

    DebugMsg(("watc_param(%s, param=%u [name=%s, state=%u]): size=%u\n", proc->sym.name, index, param->sym.name, param->sym.state, psize ));
    if ( param->sym.state != SYM_TMACRO )
        return( 0 );
    DebugMsg(("watc_param(%s): register param=%s\n", proc->sym.name, param->sym.string_ptr ));

    /* the "name" might be a register pair */

    reg[0] = param->sym.string_ptr;
    reg[1] = NULL;
    reg[2] = NULL;
    reg[3] = NULL;
    if ( strchr( reg[0], ':' ) ) {
        strcpy( regs, reg[0] );
        for ( p = regs, i = 0; i < 4; i++ ) {
            reg[i] = p;
            p = strchr( p, ':' );
            if ( p == NULL )
                break;
            *p++ = NULLC;
            p++;
        }
    }

    if ( addr ) {
        if ( opndx->kind == T_REG || opndx->sym->state == SYM_STACK ) {
            opc = T_LEA;
            qual = T_NULL;
        } else {
            opc = T_MOV;
            qual = T_OFFSET;
        }
        AddLineQueueX( "%r %s, %r %s", opc,
            reg[0], qual, paramvalue );
        return( 1 );
    }
    for ( i = 3; i >= 0; i-- ) {
        if ( reg[i] ) {
            if ( opndx->kind == EXPR_CONST ) {
                if ( i > 0 )
                    qual = T_LOWWORD;
                else if ( i == 0 && reg[1] != NULL )
                    qual = T_HIGHWORD;
                else
                    qual = T_NULL;
                if ( qual != T_NULL )
                    AddLineQueueX( "mov %s, %r (%s)", reg[i], qual, paramvalue );
                else
                    AddLineQueueX( "mov %s, %s", reg[i], paramvalue );
            } else if ( opndx->kind == EXPR_REG ) {
                AddLineQueueX( "mov %s, %s", reg[i], paramvalue );
            } else {
                if ( i == 0 && reg[1] == NULL )
                    AddLineQueueX( "mov %s, %s", reg[i], paramvalue );
                else {
                    if ( ModuleInfo.Ofssize )
                        qual = T_DWORD;
                    else
                        qual = T_WORD;
                    AddLineQueueX( "mov %s, %r %r %s[%u]", reg[i], qual, T_PTR, paramvalue, psize - ( (i+1) * ( 2 << ModuleInfo.Ofssize ) ) );
                }
            }
        }
    }
    return( 1 );
}

#endif

static void SkipTypecast(char * fullparam, int i)
/***********************************************/
{
    int j;
    fullparam[0] = NULLC;
    for ( j = i; ; j++ ) {
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
 * push one parameter of a procedure called with INVOKE onto the stack
 * - proc    : the PROC to call
 * - curr    : the current parameter
 * - i       : the AsmBuffer index of the start of the parameter list
 * - reqParam: the index of the parameter which is to be pushed
 * - r0flags : flags for register usage across params
 *
 * psize,asize: size of parameter/argument in bytes.
 */

static int PushInvokeParam( dir_node *proc, dir_node *curr, int i, int reqParam, uint_8 *r0flags)
/***********************************************************************************************/
{
    int currParm;
    int psize;
    int asize;
    int pushsize;
    int j;
    int fptrsize;
    char *p;
    bool addr = FALSE; /* ADDR operator found */
    expr_list opndx;
    char fullparam[MAX_LINE_LEN];
    char buffer[MAX_LINE_LEN];

    DebugMsg1(("PushInvokeParam(%s, param=%s:%u, i=%u ) enter\n", proc->sym.name, curr ? curr->sym.name : "NULL", reqParam, i ));
    for ( currParm = 0; currParm <= reqParam; ) {
        if ( AsmBuffer[i]->token == T_FINAL ) { /* this is no real error! */
            DebugMsg1(("PushInvokeParam(%s): T_FINAL token, i=%u\n", proc->sym.name, i));
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA ) {
            currParm++;
        }
        i++;
    }
    /* if curr is NULL this call is just a parameter check */
    if ( !curr ) return( NOT_ERROR );

    /* set psize (size of parameter) */
    if ( curr->is_ptr ) {
        psize = 2 << curr->sym.Ofssize;
        if ( curr->sym.isfar )
            psize += 2;
    } else
        psize = SizeFromMemtype( curr->sym.mem_type, curr->sym.Ofssize, curr->sym.type );

    DebugMsg1(("PushInvokeParam(%s,%u): is_ptr=%u, memtype=%X, psize=%u\n", proc->sym.name, reqParam, curr->is_ptr, curr->sym.mem_type, psize ));

    /* ADDR: the argument's address is to be pushed? */
    if ( AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_ADDR ) {
        addr = TRUE;
        i++;
    }

    /* get the full parameter */
    p = fullparam;
    fullparam[0] = NULLC;
    for ( j = i;
          AsmBuffer[j]->token != T_COMMA && AsmBuffer[j]->token != T_FINAL;
          j++ ) {
        if ( fullparam[0] )
            *p++ = ' ';
        asize = strlen( AsmBuffer[j]->string_ptr );
        memcpy( p, AsmBuffer[j]->string_ptr, asize+1 );
        p += asize;
    }

    j = i;
    fptrsize = 2 + ( 2 << proc->sym.Ofssize );

    if ( addr ) {
        if ( EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );

        /* DWORD (16bit) and FWORD(32bit) are treated like FAR ptrs */
        if ( psize > fptrsize ) {
            /* QWORD is NOT accepted as a FAR ptr */
            DebugMsg1(("PushInvokeParm(%u): error, psize=%u, fptrsize=%u\n",
                      reqParam, psize, fptrsize));
            AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
            return( NOT_ERROR );
        }

        if ( proc->sym.langtype == LANG_FASTCALL )
            if ( fastcall_tab[Options.fastcall].handleparam( proc, reqParam, curr, addr, &opndx, fullparam, r0flags ) )
                return( NOT_ERROR );

        if ( opndx.kind == EXPR_REG || opndx.indirect ) {
            if ( curr->sym.isfar || psize == fptrsize ) {
                DebugMsg1(("PushInvokeParam: far ptr, %s isfar=%u, psize=%u, fptrsize=%u\n", curr->sym.name, curr->sym.isfar, psize, fptrsize ));
                if ( opndx.sym && opndx.sym->state == SYM_STACK )
                    GetResWName( T_SS, buffer );
                else if ( opndx.override != EMPTY )
                    strcpy( buffer, AsmBuffer[opndx.override]->string_ptr );
                else
                    GetResWName( T_DS, buffer );
                AddLineQueueX( " push %s", buffer );
            }
            AddLineQueueX( " lea %r, %s", regax[ModuleInfo.Ofssize], fullparam );
            *r0flags |= R0_USED;
            AddLineQueueX( " push %r", regax[ModuleInfo.Ofssize] );
        } else {
        push_address:

            /* push segment part of address? */
            if ( curr->sym.isfar || psize == fptrsize ) {

                if ( opndx.override != EMPTY ) {
                    strcpy( buffer, AsmBuffer[opndx.override]->string_ptr );
                } else if ( opndx.sym != NULL && opndx.sym->segment != NULL ) {
                    dir_node *dir = GetSeg( opndx.sym );
                    enum assume_segreg as;
                    if (dir->e.seginfo->segtype == SEGTYPE_DATA ||
                        dir->e.seginfo->segtype == SEGTYPE_BSS)
                        as = search_assume( (asm_sym *)dir, ASSUME_DS, TRUE );
                    else
                        as = search_assume( (asm_sym *)dir, ASSUME_CS, TRUE );
                    if ( as != ASSUME_NOTHING ) {
                        GetResWName( segreg_tab[as], buffer );
                    } else {
                        struct asm_sym *seg;
                        seg = GetGroup( opndx.sym );
                        if (seg == NULL)
                            seg = &dir->sym;
                        if ( seg )
                            strcpy( buffer, seg->name );
                        else {
                            strcpy( buffer, "seg " );
                            strcat( buffer, fullparam );
                        }
                    }
                } else {
                    strcpy( buffer,"seg " );
                    strcat( buffer, fullparam );
                }
                AddLineQueueX( " push %s", buffer );
            }
            /* push offset part of address */
            if ( (ModuleInfo.curr_cpu & P_CPU_MASK ) < P_186 ) {
                AddLineQueueX( " mov %r, offset %s", T_AX, fullparam );
                AddLineQueueX( " push %r", T_AX );
                *r0flags |= R0_USED;
            } else {
                if ( curr->is_vararg &&
                    opndx.Ofssize == USE_EMPTY &&
                    opndx.sym )
                    opndx.Ofssize = GetSymOfssize( opndx.sym );
                /* v2.04: expand 16-bit offset to 32 */
                if ( opndx.Ofssize == USE16 && CurrWordSize > 2 ) {
                    AddLineQueueX( " pushd offset %s", fullparam );
                } else {
                    AddLineQueueX( " push offset %s", fullparam );
                    /* v2.04: a 32bit offset pushed in 16-bit code */
                    if ( curr->is_vararg &&
                        CurrWordSize == 2 &&
                        opndx.Ofssize > USE16 )
                        size_vararg += CurrWordSize;
                }
            }
        }
        if ( curr->is_vararg ) {
            size_vararg += CurrWordSize;
            DebugMsg1(("PushInvokeParm(%u): %u added to size_vararg, now=%u\n", reqParam, CurrWordSize, size_vararg ));
            if ( curr->sym.isfar ) {
                size_vararg += CurrWordSize;
                DebugMsg1(("PushInvokeParm(%u): %u added to size_vararg, now=%u\n", reqParam, CurrWordSize, size_vararg ));
            }
        }
    } else { /* ! ADDR branch */

        /* handle the <reg>::<reg> case here, the evaluator wont handle it */
        if ( AsmBuffer[j]->token == T_REG &&
            AsmBuffer[j+1]->token == T_DBL_COLON &&
            AsmBuffer[j+2]->token == T_REG ) {
            int asize2;
            /* for pointers, segreg size is assumed to be always 2 */
            if ( GetValueSp( AsmBuffer[j]->value ) & OP_SR )
                asize2 = 2;
            else
                asize2 = SizeFromRegister( AsmBuffer[j]->value );
            asize = SizeFromRegister( AsmBuffer[j+2]->value );
            AddLineQueueX( " push %r", AsmBuffer[j]->value );
            /* v2.04: changed */
            if (( curr->is_vararg ) && (asize + asize2) != CurrWordSize )
                size_vararg += asize2;
            else
                asize += asize2;
            strcpy( fullparam, AsmBuffer[j+2]->string_ptr );

            opndx.kind = EXPR_REG;
            opndx.indirect = FALSE;
            opndx.sym = NULL;
            opndx.base_reg = j+2; /* for error msg 'eax overwritten...' */
        } else {
            if ( EvalOperand( &j, Token_Count, &opndx, TRUE ) == ERROR ) {
                return( ERROR );
            }
            /* for a simple register, get its size */
            if ( opndx.kind == EXPR_REG && opndx.indirect == FALSE ) {
                asize = SizeFromRegister( AsmBuffer[opndx.base_reg]->value );
            } else if ( opndx.mem_type == MT_EMPTY ) {
                /* v2.04: added, to catch 0-size params ( STRUCT without members ) */
                if ( psize == 0 && curr->is_vararg == FALSE ) {
                    DebugMsg1(("PushInvokeParm(%u): error, psize=0\n" ));
                    AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                }
                asize = psize;
                DebugMsg1(("PushInvokeParm(%u): memtype EMPTY, asize=%u psize=%u\n", reqParam, asize, psize ));
            } else if ( opndx.mem_type != MT_TYPE ) {
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
                asize = SizeFromMemtype( opndx.mem_type, opndx.Ofssize, opndx.type );
            } else {
                if ( opndx.sym != NULL )
                    asize = opndx.sym->type->total_size;
                else
                    asize = opndx.mbr->type->total_size;
            }
        }

        if ( curr->is_vararg == TRUE )
            psize = asize;

#ifdef DEBUG_OUT
        if ( opndx.sym )
            DebugMsg1(("PushInvokeParam(%s, %u): arg name=%s, asize=%u, psize=%u\n", proc->sym.name, reqParam, opndx.sym->name, asize, psize));
        else
            DebugMsg1(("PushInvokeParam(%s, %u): arg no name, asize=%u, psize=%u\n", proc->sym.name, reqParam, asize, psize));
#endif
        pushsize = CurrWordSize;

        if ( proc->sym.langtype == LANG_FASTCALL )
            if ( fastcall_tab[Options.fastcall].handleparam( proc, reqParam, curr, addr, &opndx, fullparam, r0flags ) )
                return( NOT_ERROR );

        /* v2.04: this check has been moved behind the fastcall_tab() call */
        if ( asize > psize ) { /* argument's size too big? */
            DebugMsg(("PushInvokeParm(%u): argsize error, arg size=%u, parm size=%u\n", reqParam, asize, psize));
            AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
            return( NOT_ERROR );
        }

        if ( ( opndx.kind == EXPR_ADDR && opndx.instr != T_OFFSET ) ||
            ( opndx.kind == EXPR_REG && opndx.indirect == TRUE ) ) {

            /* catch the case when EAX has been used for ADDR,
             * and is later used as addressing register!
             *
             */
            if ( *r0flags &&
                (( opndx.base_reg != EMPTY &&
                  (AsmBuffer[opndx.base_reg]->value == T_EAX
#if AMD64_SUPPORT
                   || AsmBuffer[opndx.base_reg]->value == T_RAX
#endif
                  )) ||
                 ( opndx.idx_reg != EMPTY &&
                  ( AsmBuffer[opndx.idx_reg]->value == T_EAX
#if AMD64_SUPPORT
                   || AsmBuffer[opndx.idx_reg]->value == T_RAX
#endif
                 )))) {
                AsmErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                *r0flags = 0;
            }

            if ( asize > pushsize ) {
                short dw = T_WORD;
                if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                    pushsize = 4;
                    dw = T_DWORD;
                }
                if ( curr->is_vararg ) {
                    size_vararg += asize;
                    DebugMsg1(("PushInvokeParm(%u): asize=%u added to size_vararg, now=%u\n", reqParam, asize, size_vararg ));
                }

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
                        if ( ModuleInfo.Ofssize > USE16 ) {
                            AddLineQueueX( " sub %r, 2", T_ESP );
                        }
                        AddLineQueueX( " push word ptr %s+%u", fullparam, NUMQUAL asize-2 );
                        asize -= 2;
                    } else {
                        AddLineQueueX( " push %r ptr %s+%u", dw, fullparam, NUMQUAL asize-pushsize );
                        asize -= pushsize;
                    }
                }
                return( NOT_ERROR );
            } else if ( asize < pushsize ) {
                if ( curr->is_vararg ) {
                    size_vararg += pushsize;
                    DebugMsg1(("PushInvokeParm(%u): %u added to size_vararg, now=%u\n", reqParam, pushsize, size_vararg ));
                }
                if ( psize > 4 ) {
                    DebugMsg1(("PushInvokeParm(%u): error, ADDR, psize=%u, is > 4\n",
                              reqParam, psize ));
                    AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                }
                //switch (sym->mem_type) {
                switch ( opndx.mem_type ) {
                case MT_BYTE:
                case MT_SBYTE:
                    if ( psize == 1 && curr->is_vararg == FALSE ) {
                        AddLineQueueX( " mov %r, %s", T_AL, fullparam );
                        AddLineQueueX( " push %r", pushsize == 4 ? T_EAX : T_AX );
                    } else if ( pushsize == 2 ) {
                        if ( psize == 4 )
                            AddLineQueue( " push 0" );
                        AddLineQueueX( " mov %r, %s", T_AL, fullparam );
                        if ( opndx.mem_type == MT_BYTE ) {
                            if ( !( *r0flags & R0_H_CLEARED ))
                                AddLineQueueX( " mov %r, 0", T_AH );
                            *r0flags |= R0_H_CLEARED;
                        } else {
                            *r0flags = 0; /* reset AH_CLEARED */
                            AddLineQueue( " cbw" );
                        }
                        AddLineQueueX( " push %r", T_AX );
                    } else {
                        AddLineQueueX( " %r %r, %s", opndx.mem_type == MT_BYTE ? T_MOVZX : T_MOVSX, T_EAX, fullparam );
                        AddLineQueueX( " push %r", T_EAX );
                    }
                    *r0flags |= R0_USED;
                    break;
                case MT_WORD:
                case MT_SWORD:
                    /* v2.04: use the Masm-compatible, non-destructive
                     * PUSH if psize is 2.
                     */
                    //if ( Options.masm_compat_gencode ) {
                    if ( Options.masm_compat_gencode || psize == 2 ) {
                        AddLineQueueX( " sub %r, 2", T_ESP );
                        AddLineQueueX( " push %s", fullparam );
                    } else {
                        AddLineQueueX( " %r %r, %s", opndx.mem_type == MT_WORD ? T_MOVZX : T_MOVSX, T_EAX, fullparam );
                        AddLineQueueX( " push %r", T_EAX );
                        *r0flags = R0_USED; /* reset R0_H_CLEARED  */
                    }
                    break;
                default:
                    AddLineQueueX( " push %s", fullparam );
                }
            } else { /* asize == pushsize */
                if (( pushsize == 2 ) || (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ))
                    AddLineQueueX( " push %s", fullparam );
                else {
                    AddLineQueueX( " push word ptr %s+2", AsmBuffer[i]->string_ptr );
                    AddLineQueueX( " push word ptr %s", AsmBuffer[i]->string_ptr );
                }
                if ( curr->is_vararg ) {
                    size_vararg += pushsize;
                    DebugMsg1(("PushInvokeParm(%u): asize=%u added to size_vararg, now=%u\n", reqParam, pushsize, size_vararg ));
                }
            }
        } else { /* the parameter is a register or constant value! */
            char is_r0 = FALSE;
            if ( opndx.kind == EXPR_REG ) {
                int reg = AsmBuffer[opndx.base_reg]->value;
                if ( reg == T_AH || ( GetValueSp( reg ) & OP_A ) ) {
                    is_r0 = TRUE;
                    if ( *r0flags & R0_USED ) {
                        AsmErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                        *r0flags = 0;
                    }
                }
                if ( asize != psize || asize < pushsize ) {
                    /* register size doesn't match the needed parameter size.
                     */
                    if ( psize > 4 ) {
                        DebugMsg1(("PushInvokeParm(%u): error, REG, asize=%u, psize=%u, pushsize=%u\n",
                                  reqParam, asize, psize, pushsize ));
                        AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    }
                    if ( asize <= 2 && ( psize == 4 || pushsize == 4 ) ) {
                        if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 &&
                            asize == psize ) {
                            if ( asize == 1 )
                                reg = reg - T_AL + T_EAX;
                            else
                                reg = reg - T_AX + T_EAX;
                            asize = 2; /* done */
                        } else if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_186 ) {
                            if ( pushsize == 4 ) {
                                if ( asize == 1 ) {
                                    ;
                                } else if ( psize <= 2 ) {
                                    AddLineQueueX( " sub %r, 2", T_ESP );
                                } else {
                                    AddLineQueue( " pushw 0" );
                                }
                            } else
                                AddLineQueue( " pushw 0" );
                        } else {
                            if ( !(*r0flags & R0_X_CLEARED) )
                                AddLineQueueX( " xor %r, %r", T_AX, T_AX );
                            AddLineQueueX( " push %r", T_AX );
                            *r0flags = R0_USED | R0_H_CLEARED | R0_X_CLEARED;
                        }
                    }

                    if ( asize == 1 ) {
                        if ( ( reg >= T_AH && reg <= T_BH ) || psize > 1 ) {
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                                if ( pushsize == 4 )
                                    reg = T_EAX;
                                else
                                    reg = T_AX;
                                AddLineQueueX( " movzx %r, %s", reg, fullparam );
                                *r0flags |= (R0_USED | R0_H_CLEARED );
                            } else {
                                if ( reg != T_AL ) {
                                    AddLineQueueX( " mov %r, %s", T_AL, fullparam );
                                    *r0flags |= R0_USED;
                                    *r0flags &= ~R0_X_CLEARED;
                                }
                                if (!( *r0flags & R0_H_CLEARED )) {
                                    AddLineQueueX( " mov %r, 0", T_AH );
                                    *r0flags |= R0_H_CLEARED;
                                }
                                reg = T_AX;
                            }
                        } else {
                            /* convert 8-bit to 16/32-bit register name */
                            if ( (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386) &&
                                ( psize == 4 || pushsize == 4 ) ) {
                                reg = reg - T_AL + T_EAX;
                            } else
                                reg = reg - T_AL + T_AX;
                        }
                    }
                    if ( is_r0 && ( *r0flags & R0_USED ) ) {
                        AsmErr( REGISTER_VALUE_OVERWRITTEN_BY_INVOKE );
                        *r0flags = 0;
                    }
                }
                AddLineQueueX( " push %r", reg );
                /* v2.04: adjust psize ( for siz_vararg update ) */
                psize = pushsize;
            } else { /* constant value */
                asize = CurrWordSize;
                if ( psize < pushsize )  /* ensure that the default pushsize is met */
                    if ( psize == 0 && curr->is_vararg ) {
                        /* v2.04: push a dword constant in 16-bit */
                        if ( pushsize == 2 &&
                            ( opndx.value > 0xFFFFL || opndx.value < -65535L ) )
                            psize = 4;
                        else
                            psize = pushsize;
                    } else
                        psize = pushsize;

                if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_186 ) {
                    *r0flags |= R0_USED;
                    switch ( psize ) {
                    case 2:
                        if ( opndx.value != 0 || opndx.kind == EXPR_ADDR ) {
                            AddLineQueueX( " mov %r, %s", T_AX, fullparam );
                        } else {
                            if ( !(*r0flags & R0_X_CLEARED ) ) {
                                AddLineQueueX( " xor %r, %r", T_AX, T_AX );
                            }
                            *r0flags |= R0_H_CLEARED | R0_X_CLEARED;
                        }
                        break;
                    case 4:
                        if ( opndx.uvalue <= 0xFFFF )
                            AddLineQueueX( " xor %r, %r", T_AX, T_AX );
                        else
                            AddLineQueueX( " mov %r, HIGHWORD (%s)", T_AX, fullparam );
                        AddLineQueueX( " push %r", T_AX );
                        if ( opndx.uvalue != 0 || opndx.kind == EXPR_ADDR ) {
                            AddLineQueueX( " mov %r, LOWWORD (%s)", T_AX, fullparam );
                        } else {
                            *r0flags |= R0_H_CLEARED | R0_X_CLEARED;
                        }
                        break;
                    default:
                        DebugMsg1(("PushInvokeParm(%u): error, CONST, asize=%u, psize=%u, pushsize=%u\n",
                                  reqParam, asize, psize, pushsize ));
                        AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                    }
                    AddLineQueueX( " push %r", T_AX );
                } else {
                    uint_16 instr = T_PUSH;
                    char *qual = NULL;
                    if ( asize != psize ) {
                        switch ( psize ) {
                        case 2:
                            instr = T_PUSHW;
                            break;
                        case 6: /* v2.04: added */
                            AddLineQueueX( " pushw (%s) shr 32", fullparam );
                            /* no break */
                        case 4:
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 )
                                instr = T_PUSHD;
                            else {
                                AddLineQueueX( " pushw HIGHWORD (%s)", fullparam );
                                instr = T_PUSHW;
                                qual = "LOWWORD";
                            }
                            break;
                        case 8:
#if AMD64_SUPPORT
                            if (( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_64 )
                                break;
#endif
                        default:
                            DebugMsg1(("PushInvokeParm(%u): error, CONST, asize=%u, psize=%u, pushsize=%u\n",
                                      reqParam, asize, psize, pushsize ));
                            AsmErr( INVOKE_ARGUMENT_TYPE_MISMATCH, reqParam+1 );
                        }
                    }
                    if ( qual )
                        AddLineQueueX( " %r %s (%s)", instr, qual, fullparam );
                    else
                        AddLineQueueX( " %r %s", instr, fullparam );
                }
            }
            if ( curr->is_vararg ) {
                size_vararg += psize;
                DebugMsg1(("PushInvokeParm(%u): psize=%u added to size_vararg, now=%u\n", reqParam, psize, size_vararg ));
            }
        }
    }
    return( NOT_ERROR );
}

/* generate a call for a prototyped procedure */

ret_code InvokeDirective( int i )
/*******************************/
{
    struct asm_sym *sym;
    dir_node       *proc;
    char           *name;
    //char         *param;
    int            numParam;
    int            value;
    int            namepos = i+1;
    uint_8         r0flags = 0;
    bool           uselabel = FALSE;
    proc_info      *info;
    dir_node       *curr;
    expr_list      opndx;
    char           buffer[MAX_LINE_LEN];

    DebugMsg1(("InvokeDef(%s) enter\n", AsmBuffer[i]->pos ));

    i++; /* skip INVOKE directive */

#if FASTPASS
    /* make sure the directive is stored */
    if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
        SaveState();
    }
#endif
    /* if there is more than 1 item describing the invoke target,
     use the expression evaluator to get it
     */
    sym = NULL;
    if ( AsmBuffer[i]->token != T_ID || ( AsmBuffer[i+1]->token != T_COMMA && AsmBuffer[i+1]->token != T_FINAL ) ) {
    //if ( AsmBuffer[i+1]->token != T_COMMA && AsmBuffer[i+1]->token != T_FINAL ) {
        if ( ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE ) )
            return( ERROR );
        DebugMsg(("InvokeDef: target is expression, opndx->sym=%X, opndx->mbr=%X, opndx->type=%X\n", opndx.sym, opndx.mbr, opndx.type ));
#if 1
        /* a typecast with PTR? Since v1.95, this has highest priority */
        //if (opndx.explicit == TRUE && opndx.type != NULL && opndx.type->state == SYM_TYPE ) {
        /* v1.96: removed opndx.explicit!!! */
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
            /* it may be a typecast. then the mbr member contains the explicit type
             * and sym->state is SYM_TYPE
             * v1.96: the sentence above describes an obsolete feature.
             * Not sure if <mbr> can contain a type anymore.
             * this code is to be removed/modified! */
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
        } else if ( opndx.kind == EXPR_REG ) {
            if ( GetValueSp( AsmBuffer[opndx.base_reg]->value ) & OP_RGT8 ) {
                if ( sym = GetStdAssume( GetRegNo( AsmBuffer[opndx.base_reg]->value ) ) ) {
                    proc = (dir_node *)sym;
                    if ( proc->sym.mem_type == MT_PROC )  /* added for v1.95 */
                        goto isfnproto;
                    goto isfnptr;
                }
            }
        }
    } else {
        name = AsmBuffer[i]->string_ptr;
        sym = SymSearch( name );
        i++;
    }

    if( sym == NULL ) {
        /* v2.04: msg changed */
        AsmErr( INVOKE_REQUIRES_PROTOTYPE );
        //AsmErr( SYMBOL_NOT_DEFINED, name );
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

    /* does FASTCALL variant support INVOKE? */

    if ( proc->sym.langtype == LANG_FASTCALL && fastcall_tab[Options.fastcall].invokestart == NULL ) {
        AsmError( FASTCALL_VARIANT_NOT_SUPPORTED );
        return( ERROR );
    }

    /* get the number of parameters */

    for ( curr = info->paralist, numParam = 0 ; curr ; curr = curr->nextparam, numParam++ );
    DebugMsg1(("InvokeDef: numparams=%u\n", numParam ));

    curr = info->paralist;

    PushLineQueue();

    if ( proc->sym.langtype == LANG_FASTCALL ) {
        fcscratch = 0;
        fastcall_tab[Options.fastcall].invokestart( proc, numParam, i, &value );
    }

    if ( !( info->is_vararg ) ) {
        /* check if there is a superfluous parameter in the INVOKE call */
        if ( PushInvokeParam( proc, NULL, i, numParam, &r0flags ) != ERROR ) {
            DebugMsg(("InvokeDef: superfluous argument, i=%u\n", i));
            AsmErr( TOO_MANY_ARGUMENTS_TO_INVOKE );
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
        for ( ; j >= numParam; j-- )
            PushInvokeParam( proc, curr, i, j, &r0flags );
        /* VARARG procs have at least 1 param, the VARARG param */
        curr = curr->nextparam;
    }

    /* the parameters are usually stored in "push" order */

    if ( sym->langtype == LANG_STDCALL ||
        sym->langtype == LANG_C ||
#if AMD64_SUPPORT
        /* since Win64 fastcall doesn't push, it's a better/faster strategy to 
         * handle the arguments the other way
         */
        (sym->langtype == LANG_FASTCALL && sym->Ofssize != USE64 ) ||
#else
        sym->langtype == LANG_FASTCALL ||
#endif
        sym->langtype == LANG_SYSCALL ) {
        for ( ; curr ; curr = curr->nextparam ) {
            numParam--;
            if ( PushInvokeParam( proc, curr, i, numParam, &r0flags ) == ERROR ) {
                DebugMsg(("PushInvokeParam(curr=%u, i=%u, numParam=%u) failed\n", curr, i, numParam));
                AsmErr( TOO_FEW_ARGUMENTS_TO_INVOKE, sym->name );
            }
        }
    } else {
        for ( numParam = 0 ; curr ; curr = curr->nextparam, numParam++ ) {
            if ( PushInvokeParam( proc, curr, i, numParam, &r0flags ) == ERROR ) {
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
            if ( buffer[6] != NULLC && is_valid_id_char( *(AsmBuffer[namepos]->string_ptr ) ) )
                strcat( buffer," " );
            strcat( buffer, AsmBuffer[namepos]->string_ptr );
        }
    AddLineQueue( buffer );

    if (( sym->langtype == LANG_C || sym->langtype == LANG_SYSCALL ) &&
        ( info->parasize || ( info->is_vararg && size_vararg ) )) {
        if ( info->is_vararg ) {
            DebugMsg1(("InvokeDef: size of fix args=%u, var args=%u\n", info->parasize, size_vararg));
            AddLineQueueX( " add %r, %u", regsp[ModuleInfo.Ofssize], NUMQUAL info->parasize + size_vararg );
        } else
            AddLineQueueX( " add %r, %u", regsp[ModuleInfo.Ofssize], NUMQUAL info->parasize );
    } else if ( sym->langtype == LANG_FASTCALL ) {
        fastcall_tab[Options.fastcall].invokeend( proc, numParam, value );
    }

    LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    RunLineQueue();

    return( NOT_ERROR );
}

