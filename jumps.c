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
* Description:  CALL/JMP processing. Includes
*               - "far call optimisation": a "call FAR ptr <proc>" is
*                 exchanged by a "push cs" + "call NEAR ptr <proc>".
*               - "short jump extension": a conditional jump with a
*                 destination not within the SHORT range is exchanged
*                 by "j<cond> $+3|5" and "jmp <dest>" if cpu is < 386
*                 (see OPTION LJMP | NOLJMP).
****************************************************************************/


#include "globals.h"

#include "parser.h"
#include "codegen.h"
#include "fixup.h"
#include "expreval.h"
#include "directiv.h"
#include "fastpass.h"

#if defined( _STANDALONE_ )
  #include "directiv.h"
  #include "input.h"
#endif

/* prototypes */
int ptr_operator( memtype mem_type, uint_8 fix_mem_type );
int jmp( expr_list *opndx );

#if defined( _STANDALONE_ )

extern void             GetInsString( enum asm_token, char *, int );
extern void             check_assume( struct asm_sym *sym, enum prefix_reg default_reg );

static enum asm_token getJumpNegation( enum asm_token instruction )
/*****************************************************************/
{
    switch( instruction ) {
    case T_JA:          return( T_JNA );
    case T_JAE:         return( T_JNAE );
    case T_JB:          return( T_JNB );
    case T_JBE:         return( T_JNBE );
    case T_JC:          return( T_JNC );
    case T_JE:          return( T_JNE );
    case T_JG:          return( T_JNG );
    case T_JGE:         return( T_JNGE );
    case T_JL:          return( T_JNL );
    case T_JLE:         return( T_JNLE );
    case T_JNA:         return( T_JA );
    case T_JNAE:        return( T_JAE );
    case T_JNB:         return( T_JB );
    case T_JNBE:        return( T_JBE );
    case T_JNC:         return( T_JC );
    case T_JNE:         return( T_JE );
    case T_JNG:         return( T_JG );
    case T_JNGE:        return( T_JGE );
    case T_JNL:         return( T_JL );
    case T_JNLE:        return( T_JLE );
    case T_JNO:         return( T_JO );
    case T_JNP:         return( T_JP );
    case T_JNS:         return( T_JS );
    case T_JNZ:         return( T_JZ );
    case T_JO:          return( T_JNO );
    case T_JP:          return( T_JNP );
    case T_JPE:         return( T_JPO );
    case T_JPO:         return( T_JPE );
    case T_JS:          return( T_JNS );
    case T_JZ:          return( T_JNZ );
    default:
        return( (enum asm_token)ERROR );
    }
}

// "short jump extension": extend a (conditional) jump.
// example:
// "jz label"
// is converted to
// "jnz SHORT $+3|5"
// "jmp label"

static void jumpExtend( int far_flag )
/*************************************/
{
    unsigned i;
    unsigned next_ins_size;
    enum asm_token negation;
    char buffer[MAX_LINE_LEN];

    /* there MUST be a conditional jump instruction in asmbuffer */
    for( i = 0; ; i++ ) {
        if( ( AsmBuffer[i]->token == T_INSTRUCTION )
            && IS_JMP( AsmBuffer[i]->value ) ) {
            break;
        }
    }

    PushLineQueue();

    if( Parse_Pass == PASS_2 )
        AsmWarn( 4, EXTENDING_JUMP );

    negation = getJumpNegation( AsmBuffer[i]->value );
    GetInsString( negation, buffer, MAX_LINE_LEN );
    if( far_flag ) {
        next_ins_size = CodeInfo->use32 ? 7 : 5;
    } else {
        next_ins_size = CodeInfo->use32 ? 5 : 3;
    }
    sprintf( buffer + strlen( buffer ), " SHORT $+%u", next_ins_size+2 );
#if FASTPASS
    InputQueueLineEx( buffer, 0 );
#else
    InputQueueLine( buffer );
#endif

#if 1
    strcpy( buffer, "jmp " );
    /* In JWasm the original source is stored in AsmBuffer->pos */
    strcat( buffer, AsmBuffer[i+1]->pos );
#else
    if( far_flag ) {
        strcpy( buffer, "jmpf " ); /* JWasm won't accept "jmpf" */
    } else {
        strcpy( buffer, "jmp " );
    }
    for( i++; AsmBuffer[i]->token != T_FINAL; i++ ) {
        switch( AsmBuffer[i]->token ) {
        case T_NUM:
            itoa( AsmBuffer[i]->value, buffer+strlen( buffer ), 10 );
            break;
        case T_OP_SQ_BRACKET:
            strcat( buffer, "[" );
            break;
        case T_CL_SQ_BRACKET:
            strcat( buffer, "]" );
            break;
        default:
            strcat( buffer, AsmBuffer[i]->string_ptr );
            break;
        }
    }
#endif
#if FASTPASS
    InputQueueLineEx( buffer, 1 );
#else
    InputQueueLine( buffer );
#endif
    return;
}

// "far call optimisation": a far call is done to a near label
// optimize (call SSSS:OOOO -> PUSH CS, CALL OOOO)

static void FarCallToNear( void )
/*******************************/
{
    unsigned i;
    char buffer[MAX_LINE_LEN];

    /* there MUST be a call instruction in asmbuffer */
    for( i = 0; ; i++ ) {
        if( ( AsmBuffer[i]->token == T_INSTRUCTION )
            && ( AsmBuffer[i]->value == T_CALL ) ) {
            break;
        }
    }
    if( Parse_Pass == PASS_2 )
        AsmWarn( 4, CALL_FAR_TO_NEAR );

    PushLineQueue();

#if FASTPASS
    InputQueueLineEx( "PUSH CS", 0 );
#else
    InputQueueLine( "PUSH CS" );
#endif
    strcpy( buffer, "CALL NEAR PTR " );
#if 1
    /* In JWasm the original source is stored in AsmBuffer->pos */
    strcat( buffer, AsmBuffer[i+1]->pos);
#else
    for( i++; AsmBuffer[i]->token != T_FINAL; i++ ) {
        switch( AsmBuffer[i]->token ) {
        case T_NUM:
            itoa( AsmBuffer[i]->value, buffer+strlen( buffer ), 10 );
            break;
        case T_OP_SQ_BRACKET:
            strcat( buffer, "[" );
            break;
        case T_CL_SQ_BRACKET:
            strcat( buffer, "]" );
            break;
        default:
            strcat( buffer, AsmBuffer[i]->string_ptr );
            break;
        }
    }
#endif
#if FASTPASS
    InputQueueLineEx( buffer, 1 );
#else
    InputQueueLine( buffer );
#endif
    return;
}
#endif

int jmp( expr_list *opndx )
/*
 determine the displacement of jmp;
 current instruction is CALL, JMP, Jxx, LOOPx, JCXZ or JECXZ
 possible return values are: NOT_ERROR, ERROR, SCRAP_INSTRUCTION, INDIRECT_JUMP
*/
{
    int_32              addr;
    enum fixup_types    fixup_type;
    enum fixup_options  fixup_option;
    enum sym_state      state;
    struct asm_sym      *sym;
#if defined( _STANDALONE_ )
    dir_node            *seg;
#endif

    DebugMsg(("Jmp(sym=%X) enter\n", opndx->sym));
    CodeInfo->data[Opnd_Count] = opndx->value;
    sym = opndx->sym;
    if( sym == NULL ) {
        if( IS_JMPCALL( CodeInfo->info.token ) )
            CodeInfo->isfar = TRUE;
        if( CodeInfo->data[Opnd_Count] > USHRT_MAX )
            CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
        else
            CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
        return( NOT_ERROR );
    }
    DebugMsg(("Jmp: sym=>%s<, state=%u, mem_type=%u, ofs=%X\n", sym->name, sym->state, sym->mem_type, sym->offset));

#if 0 // defined( _STANDALONE_ )
    /* MT_ERROR will never be set in a symbol! */
    /* undefined labels are detected in the expression evaluator */
    if( sym->mem_type == MT_ERROR ) {
        AsmErr( LABEL_NOT_DEFINED, sym->name );
        return( ERROR );
    }
#endif
    state = sym->state;
#if defined( _STANDALONE_ )
    seg = GetSeg( sym );
    if( seg == NULL || CurrSeg == NULL || CurrSeg->seg != seg ) {
        /* if label has a different segment and jump/call is near,
         report an error */
        if (MAGIC_FLAT_GROUP != 0)
            ;
        else if (seg != NULL && CurrSeg != NULL)
            /* if the segments belong to the same group, it's ok */
            if (((dir_node *)seg)->e.seginfo->group != NULL &&
                CurrSeg->seg->e.seginfo->group != NULL &&
                ((dir_node *)seg)->e.seginfo->group == CurrSeg->seg->e.seginfo->group)
                ;
            else if (opndx->mem_type == MT_NEAR) {
                AsmError(NO_FAR_JUMP_TO_NEAR_LABEL);
                return( ERROR );
            }
        /* jumps to another segment are just like to another file */
        state = SYM_EXTERNAL;
    }
#endif

    if( !CodeInfo->mem_type_fixed ) {
        CodeInfo->mem_type = MT_EMPTY;
    }
    fixup_option = OPTJ_NONE;
    fixup_type = FIX_RELOFF8;
    switch( state ) {
    case SYM_UNDEFINED:
        SetSymSegOfs(sym);
        /* fall through */
    case SYM_INTERNAL:
#if defined( _STANDALONE_ )
    case SYM_PROC:
#endif
        if(  ( CodeInfo->mem_type == MT_EMPTY || CodeInfo->mem_type == MT_SHORT
                || CodeInfo->mem_type == MT_NEAR )
            && sym->mem_type != MT_WORD     /* the symbol's memtype shouldn't */
            && sym->mem_type != MT_DWORD    /* be used (type casts!) */
            && sym->mem_type != MT_FWORD
            && CodeInfo->isfar == FALSE ) {
#if defined( _STANDALONE_ )
            if( ( CodeInfo->info.token == T_CALL )
                && ( CodeInfo->mem_type == MT_EMPTY )
                && ( sym->mem_type == MT_FAR ) ) {
                FarCallToNear();
                return( SCRAP_INSTRUCTION );
            }
            addr = sym->offset;
#else
            addr = sym->addr;
#endif
            addr -= ( GetCurrOffset() + 2 );  // calculate the displacement
            addr += CodeInfo->data[Opnd_Count];
            switch( CodeInfo->info.token ) {
            case T_JCXZ:
            case T_LOOPW:
            case T_LOOPEW:
            case T_LOOPZW:
            case T_LOOPNEW:
            case T_LOOPNZW:
                if( CodeInfo->use32 ) {
                    // 1 extra byte for OPNSIZ
                    addr--;
                }
                break;
            case T_JECXZ:
            case T_LOOPD:
            case T_LOOPED:
            case T_LOOPZD:
            case T_LOOPNED:
            case T_LOOPNZD:
                if( !CodeInfo->use32 ) {
                    // 1 extra byte for OPNSIZ
                    addr--;
                }
                break;
            }
            if( CodeInfo->info.token == T_CALL && CodeInfo->mem_type == MT_EMPTY ) {
                CodeInfo->mem_type = MT_NEAR;
            }
            if( CodeInfo->mem_type != MT_NEAR && CodeInfo->info.token != T_CALL
                && ( addr >= SCHAR_MIN && addr <= SCHAR_MAX ) ) {
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
            } else {
                /* near jmp */
                if( CodeInfo->use32 ) {
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                    addr -= 3; // 32 bit displacement
                } else {
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
                    addr -= 1; // 16 bit displacement
                }
                if( IS_JMP( CodeInfo->info.token ) ) {
                    switch( CodeInfo->info.token ) {
                    case T_JMP:
                    case T_JCXZ:
                    case T_JECXZ:
                        break;
                    default: /* the rest are the conditional jumps */
                        // 1 extra byte for opcode ( 0F )
                        addr--;
                        break;
                    }
                }
            }

            /* store the displacement */
            CodeInfo->data[Opnd_Count] = addr;

#if defined( _STANDALONE_ )
#define GOOD_PHASE  !PhaseError &&
#else
#define GOOD_PHASE
#endif
            if ( CodeInfo->info.token == T_JCXZ ||
                 CodeInfo->info.token == T_JECXZ ||
                 (CodeInfo->info.token >= T_LOOP &&
                  CodeInfo->info.token <= T_LOOPZW) ) {
                if( GOOD_PHASE (CodeInfo->info.opnd_type[Opnd_Count] != OP_I8) ) {
                    AsmError( JUMP_OUT_OF_RANGE );
                    return( ERROR );
                }
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                break;
            }
            /* remaining instructions: CALL, JMP, Jxx */

            /* automatic (conditional) jump expansion */
            /* for 386 and above this is not needed, since there exists
             an extended version of Jxx */
            if( (curr_cpu & P_CPU_MASK) < P_386 &&
                CodeInfo->info.token != T_CALL &&
                CodeInfo->info.token != T_JMP) {
                /* look into jump extension */
                if( CodeInfo->info.opnd_type[Opnd_Count] != OP_I8 ) {
#if defined( _STANDALONE_ )
                    if( CodeInfo->mem_type == MT_EMPTY && ModuleInfo.ljmp == TRUE) {
                        jumpExtend( FALSE );
                        return( SCRAP_INSTRUCTION );
                    } else if( !PhaseError ) {
                        AsmError( JUMP_OUT_OF_RANGE );
                        return( ERROR );
                    }
#else
                    AsmError( JUMP_OUT_OF_RANGE );
                    return( ERROR );
#endif
                }
            }
            break;
        }
        /* otherwise fall through & get handled like external symbols */
//    case SYM_UNDEFINED:
    case SYM_EXTERNAL:

        /* forward ref, or external symbol */
        if( CodeInfo->mem_type == MT_EMPTY && sym->mem_type != MT_EMPTY ) {
            switch( sym->mem_type ) {
            case MT_FAR:
                if( IS_JMPCALL( CodeInfo->info.token ) ) {
                    CodeInfo->isfar = TRUE;
                }
                // fall through
            case MT_SHORT:
            case MT_NEAR:
                CodeInfo->mem_type = sym->mem_type;
                break;
#if defined( _STANDALONE_ )
            case MT_PROC:
                CodeInfo->mem_type = IS_PROC_FAR() ? MT_FAR : MT_NEAR;
                if( IS_JMPCALL( CodeInfo->info.token )
                    && ( CodeInfo->mem_type == MT_FAR ) ) {
                    CodeInfo->isfar = TRUE;
                }
                break;
#endif
            case MT_FWORD:
                if( ptr_operator( MT_FWORD, TRUE ) == ERROR )
                    return( ERROR );
                break;
            default:
                CodeInfo->mem_type = sym->mem_type;
            }
        }

        /* handle far JMP + CALL? */
        if (IS_JMPCALL( CodeInfo->info.token ) &&
            (CodeInfo->isfar == TRUE ||
             CodeInfo->mem_type == MT_FAR)) {
            switch( CodeInfo->mem_type ) {
            case MT_SHORT:
            case MT_NEAR:
                if( Opnd_Count == OPND1 && CodeInfo->mem_type_fixed ) {
                    AsmError( CANNOT_USE_SHORT_OR_NEAR );
                    return( ERROR );
                }
                /* fall through */
            case MT_FAR:
            case MT_EMPTY:
#if defined( _STANDALONE_ )
                SET_OPSIZ( CodeInfo, SymIs32( sym ));
                find_frame( sym );
#endif
                if( Opnd_Count == OPND2 ) {
                    if( oper_32( CodeInfo ) ) {
                        fixup_type = FIX_OFF32;
                        CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                    } else {
                        fixup_type = FIX_OFF16;
                        CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
                    }
                } else {
                    if( oper_32( CodeInfo ) ) {
                        fixup_type = FIX_PTR32;
                        CodeInfo->info.opnd_type[Opnd_Count] = OP_J48;
                    } else {
                        fixup_type = FIX_PTR16;
                        CodeInfo->info.opnd_type[Opnd_Count] = OP_J32;
                    }
                }
                break;
            case MT_BYTE:
            case MT_WORD:
#if defined( _STANDALONE_ )
            case MT_SBYTE:
            case MT_SWORD:
#endif
                AsmError( INVALID_SIZE );
                return( ERROR );
            case MT_DWORD:
            case MT_FWORD:
#if defined( _STANDALONE_ )
            case MT_SDWORD:
#endif
                return( INDIRECT_JUMP );
            case MT_QWORD:
            case MT_TBYTE:
            case MT_OWORD:
                AsmError( INVALID_SIZE );
                return( ERROR );
            }
            AddFixup( sym, fixup_type, fixup_option );
            return(NOT_ERROR);
        }  /* end FAR JMP/CALL */

        switch( CodeInfo->info.token ) {
        case T_CALL:
            if( CodeInfo->mem_type == MT_SHORT ) {
                AsmError( CANNOT_USE_SHORT_WITH_CALL );
                return( ERROR );
            } else if( CodeInfo->mem_type == MT_EMPTY ) {
#if defined( _STANDALONE_ )
                fixup_option = OPTJ_CALL;
#else
                fixup_option = OPTJ_NONE;
#endif
                if( CodeInfo->use32 ) {
                    fixup_type = FIX_RELOFF32;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                } else {
                    fixup_type = FIX_RELOFF16;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
                }
                break;
            }
            /* fall through */
        case T_JMP:
            switch( CodeInfo->mem_type ) {
            case MT_SHORT:
                fixup_option = OPTJ_EXPLICIT;
                fixup_type = FIX_RELOFF8;
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                break;
            case MT_FAR:
                AsmError( SYNTAX_ERROR );
                break;
            case MT_EMPTY:
                // forward reference
                // inline assembler jmp default distance is near
                // stand-alone assembler jmp default distance is short
                fixup_option = OPTJ_NONE;
#if defined( _STANDALONE_ )
                /* guess short if JMP, we will expand later if needed */
                fixup_type = FIX_RELOFF8;
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
#else
                if( CodeInfo->use32 ) {
                    fixup_type = FIX_RELOFF32;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                } else {
                    fixup_type = FIX_RELOFF16;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
                }
#endif
                break;
            case MT_NEAR:
                fixup_option = OPTJ_EXPLICIT;
                if( CodeInfo->use32 ) {
                    fixup_type = FIX_RELOFF32;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                } else {
                    fixup_type = FIX_RELOFF16;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
                }
                break;
            case MT_DWORD:
            case MT_WORD:
#if defined( _STANDALONE_ )
            case MT_SDWORD:
            case MT_SWORD:
#endif
                return( INDIRECT_JUMP );
#if defined( _STANDALONE_ )
            case MT_SBYTE:
#endif
            case MT_BYTE:
            case MT_FWORD:
            case MT_QWORD:
            case MT_TBYTE:
            case MT_OWORD:
                AsmError( INVALID_SIZE );
                return( ERROR );
            }
//            check_assume( sym, EMPTY );
            break;
        default: /* JCXZ, JECXZ, LOOPxx, Jxx */
            // JCXZ, JECXZ and LOOPxx always require SHORT label
            if ( CodeInfo->info.token == T_JCXZ ||
                 CodeInfo->info.token == T_JECXZ ||
                 (CodeInfo->info.token >= T_LOOP &&
                  CodeInfo->info.token <= T_LOOPZW) ) {
                if( CodeInfo->mem_type != MT_EMPTY && CodeInfo->mem_type != MT_SHORT ) {
                    AsmError( ONLY_SHORT_DISPLACEMENT_IS_ALLOWED );
                    return( ERROR );
                }
                CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                fixup_option = OPTJ_EXPLICIT;
                fixup_type = FIX_RELOFF8;
                break;
            }
            /* just Jxx remaining */
            if( (curr_cpu & P_CPU_MASK) >= P_386 ) {
                switch( CodeInfo->mem_type ) {
                case MT_SHORT:
                    fixup_option = OPTJ_EXPLICIT;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                    break;
                case MT_EMPTY:
                    // forward reference
                    // inline assembler default distance is near
                    // stand-alone assembler default distance is short
#if defined( _STANDALONE_ )
                    fixup_option = OPTJ_JXX;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                    break;
#endif
                case MT_NEAR:
                    fixup_option = OPTJ_EXPLICIT;
                    if( CodeInfo->use32 ) {
                        fixup_type = FIX_RELOFF32;
                        CodeInfo->info.opnd_type[Opnd_Count] = OP_I32;
                    } else {
                        fixup_type = FIX_RELOFF16;
                        CodeInfo->info.opnd_type[Opnd_Count] = OP_I16;
                    }
                    break;
                case MT_FAR:
                    if (ModuleInfo.ljmp) {
                        jumpExtend( TRUE );
                        return( SCRAP_INSTRUCTION );
                    }
                default:
                    AsmError( ONLY_SHORT_AND_NEAR_DISPLACEMENT_IS_ALLOWED );
                    return( ERROR );
                }
            } else {
                // the only mode in 8086, 80186, 80286 is
                // Jxx SHORT
                switch( CodeInfo->mem_type ) {
                case MT_EMPTY:
#if defined( _STANDALONE_ )
                    fixup_option = OPTJ_EXTEND;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                    break;
#endif
                case MT_SHORT:
                    fixup_option = OPTJ_EXPLICIT;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->info.opnd_type[Opnd_Count] = OP_I8;
                    break;
                case MT_FAR:
                    if (ModuleInfo.ljmp) {
                        jumpExtend( TRUE );
                        return( SCRAP_INSTRUCTION );
                    }
                default:
                    AsmError( ONLY_SHORT_DISPLACEMENT_IS_ALLOWED );
                    return( ERROR );
                }
            }
        }
        AddFixup( sym, fixup_type, fixup_option );
        break;
    default: /* SYM_STACK */
        DebugMsg(("jmp, error, sym=%s, state=%u, memtype=%u\n", sym->name, sym->state, sym->mem_type));
        AsmError( NO_JUMP_TO_AUTO );
        return( ERROR );
    }
    return( NOT_ERROR );
}

int ptr_operator( memtype mem_type, uint_8 fix_mem_type )
/***********************************************/
/*
  determine what should be done with SHORT, NEAR, FAR, BYTE, WORD, DWORD, PTR
  operator;
  out: CodeInfo->mem_type
       CodeInfo->mem_type_fixed
       CodeInfo->isfar
       CodeInfo->prefix.opsiz
*/
{
    /* new idea:
     * when we get a near/far/dword/etc, just set distance / mem_type
     * operator will be called again with PTR, then we set the opsiz, etc.
     */
    if( CodeInfo->info.token == T_LEA )
        return( NOT_ERROR );
    if( CodeInfo->info.token == T_SMSW )
        return( NOT_ERROR );
    if( mem_type == MT_PTR ) {
        /* finish deciding what type to make the inst NOW
         * ie: decide size overrides etc.
         */
        if( CodeInfo->use32 && MEM_TYPE( CodeInfo->mem_type, WORD ) ) {
            // if we are in use32 mode, we have to add OPSIZ prefix for
            // most of the 386 instructions ( except MOVSX and MOVZX )
            // when we find WORD PTR

            if( !IS_BRANCH( CodeInfo->info.token ) ) {
                if( CodeInfo->info.opnd_type[OPND1] == OP_MMX ) {
                /* JBS 2001/02/19
                no WORD operands for MMX instructions, only 64-bit or 128-bit
                so no WORD override needed
                    */
                } else {
                    switch( CodeInfo->info.token ) {
                    case T_MOVSX:
                    case T_MOVZX:
                        break;
                    default:
                        CodeInfo->prefix.opsiz = TRUE;
                        break;
                    }
                }
            }

        } else if( !CodeInfo->use32 && MEM_TYPE( CodeInfo->mem_type, DWORD ) ) {

            /* if we are not in use32 mode, we have to add OPSIZ
             * when we find DWORD PTR
             * unless we have a LXS ins.
             * which moves a DWORD ptr into SR:word reg
             * fixme  - can this be done by breaking up the LXS instructions in
             *          asmins.h, and then putting F_32 or F_16 to append
             *      opsize bytes when necessary ?
             */
            if( !IS_BRANCH( CodeInfo->info.token ) ) {

                if( CodeInfo->info.opnd_type[OPND1] == OP_MMX ) {
                    /* JBS 2001/02/19
                       no WORD operands for MMX instructions, only 64-bit or 128-bit
                       so no DWORD override needed
                     */
                } else {
                    switch( CodeInfo->info.token ) {
                    case T_LDS:
                    case T_LES:
                    case T_LFS:
                    case T_LGS:
                    case T_LSS:
                        /* in these cases, opsize does NOT need to be changed  */
                        break;
                    default:
                        // OPSIZ prefix
                        CodeInfo->prefix.opsiz = TRUE;
                    }
                }
            }
        }
    } else {
        if( ( mem_type != MT_EMPTY ) && ( CodeInfo->mem_type_fixed == FALSE ) ) {
#if defined( _STANDALONE_ )
            if( mem_type != MT_TYPE ) {
#endif
                CodeInfo->mem_type = mem_type;
                if( fix_mem_type ) {
                    CodeInfo->mem_type_fixed = TRUE;
                    if( IS_JMPCALL( CodeInfo->info.token ) && ( mem_type == MT_FAR ) ) {
                        CodeInfo->isfar = TRUE;
                    }
                }

#if defined( _STANDALONE_ )
            }
#endif
        }
    }
    return( NOT_ERROR );
}
