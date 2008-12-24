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
* Description:  parser CALL/JMP processing. Includes
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
#include "directiv.h"
#include "segment.h"
#include "input.h"
#include "assume.h"

extern void     Set_Memtype( memtype mem_type, bool fix_mem_type );

// "short jump extension": extend a (conditional) jump.
// example:
// "jz label"
// is converted to
// "jnz SHORT $+x"  ( x = sizeof(next ins), may be 3|5|6|7|8 )
// "jmp label"

static void jumpExtend( int far_flag )
/*************************************/
{
    uint_8 opcode;
    unsigned next_ins_size;

    if( Parse_Pass == PASS_2 )
        AsmWarn( 4, EXTENDING_JUMP );

    if( far_flag ) {
        if ( CodeInfo->prefix.opsiz ) {
            /* it's 66 EA OOOO SSSS or 66 EA OOOOOOOO SSSS */
            next_ins_size = CodeInfo->use32 ? 6 : 8;
        } else {
            /* it's EA OOOOOOOO SSSS or EA OOOO SSSS */
            next_ins_size = CodeInfo->use32 ? 7 : 5;
        }
    } else {
        /* it's E9 OOOOOOOO or E9 OOOO */
        next_ins_size = CodeInfo->use32 ? 5 : 3;
    }

    /* it's ensured that the short jump version is first in AsmOpTable */
    opcode = AsmOpTable[AsmResWord[CodeInfo->token].position].opcode;
    OutputCodeByte( opcode ^ 1 ); /* the negation is achieved by XOR 1 */
    OutputCodeByte( next_ins_size );
    CodeInfo->token = T_JMP;

    return;
}

// "far call optimisation": a far call is done to a near label
// optimize (call SSSS:OOOO -> PUSH CS, CALL OOOO)

static void FarCallToNear( void )
/*******************************/
{
    if( Parse_Pass == PASS_2 )
        AsmWarn( 4, CALL_FAR_TO_NEAR );

    OutputCodeByte( 0x0E ); /* 0x0E is "PUSH CS" opcode */
    CodeInfo->mem_type = MT_NEAR;
    CodeInfo->mem_type_fixed = TRUE;

    return;
}

ret_code jmp( expr_list *opndx )
/*
 called by idata_fixup() and idata_nofixup()
 ( that means, it isn't called for memory operands! )
 determine the displacement of jmp;
 current instruction is CALL, JMP, Jxx, LOOPx, JCXZ or JECXZ
 possible return values are:
 - NOT_ERROR,
 - ERROR,
 - INDIRECT_JUMP (is handled as error by the caller, but probably never occurs)
*/
{
    int_32              addr;
    enum fixup_types    fixup_type;
    enum fixup_options  fixup_option;
    enum sym_state      state;
    struct asm_sym      *sym;
    memtype             mem_type;
    dir_node            *seg;

    DebugMsg(("Jmp(sym=%X) enter\n", opndx->sym ));
    CodeInfo->data[Opnd_Count] = opndx->value;
    sym = opndx->sym;
    if( sym == NULL ) { /* the codegen most likely will reject this instr! */

        if( IS_JMPCALL( CodeInfo->token ) )
            CodeInfo->isfar = TRUE;

        if( CodeInfo->data[Opnd_Count] > USHRT_MAX )
            CodeInfo->opnd_type[Opnd_Count] = OP_I32;
        else
            CodeInfo->opnd_type[Opnd_Count] = OP_I16;

        return( NOT_ERROR );
    }
    DebugMsg(("Jmp: explicit=%u op.memtype=%u sym=>%s< state=%u mem_type=%u ofs=%X\n", opndx->explicit, opndx->mem_type, sym->name, sym->state, sym->mem_type, sym->offset));

    state = sym->state;
    seg = GetSeg( sym );
    if( seg == NULL || CurrSeg == NULL || CurrSeg->seg != seg ) {
        /* if label has a different segment and jump/call is near or short,
         report an error */
        if ( ModuleInfo.flatgrp_idx != 0 )
            ;
        else if (seg != NULL && CurrSeg != NULL) {
            /* if the segments belong to the same group, it's ok */
            if (((dir_node *)seg)->e.seginfo->group != NULL &&
                CurrSeg->seg->e.seginfo->group != NULL &&
                ((dir_node *)seg)->e.seginfo->group == CurrSeg->seg->e.seginfo->group)
                ;
            else if ( opndx->mem_type == MT_NEAR || opndx->mem_type == MT_SHORT ) {
                AsmError( NO_FAR_JUMP_TO_NEAR_LABEL );
                return( ERROR );
            }
        }
        /* jumps to another segment are just like to another file */
        state = SYM_EXTERNAL;
    }

    if( !CodeInfo->mem_type_fixed ) {
        CodeInfo->mem_type = MT_EMPTY;
    }
    fixup_option = OPTJ_NONE;
    fixup_type = FIX_RELOFF8;
    switch( state ) {
    case SYM_UNDEFINED:
        SetSymSegOfs( sym );
        /* fall through */
    case SYM_INTERNAL:
    case SYM_PROC:
        /* if a segment override is active,
         check if it's matching the assumed value of CS.
         If no, assume a FAR call.
         */
        if ( CodeInfo->prefix.SegOverride != NULL && CodeInfo->mem_type == MT_EMPTY ) {
            if ( CodeInfo->prefix.SegOverride != GetOverrideAssume( ASSUME_CS) ) {
                CodeInfo->mem_type = MT_FAR;
            }
        }
        if(  ( CodeInfo->mem_type == MT_EMPTY ||
               CodeInfo->mem_type == MT_SHORT ||
               CodeInfo->mem_type == MT_NEAR )
             && sym->mem_type != MT_WORD     /* the symbol's memtype shouldn't */
             && sym->mem_type != MT_DWORD    /* be used (type casts!) */
             && sym->mem_type != MT_FWORD
             && CodeInfo->isfar == FALSE ) {

            /* if the label is FAR - or there is a segment override
             which equals assumed value of CS - and there is no type cast,
             then do "far call translation".
             */
            if( CodeInfo->token == T_CALL &&
                CodeInfo->mem_type == MT_EMPTY &&
                ( sym->mem_type == MT_FAR || CodeInfo->prefix.SegOverride ) ) {
                FarCallToNear();
            }

            addr = sym->offset;
            addr -= ( GetCurrOffset() + 2 );  // calculate the displacement
            addr += CodeInfo->data[Opnd_Count];
            switch( CodeInfo->token ) {
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

            if( CodeInfo->token == T_CALL && CodeInfo->mem_type == MT_EMPTY ) {
                CodeInfo->mem_type = MT_NEAR;
            }

            if( CodeInfo->mem_type != MT_NEAR && CodeInfo->token != T_CALL
                && ( addr >= SCHAR_MIN && addr <= SCHAR_MAX ) ) {
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
            } else {
                /* near destination */
                /* is there a type coercion? */
                if ( opndx->ofs_size != OFSSIZE_EMPTY ) {
                    if ( opndx->ofs_size == OFSSIZE_16 ) {
                        CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                        addr -= 1; // 16 bit displacement
                    } else {
                        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                        addr -= 3; // 32 bit displacement
                    }
                    SET_OPSIZ( CodeInfo, opndx->ofs_size == OFSSIZE_32 );
                    if ( CodeInfo->prefix.opsiz )
                        addr--;
                } else if( CodeInfo->use32 ) {
                    CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                    addr -= 3; // 32 bit displacement
                } else {
                    CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                    addr -= 1; // 16 bit displacement
                }
                if( IS_CONDJMP( CodeInfo->token ) ) {
                    // 1 extra byte for opcode ( 0F )
                    addr--;
                }
            }

            /* store the displacement */
            CodeInfo->data[Opnd_Count] = addr;

            if ( CodeInfo->mem_type == MT_SHORT ||
                 CodeInfo->token == T_JCXZ ||
                 CodeInfo->token == T_JECXZ ||
                 (CodeInfo->token >= T_LOOP &&
                  CodeInfo->token <= T_LOOPZW) ) {
                if( Parse_Pass > PASS_1 &&
                    PhaseError == FALSE &&
                    (CodeInfo->opnd_type[Opnd_Count] != OP_I8) ) {
                    AsmError( JUMP_OUT_OF_RANGE );
                    return( ERROR );
                }
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                break;
            }
            /* remaining instructions: CALL, JMP, Jxx */

            /* automatic (conditional) jump expansion */
            /* for 386 and above this is not needed, since there exists
             an extended version of Jxx */
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK) < P_386 &&
                CodeInfo->token != T_CALL &&
                CodeInfo->token != T_JMP) {
                /* look into jump extension */
                if( CodeInfo->opnd_type[Opnd_Count] != OP_I8 ) {
                    if( CodeInfo->mem_type == MT_EMPTY && ModuleInfo.ljmp == TRUE) {
                        jumpExtend( FALSE );
                        addr -= 1;
                        CodeInfo->data[Opnd_Count] = addr;
                        //return( SCRAP_INSTRUCTION );
                    } else if( !PhaseError ) {
                        AsmError( JUMP_OUT_OF_RANGE );
                        return( ERROR );
                    }
                }
            }
            break;
        }
        /* fall through, handle FAR destinations like external symbols */
    case SYM_EXTERNAL:

        if ( opndx->explicit ) {
            mem_type = opndx->mem_type;
        } else {
            mem_type = sym->mem_type;
        }
        /* forward ref, or external symbol */
        if( CodeInfo->mem_type == MT_EMPTY && mem_type != MT_EMPTY ) {
            switch( mem_type ) {
            case MT_FAR:
                if( IS_JMPCALL( CodeInfo->token ) ) {
                    CodeInfo->isfar = TRUE;
                }
                // fall through
            case MT_SHORT:
            case MT_NEAR:
                CodeInfo->mem_type = mem_type;
                break;
            case MT_PROC:
                CodeInfo->mem_type = IS_PROC_FAR() ? MT_FAR : MT_NEAR;
                if( CodeInfo->mem_type == MT_FAR )
                    if ( IS_JMPCALL( CodeInfo->token ) )
                        CodeInfo->isfar = TRUE;
                break;
            case MT_FWORD: /* shouldn't happen! */
                Set_Memtype( MT_FWORD, TRUE );
                break;
            default:
                CodeInfo->mem_type = mem_type;
            }
        }

        /* handle far JMP + CALL? */
        if ( IS_JMPCALL( CodeInfo->token ) &&
             ( CodeInfo->isfar == TRUE || CodeInfo->mem_type == MT_FAR )) {
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
                if ( opndx->explicit && opndx->ofs_size != OFSSIZE_EMPTY )
                    SET_OPSIZ( CodeInfo, opndx->ofs_size == OFSSIZE_32 );
                else
                    SET_OPSIZ( CodeInfo, SymIs32( sym ));

                /* set global vars Frame + Frame_Datum */
                find_frame( sym );

                if( Opnd_Count == OPND2 ) {
                    if( oper_32( CodeInfo ) ) {
                        fixup_type = FIX_OFF32;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                    } else {
                        fixup_type = FIX_OFF16;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                    }
                } else {
                    if( oper_32( CodeInfo ) ) {
                        fixup_type = FIX_PTR32;
                        CodeInfo->opnd_type[Opnd_Count] = OP_J48;
                    } else {
                        fixup_type = FIX_PTR16;
                        CodeInfo->opnd_type[Opnd_Count] = OP_J32;
                    }
                }
                break;
            case MT_DWORD:
            case MT_FWORD:
            case MT_SDWORD:
            case MT_PTR:
                /* shouldn't happen! */
                return( INDIRECT_JUMP );
            default:
                AsmError( INVALID_SIZE );
                return( ERROR );
            }
            AddFixup( sym, fixup_type, fixup_option );
            return( NOT_ERROR );
        }  /* end if FAR JMP/CALL */

        switch( CodeInfo->token ) {
        case T_CALL:
            if( CodeInfo->mem_type == MT_SHORT ) {
                AsmError( CANNOT_USE_SHORT_WITH_CALL );
                return( ERROR );
            } else if( CodeInfo->mem_type == MT_EMPTY ) {
                fixup_option = OPTJ_CALL;
                if( CodeInfo->use32 ) {
                    fixup_type = FIX_RELOFF32;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                } else {
                    fixup_type = FIX_RELOFF16;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                }
                break;
            }
            /* fall through */
        case T_JMP:
            switch( CodeInfo->mem_type ) {
            case MT_SHORT:
                fixup_option = OPTJ_EXPLICIT;
                fixup_type = FIX_RELOFF8;
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                break;
            case MT_EMPTY:
                // forward reference
                // default distance is short
                fixup_option = OPTJ_NONE;
                /* guess short if JMP, we will expand later if needed */
                fixup_type = FIX_RELOFF8;
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                break;
            case MT_NEAR:
                fixup_option = OPTJ_EXPLICIT;
                if( CodeInfo->use32 ) {
                    fixup_type = FIX_RELOFF32;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                } else {
                    fixup_type = FIX_RELOFF16;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                }
                break;
            case MT_DWORD:
            case MT_WORD:
            case MT_SDWORD:
            case MT_SWORD:
                /* shouldn't happen! */
                return( INDIRECT_JUMP );
            default:
                AsmError( INVALID_SIZE );
                return( ERROR );
            }
            // deactivated because there's no override involved here
            // check_assume( sym, EMPTY );
            break;
        default: /* JCXZ, JECXZ, LOOPxx, Jxx */
            // JCXZ, JECXZ and LOOPxx always require SHORT label
            if ( CodeInfo->token == T_JCXZ ||
                 CodeInfo->token == T_JECXZ ||
                 (CodeInfo->token >= T_LOOP &&
                  CodeInfo->token <= T_LOOPZW) ) {
                if( CodeInfo->mem_type != MT_EMPTY && CodeInfo->mem_type != MT_SHORT ) {
                    AsmError( ONLY_SHORT_DISPLACEMENT_IS_ALLOWED );
                    return( ERROR );
                }
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                fixup_option = OPTJ_EXPLICIT;
                fixup_type = FIX_RELOFF8;
                break;
            }
            /* just Jxx remaining */
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_386 ) {
                switch( CodeInfo->mem_type ) {
                case MT_SHORT:
                    fixup_option = OPTJ_EXPLICIT;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                    break;
                case MT_EMPTY:
                    // forward reference
                    fixup_option = OPTJ_JXX;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                    break;
                case MT_NEAR:
                    fixup_option = OPTJ_EXPLICIT;
                    /* it might be useful to allow to use the
                     16bit version of the near jump in 32bit mode
                     since it is 1 byte shorter! */
                    if ( opndx->explicit && opndx->ofs_size != OFSSIZE_EMPTY ) {
                        SET_OPSIZ( CodeInfo, opndx->ofs_size == OFSSIZE_32 );
                        CodeInfo->opnd_type[Opnd_Count] = (opndx->ofs_size == OFSSIZE_32) ? OP_I32 : OP_I16;
                    } else if( CodeInfo->use32 ) {
                        fixup_type = FIX_RELOFF32;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                    } else {
                        fixup_type = FIX_RELOFF16;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                    }
                    break;
                case MT_FAR:
                    if ( ModuleInfo.ljmp ) { /* OPTION LJMP set? */
                        if ( opndx->explicit && opndx->ofs_size != OFSSIZE_EMPTY )
                            SET_OPSIZ( CodeInfo, opndx->ofs_size == OFSSIZE_32 );
                        else
                            SET_OPSIZ( CodeInfo, SymIs32( sym ));
                        jumpExtend( TRUE );
                        if( oper_32( CodeInfo ) ) {
                            fixup_type = FIX_PTR32;
                            CodeInfo->opnd_type[Opnd_Count] = OP_J48;
                        } else {
                            fixup_type = FIX_PTR16;
                            CodeInfo->opnd_type[Opnd_Count] = OP_J32;
                        }
                        break;
                        //return( SCRAP_INSTRUCTION );
                    }
                    /* fall through */
                default:
                    AsmError( ONLY_SHORT_AND_NEAR_DISPLACEMENT_IS_ALLOWED );
                    return( ERROR );
                }
            } else {
                // the only mode in 8086, 80186, 80286 is
                // Jxx SHORT
                // Masm allows "Jxx near" if LJMP is on (default)
                switch( CodeInfo->mem_type ) {
                case MT_EMPTY:
                    fixup_option = OPTJ_EXTEND;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                    break;
                case MT_SHORT:
                    fixup_option = OPTJ_EXPLICIT;
                    fixup_type = FIX_RELOFF8;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I8;
                    break;
                case MT_NEAR: /* allow Jxx NEAR if LJMP on */
                case MT_FAR:
                    if ( ModuleInfo.ljmp ) {
                        if ( CodeInfo->mem_type == MT_FAR ) {
                            jumpExtend( TRUE );
                            fixup_type = FIX_PTR16;
                            CodeInfo->opnd_type[Opnd_Count] = OP_J32;
                        } else {
                            jumpExtend( FALSE );
                            fixup_type = FIX_RELOFF16;
                            CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                        }
                        break;
                        //return( SCRAP_INSTRUCTION );
                    }
                    /* fall through */
                default:
                    AsmError( ONLY_SHORT_DISPLACEMENT_IS_ALLOWED );
                    return( ERROR );
                }
            }
        }
        AddFixup( sym, fixup_type, fixup_option );
        break; /* end case SYM_EXTERNAL */
    default: /* SYM_STACK, SYM_SEG, SYM_GRP, ... */
        DebugMsg(("jmp, error, sym=%s, state=%u, memtype=%u\n", sym->name, sym->state, sym->mem_type));
        AsmError( NO_JUMP_TO_AUTO );
        return( ERROR );
    }
    return( NOT_ERROR );
}

