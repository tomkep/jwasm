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
* Description:  parser's processing of branch instructions with immediate
*                operand (CALL, JMP, Jxx, LOOPxx, JxCXZ). Includes:
*               - "far call optimisation": a "call FAR ptr <proc>" is
*                 exchanged by a "push cs" + "call NEAR ptr <proc>".
*               - "short jump extension": a conditional jump with a
*                 destination not within the SHORT range is exchanged
*                 by "j<cond> $+3|5" and "jmp <dest>" if cpu is < 386
*                 (see OPTION LJMP | NOLJMP).
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "fixup.h"
#include "expreval.h"
#include "directiv.h"
#include "fastpass.h"
#include "directiv.h"
#include "segment.h"
#include "input.h"
#include "assume.h"

#define NEEDLABEL 1

#define IS_CONDJMP( inst )  ( ( inst >= T_JA ) && ( inst <= T_JZ ) )

//extern void     Set_Memtype( struct code_info *, memtype mem_type, bool fix_mem_type );
extern unsigned int     Opnd_Count;     // operand count of current instr

/* "short jump extension": extend a (conditional) jump.
 * example:
 * "jz label"
 * is converted to
 * "jnz SHORT $+x"  ( x = sizeof(next ins), may be 3|5|6|7|8 )
 * "jmp label"
 *
 * there is a problem if it's a short forward jump with a distance
 * of 7D-7F (16bit), because the additional "jmp label" will increase
 * the code size.
 */
static void jumpExtend( struct code_info *CodeInfo, int far_flag )
/****************************************************************/
{
    uint_8 opcode;
    unsigned next_ins_size;

    if( Parse_Pass == PASS_2 )
        AsmWarn( 4, EXTENDING_JUMP );

    DebugMsg(("jumpExtend(far=%u), pass=%u, curr offset=%X, Ofssize=%u\n", far_flag, Parse_Pass + 1, GetCurrOffset(), CodeInfo->Ofssize ));
    if( far_flag ) {
        if ( CodeInfo->prefix.opsiz ) {
            /* it's 66 EA OOOO SSSS or 66 EA OOOOOOOO SSSS */
            next_ins_size = CodeInfo->Ofssize ? 6 : 8;
        } else {
            /* it's EA OOOOOOOO SSSS or EA OOOO SSSS */
            next_ins_size = CodeInfo->Ofssize ? 7 : 5;
        }
    } else {
        /* it's E9 OOOOOOOO or E9 OOOO */
        next_ins_size = CodeInfo->Ofssize ? 5 : 3;
    }

    /* it's ensured that the short jump version is first in AsmOpTable */
    opcode = AsmOpTable[AsmResWord[CodeInfo->token].position].opcode;
    OutputCodeByte( opcode ^ 1 ); /* the negation is achieved by XOR 1 */
    OutputCodeByte( next_ins_size );
    CodeInfo->token = T_JMP;
    CodeInfo->pcurr = &AsmOpTable[AsmResWord[T_JMP].position];

    return;
}

// "far call optimisation": a far call is done to a near label
// optimize (call SSSS:OOOO -> PUSH CS, CALL OOOO)

static void FarCallToNear( struct code_info *CodeInfo )
/*****************************************************/
{
    if( Parse_Pass == PASS_2 )
        AsmWarn( 4, CALL_FAR_TO_NEAR );

    OutputCodeByte( 0x0E ); /* 0x0E is "PUSH CS" opcode */
    CodeInfo->mem_type = MT_NEAR;
    CodeInfo->mem_type_fixed = TRUE;

    return;
}

ret_code branch( struct code_info *CodeInfo, expr_list *opndx )
/*************************************************************/
/*
 called by idata_fixup() and idata_nofixup().
 current instruction is CALL, JMP, Jxx, LOOPx, JCXZ or JECXZ
 determine the displacement of jmp;
 possible return values are:
 - NOT_ERROR,
 - ERROR,
 - (INDIRECT_JUMP)
 INDIRECT_JUMP is handled as error by the caller, but should never occur
 because idata_fixup() and idata_nofixup() won't be called for a memory
 operand.
*/
{
    int_32              addr;
    enum fixup_types    fixup_type;
    enum fixup_options  fixup_option;
    enum sym_state      state;
    struct asm_sym      *sym;
    memtype             mem_type;
    dir_node            *seg;
    unsigned            opidx = AsmResWord[CodeInfo->token].position;

    CodeInfo->data[Opnd_Count] = opndx->value;
    sym = opndx->sym;
    if( sym == NULL ) { /* no symbolic label specified? */
        DebugMsg(("branch: sym=NULL, op.memtype=%Xh\n", opndx->mem_type ));

        /* Masm rejects: "jump dest must specify a label */
#if NEEDLABEL
        AsmError( JUMP_DESTINATION_MUST_SPECIFY_A_LABEL );
        return( ERROR );
#else
        if( IS_JMPCALL( CodeInfo->token ) )
            CodeInfo->isfar = TRUE; /* ??? most likely nonsense! */

        if( CodeInfo->data[Opnd_Count] > USHRT_MAX )
            CodeInfo->opnd_type[Opnd_Count] = OP_I32;
        else
            CodeInfo->opnd_type[Opnd_Count] = OP_I16;

        return( NOT_ERROR );
#endif
    }
    DebugMsg(("branch(%X): explicit=%u op.memtype=%X sym=>%s< sym.state=%u/mem_type=%Xh/ofs=%X\n", GetCurrOffset(), opndx->explicit, opndx->mem_type, sym->name, sym->state, sym->mem_type, sym->offset));

    state = sym->state;
    if ( state == SYM_UNDEFINED || state == SYM_INTERNAL || state == SYM_PROC || state == SYM_EXTERNAL ) {
        seg = GetSeg( sym );
        if( seg == NULL || ( CurrSeg != seg ) ) {
            /* if label has a different segment and jump/call is near or short,
             report an error */
            if ( ModuleInfo.flatgrp_idx != 0 )
                ;
            else if (seg != NULL && CurrSeg != NULL) {
                /* if the segments belong to the same group, it's ok */
                if (((dir_node *)seg)->e.seginfo->group != NULL &&
                    CurrSeg->e.seginfo->group != NULL &&
                    ((dir_node *)seg)->e.seginfo->group == CurrSeg->e.seginfo->group)
                    ;
                else if ( opndx->mem_type == MT_NEAR || opndx->mem_type == MT_SHORT ) {
                    AsmError( NO_FAR_JUMP_TO_NEAR_LABEL );
                    return( ERROR );
                }
            }
            /* jumps to another segment are just like to another file */
            state = SYM_EXTERNAL;
        }
    }

    if( !CodeInfo->mem_type_fixed ) {
        CodeInfo->mem_type = MT_EMPTY;
    }
    fixup_option = OPTJ_NONE;
    fixup_type = FIX_RELOFF8;
    switch( state ) {
    case SYM_UNDEFINED:
        SetSymSegOfs( sym ); /* set symbol's seg:ofs to current seg:ofs */
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
#if 0
           /* 1. opndx memtype preferable, the symbol's memtype shouldn't be
            *  used.
            * 2. it's useless because it won't occur.
            */
           && sym->mem_type != MT_WORD
           && sym->mem_type != MT_DWORD
           && sym->mem_type != MT_FWORD
#endif
            && CodeInfo->isfar == FALSE ) {

            /* if the label is FAR - or there is a segment override
             which equals assumed value of CS - and there is no type cast,
             then do "far call translation".
             */
            if( CodeInfo->token == T_CALL &&
                CodeInfo->mem_type == MT_EMPTY &&
                ( sym->mem_type == MT_FAR || CodeInfo->prefix.SegOverride ) ) {
                FarCallToNear( CodeInfo );
            }

            addr = sym->offset;
            addr -= ( GetCurrOffset() + 2 );  // calculate the displacement
            addr += CodeInfo->data[Opnd_Count];
            /*  JCXZ, LOOPW, LOOPEW, LOOPZW, LOOPNEW, LOOPNZW,
               JECXZ, LOOPD, LOOPED, LOOPZD, LOOPNED, LOOPNZD? */
            if (( CodeInfo->Ofssize && AsmOpTable[opidx].byte1_info == F_16A) ||
                ( CodeInfo->Ofssize != USE32 && AsmOpTable[opidx].byte1_info == F_32A))
                addr--; /* 1 extra byte for ADRSIZ (0x67) */

            if( CodeInfo->token == T_CALL && CodeInfo->mem_type == MT_EMPTY ) {
                CodeInfo->mem_type = MT_NEAR;
            }

            if( CodeInfo->mem_type != MT_NEAR && CodeInfo->token != T_CALL
                && ( addr >= SCHAR_MIN && addr <= SCHAR_MAX ) ) {
                CodeInfo->opnd_type[Opnd_Count] = OP_I8;
            } else {
                if ( CodeInfo->mem_type == MT_SHORT || ( IS_XCX_BRANCH( CodeInfo->token ) ) ) {
                    /* v1.96: since JWasm's backpatch strategy is to move from
                     * "smallest" to "largest" distance, an "out of range"
                     * error can be detected at any time.
                     */
                    DebugMsg(("branch: 1, jump out of range, addr=%Xh\n", addr ));
                    if ( addr < 0 ) {
                        addr -= SCHAR_MIN;
                        addr = 0 - addr;
                    } else
                        addr -= SCHAR_MAX;
                    AsmErr( JUMP_OUT_OF_RANGE, addr );
                    return( ERROR );
                }
                /* near destination */
                /* is there a type coercion? */
                if ( opndx->Ofssize != USE_EMPTY ) {
                    if ( opndx->Ofssize == USE16 ) {
                        CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                        addr -= 1; // 16 bit displacement
                    } else {
                        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                        addr -= 3; // 32 bit displacement
                    }
                    SET_OPSIZ( CodeInfo, opndx->Ofssize == USE32 );
                    if ( CodeInfo->prefix.opsiz )
                        addr--;
                } else if( CodeInfo->Ofssize ) {
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
            DebugMsg(("branch: displacement=%X\n", addr ));

            /* automatic (conditional) jump expansion.
             * for 386 and above this is not needed, since there exists
             * an extended version of Jcc
             */
            if( ( ModuleInfo.curr_cpu & P_CPU_MASK) < P_386 && IS_JCC( CodeInfo->token ) ) {
                /* look into jump extension */
                if( CodeInfo->opnd_type[Opnd_Count] != OP_I8 ) {
                    if( CodeInfo->mem_type == MT_EMPTY && ModuleInfo.ljmp == TRUE ) {
                        jumpExtend( CodeInfo, FALSE );
                        addr -= 1;
                        CodeInfo->data[Opnd_Count] = addr;
                        //return( SCRAP_INSTRUCTION );
                    //} else if( !PhaseError ) {
                    } else {
                        DebugMsg(("%u branch: 2, jump out of range, mem_type=%X, curr_ofs=%X, sym->offs=%X, addr=%d\n", Parse_Pass + 1, CodeInfo->mem_type, GetCurrOffset(), sym->offset, addr ));
                        AsmErr( JUMP_OUT_OF_RANGE, addr );
                        return( ERROR );
                    }
                }
            }
            break;
        }
        /* fall through, handle FAR destinations like external symbols */
    case SYM_EXTERNAL:
        DebugMsg(("branch: SYM_EXTERNAL\n" ));

        /* v1.95: explicit flag to be removed! */
        //if ( opndx->explicit )
        mem_type = opndx->mem_type;
        //else
        //    mem_type = sym->mem_type;

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
                CodeInfo->mem_type = SimpleType[ST_PROC].mem_type;
                if( CodeInfo->mem_type == MT_FAR )
                    if ( IS_JMPCALL( CodeInfo->token ) )
                        CodeInfo->isfar = TRUE;
                break;
#if 0
            case MT_FWORD: /* shouldn't happen! */
                Set_Memtype( CodeInfo, MT_FWORD, TRUE );
                break;
#endif
            default:
                DebugMsg(("branch: strange mem_type %Xh\n", mem_type ));
                CodeInfo->mem_type = mem_type;
            }
        }

        /* handle far JMP + CALL? */
        if ( IS_JMPCALL( CodeInfo->token ) &&
            ( CodeInfo->isfar == TRUE || CodeInfo->mem_type == MT_FAR )) {
            CodeInfo->isfar = TRUE; /* flag isn't set if explicit is true */
            DebugMsg(("branch: FAR call/jmp\n"));
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
                /* v1.95: explicit flag to be removed! */
                //if ( opndx->explicit && opndx->Ofssize != USE_EMPTY )
                if ( opndx->Ofssize != USE_EMPTY )
                    SET_OPSIZ( CodeInfo, opndx->Ofssize == USE32 );
                else
                    SET_OPSIZ( CodeInfo, GetSymOfssize( sym ));

                /* set global vars Frame + Frame_Datum */
                find_frame( CodeInfo, sym );
#if 0 /* v1.96: removed */
                if( Opnd_Count == OPND2 ) { /* WASM hack (call/jmp with 2 ops) */
                    if( IS_OPER_32( CodeInfo ) ) {
                        fixup_type = FIX_OFF32;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                    } else {
                        fixup_type = FIX_OFF16;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                    }
                } else {
#endif
                    if( IS_OPER_32( CodeInfo ) ) {
                        fixup_type = FIX_PTR32;
                        CodeInfo->opnd_type[Opnd_Count] = OP_J48;
                    } else {
                        fixup_type = FIX_PTR16;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                    }
                //}
                break;
#if 0
            case MT_DWORD:
            case MT_FWORD:
            case MT_SDWORD:
            case MT_PTR:
                /* shouldn't happen! */
                return( INDIRECT_JUMP );
#endif
            default:
                AsmError( INVALID_SIZE );
                return( ERROR );
            }
            CodeInfo->InsFixup[Opnd_Count] = AddFixup( sym, fixup_type, fixup_option );
            return( NOT_ERROR );
        }  /* end if FAR JMP/CALL */

        switch( CodeInfo->token ) {
        case T_CALL:
            if( CodeInfo->mem_type == MT_SHORT ) {
                AsmError( CANNOT_USE_SHORT_WITH_CALL );
                return( ERROR );
            } else if( CodeInfo->mem_type == MT_EMPTY ) {
                fixup_option = OPTJ_CALL;
                if( CodeInfo->Ofssize ) {
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
                if( CodeInfo->Ofssize ) {
                    fixup_type = FIX_RELOFF32;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                } else {
                    fixup_type = FIX_RELOFF16;
                    CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                }
                find_frame( CodeInfo, sym );/* added v1.95 (after change in fixup.c */
                break;
#if 0
            case MT_DWORD:
            case MT_WORD:
            case MT_SDWORD:
            case MT_SWORD:
                /* shouldn't happen! */
                return( INDIRECT_JUMP );
#endif
            default:
                AsmError( INVALID_SIZE );
                return( ERROR );
            }
            // deactivated because there's no override involved here
            // check_assume( sym, EMPTY );
            break;
        default: /* JxCXZ, LOOPxx, Jxx */
            // JxCXZ and LOOPxx always require SHORT label
            if ( IS_XCX_BRANCH( CodeInfo->token ) ) {
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
                    /* v1.95: explicit flag to be removed! */
                    //if ( opndx->explicit && opndx->Ofssize != USE_EMPTY ) {
                    if ( opndx->Ofssize != USE_EMPTY ) {
                        SET_OPSIZ( CodeInfo, opndx->Ofssize >= USE32 );
                        CodeInfo->opnd_type[Opnd_Count] = (opndx->Ofssize >= USE32) ? OP_I32 : OP_I16;
                    } else if( CodeInfo->Ofssize ) {
                        fixup_type = FIX_RELOFF32;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                    } else {
                        fixup_type = FIX_RELOFF16;
                        CodeInfo->opnd_type[Opnd_Count] = OP_I16;
                    }
                    break;
                case MT_FAR:
                    if ( ModuleInfo.ljmp ) { /* OPTION LJMP set? */
                        /* v1.95: explicit flag to be removed! */
                        //if ( opndx->explicit && opndx->Ofssize != USE_EMPTY )
                        if ( opndx->Ofssize != USE_EMPTY )
                            SET_OPSIZ( CodeInfo, opndx->Ofssize >= USE32 );
                        else
                            SET_OPSIZ( CodeInfo, GetSymOfssize( sym ));
                        jumpExtend( CodeInfo, TRUE );
                        CodeInfo->isfar = TRUE;
                        if( IS_OPER_32( CodeInfo ) ) {
                            fixup_type = FIX_PTR32;
                            CodeInfo->opnd_type[Opnd_Count] = OP_J48;
                        } else {
                            fixup_type = FIX_PTR16;
                            CodeInfo->opnd_type[Opnd_Count] = OP_I32;
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
                            jumpExtend( CodeInfo, TRUE );
                            fixup_type = FIX_PTR16;
                            CodeInfo->isfar = TRUE;
                            CodeInfo->opnd_type[Opnd_Count] = OP_I32;
                        } else {
                            jumpExtend( CodeInfo, FALSE );
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
        CodeInfo->InsFixup[Opnd_Count] = AddFixup( sym, fixup_type, fixup_option );
        break; /* end case SYM_EXTERNAL */
    default: /* other types: SYM_SEG, SYM_GRP, SYM_STACK?, SYM_STRUCT_FIELD, SYM_TYPE,  ... */
        DebugMsg(("branch: error, sym=%s, state=%u, memtype=%u\n", sym->name, sym->state, sym->mem_type));
        AsmErr( JUMP_DESTINATION_MUST_SPECIFY_A_LABEL );
        return( ERROR );
    }
    return( NOT_ERROR );
}

