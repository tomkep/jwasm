/****************************************************************************
*
*  This code is Public Domain.
*
*  ========================================================================
*
* Description:  handles EQU and '=' directives
*               equate and assembly time variable handling
*               has been rewritten for JWasm.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "expreval.h"
#include "equate.h"
#include "labels.h"
#include "tokenize.h"
#include "macro.h"
#include "fastpass.h"
#include "listing.h"
#include "input.h"

#if defined(LLONG_MAX) || defined(__GNUC__) || defined(__TINYC__)
/* gcc needs suffixes if the constants won't fit in long type */
const int_64 maxintvalues[] = { 0x00000000ffffffffULL, 0x00000000ffffffffULL,
#if AMD64_SUPPORT
0x7fffffffffffffffULL
#endif
};
const int_64 minintvalues[] = { 0xffffffff00000000ULL, 0xffffffff00000000ULL,
#if AMD64_SUPPORT
0x8000000000000000ULL
#endif
};
#else
const int_64 maxintvalues[] = { 0x00000000ffffffff, 0x00000000ffffffff,
#if AMD64_SUPPORT
0x7fffffffffffffff
#endif
};
const int_64 minintvalues[] = { 0xffffffff00000000, 0xffffffff00000000,
#if AMD64_SUPPORT
0x8000000000000000
#endif
};
#endif

#if FASTPASS

/* for FASTPASS, just pass 1 is a full pass, the other passes
 don't start from scratch and they just assemble the preprocessed
 source. To be able to restart the assembly process from a certain
 location within the source, it's necessary to save the value of
 assembly time variables.
 */

static void SaveVariableState(asm_sym *sym)
/*****************************************/
{
    equ_item *p;
    DebugMsg1(( "SaveVariableState(%s)=%d\n", sym->name, sym->value ));
    sym->saved = TRUE; /* don't try to save this symbol (anymore) */
    p = AsmAlloc(sizeof(equ_item));
    p->next = NULL;
    p->sym = sym;
    p->value   = sym->value;
    p->isdefined = sym->isdefined;
    if (modstate.EquTail) {
        modstate.EquTail->next = p;
        modstate.EquTail = p;
    } else {
        modstate.EquHead = modstate.EquTail = p;
    }
//    printf("state of symbol >%s< saved, value=%u, defined=%u\n", sym->name, sym->value, sym->defined);
}
#endif

static void SetValue( asm_sym *sym, expr_list *opndx )
/****************************************************/
{
    sym->equate = TRUE;
    sym->state = SYM_INTERNAL;
    sym->isdefined = TRUE;
    if ( opndx->kind == EXPR_CONST ) {
        sym->mem_type = MT_ABS;
        sym->uvalue = opndx->uvalue;
        sym->value3264 = opndx->hvalue;
        sym->segment = NULL;
        sym->isproc = FALSE;
    } else {
        sym->isproc = opndx->sym->isproc;
        /* for a PROC alias, copy the procinfo extension! */
        if ( sym->isproc ) {
            dir_node *dir = (dir_node *)sym;
            dir->e.procinfo = ((dir_node *)opndx->sym)->e.procinfo;
        }
        sym->mem_type = opndx->mem_type;
        /* v2.01: allow equates of variables with arbitrary type.
         * Currently the expression evaluator sets opndx.mem_type
         * to the mem_type of the type (i.e. QWORD for a struct with size 8),
         * which is a bad idea in this case. So the original mem_type of the
         * label is used instead.
         */
        if ( opndx->sym->mem_type == MT_TYPE && opndx->explicit == FALSE ) {
            sym->mem_type = opndx->sym->mem_type;
            sym->type = opndx->sym->type;
        }
        sym->offset = opndx->sym->offset + opndx->value;
        sym->segment = opndx->sym->segment;
    }
    return;
}

static asm_sym * CreateAssemblyTimeVariable( void )
/*************************************************/
{
    struct asm_sym      *sym;
    char                *name = AsmBuffer[0]->string_ptr;
    int                 i = 2;
    expr_list           opndx;

    DebugMsg1(( "CreateAssemblyTimeVariable(%s) enter\n", name ));
    if (AsmBuffer[2]->token == T_NUM &&
        AsmBuffer[3]->token == T_FINAL &&
        AsmBuffer[2]->hvalue == 0 ) {
        opndx.llvalue = AsmBuffer[2]->value64;
        opndx.hlvalue = 0;
        opndx.kind = EXPR_CONST;
    } else {
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( NULL );
        if( AsmBuffer[i]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( NULL );
        }
        if( opndx.kind != EXPR_CONST &&
           ( opndx.kind != EXPR_ADDR ||
            opndx.indirect == TRUE ||
            ( opndx.sym != NULL && opndx.sym->state != SYM_INTERNAL && opndx.sym->state != SYM_UNDEFINED ) ) ) {
            DebugMsg(( "CreateAssemblyTimeVariable(%s) kind=%u sym=%p state=%u\n", name, opndx.kind, opndx.sym, opndx.sym ? opndx.sym->state : 0 ));
            AsmError( CONSTANT_EXPECTED );
            return( NULL );
        }
        if ( opndx.hlvalue != 0 ||
            opndx.value64 < minintvalues[ModuleInfo.Ofssize] ||
            opndx.value64 > maxintvalues[ModuleInfo.Ofssize] ) {
            AsmError( CONSTANT_VALUE_TOO_LARGE );
            return( NULL );
        }
    }

    sym = SymSearch( name );
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        if( sym == NULL ) {
            sym = SymCreate( name, TRUE );
        } else {
            dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );
        }
        sym->variable  = TRUE;
    } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE && sym->mem_type == MT_ABS ) {
        dir_ext2int( (dir_node *)sym );
        sym->variable  = TRUE;
    } else if ( sym->state != SYM_INTERNAL ||
               ( sym->variable == FALSE &&
                ( opndx.uvalue != sym->uvalue || opndx.hvalue != sym->value3264 ) ) ) {
        AsmErr( SYMBOL_REDEFINITION, sym->name );
        return( NULL );
    }
#if FASTPASS
    /* v2.04a regression in v2.04. Do not save the variable when it
     * is defined the first time */
    if ( StoreState && sym->saved == FALSE && sym->isdefined == TRUE ) {
        SaveVariableState( sym );
    }
#endif
    sym->variable = TRUE;
    SetValue( sym, &opndx );
    DebugMsg1(( "CreateAssemblyTimeVariable(%s)=%d\n", name, sym->value ));
    return( sym );
}

/* CreateVariable().
 * define an assembly time variable directly without using the token buffer.
 * this is used for some internally generated variables.
 * NO listing is written!
 */
asm_sym * CreateVariable( const char *name, int value )
/*****************************************************/
{
    struct asm_sym      *sym;

    DebugMsg1(( "CreateVariableEx(%s, %d ) enter\n", name, value ));

    sym = SymSearch( name );
    if( sym == NULL ) {
        sym = SymCreate( name, TRUE );
#if FASTPASS
        sym->saved = FALSE;
#endif
    } else if ( sym->state == SYM_UNDEFINED ) {
        dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );
    } else if ( sym->equate == FALSE ) {
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    }
#if FASTPASS
    if ( StoreState && sym->saved == FALSE ) {
        SaveVariableState( sym );
    }
#endif
    sym->isdefined  = TRUE;
    sym->state    = SYM_INTERNAL;
    sym->mem_type = MT_ABS;
    sym->variable = TRUE;
    sym->offset   = value;
    sym->equate   = TRUE;
    return( sym );
}

/*
 * CreateConstant()
 * this is the worker behind EQU.
 * EQU may define 3 different types of equates:
 * - numbers
 * - relocatable items ( aliases )
 * - text macros
 * the argument may be
 * - an expression which can be evaluated to a number or address
 * - a text literal (enclosed in <>)
 * - anything. This will also become a text literal.
 */

asm_sym * CreateConstant( void )
/******************************/
{
    struct asm_sym      *sym;
    char                *name = AsmBuffer[0]->string_ptr;
    int                 i = 2;
    ret_code            rc;
    bool                cmpvalue = FALSE;
    expr_list           opndx;
    char                buffer[MAX_LINE_LEN];
    char                nameb[MAX_ID_LEN+1];

    DebugMsg1(( "CreateConstant(%s) enter\n", name ));

    sym = SymSearch( name );

    if( sym == NULL ||
       sym->state == SYM_UNDEFINED ||
       ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) ) {
        /* It's a "new" equate.
         * wait with definition until type of equate is clear
         */
    } else if( sym->state == SYM_TMACRO ) {

        return ( SetTextMacro( sym, name, AsmBuffer[1]->pos + 4 ) );

    } else if( sym->equate == FALSE ) {

        DebugMsg1(( "CreateConstant(%s) state=%u, mem_type=%Xh, value=%" FX32 ", symbol redefinition\n", name, sym->state, sym->mem_type, sym->value));
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );

    } else {
        if ( sym->pass == ( Parse_Pass & 0xFF ) )
            cmpvalue = TRUE;
        sym->pass = Parse_Pass;
    }

    /* try to evalate the expression */

    if ( AsmBuffer[2]->token == T_STRING && AsmBuffer[2]->string_delim == '<' ) {
        if ( AsmBuffer[3]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[3]->string_ptr );
            return( NULL );
        }
        return ( SetTextMacro( sym, name, AsmBuffer[2]->string_ptr ) );
    } else if ( AsmBuffer[2]->token == T_NUM &&
        AsmBuffer[3]->token == T_FINAL &&
        AsmBuffer[2]->hvalue == 0 ) {
        opndx.llvalue = AsmBuffer[2]->value64;
        opndx.hlvalue = 0;
        opndx.string = NULL;
        opndx.instr = EMPTY;
        opndx.kind = EXPR_CONST;
        opndx.flags = 0;
        rc = NOT_ERROR;
        DebugMsg1(( "CreateConstant(%s): simple numeric value=%" FX32 "\n", name, AsmBuffer[2]->value64 ));
        i++;
    } else {
        if ( Parse_Pass == PASS_1 ) {
            /* if the expression cannot be evaluated to a numeric value,
             * it's to become a text macro. The value of this macro is the
             * ORIGINAL, unexpanded line!!! Also important is that macro
             * function calls are NEVER resolved,
             */
            int k;
            char *p;
            DebugMsg1(("CreateConstant(%s): b.E line=>%s<\n", name, AsmBuffer[1]->pos + 4 ));
            p = AsmBuffer[1]->pos+4;
            strcpy( buffer, p ); /* save original line */
            /* the name string might get destroyed if a macro is executed */
            strcpy( nameb, name );
            name = nameb;

            /* expand the line */
            while ( 1 ) {
                rc = NOT_ERROR;
                for( k = 2; k < Token_Count; k++ ) {
                    if ( ExpandToken( k, p, FALSE, TRUE ) == STRING_EXPANDED )
                        rc = STRING_EXPANDED;
                }
                /* if there was an expansion, the tokenizer must be called. */
                /* if Token_Count is 0, there was a macro function call and
                 * the loop must continue. this should never happen, however!
                 */
                if ( rc == STRING_EXPANDED ) {
                    k = Token_Count;
                    Token_Count = Tokenize( p, 2, FALSE );
                    if (k)
                        break;
                } else
                    break;
            }
            DebugMsg1(("CreateConstant(%s): a.E line=>%s<\n", name, AsmBuffer[1]->pos + 4 ));
        }
        rc = EvalOperand( &i, Token_Count, &opndx, FALSE );
    }
    /* what is an acceptable 'number' for EQU?
     * 1. a constant value - if magnitude is <= 32.
     *    This includes struct fields.
     * 2. an address - if it is direct and doesn't contain an external reference.
     * Anything else will be stored as a text macro.
     * v2.04: large parts rewritten.
     */
    if ( rc != ERROR &&
        AsmBuffer[i]->token == T_FINAL &&

         /* CONSTs must be internal, and the magnitude must be <= 32 */
        ( ( opndx.kind == EXPR_CONST && opndx.abs == FALSE &&
           opndx.hlvalue == 0 && /* magnitude > 64 bits? */
           opndx.value64 >= minintvalues[ModuleInfo.Ofssize]  &&
           opndx.value64 <= maxintvalues[ModuleInfo.Ofssize] ) ||
         ( opndx.kind == EXPR_ADDR &&
          opndx.indirect == FALSE &&
          opndx.sym != NULL &&
          //opndx.sym->state != SYM_EXTERNAL ) ) && /* SYM_SEG, SYM_GROUP are also not ok */
          opndx.sym->state == SYM_INTERNAL ) ) &&
        ( opndx.instr == EMPTY ) ) {

        if ( !sym ) {
            sym = SymCreate( name, TRUE );
            sym->pass = Parse_Pass;
        } else if ( sym->state == SYM_UNDEFINED ) {
            dir_remove_table( &Tables[TAB_UNDEF], (dir_node *)sym );
        } else if ( sym->state == SYM_EXTERNAL ) {
            dir_ext2int( (dir_node *)sym );
        } else if ( cmpvalue ) {
            if ( opndx.kind == EXPR_CONST ) {
                /* for 64bit, it may be necessary to check 64bit value! */
                if ( sym->value != opndx.value ) {
                    DebugMsg(("CreateConstant(%s), CONST value changed: old=%X, new=%X\n", name, sym->offset, opndx.value ));
                    AsmErr( SYMBOL_REDEFINITION, name );
                    return( NULL );
                }
            } else if ( opndx.kind == EXPR_ADDR ) {
                if ((sym->offset != (opndx.sym->offset + opndx.value)) || (sym->segment != opndx.sym->segment)) {
                    DebugMsg(("CreateConstant(%s), ADDR value changed: old=%X, new ofs+val=%X+%X\n", name, sym->offset, opndx.sym->offset, opndx.value));
                    AsmErr( SYMBOL_REDEFINITION, name );
                    return( NULL );
                }
            }
        }
        /* change from alias to number is ok if value (=offset) won't change!
         * memtype must not be checked!
         */
        //if ( opndx.kind == EXPR_CONST ) {
        //    if ( sym->mem_type != MT_ABS && sym->mem_type != MT_EMPTY ) {
        //        AsmErr( SYMBOL_REDEFINITION, name );
        //        return( NULL );
        //    }
        //}
        sym->variable = FALSE;
        SetValue( sym, &opndx );
        DebugMsg1(("CreateConstant(%s): memtype=%X value=%I64X isproc=%u variable=%u\n", name, sym->mem_type, sym->value, sym->value3264, sym->isproc, sym->variable ));
        return( sym );
    }
    DebugMsg1(("CreateConstant(%s): value is NOT numeric, calling SetTextMacro()\n", name ));
    return ( SetTextMacro( sym, name, buffer ) );
}

/* DefineConstant is used by
 *   EQU:    redefine=FALSE
 *   '=':    redefine=TRUE
 */
ret_code DefineConstant( bool redefine )
/**************************************/
{
    asm_sym *sym;

    if( AsmBuffer[0]->token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }
    if ( sym = ( redefine ? CreateAssemblyTimeVariable() : CreateConstant() ) ) {
        if ( ModuleInfo.list == TRUE ) {
            LstWrite( LSTTYPE_EQUATE, 0, sym );
        }
        return( NOT_ERROR );
    }
    return( ERROR );
}

