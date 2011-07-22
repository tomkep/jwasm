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
#include "parser.h"
#include "expreval.h"
#include "equate.h"
#include "tokenize.h"
#include "macro.h"
#include "fastpass.h"
#include "listing.h"
#include "input.h"

extern void myatoi128( const char *, uint_64[], int, int );

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
/* the "i64" suffixes shouldn't be necessary, but sometimes it's needed (OCC) */
const int_64 maxintvalues[] = { 0x00000000ffffffffi64, 0x00000000ffffffffi64,
#if AMD64_SUPPORT
0x7fffffffffffffffi64
#endif
};
const int_64 minintvalues[] = { 0xffffffff00000000i64, 0xffffffff00000000i64,
#if AMD64_SUPPORT
0x8000000000000000i64
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

static void SaveVariableState( struct asym *sym )
/***********************************************/
{
    struct equ_item *p;
    DebugMsg1(( "SaveVariableState(%s)=%d\n", sym->name, sym->value ));
    sym->saved = TRUE; /* don't try to save this symbol (anymore) */
    p = AsmAlloc( sizeof( struct equ_item ) );
    p->next = NULL;
    p->sym = sym;
    p->lvalue   = sym->value;
    p->hvalue   = sym->value3264; /* v2.05: added */
    p->isdefined = sym->isdefined;
    if ( modstate.EquTail ) {
        modstate.EquTail->next = p;
        modstate.EquTail = p;
    } else {
        modstate.EquHead = modstate.EquTail = p;
    }
//    printf("state of symbol >%s< saved, value=%u, defined=%u\n", sym->name, sym->value, sym->defined);
}
#endif

static void SetValue( struct asym *sym, struct expr *opndx )
/**********************************************************/
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
            struct dsym *dir = (struct dsym *)sym;
            dir->e.procinfo = ((struct dsym *)opndx->sym)->e.procinfo;
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

static struct asym *CreateAssemblyTimeVariable( struct asm_tok tokenarray[] )
/***************************************************************************/
{
    struct asym         *sym;
    const char          *name = tokenarray[0].string_ptr;
    int                 i = 2;
    struct expr         opndx;

    DebugMsg1(( "CreateAssemblyTimeVariable(%s) enter\n", name ));
    if ( tokenarray[2].token == T_NUM &&
        tokenarray[3].token == T_FINAL &&
        tokenarray[2].numlen <= 8 ) {
        //opndx.llvalue = tokenarray[2].value64;
        //opndx.llvalue = *(uint_64 *)(tokenarray[2].string_ptr - sizeof(uint_64) );
        myatoi128( tokenarray[i].string_ptr, &opndx.llvalue, tokenarray[i].numbase, tokenarray[i].numlen );
        opndx.kind = EXPR_CONST;
    } else {
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( NULL );
        if( tokenarray[i].token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
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
            EmitConstError( &opndx );
            return( NULL );
        }
    }

    sym = SymSearch( name );
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        if( sym == NULL ) {
            sym = SymCreate( name, TRUE );
        } else {
            sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
        }
        sym->variable  = TRUE;
    } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE && sym->mem_type == MT_ABS ) {
        sym_ext2int( sym );
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
struct asym *CreateVariable( const char *name, int value )
/********************************************************/
{
    struct asym      *sym;

    DebugMsg1(( "CreateVariableEx(%s, %d ) enter\n", name, value ));

    sym = SymSearch( name );
    if( sym == NULL ) {
        sym = SymCreate( name, TRUE );
#if FASTPASS
        sym->saved = FALSE;
#endif
    } else if ( sym->state == SYM_UNDEFINED ) {
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
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

struct asym *CreateConstant( struct asm_tok tokenarray[] )
/********************************************************/
{
    struct asym         *sym;
    const char          *name = tokenarray[0].string_ptr;
    int                 i = 2;
    ret_code            rc;
    char                *p;
    bool                cmpvalue = FALSE;
    struct expr         opndx;
    char                argbuffer[MAX_LINE_LEN];

    DebugMsg1(( "CreateConstant(%s) enter\n", name ));

    sym = SymSearch( name );

    if( sym == NULL ||
       sym->state == SYM_UNDEFINED ||
       ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) ) {
        /* It's a "new" equate.
         * wait with definition until type of equate is clear
         */
    } else if( sym->state == SYM_TMACRO ) {

        return ( SetTextMacro( tokenarray, sym, name, tokenarray[2].tokpos ) );

    } else if( sym->equate == FALSE ) {

        DebugMsg1(( "CreateConstant(%s) state=%u, mem_type=%Xh, value=%" FX32 ", symbol redefinition\n", name, sym->state, sym->mem_type, sym->value));
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );

    } else {
        if ( sym->asmpass == ( Parse_Pass & 0xFF ) )
            cmpvalue = TRUE;
        sym->asmpass = Parse_Pass;
    }

    /* try to evalate the expression */

    if ( tokenarray[2].token == T_STRING && tokenarray[2].string_delim == '<' ) {
        if ( tokenarray[3].token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, tokenarray[3].string_ptr );
            return( NULL );
        }
        return ( SetTextMacro( tokenarray, sym, name, tokenarray[2].string_ptr ) );
    } else if ( tokenarray[2].token == T_NUM &&
               tokenarray[3].token == T_FINAL &&
               tokenarray[2].numlen <= 8 ) {
        //opndx.llvalue = tokenarray[2].value64;
        //opndx.llvalue = *(uint_64 *)(tokenarray[2].string_ptr - sizeof(uint_64));
        myatoi128( tokenarray[i].string_ptr, &opndx.llvalue, tokenarray[i].numbase, tokenarray[i].numlen );
        //opndx.hlvalue = 0;
        //opndx.string = NULL;
        opndx.instr = EMPTY;
        opndx.kind = EXPR_CONST;
        opndx.flags1 = 0;
        rc = NOT_ERROR;
        //DebugMsg1(( "CreateConstant(%s): simple numeric value=%" FX32 "\n", name, tokenarray[2].value64 ));
        i++;
    } else {
        if ( Parse_Pass == PASS_1 ) {
            p = tokenarray[2].tokpos;
            /* if the expression cannot be evaluated to a numeric value,
             * it's to become a text macro. The value of this macro will be
             * the original (unexpanded!) line - that's why it has to be
             * saved here to argbuffer[].
             */
            strcpy( argbuffer, p );
            DebugMsg1(("CreateConstant(%s): before ExpandLinePart: >%s<\n", name, p ));
            /* expand EQU argument (macro functions won't be expanded!) */
            ExpandLinePart( 2, tokenarray, p, FALSE, TRUE );
            DebugMsg1(("CreateConstant(%s): after ExpandLinePart: >%s<\n", name, p ));
        }
        rc = EvalOperand( &i, tokenarray, Token_Count, &opndx, EXPF_NOERRMSG | EXPF_NOLCREATE );
    }
    /* what is an acceptable 'number' for EQU?
     * 1. a constant value - if magnitude is <= 32.
     *    This includes struct fields.
     * 2. an address - if it is direct and doesn't contain an external reference.
     * Anything else will be stored as a text macro.
     * v2.04: large parts rewritten.
     */
    if ( rc != ERROR &&
        tokenarray[i].token == T_FINAL &&

         /* magnitude of CONSTs must be <= 32 */
        ( ( opndx.kind == EXPR_CONST && // opndx.abs == FALSE && /* v2.06: always true */
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
            sym->asmpass = Parse_Pass;
        } else if ( sym->state == SYM_UNDEFINED ) {
            sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
        } else if ( sym->state == SYM_EXTERNAL ) {
            sym_ext2int( sym );
        } else if ( cmpvalue ) {
            if ( opndx.kind == EXPR_CONST ) {
                /* for 64bit, it may be necessary to check 64bit value! */
                if ( sym->value != opndx.value ) {
                    DebugMsg(("CreateConstant(%s), CONST value changed: old=%X, new=%X\n", name, sym->offset, opndx.value ));
                    AsmErr( SYMBOL_REDEFINITION, name );
                    return( NULL );
                }
            } else if ( opndx.kind == EXPR_ADDR ) {
                if ( ( sym->offset != ( opndx.sym->offset + opndx.value ) ) || ( sym->segment != opndx.sym->segment ) ) {
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
    return ( SetTextMacro( tokenarray, sym, name, argbuffer ) );
}

/* EQU and '=' directives */

ret_code EquDirective( int i, struct asm_tok tokenarray[] )
/*********************************************************/
{
    struct asym *sym;

    if( tokenarray[0].token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
    if ( sym = ( ( tokenarray[i].dirtype == DRT_EQUALSGN ) ? CreateAssemblyTimeVariable( tokenarray ) : CreateConstant( tokenarray ) ) ) {
        if ( ModuleInfo.list == TRUE ) {
            LstWrite( LSTTYPE_EQUATE, 0, sym );
        }
        return( NOT_ERROR );
    }
    return( ERROR );
}

