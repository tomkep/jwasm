/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  implements hll directives:
*               .IF, .WHILE, .REPEAT, .ELSE, .ELSEIF, .ENDIF,
*               .ENDW, .UNTIL, .UNTILCXZ, .BREAK, .CONTINUE.
*               also handles C operators:
*               ==, !=, >=, <=, >, <, &&, ||, &, !,
*               and flags:
*               ZERO?, CARRY?, SIGN?, PARITY?, OVERFLOW?
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "equate.h"
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "types.h"
#include "hll.h"
#include "segment.h"
#include "listing.h"

#define LABELSIZE 8

#define LABELSGLOBAL 0 /* make the generated labels global */

#if LABELSGLOBAL
#define LABELQUAL "::"
#else
#define LABELQUAL ":"
#endif

#define LSTART 0      /* loop start label         */
#define LTEST  1      /* continue, test for exit  */
#define LEXIT  2      /* loop exit label          */

enum hll_cmd {
    HLL_IF,
    HLL_WHILE,
    HLL_REPEAT,
    HLL_BREAK  /* .IF behind .BREAK or .CONTINUE */
};

/* item for .IF, .WHILE, .REPEAT, ... */
struct hll_item {
    struct hll_item     *next;
    uint_32             labels[3];
    char                *condlines;     /* for .WHILE: lines to add after test */
    enum hll_cmd        cmd;            /* start cmd (IF, WHILE, REPEAT) */
};

static ret_code GetExpression( struct hll_item *hll, int *i, struct asm_tok[], int ilabel, bool is_true, char *buffer, char **lastjmp, struct expr *opndx );

/* c binary ops */

enum c_bop {
    COP_NONE,
    COP_EQ,   /* == */
    COP_NE,   /* != */
    COP_GT,   /* >  */
    COP_LT,   /* <  */
    COP_GE,   /* >= */
    COP_LE,   /* <= */
    COP_AND,  /* && */
    COP_OR,   /* || */
    COP_ANDB, /* &  */
    COP_NEG,  /* !  */
    COP_ZERO, /* ZERO?   not really a valid C operator */
    COP_CARRY,/* CARRY?  not really a valid C operator */
    COP_SIGN, /* SIGN?   not really a valid C operator */
    COP_PARITY,  /* PARITY?   not really a valid C operator */
    COP_OVERFLOW /* OVERFLOW? not really a valid C operator */
};

/* must be in same order as in enum c_bop COP_ZERO .. COP_OVERFLOW */
static const char flaginstr[] = {
    'z',  'c',  's',  'p',  'o'
};

/* in Masm, there's a nesting level limit of 20. In JWasm, there's
 * currently no limit.
 */
static struct hll_item     *HllStack; /* for .WHILE, .IF, .REPEAT */
/* v2.06: <struct hll>-items made reuseable */
static struct hll_item     *HllFree; /* stack of free <struct hll>-items */

#ifdef DEBUG_OUT
static int cntAlloc = 0;
static int cntReused = 0;
static int cntCond = 0;
static int cntCondBytes = 0;
#endif

static uint_32 GetHllLabel( void )
/********************************/
{
    return ( ++ModuleInfo.hll_label );
}

/* get a C binary operator from the token stream.
 * there is a problem with the '<' because it is a "string delimiter"
 * which Tokenize() usually is to remove.
 * There has been a hack implemented in Tokenize() so that it won't touch the
 * '<' if .IF, .ELSEIF, .WHILE, .UNTIL, .UNTILCXZ or .BREAK/.CONTINUE has been
 * detected
 */
static enum c_bop GetCOp( struct asm_tok *item )
/**********************************************/
{
    int size;
    enum c_bop rc;
    char *p = item->string_ptr;

    size = ( item->token == T_STRING ? item->stringlen : 0 );

    if ( size == 2 ) {
        if ( *p == '=' && *(p+1) == '=' )
            rc = COP_EQ;
        else if ( *p == '!' && *(p+1) == '=' )
            rc = COP_NE;
        else if ( *p == '>' && *(p+1) == '=' )
            rc = COP_GE;
        else if ( *p == '<' && *(p+1) == '=' )
            rc = COP_LE;
        else if ( *p == '&' && *(p+1) == '&' )
            rc = COP_AND;
        else if ( *p == '|' && *(p+1) == '|' )
            rc = COP_OR;
        else
            return( COP_NONE );
    } else if ( size == 1 ) {
        if ( *p == '>' )
            rc = COP_GT;
        else if ( *p == '<' )
            rc = COP_LT;
        else if ( *p == '&' )
            rc = COP_ANDB;
        else if ( *p == '!' )
            rc = COP_NEG;
        else
            return( COP_NONE );
    } else {
        if ( item->token != T_ID )
            return( COP_NONE );
        /* a valid "flag" string must end with a question mark */
        size = strlen( p );
        if ( *(p+size-1) != '?' )
            return( COP_NONE );
        if ( size == 5 && ( 0 == _memicmp( p, "ZERO", 4 ) ) )
            rc = COP_ZERO;
        else if ( size == 6 && ( 0 == _memicmp( p, "CARRY", 5 ) ) )
            rc = COP_CARRY;
        else if ( size == 5 && ( 0 == _memicmp( p, "SIGN", 4 ) ) )
            rc = COP_SIGN;
        else if ( size == 7 && ( 0 == _memicmp( p, "PARITY", 6 ) ) )
            rc = COP_PARITY;
        else if ( size == 9 && ( 0 == _memicmp( p, "OVERFLOW", 8 ) ) )
            rc = COP_OVERFLOW;
        else
            return( COP_NONE );
    }
    return( rc );
}

/* render an instruction */

static char *RenderInstr( char *p, char *instr, struct expr *op, int start1, int end1, int start2, int end2, struct asm_tok tokenarray[] )
/****************************************************************************************************************************************/
{
    int i;
    i = strlen( instr );
    /* copy the instruction */
    memcpy( p, instr, i );
    p += i;
    /* copy the first operand's tokens */
    /* v2.06: use tokpos instead of string_ptr */
    //for ( ; start1 < end1; start1++ ) {
    //    *p++ = ' ';
    //    strcpy( p, tokenarray[start1].string_ptr );
    //    p += strlen( p );
    //}
    *p++ = ' ';
    i = tokenarray[end1].tokpos - tokenarray[start1].tokpos;
    memcpy( p, tokenarray[start1].tokpos, i );
    p += i;
    if ( start2 != EMPTY ) {
        *p++ = ',';
        /* copy the second operand's tokens */
        /* v2.06: use tokpos instead of string_ptr */
        //for ( ; start2 < end2; start2++ ) {
        //    *p++ = ' ';
        //    strcpy( p, tokenarray[start2].string_ptr );
        //    p += strlen( p );
        //}
        *p++ = ' ';
        i = tokenarray[end2].tokpos - tokenarray[start2].tokpos;
        memcpy( p, tokenarray[start2].tokpos, i );
        p += i;
    } else if ( end2 != EMPTY ) {
        p += sprintf( p, ", %d", end2 );
    }
    *p++ = '\n';
    *p = NULLC;
    return( p );
}

/* render a Jcc instruction */

static char *RenderJcc( char *p, char cc, int neg, char *label )
/**************************************************************/
{
    /* create the jump opcode: j[n]cc */
    *p++ = 'j';
    if ( neg )
        *p++ = 'n';
    *p++ = cc;
    if ( neg == FALSE )
        *p++ = ' '; /* make sure there's room for the inverse jmp */

    *p++ = ' ';
    strcpy( p, label );
    p += strlen( p );
    *p++ = '\n';
    *p = NULLC;
    return( p );
}

/* a "token" in a C expression actually is a set of ASM tokens */

static ret_code HllGetToken( struct hll_item *hll, int *i, struct asm_tok tokenarray[], bool is_true, struct expr *opndx )
/************************************************************************************************************************/
{
    int end_tok;

    /* scan for the next C operator in the token array.
     because the ASM evaluator will report an error if such a thing
     is found */
    for ( end_tok = *i; end_tok < Token_Count; end_tok++ ) {
        if ( ( GetCOp( &tokenarray[end_tok] ) ) != COP_NONE )
            break;
    }
    opndx->kind = EXPR_EMPTY;
    if ( end_tok > *i )
        if ( ERROR == EvalOperand( i, tokenarray, end_tok, opndx, 0 ) )
            return( ERROR );
    return( NOT_ERROR );
}

static char *GetLabelStr( int_32 label, char *buff )
/**************************************************/
{
    sprintf( buff, "@C%04X", label );
    return( buff );
}

static uint_32 GetLabel( struct hll_item *hll, int index )
/********************************************************/
{
    if ( hll->labels[index] > 0 )
        return( hll->labels[index] );
    /* the test label may not exist due to optimization */
    if ( index == LTEST )
        return( hll->labels[LSTART] );
    return( 0 ); /* shouldn't happen */
}

/* a "simple" expression is
 * 1. two tokens, coupled with a <cmp> operator: == != >= <= > <
 * 2. two tokens, coupled with a "&" operator
 * 3. unary operator "!" + one token
 * 4. one token (short form for "<token> != 0")
 */
static ret_code GetSimpleExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true, char *buffer, char **jmp, struct expr *opndx )
/**********************************************************************************************************************************************************************/
{
    //struct expr opndx;
    struct expr op2;
    enum c_bop op;
    //int size;
    char instr;
    int op1_pos;
    int op1_end;
    int op2_pos;
    int op2_end;
    char label[16];
    char *p;
    bool issigned;
    bool neg;

    DebugMsg1(("GetSimpleExpression(buffer=%s) enter\n", buffer ));

    while ( tokenarray[*i].string_ptr[0] == '!' && tokenarray[*i].string_ptr[1] == '\0' ) {
        (*i)++; //GetCOp( i );
        is_true = 1 - is_true;
    }

    op1_pos = *i;
    /* the problem with '()' is that is might enclose just a standard Masm
     * expression or a "hll" expression. The first case is to be handled 
     * entirely by the expression evaluator, while the latter case is to be
     * handled HERE!
     */
    if ( tokenarray[*i].token == T_OP_BRACKET ) {
        int brcnt;
        int j;
        for ( brcnt = 1, j = *i + 1; tokenarray[j].token != T_FINAL; j++ ) {
            if ( tokenarray[j].token == T_OP_BRACKET )
                brcnt++;
            else if ( tokenarray[j].token == T_CL_BRACKET ) {
                brcnt--;
                if ( brcnt == 0 ) /* a standard Masm expression? */
                    break;
            } else if ( ( GetCOp( &tokenarray[j] )) != COP_NONE )
                break;
        }
        if ( brcnt ) {
            (*i)++;
            DebugMsg(("GetSimpleExpression: calling GetExpression, i=%u\n", *i));
            if ( ERROR == GetExpression( hll, i, tokenarray, ilabel, is_true, buffer, jmp, opndx ) )
                return( ERROR );
            DebugMsg(("return from GetExpression, i=%u\n", *i));

            if ( tokenarray[*i].token != T_CL_BRACKET ) {
                //if (( tokenarray[*i].token == T_FINAL ) || ( tokenarray[*i].token == T_CL_BRACKET ))
                DebugMsg(( "GetSimpleExpression: expected ')', found: %s\n", tokenarray[*i].string_ptr ));
                AsmError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
                return( ERROR );
            }
            (*i)++;
            return( NOT_ERROR );
        }
    }

    if ( ERROR == HllGetToken( hll, i, tokenarray, is_true, opndx ) )
        return ( ERROR );

    op1_end = *i;

    if ( ( op = GetCOp( &tokenarray[*i] ) ) != COP_NONE )
        (*i)++;

    if ( op == COP_AND || op == COP_OR ) {
        *i = op1_end;
        if ( opndx->kind == EXPR_EMPTY )
            return( NOT_ERROR );
        op = COP_NONE;
    }

    GetLabelStr( GetLabel( hll, ilabel ), label );

    DebugMsg1(("GetSimpleExpression: EvalOperand ok, kind=%X, i=%u [%s]\n", opndx->kind, *i, tokenarray[*i].tokpos ));

    if ( opndx->kind == EXPR_EMPTY ) {
        /* no valid ASM expression detected. check for some special ops */
        /* COP_ZERO, COP_CARRY, COP_SIGN, COP_PARITY, COP_OVERFLOW */
        if ( op >= COP_ZERO ) {
            p = buffer;
            *jmp = p;
            RenderJcc( p, flaginstr[ op - COP_ZERO ], !is_true, label );
            return( NOT_ERROR );
        }
        if ( hll->condlines )
            return( NOT_ERROR );

        AsmError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
        return( NOT_ERROR );
    }

    if ( ( opndx->kind != EXPR_CONST ) && ( opndx->kind != EXPR_ADDR ) && ( opndx->kind != EXPR_REG ) )
        return( ERROR );

    op2_pos = *i;

    if ( op != COP_NONE ) {
        if ( ERROR == HllGetToken( hll, i, tokenarray, is_true, &op2 ) ) {
            return( ERROR );
        }
        DebugMsg1(("GetSimpleExpression: EvalOperand 2 ok, type=%X, i=%u [%s]\n", op2.type, *i, tokenarray[*i].tokpos));
        if ( op2.kind != EXPR_CONST && op2.kind != EXPR_ADDR && op2.kind != EXPR_REG ) {
            DebugMsg(("GetSimpleExpression: syntax error, op2.kind=%u\n", op2.kind ));
            AsmError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
            return( ERROR );
        }
    }
    op2_end = *i;

    /* now generate ASM code for expression */

    buffer[0] = NULLC;
    switch ( op ) {
    case COP_EQ:
    case COP_NE:
    case COP_GT:
    case COP_LT:
    case COP_GE:
    case COP_LE:
        /* optimisation: generate 'or EAX,EAX' instead of 'cmp EAX,0' */
        if ( Options.masm_compat_gencode &&
            ( op == COP_EQ || op == COP_NE ) &&
            opndx->kind == EXPR_REG &&
            opndx->indirect == FALSE &&
            op2.kind == EXPR_CONST &&
            op2.value == 0 ) {
            p = RenderInstr( buffer, "or", opndx, op1_pos, op1_end, op1_pos, op1_end, tokenarray );
        } else {
            p = RenderInstr( buffer, "cmp", opndx, op1_pos, op1_end, op2_pos, op2_end, tokenarray );
        }

        *jmp = p;

        if ( IS_SIGNED( opndx->mem_type ) || IS_SIGNED( op2.mem_type ) )
            issigned = TRUE;
        else
            issigned = FALSE;

        switch ( op ) {
        case COP_EQ:
            instr = 'z';
            neg = !is_true;
            break;
        case COP_NE:
            instr = 'z';
            neg = is_true;
            break;
        case COP_GT:
            instr = ( issigned ? 'g' : 'a' );
            neg = !is_true;
            break;
        case COP_LT:
            instr = ( issigned ? 'l' : 'b' );
            neg = !is_true;
            break;
        case COP_GE:
            instr = ( issigned ? 'l' : 'b' );
            neg = is_true;
            break;
        case COP_LE:
            instr = ( issigned ? 'g' : 'a' );
            neg = is_true;
            break;
        }
        RenderJcc( p, instr, neg, label );
        break;
    case COP_ANDB:
        p = RenderInstr( buffer, "test", opndx, op1_pos, op1_end, op2_pos, op2_end, tokenarray );
        *jmp = p;
        RenderJcc( p, 'e', is_true, label );
        break;
    case COP_NONE:
        switch ( opndx->kind ) {
        case EXPR_REG:
            if ( opndx->indirect == FALSE ) {
                p = RenderInstr( buffer, "and", opndx, op1_pos, op1_end, op1_pos, op1_end, tokenarray );
                *jmp = p;
                RenderJcc( p, 'z', is_true, label );
                break;
            }
            /* no break */
        case EXPR_ADDR:
            p = RenderInstr( buffer, "cmp", opndx, op1_pos, op1_end, EMPTY, 0, tokenarray );
            *jmp = p;
            RenderJcc( p, 'z', is_true, label );
            break;
        case EXPR_CONST:
#if 0
            /* v2.05: string constant is allowed! */
            if ( opndx->string != NULL ) {
                AsmError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
                return ( ERROR );
            }
#endif
            *jmp = buffer;
            if ( ( is_true == TRUE && opndx->value ) ||
                ( is_true == FALSE && opndx->value == 0 ) )
                sprintf( buffer, "jmp %s\n", label );
            else
                strcpy( buffer, " \n" ); /* make sure there is a char */
            break;
        }
    }
    return( NOT_ERROR );
}

static void InvertJmp( char *p )
/******************************/
{
    if ( *p == 'e' || *p == 'z' || *p == 'c' || *p == 's' || *p == 'p' || *p == 'o' ) {
        *(p+1) = *p;
        *p = 'n';
        return;
    } else if ( *p == 'n' ) {
        *p = *(p+1);
        *(p+1) = ' ';
        return;
    } else if ( *p == 'a' ) {
        *p++ = 'b';
    } else if ( *p == 'b' ) {
        *p++ = 'a';
    } else if ( *p == 'g' ) {
        *p++ = 'l';
    } else if ( *p == 'l' ) {
        *p++ = 'g';
    } else
        return;

    if ( *p == 'e' )
        *p = ' ';
    else
        *p = 'e';
    return;
}

static void ReplaceLabel( char *p, uint_32 olabel, uint_32 nlabel )
/*****************************************************************/
{
    char oldlbl[16];
    char newlbl[16];
    int i;

    GetLabelStr( olabel, oldlbl );
    GetLabelStr( nlabel, newlbl );

    i = strlen( newlbl );

    DebugMsg1(("ReplaceLabel(%s, %s, %s)\n", p, oldlbl, newlbl ));
    while ( p = strstr( p, oldlbl ) ) {
        memcpy( p, newlbl, i );
        p = p  + i;
    }
}

/* operator &&, which has the second lowest precedence, is handled here */

static ret_code GetAndExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true, char *buffer, char **lastjmp, struct expr *opndx )
/***********************************************************************************************************************************************************************/
{
    enum c_bop op;
    int cur_pos;
    char *ptr = buffer;
    uint_32 truelabel = 0;
    char buff[16];
    //char *nlabel;
    //char *olabel;

    DebugMsg1(("GetAndExpression(buffer=%s) enter\n", buffer ));

    while (1) {
        ptr = ptr + strlen( ptr );
        cur_pos = *i;
        if ( ERROR == GetSimpleExpression( hll, i, tokenarray, ilabel, is_true, ptr, lastjmp, opndx ) )
            return( ERROR );
        cur_pos = *i;
        op = GetCOp( &tokenarray[*i] );
        if ( op != COP_AND )
            break;
        (*i)++;
        /* v2.02: query is_true var instead of cmd field!
         * this is important if the '!' operator was used.
         */
        //if ( hll->cmd == HLL_WHILE || hll->cmd == HLL_BREAK ) {
        if ( is_true ) {
            /* todo: please describe what's done here and why! */
            if ( *lastjmp ) {
                char *p = *lastjmp;
                InvertJmp( p+1 );         /* step 1 */
                if ( truelabel == 0 )     /* step 2 */
                    truelabel = GetHllLabel();
                ReplaceLabel( buffer, GetLabel( hll, ilabel ), truelabel );
                *lastjmp = NULL;
            }
        }
    };

    if ( truelabel > 0 ) {
        sprintf( ptr+strlen(ptr), "%s" LABELQUAL "\n", GetLabelStr( truelabel, buff ) );
        *lastjmp = NULL;
    }
    *i = cur_pos;
    return( NOT_ERROR );
}

/* operator ||, which has the lowest precedence, is handled here */

static ret_code GetExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true, char *buffer, char **lastjmp, struct expr *opndx )
/********************************************************************************************************************************************************************/
{
    enum c_bop op;
    int cur_pos;
    //bool ordetected = FALSE;
    char *ptr = buffer;
    char buff[16];
    uint_32 truelabel = 0;
    uint_32 nlabel;
    uint_32 olabel;

    DebugMsg1(("GetExpression(buffer=%s) enter\n", buffer ));

    for ( ;; ) {
        ptr = ptr + strlen( ptr );
        cur_pos = *i;
        if ( ERROR == GetAndExpression( hll, i, tokenarray, ilabel, is_true, ptr, lastjmp, opndx ) )
            return( ERROR );

        cur_pos = *i;
        op = GetCOp( &tokenarray[*i] );
        if ( op != COP_OR ) {
            //*i = cur_pos;
            if ( truelabel > 0 ) {
                sprintf( ptr+strlen( ptr ), "%s" LABELQUAL "\n", GetLabelStr( truelabel, buff ) );
                truelabel = 0;
            }
            break;
        }
        (*i)++;
        /* the generated code of last simple expression has to be modified
         1. the last jump must be inverted
         2. a "is_true" label must be created (it's used to jump "behind" the expr)
         3. create a new label
         4. the current "false" label must be generated

         if it is a .REPEAT, step 4 is slightly more difficult, since the "false"
         label is already "gone":
         4a. create a new label
         4b. replace the "false" label in the generated code by the new label

         */
        /* v2.02: query is_true var instead of cmd field!
         * this is important if the '!' operator was used.
         */
        //if (*lastjmp && ( hll->cmd != HLL_BREAK ) && ( hll->cmd != HLL_WHILE ) ) {
        if ( *lastjmp && is_true == FALSE ) {
            char *p = *lastjmp;
            InvertJmp(p+1);         /* step 1 */
            p += 4;
            if ( truelabel == 0 )  /* step 2 */
                truelabel = GetHllLabel();
            strcpy( p, GetLabelStr( truelabel, buff ) );
            strcat( p, "\n" );
            *lastjmp = NULL;
            nlabel = GetHllLabel();  /* step 3 */
            olabel = GetLabel( hll, ilabel );
            if ( hll->cmd == HLL_REPEAT ) {
                ReplaceLabel( buffer, olabel, nlabel );
                sprintf( ptr + strlen( ptr ), "%s" LABELQUAL "\n", GetLabelStr( nlabel, buff ) );
            } else {
#if 0
                sprintf( ptr + strlen( ptr ), "%s" LABELQUAL "\n", GetLabelStr( olabel, buff ) );
                SetLabel(hll, ilabel, nlabel);
#else
                sprintf( ptr + strlen( ptr ), "%s" LABELQUAL "\n", GetLabelStr( olabel, buff ) );
                ReplaceLabel( buffer, olabel, nlabel );
#endif
            }
        }
    }
    return( NOT_ERROR );
}

/* update hll->condlines */

static ret_code WriteExprSrc( struct hll_item *hll, char *buffer )
/****************************************************************/
{
    int size;
    char *p;

    size = strlen( buffer ) + 1;
    if ( hll->condlines ) {
        size += strlen( hll->condlines ) + 1;
    }
    p = AsmAlloc( size );
#ifdef DEBUG_OUT
    cntCond++;
    cntCondBytes += size;
#endif
    if ( hll->condlines ) {
        strcpy( p, hll->condlines );
        strcat( p, "\n" );
        strcat( p, buffer );
    } else
        strcpy( p, buffer );

    AsmFree( hll->condlines );
    hll->condlines = p;
    return( NOT_ERROR );
}

/*
 * evaluate the C like boolean expression found in HLL structs
 * like .IF, .ELSEIF, .WHILE, .UNTIL and .UNTILCXZ
 * might return multiple lines (strings separated by 0x0A)
 * - i = index for tokenarray[] where expression starts. Is restricted
 *       to one source line (till T_FINAL)
 * - label: label to jump to if expression is <is_true>!
 * is_true:
 *   .IF:       FALSE
 *   .ELSEIF:   FALSE
 *   .WHILE:    TRUE
 *   .UNTIL:    FALSE
 *   .UNTILCXZ: FALSE
 *   .BREAK .IF:TRUE
 *   .CONT .IF: TRUE
 */


static ret_code EvaluateHllExpression( struct hll_item *hll, int *i, struct asm_tok tokenarray[], int ilabel, bool is_true )
/**************************************************************************************************************************/
{
    char *lastjmp = NULL;
    struct expr opndx;
    char buffer[MAX_LINE_LEN*2];

    DebugMsg1(("EvaluateHllExpression enter\n"));

    buffer[0] = NULLC;
    if ( ERROR == GetExpression( hll, i, tokenarray, ilabel, is_true, buffer, &lastjmp, &opndx ) )
        return( ERROR );
    if ( buffer[0] )
        WriteExprSrc( hll, buffer );
    if ( hll->condlines != NULL && *hll->condlines == '\n' ) {
        AsmError( SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE );
        return( ERROR );
    }
    return( NOT_ERROR );
}

/* write ASM test lines */

static ret_code HllPushTestLines( struct hll_item *hll )
/******************************************************/
{
    char *p = hll->condlines;
    char *p2;
    char buffer[MAX_LINE_LEN];

    DebugMsg1(("HllPushTestLines enter\n"));
    if ( !p )
        return( ERROR );

    while ( *p ) {
        if (*p == ' ') p++; /* there might be lines with 1 ' ' only! */
        for ( p2=buffer; *p && (*p != 0x0A);)
            *p2++ = *p++;
        *p2 = NULLC;
        if ( *p == 0x0A )
            p++;
        if ( *buffer )
            AddLineQueue( buffer );
    }
    AsmFree( hll->condlines );
    hll->condlines = NULL;


    return( NOT_ERROR );
}

/* for .UNTILCXZ: check if expression is simple enough.
 * what's acceptable is ONE condition, and just operators == and !=
 * Constants (0 or != 0) are also accepted.
 */

static ret_code CheckCXZLines( struct hll_item *hll )
/***************************************************/
{
    int lines = 0;
    int i;
    bool NL = TRUE;
    char *p = hll->condlines;

    for (; *p; p++ ) {
        if ( *p == 0x0a ) {
            NL = TRUE;
            lines++;
        } else if ( NL ) {
            NL = FALSE;
            if ( *p == 'j' ) {
                p++;
                /* v2.06: rewritten. case ".untilcxz 1" still has problems */
                if ( *p == 'm' && lines == 0 ) {
                    p--;
                    i = strlen( p );
                    while ( i ) {
                        *(p+2+i) = *(p+i);
                        i--;
                    }
                    memcpy( p,"loope",5 );
                } else if ( lines == 1 && ( *p == 'z' || (*p == 'n' && *(p+1) == 'z') ) ) {
                    p--;
                    i = strlen( p );
                    while ( i ) {
                        *(p+3+i) = *(p+i);
                        i--;
                    }
                    memcpy( p,"loop",4 );
                } else
                    return( ERROR );
            }
        }
    }
    if ( lines > 2 )
        return( ERROR );
    return( NOT_ERROR );
}

/* Start a .IF, .WHILE, .REPEAT item */

ret_code HllStartDir( int i, struct asm_tok tokenarray[] )
/********************************************************/
{
    struct hll_item      *hll;
    int                  cmd = tokenarray[i].tokval;
    char buff[16];

    DebugMsg1(("HllStartDir(%s) enter\n", tokenarray[i].string_ptr ));

    i++; /* skip directive */

    /* v2.06: is there an item on the free stack? */
    if ( HllFree ) {
        hll = HllFree;
#ifdef DEBUG_OUT
        cntReused++;
#endif
    } else {
        hll = AsmAlloc( sizeof( struct hll_item ) );
#ifdef DEBUG_OUT
        cntAlloc++;
#endif
    }

    /* create labels which are always needed */
    /* for .IF -.ENDIF without .ELSE no LEXIT-label is needed. */

    hll->labels[LSTART] = 0;
    hll->labels[LTEST] = GetHllLabel();
    hll->labels[LEXIT] = 0;

    hll->condlines = NULL;

    /* structure for .IF .ELSE .ENDIF
     *    cond jump to LTEST-label
     *    ...
     *    jmp LEXIT
     *  LTEST:
     *    ...
     *  LEXIT:

     * structure for .IF .ELSEIF
     *    cond jump to LTEST
     *    ...
     *    jmp LEXIT
     *  LTEST:
     *    cond jump to (new) LTEST
     *    ...
     *    jmp LEXIT
     *  LTEST:
     *    ...

     * structure for .WHILE and .REPEAT:
     *   jmp LTEST (for .WHILE only)
     * LSTART:
     *   ...
     * LTEST: (jumped to by .continue)
     *   test end condition, cond jump to LSTART label
     * LEXIT: (jumped to by .break)
     */
    PushLineQueue();

    switch ( cmd ) {
    case T_DOT_IF:
        hll->cmd = HLL_IF;
        /* get the C-style expression, convert to ASM code lines */
        if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LTEST, FALSE ) ) {
            return( ERROR );
        }
        HllPushTestLines( hll );
#if 1
        /* if no lines have been created, the LTEST label isn't needed */
        if ( line_queue == NULL ) {
            hll->labels[LTEST] = 0;
        }
#endif
        break;
    case T_DOT_WHILE:
    case T_DOT_REPEAT:
        /* create the label to loop start */
        hll->labels[LSTART] = GetHllLabel();
        hll->labels[LEXIT] = GetHllLabel();
        if ( cmd == T_DOT_WHILE ) {
            hll->cmd = HLL_WHILE;
            if ( tokenarray[i].token != T_FINAL ) {
                if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LSTART, TRUE ) ) {
                    return( ERROR );
                }
            } else
                hll->condlines = "";
            /* create a jump to second label */
            /* optimisation: if second label is just a jump, dont jump! */
            if ( hll->condlines && _memicmp( hll->condlines, "jmp", 3 ) ) {
                AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LTEST], buff ) );
            } else {
                hll->labels[LTEST] = 0;
            }
        } else {
            hll->cmd = HLL_REPEAT;
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LSTART], buff ) );
        break;
    }
    if ( tokenarray[i].token != T_FINAL ) {
        AsmFree( hll->condlines );
        DebugMsg(("HllStartDir: unexpected token %u [%s]\n", tokenarray[i].token, tokenarray[i].string_ptr ));
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    /* v2.06: remove the item from the free stack */
    if ( hll == HllFree )
        HllFree = hll->next;
    hll->next = HllStack;
    HllStack = hll;

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    if ( line_queue ) /* might be NULL! (".if 1") */
        RunLineQueue();

    return( NOT_ERROR );
}

/* End a .IF, .WHILE, .REPEAT item
 * that is: .ENDIF, .ENDW, .UNTIL and .UNTILCXZ are handled here
 */
ret_code HllEndDir( int i, struct asm_tok tokenarray[] )
/******************************************************/
{
    //struct asym       *sym;
    struct hll_item     *hll;
    int                 cmd = tokenarray[i].tokval;
    char buff[16];

    DebugMsg1(("HllEndDir(%s) enter\n", tokenarray[i].string_ptr ));

    if ( HllStack == NULL ) {
        DebugMsg(("HllEndDir: hll stack is empty\n"));
        AsmError( DIRECTIVE_MUST_BE_IN_CONTROL_BLOCK );
        return( ERROR );
    }

    hll = HllStack;
    HllStack = hll->next;
    /* v2.06: move the item to the free stack */
    hll->next = HllFree;
    HllFree = hll;

    PushLineQueue();

    switch ( cmd ) {
    case T_DOT_ENDIF:
        if ( hll->cmd != HLL_IF ) {
            DebugMsg(("HllEndDir: no .IF on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        /* if a test label isn't created yet, create it */
        if ( hll->labels[LTEST] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );
        }
        /* create the exit label if it exists */
        if ( hll->labels[LEXIT] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        }
        i++;
        break;
    case T_DOT_ENDW:
        if ( hll->cmd != HLL_WHILE ) {
            DebugMsg(("HllEndDir: no .WHILE on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        /* create test label  */
        if ( hll->labels[LTEST] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );
            DebugMsg(("HllEndDir: created: %s" LABELQUAL "\n", GetLabelStr( hll->labels[LTEST], buff ) ));
        }
        HllPushTestLines( hll );

        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        i++;
        break;
    case T_DOT_UNTILCXZ:
        if ( hll->cmd != HLL_REPEAT ) {
            DebugMsg(("HllEndDir: no .REPEAT on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );

        i++;
        /* read in optional (simple) expression */
        if ( tokenarray[i].token != T_FINAL ) {
            if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LSTART, FALSE ) ) {
                return( ERROR );
            }
            if ( CheckCXZLines( hll ) == ERROR ) {
                AsmError( EXPR_TOO_COMPLEX_FOR_UNTILCXZ );
                return( ERROR );
            }
            /* write condition lines */
            HllPushTestLines( hll );
        } else {
            AddLineQueueX( " loop %s", GetLabelStr( hll->labels[LSTART], buff ) );
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        break;
    case T_DOT_UNTIL:
        if ( hll->cmd != HLL_REPEAT ) {
            DebugMsg(("HllEndDir: no .REPEAT on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );

        i++;
        /* read in (optional) expression */
        /* if expression is missing, just generate nothing */
        if ( tokenarray[i].token != T_FINAL ) {
            if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LSTART, FALSE ) ) {
                return( ERROR );
            }
            /* write condition lines */
            HllPushTestLines( hll );
        }
#if 0
        AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LSTART], buff ) );
#endif

        AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LEXIT], buff ) );
        break;
    }

    AsmFree( hll->condlines );

    if ( tokenarray[i].token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    if ( line_queue )
        RunLineQueue();

    return( NOT_ERROR );
}

/* Exit current .IF, .WHILE, .REPEAT item
 * that is: .ELSE, .ELSEIF, .CONTINUE and .BREAK are handled here
 */
ret_code HllExitDir( int i, struct asm_tok tokenarray[] )
/*******************************************************/
{
    //int               level;
    //struct asym       *sym;
    struct hll_item     *hll;
    char                *savedlines;
    enum hll_cmd        savedcmd;
    int                 cmd = tokenarray[i].tokval;
    char buff[16];

    DebugMsg1(("HllExitDir(%s) enter\n", tokenarray[i].string_ptr ));

    hll = HllStack;

    if ( hll == NULL ) {
        DebugMsg(("HllExitDir stack error\n"));
        AsmError( DIRECTIVE_MUST_BE_IN_CONTROL_BLOCK );
        return( ERROR );
    }

    PushLineQueue();

    switch ( cmd ) {
    case T_DOT_ELSE:
    case T_DOT_ELSEIF:
        if ( hll->cmd != HLL_IF ) {
            DebugMsg(("HllExitDir(%s): labels[LTEST]=%X\n", tokenarray[i].string_ptr, hll->labels[LTEST]));
            AsmErr( BLOCK_NESTING_ERROR, tokenarray[i].string_ptr );
            return( ERROR );
        }
        /* the "labels[LEXIT]" label is only needed if an .ELSE branch exists.
         That's why it is created delayed.
         */
        if ( hll->labels[LEXIT] == 0 )
            hll->labels[LEXIT] = GetHllLabel();

        AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LEXIT], buff ) );
        if ( hll->labels[LTEST] > 0 ) {
            AddLineQueueX( "%s" LABELQUAL, GetLabelStr( hll->labels[LTEST], buff ) );
            hll->labels[LTEST] = 0;
        }
        i++;
        if ( cmd == T_DOT_ELSEIF ) {
            /* create new labels[LTEST] label */
            hll->labels[LTEST] = GetHllLabel();
            if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LTEST, FALSE ) ) {
                return( ERROR );
            }
            HllPushTestLines( hll );
        }
        break;
    case T_DOT_BREAK:
    case T_DOT_CONTINUE:
        for ( ; hll && hll->cmd == HLL_IF; hll = hll->next );
        if ( hll == NULL ) {
            AsmError( DIRECTIVE_MUST_BE_IN_CONTROL_BLOCK );
            return( ERROR );
        }
        /* .BREAK .IF ... or .CONTINUE .IF ? */
        i++;
        if ( tokenarray[i].token != T_FINAL ) {
            if ( tokenarray[i].token == T_DIRECTIVE && tokenarray[i].tokval == T_DOT_IF ) {
                savedlines = hll->condlines;
                savedcmd = hll->cmd;
                hll->condlines = NULL;
                hll->cmd = HLL_BREAK;
                i++;
                if ( cmd == T_DOT_BREAK ) {
                    if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LEXIT, TRUE ) ) {
                        return( ERROR );
                    }
                } else { /* T_DOT_CONTINUE */
                    if ( ERROR == EvaluateHllExpression( hll, &i, tokenarray, LTEST, TRUE ) ) {
                        return( ERROR );
                    }
                }
                HllPushTestLines( hll );
                AsmFree( hll->condlines );
                hll->condlines = savedlines;
                hll->cmd = savedcmd;
            }
        } else {
            if ( cmd == T_DOT_BREAK ) {
                AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LEXIT], buff ) );
            } else {
                if ( hll->labels[LTEST] > 0 )
                    AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LTEST], buff ) );
                else
                    AddLineQueueX( " jmp %s", GetLabelStr( hll->labels[LSTART], buff ) );
            }
        }
        break;
    }
    if ( tokenarray[i].token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_DIRECTIVE, GetCurrOffset(), NULL );

    RunLineQueue();

    return( NOT_ERROR );
}

/* check if an hll block has been left open */

void HllCheckOpen( void )
/***********************/
{
    if ( HllStack ) {
        //AsmErr( BLOCK_NESTING_ERROR, ".if-.repeat-.while" );
        AsmErr( UNMATCHED_BLOCK_NESTING, ".if-.repeat-.while" );
    }
    DebugMsg1(("HllCheckOpen: allocated items:%u, reused items:%u, cond-blocks/bytes:%u/%u\n", cntAlloc, cntReused, cntCond, cntCondBytes ));
}

/* HllInit() is called for each pass */

void HllInit( int pass )
/**********************/
{
    if ( pass == PASS_1 )
        HllFree = NULL;
    HllStack = NULL; /* empty stack of open hll directives */
    ModuleInfo.hll_label = 0; /* init hll label counter */
    return;
}
