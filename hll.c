/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  support the hll constructs .IF, .WHILE, .REPEAT, ...
*
****************************************************************************/


#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"
#include "queues.h"
#include "equate.h"
#include "mangle.h"
#include "labels.h"
#include "input.h"
#include "expreval.h"
#include "types.h"
#include "hll.h"
#include "fastpass.h"

#include "myassert.h"

#define LABELSIZE 8

static int GetExpression(hll_list * hll, int *i, int ilabel, bool is_true, char * buffer, char **lastjmp);

#define LABELFIRST 0
#define LABELTEST  1
#define LABELEXIT  2

#define IS_SIGNED(x)  x == MT_SBYTE || x == MT_SWORD || x == MT_SDWORD

// c binary ops

typedef enum {
    COP_NONE,
    COP_EQ,   // ==
    COP_NE,   // !=
    COP_GT,   // >
    COP_LT,   // <
    COP_GE,   // >=
    COP_LE,   // <=
    COP_AND,  // &&
    COP_OR,   // ||
    COP_ANDB, // &
    COP_NEG,  // !
    COP_ZERO, // ZERO?   not really a valid C operator
    COP_CARRY,// CARRY?  not really a valid C operator
    COP_SIGN, // SIGN?   not really a valid C operator
    COP_PARITY,  // PARITY?   not really a valid C operator
    COP_OVERFLOW // OVERFLOW? not really a valid C operator
} c_bop;

static hll_list     *HllStack; // for .WHILE, .IF, .REPEAT

static char * MakeAnonymousLabel(void)
{
    char *p = AsmAlloc( LABELSIZE );
    sprintf(p, "@C%04u", ModuleInfo.hll_label);
    ModuleInfo.hll_label++;
    return (p);
}

// get a C binary operator from the token stream.
// there is a problem with the '<' because it is a "string delimiter"
// which Tokenize() usually is to remove.
// There has been a hack implemented in Tokenize() so that it won't touch the
// '<' if .IF, .ELSEIF, .WHILE, .UNTIL, .UNTILCXZ or .BREAK/.CONTINUE has been
// detected

static c_bop GetCOp(int * i)
{
    int size = 0;
    c_bop rc;
    char *p = AsmBuffer[*i]->string_ptr;

    if (AsmBuffer[*i]->token == T_STRING)
        size = AsmBuffer[*i]->value;

    if ( size == 2) {
        if (*p == '=' && *(p+1) == '=')
            rc = COP_EQ;
        else if (*p == '!' && *(p+1) == '=')
            rc = COP_NE;
        else if (*p == '>' && *(p+1) == '=')
            rc = COP_GE;
        else if (*p == '<' && *(p+1) == '=')
            rc = COP_LE;
        else if (*p == '&' && *(p+1) == '&')
            rc = COP_AND;
        else if (*p == '|' && *(p+1) == '|')
            rc = COP_OR;
        else
            return( COP_NONE );
    } else if ( size == 1) {
        if (*p == '>')
            rc = COP_GT;
        else if (*p == '<')
            rc = COP_LT;
        else if (*p == '&')
            rc = COP_ANDB;
        else if (*p == '!')
            rc = COP_NEG;
        else
            return( COP_NONE );
    } else {
        if ( AsmBuffer[*i]->token != T_ID)
            return( COP_NONE );
        /* a valid "flag" string must end with a question mark */
        size = strlen(AsmBuffer[*i]->string_ptr);
        if (*(p+size-1) != '?')
            return( COP_NONE );
        if (size == 5 && (0 == memicmp( p, "ZERO", 4 )))
            rc = COP_ZERO;
        else if (size == 6 && (0 == memicmp( p, "CARRY", 5 )))
            rc = COP_CARRY;
        else if (size == 5 && (0 == memicmp( p, "SIGN", 4 )))
            rc = COP_SIGN;
        else if (size == 7 && (0 == memicmp( p, "PARITY", 6 )))
            rc = COP_PARITY;
        else if (size == 9 && (0 == memicmp( p, "OVERFLOW", 8 )))
            rc = COP_OVERFLOW;
        else
            return( COP_NONE );
    }
    *i += 1;
    return( rc );
}

// render an instruction operand

static void RenderOpnd(expr_list * op, char * buffer, int start, int end)
{
    /* just copy the operand's tokens into the buffer */
    for (;start < end;start++) {
        strcat(buffer, AsmBuffer[start]->string_ptr);
        strcat(buffer, " ");
    }
    return;
}

// a "token" in a C expression actually is a set of ASM tokens

static int GetToken(hll_list * hll, int *i, bool is_true, expr_list * opndx)
{
    int end_tok;

    /* scan for the next C operator in the token array.
     because the ASM evaluator will report an error if such a thing
     is found */
    for (end_tok = *i;end_tok < Token_Count;end_tok++) {
        int j = end_tok;
        if ((GetCOp(&j)) != COP_NONE)
            break;
    }
    opndx->type = EXPR_EMPTY;
    if (end_tok > *i)
        if (ERROR == EvalOperand(i, end_tok, opndx, TRUE))
            return (ERROR);
    return(NOT_ERROR);
}

static char * GetLabel(hll_list *hll, int label)
{
    if (label == LABELFIRST)
        return(hll->symfirst);
    else if (label == LABELTEST)
        return(hll->symtest);
    else
        return(hll->symexit);
}

static void SetLabel(hll_list *hll, int label, char * labelname)
{
    if (label == LABELFIRST)
        hll->symfirst = labelname;
    else if (label == LABELTEST)
        hll->symtest = labelname;
    else
        hll->symexit = labelname;
}

// a "simple" expression is
// 1. two tokens, coupled with a <cmp> operator: == != >= <= > <
// 2. two tokens, coupled with a "&" operator
// 3. unary operator "!" + one token
// 4. one token (short form for "<token> != 0")

static int GetSimpleExpression(hll_list * hll, int *i, int ilabel, bool is_true, char * buffer, char **jmp)
{
    expr_list opndx;
    expr_list op2;
    c_bop op;
    int size;
    int end_tok;
    int op1_pos;
    int op1_end;
    int op2_pos;
    int op2_end;
    char *label;
    bool issigned;

    DebugMsg(("GetSimpleExpression enter\n"));

    while (AsmBuffer[*i]->string_ptr[0] == '!' && AsmBuffer[*i]->string_ptr[1] == '\0') {
        GetCOp(i);
        is_true = 1 - is_true;
    }

    if (AsmBuffer[*i]->token == T_OP_BRACKET) {
        (*i)++;
        if (ERROR == GetExpression(hll, i, ilabel, is_true, buffer, jmp))
            return (ERROR);

        DebugMsg(("return from GetExpression, i=%u\n", *i));

        if (AsmBuffer[*i]->token != T_CL_BRACKET) {
            AsmError(SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE);
            return (ERROR);
        }
        (*i)++;
//        if ((AsmBuffer[*i]->token == T_FINAL) || (AsmBuffer[*i]->token == T_CL_BRACKET))
            return(NOT_ERROR);
    }
    op1_pos = *i;
    if (ERROR == GetToken(hll, i, is_true, &opndx))
        return (ERROR);

    op1_end = *i;

    op = GetCOp(i);

    if ((op == COP_AND) || (op == COP_OR)) {
        *i = op1_end;
        if (opndx.type == EXPR_EMPTY)
            return(NOT_ERROR);
        op = COP_NONE;
    }

    label = GetLabel(hll, ilabel);

    DebugMsg(("GetSimpleExpression: EvalOperand ok, type=%X, i=%u\n", opndx.type, *i));

    if (opndx.type == EXPR_EMPTY) {
        /* no valid ASM expression detected. check for some special ops */
        /* COP_ZERO, COP_CARRY, COP_SIGN, COP_PARITY, COP_OVERFLOW */
        if ( op >= COP_ZERO ) {
            char t;
            char * p;
            char * s;
            p = buffer;
            *jmp = p;
            *p++ = 'j';
            if (is_true == FALSE)
                *p++ = 'n';

            switch (op) {
            case COP_CARRY:
                *p++ = 'c';
                break;
            case COP_ZERO:
                *p++ = 'z';
                break;
            case COP_SIGN:
                *p++ = 's';
                break;
            case COP_PARITY:
                *p++ = 'p';
                break;
            case COP_OVERFLOW:
                *p++ = 'o';
                break;
            }
            *p++ = ' ';
            if (is_true == TRUE)
                *p++ = ' ';
            strcpy(p, label);
            goto done;
        }
        if (hll->condlines)
            return (NOT_ERROR);
        else {
            AsmError(SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE);
            return (NOT_ERROR);
        }
    }

    if ((opndx.type != EXPR_CONST) && (opndx.type != EXPR_ADDR) && (opndx.type != EXPR_REG))
        return (ERROR);

    op2_pos = *i;

    if (op != COP_NONE) {
        if (ERROR == GetToken(hll, i, is_true, &op2)) {
            return (ERROR);
        }
        DebugMsg(("GetSimpleExpression: EvalOperand 2 ok, type=%X, i=%u\n", op2.type, *i));
        if ((op2.type != EXPR_CONST) && (op2.type != EXPR_ADDR) && (op2.type != EXPR_REG)) {
            DebugMsg(("GetSimpleExpression: syntax error, op2.type=%u\n", op2.type ));
            AsmError(SYNTAX_ERROR);
            return (ERROR);
        }
    }
    op2_end = *i;

    /* now generate ASM code for expression */

    buffer[0] = 0;
    if ((op == COP_EQ) ||
        (op == COP_NE) ||
        (op == COP_GT) ||
        (op == COP_LT) ||
        (op == COP_GE) ||
        (op == COP_LE)) {
        char * p;

        strcat(buffer," cmp ");
        RenderOpnd(&opndx, buffer, op1_pos, op1_end);
        strcat(buffer, ", ");
        RenderOpnd(&op2, buffer, op2_pos, op2_end);
        strcat(buffer, "\n");

        p = buffer + strlen(buffer);
        *jmp = p;

        if (IS_SIGNED(opndx.mem_type) || IS_SIGNED(op2.mem_type))
            issigned = TRUE;
        else
            issigned = FALSE;

        switch (op) {
        case COP_EQ:
            if (is_true)
                strcpy(p, "jz  ");
            else
                strcpy(p, "jnz ");
            break;
        case COP_NE:
            if (is_true)
                strcpy(p, "jnz ");
            else
                strcpy(p, "jz  ");
            break;
        case COP_GT:
            if (issigned == TRUE) {
                if (is_true)
                    strcpy(p, "jg  ");
                else
                    strcpy(p, "jle ");
            } else {
                if (is_true)
                    strcpy(p, "ja  ");
                else
                    strcpy(p, "jbe ");
            }
            break;
        case COP_LT:
            if (issigned == TRUE) {
                if (is_true)
                    strcpy(p, "jl  ");
                else
                    strcpy(p, "jge ");
            } else {
                if (is_true)
                    strcpy(p, "jb  ");
                else
                    strcpy(p, "jae ");
            }
            break;
        case COP_GE:
            if (issigned == TRUE) {
                if (is_true)
                    strcpy(p, "jge ");
                else
                    strcpy(p, "jl  ");
            } else {
                if (is_true)
                    strcpy(p, "jae ");
                else
                    strcpy(p, "jb  ");
            }
            break;
        case COP_LE:
            if (issigned == TRUE) {
                if (is_true)
                    strcpy(p, "jle ");
                else
                    strcpy(p, "jg  ");
            } else {
                if (is_true)
                    strcpy(p, "jbe ");
                else
                    strcpy(p, "ja  ");
            }
            break;
        }
        strcat(p, label);

    } else if (op == COP_ANDB) {
        char * p;

        strcat(buffer," test ");
        RenderOpnd(&opndx, buffer, op1_pos, op1_end);
        strcat(buffer, ", ");
        RenderOpnd(&op2, buffer, op2_pos, op2_end);
        strcat(buffer, "\n");

        p = buffer + strlen(buffer);
        *jmp = p;

        if (is_true)
            strcpy(p, "jne ");
        else
            strcpy(p, "je  ");

        strcat(p, label);

    } else if (op == COP_NONE) {
        char * p;

        switch (opndx.type) {
        case EXPR_REG:
            if (opndx.indirect == FALSE) {
                strcat(buffer,"and ");
                RenderOpnd(&opndx, buffer, op1_pos, op1_end);
                strcat(buffer,", ");
                RenderOpnd(&opndx, buffer, op1_pos, op1_end);
                strcat(buffer,"\n");
                p = buffer + strlen(buffer);
                *jmp = p;
                if (is_true)
                    strcpy(p, "jnz ");
                else
                    strcpy(p, "jz  ");
                strcat(p, label);
                break;
            }
        case EXPR_ADDR:
            strcat(buffer,"cmp ");
            RenderOpnd(&opndx, buffer, op1_pos, op1_end);
            strcat(buffer,", 0\n");
            p = buffer + strlen(buffer);
            *jmp = p;
            if (is_true)
                strcpy(p, "jnz ");  /* switched */
            else
                strcpy(p, "jz  ");
            strcat(buffer, label);
            break;
        case EXPR_CONST:
            if (opndx.string != NULL) {
                AsmError(SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE);
                return (ERROR);
            }
            *jmp = buffer;
            if (is_true == TRUE)
                if (opndx.value)
                    sprintf(buffer,"jmp %s", label);
                else
                    strcpy(buffer," "); /* make sure there is a char */
            if (is_true == FALSE)
                if (opndx.value == 0)
                    sprintf(buffer,"jmp %s", label);
                else
                    strcpy(buffer," "); /* make sure there is a char */
            break;
        }
    }
done:
    strcat(buffer,"\n");

    return(NOT_ERROR);
}

static void InvertJmp(char * p)
{
    if (*p == 'e' || *p == 'z' || *p == 'c' || *p == 's' || *p == 'p' || *p == 'o') {
        *(p+1) = *p;
        *p = 'n';
        return;
    } else if (*p == 'n') {
        *p = *(p+1);
        *(p+1) = ' ';
        return;
    } else if (*p == 'a') {
        *p++ = 'b';
    } else if (*p == 'b') {
        *p++ = 'a';
    } else if (*p == 'g') {
        *p++ = 'l';
    } else if (*p == 'l') {
        *p++ = 'g';
    } else
        return;

    if (*p == 'e')
        *p = ' ';
    else
        *p = 'e';
    return;
}

static void ReplaceLabel(char * p, char * olabel, char * nlabel)
{
    int i = strlen(nlabel);

    while (p = strstr(p, olabel)) {
        memcpy(p, nlabel, i);
        p = p  + i;
    }
}

// operator &&, which has the second lowest precedence, is handled here

static int GetAndExpression(hll_list * hll, int *i, int ilabel, bool is_true, char * buffer, char **lastjmp)
{
    c_bop op;
    int cur_pos;
    char * ptr = buffer;
    char * truelabel = NULL;
    char * nlabel;
    char * olabel;

    DebugMsg(("GetAndExpression enter\n"));

    while (1) {
        ptr = ptr + strlen(ptr);
        cur_pos = *i;
        if (ERROR == GetSimpleExpression(hll, i, ilabel, is_true, ptr, lastjmp))
            return (ERROR);
        cur_pos = *i;
        op = GetCOp(i);
        if (op != COP_AND)
            break;
        if (hll->cmd == HLL_WHILE || hll->cmd == HLL_BREAK) {
            if (*lastjmp) {
                char * p = *lastjmp;
                InvertJmp(p+1);         /* step 1 */
                if (truelabel == NULL)  /* step 2 */
                    truelabel = MakeAnonymousLabel();
                ReplaceLabel(buffer, GetLabel(hll, ilabel), truelabel);
                *lastjmp = NULL;
            }
        }
    };

    if (truelabel) {
        sprintf(ptr+strlen(ptr), "%s:\n", truelabel);
        AsmFree(truelabel);
        *lastjmp = NULL;
    }
    *i = cur_pos;
    return (NOT_ERROR);
}

// operator ||, which has the lowest precedence, is handled here

static int GetExpression(hll_list * hll, int *i, int ilabel, bool is_true, char * buffer, char **lastjmp)
{
    c_bop op;
    int cur_pos;
    bool ordetected = FALSE;
    char * ptr = buffer;
    char * truelabel = NULL;
    char * nlabel;
    char * olabel;

    DebugMsg(("GetExpression enter\n"));

    for (;;) {
        ptr = ptr + strlen(ptr);
        cur_pos = *i;
        if (ERROR == GetAndExpression(hll, i, ilabel, is_true, ptr, lastjmp))
            return (ERROR);

        cur_pos = *i;
        op = GetCOp(i);
        if (op != COP_OR) {
            *i = cur_pos;
            if (truelabel) {
                sprintf(ptr+strlen(ptr), "%s:\n", truelabel);
                AsmFree(truelabel);
            }
            break;
        }
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
        if (*lastjmp && (hll->cmd != HLL_BREAK) && (hll->cmd != HLL_WHILE)) {
            char * p = *lastjmp;
            InvertJmp(p+1);         /* step 1 */
            p += 4;
            if (truelabel == NULL)  /* step 2 */
                truelabel = MakeAnonymousLabel();
            strcpy(p, truelabel);
            strcat(p, "\n");
            *lastjmp = NULL;
            nlabel = MakeAnonymousLabel();  /* step 3 */
            olabel = GetLabel(hll, ilabel);
            if (hll->cmd == HLL_REPEAT) {
                ReplaceLabel(buffer, olabel, nlabel);
                sprintf(ptr+strlen(ptr), "%s:\n", nlabel);
            } else {
                sprintf(ptr+strlen(ptr), "%s:\n", olabel);
                SetLabel(hll, ilabel, nlabel);
            }
        }
    }
    return (NOT_ERROR);
}

// update hll->condlines

static int WriteExprSrc(hll_list * hll, char * buffer)
{
    int size;
    char *p;

    size = strlen(buffer) + 1;
    if (hll->condlines) {
        size += strlen(hll->condlines) + 1;
    }
    p = AsmAlloc(size);
    if (hll->condlines) {
        strcpy(p, hll->condlines);
        strcat(p, "\n");
        strcat(p, buffer);
    } else
        strcpy(p, buffer);

    AsmFree(hll->condlines);
    hll->condlines = p;
    return(1);
}

// evaluate the C like boolean expression found in HLL structs
// like .IF, .ELSEIF, .WHILE, .UNTIL and .UNTILCXZ
// might return multiple lines (strings separated by 0x0A)
// i = index in AsmBuffer where expression starts. Is restricted
// to one source line (till T_FINAL)
// label: label to jump to if expression is <is_true>!
// is_true: TRUE/FALSE

static int EvaluateHllExpression(hll_list * hll, int *i, int ilabel, bool is_true)
{
    char *lastjmp = NULL;
    char buffer[MAX_LINE_LEN*2];

    DebugMsg(("EvaluateHllExpression enter\n"));

    buffer[0] = '\0';
    if (ERROR == GetExpression(hll, i, ilabel, is_true, buffer, &lastjmp))
        return( ERROR );
    if (buffer[0])
        WriteExprSrc(hll, buffer);
    if (hll->condlines != NULL && *hll->condlines == '\n') {
        AsmError(SYNTAX_ERROR_IN_CONTROL_FLOW_DIRECTIVE);
        return( ERROR );
    }
    return (NOT_ERROR);
}

// write ASM test lines

static int HllPushTestLines(hll_list * hll)
{
    char *p = hll->condlines;
    char *p2;
    char buffer[MAX_LINE_LEN];

    DebugMsg(("HllPushTestLines enter\n"));
    if (!p)
        return (ERROR);

    while (*p) {
        for (p2=buffer;*p && (*p != 0x0A);)
            *p2++ = *p++;
        *p2 = '\0';
        if (*p == 0x0A)
            p++;
        InputQueueLine( buffer );
    }
    AsmFree(hll->condlines);
    hll->condlines = NULL;


    return( NOT_ERROR );
}

// for .UNTILCXZ check if expression is simple enough

static int HllCheckTestLines(hll_list * hll)
{
    int lines = 0;
    int i;
    char *p = hll->condlines;

    for (;*p;p++) {
        if (*p == 0x0a) {
            lines++;
            if (*(p+1) == 'j') {
                p++;
                if (*(p+1) == 'z' || (*(p+1) == 'n' && *(p+2) == 'z'))
                    if (lines == 1) {
                        i = strlen(p);
                        while (i) {
                            *(p+3+i) = *(p+i);
                            i--;
                        }
                        memcpy(p,"loop",4);
                    }
                else
                    return( ERROR );
            }
        }
    }
    if (lines > 2)
        return( ERROR );
    return( NOT_ERROR );
}

void HllInit()
{
    HllStack = NULL;
    ModuleInfo.hll_label = 0;
    return;
}

// Start a .IF, .WHILE, .REPEAT item

int HllStartDef( int i )
/********************/
{
    struct hll_list      *hll;
    int                  cmd = AsmBuffer[i]->value;
    char                 buffer[MAX_LINE_LEN];

    DebugMsg(("HllStartDef\n"));

#if FASTPASS
    if (StoreState == FALSE && Parse_Pass == PASS_1) {
        SaveState();
    }
#endif

    switch (cmd) {
    case T_DOT_REPEAT:
        DebugMsg(("HllStartDef .REPEAT\n"));
        if (AsmBuffer[i+1]->token != T_FINAL) {
            DebugMsg(("HllStartDef: unexpected tokens behind .REPEAT\n" ));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        break;
    case T_DOT_IF:
        DebugMsg(("HllStartDef .IF\n"));
    case T_DOT_WHILE:
        DebugMsg(("HllStartDef .WHILE\n"));
#if 0
        /* Masm allows a missing expression */
        if (AsmBuffer[i+1]->token == T_FINAL) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
#endif
        break;
    }
    hll = AsmAlloc( sizeof(hll_list));

    hll->cmd = HLL_UNDEF;

    /* create labels which are always needed */
    hll->symfirst = NULL;
    hll->symexit = MakeAnonymousLabel();
    hll->symtest = MakeAnonymousLabel();

    hll->condlines = NULL;

    // structure for .IF .ELSE .ENDIF
    //    cond jump to symtest
    //    ...
    //    jmp symexit
    //  symtest:
    //    ...
    //  symexit:

    // structure for .IF .ELSEIF
    //    cond jump to symtest
    //    ...
    //    jmp symexit
    //  symtest:
    //    cond jump to (new) symtest
    //    ...
    //    jmp symexit
    //  symtest:
    //    ...

    // structure for .WHILE and .REPEAT:
    //   jmp symtest (for .WHILE only)
    // symfirst:
    //   ...
    // symtest: (jumped to by .continue)
    //   test end condition, cond jump to symfirst
    // symexit: (jumped to by .break)

    PushLineQueue();

    switch (cmd) {
    case T_DOT_IF:
        hll->cmd = HLL_IF;
        /* get the C-style expression, convert to ASM code lines */
        i++;
        if (ERROR == EvaluateHllExpression(hll, &i, LABELTEST, FALSE)) {
            return (ERROR);
        }
        HllPushTestLines(hll);
        break;
    case T_DOT_WHILE:
    case T_DOT_REPEAT:
        /* create the label to loop start */
        hll->symfirst = MakeAnonymousLabel();
        if (cmd == T_DOT_WHILE) {
            i++;
            hll->cmd = HLL_WHILE;
            if (ERROR == EvaluateHllExpression(hll, &i, LABELFIRST, TRUE)) {
                return (ERROR);
            }
            DebugMsg(("HllStartDef .WHILE (start expr=%s)\n", hll->condlines));
            /* create a jump to second label */
            /* optimisation: if second label is just a jump, dont jump! */
            if (hll->condlines && memicmp(hll->condlines, "jmp", 3)) {
                sprintf(buffer, " jmp %s", hll->symtest);
                InputQueueLine( buffer );
            }
        } else {
            i++;
            DebugMsg(("HllStartDef .REPEAT\n"));
            hll->cmd = HLL_REPEAT;
        }
        sprintf(buffer, "%s:", hll->symfirst);
        InputQueueLine( buffer );
        break;
    }
    if (AsmBuffer[i]->token != T_FINAL) {
        DebugMsg(("HllStartDef: unexpected token %u [%s]\n", AsmBuffer[i]->token, AsmBuffer[i]->pos ));
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    push(&HllStack, hll);
    return( NOT_ERROR );
}

// End a .IF, .WHILE, .REPEAT item
// that is: .ENDIF, .ENDW, .UNTIL and .UNTILCXZ are handled here

int HllEndDef( int i )
/********************/
{
    struct asm_sym      *sym;
    struct hll_list     *hll = NULL;
    int                 cmd = AsmBuffer[i]->value;
    char                buffer[MAX_LINE_LEN];
    
    if (HllStack)
        hll = pop( &HllStack );

    if (hll == NULL) {
        DebugMsg(("HllEndDef: hll stack is empty\n"));
        AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    PushLineQueue();

    switch (cmd) {
    case T_DOT_ENDIF:
        DebugMsg(("HllEndDef .ENDIF\n"));
        if (hll->cmd != HLL_IF) {
            DebugMsg(("HllEndDef no .IF on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        /* if a test label isn't created yet, create it */
        if (hll->symtest) {
            sprintf(buffer, "%s:", hll->symtest);
            InputQueueLine( buffer );
        }
        /* create the exit label */
        sprintf(buffer, "%s:", hll->symexit);
        InputQueueLine( buffer );
        i++;
        break;
    case T_DOT_ENDW:
        DebugMsg(("HllEndDef .ENDW\n"));
        if (hll->cmd != HLL_WHILE) {
            DebugMsg(("HllEndDef no .WHILE on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        /* create test label  */
        sprintf(buffer, "%s:", hll->symtest);
        DebugMsg(("HllEndDef: created: %s\n", buffer));
        InputQueueLine( buffer );

        HllPushTestLines(hll);

        sprintf(buffer, "%s:", hll->symexit);
        InputQueueLine( buffer );
        i++;
        break;
    case T_DOT_UNTILCXZ:
        DebugMsg(("HllEndDef .UNTILCXZ\n"));
        if (hll->cmd != HLL_REPEAT) {
            DebugMsg(("HllEndDef no .REPEAT on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        sprintf(buffer, "%s:", hll->symtest);
        InputQueueLine( buffer );

        i++;
        /* read in optional (simple) expression */
        if (AsmBuffer[i]->token != T_FINAL) {
            if (ERROR == EvaluateHllExpression(hll, &i, LABELFIRST, FALSE)) {
                return( ERROR );
            }
            if ( HllCheckTestLines(hll) == ERROR) {
                AsmError(EXPR_TOO_COMPLEX_FOR_UNTILCXZ);
                return( ERROR );
            }
            /* write condition lines */
            HllPushTestLines(hll);
        } else {
            sprintf(buffer, " loop %s", hll->symfirst);
            InputQueueLine( buffer );
        }
        sprintf(buffer, "%s:", hll->symexit);
        InputQueueLine( buffer );
        break;
    case T_DOT_UNTIL:
        DebugMsg(("HllEndDef .UNTIL\n"));
        if (hll->cmd != HLL_REPEAT) {
            DebugMsg(("HllEndDef no .REPEAT on the hll stack\n"));
            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        sprintf(buffer, "%s:", hll->symtest);
        InputQueueLine( buffer );

        i++;
        /* read in expression */
        if (ERROR == EvaluateHllExpression(hll, &i, LABELFIRST, FALSE)) {
            return( ERROR );
        }

        /* write condition lines */
        HllPushTestLines(hll);

#if 0
        sprintf(buffer, " jmp %s", hll->symfirst);
        InputQueueLine( buffer );
#endif

        sprintf(buffer, "%s:", hll->symexit);
        InputQueueLine( buffer );
        break;
    }

    AsmFree(hll->symfirst);
    AsmFree(hll->symtest);
    AsmFree(hll->symexit);
    AsmFree(hll->condlines);
    AsmFree(hll);

    if (AsmBuffer[i]->token != T_FINAL) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    return( NOT_ERROR );
}

// Exit current .IF, .WHILE, .REPEAT item
// that is: .ELSE, .ELSEIF, .CONTINUE and .BREAK are handled here

int HllExitDef( int i )
/********************/
{
    int                 level;
    struct asm_sym      *sym;
    struct hll_list     *hll;
    char                *savedlines;
    hll_cmd             savedcmd;
    int                 cmd = AsmBuffer[i]->value;
    char                buffer[MAX_LINE_LEN];

    hll = peek(HllStack, 0);

    if (hll == NULL) {
        DebugMsg(("HllExitDef stack error\n"));
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    PushLineQueue();

    switch (cmd) {
    case T_DOT_ELSE:
    case T_DOT_ELSEIF:
        if ((hll->cmd != HLL_IF) || (hll->symtest == NULL)) {
            DebugMsg(("HllExitDef .ELSE/.ELSEIF: cmd=%u, symtest=%X\n", hll->cmd, hll->symtest));
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        sprintf(buffer," jmp %s", hll->symexit);
        InputQueueLine( buffer );
        sprintf(buffer,"%s:", hll->symtest);
        InputQueueLine( buffer );
        AsmFree(hll->symtest);
        hll->symtest = NULL;
        i++;
        if (cmd == T_DOT_ELSEIF) {
            /* create new symtest label */
            hll->symtest = MakeAnonymousLabel();
            if (ERROR == EvaluateHllExpression(hll, &i, LABELTEST, FALSE)) {
                return( ERROR );
            }
            HllPushTestLines(hll);
        }
        break;
    case T_DOT_BREAK:
    case T_DOT_CONTINUE:
        for (level = 1;hll && (hll->cmd == HLL_IF);level++) {
            hll = peek(HllStack,level);
        }
        if (hll == NULL) {
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        /* .BREAK .IF ... or .CONTINUE .IF ? */
        i++;
        if (AsmBuffer[i]->token != T_FINAL) {
            /* .IF is only seen as a directive if it is at position 1 */
            if ((AsmBuffer[i]->token == T_DOT) && (AsmBuffer[i+1]->token == T_DIRECTIVE) && (AsmBuffer[i+1]->value == T_IF)) {
                savedlines = hll->condlines;
                savedcmd = hll->cmd;
                hll->condlines = NULL;
                hll->cmd = HLL_BREAK;
                i = i+2;
                if (cmd == T_DOT_BREAK) {
                    if (ERROR == EvaluateHllExpression(hll, &i, LABELEXIT, TRUE)) {
                        return( ERROR );
                    }
                } else { /* T_DOT_CONTINUE */
                    if (ERROR == EvaluateHllExpression(hll, &i, LABELTEST, TRUE)) {
                        return( ERROR );
                    }
                }
                HllPushTestLines(hll);
                AsmFree(hll->condlines);
                hll->condlines = savedlines;
                hll->cmd = savedcmd;
            }
        } else {
            if (cmd == T_DOT_BREAK) {
                DebugMsg(("HllExitDef .BREAK\n"));
                sprintf(buffer," jmp %s", hll->symexit);
            } else {
                DebugMsg(("HllExitDef .CONTINUE\n"));
                sprintf(buffer," jmp %s", hll->symtest);
            }
            InputQueueLine( buffer );
        }
        break;
    }
    if (AsmBuffer[i]->token != T_FINAL) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    return( NOT_ERROR );
}

