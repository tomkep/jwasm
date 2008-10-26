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
* Description:  JWasm conditional processing routines. Handles directives
*               IF[E], ELSE, ENDIF
*               [ELSE]IFB, [ELSE]IFNB
*               [ELSE]IFDEF, [ELSE]IFNDEF
*               [ELSE]IFDIF[I], [ELSE]IFIDN[I]
*               IF1, IF2
*               .ERR, .ERRNZ, .ERRE
*               .ERRB, .ERRNB
*               .ERRDEF, .ERRNDEF
*               .ERRDIF[I], .ERRIDN[I]
*               .ERR1, .ERR2
*               COMMENT
****************************************************************************/


#include <ctype.h>

#include "globals.h"
#include "parser.h"
#include "expreval.h"
#include "myassert.h"
#include "directiv.h"

extern int              get_instruction_position( char *string );
extern bool SkipMacroMode;

#define    MAX_NESTING  20

/*
 the current if-block can be in one of 4 states:
  state              assembly     possible next states
 -----------------------------------------------------------------------
  condition check    on           active, inactive
  inactive           off          condition check, active
  active             on           done
  done               off          -
*/

typedef enum  {
    BLOCK_ACTIVE,    /* current cond is true */
    COND_CHECK,      /* checking current IF cond */
    BLOCK_INACTIVE,  /* current IF cond is false, looking for elseif */
    BLOCK_DONE       /* done TRUE section of current if, just nuke
                        everything until we see an endif */
} if_state;

if_state CurrIfState;
static int blocknestlevel;
static int falseblocknestlevel;
bool inside_comment;
static char delim_char;

// fixme char *IfSymbol;        /* save symbols in IFDEF's so they don't get expanded */

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

static int StartComment( char * p )
/***********************************/
{
    while (isspace(*p)) p++;
    if (*p == NULLC) {
        AsmError( COMMENT_DELIMITER_EXPECTED );
        return( ERROR );
    }
    delim_char = *p;
    p++;
#if 0
    if (*p != NULLC && isspace(*p) == FALSE) {
        AsmError( COMMENT_DELIMITER_EXPECTED );
        return( ERROR );
    }
#endif
    for (;*p;p++)
        if (*p == delim_char)
            return( NOT_ERROR );

    inside_comment = TRUE;
    return( NOT_ERROR );
}

/*
 this code runs always. it handles the following:
 COMMENT directive
 inactive -> condition check
 active -> done
*/

void conditional_assembly_prepare( char *line )
/***************************************************/
{
    char        *ptr;
    char        *end;
    char        fix;
    int         count;

    if( inside_comment == TRUE ) {
        DebugMsg(("COMMENT active, delim is >%c<, line is >%s<\n", delim_char, line));
        if( strchr( line, delim_char ) != NULL ) {
            DebugMsg(("COMMENT mode exited\n"));
            inside_comment = FALSE;
        }
        *line = '\0';
        return;
    }

    ptr = line;
    while (isspace(*ptr)) ptr++;
    /* the expansion operator is still present here, since
     it could have been used as a comment delimiter */
    if (*ptr == '%') {
        ptr++;
        while (isspace(*ptr)) ptr++;
    }

    if( *ptr == '\0' )
        return;
    end = ptr;
    while ( is_valid_id_char( *end )) ++end;
    if (*end == ':') {
        end++;
        while (isspace(*end)) end++;
        ptr = end;
        while ( is_valid_id_char( *end )) ++end;
    }
    fix = *end;
    *end = '\0';

//    DebugMsg(("conditional_assembly_prepare IFx (%s)\n", ptr));

    count = get_instruction_position( ptr );
    *end = fix;
    if( count == EMPTY ||
        AsmOpTable[count].opnd_type[0] != OP_SPECIAL ||
        AsmOpTable[count].rm_byte != OP_DIRECTIVE ||
        (AsmOpTable[count].opcode & 2) == 0) {
        /* it's not a conditional directive or COMMENT */
        if(( CurrIfState == BLOCK_INACTIVE ) || ( CurrIfState == BLOCK_DONE ) ) {
            DebugMsg(("suppressed: >%s<\n", line));
            *line = '\0';
        }
        return;
    }

    switch( AsmOpTable[count].token) {
    case T_COMMENT:
        StartComment( end );
        DebugMsg(("COMMENT starting, delim is >%c<\n", delim_char));
        *line = '\0';
        return;
    case T_IF:
    case T_IF1:
    case T_IF2:
    case T_IFB:
    case T_IFDEF:
    case T_IFDIF:
    case T_IFDIFI:
    case T_IFE:
    case T_IFIDN:
    case T_IFIDNI:
    case T_IFNB:
    case T_IFNDEF:
        /* this must be done HERE to avoid expansion if the current
         state is INACTIVE.
         */
        if( CurrIfState == BLOCK_INACTIVE || CurrIfState == BLOCK_DONE ) {
            falseblocknestlevel++;
            *line = '\0';
        } else {
            CurrIfState = COND_CHECK;
        }
#ifdef DEBUG_OUT
        {
            char * ptr2;
        switch (CurrIfState) {
        case BLOCK_ACTIVE:
            ptr2 = "BLOCK_ACTIVE";
            break;
        case COND_CHECK:
            ptr2 = "COND_CHECK";
            break;
        case BLOCK_INACTIVE:
            ptr2 = "BLOCK_INACTIVE";
            break;
        case BLOCK_DONE:
            ptr2 = "BLOCK_DONE";
            break;
        }
        DebugMsg(("conditional_assembly_prepare IFx (%s): state=%s, level=%u, falselevel=%u\n", ptr, ptr2, blocknestlevel, falseblocknestlevel));
        }
#endif
        break;
    case T_ELSEIF:
    case T_ELSEIF1:
    case T_ELSEIF2:
    case T_ELSEIFB:
    case T_ELSEIFDEF:
    case T_ELSEIFDIF:
    case T_ELSEIFDIFI:
    case T_ELSEIFE:
    case T_ELSEIFIDN:
    case T_ELSEIFIDNI:
    case T_ELSEIFNB:
    case T_ELSEIFNDEF:
    case T_ELSE:
        if (falseblocknestlevel > 0)
            *line = '\0';
        else if (CurrIfState == BLOCK_INACTIVE || CurrIfState == COND_CHECK)
            /* the block might become active */
            /* expansion must "run" */
            CurrIfState = COND_CHECK;
        else {
            /* current state is ACTIVE or DONE */
            /* in any case this block is DONE then */
            /* don't run expansion! */
            CurrIfState = BLOCK_DONE;
            *line = '\0';
        }
#ifdef DEBUG_OUT
        {
            char * ptr2;
        switch (CurrIfState) {
        case BLOCK_ACTIVE:
            ptr2 = "BLOCK_ACTIVE";
            break;
        case COND_CHECK:
            ptr2 = "COND_CHECK";
            break;
        case BLOCK_INACTIVE:
            ptr2 = "BLOCK_INACTIVE";
            break;
        case BLOCK_DONE:
            ptr2 = "BLOCK_DONE";
            break;
        }
        DebugMsg(("conditional_assembly_prepare ELSEx (%s): state=%s, level=%u, falselevel=%u\n", ptr, ptr2, blocknestlevel, falseblocknestlevel));
        }
#endif
        break;
    case T_ENDIF:
        if (falseblocknestlevel > 0) {
            falseblocknestlevel--;
            *line = '\0';
        }
        DebugMsg(("conditional_assembly_prepare ENDIF: state=%u, level=%u, falselevel=%u\n", CurrIfState, blocknestlevel, falseblocknestlevel));
        break;
    default:
        *end = '\0';
        DebugMsg(("conditional_assembly_prepare: unknown directive %s\n", ptr));
        AsmErr( UNKNOWN_DIRECTIVE, ptr);
    }
    return;
}

static bool check_defd( char *string )
/************************************/
{
    char                *ptr;
    char                *end;
    struct asm_sym      *sym;

    /* isolate 1st word */
    ptr = string + strspn( string, " \t" );
    end = ptr + strcspn( ptr, " \t" );
    *end = '\0';

    /* there might be no argument at all, which is valid syntax
      and should return FALSE */
    if (*ptr == '\0')
        return(FALSE);

    sym = SymSearch( ptr );
    if( sym != NULL ) {
        DebugMsg(("check_defd: sym->state=%u\n", sym->state));
        if (sym->state == SYM_INTERNAL || sym->state == SYM_MACRO || sym->state == SYM_TMACRO) {
            DebugMsg(("check_defd: sym->defined=%u\n", sym->defined));
            return(sym->defined);
        } else
            return( TRUE );
    } else {
        return( FALSE );
    }
}

static bool check_blank( char *string )
/************************************/
{
    for (;*string;string++)
        if (isspace(*string) == FALSE)
            return FALSE;
    return( TRUE );
}

static bool check_dif( bool sensitive, char *string, char *string2 )
/******************************************************************/
{
    if( sensitive ) {
        return( strcmp( string, string2 ) != 0 );
    } else {
        return( stricmp( string, string2 ) != 0 );
    }
}

int conditional_assembly_directive( int i, int directive )
/*****************************************/
{
    expr_list opndx;

    switch( CurrIfState ) {
    case COND_CHECK:
        DebugMsg(("conditional_assembly_directive, COND_CHECK, CurrIfState=%u, level=%u, falselevel=%u\n", CurrIfState, blocknestlevel, falseblocknestlevel));
        switch (directive) {
        case T_IF:
        case T_IF1:
        case T_IF2:
        case T_IFB:
        case T_IFDEF:
        case T_IFDIF:
        case T_IFDIFI:
        case T_IFE:
        case T_IFIDN:
        case T_IFIDNI:
        case T_IFNB:
        case T_IFNDEF:
            CurrIfState = BLOCK_ACTIVE;
            blocknestlevel++;
            if( blocknestlevel > MAX_NESTING ) {
                blocknestlevel -= 1;
                AsmError( NESTING_LEVEL_TOO_DEEP );
                return( ERROR );
            }
            if (SkipMacroMode == TRUE) {
                /* avoid expression evaluation if in "skipmacro" mode */
                CurrIfState = BLOCK_DONE;
                return( NOT_ERROR );
            }
        }
        break;
    default:
        /* must be ENDIF */
        if (directive == T_ENDIF && blocknestlevel > 0) {
            blocknestlevel--;
            CurrIfState = BLOCK_ACTIVE;
            DebugMsg(("conditional_assembly_directive, ENDIF, CurrIfState=ACTIVE, level=%u\n", blocknestlevel));
            return(NOT_ERROR);
        }
        DebugMsg(("conditional_assembly_directive, unexpected directive=%u\n", directive));
        AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i]->string_ptr );
        return(ERROR);
        break;
    }

    /* check params and call appropriate test routine */

    switch( directive ) {
    case T_IF:
    case T_IFE:
    case T_ELSEIF:
    case T_ELSEIFE:
        i++;
        if ((ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE )))
            return(ERROR);
        if (opndx.type == EXPR_CONST && opndx.string == NULL)
            ;
        else if (opndx.type == EXPR_ADDR && opndx.indirect == FALSE)
            opndx.value += opndx.sym->offset;
        else {
            AsmError(CONSTANT_EXPECTED);
            return(ERROR);
        }
        if (directive == T_IF ||
            directive == T_ELSEIF)
            CurrIfState = ( opndx.value ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        else
            CurrIfState = ( !opndx.value ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFDIF:
    case T_IFDIFI:
    case T_ELSEIFDIF:
    case T_ELSEIFDIFI:
    case T_IFIDN:
    case T_IFIDNI:
    case T_ELSEIFIDN:
    case T_ELSEIFIDNI:
        if (Token_Count != i+4 || AsmBuffer[i+2]->token != T_COMMA) {
            AsmError(SYNTAX_ERROR);
            return(ERROR);
        }
        i++;
        if (AsmBuffer[i]->token != T_STRING || AsmBuffer[i+2]->token != T_STRING) {
            /* to return a more comprehensive error message, check if
             undefined symbols were used as params */
            if (AsmBuffer[i]->token == T_ID) {
                if (SymSearch(AsmBuffer[i]->string_ptr) == NULL) {
                    AsmErr(SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr);
                    return(ERROR);
                }
            }
            if (AsmBuffer[i+2]->token == T_ID) {
                if (SymSearch(AsmBuffer[i+2]->string_ptr) == NULL) {
                    AsmErr(SYMBOL_NOT_DEFINED, AsmBuffer[i+2]->string_ptr);
                    return(ERROR);
                }
            }
            AsmError(TEXT_ITEM_REQUIRED);
            return(ERROR);
        }
        if (directive == T_IFDIF ||
            directive == T_ELSEIFDIF) {
            DebugMsg(("conditional_assembly_directive, IFDIF, cmp >%s< and >%s<\n", AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr));
            CurrIfState = check_dif( TRUE, AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        } else if (directive == T_IFDIFI ||
                   directive == T_ELSEIFDIFI) {
            DebugMsg(("conditional_assembly_directive, IFDIFI, cmp >%s< and >%s<\n", AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr));
            CurrIfState = check_dif( FALSE, AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        } else if (directive == T_IFIDN ||
                   directive == T_ELSEIFIDN) {
            DebugMsg(("conditional_assembly_directive, IFIDN, cmp >%s< and >%s<\n", AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr));
            CurrIfState = !check_dif( TRUE, AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        } else {
            DebugMsg(("conditional_assembly_directive, IFIDNI, cmp >%s< and >%s<\n", AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr));
            CurrIfState = !check_dif( FALSE, AsmBuffer[i]->string_ptr, AsmBuffer[i+2]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        }
        break;
    case T_IFB:
    case T_IFNB:
    case T_ELSEIFB:
    case T_ELSEIFNB:
        if (Token_Count != i+2) {
            AsmError(SYNTAX_ERROR);
            return(ERROR);
        }
        i++;
        if (AsmBuffer[i]->token != T_STRING) {
            /* to return a more comprehensive error message, check if
             undefined symbols were used as params */
            if (AsmBuffer[i]->token == T_ID) {
                if (SymSearch(AsmBuffer[i]->string_ptr) == NULL) {
                    AsmErr(SYMBOL_NOT_DEFINED, AsmBuffer[i]->string_ptr);
                    return(ERROR);
                }
            }
            AsmError(TEXT_ITEM_REQUIRED);
            return(ERROR);
        }
        if (directive == T_IFB ||
            directive == T_ELSEIFB) {
            CurrIfState = check_blank( AsmBuffer[i]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        } else {
            CurrIfState = !check_blank( AsmBuffer[i]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        }
        break;
    case T_IF1:
    case T_ELSEIF1:
        CurrIfState = Parse_Pass == PASS_1 ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IF2:
    case T_ELSEIF2:
        if (ModuleInfo.setif2 == FALSE) {
            AsmError(IF2_NOT_ALLOWED);
        }
        CurrIfState = Parse_Pass == PASS_1 ? BLOCK_INACTIVE : BLOCK_ACTIVE;
        break;
    case T_IFDEF:
    case T_ELSEIFDEF:
        CurrIfState = check_defd( AsmBuffer[i+1]->string_ptr )  ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFNDEF:
    case T_ELSEIFNDEF:
        CurrIfState = !check_defd( AsmBuffer[i+1]->string_ptr )  ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_ELSE:
        CurrIfState = BLOCK_ACTIVE;
        break;
    }

#ifdef DEBUG_OUT
    {
    char * ptr;
    switch (CurrIfState) {
    case COND_CHECK:
        ptr = "COND_CHECK";
        break;
    case BLOCK_INACTIVE:
        ptr = "BLOCK_INACTIVE";
        break;
    case BLOCK_ACTIVE:
        ptr = "BLOCK_ACTIVE";
        break;
    case BLOCK_DONE:
        ptr = "BLOCK_DONE";
        break;
    }
    DebugMsg(("conditional_assembly_directive exit, CurrIfState=%s, level=%u, falselevel=%u\n", ptr, blocknestlevel, falseblocknestlevel));
    }
#endif
    return( NOT_ERROR );
}

int conditional_error_directive( int i )
/**************************************/
{
    expr_list opndx;
    uint direct;
    bool forced_error = FALSE;
    char * p = "";

    direct = AsmBuffer[i]->value;
    i++;

    /* get an expression if necessary */
    switch( direct ) {
    case T_DOT_ERR2:
        if (ModuleInfo.setif2 == FALSE) {
            AsmError(IF2_NOT_ALLOWED);
            return( ERROR );
        }
    case T_DOT_ERR1:
    case T_DOT_ERR:
        if (ModuleInfo.setif2 == TRUE) {
            if (direct == T_DOT_ERR1 && Parse_Pass != PASS_1)
                return(NOT_ERROR);
            else if (direct == T_DOT_ERR2 && Parse_Pass == PASS_1)
                return(NOT_ERROR);
        }
        if (AsmBuffer[i]->token == T_STRING) {
            p = AsmBuffer[i]->string_ptr;
            i++;
        }
        if (AsmBuffer[i]->token != T_FINAL)
            break;
        if (*p)
            AsmErr( FORCED_ARBITRARY, p);
        else
            AsmErr( FORCED, p);
        break;
    case T_DOT_ERRE:
    case T_DOT_ERRNZ:
        if ((ERROR == EvalOperand( &i, Token_Count, &opndx, TRUE )))
            return(ERROR);
        if (opndx.type == EXPR_CONST && opndx.string == NULL)
            ;
        else if (opndx.type == EXPR_ADDR && opndx.indirect == FALSE)
            opndx.value += opndx.sym->offset;
        else {
            AsmError(CONSTANT_EXPECTED);
            return(ERROR);
        }
        if (AsmBuffer[i]->token == T_COMMA && AsmBuffer[i+1]->token == T_STRING) {
            i++;
            p = AsmBuffer[i]->string_ptr;
            i++;
        }
        if (AsmBuffer[i]->token != T_FINAL)
            break;
        if (direct == T_DOT_ERRNZ && opndx.value )
            forced_error = TRUE;
        else if (direct == T_DOT_ERRE && !opndx.value)
            forced_error = TRUE;

        if (forced_error == TRUE)
            if (*p)
                AsmErr( FORCED_ARBITRARY, p );
            else if (direct == T_DOT_ERRNZ)
                AsmErr( FORCED_EQUAL, opndx.value );
            else
                AsmErr( FORCED_NOT_ZERO, opndx.value );
        break;
    case T_DOT_ERRDEF:
    case T_DOT_ERRNDEF:

        if (Token_Count < i+1) {
            AsmError(SYNTAX_ERROR);
            return(ERROR);
        }
        if (AsmBuffer[i]->token == T_ID) {
            asm_sym * sym;
            char * p1 = AsmBuffer[i]->string_ptr;
            i++;
            if (AsmBuffer[i]->token == T_COMMA && AsmBuffer[i+1]->token == T_STRING) {
                i++;
                p = AsmBuffer[i]->string_ptr; /* p is ignored */
                i++;
            }
            if (AsmBuffer[i]->token != T_FINAL)
                break;

            /* should run on pass 2 only! */
            if (Parse_Pass == PASS_1)
                break;

            /* don't use check_defd()! */
            sym = SymSearch( p1 );
            if (sym && sym->state == SYM_UNDEFINED)
                sym = NULL;

            if( direct == T_DOT_ERRDEF && sym != NULL )
                AsmErr( FORCED_DEF, p1 );
            else if( direct == T_DOT_ERRNDEF && sym == NULL )
                AsmErr( FORCED_NOT_DEF, p1 );
        }
        break;
    case T_DOT_ERRB:
    case T_DOT_ERRNB:
        if (Token_Count < i+1) {
            AsmError(SYNTAX_ERROR);
            return(ERROR);
        }
        if( AsmBuffer[i]->token != T_FINAL) {
            char * p1 = AsmBuffer[i]->string_ptr;
            i++;
            if (AsmBuffer[i]->token == T_COMMA && AsmBuffer[i+1]->token == T_STRING) {
                i++;
                p = AsmBuffer[i]->string_ptr;
                i++;
            }
            if (AsmBuffer[i]->token != T_FINAL)
                break;
            if (direct == T_DOT_ERRB && check_blank( p1 ))
                AsmErr( FORCED_BLANK, p );
            else if (direct == T_DOT_ERRNB && !check_blank( p1 ))
                AsmErr( FORCED_NOT_BLANK, p );
        }
        break;
    case T_DOT_ERRDIF:
    case T_DOT_ERRDIFI:
    case T_DOT_ERRIDN:
    case T_DOT_ERRIDNI:
        if (Token_Count < i+3) {
            AsmError(SYNTAX_ERROR);
            return(ERROR);
        }
        if (AsmBuffer[i]->token == T_STRING) {
            char * p1 = AsmBuffer[i]->string_ptr;
            i++;
            if (AsmBuffer[i]->token == T_COMMA) {
                i++;
                if (AsmBuffer[i]->token == T_STRING) {
                    char * p2 = AsmBuffer[i]->string_ptr;
                    i++;
                    if (AsmBuffer[i]->token == T_COMMA && AsmBuffer[i+1]->token == T_STRING) {
                        i++;
                        p = AsmBuffer[i]->string_ptr;
                        i++;
                    }
                    if (AsmBuffer[i]->token != T_FINAL)
                        break;
                    switch (direct) {
                    case T_DOT_ERRDIF:
                        forced_error = check_dif( TRUE, p1, p2 );
                        break;
                    case T_DOT_ERRDIFI:
                        forced_error = check_dif( FALSE, p1, p2 );
                        break;
                    case T_DOT_ERRIDN:
                        forced_error = !check_dif( TRUE, p1, p2 );
                        break;
                    default:
                        forced_error = !check_dif( FALSE, p1, p2 );
                    }
                    if (forced_error == TRUE) {
                        if (*p == NULLC) {
                            if (direct == T_DOT_ERRDIF || direct == T_DOT_ERRDIFI)
                                AsmErr( FORCED_DIF, p1, p2 );
                            else
                                AsmErr( FORCED_IDN, p1, p2 );
                        } else
                            AsmErr( FORCED_ARBITRARY, p);
                    }
                }
            }
        }
        break;
    }
    if (AsmBuffer[i]->token != T_FINAL) {
        AsmError(SYNTAX_ERROR);
        return(ERROR);
    }
    return( NOT_ERROR );
}

int CheckForOpenConditionals( void )
{
    if( blocknestlevel > 0 ) {
        AsmErr( UNCLOSED_CONDITIONALS, blocknestlevel );
        return(ERROR);
    }
    return(NOT_ERROR);
}

// init (called once per module)

void CondInit()
{
    CurrIfState = BLOCK_ACTIVE;
    blocknestlevel = 0;
    falseblocknestlevel = 0;
    inside_comment = FALSE;
}
