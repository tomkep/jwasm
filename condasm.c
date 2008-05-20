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
* Description:  JWasm conditional processing routines
*
****************************************************************************/


#include "asmglob.h"
#include <ctype.h>

#include "parser.h"
#include "directiv.h"
#include "expreval.h"
#include "asmdefs.h"

#include "myassert.h"

extern int              get_instruction_position( char *string );

#define    MAX_NESTING  20

/*
 the current if-block can be in one of 4 states:
  state              assembly     possible next states
 -----------------------------------------------------------------------
  condition check    on           active, inactive
  inactive           off          condition check, active
  active             on           condition check, done
  done               off          -
*/

typedef enum  {
    BLOCK_ACTIVE,    /* current cond is true */
    COND_CHECK,      /* checking current IF cond */
    BLOCK_INACTIVE,  /* current IF cond is false, looking for elseif */
    BLOCK_DONE       /* done TRUE section of current if, just nuke
                        everything until we see an endif */
} if_state;

if_state CurrIfState = BLOCK_ACTIVE;
static int  blocknestlevel = 0;
static int  falseblocknestlevel = 0;

// fixme char *IfSymbol;        /* save symbols in IFDEF's so they don't get expanded */

/*
 this code runs always. it handles the following:
 inactive -> condition check
 active -> done
*/

#define is_valid_id_char( ch ) \
    ( isalpha(ch) || isdigit(ch) || ch=='_' || ch=='@' || ch=='$' || ch=='?' )

void conditional_assembly_prepare( char *line )
/***************************************************/
{
    char        *ptr;
    char        *end;
    int         count;
    char        delim;
    char        fix;

    if( Comment( QUERY_COMMENT, 0 ) ) {
        delim = (char)Comment( QUERY_COMMENT_DELIM, 0 );
        if( strchr( line, delim ) != NULL ) {
            Comment( END_COMMENT, 0 );
        }
        *line = '\0';
        return;
    }

    ptr = line;
    while (isspace(*ptr)) ptr++;
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
    if( count == EMPTY ) {
        /* if it is not in the table */
        if(( CurrIfState == BLOCK_INACTIVE ) || ( CurrIfState == BLOCK_DONE ) ) {
            *line = '\0';
        }
        return;
    }

    /* otherwise, see if it is a conditional assembly directive */

    switch( AsmOpTable[count].token) {
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
        if(( CurrIfState == BLOCK_INACTIVE ) || ( CurrIfState == BLOCK_DONE )) {
            DebugMsg(("suppressed: >%s<\n", line));
            *line = '\0';
        }
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

static char check_blank( char *string )
/************************************/
{
    for (;*string;string++)
        if (isspace(*string) == FALSE)
            return FALSE;
    return( TRUE );
}

static char check_dif( bool sensitive, char *string, char *string2 )
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
                return(ERROR);
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
        AsmError( BLOCK_NESTING_ERROR );
        return(ERROR);
        break;
    }

    /* numeric operand needed? */
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
    }

    switch( directive ) {
    case T_IF1:
        CurrIfState = Parse_Pass == PASS_1 ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IF2:
        CurrIfState = Parse_Pass == PASS_1 ? BLOCK_INACTIVE : BLOCK_ACTIVE;
        break;
    case T_IF:
    case T_ELSEIF:
        CurrIfState = ( opndx.value ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFE:
    case T_ELSEIFE:
        CurrIfState = ( !opndx.value ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFDEF:
    case T_ELSEIFDEF:
        CurrIfState = check_defd( AsmBuffer[i+1]->string_ptr )  ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFNDEF:
    case T_ELSEIFNDEF:
        CurrIfState = !check_defd( AsmBuffer[i+1]->string_ptr )  ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFB:
    case T_ELSEIFB:
        CurrIfState = check_blank( AsmBuffer[i+1]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFNB:
    case T_ELSEIFNB:
        CurrIfState = !check_blank( AsmBuffer[i+1]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFDIF:
    case T_ELSEIFDIF:
        CurrIfState = check_dif( TRUE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFDIFI:
    case T_ELSEIFDIFI:
        CurrIfState = check_dif( FALSE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFIDN:
    case T_ELSEIFIDN:
        DebugMsg(("conditional_assembly_directive, IFIDN, cmp >%s< and >%s<\n", AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr));
        CurrIfState = !check_dif( TRUE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
        break;
    case T_IFIDNI:
    case T_ELSEIFIDNI:
        CurrIfState = !check_dif( FALSE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ? BLOCK_ACTIVE : BLOCK_INACTIVE;
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

    direct = AsmBuffer[i]->value;

    /* get an expression if necessary */
    switch( direct ) {
    case T_DOT_ERRE:
    case T_DOT_ERRNZ:
//    case T_DOT_ERRDIF:
//    case T_DOT_ERRDIFI:
//    case T_DOT_ERRIDN:
//    case T_DOT_ERRIDNI:
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
    }
    switch( direct ) {
    case T_DOT_ERR:
        if (AsmBuffer[i+1]->token == T_STRING)
            AsmErr( FORCED, AsmBuffer[i+1]->string_ptr);
        else
            AsmErr( FORCED, "");
        return( ERROR );
    case T_DOT_ERRNZ:
        if( opndx.value ) {
            if (AsmBuffer[i]->token == T_COMMA && AsmBuffer[i+1]->token == T_STRING)
                AsmErr( FORCED, AsmBuffer[i+1]->string_ptr );
            else
                AsmErr( FORCED_NOT_ZERO, opndx.value );
            return( ERROR );
        }
        break;
    case T_DOT_ERRE:
        if( !opndx.value ) {
            if (AsmBuffer[i]->token == T_COMMA && AsmBuffer[i+1]->token == T_STRING)
                AsmErr( FORCED, AsmBuffer[i+1]->string_ptr );
            else
                AsmErr( FORCED_EQUAL, opndx.value );
            return( ERROR );
        }
        break;
    case T_DOT_ERRDEF:
        if( check_defd( AsmBuffer[i+1]->string_ptr ) ) {
            AsmErr( FORCED_DEF, AsmBuffer[i+1]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRNDEF:
        if( !check_defd( AsmBuffer[i+1]->string_ptr ) ) {
            AsmErr( FORCED_NOT_DEF, AsmBuffer[i+1]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRB:
        if( AsmBuffer[i+1]->token == T_STRING && check_blank( AsmBuffer[i+1]->string_ptr ) ) {
            if (AsmBuffer[i+2]->token == T_COMMA && AsmBuffer[i+3]->token == T_STRING)
                AsmErr( FORCED, AsmBuffer[i+3]->string_ptr );
            else
                AsmErr( FORCED_BLANK, AsmBuffer[i+1]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRNB:
        if( AsmBuffer[i+1]->token != T_STRING || !check_blank( AsmBuffer[i+1]->string_ptr ) ) {
            if (AsmBuffer[i+2]->token == T_COMMA && AsmBuffer[i+3]->token == T_STRING)
                AsmErr( FORCED, AsmBuffer[i+3]->string_ptr );
            else
                AsmErr( FORCED_NOT_BLANK, AsmBuffer[i+1]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRDIF:
        if( check_dif( TRUE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ) {
            AsmErr( FORCED_DIF, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRDIFI:
        if( check_dif( FALSE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ) {
            AsmErr( FORCED_DIF, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRIDN:
        if( !check_dif( TRUE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ) {
            AsmErr( FORCED_IDN, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr );
            return( ERROR );
        }
        break;
    case T_DOT_ERRIDNI:
        if( !check_dif( FALSE, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr ) ) {
            AsmErr( FORCED_IDN, AsmBuffer[i+1]->string_ptr, AsmBuffer[i+3]->string_ptr );
            return( ERROR );
        }
        break;
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
