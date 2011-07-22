/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  string macro processing routines.
*
* functions:
* - CatStrDef    handle CATSTR   directive ( also TEXTEQU )
* - SetTextMacro handle EQU      directive if expression is text
* - SubStrDef    handle SUBSTR   directive
* - SizeStrDef   handle SIZESTR  directive
* - InStrDef     handle INSTR    directive
* - CatStrFunc   handle @CatStr  function
* - SubStrFunc   handle @SubStr  function
* - SizeStrFunc  handle @SizeStr function
* - InStrFunc    handle @InStr   function
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "expreval.h"
#include "equate.h"
#include "input.h"
#include "tokenize.h"
#include "fatal.h"
#include "macro.h"
#include "condasm.h"
#include "fastpass.h"
#include "listing.h"

#ifdef DEBUG_OUT
static uint_32 catstrcnt;
static uint_32 substrcnt;
static uint_32 sizstrcnt;
static uint_32 instrcnt;
static uint_32 equcnt;
#endif

extern struct asm_tok   *tokenarray;  /* start token buffer */

/* generic parameter names. In case the parameter name is
 * displayed in an error message ("required parameter %s missing")
 * v2.05: obsolete
 */
//static const char * parmnames[] = {"p1","p2","p3"};

void TextItemError( struct asm_tok *item )
/****************************************/
{
    if ( item->token == T_STRING && *item->string_ptr == '<' ) {
        AsmError( MISSING_ANGLE_BRACKET_OR_BRACE_IN_LITERAL );
        return;
    }
    /* v2.05: better error msg if (text) symbol isn't defined */
    if ( item->token == T_ID ) {
        struct asym *sym = SymSearch( item->string_ptr );
        if ( sym == NULL || sym->state == SYM_UNDEFINED ) {
            AsmErr( SYMBOL_NOT_DEFINED, item->string_ptr );
            return;
        }
    }
    AsmError( TEXT_ITEM_REQUIRED );
    return;
}

/* CATSTR directive.
 * defines a text equate
 * syntax <name> CATSTR [<string>,...]
 * TEXTEQU is an alias for CATSTR
 */

ret_code CatStrDir( int i, struct asm_tok tokenarray[] )
/******************************************************/
{
    struct asym *sym;
    int count;
    /* struct expr opndx; */
    char buffer[MAX_LINE_LEN];

#ifdef DEBUG_OUT
    catstrcnt++;
#endif
    DebugMsg1(("CatStrDir(%u) enter\n", i ));

#if 0 /* can't happen */
    /* syntax must be <id> CATSTR textitem[,textitem,...] */
    if ( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    if ( tokenarray[0].token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif
    i++; /* go past CATSTR/TEXTEQU */

    buffer[0] = NULLC;
    for ( count = 0; i < Token_Count; ) {
        DebugMsg1(("CatStrDir(%s): item=%s\n", tokenarray[0].string_ptr, tokenarray[i].string_ptr));
        if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
            DebugMsg(("CatStrDir: bad item\n"));
            TextItemError( &tokenarray[i] );
            return( ERROR );
        }
        if ( ( count + tokenarray[i].stringlen ) >= MAX_LINE_LEN ) {
            AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
            return( ERROR );
        }
        strcpy( buffer + count, tokenarray[i].string_ptr );
        count = count + tokenarray[i].stringlen;
        i++;
        if ( ( tokenarray[i].token != T_COMMA ) &&
            ( tokenarray[i].token != T_FINAL ) ) {
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        i++;
    }

    sym = SymSearch( tokenarray[0].string_ptr );
    if ( sym == NULL ) {
        sym = SymCreate( tokenarray[0].string_ptr, TRUE );
        DebugMsg1(( "CatStrDir: new symbol %s created\n", sym->name));
    } else if( sym->state == SYM_UNDEFINED ) {
        /* v2.01: symbol has been used already. Using
         * a textmacro before it has been defined is
         * somewhat problematic.
         */
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
#if FASTPASS
        SkipSavedState(); /* further passes must be FULL! */
#endif
        AsmWarn( 2, TEXT_MACRO_USED_PRIOR_TO_DEFINITION, sym->name );
    } else if( sym->state != SYM_TMACRO ) {
        /* it is defined as something else, get out */
        DebugMsg(( "CatStrDir(%s) exit, symbol redefinition\n", sym->name));
        AsmErr( SYMBOL_REDEFINITION, sym->name );
        return( ERROR );
    }


    if ( sym->string_ptr )
        AsmFree( sym->string_ptr );

    sym->state = SYM_TMACRO;
    sym->isdefined = TRUE;
    sym->string_ptr = (char *)AsmAlloc( count + 1 );
    memcpy( sym->string_ptr, buffer, count + 1 );
    DebugMsg1(("CatStrDir(%s) (new) value: >%s<\n", sym->name, buffer));

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_EQUATE, 0, sym );

    return( NOT_ERROR );
}

/*
 * used by EQU if the value to be assigned to a symbol is text.
 */
struct asym *SetTextMacro( struct asm_tok tokenarray[], struct asym *sym, const char *name, const char *value )
/*************************************************************************************************************/
{
    int count;
    char buffer[MAX_LINE_LEN];

#ifdef DEBUG_OUT
    equcnt++;
#endif
#if 0 /* FASTPASS */
    /* there's no need to set the value if FASTPASS is on, because
     * the input are just preprocessed lines.
     * this check is probably obsolete by now, since it won't be called
     * ever if pass > 1. Also, even with fastpass it's sometimes necessary
     * to do a full second pass, including source preprocessing.
     */
    if ( Parse_Pass != PASS_1 )
        return( sym );
#endif

    if ( sym == NULL )
        sym = SymCreate( name, TRUE );
    else if ( sym->state == SYM_UNDEFINED ) {
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
#if FASTPASS
        /* the text macro was referenced before being defined.
         * this is valid usage, but it requires a full second pass.
         * just simply deactivate the fastpass feature for this module!
         */
        SkipSavedState();
#endif
        AsmWarn( 2, TEXT_MACRO_USED_PRIOR_TO_DEFINITION, sym->name );
    } else if ( sym->state != SYM_TMACRO ) {
        AsmErr( SYMBOL_REDEFINITION, name );
        return( NULL );
    }

    sym->state = SYM_TMACRO;
    sym->isdefined = TRUE;

    if ( tokenarray[2].token == T_STRING && tokenarray[2].string_delim == '<' && tokenarray[3].token == T_FINAL ) {
        value = tokenarray[2].string_ptr;
        count = tokenarray[2].stringlen;
    } else {
        char *p = buffer;
        /*
         the original source is used, since the tokenizer has
         deleted some information. it's important to double '!' found inside
         the string.
         */
        while ( isspace( *value ) ) value++;
        count = strlen( value );
        if ( count ) {
            for ( ; count; count-- )
                if ( isspace( *( value + count - 1 ) ) == FALSE)
                    break;
        }
        for ( ; count; count-- ) {
            if ( *value == '!' || *value == '<' || *value == '>' )
                *p++ = '!';
            *p++ = *value++;
        }
        *p = NULLC;
        value = buffer;
        count = p - buffer;
    }
    if ( sym->string_ptr ) {
        if ( count > strlen( sym->string_ptr ) ) {
            AsmFree( sym->string_ptr );
            sym->string_ptr = (char *)AsmAlloc( count + 1 );
        }
    } else
        sym->string_ptr = (char *)AsmAlloc( count + 1 );
    memcpy( sym->string_ptr, value, count );
    *(sym->string_ptr+count) = NULLC;

    DebugMsg1(( "SetTextMacro(%s): value is >%s<, exit\n", sym->name, sym->string_ptr ));
    return( sym );
}

/* SubStr()
 * defines a text equate.
 * syntax: name SUBSTR <string>, pos [, size]
 */
ret_code SubStrDir( int i, struct asm_tok tokenarray[] )
/******************************************************/
{
    struct asym         *sym;
    char                *name;
    char                *p;
    char                *newvalue;
    int                 pos;
    int                 size;
    int                 cnt;
    bool                chksize;
    /* char                buffer[MAX_LINE_LEN]; */
    struct expr         opndx;

    DebugMsg1(("SubStrDir entry\n"));
#ifdef DEBUG_OUT
    substrcnt++;
#endif

    /* at least 5 items are needed
     * 0  1      2      3 4    5   6
     * ID SUBSTR SRC_ID , POS [, LENGTH]
     */
#if 0 /* can't happen */
    if ( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
    if ( tokenarray[0].token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif
    name = tokenarray[0].string_ptr;

    i++; /* go past SUBSTR */

    /* third item must be a string */

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        DebugMsg(("SubStrDir: error, no text item\n"));
        TextItemError( &tokenarray[i] );
        return( ERROR );
    }
    p = tokenarray[i].string_ptr;
    i++;
    DebugMsg1(("SubStrDir(%s): src=>%s<\n", name, p));

    if ( tokenarray[i].token != T_COMMA ) {
        AsmError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;

    /* get pos, must be a numeric value and > 0 */

    if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR ) {
        DebugMsg(("SubStrDir(%s): invalid pos value\n", name));
        return( ERROR );
    }

    /* v2.04: "string" constant allowed as second argument */
    //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
    if ( opndx.kind != EXPR_CONST ) {
        DebugMsg(("SubStrDir(%s): pos value is not a constant\n", name));
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }

    /* pos is expected to be 1-based */
    pos = opndx.value;
    if ( pos <= 0 ) {
        AsmError( POSITIVE_VALUE_EXPECTED );
        return( ERROR );
    }
    if ( tokenarray[i].token != T_FINAL ) {
        if ( tokenarray[i].token != T_COMMA ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
        /* get size, must be a constant */
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR ) {
            DebugMsg(("SubStrDir(%s): invalid size value\n", name));
            return( ERROR );
        }
        /* v2.04: string constant ok */
        //if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
        if ( opndx.kind != EXPR_CONST ) {
            DebugMsg(("SubStrDir(%s): size value is not a constant\n", name));
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        size = opndx.value;
        if ( tokenarray[i].token != T_FINAL ) {
            DebugMsg(("SubStrDir(%s): additional items found\n", name));
            AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
            return( ERROR );
        }
        if ( size < 0 ) {
            AsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
        chksize = TRUE;
    } else {
        size = -1;
        chksize = FALSE;
    }

    cnt = pos;
    /* position p to start of substring */
    for ( pos--; pos > 0 && *p ; pos--, p++ )
        if ( *p == '!' && *(p+1) != NULLC )
            p++;

    if ( *p == NULLC ) {
        AsmErr( INDEX_PAST_END_OF_STRING, cnt );
        return( ERROR );
    }

    if ( *p == '!' && *(p+1) != NULLC )
        p++;

    for ( newvalue = p, cnt = size; *p && cnt; cnt--, p++ )
        if (*p == '!' && *(p+1) != NULLC)
            p++;

    /* v2.04: check added */
    if ( chksize && cnt ) {
        AsmError( COUNT_VALUE_TOO_LARGE );
        return( ERROR );
    }

    sym = SymSearch( name );

    /* if we've never seen it before, put it in */
    if( sym == NULL ) {
        sym = SymCreate( name, TRUE );
    } else if( sym->state == SYM_UNDEFINED ) {
        /* it was referenced before being defined. This is
         * a bad idea for preprocessor text items, because it
         * will require a full second pass!
         */
        sym_remove_table( &SymTables[TAB_UNDEF], (struct dsym *)sym );
#if FASTPASS
        SkipSavedState();
        AsmWarn( 2, TEXT_MACRO_USED_PRIOR_TO_DEFINITION, sym->name );
#endif
    } else if( sym->state != SYM_TMACRO ) {
        /* it is defined as something incompatible, get out */
        DebugMsg(( "SubStrDir(%s) error, incompatible type\n", sym->name));
        AsmErr( SYMBOL_REDEFINITION, sym->name );
        return( ERROR );
    }

    sym->state = SYM_TMACRO;
    sym->isdefined = TRUE;

    size = p - newvalue;
    p = newvalue;

    newvalue = AsmAlloc ( size + 1 );
    memcpy( newvalue, p, size );
    *(newvalue+size) = NULLC;
    DebugMsg1(("SubStrDir(%s): result=>%s<\n", sym->name, newvalue));
    AsmFree( sym->string_ptr );
    sym->string_ptr = newvalue;

    LstWrite( LSTTYPE_EQUATE, 0, sym );

    return( NOT_ERROR );
}

/* SizeStr()
 * defines a numeric variable which contains size of a string
 */
ret_code SizeStrDir( int i, struct asm_tok tokenarray[] )
/*******************************************************/
{
    struct asym *sym;
    int sizestr;

    DebugMsg(("SizeStrDir entry\n"));
#ifdef DEBUG_OUT
    sizstrcnt++;
#endif

    if ( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
#if 0 /* this is checked in ParseLine() */
    if ( tokenarray[0].token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif
    if ( tokenarray[2].token != T_STRING || tokenarray[2].string_delim != '<' ) {
        TextItemError( &tokenarray[2] );
        return( ERROR );
    }
    if ( Token_Count > 3 ) {
        DebugMsg(("SizeStrDir: syntax error, name=%s, Token_Count=%u\n", tokenarray[0].string_ptr, Token_Count));
        AsmErr( SYNTAX_ERROR_EX, tokenarray[3].string_ptr );
        return( ERROR );
    }

    sizestr = GetLiteralValue( StringBufferEnd, tokenarray[2].string_ptr );

    if ( sym = CreateVariable( tokenarray[0].string_ptr, sizestr ) ) {
        DebugMsg(("SizeStrDir(%s) exit, value=%u\n", tokenarray[0].string_ptr, sizestr));
        LstWrite( LSTTYPE_EQUATE, 0, sym );
        return( NOT_ERROR );
    }
    return( ERROR );

}

/* InStr()
 * defines a numeric variable which contains position of substring.
 * syntax:
 * name INSTR [pos,]string, substr
 */
ret_code InStrDir( int i, struct asm_tok tokenarray[] )
/*****************************************************/
{
    struct asym *sym;
    int sizestr;
    int j;
    /* int commas; */
    char *string1;
    char buffer1[MAX_LINE_LEN];
    char buffer2[MAX_LINE_LEN];
    struct expr opndx;
    int start = 1;
    int strpos;

    DebugMsg(("InStrDir entry\n"));
#ifdef DEBUG_OUT
    instrcnt++;
#endif

    if ( i != 1) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }
#if 0 /* this is checked in ParseLine() */
    if ( tokenarray[0].token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[0].string_ptr );
        return( ERROR );
    }
#endif

    i++; /* go past INSTR */

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        if ( EvalOperand( &i, tokenarray, Token_Count, &opndx, 0 ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        start = opndx.value;
        if ( start <= 0 ) {
            /* v2.05: don't change the value. if it's invalid, the result
             * is to be 0. Emit a level 3 warning instead.
             */
            //start = 1;
            AsmWarn( 3, POSITIVE_VALUE_EXPECTED );
        }
        if ( tokenarray[i].token != T_COMMA ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++; /* skip comma */
    }

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        TextItemError( &tokenarray[i] );
        return( ERROR );
    }
    sizestr = GetLiteralValue( buffer1, tokenarray[i].string_ptr );
#ifdef DEBUG_OUT
    DebugMsg(("InStrDir: first string >%s< \n", buffer1));
#endif
    if ( start > sizestr ) {
        AsmErr( INDEX_PAST_END_OF_STRING, start );
        return( ERROR );
    }
    i++;
    if ( tokenarray[i].token != T_COMMA ) {
        AsmError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;

    if ( tokenarray[i].token != T_STRING || tokenarray[i].string_delim != '<' ) {
        TextItemError( &tokenarray[i] );
        return( ERROR );
    }
    j = GetLiteralValue( buffer2, tokenarray[i].string_ptr );
#ifdef DEBUG_OUT
    DebugMsg(("InStrDir: second string >%s< \n", buffer2));
#endif
    i++;
    if ( tokenarray[i].token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, tokenarray[i].string_ptr );
        return( ERROR );
    }

    strpos = 0;
    /* v2.05: check for start > 0 added */
    if ( ( start > 0 ) && ( sizestr >= j ) && ( string1 = strstr( buffer1 + start - 1, buffer2 ) ))
        strpos = string1 - buffer1 + 1;

    if ( sym = CreateVariable( tokenarray[0].string_ptr, strpos ) ) {
        DebugMsg(("InStrDir(%s) exit, value=%u\n", tokenarray[0].string_ptr, strpos));
        LstWrite( LSTTYPE_EQUATE, 0, sym );
        return ( NOT_ERROR );
    }
    return( ERROR );
}

#define CATSTRMAX 20

/* internal @CatStr macro function */

static ret_code CatStrFunc( char *buffer, char * *params )
/********************************************************/
{
    int i;
    char **end = params + CATSTRMAX;

    DebugMsg(("@CatStr( %s, %s, %s, ...)\n",
              *(params+0) ? *(params+0) : "",
              *(params+1) ? *(params+1) : "",
              *(params+2) ? *(params+2) : "" ));

    for (; params != end; params++) {
        if ( *params ) {
            i = strlen( *params );
            memcpy( buffer, *params, i );
            buffer += i;
        }
    }
    *buffer = NULLC;
    return( NOT_ERROR );
}

/* convert string to a number */

static ret_code GetNumber( char *string, int *pi )
/************************************************/
{
    struct expr opndx;
    int i;
    int last;

    last = Tokenize( string, Token_Count+1, TRUE );
    i = Token_Count+1;
    if( EvalOperand( &i, tokenarray, last, &opndx, 0 ) == ERROR ) {
        return( ERROR );
    }
    if( opndx.kind != EXPR_CONST || opndx.string != NULL || tokenarray[i].token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, string );
        return( ERROR );
    }
    *pi = opndx.value;
    return( NOT_ERROR );
}

/* internal @InStr macro function
 * the result is returned as string in current radix
 */
static ret_code InStrFunc( char *buffer, char * *params )
/*******************************************************/
{
    int pos = 1;
    char *p;
    uint_32 found;

    DebugMsg(("@InStr( %s, %s, %s)\n",
              *(params+0) ? *(params+0) : "",
              *(params+1) ? *(params+1) : "",
              *(params+2) ? *(params+2) : "" ));

    /* init buffer with "0" */
    *buffer = '0';
    *(buffer+1) = NULLC;

    if ( *(params+0) ) {
        if ( GetNumber( *(params+0), &pos ) == ERROR )
            return( ERROR );
        if ( pos == 0 )
            pos++;
    }

    if ( pos > strlen( *(params+1) ) ) {
        AsmErr( INDEX_PAST_END_OF_STRING, pos );
        return( ERROR );
    }
    p = strstr( *(params+1)+pos-1, *(params+2) );
    if ( p ) {
        found = p - *(params+1) + 1;
        myltoa( found, buffer, ModuleInfo.radix, FALSE, TRUE );
    }

    DebugMsg(( "@InStrFunc()=>%s<\n", buffer ));

    return( NOT_ERROR );
}

/* internal @SizeStr macro function
 * the result is returned as string in current radix
 */
static ret_code SizeStrFunc( char *buffer, char * *params )
/*********************************************************/
{
    DebugMsg(("@SizeStr(%s)\n", *params ? *params : "" ));
    if ( *params )
        myltoa( strlen( *params ), buffer, ModuleInfo. radix, FALSE, TRUE );
    else {
        buffer[0] = '0';
        buffer[1] = NULLC;
    }
    return( NOT_ERROR );
}

/* internal @SubStr macro function */

static ret_code SubStrFunc( char *buffer, char * *params )
/********************************************************/
{
    int pos;
    int size;
    char *src = *(params+0);

    DebugMsg(("@SubStr( %s, %s, %s)\n",
              src ? src : "",
              *(params+1) ? *(params+1) : "",
              *(params+2) ? *(params+2) : "" ));

    if ( GetNumber(*(params+1),&pos) == ERROR )
        return( ERROR );

    if (pos <= 0)
        pos = 1;

    size = strlen( src );
    if ( pos > size ) {
        AsmErr( INDEX_PAST_END_OF_STRING, pos );
        return( ERROR );
    }

    if ( *(params+2) ) {
        if ( GetNumber( *(params+2), &size ) == ERROR )
            return( ERROR );
        if ( size < 0 ) {
            AsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
    } else {
        size = size - pos + 1;
    }

    for( src = src+pos-1 ; size && *src ; size-- )
        *buffer++ = *src++;

    *buffer = NULLC;

    if ( size ) {
        AsmError( COUNT_VALUE_TOO_LARGE );
        return( ERROR );
    }

    return( NOT_ERROR );
}

/* string macro initialization
 * this proc is called once per module
 */
void StringInit( void )
/*********************/
{
    int i;
    struct dsym *macro;

    DebugMsg(( "StringInit() enter\n" ));

#ifdef DEBUG_OUT
    catstrcnt = 0;
    substrcnt = 0;
    sizstrcnt = 0;
    instrcnt = 0;
    equcnt = 0;
#endif

    /* add @CatStr() macro func */

    macro = CreateMacro( "@CatStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = CatStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = CATSTRMAX;
    macro->e.macroinfo->parmlist = AsmAlloc( sizeof( struct mparm_list ) * CATSTRMAX );
    for (i = 0; i < CATSTRMAX; i++) {
        macro->e.macroinfo->parmlist[i].deflt = NULL;
        //macro->e.macroinfo->parmlist[i].label = "p";
        macro->e.macroinfo->parmlist[i].required = FALSE;
    }

    /* add @InStr() macro func */

    macro = CreateMacro( "@InStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = InStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 3;
    macro->e.macroinfo->parmlist = AsmAlloc(sizeof( struct mparm_list) * 3);
    for (i = 0; i < 3; i++) {
        macro->e.macroinfo->parmlist[i].deflt = NULL;
        //macro->e.macroinfo->parmlist[i].label = parmnames[i];
        macro->e.macroinfo->parmlist[i].required = (i != 0);
    }

    /* add @SizeStr() macro func */

    macro = CreateMacro( "@SizeStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = SizeStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 1;
    macro->e.macroinfo->parmlist = AsmAlloc(sizeof( struct mparm_list));
    macro->e.macroinfo->parmlist[0].deflt = NULL;
    //macro->e.macroinfo->parmlist[0].label = parmnames[0];
    /* macro->e.macroinfo->parmlist[0].required = TRUE; */
    /* the string parameter is NOT required, '@SizeStr()' is valid */
    macro->e.macroinfo->parmlist[0].required = FALSE;

    /* add @SubStr() macro func */

    macro = CreateMacro( "@SubStr" );
    macro->sym.isdefined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = SubStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 3;
    macro->e.macroinfo->parmlist = AsmAlloc(sizeof( struct mparm_list) * 3);
    for (i = 0; i < 3; i++) {
        macro->e.macroinfo->parmlist[i].deflt = NULL;
        //macro->e.macroinfo->parmlist[i].label = parmnames[i];
        macro->e.macroinfo->parmlist[i].required = (i < 2);
    }

    return;
}
#ifdef DEBUG_OUT
void StringFini( void )
/*********************/
{
    if ( Options.quiet == FALSE )
        printf("invokation CATSTR=%u SUBSTR=%u SIZESTR=%u INSTR=%u EQU(text)=%u\n", catstrcnt, substrcnt, sizstrcnt, instrcnt, equcnt );
}
#endif
