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
#include "directiv.h"
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

/* use this version to ensure that uppercase letters are used! */
extern void myltoa( uint_32 value, char *buffer, uint radix, bool sign, bool addzero );

// generic parameter names. In case the parameter name is
// displayed in an error message ("required parameter %s missing")

static const char * parmnames[] = {"p1","p2","p3"};

/* CatStr()
 * defines a text equate
 * syntax <name> CATSTR [<string>,...]
 * TEXTEQU is an alias for CATSTR
 */

ret_code CatStrDef( int i )
/*************************/
{
    asm_sym *sym;
    int count;
    //expr_list opndx;
    char buffer[MAX_LINE_LEN];

#ifdef DEBUG_OUT
    catstrcnt++;
#endif
    DebugMsg(("CatStrDef(%u) enter\n", i ));

    /* syntax must be <id> CATSTR textitem[,textitem,...] */
    if ( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if ( AsmBuffer[0]->token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }

    i++; /* go past CATSTR/TEXTEQU */

    sym = SymSearch( AsmBuffer[0]->string_ptr );

    if( sym && sym->state != SYM_TMACRO ) {
        if( sym->state == SYM_UNDEFINED ) {
            /* v2.01: symbol has been used already. Using
             * a textmacro before it has been defined is
             * somewhat problematic!
             */
            dir_remove_table( (dir_node *)sym );
#if FASTPASS
            SkipSavedState();
#endif
            AsmWarn( 2, TEXT_MACRO_USED_BEFORE_DEFINITION, sym->name );
        } else  {
            /* it is defined as something else, get out */
            DebugMsg(( "CatStrDef(%s) exit, error\n", sym->name));
            AsmErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }
    }

    buffer[0] = NULLC;
    for ( count = 0; i < Token_Count; ) {
        DebugMsg(("CatStrDef(%s): item=%s\n", AsmBuffer[0]->string_ptr, AsmBuffer[i]->string_ptr));
        if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
            DebugMsg(("CatStrDef: bad item: token=%u\n", AsmBuffer[i]->token));
            AsmError( TEXT_ITEM_REQUIRED );
            return( ERROR );
        }
        if ( ( count + AsmBuffer[i]->value ) >= MAX_LINE_LEN) {
            AsmError( STRING_OR_TEXT_LITERAL_TOO_LONG );
            return( ERROR );
        }
        strcpy( buffer + count, AsmBuffer[i]->string_ptr );
        count = count + AsmBuffer[i]->value;
        i++;
        if ( ( AsmBuffer[i]->token != T_COMMA ) &&
            ( AsmBuffer[i]->token != T_FINAL ) ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        i++;
    }

    if ( sym == NULL ) {
        sym = SymCreate( AsmBuffer[0]->string_ptr, TRUE );
        DebugMsg(( "CatStrDef: new symbol %s created\n", sym->name));
    }

    if ( sym->string_ptr )
        AsmFree( sym->string_ptr );

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;
    sym->string_ptr = (char *)AsmAlloc( count + 1);
    memcpy( sym->string_ptr, buffer, count + 1 );
    DebugMsg(("CatStrDef(%s) result: >%s<\n", sym->name, buffer));

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_EQUATE, 0, sym );

    return( NOT_ERROR );
}

/*
 used by EQU if the value to be assigned to a symbol is text.
*/
asm_sym * SetTextMacro( asm_sym *sym, const char *name, const char *value )
/*************************************************************************/
{
    int count;
    char buffer[MAX_LINE_LEN];

#ifdef DEBUG_OUT
    equcnt++;
#endif
#if 0 // FASTPASS
    /* there's no need to set the value if FASTPASS is on, because
     the input are just preprocessed lines.
     this check is probably obsolete by now, since it won't be called
     ever if pass > 1. Also, even with fastpass it's sometimes necessary
     to do a full second pass, including source preprocessing.
     */
    if (Parse_Pass != PASS_1)
        return( sym );
#endif

    if (sym == NULL)
        sym = SymCreate( name, TRUE );

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;

    if ( AsmBuffer[2]->token == T_STRING && AsmBuffer[2]->string_delim == '<' && AsmBuffer[3]->token == T_FINAL ) {
        value = AsmBuffer[2]->string_ptr;
        count = AsmBuffer[2]->value;
    } else {
        char *p = buffer;
        /*
         the original source is used, since the tokenizer has
         deleted some information. it's important to double '!' found inside
         the string.
         */
        while ( isspace( *value ) ) value++;
        count = strlen( value );
        if (count) {
            for ( ; count; count--)
                if ( isspace( *( value + count - 1 ) ) == FALSE)
                    break;
        }
        for ( ; count; count--) {
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

    DebugMsg(( "SetTextMacro(%s): value is >%s<, exit\n", sym->name, sym->string_ptr ));
    return( sym );
}

// SubStr()
// defines a text equate
// syntax: name SUBSTR <string>, pos [, size]

ret_code SubStrDef( int i )
/*************************/
{
    struct asm_sym      *sym;
    char                *name;
    char                *p;
    char                *newvalue;
    int                 pos;
    int                 size = MAX_LINE_LEN;
    int                 cnt;
    //char                buffer[MAX_LINE_LEN];
    expr_list           opndx;

    DebugMsg(("SubStrDef entry\n"));
#ifdef DEBUG_OUT
    substrcnt++;
#endif

    // at least 5 items are needed
    // 0  1      2      3 4    5   6
    // ID SUBSTR SRC_ID , POS [, LENGTH]

    if ( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if ( AsmBuffer[0]->token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }

    name = AsmBuffer[0]->string_ptr;

    i++; /* go past SUBSTR */

    // third item must be a string

    if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
        DebugMsg(("SubStrDef: error, no text item\n"));
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    p = AsmBuffer[i]->string_ptr;
    i++;
    DebugMsg(("SubStrDef(%s): src=>%s<\n", name, p));

    if ( AsmBuffer[i]->token != T_COMMA ) {
        AsmError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;

    // get pos, must be a numeric value and > 0

    if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
        DebugMsg(("SubStrDef(%s): invalid pos value\n", name));
        return( ERROR );
    }

    if (opndx.kind != EXPR_CONST || opndx.string != NULL) {
        DebugMsg(("SubStrDef(%s): pos value is not a constant\n", name));
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }

    /* pos is expected to be 1-based */
    pos = opndx.value;
    if (pos <= 0) {
        AsmError( POSITIVE_VALUE_EXPECTED );
        return( ERROR );
    }
    if (AsmBuffer[i]->token != T_FINAL) {
        if (AsmBuffer[i]->token != T_COMMA) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++;
        // get size, must be a numeric value
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR ) {
            DebugMsg(("SubStrDef(%s): invalid size value\n", name));
            return( ERROR );
        }
        if (opndx.kind != EXPR_CONST || opndx.string != NULL) {
            DebugMsg(("SubStrDef(%s): size value is not a constant\n", name));
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        size = opndx.value;
        if ( AsmBuffer[i]->token != T_FINAL ) {
            DebugMsg(("SubStrDef(%s): additional items found\n", name));
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        if (size < 0) {
            AsmError( COUNT_MUST_BE_POSITIVE_OR_ZERO );
            return( ERROR );
        }
    }

    sym = SymSearch( name );

    /* if we've never seen it before, put it in */
    if( sym == NULL ) {
        sym = SymCreate( name, TRUE );
    } else {
        /* was it referenced before definition (shouldn't happen anymore) */
        if( sym->state == SYM_UNDEFINED && sym->state != SYM_TMACRO ) {
            /* it is defined as something else, get out */
            DebugMsg(( "SubStrDef(%s) exit, error\n", sym->name));
            AsmErr( SYMBOL_REDEFINITION, sym->name );
            return( ERROR );
        }
    }

    sym->state = SYM_TMACRO;
    sym->defined = TRUE;

    cnt = pos;
    for ( pos--; pos > 0 && *p ; pos--, p++)
        if (*p == '!' && *(p+1) != NULLC)
            p++;

    if (*p == NULLC) {
        AsmErr( INDEX_PAST_END_OF_STRING, cnt );
        return( ERROR );
    }

    if (*p == '!' && *(p+1) != NULLC)
        p++;

    for (newvalue = p, cnt = size; *p && cnt; cnt--, p++)
        if (*p == '!' && *(p+1) != NULLC)
            p++;

    size = p - newvalue;
    p = newvalue;

    newvalue = AsmAlloc (size + 1);
    memcpy( newvalue, p, size );
    *(newvalue+size) = '\0';
    DebugMsg(("SubStrDef(%s): result=>%s<\n", sym->name, newvalue));
    AsmFree( sym->string_ptr );
    sym->string_ptr = newvalue;

    LstWrite( LSTTYPE_EQUATE, 0, sym );

    return( NOT_ERROR );
}

// SizeStr()
// defines a numeric variable which contains size of a string

ret_code SizeStrDef( int i )
/**************************/
{
    asm_sym *sym;
    int sizestr;
    char buffer[MAX_LINE_LEN];

    DebugMsg(("SizeStrDef entry\n"));
#ifdef DEBUG_OUT
    sizstrcnt++;
#endif

    if ( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if ( AsmBuffer[0]->token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }
    if ( AsmBuffer[2]->token != T_STRING || AsmBuffer[2]->string_delim != '<') {
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    if ( Token_Count > 3 ) {
        DebugMsg(("SizeStrDef: syntax error, name=%s, Token_Count=%u\n", AsmBuffer[0]->string_ptr, Token_Count));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[3]->string_ptr );
        return( ERROR );
    }

    sizestr = GetLiteralValue( buffer, AsmBuffer[2]->string_ptr );

    if ( sym = CreateConstantEx( AsmBuffer[0]->string_ptr, sizestr ) ) {
        DebugMsg(("SizeStrDef(%s) exit, value=%u\n", AsmBuffer[0]->string_ptr, sizestr));
        LstWrite( LSTTYPE_EQUATE, 0, sym );
        return( NOT_ERROR );
    } else
        return( ERROR );

}

// InStr()
// defines a numeric variable which contains position of substring
// syntax
// name INSTR [pos,]string,substr

ret_code InStrDef( int i )
/************************/
{
    asm_sym *sym;
    int sizestr;
    int j;
    //int commas;
    char *string1;
    char buffer1[MAX_LINE_LEN];
    char buffer2[MAX_LINE_LEN];
    expr_list opndx;
    int start = 1;
    int strpos;

    DebugMsg(("InStrDef entry\n"));
#ifdef DEBUG_OUT
    instrcnt++;
#endif

    if ( i != 1) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if ( AsmBuffer[0]->token != T_ID ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }

    i++; /* go past INSTR */

    if ( AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' ) {
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if ( opndx.kind != EXPR_CONST ) {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        start = opndx.value;
        if (start <= 0)
            start = 1;
        if ( AsmBuffer[i]->token != T_COMMA ) {
            AsmError( EXPECTING_COMMA );
            return( ERROR );
        }
        i++; /* skip comma */
    }

    if (AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<') {
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    sizestr = GetLiteralValue( buffer1, AsmBuffer[i]->string_ptr );
#ifdef DEBUG_OUT
    DebugMsg(("InStrDef: first string >%s< \n", buffer1));
#endif
    if (start > sizestr) {
        AsmErr( INDEX_PAST_END_OF_STRING, start );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_COMMA) {
        AsmError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;

    if (AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<') {
        AsmError( TEXT_ITEM_REQUIRED );
        return( ERROR );
    }
    j = GetLiteralValue( buffer2, AsmBuffer[i]->string_ptr );
#ifdef DEBUG_OUT
    DebugMsg(("InStrDef: second string >%s< \n", buffer2));
#endif
    i++;
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    strpos = 0;
    if ((sizestr >= j) && (string1 = strstr(buffer1+start-1, buffer2)))
        strpos = string1 - buffer1 + 1;

    if ( sym = CreateConstantEx( AsmBuffer[0]->string_ptr, strpos ) ) {
        DebugMsg(("InStrDef(%s) exit, value=%u\n", AsmBuffer[0]->string_ptr, strpos));
        LstWrite( LSTTYPE_EQUATE, 0, sym );
        return ( NOT_ERROR );
    }
    return( ERROR );
}

#define CATSTRMAX 20

// internal @CatStr macro function

static ret_code CatStrFunc( char * buffer, char * *params )
/*********************************************************/
{
    char **end = params + CATSTRMAX;

    DebugMsg(("@CatStr( %s, %s, %s, ...)\n",
              *(params+0) ? *(params+0) : "",
              *(params+1) ? *(params+1) : "",
              *(params+2) ? *(params+2) : "" ));

    for (; params != end; params++) {
        if ( *params ) {
            strcpy( buffer, *params );
            buffer += strlen( buffer );
        }
    }
    return( NOT_ERROR );
}

/* convert string to a number */

static ret_code GetNumber( char * string, int * pi )
/**************************************************/
{
    expr_list opndx;
    int i;
    int last;

    last = Tokenize( string, Token_Count+1 );
    i = Token_Count+1;
    if( EvalOperand( &i, last, &opndx, TRUE ) == ERROR ) {
        return( ERROR );
    }
    if( opndx.kind != EXPR_CONST || opndx.string != NULL || AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, string );
        return( ERROR );
    }
    *pi = opndx.value;
    return( NOT_ERROR );
}

// internal @InStr macro function
// the result is returned as string in current radix

static ret_code InStrFunc( char * buffer, char * *params )
/********************************************************/
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

    if (pos > strlen(*(params+1))) {
        AsmErr( INDEX_PAST_END_OF_STRING, pos );
        return( ERROR );
    }
    p = strstr( *(params+1)+pos-1, *(params+2) );
    if (p) {
        found = p - *(params+1) + 1;
        myltoa( found, buffer, ModuleInfo.radix, FALSE, TRUE );
    }

    DebugMsg(( "@InStrFunc()=>%s<\n", buffer ));

    return( NOT_ERROR );
}

// internal @SizeStr macro function
// the result is returned as string in current radix

static ret_code SizeStrFunc( char * buffer, char * *params )
/**********************************************************/
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

// internal @SubStr macro function

static ret_code SubStrFunc( char * buffer, char * *params )
/*********************************************************/
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

// string macro initialization
// this proc is called once per module

void StringInit( void )
/*********************/
{
    int i;
    dir_node *macro;

    DebugMsg(( "StringInit() enter\n" ));

#ifdef DEBUG_OUT
    catstrcnt = 0;
    substrcnt = 0;
    sizstrcnt = 0;
    instrcnt = 0;
    equcnt = 0;
#endif

    // add @CatStr() macro func

    macro = CreateMacro("@CatStr" );
    macro->sym.defined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = CatStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = CATSTRMAX;
    macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list) * CATSTRMAX);
    for (i = 0; i < CATSTRMAX; i++) {
        macro->e.macroinfo->parmlist[i].def = NULL;
        macro->e.macroinfo->parmlist[i].label = "p";
        macro->e.macroinfo->parmlist[i].required = FALSE;
    }

    // add @InStr() macro func

    macro = CreateMacro("@InStr" );
    macro->sym.defined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = InStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 3;
    macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list) * 3);
    for (i = 0; i < 3; i++) {
        macro->e.macroinfo->parmlist[i].def = NULL;
        macro->e.macroinfo->parmlist[i].label = parmnames[i];
        macro->e.macroinfo->parmlist[i].required = (i != 0);
    }

    // add @SizeStr() macro func

    macro = CreateMacro("@SizeStr" );
    macro->sym.defined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = SizeStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 1;
    macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list));
    macro->e.macroinfo->parmlist[0].def = NULL;
    macro->e.macroinfo->parmlist[0].label = parmnames[0];
    //macro->e.macroinfo->parmlist[0].required = TRUE;
    //the string parameter is NOT required, '@SizeStr()' is valid
    macro->e.macroinfo->parmlist[0].required = FALSE;

    // add @SubStr() macro func

    macro = CreateMacro("@SubStr" );
    macro->sym.defined = TRUE;
    macro->sym.predefined = TRUE;
    macro->sym.func_ptr = SubStrFunc;
    macro->sym.isfunc = TRUE;
    macro->e.macroinfo->parmcnt = 3;
    macro->e.macroinfo->parmlist = AsmAlloc(sizeof(mparm_list) * 3);
    for (i = 0; i < 3; i++) {
        macro->e.macroinfo->parmlist[i].def = NULL;
        macro->e.macroinfo->parmlist[i].label = parmnames[i];
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
