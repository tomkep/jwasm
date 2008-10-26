/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  Processing of OPTION directive.
*
****************************************************************************/


#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "parser.h"
#include "symbols.h"
#include "directiv.h"

#include "myassert.h"

/* prototypes */
extern asm_sym          *sym_Interface;
extern void             *InstrRemove( char * );
extern int              get_instruction_position( char *string );

enum {
#undef fix
#define fix( tok, str, val, init )              tok
#include "dirtoken.h"
};

typedef struct _option {
    char *name;
    int (*func)(int *);
} option;

// OPTION directive helper functions

/* OPTION DOTNAME */

static int SetDotName(int *pi)
{
    /* AsmWarn( 4, IGNORING_DIRECTIVE, AsmBuffer[(*pi)-1]->string_ptr ); */
    return(NOT_ERROR);
}

/* OPTION NODOTNAME */

static int SetNoDotName(int *pi)
{
    AsmWarn( 4, IGNORING_DIRECTIVE, AsmBuffer[(*pi)-1]->string_ptr );
    return( NOT_ERROR );
}

/* OPTION CASEMAP:NONE | NOTPUBLIC | ALL */
/* NOTPUBLIC isn't implemented yet */

static int SetCaseMap(int *pi)
{
    int i = *pi;
    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token == T_ID) {
        if (0 == stricmp(AsmBuffer[i]->string_ptr,"NONE")) {
            ModuleInfo.nocasemap = TRUE;
            SymSetCmpFunc(TRUE);
            i++;
        } else if (0 == stricmp(AsmBuffer[i]->string_ptr,"NOTPUBLIC")) {
            AsmWarn( 2, IGNORING_DIRECTIVE, AsmBuffer[i]->string_ptr );
            i++;
        } else if (0 == stricmp(AsmBuffer[i]->string_ptr,"ALL")) {
            ModuleInfo.nocasemap = FALSE;
            SymSetCmpFunc(FALSE);
            i++;
        }
    } else {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION M510 */

static int SetM510(int *pi)
{
    SetMasm510(TRUE);
    return( NOT_ERROR );
}

/* OPTION NOM510 */

static int SetNoM510(int *pi)
{
    SetMasm510(FALSE);
    return( NOT_ERROR );
}

/* OPTION SCOPED */

static int SetScoped(int *pi)
{
    ModuleInfo.scoped = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOSCOPED */

static int SetNoScoped(int *pi)
{
    ModuleInfo.scoped = FALSE;
    return( NOT_ERROR );
}

/* OPTION OLDSTRUCTS */

static int SetOldStructs(int *pi)
{
    ModuleInfo.oldstructs = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOOLDSTRUCTS */

static int SetNoOldStructs(int *pi)
{
    ModuleInfo.oldstructs = FALSE;
    return( NOT_ERROR );
}

/* OPTION EMULATOR */

static int SetEmulator(int *pi)
{
    ModuleInfo.emulator = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOEMULATOR */

static int SetNoEmulator(int *pi)
{
    ModuleInfo.emulator = FALSE;
    return( NOT_ERROR );
}

/* OPTION LJMP */

static int SetLJmp(int *pi)
{
    ModuleInfo.ljmp = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOLJMP */

static int SetNoLJmp(int *pi)
{
    ModuleInfo.ljmp = FALSE;
    return( NOT_ERROR );
}

/* OPTION NOREADONLY */

static int SetNoReadonly(int *pi)
{
    /* default, nothing to do */
    return( NOT_ERROR );
}

/* OPTION NOOLDMACROS */

static int SetNoOldmacros(int *pi)
{
    /* default, nothing to do */
    return( NOT_ERROR );
}

/* OPTION EXPR32 */

static int SetExpr32(int *pi)
{
    /* default, nothing to do */
    return( NOT_ERROR );
}

/* OPTION NOKEYWORD */

static int SetNoKeyword(int *pi)
{
    int i = *pi;
    unsigned int j;
    char * p;

    if( Parse_Pass != PASS_1 ) {
        *pi = Token_Count;
        return( NOT_ERROR);
    }
    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_STRING) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    for (p = AsmBuffer[i]->string_ptr; *p; ) {
        while (isspace(*p)) p++;
        if (*p) {
            char buffer[64];
            int cnt = sizeof(buffer) - 1;
            char * p2 = buffer;
            struct asm_ins *instruct;
            for (;*p && cnt;cnt--) {
                if (isspace(*p) || *p == ',')
                    break;
                *p2++ = *p++;
            }
            *p2 = NULLC;
            j = get_instruction_position(buffer);
            if (j == EMPTY) {
                AsmError( RESERVED_WORD_EXPECTED );
                return(ERROR);
            }
            /* it's valid to remove a reserved word twice! */
            if (AsmOpTable[j].opnd_type[0] == OP_SPECIAL) {
                instruct = (struct asm_ins *)&AsmOpTable[j];
                /* once the word is removed, it can't be restored! */
                instruct->specialtype = OP_UNUSED;
            } else
                InstrRemove(buffer);
        }
        while (isspace(*p)) p++;
        if (*p == ',') p++;
    }
    i++;
    *pi = i;
    return(NOT_ERROR);
}

/* OPTION LANGUAGE */

static int SetLanguage(int *pi)
{
    int i = *pi;
    int language = ERROR;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token == T_RES_ID)
        language = token_cmp( AsmBuffer[i]->string_ptr, TOK_PROC_BASIC, TOK_PROC_SYSCALL );

    if (language == ERROR) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }

    ModuleInfo.langtype = TypeInfo[language].value;
    /* update @Interface assembly time variable */
    if (sym_Interface)
        sym_Interface->value = ModuleInfo.langtype;

    i++;
    *pi = i;
    return(NOT_ERROR);
}

/* OPTION SETIF2 */

static int SetSetIF2(int *pi)
{
    int i = *pi;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_ID) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (0 == stricmp(AsmBuffer[i]->string_ptr, "TRUE"))
        ModuleInfo.setif2 = TRUE;
    else if (0 == stricmp(AsmBuffer[i]->string_ptr, "FALSE"))
        ModuleInfo.setif2 = FALSE;
    else {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    i++;
    *pi = i;
    return(NOT_ERROR);
}

/* OPTION PROLOGUE:macroname
 the prologue macro must be a macro function with 6 params:
 name macro procname, flag, parmbytes, localbytes, <reglist>, userparms
 procname: name of procedure
 flag: bits 0-2: calling convention
 bit 3: undef
 bit 4: 1 if caller restores ESP
 bit 5: 1 if proc is far
 bit 6: 1 if proc is private
 bit 7: 1 if proc is export
 bit 8: for epilogue: 1 if IRET, 0 if RET
 parmbytes: no of bytes for all params
 localbytes: no of bytes for all locals
 reglist: list of registers to save/restore, separated by commas
 userparms: prologuearg specified in PROC
 */

static int SetPrologue(int *pi)
{
    int i = *pi;
    char * name = (char *)-1;
    asm_sym * sym;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_ID) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (0 == stricmp(AsmBuffer[i]->string_ptr,"NONE")) {
        name = NULL;
    } else if (0 == stricmp(AsmBuffer[i]->string_ptr,"PROLOGUEDEF")) {
        name = "";
    } else {
        sym = SymSearch(AsmBuffer[i]->string_ptr);
        if ((sym != NULL) && (sym->state == SYM_MACRO) &&
            (sym->isfunc == TRUE))
            name = sym->name;
    }
    if (name == (char *)-1) {
        AsmError( PROLOGUE_MUST_BE_MACRO_FUNC );
        return( ERROR );
    }

    ModuleInfo.proc_prologue = name;
    i++;
    *pi = i;
    return(NOT_ERROR);
}

/* OPTION EPILOGUE:macroname */

static int SetEpilogue(int *pi)
{
    int i = *pi;
    char * name = (char *)-1;
    asm_sym * sym;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_ID) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (0 == stricmp(AsmBuffer[i]->string_ptr,"NONE")) {
        name = NULL;
    } else if (0 == stricmp(AsmBuffer[i]->string_ptr,"EPILOGUEDEF")) {
        name = "";
    } else {
        sym = SymSearch(AsmBuffer[i]->string_ptr);
        if ((sym != NULL) && (sym->state == SYM_MACRO) &&
            (sym->isfunc == FALSE))
            name = sym->name;
    }
    if (name == (char *)-1) {
        AsmError( EPILOGUE_MUST_BE_MACRO_PROC );
        return( ERROR );
    }
    ModuleInfo.proc_epilogue = name;
    i++;
    *pi = i;
    return(NOT_ERROR);
}

// OPTION OFFSET: SEGMENT | GROUP | FLAT
/* OFFSET: SEGMENT isn't supported yet */

static int SetOffset(int *pi)
{
    int i = *pi;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token == T_FINAL) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if (0 == stricmp(AsmBuffer[i]->string_ptr,"GROUP")) {
        i++;
    } else if (0 == stricmp(AsmBuffer[i]->string_ptr,"FLAT")) {
        i++;
    } else if (0 == stricmp(AsmBuffer[i]->string_ptr,"SEGMENT")) {
        AsmErr( NOT_SUPPORTED, AsmBuffer[i-3]->pos );
        return( ERROR );
    }
    *pi = i;
    return(NOT_ERROR);
}

/* OPTION PROC:PRIVATE | PUBLIC | EXPORT */

static int SetProc(int *pi)
{
    int i = *pi;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    switch (AsmBuffer[i]->token) {
    case T_ID:
        if (0 == stricmp(AsmBuffer[i]->string_ptr,"PRIVATE")) {
            ModuleInfo.procs_private = TRUE;
            ModuleInfo.procs_export = FALSE;
            i++;
        } else if (0 == stricmp(AsmBuffer[i]->string_ptr,"EXPORT")) {
            ModuleInfo.procs_private = FALSE;
            ModuleInfo.procs_export = TRUE;
            i++;
        }
        break;
    case T_RES_ID:
        if (AsmBuffer[i]->value == T_PUBLIC) {
            ModuleInfo.procs_private = FALSE;
            ModuleInfo.procs_export = FALSE;
            i++;
        }
        break;
    default:
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION SEGMENT:USE16|USE32|FLAT */

static int SetSegment(int *pi)
{
    int i = *pi;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token == T_RES_ID) {
        if (AsmBuffer[i]->value == T_USE16) {
            ModuleInfo.defUse32 = FALSE;
            i++;
        } else if (AsmBuffer[i]->value == T_USE32 ||
                   AsmBuffer[i]->value == T_FLAT) {
            ModuleInfo.defUse32 = TRUE;
            i++;
        }
    } else {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    *pi = i;
    return(NOT_ERROR);
}

static int Unsupported(int *pi)
{
    AsmErr( NOT_SUPPORTED, AsmBuffer[(*pi)-2]->pos );
    return( ERROR );
}

// the table must be here after the option helper functions
// to avoid having to define prototypes.

static option optiontab[] = {
    "CASEMAP",      SetCaseMap,
    "PROC",         SetProc,
    "PROLOGUE",     SetPrologue,
    "EPILOGUE",     SetEpilogue,
    "DOTNAME",      SetDotName,
    "NODOTNAME",    SetNoDotName,
    "M510",         SetM510,
    "NOM510",       SetNoM510,
    "SCOPED",       SetScoped,
    "NOSCOPED",     SetNoScoped,
    "OLDSTRUCTS",   SetOldStructs,
    "NOOLDSTRUCTS", SetNoOldStructs,
    "EMULATOR",     SetEmulator,
    "NOEMULATOR",   SetNoEmulator,
    "LJMP",         SetLJmp,
    "NOLJMP",       SetNoLJmp,
    "READONLY",     Unsupported,
    "NOREADONLY",   SetNoReadonly,
    "OLDMACROS",    Unsupported,
    "NOOLDMACROS",  SetNoOldmacros,
    "EXPR16",       Unsupported,
    "EXPR32",       SetExpr32,
    "NOSIGNEXTEND", Unsupported,
    "NOKEYWORD",    SetNoKeyword,
    "LANGUAGE",     SetLanguage,
    "SETIF2",       SetSetIF2,
    "OFFSET",       SetOffset,
    "SEGMENT",      SetSegment,
    NULL
};

// handle OPTION directive
// syntax:
// OPTION option[:value][,option[:value,...]]

ret_code OptionDirective( int i )
{
    option *po = NULL;

    DebugMsg(( "option directive enter\n" ));

    for ( ; ; ) {
        switch (AsmBuffer[i]->token) {
        case T_DIRECTIVE:      /* PROC, SEGMENT are of this type */
        case T_UNARY_OPERATOR: /* OFFSET keyword is of this type */
        case T_RES_ID:
        case T_ID:
            DebugMsg(( "option=%s\n", AsmBuffer[i]->string_ptr ));
            for ( po = optiontab; po->name != NULL ; po++) {
                if (0 == stricmp(AsmBuffer[i]->string_ptr, po->name)) {
                    i++;
                    if (po->func(&i) == ERROR)
                        return( ERROR );
                    break;
                }
            }
            break;
        default:
            AsmError( SYNTAX_ERROR );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA )
            i++;
        else
            break;
    }
    if ( po == NULL || po->name == NULL || AsmBuffer[i]->token != T_FINAL ) {
        DebugMsg(( "option syntax error: >%s<\n", AsmBuffer[i]->string_ptr ));
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    return( NOT_ERROR );
}

