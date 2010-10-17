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
#include "directiv.h"
#include "insthash.h"
#include "symbols.h"
#include "expreval.h"

/* prototypes */
extern asm_sym          *sym_Interface;
#if MZ_SUPPORT
extern struct MZDATA mzdata;
#endif

#ifdef __I86__
#define OPTQUAL __near
#else
#define OPTQUAL
#endif

typedef struct _option {
    const char *name;
    int OPTQUAL (*func)(int *);
} option;

/* OPTION directive helper functions */

/* OPTION DOTNAME */

static int OPTQUAL SetDotName( int *pi )
/**************************************/
{
    /* AsmWarn( 4, IGNORING_DIRECTIVE, AsmBuffer[(*pi)-1]->string_ptr ); */
    ModuleInfo.dotname = TRUE;
    return( NOT_ERROR );
}

/* OPTION NODOTNAME */

static int OPTQUAL SetNoDotName( int *pi )
/****************************************/
{
    //AsmWarn( 4, IGNORING_DIRECTIVE, AsmBuffer[(*pi)-1]->string_ptr );
    ModuleInfo.dotname = FALSE;
    return( NOT_ERROR );
}

/* OPTION CASEMAP:NONE | NOTPUBLIC | ALL */
/* NOTPUBLIC isn't implemented yet */

static int OPTQUAL SetCaseMap( int *pi )
/**************************************/
{
    int i = *pi;
    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token == T_ID) {
        if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"NONE") ) {
            ModuleInfo.case_sensitive = TRUE;        /* -Cx */
            ModuleInfo.convert_uppercase = FALSE;
        } else if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"NOTPUBLIC") ) {
            ModuleInfo.case_sensitive = FALSE;       /* -Cp */
            ModuleInfo.convert_uppercase = FALSE;
        } else if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"ALL") ) {
            ModuleInfo.case_sensitive = FALSE;       /* -Cu */
            ModuleInfo.convert_uppercase = TRUE;
        } else {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        DebugMsg1(("SetCaseMap(%s) ok\n", AsmBuffer[i]->string_ptr ));
        i++;
        SymSetCmpFunc();
    } else {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION M510 */

static int OPTQUAL SetM510( int *pi )
/***********************************/
{
    SetMasm510( TRUE );
    return( NOT_ERROR );
}

/* OPTION NOM510 */

static int OPTQUAL SetNoM510( int *pi )
/*************************************/
{
    SetMasm510(FALSE);
    return( NOT_ERROR );
}

/* OPTION SCOPED */

static int OPTQUAL SetScoped( int *pi )
/*************************************/
{
    ModuleInfo.scoped = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOSCOPED */

static int OPTQUAL SetNoScoped( int *pi )
/***************************************/
{
    ModuleInfo.scoped = FALSE;
    return( NOT_ERROR );
}

/* OPTION OLDSTRUCTS */

static int OPTQUAL SetOldStructs( int *pi )
/*****************************************/
{
    ModuleInfo.oldstructs = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOOLDSTRUCTS */

static int OPTQUAL SetNoOldStructs( int *pi )
/*******************************************/
{
    ModuleInfo.oldstructs = FALSE;
    return( NOT_ERROR );
}

/* OPTION EMULATOR */

static int OPTQUAL SetEmulator( int *pi )
/***************************************/
{
    ModuleInfo.emulator = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOEMULATOR */

static int OPTQUAL SetNoEmulator( int *pi )
/*****************************************/
{
    ModuleInfo.emulator = FALSE;
    return( NOT_ERROR );
}

/* OPTION LJMP */

static int OPTQUAL SetLJmp( int *pi )
/***********************************/
{
    ModuleInfo.ljmp = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOLJMP */

static int OPTQUAL SetNoLJmp( int *pi )
/*************************************/
{
    ModuleInfo.ljmp = FALSE;
    return( NOT_ERROR );
}

/* OPTION NOREADONLY */

static int OPTQUAL SetNoReadonly( int *pi )
/*****************************************/
{
    /* default, nothing to do */
    return( NOT_ERROR );
}

/* OPTION NOOLDMACROS */

static int OPTQUAL SetNoOldmacros( int *pi )
/******************************************/
{
    /* default, nothing to do */
    return( NOT_ERROR );
}

/* OPTION EXPR32 */

static int OPTQUAL SetExpr32( int *pi )
/*************************************/
{
    /* default, nothing to do */
    return( NOT_ERROR );
}

static int OPTQUAL SetNoSignExt( int *pi )
/****************************************/
{
    ModuleInfo.NoSignExtend = TRUE;
    return( NOT_ERROR );
}

/* OPTION NOKEYWORD */

static int OPTQUAL SetNoKeyword( int *pi )
/****************************************/
{
    int i = *pi;
    struct ReservedWord *resw;
    char * p;

    if( Parse_Pass != PASS_1 ) {
        *pi = Token_Count;
        return( NOT_ERROR);
    }
    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token != T_STRING ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    for ( p = AsmBuffer[i]->string_ptr; *p; ) {
        while ( isspace( *p ) ) p++;
        if ( *p ) {
            char buffer[64];
            int cnt = sizeof(buffer) - 1;
            char * p2 = buffer;
            //struct asm_ins *instruct;
            for (;*p && cnt;cnt--) {
                if (isspace(*p) || *p == ',')
                    break;
                *p2++ = *p++;
            }
            *p2 = NULLC;
            cnt = p2 - buffer;
            resw = FindResWord( buffer );
            if ( resw )
                DisableKeyword( resw - AsmResWord );
            else {
                if ( IsKeywordDisabled( buffer, cnt ) == EMPTY ) {
                    AsmError( RESERVED_WORD_EXPECTED );
                    return( ERROR );
                }
            }
        }
        while (isspace(*p)) p++;
        if (*p == ',') p++;
    }
    i++;
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION LANGUAGE:{C|PASCAL|BASIC|FORTRAN|SYSCALL|STDCALL|FASTCALL} */

static int OPTQUAL SetLanguage( int *pi )
/***************************************/
{
    int i = *pi;
    //lang_type langtype;
    //int language = ERROR;

    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token == T_RES_ID ) {
        if ( GetLangType( &i, &ModuleInfo.langtype ) == NOT_ERROR ) {
            /* update @Interface assembly time variable */
            if ( ModuleInfo.model != MOD_NONE && sym_Interface )
                sym_Interface->value = ModuleInfo.langtype;
            *pi = i;
            return( NOT_ERROR );
        }
    }
    AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
    return( ERROR );
}

/* OPTION SETIF2
 * syntax: option setif2:TRUE|FALSE
 */

static int OPTQUAL SetSetIF2( int *pi )
/*************************************/
{
    int i = *pi;

    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token == T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->string_ptr );
        return( ERROR );
    }
    if ( 0 == _stricmp( AsmBuffer[i]->string_ptr, "TRUE" ) ) {
        ModuleInfo.setif2 = TRUE;
        i++;
    } else if ( 0 == _stricmp( AsmBuffer[i]->string_ptr, "FALSE" ) ) {
        ModuleInfo.setif2 = FALSE;
        i++;
    }
    *pi = i;
    return( NOT_ERROR );
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

static int OPTQUAL SetPrologue( int *pi )
/***************************************/
{
    int i = *pi;
    char * name;
    //asm_sym * sym;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_ID) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if (0 == _stricmp(AsmBuffer[i]->string_ptr,"NONE")) {
        name = NULL;
    } else if (0 == _stricmp(AsmBuffer[i]->string_ptr,"PROLOGUEDEF")) {
        name = "";
    } else {
        name = AsmAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1);
        strcpy( name, AsmBuffer[i]->string_ptr );
    }
    if ( ModuleInfo.proc_prologue && *ModuleInfo.proc_prologue )
        AsmFree( ModuleInfo.proc_prologue );

    ModuleInfo.proc_prologue = name;
    i++;
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION EPILOGUE:macroname */
/*
 do NOT check the macros here!
 */

static int OPTQUAL SetEpilogue( int *pi )
/***************************************/
{
    int i = *pi;
    char * name = (char *)-1;
    //asm_sym * sym;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_ID) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if (0 == _stricmp(AsmBuffer[i]->string_ptr,"NONE")) {
        name = NULL;
    } else if (0 == _stricmp(AsmBuffer[i]->string_ptr,"EPILOGUEDEF")) {
        name = "";
    } else {
        name = AsmAlloc( strlen( AsmBuffer[i]->string_ptr ) + 1);
        strcpy( name, AsmBuffer[i]->string_ptr );
    }

    if ( ModuleInfo.proc_epilogue && *ModuleInfo.proc_epilogue )
        AsmFree( ModuleInfo.proc_epilogue );

    ModuleInfo.proc_epilogue = name;
    i++;
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION OFFSET: GROUP | FLAT | SEGMENT
 * default is GROUP.
 * determines result of OFFSET operator fixups if .model isn't set.
 */
static int OPTQUAL SetOffset( int *pi )
/*************************************/
{
    int i = *pi;

    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token == T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->string_ptr );
        return( ERROR );
    }
    if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"GROUP" ) ) {
        ModuleInfo.offsettype = OT_GROUP;
    } else if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"FLAT" ) ) {
        ModuleInfo.offsettype = OT_FLAT;
    } else if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"SEGMENT" ) ) {
        ModuleInfo.offsettype = OT_SEGMENT;
    } else {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    i++;
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION PROC:PRIVATE | PUBLIC | EXPORT */

static int OPTQUAL SetProc( int *pi )
/***********************************/
{
    int i = *pi;

    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token == T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i-1]->string_ptr );
        return( ERROR );
    }

    switch ( AsmBuffer[i]->token ) {
    case T_ID:
        if ( 0 == _stricmp( AsmBuffer[i]->string_ptr, "PRIVATE" ) ) {
            ModuleInfo.procs_private = TRUE;
            ModuleInfo.procs_export = FALSE;
            i++;
        } else if ( 0 == _stricmp( AsmBuffer[i]->string_ptr, "EXPORT" ) ) {
            ModuleInfo.procs_private = FALSE;
            ModuleInfo.procs_export = TRUE;
            i++;
        }
        break;
    case T_DIRECTIVE: /* word PUBLIC is a directive */
        if ( AsmBuffer[i]->value == T_PUBLIC ) {
            ModuleInfo.procs_private = FALSE;
            ModuleInfo.procs_export = FALSE;
            i++;
        }
        break;
    }
    *pi = i;
    return( NOT_ERROR );
}

/* OPTION SEGMENT:USE16|USE32|FLAT
 * this option set the default offset size for segments and
 * externals defined outside of segments.
 */

static int OPTQUAL SetSegment( int *pi )
/**************************************/
{
    int i = *pi;

    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token == T_RES_ID && AsmBuffer[i]->value == T_FLAT ) {
#if AMD64_SUPPORT
        if ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) >= P_64 )
            ModuleInfo.defOfssize = USE64;
        else
#endif
            ModuleInfo.defOfssize = USE32;
    } else if ( AsmBuffer[i]->token == T_ID && _stricmp( AsmBuffer[i]->string_ptr, "USE16" ) == 0) {
        ModuleInfo.defOfssize = USE16;
    } else if ( AsmBuffer[i]->token == T_ID && _stricmp( AsmBuffer[i]->string_ptr, "USE32" ) == 0) {
        ModuleInfo.defOfssize = USE32;
#if AMD64_SUPPORT
    } else if ( AsmBuffer[i]->token == T_ID && _stricmp( AsmBuffer[i]->string_ptr, "USE64" ) == 0) {
        ModuleInfo.defOfssize = USE64;
#endif
    } else {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    i++;
    *pi = i;
    return( NOT_ERROR );
}

#if FIELDALIGN
/* OPTION FIELDALIGN:1|2|4|8|16|32 */

static int OPTQUAL SetFieldAlign( int *pi )
/*****************************************/
{
    int i = *pi;
    uint temp, temp2;
    expr_list opndx;

    if ( Options.strict_masm_compat ) {
        (*pi)--;
        return( NOT_ERROR );
    }
    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
        return( ERROR );
    if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }
    if( opndx.uvalue > MAX_STRUCT_ALIGN ) {
        AsmError( STRUCT_ALIGN_TOO_HIGH );
        return( ERROR );
    }
    for( temp = 1, temp2 = 0; temp < opndx.uvalue ; temp <<= 1, temp2++ );
    if( temp != opndx.uvalue ) {
        AsmError( POWER_OF_2 );
        return( ERROR );
    }
    ModuleInfo.fieldalign = temp2;
    *pi = i;
    return( NOT_ERROR );
}
#endif

#if PROCALIGN
/* OPTION PROCALIGN:1|2|4|8|16|32 */

static int OPTQUAL SetProcAlign( int *pi )
/****************************************/
{
    int i = *pi;
    int temp, temp2;
    expr_list opndx;

    if ( Options.strict_masm_compat ) {
        (*pi)--;
        return( NOT_ERROR );
    }
    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
        return( ERROR );
    if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }
    if( opndx.value > MAX_STRUCT_ALIGN ) {
        AsmError( STRUCT_ALIGN_TOO_HIGH );
    }
    for( temp = 1, temp2 = 0; temp < opndx.value ; temp <<= 1, temp2++ );
    if( temp != opndx.value ) {
        AsmError( POWER_OF_2 );
        return( ERROR );
    }
    ModuleInfo.procalign = temp2;
    *pi = i;
    return( NOT_ERROR );
}
#endif

#if MZ_SUPPORT
static int OPTQUAL SetMZ( int *pi )
/*********************************/
{
    int i = *pi;
    int j;
    uint_16 *parms;
    expr_list opndx;

    *pi = i;
    if ( Options.strict_masm_compat ) {
        (*pi)--;
        return( NOT_ERROR );
    }
    if ( AsmBuffer[i]->token != T_COLON ) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    for (j = 0, parms = (uint_16 *)&mzdata ; j < 4; j++) {
        if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
            return( ERROR );
        if ( opndx.kind == EXPR_EMPTY ) {
        } else if ( opndx.kind == EXPR_CONST ) {
            if ( opndx.value > 0xFFFF ) {
                AsmError( CONSTANT_VALUE_TOO_LARGE );
                return( ERROR );
            }
            *(parms + j) = opndx.value;
        } else {
            AsmError( CONSTANT_EXPECTED );
            return( ERROR );
        }
        if ( AsmBuffer[i]->token == T_COMMA )
            i++;
    }
    /* ensure validity of the params */
    if ( mzdata.ofs_fixups < 0x1E )
        mzdata.ofs_fixups = 0x1E;

    for( j = 16; j < mzdata.alignment; j <<= 1 );
    if( j != mzdata.alignment )
        AsmError( INVALID_HEADER_ALIGNMENT );

    if ( mzdata.heapmax < mzdata.heapmin )
        mzdata.heapmax = mzdata.heapmin;

    *pi = i;
    return( NOT_ERROR );
}
#endif

#if AMD64_SUPPORT
/* OPTION FRAME: AUTO | NOAUTO
 * default is NOAUTO
 */
static int OPTQUAL SetFrame( int *pi )
/*************************************/
{
    int i = *pi;

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( AsmBuffer[i]->token == T_FINAL ) {
        AsmError( SYNTAX_ERROR );
        return( ERROR );
    }
    if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"AUTO" ) ) {
        ModuleInfo.frame_auto = 1;
        i++;
    } else if ( 0 == _stricmp(AsmBuffer[i]->string_ptr,"NOAUTO" ) ) {
        ModuleInfo.frame_auto = 0;
        i++;
    }
    *pi = i;
    return( NOT_ERROR );
}
#endif

#if ELF_SUPPORT
static int OPTQUAL SetElf( int *pi )
/*********************************/
{
    int i = *pi;
    expr_list opndx;

    if ( Options.strict_masm_compat ) {
        (*pi)--;
        return( NOT_ERROR );
    }
    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
        return( ERROR );
    if ( opndx.kind == EXPR_CONST ) {
        if ( opndx.llvalue > 0xFF ) {
            AsmError( CONSTANT_VALUE_TOO_LARGE );
            return( ERROR );
        }
        if ( Options.output_format == OFORMAT_ELF )
            ModuleInfo.osabi = opndx.value;
    } else {
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }
    *pi = i;
    return( NOT_ERROR );
}
#endif

#if RENAMEKEY

/* OPTION RENAMEKEYWORD */

static int OPTQUAL SetRenameKey( int *pi )
/****************************************/
{
    int i = *pi;
    struct ReservedWord *resw;
    char * oldname;

    /* reject option if -Zne is set */
    if ( Options.strict_masm_compat ) {
        (*pi)--;
        return( NOT_ERROR );
    }
    /* do nothing if pass > 1 */
    if( Parse_Pass != PASS_1 ) {
        *pi = Token_Count;
        return( NOT_ERROR );
    }
    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_STRING || AsmBuffer[i]->string_delim != '<' )  {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    oldname = AsmBuffer[i]->string_ptr;
    i++;
    if ( AsmBuffer[i]->token != T_COMMA ) {
        AsmError( EXPECTING_COMMA );
        return( ERROR );
    }
    i++;
    if (AsmBuffer[i]->token != T_ID )  {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    resw = FindResWord( oldname );
    if ( resw == NULL ) {
        AsmError( RESERVED_WORD_EXPECTED );
        return( ERROR );
    }
    RenameKeyword( resw - AsmResWord, AsmBuffer[i]->string_ptr, strlen( AsmBuffer[i]->string_ptr ) );
    i++;
    *pi = i;
    return( NOT_ERROR );
}
#endif

#if AMD64_SUPPORT
static int OPTQUAL SetWin64( int *pi )
/************************************/
{
    int i = *pi;
    expr_list opndx;

    /* if -win64 isn't set, return with ERROR.
     * this will make jwasm skip this line.
     */
    if ( Options.header_format != HFORMAT_WIN64 )
        return( ERROR );

    /* reject option if -Zne is set */
    if ( Options.strict_masm_compat ) {
        (*pi)--;
        return( NOT_ERROR );
    }

    if (AsmBuffer[i]->token != T_COLON) {
        AsmError( COLON_EXPECTED );
        return( ERROR );
    }
    i++;
    if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
        return( ERROR );
    if ( opndx.kind == EXPR_CONST ) {
        if ( opndx.llvalue > 0x1 ) {
            AsmError( CONSTANT_VALUE_TOO_LARGE );
            return( ERROR );
        }
        ModuleInfo.win64_saveparams = opndx.value;
    } else {
        AsmError( CONSTANT_EXPECTED );
        return( ERROR );
    }
    *pi = i;
    return( NOT_ERROR );
}
#endif

static int OPTQUAL Unsupported( int *pi )
/***************************************/
{
    AsmErr( NOT_SUPPORTED, AsmBuffer[(*pi)-2]->pos );
    return( ERROR );
}

/* the table must be here after the option helper functions
 * to avoid having to define prototypes.
 */
static const option optiontab[] = {
    { "CASEMAP",      SetCaseMap     },
    { "PROC",         SetProc        },
    { "PROLOGUE",     SetPrologue    },
    { "EPILOGUE",     SetEpilogue    },
    { "DOTNAME",      SetDotName     },
    { "NODOTNAME",    SetNoDotName   },
    { "M510",         SetM510        },
    { "NOM510",       SetNoM510      },
    { "SCOPED",       SetScoped      },
    { "NOSCOPED",     SetNoScoped    },
    { "OLDSTRUCTS",   SetOldStructs  },
    { "NOOLDSTRUCTS", SetNoOldStructs},
    { "EMULATOR",     SetEmulator    },
    { "NOEMULATOR",   SetNoEmulator  },
    { "LJMP",         SetLJmp        },
    { "NOLJMP",       SetNoLJmp      },
    { "READONLY",     Unsupported    },
    { "NOREADONLY",   SetNoReadonly  },
    { "OLDMACROS",    Unsupported    },
    { "NOOLDMACROS",  SetNoOldmacros },
    { "EXPR16",       Unsupported    },
    { "EXPR32",       SetExpr32      },
    { "NOSIGNEXTEND", SetNoSignExt   },
    { "NOKEYWORD",    SetNoKeyword   },
    { "LANGUAGE",     SetLanguage    },
    { "SETIF2",       SetSetIF2      },
    { "OFFSET",       SetOffset      },
    { "SEGMENT",      SetSegment     },
#if FIELDALIGN
    { "FIELDALIGN",   SetFieldAlign  },
#endif
#if PROCALIGN
    { "PROCALIGN",    SetProcAlign   },
#endif
#if MZ_SUPPORT
    { "MZ",           SetMZ          },
#endif
#if AMD64_SUPPORT
    { "FRAME",        SetFrame       },
#endif
#if ELF_SUPPORT
    { "ELF",          SetElf         },
#endif
#if RENAMEKEY
    { "RENAMEKEYWORD",SetRenameKey   },
#endif
#if AMD64_SUPPORT
    { "WIN64",        SetWin64       },
#endif
};

#define TABITEMS sizeof( optiontab) / sizeof( optiontab[0] )

/* handle OPTION directive
 * syntax:
 * OPTION option[:value][,option[:value,...]]
 */
ret_code OptionDirective( int i )
/*******************************/
{
    int idx = -1;

    DebugMsg1(( "OPTION directive enter, option=%s\n", AsmBuffer[i+1]->string_ptr ));

    i++; /* skip OPTION directive */
    while ( AsmBuffer[i]->token != T_FINAL ) {
        _strupr( AsmBuffer[i]->string_ptr );
        for ( idx = 0; idx < TABITEMS; idx++ ) {
            if ( 0 == strcmp( AsmBuffer[i]->string_ptr, optiontab[idx].name ) )
                break;
        }
        if ( idx >= TABITEMS )
            break;
        i++;
        if ( optiontab[idx].func( &i ) == ERROR )
            return( ERROR );
        if ( AsmBuffer[i]->token != T_COMMA )
            break;
        i++;
    }
    if ( idx >= TABITEMS  || AsmBuffer[i]->token != T_FINAL ) {
        DebugMsg(( "option syntax error: >%s<\n", AsmBuffer[i]->string_ptr ));
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    return( NOT_ERROR );
}

