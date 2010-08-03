/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  Processing simplified segment directives:
*               - .CODE, .DATA, .DATA?, .CONST, .STACK, .FARDATA, .FARDATA?
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "input.h"
#include "expreval.h"
#include "fastpass.h"
#include "listing.h"
#include "msgtext.h"

#include "myassert.h"

#define DEFAULT_STACK_SIZE      1024

enum sim_seg {
    SIM_CODE = 0,
    SIM_STACK,
    SIM_DATA,
    SIM_DATA_UN,            /* .DATA? */
    SIM_FARDATA,
    SIM_FARDATA_UN,         /* .FARDATA? */
    SIM_CONST,
    SIM_LAST
};

static char *SegmNames[ SIM_LAST ];

static const char * const SegmNamesDef[ SIM_LAST ] = {
    "_TEXT", "STACK", "_DATA", "_BSS", "FAR_DATA", "FAR_BSS", "CONST"
};
static const char * const SegmClass[ SIM_LAST ] = {
    "CODE", "STACK", "DATA", "BSS", "FAR_DATA", "FAR_BSS", "CONST"
};
static const char * const SegmCombine[ SIM_LAST ] = {
    "PUBLIC", "STACK", "PUBLIC", "PUBLIC", "PRIVATE", "PRIVATE", "PUBLIC"
};

char *GetCodeSegName( void )
/**************************/
{
    return( SegmNames[SIM_CODE] );
}

const char *GetCodeClass( void )
/******************************/
{
    /* option -nc set? */
    if ( Options.code_class )
        return( Options.code_class );

    return( SegmClass[SIM_CODE] );
}

/* emit DGROUP GROUP instruction */

static void AddToDgroup( enum sim_seg segm, const char *name )
/************************************************************/
{
    char        buffer[MAX_ID_LEN+1+20];

    /* no DGROUP for FLAT or COFF/ELF */
    if( ModuleInfo.model == MOD_FLAT ||
       Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
       || Options.output_format == OFORMAT_ELF
#endif
      )
        return;

    strcpy( buffer, "DGROUP GROUP " );
    if( name != NULL ) {
        strcat( buffer, name );
    } else {
        switch( segm ) {
        case SIM_STACK:
            if ( ModuleInfo.distance == STACK_FAR )
                return;
        case SIM_DATA:
        case SIM_DATA_UN:
        case SIM_CONST:
            strcat( buffer, SegmNames[segm] );
            break;
        }
    }
    AddLineQueue( buffer );
}

/* generate code to close the current segment */

static void close_currseg( void )
/*******************************/
{
    if ( CurrSeg ) {
        char buffer[MAX_ID_LEN+1+8];
        DebugMsg(("close_currseg: current seg=%s\n", CurrSeg->sym.name));
        strcpy( buffer, CurrSeg->sym.name );
        strcat( buffer, " ENDS" );
        AddLineQueue( buffer );
    }
}

/* translate a simplified segment directive to
 * a standard segment directive line
 */

static char * SetSimSeg( enum sim_seg segm, const char * name, char * buffer )
/****************************************************************************/
{
    char *pAlign = "WORD";
    char *pAlignSt = "PARA";
    char *pUse = "";
    const char *pFmt;
    const char *pClass;

    if ( ModuleInfo.defOfssize > USE16 ) {
        if ( ModuleInfo.model == MOD_FLAT )
            pUse = "FLAT";
        else
            pUse = "USE32";
        if (( ModuleInfo.curr_cpu & P_CPU_MASK ) <= P_386 )
            pAlign = "DWORD";
        else
            pAlign = "PARA";
        pAlignSt = pAlign;
    }

    if ( segm == SIM_CODE )
        pClass = GetCodeClass();
    else
        pClass = SegmClass[segm];

    if ( segm == SIM_STACK || segm == SIM_FARDATA || segm == SIM_FARDATA_UN )
        pAlign = pAlignSt;

    pFmt = "%s SEGMENT %s %s %s '%s'";
    if ( name == NULL ) {
        if ( ModuleInfo.simseg_init & ( 1 << segm ) )
            pFmt = "%s SEGMENT";
        else {
            ModuleInfo.simseg_init |= ( 1 << segm );
        }
        name = SegmNames[segm];
    } else {
        asm_sym * sym = SymSearch(name);
        if ( sym && sym->state == SYM_SEG )
            pFmt = "%s SEGMENT";
    }
    sprintf( buffer, pFmt, name, pAlign, pUse, SegmCombine[segm], pClass );
    return( buffer );
}

static char * EndSimSeg( enum sim_seg segm, char * buffer )
/*********************************************************/
{
    strcpy( buffer, SegmNames[segm] );
    strcat( buffer, " ENDS" );
    return( buffer );
}

ret_code SimplifiedSegDir( int i )
/********************************/
/*
 Handles simplified segment directives:
 .CODE, .STACK, .DATA, .DATA?, .FARDATA, .FARDATA?, .CONST
 */
{
    char        buffer[ MAX_ID_LEN+1+64 ];
    unsigned    size;
    const char  *string = NULL;
    int         type;
    enum sim_seg seg;

    DebugMsg(("SimplifiedSegDir(%u) enter\n", i ));

    LstWrite( LSTTYPE_DIRECTIVE, 0, NULL );

    if( ModuleInfo.model == MOD_NONE ) {
        AsmError( MODEL_IS_NOT_DECLARED );
        return( ERROR );
    }

    PushLineQueue();

    type = AsmBuffer[i]->value;

    if( type != T_DOT_STACK ) {
        close_currseg();  /* emit a "xxx ENDS" line to close current seg */
    }

    buffer[0] = NULLC;
    i++; /* get past the directive token */

    /* Masm accepts a name argument for .CODE and .FARDATA[?] only.
     * JWasm also accepts this for .DATA, .BSS and .CONST unless
     * option -Zne is set.
     */
    if( AsmBuffer[i]->token == T_ID &&
       ( type == T_DOT_CODE || type == T_DOT_FARDATA || type == T_DOT_FARDATA_UN
        || ( Options.strict_masm_compat == FALSE &&
            ( type == T_DOT_DATA || type == T_DOT_CONST || type == T_DOT_DATA_UN )))) {
        string = AsmBuffer[i]->string_ptr;
        i++;
    }

    switch( type ) {
    case T_DOT_CODE: /* .code */
        AddLineQueue( SetSimSeg( SIM_CODE, string, buffer ) );

        if( ModuleInfo.model == MOD_TINY ) {
            AddLineQueue( "ASSUME CS:DGROUP" );
        } else if( ModuleInfo.model == MOD_FLAT ) {
            //if ( SegAssumeTable[ASSUME_CS].flat == FALSE || SegAssumeTable[ASSUME_CS].error == TRUE )
                AddLineQueue( "ASSUME CS:FLAT" );
        } else {
            if( string == NULL )
                string = SegmNames[SIM_CODE];
            strcpy( buffer, "ASSUME CS:" );
            strcat( buffer, string );
            AddLineQueue( buffer );
        }
        break;
    case T_DOT_STACK: /* .stack */
        size = DEFAULT_STACK_SIZE;
        if( i < Token_Count ) {
            expr_list opndx;
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                return( ERROR );
            if( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
                AsmError( CONSTANT_EXPECTED );
                return( ERROR );
            }
            size = opndx.value;
        }
        AddLineQueue( SetSimSeg( SIM_STACK, NULL, buffer ));
        /* add stack to dgroup for some segmented models */
        AddToDgroup( SIM_STACK, NULL );
        AddLineQueue( "ORG 0" );
        sprintf( buffer, "db %u dup (?)", size );
        AddLineQueue( buffer );
        AddLineQueue( EndSimSeg( SIM_STACK, buffer ));
        break;
    case T_DOT_DATA:    /* .data  */
    case T_DOT_DATA_UN: /* .data? */
    case T_DOT_CONST:   /* .const */
        if( type == T_DOT_DATA ) {
            seg = SIM_DATA;
        } else if( type == T_DOT_DATA_UN ) {
            seg = SIM_DATA_UN;
        } else {
            seg = SIM_CONST;
        }

        AddLineQueue( SetSimSeg( seg, string, buffer ));
        AddLineQueue( "ASSUME CS:ERROR" );
        AddToDgroup( seg, string );
        break;
    case T_DOT_FARDATA:     /* .fardata  */
    case T_DOT_FARDATA_UN:  /* .fardata? */
        seg = ( type == T_DOT_FARDATA ) ? SIM_FARDATA : SIM_FARDATA_UN;

        AddLineQueue( SetSimSeg( seg, string, buffer ));
        AddLineQueue( "ASSUME CS:ERROR" );
        break;
    default: /* shouldn't happen */
        /**/myassert( 0 );
        break;
    }
    if ( AsmBuffer[i]->token != T_FINAL )
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );

    RunLineQueue();

    DebugMsg(("SimSeg exit\n"));
    return( NOT_ERROR );
}

/*
 * Set default values for .CODE and .DATA segment names.
 * Called by ModelDirective(), at Pass 1 only.
 */

void SetModelDefaultSegNames( void )
/**********************************/
{
    /* init segment names with default values */
    memcpy( SegmNames, SegmNamesDef, sizeof(SegmNames) );

    /* option -nt set? */
    if( Options.text_seg ) {
        SegmNames[SIM_CODE] = AsmAlloc( strlen( Options.text_seg ) + 1 );
        strcpy( SegmNames[SIM_CODE], Options.text_seg );
    } else {
        switch( ModuleInfo.model ) {
        case MOD_MEDIUM:
        case MOD_LARGE:
        case MOD_HUGE:
            /* for some models, the code segment contains the module name */
            SegmNames[SIM_CODE] = AsmAlloc( strlen( SegmNamesDef[SIM_CODE] ) + strlen( ModuleInfo.name ) + 1 );
            strcpy( SegmNames[SIM_CODE], ModuleInfo.name );
            strcat( SegmNames[SIM_CODE], SegmNamesDef[SIM_CODE] );
        }
    }

    /* option -nd set? */
    if ( Options.data_seg ) {
        SegmNames[SIM_DATA] = AsmAlloc( strlen( Options.data_seg ) + 1 );
        strcpy( SegmNames[SIM_DATA], Options.data_seg );
    }
    return;
}

/* Called by SetModel() [.MODEL directive].
 * Initializes simplified segment directives.
 * PushLineQueue() has already been called,
 * and the caller will run RunLineQueue() later.
 * Called for each pass.
 */
ret_code ModelSimSegmInit( int model )
/************************************/
{
    char buffer[ MAX_ID_LEN+1+64 ];

    if ( Parse_Pass == PASS_1 ) {
        /* create default code segment (_TEXT) */
        AddLineQueue( SetSimSeg( SIM_CODE, NULL, buffer ) );
        AddLineQueue( EndSimSeg( SIM_CODE, buffer ) );

        /* create default data segment (_DATA) */
        AddLineQueue( SetSimSeg( SIM_DATA, NULL, buffer )) ;
        AddLineQueue( EndSimSeg( SIM_DATA, buffer ) );

        /* create DGROUP for BIN/OMF if model isn't FLAT */
        if( model != MOD_FLAT &&
            ( Options.output_format == OFORMAT_OMF ||
             Options.output_format == OFORMAT_BIN )) {
            strcpy( buffer, "DGROUP GROUP " );
            if( model == MOD_TINY ) {
                strcat( buffer, SegmNames[SIM_CODE] );
                strcat( buffer, "," );
            }
            strcat( buffer, SegmNames[SIM_DATA] );
            AddLineQueue( buffer );
        }
    }
    return( NOT_ERROR );
}
