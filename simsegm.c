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

#ifdef __I86__
#define NUMQUAL (long)
#else
#define NUMQUAL
#endif

extern const char szDgroup[];

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
    /* no DGROUP for FLAT or COFF/ELF */
    if( ModuleInfo.model == MOD_FLAT ||
       Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
       || Options.output_format == OFORMAT_ELF
#endif
      )
        return;

    if( name == NULL ) {
        switch( segm ) {
        case SIM_STACK:
            if ( ModuleInfo.distance == STACK_FAR )
                return;
            break;
        }
        name = SegmNames[segm];
    }
    AddLineQueueX( "%s %r %s", szDgroup, T_GROUP, name );
}

/* generate code to close the current segment */

static void close_currseg( void )
/*******************************/
{
    if ( CurrSeg ) {
        DebugMsg1(("close_currseg: current seg=%s\n", CurrSeg->sym.name));
        AddLineQueueX( "%s %r", CurrSeg->sym.name, T_ENDS );
    }
}

/* translate a simplified segment directive to
 * a standard segment directive line
 */

static void SetSimSeg( enum sim_seg segm, const char * name )
/***********************************************************/
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

    pFmt = "%s %r %s %s %s '%s'";
    if ( name == NULL ) {
        if ( ModuleInfo.simseg_init & ( 1 << segm ) )
            pFmt = "%s %r";
        else {
            ModuleInfo.simseg_init |= ( 1 << segm );
        }
        name = SegmNames[segm];
    } else {
        asm_sym * sym = SymSearch(name);
        /* v2.04: testing for state SYM_SEG isn't enough. The segment
         * might have been "defined" by a GROUP directive. Additional
         * check for segment's lname index is needed.
         */
        //if ( sym && sym->state == SYM_SEG )
        if ( sym && sym->state == SYM_SEG && ((dir_node *)sym)->e.seginfo->lname_idx != 0 )
            pFmt = "%s %r";
    }
    AddLineQueueX( pFmt, name, T_SEGMENT, pAlign, pUse, SegmCombine[segm], pClass );
    return;
}

static void EndSimSeg( enum sim_seg segm )
/****************************************/
{
    AddLineQueueX( "%s %r", SegmNames[segm], T_ENDS );
    return;
}

ret_code SimplifiedSegDir( int i )
/********************************/
/*
 Handles simplified segment directives:
 .CODE, .STACK, .DATA, .DATA?, .FARDATA, .FARDATA?, .CONST
 */
{
    uint_32     stksize;
    const char  *name = NULL;
    int         type;
    enum sim_seg seg;
    //char        buffer[ MAX_ID_LEN+1+64 ];

    DebugMsg1(("SimplifiedSegDir(%s) enter\n", AsmBuffer[i]->string_ptr ));

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

    //buffer[0] = NULLC;
    i++; /* get past the directive token */

    /* Masm accepts a name argument for .CODE and .FARDATA[?] only.
     * JWasm also accepts this for .DATA, .BSS and .CONST unless
     * option -Zne is set.
     */
    if( AsmBuffer[i]->token == T_ID &&
       ( type == T_DOT_CODE || type == T_DOT_FARDATA || type == T_DOT_FARDATA_UN
        || ( Options.strict_masm_compat == FALSE &&
            ( type == T_DOT_DATA || type == T_DOT_CONST || type == T_DOT_DATA_UN )))) {
        name = AsmBuffer[i]->string_ptr;
        i++;
    }

    switch( type ) {
    case T_DOT_CODE: /* .code */
        SetSimSeg( SIM_CODE, name );

        if( ModuleInfo.model == MOD_TINY ) {
            name = szDgroup;
        } else if( ModuleInfo.model == MOD_FLAT ) {
            name = "FLAT";
        } else {
            if( name == NULL )
                name = SegmNames[SIM_CODE];
        }
        AddLineQueueX( "%r %r:%s", T_ASSUME, T_CS, name );
        break;
    case T_DOT_STACK: /* .stack */
        stksize = DEFAULT_STACK_SIZE;
        if( i < Token_Count ) {
            expr_list opndx;
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                return( ERROR );
            if( opndx.kind != EXPR_CONST ) {
                AsmError( CONSTANT_EXPECTED );
                return( ERROR );
            }
            stksize = opndx.value;
        }
#if FASTPASS
        /* if code is generated which does "emit" bytes,
         * the original source line has to be saved.
         */
        if ( StoreState == FALSE && Parse_Pass == PASS_1 ) {
            SaveState();
        }
#endif
        SetSimSeg( SIM_STACK, NULL );
        /* add stack to dgroup for some segmented models */
        AddToDgroup( SIM_STACK, NULL );
#if 0
        AddLineQueue( "ORG 0" );
        sprintf( buffer, "db %lu dup (?)", stksize );
        AddLineQueue( buffer );
#else
        AddLineQueueX( "ORG 0%xh", NUMQUAL stksize );
#endif
        EndSimSeg( SIM_STACK );
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

        SetSimSeg( seg, name );
        AddLineQueueX( "%r %r:ERROR", T_ASSUME, T_CS );
        AddToDgroup( seg, name );
        break;
    case T_DOT_FARDATA:     /* .fardata  */
    case T_DOT_FARDATA_UN:  /* .fardata? */
        seg = ( type == T_DOT_FARDATA ) ? SIM_FARDATA : SIM_FARDATA_UN;

        SetSimSeg( seg, name );
        AddLineQueueX( "%r %r:ERROR", T_ASSUME, T_CS );
        break;
    default: /* shouldn't happen */
        /**/myassert( 0 );
        break;
    }
    if ( AsmBuffer[i]->token != T_FINAL )
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );

    RunLineQueue();

    DebugMsg1(("SimplifiedSegDir exit\n"));
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
        if ( SIZE_CODEPTR & ( 1 << ModuleInfo.model ) ) {
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
    char buffer[20];

    if ( Parse_Pass == PASS_1 ) {
        /* create default code segment (_TEXT) */
        SetSimSeg( SIM_CODE, NULL );
        EndSimSeg( SIM_CODE );

        /* create default data segment (_DATA) */
        SetSimSeg( SIM_DATA, NULL ) ;
        EndSimSeg( SIM_DATA );

        /* create DGROUP for BIN/OMF if model isn't FLAT */
        if( model != MOD_FLAT &&
            ( Options.output_format == OFORMAT_OMF ||
             Options.output_format == OFORMAT_BIN )) {
            strcpy( buffer, "%s %r %s" );
            if( model == MOD_TINY ) {
                strcat( buffer, ", %s" );
                AddLineQueueX( buffer, szDgroup, T_GROUP, SegmNames[SIM_CODE], SegmNames[SIM_DATA] );
            } else
                AddLineQueueX( buffer, szDgroup, T_GROUP, SegmNames[SIM_DATA] );
        }
    }
    return( NOT_ERROR );
}
