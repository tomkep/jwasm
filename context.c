/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  Processing of PUSHCONTEXT and POPCONTEXT directives.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "assume.h"
#include "expreval.h"
#include "fastpass.h"
#include "listing.h"

#include "myassert.h"

enum {
    CONT_ASSUMES,
    CONT_RADIX,
    CONT_LISTING,
    CONT_CPU,
    CONT_ALL
};

typedef struct _context {
    uint_32 type;
    char data[];
} context;

typedef struct _assumes_context {
    assume_info SegAssumeTable[NUM_SEGREGS];
    assume_info StdAssumeTable[NUM_STDREGS];
} assumes_context;

typedef struct _listing_context {
    unsigned list:1;
    unsigned cref:1;
} listing_context;

typedef struct _cpu_context {
    short cpu;          /* saved ModuleInfo.cpu */
    enum asm_cpu cicpu; /* saved CodeInfo.cpu   */
} cpu_context;

typedef struct _radix_context {
    uint_8 radix; /* saved ModuleInfo.radix */
} radix_context;

typedef struct _all_context {
    assumes_context ac;
    radix_context   rc;
    listing_context lc;
    cpu_context     cc;
} all_context;

extern asm_sym *sym_Cpu;

static context *ContextStack;

#if FASTPASS
static int saved_numcontexts;
static all_context *saved_contexts;
#endif

ret_code ContextDirective( int directive, int i )
{
    int type;
    context *pcontext;
    assumes_context *acontext = NULL;
    listing_context *lcontext = NULL;
    cpu_context     *ccontext = NULL;
    radix_context   *rcontext = NULL;
    all_context     *alcontext;

    static char *context[] = { "ASSUMES", "RADIX", "LISTING", "CPU", "ALL", NULL };

    DebugMsg(( "xxxCONTEXT directive enter\n"));
    i++;
    if (AsmBuffer[i]->token == T_ID) {
        char **p;
        for (p = context, type = CONT_ASSUMES; *p ; p++, type++) {
            if (stricmp(*p, AsmBuffer[i]->string_ptr) == 0) {
                i++;
                if (AsmBuffer[i]->token == T_FINAL) {
                    if (directive == T_POPCONTEXT) {
                        DebugMsg(( "POPCONTEXT %s\n", AsmBuffer[i-1]->string_ptr ));
                        /* for POPCONTEXT, check if the proper item is pushed */
                        pcontext = peek( ContextStack, 0 );
                        if (pcontext == NULL || pcontext->type != type) {
                            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i-2]->pos);
                            return( ERROR );
                        }
                        // get the item
                        pcontext = pop( &ContextStack );

                        // restore the values
                        switch (type) {
                        case CONT_ASSUMES:
                            acontext = (assumes_context *)&pcontext->data;
                            break;
                        case CONT_RADIX:
                            rcontext = (radix_context *)&pcontext->data;
                            break;
                        case CONT_LISTING:
                            lcontext = (listing_context *)&pcontext->data;
                            break;
                        case CONT_CPU:
                            ccontext = (cpu_context *)&pcontext->data;
                            break;
                        case CONT_ALL:
                            alcontext = (all_context *)&pcontext->data;
                            acontext = &alcontext->ac;
                            rcontext = &alcontext->rc;
                            lcontext = &alcontext->lc;
                            ccontext = &alcontext->cc;
                        }
                        if (acontext) {
                            SetSegAssumeTable( acontext->SegAssumeTable );
                            SetStdAssumeTable( acontext->StdAssumeTable );
                        }
                        if (rcontext) {
                            ModuleInfo.radix = rcontext->radix;
                        }
                        if (lcontext) {
                            ModuleInfo.list = lcontext->list;
                            ModuleInfo.cref = lcontext->cref;
                        }
                        if (ccontext) {
                            ModuleInfo.cpu     = ccontext->cpu;
                            if (sym_Cpu)
                                sym_Cpu->value = ccontext->cpu;
                            ModuleInfo.curr_cpu = ccontext->cicpu;
                        }
                        // remove the item
                        AsmFree(pcontext);
                    } else {
                        DebugMsg(( "PUSHCONTEXT %s\n", AsmBuffer[i-1]->string_ptr ));
                        // setup a context item
                        switch (type) {
                        case CONT_ASSUMES:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(assumes_context));
                            acontext = (assumes_context *)&pcontext->data;
                            break;
                        case CONT_RADIX:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(radix_context));
                            rcontext = (radix_context *)&pcontext->data;
                            break;
                        case CONT_LISTING:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(listing_context));
                            lcontext = (listing_context *)&pcontext->data;
                            break;
                        case CONT_CPU:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(cpu_context));
                            ccontext = (cpu_context *)&pcontext->data;
                            break;
                        case CONT_ALL:
                            pcontext = AsmAlloc(sizeof(context) + sizeof(all_context));
                            alcontext = (all_context *)pcontext->data;
                            acontext = &alcontext->ac;
                            rcontext = &alcontext->rc;
                            lcontext = &alcontext->lc;
                            ccontext = &alcontext->cc;
                            break;
                        }

                        pcontext->type = type;

                        if (acontext) {
                            GetSegAssumeTable( acontext->SegAssumeTable );
                            GetStdAssumeTable( acontext->StdAssumeTable );
                        }
                        if (rcontext) {
                            rcontext->radix = ModuleInfo.radix;
                        }
                        if (lcontext) {
                            lcontext->list = ModuleInfo.list;
                            lcontext->cref = ModuleInfo.cref;
                        }
                        if (ccontext) {
                            ccontext->cpu   = ModuleInfo.cpu;
                            ccontext->cicpu = ModuleInfo.curr_cpu;
                        }
                        push( &ContextStack, pcontext );
                    }
                    return( NOT_ERROR );
                }
                break;
            }
        }
    }
    AsmError( SYNTAX_ERROR );
    return( ERROR );
}

#if FASTPASS

// save current context status

void ContextSaveState( void )
{
    int i;

    for ( i = 0 ; ; i++ )
        if ( peek( ContextStack, i ) == NULL )
            break;

    saved_numcontexts = i;
    if ( i ) {
        saved_contexts = AsmAlloc( i * sizeof(all_context) );
        for ( i = 0; i < saved_numcontexts ; i++ )
            memcpy( saved_contexts+i, peek( ContextStack, i), sizeof(all_context));
    }
}

// restore context status

static void ContextRestoreState( void )
{
    int i;
    for ( i = saved_numcontexts; i ; i-- )
        push( &ContextStack, saved_contexts+i-1 );
}

#endif

// init context, called once per pass

void ContextInit( int pass )
{
    ContextStack = NULL;
#if FASTPASS
    if ( pass == PASS_1)
        saved_numcontexts = 0;
    else {
        ContextRestoreState();
    }
#endif
}


