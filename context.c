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

enum {
    CONT_ASSUMES,
    CONT_RADIX,
    CONT_LISTING,
    CONT_CPU,
    CONT_ALL
};

typedef struct _context {
    struct _context *next;
    uint_32 type;
    char data[];
} context;

#if AMD64_SUPPORT
#define NUM_STDREGS 16
#else
#define NUM_STDREGS 8
#endif

typedef struct _assumes_context {
    assume_info SegAssumeTable[NUM_SEGREGS];
    assume_info StdAssumeTable[NUM_STDREGS];
} assumes_context;

typedef struct _listing_context {
    enum listmacro list_macro;
    unsigned list:1;
    unsigned cref:1;
    unsigned listif:1;
    unsigned list_generated_code:1;
} listing_context;

typedef struct _cpu_context {
    short cpu;              /* saved ModuleInfo.cpu      */
    enum asm_cpu curr_cpu;  /* saved ModuleInfo.curr_cpu */
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
static context *saved_contexts;
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
            if (_stricmp(*p, AsmBuffer[i]->string_ptr) == 0) {
                i++;
                if (AsmBuffer[i]->token == T_FINAL) {
                    if (directive == T_POPCONTEXT) {
                        DebugMsg(( "POPCONTEXT %s\n", AsmBuffer[i-1]->string_ptr ));
                        /* for POPCONTEXT, check if the proper item is pushed */
                        pcontext = ContextStack;
                        if (pcontext == NULL || pcontext->type != type) {
                            AsmErr( BLOCK_NESTING_ERROR, AsmBuffer[i-2]->pos);
                            return( ERROR );
                        }
                        ContextStack = pcontext->next;

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
                            ModuleInfo.list_macro = lcontext->list_macro;
                            ModuleInfo.list = lcontext->list;
                            ModuleInfo.cref = lcontext->cref;
                            ModuleInfo.listif = lcontext->listif;
                            ModuleInfo.list_generated_code = lcontext->list_generated_code;
                        }
                        if (ccontext) {
                            ModuleInfo.cpu     = ccontext->cpu;
                            if (sym_Cpu)
                                sym_Cpu->value = ccontext->cpu;
                            ModuleInfo.curr_cpu = ccontext->curr_cpu;
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
                            lcontext->list_macro = ModuleInfo.list_macro;
                            lcontext->list   = ModuleInfo.list;
                            lcontext->cref   = ModuleInfo.cref;
                            lcontext->listif = ModuleInfo.listif;
                            lcontext->list_generated_code = ModuleInfo.list_generated_code;
                        }
                        if (ccontext) {
                            ccontext->cpu      = ModuleInfo.cpu;
                            ccontext->curr_cpu = ModuleInfo.curr_cpu;
                        }
                        pcontext->next = ContextStack;
                        ContextStack = pcontext;
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
    context *p;

    for ( i = 0, p=ContextStack ; p ; i++, p = p->next );

    saved_numcontexts = i;
    if ( i ) {
        saved_contexts = AsmAlloc( i * (sizeof(context) + sizeof(all_context)) );
        for ( i = 0, p = ContextStack ; i < saved_numcontexts ; i++, p = p->next )
            memcpy( saved_contexts+i, p, sizeof(context) + sizeof(all_context));
    }
}

// restore context status

static void ContextRestoreState( void )
{
    int i;
    context *p;

    for ( i = saved_numcontexts; i ; i-- ) {
        p = saved_contexts+i-1;
        p->next = ContextStack;
        ContextStack = p;
    }
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


