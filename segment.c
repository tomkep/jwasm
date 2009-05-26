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
* Description:  Processing of segment and group related directives:
*               - SEGMENT, ENDS
*               - GROUP
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
#include "tokenize.h"
#include "queues.h"
#include "expreval.h"
#include "omf.h"
#include "fastpass.h"
#include "macro.h"
#include "fixup.h"
#include "coff.h"
#include "assume.h"
#include "listing.h"
#include "msgtext.h"

#include "myassert.h"

/* prototypes */

extern symbol_queue     Tables[];       // tables of definitions

#define DEFAULT_STACK_SIZE      1024
#define DEFAULT_CODE_CLASS    "CODE"

#define INIT_ATTR       0x01
#define INIT_ALIGN      0x02
#define INIT_COMBINE    0x04
#define INIT_SEGSIZE    0x08
#define INIT_AT         0x10
#define INIT_FLAT       0x20
#define INIT_ALIGNPARAM 0x40
#define INIT_CHAR       0x80

static typeinfo SegAttr[] = {
    { "READONLY",     0,              INIT_ATTR       },
    { "BYTE",         0,              INIT_ALIGN      },
    { "WORD",         1,              INIT_ALIGN      },
    { "DWORD",        2,              INIT_ALIGN      },
    { "PARA",         4,              INIT_ALIGN      },
    { "PAGE",         8,              INIT_ALIGN      },
    { "ALIGN",        0,              INIT_ALIGN | INIT_ALIGNPARAM },
    { "PRIVATE",      COMB_INVALID,   INIT_COMBINE    },
    { "PUBLIC",       COMB_ADDOFF,    INIT_COMBINE    },
    { "STACK",        COMB_STACK,     INIT_COMBINE    },
    { "COMMON",       COMB_COMMON,    INIT_COMBINE    },
    { "MEMORY",       COMB_ADDOFF,    INIT_COMBINE    },
    { "AT",           COMB_INVALID,   INIT_COMBINE | INIT_AT },
    { "USE16",        FALSE,          INIT_SEGSIZE    },
    { "USE32",        TRUE,           INIT_SEGSIZE    },
    { "FLAT",         TRUE,           INIT_SEGSIZE | INIT_FLAT },
    { "INFO",         1,                                    INIT_CHAR },
    { "DISCARD",      IMAGE_SCN_MEM_DISCARDABLE >> 24,      INIT_CHAR },
    { "NOCACHE",      IMAGE_SCN_MEM_NOT_CACHED  >> 24,      INIT_CHAR },
    { "NOPAGE",       IMAGE_SCN_MEM_NOT_PAGED   >> 24,      INIT_CHAR },
    { "SHARED",       IMAGE_SCN_MEM_SHARED      >> 24,      INIT_CHAR },
    { "EXECUTE",      IMAGE_SCN_MEM_EXECUTE     >> 24,      INIT_CHAR },
    { "READ",         IMAGE_SCN_MEM_READ        >> 24,      INIT_CHAR },
    { "WRITE",        IMAGE_SCN_MEM_WRITE       >> 24,      INIT_CHAR }
};

typedef enum {
    SIM_CODE = 0,
    SIM_STACK,
    SIM_DATA,
    SIM_DATA_UN,            // .DATA?
    SIM_FARDATA,
    SIM_FARDATA_UN,         // .FARDATA?
    SIM_CONST,
    SIM_NONE,
    SIM_LAST = SIM_NONE
} sim_seg;

typedef struct {
    sim_seg  seg;       // segment id
} last_seg_info;        // information about last opened simplified segment

extern asm_sym          *sym_CurSeg;

uint                    segdefidx;      // Number of Segment definition
static uint             grpdefidx;      // Number of Group definition
uint                    LnamesIdx;      // Number of LNAMES definition


seg_item                *CurrSeg;       // points to stack of opened segments
//bool                    Use32;          // if current segment is 32-bit

char first_issued;                      // flags if seg is initialized

// strings used by simplified segment directives

static char *SimCodeBegin[ SIM_LAST ] = {
        "%s SEGMENT %s %s PUBLIC '%s'",
        "%s SEGMENT %s %s STACK 'STACK'",
        "%s SEGMENT %s %s PUBLIC 'DATA'",
        "%s SEGMENT %s %s PUBLIC 'BSS'",
        "%s SEGMENT %s %s PRIVATE 'FAR_DATA'",
        "%s SEGMENT %s %s PRIVATE 'FAR_BSS'",
//        "%s SEGMENT %s %s PUBLIC 'CONST' READONLY"
        "%s SEGMENT %s %s PUBLIC 'CONST'"
};

static char *SegmNames[ SIM_LAST ] = {
    "_TEXT", "STACK", "_DATA", "_BSS", "FAR_DATA", "FAR_BSS", "CONST"
};

/* Code generated by simplified segment definitions */

static last_seg_info    lastseg;        // last opened simplified segment

#if FASTPASS
// saved state
static last_seg_info    saved_lastseg; 
static int              saved_NumSegs;
static dir_node         **saved_SegStack;
static char             *saved_CurSeg_name;
static char             saved_first_issued;
#endif

dir_node         *flat_grp; /* magic FLAT group */

/* generic byte buffer, used for OMF LEDATA records only */
static uint_8           codebuf[ 1024 ];

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

static void push_seg( dir_node *seg )
/**********************************/
/* Push a segment into the current segment stack */
{
#if 0 /* it's valid to open a segment which is open already */
    seg_item    *curr;
    for( curr = CurrSeg; curr; curr = curr->next ) {
        if( curr->seg == seg ) {
            DebugMsg(("push_seg: segment is already pushed\n"));
            AsmErr( BLOCK_NESTING_ERROR, curr->seg->sym.name );
            return( ERROR );
        }
    }
#endif

    pushitem( &CurrSeg, seg );
    SetAssumeCSCurrSeg();
    return;
}

static dir_node *pop_seg( void )
/******************************/
/* Pop a segment out of the current segment stack */
{
    dir_node    *seg;

    /**/myassert( CurrSeg != NULL );
    seg = popitem( &CurrSeg );
    SetAssumeCSCurrSeg();
    return( seg );
}

direct_idx GetLnameIdx( char *name )
/**********************************/
{
    struct asm_sym      *sym;
    dir_node            *dir;

    sym = SymSearch( name );
    if( sym == NULL )
        return( LNAME_NULL );

    dir = (dir_node *)sym;
    if( sym->state == SYM_UNDEFINED ) {
        return( LNAME_NULL );
    } else if( sym->state == SYM_GRP ) {
        return( dir->e.grpinfo->lname_idx );
    } else if( sym->state == SYM_SEG ) {
        return( dir->e.seginfo->lname_idx );
    } else {    /* it is an lname record */
        return( dir->sym.idx );
    }
}

// add a class name

static direct_idx InsertClassLname( char *name )
/**********************************************/
{
    asm_sym *sym;

    if( strlen( name ) > MAX_LNAME ) {
        AsmError( LNAME_TOO_LONG );
        return( LNAME_NULL );
    }

    /* the classes aren't inserted into the symbol table
     but they are in a queue */

    sym = SymCreate( name, FALSE );
    sym->state = SYM_CLASS_LNAME;
    sym->idx = ++LnamesIdx;

    /* put it into the lname table */

    AddLnameData( (dir_node *)sym );

    return( LnamesIdx );
}

uint_32 GetCurrOffset( void )
/*************************/
{
    if( CurrSeg )
        return( CurrSeg->seg->e.seginfo->current_loc );
    return( 0 );
}

dir_node *GetCurrSeg( void )
/**************************/
{
    if( CurrSeg == NULL )
        return( NULL );
    return( CurrSeg->seg );
}

int GetCurrClass( void )
/**************************/
{
    if( CurrSeg == NULL )
        return( 0 );
    return( CurrSeg->seg->e.seginfo->segrec->d.segdef.class_name_idx );
}

uint_32 GetCurrSegAlign( void )
/*****************************/
{
    if( CurrSeg == NULL )
        return( 0 );
#if 0
    switch( CurrSeg->seg->e.seginfo->segrec->d.segdef.align ) {
    case ALIGN_ABS: // same as byte aligned ?
    case ALIGN_BYTE:
        return( 1 );
    case ALIGN_WORD:
        return( 2 );
    case ALIGN_DWORD:
        return( 4 );
    case ALIGN_PARA:
        return( 16 );
    case ALIGN_PAGE:
        return( 256 );
    case ALIGN_4KPAGE:
        return( 4096 );
    default:
        return( 0 );
    }
#else
    if ( CurrSeg->seg->e.seginfo->alignment == MAX_SEGALIGNMENT ) // ABS?
        return( 0x10 ); // assume PARA alignment for AT segments
    return( 1 << CurrSeg->seg->e.seginfo->alignment );
#endif
}

uint_32 GetCurrSegStart( void )
/*****************************/
{
    if( CurrSeg == NULL )
        return( 0 );
#if 0
    /**/myassert(( CurrSeg->seg->e.seginfo->current_loc - BufSize )
                  == CurrSeg->seg->e.seginfo->start_loc );
#endif
    return( CurrSeg->seg->e.seginfo->start_loc );
}

static dir_node *CreateGroup( char *name )
/****************************************/
{
    dir_node    *grp;

    grp = (dir_node *)SymSearch( name );

    if( grp == NULL || grp->sym.state == SYM_UNDEFINED ) {
        if (grp == NULL)
            grp = dir_insert( name, SYM_GRP );
        else
            dir_settype( grp, SYM_GRP );
        grp->sym.list = TRUE;
        grp->e.grpinfo->idx = ++grpdefidx;
        grp->e.grpinfo->lname_idx = ++LnamesIdx;
        AddLnameData( grp );
    } else if( grp->sym.state != SYM_GRP ) {
        AsmErr( SYMBOL_PREVIOUSLY_DEFINED, name );
        grp = NULL;
    }
    return( grp );
}

// handle GROUP directive

ret_code GrpDef( int i )
/*****************/
{
    char        *name;
    dir_node    *grp;
    seg_item    *new;
    seg_item    *curr;
    dir_node    *seg;

    /* GROUP directive must be at pos 1, needs a name at pos 0 */
    if( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    /* GROUP valid for OMF + BIN only */
    if (Options.output_format == OFORMAT_COFF ||
        Options.output_format == OFORMAT_ELF) {
        AsmError(GROUP_DIRECTIVE_INVALID_FOR_COFF);
        return( ERROR );
    }

    name = AsmBuffer[0]->string_ptr;
    grp = CreateGroup( name );
    if( grp == NULL )
        return( ERROR );

    i++; /* go past GROUP */

    do {

        /* get segment name */
        /* the tokenizer has problems with "dotnames" */
        if ( AsmBuffer[i]->token == T_DOT && AsmBuffer[i+1]->token == T_ID) {
            name = StringBufferEnd;
            *name = '.';
            i++;
            strcpy( name + 1, AsmBuffer[i]->string_ptr );
        } else if ( AsmBuffer[i]->token == T_ID ) {
            name = AsmBuffer[i]->string_ptr;
        } else {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        i++;

        seg = (dir_node *)SymSearch( name );
        if (Parse_Pass == PASS_1) {
            if( seg == NULL ) {
                seg = dir_insert( name, SYM_SEG );
                /* inherit the offset magnitude from the group */
                if ( grp->e.grpinfo->seglist )
                    seg->e.seginfo->Use32 = grp->sym.use32;
            } else if( seg->sym.state == SYM_UNDEFINED ) {
                dir_settype( seg, SYM_SEG );
                if ( grp->e.grpinfo->seglist )
                    seg->e.seginfo->Use32 = grp->sym.use32;
            } else if( seg->sym.state != SYM_SEG ) {
                AsmErr( SYMBOL_PREVIOUSLY_DEFINED, name );
                return( ERROR );
            } else if( seg->e.seginfo->group != NULL &&
                       seg->e.seginfo->group != &grp->sym ) {
                // segment is in another group
                DebugMsg(("GrpDef: segment >%s< is in group >%s< already\n", name, seg->e.seginfo->group->name));
                AsmErr( SEGMENT_IN_ANOTHER_GROUP, name );
                return( ERROR );
            }
            /* the first segment will define the group's word size */
            if( grp->e.grpinfo->seglist == NULL ) {
                grp->sym.use32 = seg->e.seginfo->Use32;
            } else if ( grp->sym.use32 != seg->e.seginfo->Use32) {
                AsmErr( GROUP_SEGMENT_SIZE_CONFLICT, grp->sym.name, seg->sym.name );
                return( ERROR );
            }
        } else {
            if( seg == NULL || seg->sym.state != SYM_SEG || seg->sym.defined == FALSE ) {
                AsmErr( SEG_NOT_DEFINED, name );
                return( ERROR );
            }
        }

        // insert segment in group if it's not there already
        if ( seg->e.seginfo->group == NULL ) {

            /* set the grp index of the segment */
            seg->e.seginfo->group = &grp->sym;

            new = AsmAlloc( sizeof(seg_item) );
            new->seg = seg;
            new->next = NULL;
            grp->e.grpinfo->numseg++;

            /* insert the segment at the end of linked list */
            if( grp->e.grpinfo->seglist == NULL ) {
                grp->e.grpinfo->seglist = new;
            } else {
                curr = grp->e.grpinfo->seglist;
                while( curr->next != NULL ) {
                    curr = curr->next;
                }
                curr->next = new;
            }
        }

        if ( AsmBuffer[i]->token != T_FINAL )
            if ( AsmBuffer[i]->token == T_COMMA ) {
                if ( (i + 1) < Token_Count )
                    i++;
            } else {
                AsmError( EXPECTING_COMMA );
                return( ERROR );
            }

    } while ( i < Token_Count );

    return( NOT_ERROR );
}

ret_code SetUse32( void )
/*************************/
{
    if( CurrSeg == NULL ) {
        ModuleInfo.Use32 = ModuleInfo.defUse32;
    } else {
        GlobalVars.code_seg = SEGISCODE( CurrSeg );
        ModuleInfo.Use32 = CurrSeg->seg->e.seginfo->Use32;
        if( ModuleInfo.Use32 && ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) ) {
            AsmError( WRONG_CPU_FOR_32BIT_SEGMENT );
            return( ERROR );
        }
    }
    WordSize.offset = ModuleInfo.Use32 ? 4 : 2;
    return( NOT_ERROR );
}

// close segment

static ret_code CloseSeg(char * name)
{
    struct asm_sym      *sym;

    DebugMsg(("CloseSeg(%s) enter\n", name));
    if( CurrSeg == NULL ) {
        AsmError( SEGMENT_NOT_OPENED );
        return( ERROR );
    }

    if( SymCmpFunc(CurrSeg->seg->sym.name, name ) != 0 ) {
        DebugMsg(("CloseSeg(%s): ENDS segment name not on top of stack\n"));
        AsmErr( BLOCK_NESTING_ERROR, name );
        return( ERROR );
    }

    DebugMsg(("CloseSeg(%s): current ofs=%X\n", name, CurrSeg->seg->e.seginfo->current_loc));

    if (Parse_Pass > PASS_1 && Options.output_format == OFORMAT_OMF)
        omf_FlushCurrSeg();

    pop_seg();

    return( NOT_ERROR );
}

// SEGMENT/ENDS if pass is > 1

ret_code SetCurrSeg( int i )
/**********************/
{
    char        *name;
    struct asm_sym *sym;

    name = AsmBuffer[0]->string_ptr;

    switch( AsmBuffer[i]->value ) {
    case T_SEGMENT:
        if (Options.output_format == OFORMAT_OMF)
            omf_FlushCurrSeg();
        sym = SymSearch( name );
        DebugMsg(("SetCurrSeg(%s) sym=%X\n", name, sym));
        if ( sym == NULL || sym->state != SYM_SEG) {
            AsmErr(SEG_NOT_DEFINED, name);
            return( ERROR );
        }
        push_seg( (dir_node *)sym );

        if ( ModuleInfo.list )
            LstWrite( LSTTYPE_LABEL, 0, NULL );

        break;
    case T_ENDS:
        if ( ModuleInfo.list )
            LstWrite( LSTTYPE_LABEL, 0, NULL );

        CloseSeg(name);
        break;
    default:
        break;
    }

    return( SetUse32() );
}


static seg_type TypeFromClassName( dir_node * dir, char *name )
/*****************************************/
{
    int     slen;
    char    uname[257];

//    if (dir->e.seginfo->segrec->d.segdef.align == SEGDEF_ALIGN_ABS)
    if ( dir->e.seginfo->alignment == MAX_SEGALIGNMENT )
        return( SEGTYPE_ABS );

    if( name == NULL )
        return( SEGTYPE_UNDEF );

    if( ModuleInfo.model != MOD_NONE ) {
        if( _stricmp( name, Options.code_class ) == 0 ) {
            return( SEGTYPE_CODE );
        }
    }
    slen = strlen( name );
    strcpy( uname, name );
    _strupr( uname );
    switch( slen ) {
    default:
    case 5:
        // 'CONST'
        if( memcmp( uname, "CONST", 6 ) == 0 )
            return( SEGTYPE_DATA );
        // 'STACK'
        if( memcmp( uname, "STACK", 6 ) == 0 )
            return( SEGTYPE_DATA );
        // 'DBTYP'
        if( memcmp( uname, "DBTYP", 6 ) == 0 )
            return( SEGTYPE_DATA );
        // 'DBSYM'
        if( memcmp( uname, "DBSYM", 6 ) == 0 )
            return( SEGTYPE_DATA );
    case 4:
        // 'CODE'
        if( memcmp( uname , "CODE", 5 ) == 0 )
            return( SEGTYPE_CODE );
        // '...DATA'
        if( memcmp( uname + slen - 4, "DATA", 4 ) == 0 )
            return( SEGTYPE_DATA );
    case 3:
        // '...BSS'
        if( memcmp( uname + slen - 3, "BSS", 3 ) == 0 )
            return( SEGTYPE_BSS );
    case 2:
    case 1:
    case 0:
        return( SEGTYPE_UNDEF );
    }
}

/* get the type of the segment by checking it's name */
/* this is called only if the class gives no hint */

static seg_type TypeFromSegmentName( char *name )
/*******************************************/
{
    int     slen;
    char    uname[257];

    slen = strlen( name );
    strcpy( uname, name );
    _strupr( uname );
    switch( slen ) {
    default:
    case 5:
        if ( Options.output_format != OFORMAT_COFF ) {
            /* '..._TEXT'? */
            if( memcmp( uname + slen - 5, SegmNames[SIM_CODE], 5 ) == 0 )
                return( SEGTYPE_CODE );
        }
        // '..._DATA'
        if( memcmp( uname + slen - 5, SegmNames[SIM_DATA], 5 ) == 0 )
            return( SEGTYPE_DATA );
        // 'CONST'
        if( memcmp( uname + slen - 5, SegmNames[SIM_CONST], 5 ) == 0 )
            return( SEGTYPE_DATA );
    case 4:
        if ( Options.output_format != OFORMAT_COFF ) {
            // '..._BSS'
            if( memcmp( uname + slen - 4, "_BSS", 4 ) == 0 )
                return( SEGTYPE_BSS );
        }
    case 3:
    case 2:
    case 1:
    case 0:
        return( SEGTYPE_UNDEF );
    }
}

// SEGMENT and ENDS directives (Pass ONE only!)

ret_code SegDef( int i )
/*****************/
{
    char                defined;
    char                *token;
    obj_rec             *seg;
//    obj_rec             *oldobj;
    direct_idx          classidx;
    typeinfo            *type;           // type of option
    int                 temp;
    int                 temp2;
    uint                initstate = 0;  // to show if a field is initialized
    char                oldreadonly;    // readonly value of a defined segment
    char                oldsegtype;     // iscode value of a defined segment
    char                olduse32;
    char                oldalign;
    char                oldcombine;
    uint                oldclassidx;
    uint_8              oldcharacteristics;
    dir_node            *dir;
    char                *name;
    struct asm_sym      *sym;
    expr_list           opndx;

    if( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }

    name = AsmBuffer[0]->string_ptr;

    DebugMsg(("SegDef enter, segment=%s, cmd=%s\n", name, AsmBuffer[i]->string_ptr ));

    switch( AsmBuffer[i]->value ) {
    case T_SEGMENT:
        /* Check to see if the segment is already defined */
        sym = SymSearch( name );
        if( sym == NULL || sym->state == SYM_UNDEFINED) {
            // segment is not defined (yet)
            if (sym == NULL) {
                sym = (asm_sym *)dir_insert( name, SYM_SEG );
            } else
                dir_settype( (dir_node *)sym, SYM_SEG );
            sym->list = TRUE; /* always list segments */
            dir = (dir_node *)sym;
            seg = dir->e.seginfo->segrec;
            seg->d.segdef.idx = ++segdefidx;
            defined = FALSE;
            if (Options.output_format == OFORMAT_COFF ||
                Options.output_format == OFORMAT_ELF) {
                char * p;
                if (p = strchr(sym->name, '$')) {
                    char buffer[MAX_ID_LEN+1];
                    dir_node *dir2;
                    memcpy(buffer, sym->name, p - sym->name);
                    buffer[p - sym->name] = NULLC;
                    if ((dir2 = (dir_node *)SymSearch(buffer)) && dir2->sym.state == SYM_SEG) {
                        dir->e.seginfo->readonly = dir2->e.seginfo->readonly;
                        dir->e.seginfo->segtype  = dir2->e.seginfo->segtype;
                        dir->e.seginfo->Use32    = dir2->e.seginfo->Use32;
                        dir->e.seginfo->alignment= dir2->e.seginfo->alignment;
                        dir->e.seginfo->characteristics = dir2->e.seginfo->characteristics;
                        dir->e.seginfo->segrec->d.segdef.combine        = dir2->e.seginfo->segrec->d.segdef.combine;
                        dir->e.seginfo->segrec->d.segdef.class_name_idx = dir2->e.seginfo->segrec->d.segdef.class_name_idx;
                    }
                }
            }
        } else if ( sym->state == SYM_SEG ) {
            // segment already defined
            dir = (dir_node *)sym;
            seg = dir->e.seginfo->segrec;
            defined = TRUE;
            oldreadonly = dir->e.seginfo->readonly;
            oldsegtype  = dir->e.seginfo->segtype;
            olduse32    = dir->e.seginfo->Use32;
            oldalign    = dir->e.seginfo->alignment;
            oldcharacteristics = dir->e.seginfo->characteristics;
            oldcombine  = dir->e.seginfo->segrec->d.segdef.combine;
            oldclassidx = dir->e.seginfo->segrec->d.segdef.class_name_idx;
            if( dir->e.seginfo->lname_idx == 0 ) {
                // segment was mentioned in a group statement, but not really set up
                defined = FALSE;
                seg->d.segdef.idx = ++segdefidx;
            }
        } else {
            // symbol is different kind, error
            DebugMsg(("SegDef: symbol redefinition\n"));
            AsmErr( SYMBOL_REDEFINITION, name );
            return( ERROR );
        }

        i++; /* go past SEGMENT */

        for( ; i < Token_Count; i ++ ) {
            DebugMsg(("SegDef: i=%u, string=%s token=%X\n", i, AsmBuffer[i]->string_ptr, AsmBuffer[i]->token));
            if( AsmBuffer[i]->token == T_STRING ) {

                /* the class name - the only token which is of type STRING */
                token = AsmBuffer[i]->string_ptr;
                /* string must be delimited by [double]quotes */
                if (AsmBuffer[i]->string_delim == '<' ||
                    (*token != '"' && *token != '\'')) {
                    AsmErr( SYNTAX_ERROR_EX, token );
                    continue;
                }
                /* remove the quote delimiters */
                token++;
                *(token+AsmBuffer[i]->value) = NULLC;

                classidx = FindLnameIdx( token );
                if( classidx == LNAME_NULL ) {
                    classidx = InsertClassLname( token );
                    if( classidx == LNAME_NULL ) {
                        return( ERROR );
                    }
                }
                seg->d.segdef.class_name_idx = classidx;
                DebugMsg(("SegDef: class found: %s\n", token ));
                continue;
            }

            /* go through all tokens EXCEPT the class name */
            token = AsmBuffer[i]->string_ptr;

            /* look up the type of token.
             check readonly, align, combine and word size types */

            type = FindToken( token, SegAttr, sizeof(SegAttr)/sizeof(typeinfo) );
            if( type == NULL ) {
                AsmErr( UNKNOWN_SEGMENT_ATTRIBUTE, token );
                continue;
            }

            /* initstate is used to check if any field is already
            initialized */
            if( ( type->init != INIT_CHAR ) && (initstate & type->init )) {
                AsmErr( SEGMENT_ATTRIBUTE_DEFINED_ALREADY, token );
                continue;
            } else {
                initstate |= type->init; // mark it initialized
            }

            if ( type->init & INIT_ATTR ) {
                dir->e.seginfo->readonly = TRUE;
            } else if ( type->init & INIT_ALIGNPARAM ) {
                DebugMsg(("SegDef: ALIGN() found\n" ));
                if ( Options.output_format == OFORMAT_OMF ) {
                    AsmErr( NOT_SUPPORTED_WITH_OMF_FORMAT, AsmBuffer[i]->string_ptr );
                    break;
                }
                i++;
                if ( AsmBuffer[i]->token != T_OP_BRACKET ) {
                    AsmError( SYNTAX_ERROR );
                    continue;
                }
                i++;
                if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                    continue;
                if ( AsmBuffer[i]->token != T_CL_BRACKET ) {
                    AsmError( SYNTAX_ERROR );
                    continue;
                }
                if (opndx.kind != EXPR_CONST || opndx.string != NULL) {
                    AsmError( CONSTANT_EXPECTED );
                    continue;
                }
                /*
                 COFF allows alignment values
                  1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192
                 */
                for( temp = 1, temp2 = 0; temp < opndx.value && temp < 8192 ; temp <<= 1, temp2++ );
                if( temp != opndx.value ) {
                    AsmError( POWER_OF_2 );
                }
                dir->e.seginfo->alignment = temp2;

            } else if ( type->init & INIT_ALIGN ) {
                DebugMsg(("SegDef: align attribute found\n" ));
                dir->e.seginfo->alignment = type->value;
            } else if ( type->init & INIT_AT ) {
                DebugMsg(("SegDef: AT found\n" ));
                seg->d.segdef.combine = type->value;
                //seg->d.segdef.align = SEGDEF_ALIGN_ABS;
                dir->e.seginfo->alignment = -1;
                i++;
                if (EvalOperand( &i, Token_Count, &opndx, TRUE ) != ERROR) {
                    if (opndx.kind == EXPR_CONST && opndx.string == NULL) {
                        seg->d.segdef.abs.frame = opndx.value;
                        seg->d.segdef.abs.offset = 0;
                    } else {
                        AsmError( CONSTANT_EXPECTED );
                    }
                }
            } else if ( type->init & INIT_COMBINE ) {
                DebugMsg(("SegDef: combine attribute found\n" ));
                seg->d.segdef.combine = type->value;
            } else if ( type->init & INIT_FLAT ) {
                DefineFlatGroup();
                dir->e.seginfo->Use32 = 1;
                dir->e.seginfo->group = &flat_grp->sym;
            } else if ( type->init & INIT_SEGSIZE ) {
                dir->e.seginfo->Use32 = type->value;
            } else if( type->init & INIT_CHAR ) {
                DebugMsg(("SegDef: characteristics found\n" ));
                ; // characteristics restricted to COFF/ELF
                if ( Options.output_format == OFORMAT_OMF || Options.output_format == OFORMAT_BIN ) {
                    AsmErr( NOT_SUPPORTED_WITH_CURR_FORMAT, AsmBuffer[i]->string_ptr );
                    continue;
                }
                dir->e.seginfo->characteristics |= type->value;
            }
        } /* end for */

        token = GetLname( seg->d.segdef.class_name_idx );
        if( dir->e.seginfo->segtype != SEGTYPE_CODE ) {
            seg_type res;

            res = TypeFromClassName( dir, token );
            if( res != SEGTYPE_UNDEF ) {
                dir->e.seginfo->segtype = res;
            } else {
                res = TypeFromSegmentName( name );
                dir->e.seginfo->segtype = res;
            }
        }

        if( defined ) {
            int txt = 0;

            /* Check if new definition is different from previous one */

            // oldobj = dir->e.seginfo->segrec;
            if(  oldreadonly      != dir->e.seginfo->readonly )
                txt = TXT_READONLY;
            else if ( oldalign    != dir->e.seginfo->alignment )
                txt = TXT_ALIGNMENT;
            else if ( oldcombine  != seg->d.segdef.combine )
                txt = TXT_COMBINE;
            else if ( olduse32    != dir->e.seginfo->Use32 )
                txt = TXT_SEG_WORD_SIZE;
            else if ( oldclassidx != seg->d.segdef.class_name_idx )
                txt = TXT_CLASS;
            else if ( oldcharacteristics != dir->e.seginfo->characteristics )
                txt = TXT_CHARACTERISTICS;

            if (txt) {
                DebugMsg(("seg attr changed: %s, %s\n", dir->sym.name, MsgGetEx( txt ) ));
                AsmErr( SEGDEF_CHANGED, dir->sym.name, MsgGetEx( txt ) );
                return( ERROR );
            }

            push_seg( dir );

        } else {
            /* A new definition */

            push_seg( dir );

            sym = &dir->sym;
            sym->defined = TRUE;
            SetSymSegOfs( sym );
            sym->offset = 0;
            if( dir->e.seginfo->lname_idx == 0 ) {
                dir->e.seginfo->lname_idx = ++LnamesIdx;
            }
            AddLnameData( dir );
        }

        break;
    case T_ENDS:
        if ( CloseSeg(name) == ERROR )
            return( ERROR );
        i++;
        if ( AsmBuffer[i]->token != T_FINAL ) {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        }
        break;
#if 0
    default: /* shouldn't happen */
        return( ERROR );
#endif
    }
    return( SetUse32() );
}

static void input_group( int type, char *name )
/*********************************************/
/* for simplified segment directives, emit DGROUP GROUP instruction */
{
    char        buffer[MAX_ID_LEN+1+20];

    /* no DGROUP for FLAT or COFF/ELF */
    if( ModuleInfo.model == MOD_FLAT ||
        Options.output_format == OFORMAT_COFF ||
        Options.output_format == OFORMAT_ELF)
        return;

    strcpy( buffer, "DGROUP GROUP " );
    if( name != NULL ) {
        strcat( buffer, name );
    } else {
        switch( type ) {
        case T_DOT_DATA:
            strcat( buffer, SegmNames[SIM_DATA] );
            break;
        case T_DOT_DATA_UN:
            strcat( buffer, SegmNames[SIM_DATA_UN] );
            break;
        case T_DOT_CONST:
            strcat( buffer, SegmNames[SIM_CONST] );
            break;
        case T_DOT_STACK:
            if (ModuleInfo.distance == STACK_FAR)
                return;
            strcat( buffer, SegmNames[SIM_STACK] );
            break;
        }
    }
    AddLineQueue( buffer );
}

static void close_lastseg( void )
/*******************************/
/* close the last opened simplified segment? */
/* sounds like a bad idea! the only segment which can be closed
 is the current one.
*/
{
    lastseg.seg = SIM_NONE;
    DebugMsg(("close_lastseg: current seg=%s\n", sym_CurSeg->string_ptr));
    if ( sym_CurSeg->string_ptr && (*sym_CurSeg->string_ptr != '\0') ) {
        char buffer[MAX_ID_LEN+1+8];
        strcpy( buffer, sym_CurSeg->string_ptr );
        strcat( buffer, " ENDS" );
        AddLineQueue( buffer );
    }
}


static char * SetSimSeg(int segm, char * name, char * buffer)
{
    char *pAlign = "WORD";
    char *pAlignSt = "PARA";
    char *pUse = "";
    char *pCC = "CODE";

    if ( ModuleInfo.defUse32 ) {
        if ( ModuleInfo.model == MOD_FLAT)
            pUse = "FLAT";
        else
            pUse = "USE32";
        if (( ModuleInfo.curr_cpu & P_CPU_MASK ) <= P_386)
            pAlign = "DWORD";
        else
            pAlign = "PARA";
        pAlignSt = pAlign;
    }

    if (segm == SIM_STACK || segm == SIM_FARDATA || segm == SIM_FARDATA_UN)
        pAlign = pAlignSt;

    if (Options.code_class)
        pCC = Options.code_class;

    if (name == NULL) {
        if (first_issued & (1 << segm))
            sprintf(buffer, "%s SEGMENT", SegmNames[segm]);
        else {
            first_issued |= (1 << segm);
            sprintf(buffer, SimCodeBegin[segm], SegmNames[segm], pAlign, pUse, pCC);
        }
    } else {
        asm_sym * sym = SymSearch(name);
        if (sym && sym->state == SYM_SEG)
            sprintf(buffer, "%s SEGMENT", name);
        else
            sprintf(buffer, SimCodeBegin[segm], name, pAlign, pUse, pCC);
    }
    return(buffer);
}

static char * EndSimSeg(int segm, char * buffer)
{
    sprintf(buffer, "%s ENDS", SegmNames[segm]);
    return(buffer);
}

static char *get_sim_segm_end( char *buffer, char *name )
/*******************************************************/
{
    strcpy( buffer, name );
    strcat( buffer, " ENDS");
    return( buffer );
}

ret_code SimplifiedSegDir( int i )
/*********************************/
/*
 Handles simplified segment, based on optasm pg. 142-146
 directives:
 .CODE, .STACK, .DATA, .DATA?, .FARDATA, .FARDATA?, .CONST
 */
{
    char        buffer[ MAX_ID_LEN+1+64 ];
    unsigned    size;
    char        *string;
    int         type;
    int         seg;

    DebugMsg(("SimplifiedSegDir(%u) enter\n", i ));

    LstWrite( LSTTYPE_DIRECTIVE, 0, NULL );

    if( ModuleInfo.model == MOD_NONE ) {
        AsmError( MODEL_IS_NOT_DECLARED );
        return( ERROR );
    }
    ModuleInfo.cmdline = FALSE;

    PushLineQueue();

    type = AsmBuffer[i]->value;

    if( type != T_DOT_STACK ) {
        close_lastseg();  /* emit a "xxx ENDS" line to close current seg */
    }

    buffer[0] = NULLC;
    i++; /* get past the directive token */
    if( i < Token_Count && type != T_DOT_STACK ) {
        /* the segment name might begin with a dot.
         Then use ->pos instead of ->string_ptr!
         */
        if ( AsmBuffer[i]->token == T_DOT &&
             is_valid_id_char( *(AsmBuffer[i]->pos+1) ) ) {
            string = AsmBuffer[i]->pos;
            i++;
        } else
            string = AsmBuffer[i]->string_ptr;
        i++;
    } else {
        string = NULL;
    }

    switch( type ) {
    case T_DOT_CODE:
        if( string == NULL )
            string = Options.text_seg;
        AddLineQueue( SetSimSeg(SIM_CODE, string, buffer) );
//        get_sim_segm_end( lastseg.closestr, string );
        lastseg.seg = SIM_CODE;

        if( ModuleInfo.model == MOD_TINY ) {
            AddLineQueue( "ASSUME CS:DGROUP" );
        } else if( ModuleInfo.model == MOD_FLAT ) {
            //if ( SegAssumeTable[ASSUME_CS].flat == FALSE || SegAssumeTable[ASSUME_CS].error == TRUE )
                AddLineQueue( "ASSUME CS:FLAT" );
        } else {
            strcpy( buffer, "ASSUME CS:" );
            strcat( buffer, string );
            AddLineQueue( buffer );
        }
        break;
    case T_DOT_STACK:
        size = DEFAULT_STACK_SIZE;
        if( i < Token_Count ) {
            expr_list opndx;
            if (EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR)
                return(ERROR);
            if( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
                AsmError( CONSTANT_EXPECTED );
                return( ERROR );
            }
            size = opndx.value;
        }
        AddLineQueue( SetSimSeg( SIM_STACK, NULL, buffer ));
        /* add stack to dgroup for segmented models */
        input_group( type, NULL );
        AddLineQueue( "ORG 0" );
        sprintf( buffer, "db %u dup (?)", size );
        AddLineQueue( buffer );
        AddLineQueue( EndSimSeg( SIM_STACK, buffer ));
        break;
    case T_DOT_DATA:
    case T_DOT_DATA_UN:             // .data?
    case T_DOT_CONST:
        if( type == T_DOT_DATA ) {
            if( string == NULL )
                string = Options.data_seg;
            seg = SIM_DATA;
        } else if( type == T_DOT_DATA_UN ) {
            seg = SIM_DATA_UN;
            string = NULL;
        } else {
            seg = SIM_CONST;
            string = NULL;
        }

        AddLineQueue( SetSimSeg(seg, string, buffer));
        AddLineQueue( "ASSUME CS:ERROR" );
        input_group( type, string );
        lastseg.seg = seg;
#if 0
        if( string != NULL ) {
            strcpy( lastseg.closestr, string );
            strcat( lastseg.closestr, " ENDS" );
        } else {
            strcpy( lastseg.closestr, EndSimSeg(seg, buffer) );
        }
#endif
        break;
    case T_DOT_FARDATA:
    case T_DOT_FARDATA_UN:  // .fardata?
        seg = ( type == T_DOT_FARDATA ) ? SIM_FARDATA : SIM_FARDATA_UN;

        AddLineQueue( SetSimSeg( seg, string, buffer ));
        AddLineQueue( "ASSUME CS:ERROR" );
#if 0
        if( string != NULL ) {
            strcpy( lastseg.closestr, string );
            strcat( lastseg.closestr, " ENDS" );
        } else {
            strcpy( lastseg.closestr, EndSimSeg(seg, buffer));
        }
#endif
        lastseg.seg = seg;
        break;
    default:
        /**/myassert( 0 );
        break;
    }
    if (AsmBuffer[i]->token != T_FINAL)
        AsmError( SYNTAX_ERROR );
    RunLineQueue();
    DebugMsg(("SimSeg exit\n"));
    return( NOT_ERROR );
}

void set_def_seg_name( void )
/***********************************/
{
    int len;
    char *p;

    /* set Options.code_class */
    if( Options.code_class == NULL ) {
        Options.code_class = AsmAlloc( sizeof( DEFAULT_CODE_CLASS ) + 1 );
        strcpy( Options.code_class, DEFAULT_CODE_CLASS );
    }
    /* set Options.text_seg based on module name */
    if( Options.text_seg == NULL ) {
        switch( ModuleInfo.model ) {
        case MOD_MEDIUM:
        case MOD_LARGE:
        case MOD_HUGE:
            len = strlen( ModuleInfo.name ) + strlen( SegmNames[SIM_CODE] ) + 1;
            Options.text_seg = AsmAlloc( len );
            strcpy( Options.text_seg, ModuleInfo.name );
            strcat( Options.text_seg, SegmNames[SIM_CODE] );
            break;
        default:
            p = SegmNames[SIM_CODE];

            Options.text_seg = AsmAlloc( strlen(p) + 1 );
            strcpy( Options.text_seg, p );
            break;
        }
    }
    /* set Options.data_seg */
    if( Options.data_seg == NULL ) {
        p = SegmNames[SIM_DATA];

        Options.data_seg = AsmAlloc( strlen( p ) + 1 );
        strcpy( Options.data_seg, p );
    }
    return;
}

void DefineFlatGroup( void )
/***********************/
{

    if( ModuleInfo.flatgrp_idx == 0 ) {
        flat_grp = CreateGroup( "FLAT" );
        if( flat_grp != NULL ) {
            flat_grp->sym.use32 = TRUE;
            ModuleInfo.flatgrp_idx = GetGrpIdx( &flat_grp->sym );
        }
    }
}

uint GetSegIdx( struct asm_sym *sym )
/*************************************/
/* get idx to sym's segment */
{
    if( sym == NULL )
        return( 0 );
    if( ((dir_node *)sym)->e.seginfo->segrec != NULL )
        return( ((dir_node *)sym)->e.seginfo->segrec->d.segdef.idx );
    return( 0 );
}

uint GetGrpIdx( struct asm_sym *sym )
/***********************************/
/* get index of sym's group */
{
    if( sym == NULL )
        return( 0 );
    return( ((dir_node *)sym)->e.grpinfo->idx );
}

extern struct asm_sym *GetGrp( struct asm_sym *sym )
/**************************************************/
/* get ptr to sym's group sym */
{
    dir_node            *curr;

    curr = GetSeg( sym );
    if( curr != NULL )
        return( curr->e.seginfo->group );
    return( NULL );
}

int SymIs32( struct asm_sym *sym )
/********************************/
/* get sym's offset size (32=1, 16=0) */
{
    dir_node            *curr;

    if ( sym->mem_type == MT_ABS )
        return( FALSE );

    curr = GetSeg( sym );
    if( curr == NULL ) {
        if( sym->state == SYM_EXTERNAL || sym->state == SYM_PROC || sym->state == SYM_GRP )
            return( sym->use32 );
        else if( sym->state == SYM_SEG  )
            return( ((dir_node *)sym)->e.seginfo->Use32 );
    } else {
        return( curr->e.seginfo->Use32 );
    }
    return( FALSE );
}

void SetSymSegOfs( struct asm_sym *sym )
/*******************************************/
{
    sym->segment = &GetCurrSeg()->sym;
    sym->offset = GetCurrOffset();
}

// called for .MODEL directive
// allowes to use simplified segment directives
// PushLineQueue() has already been called

ret_code SegmentModuleInit(int type)
{
    char        buffer[ MAX_ID_LEN+1+64 ];

    //lastseg.stack_size = 0;

    if (Parse_Pass == PASS_1) {
        /* Generates codes for code segment */
        AddLineQueue( SetSimSeg( SIM_CODE, Options.text_seg, buffer ) );
        AddLineQueue( get_sim_segm_end( buffer, Options.text_seg ) );

        /* Generates codes for data segment */
        AddLineQueue( SetSimSeg( SIM_DATA, Options.data_seg, buffer )) ;
        AddLineQueue( get_sim_segm_end( buffer, Options.data_seg ));

        /* create DGROUP for BIN/OMF if model isn't FLAT */
        if( type != MOD_FLAT &&
            (Options.output_format == OFORMAT_OMF ||
             Options.output_format == OFORMAT_BIN)) {
            /* Generates codes for grouping */
            strcpy( buffer, "DGROUP GROUP " );
            if( type == MOD_TINY ) {
                strcat( buffer, Options.text_seg );
                strcat( buffer, "," );
            }
            strcat( buffer, Options.data_seg );
            AddLineQueue( buffer );
        }
    }
    return( NOT_ERROR );
}

// END directive has been found, close all segments

ret_code SegmentModuleExit(void)
{
    /* if simplified segment directives are used,
     * don't complain if a segment is still open.
     */
    if( lastseg.seg != SIM_NONE ||
        (CurrSeg != NULL && ModuleInfo.model != MOD_NONE)) {
        DebugMsg(("SegmentModuleExit: segment index %u must be closed\n", lastseg.seg));
    } else if (CurrSeg) {
        AsmErr( BLOCK_NESTING_ERROR, CurrSeg->seg->sym.name );
    }

    /* clear segment stack */

    while(CurrSeg && (CloseSeg(CurrSeg->seg->sym.name) == NOT_ERROR));

    return( NOT_ERROR );
}

// init. called for each pass

void SegmentInit( int pass )
{
    dir_node    *curr;
    uint_32     i;
    struct asmfixup *fix;

    first_issued = 0;

    if ( pass == PASS_1 ) {
        segdefidx   = 0;
        grpdefidx   = 0;
        LnamesIdx   = 1; /* the first Lname is a null-string */
        lastseg.seg = SIM_NONE;
    }

    /* Reset length of all segments to zero */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if( ( curr->sym.state != SYM_SEG ) || ( curr->sym.segment == NULL ) )
            continue;
        if (curr->e.seginfo->CodeBuffer == NULL) {
            switch (Options.output_format) {
            case OFORMAT_OMF:
                curr->e.seginfo->CodeBuffer = codebuf;
                break;
            default: /* COFF, ELF, BIN */
                /* the segment can grow in step 2-n due to jump
                  modifications. worst case is no_of_short_jumps * 3 for 32bit.
                  for a quick solution just add 25% to the size if segment
                  contains labels */
                i = curr->e.seginfo->segrec->d.segdef.seg_length;
                if (curr->e.seginfo->labels)
                    i = i + (i >> 2);
                curr->e.seginfo->CodeBuffer = AsmAlloc(i);
                break;
            }
        }
        if( curr->e.seginfo->segrec->d.segdef.combine != COMB_STACK ) {
            curr->e.seginfo->segrec->d.segdef.seg_length = 0;
        }
        curr->e.seginfo->start_loc = 0;
        curr->e.seginfo->current_loc = 0;
#if BIN_SUPPORT
        curr->e.seginfo->initial = FALSE;
#endif

        if ( Options.output_format != OFORMAT_OMF ) {
            curr->e.seginfo->FixupListHeadGen = NULL;
            curr->e.seginfo->FixupListTailGen = NULL;
        }
    }

    ModuleInfo.Use32 = FALSE;

#if FASTPASS
    if ( pass != PASS_1 && UseSavedState == TRUE ) {
        i = saved_NumSegs;
        while(i) {
            i--;
            push_seg(*(saved_SegStack+i));
        }

        memcpy(&lastseg, &saved_lastseg, sizeof(last_seg_info));
        sym_CurSeg->string_ptr = saved_CurSeg_name;
        first_issued           = saved_first_issued;
    }
#endif
}
#if FASTPASS
void SegmentSaveState( void )
{
    int i;
    seg_item * curr;

    for (i = 0, curr = CurrSeg;curr;curr = curr->next, i++);

    saved_NumSegs = i;
    if (i) {
        saved_SegStack = AsmAlloc(i*sizeof(dir_node *));
        for (i = 0, curr = CurrSeg;curr;curr = curr->next, i++)
            *(saved_SegStack+i) = curr->seg;
    }

    memcpy(&saved_lastseg, &lastseg, sizeof(last_seg_info));
    saved_CurSeg_name = sym_CurSeg->string_ptr;
    saved_first_issued = first_issued;
}
#endif
