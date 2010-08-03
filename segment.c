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
*               - SEGMENT, ENDS, GROUP
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "segment.h"
#include "queues.h"
#include "expreval.h"
#include "omf.h"
#include "fastpass.h"
#include "coffspec.h"
#include "assume.h"
#include "listing.h"
#include "msgtext.h"

#include "myassert.h"

/* prototypes and externals */

extern symbol_queue     Tables[];       /* tables of definitions */
extern asm_sym          *sym_CurSeg;    /* @CurSeg symbol */
extern asm_sym          symPC;          /* '$' symbol */

#define INIT_ATTR       0x01
#define INIT_ALIGN      0x02
#define INIT_COMBINE    0x04
#define INIT_SEGSIZE    0x08
#define INIT_AT         0x10
#define INIT_FLAT       0x20
#define INIT_ALIGNPARAM 0x40
#define INIT_CHAR       0x80

static const typeinfo SegAttr[] = {
    { "READONLY",     0,              INIT_ATTR       },
    { "BYTE",         0,              INIT_ALIGN      },
    { "WORD",         1,              INIT_ALIGN      },
    { "DWORD",        2,              INIT_ALIGN      },
    { "PARA",         4,              INIT_ALIGN      },
    { "PAGE",         8,              INIT_ALIGN      },
#if PAGE4K
    { "PAGE4K",       12,             INIT_ALIGN      },
#endif
    { "ALIGN",        0,              INIT_ALIGN | INIT_ALIGNPARAM },
    { "PRIVATE",      COMB_INVALID,   INIT_COMBINE    },
    { "PUBLIC",       COMB_ADDOFF,    INIT_COMBINE    },
    { "STACK",        COMB_STACK,     INIT_COMBINE    },
    { "COMMON",       COMB_COMMON,    INIT_COMBINE    },
    { "MEMORY",       COMB_ADDOFF,    INIT_COMBINE    },
    { "AT",           COMB_INVALID,   INIT_COMBINE | INIT_AT },
    { "USE16",        USE16,          INIT_SEGSIZE    },
    { "USE32",        USE32,          INIT_SEGSIZE    },
#if AMD64_SUPPORT
    { "USE64",        USE64,          INIT_SEGSIZE    },
#endif
    { "FLAT",         USE32,          INIT_SEGSIZE | INIT_FLAT },
    { "INFO",         1,                                    INIT_CHAR },
    { "DISCARD",      IMAGE_SCN_MEM_DISCARDABLE >> 24,      INIT_CHAR },
    { "NOCACHE",      IMAGE_SCN_MEM_NOT_CACHED  >> 24,      INIT_CHAR },
    { "NOPAGE",       IMAGE_SCN_MEM_NOT_PAGED   >> 24,      INIT_CHAR },
    { "SHARED",       IMAGE_SCN_MEM_SHARED      >> 24,      INIT_CHAR },
    { "EXECUTE",      IMAGE_SCN_MEM_EXECUTE     >> 24,      INIT_CHAR },
    { "READ",         IMAGE_SCN_MEM_READ        >> 24,      INIT_CHAR },
    { "WRITE",        IMAGE_SCN_MEM_WRITE       >> 24,      INIT_CHAR }
};

uint                    segdefidx;      /* Number of segment definitions */
static uint             grpdefidx;      /* Number of group definitions   */
uint                    LnamesIdx;      /* Number of LNAMES definitions  */

dir_node                *CurrSeg;       /* currently active segment */
static dir_node         *SegStack[MAX_SEG_NESTING]; /* stack of open segments */
static int              stkindex;       /* current top of stack */

#if FASTPASS
/* saved state */
static dir_node         *saved_CurrSeg;
static dir_node         **saved_SegStack;
static int              saved_stkindex;
#endif

/* generic byte buffer, used for OMF LEDATA records only */
static uint_8           codebuf[ 1024 ];

#define ROUND_UP( i, r ) (((i)+((r)-1)) & ~((r)-1))

/* set CS assume entry whenever current segment is changed.
 * Also updates values of
 * - text macro @CurSeg
 * - assembly time variable $
 */

static void UpdateCurrSegVars( void )
/***********************************/
{
    assume_info     *info;

    info = &(SegAssumeTable[ ASSUME_CS ]);
    if( CurrSeg == NULL ) {
        info->symbol = NULL;
        info->flat = FALSE;
        info->error = TRUE;
        sym_CurSeg->string_ptr = "";
        symPC.segment = NULL;
    } else {
        info->flat = FALSE;
        info->error = FALSE;
        /* fixme: OPTION OFFSET:SEGMENT? */
        if( CurrSeg->e.seginfo->group != NULL ) {
            info->symbol = CurrSeg->e.seginfo->group;
            if ( info->symbol == &ModuleInfo.flat_grp->sym )
                info->flat = TRUE;
        } else {
            info->symbol = &CurrSeg->sym;
        }
        sym_CurSeg->string_ptr = CurrSeg->sym.name;
        symPC.segment = &CurrSeg->sym;
        DebugMsg(("UpdateCurrSegVars: current segment=%s\n", CurrSeg->sym.name));
    }
    return;
}

static void push_seg( dir_node *seg )
/***********************************/
/* Push a segment into the current segment stack */
{
    //pushitem( &CurrSeg, seg ); /* changed in v1.96 */
    if ( stkindex >= MAX_SEG_NESTING ) {
        AsmError( NESTING_LEVEL_TOO_DEEP );
        return;
    }
    SegStack[stkindex] = CurrSeg;
    stkindex++;
    CurrSeg = seg;
    UpdateCurrSegVars();
    return;
}

static void pop_seg( void )
/*************************/
/* Pop a segment out of the current segment stack */
{
    //seg = popitem( &CurrSeg ); /* changed in v1.96 */
    if ( stkindex == 0 ) {
        AsmError( BLOCK_NESTING_ERROR );
        return;
    }
    stkindex--;
    CurrSeg = SegStack[stkindex];
    UpdateCurrSegVars();
    return;
}

#if 0 /* v2.03: obsolete */

/* get a symbol's lname index. used by OMF output format only */

direct_idx GetLnameIdx( const char *name )
/****************************************/
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
#endif

/* add a class name to the queue of names */

static direct_idx InsertClassLname( const char *name )
/****************************************************/
{
    asm_sym *sym;

    if( strlen( name ) > MAX_LNAME ) {
        AsmError( CLASS_NAME_TOO_LONG );
        return( LNAME_NULL );
    }

    /* the classes aren't inserted into the symbol table
     but they are in a queue */

    sym = SymCreate( name, FALSE );
    sym->state = SYM_CLASS_LNAME;
    sym->idx = ++LnamesIdx;

    /* put it into the lname table */

    AddLnameData( sym );

    return( LnamesIdx );
}

uint_32 GetCurrOffset( void )
/***************************/
{
    if( CurrSeg )
        return( CurrSeg->e.seginfo->current_loc );
    return( 0 );
}

#if 0
dir_node *GetCurrSeg( void )
/**************************/
{
    return( CurrSeg );
}
#endif

#if 0
int GetCurrClass( void )
/***********************/
{
    if( CurrSeg == NULL )
        return( 0 );
    return( CurrSeg->e.seginfo->segrec->d.segdef.class_name_idx );
}
#endif

uint_32 GetCurrSegAlign( void )
/*****************************/
{
    if( CurrSeg == NULL )
        return( 0 );
    if ( CurrSeg->e.seginfo->alignment == MAX_SEGALIGNMENT ) /* ABS? */
        return( 0x10 ); /* assume PARA alignment for AT segments */
    return( 1 << CurrSeg->e.seginfo->alignment );
}

static dir_node *CreateGroup( const char *name )
/**********************************************/
{
    dir_node    *grp;

    grp = (dir_node *)SymSearch( name );

    if( grp == NULL || grp->sym.state == SYM_UNDEFINED ) {
        if (grp == NULL)
            grp = dir_create( name, SYM_GRP );
        else
            dir_settype( grp, SYM_GRP );
        grp->sym.list = TRUE;
        grp->e.grpinfo->grp_idx = ++grpdefidx;
        grp->e.grpinfo->lname_idx = ++LnamesIdx;
        AddLnameData( &grp->sym );
    } else if( grp->sym.state != SYM_GRP ) {
        AsmErr( SYMBOL_PREVIOUSLY_DEFINED, name );
        grp = NULL;
    }
    return( grp );
}

/* handle GROUP directive */

ret_code GrpDef( int i )
/**********************/
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
    if ( Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
        || Options.output_format == OFORMAT_ELF
#endif
       ) {
        AsmError( GROUP_DIRECTIVE_INVALID_FOR_COFF );
        return( ERROR );
    }

    name = AsmBuffer[0]->string_ptr;
    grp = CreateGroup( name );
    if( grp == NULL )
        return( ERROR );

    i++; /* go past GROUP */

    do {

        /* get segment name */
#if 0 /* v2: tokenizer handles dotnames ok */
        /* the tokenizer has problems with "dotnames" */
        if ( AsmBuffer[i]->token == T_DOT && AsmBuffer[i+1]->token == T_ID) {
            name = StringBufferEnd;
            *name = '.';
            i++;
            strcpy( name + 1, AsmBuffer[i]->string_ptr );
        } else if ( AsmBuffer[i]->token == T_ID ) {
#else
        if ( AsmBuffer[i]->token == T_ID ) {
#endif
            name = AsmBuffer[i]->string_ptr;
        } else {
            AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
            return( ERROR );
        }
        i++;

        seg = (dir_node *)SymSearch( name );
        if ( Parse_Pass == PASS_1 ) {
            if( seg == NULL ) {
                seg = dir_create( name, SYM_SEG );
                /* inherit the offset magnitude from the group */
                if ( grp->e.grpinfo->seglist )
                    seg->e.seginfo->Ofssize = grp->sym.Ofssize;
            } else if( seg->sym.state == SYM_UNDEFINED ) {
                dir_settype( seg, SYM_SEG );
                if ( grp->e.grpinfo->seglist )
                    seg->e.seginfo->Ofssize = grp->sym.Ofssize;
            } else if( seg->sym.state != SYM_SEG ) {
                AsmErr( SYMBOL_PREVIOUSLY_DEFINED, name );
                return( ERROR );
            } else if( seg->e.seginfo->group != NULL &&
                       seg->e.seginfo->group != &grp->sym ) {
                /* segment is in another group */
                DebugMsg(("GrpDef: segment >%s< is in group >%s< already\n", name, seg->e.seginfo->group->name));
                AsmErr( SEGMENT_IN_ANOTHER_GROUP, name );
                return( ERROR );
            }
            /* the first segment will define the group's word size */
            if( grp->e.grpinfo->seglist == NULL ) {
                grp->sym.Ofssize = seg->e.seginfo->Ofssize;
            } else if ( grp->sym.Ofssize != seg->e.seginfo->Ofssize ) {
                AsmErr( GROUP_SEGMENT_SIZE_CONFLICT, grp->sym.name, seg->sym.name );
                return( ERROR );
            }
        } else {
            if( seg == NULL || seg->sym.state != SYM_SEG || seg->sym.defined == FALSE ) {
                AsmErr( SEG_NOT_DEFINED, name );
                return( ERROR );
            }
        }

        /* insert segment in group if it's not there already */
        if ( seg->e.seginfo->group == NULL ) {

            /* set the segment's grp */
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

ret_code SetOfssize( void )
/*************************/
{
    if( CurrSeg == NULL ) {
        ModuleInfo.Ofssize = ModuleInfo.defOfssize;
    } else {
        ModuleInfo.Ofssize = CurrSeg->e.seginfo->Ofssize;
        if( ModuleInfo.Ofssize > USE16 && ( ( ModuleInfo.curr_cpu & P_CPU_MASK ) < P_386 ) ) {
            DebugMsg(("SetOfssize, error: CurrSeg=%Xh, ModuleInfo.Ofssize=%u, curr_cpu=%X, defOfssize=%u\n",
                      CurrSeg, ModuleInfo.Ofssize, ModuleInfo.curr_cpu, ModuleInfo.defOfssize ));
            AsmError( WRONG_CPU_FOR_32BIT_SEGMENT );
            return( ERROR );
        }
    }

    WordSize.value = (2 << ModuleInfo.Ofssize);

#if AMD64_SUPPORT
    Set64Bit( ModuleInfo.Ofssize == USE64 );
#endif

    return( NOT_ERROR );
}

/* close segment */

static ret_code CloseSeg( const char * name )
/*******************************************/
{
    //struct asm_sym      *sym;

    DebugMsg(("CloseSeg(%s) enter\n", name));

    if( CurrSeg == NULL || ( SymCmpFunc( CurrSeg->sym.name, name ) != 0 ) ) {
        DebugMsg(("CloseSeg(%s): nesting error, CurrSeg=%s\n", name, CurrSeg ? CurrSeg->sym.name : "(null)" ));
        AsmErr( BLOCK_NESTING_ERROR, name );
        return( ERROR );
    }

    DebugMsg(("CloseSeg(%s): current ofs=%X\n", name, CurrSeg->e.seginfo->current_loc));

    if ( write_to_file && ( Options.output_format == OFORMAT_OMF ) )

        //if ( !omf_FlushCurrSeg() ) /* v2: error check is obsolete */
        //    AsmErr( INTERNAL_ERROR, "CloseSeg", 1 ); /* coding error! */
        omf_FlushCurrSeg();

    pop_seg();

    return( NOT_ERROR );
}

void DefineFlatGroup( void )
/**************************/
{
    if( ModuleInfo.flat_grp == NULL ) {
        ModuleInfo.flat_grp = CreateGroup( "FLAT" );
        if( ModuleInfo.flat_grp != NULL ) {
            ModuleInfo.flat_grp->sym.Ofssize = ModuleInfo.defOfssize;
            //ModuleInfo.flatgrp_idx = ModuleInfo.flat_grp->e.grpinfo->grp_idx;
        }
    }
}

uint GetSegIdx( struct asm_sym *sym )
/***********************************/
/* get idx to sym's segment */
{
    if( sym == NULL )
        return( 0 );
    if( ((dir_node *)sym)->e.seginfo->segrec != NULL )
        return( ((dir_node *)sym)->e.seginfo->segrec->d.segdef.idx );
    return( 0 );
}

struct asm_sym *GetGroup( struct asm_sym *sym )
/*********************************************/
/* get a symbol's group */
{
    dir_node            *curr;

    curr = GetSeg( sym );
    if( curr != NULL )
        return( curr->e.seginfo->group );
    return( NULL );
}

int GetSymOfssize( struct asm_sym *sym )
/**************************************/
/* get sym's offset size (64=2, 32=1, 16=0) */
{
    dir_node            *curr;

    if ( sym->mem_type == MT_ABS )
        return( USE16 );

    curr = GetSeg( sym );
    if( curr == NULL ) {
        if( sym->state == SYM_EXTERNAL || ( sym->state == SYM_INTERNAL && sym->isproc ) || sym->state == SYM_GRP )
            return( sym->Ofssize );
        else if( sym->state == SYM_SEG  )
            return( ((dir_node *)sym)->e.seginfo->Ofssize );
    } else {
        return( curr->e.seginfo->Ofssize );
    }
    return( ModuleInfo.Ofssize );
}

void SetSymSegOfs( struct asm_sym *sym )
/**************************************/
{
    sym->segment = &CurrSeg->sym;
    sym->offset = GetCurrOffset();
}


static seg_type TypeFromClassName( dir_node * dir, const char *name )
/*******************************************************************/
{
    int     slen;
    char    uname[MAX_ID_LEN+1];

    if ( dir->e.seginfo->alignment == MAX_SEGALIGNMENT )
        return( SEGTYPE_ABS );

    /* v2.03: added */
    if ( dir->e.seginfo->segrec->d.segdef.combine == COMB_STACK )
        return( SEGTYPE_STACK );

    if( name == NULL )
        return( SEGTYPE_UNDEF );

    if( _stricmp( name, GetCodeClass() ) == 0 )
        return( SEGTYPE_CODE );

    slen = strlen( name );
    strcpy( uname, name );
    _strupr( uname );
    switch( slen ) {
    default:
    case 5:
        if( memcmp( uname, "CONST", 6 ) == 0 )
            return( SEGTYPE_DATA );
        //if( memcmp( uname, "STACK", 6 ) == 0 )
        //    return( SEGTYPE_DATA );
        if( memcmp( uname, "DBTYP", 6 ) == 0 )
            return( SEGTYPE_DATA );
        if( memcmp( uname, "DBSYM", 6 ) == 0 )
            return( SEGTYPE_DATA );
    case 4:
        /* v2.03: changed */
        //if( memcmp( uname , "CODE", 5 ) == 0 )
        //    return( SEGTYPE_CODE );
        if( memcmp( uname + slen - 4, "CODE", 4 ) == 0 )
            return( SEGTYPE_CODE );
        if( memcmp( uname + slen - 4, "DATA", 4 ) == 0 )
            return( SEGTYPE_DATA );
    case 3:
        if( memcmp( uname + slen - 3, "BSS", 3 ) == 0 )
            return( SEGTYPE_BSS );
    case 2:
    case 1:
    case 0:
        return( SEGTYPE_UNDEF );
    }
}

#if 0   /* v2.03: obsolete */

/* get the type of the segment by checking it's name.
 * this is called only if the class gives no hint
 */
static seg_type TypeFromSegmentName( const char *name )
/*****************************************************/
{
    int     slen;
    char    uname[MAX_ID_LEN+1];

    slen = strlen( name );
    strcpy( uname, name );
    _strupr( uname );
    switch( slen ) {
    default:
    case 5:
        if ( Options.output_format != OFORMAT_COFF ) {
            /* '..._TEXT'? */
            if( memcmp( uname + slen - 5, SegmNamesDef[SIM_CODE], 5 ) == 0 )
                return( SEGTYPE_CODE );
        }
        /* '..._DATA' */
        if( memcmp( uname + slen - 5, SegmNamesDef[SIM_DATA], 5 ) == 0 )
            return( SEGTYPE_DATA );
        /* 'CONST' */
        if( memcmp( uname + slen - 5, SegmNamesDef[SIM_CONST], 5 ) == 0 )
            return( SEGTYPE_DATA );
    case 4:
        if ( Options.output_format != OFORMAT_COFF ) {
            /* '..._BSS' */
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
#endif

/* set the segment's class. report an error if the class has been set
 * already and the new value differs. */

static direct_idx SetSegmentClass( asm_sym *seg, const char *classname )
/**********************************************************************/
{
    direct_idx          classidx;

    classidx = FindLnameIdx( classname );
    if( classidx == LNAME_NULL ) {
        classidx = InsertClassLname( classname );
        if( classidx == LNAME_NULL ) {
            return( ERROR );
        }
    }
    /* default class name index is 1, which is the NULL class name */
    if ( ((dir_node *)seg)->e.seginfo->segrec->d.segdef.class_name_idx == 1 )
        ((dir_node *)seg)->e.seginfo->segrec->d.segdef.class_name_idx = classidx;
    else if ( ((dir_node *)seg)->e.seginfo->segrec->d.segdef.class_name_idx != classidx ) {
        AsmErr( SEGDEF_CHANGED, seg->name, MsgGetEx( TXT_CLASS ) );
        return( ERROR );
    }
    return( classidx );
}

/* CreateSegment(): used to define the codeview debugging segments
 */

asm_sym *CreateSegment( const char *name, const char *classname, uint_8 alignment, uint_8 Ofssize )
/*************************************************************************************************/
{
    dir_node *seg;
    if ( seg = dir_create( name, SYM_SEG ) ) {
        seg->e.seginfo->segrec->d.segdef.idx = ++segdefidx;
        seg->sym.segment = &seg->sym;
        seg->e.seginfo->alignment = alignment;
        seg->e.seginfo->Ofssize = Ofssize;
        SetSegmentClass( (asm_sym *)seg, classname );
        if( seg->e.seginfo->lname_idx == 0 )
            seg->e.seginfo->lname_idx = ++LnamesIdx;
        AddLnameData( &seg->sym );
        return( &seg->sym );
    }
    return( NULL );
}

/* ENDS directive */

ret_code EndsDir( int i )
/***********************/
{
    /* a label must precede ENDS */
    if( i != 1 ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
        return( ERROR );
    }
    if ( Parse_Pass != PASS_1 ) {
        if ( ModuleInfo.list )
            LstWrite( LSTTYPE_LABEL, 0, NULL );
    }
    if ( CloseSeg( AsmBuffer[0]->string_ptr ) == ERROR )
        return( ERROR );
    i++;
    if ( AsmBuffer[i]->token != T_FINAL ) {
        AsmErr( SYNTAX_ERROR_EX, AsmBuffer[i]->string_ptr );
    }
    return( SetOfssize() );
}

/* SEGMENT directive if pass is > 1 */

ret_code SetCurrSeg( int i )
/**************************/
{
    struct asm_sym *sym;

    sym = SymSearch( AsmBuffer[0]->string_ptr );
    DebugMsg(("SetCurrSeg(%s) sym=%X\n", AsmBuffer[0]->string_ptr, sym));
    if ( sym == NULL || sym->state != SYM_SEG ) {
        AsmErr( SEG_NOT_DEFINED, AsmBuffer[0]->string_ptr );
        return( ERROR );
    }
    if ( CurrSeg && Options.output_format == OFORMAT_OMF )
        omf_FlushCurrSeg();
    push_seg( (dir_node *)sym );

    if ( ModuleInfo.list )
        LstWrite( LSTTYPE_LABEL, 0, NULL );

    return( SetOfssize() );
}

/* SEGMENT directive (Pass ONE only!) */

ret_code SegmentDir( int i )
/**************************/
{
    char                defined;
    char                *token;
    obj_rec             *seg;
//    obj_rec             *oldobj;
    const typeinfo      *type;          /* type of option */
    int                 temp;
    int                 temp2;
    uint                initstate = 0;  /* flags for attribute initialization */
    unsigned char       oldreadonly;    /* readonly value of a defined segment */
    //unsigned char       oldsegtype;
    unsigned char       oldOfssize;
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

    DebugMsg(("SegDef enter, segment=%s, cmd=%s, ModuleInfo.Ofssize=%u\n", name, AsmBuffer[i]->string_ptr, ModuleInfo.Ofssize ));

    /* See if the segment is already defined */
    sym = SymSearch( name );
    if( sym == NULL || sym->state == SYM_UNDEFINED ) {
        /* segment is not defined (yet) */
        if ( sym == NULL ) {
            sym = (asm_sym *)dir_create( name, SYM_SEG );
        } else
            dir_settype( (dir_node *)sym, SYM_SEG );
        sym->list = TRUE; /* always list segments */
        dir = (dir_node *)sym;
        seg = dir->e.seginfo->segrec;
        seg->d.segdef.idx = ++segdefidx;
        defined = FALSE;
        /*
         * initialize segment with values from the one without suffix
         */
        if (Options.output_format == OFORMAT_COFF
#if ELF_SUPPORT
            || Options.output_format == OFORMAT_ELF
#endif
           ) {
            char * p;
            if ( p = strchr(sym->name, '$') ) {
                char buffer[MAX_ID_LEN+1];
                dir_node *dir2;
                memcpy(buffer, sym->name, p - sym->name);
                buffer[p - sym->name] = NULLC;
                if ((dir2 = (dir_node *)SymSearch(buffer)) && dir2->sym.state == SYM_SEG) {
                    dir->e.seginfo->readonly = dir2->e.seginfo->readonly;
                    dir->e.seginfo->segtype  = dir2->e.seginfo->segtype;
                    dir->e.seginfo->Ofssize  = dir2->e.seginfo->Ofssize;
                    dir->e.seginfo->alignment= dir2->e.seginfo->alignment;
                    dir->e.seginfo->characteristics = dir2->e.seginfo->characteristics;
                    dir->e.seginfo->segrec->d.segdef.combine        = dir2->e.seginfo->segrec->d.segdef.combine;
                    dir->e.seginfo->segrec->d.segdef.class_name_idx = dir2->e.seginfo->segrec->d.segdef.class_name_idx;
                }
            }
        }
    } else if ( sym->state == SYM_SEG ) {
        /* segment already defined */
        dir = (dir_node *)sym;
        seg = dir->e.seginfo->segrec;
        defined = TRUE;
        oldreadonly = dir->e.seginfo->readonly;
        //oldsegtype  = dir->e.seginfo->segtype;
        oldOfssize  = dir->e.seginfo->Ofssize;
        oldalign    = dir->e.seginfo->alignment;
        oldcharacteristics = dir->e.seginfo->characteristics;
        oldcombine  = dir->e.seginfo->segrec->d.segdef.combine;
        oldclassidx = dir->e.seginfo->segrec->d.segdef.class_name_idx;
        if( dir->e.seginfo->lname_idx == 0 ) {
            /* segment was mentioned in a group statement, but not really set up */
            defined = FALSE;
            seg->d.segdef.idx = ++segdefidx;
        }
    } else {
        /* symbol is different kind, error */
        DebugMsg(("SegDef: symbol redefinition\n"));
        AsmErr( SYMBOL_REDEFINITION, name );
        return( ERROR );
    }

    i++; /* go past SEGMENT */

    for( ; i < Token_Count; i++ ) {
        token = AsmBuffer[i]->string_ptr;
        DebugMsg(("SegDef: i=%u, string=%s token=%X\n", i, token, AsmBuffer[i]->token ));
        if( AsmBuffer[i]->token == T_STRING ) {

            /* the class name - the only token which is of type STRING */
            /* string must be delimited by [double]quotes */
            if ( AsmBuffer[i]->string_delim != '"' &&
                AsmBuffer[i]->string_delim != '\'' ) {
                AsmErr( SYNTAX_ERROR_EX, token );
                continue;
            }
            /* remove the quote delimiters */
            token++;
            *(token+AsmBuffer[i]->value) = NULLC;

            SetSegmentClass( &dir->sym, token );

            DebugMsg(("SegDef: class found: %s\n", token ));
            continue;
        }

        /* check the rest of segment attributes.
         */
        type = FindToken( token, SegAttr, sizeof(SegAttr)/sizeof(typeinfo) );
        if( type == NULL ) {
            AsmErr( UNKNOWN_SEGMENT_ATTRIBUTE, token );
            continue;
        }

        /* initstate is used to check if any field is already
         * initialized
         */
        if( ( type->init != INIT_CHAR ) && ( initstate & type->init ) ) {
            AsmErr( SEGMENT_ATTRIBUTE_DEFINED_ALREADY, token );
            continue;
        } else {
            initstate |= type->init; /* mark it initialized */
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
                AsmErr( EXPECTED, "(" );
                continue;
            }
            i++;
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) == ERROR )
                continue;
            if ( AsmBuffer[i]->token != T_CL_BRACKET ) {
                AsmErr( EXPECTED, ")" );
                continue;
            }
            if ( opndx.kind != EXPR_CONST || opndx.string != NULL ) {
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
            dir->e.seginfo->alignment = -1;
            i++;
            if ( EvalOperand( &i, Token_Count, &opndx, TRUE ) != ERROR ) {
                if ( opndx.kind == EXPR_CONST && opndx.string == NULL ) {
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
#if AMD64_SUPPORT
            dir->e.seginfo->Ofssize = ModuleInfo.defOfssize;
#else
            dir->e.seginfo->Ofssize = USE32;
#endif
            dir->e.seginfo->group = &ModuleInfo.flat_grp->sym;
        } else if ( type->init & INIT_SEGSIZE ) {
            dir->e.seginfo->Ofssize = type->value;
        } else if( type->init & INIT_CHAR ) {
            DebugMsg(("SegDef: characteristics found\n" ));
            ; /* characteristics are restricted to COFF/ELF */
            if ( Options.output_format == OFORMAT_OMF || Options.output_format == OFORMAT_BIN ) {
                AsmErr( NOT_SUPPORTED_WITH_CURR_FORMAT, AsmBuffer[i]->string_ptr );
                continue;
            }
            dir->e.seginfo->characteristics |= type->value;
        }
    } /* end for */

    /* make a guess about the segment's type */
    if( dir->e.seginfo->segtype != SEGTYPE_CODE ) {
        seg_type res;

        token = GetLname( seg->d.segdef.class_name_idx );
        res = TypeFromClassName( dir, token );
        if( res != SEGTYPE_UNDEF ) {
            dir->e.seginfo->segtype = res;
        }
#if 0
        else {
            res = TypeFromSegmentName( name );
            dir->e.seginfo->segtype = res;
        }
#endif
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
        else if ( oldOfssize  != dir->e.seginfo->Ofssize )
            txt = TXT_SEG_WORD_SIZE;
        else if ( oldclassidx != seg->d.segdef.class_name_idx )
            txt = TXT_CLASS;
        else if ( oldcharacteristics != dir->e.seginfo->characteristics )
            txt = TXT_CHARACTERISTICS;

        if ( txt ) {
            DebugMsg(("seg attr changed: %s, %s\n", dir->sym.name, MsgGetEx( txt ) ));
            AsmErr( SEGDEF_CHANGED, dir->sym.name, MsgGetEx( txt ) );
            //return( ERROR ); /* v2: display error, but continue */
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
        AddLnameData( sym );
    }

    return( SetOfssize() );
}

/* END directive has been found. */

ret_code SegmentModuleExit( void )
/********************************/
{
    /* close current segment if model is set.
     * This strategy is sufficient for segments opened with
     * simplified segment directives.
     */
    if ( CurrSeg && ModuleInfo.model != MOD_NONE )
        CloseSeg( CurrSeg->sym.name );

    /* if there's still an open segment, it's an error */
    if ( CurrSeg ) {
        AsmErr( BLOCK_NESTING_ERROR, CurrSeg->sym.name );
        /* but close the still open segments anyway */
        while( CurrSeg && ( CloseSeg( CurrSeg->sym.name ) == NOT_ERROR ) );
    }

    return( NOT_ERROR );
}

/* init. called for each pass */

void SegmentInit( int pass )
/**************************/
{
    dir_node    *curr;
    uint_32     i;
    //int         size;
    //struct genfixup *fix;

    DebugMsg(("SegmentInit(%u) enter\n", pass ));
    CurrSeg      = NULL;
    stkindex     = 0;

    if ( pass == PASS_1 ) {
        segdefidx   = 0;
        grpdefidx   = 0;
        LnamesIdx   = 1; /* the first Lname is a null-string */
        //flat_grp    = NULL;
#if 0 /* v2.03: obsolete, also belongs to simplified segment handling */
        /* set ModuleInfo.code_class */
        if( Options.code_class  )
            size = strlen( Options.code_class ) + 1;
        else
            size = 4 + 1;
        ModuleInfo.code_class = AsmAlloc( size );
        if ( Options.code_class )
            strcpy( ModuleInfo.code_class, Options.code_class );
        else
            strcpy( ModuleInfo.code_class, "CODE" );
#endif
    }

    /* Reset length of all segments to zero */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if( ( curr->sym.state != SYM_SEG ) || ( curr->sym.segment == NULL ) )
            continue;
        if (curr->e.seginfo->CodeBuffer == NULL && curr->e.seginfo->bytes_written ) {
            switch (Options.output_format) {
            case OFORMAT_OMF:
                /* OMF needs just one global buffer */
                curr->e.seginfo->CodeBuffer = codebuf;
                break;
            default: /* COFF, ELF, BIN */
                /* added for v1.96*/
                //if ( curr->e.seginfo->bytes_written == 0 )
                //    break;
                //i = curr->sym.max_offset;
                i = curr->sym.max_offset - curr->e.seginfo->start_loc;
                /* the segment can grow in step 2-n due to forward references.
                 * for a quick solution just add 25% to the size if segment
                 * is a code segment. (v2.02: previously if was added only if
                 * code segment contained labels, but this isn't sufficient.)
                 */
                //if ( curr->e.seginfo->labels ) /* v2.02: changed */
                if ( curr->e.seginfo->segtype == SEGTYPE_CODE )
                    i = i + (i >> 2);
                DebugMsg(("SegmentInit(%u), %s: max_ofs=%lX, alloc_size=%lXh\n", pass, curr->sym.name, curr->sym.max_offset, i ));
                curr->e.seginfo->CodeBuffer = AsmAlloc(i);
#if FASTMEM==0
                /* fastmem clears the memory blocks, but malloc() won't */
                memset( curr->e.seginfo->CodeBuffer, 0, i );
#endif
                break;
            }
        }
        if( curr->e.seginfo->segrec->d.segdef.combine != COMB_STACK ) {
            curr->sym.max_offset = 0;
        }
        if ( Options.output_format == OFORMAT_OMF ) { /* v2.03: do this selectively */
            curr->e.seginfo->start_loc = 0;
            curr->e.seginfo->data_in_code = FALSE;
        }
        curr->e.seginfo->current_loc = 0;
        curr->e.seginfo->bytes_written = 0;

        //if ( Options.output_format != OFORMAT_OMF ) {
            curr->e.seginfo->FixupListHeadGen = NULL;
            curr->e.seginfo->FixupListTailGen = NULL;
        //}
    }

    ModuleInfo.Ofssize = USE16;

#if FASTPASS
    if ( pass != PASS_1 && UseSavedState == TRUE ) {
        CurrSeg = saved_CurrSeg;
        stkindex = saved_stkindex;
        if ( stkindex )
            memcpy( &SegStack, saved_SegStack, stkindex * sizeof(dir_node *) );

        //sym_CurSeg->string_ptr = saved_CurSeg_name;

        UpdateCurrSegVars();
    }
#endif
}
#if FASTPASS
void SegmentSaveState( void )
/***************************/
{
    int i;

    i = stkindex;

    saved_CurrSeg = CurrSeg;
    saved_stkindex = stkindex;
    if ( stkindex ) {
        saved_SegStack = AsmAlloc( stkindex * sizeof(dir_node *) );
        memcpy( saved_SegStack, &SegStack, stkindex * sizeof(dir_node *) );
    }

    //saved_CurSeg_name  = sym_CurSeg->string_ptr;
}
#endif
