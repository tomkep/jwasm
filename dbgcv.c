/***************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  Generate CodeView symbolic debug info ( Version 4 format )
*
****************************************************************************/

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "directiv.h"
#include "segment.h"
#include "fixup.h"
#include "omf.h"
#include "tokenize.h"
#include "queues.h"
#include "coff.h"
#include "dbgcv.h"

#if COFF_SUPPORT
extern qdesc DebugS;
extern qdesc DebugT;
#endif

static uint_16 currtype; /* index user defined types */

typedef union {
    struct cv_primitive_type s;
    uint_16 uvalue;
} cv_typeref_u;

#ifdef DEBUG_OUT
static const char szCVCompiler[] = { "Microsoft (R) Macro Assembler Version 6.15.8803" };
#else
static const char szCVCompiler[] = { "JWasm v" _JWASM_VERSION_ };
#endif

#define SetPrefixName( p, name, len ) { *p++ = len; memcpy( p, name, len ); p += len; }


/* translate symbol's mem_type to a codeview typeref */

static uint_16 GetTyperef( struct asm_sym *sym, uint_8 Ofssize )
/**************************************************************/
{
    cv_typeref_u value = { CV_PDS_SPECIAL_NO_TYPE, 0, CV_PDT_SPECIAL, CV_PDM_DIRECT, 0 };
    int size = SizeFromMemtype( sym->mem_type, Ofssize );

    if ( ( sym->mem_type & MT_SPECIAL ) == 0 && sym->mem_type != MT_FWORD ) {
        if ( sym->mem_type & MT_FLOAT ) {
            value.s.type = CV_PDT_REAL;
            switch ( size ) {
            case 4:  value.s.size = CV_PDS_REAL_32BIT; break;
            case 8:  value.s.size = CV_PDS_REAL_64BIT; break;
            case 10: value.s.size = CV_PDS_REAL_80BIT; break;
            }
        } else {
            if ( sym->mem_type & MT_SIGNED )
                value.s.type = CV_PDT_SIGNED_INTEGRAL;
            else
                value.s.type = CV_PDT_UNSIGNED_INTEGRAL;
            switch ( size ) {
            case 1:  value.s.size = CV_PDS_INTEGRAL_1BYTE; break;
            case 2:  value.s.size = CV_PDS_INTEGRAL_2BYTE; break;
            case 4:  value.s.size = CV_PDS_INTEGRAL_4BYTE; break;
            case 8:  value.s.size = CV_PDS_INTEGRAL_8BYTE; break;
            //case 16: value.s.size = CV_PDS_INTEGRAL_16BYTE; break;
            }
        }
    } else {
        switch ( sym->mem_type ) {
        case MT_ABS:  break;
        case MT_PTR:  break;
        case MT_BITS:
            if ( sym->cv_typeref )
                return( sym->cv_typeref );
            break;
        case MT_NEAR: value.s.mode = CV_PDM_NEARPTR; break;
        case MT_FAR:  value.s.mode = CV_PDM_FARPTR; break;
        case MT_TYPE:
            for ( sym = sym->type; sym->type; sym = sym->type );
            if ( sym->cv_typeref )
                return( sym->cv_typeref );
            return( GetTyperef( sym, Ofssize ) );
            break;
        }
    }

    return( value.uvalue );
}

/* calc size of a codeview item in symbols segment */

static uint_16 GetCVStructLen( struct asm_sym *sym, uint_8 Ofssize )
/******************************************************************/
{
    uint_16 len;
    switch ( sym->state ) {
    case SYM_TYPE:
        len = sizeof( struct cv_symrec_udt );
        break;
    case SYM_STACK:
        if ( Ofssize == USE16 )
            len = sizeof( struct cv_symrec_bprel16 );
        else
            len = sizeof( struct cv_symrec_bprel32 );
        break;
    default:
        if ( sym->isproc ) {
            if ( Ofssize == USE16 )
                len = sizeof( struct cv_symrec_lproc16 );
            else
                len = sizeof( struct cv_symrec_lproc32 );
        } else if ( sym->mem_type == MT_NEAR || sym->mem_type == MT_FAR ) {
            if ( Ofssize == USE16 )
                len = sizeof( struct cv_symrec_label16 );
            else
                len = sizeof( struct cv_symrec_label32 );
        } else {
            if ( Ofssize == USE16 )
                len = sizeof( struct cv_symrec_ldata16 );
            else
                len = sizeof( struct cv_symrec_ldata32 );
        }
    }
    return( len );
}

/* flush the segment buffer for symbols and types.
 * For OMF, the buffer is written to disk. For COFF, it's
 * more complicated, because we cannot write now, but also
 * don't know yet the final size of the segment. So a linked
 * list of buffer items has to be created. The contents will
 * later be written inside coff_write_header().
 */

static uint_8 *checkflush( struct dir_node *seg, uint_8 *buffer, uint_8 *curr, int size )
/***************************************************************************************/
{
#if COFF_SUPPORT
    uint_8 *p;
#endif

    if ( ( curr - buffer ) && ( ( curr - buffer ) + size ) > ( 1024 - 8 ) ) {
        switch ( Options.output_format ) {
#if COFF_SUPPORT
        case OFORMAT_COFF:
            p = AsmAlloc( (curr - buffer) + sizeof( struct qditem ) );
            ((struct qditem *)p)->next = NULL;
            ((struct qditem *)p)->size = curr - buffer;
            memcpy( p + sizeof( struct qditem ), buffer, curr - buffer );
            if ( seg->sym.name[7] == 'S' ) {  /* .debug$S or .debug$T ? */
                if ( DebugS.head == NULL )
                    DebugS.head = DebugS.tail = p;
                else {
                    ((struct qditem *)(DebugS.tail))->next = p;
                    DebugS.tail = p;
                }
            } else {
                if ( DebugT.head == NULL )
                    DebugT.head = DebugT.tail = p;
                else {
                    ((struct qditem *)(DebugT.tail))->next = p;
                    DebugT.tail = p;
                }
            }
            seg->e.seginfo->current_loc = seg->e.seginfo->start_loc + ( curr - buffer );
            seg->e.seginfo->start_loc = seg->e.seginfo->current_loc;
            break;
#endif
        case OFORMAT_OMF:
            seg->e.seginfo->current_loc = seg->e.seginfo->start_loc + ( curr - buffer );
            omf_write_ledata( seg );
            break;
        }
        return( buffer );
    }
    return( curr );
}

static void PadBytes( uint_8 *curr, uint_8 *base )
/************************************************/
{
    static const char padtab[] = { LF_PAD1, LF_PAD2, LF_PAD3 };
    while( ( curr - base ) & 3 )
        *curr++ = padtab[3-((curr - base) & 3)];
}

static unsigned GetFieldListSize( dir_node *type )
/************************************************/
{
    field_list  *curr;
    unsigned    size = 0;
    unsigned    numsize;
    for ( curr = type->e.structinfo->head; curr; curr = curr->next ) {
        numsize = 0;
        if ( curr->sym->offset >= 0x8000 )
            numsize = sizeof( uint_32 );
        size += sizeof( struct cv_typerec_member ) + numsize + curr->sym->name_size + 1;
        size = ( size + 3 ) & ~3;
    }
    return( size );
}

/* write a bitfield to $$TYPES */

static uint_8 * WriteBitfield( dir_node *types, dir_node *type, struct asm_sym *sym, uint_8 *pt )
/***********************************************************************************************/
{
    pt = checkflush( types, (uint_8 *)CurrSource, pt, sizeof( struct cv_typerec_bitfield ) );
    sym->cv_typeref = currtype++;
    ((struct cv_typerec_bitfield *)pt)->tr.size = sizeof( struct cv_typerec_bitfield ) - sizeof(uint_16);
    ((struct cv_typerec_bitfield *)pt)->tr.leaf = LF_BITFIELD;
    ((struct cv_typerec_bitfield *)pt)->length = sym->total_size;
    ((struct cv_typerec_bitfield *)pt)->position = sym->offset;
    ((struct cv_typerec_bitfield *)pt)->type = GetTyperef( (struct asm_sym *)type, USE16 );
    pt += sizeof ( struct cv_typerec_bitfield );
    return( pt );
}

#ifdef DEBUG_OUT
#define GetTPos() (types->e.seginfo->current_loc + (pt - CurrSource))
#endif

/* write a type to $$TYPES. Items are dword-aligned */

static uint_8 * WriteType( dir_node *types, struct asm_sym *sym, uint_8 *pt )
/***************************************************************************/
{
    dir_node    *type = (dir_node *)sym;
    uint_8      *tmp;
    field_list  *curr;
    int         typelen = 0;
    int         i;
    int         size;
    uint_16     cnt = 0;

    if ( type->e.structinfo->typekind == TYPE_TYPEDEF ) /* filter typedefs */
        return( pt );

    /* remaining: structs, unions and records */

    if ( sym->total_size >= 0x8000 )
        typelen = sizeof( uint_32 );

    /* Count the member fields. If a member's type is unknown, create it! */
    for ( curr = type->e.structinfo->head; curr; curr = curr->next, cnt++ ) {
        if ( curr->sym->mem_type == MT_TYPE && curr->sym->type->cv_typeref == 0 ) {
            pt = WriteType( types, curr->sym->type, pt );
        } else if ( curr->sym->mem_type == MT_BITS && curr->sym->cv_typeref == 0 ) {
            pt = WriteBitfield( types, type, curr->sym, pt );
        }
    }

    sym->cv_typeref = currtype++;
    switch ( type->e.structinfo->typekind ) {
    case TYPE_UNION:
        DebugMsg(("WriteType(%Xh, ref=%X): UNION=%s\n", GetTPos(), sym->cv_typeref, sym->name ));
        size = ( sizeof( struct cv_typerec_union ) + typelen + 1 + sym->name_size + 3 ) & ~3;
        pt = checkflush( types, (uint_8 *)CurrSource, pt, size );
        ((struct cv_typerec_union *)pt)->tr.size = size - sizeof(uint_16);
        ((struct cv_typerec_union *)pt)->tr.leaf = LF_UNION;
        ((struct cv_typerec_union *)pt)->count = cnt;
        ((struct cv_typerec_union *)pt)->field = currtype++;
        ((struct cv_typerec_union *)pt)->property = 0;
        if ( typelen != 0 ) {
            ((struct cv_typerec_union *)pt)->length = LF_ULONG;
            tmp = pt + sizeof( struct cv_typerec_union );
            *(uint_32 *)tmp = sym->total_size;
            tmp += sizeof( uint_32 );
        } else {
            ((struct cv_typerec_union *)pt)->length = sym->total_size;
            tmp = pt + sizeof( struct cv_typerec_union );
        }
        break;
    case TYPE_RECORD:
    case TYPE_STRUCT:
        DebugMsg(("WriteType(%Xh, ref=%X): STRUCT=%s\n", GetTPos(), sym->cv_typeref, sym->name ));
        size = ( sizeof( struct cv_typerec_structure ) + typelen + 1 + sym->name_size + 3 ) & ~3;
        pt = checkflush( types, (uint_8 *)CurrSource, pt, size );
        ((struct cv_typerec_structure *)pt)->tr.size = size - sizeof(uint_16);
        ((struct cv_typerec_structure *)pt)->tr.leaf = LF_STRUCTURE;
        ((struct cv_typerec_structure *)pt)->count = cnt;
        ((struct cv_typerec_structure *)pt)->field = currtype++;
        if ( type->e.structinfo->typekind == TYPE_RECORD )
            ((struct cv_typerec_structure *)pt)->property = 1; /* is "packed" */
        else
            ((struct cv_typerec_structure *)pt)->property = 0;
        ((struct cv_typerec_structure *)pt)->dList = 0;
        ((struct cv_typerec_structure *)pt)->vshape = 0;
        if ( typelen != 0 ) {
            ((struct cv_typerec_structure *)pt)->length = LF_ULONG;
            tmp = pt + sizeof( struct cv_typerec_structure );
            *(uint_32 *)tmp = sym->total_size;
            tmp += sizeof( uint_32 );
        } else {
            ((struct cv_typerec_structure *)pt)->length = sym->total_size;
            tmp = pt + sizeof( struct cv_typerec_structure );
        }
    }
    SetPrefixName( tmp, sym->name, sym->name_size );
    PadBytes( tmp, (uint_8 *)CurrSource );
    pt += size;

    pt = checkflush( types, (uint_8 *)CurrSource, pt, sizeof( struct cv_typerec_fieldlist ) );
    size = sizeof( struct cv_typerec_fieldlist) + GetFieldListSize( type );
    ((struct cv_typerec_fieldlist *)pt)->tr.size = size - sizeof(uint_16);
    ((struct cv_typerec_fieldlist *)pt)->tr.leaf = LF_FIELDLIST;
    DebugMsg(("WriteType(%Xh, ref=%X): FIELDLIST, size=%u\n", GetTPos(), currtype-1, size ));
    pt += sizeof( struct cv_typerec_fieldlist );

    /* add the struct's members to the fieldlist */
    for ( i = cnt, curr = type->e.structinfo->head; i; curr = curr->next, i-- ) {
        typelen = 0;
        if ( curr->sym->offset >= 0x8000 )
            typelen += sizeof( uint_32 );
        size = ( sizeof( struct cv_typerec_member ) + typelen + 1 + curr->sym->name_size + 3 ) & ~3;
        pt = checkflush( types, (uint_8 *)CurrSource, pt, size );
        ((struct cv_typerec_member *)pt)->leaf = LF_MEMBER;
        ((struct cv_typerec_member *)pt)->type = GetTyperef( curr->sym, USE16 );
        ((struct cv_typerec_member *)pt)->attribute.access = CV_ATTR_ACC_PUBLIC;
        ((struct cv_typerec_member *)pt)->attribute.mprop = CV_ATTR_MPR_VANILLA;
        ((struct cv_typerec_member *)pt)->attribute.pseudo = 0;
        ((struct cv_typerec_member *)pt)->attribute.noinherit = 0;
        ((struct cv_typerec_member *)pt)->attribute.noconstruct = 0;
        ((struct cv_typerec_member *)pt)->attribute.reserved = 0;
        if ( typelen == 0 ) {
            if ( type->e.structinfo->typekind == TYPE_RECORD )
                ((struct cv_typerec_member *)pt)->offset = 0;
            else
                ((struct cv_typerec_member *)pt)->offset = curr->sym->offset;
            tmp = pt + sizeof( struct cv_typerec_member );
        } else {
            ((struct cv_typerec_member *)pt)->offset = LF_ULONG;
            tmp = pt + sizeof( struct cv_typerec_member );
            *(uint_32 *)tmp = curr->sym->offset;
            tmp += sizeof( uint_32 );
        }
        DebugMsg(("WriteType(%Xh): MEMBER=%s, typeref=%X\n", GetTPos(), curr->sym->name, ((struct cv_typerec_member *)pt)->type ));
        SetPrefixName( tmp, curr->sym->name, curr->sym->name_size );
        PadBytes( tmp, (uint_8 *)CurrSource );
        pt += size;
    }

    return( pt );
}

/* write a symbol to $$SYMBOLS */

static uint_8 * WriteSymbol( dir_node *symbols, struct asm_sym *sym, uint_8 *ps, uint_8 *sbuffer )
/************************************************************************************************/
{
    int        len;
    int        ofs;
    short      rectype;
    uint_8     Ofssize;
    struct asmfixup *fixup;

    Ofssize = GetSymOfssize( sym );
    len = GetCVStructLen( sym, Ofssize );
    ps = checkflush( symbols, sbuffer, ps, 1 + sym->name_size + len );
    switch ( sym->state ) {
    case SYM_TYPE:
        ((struct cv_symrec_udt *)ps)->sr.size = sizeof( struct cv_symrec_udt ) - sizeof(uint_16) + 1 + sym->name_size;
        ((struct cv_symrec_udt *)ps)->sr.type = S_UDT;
        if ( ((dir_node *)sym)->e.structinfo->typekind != TYPE_TYPEDEF ) {
            ((struct cv_symrec_udt *)ps)->typeref = sym->cv_typeref;
        } else {
            ((struct cv_symrec_udt *)ps)->typeref = GetTyperef( sym, Ofssize );
        }

        /* Some typedefs won't get a valid type (<name> TYPEDEF PROTO ...).
         * In such cases just skip the type!
         */
        if ( ((struct cv_symrec_udt *)ps)->typeref == 0 )
            return( ps );

        DebugMsg(( "WriteSymbol: TYPE=%s typeref=%Xh\n", sym->name, ((struct cv_symrec_udt *)ps)->typeref ));
        rectype = FIX_VOID; /* types have no fixup */
        break;
    default: /* is SYM_INTERNAL */
        if ( sym->isproc ) {
            DebugMsg(( "WriteSymbol: PROC=%s\n", sym->name ));
            if ( Ofssize == USE16 ) {
                ((struct cv_symrec_lproc16 *)ps)->sr.size = sizeof( struct cv_symrec_lproc16 ) - sizeof(uint_16) + 1 + sym->name_size;
                ((struct cv_symrec_lproc16 *)ps)->sr.type = (sym->public ? S_GPROC16 : S_LPROC16);
                ((struct cv_symrec_lproc16 *)ps)->pParent = 0;  /* filled by CVPACK */
                ((struct cv_symrec_lproc16 *)ps)->pEnd = 0;     /* filled by CVPACK */
                ((struct cv_symrec_lproc16 *)ps)->pNext = 0;    /* filled by CVPACK */
                ((struct cv_symrec_lproc16 *)ps)->proc_length = sym->total_size;
                ((struct cv_symrec_lproc16 *)ps)->debug_start = 0;
                ((struct cv_symrec_lproc16 *)ps)->debug_end = sym->total_size;
                ((struct cv_symrec_lproc16 *)ps)->offset = 0;
                ((struct cv_symrec_lproc16 *)ps)->segment = 0;
                ((struct cv_symrec_lproc16 *)ps)->proctype = 0; /* typeref */
                ((struct cv_symrec_lproc16 *)ps)->flags = ((sym->mem_type == MT_FAR) ? CV_TYPE_LABEL_FAR : CV_TYPE_LABEL_NEAR);
                rectype = FIX_PTR16;
                ofs = offsetof( struct cv_symrec_lproc16, offset );
            } else {
                ((struct cv_symrec_lproc32 *)ps)->sr.size = sizeof( struct cv_symrec_lproc32 ) - sizeof(uint_16) + 1 + sym->name_size;
                ((struct cv_symrec_lproc32 *)ps)->sr.type = (sym->public ? S_GPROC32 : S_LPROC32 );
                ((struct cv_symrec_lproc32 *)ps)->pParent = 0; /* filled by CVPACK */
                ((struct cv_symrec_lproc32 *)ps)->pEnd = 0;    /* filled by CVPACK */
                ((struct cv_symrec_lproc32 *)ps)->pNext = 0;   /* filled by CVPACK */
                ((struct cv_symrec_lproc32 *)ps)->proc_length = sym->total_size;
                ((struct cv_symrec_lproc32 *)ps)->debug_start = 0;
                ((struct cv_symrec_lproc32 *)ps)->debug_end = sym->total_size;
                ((struct cv_symrec_lproc32 *)ps)->offset = 0;
                ((struct cv_symrec_lproc32 *)ps)->segment = 0;
                ((struct cv_symrec_lproc32 *)ps)->proctype = 0; /* typeref */
                ((struct cv_symrec_lproc32 *)ps)->flags = ((sym->mem_type == MT_FAR) ? CV_TYPE_LABEL_FAR : CV_TYPE_LABEL_NEAR);
                rectype = FIX_PTR32;
                ofs = offsetof( struct cv_symrec_lproc32, offset );
            }
            break;
        }
        /* there are 3 types of INTERNAL symbols:
         * - numbers ( won't occur here )
         * - labels "without type" (MT_NEAR or MT_FAR)
         * - labels "with type"
         */
        if ( sym->mem_type == MT_NEAR || sym->mem_type == MT_FAR ) {
            if ( Ofssize == USE16 ) {
                ((struct cv_symrec_label16 *)ps)->sr.size = sizeof( struct cv_symrec_label16 ) - sizeof(uint_16) + 1 + sym->name_size;
                ((struct cv_symrec_label16 *)ps)->sr.type = S_LABEL16;
                ((struct cv_symrec_label16 *)ps)->offset = 0;
                ((struct cv_symrec_label16 *)ps)->segment = 0;
                ((struct cv_symrec_label16 *)ps)->flags = ((sym->mem_type == MT_FAR) ? CV_TYPE_LABEL_FAR : CV_TYPE_LABEL_NEAR);
                rectype = FIX_PTR16;
                ofs = offsetof( struct cv_symrec_label16, offset );
                DebugMsg(( "WriteSymbol: LABEL16=%s\n", sym->name ));
            } else {
                ((struct cv_symrec_label32 *)ps)->sr.size = sizeof( struct cv_symrec_label32 ) - sizeof(uint_16) + 1 + sym->name_size;
                ((struct cv_symrec_label32 *)ps)->sr.type = S_LABEL32;
                ((struct cv_symrec_label32 *)ps)->offset = 0;
                ((struct cv_symrec_label32 *)ps)->segment = 0;
                ((struct cv_symrec_label32 *)ps)->flags = ((sym->mem_type == MT_FAR) ? CV_TYPE_LABEL_FAR : CV_TYPE_LABEL_NEAR);
                rectype = FIX_PTR32;
                ofs = offsetof( struct cv_symrec_label32, offset );
                DebugMsg(( "WriteSymbol: LABEL32=%s\n", sym->name ));
            }
        } else {
            if ( Ofssize == USE16 ) {
                ((struct cv_symrec_ldata16 *)ps)->sr.size = sizeof( struct cv_symrec_ldata16 ) - sizeof(uint_16) + 1 + sym->name_size;
                ((struct cv_symrec_ldata16 *)ps)->sr.type = S_LDATA16;
                ((struct cv_symrec_ldata16 *)ps)->offset = 0;
                ((struct cv_symrec_ldata16 *)ps)->segment = 0;
                ((struct cv_symrec_ldata16 *)ps)->typeref = GetTyperef( sym, USE16 );
                rectype = FIX_PTR16;
                ofs = offsetof( struct cv_symrec_ldata16, offset );
                DebugMsg(( "WriteSymbol: INTERN16=%s typeref=%Xh\n", sym->name, ((struct cv_symrec_ldata16 *)ps)->typeref ));
            } else {
                ((struct cv_symrec_ldata32 *)ps)->sr.size = sizeof( struct cv_symrec_ldata32 ) - sizeof(uint_16) + 1 + sym->name_size;
                ((struct cv_symrec_ldata32 *)ps)->sr.type = S_LDATA32;
                ((struct cv_symrec_ldata32 *)ps)->offset = 0;
                ((struct cv_symrec_ldata32 *)ps)->segment = 0;
                ((struct cv_symrec_ldata32 *)ps)->typeref = GetTyperef( sym, USE32 );
                rectype = FIX_PTR32;
                ofs = offsetof( struct cv_symrec_ldata32, offset );
                DebugMsg(( "WriteSymbol: INTERN32=%s typeref=%Xh\n", sym->name, ((struct cv_symrec_ldata32 *)ps)->typeref ));
            }
        }
    }
    if ( rectype != FIX_VOID ) {
        ps += ofs;
        symbols->e.seginfo->current_loc = symbols->e.seginfo->start_loc + (ps - sbuffer);
        if ( rectype == FIX_PTR32 && Options.output_format == OFORMAT_COFF ) {
            /* COFF has no "far" fixups. Instead Masm creates a
             * section-relative fixup + a section fixup.
             */
            fixup = AddFixup( sym, FIX_OFF32_SECREL, OPTJ_NONE );
            store_fixup( fixup, (int_32 *)ps );
            fixup = AddFixup( sym, FIX_SEG, OPTJ_NONE );
            fixup->fixup_loc += sizeof (int_32 );
            store_fixup( fixup, (int_32 *)ps );
        } else {
            fixup = AddFixup( sym, rectype, OPTJ_NONE );
            /* todo: for OMF, delay fixup store until checkflush has been called! */
            store_fixup( fixup, (int_32 *)ps );
        }
        ps += len - ofs;
    } else
        ps += len;

    SetPrefixName( ps, sym->name, sym->name_size );

    /* for PROCs, scan their local symbols. These are
     * - parameters
     * - local variables (auto)
     * - local labels (currently ignored)
     * to mark the block's end, write an ENDBLK item.
     */

    if ( sym->isproc ) {
        dir_node *proc = (dir_node *)sym;
        dir_node  *lcl;

        /* scan local symbols */
        for ( lcl = proc->e.procinfo->labellist; lcl; lcl = lcl->nextll ) {
            len = GetCVStructLen( &lcl->sym, Ofssize );
            ps = checkflush( symbols, sbuffer, ps, 1 + lcl->sym.name_size + len );
            switch ( lcl->sym.state ) {
            case SYM_STACK: /* params and locals */
                if ( Ofssize == USE16 ) {
                    ((struct cv_symrec_bprel16 *)ps)->sr.size = sizeof( struct cv_symrec_bprel16 ) - sizeof(uint_16) + 1 + lcl->sym.name_size;
                    ((struct cv_symrec_bprel16 *)ps)->sr.type = S_BPREL16;
                    ((struct cv_symrec_bprel16 *)ps)->offset = lcl->sym.offset;
                    ((struct cv_symrec_bprel16 *)ps)->typeref = GetTyperef( &lcl->sym, USE16 );
                } else {
                    ((struct cv_symrec_bprel32 *)ps)->sr.size = sizeof( struct cv_symrec_bprel32 ) - sizeof(uint_16) + 1 + lcl->sym.name_size;
                    ((struct cv_symrec_bprel32 *)ps)->sr.type = S_BPREL32;
                    ((struct cv_symrec_bprel32 *)ps)->offset = lcl->sym.offset;
                    ((struct cv_symrec_bprel32 *)ps)->typeref = GetTyperef( &lcl->sym, USE32 );
                }
                ps += len;
                SetPrefixName( ps, lcl->sym.name, lcl->sym.name_size );
                break;
            default: /* local labels */
                break;
            }
        }

        ps = checkflush( symbols, sbuffer, ps, sizeof( struct cv_symrec_endblk ) );
        ((struct cv_symrec_endblk *)ps)->sr.size = sizeof( struct cv_symrec_endblk ) - sizeof(uint_16);
        ((struct cv_symrec_endblk *)ps)->sr.type = S_ENDBLK;
        ps += sizeof( struct cv_symrec_endblk );
    }
    return( ps );
}

/* option -Zi: write debug symbols and types
 * - symbols: segment $$SYMBOLS (OMF) or .debug$S (COFF)
 * - types:   segment $$TYPES (OMF) or .debug$T (COFF)
 */

void cv_write_debug_tables( dir_node *symbols, dir_node *types )
/**************************************************************/
{
    struct asm_sym *sym;
    int        i;
    int        len;
    uint_8     *pt;
    uint_8     *ps;
    char       *objname;
    uint_8     sbuffer[1024];

    DebugMsg(("cv_write_debug_tables enter\n"));

    currtype = 0x1000; /* user-defined types start at 0x1000 */

    if ( types == NULL ||
        symbols == NULL ||
        types->sym.state != SYM_SEG ||
        symbols->sym.state != SYM_SEG ) {
        DebugMsg(("cv_write_debug_tables: unexpected exit\n"));
        return;
    }
    /* write types */
    pt = (uint_8 *)CurrSource; /* use the source line buffer for types */
    types->e.seginfo->CodeBuffer = pt;
    memset( pt, 0, 1024 );
    *(uint_32 *)pt = 1; /* "signature" */
    pt += sizeof( uint_32 );

    /* scan symbol table for types */

    sym = NULL;
    while ( SymEnum( &sym, &i ) ) {
        if ( sym->state == SYM_TYPE && sym->cv_typeref == 0 ) {
            pt = WriteType( types, sym, pt );
        }
    }
    checkflush( types, (uint_8 *)CurrSource, pt, 1024 ); /* final flush */
    types->sym.max_offset = types->e.seginfo->current_loc;
    types->e.seginfo->start_loc = 0; /* required for COFF */

    /* write symbols */
    ps = sbuffer;
    symbols->e.seginfo->CodeBuffer = sbuffer;
    memset( ps, 0, sizeof( sbuffer) );
    *(uint_32 *)ps = 1; /* "signature" */
    ps += sizeof(uint_32);

    /* 1. record: object name */
    objname = FileInfo.fname[OBJ];
    for ( i = strlen( objname ); i; i-- )
        if ( *(objname+i-1) == '/' || *(objname+i-1) == '\\' )
            break;
    objname += i;
    len = strlen( objname );
    ((struct cv_symrec_objname *)ps)->sr.size = sizeof( struct cv_symrec_objname ) - sizeof(uint_16) + 1 + len;
    ((struct cv_symrec_objname *)ps)->sr.type = S_OBJNAME;
    ((struct cv_symrec_objname *)ps)->Signature = 1;
    ps += sizeof( struct cv_symrec_objname );
    SetPrefixName( ps, objname, len );

    /* 2. record: compiler */
    len = strlen( szCVCompiler );
    ((struct cv_symrec_compile *)ps)->sr.size = sizeof( struct cv_symrec_compile ) - sizeof(uint_16) + 1 + len;
    ((struct cv_symrec_compile *)ps)->sr.type = S_COMPILE;
    ((struct cv_symrec_compile *)ps)->machine = (ModuleInfo.curr_cpu & P_CPU_MASK) >> 4;
    /* 0 isnt possible, 1 is 8086 and 80186 */
    if ( ((struct cv_symrec_compile *)ps)->machine == 0 )
        ((struct cv_symrec_compile *)ps)->machine = CV_MACH_8086;
    ((struct cv_symrec_compile *)ps)->Language = CV_LANG_MASM;
    //((struct cv_symrec_compile *)ps)->Mode32 = 0;  /* always zero */
    ps += sizeof( struct cv_symrec_compile );
    SetPrefixName( ps, szCVCompiler, len );

    /* CurrSeg and Modend must be set for store_fixup() */
    CurrSeg = symbols;
    Modend = FALSE;

    /* scan symbol table for SYM_TYPE, SYM_INTERNAL */

    sym = NULL;
    while ( SymEnum( &sym, &i ) ) {
        switch ( sym->state ) {
        case SYM_TYPE: /* types also have an entry in the symbols table */
        //case SYM_STACK: /* stack symbols are local only */
        case SYM_INTERNAL:
            if ( sym->mem_type == MT_ABS || sym->predefined ) /* skip EQUates */
                break;
            ps = WriteSymbol( symbols, sym, ps, sbuffer );
            break;
        }
    }
    checkflush( symbols, sbuffer, ps, 1024 ); /* final flush */
    symbols->sym.max_offset = symbols->e.seginfo->current_loc;
    symbols->e.seginfo->start_loc = 0; /* required for COFF */

    CurrSeg = NULL;
    Modend = TRUE;

    DebugMsg(("cv_write_debug_tables exit, max type=%Xh\n", currtype - 1 ));
    return;
}
