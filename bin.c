/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  BIN output routines.
*               Also handles format MZ.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "fixup.h"
#include "omfspec.h"
#include "bin.h"
#include "fatal.h"
#include "listing.h"

#if BIN_SUPPORT

#define SECTORMAP 1 /* 1=print sector map in listing file */

extern struct asm_sym   symPC; /* '$' symbol */

#if MZ_SUPPORT
struct MZDATA mzdata = {0x1E, 0x10, 0, 0xFFFF };
#endif

static uint_32 fileoffset;
static uint_32 entryoffset;
static asm_sym *entryseg;
static uint_32 sizehdr;  /* size of MZ header, always 0 for BIN */
static uint_32 imagestart;

#if SECTORMAP
/* these strings are to be moved to msgdef.h */
static const char * const szCaption = "Binary Map:";
static const char * const szCaption2= "Segment                  Pos(file)      VA  Size(fil) Size(mem)";
static const char * const szLine    = "---------------------------------------------------------------";
static const char * const szHeader  = "<header>";
static const char * const szSegLine = "%-24s %8X %8X %9X %9X";
static const char * const szTotal   = "%-42s %9X %9X";
#endif

/* reorder segments for DOSSEG:
 1. code
 2. unknown
 3. initialized data
 4. uninitialized data
 5. stack
 */
static const seg_type typeorder[] = {
    SEGTYPE_CODE, SEGTYPE_UNDEF, SEGTYPE_DATA,
    SEGTYPE_BSS, SEGTYPE_STACK, SEGTYPE_ABS, SEGTYPE_ERROR
};

#ifdef __I86__
/* "huge" fwrite() for JWasmr.exe */
uint_32 hfwrite( uint_8 huge *pBuffer, int size, uint_32 count, FILE *file )
/**************************************************************************/
{
    uint_32 written;
    uint tmpsize;

    for ( written = 0; written < count; written += tmpsize ) {
        if ( count > 0xFE00 )
            tmpsize = 0xFE00;
        else
            tmpsize = count;
        if ( fwrite( pBuffer, size, tmpsize, file ) != tmpsize )
            WriteError();
        pBuffer += tmpsize;
    }
    return( written );
}
#endif

/* sort segments by either fileoffset (.DOSSEG) or name (.ALPHA) */

void SortSegments( void )
/***********************/
{
    bool changed = TRUE;
    bool swap;
    dir_node *curr;
    int index = 1;

    while ( changed == TRUE ) {
        dir_node *prev = NULL;
        changed = FALSE;
        for( curr = Tables[TAB_SEG].head; curr && curr->next ; prev = curr, curr = curr->next ) {
            swap = FALSE;
            if ( ModuleInfo.segorder == SEGORDER_DOSSEG ) {
                if ( curr->e.seginfo->fileoffset > curr->next->e.seginfo->fileoffset )
                    swap = TRUE;
            } else {
                if ( strcmp(curr->sym.name, curr->next->sym.name) > 0 )
                    swap = TRUE;
            }
            if ( swap ) {
                dir_node *tmp = curr->next;
                changed = TRUE;
                if (prev == NULL) {
                    Tables[TAB_SEG].head = tmp;
                } else {
                    prev->next = tmp;
                }
                curr->next = tmp->next;
                tmp->next = curr;
            }
        }
    }
    for ( curr = Tables[TAB_SEG].head; curr ; curr = curr->next ) {
        curr->e.seginfo->idx = index++;
    }
}

/* calculate starting offset of segments and groups */

static void CalcOffset( dir_node *curr, bool firstseg )
/*****************************************************/
{
    uint_32 align;
    uint_32 alignbytes;
    uint_32 offset;
    dir_node *grp;

    if ( curr->e.seginfo->segtype == SEGTYPE_ABS ) {
        curr->e.seginfo->start_offset = curr->e.seginfo->abs_frame << 4;
        DebugMsg(("CalcOffset(%s): abs seg, offset=%" FX32 "h\n",
                  curr->sym.name, curr->e.seginfo->start_offset ));
        return;
    }

    grp = (dir_node *)curr->e.seginfo->group;
    align = 1 << curr->e.seginfo->alignment;
    //alignbytes = ((offset + (align - 1)) & (-align)) - offset;
    alignbytes = ((fileoffset + (align - 1)) & (-align)) - fileoffset;
    fileoffset += alignbytes;

    if ( grp == NULL ) {
        offset = fileoffset - sizehdr; // + alignbytes;
        DebugMsg(("CalcOffset(%s): fileofs=%" FX32 "h, ofs=%" FX32 "h\n", curr->sym.name, fileoffset, offset ));
    } else {
        if ( grp->sym.total_size == 0 ) {
            grp->sym.offset = fileoffset - sizehdr;
            offset = 0;
        } else
            offset = grp->sym.total_size + alignbytes;
        DebugMsg(("CalcOffset(%s): fileofs=%" FX32 "h, alignbytes=%lu, ofs=%" FX32 "h, group=%s, grp.ofs=%" FX32 "h\n",
                  curr->sym.name, fileoffset, alignbytes, offset, grp->sym.name, grp->sym.offset ));
    }

    /* v2.04: added */
    if ( firstseg == FALSE )
        curr->e.seginfo->start_loc = 0;

    curr->e.seginfo->fileoffset = fileoffset;
    //if ( firstseg && Options.header_format == HFORMAT_NONE ) {
    if ( Options.header_format == HFORMAT_NONE ) {
        fileoffset += curr->sym.max_offset - curr->e.seginfo->start_loc;
        if ( firstseg )
            imagestart = curr->e.seginfo->start_loc;
    } else {
        curr->e.seginfo->fileoffset += curr->e.seginfo->start_loc;
        fileoffset += curr->sym.max_offset;
    }

    curr->e.seginfo->start_offset = offset;

    /* there's no real entry address for BIN, therefore the
     start label must be at the very beginning of the file */
    if (entryoffset == -1) {
        entryoffset = offset;
        entryseg = (asm_sym *)curr;
    }
    //offset += curr->sym.max_offset - curr->e.seginfo->start_loc;
    offset += curr->sym.max_offset;
    if ( grp ) {
        //grp->sym.total_size = offset + curr->e.seginfo->start_loc;
        grp->sym.total_size = offset;
    }
    DebugMsg(("CalcOffset(%s): seg.fileofs=%" FX32 "h, seg.start_offset=%" FX32 "h, endofs=%" FX32 "h\n",
              curr->sym.name, curr->e.seginfo->fileoffset, curr->e.seginfo->start_offset, offset ));

    return;
}

#if MZ_SUPPORT

/*
 * if pDst==NULL: count the number of segment related fixups
 * if pDst!=NULL: write segment related fixups
 */

static int GetSegRelocs( uint_16 *pDst )
/**************************************/
{
    dir_node *curr;
    int count = 0;
    uint_16 valueofs;
    uint_16 valueseg;
    uint_32 loc;
    struct fixup *fixup;

    DebugMsg(("GetSegRelocs( %p ) enter\n", pDst ));
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
            continue;
        for ( fixup = curr->e.seginfo->FixupListHead; fixup; fixup = fixup->nextrlc ) {
            switch ( fixup->type ) {
            case FIX_PTR32:
            case FIX_PTR16:
            case FIX_SEG:
                /* ignore fixups for absolute segments */
                if ( fixup->sym && fixup->sym->segment && ((dir_node *)fixup->sym->segment)->e.seginfo->segtype == SEGTYPE_ABS )
                    break;
                DebugMsg(("GetSegRelocs: found seg-related fixup at %s.%" FX32 "\n", curr->sym.name, fixup->location ));
                count++;
                if ( pDst ) {
                    /* v2.04: fixed */
                    loc = fixup->location + ( curr->e.seginfo->start_offset & 0xf );
                    valueseg = curr->e.seginfo->start_offset >> 4;
                    if ( curr->e.seginfo->group ) {
                        loc += curr->e.seginfo->group->offset & 0xf;
                        valueseg += curr->e.seginfo->group->offset >> 4;
                    }
                    if ( fixup->type == FIX_PTR16 )
                        loc += 2;
                    else if ( fixup->type == FIX_PTR32 )
                        loc += 4;

                    /* offset may be > 64 kB */
                    while ( loc >= 0x10000 ) {
                        loc -= 16;
                        valueseg++;
                    };

                    valueofs = loc;
                    DebugMsg(("GetSegRelocs: location=%" FX32 " fileofs=%" FX32 " segofs=%" FX32 " grpofs=%" FX32 ", fixup value: %X %X\n",
                              fixup->location, curr->e.seginfo->fileoffset, curr->e.seginfo->start_offset, curr->e.seginfo->group ? curr->e.seginfo->group->offset: 0, valueofs, valueseg ));
                    *pDst++ = valueofs;
                    *pDst++ = valueseg;
                }
                break;
            }
        }
    }
    DebugMsg(("GetSegRelocs()=%u\n", count ));
    return( count );
}

#endif

/* get image size.
 * memimage=FALSE: get size without uninitialized segments (BSS and STACK)
 * memimage=TRUE: get full size
 */

static uint_32 GetImageSize( bool memimage )
/******************************************/
{
    dir_node *curr;
    uint_32 size = 0;

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        uint_32 tmp;
        if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
            continue;
        if ( memimage == FALSE ) {
            if ( curr->e.seginfo->bytes_written == 0 ) {
                dir_node *dir;
                for ( dir = curr->next; dir; dir = dir->next )
                    if ( dir->e.seginfo->bytes_written )
                        break;
                if ( !dir )
                    break; /* done, skip rest of segments! */
            }
        }
        DebugMsg(("GetImageSize(%s): fileofs=%" FX32 "h, max_offs=%" FX32 "h\n", curr->sym.name, curr->e.seginfo->fileoffset, curr->sym.max_offset ));
        tmp = curr->e.seginfo->fileoffset + (curr->sym.max_offset - curr->e.seginfo->start_loc );
        if ( size < tmp )
            size = tmp;
    }
    DebugMsg(("GetImageSize(%u)=%" FX32 "h\n", memimage, size ));
    return( size );
}

/* micro-linker. resolve internal fixups.
 */

union genptr {
    uint_8  *db;
    uint_16 *dw;
    uint_32 *dd;
#if AMD64_SUPPORT
    uint_64 *dq;
#endif
};

static ret_code DoFixup( dir_node *curr )
/***************************************/
{
    union genptr codeptr;
    dir_node *seg;
    uint_32 value;
    struct fixup *fixup;

    if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
        return( NOT_ERROR );

    DebugMsg(("DoFixup(%s) enter, segment start ofs=%" FX32 "h\n", curr->sym.name, curr->e.seginfo->start_offset ));
    for ( fixup = curr->e.seginfo->FixupListHead; fixup; fixup = fixup->nextrlc ) {
        codeptr.db = curr->e.seginfo->CodeBuffer +
            ( fixup->location - curr->e.seginfo->start_loc );
        /* assembly time variable (also $ symbol) in reloc? */
        if ( fixup->sym && fixup->sym->variable ) {
            seg = (dir_node *)fixup->segment;
            value = seg->e.seginfo->start_offset + fixup->offset;
            DebugMsg(("DoFixup(%s, %04" FX32 ", %s): variable, fixup->segment=%Xh fixup->offset=%" FX32 "h, fixup->sym->offset=%" FX32 "h\n",
                      curr->sym.name, fixup->location, fixup->sym->name, seg, fixup->offset, fixup->sym->offset ));
        } else if ( fixup->sym && fixup->sym->segment ) {
            seg = (dir_node *)fixup->sym->segment;
            /* the offset result consists of
             * - the symbol's offset
             * - the fixup's offset (usually the displacement )
             * - the segment/group offset in the image
             */
            if ( fixup->type == FIX_OFF32_IMGREL ) {
                value = ( fixup->offset + fixup->sym->offset + seg->e.seginfo->start_offset ) - imagestart;
                DebugMsg(("DoFixup(%s): IMGREL, loc=%" FX32 " value=%" FX32 " seg.start=%" FX32 " imagestart=%" FX32 "\n",
                          curr->sym.name, fixup->location, value, seg->e.seginfo->start_offset, imagestart ));
            } else if ( fixup->type == FIX_OFF32_SECREL ) {
                char *tmp;
                /* check if symbol's segment name contains a '$'.
                 * If yes, search the segment without suffix.
                 */
                value = ( fixup->offset + fixup->sym->offset ) - seg->e.seginfo->start_loc;
                if ( tmp = strchr( seg->sym.name, '$' ) ) {
                    int namlen = tmp - seg->sym.name;
                    dir_node *segfirst;
                    for( segfirst = Tables[TAB_SEG].head; segfirst; segfirst = segfirst->next ) {
                        if ( segfirst->sym.name_size == namlen &&
                            ( memcmp( segfirst->sym.name, seg->sym.name, namlen ) == 0 ) ) {
                            value = ( fixup->offset + fixup->sym->offset + seg->e.seginfo->start_offset ) - segfirst->e.seginfo->start_offset;
                            DebugMsg(("DoFixup(%s): SECREL, primary seg=%s, start_offset=%" FX32 "\n",
                                      curr->sym.name, segfirst->sym.name, segfirst->e.seginfo->start_offset ));
                            break;
                        }
                    }
                }
                DebugMsg(("DoFixup(%s): SECREL, loc=%" FX32 ", value=%" FX32 "\n", curr->sym.name, fixup->location, value ));
            /* v2.01: don't use group if fixup explicitely refers the segment! */
            //} else if ( seg->e.seginfo->group ) {
            } else if ( seg->e.seginfo->group && fixup->frame != FRAME_SEG ) {
                value = (seg->e.seginfo->group->offset & 0xF) + seg->e.seginfo->start_offset + fixup->offset + fixup->sym->offset;
            } else if ( fixup->type >= FIX_RELOFF8 && fixup->type <= FIX_RELOFF32 ) {
                /* v1.96: special handling for "relative" fixups */
                value = seg->e.seginfo->start_offset + fixup->offset + fixup->sym->offset;
            } else
                value = (seg->e.seginfo->start_offset & 0xF) + fixup->offset + fixup->sym->offset;
            DebugMsg(("DoFixup(%s, %04" FX32 ", %s): target->start_offset=%" FX32 "h, fixup->offset=%" FX32 "h, fixup->sym->offset=%" FX32 "h\n",
                      curr->sym.name, fixup->location, fixup->sym->name, seg->e.seginfo->start_offset, fixup->offset, fixup->sym->offset ));
        } else {
            seg = (dir_node *)fixup->segment;
            DebugMsg(("DoFixup(%s, %04" FX32 ", %s): target segment=0, fixup->offset=%" FX32 "h, fixup->sym->offset=%" FX32 "h\n",
                      curr->sym.name, fixup->location, fixup->sym ? fixup->sym->name : "", fixup->offset, fixup->sym ? fixup->sym->offset : 0 ));
            value = 0;
        }
        switch (fixup->type) {
        case FIX_RELOFF8:
            //*codeptr.db += (value - fixup->location + 1) & 0xff;
            /* changed in v1.95 */
            *codeptr.db += (value - (fixup->location + curr->e.seginfo->start_offset) - 1) & 0xff;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_RELOFF8, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.db ));
            break;
        case FIX_RELOFF16:
            //*codeptr.dw += (value - fixup->location + 2) & 0xffff;
            /* changed in v1.95 */
            *codeptr.dw += (value - (fixup->location + curr->e.seginfo->start_offset) - 2) & 0xffff;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_RELOFF16, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.dw ));
            break;
        case FIX_RELOFF32:
#if AMD64_SUPPORT
            /* adjust the location for EIP-related offsets if USE64 */
            if ( curr->e.seginfo->Ofssize == USE64 ) {
                fixup->location += fixup->addbytes - 4;
            }
#endif
            //*codeptr.dd += (value - fixup->location + 4);
            /* changed in v1.95 */
            *codeptr.dd += (value - (fixup->location + curr->e.seginfo->start_offset) - 4);
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_RELOFF32, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.dd ));
            break;
        case FIX_LOBYTE:
            *codeptr.db = value & 0xff;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_LOBYTE, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.db ));
            break;
        case FIX_HIBYTE:
            *codeptr.db = (value >> 8) & 0xff;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_HIBYTE, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.db ));
            break;
        case FIX_OFF16:
            *codeptr.dw = value & 0xffff;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_OFF16, value=%" FX32 "h, target=%p *target=%Xh\n", curr->sym.name, fixup->location, value, codeptr, *codeptr.dw ));
            break;
        case FIX_OFF32:
            *codeptr.dd = value;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_OFF32, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.dd ));
            break;
        case FIX_OFF32_IMGREL:
            *codeptr.dd = value;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_OFF32_IMGREL, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.dd ));
            break;
        case FIX_OFF32_SECREL:
            *codeptr.dd = value;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_OFF32_SECREL, value=%" FX32 "h, *target=%Xh\n", curr->sym.name, fixup->location, value, *codeptr.dd ));
            break;
#if AMD64_SUPPORT
        case FIX_OFF64:
            *codeptr.dq = value;
            DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_OFF64, value=%" FX32 "h, *target=%I64Xh\n", curr->sym.name, fixup->location, value, *codeptr.dq ));
            break;
#endif
        case FIX_SEG:
            /* absolute segments are ok */
            if ( fixup->sym &&
                fixup->sym->state == SYM_SEG &&
                ((dir_node *)fixup->sym)->e.seginfo->segtype == SEGTYPE_ABS ) {
                *codeptr.dw = ((dir_node *)fixup->sym)->e.seginfo->abs_frame;
                break;
            }
#if MZ_SUPPORT
            if ( Options.header_format == HFORMAT_MZ ) {
                DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_SEG frame=%u, ", curr->sym.name, fixup->location, fixup->frame ));
                if ( fixup->sym->state == SYM_GRP ) {
                    seg = (dir_node *)fixup->sym;
                    *codeptr.dw = seg->sym.offset >> 4;
                    DebugMsg(("GROUP symbol, offset=%" FX32 "h codeptr=%p\n", seg->sym.offset, codeptr ));
                } else if ( fixup->sym->state == SYM_SEG ) {
                    /* v2.04: added */
                    seg = (dir_node *)fixup->sym;
                    *codeptr.dw = ( seg->e.seginfo->start_offset + ( seg->e.seginfo->group ? seg->e.seginfo->group->offset : 0 ) ) >> 4;
                    DebugMsg(("SEGMENT symbol, start_offset=%" FX32 "h\n", seg->e.seginfo->start_offset ));
                //} else if ( seg->e.seginfo->group ) {
                } else if ( fixup->frame == FRAME_GRP ) {
                    /* v2.04: changed */
                    //*codeptr.dw = (seg->e.seginfo->start_offset + seg->e.seginfo->group->offset) >> 4;
                    *codeptr.dw = seg->e.seginfo->group->offset >> 4;
                    DebugMsg(("group.offset=%" FX32 "h\n", seg->e.seginfo->group->offset ));
                } else {
                    *codeptr.dw = seg->e.seginfo->start_offset >> 4;
                    DebugMsg(("segment.offset=%" FX32 "h\n", seg->e.seginfo->start_offset ));
                }
                break;
            }
#endif
        case FIX_PTR16:
#if MZ_SUPPORT
            if ( Options.header_format == HFORMAT_MZ ) {
                DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_PTR16, seg->start=%Xh\n", curr->sym.name, fixup->location, seg->e.seginfo->start_offset ));
                *codeptr.dw = value & 0xffff;
                codeptr.dw++;
                //if ( seg->e.seginfo->group ) { /* v2.04: changed */
                if ( fixup->frame == FRAME_GRP ) {
                    /* v2.04: changed */
                    //*codeptr.dw = (seg->e.seginfo->start_offset + seg->e.seginfo->group->offset) >> 4;
                    *codeptr.dw = seg->e.seginfo->group->offset >> 4;
                } else {
                    *codeptr.dw = seg->e.seginfo->start_offset >> 4;
                }
                break;
            }
#endif
        case FIX_PTR32:
#if MZ_SUPPORT
            if ( Options.header_format == HFORMAT_MZ ) {
                DebugMsg(("DoFixup(%s, %04" FX32 "): FIX_PTR32\n", curr->sym.name, fixup->location ));
                *codeptr.dd = value;
                codeptr.dd++;
                //if (seg->e.seginfo->group ) { /* v2.04: changed */
                if ( fixup->frame == FRAME_GRP ) {
                    /* v2.04: changed */
                    //*codeptr.dw = (seg->e.seginfo->start_offset + seg->e.seginfo->group->offset) >> 4;
                    *codeptr.dw = seg->e.seginfo->group->offset >> 4;
                } else {
                    *codeptr.dw = seg->e.seginfo->start_offset >> 4;
                }
                break;
            }
#endif
        default:
            DebugMsg(("DoFixup(%s, %04" FX32 "): invalid fixup %u\n", curr->sym.name, fixup->location, fixup->type ));
            AsmErr( INVALID_FIXUP_TYPE, "BIN", fixup->type, curr->sym.name, fixup->location );
            //return( ERROR );
        }
    }
    return( NOT_ERROR );
}

/* write section contents
 * this is done after the last step only!
 */

ret_code bin_write_data( module_info *ModuleInfo )
/************************************************/
{
    dir_node *curr;
    uint_32 size;
    const seg_type *segtype;
    bool first = TRUE;
#if MZ_SUPPORT
    uint_16 reloccnt;
    uint_32 sizemem;
    dir_node *stack = NULL;
    uint_16 *pReloc;
    uint_32 sizetotal;
    uint_32 sizeheap;
    uint_8  *hdrbuf;
#endif

    DebugMsg(("bin_write_data: enter\n" ));

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        /* reset the offset fields of segments */
        /* it was used to store the size in there */
        curr->e.seginfo->start_offset = 0;
        /* set STACK segment type */
        if ( curr->e.seginfo->combine == COMB_STACK )
            curr->e.seginfo->segtype = SEGTYPE_STACK;
    }
    fileoffset = 0;
    sizehdr = 0;
#if MZ_SUPPORT
    if ( Options.header_format == HFORMAT_MZ ) {
        reloccnt = GetSegRelocs( NULL );
        sizehdr = (reloccnt * 4 + mzdata.ofs_fixups + (mzdata.alignment - 1)) & ~(mzdata.alignment-1);
        hdrbuf = AsmAlloc( sizehdr );
        memset( hdrbuf, 0, sizehdr );
        fileoffset = sizehdr;
        DebugMsg(("bin_write_data: MZ format, fixups=%u, sizehdr=%" FX32 "\n", reloccnt, sizehdr ));
    }
#endif

    entryoffset = -1;

    /* set starting offsets for all sections */

    if ( ModuleInfo->segorder == SEGORDER_DOSSEG ) {
        DebugMsg(("bin_write_data: .DOSSEG active\n" ));
        /* for .DOSSEG, regroup segments (CODE, UNDEF, DATA, BSS) */
        for ( segtype = typeorder; *segtype != SEGTYPE_ERROR; segtype++ ) {
            DebugMsg(("bin_write_data: searching segment types %Xh\n", *segtype ));
            for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
                if (curr->e.seginfo->segtype != *segtype)
                    continue;
                CalcOffset( curr, first );
                first = FALSE;
                DebugMsg(("bin_write_data: section %s, start ofs=%" FX32 "h, size=%" FX32 "h, file ofs=%" FX32 "h\n",
                          curr->sym.name, curr->e.seginfo->start_offset, curr->sym.max_offset - curr->e.seginfo->start_loc, curr->e.seginfo->fileoffset ));
            }
        }
        SortSegments();
    } else { /* segment order .SEQ (default) and .ALPHA */
        
        if ( ModuleInfo->segorder == SEGORDER_ALPHA ) {
            DebugMsg(("bin_write_data: .ALPHA active\n" ));
            SortSegments();
        }
        for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
            /* ignore absolute segments */
            CalcOffset( curr, first );
            if ( curr->e.seginfo->segtype != SEGTYPE_ABS )
                first = FALSE;
            DebugMsg(("bin_write_data(%s): start ofs=%" FX32 "h, size=%" FX32 "h, file ofs=%" FX32 "h, grp=%s\n",
                      curr->sym.name, curr->e.seginfo->start_offset, curr->sym.max_offset - curr->e.seginfo->start_loc, curr->e.seginfo->fileoffset, (curr->e.seginfo->group ? curr->e.seginfo->group->name : "NULL" )));
        }
    }
    DebugMsg(("bin_write_data: all CalcOffset() done\n" ));

    /* handle relocs */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        /* v2.04: scan ALL fixups! */
        //if ( DoFixup( curr ) == ERROR )
        //    return( ERROR );
        DoFixup( curr );
#if MZ_SUPPORT
        if ( stack == NULL &&
            curr->e.seginfo->combine == COMB_STACK )
            stack = curr;
#endif
    }
    /* v2.04: return if any errors occured during fixup handling */
    if ( ModuleInfo->g.error_count )
        return( ERROR );

    /* for plain binaries make sure the start label is at
     * the beginning of the first segment */
    if ( Options.header_format == HFORMAT_NONE ) {
        if ( ModuleInfo->start_label ) {
            if ( entryoffset == -1 || entryseg != ModuleInfo->start_label->segment ) {
                AsmError( START_LABEL_INVALID );
                return( ERROR );
            }
        }
    }

    sizetotal = GetImageSize( FALSE );

#if MZ_SUPPORT

    /* for MZ format, initialize the header */

    if ( Options.header_format == HFORMAT_MZ ) {
        /* set fields in MZ header */
        DebugMsg(( "bin_write_data: MZ, sizetotal=%Xh\n", sizetotal ));
        pReloc = (uint_16 *)(hdrbuf);
        *(pReloc+0) = 'M' + ('Z' << 8);
        *(pReloc+1) = sizetotal % 512; /* bytes last page */
        *(pReloc+2) = sizetotal / 512 + (sizetotal % 512 ? 1 : 0); /* pages */
        *(pReloc+3) = reloccnt;
        *(pReloc+4) = sizehdr >> 4; /* size header in paras */
        sizeheap = GetImageSize( TRUE ) - sizetotal;
        *(pReloc+5) = sizeheap / 16 + ((sizeheap % 16) ? 1 : 0); /* heap min */
        if (*(pReloc+5) < mzdata.heapmin )
            *(pReloc+5) = mzdata.heapmin;
        *(pReloc+6) = mzdata.heapmax;
        if (*(pReloc+6) < *(pReloc+5))
            *(pReloc+6) = *(pReloc+5); /* heap max */

        /* set stack if there's one defined */

        if ( stack ) {
            uint_32 addr = stack->e.seginfo->start_offset;
            if ( stack->e.seginfo->group )
                addr += stack->e.seginfo->group->offset;
            DebugMsg(("bin_write_data: MZ, stack=%" FX32 "h ofs=%" FX32 "h\n", addr, stack->sym.offset ));
            *(pReloc+7) = (addr >> 4) + ((addr & 0xF) ? 1 : 0); /* SS */
            *(pReloc+8) = stack->sym.offset; /* SP */
        } else {
            AsmWarn( 2, NO_STACK );
        }
        *(pReloc+9) = 0; /* checksum */

        /* set entry CS:IP if defined */

        if ( ModuleInfo->start_label ) {
            uint_32 addr;
            curr = (dir_node *)ModuleInfo->start_label->segment;
            DebugMsg(("bin_write_data, start_label: offs=%" FX32 "h, seg.offs=%" FX32 "h, group.offs=%" FX32 "h\n",
                      ModuleInfo->start_label->offset, curr->e.seginfo->start_offset, curr->e.seginfo->group ? curr->e.seginfo->group->offset : 0 ));
            if ( curr->e.seginfo->group ) {
                addr = curr->e.seginfo->group->offset;
                *(pReloc+10) = (addr & 0xF ) + curr->e.seginfo->start_offset + ModuleInfo->start_label->offset; /* IP */
                *(pReloc+11) = addr >> 4; /* CS */
            } else {
                addr = curr->e.seginfo->start_offset;
                *(pReloc+10) = (addr & 0xF ) + ModuleInfo->start_label->offset; /* IP */
                *(pReloc+11) = addr >> 4; /* CS */
            }
        } else {
            DebugMsg(("bin_write_data, ModuleInfo->start_label=%p\n", ModuleInfo->start_label ));
            AsmWarn( 2, NO_START_LABEL );
        }
        *(pReloc+12) = mzdata.ofs_fixups;
        DebugMsg(("bin_write_data: MZ, mzdata ofs_fixups=%Xh, alignment=%Xh\n", mzdata.ofs_fixups, mzdata.alignment ));
        pReloc = (uint_16 *)(hdrbuf + mzdata.ofs_fixups);
        GetSegRelocs( pReloc );
    }
#endif

#if SECTORMAP
    /* go to EOF */
    if( FileInfo.file[LST] ) {
        fseek( FileInfo.file[LST], 0, SEEK_END );
        LstNL();
        LstNL();
        LstPrintf( szCaption );
        LstNL();
        LstNL();
        LstPrintf( szCaption2 );
        LstNL();
        LstPrintf( szLine );
        LstNL();
    }
#endif

    if ( Options.header_format == HFORMAT_MZ ) {
        if ( fwrite( hdrbuf, 1, sizehdr, FileInfo.file[OBJ] ) != sizehdr )
            WriteError();
#if SECTORMAP
        LstPrintf( szSegLine, szHeader, 0, 0, sizehdr, 0 );
        LstNL();
#endif
    }

#ifdef DEBUG_OUT
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        DebugMsg(("bin_write_data(%s): type=%u written=%" FX32 " max=%" FX32 " start=%" FX32 "\n",
                curr->sym.name, curr->e.seginfo->segtype,
                curr->e.seginfo->bytes_written,
                curr->sym.max_offset,
                curr->e.seginfo->start_loc ));
    }
#endif

    /* write sections */
    for( curr = Tables[TAB_SEG].head, first = TRUE; curr; curr = curr->next ) {
        if ( curr->e.seginfo->segtype == SEGTYPE_ABS ) {
            DebugMsg(("bin_write_data(%s): ABS segment not written\n", curr->sym.name ));
            continue;
        }
        sizemem = curr->sym.max_offset - curr->e.seginfo->start_loc;
        size = sizemem;
        /* if no bytes have been written to the segment, check if there's
         * any further segments with bytes set. If no, skip write! */
        if ( curr->e.seginfo->bytes_written == 0 ) {
            dir_node *dir;
            for ( dir = curr->next; dir; dir = dir->next )
                if ( dir->e.seginfo->bytes_written )
                    break;
            if ( !dir ) {
                DebugMsg(("bin_write_data(%s): segment not written, size=% " FX32 "h sizemem=%" FX32 "\n",
                          curr->sym.name, size, sizemem ));
                size = 0;
            }
        }
#if SECTORMAP
        LstPrintf( szSegLine, curr->sym.name, curr->e.seginfo->fileoffset, curr->e.seginfo->start_offset + curr->e.seginfo->start_loc, size, sizemem );
        LstNL();
#endif
        first = FALSE;
        if (size != 0 && curr->e.seginfo->CodeBuffer ) {
            DebugMsg(("bin_write_data(%s): write %" FX32 "h bytes at offset %" FX32 "h, initialized bytes=%lu, buffer=%p\n",
                      curr->sym.name, size, curr->e.seginfo->fileoffset, curr->e.seginfo->bytes_written, curr->e.seginfo->CodeBuffer ));
            fseek( FileInfo.file[OBJ], curr->e.seginfo->fileoffset, SEEK_SET );
#ifdef __I86__
            if ( hfwrite( curr->e.seginfo->CodeBuffer, 1, size, FileInfo.file[OBJ] ) != size )
                WriteError();
#else
            if ( fwrite( curr->e.seginfo->CodeBuffer, 1, size, FileInfo.file[OBJ] ) != size )
                WriteError();
#endif
        }
    }
#if SECTORMAP
    LstPrintf( szLine );
    LstNL();
#if MZ_SUPPORT
    if ( Options.header_format == HFORMAT_MZ )
        sizeheap += sizetotal - sizehdr;
    else
#endif
        sizeheap = GetImageSize( TRUE );
    LstPrintf( szTotal, " ", sizetotal, sizeheap );
    LstNL();
#endif
    DebugMsg(("bin_write_data: exit\n"));

    return( NOT_ERROR );
}
#endif
