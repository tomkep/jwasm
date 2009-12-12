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
#include "symbols.h"
#include "memalloc.h"
#include "directiv.h"
#include "fixup.h"
#include "queues.h"
#include "bin.h"
#include "fatal.h"
#include "listing.h"

#if BIN_SUPPORT

#define SECTORMAP 1 /* 1=print sector map in listing file */

extern symbol_queue     Tables[];       // tables of definitions
extern asm_sym          *start_label;   // symbol for Modend (COFF)
extern struct asm_sym   symPC; /* '$' symbol */

#if MZ_SUPPORT
struct MZDATA mzdata = {0x1E, 0x10, 0, 0xFFFF };
#endif

static uint_32 fileoffset;
static uint_32 entryoffset;
static asm_sym *entryseg;
static uint_32 sizehdr;
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
        curr->e.seginfo->segrec->d.segdef.idx = index++;
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
        curr->e.seginfo->start_offset = curr->e.seginfo->segrec->d.segdef.abs.frame << 4;
        DebugMsg(("CalcOffset(%s): abs seg, offset=%Xh\n",
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
        DebugMsg(("CalcOffset(%s): fileofs=%Xh, ofs=%Xh\n", curr->sym.name, fileoffset, offset ));
    } else {
        if ( grp->sym.total_size == 0 ) {
            grp->sym.offset = fileoffset - sizehdr;
            offset = 0;
        } else
            offset = grp->sym.total_size + alignbytes;
        DebugMsg(("CalcOffset(%s): fileofs=%Xh, alignbytes=%u, ofs=%Xh, group=%s, grp.ofs=%Xh\n",
                  curr->sym.name, fileoffset, alignbytes, offset, grp->sym.name, grp->sym.offset ));
    }

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
    DebugMsg(("CalcOffset(%s): seg.fileofs=%Xh, seg.start_offset=%Xh, endofs=%Xh\n",
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
    uint_32 size;
    struct asmfixup *fixup;

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
            continue;
        for ( fixup = curr->e.seginfo->FixupListHeadGen; fixup; fixup = fixup->nextrlc ) {
            switch (fixup->type) {
            case FIX_PTR32:
            case FIX_PTR16:
            case FIX_SEG:
                /* ignore fixups for absolute segments */
                if ( fixup->sym && fixup->sym->segment && ((dir_node *)fixup->sym->segment)->e.seginfo->segtype == SEGTYPE_ABS )
                    break;
                count++;
                if (pDst) {
                    size = fixup->fixup_loc + (curr->e.seginfo->fileoffset & 0xf);
                    if ( fixup->type == FIX_PTR16 )
                        size += 2;
                    else if ( fixup->type == FIX_PTR32 )
                        size += 4;
                    if ( curr->e.seginfo->group) {
                        valueseg = curr->e.seginfo->group->offset >> 4;
                    } else
                        valueseg = curr->e.seginfo->start_offset >> 4;
                    while (size > 0x10000) {
                        size -= 16;
                        valueseg++;
                    };
                    valueofs = size;
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
        DebugMsg(("GetImageSize(%s): fileofs=%Xh, max_offs=%Xh\n", curr->sym.name, curr->e.seginfo->fileoffset, curr->sym.max_offset ));
        tmp = curr->e.seginfo->fileoffset + (curr->sym.max_offset - curr->e.seginfo->start_loc );
        if ( size < tmp )
            size = tmp;
    }
    DebugMsg(("GetImageSize(%u)=%Xh\n", memimage, size ));
    return( size );
}

/* micro-linker. resolve internal fixups.
 */

static ret_code DoFixup( dir_node *curr )
/***************************************/
{
    uint_16 *codeptr16;
    uint_32 *codeptr32;
#if AMD64_SUPPORT
    uint_64 *codeptr64;
#endif
    uint_8 *codeptr;
    dir_node *seg;
    uint_32 value;
    struct asmfixup *fixup;

    if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
        return( NOT_ERROR );

    DebugMsg(("DoFixup(%s) enter, segment start ofs=%lXh\n", curr->sym.name, curr->e.seginfo->start_offset ));
    for ( fixup = curr->e.seginfo->FixupListHeadGen; fixup; fixup = fixup->nextrlc ) {
        codeptr = curr->e.seginfo->CodeBuffer +
            (fixup->fixup_loc - curr->e.seginfo->start_loc);
        /* assembly time variable (also $ symbol) in reloc? */
        if (fixup->sym && fixup->sym->variable) {
            seg = (dir_node *)fixup->segment;
            value = seg->e.seginfo->start_offset + fixup->offset;
            DebugMsg(("DoFixup(%s, %Xh, %s): variable, fixup->segment=%Xh fixup->offset=%lXh, fixup->sym->offset=%lXh\n",
                      curr->sym.name, fixup->fixup_loc, fixup->sym->name, seg, fixup->offset, fixup->sym->offset ));
        } else if ( fixup->sym && fixup->sym->segment ) {
            seg = (dir_node *)fixup->sym->segment;
            /* the offset result consists of
             * - the symbol's offset
             * - the fixup's offset (usually the displacement )
             * - the segment/group offset in the image
             */
            if ( fixup->type == FIX_OFF32_IMGREL ) {
                value = ( fixup->offset + fixup->sym->offset + seg->e.seginfo->start_offset ) - imagestart;
                DebugMsg(("DoFixup(%s): IMGREL, loc=%lX value=%lX seg.start=%lX imagestart=%lX\n",
                          curr->sym.name, fixup->fixup_loc, value, seg->e.seginfo->start_offset, imagestart ));
            } else if ( fixup->type == FIX_OFF32_SECREL  ) {
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
                            DebugMsg(("DoFixup(%s): SECREL, primary seg=%s, start_offset=%X\n",
                                      curr->sym.name, segfirst->sym.name, segfirst->e.seginfo->start_offset ));
                            break;
                        }
                    }
                }
                DebugMsg(("DoFixup(%s): SECREL, loc=%lX, value=%lX\n", curr->sym.name, fixup->fixup_loc, value ));
            /* v2.01: don't use group if fixup explicitely refers the segment! */
            //} else if ( seg->e.seginfo->group ) {
            } else if ( seg->e.seginfo->group && fixup->frame != FRAME_SEG ) {
                value = (seg->e.seginfo->group->offset & 0xF) + seg->e.seginfo->start_offset + fixup->offset + fixup->sym->offset;
            } else if ( fixup->type >= FIX_RELOFF8 && fixup->type <= FIX_RELOFF32 ) {
                /* v1.96: special handling for "relative" fixups */
                value = seg->e.seginfo->start_offset + fixup->offset + fixup->sym->offset;
            } else
                value = (seg->e.seginfo->start_offset & 0xF) + fixup->offset + fixup->sym->offset;
            DebugMsg(("DoFixup(%s, %lXh, %s): target->start_offset=%lXh, fixup->offset=%lXh, fixup->sym->offset=%lXh\n",
                      curr->sym.name, fixup->fixup_loc, fixup->sym->name, seg->e.seginfo->start_offset, fixup->offset, fixup->sym->offset ));
        } else {
            seg = (dir_node *)fixup->segment;
            DebugMsg(("DoFixup(%s, %lXh, %s): target segment=0, fixup->offset=%lXh, fixup->sym->offset=%lXh\n",
                      curr->sym.name, fixup->fixup_loc, fixup->sym ? fixup->sym->name : "", fixup->offset, fixup->sym ? fixup->sym->offset : 0 ));
            value = 0;
        }
        switch (fixup->type) {
        case FIX_RELOFF8:
            //*codeptr += (value - fixup->fixup_loc + 1) & 0xff;
            // changed in v1.95
            *codeptr += (value - (fixup->fixup_loc + curr->e.seginfo->start_offset) - 1) & 0xff;
            DebugMsg(("DoFixup(%s, %lXh): FIX_RELOFF8, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr ));
            break;
        case FIX_RELOFF16:
            codeptr16 = (uint_16 *)codeptr;
            //*codeptr16 += (value - fixup->fixup_loc + 2) & 0xffff;
            // changed in v1.95
            *codeptr16 += (value - (fixup->fixup_loc + curr->e.seginfo->start_offset) - 2) & 0xffff;
            DebugMsg(("DoFixup(%s, %lXh): FIX_RELOFF16, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr16 ));
            break;
        case FIX_RELOFF32:
#if AMD64_SUPPORT
            /* adjust the location for EIP-related offsets if USE64 */
            if ( curr->e.seginfo->Ofssize == USE64 ) {
                fixup->fixup_loc += fixup->addbytes - 4;
            }
#endif
            codeptr32 = (uint_32 *)codeptr;
            //*codeptr32 += (value - fixup->fixup_loc + 4);
            // changed in v1.95
            *codeptr32 += (value - (fixup->fixup_loc + curr->e.seginfo->start_offset) - 4);
            DebugMsg(("DoFixup(%s, %lXh): FIX_RELOFF32, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr32 ));
            break;
        case FIX_LOBYTE:
            *codeptr = value & 0xff;
            DebugMsg(("DoFixup(%s, %lXh): FIX_LOBYTE, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr ));
            break;
        case FIX_HIBYTE:
            *codeptr = (value >> 8) & 0xff;
            DebugMsg(("DoFixup(%s, %lXh): FIX_HIBYTE, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr ));
            break;
        case FIX_OFF16:
            codeptr16 = (uint_16 *)codeptr;
            *codeptr16 = value & 0xffff;
            DebugMsg(("DoFixup(%s, %lXh): FIX_OFF16, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr16 ));
            break;
        case FIX_OFF32:
            codeptr32 = (uint_32 *)codeptr;
            *codeptr32 = value;
            DebugMsg(("DoFixup(%s, %lXh): FIX_OFF32, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr32 ));
            break;
        case FIX_OFF32_IMGREL:
            codeptr32 = (uint_32 *)codeptr;
            *codeptr32 = value;
            DebugMsg(("DoFixup(%s, %lXh): FIX_OFF32_IMGREL, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr32 ));
        case FIX_OFF32_SECREL:
            codeptr32 = (uint_32 *)codeptr;
            *codeptr32 = value;
            DebugMsg(("DoFixup(%s, %lXh): FIX_OFF32_SECREL, value=%lXh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr32 ));
            break;
#if AMD64_SUPPORT
        case FIX_OFF64:
            codeptr64 = (uint_64 *)codeptr;
            *codeptr64 = value;
            DebugMsg(("DoFixup(%s, %lXh): FIX_OFF64, value=%lXh, *target=%I64Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr64 ));
            break;
#endif
        case FIX_SEG:
            codeptr16 = (uint_16 *)codeptr;
            /* absolute segments are ok */
            if (fixup->sym &&
                fixup->sym->state == SYM_SEG &&
                ((dir_node *)fixup->sym)->e.seginfo->segtype == SEGTYPE_ABS) {
                *codeptr16 = ((dir_node *)fixup->sym)->e.seginfo->segrec->d.segdef.abs.frame;
                break;
            }
#if MZ_SUPPORT
            if ( Options.header_format == HFORMAT_MZ ) {
                seg = (dir_node *)fixup->sym;
                if ( seg->sym.state == SYM_GRP ) {
                    *codeptr16 = seg->sym.offset >> 4;
                    DebugMsg(("DoFixup(%s, %lXh): FIX_SEG, group.offset=%lXh\n", curr->sym.name, fixup->fixup_loc, seg->sym.offset ));
                } else if (seg->e.seginfo->group ) {
                    *codeptr16 = (seg->e.seginfo->start_offset + seg->e.seginfo->group->offset) >> 4;
                    DebugMsg(("DoFixup(%s, %lXh): FIX_SEG, segment.offset=%lXh, group.offset=%lXh\n", curr->sym.name, fixup->fixup_loc, seg->e.seginfo->start_offset, seg->e.seginfo->group->offset ));
                } else {
                    *codeptr16 = seg->e.seginfo->start_offset >> 4;
                    DebugMsg(("DoFixup(%s, %lXh): FIX_SEG, segment.offset=%lXh\n", curr->sym.name, fixup->fixup_loc, seg->e.seginfo->start_offset ));
                }
                break;
            }
#endif
        case FIX_PTR16:
#if MZ_SUPPORT
            if ( Options.header_format == HFORMAT_MZ ) {
                DebugMsg(("DoFixup(%s, %lXh): FIX_PTR16, seg->start=%Xh\n", curr->sym.name, fixup->fixup_loc, seg->e.seginfo->start_offset ));
                codeptr16 = (uint_16 *)codeptr;
                *codeptr16 = value & 0xffff;
                codeptr16++;
                if (seg->e.seginfo->group ) {
                    *codeptr16 = (seg->e.seginfo->start_offset + seg->e.seginfo->group->offset) >> 4;
                } else {
                    *codeptr16 = seg->e.seginfo->start_offset >> 4;
                }
                break;
            }
#endif
        case FIX_PTR32:
#if MZ_SUPPORT
            if ( Options.header_format == HFORMAT_MZ ) {
                DebugMsg(("DoFixup(%s, %lXh): FIX_PTR32\n", curr->sym.name, fixup->fixup_loc ));
                codeptr32 = (uint_32 *)codeptr;
                *codeptr32 = value;
                codeptr32++;
                codeptr16 = (uint_16 *)codeptr32;
                if (seg->e.seginfo->group ) {
                    *codeptr16 = (seg->e.seginfo->start_offset + seg->e.seginfo->group->offset) >> 4;
                } else {
                    *codeptr16 = seg->e.seginfo->start_offset >> 4;
                }
                break;
            }
#endif
        default:
            DebugMsg(("DoFixup(%s, %lXh): invalid fixup %u\n", curr->sym.name, fixup->fixup_loc, fixup->type ));
            AsmErr( INVALID_FIXUP_TYPE, "BIN", fixup->fixup_loc );
            return( ERROR );
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

    DebugMsg(("bin_write_data: enter ($=%Xh)\n", symPC.offset ));

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        /* reset the offset fields of segments */
        /* it was used to store the size in there */
        curr->e.seginfo->start_offset = 0;
        /* set STACK segment type */
        if ( curr->e.seginfo->segrec->d.segdef.combine == COMB_STACK )
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
        DebugMsg(("bin_write_data: MZ format, fixups=%u, sizehdr=%X\n", reloccnt, sizehdr ));
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
                DebugMsg(("bin_write_data: section %s, start ofs=%Xh, size=%Xh, file ofs=%Xh\n",
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
            DebugMsg(("bin_write_data(%s): start ofs=%Xh, size=%Xh, file ofs=%Xh, grp=%s\n",
                      curr->sym.name, curr->e.seginfo->start_offset, curr->sym.max_offset - curr->e.seginfo->start_loc, curr->e.seginfo->fileoffset, (curr->e.seginfo->group ? curr->e.seginfo->group->name : "NULL" )));
        }
    }
    DebugMsg(("bin_write_data: all CalcOffset() done\n" ));

    /* handle relocs */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( DoFixup( curr ) == ERROR )
            return( ERROR );
#if MZ_SUPPORT
        if ( stack == NULL &&
            curr->e.seginfo->segrec->d.segdef.combine == COMB_STACK )
            stack = curr;
#endif
    }

    /* for plain binaries make sure the start label is at
     * the beginning of the first segment */
    if ( Options.header_format == HFORMAT_NONE ) {
        if ( start_label ) {
            if ( entryoffset == -1 || entryseg != start_label->segment ) {
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
            DebugMsg(("bin_write_data: MZ, stack=%Xh ofs=%Xh\n", addr, stack->sym.offset ));
            *(pReloc+7) = (addr >> 4) + ((addr & 0xF) ? 1 : 0); /* SS */
            *(pReloc+8) = stack->sym.offset; /* SP */
        } else {
            AsmWarn( 2, NO_STACK );
        }
        *(pReloc+9) = 0; /* checksum */

        /* set entry CS:IP if defined */

        if ( start_label ) {
            uint_32 addr;
            curr = (dir_node *)start_label->segment;
            DebugMsg(("bin_write_data, start_label: offs=%Xh, seg.offs=%Xh, group.offs=%Xh\n",
                      start_label->offset, curr->e.seginfo->start_offset, curr->e.seginfo->group ? curr->e.seginfo->group->offset : 0 ));
            if ( curr->e.seginfo->group ) {
                addr = curr->e.seginfo->group->offset;
                *(pReloc+10) = (addr & 0xF ) + curr->e.seginfo->start_offset + start_label->offset; /* IP */
                *(pReloc+11) = addr >> 4; /* CS */
            } else {
                addr = curr->e.seginfo->start_offset;
                *(pReloc+10) = (addr & 0xF ) + start_label->offset; /* IP */
                *(pReloc+11) = addr >> 4; /* CS */
            }
        } else {
            AsmWarn( 2, NO_START_LABEL );
        }
        *(pReloc+12) = mzdata.ofs_fixups;
        DebugMsg(("bin_write_data: MZ, mzdata ofs_fixups=%Xh, alignment=%Xh\n", mzdata.ofs_fixups, mzdata.alignment ));
        pReloc = (uint_16 *)(hdrbuf + mzdata.ofs_fixups);
        GetSegRelocs( pReloc );
    }
#endif

#if SECTORMAP
    LstNL();
    LstNL();
    LstPrintf( szCaption );
    LstNL();
    LstNL();
    LstPrintf( szCaption2 );
    LstNL();
    LstPrintf( szLine );
    LstNL();
#endif

    if ( Options.header_format == HFORMAT_MZ ) {
        if ( fwrite( hdrbuf, 1, sizehdr, FileInfo.file[OBJ] ) != sizehdr )
            WriteError();
#if SECTORMAP
        LstPrintf( szSegLine, szHeader, 0, 0, sizehdr, 0 );
        LstNL();
#endif
    }

    /* write sections */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
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
                DebugMsg(("bin_write_data(%s): segment not written, size=%Xh\n", curr->sym.name, size ));
                size = 0;
            }
        }
#if SECTORMAP
        LstPrintf( szSegLine, curr->sym.name, curr->e.seginfo->fileoffset, curr->e.seginfo->start_offset + curr->e.seginfo->start_loc, size, sizemem );
        LstNL();
#endif
        if (size != 0 && curr->e.seginfo->CodeBuffer ) {
            DebugMsg(("bin_write_data(%s): write %Xh bytes at offset %Xh, initialized bytes=%lu, buffer=%Xh\n", curr->sym.name, size, curr->e.seginfo->fileoffset, curr->e.seginfo->bytes_written, curr->e.seginfo->CodeBuffer ));
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
