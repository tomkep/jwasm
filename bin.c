/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  BIN output routines.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "myunistd.h"
#include "symbols.h"
#include "mangle.h"
#include "memalloc.h"
#include "fixup.h"
#include "directiv.h"
#include "queues.h"
#include "bin.h"
#include "fatal.h"

#if BIN_SUPPORT

/* BINHDR makes JWasm aware of a special segment with class "BINHDR".
 * It will put segment relocs in DOS MZ format in this segment.
 * This allows native support for DOS MZ binaries.
 */

#define BINHDR 0 /* 1 won't work yet */

extern symbol_queue     Tables[];       // tables of definitions
extern asm_sym          *start_label;   // symbol for Modend (COFF)

static uint_32 fileoffset;
static uint_32 entryoffset;
static asm_sym *entryseg;
#if BINHDR
static uint_32 sizehdr;
#endif

/* reorder segments for DOSSEG:
 1. code
 2. unknown
 3. initialized data
 4. uninitialized data
 */
static seg_type typeorder[] = {SEGTYPE_CODE, SEGTYPE_UNDEF, SEGTYPE_DATA, SEGTYPE_BSS, 0};

void SortSegments( void )
{
    bool changed = TRUE;
    dir_node *curr;
    int index = 1;

    while (changed == TRUE) {
        dir_node *prev = NULL;
        changed = FALSE;
        for( curr = Tables[TAB_SEG].head; curr && curr->next ; prev = curr, curr = curr->next ) {
            if (strcmp(curr->sym.name, curr->next->sym.name) > 0) {
                dir_node *swap = curr->next;
                changed = TRUE;
                if (prev == NULL) {
                    Tables[TAB_SEG].head = swap;
                } else {
                    prev->next = swap;
                }
                curr->next = swap->next;
                swap->next = curr;
            }
        }
    }
    for (curr = Tables[TAB_SEG].head; curr ; curr = curr->next) {
        curr->e.seginfo->segrec->d.segdef.idx = index++;
    }
}

/* for segments contained in a GROUP, calc starting offset */

static void CalcOffset( dir_node *curr )
{
    uint_32 align;
    uint_32 alignbytes;
    uint_32 offset;
    dir_node *grp;

    /* segment not in a group? then start offset is 0 */
    grp = (dir_node *)curr->e.seginfo->group;
    if (grp == NULL)
        offset = 0;
    else
        offset = grp->sym.total_size;

    if ( curr->e.seginfo->alignment == MAX_SEGALIGNMENT )
        align = 1;
    else
        align = 1 << curr->e.seginfo->alignment;

    //alignbytes = ((offset + (align - 1)) & (-align)) - offset;
#if BINHDR
	alignbytes = (((fileoffset-sizehdr) + (align - 1)) & (-align)) - (fileoffset-sizehdr);
#else
	alignbytes = ((fileoffset + (align - 1)) & (-align)) - fileoffset;
#endif
	fileoffset += alignbytes;
    DebugMsg(("CalcOffset(%s): ofs=%Xh, alignbytes=%u, fileofs=%Xh\n", curr->sym.name, offset, alignbytes, fileoffset ));
#if BINHDR
    /* set offset of group within memory image */
    if (grp && grp->sym.total_size == 0)
        grp->sym.offset = fileoffset - sizehdr;
#endif
    /* v1.95: start_loc is now added to group->offset, not to offset */
    //offset += alignbytes + curr->e.seginfo->start_loc;
#if 1
	offset += alignbytes;
#else
    offset += fileoffset & 0xf; /* wrong! */
#endif
	curr->e.seginfo->fileoffset = fileoffset;

    curr->e.seginfo->offset = offset;

    /* there's no real entry address for BIN, therefore the
     start label must be at the very beginning of the file */
    if (entryoffset == -1) {
        entryoffset = offset;
        entryseg = (asm_sym *)curr;
    }
    fileoffset += curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc;
    offset += curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc;
    if ( grp ) {
        grp->sym.total_size = offset + curr->e.seginfo->start_loc;
        DebugMsg(("CalcOffset: group=%s, offset=%X\n", grp->sym.name, offset ));
    }
    return;
}

#if BINHDR

/*
 * if pDst==NULL: count the number of segment related fixups
 */

static int GetRelocs( dir_node *hdr, uint_16 *pDst )
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
        if (curr == hdr )
            continue;
        for (fixup = curr->e.seginfo->FixupListHeadGen; fixup; fixup = fixup->next2) {
            switch (fixup->type) {
            case FIX_PTR32:
            case FIX_PTR16:
            case FIX_SEG:
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
                        valueseg = (curr->e.seginfo->fileoffset - sizehdr) >> 4;
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
    DebugMsg(("GetRelocs()=%u\n", count ));
    return( count );
}

uint_32 GetImageSize( dir_node *hdr, bool memimage )
{
    dir_node *curr;
    uint_32 size = 0;
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
            continue;
        if ( memimage == FALSE ) {
            if ( curr->e.seginfo->segtype == SEGTYPE_BSS )
                continue;
            if ( curr->e.seginfo->segrec->d.segdef.combine == COMB_STACK )
                continue;
        } else {
            if ( curr == hdr )
                continue;
        }
        size += curr->e.seginfo->segrec->d.segdef.seg_length;
    }
    return( size );
}
#endif

#if BINHDR
static ret_code DoFixup( dir_node *curr, dir_node *relocseg )
#else
static ret_code DoFixup( dir_node *curr )
#endif
{
    uint_16 *codeptr16;
    uint_32 *codeptr32;
    uint_8 *codeptr;
    dir_node *seg;
    uint_32 value;
    struct asmfixup *fixup;

    if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
        return( NOT_ERROR );
    DebugMsg(("DoFixup(%s) enter\n", curr->sym.name ));
    for ( fixup = curr->e.seginfo->FixupListHeadGen; fixup; fixup = fixup->next2 ) {
        codeptr = curr->e.seginfo->CodeBuffer +
            (fixup->fixup_loc - curr->e.seginfo->start_loc);
        /* assembly time variable in reloc? */
        if (fixup->sym->variable) {
            seg = (dir_node *)fixup->segment;
            value = seg->e.seginfo->offset + fixup->offset;
        } else if (fixup->sym->segment) {
            seg = (dir_node *)fixup->sym->segment;
            value = seg->e.seginfo->offset + fixup->offset + fixup->sym->offset;
            DebugMsg(("DoFixup(%s): fixup->sym->segment->offset=%Xh, fixup->offset=%Xh, fixup->sym->offset=%Xh\n",
                      curr->sym.name, seg->e.seginfo->offset, fixup->offset, fixup->sym->offset ));
        } else {
            value = 0;
        }
        switch (fixup->type) {
        case FIX_LOBYTE:
            *codeptr = value & 0xff;
            break;
        case FIX_HIBYTE:
            *codeptr = (value >> 8) & 0xff;
            break;
        case FIX_RELOFF8:
            //*codeptr += (value - fixup->fixup_loc + 1) & 0xff;
            // changed in v1.95
            *codeptr += (value - (fixup->fixup_loc + curr->e.seginfo->offset) - 1) & 0xff;
            break;
        case FIX_RELOFF16:
            codeptr16 = (uint_16 *)codeptr;
            //*codeptr16 += (value - fixup->fixup_loc + 2) & 0xffff;
            // changed in v1.95
            *codeptr16 += (value - (fixup->fixup_loc + curr->e.seginfo->offset) - 2) & 0xffff;
            DebugMsg(("DoFixup(%s): FIX_RELOFF16 at %Xh, value=%Xh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr16 ));
            break;
        case FIX_OFF16:
            codeptr16 = (uint_16 *)codeptr;
            *codeptr16 = value & 0xffff;
            DebugMsg(("DoFixup(%s): FIX_OFF16 at %Xh, value=%Xh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr16 ));
            break;
        case FIX_RELOFF32:
            codeptr32 = (uint_32 *)codeptr;
            //*codeptr32 += (value - fixup->fixup_loc + 4);
            // changed in v1.95
            *codeptr32 += (value - (fixup->fixup_loc + curr->e.seginfo->offset) - 4);
            DebugMsg(("DoFixup(%s): FIX_RELOFF32 at %Xh, value=%Xh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr32 ));
            break;
        case FIX_OFF32:
            codeptr32 = (uint_32 *)codeptr;
            *codeptr32 = value;
            DebugMsg(("DoFixup(%s): FIX_OFF32 at %Xh, value=%Xh, *target=%Xh\n", curr->sym.name, fixup->fixup_loc, value, *codeptr32 ));
            break;
        case FIX_SEG:
            codeptr16 = (uint_16 *)codeptr;
            /* absolute segments are ok */
            if (fixup->sym &&
                fixup->sym->state == SYM_SEG &&
                ((dir_node *)fixup->sym)->e.seginfo->segtype == SEGTYPE_ABS) {
                *codeptr16 = ((dir_node *)fixup->sym)->e.seginfo->segrec->d.segdef.abs.frame;
                break;
            }
#if BINHDR
            if (relocseg) {
                seg = (dir_node *)fixup->sym;
                if ( seg->sym.state == SYM_GRP )
                    *codeptr16 = seg->sym.offset >> 4;
                else if (seg->e.seginfo->group ) {
                    *codeptr16 = seg->e.seginfo->group->offset >> 4;
                    DebugMsg(("DoFixup(%s): FIX_SEG detected at %Xh, group.offset=%X\n", curr->sym.name, fixup->fixup_loc, seg->e.seginfo->group->offset ));
                } else {
                    *codeptr16 = seg->e.seginfo->offset >> 4;
                    DebugMsg(("DoFixup(%s): FIX_SEG detected at %Xh, segment.offset=%X\n", curr->sym.name, fixup->fixup_loc, seg->e.seginfo->offset ));
                }
                break;
            }
#endif
        case FIX_PTR16:
#if BINHDR
            if (relocseg) {
                DebugMsg(("DoFixup(%s): FIX_PTR16 detected at %Xh\n", curr->sym.name, fixup->fixup_loc ));
                break;
            }
#endif
        case FIX_PTR32:
#if BINHDR
            if (relocseg) {
                DebugMsg(("DoFixup(%s): FIX_PTR32 detected at %Xh\n", curr->sym.name, fixup->fixup_loc ));
                break;
            }
#endif
        default:
            AsmErr( SEGMENT_FIXUPS_INVALID, fixup->type );
            return( ERROR );
        }
    }
    return( NOT_ERROR );
}

// write section contents
// this is done after the last step only!

ret_code bin_write_data(int fh)
{
    dir_node *curr;
    uint_32 size;
    seg_type *segtype;
#if BINHDR
    dir_node *relocseg = NULL;
    uint_32 relocofs;
    uint_16 reloccnt;
    uint_16 *pReloc;
    uint_32 sizetotal;
    uint_32 sizeimage;
#endif

    DebugMsg(("bin_write_data: enter\n"));

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        /* reset the offset fields of segments */
        /* it was used to store the size in there */
        curr->e.seginfo->offset = 0;
    }
#if BINHDR
    sizehdr = 0;
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        /* check if there's a BINHDR segment which is at least 0x18 bytes long */
        char *token = GetLname( curr->e.seginfo->segrec->d.segdef.class_name_idx );
        if ( (strcmp( token, "BINHDR") == 0) &&
            curr->e.seginfo->segrec->d.segdef.seg_length >= 0x18 ) {
            relocseg = curr;
            DebugMsg(("bin_write_data: BINHDR segment %s found\n", relocseg->sym.name ));
            reloccnt = GetRelocs( relocseg, NULL );
            DoFixup( relocseg, relocseg );
            pReloc = (uint_16 *)relocseg->e.seginfo->CodeBuffer;
            pReloc += *(pReloc+12) >> 1; /* ==+18h in bytes */
            sizehdr = (reloccnt * 4 + ( (uint_8 *)pReloc - relocseg->e.seginfo->CodeBuffer) + 0xF) & ~0xF;
            /* will relocs fit in header segment? */
            if (sizehdr > relocseg->e.seginfo->segrec->d.segdef.seg_length ) {
                void *tmpbuf = AsmAlloc( sizehdr );
                DebugMsg(("bin_write_data: segment %s resized to 0x%Xh\n", relocseg->sym.name, sizehdr ));
                memcpy( tmpbuf, relocseg->e.seginfo->CodeBuffer, relocseg->e.seginfo->segrec->d.segdef.seg_length);
                relocseg->e.seginfo->CodeBuffer = tmpbuf;
                relocseg->e.seginfo->segrec->d.segdef.seg_length = sizehdr;
            }
            break; /* skip further segments */
        }
    }
#endif

    fileoffset = 0;
    entryoffset = -1;

    /* set starting offsets for all sections */

    if ( ModuleInfo.segorder == SEGORDER_DOSSEG ) {
        /* for .DOSSEG, regroup segments (CODE, UNDEF, DATA, BSS) */
        for ( segtype = typeorder; *segtype; segtype++ ) {
            for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
                if (curr->e.seginfo->segtype != *segtype)
                    continue;
                CalcOffset(curr);
                DebugMsg(("bin_write_data: section %s, start ofs=%Xh, size=%Xh, file ofs=%Xh\n", curr->sym.name, curr->e.seginfo->offset, curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc, curr->e.seginfo->fileoffset ));
            }
        }
    } else { /* segment order .SEQ (default) and .ALPHA */
        
        if ( ModuleInfo.segorder == SEGORDER_ALPHA )
            SortSegments();

        for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
            /* ignore absolute segments */
            if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
                continue;
            CalcOffset(curr);
            DebugMsg(("bin_write_data(%s): start ofs=%Xh, size=%Xh, file ofs=%Xh, grp=%X\n", curr->sym.name, curr->e.seginfo->offset, curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc, curr->e.seginfo->fileoffset, curr->e.seginfo->group ));
        }
    }

    /* make sure the start label is at the beginning of the first segment */
#if BINHDR
    if ( start_label && relocseg == NULL) {
#else
    if ( start_label ) {
#endif
        if (entryoffset == -1 || entryseg != start_label->segment) {
            AsmError( START_LABEL_INVALID );
            return( ERROR );
        }
    }

    /* handle relocs */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
#if BINHDR
        if (curr != relocseg)
            if (DoFixup( curr, relocseg ) == ERROR)
                return( ERROR );
#else
        if (DoFixup( curr ) == ERROR)
            return( ERROR );
#endif
    }
#if BINHDR
    if ( relocseg ) {
        pReloc = (uint_16 *)relocseg->e.seginfo->CodeBuffer;
        sizetotal = GetImageSize( relocseg, FALSE );
        /* set fields in MZ header */
        DebugMsg(("bin_write_data: sizetotal=%Xh\n", sizetotal));
        *(pReloc+1) = sizetotal % 512; /* bytes last page */
        *(pReloc+2) = sizetotal / 512 + 1; /* pages */
        *(pReloc+3) = reloccnt;
        *(pReloc+4) = relocseg->e.seginfo->segrec->d.segdef.seg_length >> 4;
        sizeimage = GetImageSize( relocseg, TRUE );
        *(pReloc+5) = sizeimage / 16 + ((sizeimage % 16) ? 1 : 0); /* min alloc */
        if (*(pReloc+6) < *(pReloc+5))
            *(pReloc+6) = *(pReloc+5);
        pReloc += *(pReloc+12) >> 1;
        GetRelocs( relocseg, pReloc );
    }
#endif

    /* write sections */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
            continue;
        /* don't write stack segment if it is the last segment! */
        if ( curr->next == NULL &&
            curr->e.seginfo->segrec->d.segdef.combine == COMB_STACK )
            continue;
        size = curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc;
        if (size != 0) {
            DebugMsg(("bin_write_data(%s): write %Xh bytes at offset %Xh\n", curr->sym.name, size, curr->e.seginfo->fileoffset ));
            _lseek( fh, curr->e.seginfo->fileoffset, SEEK_SET );
            if (_write( fh, curr->e.seginfo->CodeBuffer, size) != size)
                WriteError();
        }
    }
    DebugMsg(("bin_write_data: exit\n"));

    return( NOT_ERROR );
}
#endif
