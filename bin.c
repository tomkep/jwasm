/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  BIN output routines.
*
****************************************************************************/


#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <io.h>

#include "globals.h"
#include "symbols.h"
#include "mangle.h"
#include "memalloc.h"
#include "fixup.h"
#include "directiv.h"
#include "queues.h"
#include "bin.h"
#include "myassert.h"

#if BIN_SUPPORT


extern symbol_queue     Tables[];       // tables of definitions
extern asm_sym          *start_label;   // symbol for Modend (COFF)

static uint_32 fileoffset;
static uint_32 entryoffset;
static asm_sym *entryseg;

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

    /* segment not in a group? then start offset is 0 */
    if (curr->e.seginfo->group == NULL)
        offset = 0;
    else
        offset = curr->e.seginfo->group->offset;

    switch (curr->e.seginfo->segrec->d.segdef.align) {
    case SEGDEF_ALIGN_PAGE:
        align = 256;
        break;
    case SEGDEF_ALIGN_PARA:
        align = 16;
        break;
    case SEGDEF_ALIGN_DWORD:
        align = 4;
        break;
    case SEGDEF_ALIGN_WORD:
        align = 2;
        break;
    default:
        align = 1;
    }
    alignbytes = ((offset + (align - 1)) & (-align)) - offset;
    fileoffset += alignbytes;
    offset += alignbytes + curr->e.seginfo->start_loc;
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
    if (curr->e.seginfo->group != NULL)
        curr->e.seginfo->group->offset = offset;
    return;
}

// write section contents
// this is done after the last step only!

int bin_write_data(int fh)
{
    dir_node *curr;
    dir_node *seg;
    uint_32 size;
    uint_32 value;
    seg_type *segtype;
    uint_8 *codeptr;
    uint_16 *codeptr16;
    uint_32 *codeptr32;
    struct asmfixup *fixup;

    DebugMsg(("bin_write_data: enter\n"));

    /* reset the offset fields of segments */
    /* it was used to store the size in there */

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        curr->e.seginfo->offset = 0;
    }

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
                DebugMsg(("bin_write_data: section %s, start ofs=%X, size=%X, file ofs=%u\n", curr->sym.name, curr->e.seginfo->offset, curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc, curr->e.seginfo->fileoffset ));
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
            DebugMsg(("bin_write_data: section %s, start ofs=%X, size=%X, file ofs=%u, grp=%X\n", curr->sym.name, curr->e.seginfo->offset, curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc, curr->e.seginfo->fileoffset, curr->e.seginfo->group ));
        }
    }

    /* make sure the start label is at the beginning of the first segment */
    if (start_label) {
        if (entryoffset == -1 || entryseg != start_label->segment) {
            AsmError( START_LABEL_INVALID );
            return( ERROR );
        }
    }

    /* do relocs and write section contents */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->segtype == SEGTYPE_ABS )
            continue;
        for (fixup = curr->e.seginfo->FixupListHeadCoff; fixup; fixup = fixup->next2) {
            codeptr = curr->e.seginfo->CodeBuffer +
                (fixup->fixup_loc - curr->e.seginfo->start_loc);
            /* assembly time variable in reloc? */
            if (fixup->sym->variable) {
                seg = (dir_node *)fixup->segment;
                value = seg->e.seginfo->offset + fixup->offset;
            } else if (fixup->sym->segment) {
                seg = (dir_node *)fixup->sym->segment;
                value = seg->e.seginfo->offset + fixup->offset + fixup->sym->offset;
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
                *codeptr += (value - fixup->fixup_loc + 1) & 0xff;
                break;
            case FIX_RELOFF16:
                codeptr16 = (uint_16 *)codeptr;
                *codeptr16 += (value - fixup->fixup_loc + 2) & 0xffff;
                break;
            case FIX_OFF16:
                codeptr16 = (uint_16 *)codeptr;
                *codeptr16 = value & 0xffff;
                break;
            case FIX_RELOFF32:
                codeptr32 = (uint_32 *)codeptr;
                *codeptr32 += (value - fixup->fixup_loc + 4);
                break;
            case FIX_OFF32:
                codeptr32 = (uint_32 *)codeptr;
                *codeptr32 = value;
                break;
            case FIX_SEG:
                /* absolute segments are ok */
                if (fixup->sym &&
                    fixup->sym->state == SYM_SEG &&
                    ((dir_node *)fixup->sym)->e.seginfo->segtype == SEGTYPE_ABS) {
                    codeptr16 = (uint_16 *)codeptr;
                    *codeptr16 = ((dir_node *)fixup->sym)->e.seginfo->segrec->d.segdef.abs.frame;
                    break;
                }
            case FIX_PTR16:
            case FIX_PTR32:
            default:
                AsmErr( SEGMENT_FIXUPS_INVALID, fixup->type );
                return( ERROR );
            }
        }
        size = curr->e.seginfo->segrec->d.segdef.seg_length - curr->e.seginfo->start_loc;
        if (size != 0) {
            lseek( fh, curr->e.seginfo->fileoffset, SEEK_SET );
            write( fh, curr->e.seginfo->CodeBuffer, size);
        }
    }

    DebugMsg(("bin_write_data: exit\n"));

    return(NOT_ERROR);
}
#endif
