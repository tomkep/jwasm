/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  COFF output routines
*
****************************************************************************/


#include <errno.h>
#include <ctype.h>
#include <time.h>
#include <io.h>

#include "globals.h"
#include "symbols.h"
#include "fatal.h"
#include "mangle.h"
#include "memalloc.h"
#include "fixup.h"
#include "directiv.h"
#include "queues.h"
#include "coff.h"
#include "myassert.h"

#define SETDATAPOS 1

typedef struct stringitem {
    struct stringitem *next;
    char string[];
} stringitem;

extern dir_node * GetPublicData2( void ** );
extern line_num_info * GetLinnumData2( void ** );

extern symbol_queue     Tables[];       // tables of definitions
extern unsigned  total_segs;
extern asm_sym          *start_label;   // symbol for Modend (COFF)
extern uint             segdefidx;      // Number of Segment definition

static uint_32 coff_raw_data;  /* total of section contents (incl relocs + linnums) */
static uint_32 size_drectve;   /* size of .drectve section */
static dir_node *directives;

static IMAGE_FILE_HEADER ifh;

static stringitem *LongNamesHead;
static stringitem *LongNamesTail;
static uint_32 SizeLongNames;
static char *srcname; /* name of source module (name + extension) */

#if SETDATAPOS
static uint_32 data_pos;
#endif

/* translate section names: */
// _TEXT -> .text
// _DATA -> .data
// _BSS  -> .bss
// CONST -> .rdata

static char * CoffConvertSectionName(asm_sym * sym)
{
    static char coffname[MAX_LINE_LEN];

    if (memcmp(sym->name, "_TEXT", 5) == 0) {
        if (sym->name[5] == NULLC)
            return(".text");
        else if (sym->name[5] == '$') {
            strcpy(coffname, ".text");
            strcpy(coffname+5, sym->name+5);
            return(coffname);
        }
    } else if (memcmp(sym->name, "_DATA", 5) == 0) {
        if (sym->name[5] == NULLC)
            return(".data");
        else if (sym->name[5] == '$') {
            strcpy(coffname, ".data");
            strcpy(coffname+5, sym->name+5);
            return(coffname);
        }
    } else if (memcmp(sym->name, "CONST", 5) == 0) {
        if (sym->name[5] == NULLC)
            return(".rdata");
        else if (sym->name[5] == '$') {
            strcpy(coffname, ".rdata");
            strcpy(coffname+6, sym->name+5);
            return(coffname);
        }
    } else if (strcmp(sym->name, "_BSS") == 0) {
        return(".bss");
    }
    return(sym->name);
}

// alloc a string which will be stored in the COFF string table

static uint_32 Coff_AllocString(char * string, int len)
{
    stringitem *name;
    uint_32 oldsize = SizeLongNames;

    SizeLongNames += len+1;
    name = AsmAlloc(len+1+sizeof(stringitem));
    name->next = NULL;
    strcpy(name->string, string);
    if (LongNamesHead) {
        LongNamesTail->next = name;
        LongNamesTail = name;
    } else {
        LongNamesHead = LongNamesTail = name;
    }
    return(oldsize);
}

/* write COFF section table */

int coff_write_section_table( int fh )
{
    dir_node    *curr;
    obj_rec     *objr;
    char        *p;
    struct asmfixup *fix;
    uint        seg_index;
    uint        offset;
    uint        len;
    IMAGE_SECTION_HEADER ish;
    uint        size_relocs = 0;
    char        buffer[256];

    DebugMsg(("coff_write_section_table: enter\n"));

    offset = sizeof(IMAGE_FILE_HEADER) + total_segs * sizeof(IMAGE_SECTION_HEADER);
    for( seg_index = 1; seg_index <= total_segs; seg_index++ ) {
        /* find segment by index */
        for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
            if( GetSegIdx( curr->sym.segment ) == seg_index ) {
                break;
            }
        }
        if( curr == NULL )
            continue;
        if( curr->sym.state != SYM_SEG ) {
            AsmErr( SEG_NOT_DEFINED, curr->sym.name );
            continue;
        }
        /* if section name is longer than 8 chars, a '/' is stored,
         followed by a number in ascii which is the offset for the string table
         */
        strncpy(buffer, CoffConvertSectionName(&curr->sym), sizeof(buffer));
        len = strlen(buffer);
        if (len <= IMAGE_SIZEOF_SHORT_NAME)
            strncpy(ish.Name, buffer, IMAGE_SIZEOF_SHORT_NAME);
        else
            sprintf(ish.Name, "/%u", Coff_AllocString(buffer, len));

        ish.Misc.PhysicalAddress = offset - (size_relocs + sizeof(IMAGE_FILE_HEADER) + total_segs * sizeof(IMAGE_SECTION_HEADER));
        ish.VirtualAddress = 0;
        ish.SizeOfRawData = curr->e.seginfo->segrec->d.segdef.seg_length;
        if (ish.SizeOfRawData)
            ish.PointerToRawData = offset;
        else
            ish.PointerToRawData = 0;

        /* set Characteristics in section header */

        ish.Characteristics = 0;

        switch (curr->e.seginfo->segrec->d.segdef.align) {
        case SEGDEF_ALIGN_ABS:
        case SEGDEF_ALIGN_BYTE:
            ish.Characteristics |= IMAGE_SCN_ALIGN_1BYTES;
            break;
        case SEGDEF_ALIGN_WORD:
            ish.Characteristics |= IMAGE_SCN_ALIGN_2BYTES;
            break;
        case SEGDEF_ALIGN_DWORD:
            ish.Characteristics |= IMAGE_SCN_ALIGN_4BYTES;
            break;
        case SEGDEF_ALIGN_PARA:
            ish.Characteristics |= IMAGE_SCN_ALIGN_16BYTES;
            break;
        case SEGDEF_ALIGN_PAGE:
            ish.Characteristics |= IMAGE_SCN_ALIGN_512BYTES;
            break;
        case SEGDEF_ALIGN_4KPAGE:
            ish.Characteristics |= IMAGE_SCN_ALIGN_4096BYTES;
            break;
        }
        if (curr->e.seginfo->segtype == SEGTYPE_CODE)
            ish.Characteristics |= IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ;
        else if (curr->e.seginfo->segtype == SEGTYPE_BSS) {
            ish.Characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
            /* ish.SizeOfRawData = 0; */
            ish.PointerToRawData = 0;
        } else if (curr->e.seginfo->segrec->d.segdef.combine == COMB_STACK) {
            ish.Characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
            ish.SizeOfRawData = 0;
            ish.PointerToRawData = 0;
        } else if (curr->e.seginfo->readonly) {
            ish.Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        } else if ((p = GetLname( curr->e.seginfo->segrec->d.segdef.class_name_idx )) && strcmp( p, "CONST") == 0) {
            ish.Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        } else if (strcmp(curr->sym.name, ".drectve") == 0) {
            ish.Characteristics &= 0xFF0FFFFF;
            ish.Characteristics |= IMAGE_SCN_LNK_INFO | IMAGE_SCN_LNK_REMOVE;
        } else
            ish.Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;

        if (ish.PointerToRawData)
            offset += ish.SizeOfRawData;

        /* set relocation pos+count in section header */

        ish.PointerToRelocations = NULL;
        ish.NumberOfRelocations = 0;
        if (curr->e.seginfo->FixupListHeadCoff) {
            for ( fix = curr->e.seginfo->FixupListHeadCoff; fix ; fix = fix->next2) {
//                printf("segment %s, reloc=%X\n", curr->sym.name, fix);
                ish.NumberOfRelocations++;
            }
            offset = (offset + 1) & ~1;
            ish.PointerToRelocations = offset;
            size_relocs += ish.NumberOfRelocations * sizeof(IMAGE_RELOCATION);
            offset += ish.NumberOfRelocations * sizeof(IMAGE_RELOCATION);
//            printf("segment %s has %u relocs\n", curr->sym.name, ish.NumberOfRelocations);
        }

        /* set linenumber pos+count in section header */

        if (curr->e.seginfo->LinnumQueue) {
            ish.PointerToLinenumbers = offset;
            ish.NumberOfLinenumbers = GetQueueItems(curr->e.seginfo->LinnumQueue);
            offset += ish.NumberOfLinenumbers * sizeof(IMAGE_LINENUMBER);
        } else {
            ish.PointerToLinenumbers = NULL;
            ish.NumberOfLinenumbers = 0;
        }

        DebugMsg(("coff_write_section_table: %s\n", curr->sym.name));
        write(fh, &ish, sizeof(ish));
    }
#if SETDATAPOS
    data_pos = tell(fh);
#endif
    DebugMsg(("coff_write_section_table: exit\n"));
    return(NOT_ERROR);
}


static short CoffGetType(asm_sym * sym)
{
    if (sym->state == SYM_PROC)
        return( 0x20 );

    switch (sym->mem_type) {
#if 0
    case MT_BYTE:
    case MT_SBYTE:
        return( IMAGE_SYM_TYPE_BYTE );
    case MT_WORD:
    case MT_SWORD:
        return( IMAGE_SYM_TYPE_WORD );
    case MT_DWORD:
    case MT_SDWORD:
        return( IMAGE_SYM_TYPE_DWORD );
#endif
    default:
        return( IMAGE_SYM_TYPE_NULL );
    }
}

static short CoffGetClass(asm_sym * sym)
{
    if (sym->state == SYM_EXTERNAL || sym->public == TRUE)
        return( IMAGE_SYM_CLASS_EXTERNAL );
    else if (sym->state == SYM_PROC && ((dir_node *)sym)->e.procinfo->defined == FALSE)
        return( IMAGE_SYM_CLASS_EXTERNAL );
    return( IMAGE_SYM_CLASS_STATIC );
}

// update the file header once all has been written

static void update_header(int fh)
{
    if (ifh.NumberOfSymbols)
        ifh.PointerToSymbolTable =
            sizeof(IMAGE_FILE_HEADER) + total_segs * sizeof(IMAGE_SECTION_HEADER) + coff_raw_data;
    lseek(fh, 0, SEEK_SET);
    write(fh, &ifh, sizeof(ifh));
}

// write COFF symbol table and string table
// contents of the table
// 0+1: file (+aux)
// 2-l: 2 entries per section
// l-m: externals, communals and publics
// m-n: entries for relocations (internal)
// n-o: aliases (weak externals)

int coff_write_symbols(int fh )
/**********************************************/
{
    dir_node    *curr;
    void        *vp;
    char        *p;
    uint        len;
    uint        i;
    stringitem  *pName;
    IMAGE_SYMBOL is;
    IMAGE_AUX_SYMBOL ias;
    char        buffer[MAX_LINE_LEN];

    DebugMsg(("coff_write_symbols: enter\n"));
    ifh.NumberOfSymbols = 0;

    // first entry is the .file (optional)

    if (Options.no_file_entry == FALSE) {
        strncpy(is.N.ShortName, ".file", IMAGE_SIZEOF_SHORT_NAME);
        is.Value = 0;
        is.SectionNumber = IMAGE_SYM_DEBUG;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_FILE;

        p = srcname;
        i = strlen(p);
        is.NumberOfAuxSymbols = i / sizeof(IMAGE_AUX_SYMBOL);
        if (i % sizeof(IMAGE_AUX_SYMBOL))
            is.NumberOfAuxSymbols++;
        write(fh, &is, sizeof(is));

        for (i=is.NumberOfAuxSymbols;i;i--, p += sizeof(IMAGE_AUX_SYMBOL)) {
            strncpy(ias.File.Name, p, sizeof(IMAGE_AUX_SYMBOL));
            write(fh, &ias, sizeof(ias));
        }
        ifh.NumberOfSymbols = is.NumberOfAuxSymbols + 1;
    }

    // second are section entries

    for( i = 1, curr = Tables[TAB_SEG].head; curr; curr = curr->next, i++) {
        p = CoffConvertSectionName(&curr->sym);
        len = strlen(p);
        if (len <= 8)
            strncpy(is.N.ShortName, p, 8);
        else {
            is.N.Name.Short = NULL;
            is.N.Name.Long = Coff_AllocString(p, len);
        }
        is.Value = 0;
        is.SectionNumber = i;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_STATIC;
        is.NumberOfAuxSymbols = 0;
        if (Options.no_section_aux_entry == FALSE)
            is.NumberOfAuxSymbols = 1;
        write(fh, &is, sizeof(is));
        ifh.NumberOfSymbols++;

        if (Options.no_section_aux_entry == FALSE) {
            ias.Section.Length = curr->e.seginfo->segrec->d.segdef.seg_length;
            ias.Section.NumberOfRelocations = curr->e.seginfo->num_relocs;
            ias.Section.NumberOfLinenumbers = 0;
            ias.Section.CheckSum = 0;
            ias.Section.Number = 0;
            ias.Section.Selection = 0;
            write(fh, &ias, sizeof(ias));
            ifh.NumberOfSymbols++;
        }
    }

    // third are externals + communals

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if (curr->e.extinfo->comm == FALSE && curr->e.extinfo->weak == TRUE)
            continue;
        DebugMsg(("coff_write_symbols: %s\n", curr->sym.name));
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        /* for COMMUNALs, store their size in the Value field */
        if (curr->e.extinfo->comm == TRUE)
            is.Value = curr->sym.total_size;
        else
            is.Value = curr->sym.offset; /* is always 0 */
        is.SectionNumber = 0;
        is.NumberOfAuxSymbols = 0;

        if (len <= 8)
            strncpy(is.N.ShortName, buffer, 8);
        else {
            is.N.Name.Short = NULL;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }
        write(fh, &is, sizeof(is));
        ifh.NumberOfSymbols++;
    }

    // PROTOs which have been "used" and have no matching PROC are also
    // externals.

    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->e.procinfo->defined == TRUE )
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        is.Value = curr->sym.offset;
        is.SectionNumber = 0;
        is.NumberOfAuxSymbols = 0;

        if (len <= 8)
            strncpy(is.N.ShortName, buffer, 8);
        else {
            is.N.Name.Short = NULL;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }
        write(fh, &is, sizeof(is));
        ifh.NumberOfSymbols++;
    }

    // publics and internal symbols. The internal symbols have
    // been written to the "public" queue inside coff_write_data().

    vp = NULL;
    while ( curr = GetPublicData2(&vp) ) {
        if( curr->sym.state == SYM_UNDEFINED ) {
            continue;
        } else if( curr->sym.state == SYM_PROC ) {
            /* skip PROTOs without matching PROC */
            if(curr->e.procinfo->defined == FALSE) {
                continue;
            }
        } else if (curr->sym.state == SYM_EXTERNAL) {
            /* skip EXTERNDEFs which aren't used */
            if (curr->e.extinfo->weak == TRUE)
                continue;
        }
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        is.Value = curr->sym.offset;
        if (curr->sym.state == SYM_EXTERNAL)
            is.SectionNumber = 0;
        else if (curr->sym.mem_type == MT_ABS)
            is.SectionNumber = IMAGE_SYM_ABSOLUTE;
        else if (curr->sym.segment)
            is.SectionNumber = GetSegIdx(curr->sym.segment);
        else
            is.SectionNumber = 0;

        is.NumberOfAuxSymbols = 0;

        if (len <= 8)
            strncpy(is.N.ShortName, buffer, 8);
        else {
            is.N.Name.Short = NULL;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }

        DebugMsg(("coff_write_symbols: symbol %s, ofs=%X\n", buffer, is.Value));

        write(fh, &is, sizeof(is));
        ifh.NumberOfSymbols++;
    }

    // aliases. A weak external entry with 1 aux entry is created for
    // each alias.

    for( curr = Tables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        asm_sym * sym;

        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        if (len <= 8)
            strncpy(is.N.ShortName, buffer, 8);
        else {
            is.N.Name.Short = NULL;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }

        is.Value = 0;
        is.SectionNumber = IMAGE_SYM_UNDEFINED;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_WEAK_EXTERNAL;
        is.NumberOfAuxSymbols = 1;

        DebugMsg(("coff_write_symbols: symbol %s, ofs=%X\n", buffer, is.Value));

        write(fh, &is, sizeof(is));
        ifh.NumberOfSymbols++;

        memset(&ias, 0, sizeof(ias));

        sym = SymSearch(curr->sym.string_ptr);
        if (sym)
            ias.Sym.TagIndex = sym->idx;

        ias.Sym.Misc.TotalSize = IMAGE_WEAK_EXTERN_SEARCH_ALIAS;
        write(fh, &ias, sizeof(ias));
        ifh.NumberOfSymbols++;

    }

    /* the string table is ALWAYS written, even if no strings are defined */

    write(fh, &SizeLongNames, sizeof(SizeLongNames));
    for (pName = LongNamesHead; pName; pName = pName->next) {
        write(fh, pName->string, strlen(pName->string)+1);
    }

    /* update the COFF file header */

    update_header(fh);

    DebugMsg(("coff_write_symbols: exit\n"));
    return(NOT_ERROR);
}

// write COFF file header
// total_segs has been set by the caller
// however, it might be necessary to add a .drectve section

int coff_write_header( int fh )
{
    dir_node *dir;
    char buffer[MAX_LINE_LEN];

    DebugMsg(("coff_write_header: enter\n"));

    LongNamesHead = NULL;
    SizeLongNames = sizeof(uint_32);

    srcname = AsmFiles.fname[ASM];
    srcname += strlen(srcname);
    while (srcname > AsmFiles.fname[ASM] &&
           *(srcname-1) != '/' &&
           *(srcname-1) != '\\') srcname--;

    directives = NULL;

    for( dir = Tables[TAB_PROC].head; dir != NULL; dir = dir->next ) {
        if( dir->e.procinfo->export )
            break;
    }
    /* if a start_label is defined, a library is included or a
     proc is exported, add a .drectve section
     */
    if (start_label != NULL ||
        Tables[TAB_LIB].head != NULL ||
        dir != NULL) {
        directives = dir_insert(".drectve", TAB_SEG);
        if (directives) {
            int size = 0;
            char * p;
            directives->e.seginfo->segrec->d.segdef.idx = ++segdefidx;
            directives->sym.segment = &directives->sym;
            directives->e.seginfo->segtype = SEGTYPE_DATA;
            total_segs++;

            /* calc the size for this segment */
            /* 1. exports */
            for( ; dir ; dir = dir->next ) {
                if( dir->e.procinfo->export ) {
                    Mangle(&dir->sym, buffer);
                    size += strlen(buffer) + sizeof(" -export:");
                }
            }
            for( dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
                size += strlen(dir->sym.name) + sizeof(" -defaultlib:") + 2;
            }
            if ( start_label ) {
                if (Options.entry_decorated)
                    Mangle(start_label, buffer);
                else
                    strcpy(buffer, start_label->name);
                size += strlen(buffer) + sizeof(" -entry:");
            }
            size++;
            directives->e.seginfo->segrec->d.segdef.seg_length = size;
            directives->e.seginfo->CodeBuffer = AsmAlloc(size);

            p = directives->e.seginfo->CodeBuffer;

            /* copy the data */
            size = 0;

            /* 1. exports */
            for( dir = Tables[TAB_PROC].head; dir ; dir = dir->next ) {
                if( dir->e.procinfo->export ) {
                    Mangle(&dir->sym, buffer);
                    size = sprintf(p, "-export:%s ", buffer);
                    p += size;
                }
            }
            for( dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
                size = sprintf(p,"-defaultlib:\"%s\" ", dir->sym.name);
                p += size;
            }
            if ( start_label) {
                if (Options.entry_decorated)
                    Mangle(start_label, buffer);
                else
                    strcpy(buffer, start_label->name);
                size = sprintf(p, "-entry:%s ", buffer);
                p += size;
            }
            size_drectve = p - directives->e.seginfo->CodeBuffer;
        }
    }

    if (directives)
        directives->e.seginfo->segrec->d.segdef.seg_length = size_drectve;

    ifh.Machine = IMAGE_FILE_MACHINE_I386;
    ifh.NumberOfSections = total_segs;
    time(&ifh.TimeDateStamp);
    ifh.PointerToSymbolTable = NULL;
    ifh.NumberOfSymbols = 0;
    ifh.SizeOfOptionalHeader = 0;
    ifh.Characteristics = 0;

    lseek(fh, 0, SEEK_SET);
    write(fh, &ifh, sizeof(ifh));

    DebugMsg(("coff_write_header: exit\n"));
    return(NOT_ERROR);
}

// calc the current number of entries in the symbol table
// so we know the index if a new entry has to be added

static uint_32 CoffGetSymIndex(void)
{
    void * vp;
    dir_node * curr;
    uint_32 index;
    uint_32 i;

    index = total_segs;
    if (Options.no_section_aux_entry == FALSE)
        index += index;

    /* count AUX entries for .file. Depends on sizeof filename */

    if (Options.no_file_entry == FALSE) {
        index++;
        i = strlen(srcname);
        index += i / sizeof(IMAGE_AUX_SYMBOL);
        if (i % sizeof(IMAGE_AUX_SYMBOL))
            index++;
    }

    /* count externals and protos */
    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if (curr->e.extinfo->comm == 0 && curr->e.extinfo->weak == 1)
            continue;
        curr->sym.idx = index++;
    }
    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->e.procinfo->defined == TRUE )
            continue;
        curr->sym.idx = index++;
    }

    /* count publics */
    vp = NULL;
    while( curr = GetPublicData2(&vp)) {
        if( curr->sym.state == SYM_UNDEFINED ) {
            continue;
        } else if( curr->sym.state == SYM_PROC && curr->e.procinfo->defined == FALSE) {
            continue;
        } else if (curr->sym.state == SYM_EXTERNAL && curr->e.extinfo->weak == TRUE) {
            continue;
        }
        curr->sym.idx = index++;
    }
    return(index);
}

// write section contents and fixups
// this is done after the last step only!

int coff_write_data(int fh)
{
    dir_node *dir;
    struct asmfixup *fix;
    uint_32 offset = 0;
    IMAGE_RELOCATION ir;
    IMAGE_LINENUMBER il;
    uint_32 index;

    DebugMsg(("coff_write_data: enter\n"));

    if (directives)
        directives->e.seginfo->segrec->d.segdef.seg_length = size_drectve;

    /* calc the current index for the COFF symbol table */
    index = CoffGetSymIndex();

#if SETDATAPOS
    lseek(fh, data_pos, SEEK_SET);
#endif
    for( dir = Tables[TAB_SEG].head; dir; dir = dir->next ) {
        int size;
        if( ( dir->sym.state != SYM_SEG ) || ( dir->sym.segment == NULL ) )
            continue;
        if (dir->e.seginfo->segrec->d.segdef.combine == COMB_STACK)
            continue;
        if (dir->e.seginfo->segtype == SEGTYPE_BSS)
            continue;

        size = dir->e.seginfo->segrec->d.segdef.seg_length;
        if (size) {
            offset += size;
            if ((offset & 1) && dir->e.seginfo->FixupListHeadCoff) {
                offset++;
                size++;
            }
            DebugMsg(("coff_write_data(%s): size=%X, content=[%02X %02X]\n", dir->sym.name, size, *(dir->e.seginfo->CodeBuffer), *(dir->e.seginfo->CodeBuffer+1)));
            write(fh, dir->e.seginfo->CodeBuffer, size);
            for (fix = dir->e.seginfo->FixupListHeadCoff; fix ; fix = fix->next2) {
                switch (fix->type) {
                case FIX_LOBYTE:
                case FIX_HIBYTE:
                case FIX_RELOFF8:
                    /* not supported by COFF! */
                    ir.Type = IMAGE_REL_I386_ABSOLUTE; /* ??? */
                    break;
                case FIX_RELOFF16: /* 16bit offset */
                    ir.Type = IMAGE_REL_I386_REL16;
                    break;
                case FIX_OFF16: /* 16bit offset */
                    ir.Type = IMAGE_REL_I386_DIR16;
                    break;
                case FIX_RELOFF32: /* 32bit offset */
                    ir.Type = IMAGE_REL_I386_REL32;
                    break;
                case FIX_OFF32: /* 32bit offset */
                    ir.Type = IMAGE_REL_I386_DIR32;
                    break;
                case FIX_SEG: /* segment fixup */
                    ir.Type = IMAGE_REL_I386_SECTION; /* ??? */
                    break;
                case FIX_PTR16: /* 16bit far pointer */
                    ir.Type = IMAGE_REL_I386_ABSOLUTE; /* ??? */
                    break;
                case FIX_PTR32: /* 32bit far pointer */
                    ir.Type = IMAGE_REL_I386_ABSOLUTE; /* ??? */
                    break;
                default:
                    break;
                }
                /* if it's not EXTERNAL/PUBLIC, add symbol */
                if ((fix->sym->state == SYM_INTERNAL ||
                     (fix->sym->state == SYM_PROC && ((dir_node *)fix->sym)->e.procinfo->defined == TRUE)) &&
                    fix->sym->included == FALSE &&
                    fix->sym->public == FALSE) {
                    fix->sym->included = TRUE;
                    AddPublicData((dir_node *)(fix->sym));
                    fix->sym->idx = index++;
                }
                ir.VirtualAddress = fix->fixup_loc;
                ir.SymbolTableIndex = fix->sym->idx;
                write(fh, &ir, sizeof(ir));
                DebugMsg(("coff_write_data(%s): reloc loc=%X type=%u idx=%u sym=%s\n", dir->sym.name, ir.VirtualAddress, ir.Type, ir.SymbolTableIndex, fix->sym->name));
                offset += sizeof(ir);
                dir->e.seginfo->num_relocs++;
            }
            /* write line number data */
            if( Options.line_numbers ) {
                void *vp = dir->e.seginfo->LinnumQueue;
                line_num_info *lni;
                while ( lni = GetLinnumData2(&vp) ) {
                    DebugMsg(("coff_write_data(%s): linnum, &data=%X\n", dir->sym.name, &lni));
                    il.Linenumber = lni->number;
                    if (lni->number == 0) {
                        AddPublicData((dir_node *)(lni->sym));
                        il.Type.SymbolTableIndex = index++;
                    } else
                        il.Type.VirtualAddress = lni->offset;
                    write(fh, &il, sizeof(il));
                    offset += sizeof(il);
                }
            }
        }
    }

    coff_raw_data = offset;
    DebugMsg(("coff_write_data: exit\n"));

    return(NOT_ERROR);
}

