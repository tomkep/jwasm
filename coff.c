/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  COFF output routines
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "myunistd.h"
#include "symbols.h"
#include "mangle.h"
#include "memalloc.h"
#include "fixup.h"
#include "directiv.h"
#include "segment.h"
#include "queues.h"
#include "coff.h"
#include "fatal.h"

#if COFF_SUPPORT

#define SETDATAPOS   1
#define HELPSYMS     0 /* use helper symbols for assembly time symbol refs */

typedef struct stringitem {
    struct stringitem *next;
    char string[];
} stringitem;

extern dir_node * GetPublicData( void ** );
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
static uint_32 sectionstart; /* symbol table index start sections */
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
    static char coffname[MAX_ID_LEN+1];

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

ret_code coff_write_section_table( int fh )
{
    dir_node    *curr;
    //obj_rec     *objr;
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

        ish.Characteristics = 0;

        if ( curr->e.seginfo->alignment == MAX_SEGALIGNMENT ) // ABS not possible
            ish.Characteristics |= IMAGE_SCN_ALIGN_1BYTES;
        else
            ish.Characteristics |= (uint_32)(curr->e.seginfo->alignment + 1) << 20;

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

        /* manual characteristics set? */
        if ( curr->e.seginfo->characteristics ) {
            ish.Characteristics &= 0x1FFFFFF; /* clear the IMAGE_SCN_MEM flags */
            ish.Characteristics |= (uint_32)(curr->e.seginfo->characteristics & 0xFE) << 24;
            /* the INFO bit (bit 0) needs special handling! */
            if ( curr->e.seginfo->characteristics & 1 )
                ish.Characteristics |= IMAGE_SCN_LNK_INFO;
        }

        if (ish.PointerToRawData)
            offset += ish.SizeOfRawData;

        /* set relocation pos+count in section header */

        ish.PointerToRelocations = 0;
        ish.NumberOfRelocations = 0;
        if ( curr->e.seginfo->FixupListHeadGen ) {
            for ( fix = curr->e.seginfo->FixupListHeadGen; fix ; fix = fix->next2) {
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
            ish.PointerToLinenumbers = 0;
            ish.NumberOfLinenumbers = 0;
        }

        DebugMsg(( "coff_write_section_table: %s, Fixups=%u, Linnums=%u\n", curr->sym.name, ish.NumberOfRelocations, ish.NumberOfLinenumbers ));
        if ( _write(fh, &ish, sizeof(ish)) != sizeof(ish) )
            WriteError();
    }
#if SETDATAPOS
    data_pos = _tell(fh);
#endif
    DebugMsg(("coff_write_section_table: exit\n"));
    return( NOT_ERROR );
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
    else if ( sym->state == SYM_PROC && sym->isproc == FALSE )
        return( IMAGE_SYM_CLASS_EXTERNAL );
#if HELPSYMS
    else if (sym->variable == TRUE) /* assembly time variable in fixup */
        return( IMAGE_SYM_CLASS_LABEL );
#endif
    return( IMAGE_SYM_CLASS_STATIC );
}

// update the file header once all has been written

static void update_header(int fh)
{
    if (ifh.NumberOfSymbols)
        ifh.PointerToSymbolTable =
            sizeof(IMAGE_FILE_HEADER) + total_segs * sizeof(IMAGE_SECTION_HEADER) + coff_raw_data;
    _lseek(fh, 0, SEEK_SET);
    if ( _write(fh, &ifh, sizeof(ifh)) != sizeof(ifh) )
        WriteError();
}

// write COFF symbol table and string table
// contents of the table
// 0+1: file (+aux)
// 2-l: 2 entries per section
// l-m: externals, communals and publics
// m-n: entries for relocations (internal)
// n-o: aliases (weak externals)

ret_code coff_write_symbols(int fh )
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
    char        buffer[MAX_ID_LEN+1];

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
        if ( _write(fh, &is, sizeof(is)) != sizeof(is) )
            WriteError();

        for (i=is.NumberOfAuxSymbols;i;i--, p += sizeof(IMAGE_AUX_SYMBOL)) {
            strncpy(ias.File.Name, p, sizeof(IMAGE_AUX_SYMBOL));
            if ( _write(fh, &ias, sizeof(ias)) != sizeof(ias) )
                WriteError();
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
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(p, len);
        }
        is.Value = 0;
        is.SectionNumber = i;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_STATIC;
        is.NumberOfAuxSymbols = 0;
        if (Options.no_section_aux_entry == FALSE)
            is.NumberOfAuxSymbols = 1;

        DebugMsg(("coff_write_symbols(SECT): %s, type=%x, stgcls=%x\n", curr->sym.name, is.Type, is.StorageClass ));

        if ( _write(fh, &is, sizeof(is)) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;

        if (Options.no_section_aux_entry == FALSE) {
            ias.Section.Length = curr->e.seginfo->segrec->d.segdef.seg_length;
            ias.Section.NumberOfRelocations = curr->e.seginfo->num_relocs;
            ias.Section.NumberOfLinenumbers = 0;
            ias.Section.CheckSum = 0;
            ias.Section.Number = 0;
            ias.Section.Selection = 0;
            if ( _write(fh, &ias, sizeof(ias)) != sizeof(ias) )
                WriteError();
            ifh.NumberOfSymbols++;
        }
    }

    // third are externals + communals

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if (curr->sym.comm == FALSE && curr->sym.weak == TRUE)
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        DebugMsg(("coff_write_symbols(EXT+COMM): %s, type=%x, stgcls=%x\n", curr->sym.name, is.Type, is.StorageClass ));

        /* for COMMUNALs, store their size in the Value field */
        if (curr->sym.comm == TRUE)
            is.Value = curr->sym.total_size;
        else
            is.Value = curr->sym.offset; /* is always 0 */
        is.SectionNumber = 0;
        is.NumberOfAuxSymbols = 0;

        if (len <= 8)
            strncpy(is.N.ShortName, buffer, 8);
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }
        if ( _write(fh, &is, sizeof(is)) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;
    }

    // PROTOs which have been "used" and have no matching PROC are also
    // externals.

    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->sym.isproc == TRUE )
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        is.Value = curr->sym.offset;
        is.SectionNumber = 0;
        is.NumberOfAuxSymbols = 0;

        DebugMsg(("coff_write_symbols(PROTO): %s, type=%x, stgcls=%x\n", curr->sym.name, is.Type, is.StorageClass ));

        if (len <= 8)
            strncpy(is.N.ShortName, buffer, 8);
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }
        if ( _write(fh, &is, sizeof(is)) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;
    }

    // publics and internal symbols. The internal symbols have
    // been written to the "public" queue inside coff_write_data().

    vp = NULL;
    while ( curr = GetPublicData(&vp) ) {
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
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }

        DebugMsg(("coff_write_symbols(PUB+INT): symbol %s, ofs=%X, type=%X, stgcls=%X\n", buffer, is.Value, is.Type, is.StorageClass ));

        if ( _write(fh, &is, sizeof(is)) != sizeof(is) )
            WriteError();
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
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }

        is.Value = 0;
        is.SectionNumber = IMAGE_SYM_UNDEFINED;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_WEAK_EXTERNAL;
        is.NumberOfAuxSymbols = 1;

        DebugMsg(("coff_write_symbols(ALIAS): symbol %s, ofs=%X\n", buffer, is.Value));

        if ( _write(fh, &is, sizeof(is)) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;

        memset(&ias, 0, sizeof(ias));

        sym = SymSearch(curr->sym.string_ptr);
        if (sym)
            ias.Sym.TagIndex = sym->idx;

        ias.Sym.Misc.TotalSize = IMAGE_WEAK_EXTERN_SEARCH_ALIAS;
        if ( _write(fh, &ias, sizeof(ias)) != sizeof(ias) )
            WriteError();
        ifh.NumberOfSymbols++;

    }

    /* the string table is ALWAYS written, even if no strings are defined */

    if ( _write(fh, &SizeLongNames, sizeof(SizeLongNames)) != sizeof(SizeLongNames) )
        WriteError();
    for (pName = LongNamesHead; pName; pName = pName->next) {
        i = strlen(pName->string)+1;
        if ( _write(fh, pName->string, i ) != i )
            WriteError();
    }

    /* update the COFF file header */

    update_header(fh);

    DebugMsg(("coff_write_symbols: exit\n"));
    return(NOT_ERROR);
}

static int GetStartLabel(char * buffer, bool msg)
{
    int size = 0;
    char temp[ MAX_ID_LEN+1 ];

    if ( start_label ) {
        Mangle( start_label, temp );
        if ( Options.entry_decorated )
            strcpy( buffer, temp );
        else {
            if (start_label->langtype != LANG_C &&
                start_label->langtype != LANG_STDCALL &&
                start_label->langtype != LANG_SYSCALL ) {
                if (*start_label->name != '_') {
                    if (msg)
                        AsmWarn( 2, LEADING_UNDERSCORE_REQUIRED_FOR_START_LABEL, start_label->name );
                    strcpy( buffer, temp );
                } else {
                    strcpy( buffer, temp+1 );
                }
            } else
                strcpy(buffer, temp+1 );
        }
        size = strlen(buffer) + sizeof(" -entry:");
    }
    return( size );
}

// write COFF file header
// total_segs has been set by the caller
// however, it might be necessary to add a .drectve section

ret_code coff_write_header( int fh )
{
    dir_node *dir;
    char buffer[MAX_ID_LEN + 1];

    DebugMsg(("coff_write_header: enter\n"));

    LongNamesHead = NULL;
    SizeLongNames = sizeof(uint_32);

    srcname = FileInfo.fname[ASM];
    srcname += strlen(srcname);
    while (srcname > FileInfo.fname[ASM] &&
           *(srcname-1) != '/' &&
           *(srcname-1) != '\\') srcname--;

    directives = NULL;

    /* does a proc exist with the EXPORT attribute? */
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
        directives = dir_insert(".drectve", SYM_SEG);
        if (directives) {
            int size = 0;
            uint_8 *p;
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
                    if ( Options.no_stdcall_export_decoration == TRUE )
                        size += dir->sym.name_size + 1;
                }
            }
            for( dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
                size += strlen(dir->sym.name) + sizeof(" -defaultlib:");
                /* if the name isn't enclosed in double quotes and contains
                 a space, add 2 bytes to enclose it */
                if (*(dir->sym.name) != '"' && strchr(dir->sym.name, ' '))
                    size += 2;
            }
            size += GetStartLabel(buffer, TRUE);
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
                    if ( Options.no_stdcall_export_decoration == FALSE )
                        size = sprintf(p, "-export:%s ", buffer);
                    else
                        size = sprintf(p, "-export:%s=%s ", dir->sym.name, buffer);
                    p += size;
                }
            }
            for( dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
                if (*dir->sym.name != '"' && strchr(dir->sym.name, ' '))
                    size = sprintf(p,"-defaultlib:\"%s\" ", dir->sym.name);
                else
                    size = sprintf(p,"-defaultlib:%s ", dir->sym.name);
                p += size;
            }
            if ( start_label) {
                GetStartLabel(buffer, FALSE);
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
#ifdef __UNIX__
    time((long *)&ifh.TimeDateStamp);
#else
    time((time_t *)&ifh.TimeDateStamp);
#endif
    ifh.PointerToSymbolTable = 0;
    ifh.NumberOfSymbols = 0;
    ifh.SizeOfOptionalHeader = 0;
    ifh.Characteristics = 0;

    _lseek(fh, 0, SEEK_SET);
    if ( _write(fh, &ifh, sizeof(ifh)) != sizeof(ifh) )
        WriteError();

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

    /* count AUX entries for .file. Depends on sizeof filename */

    sectionstart = 0;
    index = 0;
    if (Options.no_file_entry == FALSE) {
        i = strlen(srcname);
        sectionstart = i / sizeof(IMAGE_AUX_SYMBOL) + 1;
        if (i % sizeof(IMAGE_AUX_SYMBOL))
            sectionstart++;
        index = sectionstart;
    }

    /* add entries for sections */

    index += total_segs;
    if (Options.no_section_aux_entry == FALSE)
        index += total_segs;

    /* count externals and protos */
    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if (curr->sym.comm == 0 && curr->sym.weak == 1)
            continue;
        curr->sym.idx = index++;
    }
    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->sym.isproc == TRUE )
            continue;
        curr->sym.idx = index++;
    }

    /* count publics */
    vp = NULL;
    while( curr = GetPublicData(&vp)) {
        if( curr->sym.state == SYM_UNDEFINED ) {
            continue;
        } else if( curr->sym.state == SYM_PROC && curr->sym.isproc == FALSE) {
            continue;
        } else if (curr->sym.state == SYM_EXTERNAL && curr->sym.weak == TRUE) {
            continue;
        }
        curr->sym.idx = index++;
    }
    return(index);
}

// write section contents and fixups
// this is done after the last step only!

ret_code coff_write_data(int fh)
{
    dir_node *section;
    struct asmfixup *fix;
    uint_32 offset = 0;
    IMAGE_RELOCATION ir;
    IMAGE_LINENUMBER il;
    int i;
    uint_32 index;

    DebugMsg(("coff_write_data: enter\n"));

    if (directives)
        directives->e.seginfo->segrec->d.segdef.seg_length = size_drectve;

    /* calc the current index for the COFF symbol table */
    index = CoffGetSymIndex();

#if SETDATAPOS
    _lseek(fh, data_pos, SEEK_SET);
#endif

#if HELPSYMS==0
    for( i = 0, section = Tables[TAB_SEG].head; section ; i++, section = section->next ) {
        if( section->sym.state != SYM_SEG )
            continue;
        section->sym.idx = sectionstart + i;
        if (Options.no_section_aux_entry == FALSE)
            section->sym.idx += i;
    }
#endif

    /* now scan all section's relocations. If a relocation refers to
     a symbol which is not public/external, it must be added to the
     symbol table. If the symbol is an assembly time variable, a helper
     symbol - name is $$<offset:6> is to be added.
     */

    for( section = Tables[TAB_SEG].head; section ; section = section->next ) {
        int size;
        if( section->sym.state != SYM_SEG )
            continue;
        if ( section->e.seginfo->segrec->d.segdef.combine == COMB_STACK)
            continue;
        if ( section->e.seginfo->segtype == SEGTYPE_BSS)
            continue;

        size = section->e.seginfo->segrec->d.segdef.seg_length;
        if (size) {
            offset += size;
            if ((offset & 1) && section->e.seginfo->FixupListHeadGen ) {
                offset++;
                size++;
            }
            DebugMsg(("coff_write_data(%s): size=%X, content=[%02X %02X]\n", section->sym.name, size, *(section->e.seginfo->CodeBuffer), *(section->e.seginfo->CodeBuffer+1)));
            if ( _write(fh, section->e.seginfo->CodeBuffer, size) != size )
                WriteError();
            for (fix = section->e.seginfo->FixupListHeadGen; fix ; fix = fix->next2) {
                switch (fix->type) {
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
#if IMAGERELSUPP
                case FIX_OFF32_IMGREL:
                    ir.Type = IMAGE_REL_I386_DIR32NB;
                    break;
#endif
#if SECRELSUPP
                case FIX_OFF32_SECREL:
                    ir.Type = IMAGE_REL_I386_SECREL;
                    break;
#endif
                case FIX_SEG: /* segment fixup */
                    ir.Type = IMAGE_REL_I386_SECTION; /* ??? */
                    break;
                case FIX_LOBYTE:
                case FIX_HIBYTE:
                case FIX_RELOFF8:
                case FIX_PTR16: /* 16bit far pointer */
                case FIX_PTR32: /* 32bit far pointer */
                    /* not supported by COFF! shouldn't reach this point */
                    // ir.Type = IMAGE_REL_I386_ABSOLUTE; /* ??? */
                    // break;
                default:
                    AsmErr( UNKNOWN_FIXUP_TYPE, fix->type );
                    break;
                }
                /* if it's not EXTERNAL/PUBLIC, add symbol */
                /* if it's an assembly time variable, create helper symbol */
                if ( fix->sym->variable == TRUE ) {
#if HELPSYMS
                    asm_sym *sym;
                    char buffer[12];
                    sprintf( buffer, "$$%06X", fix->offset );
                    sym = SymCreate( buffer, FALSE );
                    sym->state = fix->sym->state;
                    sym->mem_type = fix->sym->mem_type;
                    sym->offset = fix->offset;
                    sym->segment = fix->segment;
                    sym->variable = TRUE; /* storage class LABEL */
                    fix->sym = sym;
                    AddPublicData( fix->sym );
                    fix->sym->idx = index++;
#else
                    /* just use the segment entry. This approach requires
                     that the offset is stored inline at the reloc location
                     (patch in fixup.c)
                     */
                    fix->sym = fix->segment;
#endif
                } else if (( fix->sym->state == SYM_INTERNAL ||
                     ( fix->sym->state == SYM_PROC && fix->sym->isproc == TRUE ) ) &&
                    fix->sym->included == FALSE &&
                    fix->sym->public == FALSE) {
                    fix->sym->included = TRUE;
                    AddPublicData( fix->sym );
                    fix->sym->idx = index++;
                }
                ir.VirtualAddress = fix->fixup_loc;
                ir.SymbolTableIndex = fix->sym->idx;
                if ( _write(fh, &ir, sizeof(ir)) != sizeof(ir) )
                    WriteError();
                DebugMsg(("coff_write_data(%s): reloc loc=%X type=%u idx=%u sym=%s\n", section->sym.name, ir.VirtualAddress, ir.Type, ir.SymbolTableIndex, fix->sym->name));
                offset += sizeof(ir);
                section->e.seginfo->num_relocs++;
            }
            /* write line number data */
            if( Options.line_numbers ) {
                line_num_info *lni;
                while ( lni = GetLinnumData2(section->e.seginfo->LinnumQueue) ) {
                    DebugMsg(("coff_write_data(%s): linnum, &data=%X\n", section->sym.name, &lni));
                    il.Linenumber = lni->number;
                    if (lni->number == 0) {
                        AddPublicData( lni->sym );
                        il.Type.SymbolTableIndex = index++;
                    } else
                        il.Type.VirtualAddress = lni->offset;
                    if ( _write(fh, &il, sizeof(il)) != sizeof(il) )
                        WriteError();
                    offset += sizeof(il);
                }
            }
        }
    }

    coff_raw_data = offset;
    DebugMsg(("coff_write_data: exit\n"));

    return(NOT_ERROR);
}

#endif

