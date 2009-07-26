/****************************************************************************
*
*  This code is Public Domain. It's new for JWasm.
*
*  ========================================================================
*
* Description:  ELF output routines
*
****************************************************************************/

#include <ctype.h>
#include <time.h>

#include "globals.h"
#include "myunistd.h"
#include "symbols.h"
#include "mangle.h"
#include "memalloc.h"
#include "directiv.h"
#include "fixup.h"
#include "segment.h"
#include "queues.h"
#include "elf.h"
#include "elfspec.h"
#include "fatal.h"

#if ELF_SUPPORT

// start label is always public for COFF/ELF, no need to add it
#define ADDSTARTLABEL 0

// there's no STT_IMPORT type for ELF, it's OW specific
#define OWELFIMPORT 0

extern dir_node * GetPublicData( void ** );
extern line_num_info * GetLinnumData2( void ** );

extern symbol_queue     Tables[];       // tables of definitions
extern unsigned  total_segs;
extern asm_sym          *start_label;   // symbol for Modend (COFF)
extern uint             segdefidx;      // Number of Segment definition

//static uint_32 size_drectve;   /* size of .drectve section */
static uint_32 symindex;       /* entries in symbol table */
static uint_32 start_globals;  /* start index globals in symbol table */
static dir_node *directives;
static Elf32_Ehdr ehdr;

static uint_32 SizeLongNames;
static char *srcname; /* name of source module (name + extension) */

#if SETDATAPOS
static uint_32 data_pos;
#endif

#ifdef __I86__
extern uint_32 _hwrite( int fh, uint_8 huge *pBuffer, uint_32 size );
#define _write _hwrite
#endif

typedef struct _intseg {
    char * name;
    uint type;
    uint size;
    uint offset;
    void *data;
} intseg;

static intseg internal_segs[] = {
    { ".shstrtab", SHT_STRTAB, 0, 0, NULL },
    { ".symtab", SHT_SYMTAB, 0, 0, NULL },
    { ".strtab", SHT_STRTAB, 0, 0, NULL },
    { NULL, SHT_NULL, 0, 0, NULL }
};

#define SHSTRTAB_IDX 0
#define SYMTAB_IDX   1
#define STRTAB_IDX   2

/* translate section names: */
// _TEXT -> .text
// _DATA -> .data
// _BSS  -> .bss
// CONST -> .rdata

static char * ElfConvertSectionName(asm_sym * sym)
/************************************************/
{
    static char name[MAX_ID_LEN+1];

    if (memcmp(sym->name, "_TEXT", 5) == 0) {
        if (sym->name[5] == NULLC)
            return(".text");
        else if (sym->name[5] == '$') {
            strcpy(name, ".text");
            strcpy(name+5, sym->name+5);
            return(name);
        }
    } else if (memcmp(sym->name, "_DATA", 5) == 0) {
        if (sym->name[5] == NULLC)
            return(".data");
        else if (sym->name[5] == '$') {
            strcpy(name, ".data");
            strcpy(name+5, sym->name+5);
            return(name);
        }
    } else if (memcmp(sym->name, "CONST", 5) == 0) {
        if (sym->name[5] == NULLC)
            return(".rdata");
        else if (sym->name[5] == '$') {
            strcpy(name, ".rdata");
            strcpy(name+6, sym->name+5);
            return(name);
        }
    } else if (strcmp(sym->name, "_BSS") == 0) {
        return(".bss");
    }
    return(sym->name);
}

static int get_num_reloc_sections( void )
/***************************************/
{
    dir_node    *curr;
    int num = 0;

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->FixupListHeadGen )
            num++;
    }
    return( num );
}

typedef struct localname {
    void * next;
    asm_sym *sym;
} localname;

/* calculate size of .symtab + .strtab section.
   set content of these sections
*/
static void set_symtab_values( void )
/*****************************/
{
    uint_32 entries;
    uint_32 strsize = 1;
    uint_32 len;
    dir_node *curr;
    dir_node *seg;
    char *p2;
    void *vp;
    Elf32_Sym *p;
    //uint_8 stb;
    uint_8 stt;
    localname *localshead = NULL;
    localname *localstail = NULL;
    localname *localscurr;
    char buffer[MAX_ID_LEN+1];

    /* symbol table. there is
     - 1 NULL entry,
     - 1 entry for the module/file,
     - 1 entry for each section and
     - n entries for local symbols
     - m entries for global symbols
     */

    /* symbol table starts with 1 NULL entry + 1 file entry */
    symindex = 1 + 1;

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next )
        curr->sym.idx = symindex++;

    /* add local symbols to symbol table */

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if (curr->e.seginfo->num_relocs) {
            struct asmfixup * fix = curr->e.seginfo->FixupListHeadGen;
            for ( ; fix; fix = fix->nextrlc ) {
                /* if it's not EXTERNAL/PUBLIC, add symbol. */
                /* however, if it's an assembly time variable */
                /* use a raw section reference */
                if (fix->sym->variable) {
                    fix->sym = fix->segment;
                } else if ((fix->sym->state == SYM_INTERNAL ||
                     ( fix->sym->state == SYM_PROC && fix->sym->isproc == TRUE )) &&
                    fix->sym->included == FALSE &&
                    fix->sym->public == FALSE) {
                    fix->sym->included = TRUE;
                    localscurr = AsmAlloc(sizeof(localname));
                    localscurr->next = NULL;
                    localscurr->sym = fix->sym;
                    if (localstail) {
                        localstail->next = localscurr;
                        localstail = localscurr;
                    } else {
                        localshead = localstail = localscurr;
                    }
                    fix->sym->idx = symindex++;
                }
            }
        }
    }
    start_globals = symindex;

    /* count EXTERNs and used EXTERNDEFs */
    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if (curr->sym.comm == 0 && curr->sym.weak == 1)
            continue;
        curr->sym.idx = symindex++;
    }
    DebugMsg(("ElfGetSymIndex: index after EXTERNALs: %u\n", symindex));

    /* count PROTOs which are used and external */
    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->sym.isproc == TRUE )
            continue;
        curr->sym.idx = symindex++;
    }
    DebugMsg(("ElfGetSymIndex: index after PROTOs: %u\n", symindex));

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
        curr->sym.idx = symindex++;
    }
    DebugMsg(("ElfGetSymIndex: index after PUBLICs: %u\n", symindex));

    /* size of symbol table is defined */

    entries = symindex;

#if ADDSTARTLABEL
    if ( start_label )
        entries++;
#endif

    internal_segs[SYMTAB_IDX].size = entries * sizeof(Elf32_Sym);
    internal_segs[SYMTAB_IDX].data = AsmAlloc(internal_segs[SYMTAB_IDX].size);
    memset(internal_segs[SYMTAB_IDX].data, 0, internal_segs[SYMTAB_IDX].size);
    p = (Elf32_Sym *)internal_segs[SYMTAB_IDX].data;

    p++; /* skip NULL entry */

    /* 1. make file entry */
    p->st_name = strsize;
    strsize += strlen(srcname) + 1;
    p->st_value = 0;
    p->st_size = 0;
    p->st_info = ELF32_ST_INFO(STB_LOCAL, STT_FILE); /* symbol's type and binding info */
    p->st_shndx = SHN_ABS; /* section index */
    p++;

    /* 2. make section entries */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        p->st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
        p->st_shndx = GetSegIdx( curr->sym.segment );
        p++;
    }

    /* 3. locals */

    for ( localscurr = localshead ; localscurr ; localscurr = localscurr->next ) {
        Mangle( localscurr->sym, buffer );
        len = strlen( buffer );
        p->st_name = strsize;
        seg = (dir_node *)localscurr->sym->segment;
        if (seg && seg->e.seginfo->segtype != SEGTYPE_CODE)
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;
        p->st_info = ELF32_ST_INFO(STB_LOCAL, stt);
        p->st_value = localscurr->sym->offset;
        if (localscurr->sym->mem_type == MT_ABS)
            p->st_shndx = SHN_ABS;
        else
            p->st_shndx = GetSegIdx(localscurr->sym->segment);
        strsize += len + 1;
        DebugMsg(("elf_write_symbols, LOCAL: symbol %s, ofs=%X\n", buffer, p->st_value));
        p++;
    }

    /* 4. externals + communals */

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if (curr->sym.comm == FALSE && curr->sym.weak == TRUE)
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        p->st_name = strsize;

        /* for COMMUNALs, store their size in the Value field */
        if (curr->sym.comm == TRUE) {
            p->st_info = ELF32_ST_INFO(STB_GLOBAL, STT_COMMON);
            p->st_value = curr->sym.total_size;
            p->st_shndx = SHN_COMMON;
        } else {
#if OWELFIMPORT
            p->st_info = ELF32_ST_INFO(STB_GLOBAL, STT_IMPORT);
#else
            p->st_info = ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE);
#endif
            p->st_value = curr->sym.offset; /* is always 0 */
            p->st_shndx = SHN_UNDEF;
        }

        strsize += len + 1;
        DebugMsg(("elf_write_symbols, EXTERNAL: symbol %s, ofs=%X\n", buffer, p->st_value));
        p++;
    }

    // 5. PROTOs which have been "used" and have no matching PROC are also
    // externals.

    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->sym.isproc == TRUE )
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        p->st_name = strsize;
#if OWELFIMPORT
        p->st_info = ELF32_ST_INFO(STB_GLOBAL, STT_IMPORT);
#else
        p->st_info = ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE);
#endif
        p->st_value = 0;
        p->st_shndx = SHN_UNDEF;

        strsize += len + 1;
        DebugMsg(("elf_write_symbols, PROTO: symbol %s, ofs=%X\n", buffer, p->st_value));
        p++;
    }

    /* 6. PUBLIC entries */
    vp = NULL;
    while ( curr = GetPublicData(&vp) ) {
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        seg = (dir_node *)curr->sym.segment;
        if (seg && seg->e.seginfo->segtype != SEGTYPE_CODE)
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;

        p->st_name = strsize;
        p->st_info = ELF32_ST_INFO(STB_GLOBAL, stt);
        p->st_value = curr->sym.offset;
        if (curr->sym.mem_type == MT_ABS)
            p->st_shndx = SHN_ABS;
        else if (seg)
            p->st_shndx = GetSegIdx(curr->sym.segment);
        else
            p->st_shndx = SHN_UNDEF;

        strsize += len + 1;

        DebugMsg(("elf_write_symbols, PUBLIC+LOCAL: symbol %s, ofs=%X\n", buffer, p->st_value));

        p++;
    }
#if ADDSTARTLABEL
    if (start_label) {
        Mangle( start_label, buffer );
        len = strlen( buffer );
        p->st_name = strsize;
        p->st_info = ELF32_ST_INFO(STB_ENTRY, STT_FUNC);
        p->st_value = start_label->offset;
        p->st_shndx = GetSegIdx(start_label->segment);
        strsize += len + 1;
        DebugMsg(("elf_write_symbols, ENTRY: symbol %s, ofs=%X\n", buffer, p->st_value));
        p++;
    }
#endif
    /* generate the string table */

    internal_segs[STRTAB_IDX].size = strsize;
    internal_segs[STRTAB_IDX].data = AsmAlloc(internal_segs[STRTAB_IDX].size);
    memset(internal_segs[STRTAB_IDX].data, 0, internal_segs[STRTAB_IDX].size);
    p2 = internal_segs[STRTAB_IDX].data;
    *p2++ = NULLC;

    strcpy(p2, srcname);
    p2 += strlen(p2) + 1;

    for ( localscurr = localshead ; localscurr ; localscurr = localscurr->next ) {
        Mangle( localscurr->sym, p2 );
        p2 += strlen(p2) + 1;
    }

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if (curr->sym.comm == FALSE && curr->sym.weak == TRUE)
            continue;
        Mangle( &curr->sym, p2 );
        p2 += strlen(p2) + 1;
    }
    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->sym.isproc == TRUE )
            continue;
        Mangle( &curr->sym, p2 );
        p2 += strlen(p2) + 1;
    }
    vp = NULL;
    while ( curr = GetPublicData(&vp) ) {
        Mangle( &curr->sym, p2 );
        p2 += strlen(p2) + 1;
    }
#if ADDSTARTLABEL
    if (start_label) {
        Mangle( start_label, p2 );
    }
#endif
    return;
}

// set content + size of .shstrtab section
// alloc .shstrtab

void set_shstrtab_values( void )
/************************/
{
    intseg      *seg;
    dir_node    *curr;
    char        *p;
    unsigned int size = 1;

    /* get program + reloc section sizes */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        p = ElfConvertSectionName( &curr->sym );
        size += strlen(p) + 1;
        if (curr->e.seginfo->FixupListHeadGen)
            size += strlen(p) + 1 + 4;
    }
    /* get internal sections sizes */
    for (seg = internal_segs ; seg->name; seg++) {
        size += strlen(seg->name) + 1;
    }

    internal_segs[SHSTRTAB_IDX].size = size;
    /* size is set, now alloc the section and fill it */
    internal_segs[SHSTRTAB_IDX].data = AsmAlloc(size);
    p = (char *)internal_segs[SHSTRTAB_IDX].data;
    *p++ = NULLC;
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        strcpy(p, ElfConvertSectionName( &curr->sym ));
        p += strlen(p) + 1;
    }
    for (seg = internal_segs ; seg->name; seg++) {
        strcpy(p, seg->name);
        p += strlen(p) + 1;
    }
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if (curr->e.seginfo->FixupListHeadGen) {
            strcpy(p, ".rel");
            p += strlen(p);
            strcpy(p, ElfConvertSectionName( &curr->sym ));
            p += strlen(p) + 1;
        }
    }
    return;
}


static unsigned int Get_Num_Relocs(dir_node *curr)
/************************************************/
{
    unsigned relocs;
    struct asmfixup *fix;

    for (relocs = 0, fix = curr->e.seginfo->FixupListHeadGen; fix ; fix = fix->nextrlc, relocs++);

    return( relocs );
}

static unsigned int Get_Alignment(dir_node *curr)
{
    if ( curr->e.seginfo->alignment == MAX_SEGALIGNMENT )
        return( 0 );
    return( 1 << curr->e.seginfo->alignment );
}

/* write ELF section table */

static int elf_write_section_table( int fh )
/******************************************/
{
    dir_node    *curr;
    uint_8      *p;
    uint        offset;
    intseg      *seg;
    Elf32_Shdr shdr;

    DebugMsg(("elf_write_section_table: enter\n"));

    offset = sizeof(ehdr) + ehdr.e_shnum * ehdr.e_shentsize;
    offset = (offset + 0xF) & ~0xF;

    set_shstrtab_values();

    memset(&shdr, 0, sizeof(shdr));
    if ( _write(fh, &shdr, sizeof(shdr)) != sizeof(shdr) ) /* write the empty NULL entry */
        WriteError();

    p = (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
    p++;

    /* write the section headers defined in the module */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {

        if( curr->sym.state != SYM_SEG ) {
            AsmErr( SEG_NOT_DEFINED, curr->sym.name );
            return( ERROR );
        }
        memset(&shdr, 0, sizeof(shdr));

        shdr.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr.sh_type = SHT_PROGBITS;
        if (curr->e.seginfo->segtype == SEGTYPE_BSS) {
            shdr.sh_flags = SHF_WRITE | SHF_ALLOC;
            shdr.sh_type = SHT_NOBITS;
        } else if (curr->e.seginfo->segtype == SEGTYPE_CODE) {
            shdr.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
        } else if (curr->e.seginfo->readonly == TRUE) {
            shdr.sh_flags = SHF_ALLOC;
        } else {
            shdr.sh_flags = SHF_WRITE | SHF_ALLOC;
        }
        shdr.sh_addr = 0;
        if (curr->e.seginfo->segtype != SEGTYPE_BSS) {
            shdr.sh_offset = offset; /* start of section in file */
            /* size of section in file */
            shdr.sh_size = curr->sym.max_offset;
        }
        shdr.sh_link = 0;
        shdr.sh_info = 0;
        shdr.sh_addralign = Get_Alignment( curr );
        shdr.sh_entsize = 0;

        if ( _write( fh, &shdr, sizeof(shdr) ) != sizeof(shdr) )
            WriteError();

        /* save the file offset in the segment item */
        curr->e.seginfo->fileoffset = offset;

        curr->e.seginfo->num_relocs = Get_Num_Relocs(curr);

        offset += shdr.sh_size;
        offset = (offset + 0xF) & ~0xF;

        DebugMsg(("elf_write_section_table: numrelocs=%u\n", curr->e.seginfo->num_relocs));

    }

    set_symtab_values();

    /* write headers of internal sections */
    for (seg = internal_segs ; seg->name; seg++) {
        shdr.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr.sh_type = seg->type;
        shdr.sh_flags = 0;
        shdr.sh_offset = offset; /* start of section in file */
        seg->offset = offset;
        shdr.sh_size = seg->size;
        if (seg->type == SHT_SYMTAB) {
            shdr.sh_link = 1 + total_segs + STRTAB_IDX;
            shdr.sh_info = start_globals;
            shdr.sh_addralign = 4;
            shdr.sh_entsize = sizeof(Elf32_Sym);
        } else {
            shdr.sh_link = 0;
            shdr.sh_info = 0;
            shdr.sh_addralign = 1;
            shdr.sh_entsize = 0;
        }
        if ( _write(fh, &shdr, sizeof(shdr)) != sizeof(shdr) )
            WriteError();

        offset += shdr.sh_size;
        offset = (offset + 0xF) & ~0xF;
    }

    /* write headers of reloc sections */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if (curr->e.seginfo->FixupListHeadGen == NULL)
            continue;
        memset(&shdr, 0, sizeof(shdr));

        shdr.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr.sh_type = SHT_REL;
        shdr.sh_flags = 0;
        shdr.sh_addr = 0;
        shdr.sh_offset = offset; /* start of section in file */
        /* save the file offset in the slot reserved for ELF relocs */
        curr->e.seginfo->reloc_offset = offset;
        /* size of section in file */
        shdr.sh_size = curr->e.seginfo->num_relocs * sizeof(Elf32_Rel);
        shdr.sh_link = 1 + total_segs + SYMTAB_IDX;
        /* set info to the src section index */
        shdr.sh_info = GetSegIdx( curr->sym.segment );
        shdr.sh_addralign = 4;
        shdr.sh_entsize = sizeof(Elf32_Rel);

        if ( _write(fh, &shdr, sizeof(shdr)) != sizeof(shdr) )
            WriteError();

        offset += shdr.sh_size;
        offset = (offset + 0xF) & ~0xF;

    }
    DebugMsg(("elf_write_section_table: exit\n"));
    return( NOT_ERROR );
}

// write ELF header
// total_segs has been set by the caller

ret_code elf_write_header( module_info *ModuleInfo )
/**************************************************/
{
    //dir_node   *dir;

    DebugMsg(("elf_write_header: enter\n"));

    SizeLongNames = sizeof(uint_32);

    srcname = FileInfo.fname[ASM];
    srcname += strlen(srcname);
    while (srcname > FileInfo.fname[ASM] &&
           *(srcname-1) != '/' &&
           *(srcname-1) != '\\') srcname--;

    directives = NULL;

    memset(&ehdr, 0, sizeof(ehdr));
    memcpy(&ehdr.e_ident, ELF_SIGNATURE, ELF_SIGNATURE_LEN);
    ehdr.e_ident[EI_CLASS] = ELFCLASS32;
    ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr.e_ident[EI_VERSION] = EV_CURRENT;
    ehdr.e_ident[EI_OSABI] = ELFOSABI_LINUX;
    ehdr.e_ident[EI_ABIVERSION] = EV_CURRENT;
    ehdr.e_type = ET_REL; /* file type */
    ehdr.e_machine = EM_386;
    ehdr.e_version = EV_CURRENT;
    ehdr.e_entry = 0; /* no entry for relocatable objects */
    ehdr.e_phoff = 0; /* no progheaders for relocatable objects */
    ehdr.e_shoff = sizeof(ehdr);
    ehdr.e_flags = 0;
    ehdr.e_ehsize = sizeof(ehdr);
    ehdr.e_phentsize = 0; /* no progheaders for relocatable objects */
    ehdr.e_phnum = 0;
    ehdr.e_shentsize = sizeof(Elf32_Shdr);
    /* 4 additional segment entries:
     - NULL entry
     - .shstrtab
     - .symtab
     - .strtab
     - .rel<xxx> entries
     */
    ehdr.e_shnum = 1 + total_segs + 3 + get_num_reloc_sections();
    ehdr.e_shstrndx = 1 + total_segs + SHSTRTAB_IDX;

    _lseek( ModuleInfo->obj_fh, 0, SEEK_SET );
    if ( _write( ModuleInfo->obj_fh, &ehdr, sizeof(ehdr)) != sizeof(ehdr) )
        WriteError();

    elf_write_section_table( ModuleInfo->obj_fh );

    DebugMsg(("elf_write_header: exit\n"));
    return( NOT_ERROR );
}

// write ELF symbol table
// contents of the table:
#if 0
ret_code elf_write_symbols( module_info *ModuleInfo )
/***************************************************/
{
    dir_node    *curr;
    void        *vp;
    char        *p;
    uint        len;
    uint        i;

    DebugMsg(("elf_write_symbols: enter\n"));

    DebugMsg(("elf_write_symbols: exit\n"));
    return( NOT_ERROR );
}
#endif

// write section contents and fixups
// this is done after the last step only!

ret_code elf_write_data( module_info *ModuleInfo )
/*************************************************/
{
    dir_node *curr;
    //int seg_index;
    //uint_32 offset = 0;
    uint_32     size;
    intseg      *seg;

    DebugMsg(("elf_write_data: enter\n"));

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        size = curr->sym.max_offset - curr->e.seginfo->start_loc;
        DebugMsg(("elf_write_data: program data at ofs=%X, size=%X\n", curr->e.seginfo->fileoffset, size ));
        if (curr->e.seginfo->segtype != SEGTYPE_BSS && curr->sym.max_offset != 0) {
            _lseek( ModuleInfo->obj_fh, curr->e.seginfo->fileoffset + curr->e.seginfo->start_loc, SEEK_SET );
            if ( curr->e.seginfo->CodeBuffer ) {
                if ( _write( ModuleInfo->obj_fh, curr->e.seginfo->CodeBuffer, size ) != size )
                    WriteError();
            }
        }
    }

    /* write internal sections */
    for ( seg = internal_segs ; seg->name; seg++ ) {
        if ( seg->data ) {
            DebugMsg(("elf_write_data: internal at ofs=%X, size=%X\n", seg->offset, seg->size));
            _lseek( ModuleInfo->obj_fh, seg->offset, SEEK_SET );
            if ( _write( ModuleInfo->obj_fh, seg->data, seg->size ) != seg->size )
                WriteError();
        }
    }

    /* write reloc sections content */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if (curr->e.seginfo->num_relocs) {
            struct asmfixup *fixup;
            Elf32_Rel reloc;
            DebugMsg(("elf_write_data: relocs at ofs=%X, size=%X\n", curr->e.seginfo->reloc_offset, curr->e.seginfo->num_relocs * sizeof(Elf32_Rel)));
            _lseek( ModuleInfo->obj_fh, curr->e.seginfo->reloc_offset, SEEK_SET );
            for ( fixup = curr->e.seginfo->FixupListHeadGen; fixup; fixup = fixup->nextrlc ) {
                uint_8 elftype;
                reloc.r_offset = fixup->fixup_loc;
                switch (fixup->type) {
                case FIX_RELOFF32:
                    elftype = R_386_PC32;
                    break;
                case FIX_OFF32:
                    elftype = R_386_32;
                    break;
                case FIX_OFF32_IMGREL:
                    elftype = R_386_RELATIVE;
                    break;
                case FIX_OFF32_SECREL:
                    elftype = R_386_GOT32;
                    break;
                default:
                    elftype = R_386_NONE;
                    if (fixup->type == FIX_SEG ||
                        fixup->type == FIX_LOBYTE ||
                        fixup->type == FIX_HIBYTE ||
                        fixup->type == FIX_RELOFF8 ||
                        fixup->type == FIX_RELOFF16 ||
                        fixup->type == FIX_OFF16 ||
                        fixup->type == FIX_PTR16 ||
                        fixup->type == FIX_PTR32) {
                        AsmErr( INVALID_FIXUP_TYPE, "ELF", fixup->type );
                    } else
                        AsmErr( UNKNOWN_FIXUP_TYPE, fixup->type );
                }
                /* the low 8 bits of info are type */
                /* the high 24 bits are symbol table index */
                reloc.r_info = ELF32_R_INFO(fixup->sym->idx, elftype);
                if ( _write( ModuleInfo->obj_fh, &reloc, sizeof(reloc) ) != sizeof(reloc) )
                    WriteError();
            }
        }
    }

    DebugMsg(("elf_write_data: exit\n"));

    return( NOT_ERROR );
}
#endif
