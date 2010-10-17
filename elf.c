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
#include "memalloc.h"
#include "symbols.h"
#include "parser.h"
#include "directiv.h"
#include "mangle.h"
#include "fixup.h"
#include "segment.h"
#include "extern.h"
#include "elf.h"
#include "elfspec.h"
#include "fatal.h"

#if ELF_SUPPORT

/* v2.03: create weak externals for ALIAS symbols.
 * Since the syntax of the ALIAS directive requires
 * 2 names and, OTOH, the ELF implementation of weak 
 * externals has no "default resolution", the usefullness
 * of this option is questionable. Additionally, there's
 * the "EXTERN <name> (<altname>)" syntax, which also allows
 * to define a weak external.
 */
#define ELFALIAS 0

/* start label is always public for COFF/ELF, no need to add it */
#define ADDSTARTLABEL 0

/* there's no STT_IMPORT type for ELF, it's OW specific */
#define OWELFIMPORT 0

/* use GNU extensions for LD ( 16bit and 8bit relocations ) */
#define GNURELOCS 1

#define MANGLE_BYTES 8 /* extra size required for name decoration */

#define IsWeak( x ) ( x.comm == FALSE && x.altname )

/* section attributes for ELF
 *         execute write  alloc  type
 *---------------------------------------
 * CODE      x              x    progbits
 * CONST                    x    progbits
 * DATA              x      x    progbits
 * BSS               x      x    nobits
 * STACK             x      x    progbits
 * others            x      x    progbits
 */

//static uint_32 size_drectve;   /* size of .drectve section */
static uint_32 symindex;       /* entries in symbol table */
static uint_32 start_globals;  /* start index globals in symbol table */
static dir_node *directives;

static uint_32 SizeLongNames;
static char *srcname; /* name of source module (name + extension) */

#if SETDATAPOS
static uint_32 data_pos;
#endif
#if GNURELOCS
static bool extused;
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

struct conv_section {
    uint_8 len;
    uint_8 flags;
    const char * src;
    const char * dst;
};
enum cvs_flags {
    CSF_GRPCHK = 1
};

static const struct conv_section cst[] = {
    { 5, CSF_GRPCHK, "_TEXT", ".text" },
    { 5, CSF_GRPCHK, "_DATA", ".data" },
    { 5, CSF_GRPCHK, "CONST", ".rdata" },
    { 4, 0, "_BSS", ".bss" }
};


/* translate section names:
 * _TEXT -> .text
 * _DATA -> .data
 * _BSS  -> .bss
 * CONST -> .rdata
 */
static char * ElfConvertSectionName( const asm_sym * sym )
/********************************************************/
{
    int i;
    static char name[MAX_ID_LEN+1];

    for ( i = 0; i < sizeof( cst ) / sizeof( cst[0] ); i++ ) {
        if ( memcmp( sym->name, cst[i].src, cst[i].len ) == 0 ) {
            if ( sym->name[cst[i].len] == NULLC )
                return( (char *)cst[i].dst );
            else if ( ( cst[i].flags & CSF_GRPCHK )  && sym->name[cst[i].len] == '$' ) {
                strcpy( name, cst[i].dst );
                strcat( name, sym->name+cst[i].len );
                return( name );
            }
        }
    }
    return( sym->name );
}

static int get_num_reloc_sections( void )
/***************************************/
{
    dir_node    *curr;
    int num = 0;

    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->FixupListHead )
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
static void set_symtab_values( void *hdr )
/****************************************/
{
    uint_32 entries;
    uint_32 strsize = 1;
    uint_32 len;
    dir_node *curr;
    asm_sym *sym;
    dir_node *seg;
    char *p2;
    void *vp;
    Elf32_Sym *p32;
    Elf64_Sym *p64;
    //uint_8 stb;
    uint_8 stt;
    localname *localshead = NULL;
    localname *localstail = NULL;
    localname *localscurr;
    char buffer[MAX_ID_LEN + MANGLE_BYTES + 1];

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
            struct fixup * fix = curr->e.seginfo->FixupListHead;
            for ( ; fix; fix = fix->nextrlc ) {
                /* if it's not EXTERNAL/PUBLIC, add symbol. */
                /* however, if it's an assembly time variable */
                /* use a raw section reference */
                if (fix->sym->variable) {
                    fix->sym = fix->segment;
                } else if ((fix->sym->state == SYM_INTERNAL ) &&
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

    /* count EXTERNs and used EXTERNDEFs (and PROTOs [since v2.01]) */
    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if ( curr->sym.comm == FALSE && curr->sym.weak == TRUE )
            continue;
        curr->sym.idx = symindex++;
    }
    DebugMsg(("set_symtab_values: index after EXTERNALs: %u\n", symindex));

#if ELFALIAS
    /* count aliases */
    for( curr = Tables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        curr->sym.idx = symindex++;
    }
    DebugMsg(("set_symtab_values: index after ALIASES: %u\n", symindex));
#endif

    /* count publics */
    vp = NULL;
    while( sym = GetPublicData( &vp ) ) {
        sym->idx = symindex++;
    }
    DebugMsg(("set_symtab_values: index after PUBLICs: %u\n", symindex));

    /* size of symbol table is defined */

    entries = symindex;

#if ADDSTARTLABEL
    if ( ModuleInfo.start_label )
        entries++;
#endif

#if AMD64_SUPPORT
    if ( Options.header_format == HFORMAT_ELF64 ) {
    internal_segs[SYMTAB_IDX].size = entries * sizeof(Elf64_Sym);
    internal_segs[SYMTAB_IDX].data = AsmAlloc(internal_segs[SYMTAB_IDX].size);
    memset(internal_segs[SYMTAB_IDX].data, 0, internal_segs[SYMTAB_IDX].size);

    p64 = (Elf64_Sym *)internal_segs[SYMTAB_IDX].data;

    p64++; /* skip NULL entry */

    /* 1. make file entry */
    p64->st_name = strsize;
    strsize += strlen(srcname) + 1;
    p64->st_value = 0;
    p64->st_size = 0;
    p64->st_info = ELF64_ST_INFO(STB_LOCAL, STT_FILE); /* symbol's type and binding info */
    p64->st_shndx = SHN_ABS; /* section index */
    p64++;

    /* 2. make section entries */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        p64->st_info = ELF64_ST_INFO(STB_LOCAL, STT_SECTION);
        p64->st_shndx = GetSegIdx( curr->sym.segment );
        p64++;
    }

    /* 3. locals */

    for ( localscurr = localshead ; localscurr ; localscurr = localscurr->next ) {
        Mangle( localscurr->sym, buffer );
        len = strlen( buffer );
        p64->st_name = strsize;
        seg = (dir_node *)localscurr->sym->segment;
        if (seg && seg->e.seginfo->segtype != SEGTYPE_CODE)
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;
        p64->st_info = ELF64_ST_INFO(STB_LOCAL, stt);
        p64->st_value = localscurr->sym->offset;
        if (localscurr->sym->mem_type == MT_ABS)
            p64->st_shndx = SHN_ABS;
        else
            p64->st_shndx = GetSegIdx(localscurr->sym->segment);
        strsize += len + 1;
        DebugMsg(("set_symtab_values, LOCAL: symbol %s, ofs=%X\n", buffer, p64->st_value));
        p64++;
    }

    /* 4. externals + communals ( + protos [since v2.01]) */

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if (curr->sym.comm == FALSE && curr->sym.weak == TRUE)
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        p64->st_name = strsize;

        /* for COMMUNALs, store their size in the Value field */
        if ( curr->sym.comm == TRUE ) {
            p64->st_info = ELF64_ST_INFO(STB_GLOBAL, STT_COMMON);
            p64->st_value = curr->sym.total_size;
            p64->st_shndx = SHN_COMMON;
        } else {
#if OWELFIMPORT
            p64->st_info = ( IsWeak( curr->sym ) ? ELF64_ST_INFO(STB_WEAK, STT_IMPORT) : ELF64_ST_INFO(STB_GLOBAL, STT_IMPORT) );
#else
            p64->st_info = ( IsWeak( curr->sym ) ? ELF64_ST_INFO(STB_WEAK, STT_NOTYPE) : ELF64_ST_INFO(STB_GLOBAL, STT_NOTYPE) );
#endif
            p64->st_value = curr->sym.offset; /* is always 0 */
            p64->st_shndx = SHN_UNDEF;
        }

        strsize += len + 1;
        DebugMsg(("set_symtab_values, EXTERNAL: symbol %s, ofs=%X\n", buffer, p64->st_value));
        p64++;
    }

#if ELFALIAS
    /* 5. aliases */

    for( curr = Tables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        p64->st_name = strsize;

#if OWELFIMPORT
        p64->st_info = ELF64_ST_INFO(STB_WEAK, STT_IMPORT);
#else
        p64->st_info = ELF64_ST_INFO(STB_WEAK, STT_NOTYPE);
#endif
        p64->st_value = 0; /* is always 0 */
        p64->st_shndx = SHN_UNDEF;

        strsize += len + 1;
        DebugMsg(("set_symtab_values, ALIASES: symbol %s, ofs=%X\n", buffer, p64->st_value));
        p64++;
    }
#endif

    /* 6. PUBLIC entries */
    vp = NULL;
    while ( sym = GetPublicData(&vp) ) {
        Mangle( sym, buffer );
        len = strlen( buffer );

        seg = (dir_node *)sym->segment;
        if (seg && seg->e.seginfo->segtype != SEGTYPE_CODE)
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;

        p64->st_name = strsize;
        p64->st_info = ELF64_ST_INFO(STB_GLOBAL, stt);
        p64->st_value = sym->offset;
        if ( sym->mem_type == MT_ABS )
            p64->st_shndx = SHN_ABS;
        else if (seg)
            p64->st_shndx = GetSegIdx( sym->segment );
        else
            p64->st_shndx = SHN_UNDEF;

        strsize += len + 1;

        DebugMsg(("set_symtab_values, PUBLIC+LOCAL: symbol %s, ofs=%X\n", buffer, p64->st_value));

        p64++;
    }
#if ADDSTARTLABEL
    if ( ModuleInfo.start_label ) {
        Mangle( ModuleInfo.start_label, buffer );
        len = strlen( buffer );
        p64->st_name = strsize;
        p64->st_info = ELF64_ST_INFO(STB_ENTRY, STT_FUNC);
        p64->st_value = ModuleInfo.start_label->offset;
        p64->st_shndx = GetSegIdx( ModuleInfo.start_label->segment );
        strsize += len + 1;
        DebugMsg(("set_symtab_values, ENTRY: symbol %s, ofs=%X\n", buffer, p64->st_value));
        p64++;
    }
#endif
    } else {
#endif
    internal_segs[SYMTAB_IDX].size = entries * sizeof(Elf32_Sym);
    internal_segs[SYMTAB_IDX].data = AsmAlloc(internal_segs[SYMTAB_IDX].size);
    memset(internal_segs[SYMTAB_IDX].data, 0, internal_segs[SYMTAB_IDX].size);

    p32 = (Elf32_Sym *)internal_segs[SYMTAB_IDX].data;

    p32++; /* skip NULL entry */

    /* 1. make file entry */
    p32->st_name = strsize;
    strsize += strlen(srcname) + 1;
    p32->st_value = 0;
    p32->st_size = 0;
    p32->st_info = ELF32_ST_INFO(STB_LOCAL, STT_FILE); /* symbol's type and binding info */
    p32->st_shndx = SHN_ABS; /* section index */
    p32++;

    /* 2. make section entries */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        p32->st_info = ELF32_ST_INFO(STB_LOCAL, STT_SECTION);
        p32->st_shndx = GetSegIdx( curr->sym.segment );
        p32++;
    }

    /* 3. locals */

    for ( localscurr = localshead ; localscurr ; localscurr = localscurr->next ) {
        Mangle( localscurr->sym, buffer );
        len = strlen( buffer );
        p32->st_name = strsize;
        seg = (dir_node *)localscurr->sym->segment;
        if (seg && seg->e.seginfo->segtype != SEGTYPE_CODE)
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;
        p32->st_info = ELF32_ST_INFO(STB_LOCAL, stt);
        p32->st_value = localscurr->sym->offset;
        if (localscurr->sym->mem_type == MT_ABS)
            p32->st_shndx = SHN_ABS;
        else
            p32->st_shndx = GetSegIdx(localscurr->sym->segment);
        strsize += len + 1;
        DebugMsg(("set_symtab_values, LOCAL: symbol %s, ofs=%X\n", buffer, p32->st_value));
        p32++;
    }

    /* 4. externals + communals (+ protos [since v2.01]) */

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if ( curr->sym.comm == FALSE && curr->sym.weak == TRUE )
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        p32->st_name = strsize;

        /* for COMMUNALs, store their size in the Value field */
        if ( curr->sym.comm == TRUE ) {
            p32->st_info = ELF32_ST_INFO(STB_GLOBAL, STT_COMMON);
            p32->st_value = curr->sym.total_size;
            p32->st_shndx = SHN_COMMON;
        } else {
#if OWELFIMPORT
            p32->st_info = ( IsWeak( curr->sym ) ? ELF32_ST_INFO(STB_WEAK, STT_IMPORT) : ELF32_ST_INFO(STB_GLOBAL, STT_IMPORT) );
#else
            p32->st_info = ( IsWeak( curr->sym ) ? ELF32_ST_INFO(STB_WEAK, STT_NOTYPE) : ELF32_ST_INFO(STB_GLOBAL, STT_NOTYPE) );
#endif
            p32->st_value = curr->sym.offset; /* is always 0 */
            p32->st_shndx = SHN_UNDEF;
        }

        strsize += len + 1;
        DebugMsg(("set_symtab_values, EXTERNAL: symbol %s, ofs=%X\n", buffer, p32->st_value));
        p32++;
    }

#if ELFALIAS
    /* 5. aliases */
    for( curr = Tables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        p32->st_name = strsize;

#if OWELFIMPORT
        p32->st_info = ELF32_ST_INFO(STB_WEAK, STT_IMPORT);
#else
        p32->st_info = ELF32_ST_INFO(STB_WEAK, STT_NOTYPE);
#endif
        p32->st_value = 0; /* is always 0 */
        p32->st_shndx = SHN_UNDEF;

        strsize += len + 1;
        DebugMsg(("set_symtab_values, ALIAS: symbol %s, ofs=%X\n", buffer, p32->st_value));
        p32++;
    }
#endif

    /* 6. PUBLIC entries */
    vp = NULL;
    while ( sym = GetPublicData(&vp) ) {
        Mangle( sym, buffer );
        len = strlen( buffer );

        seg = (dir_node *)sym->segment;
        if (seg && seg->e.seginfo->segtype != SEGTYPE_CODE)
            stt = STT_OBJECT;
        else
            stt = STT_FUNC;

        p32->st_name = strsize;
        p32->st_info = ELF32_ST_INFO(STB_GLOBAL, stt);
        p32->st_value = sym->offset;
        if ( sym->mem_type == MT_ABS )
            p32->st_shndx = SHN_ABS;
        else if (seg)
            p32->st_shndx = GetSegIdx( sym->segment );
        else
            p32->st_shndx = SHN_UNDEF;

        strsize += len + 1;

        DebugMsg(("set_symtab_values, PUBLIC+LOCAL: symbol %s, ofs=%X\n", buffer, p32->st_value));

        p32++;
    }
#if ADDSTARTLABEL
    if ( ModuleInfo.start_label ) {
        Mangle( ModuleInfo.start_label, buffer );
        len = strlen( buffer );
        p32->st_name = strsize;
        p32->st_info = ELF32_ST_INFO(STB_ENTRY, STT_FUNC);
        p32->st_value = ModuleInfo.start_label->offset;
        p32->st_shndx = GetSegIdx( ModuleInfo.start_label->segment );
        strsize += len + 1;
        DebugMsg(("set_symtab_values, ENTRY: symbol %s, ofs=%X\n", buffer, p32->st_value));
        p32++;
    }
#endif
#if AMD64_SUPPORT
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
        if ( curr->sym.comm == FALSE && curr->sym.weak == TRUE )
            continue;
        Mangle( &curr->sym, p2 );
        p2 += strlen(p2) + 1;
    }

#if ELFALIAS
    for( curr = Tables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        Mangle( &curr->sym, p2 );
        p2 += strlen(p2) + 1;
    }
#endif

    vp = NULL;
    while ( sym = GetPublicData( &vp ) ) {
        Mangle( sym, p2 );
        p2 += strlen( p2 ) + 1;
    }
#if ADDSTARTLABEL
    if ( ModuleInfo.start_label ) {
        Mangle( ModuleInfo.start_label, p2 );
    }
#endif
    return;
}

/* set content + size of .shstrtab section
 * alloc .shstrtab
 */
static void set_shstrtab_values( void )
/*************************************/
{
    intseg      *seg;
    dir_node    *curr;
    char        *p;
    unsigned int size = 1;

    /* get program + reloc section sizes */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        p = ElfConvertSectionName( &curr->sym );
        size += strlen(p) + 1;
        if ( curr->e.seginfo->FixupListHead )
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
        if ( curr->e.seginfo->FixupListHead ) {
            strcpy(p, ".rel");
            p += strlen(p);
            strcpy(p, ElfConvertSectionName( &curr->sym ));
            p += strlen(p) + 1;
        }
    }
    return;
}


static unsigned int Get_Num_Relocs( dir_node *curr )
/**************************************************/
{
    unsigned relocs;
    struct fixup *fix;

    for (relocs = 0, fix = curr->e.seginfo->FixupListHead; fix ; fix = fix->nextrlc, relocs++);

    return( relocs );
}

static unsigned int Get_Alignment(dir_node *curr)
{
    if ( curr->e.seginfo->alignment == MAX_SEGALIGNMENT )
        return( 0 );
    return( 1 << curr->e.seginfo->alignment );
}

/* write ELF section table */

static int elf_write_section_table( module_info *ModuleInfo, void *hdr, uint offset )
/***********************************************************************************/
{
    dir_node    *curr;
    uint_8      *p;
    //uint        offset;
    uint        entrysize;
    intseg      *seg;
    Elf32_Shdr  shdr32;
    Elf64_Shdr  shdr64;

    DebugMsg(("elf_write_section_table: enter\n"));

    //offset = sizeof(Elf64_Ehdr) + ehdr->e_shnum * ehdr->e_shentsize;
    offset = (offset + 0xF) & ~0xF;

    set_shstrtab_values();

#if AMD64_SUPPORT
    if ( Options.header_format == HFORMAT_ELF64 ) {
        entrysize = sizeof( Elf64_Shdr );
        memset( &shdr64, 0, sizeof( shdr64) );
        if ( fwrite( &shdr64, 1, sizeof(shdr64), FileInfo.file[OBJ] ) != sizeof(shdr64) ) /* write the empty NULL entry */
            WriteError();
    } else {
#endif
        entrysize = sizeof( Elf32_Shdr );
        memset( &shdr32, 0, sizeof( shdr32) );
        if ( fwrite( &shdr32, 1, sizeof(shdr32), FileInfo.file[OBJ] ) != sizeof(shdr32) ) /* write the empty NULL entry */
            WriteError();
#if AMD64_SUPPORT
    }
#endif
    p = (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
    p++;

    /* write the section headers defined in the module */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {

#if AMD64_SUPPORT
        if ( Options.header_format == HFORMAT_ELF64 ) {
            memset( &shdr64, 0, sizeof(shdr64) );

            shdr64.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
            p += strlen( (char *)p ) + 1;
            shdr64.sh_type = SHT_PROGBITS;
            if ( curr->e.seginfo->segtype == SEGTYPE_BSS ) {
                shdr64.sh_flags = SHF_WRITE | SHF_ALLOC;
                shdr64.sh_type = SHT_NOBITS;
            } else if ( curr->e.seginfo->segtype == SEGTYPE_CODE ) {
                shdr64.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
            } else if ( curr->e.seginfo->readonly == TRUE ) {
                shdr64.sh_flags = SHF_ALLOC;
            } else {
                shdr64.sh_flags = SHF_WRITE | SHF_ALLOC;
            }
            shdr64.sh_addr = 0;
            if ( curr->e.seginfo->segtype != SEGTYPE_BSS ) {
                shdr64.sh_offset = offset; /* start of section in file */
                /* size of section in file */
                shdr64.sh_size = curr->sym.max_offset;
            }
            shdr64.sh_link = 0;
            shdr64.sh_info = 0;
            shdr64.sh_addralign = Get_Alignment( curr );
            shdr64.sh_entsize = 0;

            if ( fwrite( &shdr64, 1, sizeof(shdr64), FileInfo.file[OBJ] ) != sizeof(shdr64) )
                WriteError();
            curr->e.seginfo->fileoffset = offset;
            offset += shdr64.sh_size;
        } else {
#endif
        memset(&shdr32, 0, sizeof(shdr32));

        shdr32.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr32.sh_type = SHT_PROGBITS;
        if ( curr->e.seginfo->segtype == SEGTYPE_BSS ) {
            shdr32.sh_flags = SHF_WRITE | SHF_ALLOC;
            shdr32.sh_type = SHT_NOBITS;
        } else if ( curr->e.seginfo->segtype == SEGTYPE_CODE ) {
            shdr32.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
        } else if ( curr->e.seginfo->readonly == TRUE ) {
            shdr32.sh_flags = SHF_ALLOC;
        } else {
            shdr32.sh_flags = SHF_WRITE | SHF_ALLOC;
        }

#if 0
        /* todo: translate values in field <characteristics> to
         * elf section flags.
         */
        if ( curr->e.seginfo->characteristics == ??? ) {
        }
#endif

        shdr32.sh_addr = 0;
        if ( curr->e.seginfo->segtype != SEGTYPE_BSS ) {
            shdr32.sh_offset = offset; /* start of section in file */
            /* size of section in file */
            shdr32.sh_size = curr->sym.max_offset;
        }
        shdr32.sh_link = 0;
        shdr32.sh_info = 0;
        shdr32.sh_addralign = Get_Alignment( curr );
        shdr32.sh_entsize = 0;

        if ( fwrite( &shdr32, 1, sizeof(shdr32), FileInfo.file[OBJ] ) != sizeof(shdr32) )
            WriteError();
        /* save the file offset in the segment item */
        curr->e.seginfo->fileoffset = offset;
        offset += shdr32.sh_size;
#if AMD64_SUPPORT
        }
#endif
        curr->e.seginfo->num_relocs = Get_Num_Relocs(curr);

        offset = (offset + 0xF) & ~0xF;

        DebugMsg(("elf_write_section_table: numrelocs=%u\n", curr->e.seginfo->num_relocs));

    }

    set_symtab_values( hdr );

    /* write headers of internal sections */
    for (seg = internal_segs ; seg->name; seg++) {
#if AMD64_SUPPORT
        if ( Options.header_format == HFORMAT_ELF64 ) {
        shdr64.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr64.sh_type = seg->type;
        shdr64.sh_flags = 0;
        shdr64.sh_offset = offset; /* start of section in file */
        seg->offset = offset;
        shdr64.sh_size = seg->size;
        if (seg->type == SHT_SYMTAB) {
            shdr64.sh_link = 1 + ModuleInfo->g.num_segs + STRTAB_IDX;
            shdr64.sh_info = start_globals;
            shdr64.sh_addralign = 4;
            shdr64.sh_entsize = sizeof(Elf64_Sym);
        } else {
            shdr64.sh_link = 0;
            shdr64.sh_info = 0;
            shdr64.sh_addralign = 1;
            shdr64.sh_entsize = 0;
        }
        if ( fwrite( &shdr64, 1, sizeof(shdr64), FileInfo.file[OBJ] ) != sizeof(shdr64) )
            WriteError();
        offset += shdr64.sh_size;
        } else {
#endif
        shdr32.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr32.sh_type = seg->type;
        shdr32.sh_flags = 0;
        shdr32.sh_offset = offset; /* start of section in file */
        seg->offset = offset;
        shdr32.sh_size = seg->size;
        if (seg->type == SHT_SYMTAB) {
            shdr32.sh_link = 1 + ModuleInfo->g.num_segs + STRTAB_IDX;
            shdr32.sh_info = start_globals;
            shdr32.sh_addralign = 4;
            shdr32.sh_entsize = sizeof(Elf32_Sym);
        } else {
            shdr32.sh_link = 0;
            shdr32.sh_info = 0;
            shdr32.sh_addralign = 1;
            shdr32.sh_entsize = 0;
        }
        if ( fwrite( &shdr32, 1, sizeof(shdr32), FileInfo.file[OBJ] ) != sizeof(shdr32) )
            WriteError();
        offset += shdr32.sh_size;
#if AMD64_SUPPORT
        }
#endif

        offset = (offset + 0xF) & ~0xF;
    }

    /* write headers of reloc sections */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if ( curr->e.seginfo->FixupListHead == NULL )
            continue;

#if AMD64_SUPPORT
        if ( Options.header_format == HFORMAT_ELF64 ) {
        memset(&shdr64, 0, sizeof(shdr64));

        shdr64.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr64.sh_type = SHT_REL;
        shdr64.sh_flags = 0;
        shdr64.sh_addr = 0;
        shdr64.sh_offset = offset; /* start of section in file */
        /* save the file offset in the slot reserved for ELF relocs */
        curr->e.seginfo->reloc_offset = offset;
        /* size of section in file */
        shdr64.sh_size = curr->e.seginfo->num_relocs * sizeof(Elf64_Rel);
        shdr64.sh_link = 1 + ModuleInfo->g.num_segs + SYMTAB_IDX;
        /* set info to the src section index */
        shdr64.sh_info = GetSegIdx( curr->sym.segment );
        shdr64.sh_addralign = 4;
        shdr64.sh_entsize = sizeof(Elf64_Rel);

        if ( fwrite( &shdr64, 1, sizeof(shdr64), FileInfo.file[OBJ] ) != sizeof(shdr64) )
            WriteError();

        offset += shdr64.sh_size;
        } else {
#endif

        memset(&shdr32, 0, sizeof(shdr32));

        shdr32.sh_name = p - (uint_8 *)internal_segs[SHSTRTAB_IDX].data;
        p += strlen( (char *)p ) + 1;
        shdr32.sh_type = SHT_REL;
        shdr32.sh_flags = 0;
        shdr32.sh_addr = 0;
        shdr32.sh_offset = offset; /* start of section in file */
        /* save the file offset in the slot reserved for ELF relocs */
        curr->e.seginfo->reloc_offset = offset;
        /* size of section in file */
        shdr32.sh_size = curr->e.seginfo->num_relocs * sizeof(Elf32_Rel);
        shdr32.sh_link = 1 + ModuleInfo->g.num_segs + SYMTAB_IDX;
        /* set info to the src section index */
        shdr32.sh_info = GetSegIdx( curr->sym.segment );
        shdr32.sh_addralign = 4;
        shdr32.sh_entsize = sizeof(Elf32_Rel);

        if ( fwrite( &shdr32, 1, sizeof(shdr32), FileInfo.file[OBJ] ) != sizeof(shdr32) )
            WriteError();

        offset += shdr32.sh_size;
#if AMD64_SUPPORT
        }
#endif
        offset = (offset + 0xF) & ~0xF;

    }
    DebugMsg(("elf_write_section_table: exit\n"));
    return( NOT_ERROR );
}

/* write ELF header
 */
ret_code elf_write_header( module_info *ModuleInfo )
/**************************************************/
{
    //dir_node   *dir;
    Elf32_Ehdr ehdr32;
    Elf64_Ehdr ehdr64;

    DebugMsg(("elf_write_header: enter\n"));

    SizeLongNames = sizeof(uint_32);

    srcname = FileInfo.fname[ASM];
    srcname += strlen(srcname);
    while (srcname > FileInfo.fname[ASM] &&
           *(srcname-1) != '/' &&
           *(srcname-1) != '\\') srcname--;

    directives = NULL;
#if GNURELOCS
    extused = FALSE;
#endif

#if AMD64_SUPPORT
    if ( Options.header_format == HFORMAT_ELF64 ) {
    memset(&ehdr64, 0, sizeof( ehdr64 ) );
    memcpy(&ehdr64.e_ident, ELF_SIGNATURE, ELF_SIGNATURE_LEN);
    ehdr64.e_ident[EI_CLASS] = ELFCLASS64;
    ehdr64.e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr64.e_ident[EI_VERSION] = EV_CURRENT;
    ehdr64.e_ident[EI_OSABI] = ModuleInfo->osabi;
    ehdr64.e_ident[EI_ABIVERSION] = EV_CURRENT;
    ehdr64.e_type = ET_REL; /* file type */
    ehdr64.e_machine = EM_X86_64;
    ehdr64.e_version = EV_CURRENT;
    ehdr64.e_entry = 0; /* no entry for relocatable objects */
    ehdr64.e_phoff = 0; /* no progheaders for relocatable objects */
    ehdr64.e_shoff = sizeof(ehdr64);
    ehdr64.e_flags = 0;
    ehdr64.e_ehsize = sizeof(ehdr64);
    ehdr64.e_phentsize = 0; /* no progheaders for relocatable objects */
    ehdr64.e_phnum = 0;
    ehdr64.e_shentsize = sizeof(Elf64_Shdr);
    /* 4 additional segment entries:
     - NULL entry
     - .shstrtab
     - .symtab
     - .strtab
     - .rel<xxx> entries
     */
    ehdr64.e_shnum = 1 + ModuleInfo->g.num_segs + 3 + get_num_reloc_sections();
    ehdr64.e_shstrndx = 1 + ModuleInfo->g.num_segs + SHSTRTAB_IDX;
    fseek( FileInfo.file[OBJ], 0, SEEK_SET );
    if ( fwrite( &ehdr64, 1, sizeof(ehdr64), FileInfo.file[OBJ] ) != sizeof(ehdr64) )
        WriteError();
    elf_write_section_table( ModuleInfo, &ehdr64,
                            sizeof(Elf64_Ehdr) + ehdr64.e_shnum * ehdr64.e_shentsize );
    } else {
#endif
    memset(&ehdr32, 0, sizeof( ehdr32 ) );
    memcpy(&ehdr32.e_ident, ELF_SIGNATURE, ELF_SIGNATURE_LEN);
    ehdr32.e_ident[EI_CLASS] = ELFCLASS32;
    ehdr32.e_ident[EI_DATA] = ELFDATA2LSB;
    ehdr32.e_ident[EI_VERSION] = EV_CURRENT;
    ehdr32.e_ident[EI_OSABI] = ModuleInfo->osabi;
    ehdr32.e_ident[EI_ABIVERSION] = EV_CURRENT;
    ehdr32.e_type = ET_REL; /* file type */
    ehdr32.e_machine = EM_386;
    ehdr32.e_version = EV_CURRENT;
    ehdr32.e_entry = 0; /* no entry for relocatable objects */
    ehdr32.e_phoff = 0; /* no progheaders for relocatable objects */
    ehdr32.e_shoff = sizeof(ehdr32);
    ehdr32.e_flags = 0;
    ehdr32.e_ehsize = sizeof(ehdr32);
    ehdr32.e_phentsize = 0; /* no progheaders for relocatable objects */
    ehdr32.e_phnum = 0;
    ehdr32.e_shentsize = sizeof(Elf32_Shdr);
    /* 4 additional segment entries:
     - NULL entry
     - .shstrtab
     - .symtab
     - .strtab
     - .rel<xxx> entries
     */
    ehdr32.e_shnum = 1 + ModuleInfo->g.num_segs + 3 + get_num_reloc_sections();
    ehdr32.e_shstrndx = 1 + ModuleInfo->g.num_segs + SHSTRTAB_IDX;
    fseek( FileInfo.file[OBJ], 0, SEEK_SET );
    if ( fwrite( &ehdr32, 1, sizeof(ehdr32), FileInfo.file[OBJ] ) != sizeof(ehdr32) )
        WriteError();
    elf_write_section_table( ModuleInfo, &ehdr32,
                            sizeof(Elf32_Ehdr) + ehdr32.e_shnum * ehdr32.e_shentsize );
#if AMD64_SUPPORT
    };
#endif
    DebugMsg(("elf_write_header: exit\n"));
    return( NOT_ERROR );
}

/* write ELF symbol table
 * contents of the table:
 */
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

/* write section contents and fixups
 * this is done after the last step only!
 */
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
            fseek( FileInfo.file[OBJ], curr->e.seginfo->fileoffset + curr->e.seginfo->start_loc, SEEK_SET );
            if ( curr->e.seginfo->CodeBuffer ) {
                if ( fwrite( curr->e.seginfo->CodeBuffer, 1, size, FileInfo.file[OBJ] ) != size )
                    WriteError();
            }
        }
    }

    /* write internal sections */
    for ( seg = internal_segs ; seg->name; seg++ ) {
        if ( seg->data ) {
            DebugMsg(("elf_write_data: internal at ofs=%X, size=%X\n", seg->offset, seg->size));
            fseek( FileInfo.file[OBJ], seg->offset, SEEK_SET );
            if ( fwrite( seg->data, 1, seg->size, FileInfo.file[OBJ] ) != seg->size )
                WriteError();
        }
    }

    /* write reloc sections content */
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {
        if (curr->e.seginfo->num_relocs) {
            struct fixup *fixup;
            Elf32_Rel reloc32;
            Elf64_Rel reloc64;
            DebugMsg(("elf_write_data: relocs at ofs=%X, size=%X\n", curr->e.seginfo->reloc_offset, curr->e.seginfo->num_relocs * sizeof(Elf32_Rel)));
            fseek( FileInfo.file[OBJ], curr->e.seginfo->reloc_offset, SEEK_SET );
            for ( fixup = curr->e.seginfo->FixupListHead; fixup; fixup = fixup->nextrlc ) {
                uint_8 elftype;
#if AMD64_SUPPORT
                if ( Options.header_format == HFORMAT_ELF64 ) {
                reloc64.r_offset = fixup->location;
                switch (fixup->type) {
                case FIX_RELOFF32:
                    elftype = R_X86_64_PC32;
                    break;
                case FIX_OFF32:
                    elftype = R_X86_64_32;
                    break;
                case FIX_OFF64:
                    elftype = R_X86_64_64;
                    break;
                case FIX_OFF32_IMGREL:
                    elftype = R_X86_64_RELATIVE;
                    break;
                case FIX_OFF32_SECREL:
                    elftype = R_X86_64_GOT32;
                    break;
                default:
                    elftype = R_X86_64_NONE;
                    if (fixup->type == FIX_SEG ||
                        fixup->type == FIX_LOBYTE ||
                        fixup->type == FIX_HIBYTE ||
                        fixup->type == FIX_RELOFF8 ||
                        fixup->type == FIX_RELOFF16 ||
                        fixup->type == FIX_OFF16 ||
                        fixup->type == FIX_PTR16 ||
                        fixup->type == FIX_PTR32) {
                        AsmErr( INVALID_FIXUP_TYPE, "ELF", fixup->type, curr->sym.name, fixup->location );
                    } else
                        AsmErr( UNKNOWN_FIXUP_TYPE, fixup->type, fixup->location );
                }
                /* the low 8 bits of info are type */
                /* the high 24 bits are symbol table index */
                reloc64.r_info = ELF64_R_INFO( fixup->sym->idx, elftype );
                if ( fwrite( &reloc64, 1, sizeof( reloc64 ), FileInfo.file[OBJ] ) != sizeof(reloc64) )
                    WriteError();
                } else {
#endif
                reloc32.r_offset = fixup->location;
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
#if GNURELOCS
                case FIX_OFF16:
                    extused = TRUE;
                    elftype = R_386_16;
                    break;
                case FIX_RELOFF16:
                    extused = TRUE;
                    elftype = R_386_PC16;
                    break;
                case FIX_LOBYTE:
                    extused = TRUE;
                    elftype = R_386_8;
                    break;
                case FIX_RELOFF8:
                    extused = TRUE;
                    elftype = R_386_PC8;
                    break;
#endif
                default:
                    elftype = R_386_NONE;
                    if (fixup->type == FIX_SEG ||
#if GNURELOCS==0
                        fixup->type == FIX_LOBYTE ||
                        fixup->type == FIX_RELOFF8 ||
                        fixup->type == FIX_RELOFF16 ||
                        fixup->type == FIX_OFF16 ||
#endif
                        fixup->type == FIX_HIBYTE ||
                        fixup->type == FIX_PTR16 ||
                        fixup->type == FIX_PTR32) {
                        AsmErr( INVALID_FIXUP_TYPE, "ELF", fixup->type, curr->sym.name, fixup->location );
                    } else
                        AsmErr( UNKNOWN_FIXUP_TYPE, fixup->type, fixup->location );
                }
                /* the low 8 bits of info are type */
                /* the high 24 bits are symbol table index */
                reloc32.r_info = ELF32_R_INFO( fixup->sym->idx, elftype );
                if ( fwrite( &reloc32, 1, sizeof(reloc32), FileInfo.file[OBJ] ) != sizeof(reloc32) )
                    WriteError();
#if AMD64_SUPPORT
                }
#endif
            }
        }
    }
#if GNURELOCS
    if ( extused ) {
        AsmWarn( 2, ELF_GNU_EXTENSIONS_USED );
    }
#endif

    DebugMsg(("elf_write_data: exit\n"));

    return( NOT_ERROR );
}

/* format-specific init.
 * called once per module.
 */

void elf_init( module_info * ModuleInfo )
{
    ModuleInfo->osabi = ELFOSABI_LINUX;
    return;
}

#endif
