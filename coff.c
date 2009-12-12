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
#include "symbols.h"
#include "mangle.h"
#include "memalloc.h"
#include "directiv.h"
#include "fixup.h"
#include "segment.h"
#include "queues.h"
#include "coff.h"
#include "coffspec.h"
#include "fatal.h"
#include "input.h"
#include "myassert.h"

#if COFF_SUPPORT

#define SETDATAPOS   1
#define HELPSYMS     0 /* use helper symbols for assembly time symbol refs */

typedef struct stringitem {
    struct stringitem *next;
    char string[];
} stringitem;

typedef struct stacknode {
    void    *next;
    asm_sym *sym;
} stacknode;

extern void cv_write_debug_tables( dir_node *, dir_node *);

extern symbol_queue     Tables[];       // tables of definitions
extern asm_sym          *start_label;   // symbol for Modend (COFF)
extern uint             segdefidx;      // current segment index
extern stacknode        *SafeSEHStack;

qdesc  DebugS;
qdesc  DebugT;

static uint_32 coff_raw_data;  /* total of section contents (incl relocs + linnums) */
static uint_32 size_drectve;   /* size of .drectve section */
static dir_node *directives;   /* section for linker directives */
static dir_node *sxdata;       /* section for safe exception handler data */
static dir_node *symbols;      /* .debug$S section if -Zi is set */
static dir_node *types;        /* .debug$T section if -Zi is set */

static IMAGE_FILE_HEADER ifh;

static stringitem *LongNamesHead;
static stringitem *LongNamesTail;
static uint_32 SizeLongNames;
static uint_32 sectionstart; /* symbol table index start sections */
static asm_sym *lastproc;    /* used if -Zd is set */
static char *srcname; /* name of source module (name + extension) */

#if SETDATAPOS
static uint_32 data_pos;
#endif

static uint_32 start_files;

static const char szCVSymbols[]  = { ".debug$S"};
static const char szCVTypes[]    = { ".debug$T"};

/* translate section names:
 * _TEXT -> .text
 * _DATA -> .data
 * _BSS  -> .bss
 * CONST -> .rdata
 */

static char * CoffConvertSectionName( asm_sym * sym )
/***************************************************/
{
    static char coffname[MAX_ID_LEN+1];

    if ( memcmp(sym->name, "_TEXT", 5 ) == 0) {
        if ( sym->name[5] == NULLC )
            return( ".text" );
        else if (sym->name[5] == '$') {
            strcpy( coffname, ".text" );
            strcpy( coffname+5, sym->name+5 );
            return( coffname );
        }
    } else if ( memcmp(sym->name, "_DATA", 5 ) == 0) {
        if ( sym->name[5] == NULLC )
            return( ".data" );
        else if (sym->name[5] == '$') {
            strcpy( coffname, ".data" );
            strcpy( coffname+5, sym->name+5 );
            return( coffname );
        }
    } else if ( memcmp(sym->name, "CONST", 5 ) == 0) {
        if ( sym->name[5] == NULLC )
            return( ".rdata" );
        else if ( sym->name[5] == '$' ) {
            strcpy( coffname, ".rdata" );
            strcpy( coffname+6, sym->name+5 );
            return( coffname );
        }
    } else if ( strcmp(sym->name, "_BSS" ) == 0) {
        return( ".bss" );
    }
    return( sym->name );
}

// alloc a string which will be stored in the COFF string table

static uint_32 Coff_AllocString( const char * string, int len )
/*************************************************************/
{
    stringitem *name;
    uint_32 oldsize = SizeLongNames;

    SizeLongNames += len+1;
    name = AsmAlloc(len+1+sizeof( stringitem ));
    name->next = NULL;
    strcpy( name->string, string );
    if (LongNamesHead) {
        LongNamesTail->next = name;
        LongNamesTail = name;
    } else {
        LongNamesHead = LongNamesTail = name;
    }
    return(oldsize);
}

static uint GetLinnumItems( qdesc * q )
/*************************************/
{
    uint i;
    struct line_num_info *curr;
    for ( i = 0, curr = q->head; curr; i++, curr = curr->next );
    return( i );
}

/* write COFF section table */

ret_code coff_write_section_table( module_info *ModuleInfo )
/**********************************************************/
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

    DebugMsg(("coff_write_section_table: enter, sections=%u\n", ModuleInfo->total_segs ));

    offset = sizeof( IMAGE_FILE_HEADER ) + ModuleInfo->total_segs * sizeof( IMAGE_SECTION_HEADER );
    for( seg_index = 1; seg_index <= ModuleInfo->total_segs; seg_index++ ) {
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
        strncpy( buffer, CoffConvertSectionName(&curr->sym), sizeof(buffer) );
        len = strlen(buffer);
        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( ish.Name, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else
            sprintf( ish.Name, "/%u", Coff_AllocString(buffer, len) );

        ish.Misc.PhysicalAddress = offset - (size_relocs + sizeof(IMAGE_FILE_HEADER) + ModuleInfo->total_segs * sizeof(IMAGE_SECTION_HEADER));
        ish.VirtualAddress = 0;
        ish.SizeOfRawData = curr->sym.max_offset;
        if (ish.SizeOfRawData)
            ish.PointerToRawData = offset;
        else
            ish.PointerToRawData = 0;

        ish.Characteristics = 0;

        if ( curr->e.seginfo->alignment != MAX_SEGALIGNMENT ) // ABS not possible
            ish.Characteristics |= (uint_32)(curr->e.seginfo->alignment + 1) << 20;

        if ( curr->e.seginfo->segtype == SEGTYPE_CODE )
            ish.Characteristics |= IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ;
        else if ( curr->e.seginfo->segtype == SEGTYPE_BSS ) {
            ish.Characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
            /* ish.SizeOfRawData = 0; */
            ish.PointerToRawData = 0;
        } else if ( curr->e.seginfo->segrec->d.segdef.combine == COMB_STACK && curr->e.seginfo->bytes_written == 0 ) {
            ish.Characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
            ish.SizeOfRawData = 0;
            ish.PointerToRawData = 0;
        } else if ( curr->e.seginfo->readonly ) {
            ish.Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        } else if (( p = GetLname( curr->e.seginfo->segrec->d.segdef.class_name_idx ) ) && strcmp( p, "CONST" ) == 0) {
            ish.Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        } else if ( strcmp(curr->sym.name, ".drectve") == 0 ) {
            ish.Characteristics |= IMAGE_SCN_LNK_INFO | IMAGE_SCN_LNK_REMOVE;
        } else if ( curr->e.seginfo->characteristics & 1 ) {
            /* INFO, characteristics has been set already */
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

        if ( ish.PointerToRawData )
            offset += ish.SizeOfRawData;

        /* set relocation pos+count in section header */

        ish.PointerToRelocations = 0;
        ish.NumberOfRelocations = 0;
        if ( curr->e.seginfo->FixupListHeadGen ) {
            for ( fix = curr->e.seginfo->FixupListHeadGen; fix ; fix = fix->nextrlc ) {
//                printf("segment %s, reloc=%X\n", curr->sym.name, fix);
                if ( fix->sym == NULL ) {
#if AMD64_SUPPORT
                    if ( fix->type == FIX_RELOFF32 ) {
                        uint_32 *cp = (uint_32 *)( curr->e.seginfo->CodeBuffer + (fix->fixup_loc - curr->e.seginfo->start_loc ));
                        uint_32 src = fix->fixup_loc + fix->addbytes;
                        (*cp) -= src;
                    }
#endif
                    fix->type = FIX_VOID;
                    continue;
                }
                ish.NumberOfRelocations++;
            }
            offset = (offset + 1) & ~1;
            ish.PointerToRelocations = offset;
            size_relocs += ish.NumberOfRelocations * sizeof( IMAGE_RELOCATION );
            offset += ish.NumberOfRelocations * sizeof( IMAGE_RELOCATION );
//            printf("segment %s has %u relocs\n", curr->sym.name, ish.NumberOfRelocations);
        }

        /* set linenumber pos+count in section header */

        if ( curr->e.seginfo->LinnumQueue ) {
            ish.PointerToLinenumbers = offset;
            ish.NumberOfLinenumbers = GetLinnumItems( curr->e.seginfo->LinnumQueue );
            offset += ish.NumberOfLinenumbers * sizeof( IMAGE_LINENUMBER );
        } else {
            ish.PointerToLinenumbers = 0;
            ish.NumberOfLinenumbers = 0;
        }

        DebugMsg(( "coff_write_section_table: %s, Fixups=%u, Linnums=%u\n", curr->sym.name, ish.NumberOfRelocations, ish.NumberOfLinenumbers ));
        if ( fwrite( &ish, 1, sizeof( ish ), FileInfo.file[OBJ] ) != sizeof( ish ) )
            WriteError();
    }
#if SETDATAPOS
    data_pos = ftell( FileInfo.file[OBJ] );
#endif
    DebugMsg(("coff_write_section_table: exit\n"));
    return( NOT_ERROR );
}


static short CoffGetType( const asm_sym * sym )
/*********************************************/
{
    if ( sym->isproc )
        return( 0x20 );

#if 0
    switch (sym->mem_type) {
    case MT_BYTE:
    case MT_SBYTE:
        return( IMAGE_SYM_TYPE_BYTE );
    case MT_WORD:
    case MT_SWORD:
        return( IMAGE_SYM_TYPE_WORD );
    case MT_DWORD:
    case MT_SDWORD:
        return( IMAGE_SYM_TYPE_DWORD );
    }
#endif
    return( IMAGE_SYM_TYPE_NULL );
}

static short CoffGetClass( const asm_sym * sym )
/**********************************************/
{
    if ( sym->state == SYM_EXTERNAL || sym->public == TRUE )
        return( IMAGE_SYM_CLASS_EXTERNAL );
#if HELPSYMS
    else if ( sym->variable == TRUE ) /* assembly time variable in fixup */
        return( IMAGE_SYM_CLASS_LABEL );
#endif
    else if ( sym->mem_type == MT_NEAR )/* added v2.0 */
        return( IMAGE_SYM_CLASS_LABEL );
    return( IMAGE_SYM_CLASS_STATIC );
}

// update the file header once all has been written

static void update_header( FILE *file )
/*************************************/
{
    if ( ifh.NumberOfSymbols )
        ifh.PointerToSymbolTable =
            sizeof(IMAGE_FILE_HEADER) + ModuleInfo.total_segs * sizeof(IMAGE_SECTION_HEADER) + coff_raw_data;
    fseek( file, 0, SEEK_SET);
    if ( fwrite( &ifh, 1, sizeof(ifh), file ) != sizeof(ifh) )
        WriteError();
}

/* calc number of entries in symbol table for a file entry */

static uint GetFileAuxEntries( uint_16 file, char * *fname )
/**********************************************************/
{
    const FNAME *curr;
    uint len;
    curr = GetFName( file );
    if ( fname )
        *fname = curr->name;
    len = strlen( curr->name );
    return ( len / sizeof( IMAGE_AUX_SYMBOL ) + ( len % sizeof( IMAGE_AUX_SYMBOL ) ? 1 : 0 ) );
}

// write COFF symbol table and string table
// contents of the table
// 0+1: file (+aux)
// 2-l: 2 entries per section
// l-m: externals, communals and publics
// m-n: entries for relocations (internal)
// n-o: aliases (weak externals)

ret_code coff_write_symbols( module_info *ModuleInfo )
/****************************************************/
{
    dir_node    *curr;
    asm_sym     *sym;
    void        *vp;
    char        *p;
    uint        len;
    uint        i;
    stringitem  *pName;
    IMAGE_SYMBOL is;
    IMAGE_AUX_SYMBOL ias;
    uint lastfile = 0;
    char        buffer[MAX_ID_LEN+1];

    DebugMsg(("coff_write_symbols: enter\n"));
    ifh.NumberOfSymbols = 0;

    // first entry is the .file (optional)

    if (Options.no_file_entry == FALSE) {
        strncpy( is.N.ShortName, ".file", IMAGE_SIZEOF_SHORT_NAME );
        if ( Options.line_numbers )
            is.Value = start_files;  /* index of next .file entry */
        else
            is.Value = 0;  /* index of next .file entry */
        is.SectionNumber = IMAGE_SYM_DEBUG;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_FILE;

        p = srcname;
        i = strlen(p);
        is.NumberOfAuxSymbols = i / sizeof(IMAGE_AUX_SYMBOL) + (i % sizeof(IMAGE_AUX_SYMBOL) ? 1 : 0);
        if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
            WriteError();

        for (i=is.NumberOfAuxSymbols;i;i--, p += sizeof(IMAGE_AUX_SYMBOL)) {
            strncpy( ias.File.Name, p, sizeof(IMAGE_AUX_SYMBOL) );
            if ( fwrite( &ias, 1, sizeof(ias), FileInfo.file[OBJ] ) != sizeof(ias) )
                WriteError();
        }
        ifh.NumberOfSymbols = is.NumberOfAuxSymbols + 1;
    }

    // second are section entries

    for( i = 1, curr = Tables[TAB_SEG].head; curr; curr = curr->next, i++) {
        p = CoffConvertSectionName(&curr->sym);
        len = strlen(p);
        if (len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, p, IMAGE_SIZEOF_SHORT_NAME );
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

        DebugMsg(("coff_write_symbols(%u, SECT): %s, type=%x, stgcls=%x\n", ifh.NumberOfSymbols, curr->sym.name, is.Type, is.StorageClass ));

        if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;

        if ( Options.no_section_aux_entry == FALSE ) {
            ias.Section.Length = curr->sym.max_offset;
            ias.Section.NumberOfRelocations = curr->e.seginfo->num_relocs;
            if ( Options.line_numbers )
                ias.Section.NumberOfLinenumbers = curr->e.seginfo->num_linnums;
            else
                ias.Section.NumberOfLinenumbers = 0;
            ias.Section.CheckSum = 0;
            ias.Section.Number = 0;
            ias.Section.Selection = 0;
            if ( fwrite( &ias, 1, sizeof(ias), FileInfo.file[OBJ] ) != sizeof(ias) )
                WriteError();
            DebugMsg(("coff_write_symbols(%u, SECT): %s, AUX, relocs=%u, linnums=%u\n", ifh.NumberOfSymbols, curr->sym.name, ias.Section.NumberOfRelocations, ias.Section.NumberOfLinenumbers ));
            ifh.NumberOfSymbols++;
        }
    }

    // third are externals + communals ( + protos [since v2.01] )

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if ( curr->sym.comm == FALSE && curr->sym.weak == TRUE ) {
            DebugMsg(("coff_write_symbols(EXT+COMM): %s skipped, used=%u, comm=%u, weak=%u\n", curr->sym.name, curr->sym.used, curr->sym.comm, curr->sym.weak ));
            continue;
        }
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        DebugMsg(("coff_write_symbols(%u, EXT+COMM): %s, type=%x, stgcls=%x\n", ifh.NumberOfSymbols, curr->sym.name, is.Type, is.StorageClass ));

        /* for COMMUNALs, store their size in the Value field */
        if (curr->sym.comm == TRUE)
            is.Value = curr->sym.total_size;
        else
            is.Value = curr->sym.offset; /* is always 0 */
        is.SectionNumber = 0;
        is.NumberOfAuxSymbols = 0;

        if (len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }
        if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;
    }

#if 0 /* v2.01: PROTOs are now in TAB_EXT */
    // PROTOs which have been "used" and have no matching PROC are also
    // externals.
    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->sym.state != SYM_EXTERNAL )
            continue;
        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        is.Value = curr->sym.offset;
        is.SectionNumber = 0;
        is.NumberOfAuxSymbols = 0;

        DebugMsg(("coff_write_symbols(%u, PROTO): %s, type=%x, stgcls=%x\n", ifh.NumberOfSymbols, curr->sym.name, is.Type, is.StorageClass ));

        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }
        if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;
    }
#endif
    // publics and internal symbols. The internal symbols have
    // been written to the "public" queue inside coff_write_data().

    vp = NULL;
    while ( sym = GetPublicData(&vp) ) {
        Mangle( sym, buffer );
        len = strlen( buffer );
#ifdef DEBUG_OUT
        if ( sym->state == SYM_INTERNAL && sym->isproc == TRUE && Options.line_numbers )
            DebugMsg(("coff_write_symbols(%u): %s, file=%u\n", ifh.NumberOfSymbols, sym->name, sym->debuginfo->file ));
#endif
        if ( Options.line_numbers &&
            sym->isproc &&
            sym->debuginfo->file != lastfile ) {
            lastfile = sym->debuginfo->file;
            strncpy( is.N.ShortName, ".file", IMAGE_SIZEOF_SHORT_NAME );
            is.SectionNumber = IMAGE_SYM_DEBUG;
            is.Type = IMAGE_SYM_TYPE_NULL;
            is.StorageClass = IMAGE_SYM_CLASS_FILE;
            is.NumberOfAuxSymbols = GetFileAuxEntries( sym->debuginfo->file, &p );
            is.Value = sym->debuginfo->next_file;
            if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
                WriteError();

            for ( i = is.NumberOfAuxSymbols; i; i--, p += sizeof(IMAGE_AUX_SYMBOL) ) {
                strncpy( ias.File.Name, p, sizeof(IMAGE_AUX_SYMBOL) );
                if ( fwrite( &ias, 1, sizeof(ias), FileInfo.file[OBJ] ) != sizeof(ias) )
                    WriteError();
            }
            ifh.NumberOfSymbols += is.NumberOfAuxSymbols + 1;
        }
        is.Type = CoffGetType( sym );
        is.StorageClass = CoffGetClass( sym );


        is.Value = sym->offset;
        if ( sym->state == SYM_EXTERNAL )
            is.SectionNumber = 0;
        else if ( sym->mem_type == MT_ABS )
            is.SectionNumber = IMAGE_SYM_ABSOLUTE;
        else if ( sym->segment )
            is.SectionNumber = GetSegIdx( sym->segment );
        else
            is.SectionNumber = 0;

        is.NumberOfAuxSymbols = 0;
        if ( Options.line_numbers && sym->isproc )
            is.NumberOfAuxSymbols++;

        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }

        DebugMsg(("coff_write_symbols(%u, PUB+INT): %s, ofs=%X, type=%X, stgcls=%X\n", ifh.NumberOfSymbols, buffer, is.Value, is.Type, is.StorageClass ));

        if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;
        if ( Options.line_numbers && sym->isproc ) {
            /* write:
             * 1.   the aux for the proc
             * 2+3. a .bf record with 1 aux
             * 4.   a .lf record with 0 aux
             * 5+6. a .ef record with 1 aux
             */
            ias.Sym.TagIndex = ifh.NumberOfSymbols+1;
            ias.Sym.Misc.TotalSize = sym->total_size;
            ias.Sym.FcnAry.Function.PointerToLinenumber = sym->debuginfo->ln_fileofs;
            ias.Sym.FcnAry.Function.PointerToNextFunction = sym->debuginfo->next_proc;
            if ( fwrite( &ias, 1, sizeof(ias), FileInfo.file[OBJ] ) != sizeof(ias) )
                WriteError();

            strncpy( is.N.ShortName, ".bf", IMAGE_SIZEOF_SHORT_NAME );
            is.Type = IMAGE_SYM_TYPE_NULL;
            is.NumberOfAuxSymbols = 1;
            is.StorageClass = IMAGE_SYM_CLASS_FUNCTION;
            if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
                WriteError();
            ias.Sym.TagIndex = 0;
            ias.Sym.Misc.LnSz.Linenumber = sym->debuginfo->start_line;
            if ( sym->debuginfo->next_proc )
                ias.Sym.FcnAry.Function.PointerToNextFunction = sym->debuginfo->next_proc + 2;
            else
                ias.Sym.FcnAry.Function.PointerToNextFunction = 0;
            if ( fwrite( &ias, 1, sizeof(ias), FileInfo.file[OBJ] ) != sizeof(ias) )
                WriteError();

            strncpy( is.N.ShortName, ".lf", IMAGE_SIZEOF_SHORT_NAME );
            is.Type = IMAGE_SYM_TYPE_NULL;
            is.NumberOfAuxSymbols = 0;
            is.StorageClass = IMAGE_SYM_CLASS_FUNCTION;
            is.Value = sym->debuginfo->line_numbers;
            if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
                WriteError();

            strncpy( is.N.ShortName, ".ef", IMAGE_SIZEOF_SHORT_NAME );
            is.Type = IMAGE_SYM_TYPE_NULL;
            is.NumberOfAuxSymbols = 1;
            is.StorageClass = IMAGE_SYM_CLASS_FUNCTION;
            is.Value = sym->offset + sym->total_size;
            if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
                WriteError();
            ias.Sym.TagIndex = 0;
            ias.Sym.Misc.LnSz.Linenumber = sym->debuginfo->end_line;
            if ( fwrite( &ias, 1, sizeof(ias), FileInfo.file[OBJ] ) != sizeof(ias) )
                WriteError();

            ifh.NumberOfSymbols += 6;
        }
    }

    // aliases. A weak external entry with 1 aux entry is created for
    // each alias.

    for( curr = Tables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        asm_sym * sym;

        Mangle( &curr->sym, buffer );
        len = strlen( buffer );

        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString(buffer, len);
        }

        is.Value = 0;
        is.SectionNumber = IMAGE_SYM_UNDEFINED;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_WEAK_EXTERNAL;
        is.NumberOfAuxSymbols = 1;

        DebugMsg(("coff_write_symbols(%u, ALIAS): symbol %s, ofs=%X\n", ifh.NumberOfSymbols, buffer, is.Value ));

        if ( fwrite( &is, 1, sizeof(is), FileInfo.file[OBJ] ) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;

        memset( &ias, 0, sizeof(ias) );

        sym = SymSearch(curr->sym.string_ptr);
        if (sym)
            ias.Sym.TagIndex = sym->idx;

        ias.Sym.Misc.TotalSize = IMAGE_WEAK_EXTERN_SEARCH_ALIAS;
        if ( fwrite( &ias, 1, sizeof(ias), FileInfo.file[OBJ] ) != sizeof(ias) )
            WriteError();
        ifh.NumberOfSymbols++;

    }

    /* the string table is ALWAYS written, even if no strings are defined */

    DebugMsg(("coff_write_symbols(string_table): size=%u\n", SizeLongNames ));
    if ( fwrite( &SizeLongNames, 1, sizeof(SizeLongNames), FileInfo.file[OBJ] ) != sizeof(SizeLongNames) )
        WriteError();
    for (pName = LongNamesHead; pName; pName = pName->next) {
        i = strlen(pName->string)+1;
        if ( fwrite( pName->string, 1, i, FileInfo.file[OBJ] ) != i )
            WriteError();
    }

    /* update the COFF file header */

    update_header( FileInfo.file[OBJ] );

    DebugMsg(("coff_write_symbols: exit\n"));
    return( NOT_ERROR );
}

static int GetStartLabel( char * buffer, bool msg )
/*************************************************/
{
    int size = 0;
    char temp[ MAX_ID_LEN+1 ];

    if ( start_label ) {
        Mangle( start_label, temp );
#if AMD64_SUPPORT
        if ( Options.entry_decorated || Options.header_format == HFORMAT_WIN64 )
#else
        if ( Options.entry_decorated )
#endif
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

/* write COFF file header.
 * This function is called AFTER all assembly passes have been done.
 * ModuleInfo.total_segs has been set already.
 * However, it might be necessary to add "internal" sections:
 * - .drectve section if start label, exports or includelibs are used
 * - .sxdata section if .SAFESEH was used
 * - .debug$S and .debug$T sections if -Zi was set
 */

ret_code coff_write_header( module_info *ModuleInfo )
/***************************************************/
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

    /* if -Zi is set, add .debug$S and .debug$T sections */
    if ( Options.debug_symbols ) {
        if ( symbols = (dir_node *)CreateSegment( szCVSymbols, "", 0, USE32 ) ) {
            symbols->e.seginfo->characteristics = (IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE) >> 24;
            ModuleInfo->total_segs++;
            if ( types = (dir_node *)CreateSegment( szCVTypes, "", 0, USE32 ) ) {
                uint_8 *src;
                uint_8 *dst;
                types->e.seginfo->characteristics = (IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE) >> 24;
                ModuleInfo->total_segs++;
                DebugS.head = NULL;
                DebugT.head = NULL;
                cv_write_debug_tables( symbols, types );
                /* the contents have been written in a queue. now
                 * copy all queue items in ONE buffer
                 */
                if ( symbols->sym.max_offset ) {
                    symbols->e.seginfo->CodeBuffer = AsmAlloc( symbols->sym.max_offset );
                    dst = symbols->e.seginfo->CodeBuffer;
                    for ( src = DebugS.head; src; src = ((struct qditem *)src)->next ) {
                        memcpy( dst, src + sizeof( struct qditem ), ((struct qditem *)src)->size );
                        dst += ((struct qditem *)src)->size;
                    }
                }
                if ( types->sym.max_offset ) {
                    types->e.seginfo->CodeBuffer = AsmAlloc( types->sym.max_offset );
                    dst = types->e.seginfo->CodeBuffer;
                    for ( src = DebugT.head; src; src = ((struct qditem *)src)->next ) {
                        memcpy( dst, src + sizeof( struct qditem ), ((struct qditem *)src)->size );
                        dst += ((struct qditem *)src)->size;
                    }
                }
            }
        }
    }

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
        directives = dir_insert( ".drectve", SYM_SEG );
        if (directives) {
            int size = 0;
            uint_8 *p;
            directives->e.seginfo->segrec->d.segdef.idx = ++segdefidx;
            directives->sym.segment = &directives->sym;
            directives->e.seginfo->segtype = SEGTYPE_UNDEF;
            directives->e.seginfo->alignment = MAX_SEGALIGNMENT;
            directives->e.seginfo->characteristics = 1; /* INFO */
            ModuleInfo->total_segs++;

            /* calc the size for this segment */
            /* 1. exports */
            for( ; dir ; dir = dir->next ) {
                if( dir->e.procinfo->export ) {
                    Mangle( &dir->sym, buffer );
                    size += strlen( buffer ) + sizeof(" -export:");
                    if ( Options.no_export_decoration == TRUE )
                        size += dir->sym.name_size + 1;
                }
            }
            for( dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
                size += strlen( dir->sym.name ) + sizeof(" -defaultlib:");
                /* if the name isn't enclosed in double quotes and contains
                 a space, add 2 bytes to enclose it */
                if (*(dir->sym.name) != '"' && strchr(dir->sym.name, ' '))
                    size += 2;
            }
            size += GetStartLabel(buffer, TRUE);
            size++;
            directives->sym.max_offset = size;
            directives->e.seginfo->CodeBuffer = AsmAlloc(size);

            p = directives->e.seginfo->CodeBuffer;

            /* copy the data */
            size = 0;

            /* 1. exports */
            for( dir = Tables[TAB_PROC].head; dir ; dir = dir->next ) {
                if( dir->e.procinfo->export ) {
                    Mangle(&dir->sym, buffer);
                    if ( Options.no_export_decoration == FALSE )
                        size = sprintf( (char *)p, "-export:%s ", buffer);
                    else
                        size = sprintf( (char *)p, "-export:%s=%s ", dir->sym.name, buffer);
                    p += size;
                }
            }
            for( dir = Tables[TAB_LIB].head; dir ; dir = dir->next ) {
                if (*dir->sym.name != '"' && strchr(dir->sym.name, ' '))
                    size = sprintf( (char *)p,"-defaultlib:\"%s\" ", dir->sym.name);
                else
                    size = sprintf( (char *)p,"-defaultlib:%s ", dir->sym.name);
                p += size;
            }
            if ( start_label) {
                GetStartLabel(buffer, FALSE);
                size = sprintf( (char *)p, "-entry:%s ", buffer );
                p += size;
            }
            size_drectve = p - directives->e.seginfo->CodeBuffer;
        }
    }

    if (directives)
        directives->sym.max_offset = size_drectve;

    /* if safeSEH procs are defined, add a .sxdata section
     */
    if ( SafeSEHStack ) {
        stacknode *sehp;
        unsigned cnt = 0;
        sxdata = dir_insert( ".sxdata", SYM_SEG );
        sxdata->e.seginfo->segrec->d.segdef.idx = ++segdefidx;
        sxdata->e.seginfo->alignment = MAX_SEGALIGNMENT;
        sxdata->sym.segment = &sxdata->sym;
        sxdata->e.seginfo->segtype = SEGTYPE_UNDEF;
        sxdata->e.seginfo->characteristics = 1; /* INFO */
        ModuleInfo->total_segs++;
        /* calc the size for this segment */
        for( sehp = SafeSEHStack; sehp ; sehp = sehp->next, cnt++ );
        sxdata->sym.max_offset = cnt*4;
        sxdata->e.seginfo->CodeBuffer = AsmAlloc(cnt*4);
    }

#if AMD64_SUPPORT
    if ( Options.header_format == HFORMAT_WIN64 )
        ifh.Machine = IMAGE_FILE_MACHINE_AMD64;
    else
#endif
        ifh.Machine = IMAGE_FILE_MACHINE_I386;
    ifh.NumberOfSections = ModuleInfo->total_segs;
#ifdef __UNIX__
    time((long *)&ifh.TimeDateStamp);
#else
    time((time_t *)&ifh.TimeDateStamp);
#endif
    ifh.PointerToSymbolTable = 0;
    ifh.NumberOfSymbols = 0;
    ifh.SizeOfOptionalHeader = 0;
    ifh.Characteristics = 0;

    fseek( FileInfo.file[OBJ], 0, SEEK_SET );
    if ( fwrite( &ifh, 1, sizeof(ifh), FileInfo.file[OBJ] ) != sizeof(ifh) ) {
        DebugMsg(("coff_write_header: error writing file header\n"));
        WriteError();
    }

    DebugMsg(("coff_write_header: exit\n"));
    return( NOT_ERROR );
}

// calc the current number of entries in the symbol table
// so we know the index if a new entry has to be added
// called by coff_write_data()

static uint_32 SetSymbolIndices( void )
/*************************************/
{
    void * vp;
    dir_node * curr;
    asm_sym *sym;
    uint_32 index;
    uint_32 i;
    asm_sym *lastfproc;
    uint lastfile = 0;

    /* count AUX entries for .file. Depends on sizeof filename */

    sectionstart = 0;
    index = 0;
    lastproc = NULL;

    if ( Options.no_file_entry == FALSE ) {
        i = strlen(srcname);
        sectionstart = i / sizeof(IMAGE_AUX_SYMBOL) + 1;
        if (i % sizeof(IMAGE_AUX_SYMBOL))
            sectionstart++;
        index = sectionstart;
    }

    /* add entries for sections */

    index += ModuleInfo.total_segs;
    if (Options.no_section_aux_entry == FALSE)
        index += ModuleInfo.total_segs;

    /* count externals and protos */
    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        if (curr->sym.comm == FALSE && curr->sym.weak == TRUE )
            continue;
        curr->sym.idx = index++;
    }
#if 0
    /* v2.01: PROTOs are now in TAB_EXT */
    for( curr = Tables[TAB_PROC].head ; curr != NULL ;curr = curr->next ) {
        if( curr->sym.used == FALSE || curr->sym.isproc == TRUE )
            continue;
        curr->sym.idx = index++;
    }
#endif
    /* count publics */
    vp = NULL;
    while( sym = GetPublicData( &vp ) ) {
        if( sym->state == SYM_UNDEFINED ) {
            continue;
        } else if ( sym->state == SYM_EXTERNAL && sym->isproc == TRUE ) {
            continue;
        } else if ( sym->state == SYM_EXTERNAL && sym->weak == TRUE ) {
            continue;
        }
        /* if line numbers are on, co, add 6 entries for procs */
        if ( Options.line_numbers && sym->isproc ) {
            if (  sym->debuginfo->file != lastfile ) {
                if ( start_files == 0 )
                    start_files = index;
                else
                    lastfproc->debuginfo->next_file = index;
                lastfproc = sym;
                index += 1 + GetFileAuxEntries( sym->debuginfo->file, NULL );
                lastfile = sym->debuginfo->file;
            }
            sym->idx = index++;
            index += 6;
        } else
            sym->idx = index++;
    }
    return( index );
}

// write section contents and fixups
// this is done after the last step only!

ret_code coff_write_data( module_info *ModuleInfo )
/*************************************************/
{
    dir_node *section;
    struct asmfixup *fix;
    uint_32 offset = 0;
    IMAGE_RELOCATION ir;
    int i;
    uint_32 index;

    DebugMsg(("coff_write_data(%s): enter\n", ModuleInfo->name ));

    if ( directives )
        directives->sym.max_offset = size_drectve;

    /* calc the current index for the COFF symbol table */
    index = SetSymbolIndices();

    /* fill the SafeSEH array */
    if ( SafeSEHStack ) {
        stacknode *sehp;
        stacknode *sehp2;
        uint_32 *pdw;
        for( sehp = SafeSEHStack, pdw = (uint_32 *)sxdata->e.seginfo->CodeBuffer; sehp ; sehp = sehp2 ) {
            sehp2 = sehp->next;
            *pdw++ = sehp->sym->idx;
            AsmFree( sehp );
        }
    }

#if SETDATAPOS
    fseek( FileInfo.file[OBJ], data_pos, SEEK_SET );
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
        uint_32 size;
        if( section->sym.state != SYM_SEG )
            continue;
        size = section->sym.max_offset;
#ifdef DEBUG_OUT
        if ( section->e.seginfo->CodeBuffer )
            DebugMsg(("coff_write_data(%s, %Xh): size=%X, written=%X, content=[%02X %02X ...]\n",
                      section->sym.name, offset, size - section->e.seginfo->start_loc, section->e.seginfo->bytes_written, *(section->e.seginfo->CodeBuffer), *(section->e.seginfo->CodeBuffer+1)));
        else
            DebugMsg(("coff_write_data(%s, %Xh): size=%X, buffer=NULL\n",
                      section->sym.name, offset, size - section->e.seginfo->start_loc ));
#endif
#if 1
        if ( section->e.seginfo->segrec->d.segdef.combine == COMB_STACK && section->e.seginfo->bytes_written == 0 )
            continue;
        if ( section->e.seginfo->segtype == SEGTYPE_BSS)
            continue;
#endif
        if (size) {
            offset += size;
            if ((offset & 1) && section->e.seginfo->FixupListHeadGen ) {
                offset++;
                size++;
            }
            if ( section->e.seginfo->CodeBuffer == NULL ) {
                fseek( FileInfo.file[OBJ], size, SEEK_CUR );
            } else {
                /* if there was an ORG, the buffer content will
                 * start with the ORG address. The bytes from
                 * 0 - ORG must be written by moving the file pointer!
                 */
                if ( section->e.seginfo->start_loc ) {
                    fseek( FileInfo.file[OBJ], section->e.seginfo->start_loc, SEEK_CUR );
                    size -= section->e.seginfo->start_loc;
                }

                if ( fwrite( section->e.seginfo->CodeBuffer, 1, size, FileInfo.file[OBJ] ) != size )
                    WriteError();
            }
            for ( fix = section->e.seginfo->FixupListHeadGen; fix ; fix = fix->nextrlc ) {
#if AMD64_SUPPORT
                if ( Options.header_format == HFORMAT_WIN64 ) {
                    switch ( fix->type ) {
                    case FIX_VOID:
                        continue;
                    case FIX_RELOFF32: /* 32bit offset */
                        ir.Type = IMAGE_REL_AMD64_REL32 + (fix->addbytes - 4);
                        break;
                    case FIX_OFF32: /* 32bit offset */
                        ir.Type = IMAGE_REL_AMD64_ADDR32;
                        break;
#if IMAGERELSUPP
                    case FIX_OFF32_IMGREL:
                        ir.Type = IMAGE_REL_AMD64_ADDR32NB;
                        break;
#endif
#if SECTIONRELSUPP
                    case FIX_OFF32_SECREL:
                        ir.Type = IMAGE_REL_AMD64_SECREL;
                        break;
#endif
                    case FIX_OFF64: /* 64bit offset */
                        ir.Type = IMAGE_REL_AMD64_ADDR64;
                        break;
                    case FIX_SEG: /* segment fixup */
                        ir.Type = IMAGE_REL_AMD64_SECTION; /* ??? */
                        break;
                    //case FIX_???: /* absolute fixup */
                    //  ir.Type = IMAGE_REL_I386_ABSOLUTE; /* ??? */
                    //  break;
                    case FIX_LOBYTE:
                    case FIX_HIBYTE:
                    case FIX_RELOFF8:
                    case FIX_RELOFF16:
                    case FIX_OFF16:
                    case FIX_PTR16: /* 16bit far pointer */
                    case FIX_PTR32: /* 32bit far pointer */
                        /* not supported by COFF64! shouldn't reach this point */
                    default:
                        AsmErr( UNKNOWN_FIXUP_TYPE, fix->type );
                        break;
                    }
                } else
#endif
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
#if SECTIONRELSUPP
                case FIX_OFF32_SECREL:
                    ir.Type = IMAGE_REL_I386_SECREL;
                    break;
#endif
                case FIX_SEG: /* segment fixup */
                    ir.Type = IMAGE_REL_I386_SECTION; /* ??? */
                    break;
                case FIX_LOBYTE:
                case FIX_HIBYTE:
                case FIX_RELOFF8:/* shouldn't happen */
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
                } else if (( fix->sym->state == SYM_INTERNAL ) &&
                    fix->sym->included == FALSE &&
                    fix->sym->public == FALSE) {
                    fix->sym->included = TRUE;
                    AddPublicData( fix->sym );
                    DebugMsg(("coff_write_data(%s, %Xh): %s added to symbol table, idx=%u\n",
                              section->sym.name, offset, fix->sym->name, index ));
                    fix->sym->idx = index++;
                    if ( Options.line_numbers && fix->sym->isproc )
                        index += 6;
                }
                ir.VirtualAddress = fix->fixup_loc;
                ir.SymbolTableIndex = fix->sym->idx;
                if ( fwrite( &ir, 1, sizeof(ir), FileInfo.file[OBJ] ) != sizeof(ir) )
                    WriteError();
                DebugMsg(("coff_write_data(%s, %Xh): reloc loc=%X type=%u idx=%u sym=%s\n",
                          section->sym.name, offset, ir.VirtualAddress, ir.Type, ir.SymbolTableIndex, fix->sym->name));
                offset += sizeof(ir);
                section->e.seginfo->num_relocs++;
            }
            /* write line number data. The peculiarity of COFF (compared to OMF) is
             * that line numbers are always relative to a function start.
             */
            if( Options.line_numbers && section->e.seginfo->LinnumQueue ) {
                IMAGE_LINENUMBER il;
                line_num_info *lni;
                asm_sym *last;
                uint_32 line_numbers = 0;
                last = NULL;
                lni = (struct line_num_info *)((qdesc *)section->e.seginfo->LinnumQueue)->head;
                DebugMsg(("coff_write_data(%s): writing linnum data\n", section->sym.name ));
                while ( lni ) {
                    DebugMsg(("coff_write_data(%s, %Xh): linnum, #=%u, ofs=%X, sym=%s\n",
                              section->sym.name, offset, lni->number, lni->offset, lni->number ? "NULL" : lni->sym->name ));
                    if ( lni->number == 0 ) {
                        last = lni->sym;
                        if ( lastproc )
                             lastproc->debuginfo->next_proc = lni->sym->idx;
                        lastproc = lni->sym;
                        lni->sym->debuginfo->next_proc = 0;
                        il.Linenumber = 0;
#ifdef DEBUG_OUT
                        /* if symbol table index is 0, then the proc wasn't added
                         * to the "publics" queue in AddLinnumDataRef().
                         */
                        if ( lni->sym->idx == 0 ) {
                            DebugMsg(("coff_write_data(%s, %Xh): error, %s has symbol table index 0\n",
                                      section->sym.name, offset, lni->sym->name ));
                        }
#endif
                        il.Type.SymbolTableIndex = lni->sym->idx;
                        lni->sym->debuginfo->start_line = lni->line_number;
                        //((dir_node *)lni->sym)->e.procinfo->file = lni->file;
                        lni->sym->debuginfo->ln_fileofs = data_pos + offset;
                    } else {
                        myassert( last != NULL );
                        il.Linenumber = lni->number - last->debuginfo->start_line;
                        if ( il.Linenumber == 0) /* avoid # 0 */
                            il.Linenumber = 0x7FFF;
                        il.Type.VirtualAddress = lni->offset;
                    }
                    if ( last ) {
                        last->debuginfo->line_numbers++;
                        last->debuginfo->end_line = lni->number;
                    }
                    if ( fwrite( &il, 1, sizeof(il), FileInfo.file[OBJ] ) != sizeof(il) )
                        WriteError();
                    offset += sizeof(il);
                    line_numbers++;
                    lni = lni->next;
                } /* end while */
                section->e.seginfo->num_linnums = line_numbers;
            }
        }
    } /* end for */

    coff_raw_data = offset;
    DebugMsg(("coff_write_data: exit, size of sections=%Xh\n", coff_raw_data ));

    return( NOT_ERROR );
}

#endif
