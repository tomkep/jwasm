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
#include "memalloc.h"
#include "symbols.h"
#include "mangle.h"
#include "parser.h"
#include "directiv.h"
#include "fixup.h"
#include "segment.h"
#include "extern.h"
#include "coff.h"
#include "coffspec.h"
#include "fatal.h"
#include "input.h"
#include "myassert.h"
#include "omfspec.h"

#if COFF_SUPPORT

/* the assembler calls the functions in this order:
 * 1. coff_write_header
 * 2. coff_write_section_table
 * 3. coff_write_data
 * 4. coff_write_symbols
 */

#define SETDATAPOS   1
#define HELPSYMS     0 /* use helper symbols for assembly time symbol refs */
#define MANGLE_BYTES 8 /* extra size required for name decoration */
/* v2.04: to make JWasm always add private procs to the symbol table
 * set STATIC_PROCS to 1. Normally those procs are only added if line
 * number/symbolic debugging information is generated.
 */
#define STATIC_PROCS 0
#define COMPID       0 /* 1=add comp.id absolute symbol */

typedef struct stringitem {
    struct stringitem *next;
    char string[];
} stringitem;

extern void cv_write_debug_tables( dir_node *, dir_node *);

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

static const char szdrectve[]    = { ".drectve" };

static const IMAGE_SYMBOL isFeat00 = {
    {"@feat.00"},
     1,
     IMAGE_SYM_ABSOLUTE,
     IMAGE_SYM_TYPE_NULL,
     IMAGE_SYM_CLASS_STATIC,
     0
};
#if COMPID
static const IMAGE_SYMBOL isCompId = {
    {"@comp.id"},
    50727 + ( 0x7d << 16 ), /* value of Masm v8 */
    IMAGE_SYM_ABSOLUTE,
    IMAGE_SYM_TYPE_NULL,
    IMAGE_SYM_CLASS_STATIC,
    0 /* no of aux symbols */
};
#endif

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

static char * CoffConvertSectionName( asm_sym * sym )
/***************************************************/
{
    int i;
    static char coffname[MAX_ID_LEN+1];

#if DJGPP_SUPPORT
    /* DJGPP won't be happy with .rdata segment name */
    if( Options.header_format == HFORMAT_DJGPP && ( strcmp( sym->name, "CONST" ) == 0 ) ) {
        return( ".const" );
    }
#endif
    for ( i = 0; i < sizeof( cst ) / sizeof( cst[0] ); i++ ) {
        if ( memcmp( sym->name, cst[i].src, cst[i].len ) == 0 ) {
            if ( sym->name[cst[i].len] == NULLC )
                return( (char *)cst[i].dst );
            else if ( ( cst[i].flags & CSF_GRPCHK )  && sym->name[cst[i].len] == '$' ) {
                strcpy( coffname, cst[i].dst );
                strcat( coffname, sym->name+cst[i].len );
                return( coffname );
            }
        }
    }
    return( sym->name );
}

/* alloc a string which will be stored in the COFF string table */

static uint_32 Coff_AllocString( const char * string, int len )
/*************************************************************/
{
    stringitem *name;
    uint_32 oldsize = SizeLongNames;

    SizeLongNames += len + 1;
    name = AsmAlloc( len + 1 + sizeof( stringitem ) );
    name->next = NULL;
    strcpy( name->string, string );
    if ( LongNamesHead ) {
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
    struct fixup *fix;
    uint        seg_index;
    uint        offset;
    uint        len;
    IMAGE_SECTION_HEADER ish;
    uint        size_relocs = 0;
    char        buffer[256];

    DebugMsg(("coff_write_section_table: enter, sections=%u\n", ModuleInfo->g.num_segs ));

    offset = sizeof( IMAGE_FILE_HEADER ) + ModuleInfo->g.num_segs * sizeof( IMAGE_SECTION_HEADER );
    for( curr = Tables[TAB_SEG].head; curr; curr = curr->next ) {

        seg_index = GetSegIdx( &curr->sym );
        /* v2.04: can't happen */
        //if( curr->sym.state != SYM_SEG ) {
        //    AsmErr( SEG_NOT_DEFINED, curr->sym.name );
        //    continue;
        //}
        /* if section name is longer than 8 chars, a '/' is stored,
         followed by a number in ascii which is the offset for the string table
         */
        strncpy( buffer, CoffConvertSectionName(&curr->sym), sizeof(buffer) );
        len = strlen( buffer );
        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( ish.Name, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else
            sprintf( ish.Name, "/%u", Coff_AllocString( buffer, len ) );

        /* v2.04: what is the old line supposed to do? */
        //ish.Misc.PhysicalAddress = offset - (size_relocs + sizeof(IMAGE_FILE_HEADER) + ModuleInfo->g.num_segs * sizeof(IMAGE_SECTION_HEADER));
        ish.Misc.PhysicalAddress = 0;

        ish.VirtualAddress = 0;
        ish.SizeOfRawData = curr->sym.max_offset;
        if ( ish.SizeOfRawData )
            ish.PointerToRawData = offset;
        else
            ish.PointerToRawData = 0;

        ish.Characteristics = 0;

        if ( curr->e.seginfo->alignment != MAX_SEGALIGNMENT ) /* ABS not possible */
            ish.Characteristics |= (uint_32)(curr->e.seginfo->alignment + 1) << 20;

        if ( curr->e.seginfo->segtype == SEGTYPE_CODE )
            ish.Characteristics |= IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_MEM_READ;
        else if ( curr->e.seginfo->segtype == SEGTYPE_BSS ) {
            ish.Characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
            /* ish.SizeOfRawData = 0; */
            ish.PointerToRawData = 0;
        } else if ( curr->e.seginfo->combine == COMB_STACK && curr->e.seginfo->bytes_written == 0 ) {
            ish.Characteristics |= IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE;
            ish.SizeOfRawData = 0;
            ish.PointerToRawData = 0;
        } else if ( curr->e.seginfo->readonly ) {
            ish.Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        } else if (( p = GetLname( curr->e.seginfo->class_name_idx ) ) && strcmp( p, "CONST" ) == 0) {
            ish.Characteristics |= IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ;
        } else if ( strcmp( curr->sym.name, szdrectve ) == 0 ) {
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
        if ( curr->e.seginfo->FixupListHead ) {
            for ( fix = curr->e.seginfo->FixupListHead; fix ; fix = fix->nextrlc ) {
//                printf("segment %s, reloc=%X\n", curr->sym.name, fix);
                if ( fix->sym == NULL ) {
#if AMD64_SUPPORT
                    if ( fix->type == FIX_RELOFF32 ) {
                        uint_32 *cp = (uint_32 *)( curr->e.seginfo->CodeBuffer + (fix->location - curr->e.seginfo->start_loc ));
                        uint_32 src = fix->location + fix->addbytes;
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
        if ( fwrite( &ish, 1, sizeof( ish ), AsmFile[OBJ] ) != sizeof( ish ) )
            WriteError();
    }
#if SETDATAPOS
    data_pos = ftell( AsmFile[OBJ] );
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
    if ( sym->state == SYM_EXTERNAL )
        if ( sym->comm == FALSE && sym->altname )
            return( IMAGE_SYM_CLASS_WEAK_EXTERNAL );
        else
            return( IMAGE_SYM_CLASS_EXTERNAL );
    else if ( sym->public == TRUE )
        return( IMAGE_SYM_CLASS_EXTERNAL );
#if HELPSYMS
    else if ( sym->variable == TRUE ) /* assembly time variable in fixup */
        return( IMAGE_SYM_CLASS_LABEL );
#endif
    else if ( sym->mem_type == MT_NEAR )/* added v2.0 */
        return( IMAGE_SYM_CLASS_LABEL );

    return( IMAGE_SYM_CLASS_STATIC );
}

/* update the file header once all has been written */

static void update_header( FILE *file )
/*************************************/
{
    if ( ifh.NumberOfSymbols )
        ifh.PointerToSymbolTable =
            sizeof(IMAGE_FILE_HEADER) + ModuleInfo.g.num_segs * sizeof(IMAGE_SECTION_HEADER) + coff_raw_data;
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

/* write COFF symbol table and string table
 * contents of the table
 * - @@feat.00 if .SAFESEH was set
 * - file (+ 1-n aux)
 * - 2 entries per section
 * - externals, communals and publics
 * - entries for relocations (internal)
 * - aliases (weak externals)
 */
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
    uint        lastfile = 0;
    char        buffer[MAX_ID_LEN + MANGLE_BYTES + 1];

    DebugMsg(("coff_write_symbols: enter\n"));

    ifh.NumberOfSymbols = 0;

#if COMPID
    /* write "@comp.id" entry */
    if ( fwrite( &isCompId, 1, sizeof(IMAGE_SYMBOL), AsmFile[OBJ] ) != sizeof(IMAGE_SYMBOL) )
        WriteError();
    ifh.NumberOfSymbols++;
#endif
    /* "@feat.00" entry (for SafeSEH) */
    if ( Options.safeseh ) {
        if ( fwrite( &isFeat00, 1, sizeof(IMAGE_SYMBOL), AsmFile[OBJ] ) != sizeof(IMAGE_SYMBOL) )
            WriteError();
        ifh.NumberOfSymbols++;
    }

    /* .file entry (optionally disabled by -zlf) */

    if ( Options.no_file_entry == FALSE ) {
        strncpy( is.N.ShortName, ".file", IMAGE_SIZEOF_SHORT_NAME );
        if ( Options.line_numbers )
            is.Value = start_files;  /* index of next .file entry */
        else
            is.Value = 0;  /* index of next .file entry */
        is.SectionNumber = IMAGE_SYM_DEBUG;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_FILE;

        p = srcname;
        i = strlen( p );
        is.NumberOfAuxSymbols = i / sizeof(IMAGE_AUX_SYMBOL) + (i % sizeof(IMAGE_AUX_SYMBOL) ? 1 : 0);
        if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
            WriteError();

        for ( i = is.NumberOfAuxSymbols;i;i--, p += sizeof(IMAGE_AUX_SYMBOL) ) {
            strncpy( ias.File.Name, p, sizeof(IMAGE_AUX_SYMBOL) );
            if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
                WriteError();
        }
        ifh.NumberOfSymbols += is.NumberOfAuxSymbols + 1;
    }

    /* next are section entries */

    for( i = 1, curr = Tables[TAB_SEG].head; curr; curr = curr->next, i++) {
        p = CoffConvertSectionName(&curr->sym);
        len = strlen( p );
        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, p, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString( p, len );
        }
        is.Value = 0;
        is.SectionNumber = i;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_STATIC;
        is.NumberOfAuxSymbols = 0;
        if (Options.no_section_aux_entry == FALSE)
            is.NumberOfAuxSymbols = 1;

        DebugMsg(("coff_write_symbols(%u, SECT): %s, type=%x, stgcls=%x\n", ifh.NumberOfSymbols, curr->sym.name, is.Type, is.StorageClass ));

        if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
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
            if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
                WriteError();
            DebugMsg(("coff_write_symbols(%u, SECT): %s, AUX, relocs=%u, linnums=%u\n", ifh.NumberOfSymbols, curr->sym.name, ias.Section.NumberOfRelocations, ias.Section.NumberOfLinenumbers ));
            ifh.NumberOfSymbols++;
        }
    }

    /* third are externals + communals ( + protos [since v2.01] ) */

    for( curr = Tables[TAB_EXT].head ; curr != NULL ;curr = curr->next ) {
        /* skip "weak" (=unused) externdefs */
        if ( curr->sym.comm == FALSE && curr->sym.weak == TRUE ) {
            DebugMsg(("coff_write_symbols(EXT+COMM): %s skipped, used=%u, comm=%u, weak=%u\n", curr->sym.name, curr->sym.used, curr->sym.comm, curr->sym.weak ));
            continue;
        }
        len = Mangle( &curr->sym, buffer );

        is.Type = CoffGetType(&curr->sym);
        is.StorageClass = CoffGetClass(&curr->sym);

        DebugMsg(("coff_write_symbols(%u, EXT+COMM): %s, type=%x, stgcls=%x\n", ifh.NumberOfSymbols, curr->sym.name, is.Type, is.StorageClass ));

        /* for COMMUNALs, store their size in the Value field */
        if (curr->sym.comm == TRUE)
            is.Value = curr->sym.total_size;
        else
            is.Value = curr->sym.offset; /* is always 0 */
        is.SectionNumber = IMAGE_SYM_UNDEFINED;
        is.NumberOfAuxSymbols = (( curr->sym.comm == FALSE && curr->sym.altname ) ? 1 : 0 );

        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString( buffer, len );
        }
        if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;
        if ( curr->sym.comm == FALSE && curr->sym.altname ) {
            memset( &ias, 0, sizeof(ias) );
            ias.Sym.TagIndex = curr->sym.altname->ext_idx;
            ias.Sym.Misc.TotalSize = IMAGE_WEAK_EXTERN_SEARCH_ALIAS;
            if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
                WriteError();
            ifh.NumberOfSymbols++;
        }
    }

    /* publics and internal symbols. The internal symbols have
     * been written to the "public" queue inside coff_write_data().
     */
    vp = NULL;
    while ( sym = GetPublicData(&vp) ) {
        len = Mangle( sym, buffer );
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
            if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
                WriteError();

            for ( i = is.NumberOfAuxSymbols; i; i--, p += sizeof(IMAGE_AUX_SYMBOL) ) {
                strncpy( ias.File.Name, p, sizeof(IMAGE_AUX_SYMBOL) );
                if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
                    WriteError();
            }
            ifh.NumberOfSymbols += is.NumberOfAuxSymbols + 1;
        }
        is.Type = CoffGetType( sym );
        is.StorageClass = CoffGetClass( sym );


        is.Value = sym->offset;
        if ( sym->state == SYM_EXTERNAL )
            is.SectionNumber = IMAGE_SYM_UNDEFINED;
        else if ( sym->mem_type == MT_ABS )
            is.SectionNumber = IMAGE_SYM_ABSOLUTE;
        else if ( sym->segment )
            is.SectionNumber = GetSegIdx( sym->segment );
        else
            is.SectionNumber = IMAGE_SYM_UNDEFINED;

        is.NumberOfAuxSymbols = 0;
        if ( Options.line_numbers && sym->isproc )
            is.NumberOfAuxSymbols++;

        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString( buffer, len );
        }

        DebugMsg(("coff_write_symbols(%u, PUB+INT): %s, ofs=%X, type=%X, stgcls=%X\n", ifh.NumberOfSymbols, buffer, is.Value, is.Type, is.StorageClass ));

        if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
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
            if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
                WriteError();

            strncpy( is.N.ShortName, ".bf", IMAGE_SIZEOF_SHORT_NAME );
            is.Type = IMAGE_SYM_TYPE_NULL;
            is.NumberOfAuxSymbols = 1;
            is.StorageClass = IMAGE_SYM_CLASS_FUNCTION;
            if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
                WriteError();
            ias.Sym.TagIndex = 0;
            ias.Sym.Misc.LnSz.Linenumber = sym->debuginfo->start_line;
            if ( sym->debuginfo->next_proc )
                ias.Sym.FcnAry.Function.PointerToNextFunction = sym->debuginfo->next_proc + 2;
            else
                ias.Sym.FcnAry.Function.PointerToNextFunction = 0;
            if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
                WriteError();

            strncpy( is.N.ShortName, ".lf", IMAGE_SIZEOF_SHORT_NAME );
            is.Type = IMAGE_SYM_TYPE_NULL;
            is.NumberOfAuxSymbols = 0;
            is.StorageClass = IMAGE_SYM_CLASS_FUNCTION;
            is.Value = sym->debuginfo->line_numbers;
            if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
                WriteError();

            strncpy( is.N.ShortName, ".ef", IMAGE_SIZEOF_SHORT_NAME );
            is.Type = IMAGE_SYM_TYPE_NULL;
            is.NumberOfAuxSymbols = 1;
            is.StorageClass = IMAGE_SYM_CLASS_FUNCTION;
            is.Value = sym->offset + sym->total_size;
            if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
                WriteError();
            ias.Sym.TagIndex = 0;
            ias.Sym.Misc.LnSz.Linenumber = sym->debuginfo->end_line;
            if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
                WriteError();

            ifh.NumberOfSymbols += 6;
        }
    }

    /* aliases. A weak external entry with 1 aux entry is created.
     */
    for( curr = Tables[TAB_ALIAS].head ; curr != NULL ;curr = curr->next ) {
        asm_sym * sym;

        len = Mangle( &curr->sym, buffer );

        if ( len <= IMAGE_SIZEOF_SHORT_NAME )
            strncpy( is.N.ShortName, buffer, IMAGE_SIZEOF_SHORT_NAME );
        else {
            is.N.Name.Short = 0;
            is.N.Name.Long = Coff_AllocString( buffer, len );
        }

        is.Value = 0;
        is.SectionNumber = IMAGE_SYM_UNDEFINED;
        is.Type = IMAGE_SYM_TYPE_NULL;
        is.StorageClass = IMAGE_SYM_CLASS_WEAK_EXTERNAL;
        is.NumberOfAuxSymbols = 1;

        DebugMsg(("coff_write_symbols(%u, ALIAS): symbol %s, ofs=%X\n", ifh.NumberOfSymbols, buffer, is.Value ));

        if ( fwrite( &is, 1, sizeof(is), AsmFile[OBJ] ) != sizeof(is) )
            WriteError();
        ifh.NumberOfSymbols++;

        memset( &ias, 0, sizeof(ias) );

        /* v2.04b: adjusted to new field <substitute> */
        //sym = SymSearch( curr->sym.string_ptr );
        sym = curr->sym.substitute;
        if (sym)
            ias.Sym.TagIndex = sym->ext_idx;

        ias.Sym.Misc.TotalSize = IMAGE_WEAK_EXTERN_SEARCH_ALIAS;
        if ( fwrite( &ias, 1, sizeof(ias), AsmFile[OBJ] ) != sizeof(ias) )
            WriteError();
        ifh.NumberOfSymbols++;

    }

    /* the string table is ALWAYS written, even if no strings are defined */

    DebugMsg(("coff_write_symbols(string_table): size=%u\n", SizeLongNames ));
    if ( fwrite( &SizeLongNames, 1, sizeof(SizeLongNames), AsmFile[OBJ] ) != sizeof(SizeLongNames) )
        WriteError();
    for ( pName = LongNamesHead; pName; pName = pName->next ) {
        i = strlen( pName->string ) + 1;
        if ( fwrite( pName->string, 1, i, AsmFile[OBJ] ) != i )
            WriteError();
    }

    /* update the COFF file header */

    update_header( AsmFile[OBJ] );

    DebugMsg(("coff_write_symbols: exit\n"));
    return( NOT_ERROR );
}

static int GetStartLabel( char * buffer, bool msg )
/*************************************************/
{
    int size = 0;
    char temp[ MAX_ID_LEN + MANGLE_BYTES + 1 ];

    if ( ModuleInfo.start_label ) {
        Mangle( ModuleInfo.start_label, temp );
        if ( Options.entry_decorated )
            strcpy( buffer, temp );
        else {
            if ( ModuleInfo.start_label->langtype != LANG_C &&
                ModuleInfo.start_label->langtype != LANG_STDCALL &&
                ModuleInfo.start_label->langtype != LANG_SYSCALL ) {
                if ( *ModuleInfo.start_label->name != '_' ) {
                    if ( msg && ( Options.fastcall != FCT_WIN64 ) )
                        AsmWarn( 2, LEADING_UNDERSCORE_REQUIRED_FOR_START_LABEL, ModuleInfo.start_label->name );
                    strcpy( buffer, temp );
                } else {
                    strcpy( buffer, temp+1 );
                }
            } else
                strcpy( buffer, temp+1 );
        }
        size = strlen( buffer ) + sizeof(" -entry:");
    }
    return( size );
}

/* write COFF file header.
 * This function is called AFTER all assembly passes have been done.
 * However, it might be necessary to add "internal" sections:
 * - .drectve section if start label, exports or includelibs are used
 * - .sxdata section if .SAFESEH was used
 * - .debug$S and .debug$T sections if -Zi was set
 */

ret_code coff_write_header( module_info *ModuleInfo )
/***************************************************/
{
    dir_node *dir;
    char buffer[MAX_ID_LEN + MANGLE_BYTES + 1];

    DebugMsg(("coff_write_header: enter, sections=%u\n", ModuleInfo->g.num_segs ));

    LongNamesHead = NULL;
    SizeLongNames = sizeof(uint_32);

    srcname = AsmFName[ASM];
    srcname += strlen( srcname );
    while ( srcname > AsmFName[ASM] &&
           *(srcname-1) != '/' &&
           *(srcname-1) != '\\') srcname--;

    /* if -Zi is set, add .debug$S and .debug$T sections */
    if ( Options.debug_symbols ) {
        if ( symbols = (dir_node *)CreateIntSegment( szCVSymbols, "", 0, USE32, TRUE ) ) {
            DebugMsg(("coff_write_header: %s section added\n", szCVSymbols));
            symbols->e.seginfo->characteristics = (IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE) >> 24;
            if ( types = (dir_node *)CreateIntSegment( szCVTypes, "", 0, USE32, TRUE ) ) {
                uint_8 *src;
                uint_8 *dst;
                DebugMsg(("coff_write_header: %s section added\n", szCVTypes));
                types->e.seginfo->characteristics = (IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_DISCARDABLE) >> 24;
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

    /* if safeSEH procs are defined, add a .sxdata section */
    if ( ModuleInfo->g.SafeSEHList.head ) {
        qnode *sehp;
        unsigned cnt = 0;
        if ( sxdata = (dir_node *)CreateIntSegment( ".sxdata", "", MAX_SEGALIGNMENT, ModuleInfo->Ofssize, FALSE ) ) {
            sxdata->e.seginfo->characteristics = 1; /* INFO */
            /* calc the size for this segment */
            for( sehp = ModuleInfo->g.SafeSEHList.head; sehp ; sehp = sehp->next, cnt++ );
            sxdata->sym.max_offset = cnt*4;
            sxdata->e.seginfo->CodeBuffer = AsmAlloc( cnt*4 );
            DebugMsg(("coff_write_header: .sxdata section added, size=%u\n", cnt*4));
        }
    }

    directives = NULL;

    /* does a proc exist with the EXPORT attribute? */
    for( dir = Tables[TAB_PROC].head; dir != NULL; dir = dir->nextproc ) {
        if( dir->e.procinfo->export )
            break;
    }
    /* if a start_label is defined, a library is included or a
     proc is exported, add a .drectve section
     */
    if ( ModuleInfo->start_label != NULL ||
        ModuleInfo->g.LibQueue.head != NULL ||
        dir != NULL ) {
        if ( directives = (dir_node *)CreateIntSegment( szdrectve, "", MAX_SEGALIGNMENT, ModuleInfo->Ofssize, FALSE ) ) {
            int size = 0;
            qnode *q;
            uint_8 *p;
            directives->e.seginfo->characteristics = 1; /* INFO */
            DebugMsg(("coff_write_header: %s section added\n", szdrectve));

            /* calc the size for this segment */
            /* 1. exports */
            for( ; dir ; dir = dir->next ) {
                if( dir->e.procinfo->export ) {
                    size += Mangle( &dir->sym, buffer );
                    size += sizeof(" -export:");
                    if ( Options.no_export_decoration == TRUE )
                        size += dir->sym.name_size + 1;
                }
            }
            for( q = ModuleInfo->g.LibQueue.head; q ; q = q->next ) {
                size += strlen( q->elmt ) + sizeof(" -defaultlib:");
                /* if the name isn't enclosed in double quotes and contains
                 a space, add 2 bytes to enclose it */
                if ( *( (char *)q->elmt ) != '"' && strchr( q->elmt, ' ') )
                    size += 2;
            }
            size += GetStartLabel( buffer, TRUE );
            size++;
            directives->sym.max_offset = size;
            directives->e.seginfo->CodeBuffer = AsmAlloc( size );
            p = directives->e.seginfo->CodeBuffer;

            /* copy the data */
            size = 0;

            /* 1. exports */
            for( dir = Tables[TAB_PROC].head; dir ; dir = dir->nextproc ) {
                if( dir->e.procinfo->export ) {
                    Mangle( &dir->sym, buffer );
                    if ( Options.no_export_decoration == FALSE )
                        size = sprintf( (char *)p, "-export:%s ", buffer );
                    else
                        size = sprintf( (char *)p, "-export:%s=%s ", dir->sym.name, buffer );
                    p += size;
                }
            }
            /* 2. libraries */
            for( q = ModuleInfo->g.LibQueue.head; q ; q = q->next ) {
                if ( *(char *)q->elmt != '"' && strchr( q->elmt, ' ' ) )
                    size = sprintf( (char *)p,"-defaultlib:\"%s\" ", q->elmt );
                else
                    size = sprintf( (char *)p,"-defaultlib:%s ", q->elmt );
                p += size;
            }
            /* 3. entry */
            if ( ModuleInfo->start_label ) {
                GetStartLabel( buffer, FALSE );
                size = sprintf( (char *)p, "-entry:%s ", buffer );
                p += size;
            }
            size_drectve = p - directives->e.seginfo->CodeBuffer;
        }
    }

    if ( directives )
        directives->sym.max_offset = size_drectve;

#if AMD64_SUPPORT
    if ( Options.fastcall == FCT_WIN64 )
        ifh.Machine = IMAGE_FILE_MACHINE_AMD64;
    else
#endif
        ifh.Machine = IMAGE_FILE_MACHINE_I386;
    ifh.NumberOfSections = ModuleInfo->g.num_segs;
#ifdef __UNIX__
    time((long *)&ifh.TimeDateStamp);
#else
    time((time_t *)&ifh.TimeDateStamp);
#endif
    ifh.PointerToSymbolTable = 0;
    ifh.NumberOfSymbols = 0;
    ifh.SizeOfOptionalHeader = 0;
    ifh.Characteristics = 0;

    fseek( AsmFile[OBJ], 0, SEEK_SET );
    if ( fwrite( &ifh, 1, sizeof(ifh), AsmFile[OBJ] ) != sizeof(ifh) ) {
        DebugMsg(("coff_write_header: error writing file header\n"));
        WriteError();
    }

    DebugMsg(("coff_write_header: exit\n"));
    return( NOT_ERROR );
}

/* calc the current number of entries in the symbol table
 * so we know the index if a new entry has to be added
 * called by coff_write_data()
 */
static uint_32 SetSymbolIndices( module_info *ModuleInfo )
/********************************************************/
{
    void * vp;
    dir_node * curr;
    asm_sym *sym;
    uint_32 index;
    uint_32 i;
    asm_sym *lastfproc;
    uint lastfile = 0;

    index = 0;
    lastproc = NULL;

#if COMPID
    index++;
#endif
    /* add absolute symbol @@feat.00 if -SAFESEH is set */
    if ( Options.safeseh )
        index++;

    /* count AUX entries for .file. Depends on sizeof filename */

    if ( Options.no_file_entry == FALSE ) {
        i = strlen( srcname );
        index += i / sizeof( IMAGE_AUX_SYMBOL ) + 1;
        if ( i % sizeof( IMAGE_AUX_SYMBOL ) )
            index++;
    }

    /* add entries for sections */

    sectionstart = index;
    index += ModuleInfo->g.num_segs;
    if ( Options.no_section_aux_entry == FALSE )
        index += ModuleInfo->g.num_segs;

    /* count externals and protos */
    for( curr = Tables[TAB_EXT].head ; curr != NULL ; curr = curr->next ) {
        if ( curr->sym.comm == FALSE && curr->sym.weak == TRUE )
            continue;
        curr->sym.ext_idx = index++;
        if ( curr->sym.comm == FALSE && curr->sym.altname )
            index++;
    }

#if STATIC_PROCS
    /* v2.04: count private procedures (will become static symbols) */
    for( curr = Tables[TAB_PROC].head ; curr != NULL ; curr = curr->nextproc )
        if ( curr->sym.state == SYM_INTERNAL && curr->sym.public == FALSE && curr->sym.included == FALSE ) {
            curr->sym.included = TRUE;
            AddPublicData( &curr->sym );
        }
#endif
    /* count items in public queue */
    vp = NULL;
    while( sym = GetPublicData( &vp ) ) {
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
            sym->ext_idx = index++;
            index += 6;
        } else
            sym->ext_idx = index++;
    }
    return( index );
}

/* write fixups for a section. */

static void coff_write_fixups( dir_node *section, uint_32 *poffset, uint_32 *pindex )
/***********************************************************************************/
{
    uint_32 offset = *poffset;
    uint_32 index = *pindex;
    struct fixup *fix;
    IMAGE_RELOCATION ir;

    for ( fix = section->e.seginfo->FixupListHead; fix ; fix = fix->nextrlc ) {
#if AMD64_SUPPORT
        //if ( Options.header_format == HFORMAT_WIN64 ) {
        if ( section->e.seginfo->Ofssize == USE64 ) {
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
            case FIX_RELOFF8:
            case FIX_RELOFF16:
            case FIX_OFF8:
            case FIX_OFF16:
            case FIX_HIBYTE:
            case FIX_PTR16: /* 16bit far pointer */
            case FIX_PTR32: /* 32bit far pointer */
                /* not supported by COFF64! shouldn't reach this point */
            default:
                AsmErr( UNKNOWN_FIXUP_TYPE, fix->type, section->sym.name, fix->location );
                continue; /* v2.03: skip this fixup */
                //break;
            }
        } else
#endif
            switch ( fix->type ) {
            case FIX_VOID:
                continue;
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
            case FIX_OFF8:
            case FIX_RELOFF8:/* shouldn't happen */
            case FIX_HIBYTE:
            case FIX_PTR16: /* 16bit far pointer */
            case FIX_PTR32: /* 32bit far pointer */
                /* not supported by COFF! shouldn't reach this point */
                // ir.Type = IMAGE_REL_I386_ABSOLUTE; /* ??? */
                // break;
            default:
                AsmErr( UNKNOWN_FIXUP_TYPE, fix->type, section->sym.name, fix->location );
                continue; /* v2.03: skip this fixup */
                //break;
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
             * that the offset is stored inline at the reloc location
             * (patch in fixup.c)
             */
            fix->sym = fix->segment;
#endif
        } else if (( fix->sym->state == SYM_INTERNAL ) &&
                   fix->sym->included == FALSE &&
                   fix->sym->public == FALSE ) {
            fix->sym->included = TRUE;
            AddPublicData( fix->sym );
            DebugMsg(("coff_write_data(%s, %Xh): %s added to symbol table, idx=%u\n",
                      section->sym.name, offset, fix->sym->name, index ));
            fix->sym->ext_idx = index++;
            if ( Options.line_numbers && fix->sym->isproc )
                index += 6;
        }
        ir.VirtualAddress = fix->location;
        ir.SymbolTableIndex = fix->sym->ext_idx;
        if ( fwrite( &ir, 1, sizeof(ir), AsmFile[OBJ] ) != sizeof(ir) )
            WriteError();
        DebugMsg(("coff_write_fixup(%s, %Xh): reloc loc=%X type=%u idx=%u sym=%s\n",
                  section->sym.name, offset, ir.VirtualAddress, ir.Type, ir.SymbolTableIndex, fix->sym->name));
        offset += sizeof( ir );
        section->e.seginfo->num_relocs++;
    } /* end for */
    *poffset = offset;
    *pindex = index;
}

/* write section contents and fixups
 * this is done after the last step only!
 */
ret_code coff_write_data( module_info *ModuleInfo )
/*************************************************/
{
    dir_node *section;
    uint_32 offset = 0;
    int i;
    uint_32 index;

    DebugMsg(("coff_write_data(%s): enter\n", ModuleInfo->name ));

    if ( directives )
        directives->sym.max_offset = size_drectve;

    /* calc the current index for the COFF symbol table */
    index = SetSymbolIndices( ModuleInfo );

    /* fill the SafeSEH array */
    if ( ModuleInfo->g.SafeSEHList.head ) {
        qnode *sehp;
        qnode *sehp2;
        uint_32 *pdw;
        for( sehp = ModuleInfo->g.SafeSEHList.head, pdw = (uint_32 *)sxdata->e.seginfo->CodeBuffer; sehp ; sehp = sehp2 ) {
            sehp2 = sehp->next;
            DebugMsg(("coff_write_data: .sxdata value=%08Xh\n", ((asm_sym *)sehp->elmt)->ext_idx));
            *pdw++ = ((asm_sym *)sehp->elmt)->ext_idx;
            AsmFree( sehp );
        }
    }

#if SETDATAPOS
    fseek( AsmFile[OBJ], data_pos, SEEK_SET );
#endif

#if HELPSYMS==0
    for( i = 0, section = Tables[TAB_SEG].head; section ; i++, section = section->next ) {
        section->sym.ext_idx = sectionstart + i;
        if ( Options.no_section_aux_entry == FALSE )
            section->sym.ext_idx += i;
    }
#endif

    /* now scan the section's relocations. If a relocation refers to
     a symbol which is not public/external, it must be added to the
     symbol table. If the symbol is an assembly time variable, a helper
     symbol - name is $$<offset:6> is to be added.
     */

    for( section = Tables[TAB_SEG].head; section ; section = section->next ) {
        uint_32 size;
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
        if ( section->e.seginfo->combine == COMB_STACK && section->e.seginfo->bytes_written == 0 )
            continue;
        if ( section->e.seginfo->segtype == SEGTYPE_BSS )
            continue;
#endif
        if ( size ) {
            offset += size;
            if ((offset & 1) && section->e.seginfo->FixupListHead ) {
                offset++;
                size++;
            }
            if ( section->e.seginfo->CodeBuffer == NULL ) {
                fseek( AsmFile[OBJ], size, SEEK_CUR );
            } else {
                /* if there was an ORG, the buffer content will
                 * start with the ORG address. The bytes from
                 * 0 - ORG must be written by moving the file pointer!
                 */
                if ( section->e.seginfo->start_loc ) {
                    fseek( AsmFile[OBJ], section->e.seginfo->start_loc, SEEK_CUR );
                    size -= section->e.seginfo->start_loc;
                }

                if ( fwrite( section->e.seginfo->CodeBuffer, 1, size, AsmFile[OBJ] ) != size )
                    WriteError();
            }

            coff_write_fixups( section, &offset, &index );

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
                             lastproc->debuginfo->next_proc = lni->sym->ext_idx;
                        lastproc = lni->sym;
                        lni->sym->debuginfo->next_proc = 0;
                        il.Linenumber = 0;
#ifdef DEBUG_OUT
                        /* if symbol table index is 0, then the proc wasn't added
                         * to the "publics" queue in AddLinnumDataRef().
                         */
                        if ( lni->sym->ext_idx == 0 ) {
                            DebugMsg(("coff_write_data(%s, %Xh): error, %s has symbol table index 0\n",
                                      section->sym.name, offset, lni->sym->name ));
                        }
#endif
                        il.Type.SymbolTableIndex = lni->sym->ext_idx;
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
                    if ( fwrite( &il, 1, sizeof(il), AsmFile[OBJ] ) != sizeof(il) )
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
