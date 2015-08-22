
/* COFF structures and definitions
 Public Domain.
 */

#define IMAGE_FILE_MACHINE_I386		0x014c /* Intel 386 or later processors */

typedef struct _IMAGE_FILE_HEADER {
	uint_16 Machine;
	uint_16 NumberOfSections;
	uint_32 TimeDateStamp;
	uint_32 PointerToSymbolTable;
	uint_32 NumberOfSymbols;
	uint_16 SizeOfOptionalHeader;
	uint_16 Characteristics;
} IMAGE_FILE_HEADER;

#define IMAGE_SIZEOF_SHORT_NAME 8

typedef struct _IMAGE_SECTION_HEADER {
	uint_8 Name[IMAGE_SIZEOF_SHORT_NAME];
	union {
		uint_32 PhysicalAddress;
		uint_32 VirtualSize;
	} Misc;
	uint_32 VirtualAddress;
	uint_32 SizeOfRawData;
	uint_32 PointerToRawData;
	uint_32 PointerToRelocations;
	uint_32 PointerToLinenumbers;
	uint_16 NumberOfRelocations;
	uint_16 NumberOfLinenumbers;
	uint_32 Characteristics;
} IMAGE_SECTION_HEADER;

#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES 16

#define IMAGE_SCN_TYPE_REG    0
#define IMAGE_SCN_TYPE_DSECT  1
#define IMAGE_SCN_TYPE_NOLOAD 2
#define IMAGE_SCN_TYPE_GROUP  4
#define IMAGE_SCN_TYPE_NO_PAD 8
#define IMAGE_SCN_TYPE_COPY  16
#define IMAGE_SCN_CNT_CODE               0x00000020
#define IMAGE_SCN_CNT_INITIALIZED_DATA   0x00000040
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA 0x00000080
#define IMAGE_SCN_LNK_OTHER       0x00000100
#define IMAGE_SCN_LNK_INFO        0x00000200
#define IMAGE_SCN_TYPE_OVER       0x00000400
#define IMAGE_SCN_LNK_REMOVE      0x00000800
#define IMAGE_SCN_LNK_COMDAT      0x00001000
#define IMAGE_SCN_GPREL           0x00008000 /*  Valid only for IA64 */
#define IMAGE_SCN_MEM_FARDATA     0x00008000 /* Not in PECOFF v8 spec */
#define IMAGE_SCN_MEM_PURGEABLE   0x00020000
#define IMAGE_SCN_MEM_16BIT       0x00020000
#define IMAGE_SCN_MEM_LOCKED      0x00040000
#define IMAGE_SCN_MEM_PRELOAD     0x00080000
#define IMAGE_SCN_ALIGN_1BYTES    0x00100000
#define IMAGE_SCN_ALIGN_2BYTES    0x00200000
#define IMAGE_SCN_ALIGN_4BYTES    0x00300000
#define IMAGE_SCN_ALIGN_8BYTES    0x00400000
#define IMAGE_SCN_ALIGN_16BYTES   0x00500000
#define IMAGE_SCN_ALIGN_32BYTES   0x00600000
#define IMAGE_SCN_ALIGN_64BYTES   0x00700000
#define IMAGE_SCN_ALIGN_128BYTES  0x00800000
#define IMAGE_SCN_ALIGN_256BYTES  0x00900000
#define IMAGE_SCN_ALIGN_512BYTES  0x00a00000
#define IMAGE_SCN_ALIGN_1024BYTES 0x00b00000
#define IMAGE_SCN_ALIGN_2048BYTES 0x00c00000
#define IMAGE_SCN_ALIGN_4096BYTES 0x00d00000
#define IMAGE_SCN_ALIGN_8192BYTES 0x00e00000
#define IMAGE_SCN_LNK_NRELOC_OVFL 0x01000000
#define IMAGE_SCN_MEM_DISCARDABLE 0x02000000
#define IMAGE_SCN_MEM_NOT_CACHED  0x04000000
#define IMAGE_SCN_MEM_NOT_PAGED   0x08000000
#define IMAGE_SCN_MEM_SHARED      0x10000000
#define IMAGE_SCN_MEM_EXECUTE     0x20000000
#define IMAGE_SCN_MEM_READ        0x40000000
#define IMAGE_SCN_MEM_WRITE       0x80000000

#pragma pack(push,2)
typedef struct _IMAGE_RELOCATION {
	union {
		uint_32 VirtualAddress;
		uint_32 RelocCount;
	};
	uint_32 SymbolTableIndex;
	uint_16 Type;
} IMAGE_RELOCATION;
#pragma pack(pop)

#define IMAGE_REL_I386_ABSOLUTE	0x0000
#define IMAGE_REL_I386_DIR16	0x0001
#define IMAGE_REL_I386_REL16	0x0002
#define IMAGE_REL_I386_DIR32	0x0006
#define IMAGE_REL_I386_DIR32NB	0x0007
#define IMAGE_REL_I386_SEG12	0x0009
#define IMAGE_REL_I386_SECTION	0x000A
#define IMAGE_REL_I386_SECREL	0x000B
#define IMAGE_REL_I386_TOKEN	0x000C
#define IMAGE_REL_I386_SECREL7	0x000D
#define IMAGE_REL_I386_REL32	0x0014

#define IMAGE_SIZEOF_SYMBOL 18
#define IMAGE_SIZEOF_AUX_SYMBOL 18

#pragma pack(push,2)
typedef struct _IMAGE_SYMBOL {
	union {
		uint_8 ShortName[8];
		struct {
			uint_32 Short;
			uint_32 Long;
		} Name;
		unsigned char *LongName[2];
	} N;
	uint_32 Value;
	uint_16 SectionNumber;
	uint_16 Type;
	uint_8  StorageClass;
	uint_8  NumberOfAuxSymbols;
} IMAGE_SYMBOL;

/* special section numbers */

#define IMAGE_SYM_UNDEFINED  0
#define IMAGE_SYM_ABSOLUTE (-1)
#define IMAGE_SYM_DEBUG	   (-2)

#define IMAGE_SYM_TYPE_NULL   0
#define IMAGE_SYM_TYPE_VOID   1
#define IMAGE_SYM_TYPE_CHAR   2
#define IMAGE_SYM_TYPE_SHORT  3
#define IMAGE_SYM_TYPE_INT    4
#define IMAGE_SYM_TYPE_LONG   5
#define IMAGE_SYM_TYPE_FLOAT  6
#define IMAGE_SYM_TYPE_DOUBLE 7
#define IMAGE_SYM_TYPE_STRUCT 8
#define IMAGE_SYM_TYPE_UNION  9
#define IMAGE_SYM_TYPE_ENUM  10
#define IMAGE_SYM_TYPE_MOE   11
#define IMAGE_SYM_TYPE_BYTE  12
#define IMAGE_SYM_TYPE_WORD  13
#define IMAGE_SYM_TYPE_UINT  14
#define IMAGE_SYM_TYPE_DWORD 15
#define IMAGE_SYM_TYPE_PCODE 32768

#define IMAGE_SYM_DTYPE_NULL     0
#define IMAGE_SYM_DTYPE_POINTER  1
#define IMAGE_SYM_DTYPE_FUNCTION 2
#define IMAGE_SYM_DTYPE_ARRAY    3

/* StorageClass values */

#define IMAGE_SYM_CLASS_END_OF_FUNCTION	(-1)
#define IMAGE_SYM_CLASS_NULL              0
#define IMAGE_SYM_CLASS_AUTOMATIC         1
#define IMAGE_SYM_CLASS_EXTERNAL          2
#define IMAGE_SYM_CLASS_STATIC            3
#define IMAGE_SYM_CLASS_REGISTER          4
#define IMAGE_SYM_CLASS_EXTERNAL_DEF      5
#define IMAGE_SYM_CLASS_LABEL             6
#define IMAGE_SYM_CLASS_UNDEFINED_LABEL   7
#define IMAGE_SYM_CLASS_MEMBER_OF_STRUCT  8
#define IMAGE_SYM_CLASS_ARGUMENT          9
#define IMAGE_SYM_CLASS_STRUCT_TAG       10
#define IMAGE_SYM_CLASS_MEMBER_OF_UNION  11
#define IMAGE_SYM_CLASS_UNION_TAG        12
#define IMAGE_SYM_CLASS_TYPE_DEFINITION  13
#define IMAGE_SYM_CLASS_UNDEFINED_STATIC 14
#define IMAGE_SYM_CLASS_ENUM_TAG         15
#define IMAGE_SYM_CLASS_MEMBER_OF_ENUM   16
#define IMAGE_SYM_CLASS_REGISTER_PARAM   17
#define IMAGE_SYM_CLASS_BIT_FIELD        18
#define IMAGE_SYM_CLASS_FAR_EXTERNAL     68 /* Not in PECOFF v8 spec */
#define IMAGE_SYM_CLASS_BLOCK           100
#define IMAGE_SYM_CLASS_FUNCTION        101
#define IMAGE_SYM_CLASS_END_OF_STRUCT   102
#define IMAGE_SYM_CLASS_FILE            103
#define IMAGE_SYM_CLASS_SECTION         104
#define IMAGE_SYM_CLASS_WEAK_EXTERNAL   105
#define IMAGE_SYM_CLASS_CLR_TOKEN       107

#define IMAGE_WEAK_EXTERN_SEARCH_NOLIBRARY  1
#define IMAGE_WEAK_EXTERN_SEARCH_LIBRARY    2
#define IMAGE_WEAK_EXTERN_SEARCH_ALIAS      3

typedef union _IMAGE_AUX_SYMBOL {
	struct {
		uint_32 TagIndex;
		union {
			struct {
				uint_16 Linenumber;
				uint_16 Size;
			} LnSz;
			uint_32 TotalSize;
		} Misc;
		union {
			struct {
				uint_32 PointerToLinenumber;
				uint_32 PointerToNextFunction;
			} Function;
			struct {
				uint_16 Dimension[4];
			} Array;
		} FcnAry;
		uint_16 TvIndex;
	} Sym;
	struct {
		uint_8 Name[IMAGE_SIZEOF_SYMBOL];
	} File;
	struct {
		uint_32 Length;
		uint_16 NumberOfRelocations;
		uint_16 NumberOfLinenumbers;
		uint_32 CheckSum;
		uint_16 Number;
		uint_8  Selection;
	} Section;
} IMAGE_AUX_SYMBOL;
#pragma pack(pop)

typedef struct _IMAGE_COFF_SYMBOLS_HEADER {
	uint_32 NumberOfSymbols;
	uint_32 LvaToFirstSymbol;
	uint_32 NumberOfLinenumbers;
	uint_32 LvaToFirstLinenumber;
	uint_32 RvaToFirstByteOfCode;
	uint_32 RvaToLastByteOfCode;
	uint_32 RvaToFirstByteOfData;
	uint_32 RvaToLastByteOfData;
} IMAGE_COFF_SYMBOLS_HEADER;

#pragma pack(push,2)
typedef struct _IMAGE_LINENUMBER {
	union {
		uint_32 SymbolTableIndex;
		uint_32 VirtualAddress;
	} Type;
	uint_16 Linenumber;
} IMAGE_LINENUMBER;
#pragma pack(pop)

int coff_write_header( int fh );
int coff_write_section_table( int fh );
int coff_write_data(int fh);
int coff_write_symbols(int fh );


