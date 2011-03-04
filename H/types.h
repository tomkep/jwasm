
/* prototypes of TYPES.C */

#ifndef _TYPES_H_INCLUDED
#define _TYPES_H_INCLUDED_

/* qualified_type us used for parsing a qualified type. */
struct qualified_type {
    int             size;
    struct asm_sym  *symtype;
    memtype         mem_type;
    uint_8          is_ptr; /* contains level of indirection */
    uint_8          is_far;
    uint_8          Ofssize;
    memtype         ptr_memtype;
};

extern dir_node  *CurrStruct; /* start of current STRUCT list */

extern asm_sym   *CreateTypeSymbol( struct asm_sym *, const char *, bool );
extern asm_sym   *SearchNameInStruct(asm_sym *sym, const char *name, unsigned int *poffset, int level );
extern ret_code  StructDirective( int );
extern ret_code  EndstructDirective( int );
extern ret_code  TypedefDirective( int );
extern ret_code  RecordDirective( int );
extern ret_code  GetQualifiedType( int *, struct qualified_type * );
extern asm_sym   *AddFieldToStruct( int, int, memtype, dir_node *, int );
extern void      UpdateStructSize( int );
extern ret_code  SetStructCurrentOffset( int );
extern ret_code  AlignInStruct( int );
extern void      TypesInit( void );
extern void      DeleteType( dir_node * );
#endif
