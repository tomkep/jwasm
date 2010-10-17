
/* prototypes of TYPES.C */

#ifndef _TYPES_H_INCLUDED
#define _TYPES_H_INCLUDED_

extern dir_node  *CurrStruct; /* start of current STRUCT list */

extern asm_sym   *CreateTypeSymbol( struct asm_sym *, const char *, bool );
extern asm_sym   *SearchNameInStruct(asm_sym *sym, const char *name, unsigned int *poffset, int level );
extern ret_code  StructDirective( int );
extern ret_code  EndstructDirective( int );
extern ret_code  TypedefDirective( int );
extern ret_code  RecordDef( int );
extern asm_sym   *CreateTypeDef( char *, int *);
extern asm_sym   *AddFieldToStruct( int, int, memtype, dir_node *, int);
extern void      UpdateStructSize( int );
extern ret_code  SetStructCurrentOffset( int );
extern ret_code  AlignInStruct( int );
extern void      TypesInit( void );
extern void      DeleteType( dir_node * );
#endif
