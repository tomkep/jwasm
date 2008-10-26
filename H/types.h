
/* prototypes of TYPES.C */

/* global STRUCT status */

typedef struct struct_state {
    unsigned            struct_depth;   // nesting level
    stacknode           *struct_stack;  // stack of nested structs
    dir_node            *curr_struct;
} struct_state;

extern struct_state StructDef;

extern asm_sym   *SearchNameInStruct(asm_sym *sym, const char *name, unsigned int *poffset );
extern ret_code  StructDirective( int );
extern ret_code  RecordDef( int );
extern ret_code  TypeDef( int );         // define a simple type
extern asm_sym   *CreateTypeDef( char *, int *);
extern ret_code  InitializeStructure( asm_sym *, asm_sym *, char *, char );
extern asm_sym   *AddFieldToStruct( int , int, memtype, struct asm_sym *, int);
extern void      UpdateStructSize( int );
extern ret_code  SetStructCurrentOffset( int );
extern ret_code  AlignInStruct( int );
extern void      TypesInit( void );

