
/* prototypes of TYPES.C */

/* global STRUCT status */

typedef struct struct_state {
    unsigned            struct_depth;   // nesting level
    stacknode           *struct_stack;  // stack of nested structs
    dir_node            *curr_struct;
} struct_state;

extern struct_state StructDef;

extern struct asm_sym  *SearchNameInStruct(asm_sym *sym, const char *name, unsigned int *poffset );
extern int StructDirective( int );
extern int RecordDef( int );
extern int TypeDef( int );         // define a simple type
extern asm_sym * CreateTypeDef( char *, int *);
extern int InitializeStructure( asm_sym *, asm_sym *, char *, char );
extern struct asm_sym * AddFieldToStruct( int , int, memtype, struct asm_sym *, int);
extern void  UpdateStructSize( int );
extern int  SetStructCurrentOffset( int );
extern int  AlignInStruct( int );
extern void TypesInit( void );

