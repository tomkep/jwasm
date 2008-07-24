
/* prototypes of TYPES.C */

/* global structure */

typedef struct a_definition_struct {
    unsigned short      struct_depth;
    stacknode           *struct_stack;      // stack of nested structs being defined
    dir_node            *curr_struct;
} a_definition_struct;

extern a_definition_struct      Definition;

extern struct asm_sym  *SearchNameInStruct(asm_sym *sym, const char *name, unsigned int *poffset );
extern int StructDef( int );
extern int RecordDef( int );
extern int TypeDef( int );         // define a simple type
extern asm_sym * CreateTypeDef( char *, int *);
extern char * InitializeStructure( asm_sym *, asm_sym *, char *, bool );
extern struct asm_sym * AddFieldToStruct( int , int, memtype, struct asm_sym *, int);
extern void  UpdateStructSize( int );
extern void TypesInit( void );

