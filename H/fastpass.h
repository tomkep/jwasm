
/* this is Public Domain.
 fastpass.h defines structures and externals which are needed by the
 "fast pass" feature. This feature speeds JWasm's assembly significantly
 if huge header files containing declarations and definitions are used
 (as it is the case with Win32Inc and Masm32), since the header files are
 then scanned in pass one only.
 */

#if FASTPASS

/* equ_item: used for a linked list of assembly time variables. Any variable
 which is defined or modified after SaveState() has been called is to be stored
 here - once! */

typedef struct equ_item {
    struct equ_item *next;
    asm_sym *sym;
    int value;
    bool defined;
} equ_item;

/* line_item: used for a linked list of preprocessed lines. After SaveState()
 has been called, all preprocessed lines are written in pass one and read
 in further passes */

typedef struct line_item {
    struct line_item *next;
    uint_32 lineno:20, srcfile:12;
    uint_32 list_pos; /* position .LST file */
    char line[];
} line_item;

extern line_item *LineStoreCurr;

/* mod_state: used to store the module state within SaveState()
 */

typedef struct mod_state {
    bool init;           /* is this struct initialized? */
    equ_item *EquHead;   /* the list of modified assembly time variables */
    equ_item *EquTail;
    module_info modinfo;
} mod_state;

/* source lines start to be "stored" when one of the following is detected:
 * - an instruction
 * - a data item (but not a struct field)
 * - directives which "generate source": PROC, INVOKE, .IF, .WHILE, .REPEAT
 * - directives ALIGN and ORG (which emit bytes and/or change $)
 * - directive END (to ensure that there is at least 1 line)
 * - directive ASSUME if operand is a forward reference
 */

extern mod_state modstate;
extern bool StoreState; /* is 1 if states are to be stored in pass one */
extern bool UseSavedState; /* is 1 if preprocessed lines are to be used */

void SaveState( void );
void SegmentSaveState( void );
void AssumeSaveState( void );
void ContextSaveState( void );
void StoreLine( char * );
void SkipSavedState( void );

#endif

