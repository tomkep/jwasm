
#ifndef FASTPASS
#define FASTPASS 0
#endif

#if FASTPASS

/* this is Public Domain
 fastpass.h defines structures and externals which are needed by the
 "fast pass" feature. This feature speeds JWasm's assembly significantly
 if huge header files containing declarations and definitions are used
 (as it is the case with Win32Inc and Masm32), since the header files are
 then scanned in pass one only.
 */

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
    unsigned lineno;
    char line[];
} line_item;

/* mod_state: used to store the module state within SaveState()
 */

typedef struct mod_state {
    bool init;           /* is this struct initialized? */
    equ_item *EquHead;   /* the list of modified assembly time variables */
    equ_item *EquTail;
    module_info modinfo;
} mod_state;

extern mod_state modstate;
extern bool StoreState; /* is 1 if states are to be stored in pass one */

void SaveState( void );
void SegmentSaveState( void );
void AssumeSaveState( void );
void StoreLine( char * );
void ResetUseSavedState( void );

#endif

