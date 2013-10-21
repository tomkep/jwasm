
/* prototypes of functions defined in coff.c */

#ifndef _COFF_H_INCLUDED
#define _COFF_H_INCLUDED

struct qditem {
    uint_8 *next;
    uint_16 size;
};

void     coff_init( struct module_info * );

#endif
