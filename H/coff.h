
/* prototypes of functions defined in coff.c */

#ifndef _COFF_H_INCLUDED
#define _COFF_H_INCLUDED

struct qditem {
    uint_8 *next;
    uint_16 size;
};

ret_code coff_write_header( module_info * );
ret_code coff_write_section_table( module_info * );
ret_code coff_write_data( module_info * );
ret_code coff_write_symbols( module_info * );

#endif
