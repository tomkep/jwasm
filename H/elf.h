
/* prototypes of functions defined in elf.c */

#ifndef _ELF_H_INCLUDED_
#define _ELF_H_INCLUDED_

ret_code elf_write_header( module_info * );
ret_code elf_write_data( module_info * );
void     elf_init( module_info * );

#endif // _ELF_H_INCLUDED_
