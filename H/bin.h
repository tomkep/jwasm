/****************************************************************************
*
* Description:  header for BIN output format
*
****************************************************************************/


#ifndef _BIN_H_INCLUDED_
#define _BIN_H_INCLUDED_

ret_code bin_write_data( struct module_info * );
void     bin_init( struct module_info * );

#if PE_SUPPORT
void     pe_create_MZ_header( void );
void     pe_create_PE_header( void );
void     pe_create_section_table( void );
void     pe_emit_export_data( void );
void     pe_emit_import_data( void );
#endif

#endif // _BIN_H_INCLUDED_
