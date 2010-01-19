
// interface to OMF format output functions

#ifndef _OMF_H_INCLUDED_
#define _OMF_H_INCLUDED_

void      omf_init( module_info * );
void      omf_fini( void );
ret_code  omf_write_public( bool );
void      omf_write_alias( void );
ret_code  omf_write_autodep( void );
void      omf_write_header( void );
ret_code  omf_write_comdef( void );
void      omf_write_extdef( void );
void      omf_write_lnames( void );
void      omf_write_seg( bool );
void      omf_write_grp( void );
void      omf_write_export( void );
void      omf_write_lib( void );
void      omf_write_dosseg( void );
void      omf_end_of_pass1( void );
void      omf_set_filepos( void );
void      omf_write_ledata( dir_node * );
void      omf_write_linnum( void );
void      omf_write_modend( void );
void      omf_OutSelect( bool );
void      omf_FlushCurrSeg( void );

struct fixup *omf_create_fixup( struct asmfixup * );

void      omf_write_header_dbgcv( void );
void      omf_write_debug_tables( void );

#endif // _OMF_H_INCLUDED_

