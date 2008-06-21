
// interface to OMF write functions

//extern pobj_state       pobjState;

void omf_write_record( obj_rec *objr, char kill );
int omf_write_pub( void );
void omf_write_alias( void );
int omf_write_autodep( void );
void omf_write_header( void );
int omf_write_comdef( void );
void omf_write_extdef( void );
void omf_write_lnames( void );
void omf_write_seg( void );
void omf_write_grp( void );
void omf_write_export( void );
void omf_write_lib( void );
void omf_write_dosseg( void );
void omf_write_end_of_pass1( void );
void omf_write_ledata( void );
void omf_write_linnum( void );
void omf_OutSelect( bool );
void omf_FlushCurrSeg( void );


