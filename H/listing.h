/****************************************************************************
*
* Description:  listing interface.
*
****************************************************************************/


#ifndef _LISTING_H_INCLUDED
#define _LISTING_H_INCLUDED

enum lsttype {
 LSTTYPE_LIDATA    = 0,
 LSTTYPE_EQUATE    = 1,
 LSTTYPE_DIRECTIVE = 2,
 LSTTYPE_MACRO     = 3,
 LSTTYPE_STRUCT    = 4,
 LSTTYPE_LABEL     = 5,
 LSTTYPE_MACROLINE = 6
};

extern void LstOpenFile( void );
extern void LstCloseFile( void );
extern void LstWrite( enum lsttype, unsigned int ofs, void * sym );
extern void LstWriteSrcLine( void );
extern void LstWriteCRef( void );
extern void LstPrintf( const char *format, ... );
extern void LstNL( void );

extern uint_32 list_pos;        /* current LST file position */
extern uint_32 list_pos_start;  /* position of current line */

#endif
