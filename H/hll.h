/****************************************************************************
*
* Description:  hll constructs .IF, ...
*
****************************************************************************/


#ifndef _HLL_H_
#define _HLL_H_

extern void             HllInit( void );    // reset label counter for hll labels
extern ret_code         HllStartDef( int ); // begin a .IF, .WHILE, .REPEAT
extern ret_code         HllEndDef( int );   // end a .IF, .WHILE, .REPEAT
extern ret_code         HllExitDef( int );  // exit a .IF, .WHILE, .REPEAT
extern void             HllCheckOpen( void );

#endif
