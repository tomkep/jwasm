/****************************************************************************
*
* Description:  interface posndir.c - handles ORG, ALIGN, EVEN directives
*
****************************************************************************/

#ifndef _POSNDIR_H_
#define _POSNDIR_H_

extern ret_code         OrgDirective( int );
extern ret_code         AlignDirective( int, int );
extern void             AlignCurrOffset( int );

#endif
