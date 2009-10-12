/****************************************************************************
*
* Description:  prototypes of extern.c
*
****************************************************************************/

#ifndef _EXTERN_H_
#define _EXTERN_H_

/*---------------------------------------------------------------------------*/

extern struct asm_sym   *MakeExtern( const char *name, memtype type, struct asm_sym * vartype, struct asm_sym *, unsigned char );

extern ret_code CommDirective( int );
extern ret_code ExterndefDirective( int i );
extern ret_code ExternDirective( int i );
extern ret_code ExternDirective2( int i ); /* if pass > 1 */
extern ret_code PublicDirective( int i );

#endif
