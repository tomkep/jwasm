/****************************************************************************
*
*                            Open Watcom Project
*
*    Portions Copyright (c) 1983-2002 Sybase, Inc. All Rights Reserved.
*
*  ========================================================================
*
*    This file contains Original Code and/or Modifications of Original
*    Code as defined in and that are subject to the Sybase Open Watcom
*    Public License version 1.0 (the 'License'). You may not use this file
*    except in compliance with the License. BY USING THIS FILE YOU AGREE TO
*    ALL TERMS AND CONDITIONS OF THE LICENSE. A copy of the License is
*    provided with the Original Code and Modifications, and is also
*    available at www.sybase.com/developer/opensource.
*
*    The Original Code and all software distributed under the License are
*    distributed on an 'AS IS' basis, WITHOUT WARRANTY OF ANY KIND, EITHER
*    EXPRESS OR IMPLIED, AND SYBASE AND ALL CONTRIBUTORS HEREBY DISCLAIM
*    ALL SUCH WARRANTIES, INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF
*    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, QUIET ENJOYMENT OR
*    NON-INFRINGEMENT. Please see the License for the specific language
*    governing rights and limitations under the License.
*
*  ========================================================================
*
* Description:  Assembler symbols internal structures and definitions.
*
****************************************************************************/


#ifndef _ASMSYM_H_
#define _ASMSYM_H_

#include "asminlin.h"
#include "asmops2.h"

typedef enum {
        MT_BYTE,
        MT_WORD,
        MT_DWORD,
        MT_QWORD,
        MT_FWORD,
        MT_TBYTE,
        MT_OWORD,

        MT_SHORT,
        MT_NEAR,
        MT_FAR,

        MT_PTR,

#if defined( _STANDALONE_ )
        MT_SBYTE,
        MT_SWORD,
        MT_SDWORD,

        MT_TYPE,  /* field <type> specifies type */

        MT_PROC,
        MT_ABS,
        MT_BITS,
#endif
        MT_ERROR,
        MT_EMPTY
} memtype;

#if defined( _STANDALONE_ )
typedef enum {
    LANG_NONE     = 0,
    LANG_C        = 1,
    LANG_SYSCALL  = 2,
    LANG_STDCALL  = 3,
    LANG_PASCAL   = 4,
    LANG_FORTRAN  = 5,
    LANG_BASIC    = 6,
    LANG_WATCOM_C = 7
} lang_type;
#endif

// symbols can be
// - "labels" (data or code, internal, external, stack)
//   which have mem_type MT_BYTE..MT_OWORD, MT_NEAR, MT_FAR, MT_PTR
// - constants
//   either numbers, defined by EQU or "=", then mem_type is MT_ABS
//   or text equates, then mem_type is MT_EMPTY (to be removed)
// - types (STRUCT, UNION, TYPEDEF, RECORD) (mem_type = MT_TYPE)

typedef int (* macro_func)(char *, char * *);

typedef struct asm_sym {
        struct asm_sym  *next;
        char            *name;

#if defined( _STANDALONE_ )
        struct asm_sym  *segment;
        union {
            uint_32         offset;
            char *          string_ptr;
            macro_func      func_ptr;
        };
        uint_32         first_size;   /* size of 1st initializer in bytes */
        uint_32         first_length; /* size of 1st initializer--elts. dup'd */
        uint_32         total_size;   /* total number of bytes (sizeof) */
        uint_32         total_length; /* total number of elements (lengthof) */
        char            *(*mangler)( struct asm_sym *sym, char *buffer );
        unsigned        used:1;       /* symbol has been referenced */
        unsigned        defined:1;    /* symbol has been defined */
        unsigned        local:1;      /* symbol is local */
        unsigned        global:1;     /* symbol is global */
        unsigned        equate:1;     /* symbol has been defined with EQU */
        unsigned        predefined:1; /* symbol is predefined */
        unsigned        variable:1;   /* symbol is variable (redef) */
        unsigned        public:1;     /* symbol is to make public */
        lang_type       langtype;
#else
        long            addr;
#endif
        memtype         mem_type;
        struct asm_sym  *type;        /* set if memtype is MT_TYPE */
        enum sym_state  state;
        struct asmfixup *fixup;
} asm_sym;

extern  struct asm_sym  *SymLookup( const char *name );
extern  struct asm_sym  *SymLookupLabel( const char *name, int bDefine );
extern  struct asm_sym  *SymSearch( const char *name );
extern  void            SymSetCmpFunc( bool nocasemap );

#if defined( _STANDALONE_ )

extern  void            SymTakeOut( const char *name );
extern  void            SymFree( struct asm_sym *sym);
extern  int             SymChangeName( const char *old, const char *new );
extern  void            SymSetName( struct asm_sym * sym, const char *name );
extern  void            SymWriteCRef( void );
extern  void            SymInit( int pass );
extern  void            SymFini( void );

typedef int (* StrCmpFunc)(char const *, char const *);
extern StrCmpFunc SymCmpFunc;

extern  struct asm_sym  *SymCreate( const char *, int );

#define IS_SYM_COUNTER( x ) ( ( x[0] == '$' ) && ( x[1] == 0 ) )

#endif

#endif
