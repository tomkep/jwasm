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
* Description:  Symbol name mangling routines.
*
****************************************************************************/

#include <ctype.h>

#include "globals.h"
#include "symbols.h"
#include "memalloc.h"
#include "directiv.h"
#include "mangle.h"

/* constants used by the OW fastcall name mangler ( changes ) */
enum changes {
    NORMAL           = 0,
    USCORE_FRONT     = 1,
    USCORE_BACK      = 2
};

#if MANGLERSUPP
#if !defined(__GNUC__) && !defined(__POCC__)
#define tolower(c) ((c >= 'A' && c <= 'Z') ? c | 0x20 : c )
#endif
#endif

typedef char *(*mangle_func)( struct asm_sym *, char * );

static char *ow_decorate( struct asm_sym *sym, char *buffer );
static char *ms32_decorate( struct asm_sym *sym, char *buffer );
#if AMD64_SUPPORT
static char *ms64_decorate( struct asm_sym *sym, char *buffer );
#endif

/* table of FASTCALL types.
 * order must match the one of enum fastcall_type
 * also see proc.c and invoke.c!
 */

static mangle_func fcmanglers[] = {
    ms32_decorate,
    ow_decorate,
#if AMD64_SUPPORT
    ms64_decorate
#endif
};

/* VoidMangler: no change to symbol name */

static char *VoidMangler( struct asm_sym *sym, char *buffer )
/**********************************************************/
{
    strcpy( buffer, sym->name );
    return( buffer );
}

/* UCaseMangler: convert symbol name to upper case */

static char *UCaseMangler( struct asm_sym *sym, char *buffer )
/************************************************************/
{
    strcpy( buffer, sym->name );
    _strupr( buffer );
    return( buffer );
}

/* UScoreMangler: add '_' prefix to symbol name */

static char *UScoreMangler( struct asm_sym *sym, char *buffer )
/*************************************************************/
{
    buffer[0] = '_';
    strcpy( buffer+1, sym->name );
    return( buffer );
}

/* StdcallMangler: add '_' prefix and '@size' suffix to proc names */
/*                 add '_' prefix to other symbols */

static char *StdcallMangler( struct asm_sym *sym, char *buffer )
/*************************************************************/
{
    dir_node    *dir = (dir_node *)sym;

    if( Options.stdcall_decoration == STDCALL_FULL &&
       sym->isproc ) {
        sprintf( buffer, "_%s@%d", sym->name, dir->e.procinfo->parasize );
        return( buffer );
    } else {
        return( UScoreMangler( sym, buffer ) );
    }
}

/* FASTCALL OW style:
 *  add '_' suffix to proc names and labels
 *  add '_' prefix to other symbols
 */

static char *ow_decorate( struct asm_sym *sym, char *buffer )
/***********************************************************/
{
    char                *name;
    enum changes        changes = NORMAL;

    if( sym->isproc ) {
        changes |= USCORE_BACK;
    } else {
        switch( sym->mem_type ) {
        case MT_NEAR:
        case MT_FAR:
        case MT_EMPTY:
            changes |= USCORE_BACK;
            break;
        default:
            changes |= USCORE_FRONT;
        }
    }

    name = buffer;

    if( changes & USCORE_FRONT )
        *name++ = '_';
    strcpy( name, sym->name );
    if( changes & USCORE_BACK ) {
        name += sym->name_size;
        *name++ = '_';
        *name = NULLC;
    }
    return( name );
}

/* MS FASTCALL 32bit */

static char * ms32_decorate( struct asm_sym *sym, char *buffer )
/**************************************************************/
{
    sprintf( buffer, "@%s@%u", sym->name, ((dir_node *)sym)->e.procinfo->parasize );
    return(buffer);
}

#if AMD64_SUPPORT

/* MS FASTCALL 64bit */

static char * ms64_decorate( struct asm_sym *sym, char *buffer )
/**************************************************************/
{
    strcpy( buffer, sym->name );
    return( buffer );
}
#endif

#if MANGLERSUPP
static char *CMangler( struct asm_sym *sym, char *buffer )
/********************************************************/
{
    if( Options.naming_convention == NC_ADD_USCORES ) {
        return( WatcomCMangler( sym, buffer ) );
    } else {
        return( VoidMangler( sym, buffer ) );
    }
}

static mangle_func GetMangler( char *mangle_type )
/************************************************/
{
    if( mangle_type != NULL && mangle_type[1] == NULLC ) {
        switch ( tolower( *mangle_type ) ) {
        case 'c':
            return( CMangler );
        case 'n':
            return( VoidMangler );
        }
    }
    if ( mangle_type )
        AsmErr( UNKNOWN_MANGLER, mangle_type );

    return( NULL );
}
#endif

char *Mangle( struct asm_sym *sym, char *buffer )
/***********************************************/
{
    mangle_func mangler;

    switch( sym->langtype ) {
    case LANG_SYSCALL:
        mangler = VoidMangler;
        break;
    case LANG_STDCALL:
        if( Options.stdcall_decoration == STDCALL_NONE )
            mangler = VoidMangler;
        else
            mangler = StdcallMangler;
        break;
    case LANG_BASIC:
    case LANG_FORTRAN:
    case LANG_PASCAL:
        mangler = UCaseMangler;
        break;
    case LANG_FASTCALL:          /* registers passing parameters */
        mangler = fcmanglers[Options.fastcall];
        break;
    case LANG_C:
        if( Options.no_cdecl_decoration )
            mangler = VoidMangler;
        else
            mangler = UScoreMangler; /* leading underscore (MS C) */
        break;
    case LANG_NONE:
#if MANGLERSUPP
        mangler = sym->mangler;
        if( mangler == NULL )
            mangler = GetMangler( Options.default_name_mangler );
        if( mangler == NULL )
#endif
            mangler = VoidMangler;
        break;
    }
#if MANGLERSUPP
    sym->mangler = mangler;
#endif
    return( mangler( sym, buffer ) );
}

// the "mangle_type" is an extension inherited from OW Wasm
// accepted are "C" and "N". It's NULL if MANGLESUPP == 0 (standard)

void SetMangler( struct asm_sym *sym, char *mangle_type, int langtype )
/*********************************************************************/
{
#if MANGLERSUPP
    mangle_func mangler;
#endif

    if( langtype != LANG_NONE )
        sym->langtype = langtype;

#if MANGLERSUPP
    mangler = GetMangler( mangle_type );
    if( mangler == NULL ) {
        /* nothing to do */
    } else if( sym->mangler == NULL ) {
        sym->mangler = mangler;
    } else if( sym->mangler != mangler ) {
        AsmErr( CONFLICTING_MANGLER, sym->name );
    }
#endif
}
