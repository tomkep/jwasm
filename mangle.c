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


/* constants used by the name manglers ( changes ) */
enum changes {
    NORMAL           = 0,
    USCORE_FRONT     = 1,
    USCORE_BACK      = 2,
    REM_USCORE_FRONT = 4,
    REM_USCORE_BACK  = 8,
    UPPERCASE        = 16
};

#define USCORE "_"

typedef char *(*mangle_func)( struct asm_sym *, char * );

/* AsmMangler: no change to symbol name */

static char *AsmMangler( struct asm_sym *sym, char *buffer )
/**********************************************************/
{
    char        *name;

    if( buffer == NULL ) {
        name = AsmAlloc( strlen( sym->name ) + 1 );
    } else {
        name = buffer;
    }
    strcpy( name, sym->name );
    return( name );
}

/* UCaseMangler: convert symbol name to upper case */

static char *UCaseMangler( struct asm_sym *sym, char *buffer )
/************************************************************/
{
    char        *name;

    if( buffer == NULL ) {
        name = AsmAlloc( strlen( sym->name ) + 1 );
    } else {
        name = buffer;
    }
    strcpy( name, sym->name );
    strupr( name );
    return( name );
}

/* UScoreMangler: add '_' prefix to symbol name */

static char *UScoreMangler( struct asm_sym *sym, char *buffer )
/*************************************************************/
{
    char        *name;

    if( buffer == NULL ) {
        name = AsmAlloc( strlen( sym->name ) + 2 );
    } else {
        name = buffer;
    }
    strcpy( name, USCORE );
    strcat( name, sym->name );
    return( name );
}

/* StdUScoreMangler: add '_' prefix and '@size' suffix to proc names */
/*                   add '_' prefix to other symbols */

static char *StdUScoreMangler( struct asm_sym *sym, char *buffer )
/*************************************************************/
{
    char        *name;
    dir_node    *dir = (dir_node *)sym;

    if( Options.no_stdcall_decoration )
        return( AsmMangler( sym, buffer ) );

    if( !Options.no_stdcall_suffix && ( sym->state == SYM_PROC ) ) {
        if( buffer == NULL ) {
            int         count;
            dir_node    *dir = (dir_node *)sym;
            int         parasize = dir->e.procinfo->parasize;

            for( count = 2; parasize > 9; count++ )
                parasize /= 10;
            name = AsmAlloc( strlen( sym->name ) + 2 + count );
        } else {
            name = buffer;
        }
        sprintf( name, "_%s@%d", sym->name, dir->e.procinfo->parasize );
        return( name );
    } else {
        return( UScoreMangler( sym, buffer ) );
    }
}

/* WatcomCMangler: add '_' suffix to proc names and labels */
/*                 add '_' prefix to other symbols */

static char *WatcomCMangler( struct asm_sym *sym, char *buffer )
/********************************************************/
{
    char                *name;
    char                *ptr = sym->name;
    enum changes        changes = NORMAL;

    if( sym->state == SYM_PROC ) {
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

    if( buffer == NULL ) {
        name = AsmAlloc( strlen( ptr ) + 2 );
    } else {
        name = buffer;
    }

    if( changes & USCORE_FRONT ) {
        strcpy( name, USCORE );
    } else {
        strcpy( name, NULLS );
    }
    strcat( name, ptr );
    if( changes & USCORE_BACK ) {
        strcat( name, USCORE );
    }
    return( name );
}

static char *CMangler( struct asm_sym *sym, char *buffer )
/********************************************************/
{
    if( Options.naming_convention == NC_ADD_USCORES ) {
        return( WatcomCMangler( sym, buffer ) );
    } else {
        return( AsmMangler( sym, buffer ) );
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
            return( AsmMangler );
        }
    }
    if ( mangle_type )
        AsmErr( UNKNOWN_MANGLER, mangle_type );

    return( NULL );
}

char *Mangle( struct asm_sym *sym, char *buffer )
/***********************************************/
{
    mangle_func mangler;

    switch( sym->langtype ) {
    case LANG_SYSCALL:
        mangler = AsmMangler;
        break;
    case LANG_STDCALL:
        mangler = StdUScoreMangler;
        break;
    case LANG_BASIC:
    case LANG_FORTRAN:
    case LANG_PASCAL:
        mangler = UCaseMangler;
        break;
    case LANG_WATCOM_C:          // registers passing parameters, only Open Watcom
        mangler = WatcomCMangler;
        break;
    case LANG_C:                 // stack passing parameters
        DebugMsg(("lang C detected\n"));
        if( Options.watcom_c_mangler ) {
            mangler = AsmMangler;
        } else {
            DebugMsg(("lang C UScore mangler\n"));
            mangler = UScoreMangler;
        }
        break;
    case LANG_NONE:
        mangler = sym->mangler;
#if MANGLERSUPP
        if( mangler == NULL )
            mangler = GetMangler( Options.default_name_mangler );
#endif
        if( mangler == NULL )
            mangler = AsmMangler;
        break;
    }
    sym->mangler = mangler;
    return( mangler( sym, buffer ) );
}

// the "mangle_type" is an extension inherited from OW Wasm
// accepted are "C" and "N".

void SetMangler( struct asm_sym *sym, char *mangle_type, int langtype )
/*********************************************************************/
{
    mangle_func mangler;

    if( langtype != LANG_NONE )
        sym->langtype = langtype;

    mangler = GetMangler( mangle_type );
    if( mangler == NULL ) {
        /* nothing to do */
    } else if( sym->mangler == NULL ) {
        sym->mangler = mangler;
    } else if( sym->mangler != mangler ) {
        AsmErr( CONFLICTING_MANGLER, sym->name );
    }
}
