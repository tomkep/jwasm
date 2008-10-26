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
* Description:  instruction search functions
*
****************************************************************************/


#include "globals.h"
#include "parser.h"

#ifdef __USE_BSD
#define strnicmp strncasecmp
#endif

//#define HASH_TABLE_SIZE 211
#define HASH_TABLE_SIZE 599
//#define HASH_TABLE_SIZE 811
//#define HASH_TABLE_SIZE 2003

//static struct ReservedWord *inst_table[ HASH_TABLE_SIZE ] = { NULL };
static struct ReservedWord *inst_table[ HASH_TABLE_SIZE ];

static unsigned int hashpjw( const char *s )
/******************************************/
{
    unsigned h;
    unsigned g;

    for( h = 0; *s; ++s ) {
        /* ( h & ~0x0fff ) == 0 is always true here */
        h = (h << 4) + (*s | ' ');
        g = h & ~0x0fff;
        h ^= g;
        h ^= g >> 12;
    }
    return( h % HASH_TABLE_SIZE );
}

static struct ReservedWord *InstrFind( char *name )
/********************************************/
/* find an instruction in the hash table */
{
    struct ReservedWord *inst;

    inst = inst_table[ hashpjw( name ) ];

    for( ; inst; inst = inst->next ) {
        /* check if the name matches the entry for this inst in AsmChars */
        if( name[ inst->len ] == NULLC && strnicmp( name, inst->name, inst->len ) == 0) {
            return( inst );
        }
    }
    return( NULL );
}

static struct ReservedWord *InstrAdd( struct ReservedWord *inst )
/********************************************************/
{
    struct ReservedWord  **location;
    char buffer[20];

    memcpy( buffer, inst->name, inst->len );
    buffer[ inst->len ] = NULLC;

    location = &inst_table[ hashpjw( buffer ) ];

    for( ; *location; location = &((*location)->next) ) {
        /* to be safe check if the name is already in there */
        if( inst->len == (*location)->len &&
            memcmp( buffer, (*location)->name, inst->len ) == 0 ) {
            /* we already have one, shouldn't happen */
            AsmErr( SYMBOL_ALREADY_DEFINED, buffer );
            return( NULL );
        }
    }

    inst->next = *location;
    *location = inst;

    return( inst );
}

/* entry points */

/* returns index of instruction in AsmOpTable */

int get_instruction_position( char *string )
/******************************************/
{
    struct ReservedWord *inst;

    inst = InstrFind( string );
    if( inst ) return( inst->position );
    return( EMPTY );
}

// make this whole table static? use indices instead of pointers

void make_inst_hash_table( void )
/*******************************/
{
    unsigned i;

    for( i = 0; i != T_NULL; i++ ) {
        InstrAdd( &AsmResWord[i] );
    }
    return;
}

// remove a reserved word.

struct ReservedWord *InstrRemove( char *string )
{
    struct ReservedWord  *prev = NULL;
    struct ReservedWord  *curr;
    int i;

    i = hashpjw( string );
    curr = inst_table[i];

    for( ; curr ; prev = curr, curr = curr->next)  {
        if( strnicmp( string, curr->name, curr->len ) == 0 && *(string + curr->len) == '\0' ) {
            if (prev)
                prev->next = curr->next;
            else
                inst_table[i] = curr->next;
            return( curr );
        }
    }
    return( NULL );
}
