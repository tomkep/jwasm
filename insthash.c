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
* Description:  instruction hash table access
*
****************************************************************************/

#include "globals.h"
#include "parser.h"
#include "insthash.h"

//#define HASH_TABLE_SIZE 211
#define HASH_TABLE_SIZE 599
//#define HASH_TABLE_SIZE 701
//#define HASH_TABLE_SIZE 2003

//static struct ReservedWord *inst_table[ HASH_TABLE_SIZE ] = { NULL };
static struct ReservedWord *inst_table[ HASH_TABLE_SIZE ];

static unsigned int hashpjw( const char *s )
/******************************************/
{
    uint_32 h;
    uint_32 g;

    for( h = 0; *s; ++s ) {
        /* ( h & ~0x0fff ) == 0 is always true here */
        h = (h << 3) + (*s | ' ');
        g = h & ~0x1fff;
        h ^= g;
        h ^= g >> 13;
    }
    return( h % HASH_TABLE_SIZE );
}

struct ReservedWord *FindResWord( char *name )
/********************************************/
/* find an instruction in the hash table */
{
    struct ReservedWord *inst;

    inst = inst_table[ hashpjw( name ) ];

    for( ; inst; inst = inst->next ) {
        /* check if the name matches the entry for this inst in AsmChars */
//        if( name[ inst->len ] == NULLC && _strnicmp( name, inst->name, inst->len ) == 0) {
        if( name[ inst->len ] == NULLC && _memicmp( name, inst->name, inst->len ) == 0) {
            return( inst );
        }
    }
    return( NULL );
}

/* returns index of instruction in AsmOpTable */

#if 0
int get_instruction_position( char *string )
/******************************************/
{
    struct ReservedWord *inst;

    inst = FindResWord( string );
    if( inst ) return( inst->position );
    return( EMPTY );
}
#endif

/* add reserved word to hash table */

struct ReservedWord *AddResWord( struct ReservedWord *inst )
/********************************************************/
{
    struct ReservedWord  **location;
    char buffer[MAX_RESW_LEN];

    memcpy( buffer, inst->name, inst->len );
    buffer[ inst->len ] = NULLC;

    location = &inst_table[ hashpjw( buffer ) ];

    /* sort the items of a line by length! */

    for( ; *location && (*location)->len <= inst->len; location = &((*location)->next) ) {
#if 0 /* this isn't needed anymore, a duplicate entry would cause a compile error */
        /* to be safe check if the name is already in there */
        if( inst->len == (*location)->len &&
            memcmp( buffer, (*location)->name, inst->len ) == 0 ) {
            /* we already have one, shouldn't happen */
            AsmErr( SYMBOL_ALREADY_DEFINED, buffer );
            return( NULL );
        }
#endif
    }

    inst->next = *location;
    *location = inst;

    return( inst );
}

// remove a reserved word from the hash table.

int RemoveResWord( struct ReservedWord *inst )
{
    struct ReservedWord  *prev = NULL;
    struct ReservedWord  *curr;
    int i;
    char buffer[MAX_RESW_LEN];

    memcpy( buffer, inst->name, inst->len );
    buffer[ inst->len ] = NULLC;
    i = hashpjw( buffer );
    curr = inst_table[i];

    for( ; curr ; prev = curr, curr = curr->next)  {
        if( curr == inst ) {
            if ( prev )
                prev->next = curr->next;
            else
                inst_table[i] = curr->next;
            return( TRUE );
        }
    }
    return( FALSE );
}

#ifdef DEBUG_OUT
void DumpInstrStats( void )
{
    unsigned            i;
    struct ReservedWord *inst;
    unsigned            count = 0;
    unsigned            max = 0;
    unsigned            curr = 0;
    unsigned            num[8] = {0,0,0,0,0,0,0,0};

    for( i = 0; i < HASH_TABLE_SIZE; i++ ) {
        for( inst = inst_table[i], curr = 0; inst; inst = inst->next ) {
            curr++;
        }
        count += curr;
        if ( curr <= 7 )
            num[curr]++;
        if (max < curr)
            max = curr;
    }
    if ( Options.quiet == FALSE ) {
        printf( "%u items in ins table, max items/line=%u ", count, max );
        printf( "[%u %u %u %u %u %u %u %u]\n", num[0], num[1], num[2], num[3], num[4], num[5], num[6], num[7] );
    }
}
#endif
