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
static short inst_table[ HASH_TABLE_SIZE ];

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

struct ReservedWord *FindResWord( const char *name )
/**************************************************/
/* search reserved word in hash table */
{
    struct ReservedWord *inst;
    int i;

    for( i = inst_table[ hashpjw( name ) ]; i != EMPTY; i = inst->next ) {
        inst = &AsmResWord[i];
        /* check if the name matches the entry for this inst in AsmChars */
        //if( name[ inst->len ] == NULLC && _strnicmp( name, inst->name, inst->len ) == 0) {
        if( name[ inst->len ] == NULLC && _memicmp( name, inst->name, inst->len ) == 0) {
            return( inst );
        }
    }
    return( NULL );
}

/* returns index of instruction in AsmOpTable */

#if 0
int get_instruction_position( const char *string )
/************************************************/
{
    struct ReservedWord *inst;

    inst = FindResWord( string );
    if( inst ) return( inst->position );
    return( EMPTY );
}
#endif

void InitInstHashTable( void )
/****************************/
{
    int i;
    for ( i = 0; i < HASH_TABLE_SIZE; i++ )
         inst_table[i] = EMPTY;
}

/* add reserved word to hash table */

struct ReservedWord *AddResWord( struct ReservedWord *inst )
/**********************************************************/
{
    int i;
    int old;
    int curr;
    char buffer[MAX_RESW_LEN];

#ifdef DEBUG_OUT
    if ( inst->len >= MAX_RESW_LEN ) {
        printf( "internal error, reserved word %.*s exceeds max size %u\n", inst->len, inst->name, MAX_RESW_LEN );
        exit(-1);
    }
#endif
    memcpy( buffer, inst->name, inst->len );
    buffer[ inst->len ] = NULLC;
    i = hashpjw( buffer );

    /* sort the items of a line by length! */

    for( curr = inst_table[i], old = EMPTY; curr != EMPTY && AsmResWord[curr].len <= inst->len; old = curr, curr = AsmResWord[curr].next );

    if ( old == EMPTY ) {
        inst->next = inst_table[i];
        inst_table[i] = inst - AsmResWord;
    } else {
        inst->next = AsmResWord[old].next;
        AsmResWord[old].next = inst - AsmResWord;
    }

    return( inst );
}

// remove a reserved word from the hash table.

int RemoveResWord( struct ReservedWord *inst )
/********************************************/
{
    int i;
    int old;
    int curr;
    char buffer[MAX_RESW_LEN];

    memcpy( buffer, inst->name, inst->len );
    buffer[ inst->len ] = NULLC;
    i = hashpjw( buffer );

    for( curr = inst_table[i], old = EMPTY ; curr != EMPTY ; old = curr, curr = AsmResWord[curr].next )  {
        if( &AsmResWord[curr] == inst ) {
            if ( old != EMPTY )
                AsmResWord[old].next = AsmResWord[curr].next;
            else
                inst_table[i] = AsmResWord[curr].next;
            return( TRUE );
        }
    }
    return( FALSE );
}

#ifdef DEBUG_OUT
void DumpInstrStats( void )
/*************************/
{
    unsigned            i;
    int                 inst;
    unsigned            count = 0;
    unsigned            max = 0;
    unsigned            curr = 0;
    unsigned            num[8] = {0,0,0,0,0,0,0,0};

    for( i = 0; i < HASH_TABLE_SIZE; i++ ) {
        for( inst = inst_table[i], curr = 0; inst != EMPTY; inst = AsmResWord[inst].next ) {
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
