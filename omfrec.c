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
* Description:  handle OMF records
*
****************************************************************************/


#include <string.h>
#include "memalloc.h"
#include "omfrec.h"
#include "omfpc.h"
#include "myassert.h"
#include "carve.h"

static carve_t myCarver;

void OmfRecInit( void ) {
/*********************/
    myCarver = CarveCreate( sizeof( obj_rec ), 16 );
}

void OmfRecFini( void ) {
/*********************/
    CarveDestroy( myCarver );
}

obj_rec *OmfNewRec( uint_8 command ) {
/**********************************/
    obj_rec *new;

    new = CarveAlloc( myCarver );
    new->command = command;
    new->data = NULL;
    new->length = 0;
    new->curoff = 0;
    new->is_32 = 0;
    new->free_data = 0;
    return( new );
}

void OmfKillRec( obj_rec *objr ) {
/******************************/
/**/myassert( objr != NULL );
    if( objr->free_data && objr->data != NULL ) {
        OmfDetachData( objr );
    }
    switch( objr->command ) {
    case CMD_FIXUP:
        {
            fixup   *cur;
            fixup   *next;

            cur = objr->d.fixup.fixup;
            while( cur != NULL ) {
                next = cur->next;
                FixKill( cur );
                cur = next;
            }
        }
        break;
    case CMD_LINNUM:
    case CMD_LINSYM:
        {
            struct linnum_data *lines;

            lines = objr->d.linnum.lines;
            if( lines != NULL ) {
                AsmFree( lines );
            }
        }
        break;
    case CMD_PUBDEF:
    case CMD_STATIC_PUBDEF:
        if( objr->d.pubdef.free_pubs ) {
/**/        myassert( objr->d.pubdef.pubs != NULL );
            AsmFree( objr->d.pubdef.pubs );
        }
        break;
    }
    CarveFree( myCarver, objr );
}

void OmfAllocData( obj_rec *objr, uint_16 len ) {
/*********************************************/
/**/myassert( objr->data == NULL );
    objr->data = AsmAlloc( len );
    objr->length = len;
    objr->free_data = 1;
}

void OmfAttachData( obj_rec *objr, uint_8 *data, uint_16 len ) {
/************************************************************/
/**/myassert( objr->data == NULL );
    objr->data = data;
    objr->length = len;
    objr->free_data = 0;
}

void OmfDetachData( obj_rec *objr ) {
/*********************************/
/**/myassert( objr != NULL );
/**/myassert( objr->data != NULL );
    if( objr->free_data ) {
        AsmFree( objr->data );
    }
    objr->data = NULL;
    objr->length = 0;
}

uint_8 OmfGet8( obj_rec *objr ) {
/*****************************/
/**/myassert( objr != NULL && objr->data != NULL );
    return( objr->data[ objr->curoff++ ] );
}

uint_16 OmfGet16( obj_rec *objr ) {
/*******************************/
    uint_8  *p;

/**/myassert( objr != NULL && objr->data != NULL );
    p = OmfGet( objr, 2 );
    return( ReadU16( p ) );
}

uint_32 OmfGet32( obj_rec *objr ) {
/*******************************/
    uint_8  *p;

/**/myassert( objr != NULL && objr->data != NULL );

    p = OmfGet( objr, 4 );
    return( ReadU32( p ) );
}

#if 0
uint_32 OmfGetEither( obj_rec *objr ) {
/***********************************/
/**/myassert( objr != NULL );
    if( objr->is_32 ) {
        return( OmfGet32( objr ) );
    } else {
        return( (uint_32)OmfGet16( objr ) );
    }
}
#endif

uint_16 OmfGetIndex( obj_rec *objr ) {
/**********************************/
    uint_16 index;

/**/myassert( objr != NULL && objr->data != NULL );

    index = OmfGet8( objr );
    if( index > 0x7f ) {
        index = ( ( index & 0x7f ) << 8 ) | OmfGet8( objr );
    }
    return( index );
}

uint_8 *OmfGet( obj_rec *objr, uint_16 len ) {
/******************************************/
    uint_8  *p;

#if DEBUG_OUT
    if ( objr == NULL || objr->data == NULL ) {
        _asm int 3;
    }
#else
/**/myassert( objr != NULL && objr->data != NULL );
#endif

    p = objr->data + objr->curoff;
    objr->curoff += len;
/**/myassert( objr->curoff <= objr->length );
    return( p );
}

void OmfPut8( obj_rec *objr, uint_8 byte ) {
/****************************************/
/**/myassert( objr != NULL && objr->data != NULL );
    objr->data[ objr->curoff++ ] = byte;
}

void OmfPut16( obj_rec *objr, uint_16 word ) {
/******************************************/
/**/myassert( objr != NULL && objr->data != NULL );

    WriteU16( objr->data + objr->curoff, word );
    objr->curoff += 2;
}

void OmfPut32( obj_rec *objr, uint_32 dword ) {
/*******************************************/
/**/myassert( objr != NULL && objr->data != NULL );

    WriteU32( objr->data + objr->curoff, dword );
    objr->curoff += 4;
}

#if 0
void OmfPutEither( obj_rec *objr, uint_32 data ) {
/**********************************************/
/**/myassert( objr != NULL && objr->data != NULL );
    if( objr->is_32 ) {
        OmfPut32( objr, data );
    } else {
        OmfPut16( objr, (uint_16)data );
    }
}
#endif

void OmfPutIndex( obj_rec *objr, uint_16 idx ) {
/********************************************/
/**/myassert( objr != NULL && objr->data != NULL );
    if( idx > 0x7f ) {
        OmfPut8( objr, ( idx >> 8 ) | 0x80 );
    }
    OmfPut8( objr, idx & 0xff );
}

void OmfPut( obj_rec *objr, const uint_8 *data, uint_16 len ) {
/***********************************************************/
/**/myassert( objr != NULL && objr->data != NULL );
    memcpy( objr->data + objr->curoff, data, len );
    objr->curoff += len;
}

void OmfPutName( obj_rec *objr, const char *name, uint_8 len ) {
/************************************************************/
/**/myassert( objr != NULL && objr->data != NULL );
    OmfPut8( objr, len );
    OmfPut( objr, (uint_8 *)name, len );
}
