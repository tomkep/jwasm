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
* Description:  conversion function for autodependency date/time stamps.
*       All object files in OMF format use the DOS date/time format for
*       dependency comment record.
****************************************************************************/

#include <stdlib.h>
#include <sys/stat.h>

#ifdef __GNUC__
#include "globals.h"
#endif
#include "autodept.h"

enum {
    TIME_SEC_B  = 0,
    TIME_SEC_F  = 0x001f,
    TIME_MIN_B  = 5,
    TIME_MIN_F  = 0x07e0,
    TIME_HOUR_B = 11,
    TIME_HOUR_F = 0xf800
};

enum {
    DATE_DAY_B  = 0,
    DATE_DAY_F  = 0x001f,
    DATE_MON_B  = 5,
    DATE_MON_F  = 0x01e0,
    DATE_YEAR_B = 9,
    DATE_YEAR_F = 0xfe00
};

typedef union {
    struct {
        unsigned short time;
        unsigned short date;
    } dos;
    time_t timet;
} DOS_DATETIME;

time_t _timet2dos(time_t x)
/******************************************************************/
{
    struct tm *    ltime;
    DOS_DATETIME   dt;

    ltime = localtime( &x );
    dt.dos.date = (( ltime->tm_year - 80 ) << DATE_YEAR_B )
             | (( ltime->tm_mon + 1 ) << DATE_MON_B )
             | (( ltime->tm_mday ) << DATE_DAY_B );
    dt.dos.time = (( ltime->tm_hour ) << TIME_HOUR_B )
             | (( ltime->tm_min ) << TIME_MIN_B )
             | (( ltime->tm_sec / 2 ) << TIME_SEC_B );
    return dt.timet;
}

#if 0
time_t _dos2timet(time_t x)
/******************************************************************/
{
    struct tm      ltime;
    DOS_DATETIME   dt;

    dt.timet = x;

    ltime.tm_year = ( ( dt.dos.date & DATE_YEAR_F ) >> DATE_YEAR_B ) + 80;
    ltime.tm_mon  = ( ( dt.dos.date & DATE_MON_F ) >> DATE_MON_B ) - 1;
    ltime.tm_mday = ( dt.dos.date & DATE_DAY_F ) >> DATE_DAY_B;

    ltime.tm_hour = ( dt.dos.time & TIME_HOUR_F ) >> TIME_HOUR_B;
    ltime.tm_min  = ( dt.dos.time & TIME_MIN_F ) >> TIME_MIN_B;
    ltime.tm_sec  = ( ( dt.dos.time & TIME_SEC_F ) >> TIME_SEC_B ) * 2;

    ltime.tm_isdst= -1;

    return mktime( &ltime );
}
#endif

#if 0
time_t _DOSStampToTime( unsigned short date, unsigned short time )
/******************************************************************/
{
    struct tm tmbuf;

    tmbuf.tm_year = ( ( date & DATE_YEAR_F ) >> DATE_YEAR_B ) + 80;
    tmbuf.tm_mon  = ( ( date & DATE_MON_F ) >> DATE_MON_B ) - 1;
    tmbuf.tm_mday = ( date & DATE_DAY_F ) >> DATE_DAY_B;

    tmbuf.tm_hour = ( time & TIME_HOUR_F ) >> TIME_HOUR_B;
    tmbuf.tm_min  = ( time & TIME_MIN_F ) >> TIME_MIN_B;
    tmbuf.tm_sec  = ( ( time & TIME_SEC_F ) >> TIME_SEC_B ) * 2;

    tmbuf.tm_isdst= -1;

    return( mktime( &tmbuf ) );
}
#endif

#if 0
void _TimeToDOSStamp( time_t x, unsigned short *date, unsigned short *time )
/***************************************************************************/
{
    struct tm *    ltime;

    ltime = localtime( &x );
    *date = (( ltime->tm_year - 80 ) << DATE_YEAR_B )
             | (( ltime->tm_mon + 1 ) << DATE_MON_B )
             | (( ltime->tm_mday ) << DATE_DAY_B );
    *time = (( ltime->tm_hour ) << TIME_HOUR_B )
             | (( ltime->tm_min ) << TIME_MIN_B )
             | (( ltime->tm_sec / 2 ) << TIME_SEC_B );
    return;
}
#endif

char *_getFilenameFullPath( char *buff, char const *name, size_t max )
/*********************************************************************/
{
    char        *p;

    p = _fullpath( buff, name, max );
    if( p == NULL )
        p = (char *)name;

#ifdef __UNIX__
    if( (p[0] == '/' && p[1] == '/') && (name[0] != '/' || name[1] != '/') ) {
        //
        // if the _fullpath result has a node number and
        // the user didn't specify one, strip the node number
        // off before returning
        //
        p += 2;
        while( *(p++) != '/' ) ;
    }
#endif
    return( p );
}

time_t _getFilenameTimeStamp( char const *filename )
{
    struct stat statbuf;

    if( stat( filename, &statbuf ) != 0 ) {
        return( 0 );
    } else {
        return( statbuf.st_mtime );
    }
}
