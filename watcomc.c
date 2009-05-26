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
*  Description: declarations for Another C/C++
*
****************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <time.h>
#include <string.h>
#include <errno.h>
#include "watcomc.h"

#define __set_errno( err ) errno = (err)

static const char __Alphabet[] = "0123456789abcdefghijklmnopqrstuvwxyz";

/****************************************************************************
*
* Description:  Implementation of ltoa().
*
****************************************************************************/

char *ultoa( unsigned long value, char *buffer, int radix )
{
    char   *p = buffer;
    char        *q;
    unsigned    rem;
    char        buf[34];        // only holds ASCII so 'char' is OK

    buf[0] = '\0';
    q = &buf[1];
    do {
        rem = value % radix;
        value = value / radix;
        *q = __Alphabet[rem];
        ++q;
    } while( value != 0 );
    while( (*p++ = (char)*--q) )
        ;
    return( buffer );
}


char *ltoa( long value, char *buffer, int radix )
{
    char   *p = buffer;

    if( radix == 10 ) {
        if( value < 0 ) {
            *p++ = '-';
            value = - value;
        }
    }
    ultoa( value, p, radix );
    return( buffer );
}

/****************************************************************************
*
* Description:  Platform independent _splitpath() implementation.
*
****************************************************************************/

#if defined(__UNIX__) || defined(__CYGWIN__)
  #define PC '/'
#else   /* DOS, OS/2, Windows */
  #define PC '\\'
  #define ALT_PC '/'
#endif


static void copypart( char *buf, const char *p, int len, int maxlen )
{
    if( buf != NULL ) {
        if( len > maxlen ) len = maxlen;
            #if defined(__UNIX__) || defined(__CYGWIN__)
                memcpy( buf, p, len );
                buf[len] = '\0';
            #else
                len = _mbsnccnt( p, len );          /* # chars in len bytes */
                _mbsncpy( buf, p, len );            /* copy the chars */
                buf[ _mbsnbcnt(buf,len) ] = '\0';
            #endif
    }
}

#if !defined(_MAX_NODE)
#define _MAX_NODE   _MAX_DRIVE  /*  maximum length of node name w/ '\0' */
#endif

/* split full QNX path name into its components */

/* Under QNX we will map drive to node, dir to dir, and
 * filename to (filename and extension)
 *          or (filename) if no extension requested.
 */

void _splitpath( const char *path,
    char *drive, char *dir, char *fname, char *ext )
{
    const char *dotp;
    const char *fnamep;
    const char *startp;
    unsigned    ch;

    /* take apart specification like -> //0/hd/user/fred/filename.ext for QNX */
    /* take apart specification like -> c:\fred\filename.ext for DOS, OS/2 */

#if defined(__UNIX__) || defined(__CYGWIN__)

    /* process node/drive specification */
    startp = path;
    if( path[0] == PC  &&  path[1] == PC ) {
        path += 2;
        for( ;; ) {
            if( *path == '\0' ) break;
            if( *path == PC ) break;
            if( *path == '.' ) break;
                #if defined(__UNIX__) || defined(__CYGWIN__)
                    path++;
                #else
                    path = _mbsinc( path );
                #endif
        }
    }
    copypart( drive, startp, path - startp, _MAX_NODE );

#else

    /* processs drive specification */
    if( path[0] != '\0'  &&  path[1] == ':' ) {
        if( drive != NULL ) {
            drive[0] = path[0];
            drive[1] = ':';
            drive[2] = '\0';
        }
        path += 2;
    } else if( drive != NULL ) {
        drive[0] = '\0';
    }

#endif

    /* process /user/fred/filename.ext for QNX */
    /* process /fred/filename.ext for DOS, OS/2 */
    dotp = NULL;
    fnamep = path;
    startp = path;

    for(;;) {           /* 07-jul-91 DJG -- save *path in ch for speed */
        if( *path == '\0' )  break;
            #if defined(__UNIX__) || defined(__CYGWIN__)
                ch = *path;
            #else
                ch = _mbsnextc( path );
            #endif
        if( ch == '.' ) {
            dotp = path;
            ++path;
            continue;
        }
            #if defined(__UNIX__) || defined(__CYGWIN__)
                path++;
            #else
                path = _mbsinc( path );
            #endif
#if defined(__UNIX__) || defined(__CYGWIN__)
        if( ch == PC ) {
#else /* DOS, OS/2, Windows */
        if( ch == PC  ||  ch == ALT_PC ) {
#endif
            fnamep = path;
            dotp = NULL;
        }
    }
    copypart( dir, startp, fnamep - startp, _MAX_DIR - 1 );
    if( dotp == NULL ) dotp = path;
    copypart( fname, fnamep, dotp - fnamep, _MAX_FNAME - 1 );
    copypart( ext,   dotp,   path - dotp,   _MAX_EXT - 1);
}

/****************************************************************************
*
* Description:  Platform independent _makepath() implementation.
*
****************************************************************************/

#undef _makepath

#if defined(__UNIX__) || defined(__CYGWIN__)
  #define PC '/'
#else   /* DOS, OS/2, Windows */
  #define PC '\\'
  #define ALT_PC '/'
#endif


#if defined(__UNIX__) || defined(__CYGWIN__)

/* create full Unix style path name from the components */

void _makepath(
        char           *path,
        const char  *node,
        const char  *dir,
        const char  *fname,
        const char  *ext )
{
    *path = '\0';

    if( node != NULL ) {
        if( *node != '\0' ) {
            strcpy( path, node );
            path = strchr( path, '\0' );

            /* if node did not end in '/' then put in a provisional one */
            if( path[-1] == PC )
                path--;
            else
                *path = PC;
        }
    }
    if( dir != NULL ) {
        if( *dir != '\0' ) {
            /*  if dir does not start with a '/' and we had a node then
                    stick in a separator
            */
            if( (*dir != PC) && (*path == PC) ) path++;

            strcpy( path, dir );
            path = strchr( path, '\0' );

            /* if dir did not end in '/' then put in a provisional one */
            if( path[-1] == PC )
                path--;
            else
                *path = PC;
        }
    }

    if( fname != NULL ) {
        if( (*fname != PC) && (*path == PC) ) path++;

        strcpy( path, fname );
        path = strchr( path, '\0' );

    } else {
        if( *path == PC ) path++;
    }
    if( ext != NULL ) {
        if( *ext != '\0' ) {
            if( *ext != '.' )  *path++ = '.';
            strcpy( path, ext );
            path = strchr( path, '\0' );
        }
    }
    *path = '\0';
}

#else

/*
    For silly two choice DOS path characters / and \,
    we want to return a consistent path character.
*/

static unsigned pickup( unsigned c, unsigned *pc_of_choice )
{
    if( c == PC || c == ALT_PC ) {
        if( *pc_of_choice == '\0' ) *pc_of_choice = c;
        c = *pc_of_choice;
    }
    return( c );
}

/* create full MS-DOS path name from the components */

void _makepath( char *path, const char *drive,
                const char *dir, const char *fname, const char *ext )
{
    unsigned            first_pc = '\0';
    char *              pathstart = path;
    unsigned            ch;

    if( drive != NULL ) {
        if( *drive != '\0' ) {
            if ((*drive == '\\') && (drive[1] == '\\')) {
                strcpy(path, drive);
                path += strlen(drive);
            } else {
                *path++ = *drive;                               /* OK for MBCS */
                *path++ = ':';
            }
        }
    }
    *path = '\0';
    if( dir != NULL ) {
        if( *dir != '\0' ) {
            do {
                    ch = pickup( _mbsnextc(dir), &first_pc );
                    _mbvtop( ch, path );
                    path[_mbclen(path)] = '\0';
                    path = _mbsinc( path );
                    dir = _mbsinc( dir );
            } while( *dir != '\0' );
            /* if no path separator was specified then pick a default */
            if( first_pc == '\0' ) first_pc = PC;
            /* if dir did not end in '/' then put in a provisional one */
                if( (unsigned) (unsigned char *)(_mbsdec(pathstart,path)) == first_pc )
//                if( *(_mbsdec(pathstart,path)) == first_pc )
                    path--;
                else
                    *path = first_pc;
        }
    }

    /* if no path separator was specified thus far then pick a default */
    if( first_pc == '\0' ) first_pc = PC;
    if( fname != NULL ) {
            ch = _mbsnextc( fname );
            if( pickup(ch,&first_pc) != first_pc  &&  *path == first_pc )
                path++;

        while (*fname != '\0')
        {
        //do {
                ch = pickup( _mbsnextc(fname), &first_pc );
                _mbvtop( ch, path );
                path[_mbclen(path)] = '\0';
                path = _mbsinc( path );
                fname = _mbsinc( fname );
        } //while( *fname != '\0' );
    } else {
        if( *path == first_pc ) path++;
    }
    if( ext != NULL ) {
        if( *ext != '\0' ) {
            if( *ext != '.' )  *path++ = '.';
            while( *ext != '\0' ) *path++ = *ext++;     /* OK for MBCS */
        }
    }
    *path = '\0';
}
#endif

/****************************************************************************
*
* Description:  Implementation of fullpath() - returns fully qualified
*               pathname of a file.
*
****************************************************************************/

#define _WILL_FIT( c )  if(( (c) + 1 ) > size ) {       \
                            __set_errno( ERANGE );      \
                            return( NULL );             \
                        }                               \
                        size -= (c);

#if defined(__UNIX__) || defined(__CYGWIN__)
#define _IS_SLASH( c )  ((c) == '/')
#else
#define _IS_SLASH( c )  (( (c) == '/' ) || ( (c) == '\\' ))
#endif

char *_sys_fullpath( char *buff, const char *path, size_t size )
/*********************************************************************/
{

#if defined(__NT__)
    char *         filepart;
    DWORD               rc;

    if( stricmp( path, __F_NAME("con",L"con") ) == 0 ) {
        _WILL_FIT( 3 );
        return( strcpy( buff, __F_NAME("con",L"con") ) );
    }

    /*** Get the full pathname ***/
    rc = GetFullPathNameA( path, size, buff, &filepart );
    // If the buffer is too small, the return value is the size of
    // the buffer, in TCHARs, required to hold the path.
    // If the function fails, the return value is zero. To get extended error
    // information, call GetLastError.
    if( (rc == 0) || (rc > size) ) {
        __set_errno_nt();
        return( NULL );
    }

    return( buff );
#elif defined(__WARP__)
    APIRET      rc;
    char        root[4];    /* SBCS: room for drive, ':', '\\', and null */

    if (isalpha( path[0] ) && ( path[1] == ':' )
            && ( path[2] == '\\' ) )
    {
        int i;
        i = strlen( path );
        _WILL_FIT(i);
        strcpy( buff, path );
        return( buff );
    }

    /*
     * Check for x:filename.ext when drive x doesn't exist.  In this
     * case, return x:\filename.ext, not NULL, to be consistent with
     * MS and with the NT version of _fullpath.
     */
    if( isalpha( path[0] )  &&  path[1] == ':' ) {
        /*** We got this far, so path can't start with letter:\ ***/
        root[0] = (char) path[0];
        root[1] = ':';
        root[2] = '\\';
        root[3] = '\0';
        rc = DosQueryPathInfo( root, FIL_QUERYFULLNAME, buff, size );
        if( rc != NO_ERROR ) {
            /*** Drive does not exist; return x:\filename.ext ***/
            _WILL_FIT( strlen( &path[2] ) + 3 );
            buff[0] = root[0];
            buff[1] = ':';
            buff[2] = '\\';
            strcpy( &buff[3], &path[2] );
            return( buff );
        }
    }

    rc = DosQueryPathInfo( (PSZ)path, FIL_QUERYFULLNAME, buff, size );
    if( rc != 0 ) {
        __set_errno_dos( rc );
        return( NULL );
    }
    return( buff );
#elif defined(__UNIX__) || defined(__CYGWIN__)
    const char  *p;
    char        *q;
    size_t      len;
    char        curr_dir[_MAX_PATH];

    p = path;
    q = buff;
    if( ! _IS_SLASH( p[0] ) ) {
        if( getcwd( curr_dir, sizeof(curr_dir) ) == NULL ) {
            __set_errno( ENOENT );
            return( NULL );
        }
        len = strlen( curr_dir );
        _WILL_FIT( len );
        strcpy( q, curr_dir );
        q += len;
        if( q[-1] != '/' ) {
            _WILL_FIT( 1 );
            *(q++) = '/';
        }
        for(;;) {
            if( p[0] == '\0' ) break;
            if( p[0] != '.' ) {
                _WILL_FIT( 1 );
                *(q++) = *(p++);
                continue;
            }
            ++p;
            if( _IS_SLASH( p[0] ) ) {
                /* ignore "./" in directory specs */
                if( ! _IS_SLASH( q[-1] ) ) {
                    *q++ = '/';
                }
                ++p;
                continue;
            }
            if( p[0] == '\0' ) break;
            if( p[0] == '.' && _IS_SLASH( p[1] ) ) {
                /* go up a directory for a "../" */
                p += 2;
                if( ! _IS_SLASH( q[-1] ) ) {
                    return( NULL );
                }
                q -= 2;
                for(;;) {
                    if( q < buff ) {
                        return( NULL );
                    }
                    if( _IS_SLASH( *q ) ) break;
                    --q;
                }
                ++q;
                *q = '\0';
                continue;
            }
            _WILL_FIT( 1 );
            *(q++) = '.';
        }
        *q = '\0';
    } else {
        len = strlen( p );
        _WILL_FIT( len );
        strcpy( q, p );
    }
    return( buff );
#else
    const char *   p;
    char *         q;
    size_t              len;
    unsigned            path_drive_idx;
    char                curr_dir[_MAX_PATH];

    p = path;
    q = buff;
    _WILL_FIT( 2 );
    if( isalpha( p[0] ) && p[1] == ':' ) {
        path_drive_idx = ( tolower( p[0] ) - 'a' ) + 1;
        q[0] = p[0];
        q[1] = p[1];
        p += 2;
    } else {
  #if defined(__OS2__)
        ULONG   drive_map;
        OS_UINT os2_drive;

        if( DosQCurDisk( &os2_drive, &drive_map ) ) {
            __set_errno( ENOENT );
            return( NULL );
        }
        path_drive_idx = os2_drive;
  #else
        path_drive_idx = TinyGetCurrDrive() + 1;
  #endif
        q[0] = 'A' + ( path_drive_idx - 1 );
        q[1] = ':';
    }
    q += 2;
    if( ! _IS_SLASH( p[0] ) ) {
  #if defined(__OS2__)
        OS_UINT dir_len = sizeof( curr_dir );

        if( DosQCurDir( path_drive_idx, curr_dir, &dir_len ) ) {
            __set_errno( ENOENT );
            return( NULL );
        }
  #else
        tiny_ret_t rc;

        rc = TinyGetCWDir( curr_dir, path_drive_idx );
        if( TINY_ERROR( rc ) ) {
            __set_errno( ENOENT );
            return( NULL );
        }
  #endif
        len = strlen( curr_dir );
        if( curr_dir[0] != '\\' ) {
            _WILL_FIT( 1 );
            *(q++) = '\\';
        }
        _WILL_FIT( len );
        strcpy( q, curr_dir );
        q += len;
        if( q[-1] != '\\' ) {
            _WILL_FIT( 1 );
            *(q++) = '\\';
        }
        for(;;) {
            if( p[0] == '\0' ) break;
            if( p[0] != '.' ) {
                _WILL_FIT( 1 );
                *(q++) = *(p++);
                continue;
            }
            ++p;     // at least '.'
            if( _IS_SLASH( p[0] ) ) {
                /* ignore "./" in directory specs */
                if( ! _IS_SLASH( q[-1] ) ) {            /* 14-jan-93 */
                    *q++ = '\\';
                }
                ++p;
                continue;
            }
            if( p[0] == '\0' ) break;
            if( p[0] == '.' ) {  /* .. */
                ++p;
                if( _IS_SLASH( p[0] ) ){ /* "../" */
                    ++p;
                }
                if( ! _IS_SLASH( q[-1] ) ) {
                    return( NULL );
                }
                q -= 2;
                for(;;) {
                    if( q < buff ) {
                        return( NULL );
                    }
                    if( _IS_SLASH( *q ) ) break;
                    if( *q == ':' ) {
                        ++q;
                        *q = '\\';
                        break;
                    }
                    --q;
                }
                ++q;
                *q = '\0';
                continue;
            }
            _WILL_FIT( 1 );
            *(q++) = '.';
        }
        *q = '\0';
    } else {
        len = strlen( p );
        _WILL_FIT( len );
        strcpy( q, p );
    }
    /* force to all backslashes */
    for( q = buff; *q; ++q ) {
        if( *q == '/' ) {
            *q = '\\';
        }
    }
    return( buff );
#endif
}

char *_fullpath( char *buff, const char *path, size_t size )
/**********************************************************/
{
    char *ptr = NULL;

    if( buff == NULL ) {
        size = _MAX_PATH;
        ptr = malloc( size );
        if( ptr == NULL ) __set_errno( ENOMEM );
        buff = ptr;
    }
    if( buff != NULL ) {
        buff[0] = '\0';
        if( path == NULL || path[0] == '\0' ) {
            buff = getcwd( buff, size );
        } else {
            buff = _sys_fullpath( buff, path, size );
        }
        if( buff == NULL ) {
            if( ptr != NULL ) free( ptr );
        }
    }
    return buff;
}

/****************************************************************************
*
* Description:  Implementation of strupr().
*
****************************************************************************/

 char *strupr( char *str ) {
    char    *p;
    unsigned char   c;

    p = str;
    while( (c = *p) ) {
        c -= 'a';
        if( c <= 'z' - 'a' ) {
            c += 'A';
            *p = c;
        }
        ++p;
    }
    return( str );
}

/****************************************************************************
*
* Description:  Implementation of _memicmp().
*
****************************************************************************/

int _memicmp( const void *in_s1, const void *in_s2, size_t len )
    {
        const unsigned char *   s1 = (const unsigned char *)in_s1;
        const unsigned char *   s2 = (const unsigned char *)in_s2;
        unsigned char           c1;
        unsigned char           c2;

        for( ; len; --len )  {
            c1 = *s1;
            c2 = *s2;
            if( c1 >= 'A'  &&  c1 <= 'Z' )  c1 += 'a' - 'A';
            if( c2 >= 'A'  &&  c2 <= 'Z' )  c2 += 'a' - 'A';
            if( c1 != c2 ) return( c1 - c2 );
            ++s1;
            ++s2;
        }
        return( 0 );    /* both operands are equal */
    }

/****************************************************************************
*
* Description:  Implementation of _tell().
*
****************************************************************************/

off_t _tell( int handle )
{
    return( lseek( handle, 0L, SEEK_CUR ) );
}

/****************************************************************************
*
* Description:  Implements POSIX _filelength() function
*
****************************************************************************/

long _filelength( int handle )
{
    long                current_posn, file_len;

    current_posn = lseek( handle, 0L, SEEK_CUR );
    if( current_posn == -1L )
    {
        return( -1L );
    }
    file_len = lseek( handle, 0L, SEEK_END );
    lseek( handle, current_posn, SEEK_SET );

    return( file_len );
}



/****************************************************************************
*
* Description:  _strdate() writes time now as mm/dd/yy
*
****************************************************************************/

char *_strdate( char *buf )
{
    time_t time_of_day;
    struct tm now;

    time_of_day = time( NULL );
    localtime_r( &time_of_day, &now );
    strftime( buf, 9, "%D", &now );
    return( buf );
}



/****************************************************************************
*
* Description:  Writes time now as hh:mm:ss (ISO 8601 format)
*
****************************************************************************/

char *_strtime( char *buf )
{
    time_t      time_of_day;
    struct tm   now;

    time_of_day = time( NULL );
    localtime_r( &time_of_day, &now );
    strftime( buf, 9, "%T", &now );
    return( buf );
}

