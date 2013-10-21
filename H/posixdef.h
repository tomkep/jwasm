/****************************************************************************
*
*  Description: file for GNU (Posix) CRT. Included by globals.h
*               if __UNIX__ or __CYGWIN__ is defined.
*
****************************************************************************/

#define _stricmp strcasecmp
//#define _strcmpi strcasecmp /* not used */
//#define _strnicmp strncasecmp /* not used */
#ifndef __WATCOMC__
#define _memicmp strncasecmp
#endif

#define _ltoa   ltoa
#define _strupr strupr

/* v2.11: _fullpath() no longer used */
//char *_fullpath( char *, const char *, size_t );

#ifndef __WATCOMC__
#define _MAX_DRIVE      48           /*  maximum length of node name w/ '\0' */
#define _MAX_DIR        FILENAME_MAX /*  maximum length of subdirectory      */
#define _MAX_FNAME      FILENAME_MAX /*  maximum length of a file name       */
#define _MAX_EXT        FILENAME_MAX /*  maximum length of a file extension  */
#endif

