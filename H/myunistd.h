/*
 *  myunistd.h
 *
 * this is a simplified version of unistd.h included in Open Watcom.
 *
 * =========================================================================
 */
#ifndef _MYUNISTD_H_INCLUDED
#define _MYUNISTD_H_INCLUDED

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#ifndef __GNUC__
#ifdef __UNIX__
 #include <unistd.h>
#else
#ifndef _IO_H_INCLUDED
 #include <io.h>
#endif
#endif
#endif

#ifdef __WATCOMC__
#define _close      close
//#define _filelength filelength
#define _lseek      lseek
#define _open       open
//#define _read       read
#define _tell       tell
#define _write      write
#endif

#ifdef __cplusplus
} /* End of extern "C" */
#endif
#endif
