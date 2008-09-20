
// this is a rudimentary Win32 include file.
// It contains prototypes of all Win32 functions which are
// called directly by JWasm.
// using this header file makes JWasm's source independant from
// the MS Platform SDK.

#ifdef __WATCOMC__
// __declspec(dllimport) is a problem if Win32 is linked statically (HX)
// and WLINK is used. MS link is smarter, it knows what to do if a symbol
// has been declared with dllimport and then turns out to be static.
#define WINBASEAPI
#else
#define WINBASEAPI __declspec(dllimport)
#endif
#define WINAPI __stdcall

#define MEM_COMMIT     0x1000
#define MEM_RELEASE    0x8000
#define PAGE_READWRITE 0x0004

WINBASEAPI uint_32 WINAPI GetTickCount( void );
WINBASEAPI char *  WINAPI GetCommandLineA( void );
WINBASEAPI void *  WINAPI VirtualAlloc( void *, uint_32, uint_32, uint_32 );
WINBASEAPI int     WINAPI VirtualFree( void *, uint_32, uint_32 );
