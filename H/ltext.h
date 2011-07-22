
/* text constants for listing */

ltext(BYTE,     "Byte"   )
ltext(WORD,     "Word"   )
ltext(DWORD,    "DWord"  )
ltext(FWORD,    "FWord"  )
ltext(QWORD,    "QWord"  )
ltext(TBYTE,    "TByte"  )
ltext(PARA,     "Para"   )
ltext(OWORD,    "XmmWord")
ltext(PAGE,     "Page"   )
/* [L]NEAR|FAR[16|32|64] variants must be consecutive */
ltext(NEAR,     "Near"   )
ltext(NEAR16,   "Near16" )
ltext(NEAR32,   "Near32" )
#if AMD64_SUPPORT
ltext(NEAR64,   "Near64" )
#endif
ltext(FAR,      "Far"    )
ltext(FAR16,    "Far16"  )
ltext(FAR32,    "Far32"  )
#if AMD64_SUPPORT
ltext(FAR64,    "Far64"  )
#endif
ltext(LNEAR,    "L Near" )
ltext(LNEAR16,  "L Near16")
ltext(LNEAR32,  "L Near32")
#if AMD64_SUPPORT
ltext(LNEAR64,  "L Near64")
#endif
ltext(LFAR,     "L Far"  )
ltext(LFAR16,   "L Far16" )
ltext(LFAR32,   "L Far32" )
#if AMD64_SUPPORT
ltext(LFAR64,   "L Far64" )
#endif
ltext(PTR,      "Ptr"    )
ltext(PROC,     "Proc"   )
ltext(FUNC,     "Func"   )
ltext(NUMBER,   "Number" )
ltext(PRIVATE,  "Private")
ltext(STACK,    "Stack"  )
ltext(PUBLIC,   "Public" )
ltext(COMMON,   "Common" )
ltext(EXTERNAL, "External"  )
ltext(UNDEFINED,"Undefined")
ltext(GROUP,    "GROUP"  )
ltext(NOSEG,    "No Seg" )
ltext(TEXT,     "Text"   )
ltext(ALIAS,    "Alias"  )
ltext(ABS,      "Abs"    )
ltext(COMM,     "COMM"   )
ltext(VARARG,   "VARARG" )
/* language order must match enum lang_type in globals.h */
ltext(VOID,     ""       )
ltext(C,        "C"      )
ltext(SYSCALL,  "SYSCALL")
ltext(STDCALL,  "STDCALL")
ltext(PASCAL,   "PASCAL" )
ltext(FORTRAN,  "FORTRAN")
ltext(BASIC,    "BASIC"  )
ltext(FASTCALL, "FASTCALL")
