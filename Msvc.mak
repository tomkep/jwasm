
# this makefile (NMake) creates the JWasm Win32 binary with MSVC.
# it has been tested with:
# - MS VC++ Toolkit 2003
# - MS VC++ 6
# - MS VC++ 2005 EE (not recommended, also see definition of c_flags below)
#
# By setting DOS=1 one can additionally create a 32bit DOS binary.
# This requires the HXDEV package.

name = jwasm

DOS=0
WIN=1

# directory paths to adjust
# VCDIR  - root directory for VC compiler, linker, include and lib files
# W32LIB - directory for Win32 import library files (kernel32.lib)
# HXDIR  - for DOS=1 only: root directory to search for stub LOADPEX.BIN,
#          libs DKRNL32S.LIB + IMPHLP.LIB and tool PATCHPE.EXE.

VCDIR  = \msvc8
W32LIB = \Win32Inc\Lib
HXDIR  = \HX

!ifndef DEBUG
DEBUG=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=MSVCD
!else
OUTD=MSVCR
!endif
!endif

inc_dirs  = -IH -I"$(VCDIR)\include"

TRMEM=0

linker = $(VCDIR)\Bin\link.exe

!if $(DEBUG)
!if $(TRMEM)
extra_c_flags = -Zd -Od -DDEBUG_OUT -DTRMEM
!else
extra_c_flags = -Zd -Od -DDEBUG_OUT
!endif
!else
extra_c_flags = -O2 -Gs -DNDEBUG
#extra_c_flags = -Ox -DNDEBUG
!endif

c_flags =-D__NT__ $(extra_c_flags) $(c_flags64)

# if MSVC++ 2005 EE is used:
# 1. define __STDC_WANT_SECURE_LIB__=0 to avoid "deprecated" warnings
# 2. define -GS- to disable security checks
#c_flags =-D__NT__ $(extra_c_flags) -D__STDC_WANT_SECURE_LIB__=0 -GS-

#lflags stuff
#########
LOPT = /NOLOGO
!if $(DEBUG)
LOPTD = /debug
!endif

lflagsd = $(LOPTD) /SUBSYSTEM:CONSOLE $(LOPT) /map:$^*.map /Libpath:$(HXDIR)\lib /OPT:NOWIN98
lflagsw = $(LOPTD) /SUBSYSTEM:CONSOLE $(LOPT) /map:$^*.map /OPT:NOWIN98

CC=@$(VCDIR)\bin\cl.exe -c -nologo $(inc_dirs) $(c_flags)

.c{$(OUTD)}.obj:
	$(CC) -Fo$* $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/assemble.obj $(OUTD)/assume.obj  \
           $(OUTD)/directiv.obj $(OUTD)/posndir.obj  $(OUTD)/segment.obj \
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  \
           $(OUTD)/macro.obj    $(OUTD)/string.obj   $(OUTD)/condasm.obj \
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  \
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    \
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   \
           $(OUTD)/expans.obj   $(OUTD)/symbols.obj  $(OUTD)/labels.obj  \
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    \
           $(OUTD)/insthash.obj $(OUTD)/branch.obj   $(OUTD)/queues.obj  \
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  \
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/omf.obj     \
           $(OUTD)/bin.obj      $(OUTD)/queue.obj    $(OUTD)/carve.obj   \
           $(OUTD)/omfgenms.obj $(OUTD)/omfio.obj    $(OUTD)/omfrec.obj  \
           $(OUTD)/omffixup.obj $(OUTD)/listing.obj  $(OUTD)/fatal.obj   \
           $(OUTD)/context.obj  $(OUTD)/extern.obj  \
!if $(TRMEM)
           $(OUTD)/trmem.obj    \
!endif
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   \
           $(OUTD)/dbgcv.obj
######

!if $(WIN)
TARGET1=$(OUTD)\$(name).exe
!endif
!if $(DOS)
TARGET2=$(OUTD)\$(name)d.exe
!endif

ALL: $(OUTD) $(TARGET1) $(TARGET2)

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).exe : $(proj_obj)
	$(linker) @<<
$(lflagsw) $(proj_obj)
/LIBPATH:"$(VCDIR)\Lib" /LIBPATH:"$(W32LIB)" kernel32.lib /OUT:$@
<<

$(OUTD)\$(name)d.exe : $(proj_obj)
	$(linker) @<<
$(lflagsd) /NODEFAULTLIB initw32.obj $(proj_obj) /LIBPATH:$(VCDIR)\Lib
libc.lib oldnames.lib /LIBPATH:$(HXDIR)\Lib dkrnl32s.lib imphlp.lib /STUB:$(HXDIR)\Bin\LOADPEX.BIN
/OUT:$@ /FIXED:NO
<<
	@$(HXDIR)\bin\patchpe $@

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) -Fo$* msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/special.h
	$(CC) -Fo$* parser.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
