
# this NMAKE makefile creates JWasm Windows and DOS binaries with MSVC.

name = jwasm

DOS=0
WIN=1

# directory paths to adjust
# VCDIR  - root directory for VC compiler, linker, include and lib files
# SDKDIR - root directory for Windows include and lib files
# HXDIR  - for DOS=1 only: root directory to search for stub LOADPEX.BIN
#          and libs DKRNL32S.LIB + IMPHLP.LIB

VCDIR  = \msvc8
SDKDIR = \Microsoft SDK
HXDIR  = \asm\hx

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=MSVCD
!else
OUTD=MSVCR
!endif

inc_dirs  = -IH -I"$(VCDIR)\include" -I"$(SDKDIR)\Include"

TRMEM=0

linker = $(VCDIR)\Bin\link.exe

#cflags stuff
#########
c_flags =-D_STANDALONE_ -D__NT__ -DFASTPASS=1
!if $(DEBUG)
!if $(TRMEM)
extra_c_flags = $(c_flags) -Zd -Od -DDEBUG_OUT -DTRMEM
!else
extra_c_flags = $(c_flags) -Zd -Od -DDEBUG_OUT
!endif
!else
extra_c_flags = $(c_flags) -Ox -DNDEBUG
!endif

#lflags stuff
#########
LOPT = /NOLOGO
!if $(DEBUG)
LOPTD = /debug
!endif

lflagsd = $(LOPTD) /SUBSYSTEM:CONSOLE $(LOPT) /map:$^*.map /Libpath:$(HXDIR)\lib /OPT:NOWIN98
lflagsw = $(LOPTD) /SUBSYSTEM:CONSOLE $(LOPT) /map:$^*.map /OPT:NOWIN98

CC=@$(VCDIR)\bin\cl.exe -c -nologo $(inc_dirs) $(extra_c_flags) -Fo$@

.c{$(OUTD)}.obj:
	 $(CC) $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/write.obj    $(OUTD)/fatal.obj   \
           $(OUTD)/directiv.obj $(OUTD)/posndir.obj  $(OUTD)/segment.obj \
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  \
           $(OUTD)/msgtext.obj  $(OUTD)/macro.obj    $(OUTD)/condasm.obj \
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  \
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    \
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   \
           $(OUTD)/symbols.obj  $(OUTD)/tbyte.obj    $(OUTD)/labels.obj  \
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    \
           $(OUTD)/insthash.obj $(OUTD)/jumps.obj    $(OUTD)/queues.obj  \
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  \
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/omf.obj     \
           $(OUTD)/bin.obj      $(OUTD)/queue.obj    $(OUTD)/carve.obj   \
           $(OUTD)/omfgenms.obj $(OUTD)/omfio.obj    $(OUTD)/omfrec.obj  \
           $(OUTD)/omffixup.obj \
!if $(TRMEM)
           $(OUTD)/trmem.obj    \
!endif
           $(OUTD)/autodept.obj
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

$(OUTD)\$(name).exe : H/opcodes.gh $(proj_obj)
	$(linker) @<<
$(lflagsw) $(proj_obj)
/LIBPATH:"$(VCDIR)\Lib" /LIBPATH:"$(SDKDIR)\Lib" kernel32.lib /OUT:$@
<<

$(OUTD)\$(name)d.exe : H/opcodes.gh $(proj_obj)
	$(linker) @<<
$(lflagsd) /NODEFAULTLIB initw32.obj $(proj_obj) /LIBPATH:$(VCDIR)\Lib
libc.lib oldnames.lib dkrnl32s.lib imphlp.lib /STUB:$(HXDIR)\Bin\LOADPEX.BIN
/OUT:$@ /FIXED:NO
<<
	@$(HXDIR)\bin\patchpe $@

$(OUTD)/msgtext.obj: msgtext.c H/msgtext.h H/usage.h H/banner.h
	$(CC) msgtext.c

######

H/opcodes.gh: opcodes.tok
	Bin\mkopcode.exe opcodes.tok H/opcodes.gh

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
