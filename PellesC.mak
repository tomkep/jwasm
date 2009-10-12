
# This makefile creates JWasm Win32 binary with PellesC. Enter
#   pomake -f PellesC.mak
#
# Optionally, with 'pomake -f PellesC.mak DOS' one can additionally
# create a JWasm DOS binary. This needs the HXDEV package (see HXDIR
# below).

name = jwasm

# directory paths to adjust
# PODIR  - root directory for compiler, linker, include and lib files

PODIR  = \PellesC

!ifdef DEBUG
OUTD=POCD
extra_c_flags = -Zi -DDEBUG_OUT
LOPTD = /debug
!else
OUTD=POCR
extra_c_flags = -O2 -DNDEBUG
LOPTD =
!endif

inc_dirs = -IH -I"$(PODIR)\include"

LINK = $(PODIR)\Bin\polink.exe

c_flags =-D__NT__ -Ze $(extra_c_flags)

lflagsw = $(LOPTD) /SUBSYSTEM:CONSOLE /map:$*.map

CC=$(PODIR)\bin\pocc.exe $(inc_dirs) $(c_flags)

.c{$(OUTD)}.obj:
	@$(CC) -J -Fo$*.obj $<

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
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   \
           $(OUTD)/apiemu.obj   $(OUTD)/dbgcv.obj
######

!ifdef DOS
# directory where HXDEV has been installed.
HXDIR=\hx
DOSTARG=$(OUTD)\$(name)d.exe
lflagsd = $(LOPTD) /NODEFAULTLIB /FIXED:NO /SUBSYSTEM:CONSOLE /map:$*.map /STUB:$(HXDIR)\bin\loadpex.bin /LIBPATH:$(HXDIR)\Lib dkrnl32s.lib imphlp.lib /STACK:0x40000,0x1000 /HEAP:0x1000,0x1000
!endif

all: $(OUTD) $(OUTD)\$(name).exe $(DOSTARG)

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)\$(name).exe : $(proj_obj)
	$(LINK) $(lflagsw) $(proj_obj) /LIBPATH:$(PODIR)\Lib /LIBPATH:$(PODIR)\Lib\Win /OUT:$@

$(OUTD)\$(name)d.exe : $(proj_obj)
	@$(LINK) $(lflagsd) $(HXDIR)\Lib\initw32.obj $(proj_obj) /LIBPATH:$(PODIR)\Lib crt.lib /OUT:$@
	@$(HXDIR)\bin\patchpe $*.exe

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
