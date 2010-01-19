
# this makefile in OW WMake style creates JWASM.EXE (Win32) and optionally
# JWASMD.EXE (DOS).
# tools used:
# - Open Watcom v1.8
# - HXDEV (only needed if DOS=1 is set below to create JWASMD.EXE)
#
# to create a debug version, run "WMake debug=1".

WIN=1
DOS=1

# Open Watcom root directory

WATCOM = \Watcom

# if DOS=1, HXDIR must contain the HX root directory

HXDIR = \HX

name = JWasm

!ifndef DEBUG
DEBUG=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=Debug
!else
OUTD=Release
!endif
!endif

# calling convention for compiler: s=Stack, r=register
# r will create a slightly smaller binary
CCV=r

inc_dirs  = -IH -I$(WATCOM)\H

# to track memory leaks, the Open Watcom TRMEM module can be included.
# it's useful only if FASTMEM=0 is set, though, otherwise most allocs 
# won't use the C heap.
!ifndef TRMEM
TRMEM=0
!endif

LINK = $(WATCOM)\binnt\wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
!if $(TRMEM)
extra_c_flags += -od -d2 -DDEBUG_OUT -DTRMEM -DFASTMEM=0
!else
extra_c_flags += -od -d2 -w3 -DDEBUG_OUT
!endif
!else
extra_c_flags += -obmilrt -s -DNDEBUG
!endif

#########

LOPT = op quiet
!if $(DEBUG)
# for OW v1.8, the debug version needs user32.lib to resolve CharUpperA()
# without it, WD(W) will crash immediately.
LOPTD = debug dwarf op symfile lib user32.lib
!endif

lflagsw = $(LOPTD) system nt $(LOPT) op map=$^*

CC=$(WATCOM)\binnt\wcc386 -q -3$(CCV) -zc -bc -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@

.c{$(OUTD)}.obj:
	$(CC) $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/assemble.obj $(OUTD)/assume.obj  &
           $(OUTD)/directiv.obj $(OUTD)/posndir.obj  $(OUTD)/segment.obj &
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  &
           $(OUTD)/macro.obj    $(OUTD)/string.obj   $(OUTD)/condasm.obj &
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  &
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    &
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   &
           $(OUTD)/expans.obj   $(OUTD)/symbols.obj  $(OUTD)/labels.obj  &
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    &
           $(OUTD)/insthash.obj $(OUTD)/branch.obj   $(OUTD)/queues.obj  &
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  &
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/omf.obj     &
           $(OUTD)/bin.obj      $(OUTD)/queue.obj    $(OUTD)/carve.obj   &
           $(OUTD)/omfgenms.obj $(OUTD)/omfio.obj    $(OUTD)/omfrec.obj  &
           $(OUTD)/omffixup.obj $(OUTD)/listing.obj  $(OUTD)/fatal.obj   &
           $(OUTD)/context.obj  $(OUTD)/extern.obj  &
!if $(DEBUG)
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
!endif
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   &
           $(OUTD)/apiemu.obj   $(OUTD)/dbgcv.obj
######

!if $(WIN)
TARGET1=$(OUTD)/$(name).exe
!endif
!if $(DOS)
TARGET2=$(OUTD)/$(name)d.exe
!endif

ALL: $(OUTD) $(TARGET1) $(TARGET2)

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name).exe: $(proj_obj)
	$(LINK) @<<
$(lflagsw) file { $(proj_obj) } name $@ op stack=0x20000, heapsize=0x100000, norelocs com stack=0x1000
<<
!if $(DEBUG)
	@copy $(OUTD)\$(name).exe TEST\*.* >NUL
	@copy $(OUTD)\$(name).sym TEST\*.* >NUL
!endif

$(OUTD)/$(name)d.exe: $(proj_obj)
	$(LINK) @<<
$(LOPTD)
format windows nt 
runtime console 
file { $(proj_obj) }
name $@
Libpath $(WATCOM)\lib386 
Libpath $(WATCOM)\lib386\nt
Libpath $(HXDIR)\lib
Library imphlp.lib, dkrnl32s.lib 
Libfile cstrtwhx.obj 
$(LOPT)
op map=$^*, stub=$(HXDIR)\Bin\loadpex.bin, stack=0x40000, heapsize=0x100000
<<
	@$(HXDIR)\Bin\patchpe.exe $@

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/special.h
	$(CC) parser.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
