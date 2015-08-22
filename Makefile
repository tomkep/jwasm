
# this makefile creates JWASM.EXE (Win32) and optionally JWASMD.EXE (DOS).
# tools used:
# - Open Watcom v1.7
# - HXDEV (optionally, only if DOS=1 is set below to create JWASMD.EXE)

name = JWasm

DOS=1
WIN=1

# if DOS=1, the Open Watcom and HX directories must be set below.

WATCOM = \Watcom
HXDIR = \HX

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=Debug
!else
OUTD=Release
!endif

# calling convention for compiler: s=Stack, r=register
# r will create a slightly smaller binary
CCV=r

inc_dirs  = -IH

# to track memory leaks, the Open Watcom TRMEM module can be included.
# it's useful only if FASTMEM=0 is set, though, otherwise most allocs 
# won't use the C heap.
!ifndef TRMEM
TRMEM=0
!endif

linker = wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
!if $(TRMEM)
extra_c_flags += -od -d2 -DDEBUG_OUT -DTRMEM -DFASTMEM=0
!else
extra_c_flags += -od -d2 -DDEBUG_OUT
!endif
!else
extra_c_flags += -obmilrt -s -DNDEBUG
!endif

#########

LOPT = op quiet
!if $(DEBUG)
LOPTD = debug dwarf op symfile
!endif

lflagsd = $(LOPTD) format windows nt runtime console Libpath $(HXDIR)\Lib Libpath $(WATCOM)\lib386 Libpath $(WATCOM)\lib386\nt library dkrnl32s.lib libfile cstrtwhx.obj $(LOPT) op map=$^* op stub=$(HXDIR)\Bin\loadpex.bin, stack=0x40000
lflagsw = $(LOPTD) system nt $(LOPT) op map=$^*

CC=wcc386 -q -3$(CCV) -bc -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@

.c{$(OUTD)}.obj:
   $(CC) $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/assemble.obj $(OUTD)/assume.obj  &
           $(OUTD)/directiv.obj $(OUTD)/posndir.obj  $(OUTD)/segment.obj &
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  &
           $(OUTD)/msgtext.obj  $(OUTD)/macro.obj    $(OUTD)/condasm.obj &
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  &
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    &
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   &
           $(OUTD)/symbols.obj  $(OUTD)/tbyte.obj    $(OUTD)/labels.obj  &
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    &
           $(OUTD)/insthash.obj $(OUTD)/jumps.obj    $(OUTD)/queues.obj  &
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  &
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/omf.obj     &
           $(OUTD)/bin.obj      $(OUTD)/queue.obj    $(OUTD)/carve.obj   &
           $(OUTD)/omfgenms.obj $(OUTD)/omfio.obj    $(OUTD)/omfrec.obj  &
           $(OUTD)/omffixup.obj $(OUTD)/listing.obj  $(OUTD)/fatal.obj   &
!if $(DEBUG)
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
!endif
           $(OUTD)/autodept.obj $(OUTD)/context.obj  $(OUTD)/extern.obj
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
	$(linker) @<<
$(lflagsw) file { $(proj_obj) } name $@ op stack=0x20000 op norelocs com stack=0x1000
<<
!if $(DEBUG)
	@copy $(OUTD)\$(name).exe TEST\*.* >NUL
	@copy $(OUTD)\$(name).sym TEST\*.* >NUL
!endif

$(OUTD)/$(name)d.exe: $(proj_obj)
	$(linker) @<<
$(lflagsd) file { $(proj_obj) } name $@
<<
	@$(HXDIR)\Bin\patchpe.exe $@

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/reswords.h
	$(CC) parser.c
    
######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
