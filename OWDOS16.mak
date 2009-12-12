
# this makefile creates a DOS 16-bit real-mode version of JWasm (JWASMR.EXE).
# tools used:
# - Open Watcom v1.7a/v1.8

name = JWasm

WATCOM = \Watcom

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=OWDOS16D
!else
OUTD=OWDOS16R
!endif

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
extra_c_flags += -od -d2 -DDEBUG_OUT -DTRMEM
!else
extra_c_flags += -od -d2 -DDEBUG_OUT
!endif
!else
extra_c_flags += -obmilrs -s -DNDEBUG
!endif

#########

!if $(DEBUG)
LOPTD = debug dwarf op symfile
!endif

lflagsd = $(LOPTD) sys dos op map=$^*, stack=0x4000

CC=$(WATCOM)\binnt\wcc -q -0 -w3 -zc -ml -bc -bt=dos $(inc_dirs) $(extra_c_flags) -fo$@ -DFASTMEM=0 -DFASTPASS=0 -DCOFF_SUPPORT=0 -DELF_SUPPORT=0 -DAMD64_SUPPORT=0 -DSSE4SUPP=0 -zt=10000

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
           $(OUTD)/dbgcv.obj
######

TARGET=

ALL: $(OUTD) $(OUTD)/$(name)r.exe

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name)r.exe: $(proj_obj)
	$(LINK) @<<
$(lflagsd) file { $(proj_obj) } name $@
<<

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/special.h
	$(CC) parser.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
