
# this makefile creates the 32bit OS/2 binary of JWasm.
# tools used:
# - Open Watcom v1.7a/v1.8

name = JWasm

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=OWOS2D
!else
OUTD=OWOS2R
!endif

# calling convention for compiler: s=Stack, r=register
# r will create a slightly smaller binary
CCV=r

inc_dirs  = -IH

LINK = wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
extra_c_flags += -od -d2 -DDEBUG_OUT
!else
extra_c_flags += -obmilrt -s -DNDEBUG
!endif

#########

LOPT = op quiet
!if $(DEBUG)
LOPTD = debug dwarf op symfile
!endif

lflagso = $(LOPTD) system os2v2 $(LOPT) op map=$^*

CC=wcc386 -q -3$(CCV) -bc -bt=os2 $(inc_dirs) $(extra_c_flags) -fo$@

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
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   &
           $(OUTD)/dbgcv.obj
######

TARGET1=$(OUTD)/$(name).exe

ALL: $(OUTD) $(TARGET1)

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(TARGET1): $(proj_obj)
	$(LINK) @<<
$(lflagso) file { $(proj_obj) } name $@ op stack=0x20000
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
