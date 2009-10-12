
# this makefile (WMake) creates the Linux binary of JWasm.
# Open Watcom v1.7 or v1.8 is used.

name = jwasm

# support for 64bit?
!ifdef AMD64
c_flags64=-DAMD64_SUPPORT=1
!endif

!ifndef DEBUG
DEBUG=0
!endif

!ifndef OUTD
!if $(DEBUG)
OUTD=OWLinuxD
!else
OUTD=OWLinuxR
!endif
!endif

# calling convention for compiler: s=Stack, r=Register
CCV=r

WATCOM=\Watcom

inc_dirs  = -IH -I$(WATCOM)\LH

# to track memory leaks, the Open Watcom TRMEM module can be included
TRMEM=0

LINK = $(WATCOM)\Binnt\wlink.exe

#cflags stuff
#########
extra_c_flags =
!if $(DEBUG)
extra_c_flags += -od -d2 -DDEBUG_OUT
!else
extra_c_flags += -ot -s -DNDEBUG
!endif

#lflags stuff
#########
!if $(DEBUG)
LOPTD = debug dwarf op symfile
!endif

CC = $(WATCOM)\Binnt\wcc386 -q -3$(CCV) -bc -bt=linux $(inc_dirs) $(extra_c_flags) $(c_flags64) -fo$@

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
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   &
           $(OUTD)/dbgcv.obj
######

ALL: $(OUTD) $(OUTD)/$(name)

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name) : $(proj_obj)
	$(LINK) @<<
format elf
runtime linux
$(LOPTD) $(LOPT) op map=$^*
libpath $(WATCOM)/lib386
libpath $(WATCOM)/lib386/linux
op exportall, norelocs, quiet, stack=0x20000 
file { $(proj_obj) }
name $@.
<<

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h H/globals.h
	$(CC) msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/special.h
	$(CC) parser.c

######

clean:
	@erase $(OUTD)\*.
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
