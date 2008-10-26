
# the makefile creates the Linux binary of JWasm.
# Open Watcom v1.7 is needed for the build process.

name = JWasm

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=DebLnx
!else
OUTD=RelLnx
!endif

# calling convention for compiler: s=Stack, r=Register
CCV=r

inc_dirs  = -IH -I\Watcom\LH

# to track memory leaks, the Open Watcom TRMEM module can be included
TRMEM=0

linker = wlink.exe

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
LOPT = op quiet
!if $(DEBUG)
LOPTD = debug dwarf op symfile 
!endif

lflagsl = $(LOPTD) sys linux $(LOPT) op map=$^*

CC = wcc386 -q -3$(CCV) -bc -bt=linux $(inc_dirs) $(extra_c_flags) -fo$@

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
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
           $(OUTD)/autodept.obj $(OUTD)/context.obj
######

ALL: $(OUTD) $(OUTD)/$(name)

$(OUTD):
	@if not exist $(OUTD) mkdir $(OUTD)

$(OUTD)/$(name) : $(proj_obj)
	@%write  $^*.lnk $(lflagsl)
	@%append $^*.lnk file { $(proj_obj) }
	@%append $^*.lnk name $@.
	$(linker) @$^*.lnk

$(OUTD)/msgtext.obj: msgtext.c H/msgtext.h H/usage.h H/banner.h
	$(CC) msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/reswords.h
	$(CC) parser.c
    
######

clean:
	@erase $(OUTD)\*.
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
