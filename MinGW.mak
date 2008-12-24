
# This makefile (WMake) creates the JWasm Win32 binary with MinGW.

BINDIR=\MSys\MinGW\Bin

name = jwasm

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=MinGWD
!else
OUTD=MinGWR
!endif

inc_dirs  = -I H

#cflags stuff
#########
c_flags =
!if $(DEBUG)
extra_c_flags = $(c_flags) -DDEBUG_OUT
!else
extra_c_flags = $(c_flags) -DNDEBUG -O2
!endif

#lflags stuff
#########
LOPT = 
!if $(DEBUG)
LOPTD = 
!endif

lflagsw = $(LOPTD) $(LOPT) 

CC=$(BINDIR)\gcc.exe -c $(inc_dirs) $(extra_c_flags)

.c{$(OUTD)}.obj:
	 $(CC) -o$*.obj $<

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
           $(OUTD)/autodept.obj $(OUTD)/context.obj  $(OUTD)/extern.obj
######

TARGET1=$(OUTD)\$(name).exe 

ALL: $(OUTD) $(TARGET1)

$(OUTD):
	@mkdir $(OUTD)

$(OUTD)/$(name).exe : $(proj_obj)
#	$(BINDIR)\gcc.exe $(proj_obj) -s -o $*.exe -Wl,-Map $*.map
	$(BINDIR)\gcc.exe $(proj_obj) -s -o $*.exe

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h
	$(CC) -o$*.obj msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/reswords.h
	$(CC) -o$*.obj parser.c

######

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
