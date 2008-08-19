
# the makefile creates JWASM.EXE (JWASM)
# Open Watcom v1.7 is needed for the build process.
# Optionally - if DOS=1 is set below - the DOS version of JWasm, JWASMD.EXE,
# can be created.

name = JWasm

DOS=1
WIN=1

# if DOS=1 is set, then HXDEV is required and the HXDIR path below has to be
# adjusted before running WMAKE. Additionally, HX's Open Watcom support must
# be installed ("system hxnts").

HXDIR = \asm\hx

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
# it's not very useful, though, since most allocs won't use the C heap.
TRMEM=0

linker = wlink.exe

#cflags stuff
#########
extra_c_flags =-D_STANDALONE_ -DFASTPASS=1
!if $(DEBUG)
!if $(TRMEM)
extra_c_flags += -od -d2 -DDEBUG_OUT -DTRMEM
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

lflagsd = $(LOPTD) system hxnts $(LOPT) op map=$^* Libpath $(HXDIR)\lib op stub=$(HXDIR)\bin\loadpex.bin
lflagsw = $(LOPTD) system nt $(LOPT) op map=$^*

CC=wcc386 -q -3$(CCV) -bc -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@

.c{$(OUTD)}.obj:
   $(CC) $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/write.obj    $(OUTD)/fatal.obj   &
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
           $(OUTD)/omffixup.obj &
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
           $(OUTD)/autodept.obj
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

$(OUTD)/$(name).exe: H/opcodes.gh $(proj_obj)
	$(linker) @<<
$(lflagsw) file { $(proj_obj) } name $@ op stack=0x20000 op norelocs com stack=0x1000 
<<
!if $(DEBUG)        
	@copy $(OUTD)\$(name).exe TEST\*.* >NUL
	@copy $(OUTD)\$(name).sym TEST\*.* >NUL
!endif        

$(OUTD)/$(name)d.exe: H/opcodes.gh $(proj_obj)
	$(linker) @<<
$(lflagsd) file { $(proj_obj) } name $@
<<
	@$(HXDIR)\bin\patchpe $@

$(OUTD)/msgtext.obj: msgtext.c H/msgtext.h H/usage.h H/banner.h
	$(CC) msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/opcodes.gh
	$(CC) parser.c
    
######

H/opcodes.gh: opcodes.tok
	Bin\mkopcode.exe opcodes.tok $^@

clean:
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.map
