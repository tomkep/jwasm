
# the makefile creates JWASM.EXE (JWASM)
# Open Watcom v1.7 is needed for the build process.
#
# Optionally - if DOS=1 is set below - a DOS version of JWasm, JWASMD.EXE,
# can be created. Then HXDEV is required and the HXDIR path below has to be
# adjusted before running WMAKE. HX's Open Watcom support must be installed
# ("system hxnts").

name = jwasm
host_OS=nt

DOS=1
WIN=1

HXDIR = \asm\hx

!ifndef DEBUG
DEBUG=1
!endif

!if $(DEBUG)
OUTD=Debug
!else
OUTD=Release
!endif

# calling convention for compiler: s=Stack, r=register
# r creates a slightly smaller binary
CCV=r

inc_dirs  = -I$(OUTD) -IH

# to track memory leaks, the Open Watcom TRMEM module can be included
TRMEM=0

libs = 
linker = wlink.exe

#cflags stuff
#########
extra_c_flags =-D_STANDALONE_
!if $(DEBUG)
!if $(TRMEM)
extra_c_flags += -od -d2 -DDEBUG_OUT -DTRMEM
!else
extra_c_flags += -od -d2 -DDEBUG_OUT
!endif
!else
extra_c_flags += -ot -s
!endif

#lflags stuff
#########
LOPT = op quiet
!if $(DEBUG)
LOPTD = debug dwarf op symfile 
!endif

lflagsd = $(LOPTD) system hxnts $(LOPT) op map=$^* Libpath $(HXDIR)\lib
lflagsw = $(LOPTD) system nt $(LOPT) op map=$^*

.c{$(OUTD)}.obj:
	@if not exist $(OUTD) mkdir $(OUTD)
    wcc386 -q -3$(CCV) -bc -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@ $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/write.obj    $(OUTD)/fatal.obj   &
           $(OUTD)/direct.obj   $(OUTD)/posndir.obj  &
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  &
           $(OUTD)/jwasmmsg.obj $(OUTD)/macro.obj    $(OUTD)/condasm.obj &
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  &
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    &
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   &
           $(OUTD)/symbols.obj  $(OUTD)/tbyte.obj    $(OUTD)/labels.obj  &
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    &
           $(OUTD)/insthash.obj $(OUTD)/jumps.obj    $(OUTD)/queues.obj  &
           $(OUTD)/finger.obj   $(OUTD)/hll.obj      $(OUTD)/proc.obj    &
           $(OUTD)/segment.obj  $(OUTD)/coff.obj     $(OUTD)/omf.obj     &
           $(OUTD)/queue.obj    $(OUTD)/carve.obj    $(OUTD)/omffixup.obj&
           $(OUTD)/omfgenms.obj $(OUTD)/omfio.obj    $(OUTD)/omfrec.obj  &
!if $(TRMEM)
           $(OUTD)/trmem.obj    &
!endif
           $(OUTD)/autodept.obj
######

!ifeq release 0
wsplice_keys = -kIS_RC -k$(host_OS) -kHIDDEN
!else
wsplice_keys = -kIS_RC -k$(host_OS)
!endif

!if $(DOS) && $(WIN)
ALL: $(OUTD)/$(name).exe $(OUTD)/$(name)d.exe
!endif

!if $(DOS)
$(OUTD)/$(name)d.exe : $(OUTD)\opcodes.gh $(OUTD)/$(name).res $(proj_obj)
        @%write  $^*.lnk $(lflagsd)
        @%append $^*.lnk file { $(proj_obj) }
        @%append $^*.lnk name $@
        @%append $^*.lnk op res=$(OUTD)/$(name).res
        $(linker) @$^*.lnk
        @$(HXDIR)\bin\patchpe $@
!endif        

!if $(WIN)
$(OUTD)/$(name).exe : $(OUTD)\opcodes.gh $(OUTD)/$(name).res $(proj_obj)
        @%write  $^*.lnk $(lflagsw)
        @%append $^*.lnk file { $(proj_obj) }
        @%append $^*.lnk name $@ op stack=0x20000
        @%append $^*.lnk op res=$(OUTD)/$(name).res, norelocs com stack=0x1000 
        $(linker) @$^*.lnk
!if $(DEBUG)        
        @copy $(OUTD)\$(name).exe TEST\*.* >NUL
        @copy $(OUTD)\$(name).sym TEST\*.* >NUL
!endif        
!endif        

$(OUTD)/$(name).res : h/$(name).rc $(OUTD)/msg.gh
    wrc -q -r -bt=nt -D_STANDALONE_ -iH -i$(OUTD) $[@ -fo=$^@

$(OUTD)/msg.gh : h/jwasm.msg
    vi -s h/makemsg.vi -p"$^@ MSG_JWASM_RC_BASE" $[@

######

$(OUTD)/opcodes.gh: fullops.tok inlnops.tok Bin/mkopcode.exe
        $]@ fullops.tok inlnops.tok $^@

clean:
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.map
	@erase $(OUTD)\*.gh
	@erase $(OUTD)\*.rc
	@erase $(OUTD)\*.res
