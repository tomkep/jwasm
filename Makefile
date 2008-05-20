
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

!ifndef jwasm_autodepends
jwasm_autodepends = .AUTODEPEND
!endif

!if $(DEBUG)
OUTD=Debug
!else
OUTD=Release
!endif

# calling convention: s=Stack, r=register
CCV=r

inc_dirs  = -I$(OUTD) -IH -IWomp -IWatcom

WRES=Wres
WMPD=Womp
WATD=Watcom

TRMEM=0

libs = $(WRES)/wres.lib
linker = Bin\wlink.exe

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
extra_c_flags += -ox
!endif

extra_c_flags_genmsomf = -DMSG_WOMP_RC_BASE=200
extra_c_flags_fixup = -DMSG_WOMP_RC_BASE=200

#lflags stuff
#########
LOPT = op quiet
!if $(DEBUG)
LOPTD = debug dwarf op symfile 
!endif

lflagsd = $(LOPTD) system hxnts $(LOPT) op map=$^* Libpath $(HXDIR)\lib lib $(libs)
lflagsw = $(LOPTD) system nt $(LOPT) op map=$^* lib $(libs)

.c{$(OUTD)}.obj:
	@if not exist $(OUTD) mkdir $(OUTD)
    wcc386 -q -3$(CCV) -bc -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@ $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/write.obj    $(OUTD)/fatal.obj   &
           $(OUTD)/womputil.obj $(OUTD)/direct.obj   $(OUTD)/posndir.obj &
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  &
           $(OUTD)/jwasmmsg.obj $(OUTD)/macro.obj    $(OUTD)/condasm.obj &
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  &
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/for.obj     &
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/asmline.obj &
           $(OUTD)/symbols.obj  $(OUTD)/tbyte.obj    $(OUTD)/labels.obj  &
           $(OUTD)/asmfixup.obj $(OUTD)/asmmatch.obj $(OUTD)/data.obj    &
           $(OUTD)/insthash.obj $(OUTD)/jumps.obj    $(OUTD)/queues.obj  &
           $(OUTD)/finger.obj   $(OUTD)/hll.obj      $(OUTD)/proc.obj    &
           $(OUTD)/segment.obj  &
           $(WMPD)/objio.obj    $(WMPD)/carve.obj    $(WMPD)/genmsomf.obj &
           $(WMPD)/objrec.obj   $(WMPD)/queue.obj    $(WMPD)/fixup.obj   &
           $(WMPD)/lifix.obj    &
!if $(TRMEM)
           $(WATD)/trmem.obj    &
!endif
           $(WATD)/autodept.obj $(WATD)/swchar.obj   
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
$(OUTD)/$(name)d.exe : $(OUTD)/fullops.gh $(OUTD)/$(name).res $(proj_obj)
        @%write  $^*.lnk $(lflagsd)
        @%append $^*.lnk file { $(proj_obj) }
        @%append $^*.lnk name $@
        $(linker) @$^*.lnk
        @$(HXDIR)\bin\patchpe $@
        @wstrip -a -r -q $@ . $(OUTD)/$(name).res
!endif        

!if $(WIN)
$(OUTD)/$(name).exe : $(OUTD)/fullops.gh $(OUTD)/$(name).res $(proj_obj)
        @%write  $^*.lnk $(lflagsw)
        @%append $^*.lnk file { $(proj_obj) }
        @%append $^*.lnk name $@
        $(linker) @$^*.lnk
        @wstrip -a -r -q $@ . $(OUTD)/$(name).res
!if $(DEBUG)        
        @copy $(OUTD)\$(name).exe TEST\*.* >NUL
        @copy $(OUTD)\$(name).sym TEST\*.* >NUL
!endif        
!endif        

$(OUTD)/$(name).res : h/$(name).rc $(OUTD)/usage.rc $(OUTD)/usagej.rc $(OUTD)/msg.gh $(OUTD)/jwasmmsg.gh $(OUTD)/wmpmsg.gh
        wrc -q -r -zk0 -bt=windows -D_STANDALONE_ -iH -iWatcom -i$(OUTD) $[@ -fo=$^@

$(OUTD)/usage.rc : h/usage.sp
        @%write $(OUTD)/usage.tmp -kENGLISH $(wsplice_keys) -t8 -f "%+(MSG_USE_E_BASE+%$#-1), \"%s\"" $< $^@
        Bin\wsplice @$(OUTD)/usage.tmp

$(OUTD)/usagej.rc : h/usage.sp
        @%write $(OUTD)/usagej.tmp -kJAPANESE $(wsplice_keys) -t8 -f "%+(MSG_USE_J_BASE+%$#-1), \"%s\"" $< $^@
        Bin\wsplice @$(OUTD)/usagej.tmp

$(OUTD)/msg.gh : h/shared.msg
        vi -s h/makemsg.vi -p"$^@ MSG_SHARE_RC_BASE" $[@

$(OUTD)/jwasmmsg.gh : h/jwasm.msg
        vi -s h/makemsg.vi -p"$^@ MSG_JWASM_RC_BASE" $[@

$(OUTD)/wmpmsg.gh : h/womp.msg
        vi -s h/makemsg.vi -p"$^@ MSG_WOMP_RC_BASE" $[@

######

$(OUTD)/fullops.gh: fullops.tok inlnops.tok Bin/mkopcode.exe
        $]@ fullops.tok inlnops.tok $^@

clean:
	@erase $(OUTD)\*.obj
	@erase $(OUTD)\*.exe
	@erase $(OUTD)\*.map
	@erase $(OUTD)\*.gh
	@erase $(OUTD)\*.rc
	@erase $(OUTD)\*.res
