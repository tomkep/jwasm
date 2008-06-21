
# the makefile creates JWASM.EXE/JWASMD.EXE with MSVC.
# it's expected that directories MSVCR/MSVCD exist!
# please note that Open Watcom's VI editor is still used
# to create some header files.

name = jwasm
host_OS=nt

DOS=0
WIN=1

# directory paths to adjust
# OWDIR - needed for VI
# VCDIR - the compiler, linker, include and lib files are assumed to be there
# SDKDIR - where the windows include and lib files are
# HXDIR - for DOS=1 only: to search for LOADPE.BIN and DKRNL32S.LIB

OWDIR  = \Watcom
VCDIR  = \msvc8
SDKDIR = \Microsoft SDK
HXDIR  = \asm\hx

!ifndef DEBUG
DEBUG=0
!endif

!if $(DEBUG)
OUTD=MSVCD
!else
OUTD=MSVCR
!endif

inc_dirs  = -I$(OUTD) -IH -I$(VCDIR)\include -I"$(SDKDIR)\Include"

TRMEM=0

libs = 
linker = $(VCDIR)\Bin\link.exe

#cflags stuff
#########
extra_c_flags =-D_STANDALONE_ -D__NT__
!if $(DEBUG)
!if $(TRMEM)
extra_c_flags += -Zd -Od -DDEBUG_OUT -DTRMEM
!else
extra_c_flags += -Zd -Od -DDEBUG_OUT
!endif
!else
extra_c_flags += -Ot -Ox -Gs
!endif

#lflags stuff
#########
LOPT = /NOLOGO
!if $(DEBUG)
LOPTD = /debug
!endif

lflagsd = $(LOPTD) /SUBSYSTEM:CONSOLE $(LOPT) /map:$^*.map /Libpath:$(HXDIR)\lib /OPT:NOWIN98
lflagsw = $(LOPTD) /SUBSYSTEM:CONSOLE $(LOPT) /map:$^*.map /OPT:NOWIN98

.c{$(OUTD)}.obj:
	@if not exist $(OUTD) mkdir $(OUTD)
    $(VCDIR)\bin\cl -c $(inc_dirs) $(extra_c_flags) -Fo$@ $<

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
        @%write  $^*.lnk $(lflagsd) /NOD
        @%append $^*.lnk initw32.obj $(proj_obj) dkrnl32s.lib imphlp.lib libc.lib oldnames.lib /stub:LOADPE.BIN
        @%append $^*.lnk /OUT:$@
        @%append $^*.lnk $(OUTD)/$(name).res /FIXED:NO
        $(linker) @$^*.lnk
        @$(HXDIR)\bin\patchpe $@
!endif        

!if $(WIN)
$(OUTD)/$(name).exe : $(OUTD)\opcodes.gh $(OUTD)/$(name).res $(proj_obj)
        @%write  $^*.lnk $(lflagsw)
        @%append $^*.lnk $(proj_obj) /LIBPATH:$(VCDIR)\Lib /LIBPATH:"$(SDKDIR)\Lib" kernel32.lib
        @%append $^*.lnk /OUT:$@
        @%append $^*.lnk $(OUTD)/$(name).res
        $(linker) @$^*.lnk
!if $(DEBUG)        
        @copy $(OUTD)\$(name).exe TEST\*.* >NUL
        @copy $(OUTD)\$(name).sym TEST\*.* >NUL
!endif        
!endif        

$(OUTD)/$(name).res : h/$(name).rc $(OUTD)/msg.gh
    $(VCDIR)\bin\rc -r -d_STANDALONE_ -iH -i$(OUTD) -fo $^@ $[@ 

$(OUTD)/msg.gh : h/jwasm.msg
    $(OWDIR)\binnt\vi -s h/makemsg.vi -p"$^@ MSG_JWASM_RC_BASE" $[@

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
