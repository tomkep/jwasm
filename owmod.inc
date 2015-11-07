!ifndef DEBUG
DEBUG = 0
!endif

!ifndef TRMEM
TRMEM = 0
!endif

!ifndef CFLAGS
!error CFLAGS not defined
!endif

!ifndef OUTD
!error OUTD not defined
!endif

!ifndef %WATCOM
!error WATCOM environment variable not defined!
!endif

OBJS = &
$(OUTD)/apiemu.obj   &
$(OUTD)/assemble.obj &
$(OUTD)/assume.obj   &
$(OUTD)/atofloat.obj &
$(OUTD)/backptch.obj &
$(OUTD)/bin.obj      &
$(OUTD)/branch.obj   &
$(OUTD)/cmdline.obj  &
$(OUTD)/codegen.obj  &
$(OUTD)/coff.obj     &
$(OUTD)/condasm.obj  &
$(OUTD)/context.obj  &
$(OUTD)/cpumodel.obj &
$(OUTD)/data.obj     &
$(OUTD)/dbgcv.obj    &
$(OUTD)/directiv.obj &
$(OUTD)/elf.obj      &
$(OUTD)/end.obj      &
$(OUTD)/equate.obj   &
$(OUTD)/errmsg.obj   &
$(OUTD)/expans.obj   &
$(OUTD)/expreval.obj &
$(OUTD)/extern.obj   &
$(OUTD)/fastpass.obj &
$(OUTD)/fixup.obj    &
$(OUTD)/fpfixup.obj  &
$(OUTD)/hll.obj      &
$(OUTD)/input.obj    &
$(OUTD)/invoke.obj   &
$(OUTD)/label.obj    &
$(OUTD)/linnum.obj   &
$(OUTD)/listing.obj  &
$(OUTD)/loop.obj     &
$(OUTD)/lqueue.obj   &
$(OUTD)/macro.obj    &
$(OUTD)/mangle.obj   &
$(OUTD)/memalloc.obj &
$(OUTD)/msgtext.obj  &
$(OUTD)/omf.obj      &
$(OUTD)/omffixup.obj &
$(OUTD)/omfint.obj   &
$(OUTD)/option.obj   &
$(OUTD)/parser.obj   &
$(OUTD)/posndir.obj  &
$(OUTD)/preproc.obj  &
$(OUTD)/proc.obj     &
$(OUTD)/queue.obj    &
$(OUTD)/reswords.obj &
$(OUTD)/safeseh.obj  &
$(OUTD)/segment.obj  &
$(OUTD)/simsegm.obj  &
$(OUTD)/string.obj   &
$(OUTD)/symbols.obj  &
$(OUTD)/tbyte.obj    &
$(OUTD)/tokenize.obj &
$(OUTD)/types.obj

!ifndef LINK
LINK = wlink
!endif

!ifndef CC
CC = wcc386
!endif

!ifdef __LOADDLL__
!  ifeq CC wcc
!    loaddll wcc    wccd
!  endif
!  ifeq CC wcc386
!    loaddll wcc386 wccd386
!  endif
!  ifeq LINK
!    loaddll wlink  wlinkd
!  endif
!endif


CFLAGS += -q -6 -zc -i=H

!if $(DEBUG)
CFLAGS += -od -d2 -DDEBUG_OUT
OUTD   =  $+ OW$(OUTD)D $-
!else
CFLAGS += -oxs -DNDEBUG
OUTD   =  $+ OW$(OUTD)R $-
!endif

!if $(TRMEM)
CFLAGS += -DTRMEM -DFASTMEM=0
OBJS   += $(OUTD)/trmem.obj
!endif

.BEFORE
        set LINUX_INCLUDE=$(%WATCOM)/lh
        set DOS_INCLUDE=$(%WATCOM)/h
        set NT_INCLUDE=$(%WATCOM)/h
        set OS2_INCLUDE=$(%WATCOM)/h;$(%WATCOM)/h/os2

{}.c{$(OUTD)}.obj: .AUTODEPEND
        $(CC) $(CFLAGS) -fo=$^@ $[@

$(OUTD): .EXPLICIT
        if not exist $^@ mkdir $^@

clean: .SYMBOLIC .EXPLICIT
        rm -rf $(OUTD)
