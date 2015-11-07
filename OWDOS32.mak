CFLAGS = -bt=dos
OUTD   = DOS32

!include owmod.inc

all: $(OUTD) $(OUTD)/jwasm.exe

$(OUTD)/jwasm.exe: $(OUTD)/main.obj $(OBJS)
	$(LINK) @<<
!if $(DEBUG)
debug dwarf option symfile
!endif
system dos4g
file { $< }
name $^@
option map=$^*,verbose,stub=wstubq.exe,stack=256k,heapsize=256k,quiet
<<
