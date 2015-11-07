CFLAGS = -bt=nt -bc -fp6 -fpi87
OUTD   = WinExe

!include owmod.inc

all: $(OUTD) $(OUTD)/jwasm.exe

$(OUTD)/jwasm.exe: $(OUTD)/main.obj $(OBJS)
        $(LINK) @<<
!if $(DEBUG)
debug dwarf option symfile
!endif
system nt
file { $< }
name $^@
disable 1171
option map=$^*,verbose,norelocs,stack=256k,heapsize=1m,quiet
commit stack=4k
<<
