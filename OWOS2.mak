CFLAGS = -bt=os2
OUTD   = OS2

!include owmod.inc

all: $(OUTD) $(OUTD)/jwasm.exe

$(OUTD)/jwasm.exe: $(OUTD)/main.obj $(OBJS)
        $(LINK) @<<
!if $(DEBUG)
debug dwarf option symfile
!endif
system os2v2
file { $< }
name $^@
option map=$^*,verbose,stack=128k,quiet
<<
