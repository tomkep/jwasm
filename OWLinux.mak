CFLAGS = -bt=linux
OUTD   = Linux

!include owmod.inc

all: $(OUTD) $(OUTD)/jwasm

$(OUTD)/jwasm: $(OUTD)/main.obj $(OBJS)
        $(LINK) @<<
!if $(DEBUG)
debug dwarf option symfile
!endif
system linux
file { $< }
name $^@
option map=$^*,verbose,norelocs,stack=128k,quiet
!ifndef __UNIX__
option noextension
!endif
<<
