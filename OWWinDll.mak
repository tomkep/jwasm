CFLAGS = -bt=nt -bd
OUTD   = WinDll

!include owmod.inc

all: $(OUTD) $(OUTD)/jwasmd.dll

$(OUTD)/jwasmd.dll: $(OBJS)
        $(LINK) @<<
!if $(DEBUG)
debug dwarf option symfile
!endif
system nt
file { $< }
name $^@
export AssembleModule='_AssembleModule@4',ParseCmdline='_ParseCmdline@8',CmdlineFini='_CmdlineFini@0'
disable 1171
option map=$^*,verbose,offset=0x5000000,quiet
<<
