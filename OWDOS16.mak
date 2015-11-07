CC     = wcc
CFLAGS = -bt=dos -ml -zt=12000
OUTD   = DOS16

!include owmod.inc

CFLAGS += -DFASTMEM=0 -DFASTPASS=0
CFLAGS += -DCOFF_SUPPORT=0 -DELF_SUPPORT=0 -DAMD64_SUPPORT=0 -DSSSE3SUPP=0
CFLAGS += -DSSE4SUPP=0 -DOWFC_SUPPORT=0 -DDLLIMPORT=0 -DAVXSUPP=0 -DPE_SUPPORT=0
CFLAGS += -DVMXSUPP=0 -DSVMSUPP=0 -DCVOSUPP=0 -DCOMDATSUPP=0 -DSTACKBASESUPP=0

all: $(OUTD) $(OUTD)/jwasmr.exe

$(OUTD)/jwasmr.exe: $(OUTD)/main.obj $(OBJS)
        $(LINK) @<<
!if $(DEBUG)
debug dwarf option symfile
!endif
system dos
file { $< }
name $^@
option map=$^*,verbose,stack=33k,quiet
<<
