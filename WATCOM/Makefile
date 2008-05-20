#pmake: build os_nt cpu_386 womp 

host_os  = nt
host_cpu = 386

proj_name = watcom

#calling convention (r or s)
CCV=s

!ifndef DEBUG
DEBUG=1
!endif

!if $(DEBUG)
extra_c_flags += -od -d2 -DDEBUG_OUT
!else
extra_c_flags += -ox
!endif

OUTD=Rel$(CCV)

inc_dirs = -IH

.c{$(OUTD)}.obj:
	@if not exist $(OUTD) mkdir $(OUTD)
    wcc386 -q -3$(CCV) -bt=nt $(inc_dirs) $(extra_c_flags) -fo$@ $<

objs =  $(OUTD)/autodept.obj  $(OUTD)/swchar.obj

ALL: $(objs)

