
# This makefile creates the JWasm Win32 binary with either MinGW or Cygwin.
#  'make -f GccWin.mak'            will use MinGW.
#  'make -f GccWin.mak CYGWIN=1'   will use Cygwin.

name = jwasm

ifndef CYGWIN
CYGWIN=0
endif

ifndef DEBUG
DEBUG=0
endif

inc_dirs  = -IH

#cflags stuff

ifeq ($(DEBUG),1)
extra_c_flags = -DDEBUG_OUT -g
ifeq ($(CYGWIN),1)
OUTD=CygwinD
else
OUTD=MinGWD
endif
else
extra_c_flags = -DNDEBUG -O2
ifeq ($(CYGWIN),1)
OUTD=CygwinR
else
OUTD=MinGWR
endif
endif

c_flags = -D__NT__ $(extra_c_flags)

CC=gcc.exe -c $(inc_dirs) $(c_flags)

$(OUTD)/%.obj: %.c
	$(CC) -o $(OUTD)/$*.obj $<

proj_obj = $(OUTD)/main.obj     $(OUTD)/assemble.obj $(OUTD)/assume.obj  \
           $(OUTD)/directiv.obj $(OUTD)/posndir.obj  $(OUTD)/segment.obj \
           $(OUTD)/expreval.obj $(OUTD)/memalloc.obj $(OUTD)/errmsg.obj  \
           $(OUTD)/macro.obj    $(OUTD)/string.obj   $(OUTD)/condasm.obj \
           $(OUTD)/types.obj    $(OUTD)/fpfixup.obj  $(OUTD)/invoke.obj  \
           $(OUTD)/equate.obj   $(OUTD)/mangle.obj   $(OUTD)/loop.obj    \
           $(OUTD)/parser.obj   $(OUTD)/tokenize.obj $(OUTD)/input.obj   \
           $(OUTD)/expans.obj   $(OUTD)/symbols.obj  $(OUTD)/labels.obj  \
           $(OUTD)/fixup.obj    $(OUTD)/codegen.obj  $(OUTD)/data.obj    \
           $(OUTD)/insthash.obj $(OUTD)/branch.obj   $(OUTD)/queues.obj  \
           $(OUTD)/hll.obj      $(OUTD)/proc.obj     $(OUTD)/option.obj  \
           $(OUTD)/coff.obj     $(OUTD)/elf.obj      $(OUTD)/omf.obj     \
           $(OUTD)/bin.obj      $(OUTD)/queue.obj    $(OUTD)/carve.obj   \
           $(OUTD)/omfgenms.obj $(OUTD)/omfio.obj    $(OUTD)/omfrec.obj  \
           $(OUTD)/omffixup.obj $(OUTD)/listing.obj  $(OUTD)/fatal.obj   \
           $(OUTD)/context.obj  $(OUTD)/extern.obj  \
           $(OUTD)/backptch.obj $(OUTD)/msgtext.obj  $(OUTD)/tbyte.obj   \
           $(OUTD)/dbgcv.obj
######
ifeq ($(CYGWIN),1)
proj_obj += $(OUTD)/apiemu.obj
endif

TARGET1=$(OUTD)/$(name).exe

ALL: $(OUTD) $(TARGET1)

$(OUTD):
	mkdir $(OUTD)

$(OUTD)/$(name).exe : $(proj_obj)
	gcc.exe $(proj_obj) -s -o $(OUTD)/$(name).exe -Wl,-Map,$(OUTD)/$(name).map

$(OUTD)/msgtext.obj: msgtext.c H/msgdef.h H/usage.h
	$(CC) -o $(OUTD)/msgtext.obj msgtext.c

$(OUTD)/parser.obj: parser.c H/instruct.h H/special.h
	$(CC) -o $(OUTD)/parser.obj parser.c

######

clean:
	@rm $(OUTD)/*.exe
	@rm $(OUTD)/*.obj
	@rm $(OUTD)/*.map
