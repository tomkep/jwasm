
# This makefile creates the JWasm Elf binary for Linux/FreeBSD.
# In case there are problems with the POSIX names of the low-level
# io routines (open(), close(), ...), see H/watcomc.h.

TARGET1=jwasm

ifndef DEBUG
DEBUG=0
endif

inc_dirs  = -IH

#cflags stuff

ifeq ($(DEBUG),0)
extra_c_flags = -DNDEBUG -O2
OUTD=GccUnixR
else
extra_c_flags = -DDEBUG_OUT -g
OUTD=GccUnixD
endif

c_flags =-D__UNIX__ $(extra_c_flags)

CC = gcc

.SUFFIXES:
.SUFFIXES: .c .o

proj_obj = $(OUTD)/main.o     $(OUTD)/assemble.o $(OUTD)/assume.o  \
           $(OUTD)/directiv.o $(OUTD)/posndir.o  $(OUTD)/segment.o \
           $(OUTD)/expreval.o $(OUTD)/memalloc.o $(OUTD)/errmsg.o  \
           $(OUTD)/macro.o    $(OUTD)/string.o   $(OUTD)/condasm.o \
           $(OUTD)/types.o    $(OUTD)/fpfixup.o  $(OUTD)/invoke.o  \
           $(OUTD)/equate.o   $(OUTD)/mangle.o   $(OUTD)/loop.o    \
           $(OUTD)/parser.o   $(OUTD)/tokenize.o $(OUTD)/input.o   \
           $(OUTD)/expans.o   $(OUTD)/symbols.o  $(OUTD)/labels.o  \
           $(OUTD)/fixup.o    $(OUTD)/codegen.o  $(OUTD)/data.o    \
           $(OUTD)/insthash.o $(OUTD)/branch.o   $(OUTD)/queues.o  \
           $(OUTD)/hll.o      $(OUTD)/proc.o     $(OUTD)/option.o  \
           $(OUTD)/coff.o     $(OUTD)/elf.o      $(OUTD)/omf.o     \
           $(OUTD)/bin.o      $(OUTD)/queue.o    $(OUTD)/carve.o   \
           $(OUTD)/omfgenms.o $(OUTD)/omfio.o    $(OUTD)/omfrec.o  \
           $(OUTD)/omffixup.o $(OUTD)/listing.o  $(OUTD)/fatal.o   \
           $(OUTD)/context.o  $(OUTD)/extern.o  \
           $(OUTD)/apiemu.o   $(OUTD)/dbgcv.o    \
           $(OUTD)/backptch.o $(OUTD)/msgtext.o  $(OUTD)/tbyte.o
######

#.c.o:
#	$(CC) -c $(inc_dirs) $(c_flags) -o $(OUTD)/$*.o $<
$(OUTD)/%.o: %.c
	$(CC) -c $(inc_dirs) $(c_flags) -o $(OUTD)/$*.o $<

all:  $(OUTD) $(OUTD)/$(TARGET1)

$(OUTD):
	mkdir $(OUTD)

$(OUTD)/$(TARGET1) : $(proj_obj)
ifeq ($(DEBUG),0)
	$(CC) $(proj_obj) -s -o $@ -Wl,-Map,$(OUTD)/$(TARGET1).map
else
	$(CC) $(proj_obj) -o $@ -Wl,-Map,$(OUTD)/$(TARGET1).map
endif

$(OUTD)/msgtext.o: msgtext.c H/msgdef.h H/usage.h
	$(CC) -c $(inc_dirs) $(c_flags) -o $*.o msgtext.c

$(OUTD)/parser.o: parser.c H/instruct.h H/special.h
	$(CC) -c $(inc_dirs) $(c_flags) -o $*.o parser.c

######

clean:
	rm $(OUTD)/$(TARGET1)
	rm $(OUTD)/*.o
	rm $(OUTD)/*.map

