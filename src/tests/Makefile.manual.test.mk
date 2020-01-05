# Common Makefile rules that are included from each test subdirectory's
# Makefile

CPPFLAGS += -I../..
LDFLAGS += -L../.. -lstdlib

OBJS = $(PROGS_SRC:.f90=.o) $(PROGS_SRC:.F90=.o)
PROGS = $(OBJS:.o=)
TESTPROGS = $(PROGS:=TEST)

.PHONY: all clean test $(TESTPROGS)

all: $(PROGS)

test: $(TESTPROGS)

$(TESTPROGS):
	./$(@:TEST=)

clean:
	$(RM) $(PROGS) $(OBJS) $(CLEAN_FILES)

%.o: %.f90 %.F90
	$(FC) $(FFLAGS) $(CPPFLAGS) -c $<

$(PROGS): %: %.o
	$(FC) $(FFLAGS) $(CPPFLAGS) -o $@ $^ $(LDFLAGS)
