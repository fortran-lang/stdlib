# Common Makefile rules that are included from each test subdirectory's
# Makefile


OBJS = $(PROGS_SRC:.f90=.o)
PROGS = $(OBJS:.o=)
TESTPROGS = $(PROGS:=TEST)

.PHONY: all clean test $(TESTPROGS)

all: $(PROGS)

test: $(TESTPROGS)

$(TESTPROGS):
	./$(@:TEST=)

clean:
	$(RM) $(PROGS) $(OBJS) $(CLEAN_FILES)

%.o: %.f90
	$(FC) $(FFLAGS) $(CPPFLAGS) -c $<

$(PROGS): %: %.o
	$(FC) $(FFLAGS) $(CPPFLAGS) -o $@ $^ $(LDFLAGS)
