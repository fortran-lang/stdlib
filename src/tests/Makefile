# Fortran stdlib Makefile

CPPFLAGS=-I../lib
OBJS = ../lib/mod_stdlib.o

.PHONY: all clean
.SUFFIXES: .f90 .o

all: test_dummy

test_dummy: test_dummy.f90 $(OBJS)
	$(FC) $(FCFLAGS) $(CPPFLAGS) $< -o $@ $(OBJS)

%.o: %.mod

clean:
	$(RM) test_dummy
	$(RM) *.o *.mod
