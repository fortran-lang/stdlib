# Fortran stdlib Makefile

OBJS = mod_stdlib.o

.PHONY: all clean
.SUFFIXES: .f90 .o

all: mod_stdlib.o

.f90.o:
	$(FC) $(FCFLAGS) -c $<

%.o: %.mod

mod_stdlib.o: mod_stdlib.f90

clean:
	$(RM) *.o *.mod
