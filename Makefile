# Fortran stdlib Makefile

FC = gfortran
FCFLAGS=-O0

.PHONY: all clean

all: stdlib tests 

stdlib:
	$(MAKE) FC=${FC} FCFLAGS=${FCFLAGS} --directory=src/lib

tests: stdlib
	$(MAKE) FC=${FC} FCFLAGS=${FCFLAGS} --directory=src/tests

clean:
	$(MAKE) clean --directory=src/lib
	$(MAKE) clean --directory=src/tests
