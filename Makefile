# Fortran STDLIB Makefile for FPM BUILD
FYPPFLAGS=

export FYPPFLAGS

.PHONY: dev clean

dev:
	$(MAKE) -f Makefile.fpm --directory=src

clean:
	$(MAKE) -f Makefile.fpm clean --directory=src