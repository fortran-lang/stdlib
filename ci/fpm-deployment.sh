#!/usr/bin/env bash
#
#
set -ex

DESTDIR="stdlib-fpm"

FYFLAGS="-DMAXRANK=4"

mkdir -p $DESTDIR/src
mkdir -p $DESTDIR/test

# Preprocess stdlib sources
ls src/*.fypp | cut -f1 -d. | xargs -i{} fypp {}.fypp {}.f90 $FYFLAGS

# Collect stdlib files
find src -maxdepth 1 -iname "*.f90" -exec cp {} $DESTDIR/src/ \;
find src/tests -name "test_*.f90" -exec cp {} $DESTDIR/test/ \;
find src/tests -name "*.dat" -exec cp {} $DESTDIR/ \;
cp LICENSE $DESTDIR/
cp ci/fpm.toml $DESTDIR/

# Source file workarounds for fpm
rm $DESTDIR/test/test_always_fail.f90
rm $DESTDIR/test/test_always_skip.f90
rm $DESTDIR/test/test_mean_f03.f90
rm $DESTDIR/src/common.f90
rm $DESTDIR/src/f18estop.f90

# List stdlib-fpm package contents
ls -R $DESTDIR
