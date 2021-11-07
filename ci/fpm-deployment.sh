#!/usr/bin/env bash

set -ex

# Target directory to deploy stdlib to
destdir="${DESTDIR:-stdlib-fpm}"

# Get fypp preprocessor
fypp="${FYPP:-$(which fypp)}"

# Arguments for the fypp preprocessor
fyflags="${FYFLAGS:--DMAXRANK=4}"

# Number of parallel jobs for preprocessing
njob="$(nproc)"

# Additional files to include
include=(
  "ci/fpm.toml"
  "LICENSE"
)

# Files to remove from collection
prune=(
  "$destdir/test/test_always_fail.f90"
  "$destdir/test/test_always_skip.f90"
  "$destdir/src/common.f90"
  "$destdir/src/f18estop.f90"
)

mkdir -p "$destdir/src" "$destdir/test"

# Preprocess stdlib sources
find src -maxdepth 1 -iname "*.fypp" \
  | cut -f1 -d. | xargs -P "$njob" -I{} "$fypp" "{}.fypp" "$destdir/{}.f90" $fyflags

# Collect stdlib source files
find src -maxdepth 1 -iname "*.f90" -exec cp {} "$destdir/src/" \;
find src/tests -name "test_*.f90" -exec cp {} "$destdir/test/" \;
find src/tests -name "*.dat" -exec cp {} "$destdir/" \;

# Include additional files
cp "${include[@]}" "$destdir/"

# Source file workarounds for fpm; ignore missing files
rm "${prune[@]}"

# List stdlib-fpm package contents
ls -R "$destdir"
