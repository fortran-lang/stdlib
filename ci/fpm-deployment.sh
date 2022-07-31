#!/usr/bin/env bash

set -ex

# Target directory to deploy stdlib to
destdir="${DESTDIR:-stdlib-fpm}"

# Get fypp preprocessor
fypp="${FYPP:-$(which fypp)}"

# Arguments for the fypp preprocessor
fyflags="${FYFLAGS:--DMAXRANK=4}"

# Number of parallel jobs for preprocessing
if [ $(uname) = "Darwin" ]; then
  njob="$(sysctl -n hw.ncpu)"
else
  njob="$(nproc)"
fi

# Additional files to include
include=(
  "ci/fpm.toml"
  "LICENSE"
  "VERSION"
)

# Files to remove from collection
prune=(
  "$destdir/test/test_always_fail.f90"
  "$destdir/test/test_always_skip.f90"
  "$destdir/test/test_hash_functions.f90"
  "$destdir/src/common.f90"
  "$destdir/src/f18estop.f90"
)

major=$(cut -d. -f1 VERSION)
minor=$(cut -d. -f2 VERSION)
patch=$(cut -d. -f3 VERSION)
fyflags="${fyflags} -DPROJECT_VERSION_MAJOR=${major} -DPROJECT_VERSION_MINOR=${minor} -DPROJECT_VERSION_PATCH=${patch}"

mkdir -p "$destdir/src" "$destdir/test" "$destdir/example"

# Preprocess stdlib sources
find src -maxdepth 1 -iname "*.fypp" \
  | cut -f1 -d. | xargs -P "$njob" -I{} "$fypp" "{}.fypp" "$destdir/{}.f90" $fyflags

# Collect stdlib source files
find src -maxdepth 1 -iname "*.f90" -exec cp {} "$destdir/src/" \;
find test -name "test_*.f90" -exec cp {} "$destdir/test/" \;
find test -name "*.dat" -exec cp {} "$destdir/" \;
find example -name "example_*.f90" -exec cp {} "$destdir/example/" \;

# Include additional files
cp "${include[@]}" "$destdir/"

# Source file workarounds for fpm; ignore missing files
rm "${prune[@]}"

# List stdlib-fpm package contents
ls -R "$destdir"
