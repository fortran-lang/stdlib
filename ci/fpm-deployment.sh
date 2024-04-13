#!/usr/bin/env bash

set -ex

# Target directory to deploy stdlib to
destdir="${DESTDIR:-stdlib-fpm}"

# Get fypp preprocessor
fypp="${FYPP:-$(which fypp)}"

# Arguments for the fypp preprocessor
maxrank=4
fyflags="${FYFLAGS:--DMAXRANK=$maxrank}"

# Number of parallel jobs for preprocessing
if [ $(uname) = "Darwin" ]; then
  njob="$(sysctl -n hw.ncpu)"
else
  njob="$(nproc)"
fi

# Additional files to include
include=(
  "ci/fpm.toml"
  "ci/.gitignore"
  "LICENSE"
  "VERSION"
)

# Files to remove from collection
prune=(
  "$destdir/test/test_always_fail.f90"
  "$destdir/test/test_always_skip.f90"
  "$destdir/test/test_hash_functions.f90"
  "$destdir/src/f18estop.f90"
)

# Files that need preprocessed Fortran extension -> .F90 
preprocessed=(
   "$destdir/src/stdlib_linalg_constants" 
   "$destdir/src/stdlib_linalg_blas" 
   "$destdir/src/stdlib_linalg_blas_aux"
   "$destdir/src/stdlib_linalg_blas_s"
   "$destdir/src/stdlib_linalg_blas_d"
   "$destdir/src/stdlib_linalg_blas_q"
   "$destdir/src/stdlib_linalg_blas_c"
   "$destdir/src/stdlib_linalg_blas_z"
   "$destdir/src/stdlib_linalg_blas_w"
   "$destdir/src/stdlib_linalg_lapack"
   "$destdir/src/stdlib_linalg_lapack_aux"
   "$destdir/src/stdlib_linalg_lapack_s"
   "$destdir/src/stdlib_linalg_lapack_d"
   "$destdir/src/stdlib_linalg_lapack_q"
   "$destdir/src/stdlib_linalg_lapack_c"
   "$destdir/src/stdlib_linalg_lapack_z"
   "$destdir/src/stdlib_linalg_lapack_w"
)

major=$(cut -d. -f1 VERSION)
minor=$(cut -d. -f2 VERSION)
patch=$(cut -d. -f3 VERSION)
fyflags="${fyflags} -DPROJECT_VERSION_MAJOR=${major} -DPROJECT_VERSION_MINOR=${minor} -DPROJECT_VERSION_PATCH=${patch} -I include"

mkdir -p "$destdir/src" "$destdir/test" "$destdir/example"

# Preprocess stdlib sources
find src -maxdepth 1 -iname "*.fypp" \
  | cut -f1 -d. | xargs -P "$njob" -I{} "$fypp" "{}.fypp" "$destdir/{}.f90" $fyflags

find test -name "test_*.fypp" -exec cp {} "$destdir/test/" \;
fyflags="${fyflags} -I src"
find $destdir/test -maxdepth 1 -iname "*.fypp" \
  | cut -f1 -d. | xargs -P "$njob" -I{} "$fypp" "{}.fypp" "{}.f90" $fyflags
find $destdir/test -name "test_*.fypp" -exec rm {} \;

# Collect stdlib source files
find src -maxdepth 1 -iname "*.f90" -exec cp {} "$destdir/src/" \;
find test -name "test_*.f90" -exec cp {} "$destdir/test/" \;
find test -name "*.dat" -exec cp {} "$destdir/" \;
find example -name "example_*.f90" -exec cp {} "$destdir/example/" \;
find example -name "*.dat" -exec cp {} "$destdir/" \;
find example -name "*.npy" -exec cp {} "$destdir/" \;

# Include additional files
cp "${include[@]}" "$destdir/"

# Source file workarounds for fpm; ignore missing files
rm "${prune[@]}"

# Capitalize .f90 -> .F90 for preprocessed files
for pp_source in "${preprocessed[@]}"
do
   # workaround for case-insensitive fs    	
   mv "$pp_source.f90" "$pp_source.rename" 
   mv "$pp_source.rename" "$pp_source.F90" 
done

# List stdlib-fpm package contents
ls -R "$destdir"
