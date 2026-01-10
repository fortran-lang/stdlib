if (STDLIB_ANSI)
  add_subdirectory(ansi)
endif()

add_subdirectory(array)

if (STDLIB_BITSET)
  add_subdirectory(bitsets)
endif()

add_subdirectory(blas)
add_subdirectory(constants)
add_subdirectory(core)

add_subdirectory(hash)

if (STDLIB_HASHMAPS)
  add_subdirectory(hashmaps)
endif()

add_subdirectory(intrinsics)
add_subdirectory(io)

add_subdirectory(lapack)
add_subdirectory(lapack_extended)
add_subdirectory(linalg_core)
add_subdirectory(linalg_iterative)
add_subdirectory(linalg)
add_subdirectory(logger)
add_subdirectory(math)
add_subdirectory(quadrature)
add_subdirectory(selection)
add_subdirectory(sorting)
add_subdirectory(specialfunctions)
add_subdirectory(specialmatrices)
add_subdirectory(stringlist)
add_subdirectory(strings)
add_subdirectory(system)

if (STDLIB_STATS)
  add_subdirectory(stats)
endif()

add_subdirectory(sparse)

set(fppFiles
    stdlib_version.fypp
)
set(cppFiles
    )
set(f90Files
)

configure_stdlib_target(${PROJECT_NAME} f90Files fppFiles cppFiles)

target_link_libraries(${PROJECT_NAME} PUBLIC
  array
  constants
  core
  $<$<BOOL:${STDLIB_ANSI}>:ansi>
  $<$<BOOL:${STDLIB_BITSET}>:bitsets>
  hash
  $<$<BOOL:${STDLIB_HASHMAPS}>:hashmaps>
  intrinsics
  io
  linalg_core
  linalg_iterative
  linalg
  logger
  $<$<BOOL:${STDLIB_STATS}>:stats>
  math
  quadrature
  selection
  specialfunctions
  specialmatrices
  sorting
  stringlist
  strings
  system
  blas lapack lapack_extended
  sparse
  )
