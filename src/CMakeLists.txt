if (NOT STDLIB_NO_ANSI)
  add_subdirectory(ansi)
endif()

add_subdirectory(array)

if (NOT STDLIB_NO_BITSET)
  add_subdirectory(bitsets)
endif()

add_subdirectory(blas)
add_subdirectory(constants)
add_subdirectory(core)

add_subdirectory(hash)

if (NOT STDLIB_NO_HASHMAPS)
  add_subdirectory(hashmaps)
endif()

add_subdirectory(intrinsics)
add_subdirectory(io)

add_subdirectory(lapack)
add_subdirectory(lapack_extended)
add_subdirectory(linalg_core)
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

if (NOT STDLIB_NO_STATS)
  add_subdirectory(stats)
endif()

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
  $<$<NOT:$<BOOL:${STDLIB_NO_ANSI}>>:ansi>
  $<$<NOT:$<BOOL:${STDLIB_NO_BITSET}>>:bitsets>
  hash
  $<$<NOT:$<BOOL:${STDLIB_NO_HASHMAPS}>>:hashmaps>
  intrinsics
  io
  linalg_core
  linalg
  logger
  $<$<NOT:$<BOOL:${STDLIB_NO_STATS}>>:stats>
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
  )
