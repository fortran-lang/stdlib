set(OPTIONAL_LIB)

if (STDLIB_ANSI)
  add_subdirectory(ansi)
  list(APPEND OPTIONAL_LIB ansi)
endif()

add_subdirectory(array)

if (STDLIB_BITSET)
  add_subdirectory(bitsets)
  list(APPEND OPTIONAL_LIB bitsets)
endif()

add_subdirectory(blas)
add_subdirectory(constants)
add_subdirectory(core)

add_subdirectory(hash)

if (STDLIB_HASHMAPS)
  add_subdirectory(hashmaps)
  list(APPEND OPTIONAL_LIB hashmaps)
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
  list(APPEND OPTIONAL_LIB stats)
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
  hash
  intrinsics
  io
  linalg_core
  linalg_iterative
  linalg
  logger
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
  ${OPTIONAL_LIB}
  )
