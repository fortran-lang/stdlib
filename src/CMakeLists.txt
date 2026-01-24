set(OPTIONAL_LIB)

if (STDLIB_ANSI)
  add_subdirectory(ansi)
  list(APPEND OPTIONAL_LIB ${PROJECT_NAME}_ansi)
endif()

add_subdirectory(array)

if (STDLIB_BITSET)
  add_subdirectory(bitsets)
  list(APPEND OPTIONAL_LIB ${PROJECT_NAME}_bitsets)
endif()

add_subdirectory(blas)
add_subdirectory(constants)
add_subdirectory(core)

add_subdirectory(hash)

if (STDLIB_HASHMAPS)
  add_subdirectory(hashmaps)
  list(APPEND OPTIONAL_LIB ${PROJECT_NAME}_hashmaps)
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
  list(APPEND OPTIONAL_LIB ${PROJECT_NAME}_stats)
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
  ${PROJECT_NAME}_array
  ${PROJECT_NAME}_constants
  ${PROJECT_NAME}_core
  ${PROJECT_NAME}_hash
  ${PROJECT_NAME}_intrinsics
  ${PROJECT_NAME}_io
  ${PROJECT_NAME}_linalg_core
  ${PROJECT_NAME}_linalg_iterative
  ${PROJECT_NAME}_linalg
  ${PROJECT_NAME}_logger
  ${PROJECT_NAME}_math
  ${PROJECT_NAME}_quadrature
  ${PROJECT_NAME}_selection
  ${PROJECT_NAME}_specialfunctions
  ${PROJECT_NAME}_specialmatrices
  ${PROJECT_NAME}_sorting
  ${PROJECT_NAME}_stringlist
  ${PROJECT_NAME}_strings
  ${PROJECT_NAME}_system
  ${PROJECT_NAME}_blas ${PROJECT_NAME}_lapack ${PROJECT_NAME}_lapack_extended
  ${PROJECT_NAME}_sparse
  ${OPTIONAL_LIB}
  )
