macro(ADD_SUBDIR name)
  string(TOUPPER "${name}" uname)
  if (STDLIB_${uname})
    add_subdirectory(${name})
    list(APPEND OPTIONAL_LIB ${name})
  endif()
endmacro()

set(OPTIONAL_LIB)

ADD_SUBDIR(ansi)

add_subdirectory(array)

ADD_SUBDIR(bitsets)

add_subdirectory(blas)
add_subdirectory(constants)
add_subdirectory(core)

add_subdirectory(hash)

ADD_SUBDIR(hashmaps)

add_subdirectory(intrinsics)
ADD_SUBDIR(io)

add_subdirectory(lapack)
add_subdirectory(lapack_extended)
add_subdirectory(linalg_core)
ADD_SUBDIR(linalg_iterative)
add_subdirectory(linalg)
ADD_SUBDIR(logger)
add_subdirectory(math)
ADD_SUBDIR(quadrature)
add_subdirectory(selection)
add_subdirectory(sorting)
add_subdirectory(specialfunctions)
add_subdirectory(specialmatrices)
add_subdirectory(stringlist)
add_subdirectory(strings)
add_subdirectory(system)

ADD_SUBDIR(stats)

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
  linalg_core
  linalg
  math
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
