macro(ADD_SUBDIR name)
  string(TOUPPER "${name}" uname)
  if (STDLIB_${uname})
    add_subdirectory(${name})
    list(APPEND OPTIONAL_LIB ${PROJECT_NAME}_${name})
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
ADD_SUBDIR(specialmatrices)
ADD_SUBDIR(stringlist)
add_subdirectory(strings)
ADD_SUBDIR(system)

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
  ${PROJECT_NAME}_array
  ${PROJECT_NAME}_constants
  ${PROJECT_NAME}_core
  ${PROJECT_NAME}_hash
  ${PROJECT_NAME}_intrinsics
  ${PROJECT_NAME}_linalg_core
  ${PROJECT_NAME}_linalg_iterative
  ${PROJECT_NAME}_linalg
  ${PROJECT_NAME}_math
  ${PROJECT_NAME}_selection
  ${PROJECT_NAME}_specialfunctions
  ${PROJECT_NAME}_sorting
  ${PROJECT_NAME}_strings
  ${PROJECT_NAME}_system
  ${PROJECT_NAME}_blas ${PROJECT_NAME}_lapack ${PROJECT_NAME}_lapack_extended
  ${PROJECT_NAME}_sparse
  ${OPTIONAL_LIB}
  )
