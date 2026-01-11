cmake_minimum_required(VERSION 3.14.0)

# Include overwrites before setting up the project
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

project(fortran_stdlib
	LANGUAGES Fortran C
        DESCRIPTION "Community driven and agreed upon de facto standard library for Fortran"
)

# Read version from file
file(STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PROJECT_VERSION)
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)
unset(VERSION_LIST)

include(CTest)

# Follow GNU conventions for installation directories
include(GNUInstallDirs)

include(${PROJECT_SOURCE_DIR}/cmake/stdlib.cmake)

# --- CMake specific configuration and package data export
add_subdirectory(config)

# --- compiler selection
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 9.0)
  message(FATAL_ERROR "GCC Version 9 or newer required")
endif()

# --- silence gfortran-15 argument-mismatch warnings
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU
   AND CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 15.0
   AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 16.0)
    add_compile_options("$<$<COMPILE_LANGUAGE:Fortran>:-Wno-external-argument-mismatch>")
endif()

# --- compiler feature checks
include(CheckFortranSourceCompiles)
include(CheckFortranSourceRuns)
check_fortran_source_runs("i=0; error stop i; end" f18errorstop)
check_fortran_source_compiles("real, allocatable :: array(:, :, :, :, :, :, :, :, :, :); end" f03rank SRC_EXT f90)
check_fortran_source_runs("use, intrinsic :: iso_fortran_env, only : real128; real(real128) :: x; x = x+1; end" f03real128)

if(NOT DEFINED CMAKE_MAXIMUM_RANK)
  set(CMAKE_MAXIMUM_RANK 4 CACHE STRING "Maximum array rank for generated procedures")
endif()

option(STDLIB_ANSI "Compile STDLIB ANSI" ON)

if(STDLIB_ANSI)
  message(STATUS "Enable stdlib ansi module")
  add_compile_definitions(STDLIB_ANSI=1)
else()
  message(STATUS "Disable stdlib ansi module")
  add_compile_definitions(STDLIB_ANSI=0)
endif() 

option(STDLIB_BITSET "Compile STDLIB BITSET" ON)

if(STDLIB_BITSET)
  message(STATUS "Enable stdlib bitset module")
  add_compile_definitions(STDLIB_BITSET=1)
else()
  message(STATUS "Disable stdlib bitset module")
  add_compile_definitions(STDLIB_BITSET=0)
endif() 

option(STDLIB_HASHMAPS "Compile STDLIB HASHMAPS" ON)

if(STDLIB_HASHMAPS)
  message(STATUS "Enable stdlib hashmaps module")
  add_compile_definitions(STDLIB_HASHMAPS=1)
else()
  message(STATUS "Disable stdlib hashmaps module")
  add_compile_definitions(STDLIB_HASHMAPS=0)
endif()

option(STDLIB_STATS "Compile STDLIB STATS" ON)

if(STDLIB_STATS)
  message(STATUS "Enable stdlib stats module")
  add_compile_definitions(STDLIB_STATS=1)
else()
  message(STATUS "Disable stdlib stats module")
  add_compile_definitions(STDLIB_STATS=0)
endif() 

option(FIND_BLAS "Find external BLAS and LAPACK" ON)

# --- find external BLAS and LAPACK
if(FIND_BLAS)

  message(STATUS "Searching for external BLAS/LAPACK")

  # Common MKL setup
  if(DEFINED ENV{MKLROOT} OR "${BLA_VENDOR}" MATCHES "^(Intel|Intel10_64)")
    enable_language("C")
    message(STATUS "Detected Intel MKL environment")
  endif()

  find_package(BLAS)
  find_package(LAPACK)

  if(BLAS_FOUND AND LAPACK_FOUND)
    message(STATUS "Found external BLAS: ${BLAS_LIBRARIES}")
    message(STATUS "Found external LAPACK: ${LAPACK_LIBRARIES}")

    # Detect ILP64 (common function)
    function(detect_ilp64 lib_name)
      set(${lib_name}_ILP64 False PARENT_SCOPE)
      # Prefer checking BLA_SIZEOF_INTEGER (available in CMake >= 3.22)
      if(DEFINED BLA_SIZEOF_INTEGER AND BLA_SIZEOF_INTEGER EQUAL 8)
        set(${lib_name}_ILP64 True PARENT_SCOPE)
      # Fallback: Check BLA_VENDOR manually for signs of ILP64
      elseif("${BLA_VENDOR}" MATCHES ".*(_ilp|ILP64).*")
        set(${lib_name}_ILP64 True PARENT_SCOPE)
      endif()
    endfunction()

    detect_ilp64(BLAS)
    detect_ilp64(LAPACK)

    # Set compile definitions
    if(BLAS_ILP64 OR LAPACK_ILP64)
      message(STATUS "Enabling 64-bit integer support (ILP64)")
      add_compile_definitions(STDLIB_EXTERNAL_BLAS_I64 STDLIB_EXTERNAL_LAPACK_I64)
      set(WITH_ILP64 True CACHE BOOL "Use 64-bit integer BLAS/LAPACK" FORCE)
    else()
      message(STATUS "Using standard 32-bit integer interface")
      add_compile_definitions(STDLIB_EXTERNAL_BLAS STDLIB_EXTERNAL_LAPACK)
    endif()

  else()
    message(WARNING "External BLAS/LAPACK not found - "
      "Using built-in reference BLAS")
  endif()

endif()

# --- find preprocessor
find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found! Please install fypp following the instructions in https://fypp.readthedocs.io/en/stable/fypp.html#installing")
endif()

# Custom preprocessor flags
if(DEFINED CMAKE_MAXIMUM_RANK)
  set(fyppFlags "-DMAXRANK=${CMAKE_MAXIMUM_RANK}")
elseif(f03rank)
  set(fyppFlags)
else()
  set(fyppFlags "-DVERSION90")
endif()

list(
  APPEND fyppFlags
  "-DWITH_CBOOL=$<BOOL:${WITH_CBOOL}>"
  "-DWITH_QP=$<BOOL:${WITH_QP}>"
  "-DWITH_XDP=$<BOOL:${WITH_XDP}>"
  "-DWITH_ILP64=$<BOOL:${WITH_ILP64}>"
  "-DPROJECT_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}"
  "-DPROJECT_VERSION_MINOR=${PROJECT_VERSION_MINOR}"
  "-DPROJECT_VERSION_PATCH=${PROJECT_VERSION_PATCH}"
  "-I${PROJECT_SOURCE_DIR}/include"
)

include_directories(${PROJECT_SOURCE_DIR}/include)
add_subdirectory(src)

if(BUILD_TESTING)
  enable_testing()
  add_subdirectory(test)
  add_subdirectory(example)
endif()

install(EXPORT ${PROJECT_NAME}-targets
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

# Export a pkg-config file


# Inspect linked libraries
get_target_property(linked_libs ${PROJECT_NAME} LINK_LIBRARIES)

message(STATUS "Libraries linked to myapp: ${linked_libs}")


function(get_target_dependencies target dependencies)
    get_target_property(target_type ${target} TYPE)
    if(NOT target_type STREQUAL "STATIC_LIBRARY" AND NOT target_type STREQUAL "SHARED_LIBRARY")
        return()
    endif()

    get_target_property(target_link_libs ${target} LINK_LIBRARIES)
    foreach(lib ${target_link_libs})
        if(TARGET ${lib})
            list(APPEND dependencies ${lib})
            get_target_dependencies(${lib} dependencies)
        else()
            list(APPEND dependencies ${lib})
        endif()
    endforeach()
    set(${dependencies} ${${dependencies}} PARENT_SCOPE)
endfunction()


set(lib_dependencies)
foreach(lib_target ${linked_libs})
#    get_target_property(lib_include_dirs ${lib_target} INTERFACE_INCLUDE_DIRECTORIES)
#    string(REPLACE ";" " " lib_include_dirs_spaced "${lib_include_dirs}")

    # Get dependencies for the current library
    get_target_dependencies(${lib_target} tmp_lib_dependencies)
    if (tmp_lib_dependencies)
      list(APPEND lib_dependencies ${lib_target} ${tmp_lib_dependencies} )
    else()
      list(APPEND lib_dependencies ${lib_target})
    endif()
    list(REMOVE_DUPLICATES lib_dependencies)
    message(STATUS "Libraries linked to ${lib_target}: ${lib_dependencies}")
    #set(lib_dependencies "")
#    string(REPLACE ";" " " lib_dependencies_spaced "${lib_dependencies}")

#    # Add library-specific information
#    set(pc_content "${pc_content}Name: ${lib_target}\n")
#    set(pc_content "${pc_content}Description: Description of ${lib_target}\n")
#    set(pc_content "${pc_content}Version: ${PROJECT_VERSION}\n")
#    set(pc_content "${pc_content}Libs: -L\${libdir} -l${lib_target} ${lib_dependencies_spaced}\n")
#    set(pc_content "${pc_content}Cflags: -I\${includedir} ${lib_include_dirs_spaced}\n\n")
endforeach()
    list(REVERSE lib_dependencies)



set(PC_CONTENT_TMP "")
foreach(lib_target ${lib_dependencies})
    list(APPEND PC_CONTENT_TMP "-l${lib_target}")
endforeach()
list(APPEND PC_CONTENT_TMP "-l${PROJECT_NAME}")

message(WARNING ${PC_CONTENT_TMP})

string(REPLACE ";" " " PC_CONTENT "${PC_CONTENT_TMP}")

configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/config/template.pc"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  @ONLY
)
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig"
)


