cmake_minimum_required(VERSION 3.14.0)
project(fortran_stdlib
        LANGUAGES Fortran C CXX
        DESCRIPTION "Community driven and agreed upon de facto standard library for Fortran"
)

# Read version from file
file(STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PROJECT_VERSION)
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)
unset(VERSION_LIST)

enable_testing()

# Follow GNU conventions for installation directories
include(GNUInstallDirs)

include(${PROJECT_SOURCE_DIR}/cmake/stdlib.cmake)

# --- CMake specific configuration and package data export
add_subdirectory(config)

# --- compiler options
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 9.0)
    message(FATAL_ERROR "GCC Version 9 or newer required")
  endif()
  add_compile_options(-fimplicit-none)
  add_compile_options(-ffree-line-length-132)
  add_compile_options(-fno-range-check) # Needed for gfortran 9 and
  # earlier for hash functions 
  add_compile_options(-Wall)
  add_compile_options(-Wextra)
  add_compile_options(-Wimplicit-procedure)
  # -pedantic-errors triggers a false positive for optional arguments of elemental functions,
  # see test_optval and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=95446
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 11.0)
    add_compile_options(-pedantic-errors)
  endif()
  add_compile_options(-std=f2018)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")
  if(WIN32)
    add_compile_options(/warn:declarations,general,usage,interfaces,unused)
    add_compile_options(/stand:f18)
  else()
    add_compile_options(-warn declarations,general,usage,interfaces,unused)
    add_compile_options(-stand f18)
  endif()
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

# --- find preprocessor
find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found! Please install fypp following the instructions in https://fypp.readthedocs.io/en/stable/fypp.html#installing")
endif()

add_subdirectory(src)

install(EXPORT ${PROJECT_NAME}-targets
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
