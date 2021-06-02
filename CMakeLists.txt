cmake_minimum_required(VERSION 3.14.0)
project(fortran_stdlib
        LANGUAGES Fortran
        VERSION 0
        DESCRIPTION "Community driven and agreed upon de facto standard library for Fortran"
)
enable_testing()

# Follow GNU conventions for installation directories
include(GNUInstallDirs)

include(${PROJECT_SOURCE_DIR}/cmake/stdlib.cmake)

# --- CMake specific configuration and package data export
add_subdirectory(config)

# --- compiler options
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-fimplicit-none)
  add_compile_options(-ffree-line-length-132)
  add_compile_options(-Wall)
  add_compile_options(-Wextra)
  add_compile_options(-Wimplicit-procedure)
  add_compile_options(-Wconversion-extra)
  # -pedantic-errors triggers a false positive for optional arguments of elemental functions,
  # see test_optval and https://gcc.gnu.org/bugzilla/show_bug.cgi?id=95446
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 11.0)
    add_compile_options(-pedantic-errors)
  endif()
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 8.0)
    add_compile_options(-std=f2018)
  else()
    add_compile_options(-std=f2008ts)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  add_compile_options(-warn declarations,general,usage,interfaces,unused)
  if(CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 18.0)
    add_compile_options(-stand f15)
  else()
    add_compile_options(-stand f18)
  endif()
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  add_compile_options(-Mdclchk)
endif()

# --- compiler feature checks
include(CheckFortranSourceCompiles)
include(CheckFortranSourceRuns)
check_fortran_source_runs("i=0; error stop i; end" f18errorstop SRC_EXT f90)
check_fortran_source_compiles("real, allocatable :: array(:, :, :, :, :, :, :, :, :, :); end" f03rank SRC_EXT f90)
check_fortran_source_runs("use, intrinsic :: iso_fortran_env, only : real128; real(real128) :: x; x = x+1; end" f03real128)

if(DEFINED CMAKE_MAXIMUM_RANK)
  set(CMAKE_MAXIMUM_RANK ${CMAKE_MAXIMUM_RANK})
endif()

# --- find preprocessor
find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found!")
endif()

add_subdirectory(src)

install(EXPORT ${PROJECT_NAME}-targets
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
