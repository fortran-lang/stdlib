cmake_minimum_required(VERSION 3.14.0)

project(stdlib Fortran)
enable_testing()

# Add our local modules to the module path
if(NOT CMAKE_MODULE_PATH)
  set(CMAKE_MODULE_PATH)
endif()
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")

# Include the preprocessor options for stdlib
include(${CMAKE_MODULE_PATH}/stdlib.cmake)

# GNUInstallDirs is used to install libraries into correct locations
# on all platforms.
include(GNUInstallDirs)

# Setup the Fortran cmake environment
# Sets up the flags for compiling shared libraries or static libraries, and returns the libType parameter.
include("${CMAKE_MODULE_PATH}/FortranEnvironment.cmake")


# --- compiler options
#if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
#  add_compile_options(-fimplicit-none)
#elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
#  add_compile_options(-warn declarations)
#elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
#  add_compile_options(-Mdclchk)
#endif()

# --- compiler feature checks
include(CheckFortranSourceCompiles)
include(CheckFortranSourceRuns)
check_fortran_source_compiles("error stop i; end" f18errorstop SRC_EXT f90)
check_fortran_source_compiles("real, allocatable :: array(:, :, :, :, :, :, :, :, :, :); end" f03rank SRC_EXT f90)
check_fortran_source_runs("use, intrinsic :: iso_fortran_env, only : real128; real(real128) :: x; x = x+1; end" f03real128)

if(DEFINED CMAKE_MAXIMUM_RANK)
  set(CMAKE_MAXIMUM_RANK ${CMAKE_MAXIMUM_RANK})
endif()

# --- find preprocessor
find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found!\n"
	              "fypp is a python package and can be installed via, 'pip install fypp'")
endif()

add_subdirectory(src)
