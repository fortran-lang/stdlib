cmake_minimum_required(VERSION 3.14.0)
project(stdlib Fortran)
enable_testing()

# this avoids stdlib and projects using stdlib from having to introspect stdlib's directory structure
# FIXME: this eventually needs to be handled more precisely, as this spills all .mod/.smod into one directory
#  and thereby can clash if module/submodule names are the same in different parts of library
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})

option(REAL128 "make real128 precision available")

# --- compiler options
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU)
  add_compile_options(-fimplicit-none)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL Intel)
  add_compile_options(-warn declarations)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL PGI)
  add_compile_options(-Mdclchk)
endif()

# --- compiler feature checks
include(CheckFortranSourceCompiles)
include(CheckFortranSourceRuns)
check_fortran_source_compiles("error stop i; end" f18errorstop SRC_EXT f90)
check_fortran_source_runs("use, intrinsic :: iso_fortran_env, only : real128; real(real128) :: x; x = x+1; end" f03real128)
if(NOT f03real128)
  set(REAL128 false)
endif()

add_subdirectory(src)
