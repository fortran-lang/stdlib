cmake_minimum_required(VERSION 3.5.0)
project(stdlib Fortran)
enable_testing()

# this avoids stdlib and projects using stdlib from having to introspect stdlib's directory structure
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})

# compiler feature checks
include(CheckFortranSourceCompiles)
check_fortran_source_compiles("error stop i; end" f18errorstop SRC_EXT f90)

add_subdirectory(src)
