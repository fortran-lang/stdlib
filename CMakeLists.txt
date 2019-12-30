cmake_minimum_required(VERSION 3.5.0)
project(stdlib Fortran)
enable_testing()

# this avoids stdlib and projects using stdlib from having to introspect stdlib's directory structure
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR})

add_subdirectory(src)
