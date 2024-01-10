cmake_minimum_required(VERSION 3.14.0)

# Include overwrites before setting up the project
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

project(fortran_stdlib
        LANGUAGES Fortran
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
  "-DPROJECT_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}"
  "-DPROJECT_VERSION_MINOR=${PROJECT_VERSION_MINOR}"
  "-DPROJECT_VERSION_PATCH=${PROJECT_VERSION_PATCH}"
  "-I${PROJECT_SOURCE_DIR}/include"
)

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
