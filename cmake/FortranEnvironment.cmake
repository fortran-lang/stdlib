# ================================
# CMAKE Script for setting up the fortran compiling and linking flags for different operating systems and compilers

# Call this cmake file from your project's CMakeLists.txt using 'include(/path/to/FortranEnvironment.cmake)'

# ++++++++++++++++++++++++++++++++


enable_language(Fortran)

option (BUILD_SHARED_LIBS "Shared or static libraries"  ON)

# Check if linux
if(UNIX AND NOT APPLE)
  set(LINUX TRUE)
endif()


# ================================
# Default the Build type to RELEASE
# ================================
# Make sure the build type is uppercase
string(TOUPPER "${CMAKE_BUILD_TYPE}" BT)

if(BT STREQUAL "RELEASE")
  set(CMAKE_BUILD_TYPE RELEASE CACHE STRING
    "Choose the type of build, options are DEBUG, or RELEASE."
    FORCE)
elseif(BT STREQUAL "DEBUG")
  set(CMAKE_BUILD_TYPE DEBUG CACHE STRING
    "Choose the type of build, options are DEBUG, or RELEASE."
    FORCE)
elseif(NOT BT)
  set(CMAKE_BUILD_TYPE DEBUG CACHE STRING
    "Choose the type of build, options are DEBUG, or RELEASE."
    FORCE)
  message(STATUS "CMAKE_BUILD_TYPE not given, defaulting to DEBUG.")
else()
  message(FATAL_ERROR "CMAKE_BUILD_TYPE not valid, choices are DEBUG, or RELEASE.")
endif(BT STREQUAL "RELEASE")

# ++++++++++++++++++++++++++++++++

# ================================
# Find the openmp package and add compiler-specific flags
# ================================
find_package(OpenMP)
if(OPENMP_FOUND)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
  set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
endif()
# ++++++++++++++++++++++++++++++++

# ================================
# Set gfortran compile flags
# ================================
message("\n")
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  include(${CMAKE_MODULE_PATH}/gfortran_flags.cmake)

# ================================
# Set INTEL compile flags
# ================================
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  include(${CMAKE_MODULE_PATH}/intel_flags.cmake)

elseif(CMAKE_Fortran_COMILER_ID MATCHES "PGI")
  message(FATAL_ERROR "Need to define flags for PGI")

endif()

# ================================
# Set the output directories for compiled libraries and module files.
# Paths are relative to the build folder where you call cmake
# ================================
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib) # Static library location
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib) # Shared library location
# Place module files in specific include folder
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/../include)
# Place executables in bin
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../bin)
INCLUDE_DIRECTORIES(${CMAKE_Fortran_MODULE_DIRECTORY})

# ++++++++++++++++++++++++++++++++

# ================================
# Display information to the user
# ================================
message("Using compiler type ${CMAKE_Fortran_COMPILER}\n"
	"If you need to change the compiler\n"
	"Use -DCMAKE_Fortran_COMPILER=<Path to compiler>\n")

message("Build type is ${CMAKE_BUILD_TYPE}\n"
	"If you need to change the build type\n"
	"Use -DCMAKE_BUILD_TYPE=[DEBUG RELEASE]\n")

if(${CMAKE_BUILD_TYPE} STREQUAL "RELEASE")
  message("Using the following compile flags \n"
	  "${CMAKE_Fortran_FLAGS_RELEASE}\n")
elseif(${CMAKE_BUILD_TYPE} STREQUAL "DEBUG")
  message("Using the following compile flags \n"
	  "${CMAKE_Fortran_FLAGS_DEBUG}\n")
endif()

if(BUILD_SHARED_LIBS)
  message("Building with -DBUILD_SHARED_LIBS=ON")
else()
  message("Building with -DBUILD_SHARED_LIBS=OFF")
endif()
message("Using the following library link flags \n"
	"${CMAKE_SHARED_LINKER_FLAGS}\n")
message("Using the following program link flags \n"
	"${CMAKE_EXE_LINKER_FLAGS}\n")

message("'make install' will install to ${CMAKE_INSTALL_PREFIX}")
message("To change the install directory\n"
	"Use -DCMAKE_INSTALL_PREFIX:PATH=/path/to/install/to\n")

