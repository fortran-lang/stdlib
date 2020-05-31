
message(STATUS "Getting ifort flags")

if(WIN32)
  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -nologo -fpp -O3 -heap-arrays1024 -QaxCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -Qipo -fp:fast=2 -Qdiag-disable:remark -Qmkl")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -nologo -fpp -g -Od -heap-arrays1024 -traceback -CB -Qfp-stack-check -Qmkl -warn:all -warn:nounused")
endif()

if(${LINUX})
  if(BUILD_SHARED_LIBS)
    # Add any shared library related stuff here
  else()
    # see: https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/753635
    # a better alternative is to use "-static-intel" which allows for dynamic system libraries
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-intel")
  endif()

  set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
  set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
endif()

if(APPLE)
  if(BUILD_SHARED_LIBS)
    # Add any shared library related stuff here
  else()
    # see: https://software.intel.com/en-us/forums/intel-fortran-compiler-for-linux-and-mac-os-x/topic/753635
    # a better alternative is to use "-static-intel" which allows for dynamic system libraries
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-intel")
  endif()
  
    set(CMAKE_Fortran_FLAGS_RELEASE "${CMAKE_Fortran_FLAGS_RELEASE} -O3 -axCORE-AVX2,CORE-AVX-I,AVX,SSE4.2,SSSE3 -no-prec-div -fp-model fast=2")
    set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -O0 -g -traceback -CB -fp-stack-check -gen-interfaces -warn interfaces")
endif()