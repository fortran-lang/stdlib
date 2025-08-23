#ADDTEST(hash_functions)

enable_language(CXX)
enable_language(C)

ADDTEST(hash_functions)
target_sources(
  test_hash_functions
  PRIVATE
  nmhash.c
  pengyhash.c
  SpookyV2.cpp
  SpookyV2Test.cpp
  waterhash.c
  generate_hash_arrays.cpp
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "^Intel")

  # Set the C++ standard to prevent icpc breakage
  set(CMAKE_CXX_STANDARD 11)
  set(CMAKE_CXX_STANDARD_REQUIRED ON)
  set(CMAKE_CXX_EXTENSIONS OFF)

  set_target_properties(test_hash_functions PROPERTIES LINKER_LANGUAGE Fortran)
  
  if(WIN32)
    set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreadedDLL$<$<CONFIG:Debug>:Debug>")
    target_compile_options(
      test_hash_functions
      PRIVATE
      $<$<COMPILE_LANGUAGE:Fortran>:/libs:dll> )
    if (CMAKE_BUILD_TYPE STREQUAL "Debug" OR "RelWithDebInfo")
      target_link_options(test_hash_functions
        PRIVATE
        /Qoption,link,/NODEFAULTLIB:libcmt
        /Qoption,link,/NODEFAULTLIB:msvcrt.lib
        /Qoption,link,/NODEFAULTLIB:libifcoremt.lib )
    endif()
  endif()
endif()

if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 10.0)
  target_compile_options(
    test_hash_functions
    PRIVATE
    $<$<COMPILE_LANGUAGE:Fortran>:-fno-range-check>
  )
endif()

