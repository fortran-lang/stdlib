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
  set_target_properties(test_hash_functions PROPERTIES LINKER_LANGUAGE Fortran)
endif()
if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 10.0)
  target_compile_options(
    test_hash_functions
    PRIVATE
    $<$<COMPILE_LANGUAGE:Fortran>:-fno-range-check>
   )
endif()
