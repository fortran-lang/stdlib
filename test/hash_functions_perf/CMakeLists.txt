ADDTEST(32_bit_hash_performance)
ADDTEST(64_bit_hash_performance)

if(CMAKE_Fortran_COMPILER_ID STREQUAL GNU AND CMAKE_Fortran_COMPILER_VERSION VERSION_LESS 10.0)
  target_compile_options(
    test_64_bit_hash_performance
    PRIVATE
    $<$<COMPILE_LANGUAGE:Fortran>:-fno-range-check>
   )
endif()
