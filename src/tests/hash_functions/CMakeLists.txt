#ADDTEST(hash_functions)

set(SRC
nmhash_scalar.c
pengyhash.c
SpookyV2.cpp
SpookyV2Test.cpp
waterhash.c
generate_hash_arrays.cpp
)

enable_language(CXX)
enable_language(C)

add_library(libc_hash ${SRC})

set(CMAKE_FORTRAN_LINK_EXECUTABLE "<CMAKE_Fortran_COMPILER>  <CMAKE_Fortran_LINK_FLAGS> <LINK_FLAGS> <FLAGS> <OBJECTS>  -o <TARGET> <LINK_LIBRARIES>")

add_executable(test_hash_functions test_hash_functions.f90)
target_link_libraries(test_hash_functions "${PROJECT_NAME}" "test-drive::test-drive" "libc_hash")
add_test(NAME hash_functions
             COMMAND $<TARGET_FILE:test_hash_functions> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

set_target_properties(test_hash_functions PROPERTIES LINKER_LANGUAGE FORTRAN)
