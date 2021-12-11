#ADDTEST(hash_functions)

set(SRC
nmhash_scalar.c
pengyhash.c
SpookyV2.cpp
SpookyV2Test.cpp
waterhash.c
generate_hash_arrays.cpp
)

add_library(libc_hash ${SRC})


add_executable(test_hash_functions test_hash_functions.f90)
target_link_libraries(test_hash_functions "${PROJECT_NAME}" "test-drive::test-drive" "libc_hash")
add_test(NAME hash_functions
             COMMAND $<TARGET_FILE:test_hash_functions> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
