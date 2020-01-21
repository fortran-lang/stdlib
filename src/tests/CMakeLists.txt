macro(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)
    target_link_libraries(test_${name} fortran_stdlib)
    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTEST)

add_subdirectory(ascii)
add_subdirectory(io)
add_subdirectory(optval)
add_subdirectory(stats)
add_subdirectory(system)

ADDTEST(always_skip)
set_tests_properties(always_skip PROPERTIES SKIP_RETURN_CODE 77)
ADDTEST(always_fail)
set_tests_properties(always_fail PROPERTIES WILL_FAIL true)
