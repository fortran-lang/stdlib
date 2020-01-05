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

add_executable(test_skip test_skip.f90)
target_link_libraries(test_skip fortran_stdlib)
add_test(NAME AlwaysSkip COMMAND $<TARGET_FILE:test_skip>)
set_tests_properties(AlwaysSkip PROPERTIES SKIP_RETURN_CODE 77)

add_executable(test_fail test_fail.f90)
target_link_libraries(test_fail fortran_stdlib)
add_test(NAME AlwaysFail COMMAND $<TARGET_FILE:test_fail>)
set_tests_properties(AlwaysFail PROPERTIES WILL_FAIL true)
