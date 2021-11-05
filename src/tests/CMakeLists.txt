macro(ADDTEST name)
    add_executable(test_${name} test_${name}.f90)
    target_link_libraries(test_${name} PRIVATE ${PROJECT_NAME})
    add_test(NAME ${name}
             COMMAND $<TARGET_FILE:test_${name}> ${CMAKE_CURRENT_BINARY_DIR}
             WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endmacro(ADDTEST)

add_subdirectory(ascii)
add_subdirectory(bitsets)
add_subdirectory(io)
add_subdirectory(linalg)
add_subdirectory(logger)
add_subdirectory(optval)
add_subdirectory(sorting)
add_subdirectory(stats)
add_subdirectory(string)
add_subdirectory(system)
add_subdirectory(quadrature)
add_subdirectory(math)
add_subdirectory(stringlist)

# some compilers have broken error stop return code handling
if(f18errorstop)
  set(DISABLE_ESTOP_TESTS false)
else()
  set(DISABLE_ESTOP_TESTS true)
endif()

add_executable(test_error_handling test_error_driver.f90)

add_executable(test_always_77 test_always_77.f90)
target_link_libraries(test_always_77 PRIVATE ${PROJECT_NAME})
add_test(NAME test_error_77
    COMMAND $<TARGET_FILE:test_error_handling> $<TARGET_FILE:test_always_77> 77)

add_executable(test_always_1 test_always_fail.f90)
target_link_libraries(test_always_1 PRIVATE ${PROJECT_NAME})
add_test(NAME test_error_1
    COMMAND $<TARGET_FILE:test_error_handling> $<TARGET_FILE:test_always_1> 1)

set_tests_properties(test_error_77 test_error_1 PROPERTIES
  TIMEOUT 5
  DISABLED ${DISABLE_ESTOP_TESTS}
)
