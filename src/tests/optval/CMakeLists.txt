add_executable(test_optval test_optval.F90)
target_link_libraries(test_optval fortran_stdlib)
if(REAL128)
  target_compile_definitions(test_optval PRIVATE REAL128)
endif()
add_test(NAME optval
         COMMAND $<TARGET_FILE:test_optval> ${CMAKE_CURRENT_BINARY_DIR}
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
