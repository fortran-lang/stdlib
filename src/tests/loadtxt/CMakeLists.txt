project(loadtxt)

add_executable(test_loadtxt test_loadtxt.f90)
target_link_libraries(test_loadtxt fortran_stdlib)

add_executable(test_savetxt test_savetxt.f90)
target_link_libraries(test_savetxt fortran_stdlib)

add_test(NAME load_text COMMAND $<TARGET_FILE:test_loadtxt> ${CMAKE_CURRENT_BINARY_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME save_text COMMAND $<TARGET_FILE:test_savetxt> ${CMAKE_CURRENT_BINARY_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
