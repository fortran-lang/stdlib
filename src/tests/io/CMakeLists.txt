add_executable(test_loadtxt test_loadtxt.f90)
target_link_libraries(test_loadtxt fortran_stdlib)

add_executable(test_savetxt test_savetxt.f90)
target_link_libraries(test_savetxt fortran_stdlib)

add_executable(test_loadtxt_qp test_loadtxt_qp.f90)
target_link_libraries(test_loadtxt_qp fortran_stdlib)

add_executable(test_savetxt_qp test_savetxt_qp.f90)
target_link_libraries(test_savetxt_qp fortran_stdlib)

add_executable(test_open test_open.f90)
target_link_libraries(test_open fortran_stdlib)

add_test(NAME loadtxt COMMAND $<TARGET_FILE:test_loadtxt> ${CMAKE_CURRENT_BINARY_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME savetxt COMMAND $<TARGET_FILE:test_savetxt> ${CMAKE_CURRENT_BINARY_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME loadtxt_qp COMMAND $<TARGET_FILE:test_loadtxt_qp> ${CMAKE_CURRENT_BINARY_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME savetxt_qp COMMAND $<TARGET_FILE:test_savetxt_qp> ${CMAKE_CURRENT_BINARY_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
add_test(NAME open COMMAND $<TARGET_FILE:test_open> ${CMAKE_CURRENT_BINARY_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

set_tests_properties(loadtxt_qp PROPERTIES LABELS quadruple_precision)
set_tests_properties(savetxt_qp PROPERTIES LABELS quadruple_precision)
