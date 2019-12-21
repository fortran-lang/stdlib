include_directories(${PROJECT_BINARY_DIR}/src)

project(loadtxt)

add_executable(test_loadtxt test_loadtxt.f90)
target_link_libraries(test_loadtxt fortran_stdlib)

add_executable(test_savetxt test_savetxt.f90)
target_link_libraries(test_savetxt fortran_stdlib)

add_test(test_loadtxt ${PROJECT_BINARY_DIR}/test_loadtxt)
add_test(test_savetxt ${PROJECT_BINARY_DIR}/test_savetxt)
