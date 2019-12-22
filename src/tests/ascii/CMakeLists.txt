include_directories(${PROJECT_BINARY_DIR}/src)

project(ascii)

add_executable(test_ascii test_ascii.f90)
target_link_libraries(test_ascii fortran_stdlib)

add_test(test_ascii ${PROJECT_BINARY_DIR}/test_ascii)

