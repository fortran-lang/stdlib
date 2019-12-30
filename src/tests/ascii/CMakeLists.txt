add_executable(test_ascii test_ascii.f90)
target_link_libraries(test_ascii fortran_stdlib)

add_test(NAME ASCII COMMAND $<TARGET_FILE:test_ascii>)
