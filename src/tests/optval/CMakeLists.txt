add_executable(test_optval test_optval.f90)
target_link_libraries(test_optval fortran_stdlib)

add_test(NAME OPTVAL COMMAND $<TARGET_FILE:test_optval>)
