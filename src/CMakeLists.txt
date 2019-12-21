set(SRC
    stdlib_io.f90
    stdlib_error.f90
)

add_library(fortran_stdlib ${SRC})

add_subdirectory(tests)

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
