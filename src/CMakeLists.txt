set(SRC
    stdlib_types.f90
    stdlib_io.f90
)

add_library(fortran_stdlib ${SRC})

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
