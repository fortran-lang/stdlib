set(SRC
    stdlib_experimental_ascii.f90
    stdlib_experimental_io.f90
    stdlib_experimental_error.f90
    stdlib_experimental_default.f90
)

add_library(fortran_stdlib ${SRC})

if(f18errorstop)
  target_sources(fortran_stdlib PRIVATE f18estop.f90)
else()
  target_sources(fortran_stdlib PRIVATE f08estop.f90)
endif()

add_subdirectory(tests)

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
