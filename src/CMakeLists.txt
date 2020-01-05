set(SRC
    stdlib_experimental_ascii.f90
    stdlib_experimental_io.F90
    stdlib_experimental_error.f90
    stdlib_experimental_optval.F90
)

add_library(fortran_stdlib ${SRC})
if(f03real128)
  target_compile_definitions(fortran_stdlib PRIVATE REAL128)
endif()

if(f18errorstop)
  target_sources(fortran_stdlib PRIVATE f18estop.f90)
else()
  target_sources(fortran_stdlib PRIVATE f08estop.f90)
endif()

# --- tests

add_subdirectory(tests)

# --- install

install(TARGETS fortran_stdlib
        RUNTIME DESTINATION bin
        ARCHIVE DESTINATION lib
        LIBRARY DESTINATION lib
    )
