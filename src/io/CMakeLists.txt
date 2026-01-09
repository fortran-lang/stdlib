set(io_fppFiles
    ../stdlib_ascii.fypp
    ../stdlib_kinds.fypp
    ../stdlib_optval.fypp
    #../stdlib_string_type.fypp
    ../stdlib_error.fypp
    stdlib_io.fypp
    stdlib_io_npy.fypp
    stdlib_io_npy_load.fypp
    stdlib_io_npy_save.fypp
    )

set(io_cppFiles
    )

set(io_f90Files
    ../stdlib_io_constants.f90
    )

configure_stdlib_target(io io_f90Files io_fppFiles io_cppFiles)

target_link_libraries(io PUBLIC strings)
