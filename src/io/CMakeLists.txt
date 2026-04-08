set(io_fppFiles
    stdlib_io.fypp
    stdlib_io_npy.fypp
    stdlib_io_npy_load.fypp
    stdlib_io_npy_save.fypp
    stdlib_io_mm.fypp
    stdlib_io_mm_load.fypp
    stdlib_io_mm_save.fypp
    )

set(io_cppFiles
    )

set(io_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_io io_f90Files io_fppFiles io_cppFiles)

target_link_libraries(${PROJECT_NAME}_io PUBLIC ${PROJECT_NAME}_core ${PROJECT_NAME}_strings)
