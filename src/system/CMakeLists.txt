set(system_fppFiles
    ../stdlib_linalg_state.fypp
    )

set(system_cppFiles
    ../stdlib_linalg_constants.fypp
    )

set(system_f90Files
    stdlib_system_subprocess.c
    stdlib_system_subprocess.F90
    stdlib_system_path.f90
    stdlib_system.c
    stdlib_system.F90
    )

configure_stdlib_target(system system_f90Files system_fppFiles system_cppFiles)

target_link_libraries(system PUBLIC core strings)
