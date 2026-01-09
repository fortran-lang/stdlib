set(system_fppFiles
    )

set(system_cppFiles
    )

set(system_f90Files
    stdlib_system_subprocess.c
    stdlib_system_subprocess.F90
    stdlib_system_path.f90
    stdlib_system.c
    stdlib_system.F90
    )

configure_stdlib_target(system system_f90Files system_fppFiles system_cppFiles)

target_link_libraries(system PUBLIC core linalg_core strings)
