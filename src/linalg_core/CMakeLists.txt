set(linalg_core_fppFiles
    stdlib_linalg_state.fypp
    )

set(linalg_core_cppFiles
    stdlib_linalg_constants.fypp
    )

set(linalg_core_f90Files
    )

configure_stdlib_target(linalg_core linalg_core_f90Files linalg_core_fppFiles linalg_core_cppFiles)

target_link_libraries(linalg_core PUBLIC core)
