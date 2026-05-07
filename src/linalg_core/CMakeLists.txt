set(linalg_core_fppFiles
    stdlib_linalg_state.fypp
    )

set(linalg_core_cppFiles
    stdlib_linalg_constants.fypp
    )

set(linalg_core_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_linalg_core linalg_core_f90Files linalg_core_fppFiles linalg_core_cppFiles)

target_link_libraries(${PROJECT_NAME}_linalg_core PUBLIC ${PROJECT_NAME}_core)
