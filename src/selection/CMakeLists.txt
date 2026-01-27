set(selection_fppFiles
    stdlib_selection.fypp
    )

set(selection_cppFiles
    )

set(selection_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_selection selection_f90Files selection_fppFiles selection_cppFiles)

target_link_libraries(${PROJECT_NAME}_selection PUBLIC ${PROJECT_NAME}_core)
