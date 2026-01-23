set(selection_fppFiles
    stdlib_selection.fypp
    )

set(selection_cppFiles
    )

set(selection_f90Files
    )

configure_stdlib_target(selection selection_f90Files selection_fppFiles selection_cppFiles)

target_link_libraries(selection PUBLIC core)
