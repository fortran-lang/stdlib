set(selection_fppFiles
    ../stdlib_kinds.fypp
    stdlib_selection.fypp
    )

set(selection_cppFiles
    )

set(selection_f90Files
    )

configure_stdlib_target(selection selection_f90Files selection_fppFiles selection_cppFiles)
