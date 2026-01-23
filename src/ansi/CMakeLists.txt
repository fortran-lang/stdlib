set(ansi_f90Files
    stdlib_ansi.f90
    stdlib_ansi_operator.f90
    stdlib_ansi_to_string.f90
    )

set(ansi_fppFiles
    )

configure_stdlib_target(ansi ansi_f90Files ansi_fppFiles "")

target_link_libraries(ansi PUBLIC core strings)
