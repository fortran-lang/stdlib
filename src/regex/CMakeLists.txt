set(regex_fppFiles
    )

set(regex_cppFiles
    )

set(regex_f90Files
    stdlib_regex.f90
    )

configure_stdlib_target(${PROJECT_NAME}_regex regex_f90Files regex_fppFiles regex_cppFiles)
