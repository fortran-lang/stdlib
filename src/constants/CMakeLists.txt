set(constants_fppFiles
    stdlib_codata_type.fypp
    stdlib_constants.fypp
    )

set(constants_f90Files
    stdlib_codata.f90
    )

configure_stdlib_target(constants constants_f90Files constants_fppFiles "")

target_link_libraries(constants PUBLIC core)
