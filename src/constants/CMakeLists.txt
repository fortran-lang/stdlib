set(constants_fppFiles
    ../stdlib_kinds.fypp
    ../stdlib_error.fypp
    ../stdlib_optval.fypp
    ../stdlib_ascii.fypp
    ../stdlib_string_type.fypp
    stdlib_codata_type.fypp
    stdlib_constants.fypp
    )

set(constants_f90Files
    ../stdlib_io_constants.f90
    stdlib_codata.f90
    )

configure_stdlib_target(constants constants_f90Files constants_fppFiles "")
