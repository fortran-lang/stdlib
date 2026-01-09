set(logger_fppFiles
    ../stdlib_ascii.fypp
    ../stdlib_kinds.fypp
    ../stdlib_optval.fypp
    )

set(logger_cppFiles
    )

set(logger_f90Files
    stdlib_logger.f90
    )

configure_stdlib_target(logger logger_f90Files logger_fppFiles logger_cppFiles)
