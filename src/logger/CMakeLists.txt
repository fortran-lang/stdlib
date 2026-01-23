set(logger_fppFiles
    )

set(logger_cppFiles
    )

set(logger_f90Files
    stdlib_logger.f90
    )

configure_stdlib_target(logger logger_f90Files logger_fppFiles logger_cppFiles)

target_link_libraries(logger PUBLIC core)
