set(logger_fppFiles
    )

set(logger_cppFiles
    )

set(logger_f90Files
    stdlib_logger.f90
    )

configure_stdlib_target(${PROJECT_NAME}_logger logger_f90Files logger_fppFiles logger_cppFiles)

target_link_libraries(${PROJECT_NAME}_logger PUBLIC ${PROJECT_NAME}_core)
