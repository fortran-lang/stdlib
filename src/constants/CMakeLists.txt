set(constants_fppFiles
    stdlib_codata_type.fypp
    stdlib_constants.fypp
    )

set(constants_f90Files
    stdlib_codata.f90
    )

configure_stdlib_target(${PROJECT_NAME}_constants constants_f90Files constants_fppFiles "")

target_link_libraries(${PROJECT_NAME}_constants PUBLIC ${PROJECT_NAME}_core)
