set(specialmatrices_fppFiles
    stdlib_specialmatrices.fypp
    stdlib_specialmatrices_tridiagonal.fypp
    stdlib_specialmatrices_sym_tridiagonal.fypp
    )

set(specialmatrices_cppFiles
    )

set(specialmatrices_f90Files
    )

configure_stdlib_target(${PROJECT_NAME}_specialmatrices specialmatrices_f90Files specialmatrices_fppFiles specialmatrices_cppFiles)

target_link_libraries(${PROJECT_NAME}_specialmatrices PUBLIC ${PROJECT_NAME}_constants ${PROJECT_NAME}_linalg_core ${PROJECT_NAME}_linalg ${PROJECT_NAME}_lapack ${PROJECT_NAME}_lapack_extended)
