set(specialmatrices_fppFiles
    stdlib_specialmatrices.fypp
    stdlib_specialmatrices_tridiagonal.fypp
    )

set(specialmatrices_cppFiles
    )

set(specialmatrices_f90Files
    )

configure_stdlib_target(specialmatrices specialmatrices_f90Files specialmatrices_fppFiles specialmatrices_cppFiles)

target_link_libraries(specialmatrices PUBLIC constants linalg_core linalg lapack lapack_extended)
