set(linalg_fppFiles
    stdlib_linalg_cholesky.fypp
    stdlib_linalg_cross_product.fypp
    stdlib_linalg_determinant.fypp
    stdlib_linalg_diag.fypp
    stdlib_linalg_eigenvalues.fypp
    stdlib_linalg.fypp
    stdlib_linalg_inverse.fypp
    stdlib_linalg_kronecker.fypp
    stdlib_linalg_least_squares.fypp
    stdlib_linalg_matrix_functions.fypp
    stdlib_linalg_norms.fypp
    stdlib_linalg_outer_product.fypp
    stdlib_linalg_pinv.fypp
    stdlib_linalg_qr.fypp
    stdlib_linalg_schur.fypp
    stdlib_linalg_solve.fypp
    stdlib_linalg_svd.fypp
    )

set(linalg_cppFiles
    )

set(linalg_f90Files
    )

configure_stdlib_target(linalg linalg_f90Files linalg_fppFiles linalg_cppFiles)

target_link_libraries(linalg PUBLIC core linalg_core constants lapack lapack_extended sorting intrinsics)
