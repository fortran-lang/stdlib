
set(lapack_fppFiles
    stdlib_lapack_base.fypp
    stdlib_lapack_solve.fypp
    stdlib_lapack_others.fypp
    stdlib_lapack_orthogonal_factors.fypp
    stdlib_lapack_eig_svd_lsq.fypp
    stdlib_linalg_lapack_aux.fypp
    stdlib_lapack_auxiliary.fypp
    stdlib_lapack_blas_like_base.fypp
    stdlib_lapack_blas_like_l1.fypp
    stdlib_lapack_blas_like_l2.fypp
    stdlib_lapack_blas_like_l3.fypp
    stdlib_lapack_blas_like_mnorm.fypp
    stdlib_lapack_blas_like_scalar.fypp
    stdlib_lapack_cosine_sine.fypp
    stdlib_lapack_cosine_sine2.fypp
    stdlib_lapack_eigv_comp.fypp
    stdlib_lapack_eigv_comp2.fypp
    stdlib_lapack_eigv_gen.fypp
    stdlib_lapack_eigv_gen2.fypp
    stdlib_lapack_eigv_gen3.fypp
    stdlib_lapack_eigv_std_driver.fypp
    stdlib_lapack_eigv_svd_bidiag_dc.fypp
    stdlib_lapack_eigv_svd_drivers.fypp
    stdlib_lapack_eigv_svd_drivers2.fypp
    stdlib_lapack_eigv_svd_drivers3.fypp
    stdlib_lapack_eigv_sym_comp.fypp
    stdlib_lapack_eigv_sym.fypp
    stdlib_lapack_eigv_tridiag.fypp
    stdlib_lapack_eigv_tridiag2.fypp
    stdlib_lapack_eigv_tridiag3.fypp
    stdlib_lapack_givens_jacobi_rot.fypp
    stdlib_lapack_householder_reflectors.fypp
    stdlib_lapack_lsq.fypp
    stdlib_lapack_lsq_aux.fypp
    stdlib_lapack_lsq_constrained.fypp
    stdlib_lapack_orthogonal_factors_ql.fypp
    stdlib_lapack_orthogonal_factors_qr.fypp
    stdlib_lapack_orthogonal_factors_rz.fypp
    stdlib_lapack_others_sm.fypp
    stdlib_lapack_solve_aux.fypp
    stdlib_lapack_solve_chol_comp.fypp
    stdlib_lapack_solve_chol.fypp
    stdlib_lapack_solve_ldl_comp.fypp
    stdlib_lapack_solve_ldl_comp2.fypp
    stdlib_lapack_solve_ldl_comp3.fypp
    stdlib_lapack_solve_ldl_comp4.fypp
    stdlib_lapack_solve_ldl.fypp
    stdlib_lapack_solve_lu_comp.fypp
    stdlib_lapack_solve_lu.fypp
    stdlib_lapack_solve_tri_comp.fypp
    stdlib_lapack_svd_bidiag_qr.fypp
    stdlib_lapack_svd_comp.fypp
    stdlib_lapack_svd_comp2.fypp
)
set(lapack_cppFiles
    stdlib_linalg_lapack.fypp
)
set(lapack_f90Files
)

configure_stdlib_target(${PROJECT_NAME}_lapack lapack_f90Files lapack_fppFiles lapack_cppFiles)

if(LAPACK_FOUND)
    target_link_libraries(${PROJECT_NAME}_lapack PUBLIC "LAPACK::LAPACK")
endif()
target_link_libraries(${PROJECT_NAME}_lapack PUBLIC ${PROJECT_NAME}_core ${PROJECT_NAME}_linalg_core ${PROJECT_NAME}_blas ${PROJECT_NAME}_strings)
