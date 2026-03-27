set(sparse_fppFiles
    stdlib_sparse_constants.fypp
    stdlib_sparse_conversion.fypp
    stdlib_sparse_kinds.fypp
    stdlib_sparse_operators.fypp
    stdlib_sparse_spmv.fypp
    stdlib_sparse_spmv_bsr.fypp
    stdlib_sparse_spmv_coo.fypp
    stdlib_sparse_spmv_csr.fypp
    stdlib_sparse_spmv_csc.fypp
    stdlib_sparse_spmv_ell.fypp
    stdlib_sparse_spmv_sellc.fypp
    )

set(sparse_cppFiles
    )

set(sparse_f90Files
    stdlib_sparse.f90
    )

configure_stdlib_target(${PROJECT_NAME}_sparse sparse_f90Files sparse_fppFiles sparse_cppFiles)

target_link_libraries(${PROJECT_NAME}_sparse PUBLIC ${PROJECT_NAME}_constants ${PROJECT_NAME}_sorting ${PROJECT_NAME}_blas)
