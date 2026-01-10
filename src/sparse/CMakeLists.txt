set(sparse_fppFiles
    stdlib_linalg_iterative_solvers_bicgstab.fypp
    stdlib_linalg_iterative_solvers_cg.fypp
    stdlib_linalg_iterative_solvers.fypp
    stdlib_linalg_iterative_solvers_pcg.fypp
    stdlib_sparse_constants.fypp
    stdlib_sparse_conversion.fypp
    stdlib_sparse_kinds.fypp
    stdlib_sparse_spmv.fypp
    )

set(sparse_cppFiles
    )

set(sparse_f90Files
    stdlib_sparse.f90
    )


configure_stdlib_target(sparse sparse_f90Files sparse_fppFiles sparse_cppFiles)

target_link_libraries(sparse PUBLIC linalg)
