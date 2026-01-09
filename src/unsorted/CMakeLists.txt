set(unsorted_fppFiles
    stdlib_linalg_iterative_solvers_bicgstab.fypp
    stdlib_linalg_iterative_solvers_cg.fypp
    stdlib_linalg_iterative_solvers.fypp
    stdlib_linalg_iterative_solvers_pcg.fypp
    stdlib_sparse_constants.fypp
    stdlib_sparse_conversion.fypp
    stdlib_sparse_kinds.fypp
    stdlib_sparse_spmv.fypp
    )

set(unsorted_cppFiles
    )

set(unsorted_f90Files
    stdlib_sparse.f90
    )


configure_stdlib_target(unsorted unsorted_f90Files unsorted_fppFiles unsorted_cppFiles)

target_link_libraries(unsorted PUBLIC linalg)
