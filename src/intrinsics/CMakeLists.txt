set(intrinsics_fppFiles
    ../stdlib_linalg_state.fypp
    stdlib_intrinsics_dot_product.fypp
    stdlib_intrinsics.fypp
    stdlib_intrinsics_matmul.fypp
    stdlib_intrinsics_sum.fypp
    )

set(intrinsics_cppFiles
    ../stdlib_linalg_blas.fypp
    ../stdlib_linalg_constants.fypp
    )

configure_stdlib_target(intrinsics "" intrinsics_fppFiles intrinsics_cppFiles)

target_link_libraries(intrinsics PUBLIC blas constants)
