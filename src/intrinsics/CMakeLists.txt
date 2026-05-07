set(intrinsics_fppFiles
    stdlib_intrinsics_dot_product.fypp
    stdlib_intrinsics.fypp
    stdlib_intrinsics_matmul.fypp
    stdlib_intrinsics_sum.fypp
    )

set(intrinsics_cppFiles
    )

configure_stdlib_target(${PROJECT_NAME}_intrinsics "" intrinsics_fppFiles intrinsics_cppFiles)

target_link_libraries(${PROJECT_NAME}_intrinsics PUBLIC ${PROJECT_NAME}_linalg_core ${PROJECT_NAME}_blas ${PROJECT_NAME}_constants)
