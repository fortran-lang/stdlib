set(intrinsics_fppFiles
    stdlib_intrinsics_dot_product.fypp
    stdlib_intrinsics.fypp
    stdlib_intrinsics_sum.fypp
    )

configure_stdlib_target(intrinsics "" intrinsics_fppFiles "")

target_link_libraries(intrinsics PUBLIC constants)
