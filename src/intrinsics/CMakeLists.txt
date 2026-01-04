set(intrinsics_fppFiles
    ../stdlib_ascii.fypp
    ../stdlib_kinds.fypp
    ../stdlib_constants.fypp
    ../stdlib_codata_type.fypp
    ../stdlib_error.fypp
    ../stdlib_io.fypp
    ../stdlib_optval.fypp
    stdlib_intrinsics_dot_product.fypp
    stdlib_intrinsics.fypp
    stdlib_intrinsics_sum.fypp
    )

set(intrinsics_f90Files
    ../stdlib_codata.f90
    )

configure_stdlib_target(intrinsics intrinsics_f90Files intrinsics_fppFiles "")
