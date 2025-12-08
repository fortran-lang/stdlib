set(lapack_extended_fppFiles
    ../stdlib_kinds.fypp
    stdlib_extended_lapack_base.fypp
    stdlib_extended_lapack.fypp
)
set(lapack_extended_cppFiles
    ../stdlib_linalg_constants.fypp
)

configure_stdlib_target(lapack_extended "" lapack_extended_fppFiles lapack_extended_cppFiles)