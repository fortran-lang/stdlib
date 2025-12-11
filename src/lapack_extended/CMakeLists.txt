set(lapack_extended_fppFiles
    ../stdlib_kinds.fypp
    stdlib_lapack_extended_base.fypp
    stdlib_lapack_extended.fypp
)
set(lapack_extended_cppFiles
    ../stdlib_linalg_constants.fypp
)

configure_stdlib_target(lapack_extended "" lapack_extended_fppFiles lapack_extended_cppFiles)