set(blas_fppFiles
    stdlib_blas_constants.fypp
    stdlib_blas.fypp
    stdlib_blas_level1.fypp
    stdlib_blas_level2_ban.fypp
    stdlib_blas_level2_gen.fypp
    stdlib_blas_level2_pac.fypp
    stdlib_blas_level2_sym.fypp
    stdlib_blas_level2_tri.fypp
    stdlib_blas_level3_gen.fypp
    stdlib_blas_level3_sym.fypp
    stdlib_blas_level3_tri.fypp
    stdlib_linalg_blas_aux.fypp
)
set(blas_cppFiles
    ../stdlib_linalg_constants.fypp
    ../stdlib_linalg_blas.fypp
)

configure_stdlib_target(blas "" blas_fppFiles blas_cppFiles)

if(BLAS_FOUND)
    target_link_libraries(blas PUBLIC "BLAS::BLAS")
endif()
target_link_libraries(blas PUBLIC core)
