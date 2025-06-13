####

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(APPEND fppFiles
    blas/stdlib_blas_constants.fypp
    blas/stdlib_blas.fypp
    blas/stdlib_blas_level1.fypp
    blas/stdlib_blas_level2_ban.fypp
    blas/stdlib_blas_level2_gen.fypp
    blas/stdlib_blas_level2_pac.fypp
    blas/stdlib_blas_level2_sym.fypp
    blas/stdlib_blas_level2_tri.fypp
    blas/stdlib_blas_level3_gen.fypp
    blas/stdlib_blas_level3_sym.fypp
    blas/stdlib_blas_level3_tri.fypp
    blas/stdlib_linalg_blas_aux.fypp
)

set(fppFiles "${fppFiles}" PARENT_SCOPE)
