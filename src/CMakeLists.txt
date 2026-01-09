if (NOT STDLIB_NO_ANSI)
  add_subdirectory(ansi)
endif()

if (NOT STDLIB_NO_BITSET)
  add_subdirectory(bitsets)
endif()

add_subdirectory(blas)
add_subdirectory(constants)

add_subdirectory(hash)

if (NOT STDLIB_NO_HASHMAPS)
  add_subdirectory(hashmaps)
endif()

add_subdirectory(intrinsics)

add_subdirectory(lapack)
add_subdirectory(lapack_extended)
add_subdirectory(logger)
add_subdirectory(math)
add_subdirectory(sorting)
add_subdirectory(specialfunctions)
add_subdirectory(system)

if (NOT STDLIB_NO_STATS)
  add_subdirectory(stats)
endif()

set(fppFiles
    stdlib_ascii.fypp
    stdlib_error.fypp
    stdlib_io.fypp
    stdlib_io_npy.fypp
    stdlib_io_npy_load.fypp
    stdlib_io_npy_save.fypp
    stdlib_kinds.fypp
    stdlib_linalg.fypp
    stdlib_linalg_diag.fypp
    stdlib_linalg_least_squares.fypp
    stdlib_linalg_outer_product.fypp
    stdlib_linalg_kronecker.fypp
    stdlib_linalg_cross_product.fypp
    stdlib_linalg_eigenvalues.fypp
    stdlib_linalg_solve.fypp
    stdlib_linalg_determinant.fypp
    stdlib_linalg_qr.fypp
    stdlib_linalg_inverse.fypp
    stdlib_linalg_iterative_solvers.fypp
    stdlib_linalg_iterative_solvers_bicgstab.fypp
    stdlib_linalg_iterative_solvers_cg.fypp
    stdlib_linalg_iterative_solvers_pcg.fypp
    stdlib_linalg_pinv.fypp
    stdlib_linalg_norms.fypp
    stdlib_linalg_state.fypp
    stdlib_linalg_svd.fypp
    stdlib_linalg_cholesky.fypp
    stdlib_linalg_schur.fypp
    stdlib_linalg_matrix_functions.fypp
    stdlib_optval.fypp
    stdlib_selection.fypp
    stdlib_sparse_constants.fypp
    stdlib_sparse_conversion.fypp
    stdlib_sparse_kinds.fypp
    stdlib_sparse_spmv.fypp
    stdlib_specialmatrices.fypp
    stdlib_specialmatrices_tridiagonal.fypp
    stdlib_quadrature.fypp
    stdlib_quadrature_trapz.fypp
    stdlib_quadrature_simps.fypp
    stdlib_random.fypp
    stdlib_str2num.fypp
    stdlib_string_type.fypp
    stdlib_string_type_constructor.fypp
    stdlib_strings_to_string.fypp
    stdlib_strings.fypp
    stdlib_version.fypp
)
set(cppFiles stdlib_linalg_constants.fypp
    )
set(f90Files
    stdlib_array.f90
    stdlib_sparse.f90
    stdlib_quadrature_gauss.f90
    stdlib_stringlist_type.f90
    $<IF:$<BOOL:${f18errorstop}>,f18estop.f90,f08estop.f90>
)

configure_stdlib_target(${PROJECT_NAME} f90Files fppFiles cppFiles)

target_link_libraries(${PROJECT_NAME} PUBLIC
  constants
  $<$<NOT:$<BOOL:${STDLIB_NO_ANSI}>>:ansi>
  $<$<NOT:$<BOOL:${STDLIB_NO_BITSET}>>:bitsets>
  hash
  $<$<NOT:$<BOOL:${STDLIB_NO_HASHMAPS}>>:hashmaps>
  intrinsics
  logger
  $<$<NOT:$<BOOL:${STDLIB_NO_STATS}>>:stats>
  math
  specialfunctions
  sorting
  system
  blas lapack lapack_extended
  )
