set(
  fppFiles
  "test_linalg.fypp"
  "test_blas_lapack.fypp"
  "test_linalg_determinant.fypp"
  "test_linalg_matrix_property_checks.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(linalg)
ADDTEST(linalg_determinant)
ADDTEST(linalg_matrix_property_checks)
ADDTEST(blas_lapack)
