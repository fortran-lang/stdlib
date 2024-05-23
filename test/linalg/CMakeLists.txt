set(
  fppFiles
  "test_linalg.fypp"
  "test_blas_lapack.fypp"
  "test_linalg_solve.fypp"
  "test_linalg_inverse.fypp"
  "test_linalg_lstsq.fypp"
  "test_linalg_determinant.fypp"
  "test_linalg_matrix_property_checks.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(linalg)
ADDTEST(linalg_determinant)
ADDTEST(linalg_matrix_property_checks)
ADDTEST(linalg_inverse)
ADDTEST(linalg_solve)
ADDTEST(linalg_lstsq)
ADDTEST(blas_lapack)
