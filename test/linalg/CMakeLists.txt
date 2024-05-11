set(
  fppFiles
  "test_linalg.fypp"
  "test_blas_lapack.fypp"
  "test_linalg_solve.fypp"
  "test_linalg_lstsq.fypp"
  "test_linalg_determinant.fypp"
  "test_linalg_svd.fypp"
  "test_linalg_matrix_property_checks.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(linalg)
ADDTEST(linalg_determinant)
ADDTEST(linalg_matrix_property_checks)
ADDTEST(linalg_solve)
ADDTEST(linalg_lstsq)
ADDTEST(linalg_svd)
ADDTEST(blas_lapack)
