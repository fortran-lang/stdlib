set(
  fppFiles
  "test_linalg.fypp"
  "test_linalg_eigenvalues.fypp"
  "test_linalg_solve.fypp"
  "test_linalg_lstsq.fypp"
  "test_linalg_determinant.fypp"
  "test_linalg_svd.fypp"
  "test_linalg_matrix_property_checks.fypp"
)

# Preprocessed files to contain preprocessor directives -> .F90
set(
  cppFiles
  "test_blas_lapack.fypp"
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)
fypp_f90pp("${fyppFlags}" "${cppFiles}" outPreprocFiles)

ADDTEST(linalg)
ADDTEST(linalg_determinant)
ADDTEST(linalg_eigenvalues)
ADDTEST(linalg_matrix_property_checks)
ADDTEST(linalg_solve)
ADDTEST(linalg_lstsq)
ADDTEST(linalg_svd)
ADDTESTPP(blas_lapack)
