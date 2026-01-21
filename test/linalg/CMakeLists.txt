set(
  fppFiles
  "test_linalg.fypp"
  "test_linalg_eigenvalues.fypp"
  "test_linalg_solve.fypp"
  "test_linalg_inverse.fypp"
  "test_linalg_pseudoinverse.fypp"
  "test_linalg_lstsq.fypp"
  "test_linalg_constrained_lstsq.fypp"
  "test_linalg_norm.fypp"
  "test_linalg_mnorm.fypp"
  "test_linalg_determinant.fypp"
  "test_linalg_qr.fypp"
  "test_linalg_pivoting_qr.fypp"
  "test_linalg_schur.fypp"
  "test_linalg_solve_iterative.fypp"
  "test_linalg_svd.fypp"
  "test_linalg_matrix_property_checks.fypp"
  "test_linalg_sparse.fypp"
  "test_linalg_specialmatrices.fypp"
  "test_linalg_cholesky.fypp"
  "test_linalg_expm.fypp"
  "test_linalg_matrix_factorizations.fypp"
)

# Preprocessed files to contain preprocessor directives -> .F90
set(
  cppFiles
  "test_blas_lapack.fypp"
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)
fypp_f90pp("${fyppFlags}" "${cppFiles}" outPreprocFiles)

ADDTEST(linalg)
ADDTEST(linalg_cholesky)
ADDTEST(linalg_determinant)
ADDTEST(linalg_eigenvalues)
ADDTEST(linalg_expm)
ADDTEST(linalg_matrix_property_checks)
ADDTEST(linalg_inverse)
ADDTEST(linalg_pseudoinverse)
ADDTEST(linalg_norm)
ADDTEST(linalg_mnorm)
ADDTEST(linalg_solve)
ADDTEST(linalg_lstsq)
ADDTEST(linalg_constrained_lstsq)
ADDTEST(linalg_qr)
ADDTEST(linalg_pivoting_qr)
ADDTEST(linalg_schur)
ADDTEST(linalg_solve_iterative)
ADDTEST(linalg_svd)
ADDTEST(linalg_sparse)
ADDTEST(linalg_specialmatrices)
ADDTEST(linalg_matrix_factorizations)
ADDTESTPP(blas_lapack)
