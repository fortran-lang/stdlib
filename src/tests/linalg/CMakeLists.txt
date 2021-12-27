set(
  fppFiles
  "test_linalg.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(linalg)
ADDTEST(linalg_matrix_property_checks)
