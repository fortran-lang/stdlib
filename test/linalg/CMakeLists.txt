set(
  fppFiles
  "test_linalg.fypp"
  "test_linalg_matrix_property_checks.fypp"
)

if(BUILD_OPENBLAS AND OPENBLAS_FOUND)
  list(
    APPEND fppFiles
    test_linalg_openblas.fypp
  )
endif()

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(linalg)
ADDTEST(linalg_matrix_property_checks)

if(BUILD_OPENBLAS AND OPENBLAS_FOUND)
  ADDTEST(linalg_openblas)
endif()
