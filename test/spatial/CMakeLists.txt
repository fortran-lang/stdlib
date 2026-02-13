set(
  fppFiles
  "test_spatial_kabsch_umeyama.fypp"
)
fypp_f90pp("${fyppFlags}" "${fppFiles}" outFiles)

ADDTESTPP(spatial_kabsch_umeyama)