set(
  fppFiles
  "test_spatial_kabsch.fypp"
)
fypp_f90pp("${fyppFlags}" "${fppFiles}" outFiles)

ADDTESTPP(spatial_kabsch)