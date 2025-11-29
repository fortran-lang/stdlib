set(
  fppFiles
  "test_sorting.fypp"
)
fypp_f90pp("${fyppFlags}" "${fppFiles}" outFiles)

ADDTESTPP(sorting)
