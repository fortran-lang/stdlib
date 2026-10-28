set(
  fppFiles
  "test_sorting.fypp"
  "test_sorting_unique.fypp"
)
fypp_f90pp("${fyppFlags}" "${fppFiles}" outFiles)

ADDTESTPP(sorting)
ADDTESTPP(sorting_unique)