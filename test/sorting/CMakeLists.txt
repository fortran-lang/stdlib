set(
  fppFiles
  "test_sorting.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(sorting)
