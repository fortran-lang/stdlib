set(
  fppFiles
  "test_meshgrid.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

set(
  cppFiles
  "test_stdlib_math.fypp"
)
fypp_f90pp("${fyppFlags}" "${cppFiles}" outFiles)

ADDTESTPP(stdlib_math)
ADDTEST(linspace)
ADDTEST(logspace)
ADDTEST(meshgrid)
