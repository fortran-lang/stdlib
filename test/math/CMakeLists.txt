set(
  fppFiles
  "test_stdlib_math.fypp"
  "test_meshgrid.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(stdlib_math)
ADDTEST(linspace)
ADDTEST(logspace)
ADDTEST(meshgrid)
