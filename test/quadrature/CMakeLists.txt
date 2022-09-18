set(
  fppFiles
  "test_simps.fypp"
  "test_trapz.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(trapz)
ADDTEST(simps)
ADDTEST(gauss)
