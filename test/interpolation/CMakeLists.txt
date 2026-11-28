set(
  fppFiles
  "test_interpolation_linear.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(interpolation_linear)
