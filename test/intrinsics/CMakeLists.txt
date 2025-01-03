set(
  fppFiles
  "test_intrinsics.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(intrinsics)
