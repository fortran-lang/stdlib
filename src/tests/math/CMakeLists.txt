set(
  fppFiles
  "test_stdlib_math.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(stdlib_math)
ADDTEST(linspace)
ADDTEST(logspace)
ADDTEST(math_arange)
ADDTEST(math_is_close)
ADDTEST(math_all_close)
