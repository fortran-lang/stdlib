### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(
  fppFiles
    test_selection.fypp
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(selection)
