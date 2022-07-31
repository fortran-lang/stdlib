### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    test_maps.fypp
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(chaining_maps)
ADDTEST(open_maps)
ADDTEST(maps)

