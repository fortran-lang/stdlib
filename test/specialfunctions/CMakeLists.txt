### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    test_specialfunctions_activations.fypp
    test_specialfunctions_gamma.fypp
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(specialfunctions_gamma)
ADDTEST(specialfunctions_activations)