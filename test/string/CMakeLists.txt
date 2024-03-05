#### Pre-process: .fpp -> .f90 via Fypp

# Create a list of the files to be preprocessed
set(fppFiles
    test_string_assignment.fypp
    test_string_to_number.fypp
)

fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

ADDTEST(string_assignment)
ADDTEST(string_operator)
ADDTEST(string_intrinsic)
ADDTEST(string_match)
ADDTEST(string_derivedtype_io)
ADDTEST(string_functions)
ADDTEST(string_strip_chomp)
ADDTEST(string_to_number)
ADDTEST(string_to_string)
