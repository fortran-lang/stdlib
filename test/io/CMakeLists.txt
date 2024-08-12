set(
  fppFiles
  "test_loadtxt_qp.fypp"
  "test_savetxt_qp.fypp"
)
fypp_f90("${fyppFlags}" "${fppFiles}" outFiles)

set(needsCPP
    test_np.f90
    test_zip.f90
)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set_source_files_properties(${needsCPP} PROPERTIES COMPILE_FLAGS "-cpp")
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
  set_source_files_properties(${needsCPP} PROPERTIES COMPILE_FLAGS "-fpp")
endif()

ADDTEST(loadtxt)
ADDTEST(savetxt)

ADDTEST(loadtxt_qp)
ADDTEST(savetxt_qp)
set_tests_properties(loadtxt_qp PROPERTIES LABELS quadruple_precision)
set_tests_properties(savetxt_qp PROPERTIES LABELS quadruple_precision)

ADDTEST(filesystem)
ADDTEST(getline)
ADDTEST(np)
ADDTEST(open)
ADDTEST(parse_mode)
ADDTEST(zip)
