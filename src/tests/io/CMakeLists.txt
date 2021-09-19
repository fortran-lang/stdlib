ADDTEST(loadtxt)
ADDTEST(savetxt)

ADDTEST(loadtxt_qp)
ADDTEST(savetxt_qp)
set_tests_properties(loadtxt_qp PROPERTIES LABELS quadruple_precision)
set_tests_properties(savetxt_qp PROPERTIES LABELS quadruple_precision)

ADDTEST(open)
ADDTEST(parse_mode)
ADDTEST(disp)
