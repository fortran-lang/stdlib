program example_to_string
  use stdlib_strings, only: to_string
  implicit none

!> Example for `complex` type
  print *, to_string((1, 1))              !! "(1.00000000,1.00000000)"
  print *, to_string((1, 1), '(F6.2)')    !! "(  1.00,  1.00)"
  print *, to_string((1000, 1), '(ES0.2)'), to_string((1000, 1), '(SP,F6.3)')
!! "(1.00E+3,1.00)""(******,+1.000)"
!! Too narrow formatter for real number
!! Normal demonstration(`******` from Fortran Standard)

!> Example for `integer` type
  print *, to_string(-3)                  !! "-3"
  print *, to_string(42, '(I4)')          !! "  42"
  print *, to_string(1, '(I0.4)'), to_string(2, '(B4)')           !! "0001""  10"

!> Example for `real` type
  print *, to_string(1.)                  !! "1.00000000"
  print *, to_string(1., '(F6.2)')        !! "  1.00"
  print *, to_string(1., 'F6.2')          !! "  1.00"
  print *, to_string(1., '(SP,ES9.2)'), to_string(1, '(F7.3)')    !! "+1.00E+00""[*]"
!! 1 wrong demonstration (`[*]` from `to_string`)

!> Example for `logical` type
  print *, to_string(.true.)              !! "T"
  print *, to_string(.true., '(L2)')      !! " T"
  print *, to_string(.true., 'L2')        !! " T"
  print *, to_string(.false., '(I5)')     !! "[*]"
!! 1 wrong demonstrations(`[*]` from `to_string`)

end program example_to_string
