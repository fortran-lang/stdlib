program example_llt
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  logical :: res

  string = "bcd"
  res = llt(string, "abc")
! res .eqv. .false.

  res = llt(string, "bcd")
! res .eqv. .false.

  res = llt(string, "cde")
! res .eqv. .true.
end program example_llt
