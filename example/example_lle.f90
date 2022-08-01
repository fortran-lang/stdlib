program example_lle
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  logical :: res

  string = "bcd"
  res = lle(string, "abc")
! res .eqv. .false.

  res = lle(string, "bcd")
! res .eqv. .true.

  res = lle(string, "cde")
! res .eqv. .true.
end program example_lle
