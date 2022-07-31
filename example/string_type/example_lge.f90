program example_lge
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  logical :: res

  string = "bcd"
  res = lge(string, "abc")
! res .eqv. .true.

  res = lge(string, "bcd")
! res .eqv. .true.

  res = lge(string, "cde")
! res .eqv. .false.
end program example_lge
