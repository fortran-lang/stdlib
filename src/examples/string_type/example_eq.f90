program example_eq
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  logical :: res

  string = "bcd"
  res = string == "abc"
! res .eqv. .false.

  res = string == "bcd"
! res .eqv. .true.

  res = string == "cde"
! res .eqv. .false.
end program example_eq
