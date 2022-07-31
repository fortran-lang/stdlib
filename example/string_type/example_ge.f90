program example_ge
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  logical :: res

  string = "bcd"
  res = string >= "abc"
! res .eqv. .true.

  res = string >= "bcd"
! res .eqv. .true.

  res = string >= "cde"
! res .eqv. .false.
end program example_ge
