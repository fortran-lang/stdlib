program example_gt
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  logical :: res

  string = "bcd"
  res = string > "abc"
! res .eqv. .true.

  res = string > "bcd"
! res .eqv. .false.

  res = string > "cde"
! res .eqv. .false.
end program example_gt
