program demo_lgt
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  logical :: res

  string = "bcd"
  res = lgt(string, "abc")
! res .eqv. .true.

  res = lgt(string, "bcd")
! res .eqv. .false.

  res = lgt(string, "cde")
! res .eqv. .false.
end program demo_lgt
