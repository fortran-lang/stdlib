program example_iachar
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: code

  string = "Fortran"
  code = iachar(string)
end program example_iachar
