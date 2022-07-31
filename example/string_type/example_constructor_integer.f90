program example_constructor_integer
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type(42)
! len(string) == 2
  string = string_type(-289)
! len(string) == 4
end program example_constructor_integer
