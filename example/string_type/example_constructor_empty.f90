program example_constructor_empty
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type()
! len(string) == 0
end program example_constructor_empty
