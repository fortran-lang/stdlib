program example_constructor_character
  use stdlib_string_type
  implicit none
  type(string_type) :: string
! len(string) == 0
  string = "Sequence"
! len(string) == 8
end program example_constructor_character
