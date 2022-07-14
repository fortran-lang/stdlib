program example_constructor_scalar
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type("Sequence")
! len(string) == 8
  string = string_type(" S p a c e d ")
! len(string) == 13
end program example_constructor_scalar
