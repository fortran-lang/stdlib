program example_constructor_logical
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  string = string_type(.true.)
! len(string) == 1
  string = string_type(.false.)
! len(string) == 1
end program example_constructor_logical
