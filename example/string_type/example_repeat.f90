program example_repeat
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "What? "
  string = repeat(string, 3)
! string == "What? What? What? "
end program example_repeat
