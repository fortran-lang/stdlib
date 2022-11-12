program example_trim
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "Whitespace                            "
  string = trim(string)
! len(string) == 10
end program example_trim
