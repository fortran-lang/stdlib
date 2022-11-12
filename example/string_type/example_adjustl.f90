program example_adjustl
  use stdlib_string_type
  implicit none
  type(string_type) :: string

  string = "                            Whitespace"
  string = adjustl(string)
! char(string) == "Whitespace                            "
end program example_adjustl
