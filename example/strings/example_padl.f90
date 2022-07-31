program example_padl
  use stdlib_string_type, only: string_type, assignment(=), write (formatted)
  use stdlib_strings, only: padl
  implicit none
  type(string_type) :: string

  string = "left pad this string"
! string <-- "left pad this string"

  print '(dt)', padl(string, 25, "$") ! "$$$$$left pad this string"

  string = padl(string, 25)
! string <-- "     left pad this string"

end program example_padl
