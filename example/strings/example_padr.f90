program example_padr
  use stdlib_string_type, only: string_type, assignment(=), write (formatted)
  use stdlib_strings, only: padr
  implicit none
  type(string_type) :: string

  string = "right pad this string"
! string <-- "right pad this string"

  print '(dt)', padr(string, 25, "$") ! "right pad this string$$$$"

  string = padr(string, 25)
! string <-- "right pad this string    "

end program example_padr
