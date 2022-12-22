program example_zfill
  use stdlib_string_type, only: string_type, assignment(=), write (formatted)
  use stdlib_strings, only: zfill
  implicit none
  type(string_type) :: string

  string = "left pad this string with zeros"
! string <-- "left pad this string with zeros"

  print '(dt)', zfill(string, 36) ! "00000left pad this string with zeros"

  string = zfill(string, 36)
! string <-- "00000left pad this string with zeros"

end program example_zfill
