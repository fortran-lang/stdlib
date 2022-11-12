program example_slice
  use stdlib_string_type
  use stdlib_strings, only: slice
  implicit none
  type(string_type) :: string
  character(len=10) :: chars

  string = "abcdefghij"
! string <-- "abcdefghij"

  chars = "abcdefghij"
! chars <-- "abcdefghij"

  print'(a)', slice("abcdefghij", 2, 6, 2)   ! "bdf"
  print'(a)', slice(chars, 2, 6, 2)           ! "bdf"

  string = slice(string, 2, 6, 2)
! string <-- "bdf"

end program example_slice
