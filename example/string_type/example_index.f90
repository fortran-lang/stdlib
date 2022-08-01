program example_index
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: pos

  string = "Search this string for this expression"
  pos = index(string, "this")
! pos == 8

  pos = index(string, "this", back=.true.)
! pos == 24

  pos = index(string, "This")
! pos == 0
end program example_index
