program example_len
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: length

  string = "Some longer sentence for this example."
  length = len(string)
! length == 38

  string = "Whitespace                            "
  length = len(string)
! length == 38
end program example_len
