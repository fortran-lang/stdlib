program example_len_trim
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: length

  string = "Some longer sentence for this example."
  length = len_trim(string)
! length == 38

  string = "Whitespace                            "
  length = len_trim(string)
! length == 10
end program example_len_trim
