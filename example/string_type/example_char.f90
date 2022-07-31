program example_char
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  character(len=:), allocatable :: dlc

  string = "Character sequence"
  dlc = char(string)
! dlc == "Character sequence"
end program example_char
