program example_char_range
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  character(len=:), allocatable :: dlc

  string = "Fortran"
  dlc = char(string, 1, 4)
! dlc == "Fort"
end program example_char_range
