program example_char_position
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  character(len=:), allocatable :: dlc
  character(len=1), allocatable :: chars(:)

  string = "Character sequence"
  dlc = char(string, 3)
! dlc == "a"
  chars = char(string, [3, 5, 8, 12, 14, 15, 18])
! chars == ["a", "a", "e", "e", "u", "e", "e"]
end program example_char_position
