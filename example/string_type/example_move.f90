program example_move
  use stdlib_string_type, only: string_type, assignment(=), move
  implicit none
  type(string_type) :: from_string
  character(len=:), allocatable :: from_char, to_char

  from_string = "move this string"
  from_char = "move this char"
! from_string <-- "move this string"
! from_char   <-- "move this char"
! to_char   <-- (unallocated)

  call move(from_string, to_char)
! from_string <-- ""
! to_char   <-- "move this string"

  call move(from_char, to_char)
! from_char <-- (unallocated)
! to_string <-- "move this char"

end program example_move
