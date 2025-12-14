program example_input
  use stdlib_io, only : input
  implicit none

  character(len=:), allocatable :: name

  name = input("Enter your name: ")
  print *, "Hello:", name

end program example_input
