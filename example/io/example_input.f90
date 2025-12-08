program example_input
  use stdlib_io, only: input
  implicit none
  character(len=:), allocatable :: name
  integer :: stat

  print *, "Example of input() usage:"
  
  ! Simple usage
  name = input("Enter your name: ")
  print *, "Hello, ", name, "!"

  ! Usage with status check
  name = input("Enter something else (or Ctrl+D to fail): ", stat)
  if (stat == 0) then
     print *, "You entered: ", name
  else
     print *, "Input failed or EOF encountered (stat=", stat, ")"
  end if

end program example_input
