program example_input
  use stdlib_io, only: input
  implicit none
  
  character(len=:), allocatable :: name
  
  ! Get user's name with a prompt
  name = input('Enter your name: ')
  
  ! Display greeting
  print '(a,a)', 'Hello, ', name
  
end program example_input
