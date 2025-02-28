program example_loadtxt
  use stdlib_io, only: loadtxt
  implicit none
  real, allocatable :: x(:, :)
  character(len=:), allocatable :: text(:)
  call loadtxt('example.dat', x)
  
  ! Can also use list directed format if the default read fails.
  call loadtxt('example.dat', x, fmt='*')
  
  ! Load as a character array.  Character len will be equal to the largest line length.  
  call loadtxt('example.dat', text)
end program example_loadtxt
