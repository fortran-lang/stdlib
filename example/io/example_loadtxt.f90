program example_loadtxt
  use stdlib_io, only: loadtxt
  implicit none
  real, allocatable :: x(:, :)
  call loadtxt('example.dat', x)
  
  ! Can also use list directed format if the default read fails.
  call loadtxt('example.dat', x, fmt='*')

  call loadtxt('example.csv', x, delimiter=',')

end program example_loadtxt
