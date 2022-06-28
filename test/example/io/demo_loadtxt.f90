program demo_loadtxt
  use stdlib_io, only: loadtxt
  implicit none
  real, allocatable :: x(:, :)
  call loadtxt('example.dat', x)
end program demo_loadtxt
