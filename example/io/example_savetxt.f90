program example_savetxt
  use stdlib_io, only: savetxt
  implicit none
  real :: x(3, 2) = 1
  call savetxt('example.dat', x)
end program example_savetxt
