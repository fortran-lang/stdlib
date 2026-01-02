program example_savetxt
  use stdlib_io, only: savetxt
  implicit none
  real :: x(3, 2) = 1
  call savetxt('example.dat', x)
  call savetxt('example.csv', x, delimiter=',')
  call savetxt('example1.dat', x, header='x  (x-units)   y  (y-units)')
  call savetxt('example2.dat', x, header='x  (x-units)   y  (y-units)', comments='! ')
end program example_savetxt
