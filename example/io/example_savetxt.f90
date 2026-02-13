program example_savetxt
  use stdlib_io, only: savetxt
  use, intrinsic :: iso_fortran_env, only: output_unit

  implicit none
  real :: x(3, 2) = 1
  integer :: unit
  call savetxt('example.dat', x)
  call savetxt('example.csv', x, delimiter=',')
  call savetxt('example1.dat', x, header='x  (x-units)   y  (y-units)')
  call savetxt('example2.dat', x, header='x  (x-units)   y  (y-units)', comments='!#', footer='This is all data')
  call savetxt('example3.dat', x, fmt='g0.7')
  ! Output to output_unit
  call savetxt(output_unit, x, header='x  (x-units)   y  (y-units)')
  ! Save data and then add more data
  open(newunit=unit, file='example4.dat')
  call savetxt(unit, x)
  call savetxt(unit, 2 * x)
  close(unit)
end program example_savetxt
