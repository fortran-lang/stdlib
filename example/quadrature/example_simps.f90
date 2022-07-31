program example_simps
  use stdlib_quadrature, only: simps
  implicit none
  real, parameter :: x(5) = [0., 1., 2., 3., 4.]
  real :: y(5) = 3.*x**2
  print *, simps(y, x)
! 64.0
  print *, simps(y, 0.5)
! 32.0
end program example_simps
