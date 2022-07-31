program example_simps_weights
  use stdlib_quadrature, only: simps_weights
  implicit none
  real, parameter :: x(5) = [0., 1., 2., 3., 4.]
  real :: y(5) = 3.*x**2
  real :: w(5)
  w = simps_weights(x)
  print *, sum(w*y)
! 64.0
end program example_simps_weights
