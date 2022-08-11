program example_trapz_weights
  use stdlib_quadrature, only: trapz_weights
  implicit none
  real, parameter :: x(5) = [0., 1., 2., 3., 4.]
  real :: y(5) = x**2
  real :: w(5)
  w = trapz_weights(x)
  print *, sum(w*y)
! 22.0
end program example_trapz_weights

