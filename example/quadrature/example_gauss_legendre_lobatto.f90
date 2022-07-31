program example_gauss_legendre_lobatto
  use iso_fortran_env, dp => real64
  use stdlib_quadrature, only: gauss_legendre_lobatto
  implicit none

  integer, parameter :: N = 6
  real(dp), dimension(N) :: x, w
  call gauss_legendre_lobatto(x, w)
  print *, "integral of x**2 from -1 to 1 is", sum(x**2*w)
end program example_gauss_legendre_lobatto
