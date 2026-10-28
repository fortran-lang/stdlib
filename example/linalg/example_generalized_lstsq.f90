! Generalized least-squares solver with correlated errors
program example_generalized_lstsq
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: generalized_lstsq, solve_generalized_lstsq
  implicit none

  integer, parameter :: m = 3
  real(dp) :: A(m,2), b(m), W(m,m), x(2)
  real(dp), allocatable :: x_fun(:)

  ! Design matrix: intercept + slope
  A(:,1) = 1.0_dp
  A(:,2) = [1.0_dp, 2.0_dp, 3.0_dp]

  ! Observations
  b = [1.0_dp, 2.1_dp, 2.9_dp]

  ! Covariance matrix (correlated errors)
  W = reshape([1.0_dp,  0.5_dp,  0.25_dp, &
               0.5_dp,  1.0_dp,  0.5_dp,  &
               0.25_dp, 0.5_dp,  1.0_dp], [m, m])

  ! Function interface: allocates solution
  x_fun = generalized_lstsq(W, A, b)
  print '("GLS (function):   intercept = ",f8.4,", slope = ",f8.4)', x_fun(1), x_fun(2)

  ! Subroutine interface: user-provided solution vector
  call solve_generalized_lstsq(W, A, b, x)
  print '("GLS (subroutine): intercept = ",f8.4,", slope = ",f8.4)', x(1), x(2)

end program example_generalized_lstsq
