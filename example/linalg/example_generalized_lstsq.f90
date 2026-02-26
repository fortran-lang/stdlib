! Generalized least-squares solver with correlated errors
program example_generalized_lstsq
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: generalized_lstsq
  implicit none

  real(dp) :: A(3,2), b(3), W(3,3)
  real(dp), allocatable :: x(:)

  ! Design matrix: intercept + slope
  A(:,1) = 1.0_dp
  A(:,2) = [1.0_dp, 2.0_dp, 3.0_dp]

  ! Observations
  b = [1.0_dp, 2.1_dp, 2.9_dp]

  ! Covariance matrix (correlated errors)
  W(1,:) = [1.0_dp, 0.5_dp, 0.25_dp]
  W(2,:) = [0.5_dp, 1.0_dp, 0.5_dp]
  W(3,:) = [0.25_dp, 0.5_dp, 1.0_dp]

  ! Solve generalized least-squares
  x = generalized_lstsq(W, A, b)

  print '("GLS fit: intercept = ",f8.4,", slope = ",f8.4)', x(1), x(2)
  ! GLS fit: intercept =   0.0500, slope =   0.9500

end program example_generalized_lstsq
