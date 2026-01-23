! Weighted least-squares solver
program example_weighted_lstsq
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: weighted_lstsq
  implicit none

  real(dp) :: A(4,2), b(4), w(4)
  real(dp), allocatable :: x(:)

  ! Design matrix: intercept + slope
  A(:,1) = 1.0_dp
  A(:,2) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp]

  ! Observations (one outlier at position 3)
  b = [2.1_dp, 4.0_dp, 10.0_dp, 7.9_dp]

  ! Weights: downweight the outlier
  w = [1.0_dp, 1.0_dp, 0.1_dp, 1.0_dp]

  ! Solve weighted least-squares
  x = weighted_lstsq(w, A, b)

  print '("Weighted fit: intercept = ",f8.4,", slope = ",f8.4)', x(1), x(2)
  ! Weighted fit: intercept =   0.0667, slope =   1.9556

end program example_weighted_lstsq
