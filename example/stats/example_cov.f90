program example_cov
  use stdlib_stats, only: cov
  implicit none
  real :: x(1:6) = [1., 2., 3., 4., 5., 6.]
  real :: y(1:2, 1:3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
  print *, cov(x, 1)                          !returns 3.5
  print *, cov(x, 1, corrected=.false.)     !returns 2.9167
  print *, cov(y, 1)                          !returns a square matrix of size 3 with all elements equal to 0.5
end program example_cov
