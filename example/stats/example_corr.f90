program example_corr
  use stdlib_stats, only: corr
  implicit none
  real :: x(1:6) = [1., 2., 3., 4., 5., 6.]
  real :: y(1:2, 1:3) = reshape([-1., 40., -3., 4., 10., 6.], [2, 3])
  print *, corr(x, 1)           !returns 1.
  print *, corr(y, 2)           !returns reshape([ 1., -.32480, -.32480, 1. ], [ 2, 3])
end program example_corr
