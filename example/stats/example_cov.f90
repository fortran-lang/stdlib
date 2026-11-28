program example_cov
  use stdlib_stats, only: cov
  implicit none
  real :: x(1:6) = [1., 2., 3., 4., 5., 6.]
  real :: z(1:6) = [2., 4., 5., 8., 7., 10.]
  real :: c(2, 2)
  real :: y(1:2, 1:3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
  print *, cov(x, 1)                          !returns 3.5
  print *, cov(x, 1, corrected=.false.)     !returns 2.9167
  print *, cov(y, 1)                          !returns a square matrix of size 3 with all elements equal to 0.5
  c = cov(x, z)                                !returns [cov(x,x), cov(x,z); cov(z,x), cov(z,z)]
  print *, c
end program example_cov
