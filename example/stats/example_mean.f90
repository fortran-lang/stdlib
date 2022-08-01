program example_mean
  use stdlib_stats, only: mean
  implicit none
  real :: x(1:6) = [1., 2., 3., 4., 5., 6.]
  real :: y(1:2, 1:3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
  print *, mean(x)                                  !returns 3.5
  print *, mean(y)                                  !returns 3.5
  print *, mean(y, 1)                               !returns [ 1.5, 3.5, 5.5 ]
  print *, mean(y, 1, y > 3.)                       !returns [ NaN, 4.0, 5.5 ]
end program example_mean
