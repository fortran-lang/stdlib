program example_moment
  use stdlib_stats, only: moment
  implicit none
  real :: x(1:6) = [1., 2., 3., 4., 5., 6.]
  real :: y(1:2, 1:3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
  print *, moment(x, 2)                            !returns 2.9167
  print *, moment(y, 2)                            !returns 2.9167
  print *, moment(y, 2, 1)                         !returns [0.25, 0.25, 0.25]
  print *, moment(y, 2, 1, mask=(y > 3.))        !returns [NaN, 0., 0.25]
  print *, moment(x, 2, center=0.)               !returns 15.1667
  print *, moment(y, 1, 1, center=0.)            !returns [1.5, 3.5, 5.5]
end program example_moment
