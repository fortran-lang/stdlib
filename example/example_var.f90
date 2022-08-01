program example_var
  use stdlib_stats, only: var
  implicit none
  real :: x(1:6) = [1., 2., 3., 4., 5., 6.]
  real :: y(1:2, 1:3) = reshape([1., 2., 3., 4., 5., 6.], [2, 3])
  print *, var(x)                                  !returns 3.5
  print *, var(x, corrected=.false.)             !returns 2.9167
  print *, var(y)                                  !returns 3.5
  print *, var(y, 1)                               !returns [0.5, 0.5, 0.5]
  print *, var(y, 1, y > 3.)                       !returns [NaN, NaN, 0.5]
  print *, var(y, 1, y > 3., corrected=.false.)    !returns [NaN, 0., 0.25]
end program example_var
