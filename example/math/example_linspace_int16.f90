program example_linspace_int16
  use stdlib_math, only: linspace
  use stdlib_kinds, only: int16, dp
  implicit none

  integer(int16) :: start = 10_int16
  integer(int16) :: end = 23_int16

  real(dp) :: r(15)

  r = linspace(start, end, 15)
end program example_linspace_int16
