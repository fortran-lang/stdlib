program example_logspace_int
  use stdlib_math, only: logspace
  use stdlib_kinds, only: dp
  implicit none

  integer, parameter :: start = 10
  integer, parameter :: end = 23
  integer, parameter :: n = 15

  real(dp) :: r(n) ! Integer values raised to real powers results in real values

  r = logspace(start, end, n)
end program example_logspace_int
