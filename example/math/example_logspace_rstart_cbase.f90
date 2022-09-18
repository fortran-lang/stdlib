program example_logspace_rstart_cbase
  use stdlib_math, only: logspace
  use stdlib_kinds, only: dp
  implicit none

  real(dp) :: start = 0.0_dp
  real(dp) :: end = 3.0_dp
  integer, parameter :: n = 4
  complex(dp) :: base = (0.0_dp, 1.0_dp)

  complex(dp) :: z(n) ! complex values raised to real powers result in complex values

  z = logspace(start, end, n, base)

end program example_logspace_rstart_cbase
