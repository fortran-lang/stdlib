program demo_logspace_int
use stdlib_math, only: logspace
use stdlib_kinds, only: dp
implicit none

integer :: start = 10
integer :: end = 23
integer :: n = 15

real(dp) :: r(n) ! Integer values raised to real powers results in real values

r = logspace(start, end, n)
end program demo_logspace_int
