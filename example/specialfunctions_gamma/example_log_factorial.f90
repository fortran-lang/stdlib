program example_log_factorial
  use stdlib_kinds, only: int64
  use stdlib_specialfunctions_gamma, only: lf => log_factorial
  implicit none
  integer :: n

  n = 10
  print *, lf(n)

! 15.1044130

  print *, lf(35_int64)

! 92.1361771
end program example_log_factorial
