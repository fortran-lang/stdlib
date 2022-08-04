program example_check1
  use stdlib_error, only: check
  implicit none
  integer :: a = 1
! If a /= 5, stops the program with exit code 1 and prints 'Check failed.'
  call check(a == 5)
end program example_check1
