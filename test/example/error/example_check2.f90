program example_check2
  use stdlib_error, only: check
  implicit none
  integer :: a = 1
! If a /= 5, stops the program with exit code 1 and prints  'a == 5 failed.'
  call check(a == 5, msg='a == 5 failed.')
end program example_check2
