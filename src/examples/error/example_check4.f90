program example_check4
  use stdlib_error, only: check
  implicit none
  integer :: a = 1
! If a /= 5, stops the program with exit code 77 and prints 'a == 5 failed.'
  call check(a == 5, msg='a == 5 failed.', code=77)
end program example_check4
