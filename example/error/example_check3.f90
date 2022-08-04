program example_check3
  use stdlib_error, only: check
  implicit none
  integer :: a = 1
! If a /= 5,  prints 'a == 5 failed.', but doesn't stop the program.
  call check(a == 5, msg='a == 5 failed.', warn=.true.)
end program example_check3
