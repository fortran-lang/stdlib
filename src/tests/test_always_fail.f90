program test_always_fail

use stdlib_experimental_error, only : assert
implicit none

call assert(.false.)

end program
