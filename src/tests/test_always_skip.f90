program test_always_skip

use stdlib_experimental_error, only : assert
implicit none

call assert(.false., 77)

end program
