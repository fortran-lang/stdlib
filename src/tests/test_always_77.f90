program test_always_77

use stdlib_error, only: check
implicit none

call check(.false., code=77)

end program
