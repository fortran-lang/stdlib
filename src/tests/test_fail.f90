program AlwaysFail

use stdlib_experimental_error, only : assert
implicit none

call assert(.false.)

end program