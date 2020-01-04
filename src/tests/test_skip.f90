program AlwaysSkip

use stdlib_experimental_error, only : assert
implicit none

call assert(.false., 77)

end program
