program demo_error_stop1
use stdlib_error, only: error_stop
implicit none
call error_stop("Invalid argument")
end program demo_error_stop1
