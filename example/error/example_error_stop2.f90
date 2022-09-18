program example_error_stop2
  use stdlib_error, only: error_stop
  implicit none
  call error_stop("Invalid argument", code=123)
end program example_error_stop2
