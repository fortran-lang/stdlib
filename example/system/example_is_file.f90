! Demonstrate usage of `is_file`
program example_is_file
  use stdlib_system, only: is_file
  implicit none

  character(*), parameter :: path = "path/to/check"

  ! Test if path is a regular file
  if (is_file(path)) then
    print *, "The specified path is a regular file."
  else
    print *, "The specified path is not a regular file."
  end if
end program example_is_file
