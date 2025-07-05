! Demonstrate usage of `is_directory`
program example_is_directory
  use stdlib_system, only: is_directory
  implicit none
  ! Test a directory path
  if (is_directory("/path/to/check")) then 
    print *, "The specified path is a directory."
  else
    print *, "The specified path is not a directory."
  end if
end program example_is_directory
