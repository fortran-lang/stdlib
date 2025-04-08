! Demonstrate usage of `delete_file`
program example_delete_file
  use stdlib_system, only: delete_file
  use stdlib_error, only: state_type
  implicit none

  type(state_type) :: err
  character(*), parameter :: filename = "example.txt"

  ! Delete a file with error handling
  call delete_file(filename, err)

  if (err%error()) then
    print *, err%print() 
  else
    print *, "File "//filename//" deleted successfully."
  end if
end program example_delete_file
