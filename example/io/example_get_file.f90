! Demonstrate usage of `get_file`
program example_get_file
  use stdlib_io, only: get_file
  use stdlib_string_type, only: string_type
  use stdlib_error, only: state_type
  implicit none

  character(*), parameter :: filename = "example.txt"
  type(string_type) :: filecontent
  type(state_type) :: err

  ! Read a file into a string
  call get_file(filename, filecontent, err=err)

  if (err%error()) then
    print *, err%print()
  else
    print *, "Success! File "//filename//" imported."
  end if
end program example_get_file
