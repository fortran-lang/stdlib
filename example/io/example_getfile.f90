! Demonstrate usage of `getfile`
program example_getfile
  use stdlib_io, only: getfile
  use stdlib_string_type, only: string_type
  use stdlib_error, only: state_type
  implicit none

  character(*), parameter :: fileName = "example.txt"
  type(string_type) :: fileContent
  type(state_type) :: err

  ! Read a file into a string
  fileContent = getfile(fileName, err=err)

  if (err%error()) then
    print *, err%print()
  else
    print *, "Success! File "//fileName//" imported."
  end if
end program example_getfile
