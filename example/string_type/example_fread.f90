program example_fread
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: io
  string = "Important saved value"

  open (newunit=io, form="formatted", status="scratch")
  write (io, *) string
  write (io, *)

  rewind (io)

  read (io, *) string
  close (io)
end program example_fread
