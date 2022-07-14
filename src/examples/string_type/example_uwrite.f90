program example_uwrite
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: io
  string = "Important saved value"

  open (newunit=io, form="unformatted", status="scratch")
  write (io) string

  rewind (io)

  read (io) string
  close (io)
end program example_uwrite
