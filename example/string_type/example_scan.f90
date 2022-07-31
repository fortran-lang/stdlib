program example_scan
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: pos

  string = "fortran"
  pos = scan(string, "ao")
! pos == 2

  pos = scan(string, "ao", .true.)
! pos == 6

  pos = scan(string, "c++")
! pos == 0
end program example_scan
