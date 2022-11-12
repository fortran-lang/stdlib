program example_verify
  use stdlib_string_type
  implicit none
  type(string_type) :: string
  integer :: pos

  string = "fortran"
  pos = verify(string, "ao")
! pos == 1

  pos = verify(string, "fo")
! pos == 3

  pos = verify(string, "c++")
! pos == 1

  pos = verify(string, "c++", back=.true.)
! pos == 7

  pos = verify(string, string)
! pos == 0
end program example_verify
