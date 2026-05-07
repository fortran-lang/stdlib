program example_getline
  use, intrinsic :: iso_fortran_env, only: input_unit, output_unit
  use stdlib_io, only: get_line
  implicit none
  character(len=:), allocatable :: line
  integer :: stat

  call get_line(input_unit, line, stat)
  do while (stat == 0)
    write (output_unit, '(a)') line
    call get_line(input_unit, line, stat)
  end do
end program example_getline
