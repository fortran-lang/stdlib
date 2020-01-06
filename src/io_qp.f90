submodule (stdlib_experimental_io) io_qp

use stdlib_experimental_kinds, only : qp

implicit none

contains

module procedure qloadtxt
! Loads a 2D array from a text file.
!
! Arguments
! ---------
!
! Filename to load the array from
! The array 'd' will be automatically allocated with the correct dimensions
!
! Example
! -------
!
! real(qp), allocatable :: data(:, :)
! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
!
! Where 'log.txt' contains for example::
!
!     1 2 3
!     2 4 6
!     8 9 10
!     11 12 13
!     ...
!
integer :: s
integer :: nrow,ncol,i

s = open(filename)

! determine number of columns
ncol = number_of_columns(s)

! determine number or rows
nrow = number_of_rows_numeric(s)

allocate(d(nrow, ncol))
do i = 1, nrow
    read(s, *) d(i, :)
end do
close(s)
end procedure

module procedure qsavetxt
  ! Saves a 2D array into a textfile.
  !
  ! Arguments
  ! ---------
  !
  !
  ! Example
  ! -------
  !
  ! real(dp) :: data(3, 2)
  ! call savetxt("log.txt", data)

  integer :: s, i
  character(len=14) :: format_string

  write(format_string, '(a1,i06,a7)') '(', size(d, 2), 'f40.34)'
  s = open(filename, "w")
  do i = 1, size(d, 1)
      write(s, format_string) d(i, :)
  end do
  close(s)
end procedure

end submodule