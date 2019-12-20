module stdlib_io
use stdlib_types
implicit none
private
public :: loadtxt, savetxt

contains

subroutine loadtxt(filename, d)
! Loads a 2D array from a text file.
!
! Arguments
! ---------
!
! Filename to load the array from
character(len=*), intent(in) :: filename
! The array 'd' will be automatically allocated with the correct dimensions
real(dp), allocatable, intent(out) :: d(:, :)
!
! Example
! -------
!
! real(dp), allocatable :: data(:, :)
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
character :: c
integer :: s, ncol, nrow, ios, i
logical :: lastwhite
real(dp) :: r

open(newunit=s, file=filename, status="old")

! determine number of columns
ncol = 0
lastwhite = .true.
do
   read(s, '(a)', advance='no', iostat=ios) c
   if (ios /= 0) exit
   if (lastwhite .and. .not. whitechar(c)) ncol = ncol + 1
   lastwhite = whitechar(c)
end do

rewind(s)

! determine number or rows
nrow = 0
do
   read(s, *, iostat=ios) r
   if (ios /= 0) exit
   nrow = nrow + 1
end do

rewind(s)

allocate(d(nrow, ncol))
do i = 1, nrow
    read(s, *) d(i, :)
end do
close(s)
end subroutine

subroutine savetxt(filename, d)
! Saves a 2D array into a textfile.
!
! Arguments
! ---------
!
character(len=*), intent(in) :: filename  ! File to save the array to
real(dp), intent(in) :: d(:, :)           ! The 2D array to save
!
! Example
! -------
!
! real(dp) :: data(3, 2)
! call savetxt("log.txt", data)

integer :: s, i
open(newunit=s, file=filename, status="replace")
do i = 1, size(d, 1)
    write(s, *) d(i, :)
end do
close(s)
end subroutine


logical function whitechar(char) ! white character
! returns .true. if char is space (32) or tab (9), .false. otherwise
character, intent(in) :: char
if (iachar(char) == 32 .or. iachar(char) == 9) then
    whitechar = .true.
else
    whitechar = .false.
end if
end function

end module
