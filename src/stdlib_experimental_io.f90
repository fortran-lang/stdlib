module stdlib_experimental_io
use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
use stdlib_experimental_error, only: error_stop
implicit none
private
public :: loadtxt, savetxt, open

interface loadtxt
    module procedure sloadtxt
    module procedure dloadtxt
    module procedure qloadtxt
end interface

interface savetxt
    module procedure ssavetxt
    module procedure dsavetxt
    module procedure qsavetxt
end interface

contains

subroutine sloadtxt(filename, d)
! Loads a 2D array from a text file.
!
! Arguments
! ---------
!
! Filename to load the array from
character(len=*), intent(in) :: filename
! The array 'd' will be automatically allocated with the correct dimensions
real(sp), allocatable, intent(out) :: d(:,:)
!
! Example
! -------
!
! real(sp), allocatable :: data(:, :)
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
end subroutine

subroutine dloadtxt(filename, d)
! Loads a 2D array from a text file.
!
! Arguments
! ---------
!
! Filename to load the array from
character(len=*), intent(in) :: filename
! The array 'd' will be automatically allocated with the correct dimensions
real(dp), allocatable, intent(out) :: d(:,:)
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
end subroutine

subroutine qloadtxt(filename, d)
! Loads a 2D array from a text file.
!
! Arguments
! ---------
!
! Filename to load the array from
character(len=*), intent(in) :: filename
! The array 'd' will be automatically allocated with the correct dimensions
real(qp), allocatable, intent(out) :: d(:,:)
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
end subroutine


subroutine ssavetxt(filename, d)
! Saves a 2D array into a textfile.
!
! Arguments
! ---------
!
character(len=*), intent(in) :: filename  ! File to save the array to
real(sp), intent(in) :: d(:,:)           ! The 2D array to save
!
! Example
! -------
!
! real(sp) :: data(3, 2)
! call savetxt("log.txt", data)

integer :: s, i
s = open(filename, "w")
do i = 1, size(d, 1)
    write(s, *) d(i, :)
end do
close(s)
end subroutine

subroutine dsavetxt(filename, d)
! Saves a 2D array into a textfile.
!
! Arguments
! ---------
!
character(len=*), intent(in) :: filename  ! File to save the array to
real(dp), intent(in) :: d(:,:)           ! The 2D array to save
!
! Example
! -------
!
! real(dp) :: data(3, 2)
! call savetxt("log.txt", data)

integer :: s, i
s = open(filename, "w")
do i = 1, size(d, 1)
    write(s, *) d(i, :)
end do
close(s)
end subroutine

subroutine qsavetxt(filename, d)
! Saves a 2D array into a textfile.
!
! Arguments
! ---------
!
character(len=*), intent(in) :: filename  ! File to save the array to
real(qp), intent(in) :: d(:,:)           ! The 2D array to save
!
! Example
! -------
!
! real(dp) :: data(3, 2)
! call savetxt("log.txt", data)

integer :: s, i
s = open(filename, "w")
do i = 1, size(d, 1)
    write(s, *) d(i, :)
end do
close(s)
end subroutine


integer function number_of_columns(s)
 ! determine number of columns
 integer,intent(in)::s

 integer :: ios
 character :: c
 logical :: lastwhite

 rewind(s)
 number_of_columns = 0
 lastwhite = .true.
 do
    read(s, '(a)', advance='no', iostat=ios) c
    if (ios /= 0) exit
    if (lastwhite .and. .not. whitechar(c)) number_of_columns = number_of_columns + 1
    lastwhite = whitechar(c)
 end do
 rewind(s)

end function

integer function number_of_rows_numeric(s)
 ! determine number or rows
 integer,intent(in)::s
 integer :: ios

 real::r

 rewind(s)
 number_of_rows_numeric = 0
 do
    read(s, *, iostat=ios) r
    if (ios /= 0) exit
    number_of_rows_numeric = number_of_rows_numeric + 1
 end do

 rewind(s)

end function

logical function whitechar(char) ! white character
! returns .true. if char is space (32) or tab (9), .false. otherwise
character, intent(in) :: char
if (iachar(char) == 32 .or. iachar(char) == 9) then
    whitechar = .true.
else
    whitechar = .false.
end if
end function

integer function open(filename, mode) result(u)
! Open a file
!
! To open a file to read:
!
! u = open("somefile.txt")        # The default `mode` is "rt"
! u = open("somefile.txt", "r")
!
! To open a file to write:
!
! u = open("somefile.txt", "w")

! To append to the end of the file if it exists:
!
! u = open("somefile.txt", "a")

character(*), intent(in) :: filename
character(*), intent(in), optional :: mode
character(:), allocatable :: mode_
mode_ = "rt"
if (present(mode)) mode_ = mode
! Note: the Fortran standard says that the default values for `status` and
! `action` are processor dependent, so we have to explicitly set them below
if (mode_ == "r" .or. mode_ == 'rt') then
    open(newunit=u, file=filename, status="old", action="read", &
             access='sequential', form='formatted')
else if (mode_ == "w" .or. mode_ == "wt") then
    open(newunit=u, file=filename, status="replace", action="write", &
              access='sequential', form='formatted')
else if (mode_ == "a" .or. mode_ == "at") then
    open(newunit=u, file=filename, position="append", status="old", &
        action="write", access='sequential', form='formatted')
else if (mode_ == "x" .or. mode_ == "xt") then
    open(newunit=u, file=filename, status="new", &
            action="write", access='sequential', form='formatted')
else
    call error_stop("Unsupported mode")
end if
end function

end module
