module stdlib_experimental_io
use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
use stdlib_experimental_error, only: error_stop
use stdlib_experimental_optval, only: optval
implicit none
private
! Public API
public :: loadtxt, savetxt, open

! Private API that is exposed so that we can test it in tests
public :: parse_mode


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
integer :: io
character(3):: mode_
character(:),allocatable :: action_, position_, status_, access_, form_


mode_ = parse_mode(optval(mode, ""))

if (mode_(1:2) == 'r ') then
    action_='read'
    position_='asis'
    status_='old'
else if (mode_(1:2) == 'w ') then
    action_='write'
    position_='asis'
    status_='replace'
else if (mode_(1:2) == 'a ') then
    action_='write'
    position_='append'
    status_='old'
else if (mode_(1:2) == 'x ') then
    action_='write'
    position_='asis'
    status_='new'
else if (mode_(1:2) == 'r+') then
    action_='readwrite'
    position_='asis'
    status_='old'
else if (mode_(1:2) == 'w+') then
    action_='readwrite'
    position_='asis'
    status_='replace'
else if (mode_(1:2) == 'a+') then
    action_='readwrite'
    position_='append'
    status_='old'
else if (mode_(1:2) == 'x+') then
    action_='readwrite'
    position_='asis'
    status_='new'
else
    call error_stop("Unsupported mode: "//mode_(1:2))
end if

if (mode_(3:3) == 't') then
    access_='sequential'
    form_='formatted'
else if (mode_(3:3) == 'b' .or. mode_(3:3) == 's') then
    access_='stream'
    form_='unformatted'
else
    call error_stop("Unsupported mode: "//mode_(3:3))
endif

open(newunit=u, file=filename, &
     action = action_, position = position_, status = status_, &
     access = access_, form = form_, &
     iostat = io)

end function

character(3) function parse_mode(mode) result(mode_)
character(*), intent(in) :: mode

integer::i
character(:),allocatable::a

mode_ = 'r t'

if (len_trim(mode) == 0) return
a=trim(adjustl(mode))

do i=1,len(a)
    if (a(i:i) == 'r'  &
        .or. a(i:i) == 'w' &
        .or. a(i:i) == 'a' &
        .or. a(i:i) == 'x' &
        ) then
        mode_(1:1) = a(i:i)
    else if (a(i:i) == '+') then
        mode_(2:2) = a(i:i)
    else if (a(i:i) == 't' .or. a(i:i) == 'b') then
        mode_(3:3) = a(i:i)
    else if (a(i:i) == ' ') then
     cycle
    else
        call error_stop("Wrong character: "//a(i:i))
    endif
end do

end function

end module
