program test_open
use stdlib_experimental_io, only: open
use stdlib_experimental_error, only: assert
implicit none

character(:), allocatable :: filename
integer :: io, u, a(3)

!Wrong open
filename = get_outpath() // "/does_not_exist.error"

u = open(filename, "a", io)
call assert(io /= 0)

u = open(filename, "r", io)
call assert(io /= 0)


! Text file
filename = get_outpath() // "/io_open.dat"

! Test mode "w"
u = open(filename, "w")
write(u, *) 1, 2, 3
close(u)

! Test mode "r"
u = open(filename, "r")
read(u, *) a
call assert(all(a == [1, 2, 3]))
close(u)

! Test mode "a"
u = open(filename, "a")
write(u, *) 4, 5, 6
close(u)
u = open(filename, "r")
read(u, *) a
call assert(all(a == [1, 2, 3]))
read(u, *) a
call assert(all(a == [4, 5, 6]))
close(u)



! Stream file
filename = get_outpath() // "/io_open.stream"

! Test mode "w"
u = open(filename, "wb", io)
call assert(io == 0)
write(u) 1, 2, 3
close(u)

! Test mode "r"
u = open(filename, "rb", io)
call assert(io == 0)
read(u) a
call assert(all(a == [1, 2, 3]))
close(u)

! Test mode "a"
u = open(filename, "ab", io)
call assert(io == 0)
write(u) 4, 5, 6
close(u)
u = open(filename, "rb", io)
call assert(io == 0)
read(u) a
call assert(all(a == [1, 2, 3]))
read(u) a
call assert(all(a == [4, 5, 6]))
close(u)

contains

    function get_outpath() result(outpath)
    integer :: ierr
    character(256) :: argv
    character(:), allocatable :: outpath

    call get_command_argument(1, argv, status=ierr)
    if (ierr==0) then
        outpath = trim(argv)
    else
        outpath = '.'
    endif
    end function get_outpath

end program
