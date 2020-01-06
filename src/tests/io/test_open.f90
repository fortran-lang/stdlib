program test_open
use stdlib_experimental_io, only: open
use stdlib_experimental_error, only: assert
implicit none

character(:), allocatable :: filename
integer :: io, u, a(3)

! Stream text file
filename = get_outpath() // "/io_open_stream.dat"

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


! Sequential text file
filename = get_outpath() // "/io_open_seq.dat"

! Test mode "w"
u = open(filename, "w", access = 'sequential')
write(u, *) 1, 2, 3
close(u)

! Test mode "r"
u = open(filename, "r", access = 'sequential')
read(u, *) a
call assert(all(a == [1, 2, 3]))
close(u)

! Test mode "a"
u = open(filename, "a", access = 'sequential')
write(u, *) 4, 5, 6
close(u)
u = open(filename, "r", access = 'sequential')
read(u, *) a
call assert(all(a == [1, 2, 3]))
read(u, *) a
call assert(all(a == [4, 5, 6]))
close(u)


! Stream binary file
filename = get_outpath() // "/io_open_stream.bin"

! Test mode "w"
u = open(filename, "wb")
write(u) 1, 2, 3
close(u)

! Test mode "r"
u = open(filename, "rb")
read(u) a
call assert(all(a == [1, 2, 3]))
close(u)

! Test mode "a"
u = open(filename, "ab")
write(u) 4, 5, 6
close(u)
u = open(filename, "rb")
read(u) a
call assert(all(a == [1, 2, 3]))
read(u) a
call assert(all(a == [4, 5, 6]))
close(u)


! Sequential binary file
filename = get_outpath() // "/io_open_seq.bin"

! Test mode "w"
u = open(filename, "wb", access = 'sequential')
write(u) 1, 2, 3
close(u)

! Test mode "r"
u = open(filename, "rb", access = 'sequential')
read(u) a
call assert(all(a == [1, 2, 3]))
close(u)

! Test mode "a"
u = open(filename, "ab", access = 'sequential')
write(u) 4, 5, 6
close(u)
u = open(filename, "rb", access = 'sequential')
read(u) a
call assert(all(a == [1, 2, 3]))
read(u) a
call assert(all(a == [4, 5, 6]))
close(u)


!0 and non-0 open
filename = get_outpath() // "/io_open_stream.bin"

u = open(filename, "rb", io)
call assert(io == 0)
if (io == 0) close(u)

u = open(filename, "ab", io)
call assert(io == 0)
if (io == 0) close(u)


filename = get_outpath() // "/does_not_exist.error"

u = open(filename, "a", io)
call assert(io /= 0)

u = open(filename, "r", io)
call assert(io /= 0)


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
