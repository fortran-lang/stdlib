program test_open
use stdlib_experimental_io, only: open, parse_mode
use stdlib_experimental_error, only: assert
implicit none

character(:), allocatable :: filename
integer :: u, a(3)

call test_parse_mode()

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

    subroutine test_parse_mode()
    character(3) :: m
    m = parse_mode("")
    call assert(m == "r t")

    m = parse_mode("r")
    call assert(m == "r t")
    m = parse_mode("w")
    call assert(m == "w t")
    m = parse_mode("a")
    call assert(m == "a t")

    m = parse_mode("rb")
    call assert(m == "r b")
    m = parse_mode("wb")
    call assert(m == "w b")
    m = parse_mode("ab")
    call assert(m == "a b")

    m = parse_mode("br")
    call assert(m == "r b")
    m = parse_mode("bw")
    call assert(m == "w b")
    m = parse_mode("ba")
    call assert(m == "a b")

    m = parse_mode("r+")
    call assert(m == "r+t")
    m = parse_mode("w+")
    call assert(m == "w+t")
    m = parse_mode("a+")
    call assert(m == "a+t")

    m = parse_mode("r+b")
    call assert(m == "r+b")
    m = parse_mode("w+b")
    call assert(m == "w+b")
    m = parse_mode("a+b")
    call assert(m == "a+b")
    end subroutine

end program
