module test_open
    use stdlib_io, only: open
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: collect_open
contains

    !> Collect all exported unit tests
    subroutine collect_open(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("io_read_write_text", test_io_read_write_text), &
            new_unittest("io_read_write_stream", test_io_read_write_stream), &
            new_unittest("io_open_error_flag", test_io_open_error_flag) &
        ]

    end subroutine collect_open

    function get_outpath() result(outpath)
        integer :: ierr
        character(256) :: argv
        character(:), allocatable :: outpath

        call get_command_argument(1, argv, status=ierr)
        if (ierr == 0) then
            outpath = trim(argv)
        else
            outpath = '.'
        end if
    end function get_outpath


    subroutine test_io_read_write_text(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: filename
        integer :: u, a(3)

        ! Text file
        filename = get_outpath() // "/io_open.dat"

        ! Test mode "w"
        u = open(filename, "w")
        write(u, *) 1, 2, 3
        close(u)

        ! Test mode "r"
        u = open(filename, "r")
        read(u, *) a
        call check(error, all(a == [1, 2, 3]))
        close(u)
        if (allocated(error)) return

        ! Test mode "a"
        u = open(filename, "a")
        write(u, *) 4, 5, 6
        close(u)
        u = open(filename, "r")
        read(u, *) a
        call check(error, all(a == [1, 2, 3]))
        read(u, *) a
        call check(error, all(a == [4, 5, 6]))
        close(u)
        if (allocated(error)) return

    end subroutine test_io_read_write_text


    subroutine test_io_read_write_stream(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: filename
        integer :: u, a(3)

        ! Stream file
        filename = get_outpath() // "/io_open.stream"

        ! Test mode "w"
        u = open(filename, "wb")
        write(u) 1, 2, 3
        close(u)

        ! Test mode "r"
        u = open(filename, "rb")
        read(u) a
        call check(error, all(a == [1, 2, 3]))
        close(u)
        if (allocated(error)) return

        ! Test mode "a"
        u = open(filename, "ab")
        write(u) 4, 5, 6
        close(u)
        u = open(filename, "rb")
        read(u) a
        call check(error, all(a == [1, 2, 3]))
        read(u) a
        if (allocated(error)) return
        call check(error, all(a == [4, 5, 6]))
        close(u)
        if (allocated(error)) return

    end subroutine test_io_read_write_stream


    subroutine test_io_open_error_flag(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: filename
        integer :: ierr, u

        filename = get_outpath() // "/io_open.stream"

        ! Write to file first to ensure that it exists
        u = open(filename, "wb")
        write(u) 1, 2, 3
        close(u)

        u = open(filename, "rb", ierr)
        call check(error, ierr == 0)
        if (ierr == 0) close(u)
        if (allocated(error)) return

        u = open(filename, "ab", ierr)
        call check(error, ierr == 0)
        if (ierr == 0) close(u)
        if (allocated(error)) return

        filename = get_outpath() // "/does_not_exist.error"

        u = open(filename, "a", ierr)
        call check(error, ierr /= 0)
        if (allocated(error)) return

        u = open(filename, "r", ierr)
        call check(error, ierr /= 0)
        if (allocated(error)) return

    end subroutine test_io_open_error_flag

end module test_open


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_open, only : collect_open
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("open", collect_open) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
