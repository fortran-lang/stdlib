! SPDX-Identifer: MIT
module test_string_derivedtype_io
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_string_type, only : string_type, assignment(=), len, &
        write(formatted), read(formatted), write(unformatted), read(unformatted), &
        operator(==)
    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_string_derivedtype_io(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("listdirected_io", test_listdirected_io), &
            new_unittest("formatted_io", test_formatted_io), &
            new_unittest("unformatted_io", test_unformatted_io) &
            ]
    end subroutine collect_string_derivedtype_io

    subroutine test_listdirected_io(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        integer :: io, stat
        string = "Important saved value"

        open(newunit=io, form="formatted", status="scratch")
        write(io, *) string
        write(io, *) ! Pad with a newline or we might run into EOF while reading

        string = ""
        rewind(io)

        read(io, *, iostat=stat) string
        close(io)

        call check(error, stat == 0)
        if (allocated(error)) return
        call check(error, len(string) == 21)
        if (allocated(error)) return
        call check(error, string == "Important saved value")
    end subroutine test_listdirected_io

    subroutine test_formatted_io(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        integer :: io, stat
        string = "Important saved value"

        open(newunit=io, form="formatted", status="scratch")
        write(io, '(dt)') string
        write(io, '(a)') ! Pad with a newline or we might run into EOF while reading

        string = ""
        rewind(io)

        read(io, *, iostat=stat) string
        close(io)

        call check(error, stat == 0)
        if (allocated(error)) return
        call check(error, len(string) == 21)
        if (allocated(error)) return
        call check(error, string == "Important saved value")
    end subroutine test_formatted_io

    subroutine test_unformatted_io(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        integer :: io
        string = "Important saved value"

        open(newunit=io, form="unformatted", status="scratch")
        write(io) string

        string = ""
        rewind(io)

        read(io) string
        close(io)

        call check(error, len(string) == 21)
        if (allocated(error)) return
        call check(error, string == "Important saved value")
    end subroutine test_unformatted_io

end module test_string_derivedtype_io


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_string_derivedtype_io, only : collect_string_derivedtype_io
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("string-derivedtype-io", collect_string_derivedtype_io) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
