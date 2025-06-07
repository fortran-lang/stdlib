module test_path
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: joinpath, operator(/), splitpath, ISWIN
    implicit none
contains
    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('test_joinpath', test_joinpath), &
            new_unittest('test_joinpath_operator', test_joinpath_op), &
            new_unittest('test_splitpath', test_splitpath) &
        ]
    end subroutine collect_suite

    subroutine checkpath(error, funcname, expected, got)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: funcname
        character(len=*), intent(in) :: expected
        character(len=:), allocatable :: got
        character(len=:), allocatable :: message

        message = "'"//funcname//"'"//" error: Expected '"// expected // "' but got '" // got // "'"
        call check(error, expected == got, message)

    end subroutine checkpath

    subroutine test_joinpath(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path
        character(len=20) :: paths(5)

        if (ISWIN) then
            path = joinpath('C:\Users', 'Alice')
            call checkpath(error, 'joinpath', 'C:\Users\Alice', path)
            if (allocated(error)) return

            paths = [character(20) :: 'C:','Users','Bob','Pictures','2025']
            path = joinpath(paths)

            call checkpath(error, 'joinpath', 'C:\Users\Bob\Pictures\2025', path)
            if (allocated(error)) return
        else
            path = joinpath('/home', 'Alice')
            call checkpath(error, 'joinpath', '/home/Alice', path)
            if (allocated(error)) return

            paths = [character(20) :: '','home','Bob','Pictures','2025']
            path = joinpath(paths)

            call checkpath(error, 'joinpath', '/home/Bob/Pictures/2025', path)
            if (allocated(error)) return
        end if
    end subroutine test_joinpath

    !> Test the operator
    subroutine test_joinpath_op(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        if (ISWIN) then
            path = 'C:'/'Users'/'Alice'/'Desktop'
            call checkpath(error, 'joinpath operator', 'C:\Users\Alice\Desktop', path)
            if (allocated(error)) return
        else
            path = ''/'home'/'Alice'/'.config'
            call checkpath(error, 'joinpath operator', '/home/Alice/.config', path)
            if (allocated(error)) return
        end if
    end subroutine test_joinpath_op

    subroutine test_splitpath(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: head, tail

        call splitpath('', head, tail)
        call checkpath(error, 'splitpath-head', '.', head)
        if (allocated(error)) return
        call checkpath(error, 'splitpath-tail', '', tail)
        if (allocated(error)) return

        if (ISWIN) then
            call splitpath('\\\\', head, tail)
            call checkpath(error, 'splitpath-head', '\', head)
            if (allocated(error)) return
            call checkpath(error, 'splitpath-tail', '', tail)
            if (allocated(error)) return

            call splitpath('C:\', head, tail)
            call checkpath(error, 'splitpath-head', 'C:\', head)
            if (allocated(error)) return
            call checkpath(error, 'splitpath-tail', '', tail)
            if (allocated(error)) return

            call splitpath('C:\Users\Alice\\\\\', head, tail)
            call checkpath(error, 'splitpath-head', 'C:\Users', head)
            if (allocated(error)) return
            call checkpath(error, 'splitpath-tail', 'Alice', tail)
            if (allocated(error)) return
        else
            call splitpath('/////', head, tail)
            call checkpath(error, 'splitpath-head', '/', head)
            if (allocated(error)) return
            call checkpath(error, 'splitpath-tail', '', tail)
            if (allocated(error)) return

            call splitpath('/home/Alice/foo/bar.f90///', head, tail)
            call checkpath(error, 'splitpath-head', '/home/Alice/foo', head)
            if (allocated(error)) return
            call checkpath(error, 'splitpath-tail', 'bar.f90', tail)
            if (allocated(error)) return
        end if
    end subroutine test_splitpath

end module test_path

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_path, only : collect_suite

    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("path", collect_suite) &
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
