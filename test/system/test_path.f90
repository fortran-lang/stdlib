module test_path
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: join_path, operator(/), split_path, OS_TYPE, OS_WINDOWS
    implicit none
contains
    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('test_join_path', test_join_path), &
            new_unittest('test_join_path_operator', test_join_path_op), &
            new_unittest('test_split_path', test_split_path) &
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

    subroutine test_join_path(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path
        character(len=20) :: paths(5)

        if (OS_TYPE() == OS_WINDOWS) then
            path = join_path('C:\Users', 'Alice')
            call checkpath(error, 'join_path', 'C:\Users\Alice', path)
            if (allocated(error)) return

            paths = [character(20) :: 'C:','Users','Bob','Pictures','2025']
            path = join_path(paths)

            call checkpath(error, 'join_path', 'C:\Users\Bob\Pictures\2025', path)
            if (allocated(error)) return

            path = join_path('"C:\Users\John Doe"', 'Pictures\2025') ! path with spaces
            call checkpath(error, 'join_path', '"C:\Users\John Doe"\Pictures\2025', path)
            if (allocated(error)) return
        else
            path = join_path('/home', 'Alice')
            call checkpath(error, 'join_path', '/home/Alice', path)
            if (allocated(error)) return

            paths = [character(20) :: '','home','Bob','Pictures','2025']
            path = join_path(paths)

            call checkpath(error, 'join_path', '/home/Bob/Pictures/2025', path)
            if (allocated(error)) return
        end if
    end subroutine test_join_path

    !> Test the operator
    subroutine test_join_path_op(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: path

        if (OS_TYPE() == OS_WINDOWS) then
            path = 'C:'/'Users'/'Alice'/'Desktop'
            call checkpath(error, 'join_path operator', 'C:\Users\Alice\Desktop', path)
            if (allocated(error)) return
        else
            path = ''/'home'/'Alice'/'.config'
            call checkpath(error, 'join_path operator', '/home/Alice/.config', path)
            if (allocated(error)) return
        end if
    end subroutine test_join_path_op

    subroutine test_split_path(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: head, tail

        call split_path('', head, tail)
        call checkpath(error, 'split_path-head', '.', head)
        if (allocated(error)) return
        call checkpath(error, 'split_path-tail', '', tail)
        if (allocated(error)) return

        if (OS_TYPE() == OS_WINDOWS) then
            call split_path('\\\\', head, tail)
            call checkpath(error, 'split_path-head', '\', head)
            if (allocated(error)) return
            call checkpath(error, 'split_path-tail', '', tail)
            if (allocated(error)) return

            call split_path('C:\', head, tail)
            call checkpath(error, 'split_path-head', 'C:\', head)
            if (allocated(error)) return
            call checkpath(error, 'split_path-tail', '', tail)
            if (allocated(error)) return

            call split_path('C:\Users\Alice\\\\\', head, tail)
            call checkpath(error, 'split_path-head', 'C:\Users', head)
            if (allocated(error)) return
            call checkpath(error, 'split_path-tail', 'Alice', tail)
            if (allocated(error)) return
        else
            call split_path('/////', head, tail)
            call checkpath(error, 'split_path-head', '/', head)
            if (allocated(error)) return
            call checkpath(error, 'split_path-tail', '', tail)
            if (allocated(error)) return

            call split_path('/home/Alice/foo/bar.f90///', head, tail)
            call checkpath(error, 'split_path-head', '/home/Alice/foo', head)
            if (allocated(error)) return
            call checkpath(error, 'split_path-tail', 'bar.f90', tail)
            if (allocated(error)) return
        end if
    end subroutine test_split_path

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
