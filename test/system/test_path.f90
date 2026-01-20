module test_path
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_system, only: join_path, operator(/), split_path, OS_TYPE, OS_WINDOWS, &
        is_abs_path, abs_path, get_cwd
    use stdlib_error, only: state_type
    implicit none
contains
    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('test_join_path', test_join_path), &
            new_unittest('test_join_path_operator', test_join_path_op), &
            new_unittest('test_split_path', test_split_path), &
            new_unittest('test_is_abs_path', test_is_abs_path), &
            new_unittest('test_abs_path', test_abs_path) &
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

    subroutine test_is_abs_path(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: p
        logical :: res

        character(*), parameter :: msg = "is_abs_path: "

        if (OS_TYPE() == OS_WINDOWS) then
            p = '.'
            res = is_abs_path(p)
            call check(error, .not. res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            p = '..'
            res = is_abs_path(p)
            call check(error, .not. res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            p = 'C:\Windows'
            res = is_abs_path(p)
            call check(error, res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            ! a relative path pointing to the `Windows` folder
            ! in the current working directory in the drive C
            p = 'C:Windows'
            res = is_abs_path(p)
            call check(error, .not. res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            ! UNC paths
            p = '\\server_name\share_name\path'
            res = is_abs_path(p)
            call check(error, res, msg // p // " returns incorrect result")
            if (allocated(error)) return
        else
            p = '.'
            res = is_abs_path(p)
            call check(error, .not. res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            p = '..'
            res = is_abs_path(p)
            call check(error, .not. res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            p = '/'
            res = is_abs_path(p)
            call check(error, res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            p = '/home/Alice'
            res = is_abs_path(p)
            call check(error, res, msg // p // " returns incorrect result")
            if (allocated(error)) return

            p = './home/Alice'
            res = is_abs_path(p)
            call check(error, .not. res, msg // p // " returns incorrect result")
            if (allocated(error)) return
        end if
    end subroutine test_is_abs_path

    subroutine test_abs_path(error)
        type(error_type), allocatable, intent(out) :: error
        character(:), allocatable :: rel_path, absolute_path, cwd, absolute_path0
        type(state_type) :: err

        if (OS_TYPE() == OS_WINDOWS) then
            rel_path = ".\Folder\File"
        else
            rel_path = "./Folder/File"
        end if

        absolute_path = abs_path(rel_path, err)

        call check(error, err%ok(), "Could not get absolute path: " // err%print())
        if (allocated(error)) return

        call check(error, is_abs_path(absolute_path), "absolute path created is not absolute")
        if (allocated(error)) return

        call get_cwd(cwd, err)

        ! ideally shouldn't error out but just in case it does
        call check(error, err%ok(), "Could not get CWD: " // err%print())
        if (allocated(error)) return

        absolute_path0 = cwd / rel_path

        call check(error, absolute_path == absolute_path0, "absolute path != (CWD / relative path)" &
        // "absolute_path: " // absolute_path // " and (CWD / relative path): " // absolute_path0)
        if (allocated(error)) return
    end subroutine test_abs_path

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
