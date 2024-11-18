module test_cursor
    use stdlib_ansi_cursor, only: move_to, move_up, move_to_column
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_cursor_tests(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("move_to", test_move_to), &
                    new_unittest("move_<direction>", test_move_direction), &
                    new_unittest("move_to_column", test_move_to_column) &
                    ]
    end subroutine collect_cursor_tests

    subroutine test_move_to(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: str

        str = move_to(-10, 20)
        call check(error, str, "")
        if (allocated(error)) then
            print *, "ERROR: move_to fails with negative values"
            return
        end if
        str = move_to(10, 20)
        call check(error, iachar(str(1:1)), 27)
        if (allocated(error)) then
            print *, "ERROR: move_to doesn't add ESC character at the beginning"
            return
        end if
        call check(error, str(2:), "[10;20H")
        if (allocated(error)) then
            print *, "ERROR: move_to logically failed"
            return
        end if
    end subroutine test_move_to

    subroutine test_move_direction(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: str

        str = move_up(-15)
        call check(error, str, "")
        if (allocated(error)) then
            print *, "ERROR: move_up fails with negative values"
            return
        end if
        str = move_up(15)
        call check(error, iachar(str(1:1)), 27)
        if (allocated(error)) then
            print *, "ERROR: move_up doesn't add ESC character at the beginning"
            return
        end if
        call check(error, str(2:), "[15A")
        if (allocated(error)) then
            print *, "ERROR: move_up logically failed"
            return
        end if
    end subroutine test_move_direction

    subroutine test_move_to_column(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: str

        str = move_to_column(-5)
        call check(error, str, "")
        if (allocated(error)) then
            print *, "ERROR: move_to_column fails with negative values"
            return
        end if
        str = move_to_column(5)
        call check(error, iachar(str(1:1)), 27)
        if (allocated(error)) then
            print *, "ERROR: move_to_column doesn't add ESC character at the beginning"
            return
        end if
        call check(error, str(2:), "[5G")
        if (allocated(error)) then
            print *, "ERROR: move_to_column logically fails"
            return
        end if
    end subroutine test_move_to_column

end module test_cursor

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use test_cursor, only: collect_cursor_tests
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("cursor ansi codes", collect_cursor_tests) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester

