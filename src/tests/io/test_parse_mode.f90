module test_parse_mode
    use stdlib_ascii, only: reverse
    use stdlib_io, only: parse_mode
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: collect_parse_mode

    character(3), parameter :: parse_modes_input(*) = [ &
        "   ", &
        "r  ", "w  ", "a  ", "x  ", &
        "rt ", "wt ", "at ", "xt ", &
        "rb ", "wb ", "ab ", "xb ", &
        "r+ ", "w+ ", "a+ ", "x+ ", &
        "r+t", "w+t", "a+t", "x+t", &
        "r+b", "w+b", "a+b", "x+b"  &
    ]

    character(3), parameter :: parse_modes_expected(*) = [ &
        "r t", &
        "r t", "w t", "a t", "x t", &
        "r t", "w t", "a t", "x t", &
        "r b", "w b", "a b", "x b", &
        "r+t", "w+t", "a+t", "x+t", &
        "r+t", "w+t", "a+t", "x+t", &
        "r+b", "w+b", "a+b", "x+b"  &
    ]

contains

    !> Collect all exported unit tests
    subroutine collect_parse_mode(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("parse_mode_expected_order", test_parse_mode_expected_order), &
            new_unittest("parse_mode_reverse_order", test_parse_mode_reverse_order), &
            new_unittest("parse_mode_random_order", test_parse_mode_random_order) &
            !FIXME Is it possible to run tests with error stop?
            !new_unittest("parse_mode_always_fail", test_parse_mode_always_fail) &
        ]

    end subroutine collect_parse_mode


    subroutine test_parse_mode_expected_order(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer :: n

        do n = 1, size(parse_modes_input)
            call check(error, parse_mode(trim(parse_modes_input(n))) == &
                              parse_modes_expected(n))
            if (allocated(error)) return
        end do

    end subroutine test_parse_mode_expected_order


    subroutine test_parse_mode_reverse_order(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: n

        do n = 1, size(parse_modes_input)
            call check(error, &
                       parse_mode(trim(reverse(parse_modes_input(n)))) == &
                       parse_modes_expected(n))
            if (allocated(error)) return
        end do

    end subroutine test_parse_mode_reverse_order


    subroutine test_parse_mode_random_order(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, parse_mode("t r") == "r t")
        if (allocated(error)) return

        call check(error, parse_mode(" tw ") == "w t")
        if (allocated(error)) return

        call check(error, parse_mode("ta  ") == "a t")
        if (allocated(error)) return

        call check(error, parse_mode("  t   x   ") == "x t")
        if (allocated(error)) return

        call check(error, parse_mode("+ r ") == "r+t")
        if (allocated(error)) return

        call check(error, parse_mode("w   +") == "w+t")
        if (allocated(error)) return

        call check(error, parse_mode(" a+") == "a+t")
        if (allocated(error)) return

        call check(error, parse_mode(" x+   t  ") == "x+t")
        if (allocated(error)) return

        call check(error, parse_mode("tr+ ") == "r+t")
        if (allocated(error)) return

        call check(error, parse_mode("wt + ") == "w+t")
        if (allocated(error)) return

        call check(error, parse_mode("a + t") == "a+t")
        if (allocated(error)) return

        call check(error, parse_mode(" xt + ") == "x+t")
        if (allocated(error)) return

        call check(error, parse_mode(" + t") == "r+t")
        if (allocated(error)) return

        call check(error, parse_mode(" +w  b") == "w+b")
        if (allocated(error)) return

        call check(error, parse_mode("a + b") == "a+b")
        if (allocated(error)) return

        call check(error, parse_mode(" b + x  ") == "x+b")
        if (allocated(error)) return

    end subroutine test_parse_mode_random_order


    subroutine test_parse_mode_always_fail(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, parse_mode("r+w") /= "r t")
        if (allocated(error)) return
        call check(error, parse_mode("tt") /= "r t")
        if (allocated(error)) return
        call check(error, parse_mode("bt") /= "r t")
        if (allocated(error)) return

    end subroutine test_parse_mode_always_fail

end module test_parse_mode


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_parse_mode, only : collect_parse_mode
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("parse_mode", collect_parse_mode) &
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
