module test_logspace
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds, only: sp, dp, int8, int16, int32, int64
    use stdlib_math, only: logspace, DEFAULT_LOGSPACE_BASE, DEFAULT_LOGSPACE_LENGTH

    implicit none

    ! Testing logspace
    !
    ! logspace should return a rank 1 array of values equally logarithmically spaced
    ! from the base**start to base**end, using 10 as the base. If no length
    ! is specified, return a rank 1 array with 50 elements.
    !
    ! Also test to verify that the proportion between adjacent elements is constant within
    ! a certain tolerance

    real(sp), parameter :: TOLERANCE_SP = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: TOLERANCE_DP = 1000 * epsilon(1.0_dp) ! Percentage of the range for which the actual gap must not exceed


contains

    !> Collect all exported unit tests
    subroutine collect_logspace(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("logspace_sp", test_logspace_sp), &
            new_unittest("logspace_dp", test_logspace_dp), &
            new_unittest("logspace_default", test_logspace_default), &
            new_unittest("logspace_base_2", test_logspace_base_2), &
            new_unittest("logspace_base_2_cmplx_start", test_logspace_base_2_cmplx_start), &
            new_unittest("logspace_base_i_int_start", test_logspace_base_i_int_start) &
            ]

    end subroutine collect_logspace

    subroutine test_logspace_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 20
        real(sp), parameter :: start = 0.0_sp
        real(sp), parameter :: end = 2.0_sp

        real(sp) :: expected_proportion
        integer :: i

        real(sp), allocatable :: x(:)

        x = logspace(start, end, n)

        expected_proportion = 10 ** ( ( end - start ) / ( n - 1 ) )

        call check(error, size(x), n, "Array not allocated to appropriate size")
        if (allocated(error)) return
        call check(error, x(1), DEFAULT_LOGSPACE_BASE ** start, "Initial value of array is not equal to 10^start")
        if (allocated(error)) return
        call check(error, x(n), DEFAULT_LOGSPACE_BASE ** end, "Final value of array is not equal to 10^end")
        if (allocated(error)) return

        do i = 1, n-1

            call check(error, x(i + 1) / x(i), expected_proportion, &
                & thr=abs(expected_proportion) * TOLERANCE_SP)
            if (allocated(error)) return

        end do


    end subroutine

    subroutine test_logspace_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 10
        real(dp), parameter :: start = 1.0_dp
        real(dp), parameter :: end = 0.0_dp
        real(dp) :: expected_proportion
        integer :: i

        real(dp), allocatable :: x(:)

        x = logspace(start, end, n)

        expected_proportion = 10 ** ( ( end - start ) / ( n - 1 ) )


        call check(error, size(x), n, "Array not allocated to appropriate size")
        if (allocated(error)) return
        call check(error, x(1), DEFAULT_LOGSPACE_BASE ** start, "Initial value of array is not equal to 10^start")
        if (allocated(error)) return
        call check(error, x(n), DEFAULT_LOGSPACE_BASE ** end, "Final value of array is not equal to 10^end")
        if (allocated(error)) return

        do i = 1, n-1

            call check(error, x(i + 1) / x(i), expected_proportion, &
                & thr=abs(expected_proportion) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_logspace_default(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), parameter :: start = 0.0_dp
        real(dp), parameter :: end = 1.0_dp
        integer, parameter :: n = DEFAULT_LOGSPACE_LENGTH
        real(dp) :: expected_proportion
        integer :: i

        real(dp), allocatable :: x(:)

        x = logspace(start, end)

        expected_proportion = 10 ** ( ( end - start ) / ( n - 1 ) )


        call check(error, size(x), n, "Array not allocated to appropriate size")
        if (allocated(error)) return
        call check(error, x(1), DEFAULT_LOGSPACE_BASE ** start, "Initial value of array is not equal to 10^start")
        if (allocated(error)) return
        call check(error, x(n), DEFAULT_LOGSPACE_BASE ** end, "Final value of array is not equal to 10^end")
        if (allocated(error)) return

        do i = 1, n-1

            call check(error, x(i + 1) / x(i), expected_proportion, &
                & thr=abs(expected_proportion) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_logspace_base_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 10
        real(dp), parameter :: start = 1.0_dp
        real(dp), parameter :: end = 10.0_dp
        integer, parameter :: base = 2
        integer :: i
        real(dp) :: expected_proportion

        real(dp), allocatable :: x(:)

        x = logspace(start, end, n, base)

        expected_proportion = 2 ** ( ( end - start ) / ( n - 1 ) )

        call check(error, size(x), n, "Array not allocated to appropriate size")
        if (allocated(error)) return
        call check(error, x(1), base ** start, "Initial value of array is not equal to 2^start")
        if (allocated(error)) return
        call check(error, x(n), base ** end, "Final value of array is not equal to 2^end")
        if (allocated(error)) return

        do i = 1, n-1

            call check(error, x(i + 1) / x(i), expected_proportion, &
                & thr=abs(expected_proportion) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_logspace_base_2_cmplx_start(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 10
        complex(dp), parameter :: start = (1, 0)
        complex(dp), parameter :: end = (0, 1)
        integer, parameter :: base = 2
        complex(dp) :: expected_proportion
        integer :: i

        complex(dp), allocatable :: x(:)

        x = logspace(start, end, n, base)

        expected_proportion = 2 ** ( ( end - start ) / ( n - 1 ) )


        call check(error, size(x), n, "Array not allocated to appropriate size")
        if (allocated(error)) return
        call check(error, x(1), base ** start, "Initial value of array is not equal to 2^start")
        if (allocated(error)) return
        call check(error, x(n), base ** end, "Final value of array is not equal to 2^end")
        if (allocated(error)) return

        do i = 1, n-1

            call check(error, x(i + 1) / x(i), expected_proportion, &
                & thr=abs(expected_proportion) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_logspace_base_i_int_start(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 5
        integer, parameter :: start = 1
        integer, parameter :: end = 5
        complex(dp), parameter :: base = (0, 1) ! i
        complex(dp) :: expected_proportion
        integer :: i

        complex(dp), allocatable :: x(:)

        x = logspace(start, end, n, base)

        expected_proportion = base ** ( ( end - start ) / ( n - 1 ) )

        call check(error, size(x), n, "Array not allocated to appropriate size")
        if (allocated(error)) return
        call check(error, x(1), base ** start, "Initial value of array is not equal to 2^start")
        if (allocated(error)) return
        call check(error, x(n), base ** end, "Final value of array is not equal to 2^end")
        if (allocated(error)) return

        do i = 1, n-1

            call check(error, x(i + 1) / x(i), expected_proportion, &
                & thr=abs(expected_proportion) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine


end module

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_logspace, only : collect_logspace
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("logspace", collect_logspace) &
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
