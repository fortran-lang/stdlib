module test_linspace
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds, only: sp, dp, int8, int16
    use stdlib_math, only: linspace, DEFAULT_LINSPACE_LENGTH

    implicit none
    private

    public :: collect_linspace

    real(sp), parameter :: TOLERANCE_SP = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: TOLERANCE_DP = 1000 * epsilon(1.0_dp) ! Percentage of the range for which the actual gap must not exceed

    ! Testing linspace.
    !
    ! For single and double precision, check if the beginning and end values are properly recorded
    ! and make sure that the size of the result array is as expected.
    !
    ! This testing suite makes use of the a repeated section of code that will check to make
    ! sure that every element is linearly spaced (i.e., call check(|array(i+1) - array(i)| < |expected_value| * TOLERANCE)).
    ! I would convert this repeated code into a subroutine but that would require the implementation of a
    ! generic procedure given that each linear space will have a different expected_value type and kind.

contains

    !> Collect all exported unit tests
    subroutine collect_linspace(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("linspace_sp", test_linspace_sp), &
            new_unittest("linspace_dp", test_linspace_dp), &
            new_unittest("linspace_neg_index", test_linspace_neg_index), &
            new_unittest("linspace_cmplx", test_linspace_cmplx), &
            new_unittest("linspace_cmplx_2", test_linspace_cmplx_2), &
            new_unittest("linspace_cmplx_3", test_linspace_cmplx_3), &
            new_unittest("linspace_cmplx_sp", test_linspace_cmplx_sp), &
            new_unittest("linspace_cmplx_sp_2", test_linspace_cmplx_sp_2), &
            new_unittest("linspace_int16", test_linspace_int16), &
            new_unittest("linspace_int8", test_linspace_int8) &
            ]

    end subroutine collect_linspace


    subroutine test_linspace_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 20
        real(sp), parameter :: start = 1.0_sp
        real(sp), parameter :: end = 10.0_sp
        real(sp) :: expected_interval
        real(sp) :: true_difference

        integer :: i
        real(sp), allocatable :: x(:)

        x = linspace(start, end, n)

        expected_interval =( end - start ) / real(( n - 1 ), sp)

        call check(error, x(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, x(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return
        call check(error, size(x), n, "Array not allocated to appropriate size")
        if (allocated(error)) return

        print *, "Made it through first round of tests"

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            true_difference = x(i + 1) - x(i)
            call check(error, abs(true_difference - expected_interval) < abs(expected_interval) * TOLERANCE_SP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_linspace_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), parameter :: start = 1.0_dp
        real(dp), parameter :: end = 10.0_dp
        integer, parameter :: n = DEFAULT_LINSPACE_LENGTH
        real(dp) :: expected_interval
        real(dp) :: true_difference

        real(dp), allocatable :: x(:)
        integer :: i

        x = linspace(start, end)

        expected_interval =( end - start ) / ( n - 1 )

        call check(error, size(x), n, "Array not allocated to default size")
        if (allocated(error)) return
        call check(error, x(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, x(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            true_difference = x(i + 1) - x(i)
            call check(error, true_difference, expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_linspace_neg_index(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), parameter :: start = 1.0_dp
        real(dp), parameter :: end = 10.0_dp

        real(dp), allocatable :: x(:)

        x = linspace(start, end, -15)

        call check(error, size(x), 0, "Allocated array is not empty")

    end subroutine

    subroutine test_linspace_cmplx(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(dp), parameter :: start = (0.0_dp, 10.0_dp)
        complex(dp), parameter :: end = (1.0_dp, 0.0_dp)
        complex(dp) :: expected_interval
        integer, parameter :: n = 10

        complex(dp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(error, size(z), n, "Array not allocated to correct size")
        if (allocated(error)) return
        call check(error, z(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, z(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(error, z(i + 1) - z(i), expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_linspace_cmplx_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(dp), parameter :: start = (10.0_dp, 10.0_dp)
        complex(dp), parameter :: end = (1.0_dp, 1.0_dp)
        complex(dp) :: expected_interval

        integer, parameter :: n = 5

        complex(dp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(error, size(z), n, "Array not allocated to correct size")
        if (allocated(error)) return
        call check(error, z(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, z(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(error, z(i + 1) - z(i), expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_linspace_cmplx_3(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(dp), parameter :: start = (-5.0_dp, 100.0_dp)
        complex(dp), parameter :: end = (20.0_dp, 13.0_dp)
        complex(dp) :: expected_interval

        integer, parameter :: n = 20

        complex(dp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval = ( end - start ) / ( n - 1 )

        call check(error, size(z), n, "Array not allocated to correct size")
        if (allocated(error)) return
        call check(error, z(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, z(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(error, z(i + 1) - z(i), expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_linspace_cmplx_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(sp), parameter :: start = (0.5_sp, 5.0_sp)
        complex(sp), parameter :: end = (1.0_sp, -30.0_sp)
        complex(sp) :: expected_interval

        integer, parameter :: n = 10

        complex(sp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(error, size(z), n, "Array not allocated to correct size")
        if (allocated(error)) return
        call check(error, z(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, z(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(error, z(i + 1) - z(i), expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_SP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_linspace_cmplx_sp_2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(sp), parameter :: start = (50.0_sp, 500.0_sp)
        complex(sp), parameter :: end = (-100.0_sp, 2000.0_sp)
        complex(sp) :: expected_interval
        complex(sp) :: true_interval
        real(sp) :: offset

        integer, parameter :: n = DEFAULT_LINSPACE_LENGTH

        complex(sp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end)

        expected_interval =( end - start ) / ( n - 1 )

        call check(error, size(z), n, "Array not allocated to default size")
        if (allocated(error)) return
        call check(error, z(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, z(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            true_interval = (z(i + 1) - z(i))
            offset = abs(true_interval - expected_interval)
            call check(error, z(i + 1) - z(i), expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_SP)
            if (allocated(error)) return
            ! print *, i

        end do

    end subroutine

    subroutine test_linspace_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), parameter :: start = 5
        integer(int16), parameter :: end = 10
        real(dp) :: expected_interval

        integer, parameter :: n = 6

        integer(int16), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(error, size(z), n, "Array not allocated to correct size")
        if (allocated(error)) return
        call check(error, z(1), start, "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, z(n), end, "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(error, real(z(i + 1) - z(i), dp), expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine

    subroutine test_linspace_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), parameter :: start = 20
        integer(int8), parameter :: end = 50

        real(dp) :: expected_interval

        integer, parameter :: n = 10

        real(dp), allocatable :: z(:)
        integer(int8) :: z_int(n)

        integer :: i

        z = linspace(start, end, n)
        z_int = linspace(start, end, n)

        expected_interval =real( end - start, dp ) / ( n - 1 )

        call check(error, size(z), n, "Array not allocated to correct size")
        if (allocated(error)) return
        call check(error, z(1), real(start, dp), "Initial value of array is not equal to the passed start parameter")
        if (allocated(error)) return
        call check(error, z(n), real(end, dp), "Final array value is not equal to end parameter")
        if (allocated(error)) return

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(error, z(i + 1) - z(i), expected_interval, &
                & thr=abs(expected_interval) * TOLERANCE_DP)
            if (allocated(error)) return

        end do

    end subroutine



end module

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linspace, only : collect_linspace
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linspace", collect_linspace) &
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
