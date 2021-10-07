program test_linspace
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp, int8, int16
    use stdlib_math, only: linspace, DEFAULT_LINSPACE_LENGTH

    implicit none

    integer :: iunit
    logical :: warn = .false.
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

    open(newunit=iunit, file="test_linspace_log.txt", status="unknown") ! Log the results of the functions

    write(iunit,*) "Writing to unit #: ", iunit

    call test_linspace_sp
    call test_linspace_dp
    call test_linspace_neg_index ! Make sure that when passed a negative index the result is an empty array
    call test_linspace_cmplx
    call test_linspace_cmplx_2
    call test_linspace_cmplx_3
    call test_linspace_cmplx_sp
    call test_linspace_cmplx_sp_2
    call test_linspace_int16
    call test_linspace_int8

    close(unit=iunit)

contains

    subroutine test_linspace_sp

        integer, parameter :: n = 20
        real(sp) :: start = 1.0_sp
        real(sp) :: end = 10.0_sp
        real(sp) :: expected_interval
        real(sp) :: true_difference

        integer :: i
        real(sp), allocatable :: x(:)

        x = linspace(start, end, n)

        expected_interval =( end - start ) / real(( n - 1 ), sp)

        call check(x(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(x(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

        print *, "Made it through first round of tests"

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            true_difference = x(i + 1) - x(i)
            call check(abs(true_difference - expected_interval) < abs(expected_interval) * TOLERANCE_SP)

        end do

    end subroutine

    subroutine test_linspace_dp

        real(dp) :: start = 1.0_dp
        real(dp) :: end = 10.0_dp
        integer, parameter :: n = DEFAULT_LINSPACE_LENGTH
        real(dp) :: expected_interval
        real(dp) :: true_difference

        real(dp), allocatable :: x(:)
        integer :: i

        x = linspace(start, end)

        expected_interval =( end - start ) / ( n - 1 )

        call check(x(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(x(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(x) == n, msg="Array not allocated to default size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            true_difference = x(i + 1) - x(i)
            call check(abs(true_difference - expected_interval) < abs(expected_interval) * TOLERANCE_DP)

        end do

    end subroutine

    subroutine test_linspace_neg_index

        real(dp) :: start = 1.0_dp
        real(dp) :: end = 10.0_dp

        real(dp), allocatable :: x(:)

        x = linspace(start, end, -15)

        call check(size(x) == 0, msg="Allocated array is not empty", warn=warn)

    end subroutine

    subroutine test_linspace_cmplx

        complex(dp) :: start = (0.0_dp, 10.0_dp)
        complex(dp) :: end = (1.0_dp, 0.0_dp)
        complex(dp) :: expected_interval
        integer, parameter :: n = 10

        complex(dp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(z(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(z(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(z) == n, msg="Array not allocated to correct size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(abs( ( z(i + 1) - z(i) ) - expected_interval) < abs(expected_interval) * TOLERANCE_DP)

        end do

        write(unit=iunit, fmt=*) "linspace((0.0_dp, 10.0_dp), (1.0_dp, 0.0_dp), 10): "
        write(unit=iunit,fmt='(70("="))')
        do i = 1, n
            write(unit=iunit,fmt=*) z(i)
        end do
        write(iunit,*)
        write(iunit,*)

    end subroutine

    subroutine test_linspace_cmplx_2

        complex(dp) :: start = (10.0_dp, 10.0_dp)
        complex(dp) :: end = (1.0_dp, 1.0_dp)
        complex(dp) :: expected_interval

        integer, parameter :: n = 5

        complex(dp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(z(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(z(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(z) == n, msg="Array not allocated to correct size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(abs( ( z(i + 1) - z(i) ) - expected_interval) < abs(expected_interval) * TOLERANCE_DP)

        end do

        write(unit=iunit, fmt=*) "linspace((10.0_dp, 10.0_dp), (1.0_dp, 1.0_dp), 5): "
        write(unit=iunit,fmt='(70("="))')
        do i = 1, n
            write(unit=iunit,fmt=*) z(i)
        end do
        write(iunit,*)
        write(iunit,*)

    end subroutine

    subroutine test_linspace_cmplx_3

        complex(dp) :: start = (-5.0_dp, 100.0_dp)
        complex(dp) :: end = (20.0_dp, 13.0_dp)
        complex(dp) :: expected_interval

        integer, parameter :: n = 20

        complex(dp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval = ( end - start ) / ( n - 1 )

        call check(z(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(z(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(z) == n, msg="Array not allocated to correct size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(abs( ( z(i + 1) - z(i) ) - expected_interval) < abs(expected_interval) * TOLERANCE_DP)

        end do

        write(unit=iunit, fmt=*) "linspace((-5.0_dp, 100.0_dp), (20.0_dp, 13.0_dp), 20): "
        write(unit=iunit,fmt='(70("="))')
        do i = 1, n
            write(unit=iunit,fmt=*) z(i)
        end do
        write(iunit,*)
        write(iunit,*)

    end subroutine

    subroutine test_linspace_cmplx_sp

        complex(sp) :: start = (0.5_sp, 5.0_sp)
        complex(sp) :: end = (1.0_sp, -30.0_sp)
        complex(sp) :: expected_interval

        integer, parameter :: n = 10

        complex(sp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(z(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(z(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(z) == n, msg="Array not allocated to correct size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(abs( ( z(i + 1) - z(i) ) - expected_interval) < abs(expected_interval) * TOLERANCE_SP)

        end do

        write(unit=iunit, fmt=*) "linspace((0.5_sp, 5.0_sp), (1.0_sp, -30.0_sp), 10): "
        write(unit=iunit,fmt='(70("="))')
        do i = 1, n
            write(unit=iunit,fmt=*) z(i)
        end do
        write(iunit,*)
        write(iunit,*)

    end subroutine

    subroutine test_linspace_cmplx_sp_2

        complex(sp) :: start = (50.0_sp, 500.0_sp)
        complex(sp) :: end = (-100.0_sp, 2000.0_sp)
        complex(sp) :: expected_interval
        complex(sp) :: true_interval
        real(sp) :: offset

        integer, parameter :: n = DEFAULT_LINSPACE_LENGTH

        complex(sp), allocatable :: z(:)

        integer :: i

        z = linspace(start, end)

        expected_interval =( end - start ) / ( n - 1 )

        call check(z(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(z(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(z) == n, msg="Array not allocated to default size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            true_interval = (z(i + 1) - z(i))
            offset = abs(true_interval - expected_interval)
            call check(abs( ( z(i + 1) - z(i) ) - expected_interval) < abs(expected_interval) * TOLERANCE_SP)
            ! print *, i

        end do

        write(unit=iunit, fmt=*) "linspace((50.0_sp, 500.0_sp), (-100.0_sp, 2000.0_sp)): "
        write(unit=iunit,fmt='(70("="))')
        do i = 1, n
            write(unit=iunit,fmt=*) z(i)
        end do
        write(iunit,*)
        write(iunit,*)

    end subroutine

    subroutine test_linspace_int16

        integer(int16) :: start = 5
        integer(int16) :: end = 10
        real(dp) :: expected_interval

        integer, parameter :: n = 6

        integer(int16), allocatable :: z(:)

        integer :: i

        z = linspace(start, end, n)

        expected_interval =( end - start ) / ( n - 1 )

        call check(z(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(z(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(z) == n, msg="Array not allocated to correct size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(abs( ( z(i + 1) - z(i) ) - expected_interval) < abs(expected_interval) * TOLERANCE_DP)

        end do

        write(unit=iunit, fmt=*) "linspace(5_int16, 10_int16, 10): "
        write(unit=iunit,fmt='(70("="))')
        do i = 1, n
            write(unit=iunit,fmt=*) z(i)
        end do
        write(iunit,*)
        write(iunit,*)

    end subroutine

    subroutine test_linspace_int8

        integer(int8) :: start = 20
        integer(int8) :: end = 50

        real(dp) :: expected_interval

        integer, parameter :: n = 10

        real(dp), allocatable :: z(:)
        integer(int8) :: z_int(n)

        integer :: i

        z = linspace(start, end, n)
        z_int = linspace(start, end, n)

        expected_interval =real( end - start, dp ) / ( n - 1 )

        call check(z(1) == start, msg="Initial value of array is not equal to the passed start parameter", warn=warn)
        call check(z(n) == end, msg="Final array value is not equal to end parameter", warn=warn)
        call check(size(z) == n, msg="Array not allocated to correct size", warn=warn)

        ! Due to roundoff error, it is possible that the jump from x(n-1) to x(n) is slightly different than the expected interval
        do i = 1, n-1

            call check(abs( ( z(i + 1) - z(i) ) - expected_interval) < abs(expected_interval) * TOLERANCE_DP)

        end do

        write(unit=iunit, fmt=*) "linspace(5_int16, 10_int16, 10): "
        write(unit=iunit,fmt='(70("="))')
        do i = 1, n
            write(unit=iunit,fmt=*) z(i)
        end do
        write(iunit,*)
        write(iunit,*)

    end subroutine



end program
