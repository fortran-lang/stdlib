program test_logspace

    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp, int8, int16, int32, int64
    use stdlib_math, only: logspace, DEFAULT_LOGSPACE_BASE, DEFAULT_LOGSPACE_LENGTH

    implicit none

        logical :: warn = .false.

        integer :: iunit

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

        open(newunit=iunit, file="test_logspace_log.txt", status="unknown") ! Log the results of the function

        write(iunit,*) "Writing to unit #: ", iunit

        call test_logspace_sp
        call test_logspace_dp
        call test_logspace_default
        call test_logspace_base_2
        call test_logspace_base_2_cmplx_start
        call test_logspace_base_i_int_start

        close(unit=iunit)

    contains

        subroutine test_logspace_sp

            integer :: n = 20
            real(sp) :: start = 0.0_sp
            real(sp) :: end = 2.0_sp

            real(sp) :: expected_proportion
            integer :: i = 1

            real(sp), allocatable :: x(:)

            x = logspace(start, end, n)

            expected_proportion = 10 ** ( ( end - start ) / ( n - 1 ) )

            call check(x(1) == DEFAULT_LOGSPACE_BASE ** start, msg="Initial value of array is not equal to 10^start", warn=warn)
            call check(x(n) == DEFAULT_LOGSPACE_BASE ** end, msg="Final value of array is not equal to 10^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

            do i = 1, n-1

                call check(abs(x(i + 1) / x(i) - expected_proportion) < abs(expected_proportion) * TOLERANCE_SP)

            end do


            write(unit=iunit, fmt=*) "logspace(0.0_sp, 2.0_sp, 20): "
            write(unit=iunit,fmt='(70("="))')
            write(unit=iunit,fmt="(20(F7.3, 2X))") x
            write(iunit,*)
            write(iunit,*)

        end subroutine

        subroutine test_logspace_dp

            integer :: n = 10
            real(dp) :: start = 1.0_dp
            real(dp) :: end = 0.0_dp
            real(dp) :: expected_proportion
            integer :: i = 1

            real(dp), allocatable :: x(:)

            x = logspace(start, end, n)

            expected_proportion = 10 ** ( ( end - start ) / ( n - 1 ) )


            call check(x(1) == DEFAULT_LOGSPACE_BASE ** start, msg="Initial value of array is not equal to 10^start", warn=warn)
            call check(x(n) == DEFAULT_LOGSPACE_BASE ** end, msg="Final value of array is not equal to 10^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

            do i = 1, n-1

                call check(abs(x(i + 1) / x(i) - expected_proportion) < abs(expected_proportion) * TOLERANCE_DP)

            end do

            write(unit=iunit, fmt=*) "logspace(1.0_dp, 0.0_dp, 10): "
            write(unit=iunit,fmt=99)
            write(unit=iunit,fmt="(10(F7.3, 2X))") x
            write(iunit,*)
            write(iunit,*)

            99 format(70("="))

        end subroutine

        subroutine test_logspace_default

            real(dp) :: start = 0.0_dp
            real(dp) :: end = 1.0_dp
            integer :: n = DEFAULT_LOGSPACE_LENGTH
            real(dp) :: expected_proportion
            integer :: i

            real(dp), allocatable :: x(:)

            x = logspace(start, end)

            expected_proportion = 10 ** ( ( end - start ) / ( n - 1 ) )


            call check(x(1) == DEFAULT_LOGSPACE_BASE ** start, msg="Initial value of array is not equal to 10^start", warn=warn)
            call check(x(n) == DEFAULT_LOGSPACE_BASE ** end, msg="Final value of array is not equal to 10^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

            do i = 1, n-1

                call check(abs(x(i + 1) / x(i) - expected_proportion) < abs(expected_proportion) * TOLERANCE_DP)

            end do

            write(unit=iunit, fmt=*) "logspace(0.0_dp, 1.0_dp): "
            write(unit=iunit,fmt='(70("="))')
            write(unit=iunit,fmt="(50(F7.3, 2X))") x
            write(iunit,*)
            write(iunit,*)

        end subroutine

        subroutine test_logspace_base_2

            integer :: n = 10
            real(dp) :: start = 1.0_dp
            real(dp) :: end = 10.0_dp
            integer :: base = 2
            integer :: i
            real(dp) :: expected_proportion

            real(dp), allocatable :: x(:)

            x = logspace(start, end, n, base)

            expected_proportion = 2 ** ( ( end - start ) / ( n - 1 ) )

            call check(x(1) == base ** start, msg="Initial value of array is not equal to 2^start", warn=warn)
            call check(x(n) == base ** end, msg="Final value of array is not equal to 2^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

            do i = 1, n-1

                call check(abs(x(i + 1) / x(i) - expected_proportion) < abs(expected_proportion) * TOLERANCE_DP)

            end do

            write(unit=iunit, fmt=*) "logspace(1.0_dp, 10.0_dp, 10, 2): "
            write(unit=iunit,fmt='(70("="))')
            write(unit=iunit,fmt="(10(F9.3, 2X))") x
            write(iunit,*)
            write(iunit,*)

        end subroutine

        subroutine test_logspace_base_2_cmplx_start

            integer :: n = 10
            complex(dp) :: start = (1, 0)
            complex(dp) :: end = (0, 1)
            integer :: base = 2
            complex(dp) :: expected_proportion
            integer :: i

            complex(dp), allocatable :: x(:)

            x = logspace(start, end, n, base)

            expected_proportion = 2 ** ( ( end - start ) / ( n - 1 ) )


            call check(x(1) == base ** start, msg="Initial value of array is not equal to 2^start", warn=warn)
            call check(x(n) == base ** end, msg="Final value of array is not equal to 2^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

            do i = 1, n-1

                call check(abs(x(i + 1) / x(i) - expected_proportion) < abs(expected_proportion) * TOLERANCE_DP)

            end do

            write(unit=iunit, fmt=*) "logspace(1, i, 10, 2): "
            write(unit=iunit,fmt='(70("="))')
            write(unit=iunit,fmt="(10('(', F6.3, ',', 1X, F6.3, ')', 2X))") x
            write(iunit,*)
            write(iunit,*)

        end subroutine

        subroutine test_logspace_base_i_int_start

            integer :: n = 5
            integer :: start = 1
            integer :: end = 5
            complex(dp) :: base = (0, 1) ! i
            complex(dp) :: expected_proportion
            integer :: i = 1

            complex(dp), allocatable :: x(:)

            x = logspace(start, end, n, base)

            expected_proportion = base ** ( ( end - start ) / ( n - 1 ) )

            call check(x(1) == base ** start, msg="Initial value of array is not equal to 2^start", warn=warn)
            call check(x(n) == base ** end, msg="Final value of array is not equal to 2^end", warn=warn)
            call check(size(x) == n, msg="Array not allocated to appropriate size", warn=warn)

            do i = 1, n-1

                call check(abs(x(i + 1) / x(i) - expected_proportion) < abs(expected_proportion) * TOLERANCE_DP)

            end do

            write(unit=iunit, fmt=*) "logspace(1, 5, 5, i): "
            write(unit=iunit,fmt='(70("="))')
            write(unit=iunit,fmt="(10('(', F6.3, ',', 1X, F6.3, ')', 2X))") x
            write(iunit,*)
            write(iunit,*)

        end subroutine


    end program