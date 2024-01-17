
module test_trapz
    use stdlib_kinds, only: sp, dp, xdp, qp
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_quadrature, only: trapz, trapz_weights

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_trapz(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("trapz_sp", test_trapz_sp), &
            new_unittest("trapz_dp", test_trapz_dp), &
            new_unittest("trapz_qp", test_trapz_qp), &
            new_unittest("trapz_weights_sp", test_trapz_weights_sp), &
            new_unittest("trapz_weights_dp", test_trapz_weights_dp), &
            new_unittest("trapz_weights_qp", test_trapz_weights_qp), &
            new_unittest("trapz_zero_sp", test_trapz_zero_sp), &
            new_unittest("trapz_zero_dp", test_trapz_zero_dp), &
            new_unittest("trapz_zero_qp", test_trapz_zero_qp) &
            ]
    end subroutine collect_trapz

    subroutine test_trapz_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 17
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp) :: val
        real(sp) :: ans
        integer :: i

        y = [(real(i-1, sp), i = 1, n)]

        val = trapz(y, 1.0_sp)
        ans = 128.0_sp
        call check(error, abs(val - ans) < epsilon(ans))

        val = trapz(y, 0.5_sp)
        ans = 64.0_sp
        call check(error, abs(val - ans) < epsilon(ans))

        x = [((i-1)*4.0_sp/real(n-1, sp), i = 1, n)]
        val = trapz(y, x)
        ans = 32.0_sp
        call check(error, abs(val - ans) < epsilon(ans))

        x = y**2
        val = trapz(y, x)
        ans = 2728.0_sp
        call check(error, abs(val - ans) < epsilon(ans))
    end subroutine test_trapz_sp

    subroutine test_trapz_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 17
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp) :: val
        real(dp) :: ans
        integer :: i

        y = [(real(i-1, dp), i = 1, n)]

        val = trapz(y, 1.0_dp)
        ans = 128.0_dp
        call check(error, abs(val - ans) < epsilon(ans))

        val = trapz(y, 0.5_dp)
        ans = 64.0_dp
        call check(error, abs(val - ans) < epsilon(ans))

        x = [((i-1)*4.0_dp/real(n-1, dp), i = 1, n)]
        val = trapz(y, x)
        ans = 32.0_dp
        call check(error, abs(val - ans) < epsilon(ans))

        x = y**2
        val = trapz(y, x)
        ans = 2728.0_dp
        call check(error, abs(val - ans) < epsilon(ans))
    end subroutine test_trapz_dp


    subroutine test_trapz_qp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_trapz_qp


    subroutine test_trapz_weights_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 17
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp), dimension(n) :: w
        integer :: i
        real(sp) :: val
        real(sp) :: ans

        y = [(real(i-1, sp), i = 1, n)]

        x = y
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(error, abs(val - ans) < epsilon(ans))

        x = y**2
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(error, abs(val - ans) < epsilon(ans))

    end subroutine test_trapz_weights_sp


    subroutine test_trapz_weights_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer, parameter :: n = 17
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp), dimension(n) :: w
        integer :: i
        real(dp) :: val
        real(dp) :: ans

        y = [(real(i-1, dp), i = 1, n)]

        x = y
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(error, abs(val - ans) < epsilon(ans))

        x = y**2
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(error, abs(val - ans) < epsilon(ans))

    end subroutine test_trapz_weights_dp


    subroutine test_trapz_weights_qp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_trapz_weights_qp


    subroutine test_trapz_zero_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), dimension(0) :: a

        call check(error, abs(trapz(a, 1.0_sp)) < epsilon(0.0_sp))
        call check(error, abs(trapz([1.0_sp], 1.0_sp)) < epsilon(0.0_sp))
        call check(error, abs(trapz(a, a)) < epsilon(0.0_sp))
        call check(error, abs(trapz([1.0_sp], [1.0_sp])) < epsilon(0.0_sp))
    end subroutine test_trapz_zero_sp


    subroutine test_trapz_zero_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), dimension(0) :: a

        call check(error, abs(trapz(a, 1.0_dp)) < epsilon(0.0_dp))
        call check(error, abs(trapz([1.0_dp], 1.0_dp)) < epsilon(0.0_dp))
        call check(error, abs(trapz(a, a)) < epsilon(0.0_dp))
        call check(error, abs(trapz([1.0_dp], [1.0_dp])) < epsilon(0.0_dp))
    end subroutine test_trapz_zero_dp


    subroutine test_trapz_zero_qp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_trapz_zero_qp

end module test_trapz


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_trapz, only : collect_trapz
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("trapz", collect_trapz) &
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
