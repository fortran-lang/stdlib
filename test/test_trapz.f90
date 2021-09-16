program test_trapz
    use stdlib_kinds, only: sp, dp, qp
    use stdlib_error, only: check
    use stdlib_quadrature, only: trapz, trapz_weights

    implicit none

    call test_trapz_sp
    call test_trapz_dp
    call test_trapz_qp

    call test_trapz_weights_sp
    call test_trapz_weights_dp
    call test_trapz_weights_qp

    call test_trapz_zero_sp
    call test_trapz_zero_dp
    call test_trapz_zero_qp

contains

    subroutine test_trapz_sp
        integer, parameter :: n = 17
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp) :: val
        real(sp) :: ans
        integer :: i

        print *, "test_trapz_sp"

        y = [(real(i-1, sp), i = 1, n)]

        val = trapz(y, 1.0_sp)
        ans = 128.0_sp
        call check(abs(val - ans) < epsilon(ans))

        val = trapz(y, 0.5_sp)
        ans = 64.0_sp
        call check(abs(val - ans) < epsilon(ans))

        x = [((i-1)*4.0_sp/real(n-1, sp), i = 1, n)]
        val = trapz(y, x)
        ans = 32.0_sp
        call check(abs(val - ans) < epsilon(ans))

        x = y**2
        val = trapz(y, x)
        ans = 2728.0_sp
        call check(abs(val - ans) < epsilon(ans))
    end subroutine test_trapz_sp

    subroutine test_trapz_dp
        integer, parameter :: n = 17
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp) :: val
        real(dp) :: ans
        integer :: i

        print *, "test_trapz_dp"

        y = [(real(i-1, dp), i = 1, n)]

        val = trapz(y, 1.0_dp)
        ans = 128.0_dp
        call check(abs(val - ans) < epsilon(ans))

        val = trapz(y, 0.5_dp)
        ans = 64.0_dp
        call check(abs(val - ans) < epsilon(ans))

        x = [((i-1)*4.0_dp/real(n-1, dp), i = 1, n)]
        val = trapz(y, x)
        ans = 32.0_dp
        call check(abs(val - ans) < epsilon(ans))

        x = y**2
        val = trapz(y, x)
        ans = 2728.0_dp
        call check(abs(val - ans) < epsilon(ans))
    end subroutine test_trapz_dp


    subroutine test_trapz_qp
        integer, parameter :: n = 17
        real(qp), dimension(n) :: y
        real(qp), dimension(n) :: x
        real(qp) :: val
        real(qp) :: ans
        integer :: i

        print *, "test_trapz_qp"

        y = [(real(i-1, qp), i = 1, n)]

        val = trapz(y, 1.0_qp)
        ans = 128.0_qp
        call check(abs(val - ans) < epsilon(ans))

        val = trapz(y, 0.5_qp)
        ans = 64.0_qp
        call check(abs(val - ans) < epsilon(ans))

        x = [((i-1)*4.0_qp/real(n-1, qp), i = 1, n)]
        val = trapz(y, x)
        ans = 32.0_qp
        call check(abs(val - ans) < epsilon(ans))

        x = y**2
        val = trapz(y, x)
        ans = 2728.0_qp
        call check(abs(val - ans) < epsilon(ans))
    end subroutine test_trapz_qp


    subroutine test_trapz_weights_sp
        integer, parameter :: n = 17
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp), dimension(n) :: w
        integer :: i
        real(sp) :: val
        real(sp) :: ans

        print *, "test_trapz_weights_sp"

        y = [(real(i-1, sp), i = 1, n)]

        x = y
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(abs(val - ans) < epsilon(ans))

        x = y**2
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(abs(val - ans) < epsilon(ans))

    end subroutine test_trapz_weights_sp


    subroutine test_trapz_weights_dp
        integer, parameter :: n = 17
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp), dimension(n) :: w
        integer :: i
        real(dp) :: val
        real(dp) :: ans

        print *, "test_trapz_weights_dp"

        y = [(real(i-1, dp), i = 1, n)]

        x = y
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(abs(val - ans) < epsilon(ans))

        x = y**2
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(abs(val - ans) < epsilon(ans))

    end subroutine test_trapz_weights_dp


    subroutine test_trapz_weights_qp
        integer, parameter :: n = 17
        real(qp), dimension(n) :: y
        real(qp), dimension(n) :: x
        real(qp), dimension(n) :: w
        integer :: i
        real(qp) :: val
        real(qp) :: ans

        print *, "test_trapz_weights_qp"

        y = [(real(i-1, qp), i = 1, n)]

        x = y
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(abs(val - ans) < epsilon(ans))

        x = y**2
        w = trapz_weights(x)
        val = dot_product(w, y)
        ans = trapz(y, x)
        call check(abs(val - ans) < epsilon(ans))

    end subroutine test_trapz_weights_qp


    subroutine test_trapz_zero_sp
        real(sp), dimension(0) :: a

        print *, "test_trapz_zero_sp"

        call check(abs(trapz(a, 1.0_sp)) < epsilon(0.0_sp))
        call check(abs(trapz([1.0_sp], 1.0_sp)) < epsilon(0.0_sp))
        call check(abs(trapz(a, a)) < epsilon(0.0_sp))
        call check(abs(trapz([1.0_sp], [1.0_sp])) < epsilon(0.0_sp))
    end subroutine test_trapz_zero_sp


    subroutine test_trapz_zero_dp
        real(dp), dimension(0) :: a

        print *, "test_trapz_zero_dp"

        call check(abs(trapz(a, 1.0_dp)) < epsilon(0.0_dp))
        call check(abs(trapz([1.0_dp], 1.0_dp)) < epsilon(0.0_dp))
        call check(abs(trapz(a, a)) < epsilon(0.0_dp))
        call check(abs(trapz([1.0_dp], [1.0_dp])) < epsilon(0.0_dp))
    end subroutine test_trapz_zero_dp


    subroutine test_trapz_zero_qp
        real(qp), dimension(0) :: a

        print *, "test_trapz_zero_qp"

        call check(abs(trapz(a, 1.0_qp)) < epsilon(0.0_qp))
        call check(abs(trapz([1.0_qp], 1.0_qp)) < epsilon(0.0_qp))
        call check(abs(trapz(a, a)) < epsilon(0.0_qp))
        call check(abs(trapz([1.0_qp], [1.0_qp])) < epsilon(0.0_qp))
    end subroutine test_trapz_zero_qp

end program test_trapz
