program test_simps
    use stdlib_kinds, only: sp, dp, qp
    use stdlib_error, only: check
    use stdlib_quadrature, only: simps, simps_weights

    implicit none

    real(sp), parameter :: tol_sp = 1000 * epsilon(1.0_sp)
    real(dp), parameter :: tol_dp = 1000 * epsilon(1.0_dp)
    real(qp), parameter :: tol_qp = 1000 *epsilon(1.0_sp)

    call test_simps_sp
    call test_simps_dp
    call test_simps_qp

    call test_simps_weights_sp
    call test_simps_weights_dp
    call test_simps_weights_qp

    call test_simps_zero_sp
    call test_simps_zero_dp
    call test_simps_zero_qp

    call test_simps_even_sp
    call test_simps_even_dp
    call test_simps_even_qp

    call test_simps_weights_even_sp
    call test_simps_weights_even_dp
    call test_simps_weights_even_qp

    call test_simps_six_sp
    call test_simps_six_dp
    call test_simps_six_qp

contains

    subroutine test_simps_sp
        integer, parameter :: n = 13
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp) :: val
        real(sp) :: ans
        integer :: i

        print *, "test_simps_sp"

        y = [(real(i-1, sp)**2, i = 1, n)]

        val = simps(y, 1.0_sp)
        ans = 576.0_sp
        print *, "  dx=1", val, ans
        call check(abs(val - ans) < tol_sp)

        val = simps(y, 0.5_sp)
        ans = 288.0_sp
        print *, "  dx=0.5", val, ans
        call check(abs(val - ans) < tol_sp)

        x = [(0.25_sp*(i-1), i = 1, n)]
        val = simps(y, x)
        ans = 144.0_sp
        print *, "  x=0,0.25,0.5,...", val, ans
        call check(abs(val - ans) < tol_sp)
    end subroutine test_simps_sp


    subroutine test_simps_dp
        integer, parameter :: n = 13
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp) :: val
        real(dp) :: ans
        integer :: i

        print *, "test_simps_dp"

        y = [(real(i-1, dp)**2, i = 1, n)]

        val = simps(y, 1.0_dp)
        ans = 576.0_dp
        print *, "  dx=1", val, ans
        call check(abs(val - ans) < tol_dp)

        val = simps(y, 0.5_dp)
        ans = 288.0_dp
        print *, "  dx=0.5", val, ans
        call check(abs(val - ans) < tol_dp)

        x = [(0.25_dp*(i-1), i = 1, n)]
        val = simps(y, x)
        ans = 144.0_dp
        print *, "  x=0,0.25,0.5,...", val, ans
        call check(abs(val - ans) < tol_dp)
    end subroutine test_simps_dp


    subroutine test_simps_qp
        integer, parameter :: n = 13
        real(qp), dimension(n) :: y
        real(qp), dimension(n) :: x
        real(qp) :: val
        real(qp) :: ans
        integer :: i

        print *, "test_simps_qp"

        y = [(real(i-1, qp)**2, i = 1, n)]

        val = simps(y, 1.0_qp)
        ans = 576.0_qp
        print *, "  dx=1", val, ans
        call check(abs(val - ans) < tol_qp)

        val = simps(y, 0.5_qp)
        ans = 288.0_qp
        print *, "  dx=0.5", val, ans
        call check(abs(val - ans) < tol_qp)

        x = [(0.25_qp*(i-1), i = 1, n)]
        val = simps(y, x)
        ans = 144.0_qp
        print *, "  x=0,0.25,0.5,...", val, ans
        call check(abs(val - ans) < tol_qp)
    end subroutine test_simps_qp


    subroutine test_simps_weights_sp
        integer, parameter :: n = 17
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp), dimension(n) :: w
        integer :: i
        real(sp) :: val
        real(sp) :: ans

        print *, "test_simps_weights_sp"

        y = [(real(i-1, sp), i = 1, n)]

        x = y
        w = simps_weights(x)
        val = sum(w*y)
        ans = simps(y, x)
        print *, "  ", val, ans
        call check(abs(val - ans) < tol_sp)
    end subroutine test_simps_weights_sp


    subroutine test_simps_weights_dp
        integer, parameter :: n = 17
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp), dimension(n) :: w
        integer :: i
        real(dp) :: val
        real(dp) :: ans

        print *, "test_simps_weights_dp"

        y = [(real(i-1, dp), i = 1, n)]

        x = y
        w = simps_weights(x)
        val = sum(w*y)
        ans = simps(y, x)
        print *, "  ", val, ans
        call check(abs(val - ans) < tol_dp)
    end subroutine test_simps_weights_dp


    subroutine test_simps_weights_qp
        integer, parameter :: n = 17
        real(qp), dimension(n) :: y
        real(qp), dimension(n) :: x
        real(qp), dimension(n) :: w
        integer :: i
        real(qp) :: val
        real(qp) :: ans

        print *, "test_simps_weights_qp"

        y = [(real(i-1, qp), i = 1, n)]

        x = y
        w = simps_weights(x)
        val = sum(w*y)
        ans = simps(y, x)
        print *, "  ", val, ans
        call check(abs(val - ans) < tol_qp)
    end subroutine test_simps_weights_qp


    subroutine test_simps_zero_sp
        real(sp), dimension(0) :: a

        print *, "test_simps_zero_sp"

        call check(abs(simps(a, 1.0_sp)) < epsilon(0.0_sp))
        call check(abs(simps([1.0_sp], 1.0_sp)) < epsilon(0.0_sp))
        call check(abs(simps(a, a)) < epsilon(0.0_sp))
        call check(abs(simps([1.0_sp], [1.0_sp])) < epsilon(0.0_sp))
    end subroutine test_simps_zero_sp


    subroutine test_simps_zero_dp
        real(dp), dimension(0) :: a

        print *, "test_simps_zero_dp"

        call check(abs(simps(a, 1.0_dp)) < epsilon(0.0_dp))
        call check(abs(simps([1.0_dp], 1.0_dp)) < epsilon(0.0_dp))
        call check(abs(simps(a, a)) < epsilon(0.0_dp))
        call check(abs(simps([1.0_dp], [1.0_dp])) < epsilon(0.0_dp))
    end subroutine test_simps_zero_dp


    subroutine test_simps_zero_qp
        real(qp), dimension(0) :: a

        print *, "test_simps_zero_qp"

        call check(abs(simps(a, 1.0_qp)) < epsilon(0.0_qp))
        call check(abs(simps([1.0_qp], 1.0_qp)) < epsilon(0.0_qp))
        call check(abs(simps(a, a)) < epsilon(0.0_qp))
        call check(abs(simps([1.0_qp], [1.0_qp])) < epsilon(0.0_qp))
    end subroutine test_simps_zero_qp


    subroutine test_simps_even_sp
        integer, parameter :: n = 11
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp) :: val
        real(sp) :: ans
        integer :: i
        integer :: even

        print *, "test_simps_even_sp"

        y = [(3.0_sp*real(i-1, sp)**2, i = 1, n)]

        do even = -1, 1
            print *, "even=", even

            val = simps(y, 1.0_sp)
            ans = 1000.0_sp
            print *, "  dx=1", val, ans
            call check(abs(val - ans) < tol_sp)

            val = simps(y, 0.5_sp)
            ans = 500.0_sp
            print *, "  dx=0.5", val, ans
            call check(abs(val - ans) < tol_sp)

            x = [(0.25_sp*(i-1), i = 1, n)]
            val = simps(y, x)
            ans = 250.0_sp
            print *, "  x=0,0.25,0.5,...", val, ans
            call check(abs(val - ans) < tol_sp)
        end do
    end subroutine test_simps_even_sp


    subroutine test_simps_even_dp
        integer, parameter :: n = 11
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp) :: val
        real(dp) :: ans
        integer :: i

        print *, "test_simps_even_dp"

        y = [(3.0_dp*real(i-1, dp)**2, i = 1, n)]

        val = simps(y, 1.0_dp)
        ans = 1000.0_dp
        print *, "  dx=1", val, ans
        call check(abs(val - ans) < tol_dp)

        val = simps(y, 0.5_dp)
        ans = 500.0_dp
        print *, "  dx=0.5", val, ans
        call check(abs(val - ans) < tol_dp)

        x = [(0.25_dp*(i-1), i = 1, n)]
        val = simps(y, x)
        ans = 250.0_dp
        print *, "  x=0,0.25,0.5,...", val, ans
        call check(abs(val - ans) < tol_dp)
    end subroutine test_simps_even_dp


    subroutine test_simps_even_qp
        integer, parameter :: n = 11
        real(qp), dimension(n) :: y
        real(qp), dimension(n) :: x
        real(qp) :: val
        real(qp) :: ans
        integer :: i
        integer :: even

        print *, "test_simps_even_qp"

        y = [(3.0_qp*real(i-1, qp)**2, i = 1, n)]

        do even = -1, 1
            print *, "  even=", even

            val = simps(y, 1.0_qp)
            ans = 1000.0_qp
            print *, "  dx=1", val, ans
            call check(abs(val - ans) < tol_qp)

            val = simps(y, 0.5_qp)
            ans = 500.0_qp
            print *, "  dx=0.5", val, ans
            call check(abs(val - ans) < tol_qp)

            x = [(0.25_qp*(i-1), i = 1, n)]
            val = simps(y, x)
            ans = 250.0_qp
            print *, "  x=0,0.25,0.5,...", val, ans
            call check(abs(val - ans) < tol_qp)
        end do
    end subroutine test_simps_even_qp


    subroutine test_simps_weights_even_sp
        integer, parameter :: n = 16
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp), dimension(n) :: w
        integer :: i
        real(sp) :: val
        real(sp) :: ans
        integer :: even

        print *, "test_simps_weights_even_sp"

        y = [(real(i-1, sp), i = 1, n)]
        x = y

        do even = -1, 1
            w = simps_weights(x)
            val = sum(w*y)
            ans = simps(y, x)
            print *, "  even=", even, val, ans
            call check(abs(val - ans) < tol_sp)
        end do
    end subroutine test_simps_weights_even_sp


    subroutine test_simps_weights_even_dp
        integer, parameter :: n = 16
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp), dimension(n) :: w
        integer :: i
        real(dp) :: val
        real(dp) :: ans
        integer :: even

        print *, "test_simps_weights_even_dp"

        y = [(real(i-1, dp), i = 1, n)]
        x = y

        do even = -1, 1
            w = simps_weights(x)
            val = sum(w*y)
            ans = simps(y, x)
            print *, "  even=", even, val, ans
            call check(abs(val - ans) < tol_dp)
        end do
    end subroutine test_simps_weights_even_dp


    subroutine test_simps_weights_even_qp
        integer, parameter :: n = 16
        real(qp), dimension(n) :: y
        real(qp), dimension(n) :: x
        real(qp), dimension(n) :: w
        integer :: i
        real(qp) :: val
        real(qp) :: ans
        integer :: even

        print *, "test_simps_weights_even_qp"

        y = [(real(i-1, qp), i = 1, n)]

        x = y

        do even = -1, 1
            w = simps_weights(x)
            val = sum(w*y)
            ans = simps(y, x)
            print *, "  even=", even, val, ans
            call check(abs(val - ans) < tol_qp)
        end do
    end subroutine test_simps_weights_even_qp


    subroutine test_simps_six_sp
        integer, parameter :: n = 6
        real(sp), dimension(n) :: y
        real(sp), dimension(n) :: x
        real(sp) :: val
        real(sp) :: ans
        integer :: i
        integer :: even

        print *, "test_simps_six_sp"

        y = [(3.0_sp*real(i-1, sp)**2, i = 1, n)]

        do even = -1, 1
            print *, "even=", even

            val = simps(y, 1.0_sp)
            ans = 125.0_sp
            print *, "  dx=1", val, ans
            call check(abs(val - ans) < tol_sp)

            val = simps(y, 0.5_sp)
            ans = 62.5_sp
            print *, "  dx=0.5", val, ans
            call check(abs(val - ans) < tol_sp)

            x = [(0.25_sp*(i-1), i = 1, n)]
            val = simps(y, x)
            ans = 31.25_sp
            print *, "  x=0,0.25,0.5,...", val, ans
            call check(abs(val - ans) < tol_sp)
        end do
    end subroutine test_simps_six_sp


    subroutine test_simps_six_dp
        integer, parameter :: n = 6
        real(dp), dimension(n) :: y
        real(dp), dimension(n) :: x
        real(dp) :: val
        real(dp) :: ans
        integer :: i

        print *, "test_simps_six_dp"

        y = [(3.0_dp*real(i-1, dp)**2, i = 1, n)]

        val = simps(y, 1.0_dp)
        ans = 125.0_dp
        print *, "  dx=1", val, ans
        call check(abs(val - ans) < tol_dp)

        val = simps(y, 0.5_dp)
        ans = 62.5_dp
        print *, "  dx=0.5", val, ans
        call check(abs(val - ans) < tol_dp)

        x = [(0.25_dp*(i-1), i = 1, n)]
        val = simps(y, x)
        ans = 31.25_dp
        print *, "  x=0,0.25,0.5,...", val, ans
        call check(abs(val - ans) < tol_dp)
    end subroutine test_simps_six_dp


    subroutine test_simps_six_qp
        integer, parameter :: n = 6
        real(qp), dimension(n) :: y
        real(qp), dimension(n) :: x
        real(qp) :: val
        real(qp) :: ans
        integer :: i
        integer :: even

        print *, "test_simps_six_qp"

        y = [(3.0_qp*real(i-1, qp)**2, i = 1, n)]

        do even = -1, 1
            print *, "  even=", even

            val = simps(y, 1.0_qp)
            ans = 125.0_qp
            print *, "  dx=1", val, ans
            call check(abs(val - ans) < tol_qp)

            val = simps(y, 0.5_qp)
            ans = 62.5_qp
            print *, "  dx=0.5", val, ans
            call check(abs(val - ans) < tol_qp)

            x = [(0.25_qp*(i-1), i = 1, n)]
            val = simps(y, x)
            ans = 31.25_qp
            print *, "  x=0,0.25,0.5,...", val, ans
            call check(abs(val - ans) < tol_qp)
        end do
    end subroutine test_simps_six_qp

end program
