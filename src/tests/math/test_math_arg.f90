program tester

    real :: tol = epsilon(1.0)
    call test_math_arg_complex
    call test_math_argd_complex
    call test_math_argpi_complex
    print *, "All tests in `test_math_arg` passed."

contains

    subroutine test_math_arg_complex
        use stdlib_math,  only: arg
        use stdlib_error, only: check

        print *, 2.0*exp((0.0e0, 0.5))      !! (1.75516510, 0.958851099)

        call check(abs(arg(2.0*exp((0.0e0, 0.5))) - 0.5) < tol,     msg="arg(2.0*exp((0.0e0, 0.5))) - 0.5) failed.")
        call check(abs(arg((1.75516510, 0.958851099)) - 0.5) < tol, msg="arg((1.75516510, 0.958851099)) - 0.5) failed.")
        call check(abs(arg((0.0, 0.0)) - 0.0) < tol,                msg="arg((0.0, 0.0)) failed.")

    end subroutine test_math_arg_complex

    subroutine test_math_argd_complex
        use stdlib_math,  only: argd
        use stdlib_error, only: check
        
        call check(abs(argd(2.0*exp((0.0, 0.5))) - 28.6478882) < tol, msg="argd(2.0*exp((0.0, 0.5))) - 28.6478882) failed.")
        call check(abs(argd((1.75516510, 0.958851099)) - 28.6478882) < tol, &
                   msg="argd((1.75516510, 0.958851099)) - 28.6478882) failed.")
        call check(abs(argd((0.0, 0.0)) - 0.0) < tol,                 msg="argd((0.0, 0.0)) failed.")

    end subroutine test_math_argd_complex

    subroutine test_math_argpi_complex
        use stdlib_math,  only: argpi
        use stdlib_error, only: check
        real, parameter :: PI = 4*atan(1.0)

        !> Power exponent calculation will introduce calculation errors: 2.0*exp((0.0, PI)) /= (-2.0, 0.0).
        print *, 2.0*exp((0.0, PI)), 2.0*exp(cmplx(0.0, -PI))               !! (-2.00000000,-1.748455531E-07), (-2.00000000,1.748455531E-07)
        print *, argpi(2.0*exp((0.0, PI))), argpi(2.0*exp(cmplx(0.0, -PI))) !! -0.999999940,                   0.999999940

        call check(abs(argpi(2.0*exp((0.0, PI)))    - (- 1.0)) < tol, msg="argpi(2.0*exp((0.0, PI))) - (- 1.0)) failed.")
        call check(abs(argpi(2.0*exp(cmplx(0.0, -PI))) - 1.0)  < tol, msg="argpi(2.0*exp(cmplx(0.0, -PI))) - 1.0) failed.")

        call check(abs(argpi((-2.0, 0.0)) - 1.0) < tol, msg="argpi((-2.0, 0.0)) - 1.0) failed.") !! (-1.0, 1.0]
        call check(abs(argpi(( 0.0, 0.0)) - 0.0) < tol, msg="argpi((0.0, 0.0)) failed.")

    end subroutine test_math_argpi_complex

end program tester
