program tester

    real :: tol = 1.0e-3
    call test_math_arg_complex
    call test_math_argd_complex
    call test_math_argpi_complex
    print *, "All tests in `test_math_arg` passed."

contains

    subroutine test_math_arg_complex
        use stdlib_math, only: arg
        use stdlib_error, only: check

        call check(abs(arg(2.0*exp((0.0e0, 0.5))) - 0.5) < tol, msg="arg(2.0*exp((0.0, 0.5))) failed.")
        call check(abs(arg((1.7552, 0.9589)) - 0.5) < tol, msg="arg((1.7552, 0.9589)) failed.")
        call check(abs(arg((0.0, 0.0)) - 0.0) < tol, msg="arg((0.0, 0.0)) failed.")

    end subroutine test_math_arg_complex

    subroutine test_math_argd_complex
        use stdlib_math, only: argd
        use stdlib_error, only: check
        
        call check(abs(argd(2.0*exp((0.0, 0.5))) - 28.648) < tol, msg="argd(2.0*exp((0.0, 0.5))) failed.")
        call check(abs(argd((1.7552, 0.9589)) - 28.648) < tol, msg="argd((1.7552, 0.9589)) failed.")
        call check(abs(argd((0.0, 0.0)) - 0.0) < tol, msg="argd((0.0, 0.0)) failed.")

    end subroutine test_math_argd_complex

    subroutine test_math_argpi_complex
        use stdlib_math, only: argpi
        use stdlib_error, only: check

        call check(abs(argpi(2.0*exp((0.0, 0.5))) - 0.159) < tol, msg="argpi(2.0*exp((0.0, 0.5))) failed.")
        call check(abs(argpi((1.7552, 0.9589)) - 0.159) < tol, msg="argpi((1.7552, 0.9589)) failed.")
        call check(abs(argpi((0.0, 0.0)) - 0.0) < tol, msg="argpi((0.0, 0.0)) failed.")

    end subroutine test_math_argpi_complex

end program tester
