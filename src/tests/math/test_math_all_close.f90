program tester

    use stdlib_math, only: all_close
    use stdlib_error, only: check
    implicit none
    
    call test_math_all_close_real
    call test_math_all_close_complex
    print *, "All tests in `test_math_all_close` passed."

contains

    subroutine test_math_all_close_real

        real :: x(4, 4), random(4, 4)

        call random_number(random)
        x = 1.0

        call check(all_close(x+1.0e-11*random, x), msg="REAL: all_close(x+1.0e-11*random, x) failed.")
        call check(all_close(x+1.0e-5 *random, x), msg="REAL: all_close(x+1.0e-5 *random, x) failed.", warn=.true.)

    end subroutine test_math_all_close_real

    subroutine test_math_all_close_complex

        real    :: random(4, 4)
        complex :: x(4, 4)

        call random_number(random)
        x = 1.0

        call check(all_close(x+1.0e-11*random, x), msg="CMPLX: all_close(x+1.0e-11*random, x)")
        call check(all_close(x+1.0e-5 *random, x), msg="CMPLX: all_close(x+1.0e-5 *random, x) failed.", warn=.true.)

    end subroutine test_math_all_close_complex

end program tester
