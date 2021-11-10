program tester

    use stdlib_math, only: all_close
    use stdlib_error, only: check
    implicit none
    real :: y, NAN

    y = -3
    NAN = sqrt(y)

    call test_math_all_close_real
    call test_math_all_close_complex
    print *, "All tests in `test_math_all_close` passed."

contains

    subroutine test_math_all_close_real

        real :: x(4, 4) = 1.0

        call check(all_close(x + 1.0e-11, x), msg="REAL: all_close(x+1.0e-11, x) failed.")
        call check(all_close(x + 1.0e-5, x), msg="REAL: all_close(x+1.0e-5 , x) failed. (expected)", warn=.true.)

        !> Tests for NAN
        call check(all_close(x + NAN, x), msg="REAL: all_close(x+NAN, x) failed. (expected)", warn=.true.)
        call check(all_close(x + NAN, x + NAN, equal_nan=.true.), msg="REAL: all_close(x+NAN, x, equal_nan=.true.) failed.")

    end subroutine test_math_all_close_real

    subroutine test_math_all_close_complex

        complex :: x(4, 4) = cmplx(1.0, 1.0)

        call check(all_close(x + cmplx((1.0e-15, 1.0e-15)), x), msg="CMPLX: all_close(x+cmplx(1.0e-11, 1.0e-11), x)")
        call check(all_close(x + cmplx(1.0e-5, 1.0e-5), x), &
                   msg="CMPLX: all_close(x+cmplx(1.0e-5 , 1.0e-5 ), x) failed. (expected)", warn=.true.)

        !> Tests for NAN
        call check(all_close(x + cmplx(NAN, NAN), x), msg="REAL: all_close(x+cmplx(NAN, NAN), x) failed. (expected)", warn=.true.)
        call check(all_close(x + cmplx(NAN, NAN), x + cmplx(NAN, NAN), equal_nan=.true.), &
                   msg="REAL: all_close(x+cmplx(NAN, NAN), x, equal_nan=.true.) failed.")

    end subroutine test_math_all_close_complex

end program tester
