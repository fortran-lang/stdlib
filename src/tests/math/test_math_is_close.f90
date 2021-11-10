program test_math_is_close

    implicit none

    real :: x, NAN
    x   = -3
    NAN = sqrt(x)

    call test_math_is_close_real
    call test_math_is_close_complex
    print *, "All tests in `test_math_is_close` passed."

contains

    subroutine test_math_is_close_real
        use stdlib_math, only: is_close
        use stdlib_error, only: check

        call check(is_close(2.5, 2.5, rel_tol=1.0e-5), msg="is_close(2.5, 2.5, rel_tol=1.0e-5) failed.")
        call check(all(is_close([2.5, 3.2], [2.5, 10.0], rel_tol=1.0e-5)), &
                   msg="all(is_close([2.5, 3.2], [2.5, 10.0], rel_tol=1.0e-5)) failed. (expected)", warn=.true.)
        call check(all(is_close(reshape([2.5, 3.2, 2.2, 1.0], [2, 2]), reshape([2.5, 3.2001, 2.25, 1.1], [2, 2]), &
                   abs_tol=1.0e-5, rel_tol=0.1)), &
                   msg="all(is_close(reshape([2.5, 3.2, 2.2, 1.0],[2,2]), reshape([2.5, 3.2001, 2.25, 1.1],[2,2]), &
                   &rel_tol=1.0e-5, abs_tol=0.1)) failed.")

        !> Tests for zeros
        call check(is_close(0.0, -0.0), msg="is_close(0.0, -0.0) failed.")

        !> Tests for NaN
        call check(is_close(NAN, NAN), msg="is_close(NAN, NAN) failed.", warn=.true.)
        call check(is_close(NAN, NAN, equal_nan=.true.), msg="is_close(NAN, NAN, equal_nan=.true.) failed.")

    end subroutine test_math_is_close_real

    subroutine test_math_is_close_complex
        use stdlib_math, only: is_close
        use stdlib_error, only: check

        call check(is_close((2.5, 1.2), (2.5, 1.2), rel_tol=1.0e-5), &
                   msg="is_close((2.5,1.2), (2.5,1.2), rel_tol=1.0e-5) failed.")
        call check(all(is_close([(2.5, 1.2), (3.2, 1.2)], [(2.5, 1.2), (10.0, 1.2)], rel_tol=1.0e-5)), &
                   msg="all(is_close([(2.5,1.2), (3.2,1.2)], [(2.5,1.2), (10.0,1.2)], rel_tol=1.0e-5)) failed. (expected)", &
                   warn=.true.)
        call check(all(is_close(reshape([(2.5, 1.2009), (3.2, 1.199999)], [1, 2]), &
                                reshape([(2.4, 1.2009), (3.15, 1.199999)], [1, 2]), &
                                abs_tol=1.0e-5, rel_tol=0.1)), &
                                msg="all(is_close(reshape([(2.5,1.2009), (3.2,1.199999)], [1, 2]), &
                                &reshape([(2.4,1.2009), (3.15,1.199999)], [1, 2]), &
                                &rel_tol=1.0e-5, abs_tol=0.1)) failed.")

        !> Tests for zeros
        call check(is_close((0.0, -0.0), (-0.0, 0.0)), msg="is_close((0.0, -0.0), (-0.0, 0.0)) failed.")

        !> Tests for NaN
        call check(is_close(cmplx(NAN, NAN), cmplx(NAN, NAN)), &
                   msg="is_close(cmplx(NAN, NAN), cmplx(NAN, NAN)) failed. (expected)", warn=.true.)
        call check(is_close(cmplx(NAN, NAN), cmplx(NAN, NAN), equal_nan=.true.), &
                   msg="is_close(cmplx(NAN, NAN), cmplx(NAN, NAN), equal_nan=.true.) failed.")
        call check(is_close(cmplx(NAN, 1.0), cmplx(NAN, 1.0)), &
                   msg="is_close(cmplx(NAN, NAN), cmplx(NAN, NAN)) failed. (expected)", warn=.true.)
        call check(is_close(cmplx(NAN, 1.0), cmplx(NAN, 1.0), equal_nan=.true.), &
                   msg="is_close(cmplx(NAN, NAN), cmplx(NAN, NAN), equal_nan=.ture.) failed.")

    end subroutine test_math_is_close_complex

end program test_math_is_close
