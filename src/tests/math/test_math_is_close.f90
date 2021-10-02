program test_math_is_close

    call test_math_is_close_real
    call test_math_is_close_complex
    print *, "All tests in `test_math_is_close` passed."

contains

    subroutine test_math_is_close_real
        use stdlib_math, only: is_close
        use stdlib_error, only: check

        call check(is_close(2.5, 2.5, rel_tol=1.0e-5), msg="is_close(2.5, 2.5, rel_tol=1.0e-5) failed.")
        call check(all(is_close([2.5, 3.2], [2.5, 10.0], rel_tol=1.0e-5)), &
                   msg="all(is_close([2.5, 3.2], [2.5, 10.0], rel_tol=1.0e-5)) failed (expected).", warn=.true.)
        call check(all(is_close(reshape([2.5, 3.2, 2.2, 1.0], [2, 2]), reshape([2.5, 3.2001, 2.25, 1.1], [2, 2]), &
                   abs_tol=1.0e-5, rel_tol=0.1)), &
                   msg="all(is_close(reshape([2.5, 3.2, 2.2, 1.0],[2,2]), reshape([2.5, 3.2001, 2.25, 1.1],[2,2]), &
                   &rel_tol=1.0e-5, abs_tol=0.1)) failed.")

        !> Tests for zeros
        call check(is_close(0.0, -0.0), msg="is_close(0.0, -0.0) failed.")

    end subroutine test_math_is_close_real

    subroutine test_math_is_close_complex
        use stdlib_math, only: is_close
        use stdlib_error, only: check

        call check(is_close((2.5,1.2), (2.5,1.2), rel_tol=1.0e-5), &
                   msg="is_close((2.5,1.2), (2.5,1.2), rel_tol=1.0e-5) failed.")
        call check(all(is_close([(2.5,1.2), (3.2,1.2)], [(2.5,1.2), (10.0,1.2)], rel_tol=1.0e-5)), &
                   msg="all(is_close([(2.5,1.2), (3.2,1.2)], [(2.5,1.2), (10.0,1.2)], rel_tol=1.0e-5)) failed (expected).", &
                   warn=.true.)
        call check(all(is_close(reshape([(2.5,1.2009), (3.2,1.199999)], [1, 2]), reshape([(2.4,1.2009), (3.15,1.199999)], [1, 2]), &
                   abs_tol=1.0e-5, rel_tol=0.1)), &
                   msg="all(is_close(reshape([(2.5,1.2009), (3.2,1.199999)], [1, 2]), &
                   &reshape([(2.4,1.2009), (3.15,1.199999)], [1, 2]), &
                   &rel_tol=1.0e-5, abs_tol=0.1)) failed.")

        !> Tests for zeros
        call check(is_close((0.0, -0.0), (-0.0, 0.0)), msg="is_close((0.0, -0.0), (-0.0, 0.0)) failed.")

    end subroutine test_math_is_close_complex

end program test_math_is_close
