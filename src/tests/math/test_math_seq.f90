!> SPDX-Identifier: MIT
module test_math_seq

    use stdlib_error, only: check
    use stdlib_math, only: seq
    implicit none

    logical, private :: warn = .false.

contains

    subroutine test_math_seq_real
        !> Normal
        call check(all(seq(3.0) == [1.0, 2.0, 3.0]),        msg="all(seq(3.0) == [1.0,2.0,3.0]) failed.",           warn=warn)
        call check(all(seq(-1.0) == [1.0, 0.0, -1.0]),      msg="all(seq(-1.0) == [1.0,0.0,-1.0]) failed.",         warn=warn)
        call check(all(seq(0.0, 2.0) == [0.0, 1.0, 2.0]),   msg="all(seq(0.0,2.0) == [0.0,1.0,2.0]) failed.",       warn=warn)
        call check(all(seq(1.0, -1.0) == [1.0, 0.0, -1.0]), msg="all(seq(1.0,-1.0) == [1.0,0.0,-1.0]) failed.",     warn=warn)
        call check(all(seq(1.0, 1.0) == [1.0]),             msg="all(seq(1.0,1.0) == [1.0]) failed.",               warn=warn)
        call check(all(seq(0.0, 2.0, 2.0) == [0.0, 2.0]),   msg="all(seq(0.0,2.0,2.0) == [0.0,2.0]) failed.",       warn=warn)
        call check(all(seq(1.0, -1.0, 2.0) == [1.0, -1.0]), msg="all(seq(1.0,-1.0,2.0) == [1.0,-1.0]) failed.",     warn=warn)
        !> Not recommended
        call check(all(seq(0.0, 2.0, -2.0) == [0.0, 2.0]),  msg="all(seq(0.0,2.0,-2.0) == [0.0,2.0]) failed.",      warn=warn)
        call check(all(seq(1.0, -1.0, -2.0) == [1.0, -1.0]),msg="all(seq(1.0,-1.0,-2.0) == [1.0,-1.0]) failed.",    warn=warn)
        call check(all(seq(0.0, 2.0, 0.0) == [0.0,1.0,2.0]),msg="all(seq(0.0, 2.0, 0.0) == [0.0,1.0,2.0]) failed.", warn=warn)
    end subroutine test_math_seq_real

    subroutine test_math_seq_integer
        !> Normal
        call check(all(seq(3) == [1, 2, 3]),        msg="all(seq(3) == [1,2,3]) failed.",       warn=warn)
        call check(all(seq(-1) == [1, 0, -1]),      msg="all(seq(-1) == [1,0,-1]) failed.",     warn=warn)
        call check(all(seq(0, 2) == [0, 1, 2]),     msg="all(seq(0,2) == [0,1,2]) failed.",     warn=warn)
        call check(all(seq(1, -1) == [1, 0, -1]),   msg="all(seq(1,-1) == [1,0,-1]) failed.",   warn=warn)
        call check(all(seq(1, 1) == [1]),           msg="all(seq(1,1) == [1]) failed.",         warn=warn)
        call check(all(seq(0, 2, 2) == [0, 2]),     msg="all(seq(0,2,2) == [0,2]) failed.",     warn=warn)
        call check(all(seq(1, -1, 2) == [1, -1]),   msg="all(seq(1,-1,2) == [1,-1]) failed.",   warn=warn)
        !> Not recommended
        call check(all(seq(0, 2, -2) == [0, 2]),    msg="all(seq(0,2,-2) == [0,2]) failed.",    warn=warn)
        call check(all(seq(1, -1, -2) == [1, -1]),  msg="all(seq(1,-1,-2) == [1,-1]) failed.",  warn=warn)
        call check(all(seq(0, 2, 0) == [0,1,2]),    msg="all(seq(0, 2, 0) == [0,1,2]) failed.", warn=warn)
    end subroutine test_math_seq_integer

end module test_math_seq

program tester

    use test_math_seq

    call test_math_seq_real
    call test_math_seq_integer
    
    print *, "All tests in `test_math_seq` passed."

end program tester
