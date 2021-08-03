!> SPDX-Identifier: MIT
module test_math_arange

    use stdlib_error, only: check
    use stdlib_math, only: arange
    implicit none

    logical, private :: warn = .false.

contains

    subroutine test_math_arange_real
        !> Normal
        call check(all(arange(3.0) == [1.0, 2.0, 3.0]),        msg="all(arange(3.0) == [1.0,2.0,3.0]) failed.",           warn=warn)
        call check(all(arange(-1.0) == [1.0, 0.0, -1.0]),      msg="all(arange(-1.0) == [1.0,0.0,-1.0]) failed.",         warn=warn)
        call check(all(arange(0.0, 2.0) == [0.0, 1.0, 2.0]),   msg="all(arange(0.0,2.0) == [0.0,1.0,2.0]) failed.",       warn=warn)
        call check(all(arange(1.0, -1.0) == [1.0, 0.0, -1.0]), msg="all(arange(1.0,-1.0) == [1.0,0.0,-1.0]) failed.",     warn=warn)
        call check(all(arange(1.0, 1.0) == [1.0]),             msg="all(arange(1.0,1.0) == [1.0]) failed.",               warn=warn)
        call check(all(arange(0.0, 2.0, 2.0) == [0.0, 2.0]),   msg="all(arange(0.0,2.0,2.0) == [0.0,2.0]) failed.",       warn=warn)
        call check(all(arange(1.0, -1.0, 2.0) == [1.0, -1.0]), msg="all(arange(1.0,-1.0,2.0) == [1.0,-1.0]) failed.",     warn=warn)
        !> Not recommended
        call check(all(arange(0.0, 2.0, -2.0) == [0.0, 2.0]),  msg="all(arange(0.0,2.0,-2.0) == [0.0,2.0]) failed.",      warn=warn)
        call check(all(arange(1.0, -1.0, -2.0) == [1.0, -1.0]),msg="all(arange(1.0,-1.0,-2.0) == [1.0,-1.0]) failed.",    warn=warn)
        call check(all(arange(0.0, 2.0, 0.0) == [0.0,1.0,2.0]),msg="all(arange(0.0, 2.0, 0.0) == [0.0,1.0,2.0]) failed.", warn=warn)
    end subroutine test_math_arange_real

    subroutine test_math_arange_integer
        !> Normal
        call check(all(arange(3) == [1, 2, 3]),        msg="all(arange(3) == [1,2,3]) failed.",       warn=warn)
        call check(all(arange(-1) == [1, 0, -1]),      msg="all(arange(-1) == [1,0,-1]) failed.",     warn=warn)
        call check(all(arange(0, 2) == [0, 1, 2]),     msg="all(arange(0,2) == [0,1,2]) failed.",     warn=warn)
        call check(all(arange(1, -1) == [1, 0, -1]),   msg="all(arange(1,-1) == [1,0,-1]) failed.",   warn=warn)
        call check(all(arange(1, 1) == [1]),           msg="all(arange(1,1) == [1]) failed.",         warn=warn)
        call check(all(arange(0, 2, 2) == [0, 2]),     msg="all(arange(0,2,2) == [0,2]) failed.",     warn=warn)
        call check(all(arange(1, -1, 2) == [1, -1]),   msg="all(arange(1,-1,2) == [1,-1]) failed.",   warn=warn)
        !> Not recommended
        call check(all(arange(0, 2, -2) == [0, 2]),    msg="all(arange(0,2,-2) == [0,2]) failed.",    warn=warn)
        call check(all(arange(1, -1, -2) == [1, -1]),  msg="all(arange(1,-1,-2) == [1,-1]) failed.",  warn=warn)
        call check(all(arange(0, 2, 0) == [0,1,2]),    msg="all(arange(0, 2, 0) == [0,1,2]) failed.", warn=warn)
    end subroutine test_math_arange_integer

end module test_math_arange

program tester

    use test_math_arange

    call test_math_arange_real
    call test_math_arange_integer
    
    print *, "All tests in `test_math_arange` passed."

end program tester
