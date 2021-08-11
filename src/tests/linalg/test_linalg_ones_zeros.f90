!> SPDX-Identifier: MIT
module test_linalg_ones_zeros

    use stdlib_linalg, only: zeros, ones
    use stdlib_error, only: check
    use stdlib_string_type
    implicit none

    logical, parameter :: warn = .false.

contains

    !> `zeros` tests
    subroutine test_linalg_zeros_integer
        call check(all(zeros(2) == [0, 0]), msg="all(zeros(2)==[0, 0] failed", warn=warn)
        call check(all(zeros(2, 2) == reshape([0, 0, 0, 0], [2, 2])), &
                   msg="all(zeros(2,2)==reshape([0, 0, 0, 0],[2,2]) failed", warn=warn)
    end subroutine test_linalg_zeros_integer

    subroutine test_linalg_zeros_real
        real, allocatable :: rA(:), rB(:, :)
        rA = zeros(2)
        call check(all(rA == spread(0.0_4, 1, 2)), msg="all(rA == spread(0.0_4,1,2)) failed", warn=warn)
        rB = zeros(2, 2)
        call check(all(rB == reshape(spread(0.0_4, 1, 2*2), [2, 2])), &
                   msg="all(rB == reshape(spread(0.0_4, 1,2*2),[2,2])) failed", warn=warn)
    end subroutine test_linalg_zeros_real

    subroutine test_linalg_zeros_complex
        complex, allocatable :: cA(:), cB(:, :)
        cA = zeros(2)
        call check(all(cA == spread((0.0_4, 0.0_4), 1, 2)), msg="all(cA == spread((0.0_4,0.0_4),1,2)) failed", warn=warn)
        cB = zeros(2, 2)
        call check(all(cB == reshape(spread((0.0_4, 0.0_4), 1, 2*2), [2, 2])), &
                   msg="all(cB == reshape(spread((0.0_4,0.0_4), 1, 2*2), [2, 2])) failed", warn=warn)
    end subroutine test_linalg_zeros_complex

    !> `ones` tests
    subroutine test_linalg_ones_integer
        call check(all(ones(2) == [1, 1]), msg="all(ones(2)==[1, 1] failed", warn=warn)
        call check(all(ones(2, 2) == reshape([1, 1, 1, 1], [2, 2])), &
                   msg="all(ones(2,2)==reshape([1, 1, 1, 1],[2,2])) failed", warn=warn)
    end subroutine test_linalg_ones_integer

    subroutine test_linalg_ones_real
        real, allocatable :: rA(:), rB(:, :)
        rA = ones(2)
        call check(all(rA == spread(1.0_4, 1, 2)), msg="all(rA == spread(1.0_4,1,2)) failed", warn=warn)
        rB = ones(2, 2)
        call check(all(rB == reshape(spread(1.0_4, 1, 2*2), [2, 2])), &
                   msg="all(rB == reshape(spread(1.0_4, 1, 2*2), [2, 2])) failed", warn=warn)
    end subroutine test_linalg_ones_real

    subroutine test_linalg_ones_complex
        complex, allocatable :: cA(:), cB(:, :)
        cA = ones(2)
        call check(all(cA == spread((1.0_4, 0.0_4), 1, 2)), msg="all(cA == spread((1.0_4,0.0_4),1,2)) failed", warn=warn)
        cB = ones(2, 2)
        call check(all(cB == reshape(spread((1.0_4, 0.0_4), 1, 2*2), [2, 2])), &
                   msg="all(cB == reshape(spread((1.0_4, 0.0_4), 1, 2*2), [2, 2])) failed", warn=warn)
    end subroutine test_linalg_ones_complex

end module test_linalg_ones_zeros

program tester

    use test_linalg_ones_zeros

    print *, "`zeros` tests"
    call test_linalg_zeros_integer
    call test_linalg_zeros_real
    call test_linalg_zeros_complex

    print *, "`ones` tests"
    call test_linalg_ones_integer
    call test_linalg_ones_real
    call test_linalg_ones_complex

    print *, "All tests in `test_linalg_ones_zeros` passed"

end program tester
