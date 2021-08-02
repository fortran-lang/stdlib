!> SPDX-Identifier: MIT
module test_linalg_expand

    use stdlib_linalg, only: zeros, ones, expand
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
        real, allocatable :: rA(:), rB(:,:)
        rA = zeros(2)
        call check(all(rA == expand(0.0_4, 2)), msg="all(rA == expand(1.0_4, 2)) failed", warn=warn)
        rB = zeros(2,2)
        call check(all(rB == expand(0.0_4, 2,2)), &
                   msg="all(rB == expand(1.0_4, 2,2)) failed", warn=warn)
    end subroutine test_linalg_zeros_real

    subroutine test_linalg_zeros_complex
        complex, allocatable :: cA(:), cB(:,:)
        cA = zeros(2)
        call check(all(cA == expand((0.0_4,0.0_4), 2)), msg="all(cA == expand((1.0_4,0.0_4), 2)) failed", warn=warn)
        cB = zeros(2,2)
        call check(all(cB == expand((0.0_4,0.0_4), 2,2)), &
                   msg="all(cB == expand((1.0_4,0.0_4), 2,2)) failed", warn=warn)
    end subroutine test_linalg_zeros_complex

    !> `ones` tests

    subroutine test_linalg_ones_integer
        call check(all(ones(2) == [1, 1]), msg="all(ones(2)==[1, 1] failed", warn=warn)
        call check(all(ones(2, 2) == reshape([1, 1, 1, 1], [2, 2])), &
                   msg="all(ones(2,2)==reshape([1, 1, 1, 1],[2,2]) failed", warn=warn)
    end subroutine test_linalg_ones_integer

    subroutine test_linalg_ones_real
        real, allocatable :: rA(:), rB(:,:)
        rA = ones(2)
        call check(all(rA == expand(1.0_4, 2)), msg="all(rA == expand(1.0_4, 2)) failed", warn=warn)
        rB = ones(2,2)
        call check(all(rB == expand(1.0_4, 2,2)), &
                   msg="all(rB == expand(1.0_4, 2,2)) failed", warn=warn)
    end subroutine test_linalg_ones_real

    subroutine test_linalg_ones_complex
        complex, allocatable :: cA(:), cB(:,:)
        cA = ones(2)
        call check(all(cA == expand((1.0_4,0.0_4), 2)), msg="all(cA == expand((1.0_4,0.0_4), 2)) failed", warn=warn)
        cB = ones(2,2)
        call check(all(cB == expand((1.0_4,0.0_4), 2,2)), &
                   msg="all(cB == expand((1.0_4,0.0_4), 2,2)) failed", warn=warn)
    end subroutine test_linalg_ones_complex

    !> `expand` tests 

    subroutine test_linalg_expand_integer
        call check(all(expand(1, 2) == ones(2)), msg="all(expand(1, 2) == ones(2)) failed", warn=warn)
        call check(all(expand(1, 2, 2) == ones(2, 2)), &
                   msg="all(expand(1, 2, 2) == ones(2,2)) failed", warn=warn)
    end subroutine test_linalg_expand_integer

    subroutine test_linalg_expand_real
        call check(all(expand(1.0, 2) == 1.0*ones(2)), msg="all(expand(1.0, 2) == 1.0*ones(2)) failed", warn=warn)
        call check(all(expand(1.0, 2, 2) == 1.0*ones(2, 2)), &
                   msg="all(expand(1.0, 2, 2) == 1.0*ones(2,2)) failed", warn=warn)
    end subroutine test_linalg_expand_real

    subroutine test_linalg_expand_complex
        call check(all(expand((1.0, 1.0), 2) == (1.0, 1.0)*ones(2)), &
                   msg="all(expand((1.0,1.0), 2) == (1.0,1.0)*ones(2)) failed", warn=warn)
        call check(all(expand((1.0, 1.0), 2, 2) == (1.0, 1.0)*ones(2, 2)), &
                   msg="all(expand((1.0,1.0), 2, 2) == (1.0,1.0)*ones(2,2)) failed", warn=warn)
    end subroutine test_linalg_expand_complex

    subroutine test_linalg_expand_logical
        call check(all(expand(.true., 2) .eqv. [.true., .true.]), &
                   msg="all(expand(.true., 2) .eqv. [.true., .true.]) failed", warn=warn)
        call check(all(expand(.true., 1, 2) .eqv. reshape([.true., .true.], [1, 2])), &
                   msg="all(expand(.true., 1, 2) .eqv. reshape([.true., .true.],[1,2])) failed", warn=warn)
    end subroutine test_linalg_expand_logical

    subroutine test_linalg_expand_string_type

        type(string_type) :: string_list(1,2)
        string_list = string_type("A")

        call check(all(expand(string_type("A"), 2) == [string_type("A"), string_type("A")]), &
                   msg='all(expand(string_type("A"), 2) == [string_type("A"), &
                   &string_type("A")]) failed', warn=warn)
        call check(all(expand(string_type("A"), 1, 2) == string_list), &
                   msg='all(expand(string_type("A"), 1, 2) == reshape([string_type("A"), &
                   &string_type("A")],[1,2])) failed', warn=warn)

    end subroutine test_linalg_expand_string_type

end module test_linalg_expand

program tester

    use test_linalg_expand

    print *, "`zeros` tests"
    call test_linalg_zeros_integer
    call test_linalg_zeros_real
    call test_linalg_zeros_complex

    print *, "`ones` tests"
    call test_linalg_ones_integer
    call test_linalg_ones_real
    call test_linalg_ones_complex

    print *, "`expand` tests"
    call test_linalg_expand_integer
    call test_linalg_expand_real
    call test_linalg_expand_complex
    call test_linalg_expand_logical
    call test_linalg_expand_string_type

    print *, "All tests in `test_linalg_expand` passed"

end program tester
