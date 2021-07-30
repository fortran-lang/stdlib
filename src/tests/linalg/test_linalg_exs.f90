!> SPDX-Identifier: MIT
module test_linalg_exs

    use stdlib_linalg, only: zeros, ones, ex
    use stdlib_error, only: check
    use stdlib_string_type
    implicit none

    logical, parameter :: warn = .false.

contains

    subroutine test_linalg_zeros
        call check(all(zeros(2) == [0, 0]), msg="all(zeros(2)==[0, 0] failed", warn=warn)
        call check(all(zeros(2, 2) == reshape([0, 0, 0, 0], [2, 2])), &
                   msg="all(zeros(2,2)==reshape([0, 0, 0, 0],[2,2]) failed", warn=warn)
    end subroutine test_linalg_zeros

    subroutine test_linalg_ones
        call check(all(ones(2) == [1, 1]), msg="all(ones(2)==[1, 1] failed", warn=warn)
        call check(all(ones(2, 2) == reshape([1, 1, 1, 1], [2, 2])), &
                   msg="all(ones(2,2)==reshape([1, 1, 1, 1],[2,2]) failed", warn=warn)
    end subroutine test_linalg_ones

    subroutine test_linalg_ex_integer
        call check(all(ex(1, 2) == ones(2)), msg="all(ex(1, 2) == ones(2)) failed", warn=warn)
        call check(all(ex(1, 2, 2) == ones(2, 2)), &
                   msg="all(ex(1, 2, 2) == ones(2,2)) failed", warn=warn)
    end subroutine test_linalg_ex_integer

    subroutine test_linalg_ex_real
        call check(all(ex(1.0, 2) == 1.0*ones(2)), msg="all(ex(1.0, 2) == 1.0*ones(2)) failed", warn=warn)
        call check(all(ex(1.0, 2, 2) == 1.0*ones(2, 2)), &
                   msg="all(ex(1.0, 2, 2) == 1.0*ones(2,2)) failed", warn=warn)
    end subroutine test_linalg_ex_real

    subroutine test_linalg_ex_complex
        call check(all(ex((1.0, 1.0), 2) == (1.0, 1.0)*ones(2)), &
                   msg="all(ex((1.0,1.0), 2) == (1.0,1.0)*ones(2)) failed", warn=warn)
        call check(all(ex((1.0, 1.0), 2, 2) == (1.0, 1.0)*ones(2, 2)), &
                   msg="all(ex((1.0,1.0), 2, 2) == (1.0,1.0)*ones(2,2)) failed", warn=warn)
    end subroutine test_linalg_ex_complex

    subroutine test_linalg_ex_logical
        call check(all(ex(.true., 2) .eqv. [.true., .true.]), &
                   msg="all(ex(.true., 2) .eqv. [.true., .true.]) failed", warn=warn)
        call check(all(ex(.true., 1, 2) .eqv. reshape([.true., .true.], [1, 2])), &
                   msg="all(ex(.true., 1, 2) .eqv. reshape([.true., .true.],[1,2])) failed", warn=warn)
    end subroutine test_linalg_ex_logical

    subroutine test_linalg_ex_string_type
        call check(all(ex(string_type("A"), 2) == [string_type("A"), string_type("A")]), &
                   msg='all(ex(string_type("A"), 2) == [string_type("A"), &
                   &string_type("A")]) failed', warn=warn)
        call check(all(ex(string_type("A"), 1, 2) == reshape([string_type("A"), string_type("A")], [1, 2])), &
                   msg='all(ex(string_type("A"), 1, 2) == reshape([string_type("A"), &
                   &string_type("A")],[1,2])) failed', warn=warn)
    end subroutine test_linalg_ex_string_type

end module test_linalg_exs

program tester

    use test_linalg_exs

    call test_linalg_zeros
    call test_linalg_ones
    call test_linalg_ex_integer
    call test_linalg_ex_real
    call test_linalg_ex_complex
    call test_linalg_ex_logical
    call test_linalg_ex_string_type
    print *, "All tests in `test_linalg_exs` passed"

end program tester
