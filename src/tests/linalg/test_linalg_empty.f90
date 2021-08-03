! SPDX-Identifier: MIT
module test_linalg_empty

    use stdlib_error, only: check
    use stdlib_linalg, only: empty
    implicit none
    logical :: warn = .false.

contains

    subroutine test_linalg_empty_integer
        call check(size(empty(2)) == 2,    msg="size(empty(2)) == 2 failed",   warn=warn)
        call check(size(empty(1, 2)) == 2, msg="size(empty(1,2)) == 2 failed", warn=warn)
    end subroutine test_linalg_empty_integer

    subroutine test_linalg_empty_real
        real, allocatable :: rA(:), rB(:, :)
        rA = empty(2)
        call check(size(rA) == 2, msg="size(rA) == 2 failed.", warn=warn)
        rB = empty(1, 2)
        call check(size(rB) == 2, msg="size(rB) == 2 failed.", warn=warn)
    end subroutine test_linalg_empty_real

    subroutine test_linalg_empty_cmplx
        complex, allocatable :: cA(:), cB(:, :)
        cA = empty(2)
        call check(size(cA) == 2, msg="size(cA) == 2 failed.", warn=warn)
        cB = empty(1, 2)
        call check(size(cB) == 2, msg="size(cB) == 2 failed.", warn=warn)
    end subroutine test_linalg_empty_cmplx

end module test_linalg_empty

program tester

    use test_linalg_empty

    call test_linalg_empty_integer
    call test_linalg_empty_real
    call test_linalg_empty_cmplx

    print *, "All tests in `test_linalg_empty` passed."

end program tester
