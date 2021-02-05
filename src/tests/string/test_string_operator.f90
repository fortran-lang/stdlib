! SPDX-Identifer: MIT
module test_string_operator
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), len, &
        operator(.gt.), operator(.lt.), operator(.ge.), operator(.le.), &
        operator(.ne.), operator(.eq.), operator(//)
    implicit none

contains

    subroutine test_gt
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string > "abc"
        call check(res .eqv. .true.)

        res = string > "bcd"
        call check(res .eqv. .false.)

        res = string > "cde"
        call check(res .eqv. .false.)
    end subroutine test_gt

    subroutine test_lt
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string < "abc"
        call check(res .eqv. .false.)

        res = string < "bcd"
        call check(res .eqv. .false.)

        res = string < "cde"
        call check(res .eqv. .true.)
    end subroutine test_lt

    subroutine test_ge
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string >= "abc"
        call check(res .eqv. .true.)

        res = string >= "bcd"
        call check(res .eqv. .true.)

        res = string >= "cde"
        call check(res .eqv. .false.)
    end subroutine test_ge

    subroutine test_le
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string <= "abc"
        call check(res .eqv. .false.)

        res = string <= "bcd"
        call check(res .eqv. .true.)

        res = string <= "cde"
        call check(res .eqv. .true.)
    end subroutine test_le

    subroutine test_eq
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string == "abc"
        call check(res .eqv. .false.)

        res = string == "bcd"
        call check(res .eqv. .true.)

        res = string == "cde"
        call check(res .eqv. .false.)
    end subroutine test_eq

    subroutine test_ne
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string /= "abc"
        call check(res .eqv. .true.)

        res = string /= "bcd"
        call check(res .eqv. .false.)

        res = string /= "cde"
        call check(res .eqv. .true.)
    end subroutine test_ne

    subroutine test_concat
        type(string_type) :: string

        string = "Hello, "
        string = string // "World!"
        call check(len(string) == 13)
    end subroutine test_concat

end module test_string_operator

program tester
    use test_string_operator
    implicit none

    call test_gt
    call test_lt
    call test_ge
    call test_le
    call test_eq
    call test_ne
    call test_concat

end program tester
