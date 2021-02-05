! SPDX-Identifer: MIT
module test_string_intrinsic
    use stdlib_error, only : check
    use stdlib_string_type
    implicit none

contains

    subroutine test_lgt
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = lgt(string, "abc")
        call check(res .eqv. .true.)

        res = lgt(string, "bcd")
        call check(res .eqv. .false.)

        res = lgt(string, "cde")
        call check(res .eqv. .false.)
    end subroutine test_lgt

    subroutine test_llt
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = llt(string, "abc")
        call check(res .eqv. .false.)

        res = llt(string, "bcd")
        call check(res .eqv. .false.)

        res = llt(string, "cde")
        call check(res .eqv. .true.)
    end subroutine test_llt

    subroutine test_lge
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = lge(string, "abc")
        call check(res .eqv. .true.)

        res = lge(string, "bcd")
        call check(res .eqv. .true.)

        res = lge(string, "cde")
        call check(res .eqv. .false.)
    end subroutine test_lge

    subroutine test_lle
        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = lle(string, "abc")
        call check(res .eqv. .false.)

        res = lle(string, "bcd")
        call check(res .eqv. .true.)

        res = lle(string, "cde")
        call check(res .eqv. .true.)
    end subroutine test_lle

    subroutine test_trim
       type(string_type) :: string, trimmed_str

       string = "Whitespace                            "
       trimmed_str = trim(string)
       call check(len(trimmed_str) == 10)
    end subroutine test_trim

    subroutine test_len
        type(string_type) :: string
        integer :: length

        string = "Some longer sentence for this example."
        length = len(string)
        call check(length == 38)

        string = "Whitespace                            "
        length = len(string)
        call check(length == 38)
    end subroutine test_len

    subroutine test_len_trim
        type(string_type) :: string
        integer :: length

        string = "Some longer sentence for this example."
        length = len_trim(string)
        call check(length == 38)

        string = "Whitespace                            "
        length = len_trim(string)
        call check(length == 10)
    end subroutine test_len_trim

    subroutine test_adjustl
        type(string_type) :: string

        string = "                            Whitespace"
        string = adjustl(string)
        call check(char(string) == "Whitespace                            ")
    end subroutine test_adjustl

    subroutine test_adjustr
        type(string_type) :: string

        string = "Whitespace                            "
        string = adjustr(string)
        call check(char(string) == "                            Whitespace")
    end subroutine test_adjustr

    subroutine test_scan
        type(string_type) :: string
        integer :: pos

        string = "fortran"
        pos = scan(string, "ao")
        call check(pos == 2)

        pos = scan(string, "ao", .true.)
        call check(pos == 6)

        pos = scan(string, "c++")
        call check(pos == 0)
    end subroutine test_scan

    subroutine test_verify
        type(string_type) :: string
        integer :: pos

        string = "fortran"
        pos = verify(string, "ao")
        call check(pos == 1)

        pos = verify(string, "fo")
        call check(pos == 3)

        pos = verify(string, "c++")
        call check(pos == 1)

        pos = verify(string, "c++", back=.true.)
        call check(pos == 7)

        pos = verify(string, string)
        call check(pos == 0)
    end subroutine test_verify

    subroutine test_repeat
        type(string_type) :: string

        string = "What? "
        string = repeat(string, 3)
        call check(string == "What? What? What? ")
    end subroutine test_repeat

    subroutine test_index
        type(string_type) :: string
        integer :: pos

        string = "Search this string for this expression"
        pos = index(string, "this")
        call check(pos == 8)

        pos = index(string, "this", back=.true.)
        call check(pos == 24)

        pos = index(string, "This")
        call check(pos == 0)
    end subroutine test_index

    subroutine test_char
        type(string_type) :: string
        character(len=:), allocatable :: dlc
        character(len=1), allocatable :: chars(:)

        string = "Character sequence"
        dlc = char(string)
        call check(dlc == "Character sequence")

        dlc = char(string, 3)
        call check(dlc == "a")
        chars = char(string, [3, 5, 8, 12, 14, 15, 18])
        call check(all(chars == ["a", "a", "e", "e", "u", "e", "e"]))

        string = "Fortran"
        dlc = char(string, 1, 4)
        call check(dlc == "Fort")
    end subroutine test_char

    subroutine test_ichar
        type(string_type) :: string
        integer :: code

        string = "Fortran"
        code = ichar(string)
        call check(code == ichar("F"))
    end subroutine test_ichar

    subroutine test_iachar
        type(string_type) :: string
        integer :: code

        string = "Fortran"
        code = iachar(string)
        call check(code == iachar("F"))
    end subroutine test_iachar

end module test_string_intrinsic

program tester
    use test_string_intrinsic
    implicit none

    call test_lgt
    call test_llt
    call test_lge
    call test_lle
    call test_trim
    call test_len
    call test_len_trim
    call test_adjustl
    call test_adjustr
    call test_scan
    call test_verify
    call test_repeat
    call test_index
    call test_char
    call test_ichar
    call test_iachar

end program tester

