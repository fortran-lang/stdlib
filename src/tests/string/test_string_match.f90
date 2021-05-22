! SPDX-Identifier: MIT
module test_match
    use stdlib_ascii, only : reverse
    use stdlib_error, only : check
    use stdlib_strings, only : starts_with, ends_with
    use stdlib_string_type, only : string_type
    implicit none

contains

    subroutine check_starts_with(string, substring)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: match
        character(len=:), allocatable :: message

        match = index(string, substring) == 1
        if (match) then
            message = "Failed to recognize that '"//string//"' starts with '"//substring//"'"
        else
            message = "Incorrectly found that '"//string//"' starts with '"//substring//"'"
        end if

        call check(starts_with(string, substring) .eqv. match, message)
        call check(starts_with(string_type(string), substring) .eqv. match, message)
        call check(starts_with(string, string_type(substring)) .eqv. match, message)
        call check(starts_with(string_type(string), string_type(substring)) .eqv. match, message)
    end subroutine check_starts_with

    subroutine test_starts_with
        call check_starts_with("pattern", "pat")
        call check_starts_with("pat", "pattern")
        call check_starts_with("pattern", "ern")
        call check_starts_with("ern", "pattern")
    end subroutine test_starts_with

    subroutine check_ends_with(string, substring)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: match
        character(len=:), allocatable :: message

        match = index(reverse(string), reverse(substring)) == 1
        if (match) then
            message = "Failed to recognize that '"//string//"' ends with '"//substring//"'"
        else
            message = "Incorrectly found that '"//string//"' ends with '"//substring//"'"
        end if

        call check(ends_with(string, substring) .eqv. match, message)
        call check(ends_with(string_type(string), substring) .eqv. match, message)
        call check(ends_with(string, string_type(substring)) .eqv. match, message)
        call check(ends_with(string_type(string), string_type(substring)) .eqv. match, message)
    end subroutine check_ends_with

    subroutine test_ends_with
        call check_ends_with("pattern", "pat")
        call check_ends_with("pat", "pattern")
        call check_ends_with("pattern", "ern")
        call check_ends_with("ern", "pattern")
    end subroutine test_ends_with

end module test_match

program tester
    use test_match
    implicit none

    call test_starts_with
    call test_ends_with

end program tester
