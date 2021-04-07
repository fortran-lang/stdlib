! SPDX-Identifier: MIT
module test_string_functions
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), operator(==), &
                                    to_lower, to_upper, to_title, reverse
    implicit none

contains

    subroutine test_to_lower_string
        type(string_type) :: test_string, compare_string
        test_string = "To_LoWEr !$%-az09AZ"
        compare_string = "to_lower !$%-az09az"

        call check(to_lower(test_string) == compare_string)

    end subroutine test_to_lower_string

    subroutine test_to_upper_string
        type(string_type) :: test_string, compare_string
        test_string = "To_UpPeR !$%-az09AZ"
        compare_string = "TO_UPPER !$%-AZ09AZ"

        call check(to_upper(test_string) == compare_string)

    end subroutine test_to_upper_string

    subroutine test_to_title_string
        type(string_type) :: test_string, compare_string
        test_string = "_#To tiTlE !$%-az09AZ"
        compare_string = "_#To title !$%-az09az"

        call check(to_title(test_string) == compare_string)

    end subroutine test_to_title_string

    subroutine test_reverse_string
        type(string_type) :: test_string, compare_string
        test_string = "_To ReVerSe !$%-az09AZ   "
        compare_string = "   ZA90za-%$! eSreVeR oT_"

        call check(reverse(test_string) == compare_string)

    end subroutine test_reverse_string

end module test_string_functions


program tester
    use test_string_functions
    implicit none

    call test_to_lower_string
    call test_to_upper_string
    call test_to_title_string
    call test_reverse_string

end program tester
