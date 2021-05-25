! SPDX-Identifier: MIT
module test_string_functions
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), operator(==), &
                                    to_lower, to_upper, to_title, to_sentence, reverse
    use stdlib_strings, only: slice                                    
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
        test_string = "tO_%t!TL3 7h1S p#ra$e"
        compare_string = "To_%T!Tl3 7h1s P#Ra$E"

        call check(to_title(test_string) == compare_string)

    end subroutine test_to_title_string
    
    subroutine test_to_sentence_string
        type(string_type) :: test_string, compare_string
        test_string = "_#To seNtEncE !$%-az09AZ"
        compare_string = "_#To sentence !$%-az09az"

        call check(to_sentence(test_string) == compare_string)

    end subroutine test_to_sentence_string

    subroutine test_reverse_string
        type(string_type) :: test_string, compare_string
        test_string = "_To ReVerSe !$%-az09AZ   "
        compare_string = "   ZA90za-%$! eSreVeR oT_"

        call check(reverse(test_string) == compare_string)

    end subroutine test_reverse_string

    subroutine test_slice_string
        type(string_type) :: test_string
        character(len=:), allocatable :: test_char
        test_string = "abcdefghijklmnopqrstuvwxyz"
        test_char = "abcdefghijklmnopqrstuvwxyz"

        call check(slice(test_string, 2, 16, 3) == "behkn", &
                    'function slice failed', warn=.true.)
        call check(slice(test_char, 15, stride=-1) == "onmlkjihgfedcba", &
                    'function slice failed', warn=.true.)
        call check(slice(test_string, last=22, stride=-1) == "zyxwv", &
                    'function slice failed', warn=.true.)
        call check(slice(test_char, 7, 2) == "gfedcb", &
                    'function slice failed', warn=.true.)
        call check(slice(test_string, 7, 2, 1) == "", &
                    'function slice failed', warn=.true.)
        call check(slice(test_char, 2, 6, -1) == "", &
                    'function slice failed', warn=.true.)
        call check(slice(test_string, stride=-1) == "zyxwvutsrqponmlkjihgfedcba", &
                    'function slice failed', warn=.true.)
        call check(slice(test_string, 7, 7, -4) == "g", &
                    'function slice failed', warn=.true.)
        call check(slice(test_char, 7, 7, 3) == "g", &
                    'function slice failed', warn=.true.)
        call check(slice(test_string, 7, 7, 3) == "g", &
                    'function slice failed', warn=.true.)
        call check(slice(test_char, 7, -10) == "gfedcba", &
                    'function slice failed', warn=.true.)
        call check(slice(test_string, 500, 22) == "zyxwv", &
                    'function slice failed', warn=.true.)

        test_string = ""
        test_char = ""
        call check(slice(test_string, 2, 16, 3) == "", &
                    'function slice failed', warn=.true.)
        call check(slice(test_char, 2, 16, 3) == "", &
                    'function slice failed', warn=.true.)

    end subroutine test_slice_string

end module test_string_functions


program tester
    use test_string_functions
    implicit none

    call test_to_lower_string
    call test_to_upper_string
    call test_to_title_string
    call test_to_sentence_string
    call test_reverse_string
    call test_slice_string

end program tester
