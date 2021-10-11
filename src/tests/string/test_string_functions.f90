! SPDX-Identifier: MIT
module test_string_functions
    use, intrinsic :: iso_fortran_env, only : error_unit
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), operator(==), &
                                    to_lower, to_upper, to_title, to_sentence, reverse
    use stdlib_strings, only: slice, find, replace_all, padl, padr, count
    use stdlib_optval, only: optval
    use stdlib_strings, only : to_string
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
        test_string = "abcdefghijklmnopqrstuvwxyz"

        ! Only one argument is given
          ! Valid
          call check(slice(test_string, first=10) == "jklmnopqrstuvwxyz", &
                    "slice, Valid arguments: first=10") ! last=+inf
          call check(slice(test_string, last=10) == "abcdefghij", &
                    "slice, Valid arguments: last=10") ! first=-inf
          call check(slice(test_string, stride=3) == "adgjmpsvy", &
                    "slice, Valid arguments: stride=3") ! first=-inf, last=+inf
          call check(slice(test_string, stride=-3) == "zwtqnkheb", &
                    "slice, Valid arguments: stride=-3") ! first=+inf, last=-inf

          ! Invalid
          call check(slice(test_string, first=27) == "", &
                    "slice, Invalid arguments: first=27") ! last=+inf
          call check(slice(test_string, first=-10) == "abcdefghijklmnopqrstuvwxyz", &
                    "slice, Invalid arguments: first=-10") ! last=+inf
          call check(slice(test_string, last=-2) == "", &
                    "slice, Invalid arguments: last=-2") ! first=-inf
          call check(slice(test_string, last=30) == "abcdefghijklmnopqrstuvwxyz", &
                    "slice, Invalid arguments: last=30") ! first=-inf
          call check(slice(test_string, stride=0) == "abcdefghijklmnopqrstuvwxyz", &
                    "slice, Invalid arguments: stride=0") ! stride=1
        
        ! Only two arguments are given
          ! Valid
          call check(slice(test_string, first=10, last=20) == "jklmnopqrst", &
                    "slice, Valid arguments: first=10, last=20")
          call check(slice(test_string, first=7, last=2) == "gfedcb", &
                    "slice, Valid arguments: first=7, last=2") ! stride=-1
          call check(slice(test_string, first=10, stride=-2) == "jhfdb", &
                    "slice, Valid arguments: first=10, stride=-2") ! last=-inf
          call check(slice(test_string, last=21, stride=-2) == "zxv", &
                    "slice, Valid arguments: last=21, stride=-2") ! first=+inf

          ! Atleast one argument is invalid
          call check(slice(test_string, first=30, last=-3) == "zyxwvutsrqponmlkjihgfedcba", &
                    "slice, Invalid arguments: first=30, last=-3")
          call check(slice(test_string, first=1, last=-20) == "a", &
                    "slice, Invalid arguments: first=1, last=-20")
          call check(slice(test_string, first=7, last=-10) == "gfedcba", &
                    "slice, Invalid arguments: first=7, last=-10")
          call check(slice(test_string, first=500, last=22) == "zyxwv", &
                    "slice, Invalid arguments: first=500, last=22")
          call check(slice(test_string, first=50, last=27) == "", &
                    "slice, Invalid arguments: first=50, last=27")
          call check(slice(test_string, first=-20, last=0) == "", &
                    "slice, Invalid arguments: first=-20, last=0")
          call check(slice(test_string, last=-3, stride=-2) == "zxvtrpnljhfdb", &
                    "slice, Invalid arguments: last=-3, stride=-2") ! first=+inf
          call check(slice(test_string, last=10, stride=0) == "abcdefghij", &
                    "slice, Invalid arguments: last=10, stride=0") ! stride=1
          call check(slice(test_string, first=-2, stride=-2) == "", &
                    "slice, Invalid arguments: first=-2, stride=-2") ! last=-inf
          call check(slice(test_string, first=27, stride=2) == "", &
                    "slice, Invalid arguments: first=27, stride=2") ! last=+inf
          call check(slice(test_string, last=27, stride=-1) == "", &
                    "slice, Invalid arguments: last=27, stride=-1") ! first=+inf

        ! All three arguments are given
          ! Valid
          call check(slice(test_string, first=2, last=16, stride=3) == "behkn", &
                    "slice, Valid arguments: first=2, last=16, stride=3")
          call check(slice(test_string, first=16, last=2, stride=-3) == "pmjgd", &
                    "slice, Valid arguments: first=16, last=2, stride=-3")
          call check(slice(test_string, first=7, last=7, stride=-4) == "g", &
                    "slice, Valid arguments: first=7, last=7, stride=-4")
          call check(slice(test_string, first=7, last=7, stride=3) == "g", &
                    "slice, Valid arguments: first=7, last=7, stride=3")
          call check(slice(test_string, first=2, last=6, stride=-1) == "", &
                    "slice, Valid arguments: first=2, last=6, stride=-1")
          call check(slice(test_string, first=20, last=10, stride=2) == "", &
                    "slice, Valid arguments: first=20, last=10, stride=2")

          ! Atleast one argument is invalid
          call check(slice(test_string, first=20, last=30, stride=2) == "tvxz", &
                    "slice, Invalid arguments: first=20, last=30, stride=2")
          call check(slice(test_string, first=-20, last=30, stride=2) == "acegikmoqsuwy", &
                    "slice, Invalid arguments: first=-20, last=30, stride=2")
          call check(slice(test_string, first=26, last=30, stride=1) == "z", &
                    "slice, Invalid arguments: first=26, last=30, stride=1")
          call check(slice(test_string, first=1, last=-20, stride=-1) == "a", &
                    "slice, Invalid arguments: first=1, last=-20, stride=-1")
          call check(slice(test_string, first=26, last=20, stride=1) == "", &
                    "slice, Invalid arguments: first=26, last=20, stride=1")
          call check(slice(test_string, first=1, last=20, stride=-1) == "", &
                    "slice, Invalid arguments: first=1, last=20, stride=-1")
        
        test_string = ""
        ! Empty string input
        call check(slice(test_string, first=-2, last=6) == "", &
                    "slice, Empty string: first=-2, last=6")
        call check(slice(test_string, first=6, last=-2) == "", &
                    "slice, Empty string: first=6, last=-2")
        call check(slice(test_string, first=-10) == "", &
                    "slice, Empty string: first=-10") ! last=+inf
        call check(slice(test_string, last=10) == "", &
                    "slice, Empty string: last=10") ! first=-inf
        call check(slice(test_string) == "", &
                    "slice, Empty string: no arguments provided")

    end subroutine test_slice_string

    subroutine test_find
        type(string_type) :: test_string_1, test_string_2, test_pattern_1, test_pattern_2
        test_string_1 = "qwqwqwqwqwqwqw"
        test_string_2 = "abccbabccbabc"
        test_pattern_1 = "qwq"
        test_pattern_2 = "abccbabc"

        call check(all(find([test_string_1, test_string_2], test_pattern_1, 4) == [7, 0]), &
            & 'find: [test_string_1, test_string_2], test_pattern_1, 4')
        call check(all(find(test_string_1, [test_pattern_1, test_pattern_2], 3, .false.) == [9, 0]), &
            & 'find: test_string_1, [test_pattern_1, test_pattern_2], 3, .false.')
        call check(find(test_string_1, test_pattern_1, 7) == 0, &
            & 'find: test_string_1, test_pattern_1, 7')
        call check(all(find([test_string_1, test_string_2, test_string_2], [test_pattern_1, &
            & test_pattern_2, test_pattern_2], [7, 2, 2], [.true., .false., .true.]) == [0, 0, 6]), &
            & 'find: [test_string_1, test_string_2, test_string_2], [test_pattern_1, &
            & test_pattern_2, test_pattern_2], [7, 2, 2], [.true., .false., .true.]')
        call check(find("qwqwqwqwqwqwqw", test_pattern_1) == 1, &
            & 'find: "qwqwqwqwqwqwqw", test_pattern_1')
        call check(all(find(test_string_1, ["qwq", "wqw"], 2) == [3, 4]), &
            & 'find: test_string_1, ["qwq", "wqw"], 2')
        call check(find("qwqwqwqwqwqwqw", "qwq", 2, .false.) == 5, &
            & 'find: "qwqwqwqwqwqwqw", "qwq", 2, .false.')
        call check(find("", "") == 0, &
            & 'find: "", ""')
        call check(find("", test_pattern_1) == 0, &
            & 'find: "", test_pattern_1')
        call check(find(test_string_1, "") == 0, &
            & 'find: test_string_1, ""')

    end subroutine test_find

    subroutine test_slice_gen
        character(len=*), parameter :: test = &
            & "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        integer :: i, j, k
        integer, parameter :: offset = 3

        do i = 1 - offset, len(test) + offset
            call check_slicer(test, first=i)
        end do

        do i = 1 - offset, len(test) + offset
            call check_slicer(test, last=i)
        end do

        do i = -len(test) - offset, len(test) + offset
            call check_slicer(test, stride=i)
        end do

        do i = 1 - offset, len(test) + offset
            do j = 1 - offset, len(test) + offset
                call check_slicer(test, first=i, last=j)
            end do
        end do

        do i = 1 - offset, len(test) + offset
            do j = -len(test) - offset, len(test) + offset
                call check_slicer(test, first=i, stride=j)
            end do
        end do

        do i = 1 - offset, len(test) + offset
            do j = -len(test) - offset, len(test) + offset
                call check_slicer(test, last=i, stride=j)
            end do
        end do

        do i = 1 - offset, len(test) + offset
            do j = 1 - offset, len(test) + offset
                do k = -len(test) - offset, len(test) + offset
                    call check_slicer(test, first=i, last=j, stride=k)
                end do
            end do
        end do
    end subroutine test_slice_gen

    subroutine check_slicer(string, first, last, stride)
        character(len=*), intent(in) :: string
        integer, intent(in), optional :: first
        integer, intent(in), optional :: last
        integer, intent(in), optional :: stride

        character(len=:), allocatable :: actual, expected, message
        logical :: stat

        actual = slice(string, first, last, stride)
        expected = reference_slice(string, first, last, stride)

        stat = actual == expected

        if (.not.stat) then
            message = "For input '"//string//"'"//new_line('a')

            if (present(first)) then
                message = message // "first: "//to_string(first)//new_line('a')
            end if
            if (present(last)) then
                message = message // "last: "//to_string(last)//new_line('a')
            end if
            if (present(stride)) then
                message = message // "stride: "//to_string(stride)//new_line('a')
            end if
            message = message // "Expected: '"//expected//"' but got '"//actual//"'"
        end if
        call check(stat, message)

    end subroutine check_slicer

    pure function reference_slice(string, first, last, stride) result(sliced_string)
        character(len=*), intent(in) :: string
        integer, intent(in), optional :: first
        integer, intent(in), optional :: last
        integer, intent(in), optional :: stride
        character(len=:), allocatable :: sliced_string
        character(len=1), allocatable :: carray(:)

        integer :: first_, last_, stride_

        stride_ = 1
        if (present(stride)) then
            stride_ = merge(stride_, stride, stride == 0)
        else
            if (present(first) .and. present(last)) then
                if (last < first) stride_ = -1
            end if
        end if

        if (stride_ < 0) then
            last_ = min(max(optval(last, 1), 1), len(string)+1)
            first_ = min(max(optval(first, len(string)), 0), len(string))
        else
            first_ = min(max(optval(first, 1), 1), len(string)+1)
            last_ = min(max(optval(last, len(string)), 0), len(string))
        end if

        carray = string_to_carray(string)
        carray = carray(first_:last_:stride_)
        sliced_string = carray_to_string(carray)

    end function reference_slice

    pure function string_to_carray(string) result(carray)
        character(len=*), intent(in) :: string
        character(len=1) :: carray(len(string))

        carray = transfer(string, carray)
    end function string_to_carray

    pure function carray_to_string(carray) result(string)
        character(len=1), intent(in) :: carray(:)
        character(len=size(carray)) :: string

        string = transfer(carray, string)
    end function carray_to_string

    subroutine test_replace_all
        type(string_type) :: test_string_1, test_pattern_1, test_replacement_1
        type(string_type) :: test_string_2, test_pattern_2, test_replacement_2
        test_string_1 = "mutate DNA sequence: GTTATCGTATGCCGTAATTAT"
        test_pattern_1 = "TAT"
        test_replacement_1 = "ATA"
        test_string_2 = "mutate DNA sequence: AGAGAGCCTAGAGAGAG"
        test_pattern_2 = "AGA"
        test_replacement_2 = "aga"

        ! all 3 as string_type
          call check(replace_all(test_string_1, test_pattern_1, test_replacement_1) == & 
                    & "mutate DNA sequence: GTATACGATAGCCGTAATATA", &
                    & "replace_all: all 3 string_type, test case 1")
          call check(replace_all(test_string_2, test_pattern_2, test_replacement_2) == &
                    & "mutate DNA sequence: agaGAGCCTagaGagaG", &
                    & "replace_all: all 3 string_type, test case 2")
          call check(replace_all(test_string_2, test_pattern_2, test_replacement_1) == &
                    & "mutate DNA sequence: ATAGAGCCTATAGATAG", &
                    & "replace_all: all 3 string_type, test case 3")
        
        ! 2 as string_type and 1 as character scalar
          call check(replace_all(test_string_1, "tat", test_replacement_1) == &
                    & "muATAe DNA sequence: GTTATCGTATGCCGTAATTAT", &
                    & "replace_all: 2 string_type & 1 character scalar, test case 1")
          call check(replace_all(test_string_2, test_pattern_2, "GC") == &
                    & "mutate DNA sequence: GCGAGCCTGCGGCG", &
                    & "replace_all: 2 string_type & 1 character scalar, test case 2")
          call check(replace_all("mutate DNA sequence: AGAGAGCCTAGAGAGAG", test_pattern_2, &
                    & test_replacement_2) == "mutate DNA sequence: agaGAGCCTagaGagaG", &
                    & "replace_all: 2 string_type & 1 character scalar, test case 3")

        
        ! 1 as string_type and 2 as character scalar
          call check(replace_all(test_string_1, "TAT", "ATA") == &
                    & "mutate DNA sequence: GTATACGATAGCCGTAATATA", &
                    & "replace_all: 1 string_type & 2 character scalar, test case 1")
          call check(replace_all("mutate DNA sequence: AGAGAGCCTAGAGAGAG", test_pattern_2, "GC") == &
                    & "mutate DNA sequence: GCGAGCCTGCGGCG", &
                    & "replace_all: 1 string_type & 2 character scalar, test case 2")
          call check(replace_all("mutate DNA sequence: GTTATCGTATGCCGTAATTAT", "TA", &
                    & test_replacement_2) == "mutate DNA sequence: GTagaTCGagaTGCCGagaATagaT", &
                    & "replace_all: 1 string_type & 2 character scalar, test case 3")
          call check(replace_all("mutate DNA sequence: GTTATCGTATGCCGTAATTAT", &
                    & test_pattern_1, "") == "mutate DNA sequence: GTCGGCCGTAAT", &
                    & "replace_all: 1 string_type & 2 character scalar, test case 4")
          call check(replace_all(test_string_1, "", "anything here") == test_string_1, &
                    & "replace_all: 1 string_type & 2 character scalar, test case 5")
          call check(replace_all("", test_pattern_2, "anything here") == "", &
                    & "replace_all: 1 string_type & 2 character scalar, test case 6")
        
        ! all 3 as character scalar
          call check(replace_all("mutate DNA sequence: GTTATCGTATGCCGTAATTAT", &
                    & "GT", "gct") == "mutate DNA sequence: gctTATCgctATGCCgctAATTAT", &
                    & "replace_all: all 3 character scalar, test case 1")
          call check(replace_all("", "anything here", "anything here") == "", &
                    & "replace_all: all 3 character scalar, test case 2")

    end subroutine test_replace_all

    subroutine test_padl
        type(string_type) :: test_string
        character(len=:), allocatable :: test_char

        test_string = "left pad this string"
        test_char = "  left pad this string  "

      ! output_length > len(string)
        call check(padl(test_string, 25, "#") == "#####left pad this string", &
                    & 'padl: output_length > len(string), test_case 1')
        call check(padl(test_string, 22, "$") == "$$left pad this string", &
                    & 'padl: output_length > len(string), test_case 2')
        call check(padl(test_string, 23) == "   left pad this string", &
                    & 'padl: output_length > len(string), test_case 3')
        call check(padl(test_char, 26) == "    left pad this string  ", &
                    & 'padl: output_length > len(string), test_case 4')
        call check(padl(test_char, 26, "&") == "&&  left pad this string  ", &
                    & 'padl: output_length > len(string), test_case 5')
        call check(padl("", 10, "!") == "!!!!!!!!!!", &
                    & 'padl: output_length > len(string), test_case 6')

      ! output_length <= len(string)
        call check(padl(test_string, 18, "#") == "left pad this string", &
                    & 'padl: output_length <= len(string), test_case 1')
        call check(padl(test_string, -4, "@") == "left pad this string", &
                    & 'padl: output_length <= len(string), test_case 2')
        call check(padl(test_char, 20, "0") == "  left pad this string  ", &
                    & 'padl: output_length <= len(string), test_case 3')
        call check(padl(test_char, 17) == "  left pad this string  ", &
                    & 'padl: output_length <= len(string), test_case 4')
        call check(padl("", 0, "!") == "", &
                    & 'padl: output_length <= len(string), test_case 5')
        call check(padl("", -12, "!") == "", &
                    & 'padl: output_length <= len(string), test_case 6')
    
    end subroutine test_padl

    subroutine test_padr
        type(string_type) :: test_string
        character(len=:), allocatable :: test_char

        test_string = "right pad this string"
        test_char = "  right pad this string  "

      ! output_length > len(string)
        call check(padr(test_string, 25, "#") == "right pad this string####", &
                    & 'padr: output_length > len(string), test_case 1')
        call check(padr(test_string, 22, "$") == "right pad this string$", &
                    & 'padr: output_length > len(string), test_case 2')
        call check(padr(test_string, 24) == "right pad this string   ", &
                    & 'padr: output_length > len(string), test_case 3')
        call check(padr(test_char, 27) == "  right pad this string    ", &
                    & 'padr: output_length > len(string), test_case 4')
        call check(padr(test_char, 27, "&") == "  right pad this string  &&", &
                    & 'padr: output_length > len(string), test_case 5')
        call check(padr("", 10, "!") == "!!!!!!!!!!", &
                    & 'padr: output_length > len(string), test_case 6')

      ! output_length <= len(string)
        call check(padr(test_string, 18, "#") == "right pad this string", &
                    & 'padr: output_length <= len(string), test_case 1')
        call check(padr(test_string, -4, "@") == "right pad this string", &
                    & 'padr: output_length <= len(string), test_case 2')
        call check(padr(test_char, 20, "0") == "  right pad this string  ", &
                    & 'padr: output_length <= len(string), test_case 3')
        call check(padr(test_char, 17) == "  right pad this string  ", &
                    & 'padr: output_length <= len(string), test_case 4')
        call check(padr("", 0, "!") == "", &
                    & 'padr: output_length <= len(string), test_case 5')
        call check(padr("", -12, "!") == "", &
                    & 'padr: output_length <= len(string), test_case 6')
    
    end subroutine test_padr

    subroutine test_count
        type(string_type) :: test_string_1, test_string_2, test_pattern_1, test_pattern_2
        test_string_1 = "DNA sequence: AGAGAGAGTCCTGTCGAGA"
        test_string_2 = "DNA sequence: GTCCTGTCCTGTCAGA"
        test_pattern_1 = "AGA"
        test_pattern_2 = "GTCCTGTC"

        ! all 2 as string_type
          call check(all(count([test_string_1, test_string_2], test_pattern_1) == [4, 1]), &
                & 'count: all 2 as string_type, test case 1')
          call check(all(count(test_string_1, [test_pattern_1, test_pattern_2], .false.) == [3, 1]), &
                & 'count: all 2 as string_type, test case 2')
          call check(count(test_string_2, test_pattern_1, .false.) == 1, &
                & 'count: all 2 as string_type, test case 3')
          call check(all(count([test_string_2, test_string_2, test_string_1], &
                & [test_pattern_2, test_pattern_2, test_pattern_1], [.true., .false., .false.]) == &
                & [2, 1, 3]), 'count: all 2 as string_type, test case 4')
          call check(all(count([[test_string_1, test_string_2], [test_string_1, test_string_2]], &
                & [[test_pattern_1, test_pattern_2], [test_pattern_2, test_pattern_1]], .true.) == &
                & [[4, 2], [1, 1]]), 'count: all 2 as string_type, test case 5')
        
        ! 1 string_type and 1 character scalar
          call check(all(count(test_string_1, ["AGA", "GTC"], [.true., .false.]) == [4, 2]), &
                & 'count: 1 string_type and 1 character scalar, test case 1')
          call check(all(count([test_string_1, test_string_2], ["CTC", "GTC"], [.true., .false.]) == &
                & [0, 3]), 'count: 1 string_type and 1 character scalar, test case 2')
          call check(all(count(["AGAGAGAGTCCTGTCGAGA", "AGAGAGAGTCCTGTCGAGA"], &
                & test_pattern_1, [.false., .true.]) == [3, 4]), &
                & 'count: 1 string_type and 1 character scalar, test case 3')
          call check(count(test_string_1, "GAG") == 4, &
                & 'count: 1 string_type and 1 character scalar, test case 4')
          call check(count("DNA sequence: GTCCTGTCCTGTCAGA", test_pattern_2, .false.) == 1, &
                & 'count: 1 string_type and 1 character scalar, test case 5')
        
        ! all 2 character scalar
          call check(all(count("", ["mango", "trees"], .true.) == [0, 0]), &
                & 'count: all 2 character scalar, test case 1')
          call check(count("", "", .true.) == 0, 'count: all 2 character scalar, test case 2')
          call check(all(count(["mango", "trees"], "", .true.) == [0, 0]), &
                & 'count: all 2 character scalar, test case 3')

    end subroutine test_count

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
    call test_slice_gen
    call test_find
    call test_replace_all
    call test_padl
    call test_padr
    call test_count

end program tester
