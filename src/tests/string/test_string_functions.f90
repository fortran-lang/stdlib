! SPDX-Identifier: MIT
module test_string_functions
    use, intrinsic :: iso_fortran_env, only : error_unit
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), operator(==), &
                                    to_lower, to_upper, to_title, to_sentence, reverse
    use stdlib_strings, only: slice, find                                    
    use stdlib_optval, only: optval
    use stdlib_ascii, only : to_string
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
                    "Slice, Valid arguments: first=10") ! last=+inf
          call check(slice(test_string, last=10) == "abcdefghij", &
                    "Slice, Valid arguments: last=10") ! first=-inf
          call check(slice(test_string, stride=3) == "adgjmpsvy", &
                    "Slice, Valid arguments: stride=3") ! first=-inf, last=+inf
          call check(slice(test_string, stride=-3) == "zwtqnkheb", &
                    "Slice, Valid arguments: stride=-3") ! first=+inf, last=-inf

          ! Invalid
          call check(slice(test_string, first=27) == "", &
                    "Slice, Invalid arguments: first=27") ! last=+inf
          call check(slice(test_string, first=-10) == "abcdefghijklmnopqrstuvwxyz", &
                    "Slice, Invalid arguments: first=-10") ! last=+inf
          call check(slice(test_string, last=-2) == "", &
                    "Slice, Invalid arguments: last=-2") ! first=-inf
          call check(slice(test_string, last=30) == "abcdefghijklmnopqrstuvwxyz", &
                    "Slice, Invalid arguments: last=30") ! first=-inf
          call check(slice(test_string, stride=0) == "abcdefghijklmnopqrstuvwxyz", &
                    "Slice, Invalid arguments: stride=0") ! stride=1
        
        ! Only two arguments are given
          ! Valid
          call check(slice(test_string, first=10, last=20) == "jklmnopqrst", &
                    "Slice, Valid arguments: first=10, last=20")
          call check(slice(test_string, first=7, last=2) == "gfedcb", &
                    "Slice, Valid arguments: first=7, last=2") ! stride=-1
          call check(slice(test_string, first=10, stride=-2) == "jhfdb", &
                    "Slice, Valid arguments: first=10, stride=-2") ! last=-inf
          call check(slice(test_string, last=21, stride=-2) == "zxv", &
                    "Slice, Valid arguments: last=21, stride=-2") ! first=+inf

          ! Atleast one argument is invalid
          call check(slice(test_string, first=30, last=-3) == "zyxwvutsrqponmlkjihgfedcba", &
                    "Slice, Invalid arguments: first=30, last=-3")
          call check(slice(test_string, first=1, last=-20) == "a", &
                    "Slice, Invalid arguments: first=1, last=-20")
          call check(slice(test_string, first=7, last=-10) == "gfedcba", &
                    "Slice, Invalid arguments: first=7, last=-10")
          call check(slice(test_string, first=500, last=22) == "zyxwv", &
                    "Slice, Invalid arguments: first=500, last=22")
          call check(slice(test_string, first=50, last=27) == "", &
                    "Slice, Invalid arguments: first=50, last=27")
          call check(slice(test_string, first=-20, last=0) == "", &
                    "Slice, Invalid arguments: first=-20, last=0")
          call check(slice(test_string, last=-3, stride=-2) == "zxvtrpnljhfdb", &
                    "Slice, Invalid arguments: last=-3, stride=-2") ! first=+inf
          call check(slice(test_string, last=10, stride=0) == "abcdefghij", &
                    "Slice, Invalid arguments: last=10, stride=0") ! stride=1
          call check(slice(test_string, first=-2, stride=-2) == "", &
                    "Slice, Invalid arguments: first=-2, stride=-2") ! last=-inf
          call check(slice(test_string, first=27, stride=2) == "", &
                    "Slice, Invalid arguments: first=27, stride=2") ! last=+inf
          call check(slice(test_string, last=27, stride=-1) == "", &
                    "Slice, Invalid arguments: last=27, stride=-1") ! first=+inf

        ! All three arguments are given
          ! Valid
          call check(slice(test_string, first=2, last=16, stride=3) == "behkn", &
                    "Slice, Valid arguments: first=2, last=16, stride=3")
          call check(slice(test_string, first=16, last=2, stride=-3) == "pmjgd", &
                    "Slice, Valid arguments: first=16, last=2, stride=-3")
          call check(slice(test_string, first=7, last=7, stride=-4) == "g", &
                    "Slice, Valid arguments: first=7, last=7, stride=-4")
          call check(slice(test_string, first=7, last=7, stride=3) == "g", &
                    "Slice, Valid arguments: first=7, last=7, stride=3")
          call check(slice(test_string, first=2, last=6, stride=-1) == "", &
                    "Slice, Valid arguments: first=2, last=6, stride=-1")
          call check(slice(test_string, first=20, last=10, stride=2) == "", &
                    "Slice, Valid arguments: first=20, last=10, stride=2")

          ! Atleast one argument is invalid
          call check(slice(test_string, first=20, last=30, stride=2) == "tvxz", &
                    "Slice, Invalid arguments: first=20, last=30, stride=2")
          call check(slice(test_string, first=-20, last=30, stride=2) == "acegikmoqsuwy", &
                    "Slice, Invalid arguments: first=-20, last=30, stride=2")
          call check(slice(test_string, first=26, last=30, stride=1) == "z", &
                    "Slice, Invalid arguments: first=26, last=30, stride=1")
          call check(slice(test_string, first=1, last=-20, stride=-1) == "a", &
                    "Slice, Invalid arguments: first=1, last=-20, stride=-1")
          call check(slice(test_string, first=26, last=20, stride=1) == "", &
                    "Slice, Invalid arguments: first=26, last=20, stride=1")
          call check(slice(test_string, first=1, last=20, stride=-1) == "", &
                    "Slice, Invalid arguments: first=1, last=20, stride=-1")
        
        test_string = ""
        ! Empty string input
        call check(slice(test_string, first=-2, last=6) == "", &
                    "Slice, Empty string: first=-2, last=6")
        call check(slice(test_string, first=6, last=-2) == "", &
                    "Slice, Empty string: first=6, last=-2")
        call check(slice(test_string, first=-10) == "", &
                    "Slice, Empty string: first=-10") ! last=+inf
        call check(slice(test_string, last=10) == "", &
                    "Slice, Empty string: last=10") ! first=-inf
        call check(slice(test_string) == "", &
                    "Slice, Empty string: no arguments provided")

    end subroutine test_slice_string

    subroutine test_find
        type(string_type) :: test_string_1, test_string_2, test_pattern_1, test_pattern_2
        test_string_1 = "qwqwqwqwqwqwqw"
        test_string_2 = "abccbabccbabc"
        test_pattern_1 = "qwq"
        test_pattern_2 = "abccbabc"

        call check(all(find([test_string_1, test_string_2], test_pattern_1, 4) == [7, 0]), &
            & 'Find: [test_string_1, test_string_2], test_pattern_1, 4')
        call check(all(find(test_string_1, [test_pattern_1, test_pattern_2], 3, .false.) == [9, 0]), &
            & 'Find: test_string_1, [test_pattern_1, test_pattern_2], 3, .false.')
        call check(find(test_string_1, test_pattern_1, 7) == 0, &
            & 'Find: test_string_1, test_pattern_1, 7')
        call check(all(find([test_string_1, test_string_2, test_string_2], [test_pattern_1, &
            & test_pattern_2, test_pattern_2], [7, 2, 2], [.true., .false., .true.]) == [0, 0, 6]), &
            & 'Find: [test_string_1, test_string_2, test_string_2], [test_pattern_1, &
            & test_pattern_2, test_pattern_2], [7, 2, 2], [.true., .false., .true.]')
        call check(find("qwqwqwqwqwqwqw", test_pattern_1) == 1, &
            & 'Find: "qwqwqwqwqwqwqw", test_pattern_1')
        call check(all(find(test_string_1, ["qwq", "wqw"], 2) == [3, 4]), &
            & 'Find: test_string_1, ["qwq", "wqw"], 2')
        call check(find("qwqwqwqwqwqwqw", "qwq", 2, .false.) == 5, &
            & 'Find: "qwqwqwqwqwqwqw", "qwq", 2, .false.')
        call check(find("", "") == 0, &
            & 'Find: "", ""')
        call check(find("", test_pattern_1) == 0, &
            & 'Find: "", test_pattern_1')
        call check(find(test_string_1, "") == 0, &
            & 'Find: test_string_1, ""')

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

end program tester
