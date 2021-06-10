! SPDX-Identifier: MIT
module test_string_functions
    use, intrinsic :: iso_fortran_env, only : error_unit
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), operator(==), &
                                    to_lower, to_upper, to_title, to_sentence, reverse
    use stdlib_strings, only: slice                                    
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
        character(len=:), allocatable :: test_char
        test_string = "abcdefghijklmnopqrstuvwxyz"
        test_char = "abcdefghijklmnopqrstuvwxyz"

        call check(slice(test_string, 2, 16, 3) == "behkn", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, first=15, stride=-1) == "onmlkjihgfedcba", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, last=22, stride=-1) == "zyxwv", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, 7, 2) == "gfedcb", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, 7, 2, 1) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, 2, 6, -1) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, stride=-1) == "zyxwvutsrqponmlkjihgfedcba", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, 7, 7, -4) == "g", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, 7, 7, 3) == "g", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, 7, 7, 3) == "g", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, 7, -10) == "gfedcba", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, 500, 22) == "zyxwv", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, 50, 27) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, -20, -200) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, first=0, stride=-1) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, last=27, stride=-2) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, first=27, stride=2) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_string, -500, 500) == "abcdefghijklmnopqrstuvwxyz", &
                    'function slice failed', warn=.false.)

        test_string = ""
        test_char = ""
        call check(slice(test_string, 2, 16, 3) == "", &
                    'function slice failed', warn=.false.)
        call check(slice(test_char, 2, 16, 3) == "", &
                    'function slice failed', warn=.false.)

    end subroutine test_slice_string

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

end program tester
