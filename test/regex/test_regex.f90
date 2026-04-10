module test_regex_mod
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_regex, only : regex_type, regcomp, regmatch
    implicit none
    private

    public :: collect_regex

contains

    !> Collect all exported unit tests
    subroutine collect_regex(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("literal_match", test_literal_match), &
            new_unittest("literal_no_match", test_literal_no_match), &
            new_unittest("star_operator", test_star_operator), &
            new_unittest("plus_char_class", test_plus_char_class), &
            new_unittest("alternation_grouping", test_alternation_grouping), &
            new_unittest("anchor_start_fail", test_anchor_start_fail), &
            new_unittest("anchor_start_pass", test_anchor_start_pass), &
            new_unittest("dot_any", test_dot_any), &
            new_unittest("question_mark", test_question_mark), &
            new_unittest("empty_pattern", test_empty_pattern) &
            ]
    end subroutine collect_regex

    subroutine test_literal_match(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat, ms, me
        logical :: found

        call regcomp(re, "abc", stat)
        call check(error, stat == 0, "regcomp failed for 'abc'")
        if (allocated(error)) return

        call regmatch(re, "xyz_abc_def", found, ms, me)
        call check(error, found, "Should find 'abc' in 'xyz_abc_def'")
        if (allocated(error)) return

        call check(error, ms == 5, "match_start should be 5")
        if (allocated(error)) return

        call check(error, me == 7, "match_end should be 7")
        if (allocated(error)) return
    end subroutine test_literal_match

    subroutine test_literal_no_match(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "xyz", stat)
        call check(error, stat == 0, "regcomp failed for 'xyz'")
        if (allocated(error)) return

        call regmatch(re, "abcdef", found)
        call check(error, .not. found, "Should not find 'xyz' in 'abcdef'")
        if (allocated(error)) return
    end subroutine test_literal_no_match

    subroutine test_star_operator(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "a*b", stat)
        call check(error, stat == 0, "regcomp failed for 'a*b'")
        if (allocated(error)) return

        call regmatch(re, "aaaab", found)
        call check(error, found, "Should match 'aaaab' with 'a*b'")
        if (allocated(error)) return

        call regmatch(re, "b", found)
        call check(error, found, "Should match 'b' with 'a*b' (zero a's)")
        if (allocated(error)) return
    end subroutine test_star_operator

    subroutine test_plus_char_class(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat, ms, me
        logical :: found

        call regcomp(re, "[0-9]+", stat)
        call check(error, stat == 0, "regcomp failed for '[0-9]+'")
        if (allocated(error)) return

        call regmatch(re, "foo123bar", found, ms, me)
        call check(error, found, "Should find digits in 'foo123bar'")
        if (allocated(error)) return

        call regmatch(re, "no_digits_here", found)
        call check(error, .not. found, "Should not find digits in 'no_digits_here'")
        if (allocated(error)) return
    end subroutine test_plus_char_class

    subroutine test_alternation_grouping(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "(dog|cat)s?", stat)
        call check(error, stat == 0, "regcomp failed for '(dog|cat)s?'")
        if (allocated(error)) return

        call regmatch(re, "I have cats and dogs.", found)
        call check(error, found, "Should find 'cats' or 'dogs' in sentence")
        if (allocated(error)) return

        call regmatch(re, "I have birds.", found)
        call check(error, .not. found, "Should not find 'cat' or 'dog' in 'I have birds.'")
        if (allocated(error)) return
    end subroutine test_alternation_grouping

    subroutine test_anchor_start_fail(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "^foo", stat)
        call check(error, stat == 0, "regcomp failed for '^foo'")
        if (allocated(error)) return

        call regmatch(re, "bar foo", found)
        call check(error, .not. found, "'^foo' should not match 'bar foo'")
        if (allocated(error)) return
    end subroutine test_anchor_start_fail

    subroutine test_anchor_start_pass(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "^foo", stat)
        call check(error, stat == 0, "regcomp failed for '^foo'")
        if (allocated(error)) return

        call regmatch(re, "foo bar", found)
        call check(error, found, "'^foo' should match 'foo bar'")
        if (allocated(error)) return
    end subroutine test_anchor_start_pass

    subroutine test_dot_any(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "a.c", stat)
        call check(error, stat == 0, "regcomp failed for 'a.c'")
        if (allocated(error)) return

        call regmatch(re, "abc", found)
        call check(error, found, "'a.c' should match 'abc'")
        if (allocated(error)) return

        call regmatch(re, "aXc", found)
        call check(error, found, "'a.c' should match 'aXc'")
        if (allocated(error)) return

        call regmatch(re, "ac", found)
        call check(error, .not. found, "'a.c' should not match 'ac'")
        if (allocated(error)) return
    end subroutine test_dot_any

    subroutine test_question_mark(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "colou?r", stat)
        call check(error, stat == 0, "regcomp failed for 'colou?r'")
        if (allocated(error)) return

        call regmatch(re, "color", found)
        call check(error, found, "'colou?r' should match 'color'")
        if (allocated(error)) return

        call regmatch(re, "colour", found)
        call check(error, found, "'colou?r' should match 'colour'")
        if (allocated(error)) return
    end subroutine test_question_mark

    subroutine test_empty_pattern(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(regex_type) :: re
        integer :: stat
        logical :: found

        call regcomp(re, "", stat)
        call check(error, stat == 0, "regcomp should succeed for empty pattern")
        if (allocated(error)) return

        call regmatch(re, "anything", found)
        call check(error, found, "Empty pattern should match any string")
        if (allocated(error)) return
    end subroutine test_empty_pattern

end module test_regex_mod


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_regex_mod, only : collect_regex
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("regex", collect_regex) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
