! SPDX-Identifer: MIT
module test_string_operator
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_string_type, only : string_type, assignment(=), len, &
        operator(>), operator(<), operator(>=), operator(<=), &
        operator(/=), operator(==), operator(//)
    implicit none

contains


    !> Collect all exported unit tests
    subroutine collect_string_operator(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("gt", test_gt), &
            new_unittest("lt", test_lt), &
            new_unittest("ge", test_ge), &
            new_unittest("le", test_le), &
            new_unittest("eq", test_eq), &
            new_unittest("ne", test_ne), &
            new_unittest("concat", test_concat) &
            ]
    end subroutine collect_string_operator

    subroutine test_gt(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string > "abc"
        call check(error, res .eqv. .true.)
        if (allocated(error)) return

        res = string > "bcd"
        call check(error, res .eqv. .false.)
        if (allocated(error)) return

        res = string > "cde"
        call check(error, res .eqv. .false.)
    end subroutine test_gt

    subroutine test_lt(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string < "abc"
        call check(error, res .eqv. .false.)
        if (allocated(error)) return

        res = string < "bcd"
        call check(error, res .eqv. .false.)
        if (allocated(error)) return

        res = string < "cde"
        call check(error, res .eqv. .true.)
    end subroutine test_lt

    subroutine test_ge(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string >= "abc"
        call check(error, res .eqv. .true.)
        if (allocated(error)) return

        res = string >= "bcd"
        call check(error, res .eqv. .true.)
        if (allocated(error)) return

        res = string >= "cde"
        call check(error, res .eqv. .false.)
    end subroutine test_ge

    subroutine test_le(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string <= "abc"
        call check(error, res .eqv. .false.)
        if (allocated(error)) return

        res = string <= "bcd"
        call check(error, res .eqv. .true.)
        if (allocated(error)) return

        res = string <= "cde"
        call check(error, res .eqv. .true.)
    end subroutine test_le

    subroutine test_eq(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string == "abc"
        call check(error, res .eqv. .false.)
        if (allocated(error)) return

        res = string == "bcd"
        call check(error, res .eqv. .true.)
        if (allocated(error)) return

        res = string == "cde"
        call check(error, res .eqv. .false.)
    end subroutine test_eq

    subroutine test_ne(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string
        logical :: res

        string = "bcd"
        res = string /= "abc"
        call check(error, res .eqv. .true.)
        if (allocated(error)) return

        res = string /= "bcd"
        call check(error, res .eqv. .false.)
        if (allocated(error)) return

        res = string /= "cde"
        call check(error, res .eqv. .true.)
    end subroutine test_ne

    subroutine test_concat(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        type(string_type) :: string

        string = "Hello, "
        string = string // "World!"
        call check(error, len(string) == 13)
    end subroutine test_concat

end module test_string_operator


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_string_operator, only : collect_string_operator
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("string-operator", collect_string_operator) &
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
