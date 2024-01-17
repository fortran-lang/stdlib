! SPDX-Identifier: MIT
module test_string_assignment
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds, only : int8, int16, int32, int64, lk, c_bool
    use stdlib_string_type, only : string_type, assignment(=), operator(==), len
    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_string_assignment(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("assignment", test_assignment), &
            new_unittest("constructor", test_constructor) &
            ]
    end subroutine collect_string_assignment

    subroutine test_assignment(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(string_type) :: string

        call check(error, len(string) == 0)
        if (allocated(error)) return

        string = "Sequence"
        call check(error, len(string) == 8)
    end subroutine test_assignment

    subroutine test_constructor(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=128) :: flc

        write(flc, '(g0)') -1026191
        call check(error, string_type(-1026191) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') 124787
        call check(error, string_type(124787) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') -2_int8
        call check(error, string_type(-2_int8) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') 5_int8
        call check(error, string_type(5_int8) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') -72_int16
        call check(error, string_type(-72_int16) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') -8924889_int32
        call check(error, string_type(-8924889_int32) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') 2378405_int32
        call check(error, string_type(2378405_int32) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') 921092378411_int64
        call check(error, string_type(921092378411_int64) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') -1272835761_int64
        call check(error, string_type(-1272835761_int64) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') .true.
        call check(error, string_type(.true.) == trim(flc))
        if (allocated(error)) return

        write(flc, '(g0)') .false.
        call check(error, string_type(.false.) == trim(flc))
        if (allocated(error)) return


        write(flc, '(g0)') .true._lk
        call check(error, string_type(.true._lk) == trim(flc))
    end subroutine test_constructor

end module test_string_assignment


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_string_assignment, only : collect_string_assignment
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("string-assignment", collect_string_assignment) &
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
