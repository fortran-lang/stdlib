! SPDX-Identifier: MIT

module test_math_arange
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_math, only: arange
    implicit none

    public :: collect_math_arange

contains

    !> Collect all exported unit tests
    subroutine collect_math_arange(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("arange-real", test_math_arange_real), &
            new_unittest("arange-integer", test_math_arange_integer) &
            ]

    end subroutine collect_math_arange

    subroutine test_math_arange_real(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        ! Normal
        call check(error, all(arange(3.0) == [1.0, 2.0, 3.0]),        "all(arange(3.0) == [1.0,2.0,3.0]) failed.")
        call check(error, all(arange(-1.0) == [1.0, 0.0, -1.0]),      "all(arange(-1.0) == [1.0,0.0,-1.0]) failed.")
        call check(error, all(arange(0.0, 2.0) == [0.0, 1.0, 2.0]),   "all(arange(0.0,2.0) == [0.0,1.0,2.0]) failed.")
        call check(error, all(arange(1.0, -1.0) == [1.0, 0.0, -1.0]), "all(arange(1.0,-1.0) == [1.0,0.0,-1.0]) failed.")
        call check(error, all(arange(1.0, 1.0) == [1.0]),             "all(arange(1.0,1.0) == [1.0]) failed.")
        call check(error, all(arange(0.0, 2.0, 2.0) == [0.0, 2.0]),   "all(arange(0.0,2.0,2.0) == [0.0,2.0]) failed.")
        call check(error, all(arange(1.0, -1.0, 2.0) == [1.0, -1.0]), "all(arange(1.0,-1.0,2.0) == [1.0,-1.0]) failed.")
        ! Not recommended
        call check(error, all(arange(0.0, 2.0, -2.0) == [0.0, 2.0]),  "all(arange(0.0,2.0,-2.0) == [0.0,2.0]) failed.")
        call check(error, all(arange(1.0, -1.0, -2.0) == [1.0, -1.0]),"all(arange(1.0,-1.0,-2.0) == [1.0,-1.0]) failed.")
        call check(error, all(arange(0.0, 2.0, 0.0) == [0.0,1.0,2.0]),"all(arange(0.0, 2.0, 0.0) == [0.0,1.0,2.0]) failed.")
    end subroutine test_math_arange_real

    subroutine test_math_arange_integer(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        ! Normal
        call check(error, all(arange(3) == [1, 2, 3]),        "all(arange(3) == [1,2,3]) failed.")
        call check(error, all(arange(-1) == [1, 0, -1]),      "all(arange(-1) == [1,0,-1]) failed.")
        call check(error, all(arange(0, 2) == [0, 1, 2]),     "all(arange(0,2) == [0,1,2]) failed.")
        call check(error, all(arange(1, -1) == [1, 0, -1]),   "all(arange(1,-1) == [1,0,-1]) failed.")
        call check(error, all(arange(1, 1) == [1]),           "all(arange(1,1) == [1]) failed.")
        call check(error, all(arange(0, 2, 2) == [0, 2]),     "all(arange(0,2,2) == [0,2]) failed.")
        call check(error, all(arange(1, -1, 2) == [1, -1]),   "all(arange(1,-1,2) == [1,-1]) failed.")
        ! Not recommended
        call check(error, all(arange(0, 2, -2) == [0, 2]),    "all(arange(0,2,-2) == [0,2]) failed.")
        call check(error, all(arange(1, -1, -2) == [1, -1]),  "all(arange(1,-1,-2) == [1,-1]) failed.")
        call check(error, all(arange(0, 2, 0) == [0,1,2]),    "all(arange(0, 2, 0) == [0,1,2]) failed.")
    end subroutine test_math_arange_integer

end module test_math_arange

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_math_arange, only : collect_math_arange
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("math-arange", collect_math_arange) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
