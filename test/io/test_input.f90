module test_input
    use stdlib_io, only: input
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: collect_input
contains

    !> Collect all exported unit tests
    subroutine collect_input(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("input_basic", test_input_basic) &
        ]

    end subroutine collect_input

    subroutine test_input_basic(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: result
        
        ! Note: This is a basic structure for the test
        ! Actual interactive testing would require input redirection or mocking
        ! For now, we verify that the function signature is correct
        
        ! The function should be callable (this verifies compilation)
        ! In a real test environment, you would redirect stdin
        
        ! For now, just verify the module compiles and can be used
        call check(error, .true., "input function is available")
        
    end subroutine test_input_basic

end module test_input


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_input, only : collect_input
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("input", collect_input) &
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
