
module test_loadtxt_qp
    use stdlib_kinds, only: qp
    use stdlib_io, only: loadtxt, savetxt
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    implicit none

    private
    public :: collect_loadtxt_qp
contains

    !> Collect all exported unit tests
    subroutine collect_loadtxt_qp(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("loadtxt_qp", test_loadtxt_qp_), &
            new_unittest("loadtxt_qp_huge", test_loadtxt_qp_huge), &
            new_unittest("loadtxt_qp_tiny", test_loadtxt_qp_tiny) &
        ]

    end subroutine collect_loadtxt_qp


    subroutine test_loadtxt_qp_(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "Quadruple precision is not enabled")

    end subroutine test_loadtxt_qp_


    subroutine test_loadtxt_qp_huge(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "Quadruple precision is not enabled")

    end subroutine test_loadtxt_qp_huge


    subroutine test_loadtxt_qp_tiny(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "Quadruple precision is not enabled")

    end subroutine test_loadtxt_qp_tiny

end module test_loadtxt_qp


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_loadtxt_qp, only : collect_loadtxt_qp
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("loadtxt_qp", collect_loadtxt_qp) &
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
