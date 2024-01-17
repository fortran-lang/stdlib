
module test_savetxt_qp
    use stdlib_kinds, only: qp
    use stdlib_io, only: loadtxt, savetxt
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    implicit none

    private
    public :: collect_savetxt_qp
contains

    !> Collect all exported unit tests
    subroutine collect_savetxt_qp(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rqp", test_rqp), &
            new_unittest("cqp", test_cqp) &
        ]

    end subroutine collect_savetxt_qp


    function get_outpath() result(outpath)
        integer :: ierr
        character(256) :: argv
        character(:), allocatable :: outpath

        call get_command_argument(1, argv, status=ierr)
        if (ierr == 0) then
            outpath = trim(argv)
        else
            outpath = '.'
        end if
    end function get_outpath


    subroutine test_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_rqp


    subroutine test_cqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_cqp

end module test_savetxt_qp


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_savetxt_qp, only : collect_savetxt_qp
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("savetxt_qp", collect_savetxt_qp) &
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
