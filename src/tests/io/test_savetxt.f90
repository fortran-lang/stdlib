module test_savetxt
    use stdlib_kinds, only: int32, sp, dp
    use stdlib_io, only: loadtxt, savetxt
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: collect_savetxt
contains

    !> Collect all exported unit tests
    subroutine collect_savetxt(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("iint32", test_iint32), &
            new_unittest("rsp", test_rsp), &
            new_unittest("rdp", test_rdp), &
            new_unittest("csp", test_csp), &
            new_unittest("cdp", test_cdp) &
        ]

    end subroutine collect_savetxt


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


    subroutine test_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int32) :: d(3, 2), e(2, 3)
        integer(int32), allocatable :: d2(:, :)
        character(:), allocatable :: outpath

        outpath = get_outpath() // "/tmp_test_iint32.dat"

        d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call savetxt(outpath, d)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [3, 2]))
        if (allocated(error)) return
        call check(error, all(d == d2))
        if (allocated(error)) return

        e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call savetxt(outpath, e)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [2, 3]))
        if (allocated(error)) return
        call check(error, all(e == d2))
        if (allocated(error)) return
    end subroutine


    subroutine test_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp) :: d(3, 2), e(2, 3)
        real(sp), allocatable :: d2(:, :)
        character(:), allocatable :: outpath

        outpath = get_outpath() // "/tmp_test_rsp.dat"

        d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call savetxt(outpath, d)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [3, 2]))
        if (allocated(error)) return
        call check(error, all(d == d2))
        if (allocated(error)) return

        e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call savetxt(outpath, e)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [2, 3]))
        if (allocated(error)) return
        call check(error, all(e == d2))
        if (allocated(error)) return
    end subroutine test_rsp


    subroutine test_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp) :: d(3, 2), e(2, 3)
        real(dp), allocatable :: d2(:, :)
        character(:), allocatable :: outpath

        outpath = get_outpath() // "/tmp_test_rdp.dat"

        d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call savetxt(outpath, d)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [3, 2]))
        if (allocated(error)) return
        call check(error, all(d == d2))
        if (allocated(error)) return

        e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call savetxt(outpath, e)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [2, 3]))
        if (allocated(error)) return
        call check(error, all(e == d2))
        if (allocated(error)) return
    end subroutine test_rdp


    subroutine test_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        complex(sp) :: d(3, 2), e(2, 3)
        complex(sp), allocatable :: d2(:, :)
        character(:), allocatable :: outpath

        outpath = get_outpath() // "/tmp_test_csp.dat"

        d = cmplx(1, 1,kind=sp)* reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call savetxt(outpath, d)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [3, 2]))
        if (allocated(error)) return
        call check(error, all(d == d2))
        if (allocated(error)) return

        e = cmplx(1, 1,kind=sp)* reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call savetxt(outpath, e)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [2, 3]))
        if (allocated(error)) return
        call check(error, all(e == d2))
        if (allocated(error)) return
    end subroutine test_csp


    subroutine test_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        complex(dp) :: d(3, 2), e(2, 3)
        complex(dp), allocatable :: d2(:, :)
        character(:), allocatable :: outpath

        outpath = get_outpath() // "/tmp_test_cdp.dat"

        d = cmplx(1._dp, 1._dp,kind=dp)* reshape([1, 2, 3, 4, 5, 6], [3, 2])
        call savetxt(outpath, d)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [3, 2]))
        if (allocated(error)) return
        call check(error, all(d == d2))
        if (allocated(error)) return

        e = cmplx(1, 1,kind=dp)* reshape([1, 2, 3, 4, 5, 6], [2, 3])
        call savetxt(outpath, e)
        call loadtxt(outpath, d2)
        call check(error, all(shape(d2) == [2, 3]))
        if (allocated(error)) return
        call check(error, all(e == d2))
        if (allocated(error)) return
    end subroutine test_cdp

end module test_savetxt


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_savetxt, only : collect_savetxt
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("savetxt", collect_savetxt) &
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
