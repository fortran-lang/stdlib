
module test_optval
    use, intrinsic :: iso_fortran_env, only: &
        sp => real32, dp => real64, qp => real128, &
        int8, int16, int32, int64
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_optval, only: optval

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_optval(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("rsp", test_optval_rsp), &
            new_unittest("rdp", test_optval_rdp), &
            new_unittest("rqp", test_optval_rqp), &
            new_unittest("csp", test_optval_csp), &
            new_unittest("cdp", test_optval_cdp), &
            new_unittest("cqp", test_optval_cqp), &
            new_unittest("iint8", test_optval_iint8), &
            new_unittest("iint16", test_optval_iint16), &
            new_unittest("iint32", test_optval_iint32), &
            new_unittest("iint64", test_optval_iint64), &
            new_unittest("logical", test_optval_logical), &
            new_unittest("character", test_optval_character), &
            new_unittest("rsp_arr", test_optval_rsp_arr), &
            new_unittest("rdp_arr", test_optval_rdp_arr), &
            new_unittest("rqp_arr", test_optval_rqp_arr), &
            new_unittest("csp_arr", test_optval_csp_arr), &
            new_unittest("cdp_arr", test_optval_cdp_arr), &
            new_unittest("cqp_arr", test_optval_cqp_arr), &
            new_unittest("iint8_arr", test_optval_iint8_arr), &
            new_unittest("iint16_arr", test_optval_iint16_arr), &
            new_unittest("iint32_arr", test_optval_iint32_arr), &
            new_unittest("iint64_arr", test_optval_iint64_arr) &
            ]

    end subroutine collect_optval

    subroutine test_optval_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_sp(1.0_sp) == 1.0_sp)
        if (allocated(error)) return
        call check(error, foo_sp() == 2.0_sp)
    end subroutine test_optval_rsp


    function foo_sp(x) result(z)
        real(sp), intent(in), optional :: x
        real(sp) :: z
        z = optval(x, 2.0_sp)
    endfunction foo_sp


    subroutine test_optval_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_dp(1.0_dp) == 1.0_dp)
        if (allocated(error)) return
        call check(error, foo_dp() == 2.0_dp)
    end subroutine test_optval_rdp


    function foo_dp(x) result(z)
        real(dp), intent(in), optional :: x
        real(dp) :: z
        z = optval(x, 2.0_dp)
    endfunction foo_dp


    subroutine test_optval_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_optval_rqp




    subroutine test_optval_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(sp) :: z1
        z1 = cmplx(1.0_sp, 2.0_sp, kind=sp)
        call check(error, foo_csp(z1) == z1)
        if (allocated(error)) return
        call check(error, foo_csp() == z1)
    end subroutine test_optval_csp

    function foo_csp(x) result(z)
        complex(sp), intent(in), optional :: x
        complex(sp) :: z
        z = optval(x, cmplx(1.0_sp, 2.0_sp, kind=sp))
    endfunction foo_csp


    subroutine test_optval_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(dp) :: z1
        z1 = cmplx(1.0_dp, 2.0_dp,kind=dp)
        call check(error, foo_cdp(z1) == z1)
        if (allocated(error)) return
        call check(error, foo_cdp() == z1)
    end subroutine test_optval_cdp

    function foo_cdp(x) result(z)
        complex(dp), intent(in), optional :: x
        complex(dp) :: z
        z = optval(x, cmplx(1.0_dp, 2.0_dp, kind=dp))
    endfunction foo_cdp


    subroutine test_optval_cqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_optval_cqp



    subroutine test_optval_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_int8(1_int8) == 1_int8)
        if (allocated(error)) return
        call check(error, foo_int8() == 2_int8)
    end subroutine test_optval_iint8


    function foo_int8(x) result(z)
        integer(int8), intent(in), optional :: x
        integer(int8) :: z
        z = optval(x, 2_int8)
    endfunction foo_int8


    subroutine test_optval_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_int16(1_int16) == 1_int16)
        if (allocated(error)) return
        call check(error, foo_int16() == 2_int16)
    end subroutine test_optval_iint16


    function foo_int16(x) result(z)
        integer(int16), intent(in), optional :: x
        integer(int16) :: z
        z = optval(x, 2_int16)
    endfunction foo_int16


    subroutine test_optval_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_int32(1_int32) == 1_int32)
        if (allocated(error)) return
        call check(error, foo_int32() == 2_int32)
    end subroutine test_optval_iint32


    function foo_int32(x) result(z)
        integer(int32), intent(in), optional :: x
        integer(int32) :: z
        z = optval(x, 2_int32)
    endfunction foo_int32


    subroutine test_optval_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_int64(1_int64) == 1_int64)
        if (allocated(error)) return
        call check(error, foo_int64() == 2_int64)
    end subroutine test_optval_iint64


    function foo_int64(x) result(z)
        integer(int64), intent(in), optional :: x
        integer(int64) :: z
        z = optval(x, 2_int64)
    endfunction foo_int64


    subroutine test_optval_logical(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_logical(.true.))
        if (allocated(error)) return
        call check(error, .not.foo_logical())
    end subroutine test_optval_logical


    function foo_logical(x) result(z)
        logical, intent(in), optional :: x
        logical :: z
        z = optval(x, .false.)
    endfunction foo_logical


    subroutine test_optval_character(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, foo_character("x") == "x")
        if (allocated(error)) return
        call check(error, foo_character() == "y")
    end subroutine test_optval_character


    function foo_character(x) result(z)
        character(len=*), intent(in), optional :: x
        character(len=:), allocatable :: z
        z = optval(x, "y")
    endfunction foo_character


    subroutine test_optval_rsp_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, all(foo_sp_arr([1.0_sp, -1.0_sp]) == [1.0_sp, -1.0_sp]))
        if (allocated(error)) return
        call check(error, all(foo_sp_arr() == [2.0_sp, -2.0_sp]))
    end subroutine test_optval_rsp_arr


    function foo_sp_arr(x) result(z)
        real(sp), dimension(2), intent(in), optional :: x
        real(sp), dimension(2) :: z
        z = optval(x, [2.0_sp, -2.0_sp])
    end function foo_sp_arr


    subroutine test_optval_rdp_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, all(foo_dp_arr([1.0_dp, -1.0_dp]) == [1.0_dp, -1.0_dp]))
        if (allocated(error)) return
        call check(error, all(foo_dp_arr() == [2.0_dp, -2.0_dp]))
    end subroutine test_optval_rdp_arr


    function foo_dp_arr(x) result(z)
        real(dp), dimension(2), intent(in), optional :: x
        real(dp), dimension(2) :: z
        z = optval(x, [2.0_dp, -2.0_dp])
    end function foo_dp_arr


    subroutine test_optval_rqp_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_optval_rqp_arr




    subroutine test_optval_csp_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(sp), dimension(2) :: z1, z2
        z1 = cmplx(1.0_sp, 2.0_sp, kind=sp)*[1.0_sp, -1.0_sp]
        z2 = cmplx(2.0_sp, 2.0_sp, kind=sp)*[1.0_sp, -1.0_sp]
        call check(error, all(foo_csp_arr(z1) == z1))
        if (allocated(error)) return
        call check(error, all(foo_csp_arr() == z2))
    end subroutine test_optval_csp_arr


    function foo_csp_arr(x) result(z)
        complex(sp), dimension(2), intent(in), optional :: x
        complex(sp), dimension(2) :: z
        z = optval(x, cmplx(2.0_sp, 2.0_sp, kind=sp)*[1.0_sp, -1.0_sp])
    end function foo_csp_arr


    subroutine test_optval_cdp_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        complex(dp), dimension(2) :: z1, z2
        z1 = cmplx(1.0_dp, 2.0_dp, kind=dp)*[1.0_dp, -1.0_dp]
        z2 = cmplx(2.0_dp, 2.0_dp, kind=dp)*[1.0_dp, -1.0_dp]
        call check(error, all(foo_cdp_arr(z1) == z1))
        if (allocated(error)) return
        call check(error, all(foo_cdp_arr() == z2))
    end subroutine test_optval_cdp_arr


    function foo_cdp_arr(x) result(z)
        complex(dp), dimension(2), intent(in), optional :: x
        complex(dp), dimension(2) :: z
        z = optval(x, cmplx(2.0_dp, 2.0_dp, kind=dp)*[1.0_dp, -1.0_dp])
    end function foo_cdp_arr


    subroutine test_optval_cqp_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Quadruple precision is not enabled")
    end subroutine test_optval_cqp_arr




    subroutine test_optval_iint8_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, all(foo_int8_arr([1_int8, -1_int8]) == [1_int8, -1_int8]))
        if (allocated(error)) return
        call check(error, all(foo_int8_arr() == [2_int8, -2_int8]))
    end subroutine test_optval_iint8_arr


    function foo_int8_arr(x) result(z)
        integer(int8), dimension(2), intent(in), optional :: x
        integer(int8), dimension(2) :: z
        z = optval(x, [2_int8, -2_int8])
    end function foo_int8_arr


    subroutine test_optval_iint16_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, all(foo_int16_arr([1_int16, -1_int16]) == [1_int16, -1_int16]))
        if (allocated(error)) return
        call check(error, all(foo_int16_arr() == [2_int16, -2_int16]))
    end subroutine test_optval_iint16_arr


    function foo_int16_arr(x) result(z)
        integer(int16), dimension(2), intent(in), optional :: x
        integer(int16), dimension(2) :: z
        z = optval(x, [2_int16, -2_int16])
    end function foo_int16_arr


    subroutine test_optval_iint32_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, all(foo_int32_arr([1_int32, -1_int32]) == [1_int32, -1_int32]))
        if (allocated(error)) return
        call check(error, all(foo_int32_arr() == [2_int32, -2_int32]))
    end subroutine test_optval_iint32_arr


    function foo_int32_arr(x) result(z)
        integer(int32), dimension(2), intent(in), optional :: x
        integer(int32), dimension(2) :: z
        z = optval(x, [2_int32, -2_int32])
    end function foo_int32_arr


    subroutine test_optval_iint64_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, all(foo_int64_arr([1_int64, -1_int64]) == [1_int64, -1_int64]))
        if (allocated(error)) return
        call check(error, all(foo_int64_arr() == [2_int64, -2_int64]))
    end subroutine test_optval_iint64_arr


    function foo_int64_arr(x) result(z)
        integer(int64), dimension(2), intent(in), optional :: x
        integer(int64), dimension(2) :: z
        z = optval(x, [2_int64, -2_int64])
    end function foo_int64_arr


    subroutine test_optval_logical_arr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, all(foo_logical_arr()))
        if (allocated(error)) return
        call check(error, all(.not.foo_logical_arr()))
    end subroutine test_optval_logical_arr


    function foo_logical_arr(x) result(z)
        logical, dimension(2), intent(in), optional :: x
        logical, dimension(2) :: z
        z = optval(x, [.false., .false.])
    end function foo_logical_arr

end module test_optval


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_optval, only : collect_optval
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("optval", collect_optval) &
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
