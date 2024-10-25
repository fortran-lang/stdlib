! SPDX-Identifier: MIT



module test_meshgrid
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_math, only: meshgrid, stdlib_meshgrid_ij, stdlib_meshgrid_xy
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    implicit none

    public :: collect_meshgrid

contains

    !> Collect all exported unit tests
    subroutine collect_meshgrid(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("meshgrid_default_1_iint8_iint8", test_meshgrid_default_1_iint8_iint8), &
            new_unittest("meshgrid_xy_1_iint8_iint8", test_meshgrid_xy_1_iint8_iint8), &
            new_unittest("meshgrid_ij_1_iint8_iint8", test_meshgrid_ij_1_iint8_iint8), &
            new_unittest("meshgrid_default_2_iint8_iint8", test_meshgrid_default_2_iint8_iint8), &
            new_unittest("meshgrid_xy_2_iint8_iint8", test_meshgrid_xy_2_iint8_iint8), &
            new_unittest("meshgrid_ij_2_iint8_iint8", test_meshgrid_ij_2_iint8_iint8), &
            new_unittest("meshgrid_default_3_iint8_iint8", test_meshgrid_default_3_iint8_iint8), &
            new_unittest("meshgrid_xy_3_iint8_iint8", test_meshgrid_xy_3_iint8_iint8), &
            new_unittest("meshgrid_ij_3_iint8_iint8", test_meshgrid_ij_3_iint8_iint8), &
            new_unittest("meshgrid_default_4_iint8_iint8", test_meshgrid_default_4_iint8_iint8), &
            new_unittest("meshgrid_xy_4_iint8_iint8", test_meshgrid_xy_4_iint8_iint8), &
            new_unittest("meshgrid_ij_4_iint8_iint8", test_meshgrid_ij_4_iint8_iint8), &
            new_unittest("meshgrid_default_5_iint8_iint8", test_meshgrid_default_5_iint8_iint8), &
            new_unittest("meshgrid_xy_5_iint8_iint8", test_meshgrid_xy_5_iint8_iint8), &
            new_unittest("meshgrid_ij_5_iint8_iint8", test_meshgrid_ij_5_iint8_iint8), &
            new_unittest("meshgrid_default_6_iint8_iint8", test_meshgrid_default_6_iint8_iint8), &
            new_unittest("meshgrid_xy_6_iint8_iint8", test_meshgrid_xy_6_iint8_iint8), &
            new_unittest("meshgrid_ij_6_iint8_iint8", test_meshgrid_ij_6_iint8_iint8), &
            new_unittest("meshgrid_default_7_iint8_iint8", test_meshgrid_default_7_iint8_iint8), &
            new_unittest("meshgrid_xy_7_iint8_iint8", test_meshgrid_xy_7_iint8_iint8), &
            new_unittest("meshgrid_ij_7_iint8_iint8", test_meshgrid_ij_7_iint8_iint8), &
            new_unittest("meshgrid_default_1_iint16_iint16", test_meshgrid_default_1_iint16_iint16), &
            new_unittest("meshgrid_xy_1_iint16_iint16", test_meshgrid_xy_1_iint16_iint16), &
            new_unittest("meshgrid_ij_1_iint16_iint16", test_meshgrid_ij_1_iint16_iint16), &
            new_unittest("meshgrid_default_2_iint16_iint16", test_meshgrid_default_2_iint16_iint16), &
            new_unittest("meshgrid_xy_2_iint16_iint16", test_meshgrid_xy_2_iint16_iint16), &
            new_unittest("meshgrid_ij_2_iint16_iint16", test_meshgrid_ij_2_iint16_iint16), &
            new_unittest("meshgrid_default_3_iint16_iint16", test_meshgrid_default_3_iint16_iint16), &
            new_unittest("meshgrid_xy_3_iint16_iint16", test_meshgrid_xy_3_iint16_iint16), &
            new_unittest("meshgrid_ij_3_iint16_iint16", test_meshgrid_ij_3_iint16_iint16), &
            new_unittest("meshgrid_default_4_iint16_iint16", test_meshgrid_default_4_iint16_iint16), &
            new_unittest("meshgrid_xy_4_iint16_iint16", test_meshgrid_xy_4_iint16_iint16), &
            new_unittest("meshgrid_ij_4_iint16_iint16", test_meshgrid_ij_4_iint16_iint16), &
            new_unittest("meshgrid_default_5_iint16_iint16", test_meshgrid_default_5_iint16_iint16), &
            new_unittest("meshgrid_xy_5_iint16_iint16", test_meshgrid_xy_5_iint16_iint16), &
            new_unittest("meshgrid_ij_5_iint16_iint16", test_meshgrid_ij_5_iint16_iint16), &
            new_unittest("meshgrid_default_6_iint16_iint16", test_meshgrid_default_6_iint16_iint16), &
            new_unittest("meshgrid_xy_6_iint16_iint16", test_meshgrid_xy_6_iint16_iint16), &
            new_unittest("meshgrid_ij_6_iint16_iint16", test_meshgrid_ij_6_iint16_iint16), &
            new_unittest("meshgrid_default_7_iint16_iint16", test_meshgrid_default_7_iint16_iint16), &
            new_unittest("meshgrid_xy_7_iint16_iint16", test_meshgrid_xy_7_iint16_iint16), &
            new_unittest("meshgrid_ij_7_iint16_iint16", test_meshgrid_ij_7_iint16_iint16), &
            new_unittest("meshgrid_default_1_iint32_iint32", test_meshgrid_default_1_iint32_iint32), &
            new_unittest("meshgrid_xy_1_iint32_iint32", test_meshgrid_xy_1_iint32_iint32), &
            new_unittest("meshgrid_ij_1_iint32_iint32", test_meshgrid_ij_1_iint32_iint32), &
            new_unittest("meshgrid_default_2_iint32_iint32", test_meshgrid_default_2_iint32_iint32), &
            new_unittest("meshgrid_xy_2_iint32_iint32", test_meshgrid_xy_2_iint32_iint32), &
            new_unittest("meshgrid_ij_2_iint32_iint32", test_meshgrid_ij_2_iint32_iint32), &
            new_unittest("meshgrid_default_3_iint32_iint32", test_meshgrid_default_3_iint32_iint32), &
            new_unittest("meshgrid_xy_3_iint32_iint32", test_meshgrid_xy_3_iint32_iint32), &
            new_unittest("meshgrid_ij_3_iint32_iint32", test_meshgrid_ij_3_iint32_iint32), &
            new_unittest("meshgrid_default_4_iint32_iint32", test_meshgrid_default_4_iint32_iint32), &
            new_unittest("meshgrid_xy_4_iint32_iint32", test_meshgrid_xy_4_iint32_iint32), &
            new_unittest("meshgrid_ij_4_iint32_iint32", test_meshgrid_ij_4_iint32_iint32), &
            new_unittest("meshgrid_default_5_iint32_iint32", test_meshgrid_default_5_iint32_iint32), &
            new_unittest("meshgrid_xy_5_iint32_iint32", test_meshgrid_xy_5_iint32_iint32), &
            new_unittest("meshgrid_ij_5_iint32_iint32", test_meshgrid_ij_5_iint32_iint32), &
            new_unittest("meshgrid_default_6_iint32_iint32", test_meshgrid_default_6_iint32_iint32), &
            new_unittest("meshgrid_xy_6_iint32_iint32", test_meshgrid_xy_6_iint32_iint32), &
            new_unittest("meshgrid_ij_6_iint32_iint32", test_meshgrid_ij_6_iint32_iint32), &
            new_unittest("meshgrid_default_7_iint32_iint32", test_meshgrid_default_7_iint32_iint32), &
            new_unittest("meshgrid_xy_7_iint32_iint32", test_meshgrid_xy_7_iint32_iint32), &
            new_unittest("meshgrid_ij_7_iint32_iint32", test_meshgrid_ij_7_iint32_iint32), &
            new_unittest("meshgrid_default_1_iint64_iint64", test_meshgrid_default_1_iint64_iint64), &
            new_unittest("meshgrid_xy_1_iint64_iint64", test_meshgrid_xy_1_iint64_iint64), &
            new_unittest("meshgrid_ij_1_iint64_iint64", test_meshgrid_ij_1_iint64_iint64), &
            new_unittest("meshgrid_default_2_iint64_iint64", test_meshgrid_default_2_iint64_iint64), &
            new_unittest("meshgrid_xy_2_iint64_iint64", test_meshgrid_xy_2_iint64_iint64), &
            new_unittest("meshgrid_ij_2_iint64_iint64", test_meshgrid_ij_2_iint64_iint64), &
            new_unittest("meshgrid_default_3_iint64_iint64", test_meshgrid_default_3_iint64_iint64), &
            new_unittest("meshgrid_xy_3_iint64_iint64", test_meshgrid_xy_3_iint64_iint64), &
            new_unittest("meshgrid_ij_3_iint64_iint64", test_meshgrid_ij_3_iint64_iint64), &
            new_unittest("meshgrid_default_4_iint64_iint64", test_meshgrid_default_4_iint64_iint64), &
            new_unittest("meshgrid_xy_4_iint64_iint64", test_meshgrid_xy_4_iint64_iint64), &
            new_unittest("meshgrid_ij_4_iint64_iint64", test_meshgrid_ij_4_iint64_iint64), &
            new_unittest("meshgrid_default_5_iint64_iint64", test_meshgrid_default_5_iint64_iint64), &
            new_unittest("meshgrid_xy_5_iint64_iint64", test_meshgrid_xy_5_iint64_iint64), &
            new_unittest("meshgrid_ij_5_iint64_iint64", test_meshgrid_ij_5_iint64_iint64), &
            new_unittest("meshgrid_default_6_iint64_iint64", test_meshgrid_default_6_iint64_iint64), &
            new_unittest("meshgrid_xy_6_iint64_iint64", test_meshgrid_xy_6_iint64_iint64), &
            new_unittest("meshgrid_ij_6_iint64_iint64", test_meshgrid_ij_6_iint64_iint64), &
            new_unittest("meshgrid_default_7_iint64_iint64", test_meshgrid_default_7_iint64_iint64), &
            new_unittest("meshgrid_xy_7_iint64_iint64", test_meshgrid_xy_7_iint64_iint64), &
            new_unittest("meshgrid_ij_7_iint64_iint64", test_meshgrid_ij_7_iint64_iint64), &
            new_unittest("meshgrid_default_1_rsp_rsp", test_meshgrid_default_1_rsp_rsp), &
            new_unittest("meshgrid_xy_1_rsp_rsp", test_meshgrid_xy_1_rsp_rsp), &
            new_unittest("meshgrid_ij_1_rsp_rsp", test_meshgrid_ij_1_rsp_rsp), &
            new_unittest("meshgrid_default_2_rsp_rsp", test_meshgrid_default_2_rsp_rsp), &
            new_unittest("meshgrid_xy_2_rsp_rsp", test_meshgrid_xy_2_rsp_rsp), &
            new_unittest("meshgrid_ij_2_rsp_rsp", test_meshgrid_ij_2_rsp_rsp), &
            new_unittest("meshgrid_default_3_rsp_rsp", test_meshgrid_default_3_rsp_rsp), &
            new_unittest("meshgrid_xy_3_rsp_rsp", test_meshgrid_xy_3_rsp_rsp), &
            new_unittest("meshgrid_ij_3_rsp_rsp", test_meshgrid_ij_3_rsp_rsp), &
            new_unittest("meshgrid_default_4_rsp_rsp", test_meshgrid_default_4_rsp_rsp), &
            new_unittest("meshgrid_xy_4_rsp_rsp", test_meshgrid_xy_4_rsp_rsp), &
            new_unittest("meshgrid_ij_4_rsp_rsp", test_meshgrid_ij_4_rsp_rsp), &
            new_unittest("meshgrid_default_5_rsp_rsp", test_meshgrid_default_5_rsp_rsp), &
            new_unittest("meshgrid_xy_5_rsp_rsp", test_meshgrid_xy_5_rsp_rsp), &
            new_unittest("meshgrid_ij_5_rsp_rsp", test_meshgrid_ij_5_rsp_rsp), &
            new_unittest("meshgrid_default_6_rsp_rsp", test_meshgrid_default_6_rsp_rsp), &
            new_unittest("meshgrid_xy_6_rsp_rsp", test_meshgrid_xy_6_rsp_rsp), &
            new_unittest("meshgrid_ij_6_rsp_rsp", test_meshgrid_ij_6_rsp_rsp), &
            new_unittest("meshgrid_default_7_rsp_rsp", test_meshgrid_default_7_rsp_rsp), &
            new_unittest("meshgrid_xy_7_rsp_rsp", test_meshgrid_xy_7_rsp_rsp), &
            new_unittest("meshgrid_ij_7_rsp_rsp", test_meshgrid_ij_7_rsp_rsp), &
            new_unittest("meshgrid_default_1_rdp_rdp", test_meshgrid_default_1_rdp_rdp), &
            new_unittest("meshgrid_xy_1_rdp_rdp", test_meshgrid_xy_1_rdp_rdp), &
            new_unittest("meshgrid_ij_1_rdp_rdp", test_meshgrid_ij_1_rdp_rdp), &
            new_unittest("meshgrid_default_2_rdp_rdp", test_meshgrid_default_2_rdp_rdp), &
            new_unittest("meshgrid_xy_2_rdp_rdp", test_meshgrid_xy_2_rdp_rdp), &
            new_unittest("meshgrid_ij_2_rdp_rdp", test_meshgrid_ij_2_rdp_rdp), &
            new_unittest("meshgrid_default_3_rdp_rdp", test_meshgrid_default_3_rdp_rdp), &
            new_unittest("meshgrid_xy_3_rdp_rdp", test_meshgrid_xy_3_rdp_rdp), &
            new_unittest("meshgrid_ij_3_rdp_rdp", test_meshgrid_ij_3_rdp_rdp), &
            new_unittest("meshgrid_default_4_rdp_rdp", test_meshgrid_default_4_rdp_rdp), &
            new_unittest("meshgrid_xy_4_rdp_rdp", test_meshgrid_xy_4_rdp_rdp), &
            new_unittest("meshgrid_ij_4_rdp_rdp", test_meshgrid_ij_4_rdp_rdp), &
            new_unittest("meshgrid_default_5_rdp_rdp", test_meshgrid_default_5_rdp_rdp), &
            new_unittest("meshgrid_xy_5_rdp_rdp", test_meshgrid_xy_5_rdp_rdp), &
            new_unittest("meshgrid_ij_5_rdp_rdp", test_meshgrid_ij_5_rdp_rdp), &
            new_unittest("meshgrid_default_6_rdp_rdp", test_meshgrid_default_6_rdp_rdp), &
            new_unittest("meshgrid_xy_6_rdp_rdp", test_meshgrid_xy_6_rdp_rdp), &
            new_unittest("meshgrid_ij_6_rdp_rdp", test_meshgrid_ij_6_rdp_rdp), &
            new_unittest("meshgrid_default_7_rdp_rdp", test_meshgrid_default_7_rdp_rdp), &
            new_unittest("meshgrid_xy_7_rdp_rdp", test_meshgrid_xy_7_rdp_rdp), &
            new_unittest("meshgrid_ij_7_rdp_rdp", test_meshgrid_ij_7_rdp_rdp), &
            new_unittest("dummy", test_dummy) &
            ]

    end subroutine collect_meshgrid

    subroutine test_meshgrid_default_1_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length)
        integer(int8) :: xm1(length)
        integer(int8) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_1_iint8_iint8
    subroutine test_meshgrid_xy_1_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length)
        integer(int8) :: xm1(length)
        integer(int8) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_1_iint8_iint8
    subroutine test_meshgrid_ij_1_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length)
        integer(int8) :: xm1(length)
        integer(int8) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_1_iint8_iint8
    subroutine test_meshgrid_default_2_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length)
        integer(int8) :: xm1(length,length),xm2(length,length)
        integer(int8) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_2_iint8_iint8
    subroutine test_meshgrid_xy_2_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length)
        integer(int8) :: xm1(length,length),xm2(length,length)
        integer(int8) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_2_iint8_iint8
    subroutine test_meshgrid_ij_2_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length)
        integer(int8) :: xm1(length,length),xm2(length,length)
        integer(int8) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_2_iint8_iint8
    subroutine test_meshgrid_default_3_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length)
        integer(int8) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int8) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_3_iint8_iint8
    subroutine test_meshgrid_xy_3_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length)
        integer(int8) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int8) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_3_iint8_iint8
    subroutine test_meshgrid_ij_3_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length)
        integer(int8) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int8) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_3_iint8_iint8
    subroutine test_meshgrid_default_4_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length)
        integer(int8) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(len&
            &gth,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,leng&
            &th,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_4_iint8_iint8
    subroutine test_meshgrid_xy_4_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length)
        integer(int8) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(len&
            &gth,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,leng&
            &th,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_4_iint8_iint8
    subroutine test_meshgrid_ij_4_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length)
        integer(int8) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(len&
            &gth,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,leng&
            &th,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_4_iint8_iint8
    subroutine test_meshgrid_default_5_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int8) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,l&
            &ength,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(leng&
            &th,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,le&
            &ngth)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_5_iint8_iint8
    subroutine test_meshgrid_xy_5_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int8) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,l&
            &ength,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(leng&
            &th,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,le&
            &ngth)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_5_iint8_iint8
    subroutine test_meshgrid_ij_5_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int8) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,l&
            &ength,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(leng&
            &th,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,le&
            &ngth)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_5_iint8_iint8
    subroutine test_meshgrid_default_6_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int8) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,l&
            &ength,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,leng&
            &th,length),xm6(length,length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),&
            &xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(le&
            &ngth,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_6_iint8_iint8
    subroutine test_meshgrid_xy_6_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int8) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,l&
            &ength,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,leng&
            &th,length),xm6(length,length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),&
            &xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(le&
            &ngth,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_6_iint8_iint8
    subroutine test_meshgrid_ij_6_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int8) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,l&
            &ength,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,leng&
            &th,length),xm6(length,length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),&
            &xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(le&
            &ngth,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_6_iint8_iint8
    subroutine test_meshgrid_default_7_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int8) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length&
            &),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(leng&
            &th,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,&
            &length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,l&
            &ength,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length&
            &,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,leng&
            &th,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_7_iint8_iint8
    subroutine test_meshgrid_xy_7_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int8) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length&
            &),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(leng&
            &th,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,&
            &length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,l&
            &ength,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length&
            &,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,leng&
            &th,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_7_iint8_iint8
    subroutine test_meshgrid_ij_7_iint8_iint8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int8) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int8) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length&
            &),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(leng&
            &th,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,&
            &length,length,length,length,length)
        integer(int8) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,l&
            &ength,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length&
            &,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,leng&
            &th,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int8), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_7_iint8_iint8
    subroutine test_meshgrid_default_1_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length)
        integer(int16) :: xm1(length)
        integer(int16) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_1_iint16_iint16
    subroutine test_meshgrid_xy_1_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length)
        integer(int16) :: xm1(length)
        integer(int16) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_1_iint16_iint16
    subroutine test_meshgrid_ij_1_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length)
        integer(int16) :: xm1(length)
        integer(int16) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_1_iint16_iint16
    subroutine test_meshgrid_default_2_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length)
        integer(int16) :: xm1(length,length),xm2(length,length)
        integer(int16) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_2_iint16_iint16
    subroutine test_meshgrid_xy_2_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length)
        integer(int16) :: xm1(length,length),xm2(length,length)
        integer(int16) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_2_iint16_iint16
    subroutine test_meshgrid_ij_2_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length)
        integer(int16) :: xm1(length,length),xm2(length,length)
        integer(int16) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_2_iint16_iint16
    subroutine test_meshgrid_default_3_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length)
        integer(int16) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int16) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_3_iint16_iint16
    subroutine test_meshgrid_xy_3_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length)
        integer(int16) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int16) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_3_iint16_iint16
    subroutine test_meshgrid_ij_3_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length)
        integer(int16) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int16) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_3_iint16_iint16
    subroutine test_meshgrid_default_4_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length)
        integer(int16) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_4_iint16_iint16
    subroutine test_meshgrid_xy_4_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length)
        integer(int16) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_4_iint16_iint16
    subroutine test_meshgrid_ij_4_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length)
        integer(int16) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_4_iint16_iint16
    subroutine test_meshgrid_default_5_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int16) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_5_iint16_iint16
    subroutine test_meshgrid_xy_5_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int16) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_5_iint16_iint16
    subroutine test_meshgrid_ij_5_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int16) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_5_iint16_iint16
    subroutine test_meshgrid_default_6_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int16) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_6_iint16_iint16
    subroutine test_meshgrid_xy_6_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int16) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_6_iint16_iint16
    subroutine test_meshgrid_ij_6_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int16) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_6_iint16_iint16
    subroutine test_meshgrid_default_7_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int16) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_7_iint16_iint16
    subroutine test_meshgrid_xy_7_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int16) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_7_iint16_iint16
    subroutine test_meshgrid_ij_7_iint16_iint16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int16) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int16) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int16) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int16), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_7_iint16_iint16
    subroutine test_meshgrid_default_1_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length)
        integer(int32) :: xm1(length)
        integer(int32) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_1_iint32_iint32
    subroutine test_meshgrid_xy_1_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length)
        integer(int32) :: xm1(length)
        integer(int32) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_1_iint32_iint32
    subroutine test_meshgrid_ij_1_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length)
        integer(int32) :: xm1(length)
        integer(int32) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_1_iint32_iint32
    subroutine test_meshgrid_default_2_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length)
        integer(int32) :: xm1(length,length),xm2(length,length)
        integer(int32) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_2_iint32_iint32
    subroutine test_meshgrid_xy_2_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length)
        integer(int32) :: xm1(length,length),xm2(length,length)
        integer(int32) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_2_iint32_iint32
    subroutine test_meshgrid_ij_2_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length)
        integer(int32) :: xm1(length,length),xm2(length,length)
        integer(int32) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_2_iint32_iint32
    subroutine test_meshgrid_default_3_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length)
        integer(int32) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int32) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_3_iint32_iint32
    subroutine test_meshgrid_xy_3_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length)
        integer(int32) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int32) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_3_iint32_iint32
    subroutine test_meshgrid_ij_3_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length)
        integer(int32) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int32) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_3_iint32_iint32
    subroutine test_meshgrid_default_4_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length)
        integer(int32) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_4_iint32_iint32
    subroutine test_meshgrid_xy_4_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length)
        integer(int32) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_4_iint32_iint32
    subroutine test_meshgrid_ij_4_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length)
        integer(int32) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_4_iint32_iint32
    subroutine test_meshgrid_default_5_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int32) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_5_iint32_iint32
    subroutine test_meshgrid_xy_5_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int32) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_5_iint32_iint32
    subroutine test_meshgrid_ij_5_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int32) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_5_iint32_iint32
    subroutine test_meshgrid_default_6_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int32) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_6_iint32_iint32
    subroutine test_meshgrid_xy_6_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int32) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_6_iint32_iint32
    subroutine test_meshgrid_ij_6_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int32) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_6_iint32_iint32
    subroutine test_meshgrid_default_7_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int32) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_7_iint32_iint32
    subroutine test_meshgrid_xy_7_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int32) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_7_iint32_iint32
    subroutine test_meshgrid_ij_7_iint32_iint32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int32) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int32) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int32) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int32), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_7_iint32_iint32
    subroutine test_meshgrid_default_1_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length)
        integer(int64) :: xm1(length)
        integer(int64) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_1_iint64_iint64
    subroutine test_meshgrid_xy_1_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length)
        integer(int64) :: xm1(length)
        integer(int64) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_1_iint64_iint64
    subroutine test_meshgrid_ij_1_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length)
        integer(int64) :: xm1(length)
        integer(int64) :: xm1_exact(length)
        integer :: i
        integer :: i1
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_1_iint64_iint64
    subroutine test_meshgrid_default_2_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length)
        integer(int64) :: xm1(length,length),xm2(length,length)
        integer(int64) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_2_iint64_iint64
    subroutine test_meshgrid_xy_2_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length)
        integer(int64) :: xm1(length,length),xm2(length,length)
        integer(int64) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_2_iint64_iint64
    subroutine test_meshgrid_ij_2_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length)
        integer(int64) :: xm1(length,length),xm2(length,length)
        integer(int64) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_2_iint64_iint64
    subroutine test_meshgrid_default_3_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length)
        integer(int64) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int64) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_3_iint64_iint64
    subroutine test_meshgrid_xy_3_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length)
        integer(int64) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int64) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_3_iint64_iint64
    subroutine test_meshgrid_ij_3_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length)
        integer(int64) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        integer(int64) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_3_iint64_iint64
    subroutine test_meshgrid_default_4_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length)
        integer(int64) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_4_iint64_iint64
    subroutine test_meshgrid_xy_4_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length)
        integer(int64) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_4_iint64_iint64
    subroutine test_meshgrid_ij_4_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length)
        integer(int64) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(le&
            &ngth,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,len&
            &gth,length),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_4_iint64_iint64
    subroutine test_meshgrid_default_5_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int64) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_5_iint64_iint64
    subroutine test_meshgrid_xy_5_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int64) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_5_iint64_iint64
    subroutine test_meshgrid_ij_5_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        integer(int64) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,&
            &length,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(len&
            &gth,length,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,l&
            &ength)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_5_iint64_iint64
    subroutine test_meshgrid_default_6_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int64) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_6_iint64_iint64
    subroutine test_meshgrid_xy_6_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int64) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_6_iint64_iint64
    subroutine test_meshgrid_ij_6_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        integer(int64) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,&
            &length,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,len&
            &gth,length),xm6(length,length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length)&
            &,xm3_exact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(l&
            &ength,length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_6_iint64_iint64
    subroutine test_meshgrid_default_7_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int64) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_7_iint64_iint64
    subroutine test_meshgrid_xy_7_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int64) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_7_iint64_iint64
    subroutine test_meshgrid_ij_7_iint64_iint64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        integer(int64) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        integer(int64) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,lengt&
            &h),xm3(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(len&
            &gth,length,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length&
            &,length,length,length,length,length)
        integer(int64) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,&
            &length,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,lengt&
            &h,length,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,len&
            &gth,length,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        integer(int64), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_7_iint64_iint64
    subroutine test_meshgrid_default_1_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length)
        real(sp) :: xm1(length)
        real(sp) :: xm1_exact(length)
        integer :: i
        integer :: i1
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_1_rsp_rsp
    subroutine test_meshgrid_xy_1_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length)
        real(sp) :: xm1(length)
        real(sp) :: xm1_exact(length)
        integer :: i
        integer :: i1
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_1_rsp_rsp
    subroutine test_meshgrid_ij_1_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length)
        real(sp) :: xm1(length)
        real(sp) :: xm1_exact(length)
        integer :: i
        integer :: i1
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_1_rsp_rsp
    subroutine test_meshgrid_default_2_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length)
        real(sp) :: xm1(length,length),xm2(length,length)
        real(sp) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_2_rsp_rsp
    subroutine test_meshgrid_xy_2_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length)
        real(sp) :: xm1(length,length),xm2(length,length)
        real(sp) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_2_rsp_rsp
    subroutine test_meshgrid_ij_2_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length)
        real(sp) :: xm1(length,length),xm2(length,length)
        real(sp) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_2_rsp_rsp
    subroutine test_meshgrid_default_3_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length)
        real(sp) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        real(sp) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_3_rsp_rsp
    subroutine test_meshgrid_xy_3_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length)
        real(sp) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        real(sp) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_3_rsp_rsp
    subroutine test_meshgrid_ij_3_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length)
        real(sp) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        real(sp) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_3_rsp_rsp
    subroutine test_meshgrid_default_4_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length)
        real(sp) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(length,l&
            &ength,length,length)
        real(sp) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,length,le&
            &ngth),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_4_rsp_rsp
    subroutine test_meshgrid_xy_4_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length)
        real(sp) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(length,l&
            &ength,length,length)
        real(sp) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,length,le&
            &ngth),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_4_rsp_rsp
    subroutine test_meshgrid_ij_4_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length)
        real(sp) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(length,l&
            &ength,length,length)
        real(sp) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,length,le&
            &ngth),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_4_rsp_rsp
    subroutine test_meshgrid_default_5_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        real(sp) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,length&
            &,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(length,le&
            &ngth,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_5_rsp_rsp
    subroutine test_meshgrid_xy_5_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        real(sp) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,length&
            &,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(length,le&
            &ngth,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_5_rsp_rsp
    subroutine test_meshgrid_ij_5_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        real(sp) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,length&
            &,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(length,le&
            &ngth,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_5_rsp_rsp
    subroutine test_meshgrid_default_6_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        real(sp) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,length&
            &,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,length,le&
            &ngth),xm6(length,length,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),xm3_e&
            &xact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(length,&
            &length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_6_rsp_rsp
    subroutine test_meshgrid_xy_6_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        real(sp) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,length&
            &,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,length,le&
            &ngth),xm6(length,length,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),xm3_e&
            &xact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(length,&
            &length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_6_rsp_rsp
    subroutine test_meshgrid_ij_6_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        real(sp) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,length&
            &,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,length,le&
            &ngth),xm6(length,length,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),xm3_e&
            &xact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(length,&
            &length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_6_rsp_rsp
    subroutine test_meshgrid_default_7_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        real(sp) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length),xm3&
            &(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(length,le&
            &ngth,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,lengt&
            &h,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length&
            &,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length,leng&
            &th,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,length,le&
            &ngth,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_7_rsp_rsp
    subroutine test_meshgrid_xy_7_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        real(sp) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length),xm3&
            &(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(length,le&
            &ngth,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,lengt&
            &h,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length&
            &,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length,leng&
            &th,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,length,le&
            &ngth,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_7_rsp_rsp
    subroutine test_meshgrid_ij_7_rsp_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(sp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        real(sp) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length),xm3&
            &(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(length,le&
            &ngth,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,lengt&
            &h,length,length,length,length)
        real(sp) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length&
            &,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length,leng&
            &th,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,length,le&
            &ngth,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        real(sp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_7_rsp_rsp
    subroutine test_meshgrid_default_1_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length)
        real(dp) :: xm1(length)
        real(dp) :: xm1_exact(length)
        integer :: i
        integer :: i1
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_1_rdp_rdp
    subroutine test_meshgrid_xy_1_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length)
        real(dp) :: xm1(length)
        real(dp) :: xm1_exact(length)
        integer :: i
        integer :: i1
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_1_rdp_rdp
    subroutine test_meshgrid_ij_1_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length)
        real(dp) :: xm1(length)
        real(dp) :: xm1_exact(length)
        integer :: i
        integer :: i1
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        xm1_exact = reshape( &
                [(x1(i1), i1 = 1, size(x1))], &
                shape=[length] &
        )
        call meshgrid( &
                x1, &
                xm1 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_1_rdp_rdp
    subroutine test_meshgrid_default_2_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length)
        real(dp) :: xm1(length,length),xm2(length,length)
        real(dp) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_2_rdp_rdp
    subroutine test_meshgrid_xy_2_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length)
        real(dp) :: xm1(length,length),xm2(length,length)
        real(dp) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_2_rdp_rdp
    subroutine test_meshgrid_ij_2_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length)
        real(dp) :: xm1(length,length),xm2(length,length)
        real(dp) :: xm1_exact(length,length),xm2_exact(length,length)
        integer :: i
        integer :: i1,i2
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        xm1_exact = reshape( &
                [((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        xm2_exact = reshape( &
                [((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2))], &
                shape=[length,length] &
        )
        call meshgrid( &
                x1,x2, &
                xm1,xm2 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_2_rdp_rdp
    subroutine test_meshgrid_default_3_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length)
        real(dp) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        real(dp) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_3_rdp_rdp
    subroutine test_meshgrid_xy_3_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length)
        real(dp) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        real(dp) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_3_rdp_rdp
    subroutine test_meshgrid_ij_3_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length)
        real(dp) :: xm1(length,length,length),xm2(length,length,length),xm3(length,length,length)
        real(dp) :: xm1_exact(length,length,length),xm2_exact(length,length,length),xm3_exact(length,length,length)
        integer :: i
        integer :: i1,i2,i3
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        xm1_exact = reshape( &
                [(((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm2_exact = reshape( &
                [(((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        xm3_exact = reshape( &
                [(((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3))], &
                shape=[length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3, &
                xm1,xm2,xm3 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_3_rdp_rdp
    subroutine test_meshgrid_default_4_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length)
        real(dp) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(length,l&
            &ength,length,length)
        real(dp) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,length,le&
            &ngth),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_4_rdp_rdp
    subroutine test_meshgrid_xy_4_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length)
        real(dp) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(length,l&
            &ength,length,length)
        real(dp) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,length,le&
            &ngth),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_4_rdp_rdp
    subroutine test_meshgrid_ij_4_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length)
        real(dp) :: xm1(length,length,length,length),xm2(length,length,length,length),xm3(length,length,length,length),xm4(length,l&
            &ength,length,length)
        real(dp) :: xm1_exact(length,length,length,length),xm2_exact(length,length,length,length),xm3_exact(length,length,length,le&
            &ngth),xm4_exact(length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        xm1_exact = reshape( &
                [((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4))], &
                shape=[length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4, &
                xm1,xm2,xm3,xm4 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_4_rdp_rdp
    subroutine test_meshgrid_default_5_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        real(dp) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,length&
            &,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(length,le&
            &ngth,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_5_rdp_rdp
    subroutine test_meshgrid_xy_5_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        real(dp) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,length&
            &,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(length,le&
            &ngth,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_5_rdp_rdp
    subroutine test_meshgrid_ij_5_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length)
        real(dp) :: xm1(length,length,length,length,length),xm2(length,length,length,length,length),xm3(length,length,length,length&
            &,length),xm4(length,length,length,length,length),xm5(length,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length),xm2_exact(length,length,length,length,length),xm3_exact(length,le&
            &ngth,length,length,length),xm4_exact(length,length,length,length,length),xm5_exact(length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        xm1_exact = reshape( &
                [(((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5))], &
                shape=[length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5, &
                xm1,xm2,xm3,xm4,xm5 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_5_rdp_rdp
    subroutine test_meshgrid_default_6_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        real(dp) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,length&
            &,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,length,le&
            &ngth),xm6(length,length,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),xm3_e&
            &xact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(length,&
            &length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_6_rdp_rdp
    subroutine test_meshgrid_xy_6_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        real(dp) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,length&
            &,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,length,le&
            &ngth),xm6(length,length,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),xm3_e&
            &xact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(length,&
            &length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_6_rdp_rdp
    subroutine test_meshgrid_ij_6_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length)
        real(dp) :: xm1(length,length,length,length,length,length),xm2(length,length,length,length,length,length),xm3(length,length&
            &,length,length,length,length),xm4(length,length,length,length,length,length),xm5(length,length,length,length,length,le&
            &ngth),xm6(length,length,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length),xm3_e&
            &xact(length,length,length,length,length,length),xm4_exact(length,length,length,length,length,length),xm5_exact(length,&
            &length,length,length,length,length),xm6_exact(length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        xm1_exact = reshape( &
                [((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6 =&
                    & 1, size(x6))], &
                shape=[length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6, &
                xm1,xm2,xm3,xm4,xm5,xm6 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_6_rdp_rdp
    subroutine test_meshgrid_default_7_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        real(dp) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length),xm3&
            &(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(length,le&
            &ngth,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,lengt&
            &h,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length&
            &,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length,leng&
            &th,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,length,le&
            &ngth,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                 )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_default_7_rdp_rdp
    subroutine test_meshgrid_xy_7_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        real(dp) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length),xm3&
            &(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(length,le&
            &ngth,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,lengt&
            &h,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length&
            &,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length,leng&
            &th,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,length,le&
            &ngth,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i2 = 1, size(x2)), i1 = 1, size(x1)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_xy )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_xy_7_rdp_rdp
    subroutine test_meshgrid_ij_7_rdp_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: length = 3
        real(dp) :: x1(length),x2(length),x3(length),x4(length),x5(length),x6(length),x7(length)
        real(dp) :: xm1(length,length,length,length,length,length,length),xm2(length,length,length,length,length,length,length),xm3&
            &(length,length,length,length,length,length,length),xm4(length,length,length,length,length,length,length),xm5(length,le&
            &ngth,length,length,length,length,length),xm6(length,length,length,length,length,length,length),xm7(length,length,lengt&
            &h,length,length,length,length)
        real(dp) :: xm1_exact(length,length,length,length,length,length,length),xm2_exact(length,length,length,length,length,length&
            &,length),xm3_exact(length,length,length,length,length,length,length),xm4_exact(length,length,length,length,length,leng&
            &th,length),xm5_exact(length,length,length,length,length,length,length),xm6_exact(length,length,length,length,length,le&
            &ngth,length),xm7_exact(length,length,length,length,length,length,length)
        integer :: i
        integer :: i1,i2,i3,i4,i5,i6,i7
        real(dp), parameter :: ZERO = 0
        ! valid test case
        x1 = [(i, i = length * 0 + 1, length * 1)]
        x2 = [(i, i = length * 1 + 1, length * 2)]
        x3 = [(i, i = length * 2 + 1, length * 3)]
        x4 = [(i, i = length * 3 + 1, length * 4)]
        x5 = [(i, i = length * 4 + 1, length * 5)]
        x6 = [(i, i = length * 5 + 1, length * 6)]
        x7 = [(i, i = length * 6 + 1, length * 7)]
        xm1_exact = reshape( &
                [(((((((x1(i1), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm2_exact = reshape( &
                [(((((((x2(i2), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm3_exact = reshape( &
                [(((((((x3(i3), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm4_exact = reshape( &
                [(((((((x4(i4), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm5_exact = reshape( &
                [(((((((x5(i5), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm6_exact = reshape( &
                [(((((((x6(i6), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        xm7_exact = reshape( &
                [(((((((x7(i7), i1 = 1, size(x1)), i2 = 1, size(x2)), i3 = 1, size(x3)), i4 = 1, size(x4)), i5 = 1, size(x5)), i6&
                    & = 1, size(x6)), i7 = 1, size(x7))], &
                shape=[length,length,length,length,length,length,length] &
        )
        call meshgrid( &
                x1,x2,x3,x4,x5,x6,x7, &
                xm1,xm2,xm3,xm4,xm5,xm6,xm7 &
                  , stdlib_meshgrid_ij )
            call check(error, maxval(abs(xm1 - xm1_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm2 - xm2_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm3 - xm3_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm4 - xm4_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm5 - xm5_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm6 - xm6_exact)), ZERO)
            if (allocated(error)) return
            call check(error, maxval(abs(xm7 - xm7_exact)), ZERO)
            if (allocated(error)) return
    end subroutine test_meshgrid_ij_7_rdp_rdp

    subroutine test_dummy(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
    end subroutine

end module test_meshgrid

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_meshgrid, only : collect_meshgrid
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("meshgrid", collect_meshgrid) &
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
