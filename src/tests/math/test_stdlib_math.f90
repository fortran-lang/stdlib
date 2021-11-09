! SPDX-Identifier: MIT

module test_stdlib_math
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_math, only: clip
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, qp
    implicit none

    public :: collect_stdlib_math

contains

    !> Collect all exported unit tests
    subroutine collect_stdlib_math(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("clip-int8", test_clip_int8), &
            new_unittest("clip-int8-bounds", test_clip_int8_bounds), &
            new_unittest("clip-int16", test_clip_int16), &
            new_unittest("clip-int16-bounds", test_clip_int16_bounds), &
            new_unittest("clip-int32", test_clip_int32), &
            new_unittest("clip-int32-bounds", test_clip_int32_bounds), &
            new_unittest("clip-int64", test_clip_int64), &
            new_unittest("clip-int64-bounds", test_clip_int64_bounds), &
            new_unittest("clip-real-single", test_clip_rsp), &
            new_unittest("clip-real-single-bounds", test_clip_rsp_bounds), &
            new_unittest("clip-real-double", test_clip_rdp), &
            new_unittest("clip-real-double-bounds", test_clip_rdp_bounds), &
            new_unittest("clip-real-quad", test_clip_rqp), &
            new_unittest("clip-real-quad-bounds", test_clip_rqp_bounds) &
            ]

    end subroutine collect_stdlib_math

    subroutine test_clip_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! clip function
        ! testing format: check(clip(x, xmin, xmax) == correct answer)
        ! valid case: xmin is not greater than xmax
        ! invalid case: xmin is greater than xmax

        ! type: integer(int8), kind: int8
        ! valid test case
        call check(error, clip(2_int8, -2_int8, 5_int8), 2_int8)
        if (allocated(error)) return
        call check(error, clip(127_int8, -127_int8, 0_int8), 0_int8)
        if (allocated(error)) return
    end subroutine test_clip_int8

    subroutine test_clip_int8_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int8, 5_int8, -2_int8), 5_int8)
        if (allocated(error)) return
        call check(error, clip(127_int8, 0_int8, -127_int8), 0_int8)
        if (allocated(error)) return
    end subroutine test_clip_int8_bounds


    subroutine test_clip_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: integer(int16), kind: int16
        ! valid test case
        call check(error, clip(2_int16, -2_int16, 5_int16), 2_int16)
        if (allocated(error)) return
        call check(error, clip(32767_int16, -32767_int16, 0_int16), 0_int16)
        if (allocated(error)) return
    end subroutine test_clip_int16

    subroutine test_clip_int16_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int16, 5_int16, -2_int16), 5_int16)
        if (allocated(error)) return
        call check(error, clip(32767_int16, 0_int16, -32767_int16), 0_int16)
        if (allocated(error)) return
    end subroutine test_clip_int16_bounds


    subroutine test_clip_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: integer(int32), kind: int32
        ! valid test case
        call check(error, clip(2_int32, -2_int32, 5_int32), 2_int32)
        if (allocated(error)) return
        call check(error, clip(-2147483647_int32, 0_int32, 2147483647_int32), 0_int32)
        if (allocated(error)) return
    end subroutine test_clip_int32

    subroutine test_clip_int32_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int32, 5_int32, -2_int32), 5_int32)
        if (allocated(error)) return
        call check(error, clip(-2147483647_int32, 2147483647_int32, 0_int32), 2147483647_int32)
        if (allocated(error)) return
    end subroutine test_clip_int32_bounds


    subroutine test_clip_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: integer(int64), kind: int64
        ! valid test case
        call check(error, clip(2_int64, -2_int64, 5_int64), 2_int64)
        if (allocated(error)) return
        call check(error, clip(-922337203_int64, -10_int64, 25_int64), -10_int64)
        if (allocated(error)) return
    end subroutine test_clip_int64

    subroutine test_clip_int64_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(2_int64, 5_int64, -2_int64), 5_int64)
        if (allocated(error)) return
        call check(error, clip(-922337203_int64, 25_int64, -10_int64), 25_int64)
        if (allocated(error)) return
    end subroutine test_clip_int64_bounds


    subroutine test_clip_rsp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: real(sp), kind: sp
        ! valid test case
        call check(error, clip(3.025_sp, -5.77_sp, 3.025_sp), 3.025_sp)
        if (allocated(error)) return
        call check(error, clip(0.0_sp, -1578.025_sp, -59.68_sp), -59.68_sp)
        if (allocated(error)) return
    end subroutine test_clip_rsp

    subroutine test_clip_rsp_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(3.025_sp, 3.025_sp, -5.77_sp), 3.025_sp)
        if (allocated(error)) return
        call check(error, clip(0.0_sp, -59.68_sp, -1578.025_sp), -59.68_sp)
        if (allocated(error)) return
    end subroutine test_clip_rsp_bounds


    subroutine test_clip_rdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: real(dp), kind: dp
        ! valid test case
        call check(error, clip(3.025_dp, -5.77_dp, 3.025_dp), 3.025_dp)
        if (allocated(error)) return
        call check(error, clip(-7.0_dp, 0.059668_dp, 1.00268_dp), 0.059668_dp)
        if (allocated(error)) return
    end subroutine test_clip_rdp

    subroutine test_clip_rdp_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(3.025_dp, 3.025_dp, -5.77_dp), 3.025_dp)
        if (allocated(error)) return
        call check(error, clip(-7.0_dp, 1.00268_dp, 0.059668_dp), 1.00268_dp)
        if (allocated(error)) return
    end subroutine test_clip_rdp_bounds


    subroutine test_clip_rqp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! type: real(qp), kind: qp
        ! valid test case
        call check(error, clip(3.025_qp, -5.77_qp, 3.025_qp), 3.025_qp)
        if (allocated(error)) return
        call check(error, clip(-55891546.2_qp, -8958133457.23_qp, -689712245.23_qp), -689712245.23_qp)
        if (allocated(error)) return
    end subroutine test_clip_rqp


    subroutine test_clip_rqp_bounds(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        ! invalid test case
        call check(error, clip(3.025_qp, 3.025_qp, -5.77_qp), 3.025_qp)
        if (allocated(error)) return
        call check(error, clip(-55891546.2_qp, -689712245.23_qp, -8958133457.23_qp), -689712245.23_qp)
        if (allocated(error)) return

    end subroutine test_clip_rqp_bounds

end module test_stdlib_math

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stdlib_math, only : collect_stdlib_math
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stdlib-math", collect_stdlib_math) &
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
