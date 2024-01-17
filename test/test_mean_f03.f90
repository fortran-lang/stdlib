

module test_stats_meanf03
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_stats, only: mean
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp, xdp, qp
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none
    private

    public :: collect_stats_meanf03

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 2000 * epsilon(1._dp)

    integer(int8) , parameter :: d1_int8(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int8) :: d8_int8(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_int8, [2, 3, 4, 2, 3, 4, 2, 3], [integer(int8):: 3])
    integer(int16) , parameter :: d1_int16(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int16) :: d8_int16(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_int16, [2, 3, 4, 2, 3, 4, 2, 3], [integer(int16):: 3])
    integer(int32) , parameter :: d1_int32(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int32) :: d8_int32(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_int32, [2, 3, 4, 2, 3, 4, 2, 3], [integer(int32):: 3])
    integer(int64) , parameter :: d1_int64(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int64) :: d8_int64(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_int64, [2, 3, 4, 2, 3, 4, 2, 3], [integer(int64):: 3])
    real(sp) , parameter :: d1_sp(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    real(sp) :: d8_sp(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_sp, [2, 3, 4, 2, 3, 4, 2, 3], [real(sp):: 3])
    real(dp) , parameter :: d1_dp(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    real(dp) :: d8_dp(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_dp, [2, 3, 4, 2, 3, 4, 2, 3], [real(dp):: 3])

    complex(sp) , parameter :: d1_csp(18) = d1_sp
    complex(sp) :: d8_csp(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_csp, [2, 3, 4, 2, 3, 4, 2, 3], [complex(sp):: 3])
    complex(dp) , parameter :: d1_cdp(18) = d1_dp
    complex(dp) :: d8_cdp(2, 3, 4, 2, 3, 4, 2, 3) = reshape(d1_cdp, [2, 3, 4, 2, 3, 4, 2, 3], [complex(dp):: 3])

contains

    !> Collect all exported unit tests
    subroutine collect_stats_meanf03(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_stats_meanf03_all_int8", test_stats_meanf03_all_int8) &
            ,new_unittest("test_stats_meanf03_all_int8", test_stats_meanf03_all_int8) &
            , new_unittest("test_stats_meanf03_all_optmask_int8", test_stats_meanf03_all_optmask_int8) &
            , new_unittest("test_stats_meanf03_int8", test_stats_meanf03_int8) &
            , new_unittest("test_stats_meanf03_optmask_int8", test_stats_meanf03_optmask_int8) &
            , new_unittest("test_stats_meanf03_mask_all_int8", test_stats_meanf03_mask_all_int8) &
            , new_unittest("test_stats_meanf03_mask_int8", test_stats_meanf03_mask_int8) &
            ,new_unittest("test_stats_meanf03_all_int16", test_stats_meanf03_all_int16) &
            , new_unittest("test_stats_meanf03_all_optmask_int16", test_stats_meanf03_all_optmask_int16) &
            , new_unittest("test_stats_meanf03_int16", test_stats_meanf03_int16) &
            , new_unittest("test_stats_meanf03_optmask_int16", test_stats_meanf03_optmask_int16) &
            , new_unittest("test_stats_meanf03_mask_all_int16", test_stats_meanf03_mask_all_int16) &
            , new_unittest("test_stats_meanf03_mask_int16", test_stats_meanf03_mask_int16) &
            ,new_unittest("test_stats_meanf03_all_int32", test_stats_meanf03_all_int32) &
            , new_unittest("test_stats_meanf03_all_optmask_int32", test_stats_meanf03_all_optmask_int32) &
            , new_unittest("test_stats_meanf03_int32", test_stats_meanf03_int32) &
            , new_unittest("test_stats_meanf03_optmask_int32", test_stats_meanf03_optmask_int32) &
            , new_unittest("test_stats_meanf03_mask_all_int32", test_stats_meanf03_mask_all_int32) &
            , new_unittest("test_stats_meanf03_mask_int32", test_stats_meanf03_mask_int32) &
            ,new_unittest("test_stats_meanf03_all_int64", test_stats_meanf03_all_int64) &
            , new_unittest("test_stats_meanf03_all_optmask_int64", test_stats_meanf03_all_optmask_int64) &
            , new_unittest("test_stats_meanf03_int64", test_stats_meanf03_int64) &
            , new_unittest("test_stats_meanf03_optmask_int64", test_stats_meanf03_optmask_int64) &
            , new_unittest("test_stats_meanf03_mask_all_int64", test_stats_meanf03_mask_all_int64) &
            , new_unittest("test_stats_meanf03_mask_int64", test_stats_meanf03_mask_int64) &
            ,new_unittest("test_stats_meanf03_all_sp", test_stats_meanf03_all_sp) &
            , new_unittest("test_stats_meanf03_all_optmask_sp", test_stats_meanf03_all_optmask_sp) &
            , new_unittest("test_stats_meanf03_sp", test_stats_meanf03_sp) &
            , new_unittest("test_stats_meanf03_optmask_sp", test_stats_meanf03_optmask_sp) &
            , new_unittest("test_stats_meanf03_mask_all_sp", test_stats_meanf03_mask_all_sp) &
            , new_unittest("test_stats_meanf03_mask_sp", test_stats_meanf03_mask_sp) &
            ,new_unittest("test_stats_meanf03_all_dp", test_stats_meanf03_all_dp) &
            , new_unittest("test_stats_meanf03_all_optmask_dp", test_stats_meanf03_all_optmask_dp) &
            , new_unittest("test_stats_meanf03_dp", test_stats_meanf03_dp) &
            , new_unittest("test_stats_meanf03_optmask_dp", test_stats_meanf03_optmask_dp) &
            , new_unittest("test_stats_meanf03_mask_all_dp", test_stats_meanf03_mask_all_dp) &
            , new_unittest("test_stats_meanf03_mask_dp", test_stats_meanf03_mask_dp) &
            ,new_unittest("test_stats_meanf03_all_csp", test_stats_meanf03_all_csp) &
            , new_unittest("test_stats_meanf03_all_optmask_csp", test_stats_meanf03_all_optmask_csp) &
            , new_unittest("test_stats_meanf03_csp", test_stats_meanf03_csp) &
            , new_unittest("test_stats_meanf03_optmask_csp", test_stats_meanf03_optmask_csp) &
            , new_unittest("test_stats_meanf03_mask_all_csp", test_stats_meanf03_mask_all_csp) &
            , new_unittest("test_stats_meanf03_mask_csp", test_stats_meanf03_mask_csp) &
            ,new_unittest("test_stats_meanf03_all_cdp", test_stats_meanf03_all_cdp) &
            , new_unittest("test_stats_meanf03_all_optmask_cdp", test_stats_meanf03_all_optmask_cdp) &
            , new_unittest("test_stats_meanf03_cdp", test_stats_meanf03_cdp) &
            , new_unittest("test_stats_meanf03_optmask_cdp", test_stats_meanf03_optmask_cdp) &
            , new_unittest("test_stats_meanf03_mask_all_cdp", test_stats_meanf03_mask_all_cdp) &
            , new_unittest("test_stats_meanf03_mask_cdp", test_stats_meanf03_mask_cdp) &
            ]
    end subroutine collect_stats_meanf03

    subroutine test_stats_meanf03_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine
    subroutine test_stats_meanf03_all_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine
    subroutine test_stats_meanf03_all_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine
    subroutine test_stats_meanf03_all_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine
    subroutine test_stats_meanf03_all_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine
    subroutine test_stats_meanf03_all_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_all_optmask_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_optmask_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_all_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

    subroutine test_stats_meanf03_mask_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call skip_test(error, "Rank > 7 is not supported")
    end subroutine

end module test_stats_meanf03

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stats_meanf03, only : collect_stats_meanf03
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stats_meanf03", collect_stats_meanf03) &
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
