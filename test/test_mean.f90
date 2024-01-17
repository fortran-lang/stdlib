

module test_stats_mean
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_stats, only: mean
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp, xdp, qp
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none
    private

    public :: collect_stats_mean

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 2000 * epsilon(1._dp)

    integer(int8) , parameter :: d1_int8(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int8) :: d2_int8(3, 6) = reshape(d1_int8, [3, 6])
    integer(int8) :: d3_int8(3, 2, 3) = reshape(d1_int8, [3, 2, 3])
    integer(int8) :: d4_int8(3, 2, 3, 2) = reshape(d1_int8, [3, 2, 3, 2], [integer(int8) :: 3])
    integer(int16) , parameter :: d1_int16(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int16) :: d2_int16(3, 6) = reshape(d1_int16, [3, 6])
    integer(int16) :: d3_int16(3, 2, 3) = reshape(d1_int16, [3, 2, 3])
    integer(int16) :: d4_int16(3, 2, 3, 2) = reshape(d1_int16, [3, 2, 3, 2], [integer(int16) :: 3])
    integer(int32) , parameter :: d1_int32(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int32) :: d2_int32(3, 6) = reshape(d1_int32, [3, 6])
    integer(int32) :: d3_int32(3, 2, 3) = reshape(d1_int32, [3, 2, 3])
    integer(int32) :: d4_int32(3, 2, 3, 2) = reshape(d1_int32, [3, 2, 3, 2], [integer(int32) :: 3])
    integer(int64) , parameter :: d1_int64(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    integer(int64) :: d2_int64(3, 6) = reshape(d1_int64, [3, 6])
    integer(int64) :: d3_int64(3, 2, 3) = reshape(d1_int64, [3, 2, 3])
    integer(int64) :: d4_int64(3, 2, 3, 2) = reshape(d1_int64, [3, 2, 3, 2], [integer(int64) :: 3])
    real(sp) , parameter :: d1_sp(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    real(sp) :: d2_sp(3, 6) = reshape(d1_sp, [3, 6])
    real(sp) :: d3_sp(3, 2, 3) = reshape(d1_sp, [3, 2, 3])
    real(sp) :: d4_sp(3, 2, 3, 2) = reshape(d1_sp, [3, 2, 3, 2], [real(sp) :: 3])
    real(dp) , parameter :: d1_dp(18) = [-10, 2, 3, 4, -6, 6, -7, 8, 9, 4, 1, -20, 9, 10, 14, 15, 40, 30]
    real(dp) :: d2_dp(3, 6) = reshape(d1_dp, [3, 6])
    real(dp) :: d3_dp(3, 2, 3) = reshape(d1_dp, [3, 2, 3])
    real(dp) :: d4_dp(3, 2, 3, 2) = reshape(d1_dp, [3, 2, 3, 2], [real(dp) :: 3])

    complex(sp) , parameter :: d1_csp(18) = d1_sp
    complex(sp) :: d2_csp(3, 6) = reshape(d1_csp, [3, 6])
    complex(sp) :: d3_csp(3, 2, 3) = reshape(d1_csp, [3, 2, 3])
    complex(sp) :: d4_csp(3, 2, 3, 2) = reshape(d1_csp, [3, 2, 3, 2], [complex(sp) :: (3, -2)] )
    complex(dp) , parameter :: d1_cdp(18) = d1_dp
    complex(dp) :: d2_cdp(3, 6) = reshape(d1_cdp, [3, 6])
    complex(dp) :: d3_cdp(3, 2, 3) = reshape(d1_cdp, [3, 2, 3])
    complex(dp) :: d4_cdp(3, 2, 3, 2) = reshape(d1_cdp, [3, 2, 3, 2], [complex(dp) :: (3, -2)] )


contains

    !> Collect all exported unit tests
    subroutine collect_stats_mean(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_stats_mean_all_int8", test_stats_mean_all_int8) &
            ,new_unittest("test_stats_mean_all_int8", test_stats_mean_all_int8) &
            , new_unittest("test_stats_mean_all_optmask_int8", test_stats_mean_all_optmask_int8) &
            , new_unittest("test_stats_mean_int8", test_stats_mean_int8) &
            , new_unittest("test_stats_mean_optmask_int8", test_stats_mean_optmask_int8) &
            , new_unittest("test_stats_mean_mask_all_int8", test_stats_mean_mask_all_int8) &
            , new_unittest("test_stats_mean_mask_int8", test_stats_mean_mask_int8) &
            ,new_unittest("test_stats_mean_all_int16", test_stats_mean_all_int16) &
            , new_unittest("test_stats_mean_all_optmask_int16", test_stats_mean_all_optmask_int16) &
            , new_unittest("test_stats_mean_int16", test_stats_mean_int16) &
            , new_unittest("test_stats_mean_optmask_int16", test_stats_mean_optmask_int16) &
            , new_unittest("test_stats_mean_mask_all_int16", test_stats_mean_mask_all_int16) &
            , new_unittest("test_stats_mean_mask_int16", test_stats_mean_mask_int16) &
            ,new_unittest("test_stats_mean_all_int32", test_stats_mean_all_int32) &
            , new_unittest("test_stats_mean_all_optmask_int32", test_stats_mean_all_optmask_int32) &
            , new_unittest("test_stats_mean_int32", test_stats_mean_int32) &
            , new_unittest("test_stats_mean_optmask_int32", test_stats_mean_optmask_int32) &
            , new_unittest("test_stats_mean_mask_all_int32", test_stats_mean_mask_all_int32) &
            , new_unittest("test_stats_mean_mask_int32", test_stats_mean_mask_int32) &
            ,new_unittest("test_stats_mean_all_int64", test_stats_mean_all_int64) &
            , new_unittest("test_stats_mean_all_optmask_int64", test_stats_mean_all_optmask_int64) &
            , new_unittest("test_stats_mean_int64", test_stats_mean_int64) &
            , new_unittest("test_stats_mean_optmask_int64", test_stats_mean_optmask_int64) &
            , new_unittest("test_stats_mean_mask_all_int64", test_stats_mean_mask_all_int64) &
            , new_unittest("test_stats_mean_mask_int64", test_stats_mean_mask_int64) &
            ,new_unittest("test_stats_mean_all_sp", test_stats_mean_all_sp) &
            , new_unittest("test_stats_mean_all_optmask_sp", test_stats_mean_all_optmask_sp) &
            , new_unittest("test_stats_mean_sp", test_stats_mean_sp) &
            , new_unittest("test_stats_mean_optmask_sp", test_stats_mean_optmask_sp) &
            , new_unittest("test_stats_mean_mask_all_sp", test_stats_mean_mask_all_sp) &
            , new_unittest("test_stats_mean_mask_sp", test_stats_mean_mask_sp) &
            ,new_unittest("test_stats_mean_all_dp", test_stats_mean_all_dp) &
            , new_unittest("test_stats_mean_all_optmask_dp", test_stats_mean_all_optmask_dp) &
            , new_unittest("test_stats_mean_dp", test_stats_mean_dp) &
            , new_unittest("test_stats_mean_optmask_dp", test_stats_mean_optmask_dp) &
            , new_unittest("test_stats_mean_mask_all_dp", test_stats_mean_mask_all_dp) &
            , new_unittest("test_stats_mean_mask_dp", test_stats_mean_mask_dp) &
            ,new_unittest("test_stats_mean_all_csp", test_stats_mean_all_csp) &
            , new_unittest("test_stats_mean_all_optmask_csp", test_stats_mean_all_optmask_csp) &
            , new_unittest("test_stats_mean_csp", test_stats_mean_csp) &
            , new_unittest("test_stats_mean_optmask_csp", test_stats_mean_optmask_csp) &
            , new_unittest("test_stats_mean_mask_all_csp", test_stats_mean_mask_all_csp) &
            , new_unittest("test_stats_mean_mask_csp", test_stats_mean_mask_csp) &
            ,new_unittest("test_stats_mean_all_cdp", test_stats_mean_all_cdp) &
            , new_unittest("test_stats_mean_all_optmask_cdp", test_stats_mean_all_optmask_cdp) &
            , new_unittest("test_stats_mean_cdp", test_stats_mean_cdp) &
            , new_unittest("test_stats_mean_optmask_cdp", test_stats_mean_optmask_cdp) &
            , new_unittest("test_stats_mean_mask_all_cdp", test_stats_mean_mask_all_cdp) &
            , new_unittest("test_stats_mean_mask_cdp", test_stats_mean_mask_cdp) &
            ]
    end subroutine collect_stats_mean

    subroutine test_stats_mean_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int8), sum(real(d1_int8, dp))/real(size(d1_int8), dp)&
                    , 'mean(d1_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int8), sum(real(d2_int8, dp))/real(size(d2_int8), dp)&
                    , 'mean(d2_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int8), sum(real(d3_int8, dp))/real(size(d3_int8), dp)&
                    , 'mean(d3_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int8), sum(real(d4_int8, dp))/real(size(d4_int8), dp)&
                    , 'mean(d4_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int8, .false.))&
                    , 'mean(d1_int8, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d2_int8, .false.))&
                    , 'mean(d2_int8, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d3_int8, .false.))&
                    , 'mean(d3_int8, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d4_int8, .false.))&
                    , 'mean(d4_int8, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int8, 1) -&
                    sum(real(d1_int8, dp), 1)/real(size(d1_int8, 1), dp)) < dptol&
                    , 'mean(d1_int8, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int8, 1) -&
                    sum(real(d2_int8, dp), 1)/real(size(d2_int8, 1), dp))) < dptol&
                    , 'mean(d2_int8, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int8, 2) -&
                    sum(real(d2_int8, dp), 2)/real(size(d2_int8, 2), dp))) < dptol&
                    , 'mean(d2_int8, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int8, 1) -&
                    sum(real(d3_int8, dp), 1)/real(size(d3_int8, 1), dp))) < dptol&
                    , 'mean(d3_int8, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int8, 2) -&
                    sum(real(d3_int8, dp), 2)/real(size(d3_int8, 2), dp))) < dptol&
                    , 'mean(d3_int8, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int8, 3) -&
                    sum(real(d3_int8, dp), 3)/real(size(d3_int8, 3), dp))) < dptol&
                    , 'mean(d3_int8, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 1) -&
                    sum(real(d4_int8, dp), 1)/real(size(d4_int8, 1), dp))) < dptol&
                    , 'mean(d4_int8, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 2) -&
                    sum(real(d4_int8, dp), 2)/real(size(d4_int8, 2), dp))) < dptol&
                    , 'mean(d4_int8, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 3) -&
                    sum(real(d4_int8, dp), 3)/real(size(d4_int8, 3), dp))) < dptol&
                    , 'mean(d4_int8, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 4) -&
                    sum(real(d4_int8, dp), 4)/real(size(d4_int8, 4), dp))) < dptol&
                    , 'mean(d4_int8, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int8, 1, .false.))&
                    , 'mean(d1_int8, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d2_int8, 1, .false.)))&
                    , 'mean(d2_int8, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d2_int8, 2, .false.)))&
                    , 'mean(d2_int8, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int8, 1, .false.)))&
                    , 'mean(d3_int8, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int8, 2, .false.)))&
                    , 'mean(d3_int8, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int8, 3, .false.)))&
                    , 'mean(d3_int8, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int8, 1, .false.)))&
                    , 'mean(d4_int8, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int8, 2, .false.)))&
                    , 'mean(d4_int8, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int8, 3, .false.)))&
                    , 'mean(d4_int8, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int8, 4, .false.)))&
                    , 'mean(d4_int8, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int8, d1_int8 > 0)&
                    , sum(real(d1_int8, dp), d1_int8 > 0)/real(count(d1_int8 > 0), dp)&
                    , 'mean(d1_int8, d1_int8 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int8, d2_int8 > 0)&
                    , sum(real(d2_int8, dp), d2_int8 > 0)/real(count(d2_int8 > 0), dp)&
                    , 'mean(d2_int8, d2_int8 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int8, d3_int8 > 0)&
                    , sum(real(d3_int8, dp), d3_int8 > 0)/real(count(d3_int8 > 0), dp)&
                    , 'mean(d3_int8, d3_int8 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int8, d4_int8 > 0)&
                    , sum(real(d4_int8, dp), d4_int8 > 0)/real(count(d4_int8 > 0), dp)&
                    , 'mean(d4_int8, d4_int8 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int8, 1, d1_int8 > 0) -&
                    sum(real(d1_int8, dp), 1, d1_int8 > 0)/real(count(d1_int8 > 0, 1), dp)) < dptol&
                    , 'mean(d1_int8, 1, d1_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int8, 1, d2_int8 > 0) -&
                    sum(real(d2_int8, dp), 1, d2_int8 > 0)/real(count(d2_int8 > 0, 1), dp))) < dptol&
                    , 'mean(d2_int8, 1, d2_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int8, 2, d2_int8 > 0) -&
                    sum(real(d2_int8, dp), 2, d2_int8 > 0)/real(count(d2_int8 > 0, 2), dp))) < dptol&
                    , 'mean(d2_int8, 2, d2_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int8, 1, d3_int8 > 0) -&
                    sum(real(d3_int8, dp), 1, d3_int8 > 0)/real(count(d3_int8 > 0, 1), dp))) < dptol&
                    , 'mean(d3_int8, 1, d3_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int8, 2, d3_int8 > 0) -&
                    sum(real(d3_int8, dp), 2, d3_int8 > 0)/real(count(d3_int8 > 0, 2), dp))) < dptol&
                    , 'mean(d3_int8, 2, d3_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int8, 3, d3_int8 > 0) -&
                    sum(real(d3_int8, dp), 3, d3_int8 > 0)/real(count(d3_int8 > 0, 3), dp))) < dptol&
                    , 'mean(d3_int8, 3, d3_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 1, d4_int8 > 0) -&
                    sum(real(d4_int8, dp), 1, d4_int8 > 0)/real(count(d4_int8 > 0, 1), dp))) < dptol&
                    , 'mean(d4_int8, 1, d4_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 2, d4_int8 > 0) -&
                    sum(real(d4_int8, dp), 2, d4_int8 > 0)/real(count(d4_int8 > 0, 2), dp))) < dptol&
                    , 'mean(d4_int8, 2, d4_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 3, d4_int8 > 0) -&
                    sum(real(d4_int8, dp), 3, d4_int8 > 0)/real(count(d4_int8 > 0, 3), dp))) < dptol&
                    , 'mean(d4_int8, 3, d4_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int8, 4, d4_int8 > 0) -&
                    sum(real(d4_int8, dp), 4, d4_int8 > 0)/real(count(d4_int8 > 0, 4), dp))) < dptol&
                    , 'mean(d4_int8, 4, d4_int8 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine
    subroutine test_stats_mean_all_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int16), sum(real(d1_int16, dp))/real(size(d1_int16), dp)&
                    , 'mean(d1_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int16), sum(real(d2_int16, dp))/real(size(d2_int16), dp)&
                    , 'mean(d2_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int16), sum(real(d3_int16, dp))/real(size(d3_int16), dp)&
                    , 'mean(d3_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int16), sum(real(d4_int16, dp))/real(size(d4_int16), dp)&
                    , 'mean(d4_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int16, .false.))&
                    , 'mean(d1_int16, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d2_int16, .false.))&
                    , 'mean(d2_int16, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d3_int16, .false.))&
                    , 'mean(d3_int16, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d4_int16, .false.))&
                    , 'mean(d4_int16, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int16, 1) -&
                    sum(real(d1_int16, dp), 1)/real(size(d1_int16, 1), dp)) < dptol&
                    , 'mean(d1_int16, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int16, 1) -&
                    sum(real(d2_int16, dp), 1)/real(size(d2_int16, 1), dp))) < dptol&
                    , 'mean(d2_int16, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int16, 2) -&
                    sum(real(d2_int16, dp), 2)/real(size(d2_int16, 2), dp))) < dptol&
                    , 'mean(d2_int16, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int16, 1) -&
                    sum(real(d3_int16, dp), 1)/real(size(d3_int16, 1), dp))) < dptol&
                    , 'mean(d3_int16, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int16, 2) -&
                    sum(real(d3_int16, dp), 2)/real(size(d3_int16, 2), dp))) < dptol&
                    , 'mean(d3_int16, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int16, 3) -&
                    sum(real(d3_int16, dp), 3)/real(size(d3_int16, 3), dp))) < dptol&
                    , 'mean(d3_int16, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 1) -&
                    sum(real(d4_int16, dp), 1)/real(size(d4_int16, 1), dp))) < dptol&
                    , 'mean(d4_int16, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 2) -&
                    sum(real(d4_int16, dp), 2)/real(size(d4_int16, 2), dp))) < dptol&
                    , 'mean(d4_int16, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 3) -&
                    sum(real(d4_int16, dp), 3)/real(size(d4_int16, 3), dp))) < dptol&
                    , 'mean(d4_int16, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 4) -&
                    sum(real(d4_int16, dp), 4)/real(size(d4_int16, 4), dp))) < dptol&
                    , 'mean(d4_int16, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int16, 1, .false.))&
                    , 'mean(d1_int16, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d2_int16, 1, .false.)))&
                    , 'mean(d2_int16, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d2_int16, 2, .false.)))&
                    , 'mean(d2_int16, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int16, 1, .false.)))&
                    , 'mean(d3_int16, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int16, 2, .false.)))&
                    , 'mean(d3_int16, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int16, 3, .false.)))&
                    , 'mean(d3_int16, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int16, 1, .false.)))&
                    , 'mean(d4_int16, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int16, 2, .false.)))&
                    , 'mean(d4_int16, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int16, 3, .false.)))&
                    , 'mean(d4_int16, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int16, 4, .false.)))&
                    , 'mean(d4_int16, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int16, d1_int16 > 0)&
                    , sum(real(d1_int16, dp), d1_int16 > 0)/real(count(d1_int16 > 0), dp)&
                    , 'mean(d1_int16, d1_int16 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int16, d2_int16 > 0)&
                    , sum(real(d2_int16, dp), d2_int16 > 0)/real(count(d2_int16 > 0), dp)&
                    , 'mean(d2_int16, d2_int16 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int16, d3_int16 > 0)&
                    , sum(real(d3_int16, dp), d3_int16 > 0)/real(count(d3_int16 > 0), dp)&
                    , 'mean(d3_int16, d3_int16 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int16, d4_int16 > 0)&
                    , sum(real(d4_int16, dp), d4_int16 > 0)/real(count(d4_int16 > 0), dp)&
                    , 'mean(d4_int16, d4_int16 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int16, 1, d1_int16 > 0) -&
                    sum(real(d1_int16, dp), 1, d1_int16 > 0)/real(count(d1_int16 > 0, 1), dp)) < dptol&
                    , 'mean(d1_int16, 1, d1_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int16, 1, d2_int16 > 0) -&
                    sum(real(d2_int16, dp), 1, d2_int16 > 0)/real(count(d2_int16 > 0, 1), dp))) < dptol&
                    , 'mean(d2_int16, 1, d2_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int16, 2, d2_int16 > 0) -&
                    sum(real(d2_int16, dp), 2, d2_int16 > 0)/real(count(d2_int16 > 0, 2), dp))) < dptol&
                    , 'mean(d2_int16, 2, d2_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int16, 1, d3_int16 > 0) -&
                    sum(real(d3_int16, dp), 1, d3_int16 > 0)/real(count(d3_int16 > 0, 1), dp))) < dptol&
                    , 'mean(d3_int16, 1, d3_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int16, 2, d3_int16 > 0) -&
                    sum(real(d3_int16, dp), 2, d3_int16 > 0)/real(count(d3_int16 > 0, 2), dp))) < dptol&
                    , 'mean(d3_int16, 2, d3_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int16, 3, d3_int16 > 0) -&
                    sum(real(d3_int16, dp), 3, d3_int16 > 0)/real(count(d3_int16 > 0, 3), dp))) < dptol&
                    , 'mean(d3_int16, 3, d3_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 1, d4_int16 > 0) -&
                    sum(real(d4_int16, dp), 1, d4_int16 > 0)/real(count(d4_int16 > 0, 1), dp))) < dptol&
                    , 'mean(d4_int16, 1, d4_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 2, d4_int16 > 0) -&
                    sum(real(d4_int16, dp), 2, d4_int16 > 0)/real(count(d4_int16 > 0, 2), dp))) < dptol&
                    , 'mean(d4_int16, 2, d4_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 3, d4_int16 > 0) -&
                    sum(real(d4_int16, dp), 3, d4_int16 > 0)/real(count(d4_int16 > 0, 3), dp))) < dptol&
                    , 'mean(d4_int16, 3, d4_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int16, 4, d4_int16 > 0) -&
                    sum(real(d4_int16, dp), 4, d4_int16 > 0)/real(count(d4_int16 > 0, 4), dp))) < dptol&
                    , 'mean(d4_int16, 4, d4_int16 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine
    subroutine test_stats_mean_all_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int32), sum(real(d1_int32, dp))/real(size(d1_int32), dp)&
                    , 'mean(d1_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int32), sum(real(d2_int32, dp))/real(size(d2_int32), dp)&
                    , 'mean(d2_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int32), sum(real(d3_int32, dp))/real(size(d3_int32), dp)&
                    , 'mean(d3_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int32), sum(real(d4_int32, dp))/real(size(d4_int32), dp)&
                    , 'mean(d4_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int32, .false.))&
                    , 'mean(d1_int32, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d2_int32, .false.))&
                    , 'mean(d2_int32, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d3_int32, .false.))&
                    , 'mean(d3_int32, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d4_int32, .false.))&
                    , 'mean(d4_int32, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int32, 1) -&
                    sum(real(d1_int32, dp), 1)/real(size(d1_int32, 1), dp)) < dptol&
                    , 'mean(d1_int32, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int32, 1) -&
                    sum(real(d2_int32, dp), 1)/real(size(d2_int32, 1), dp))) < dptol&
                    , 'mean(d2_int32, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int32, 2) -&
                    sum(real(d2_int32, dp), 2)/real(size(d2_int32, 2), dp))) < dptol&
                    , 'mean(d2_int32, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int32, 1) -&
                    sum(real(d3_int32, dp), 1)/real(size(d3_int32, 1), dp))) < dptol&
                    , 'mean(d3_int32, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int32, 2) -&
                    sum(real(d3_int32, dp), 2)/real(size(d3_int32, 2), dp))) < dptol&
                    , 'mean(d3_int32, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int32, 3) -&
                    sum(real(d3_int32, dp), 3)/real(size(d3_int32, 3), dp))) < dptol&
                    , 'mean(d3_int32, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 1) -&
                    sum(real(d4_int32, dp), 1)/real(size(d4_int32, 1), dp))) < dptol&
                    , 'mean(d4_int32, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 2) -&
                    sum(real(d4_int32, dp), 2)/real(size(d4_int32, 2), dp))) < dptol&
                    , 'mean(d4_int32, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 3) -&
                    sum(real(d4_int32, dp), 3)/real(size(d4_int32, 3), dp))) < dptol&
                    , 'mean(d4_int32, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 4) -&
                    sum(real(d4_int32, dp), 4)/real(size(d4_int32, 4), dp))) < dptol&
                    , 'mean(d4_int32, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int32, 1, .false.))&
                    , 'mean(d1_int32, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d2_int32, 1, .false.)))&
                    , 'mean(d2_int32, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d2_int32, 2, .false.)))&
                    , 'mean(d2_int32, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int32, 1, .false.)))&
                    , 'mean(d3_int32, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int32, 2, .false.)))&
                    , 'mean(d3_int32, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int32, 3, .false.)))&
                    , 'mean(d3_int32, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int32, 1, .false.)))&
                    , 'mean(d4_int32, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int32, 2, .false.)))&
                    , 'mean(d4_int32, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int32, 3, .false.)))&
                    , 'mean(d4_int32, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int32, 4, .false.)))&
                    , 'mean(d4_int32, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int32, d1_int32 > 0)&
                    , sum(real(d1_int32, dp), d1_int32 > 0)/real(count(d1_int32 > 0), dp)&
                    , 'mean(d1_int32, d1_int32 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int32, d2_int32 > 0)&
                    , sum(real(d2_int32, dp), d2_int32 > 0)/real(count(d2_int32 > 0), dp)&
                    , 'mean(d2_int32, d2_int32 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int32, d3_int32 > 0)&
                    , sum(real(d3_int32, dp), d3_int32 > 0)/real(count(d3_int32 > 0), dp)&
                    , 'mean(d3_int32, d3_int32 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int32, d4_int32 > 0)&
                    , sum(real(d4_int32, dp), d4_int32 > 0)/real(count(d4_int32 > 0), dp)&
                    , 'mean(d4_int32, d4_int32 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int32, 1, d1_int32 > 0) -&
                    sum(real(d1_int32, dp), 1, d1_int32 > 0)/real(count(d1_int32 > 0, 1), dp)) < dptol&
                    , 'mean(d1_int32, 1, d1_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int32, 1, d2_int32 > 0) -&
                    sum(real(d2_int32, dp), 1, d2_int32 > 0)/real(count(d2_int32 > 0, 1), dp))) < dptol&
                    , 'mean(d2_int32, 1, d2_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int32, 2, d2_int32 > 0) -&
                    sum(real(d2_int32, dp), 2, d2_int32 > 0)/real(count(d2_int32 > 0, 2), dp))) < dptol&
                    , 'mean(d2_int32, 2, d2_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int32, 1, d3_int32 > 0) -&
                    sum(real(d3_int32, dp), 1, d3_int32 > 0)/real(count(d3_int32 > 0, 1), dp))) < dptol&
                    , 'mean(d3_int32, 1, d3_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int32, 2, d3_int32 > 0) -&
                    sum(real(d3_int32, dp), 2, d3_int32 > 0)/real(count(d3_int32 > 0, 2), dp))) < dptol&
                    , 'mean(d3_int32, 2, d3_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int32, 3, d3_int32 > 0) -&
                    sum(real(d3_int32, dp), 3, d3_int32 > 0)/real(count(d3_int32 > 0, 3), dp))) < dptol&
                    , 'mean(d3_int32, 3, d3_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 1, d4_int32 > 0) -&
                    sum(real(d4_int32, dp), 1, d4_int32 > 0)/real(count(d4_int32 > 0, 1), dp))) < dptol&
                    , 'mean(d4_int32, 1, d4_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 2, d4_int32 > 0) -&
                    sum(real(d4_int32, dp), 2, d4_int32 > 0)/real(count(d4_int32 > 0, 2), dp))) < dptol&
                    , 'mean(d4_int32, 2, d4_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 3, d4_int32 > 0) -&
                    sum(real(d4_int32, dp), 3, d4_int32 > 0)/real(count(d4_int32 > 0, 3), dp))) < dptol&
                    , 'mean(d4_int32, 3, d4_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int32, 4, d4_int32 > 0) -&
                    sum(real(d4_int32, dp), 4, d4_int32 > 0)/real(count(d4_int32 > 0, 4), dp))) < dptol&
                    , 'mean(d4_int32, 4, d4_int32 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine
    subroutine test_stats_mean_all_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int64), sum(real(d1_int64, dp))/real(size(d1_int64), dp)&
                    , 'mean(d1_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int64), sum(real(d2_int64, dp))/real(size(d2_int64), dp)&
                    , 'mean(d2_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int64), sum(real(d3_int64, dp))/real(size(d3_int64), dp)&
                    , 'mean(d3_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int64), sum(real(d4_int64, dp))/real(size(d4_int64), dp)&
                    , 'mean(d4_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int64, .false.))&
                    , 'mean(d1_int64, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d2_int64, .false.))&
                    , 'mean(d2_int64, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d3_int64, .false.))&
                    , 'mean(d3_int64, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d4_int64, .false.))&
                    , 'mean(d4_int64, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int64, 1) -&
                    sum(real(d1_int64, dp), 1)/real(size(d1_int64, 1), dp)) < dptol&
                    , 'mean(d1_int64, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int64, 1) -&
                    sum(real(d2_int64, dp), 1)/real(size(d2_int64, 1), dp))) < dptol&
                    , 'mean(d2_int64, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int64, 2) -&
                    sum(real(d2_int64, dp), 2)/real(size(d2_int64, 2), dp))) < dptol&
                    , 'mean(d2_int64, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int64, 1) -&
                    sum(real(d3_int64, dp), 1)/real(size(d3_int64, 1), dp))) < dptol&
                    , 'mean(d3_int64, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int64, 2) -&
                    sum(real(d3_int64, dp), 2)/real(size(d3_int64, 2), dp))) < dptol&
                    , 'mean(d3_int64, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int64, 3) -&
                    sum(real(d3_int64, dp), 3)/real(size(d3_int64, 3), dp))) < dptol&
                    , 'mean(d3_int64, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 1) -&
                    sum(real(d4_int64, dp), 1)/real(size(d4_int64, 1), dp))) < dptol&
                    , 'mean(d4_int64, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 2) -&
                    sum(real(d4_int64, dp), 2)/real(size(d4_int64, 2), dp))) < dptol&
                    , 'mean(d4_int64, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 3) -&
                    sum(real(d4_int64, dp), 3)/real(size(d4_int64, 3), dp))) < dptol&
                    , 'mean(d4_int64, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 4) -&
                    sum(real(d4_int64, dp), 4)/real(size(d4_int64, 4), dp))) < dptol&
                    , 'mean(d4_int64, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_int64, 1, .false.))&
                    , 'mean(d1_int64, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d2_int64, 1, .false.)))&
                    , 'mean(d2_int64, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d2_int64, 2, .false.)))&
                    , 'mean(d2_int64, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int64, 1, .false.)))&
                    , 'mean(d3_int64, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int64, 2, .false.)))&
                    , 'mean(d3_int64, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_int64, 3, .false.)))&
                    , 'mean(d3_int64, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int64, 1, .false.)))&
                    , 'mean(d4_int64, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int64, 2, .false.)))&
                    , 'mean(d4_int64, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int64, 3, .false.)))&
                    , 'mean(d4_int64, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_int64, 4, .false.)))&
                    , 'mean(d4_int64, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_int64, d1_int64 > 0)&
                    , sum(real(d1_int64, dp), d1_int64 > 0)/real(count(d1_int64 > 0), dp)&
                    , 'mean(d1_int64, d1_int64 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_int64, d2_int64 > 0)&
                    , sum(real(d2_int64, dp), d2_int64 > 0)/real(count(d2_int64 > 0), dp)&
                    , 'mean(d2_int64, d2_int64 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_int64, d3_int64 > 0)&
                    , sum(real(d3_int64, dp), d3_int64 > 0)/real(count(d3_int64 > 0), dp)&
                    , 'mean(d3_int64, d3_int64 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_int64, d4_int64 > 0)&
                    , sum(real(d4_int64, dp), d4_int64 > 0)/real(count(d4_int64 > 0), dp)&
                    , 'mean(d4_int64, d4_int64 > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_int64, 1, d1_int64 > 0) -&
                    sum(real(d1_int64, dp), 1, d1_int64 > 0)/real(count(d1_int64 > 0, 1), dp)) < dptol&
                    , 'mean(d1_int64, 1, d1_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_int64, 1, d2_int64 > 0) -&
                    sum(real(d2_int64, dp), 1, d2_int64 > 0)/real(count(d2_int64 > 0, 1), dp))) < dptol&
                    , 'mean(d2_int64, 1, d2_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_int64, 2, d2_int64 > 0) -&
                    sum(real(d2_int64, dp), 2, d2_int64 > 0)/real(count(d2_int64 > 0, 2), dp))) < dptol&
                    , 'mean(d2_int64, 2, d2_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int64, 1, d3_int64 > 0) -&
                    sum(real(d3_int64, dp), 1, d3_int64 > 0)/real(count(d3_int64 > 0, 1), dp))) < dptol&
                    , 'mean(d3_int64, 1, d3_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int64, 2, d3_int64 > 0) -&
                    sum(real(d3_int64, dp), 2, d3_int64 > 0)/real(count(d3_int64 > 0, 2), dp))) < dptol&
                    , 'mean(d3_int64, 2, d3_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_int64, 3, d3_int64 > 0) -&
                    sum(real(d3_int64, dp), 3, d3_int64 > 0)/real(count(d3_int64 > 0, 3), dp))) < dptol&
                    , 'mean(d3_int64, 3, d3_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 1, d4_int64 > 0) -&
                    sum(real(d4_int64, dp), 1, d4_int64 > 0)/real(count(d4_int64 > 0, 1), dp))) < dptol&
                    , 'mean(d4_int64, 1, d4_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 2, d4_int64 > 0) -&
                    sum(real(d4_int64, dp), 2, d4_int64 > 0)/real(count(d4_int64 > 0, 2), dp))) < dptol&
                    , 'mean(d4_int64, 2, d4_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 3, d4_int64 > 0) -&
                    sum(real(d4_int64, dp), 3, d4_int64 > 0)/real(count(d4_int64 > 0, 3), dp))) < dptol&
                    , 'mean(d4_int64, 3, d4_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_int64, 4, d4_int64 > 0) -&
                    sum(real(d4_int64, dp), 4, d4_int64 > 0)/real(count(d4_int64 > 0, 4), dp))) < dptol&
                    , 'mean(d4_int64, 4, d4_int64 > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_sp), sum(d1_sp)/real(size(d1_sp), sp)&
                    , 'mean(d1_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d2_sp), sum(d2_sp)/real(size(d2_sp), sp)&
                    , 'mean(d2_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d3_sp), sum(d3_sp)/real(size(d3_sp), sp)&
                    , 'mean(d3_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d4_sp), sum(d4_sp)/real(size(d4_sp), sp)&
                    , 'mean(d4_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_sp, .false.))&
                    , 'mean(d1_sp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d2_sp, .false.))&
                    , 'mean(d2_sp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d3_sp, .false.))&
                    , 'mean(d3_sp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d4_sp, .false.))&
                    , 'mean(d4_sp, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_sp, 1) - sum(d1_sp, 1)/real(size(d1_sp, 1), sp)) <sptol&
                    , 'mean(d1_sp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_sp, 1) -&
                    sum(d2_sp, 1)/real(size(d2_sp, 1), sp))) < sptol&
                    , 'mean(d2_sp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_sp, 2) -&
                    sum(d2_sp, 2)/real(size(d2_sp, 2), sp))) < sptol&
                    , 'mean(d2_sp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_sp, 1) -&
                    sum(d3_sp, 1)/real(size(d3_sp, 1), sp))) < sptol&
                    , 'mean(d3_sp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_sp, 2) -&
                    sum(d3_sp, 2)/real(size(d3_sp, 2), sp))) < sptol&
                    , 'mean(d3_sp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_sp, 3) -&
                    sum(d3_sp, 3)/real(size(d3_sp, 3), sp))) < sptol&
                    , 'mean(d3_sp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 1) -&
                    sum(d4_sp, 1)/real(size(d4_sp, 1), sp))) < sptol&
                    , 'mean(d4_sp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 2) -&
                    sum(d4_sp, 2)/real(size(d4_sp, 2), sp))) < sptol&
                    , 'mean(d4_sp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 3) -&
                    sum(d4_sp, 3)/real(size(d4_sp, 3), sp))) < sptol&
                    , 'mean(d4_sp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 4) -&
                    sum(d4_sp, 4)/real(size(d4_sp, 4), sp))) < sptol&
                    , 'mean(d4_sp, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_sp, 1, .false.))&
                    , 'mean(d1_sp, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d2_sp, 1, .false.)))&
                    , 'mean(d2_sp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d2_sp, 2, .false.)))&
                    , 'mean(d2_sp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_sp, 1, .false.)))&
                    , 'mean(d3_sp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_sp, 2, .false.)))&
                    , 'mean(d3_sp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_sp, 3, .false.)))&
                    , 'mean(d3_sp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_sp, 1, .false.)))&
                    , 'mean(d4_sp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_sp, 2, .false.)))&
                    , 'mean(d4_sp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_sp, 3, .false.)))&
                    , 'mean(d4_sp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_sp, 4, .false.)))&
                    , 'mean(d4_sp, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_sp, d1_sp > 0)&
                    , sum(d1_sp, d1_sp > 0)/real(count(d1_sp > 0), sp)&
                    , 'mean(d1_sp, d1_sp > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d2_sp, d2_sp > 0)&
                    , sum(d2_sp, d2_sp > 0)/real(count(d2_sp > 0), sp)&
                    , 'mean(d2_sp, d2_sp > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d3_sp, d3_sp > 0)&
                    , sum(d3_sp, d3_sp > 0)/real(count(d3_sp > 0), sp)&
                    , 'mean(d3_sp, d3_sp > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d4_sp, d4_sp > 0)&
                    , sum(d4_sp, d4_sp > 0)/real(count(d4_sp > 0), sp)&
                    , 'mean(d4_sp, d4_sp > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_sp, 1, d1_sp > 0) -&
                    sum(d1_sp, 1, d1_sp > 0)/real(count(d1_sp > 0, 1), sp)) < sptol&
                    , 'mean(d1_sp, 1, d1_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_sp, 1, d2_sp > 0) -&
                    sum(d2_sp, 1, d2_sp > 0)/real(count(d2_sp > 0, 1), sp))) < sptol&
                    , 'mean(d2_sp, 1, d2_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_sp, 2, d2_sp > 0) -&
                    sum(d2_sp, 2, d2_sp > 0)/real(count(d2_sp > 0, 2), sp))) < sptol&
                    , 'mean(d2_sp, 2, d2_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_sp, 1, d3_sp > 0) -&
                    sum(d3_sp, 1, d3_sp > 0)/real(count(d3_sp > 0, 1), sp))) < sptol&
                    , 'mean(d3_sp, 1, d3_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_sp, 2, d3_sp > 0) -&
                    sum(d3_sp, 2, d3_sp > 0)/real(count(d3_sp > 0, 2), sp))) < sptol&
                    , 'mean(d3_sp, 2, d3_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_sp, 3, d3_sp > 0) -&
                    sum(d3_sp, 3, d3_sp > 0)/real(count(d3_sp > 0, 3), sp))) < sptol&
                    , 'mean(d3_sp, 3, d3_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 1, d4_sp > 0) -&
                    sum(d4_sp, 1, d4_sp > 0)/real(count(d4_sp > 0, 1), sp))) < sptol&
                    , 'mean(d4_sp, 1, d4_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 2, d4_sp > 0) -&
                    sum(d4_sp, 2, d4_sp > 0)/real(count(d4_sp > 0, 2), sp))) < sptol&
                    , 'mean(d4_sp, 2, d4_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 3, d4_sp > 0) -&
                    sum(d4_sp, 3, d4_sp > 0)/real(count(d4_sp > 0, 3), sp))) < sptol&
                    , 'mean(d4_sp, 3, d4_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_sp, 4, d4_sp > 0) -&
                    sum(d4_sp, 4, d4_sp > 0)/real(count(d4_sp > 0, 4), sp))) < sptol&
                    , 'mean(d4_sp, 4, d4_sp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine
    subroutine test_stats_mean_all_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_dp), sum(d1_dp)/real(size(d1_dp), dp)&
                    , 'mean(d1_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_dp), sum(d2_dp)/real(size(d2_dp), dp)&
                    , 'mean(d2_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_dp), sum(d3_dp)/real(size(d3_dp), dp)&
                    , 'mean(d3_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_dp), sum(d4_dp)/real(size(d4_dp), dp)&
                    , 'mean(d4_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_dp, .false.))&
                    , 'mean(d1_dp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d2_dp, .false.))&
                    , 'mean(d2_dp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d3_dp, .false.))&
                    , 'mean(d3_dp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(mean(d4_dp, .false.))&
                    , 'mean(d4_dp, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_dp, 1) - sum(d1_dp, 1)/real(size(d1_dp, 1), dp)) <dptol&
                    , 'mean(d1_dp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_dp, 1) -&
                    sum(d2_dp, 1)/real(size(d2_dp, 1), dp))) < dptol&
                    , 'mean(d2_dp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_dp, 2) -&
                    sum(d2_dp, 2)/real(size(d2_dp, 2), dp))) < dptol&
                    , 'mean(d2_dp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_dp, 1) -&
                    sum(d3_dp, 1)/real(size(d3_dp, 1), dp))) < dptol&
                    , 'mean(d3_dp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_dp, 2) -&
                    sum(d3_dp, 2)/real(size(d3_dp, 2), dp))) < dptol&
                    , 'mean(d3_dp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_dp, 3) -&
                    sum(d3_dp, 3)/real(size(d3_dp, 3), dp))) < dptol&
                    , 'mean(d3_dp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 1) -&
                    sum(d4_dp, 1)/real(size(d4_dp, 1), dp))) < dptol&
                    , 'mean(d4_dp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 2) -&
                    sum(d4_dp, 2)/real(size(d4_dp, 2), dp))) < dptol&
                    , 'mean(d4_dp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 3) -&
                    sum(d4_dp, 3)/real(size(d4_dp, 3), dp))) < dptol&
                    , 'mean(d4_dp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 4) -&
                    sum(d4_dp, 4)/real(size(d4_dp, 4), dp))) < dptol&
                    , 'mean(d4_dp, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(mean(d1_dp, 1, .false.))&
                    , 'mean(d1_dp, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(mean(d2_dp, 1, .false.)))&
                    , 'mean(d2_dp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d2_dp, 2, .false.)))&
                    , 'mean(d2_dp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_dp, 1, .false.)))&
                    , 'mean(d3_dp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_dp, 2, .false.)))&
                    , 'mean(d3_dp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d3_dp, 3, .false.)))&
                    , 'mean(d3_dp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_dp, 1, .false.)))&
                    , 'mean(d4_dp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_dp, 2, .false.)))&
                    , 'mean(d4_dp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_dp, 3, .false.)))&
                    , 'mean(d4_dp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(mean(d4_dp, 4, .false.)))&
                    , 'mean(d4_dp, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_dp, d1_dp > 0)&
                    , sum(d1_dp, d1_dp > 0)/real(count(d1_dp > 0), dp)&
                    , 'mean(d1_dp, d1_dp > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_dp, d2_dp > 0)&
                    , sum(d2_dp, d2_dp > 0)/real(count(d2_dp > 0), dp)&
                    , 'mean(d2_dp, d2_dp > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_dp, d3_dp > 0)&
                    , sum(d3_dp, d3_dp > 0)/real(count(d3_dp > 0), dp)&
                    , 'mean(d3_dp, d3_dp > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_dp, d4_dp > 0)&
                    , sum(d4_dp, d4_dp > 0)/real(count(d4_dp > 0), dp)&
                    , 'mean(d4_dp, d4_dp > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_dp, 1, d1_dp > 0) -&
                    sum(d1_dp, 1, d1_dp > 0)/real(count(d1_dp > 0, 1), dp)) < dptol&
                    , 'mean(d1_dp, 1, d1_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_dp, 1, d2_dp > 0) -&
                    sum(d2_dp, 1, d2_dp > 0)/real(count(d2_dp > 0, 1), dp))) < dptol&
                    , 'mean(d2_dp, 1, d2_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_dp, 2, d2_dp > 0) -&
                    sum(d2_dp, 2, d2_dp > 0)/real(count(d2_dp > 0, 2), dp))) < dptol&
                    , 'mean(d2_dp, 2, d2_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_dp, 1, d3_dp > 0) -&
                    sum(d3_dp, 1, d3_dp > 0)/real(count(d3_dp > 0, 1), dp))) < dptol&
                    , 'mean(d3_dp, 1, d3_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_dp, 2, d3_dp > 0) -&
                    sum(d3_dp, 2, d3_dp > 0)/real(count(d3_dp > 0, 2), dp))) < dptol&
                    , 'mean(d3_dp, 2, d3_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_dp, 3, d3_dp > 0) -&
                    sum(d3_dp, 3, d3_dp > 0)/real(count(d3_dp > 0, 3), dp))) < dptol&
                    , 'mean(d3_dp, 3, d3_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 1, d4_dp > 0) -&
                    sum(d4_dp, 1, d4_dp > 0)/real(count(d4_dp > 0, 1), dp))) < dptol&
                    , 'mean(d4_dp, 1, d4_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 2, d4_dp > 0) -&
                    sum(d4_dp, 2, d4_dp > 0)/real(count(d4_dp > 0, 2), dp))) < dptol&
                    , 'mean(d4_dp, 2, d4_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 3, d4_dp > 0) -&
                    sum(d4_dp, 3, d4_dp > 0)/real(count(d4_dp > 0, 3), dp))) < dptol&
                    , 'mean(d4_dp, 3, d4_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_dp, 4, d4_dp > 0) -&
                    sum(d4_dp, 4, d4_dp > 0)/real(count(d4_dp > 0, 4), dp))) < dptol&
                    , 'mean(d4_dp, 4, d4_dp > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_csp), sum(d1_csp)/real(size(d1_csp), sp)&
                    , 'mean(d1_csp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d2_csp), sum(d2_csp)/real(size(d2_csp), sp)&
                    , 'mean(d2_csp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d3_csp), sum(d3_csp)/real(size(d3_csp), sp)&
                    , 'mean(d3_csp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d4_csp), sum(d4_csp)/real(size(d4_csp), sp)&
                    , 'mean(d4_csp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(real(mean(d1_csp, .false.)))&
                    , 'mean(d1_csp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(real(mean(d2_csp, .false.)))&
                    , 'mean(d2_csp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(real(mean(d3_csp, .false.)))&
                    , 'mean(d3_csp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(real(mean(d4_csp, .false.)))&
                    , 'mean(d4_csp, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_csp, 1) - sum(d1_csp, 1)/real(size(d1_csp, 1), sp)) <sptol&
                    , 'mean(d1_csp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_csp, 1) -&
                    sum(d2_csp, 1)/real(size(d2_csp, 1), sp))) < sptol&
                    , 'mean(d2_csp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_csp, 2) -&
                    sum(d2_csp, 2)/real(size(d2_csp, 2), sp))) < sptol&
                    , 'mean(d2_csp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_csp, 1) -&
                    sum(d3_csp, 1)/real(size(d3_csp, 1), sp))) < sptol&
                    , 'mean(d3_csp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_csp, 2) -&
                    sum(d3_csp, 2)/real(size(d3_csp, 2), sp))) < sptol&
                    , 'mean(d3_csp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_csp, 3) -&
                    sum(d3_csp, 3)/real(size(d3_csp, 3), sp))) < sptol&
                    , 'mean(d3_csp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 1) -&
                    sum(d4_csp, 1)/real(size(d4_csp, 1), sp))) < sptol&
                    , 'mean(d4_csp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 2) -&
                    sum(d4_csp, 2)/real(size(d4_csp, 2), sp))) < sptol&
                    , 'mean(d4_csp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 3) -&
                    sum(d4_csp, 3)/real(size(d4_csp, 3), sp))) < sptol&
                    , 'mean(d4_csp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 4) -&
                    sum(d4_csp, 4)/real(size(d4_csp, 4), sp))) < sptol&
                    , 'mean(d4_csp, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(real(mean(d1_csp, 1, .false.)))&
                    , 'mean(d1_csp, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(real(mean(d2_csp, 1, .false.))))&
                    , 'mean(d2_csp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d2_csp, 2, .false.))))&
                    , 'mean(d2_csp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d3_csp, 1, .false.))))&
                    , 'mean(d3_csp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d3_csp, 2, .false.))))&
                    , 'mean(d3_csp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d3_csp, 3, .false.))))&
                    , 'mean(d3_csp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_csp, 1, .false.))))&
                    , 'mean(d4_csp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_csp, 2, .false.))))&
                    , 'mean(d4_csp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_csp, 3, .false.))))&
                    , 'mean(d4_csp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_csp, 4, .false.))))&
                    , 'mean(d4_csp, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_csp, d1_csp%re > 0)&
                    , sum(d1_csp, d1_csp%re > 0)/real(count(d1_csp%re > 0), sp)&
                    , 'mean(d1_csp, d1_csp%re > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d2_csp, d2_csp%re > 0)&
                    , sum(d2_csp, d2_csp%re > 0)/real(count(d2_csp%re > 0), sp)&
                    , 'mean(d2_csp, d2_csp%re > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d3_csp, d3_csp%re > 0)&
                    , sum(d3_csp, d3_csp%re > 0)/real(count(d3_csp%re > 0), sp)&
                    , 'mean(d3_csp, d3_csp%re > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, mean(d4_csp, d4_csp%re > 0)&
                    , sum(d4_csp, d4_csp%re > 0)/real(count(d4_csp%re > 0), sp)&
                    , 'mean(d4_csp, d4_csp%re > 0): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_csp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_csp, 1, d1_csp%re > 0) -&
                    sum(d1_csp, 1, d1_csp%re > 0)/real(count(d1_csp%re > 0, 1), sp)) < sptol&
                    , 'mean(d1_csp, 1, d1_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_csp, 1, d2_csp%re > 0) -&
                    sum(d2_csp, 1, d2_csp%re > 0)/real(count(d2_csp%re > 0, 1), sp))) < sptol&
                    , 'mean(d2_csp, 1, d2_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_csp, 2, d2_csp%re > 0) -&
                    sum(d2_csp, 2, d2_csp%re > 0)/real(count(d2_csp%re > 0, 2), sp))) < sptol&
                    , 'mean(d2_csp, 2, d2_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_csp, 1, d3_csp%re > 0) -&
                    sum(d3_csp, 1, d3_csp%re > 0)/real(count(d3_csp%re > 0, 1), sp))) < sptol&
                    , 'mean(d3_csp, 1, d3_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_csp, 2, d3_csp%re > 0) -&
                    sum(d3_csp, 2, d3_csp%re > 0)/real(count(d3_csp%re > 0, 2), sp))) < sptol&
                    , 'mean(d3_csp, 2, d3_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_csp, 3, d3_csp%re > 0) -&
                    sum(d3_csp, 3, d3_csp%re > 0)/real(count(d3_csp%re > 0, 3), sp))) < sptol&
                    , 'mean(d3_csp, 3, d3_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 1, d4_csp%re > 0) -&
                    sum(d4_csp, 1, d4_csp%re > 0)/real(count(d4_csp%re > 0, 1), sp))) < sptol&
                    , 'mean(d4_csp, 1, d4_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 2, d4_csp%re > 0) -&
                    sum(d4_csp, 2, d4_csp%re > 0)/real(count(d4_csp%re > 0, 2), sp))) < sptol&
                    , 'mean(d4_csp, 2, d4_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 3, d4_csp%re > 0) -&
                    sum(d4_csp, 3, d4_csp%re > 0)/real(count(d4_csp%re > 0, 3), sp))) < sptol&
                    , 'mean(d4_csp, 3, d4_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_csp, 4, d4_csp%re > 0) -&
                    sum(d4_csp, 4, d4_csp%re > 0)/real(count(d4_csp%re > 0, 4), sp))) < sptol&
                    , 'mean(d4_csp, 4, d4_csp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine
    subroutine test_stats_mean_all_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_cdp), sum(d1_cdp)/real(size(d1_cdp), dp)&
                    , 'mean(d1_cdp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_cdp), sum(d2_cdp)/real(size(d2_cdp), dp)&
                    , 'mean(d2_cdp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_cdp), sum(d3_cdp)/real(size(d3_cdp), dp)&
                    , 'mean(d3_cdp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_cdp), sum(d4_cdp)/real(size(d4_cdp), dp)&
                    , 'mean(d4_cdp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_all_optmask_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(real(mean(d1_cdp, .false.)))&
                    , 'mean(d1_cdp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(real(mean(d2_cdp, .false.)))&
                    , 'mean(d2_cdp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(real(mean(d3_cdp, .false.)))&
                    , 'mean(d3_cdp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(real(mean(d4_cdp, .false.)))&
                    , 'mean(d4_cdp, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_cdp, 1) - sum(d1_cdp, 1)/real(size(d1_cdp, 1), dp)) <dptol&
                    , 'mean(d1_cdp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_cdp, 1) -&
                    sum(d2_cdp, 1)/real(size(d2_cdp, 1), dp))) < dptol&
                    , 'mean(d2_cdp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_cdp, 2) -&
                    sum(d2_cdp, 2)/real(size(d2_cdp, 2), dp))) < dptol&
                    , 'mean(d2_cdp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_cdp, 1) -&
                    sum(d3_cdp, 1)/real(size(d3_cdp, 1), dp))) < dptol&
                    , 'mean(d3_cdp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_cdp, 2) -&
                    sum(d3_cdp, 2)/real(size(d3_cdp, 2), dp))) < dptol&
                    , 'mean(d3_cdp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_cdp, 3) -&
                    sum(d3_cdp, 3)/real(size(d3_cdp, 3), dp))) < dptol&
                    , 'mean(d3_cdp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 1) -&
                    sum(d4_cdp, 1)/real(size(d4_cdp, 1), dp))) < dptol&
                    , 'mean(d4_cdp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 2) -&
                    sum(d4_cdp, 2)/real(size(d4_cdp, 2), dp))) < dptol&
                    , 'mean(d4_cdp, 2): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 3) -&
                    sum(d4_cdp, 3)/real(size(d4_cdp, 3), dp))) < dptol&
                    , 'mean(d4_cdp, 3): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 4) -&
                    sum(d4_cdp, 4)/real(size(d4_cdp, 4), dp))) < dptol&
                    , 'mean(d4_cdp, 4): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_optmask_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(real(mean(d1_cdp, 1, .false.)))&
                    , 'mean(d1_cdp, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(real(mean(d2_cdp, 1, .false.))))&
                    , 'mean(d2_cdp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d2_cdp, 2, .false.))))&
                    , 'mean(d2_cdp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d3_cdp, 1, .false.))))&
                    , 'mean(d3_cdp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d3_cdp, 2, .false.))))&
                    , 'mean(d3_cdp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d3_cdp, 3, .false.))))&
                    , 'mean(d3_cdp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_cdp, 1, .false.))))&
                    , 'mean(d4_cdp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_cdp, 2, .false.))))&
                    , 'mean(d4_cdp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_cdp, 3, .false.))))&
                    , 'mean(d4_cdp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(real(mean(d4_cdp, 4, .false.))))&
                    , 'mean(d4_cdp, 4, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_all_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mean(d1_cdp, d1_cdp%re > 0)&
                    , sum(d1_cdp, d1_cdp%re > 0)/real(count(d1_cdp%re > 0), dp)&
                    , 'mean(d1_cdp, d1_cdp%re > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d2_cdp, d2_cdp%re > 0)&
                    , sum(d2_cdp, d2_cdp%re > 0)/real(count(d2_cdp%re > 0), dp)&
                    , 'mean(d2_cdp, d2_cdp%re > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d3_cdp, d3_cdp%re > 0)&
                    , sum(d3_cdp, d3_cdp%re > 0)/real(count(d3_cdp%re > 0), dp)&
                    , 'mean(d3_cdp, d3_cdp%re > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, mean(d4_cdp, d4_cdp%re > 0)&
                    , sum(d4_cdp, d4_cdp%re > 0)/real(count(d4_cdp%re > 0), dp)&
                    , 'mean(d4_cdp, d4_cdp%re > 0): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_mean_mask_cdp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(mean(d1_cdp, 1, d1_cdp%re > 0) -&
                    sum(d1_cdp, 1, d1_cdp%re > 0)/real(count(d1_cdp%re > 0, 1), dp)) < dptol&
                    , 'mean(d1_cdp, 1, d1_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(mean(d2_cdp, 1, d2_cdp%re > 0) -&
                    sum(d2_cdp, 1, d2_cdp%re > 0)/real(count(d2_cdp%re > 0, 1), dp))) < dptol&
                    , 'mean(d2_cdp, 1, d2_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d2_cdp, 2, d2_cdp%re > 0) -&
                    sum(d2_cdp, 2, d2_cdp%re > 0)/real(count(d2_cdp%re > 0, 2), dp))) < dptol&
                    , 'mean(d2_cdp, 2, d2_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_cdp, 1, d3_cdp%re > 0) -&
                    sum(d3_cdp, 1, d3_cdp%re > 0)/real(count(d3_cdp%re > 0, 1), dp))) < dptol&
                    , 'mean(d3_cdp, 1, d3_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_cdp, 2, d3_cdp%re > 0) -&
                    sum(d3_cdp, 2, d3_cdp%re > 0)/real(count(d3_cdp%re > 0, 2), dp))) < dptol&
                    , 'mean(d3_cdp, 2, d3_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d3_cdp, 3, d3_cdp%re > 0) -&
                    sum(d3_cdp, 3, d3_cdp%re > 0)/real(count(d3_cdp%re > 0, 3), dp))) < dptol&
                    , 'mean(d3_cdp, 3, d3_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 1, d4_cdp%re > 0) -&
                    sum(d4_cdp, 1, d4_cdp%re > 0)/real(count(d4_cdp%re > 0, 1), dp))) < dptol&
                    , 'mean(d4_cdp, 1, d4_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 2, d4_cdp%re > 0) -&
                    sum(d4_cdp, 2, d4_cdp%re > 0)/real(count(d4_cdp%re > 0, 2), dp))) < dptol&
                    , 'mean(d4_cdp, 2, d4_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 3, d4_cdp%re > 0) -&
                    sum(d4_cdp, 3, d4_cdp%re > 0)/real(count(d4_cdp%re > 0, 3), dp))) < dptol&
                    , 'mean(d4_cdp, 3, d4_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
        call check(error&
                    , sum(abs(mean(d4_cdp, 4, d4_cdp%re > 0) -&
                    sum(d4_cdp, 4, d4_cdp%re > 0)/real(count(d4_cdp%re > 0, 4), dp))) < dptol&
                    , 'mean(d4_cdp, 4, d4_cdp%re > 0): uncorrect answer'&
                    )
        if (allocated(error)) return
    end subroutine

end module test_stats_mean

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stats_mean, only : collect_stats_mean
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stats_mean", collect_stats_mean) &
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
