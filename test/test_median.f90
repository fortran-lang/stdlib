

module test_stats_median
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_stats, only: median
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp, xdp, qp
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none
    private

    public :: collect_stats_median

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 2000 * epsilon(1._dp)

    integer(int8) , parameter :: d1_int8(12) = [integer(int8) :: 10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    integer(int8) :: d2_int8(3, 4) = reshape(d1_int8, [3, 4])
    integer(int8) :: d3_int8(2, 3, 2) = reshape(d1_int8, [2, 3, 2])
    integer(int8) , parameter :: d1odd_int8(13) = [integer(int8) :: d1_int8, 20]
    integer(int8) :: d2odd_int8(3, 5) = reshape(d1odd_int8, [3, 5], [integer(int8) :: 0])
    integer(int8) :: d3odd_int8(1, 3, 5) = reshape(d1odd_int8, [1, 3, 5], [integer(int8) :: 0])
    integer(int16) , parameter :: d1_int16(12) = [integer(int16) :: 10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    integer(int16) :: d2_int16(3, 4) = reshape(d1_int16, [3, 4])
    integer(int16) :: d3_int16(2, 3, 2) = reshape(d1_int16, [2, 3, 2])
    integer(int16) , parameter :: d1odd_int16(13) = [integer(int16) :: d1_int16, 20]
    integer(int16) :: d2odd_int16(3, 5) = reshape(d1odd_int16, [3, 5], [integer(int16) :: 0])
    integer(int16) :: d3odd_int16(1, 3, 5) = reshape(d1odd_int16, [1, 3, 5], [integer(int16) :: 0])
    integer(int32) , parameter :: d1_int32(12) = [integer(int32) :: 10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    integer(int32) :: d2_int32(3, 4) = reshape(d1_int32, [3, 4])
    integer(int32) :: d3_int32(2, 3, 2) = reshape(d1_int32, [2, 3, 2])
    integer(int32) , parameter :: d1odd_int32(13) = [integer(int32) :: d1_int32, 20]
    integer(int32) :: d2odd_int32(3, 5) = reshape(d1odd_int32, [3, 5], [integer(int32) :: 0])
    integer(int32) :: d3odd_int32(1, 3, 5) = reshape(d1odd_int32, [1, 3, 5], [integer(int32) :: 0])
    integer(int64) , parameter :: d1_int64(12) = [integer(int64) :: 10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    integer(int64) :: d2_int64(3, 4) = reshape(d1_int64, [3, 4])
    integer(int64) :: d3_int64(2, 3, 2) = reshape(d1_int64, [2, 3, 2])
    integer(int64) , parameter :: d1odd_int64(13) = [integer(int64) :: d1_int64, 20]
    integer(int64) :: d2odd_int64(3, 5) = reshape(d1odd_int64, [3, 5], [integer(int64) :: 0])
    integer(int64) :: d3odd_int64(1, 3, 5) = reshape(d1odd_int64, [1, 3, 5], [integer(int64) :: 0])
    real(sp) , parameter :: d1_sp(12) = [real(sp) :: 10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    real(sp) :: d2_sp(3, 4) = reshape(d1_sp, [3, 4])
    real(sp) :: d3_sp(2, 3, 2) = reshape(d1_sp, [2, 3, 2])
    real(sp) , parameter :: d1odd_sp(13) = [real(sp) :: d1_sp, 20]
    real(sp) :: d2odd_sp(3, 5) = reshape(d1odd_sp, [3, 5], [real(sp) :: 0])
    real(sp) :: d3odd_sp(1, 3, 5) = reshape(d1odd_sp, [1, 3, 5], [real(sp) :: 0])
    real(dp) , parameter :: d1_dp(12) = [real(dp) :: 10, 2, -3, -4, 6, -6, 7, -8, 9, 0, 1, 20]
    real(dp) :: d2_dp(3, 4) = reshape(d1_dp, [3, 4])
    real(dp) :: d3_dp(2, 3, 2) = reshape(d1_dp, [2, 3, 2])
    real(dp) , parameter :: d1odd_dp(13) = [real(dp) :: d1_dp, 20]
    real(dp) :: d2odd_dp(3, 5) = reshape(d1odd_dp, [3, 5], [real(dp) :: 0])
    real(dp) :: d3odd_dp(1, 3, 5) = reshape(d1odd_dp, [1, 3, 5], [real(dp) :: 0])


contains

    !> Collect all exported unit tests
    subroutine collect_stats_median(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("test_stats_median_size_int8", test_stats_median_size_int8) &
            , new_unittest("test_stats_median_size_int8", test_stats_median_size_int8) &
            , new_unittest("test_stats_median_odd_size_int8", test_stats_median_odd_size_int8) &
            , new_unittest("test_stats_median_all_int8", test_stats_median_all_int8) &
            , new_unittest("test_stats_median_all_odd_int8", test_stats_median_all_odd_int8) &
            , new_unittest("test_stats_median_all_optmask_int8", test_stats_median_all_optmask_int8) &
            , new_unittest("test_stats_median_int8", test_stats_median_int8) &
            , new_unittest("test_stats_median_odd_int8", test_stats_median_odd_int8) &
            , new_unittest("test_stats_median_optmask_int8", test_stats_median_optmask_int8) &
            , new_unittest("test_stats_median_mask_all_int8", test_stats_median_mask_all_int8) &
            , new_unittest("test_stats_median_mask_int8", test_stats_median_mask_int8) &
            , new_unittest("test_stats_median_size_int16", test_stats_median_size_int16) &
            , new_unittest("test_stats_median_odd_size_int16", test_stats_median_odd_size_int16) &
            , new_unittest("test_stats_median_all_int16", test_stats_median_all_int16) &
            , new_unittest("test_stats_median_all_odd_int16", test_stats_median_all_odd_int16) &
            , new_unittest("test_stats_median_all_optmask_int16", test_stats_median_all_optmask_int16) &
            , new_unittest("test_stats_median_int16", test_stats_median_int16) &
            , new_unittest("test_stats_median_odd_int16", test_stats_median_odd_int16) &
            , new_unittest("test_stats_median_optmask_int16", test_stats_median_optmask_int16) &
            , new_unittest("test_stats_median_mask_all_int16", test_stats_median_mask_all_int16) &
            , new_unittest("test_stats_median_mask_int16", test_stats_median_mask_int16) &
            , new_unittest("test_stats_median_size_int32", test_stats_median_size_int32) &
            , new_unittest("test_stats_median_odd_size_int32", test_stats_median_odd_size_int32) &
            , new_unittest("test_stats_median_all_int32", test_stats_median_all_int32) &
            , new_unittest("test_stats_median_all_odd_int32", test_stats_median_all_odd_int32) &
            , new_unittest("test_stats_median_all_optmask_int32", test_stats_median_all_optmask_int32) &
            , new_unittest("test_stats_median_int32", test_stats_median_int32) &
            , new_unittest("test_stats_median_odd_int32", test_stats_median_odd_int32) &
            , new_unittest("test_stats_median_optmask_int32", test_stats_median_optmask_int32) &
            , new_unittest("test_stats_median_mask_all_int32", test_stats_median_mask_all_int32) &
            , new_unittest("test_stats_median_mask_int32", test_stats_median_mask_int32) &
            , new_unittest("test_stats_median_size_int64", test_stats_median_size_int64) &
            , new_unittest("test_stats_median_odd_size_int64", test_stats_median_odd_size_int64) &
            , new_unittest("test_stats_median_all_int64", test_stats_median_all_int64) &
            , new_unittest("test_stats_median_all_odd_int64", test_stats_median_all_odd_int64) &
            , new_unittest("test_stats_median_all_optmask_int64", test_stats_median_all_optmask_int64) &
            , new_unittest("test_stats_median_int64", test_stats_median_int64) &
            , new_unittest("test_stats_median_odd_int64", test_stats_median_odd_int64) &
            , new_unittest("test_stats_median_optmask_int64", test_stats_median_optmask_int64) &
            , new_unittest("test_stats_median_mask_all_int64", test_stats_median_mask_all_int64) &
            , new_unittest("test_stats_median_mask_int64", test_stats_median_mask_int64) &
            , new_unittest("test_stats_median_size_sp", test_stats_median_size_sp) &
            , new_unittest("test_stats_median_odd_size_sp", test_stats_median_odd_size_sp) &
            , new_unittest("test_stats_median_all_sp", test_stats_median_all_sp) &
            , new_unittest("test_stats_median_all_odd_sp", test_stats_median_all_odd_sp) &
            , new_unittest("test_stats_median_all_optmask_sp", test_stats_median_all_optmask_sp) &
            , new_unittest("test_stats_median_sp", test_stats_median_sp) &
            , new_unittest("test_stats_median_odd_sp", test_stats_median_odd_sp) &
            , new_unittest("test_stats_median_optmask_sp", test_stats_median_optmask_sp) &
            , new_unittest("test_stats_median_mask_all_sp", test_stats_median_mask_all_sp) &
            , new_unittest("test_stats_median_mask_sp", test_stats_median_mask_sp) &
            , new_unittest("test_stats_median_size_dp", test_stats_median_size_dp) &
            , new_unittest("test_stats_median_odd_size_dp", test_stats_median_odd_size_dp) &
            , new_unittest("test_stats_median_all_dp", test_stats_median_all_dp) &
            , new_unittest("test_stats_median_all_odd_dp", test_stats_median_all_odd_dp) &
            , new_unittest("test_stats_median_all_optmask_dp", test_stats_median_all_optmask_dp) &
            , new_unittest("test_stats_median_dp", test_stats_median_dp) &
            , new_unittest("test_stats_median_odd_dp", test_stats_median_odd_dp) &
            , new_unittest("test_stats_median_optmask_dp", test_stats_median_optmask_dp) &
            , new_unittest("test_stats_median_mask_all_dp", test_stats_median_mask_all_dp) &
            , new_unittest("test_stats_median_mask_dp", test_stats_median_mask_dp) &
            ]
    end subroutine collect_stats_median

    subroutine test_stats_median_size_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), allocatable :: d0(:)

        allocate(d0(0))
        !check just to be sure that the setup of d0 is correct
        call check(error, size(d0), 0, 'size(d0): should be of size 0')

        call check(error, mod(size(d1_int8), 2), 0&
                    , 'mod(size(d1_int8), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2_int8), 2), 0&
                    , 'mod(size(d2_int8), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3_int8), 2), 0&
                    , 'mod(size(d3_int8), 2): should be an even number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_odd_size_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mod(size(d1odd_int8), 2), 1&
                    , 'mod(size(d1)_int8, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2odd_int8), 2), 1&
                    , 'mod(size(d2)_int8, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3odd_int8), 2), 1&
                    , 'mod(size(d3)_int8, 2): should be an odd number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), allocatable :: d0(:)

        allocate(d0(0))
        call check(error, ieee_is_nan(median(d0)), 'median(d0): should be NaN' )
        if (allocated(error)) return

        call check(error, median(d1_int8),  1.5_dp&
                    , 'median(d1_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d2_int8),  1.5_dp&
                    , 'median(d2_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d3_int8),  1.5_dp&
                    , 'median(d3_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_odd_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, median(d1odd_int8),  2._dp&
                    , 'median(d1odd_int8): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, median(d2odd_int8),  1._dp&
                    , 'median(d2odd_int8): uncorrect answer'&
                    , thr = dptol)

    end subroutine

    subroutine test_stats_median_all_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), allocatable :: d0_int8(:)

        allocate(d0_int8(0))

        call check(error, ieee_is_nan(median(d0_int8, .false.))&
                    , 'median(d0_int8, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d1_int8, .false.))&
                    , 'median(d1_int8, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d2_int8, .false.))&
                    , 'median(d2_int8, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d3_int8, .false.))&
                    , 'median(d3_int8, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1)), 'median(d0, 1): should return NaN' )

        call check(error&
                    , abs(median(d1_int8, 1) - 1.5_dp) < dptol&
                    , 'median(d1_int8, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int8, 1) - [2._dp, -4._dp, 7._dp, 1._dp])) < dptol&
                    , 'median(d2_int8, 1): uncorrect answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int8, 2) - [3.5_dp, 1.5_dp, 3._dp])) < dptol&
                    ,'median(d2_int8, 2): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_odd_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(median(d1odd_int8, 1) - 2._dp) < dptol&
                    , 'median(d1odd_int8, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int8, 1) - [2._dp, -4._dp, 7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int8, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int8, 2) - [7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int8, 2): wrong answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_optmask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1, .false.))&
                    , 'median(d0, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, ieee_is_nan(median(d1_int8, 1, .false.))&
                    , 'median(d1_int8, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(median(d2_int8, 1, .false.)))&
                    , 'median(d2_int8, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d2_int8, 2, .false.)))&
                    , 'median(d2_int8, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int8, 1, .false.)))&
                    , 'median(d3_int8, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int8, 2, .false.)))&
                    , 'median(d3_int8, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int8, 3, .false.)))&
                    , 'median(d3_int8, 3, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_mask_all_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), allocatable :: d0(:)

        allocate(d0(0))

        call check(error,  ieee_is_nan(median(d0, d0 > 0))&
                    , 'median(d0, d0 > 0): should be NaN' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int8, d1_int8 > huge(d1_int8)))&
                    , 'median(d1_int8, d1_int8 > huge(d1_int8))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d2_int8, d2_int8 > huge(d2_int8)))&
                    , 'median(d2_int8, d2_int8 > huge(d2_int8))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d3_int8, d3_int8 > huge(d3_int8)))&
                    , 'median(d3_int8, d3_int8 > huge(d3_int8))' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int8, d1_int8 > 0) - 7._dp) < dptol&
                    , 'median(d1_int8, d1_int8> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d2_int8, d2_int8 > 0) - 7._dp) < dptol&
                    , 'median(d2_int8, d2_int8> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d3_int8, d3_int8 > 0) - 7._dp) < dptol&
                    , 'median(d3_int8, d3_int8> 0)' ) 
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_mask_int8(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int8), allocatable :: d0(:)

        allocate(d0(0))

        call check(error&
                   , ieee_is_nan(median(d0, 1, d0 > 0))&
                   , 'median(d0, 1, d0 > 0): uncorrect answer' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int8, 1, d1_int8 > huge(d1_int8)))&
                    , 'median(d1_int8, 1_int8, d1_int8 > huge(d1_int8)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d2_int8, 1, d2_int8 > huge(d2_int8))))&
                    , 'median(d2_int8, 1_int8, d2_int8 > huge(d2_int8)): answer should be IEEE NaN' )
        if (allocated(error)) return
        call check(error&
                    , any(ieee_is_nan(median(d3_int8, 1, d3_int8 > huge(d3_int8))))&
                    , 'median(d3_int8, 1_int8, d3_int8 > huge(d3_int8)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int8, 1, d1_int8 > 0) - 7._dp) < dptol&
                    , 'median(d1_int8, 1, d1_int8 >0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs( (median(d2_int8, 1, d2_int8 > 0) - [ 6._dp, 6._dp, 8._dp, 10.5_dp] )  )) &
                    < dptol&
                    , 'median(d2_int8, 1, d2_int8 > 0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs((median(d2_int8, 2, d2_int8 > 0) - [ 8.5_dp, 2._dp, 14.5_dp] )))&
                    < dptol&
                    , 'median(d2_int8, 2, d2_int8 > 0)') 
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d3_int8, 1, d3_int8 > 0)))&
                    , 'median(d3_int8, 1, d3_int8 > 0): should contain at least 1 IEEE NaN')

    end subroutine
    subroutine test_stats_median_size_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), allocatable :: d0(:)

        allocate(d0(0))
        !check just to be sure that the setup of d0 is correct
        call check(error, size(d0), 0, 'size(d0): should be of size 0')

        call check(error, mod(size(d1_int16), 2), 0&
                    , 'mod(size(d1_int16), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2_int16), 2), 0&
                    , 'mod(size(d2_int16), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3_int16), 2), 0&
                    , 'mod(size(d3_int16), 2): should be an even number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_odd_size_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mod(size(d1odd_int16), 2), 1&
                    , 'mod(size(d1)_int16, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2odd_int16), 2), 1&
                    , 'mod(size(d2)_int16, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3odd_int16), 2), 1&
                    , 'mod(size(d3)_int16, 2): should be an odd number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), allocatable :: d0(:)

        allocate(d0(0))
        call check(error, ieee_is_nan(median(d0)), 'median(d0): should be NaN' )
        if (allocated(error)) return

        call check(error, median(d1_int16),  1.5_dp&
                    , 'median(d1_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d2_int16),  1.5_dp&
                    , 'median(d2_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d3_int16),  1.5_dp&
                    , 'median(d3_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_odd_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, median(d1odd_int16),  2._dp&
                    , 'median(d1odd_int16): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, median(d2odd_int16),  1._dp&
                    , 'median(d2odd_int16): uncorrect answer'&
                    , thr = dptol)

    end subroutine

    subroutine test_stats_median_all_optmask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), allocatable :: d0_int16(:)

        allocate(d0_int16(0))

        call check(error, ieee_is_nan(median(d0_int16, .false.))&
                    , 'median(d0_int16, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d1_int16, .false.))&
                    , 'median(d1_int16, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d2_int16, .false.))&
                    , 'median(d2_int16, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d3_int16, .false.))&
                    , 'median(d3_int16, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1)), 'median(d0, 1): should return NaN' )

        call check(error&
                    , abs(median(d1_int16, 1) - 1.5_dp) < dptol&
                    , 'median(d1_int16, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int16, 1) - [2._dp, -4._dp, 7._dp, 1._dp])) < dptol&
                    , 'median(d2_int16, 1): uncorrect answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int16, 2) - [3.5_dp, 1.5_dp, 3._dp])) < dptol&
                    ,'median(d2_int16, 2): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_odd_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(median(d1odd_int16, 1) - 2._dp) < dptol&
                    , 'median(d1odd_int16, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int16, 1) - [2._dp, -4._dp, 7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int16, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int16, 2) - [7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int16, 2): wrong answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_optmask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1, .false.))&
                    , 'median(d0, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, ieee_is_nan(median(d1_int16, 1, .false.))&
                    , 'median(d1_int16, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(median(d2_int16, 1, .false.)))&
                    , 'median(d2_int16, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d2_int16, 2, .false.)))&
                    , 'median(d2_int16, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int16, 1, .false.)))&
                    , 'median(d3_int16, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int16, 2, .false.)))&
                    , 'median(d3_int16, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int16, 3, .false.)))&
                    , 'median(d3_int16, 3, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_mask_all_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), allocatable :: d0(:)

        allocate(d0(0))

        call check(error,  ieee_is_nan(median(d0, d0 > 0))&
                    , 'median(d0, d0 > 0): should be NaN' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int16, d1_int16 > huge(d1_int16)))&
                    , 'median(d1_int16, d1_int16 > huge(d1_int16))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d2_int16, d2_int16 > huge(d2_int16)))&
                    , 'median(d2_int16, d2_int16 > huge(d2_int16))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d3_int16, d3_int16 > huge(d3_int16)))&
                    , 'median(d3_int16, d3_int16 > huge(d3_int16))' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int16, d1_int16 > 0) - 7._dp) < dptol&
                    , 'median(d1_int16, d1_int16> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d2_int16, d2_int16 > 0) - 7._dp) < dptol&
                    , 'median(d2_int16, d2_int16> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d3_int16, d3_int16 > 0) - 7._dp) < dptol&
                    , 'median(d3_int16, d3_int16> 0)' ) 
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_mask_int16(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int16), allocatable :: d0(:)

        allocate(d0(0))

        call check(error&
                   , ieee_is_nan(median(d0, 1, d0 > 0))&
                   , 'median(d0, 1, d0 > 0): uncorrect answer' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int16, 1, d1_int16 > huge(d1_int16)))&
                    , 'median(d1_int16, 1_int16, d1_int16 > huge(d1_int16)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d2_int16, 1, d2_int16 > huge(d2_int16))))&
                    , 'median(d2_int16, 1_int16, d2_int16 > huge(d2_int16)): answer should be IEEE NaN' )
        if (allocated(error)) return
        call check(error&
                    , any(ieee_is_nan(median(d3_int16, 1, d3_int16 > huge(d3_int16))))&
                    , 'median(d3_int16, 1_int16, d3_int16 > huge(d3_int16)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int16, 1, d1_int16 > 0) - 7._dp) < dptol&
                    , 'median(d1_int16, 1, d1_int16 >0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs( (median(d2_int16, 1, d2_int16 > 0) - [ 6._dp, 6._dp, 8._dp, 10.5_dp] )  )) &
                    < dptol&
                    , 'median(d2_int16, 1, d2_int16 > 0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs((median(d2_int16, 2, d2_int16 > 0) - [ 8.5_dp, 2._dp, 14.5_dp] )))&
                    < dptol&
                    , 'median(d2_int16, 2, d2_int16 > 0)') 
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d3_int16, 1, d3_int16 > 0)))&
                    , 'median(d3_int16, 1, d3_int16 > 0): should contain at least 1 IEEE NaN')

    end subroutine
    subroutine test_stats_median_size_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int32), allocatable :: d0(:)

        allocate(d0(0))
        !check just to be sure that the setup of d0 is correct
        call check(error, size(d0), 0, 'size(d0): should be of size 0')

        call check(error, mod(size(d1_int32), 2), 0&
                    , 'mod(size(d1_int32), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2_int32), 2), 0&
                    , 'mod(size(d2_int32), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3_int32), 2), 0&
                    , 'mod(size(d3_int32), 2): should be an even number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_odd_size_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mod(size(d1odd_int32), 2), 1&
                    , 'mod(size(d1)_int32, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2odd_int32), 2), 1&
                    , 'mod(size(d2)_int32, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3odd_int32), 2), 1&
                    , 'mod(size(d3)_int32, 2): should be an odd number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int32), allocatable :: d0(:)

        allocate(d0(0))
        call check(error, ieee_is_nan(median(d0)), 'median(d0): should be NaN' )
        if (allocated(error)) return

        call check(error, median(d1_int32),  1.5_dp&
                    , 'median(d1_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d2_int32),  1.5_dp&
                    , 'median(d2_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d3_int32),  1.5_dp&
                    , 'median(d3_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_odd_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, median(d1odd_int32),  2._dp&
                    , 'median(d1odd_int32): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, median(d2odd_int32),  1._dp&
                    , 'median(d2odd_int32): uncorrect answer'&
                    , thr = dptol)

    end subroutine

    subroutine test_stats_median_all_optmask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int32), allocatable :: d0_int32(:)

        allocate(d0_int32(0))

        call check(error, ieee_is_nan(median(d0_int32, .false.))&
                    , 'median(d0_int32, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d1_int32, .false.))&
                    , 'median(d1_int32, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d2_int32, .false.))&
                    , 'median(d2_int32, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d3_int32, .false.))&
                    , 'median(d3_int32, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int32), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1)), 'median(d0, 1): should return NaN' )

        call check(error&
                    , abs(median(d1_int32, 1) - 1.5_dp) < dptol&
                    , 'median(d1_int32, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int32, 1) - [2._dp, -4._dp, 7._dp, 1._dp])) < dptol&
                    , 'median(d2_int32, 1): uncorrect answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int32, 2) - [3.5_dp, 1.5_dp, 3._dp])) < dptol&
                    ,'median(d2_int32, 2): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_odd_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(median(d1odd_int32, 1) - 2._dp) < dptol&
                    , 'median(d1odd_int32, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int32, 1) - [2._dp, -4._dp, 7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int32, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int32, 2) - [7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int32, 2): wrong answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_optmask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int32), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1, .false.))&
                    , 'median(d0, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, ieee_is_nan(median(d1_int32, 1, .false.))&
                    , 'median(d1_int32, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(median(d2_int32, 1, .false.)))&
                    , 'median(d2_int32, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d2_int32, 2, .false.)))&
                    , 'median(d2_int32, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int32, 1, .false.)))&
                    , 'median(d3_int32, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int32, 2, .false.)))&
                    , 'median(d3_int32, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int32, 3, .false.)))&
                    , 'median(d3_int32, 3, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_mask_all_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int32), allocatable :: d0(:)

        allocate(d0(0))

        call check(error,  ieee_is_nan(median(d0, d0 > 0))&
                    , 'median(d0, d0 > 0): should be NaN' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int32, d1_int32 > huge(d1_int32)))&
                    , 'median(d1_int32, d1_int32 > huge(d1_int32))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d2_int32, d2_int32 > huge(d2_int32)))&
                    , 'median(d2_int32, d2_int32 > huge(d2_int32))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d3_int32, d3_int32 > huge(d3_int32)))&
                    , 'median(d3_int32, d3_int32 > huge(d3_int32))' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int32, d1_int32 > 0) - 7._dp) < dptol&
                    , 'median(d1_int32, d1_int32> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d2_int32, d2_int32 > 0) - 7._dp) < dptol&
                    , 'median(d2_int32, d2_int32> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d3_int32, d3_int32 > 0) - 7._dp) < dptol&
                    , 'median(d3_int32, d3_int32> 0)' ) 
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_mask_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int32), allocatable :: d0(:)

        allocate(d0(0))

        call check(error&
                   , ieee_is_nan(median(d0, 1, d0 > 0))&
                   , 'median(d0, 1, d0 > 0): uncorrect answer' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int32, 1, d1_int32 > huge(d1_int32)))&
                    , 'median(d1_int32, 1_int32, d1_int32 > huge(d1_int32)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d2_int32, 1, d2_int32 > huge(d2_int32))))&
                    , 'median(d2_int32, 1_int32, d2_int32 > huge(d2_int32)): answer should be IEEE NaN' )
        if (allocated(error)) return
        call check(error&
                    , any(ieee_is_nan(median(d3_int32, 1, d3_int32 > huge(d3_int32))))&
                    , 'median(d3_int32, 1_int32, d3_int32 > huge(d3_int32)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int32, 1, d1_int32 > 0) - 7._dp) < dptol&
                    , 'median(d1_int32, 1, d1_int32 >0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs( (median(d2_int32, 1, d2_int32 > 0) - [ 6._dp, 6._dp, 8._dp, 10.5_dp] )  )) &
                    < dptol&
                    , 'median(d2_int32, 1, d2_int32 > 0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs((median(d2_int32, 2, d2_int32 > 0) - [ 8.5_dp, 2._dp, 14.5_dp] )))&
                    < dptol&
                    , 'median(d2_int32, 2, d2_int32 > 0)') 
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d3_int32, 1, d3_int32 > 0)))&
                    , 'median(d3_int32, 1, d3_int32 > 0): should contain at least 1 IEEE NaN')

    end subroutine
    subroutine test_stats_median_size_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int64), allocatable :: d0(:)

        allocate(d0(0))
        !check just to be sure that the setup of d0 is correct
        call check(error, size(d0), 0, 'size(d0): should be of size 0')

        call check(error, mod(size(d1_int64), 2), 0&
                    , 'mod(size(d1_int64), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2_int64), 2), 0&
                    , 'mod(size(d2_int64), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3_int64), 2), 0&
                    , 'mod(size(d3_int64), 2): should be an even number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_odd_size_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mod(size(d1odd_int64), 2), 1&
                    , 'mod(size(d1)_int64, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2odd_int64), 2), 1&
                    , 'mod(size(d2)_int64, 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3odd_int64), 2), 1&
                    , 'mod(size(d3)_int64, 2): should be an odd number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int64), allocatable :: d0(:)

        allocate(d0(0))
        call check(error, ieee_is_nan(median(d0)), 'median(d0): should be NaN' )
        if (allocated(error)) return

        call check(error, median(d1_int64),  1.5_dp&
                    , 'median(d1_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d2_int64),  1.5_dp&
                    , 'median(d2_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d3_int64),  1.5_dp&
                    , 'median(d3_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_odd_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, median(d1odd_int64),  2._dp&
                    , 'median(d1odd_int64): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, median(d2odd_int64),  1._dp&
                    , 'median(d2odd_int64): uncorrect answer'&
                    , thr = dptol)

    end subroutine

    subroutine test_stats_median_all_optmask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int64), allocatable :: d0_int64(:)

        allocate(d0_int64(0))

        call check(error, ieee_is_nan(median(d0_int64, .false.))&
                    , 'median(d0_int64, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d1_int64, .false.))&
                    , 'median(d1_int64, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d2_int64, .false.))&
                    , 'median(d2_int64, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d3_int64, .false.))&
                    , 'median(d3_int64, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int64), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1)), 'median(d0, 1): should return NaN' )

        call check(error&
                    , abs(median(d1_int64, 1) - 1.5_dp) < dptol&
                    , 'median(d1_int64, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int64, 1) - [2._dp, -4._dp, 7._dp, 1._dp])) < dptol&
                    , 'median(d2_int64, 1): uncorrect answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_int64, 2) - [3.5_dp, 1.5_dp, 3._dp])) < dptol&
                    ,'median(d2_int64, 2): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_odd_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(median(d1odd_int64, 1) - 2._dp) < dptol&
                    , 'median(d1odd_int64, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int64, 1) - [2._dp, -4._dp, 7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int64, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_int64, 2) - [7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_int64, 2): wrong answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_optmask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int64), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1, .false.))&
                    , 'median(d0, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, ieee_is_nan(median(d1_int64, 1, .false.))&
                    , 'median(d1_int64, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(median(d2_int64, 1, .false.)))&
                    , 'median(d2_int64, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d2_int64, 2, .false.)))&
                    , 'median(d2_int64, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int64, 1, .false.)))&
                    , 'median(d3_int64, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int64, 2, .false.)))&
                    , 'median(d3_int64, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_int64, 3, .false.)))&
                    , 'median(d3_int64, 3, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_mask_all_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int64), allocatable :: d0(:)

        allocate(d0(0))

        call check(error,  ieee_is_nan(median(d0, d0 > 0))&
                    , 'median(d0, d0 > 0): should be NaN' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int64, d1_int64 > huge(d1_int64)))&
                    , 'median(d1_int64, d1_int64 > huge(d1_int64))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d2_int64, d2_int64 > huge(d2_int64)))&
                    , 'median(d2_int64, d2_int64 > huge(d2_int64))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d3_int64, d3_int64 > huge(d3_int64)))&
                    , 'median(d3_int64, d3_int64 > huge(d3_int64))' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int64, d1_int64 > 0) - 7._dp) < dptol&
                    , 'median(d1_int64, d1_int64> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d2_int64, d2_int64 > 0) - 7._dp) < dptol&
                    , 'median(d2_int64, d2_int64> 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d3_int64, d3_int64 > 0) - 7._dp) < dptol&
                    , 'median(d3_int64, d3_int64> 0)' ) 
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_mask_int64(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer(int64), allocatable :: d0(:)

        allocate(d0(0))

        call check(error&
                   , ieee_is_nan(median(d0, 1, d0 > 0))&
                   , 'median(d0, 1, d0 > 0): uncorrect answer' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_int64, 1, d1_int64 > huge(d1_int64)))&
                    , 'median(d1_int64, 1_int64, d1_int64 > huge(d1_int64)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d2_int64, 1, d2_int64 > huge(d2_int64))))&
                    , 'median(d2_int64, 1_int64, d2_int64 > huge(d2_int64)): answer should be IEEE NaN' )
        if (allocated(error)) return
        call check(error&
                    , any(ieee_is_nan(median(d3_int64, 1, d3_int64 > huge(d3_int64))))&
                    , 'median(d3_int64, 1_int64, d3_int64 > huge(d3_int64)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_int64, 1, d1_int64 > 0) - 7._dp) < dptol&
                    , 'median(d1_int64, 1, d1_int64 >0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs( (median(d2_int64, 1, d2_int64 > 0) - [ 6._dp, 6._dp, 8._dp, 10.5_dp] )  )) &
                    < dptol&
                    , 'median(d2_int64, 1, d2_int64 > 0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs((median(d2_int64, 2, d2_int64 > 0) - [ 8.5_dp, 2._dp, 14.5_dp] )))&
                    < dptol&
                    , 'median(d2_int64, 2, d2_int64 > 0)') 
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d3_int64, 1, d3_int64 > 0)))&
                    , 'median(d3_int64, 1, d3_int64 > 0): should contain at least 1 IEEE NaN')

    end subroutine

    subroutine test_stats_median_size_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: d0(:)

        allocate(d0(0))
        !check just to be sure that the setup of d0 is correct
        call check(error, size(d0), 0, 'size(d0): should be of size 0')

        call check(error, mod(size(d1_sp), 2), 0&
                    , 'mod(size(d1_sp), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2_sp), 2), 0&
                    , 'mod(size(d2_sp), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3_sp), 2), 0&
                    , 'mod(size(d3_sp), 2): should be an even number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_odd_size_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mod(size(d1odd_sp), 2), 1&
                    , 'mod(size(d1_sp), 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2odd_sp), 2), 1&
                    , 'mod(size(d2_sp), 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3odd_sp), 2), 1&
                    , 'mod(size(d3_sp), 2): should be an odd number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: d0(:)

        allocate(d0(0))
        call check(error,  ieee_is_nan(median(d0)), 'median(d0): should be NaN' )
        if (allocated(error)) return

        call check(error, median(d1_sp),  1.5_sp&
                    , 'median(d1_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, median(d2_sp),  1.5_sp&
                    , 'median(d2_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
        call check(error, median(d3_sp),  1.5_sp&
                    , 'median(d3_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_odd_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, median(d1odd_sp),  2._sp&
                    , 'median(d1odd_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return

        call check(error, median(d2odd_sp),  1._sp&
                    , 'median(d2odd_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return

        call check(error, median(d2odd_sp),  1._sp&
                    , 'median(d2odd_sp): uncorrect answer'&
                    , thr = sptol)
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_all_optmask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: d0_sp(:)

        allocate(d0_sp(0))

        call check(error, ieee_is_nan(median(d0_sp, .false.))&
                    , 'median(d0_sp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d1_sp, .false.))&
                    , 'median(d1_sp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d2_sp, .false.))&
                    , 'median(d2_sp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d3_sp, .false.))&
                    , 'median(d3_sp, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1)), 'median(d0, 1): should return NaN' )

        call check(error&
                    , abs(median(d1_sp, 1) - 1.5_sp) < sptol&
                    , 'median(d1_sp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_sp, 1) - [2._sp, -4._sp, 7._sp, 1._sp])) < sptol&
                    , 'median(d2_sp, 1): uncorrect answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_sp, 2) - [3.5_sp, 1.5_sp, 3._sp])) < sptol&
                    ,'median(d2_sp, 2): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_odd_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(median(d1odd_sp, 1) - 2._sp) < sptol&
                    , 'median(d1odd_sp, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_sp, 1) - [2._sp, -4._sp, 7._sp, 1._sp, 0._sp])) < sptol&
                    , 'median(d2odd_sp, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_sp, 2) - [7._sp, 1._sp, 0._sp])) < sptol&
                    , 'median(d2odd_sp, 2): wrong answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_optmask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1, .false.))&
                    , 'median(d0, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, ieee_is_nan(median(d1_sp, 1, .false.))&
                    , 'median(d1_sp, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(median(d2_sp, 1, .false.)))&
                    , 'median(d2_sp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d2_sp, 2, .false.)))&
                    , 'median(d2_sp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_sp, 1, .false.)))&
                    , 'median(d3_sp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_sp, 2, .false.)))&
                    , 'median(d3_sp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_sp, 3, .false.)))&
                    , 'median(d3_sp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_mask_all_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, d0 > 0))&
                    , 'median(d0, d0 > 0): should be NaN' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_sp, d1_sp > huge(d1_sp)))&
                    , 'median(d1_sp, d1_sp > huge(d1_sp))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d2_sp, d2_sp > huge(d2_sp)))&
                    , 'median(d2_sp, d2_sp > huge(d2_sp))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d3_sp, d3_sp > huge(d3_sp)))&
                    , 'median(d3_sp, d3_sp > huge(d3_sp))' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_sp, d1_sp > 0) - 7._sp) < sptol&
                    , 'median(d1_sp, d1_sp > 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d2_sp, d2_sp > 0) - 7._sp) < sptol&
                    , 'median(d2_sp, d2_sp > 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d3_sp, d3_sp > 0) - 7._sp) < sptol&
                    , 'median(d3_sp, d3_sp > 0)' ) 
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_mask_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(sp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error&
                   , ieee_is_nan(median(d0, 1, d0 > 0))&
                   , 'median(d0, 1, d0 > 0): uncorrect answer' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_sp, 1, d1_sp > huge(d1_sp)))&
                    , 'median(d1_sp, 1, d1_sp > huge(d1_sp)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d2_sp, 1, d2_sp > huge(d2_sp))))&
                    , 'median(d2_sp, 1, d2_sp > huge(d2_sp)): answer should be IEEE NaN' )
        if (allocated(error)) return
        call check(error&
                    , any(ieee_is_nan(median(d3_sp, 1, d3_sp > huge(d3_sp))))&
                    , 'median(d3_sp, 1, d3_sp > huge(d3_sp)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_sp, 1, d1_sp > 0) - 7._sp) < sptol&
                    , 'median(d1_sp, 1, d1_sp >0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs( (median(d2_sp, 1, d2_sp > 0) - [ 6._sp, 6._sp, 8._sp, 10.5_sp] )  )) &
                    < sptol&
                    , 'median(d2_sp, 1, d2_sp > 0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs((median(d2_sp, 2, d2_sp > 0) - [ 8.5_sp, 2._sp, 14.5_sp] )))&
                    < sptol&
                    , 'median(d2_sp, 2, d2_sp > 0)') 
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d3_sp, 1, d3_sp > 0)))&
                    , 'median(d3_sp, 1, d3_sp > 0): should contain at least 1 IEEE NaN')

    end subroutine

    subroutine test_stats_median_size_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), allocatable :: d0(:)

        allocate(d0(0))
        !check just to be sure that the setup of d0 is correct
        call check(error, size(d0), 0, 'size(d0): should be of size 0')

        call check(error, mod(size(d1_dp), 2), 0&
                    , 'mod(size(d1_dp), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2_dp), 2), 0&
                    , 'mod(size(d2_dp), 2): should be an even number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3_dp), 2), 0&
                    , 'mod(size(d3_dp), 2): should be an even number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_odd_size_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, mod(size(d1odd_dp), 2), 1&
                    , 'mod(size(d1_dp), 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d2odd_dp), 2), 1&
                    , 'mod(size(d2_dp), 2): should be an odd number'&
                    )
        if (allocated(error)) return
        call check(error, mod(size(d3odd_dp), 2), 1&
                    , 'mod(size(d3_dp), 2): should be an odd number'&
                    )
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), allocatable :: d0(:)

        allocate(d0(0))
        call check(error,  ieee_is_nan(median(d0)), 'median(d0): should be NaN' )
        if (allocated(error)) return

        call check(error, median(d1_dp),  1.5_dp&
                    , 'median(d1_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d2_dp),  1.5_dp&
                    , 'median(d2_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
        call check(error, median(d3_dp),  1.5_dp&
                    , 'median(d3_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_all_odd_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, median(d1odd_dp),  2._dp&
                    , 'median(d1odd_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, median(d2odd_dp),  1._dp&
                    , 'median(d2odd_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

        call check(error, median(d2odd_dp),  1._dp&
                    , 'median(d2odd_dp): uncorrect answer'&
                    , thr = dptol)
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_all_optmask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), allocatable :: d0_dp(:)

        allocate(d0_dp(0))

        call check(error, ieee_is_nan(median(d0_dp, .false.))&
                    , 'median(d0_dp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d1_dp, .false.))&
                    , 'median(d1_dp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d2_dp, .false.))&
                    , 'median(d2_dp, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, ieee_is_nan(median(d3_dp, .false.))&
                    , 'median(d3_dp, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1)), 'median(d0, 1): should return NaN' )

        call check(error&
                    , abs(median(d1_dp, 1) - 1.5_dp) < dptol&
                    , 'median(d1_dp, 1): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_dp, 1) - [2._dp, -4._dp, 7._dp, 1._dp])) < dptol&
                    , 'median(d2_dp, 1): uncorrect answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2_dp, 2) - [3.5_dp, 1.5_dp, 3._dp])) < dptol&
                    ,'median(d2_dp, 2): uncorrect answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_odd_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error&
                    , abs(median(d1odd_dp, 1) - 2._dp) < dptol&
                    , 'median(d1odd_dp, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_dp, 1) - [2._dp, -4._dp, 7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_dp, 1): wrong answer')
        if (allocated(error)) return

        call check(error&
                    , sum(abs(median(d2odd_dp, 2) - [7._dp, 1._dp, 0._dp])) < dptol&
                    , 'median(d2odd_dp, 2): wrong answer')
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_optmask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, 1, .false.))&
                    , 'median(d0, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, ieee_is_nan(median(d1_dp, 1, .false.))&
                    , 'median(d1_dp, 1, .false.): uncorrect answer'&
                    )
        if (allocated(error)) return

        call check(error, any(ieee_is_nan(median(d2_dp, 1, .false.)))&
                    , 'median(d2_dp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d2_dp, 2, .false.)))&
                    , 'median(d2_dp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_dp, 1, .false.)))&
                    , 'median(d3_dp, 1, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_dp, 2, .false.)))&
                    , 'median(d3_dp, 2, .false.): uncorrect answer')
        if (allocated(error)) return
        call check(error, any(ieee_is_nan(median(d3_dp, 3, .false.)))&
                    , 'median(d3_dp, 3, .false.): uncorrect answer')
        if (allocated(error)) return
    end subroutine

    subroutine test_stats_median_mask_all_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error, ieee_is_nan(median(d0, d0 > 0))&
                    , 'median(d0, d0 > 0): should be NaN' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_dp, d1_dp > huge(d1_dp)))&
                    , 'median(d1_dp, d1_dp > huge(d1_dp))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d2_dp, d2_dp > huge(d2_dp)))&
                    , 'median(d2_dp, d2_dp > huge(d2_dp))' )
        if (allocated(error)) return
        call check(error&
                    , ieee_is_nan(median(d3_dp, d3_dp > huge(d3_dp)))&
                    , 'median(d3_dp, d3_dp > huge(d3_dp))' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_dp, d1_dp > 0) - 7._dp) < dptol&
                    , 'median(d1_dp, d1_dp > 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d2_dp, d2_dp > 0) - 7._dp) < dptol&
                    , 'median(d2_dp, d2_dp > 0)' ) 
        if (allocated(error)) return
        call check(error&
                    , (median(d3_dp, d3_dp > 0) - 7._dp) < dptol&
                    , 'median(d3_dp, d3_dp > 0)' ) 
        if (allocated(error)) return

    end subroutine

    subroutine test_stats_median_mask_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        real(dp), allocatable :: d0(:)

        allocate(d0(0))

        call check(error&
                   , ieee_is_nan(median(d0, 1, d0 > 0))&
                   , 'median(d0, 1, d0 > 0): uncorrect answer' )
        if (allocated(error)) return

        call check(error&
                    , ieee_is_nan(median(d1_dp, 1, d1_dp > huge(d1_dp)))&
                    , 'median(d1_dp, 1, d1_dp > huge(d1_dp)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d2_dp, 1, d2_dp > huge(d2_dp))))&
                    , 'median(d2_dp, 1, d2_dp > huge(d2_dp)): answer should be IEEE NaN' )
        if (allocated(error)) return
        call check(error&
                    , any(ieee_is_nan(median(d3_dp, 1, d3_dp > huge(d3_dp))))&
                    , 'median(d3_dp, 1, d3_dp > huge(d3_dp)): answer should be IEEE NaN' )
        if (allocated(error)) return

        call check(error&
                    , (median(d1_dp, 1, d1_dp > 0) - 7._dp) < dptol&
                    , 'median(d1_dp, 1, d1_dp >0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs( (median(d2_dp, 1, d2_dp > 0) - [ 6._dp, 6._dp, 8._dp, 10.5_dp] )  )) &
                    < dptol&
                    , 'median(d2_dp, 1, d2_dp > 0): uncorrect answer') 
        if (allocated(error)) return

        call check(error&
                    , sum(abs((median(d2_dp, 2, d2_dp > 0) - [ 8.5_dp, 2._dp, 14.5_dp] )))&
                    < dptol&
                    , 'median(d2_dp, 2, d2_dp > 0)') 
        if (allocated(error)) return

        call check(error&
                    , any(ieee_is_nan(median(d3_dp, 1, d3_dp > 0)))&
                    , 'median(d3_dp, 1, d3_dp > 0): should contain at least 1 IEEE NaN')

    end subroutine


end module test_stats_median

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_stats_median, only : collect_stats_median
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("stats_median", collect_stats_median) &
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
