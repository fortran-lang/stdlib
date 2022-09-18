module test_moment
    use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
    use stdlib_kinds, only: sp, dp, int32, int64
    use stdlib_stats, only: moment
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none

    private
    public :: collect_moment, initialize_test_data

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    real(dp), parameter :: d1(5) = [1._dp, 2._dp, 3._dp, 4._dp, 5._dp]
    real(dp), parameter :: d(4,3) = reshape( &
        [1._dp, 3._dp, 5._dp, 7._dp, &
         2._dp, 4._dp, 6._dp, 8._dp, &
         9._dp, 10._dp, 11._dp, 12._dp], [4, 3])

    complex(dp) :: c1(5) = [(0.57706_dp, 0.00000_dp), &
                            (0.00000_dp, 1.44065_dp), &
                            (1.26401_dp, 0.00000_dp), &
                            (0.00000_dp, 0.88833_dp), &
                            (1.14352_dp, 0.00000_dp)]
    complex(dp) :: c2(5,3)

    real(sp) :: x1(5) = real(d1, sp)
    real(sp) :: x2(4,3) = real(d, sp)
    real(sp), allocatable :: x3(:,:,:)

    real(dp) :: dx1(5) = d1
    real(dp) :: dx2(4,3) = d
    real(dp), allocatable :: dx3(:,:,:)

    integer(int32) :: i1(5) = d1
    integer(int32) :: i2(4,3) = d
    integer(int32), allocatable :: i3(:,:,:)

    integer(int64) :: di1(5) = d1
    integer(int64) :: di2(4,3) = d
    integer(int64), allocatable :: di3(:,:,:)

contains

    subroutine initialize_test_data()

        allocate(x3(size(x2, 1),size(x2, 2),3))
        x3(:,:,1) = x2
        x3(:,:,2) = x2 * 2
        x3(:,:,3) = x2 * 4

        allocate(dx3(size(dx2, 1),size(dx2, 2),3))
        dx3(:,:,1) = dx2
        dx3(:,:,2) = dx2 * 2
        dx3(:,:,3) = dx2 * 4

        i3 = x3
        di3 = dx3

        c2(:,1) = c1
        c2(:,2) = c1 * 3
        c2(:,3) = c1 * 1.5

    end subroutine initialize_test_data

    !> Collect all exported unit tests
    subroutine collect_moment(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('moment_sp_dim1_order1', test_sp_dim1_order1), &
            new_unittest('moment_sp_dim1_mask_order1', test_sp_dim1_mask_order1), &
            new_unittest('moment_sp_dim1_mask_array_order1', test_sp_dim1_mask_array_order1), &
            new_unittest('moment_sp_dim2_order1', test_sp_dim2_order1), &
            new_unittest('moment_sp_dim2_mask_order1', test_sp_dim2_mask_order1), &
            new_unittest('moment_sp_dim2_mask_array_order1', test_sp_dim2_mask_array_order1), &
            new_unittest('moment_sp_dim3_order1', test_sp_dim3_order1), &
            new_unittest('moment_sp_dim3_mask_order1', test_sp_dim3_mask_order1), &
            new_unittest('moment_sp_dim3_mask_array_order1', test_sp_dim3_mask_array_order1), &
            new_unittest('moment_sp_dim1_order2', test_sp_dim1_order2), &
            new_unittest('moment_sp_dim1_mask_order2', test_sp_dim1_mask_order2), &
            new_unittest('moment_sp_dim1_mask_array_order2', test_sp_dim1_mask_array_order2), &
            new_unittest('moment_sp_dim2_order2', test_sp_dim2_order2), &
            new_unittest('moment_sp_dim2_mask_order2', test_sp_dim2_mask_order2), &
            new_unittest('moment_sp_dim2_mask_array_order2', test_sp_dim2_mask_array_order2), &
            new_unittest('moment_sp_dim3_order2', test_sp_dim3_order2), &
            new_unittest('moment_sp_dim3_mask_order2', test_sp_dim3_mask_order2), &
            new_unittest('moment_sp_dim3_mask_array_order2', test_sp_dim3_mask_array_order2), &
            new_unittest('moment_dp_dim1_order1', test_dp_dim1_order1), &
            new_unittest('moment_dp_dim1_mask_order1', test_dp_dim1_mask_order1), &
            new_unittest('moment_dp_dim1_mask_array_order1', test_dp_dim1_mask_array_order1), &
            new_unittest('moment_dp_dim2_order1', test_dp_dim2_order1), &
            new_unittest('moment_dp_dim2_mask_order1', test_dp_dim2_mask_order1), &
            new_unittest('moment_dp_dim2_mask_array_order1', test_dp_dim2_mask_array_order1), &
            new_unittest('moment_dp_dim3_order1', test_dp_dim3_order1), &
            new_unittest('moment_dp_dim3_mask_order1', test_dp_dim3_mask_order1), &
            new_unittest('moment_dp_dim3_mask_array_order1', test_dp_dim3_mask_array_order1), &
            new_unittest('moment_dp_dim1_order2', test_dp_dim1_order2), &
            new_unittest('moment_dp_dim1_mask_order2', test_dp_dim1_mask_order2), &
            new_unittest('moment_dp_dim1_mask_array_order2', test_dp_dim1_mask_array_order2), &
            new_unittest('moment_dp_dim2_order2', test_dp_dim2_order2), &
            new_unittest('moment_dp_dim2_mask_order2', test_dp_dim2_mask_order2), &
            new_unittest('moment_dp_dim2_mask_array_order2', test_dp_dim2_mask_array_order2), &
            new_unittest('moment_dp_dim3_order2', test_dp_dim3_order2), &
            new_unittest('moment_dp_dim3_mask_order2', test_dp_dim3_mask_order2), &
            new_unittest('moment_dp_dim3_mask_array_order2', test_dp_dim3_mask_array_order2), &
            new_unittest('moment_int32_dim1_order1', test_int32_dim1_order1), &
            new_unittest('moment_int32_dim1_mask_order1', test_int32_dim1_mask_order1), &
            new_unittest('moment_int32_dim1_mask_array_order1', test_int32_dim1_mask_array_order1), &
            new_unittest('moment_int32_dim2_order1', test_int32_dim2_order1), &
            new_unittest('moment_int32_dim2_mask_order1', test_int32_dim2_mask_order1), &
            new_unittest('moment_int32_dim2_mask_array_order1', test_int32_dim2_mask_array_order1), &
            new_unittest('moment_int32_dim3_order1', test_int32_dim3_order1), &
            new_unittest('moment_int32_dim3_mask_order1', test_int32_dim3_mask_order1), &
            new_unittest('moment_int32_dim3_mask_array_order1', test_int32_dim3_mask_array_order1), &
            new_unittest('moment_int32_dim1_order2', test_int32_dim1_order2), &
            new_unittest('moment_int32_dim1_mask_order2', test_int32_dim1_mask_order2), &
            new_unittest('moment_int32_dim1_mask_array_order2', test_int32_dim1_mask_array_order2), &
            new_unittest('moment_int32_dim2_order2', test_int32_dim2_order2), &
            new_unittest('moment_int32_dim2_mask_order2', test_int32_dim2_mask_order2), &
            new_unittest('moment_int32_dim2_mask_array_order2', test_int32_dim2_mask_array_order2), &
            new_unittest('moment_int32_dim3_order2', test_int32_dim3_order2), &
            new_unittest('moment_int32_dim3_mask_order2', test_int32_dim3_mask_order2), &
            new_unittest('moment_int32_dim3_mask_array_order2', test_int32_dim3_mask_array_order2), &
            new_unittest('moment_int64_dim1_order1', test_int64_dim1_order1), &
            new_unittest('moment_int64_dim1_mask_order1', test_int64_dim1_mask_order1), &
            new_unittest('moment_int64_dim1_mask_array_order1', test_int64_dim1_mask_array_order1), &
            new_unittest('moment_int64_dim2_order1', test_int64_dim2_order1), &
            new_unittest('moment_int64_dim2_mask_order1', test_int64_dim2_mask_order1), &
            new_unittest('moment_int64_dim2_mask_array_order1', test_int64_dim2_mask_array_order1), &
            new_unittest('moment_int64_dim3_order1', test_int64_dim3_order1), &
            new_unittest('moment_int64_dim3_mask_order1', test_int64_dim3_mask_order1), &
            new_unittest('moment_int64_dim3_mask_array_order1', test_int64_dim3_mask_array_order1), &
            new_unittest('moment_int64_dim1_order2', test_int64_dim1_order2), &
            new_unittest('moment_int64_dim1_mask_order2', test_int64_dim1_mask_order2), &
            new_unittest('moment_int64_dim1_mask_array_order2', test_int64_dim1_mask_array_order2), &
            new_unittest('moment_int64_dim2_order2', test_int64_dim2_order2), &
            new_unittest('moment_int64_dim2_mask_order2', test_int64_dim2_mask_order2), &
            new_unittest('moment_int64_dim2_mask_array_order2', test_int64_dim2_mask_array_order2), &
            new_unittest('moment_int64_dim3_order2', test_int64_dim3_order2), &
            new_unittest('moment_int64_dim3_mask_order2', test_int64_dim3_mask_order2), &
            new_unittest('moment_int64_dim3_mask_array_order2', test_int64_dim3_mask_array_order2), &
            new_unittest('moment_csp_dim1_order1', test_csp_dim1_order1), &
            new_unittest('moment_csp_dim1_mask_order1', test_csp_dim1_mask_order1), &
            new_unittest('moment_csp_dim1_mask_array_order1', test_csp_dim1_mask_array_order1), &
            new_unittest('moment_csp_dim2_order1', test_csp_dim2_order1), &
            new_unittest('moment_csp_dim2_mask_order1', test_csp_dim2_mask_order1), &
            new_unittest('moment_csp_dim2_mask_array_order1', test_csp_dim2_mask_array_order1), &
            new_unittest('moment_csp_dim1_order2', test_csp_dim1_order2), &
            new_unittest('moment_csp_dim1_mask_order2', test_csp_dim1_mask_order2), &
            new_unittest('moment_csp_dim1_mask_array_order2', test_csp_dim1_mask_array_order2), &
            new_unittest('moment_csp_dim2_order2', test_csp_dim2_order2), &
            new_unittest('moment_csp_dim2_mask_order2', test_csp_dim2_mask_order2), &
            new_unittest('moment_csp_dim2_mask_array_order2', test_csp_dim2_mask_array_order2) &
        ]

    end subroutine collect_moment

    subroutine test_sp_dim1_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(x1, order)) < sptol)
        call check(error, abs(moment(x1, order, dim=1)) < sptol)
    end subroutine

    subroutine test_sp_dim1_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(x1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(x1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_sp_dim1_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(x1, order, mask=(x1 < 5))) < sptol)
        call check(error, abs(moment(x1, order, 1, mask=(x1 < 5))) < sptol)
    end subroutine

    subroutine test_sp_dim2_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(x2, order)) < sptol)
        call check(error, all(abs(moment(x2, order, 1)) < sptol))
        call check(error, all(abs(moment(x2, order, 2)) < sptol))
    end subroutine

    subroutine test_sp_dim2_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(x2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(x2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(x2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_sp_dim2_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(x2, order, mask=(x2 < 11))) < sptol)
        call check(error, all(abs(moment(x2, order, 1, mask=(x2 < 11))) < sptol))
        call check(error, all(abs(moment(x2, order, 2, mask=(x2 < 11))) < sptol))
    end subroutine

    subroutine test_sp_dim3_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(x3, order)) < sptol)
        call check(error, all(abs(moment(x3, order, 1)) < sptol))
        call check(error, all(abs(moment(x3, order, 2)) < sptol))
        call check(error, all(abs(moment(x3, order, 3)) < sptol))
    end subroutine
    
    subroutine test_sp_dim3_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(x3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(x3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(x3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(x3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_sp_dim3_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(x3, order, mask=(x3 < 11)) ) < sptol)
        call check(error, all(abs(moment(x3, order, 1, mask=(x3 < 45))) < sptol ))
        call check(error, all(abs(moment(x3, order, 2, mask=(x3 < 45))) < sptol ))
        call check(error, all(abs(moment(x3, order, 3, mask=(x3 < 45))) < sptol ))
    end subroutine

    subroutine test_sp_dim1_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(x1, order) - 2._sp) < sptol)
        call check(error, abs(moment(x1, order, dim=1) - 2._sp) < sptol)
    end subroutine

    subroutine test_sp_dim1_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(x1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(x1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_sp_dim1_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(x1, order, mask=(x1 < 5)) - 1.25_sp) < sptol)
        call check(error, abs(moment(x1, order, 1, mask=(x1 < 5)) - 1.25_sp) < sptol)
    end subroutine

    subroutine test_sp_dim2_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(x2, order) - 107.25_sp/9.) < sptol)
        call check(error, all(abs(moment(x2, order, 1) - [5._sp, 5._sp, 1.25_sp]) < sptol))
        call check(error, all(abs(moment(x2, order, 2) -&
                           [19.0, 43. / 3., 31. / 3. , 7.0]*2./3.) < sptol))
    end subroutine

    subroutine test_sp_dim2_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(x2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(x2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(x2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_sp_dim2_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(x2, order, mask=(x2 < 11))- 2.75_sp*3.) < sptol)
        call check(error, all(abs(moment(x2, order, 1, mask=(x2 < 11)) -&
                      [5._sp, 5._sp, 0.25_sp]) < sptol))
        call check(error, all(abs(moment(x2, order, 2, mask=(x2 < 11)) -&
                      [19._sp*2./3., 43._sp/9.*2., 0.25_sp , 0.25_sp]) < sptol))
    end subroutine

    subroutine test_sp_dim3_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(x3, order) - 153.4_sp*35./36.) < sptol)
        call check(error, all(abs(moment(x3, order, 1) -&
                 reshape([20._sp / 3., 20._sp / 3., 5._sp / 3.,&
                          4* 20._sp / 3., 4* 20._sp / 3., 4* 5._sp / 3.,&
                          16* 20._sp / 3., 16* 20._sp / 3., 16* 5._sp / 3.],&
                          [size(x3,2), size(x3,3)])*3._sp/4.)&
                 < sptol))
        call check(error, all(abs(moment(x3, order, 2) -&
                 reshape([19._sp, 43._sp / 3., 31._sp / 3. , 7.0_sp,&
                          4* 19.0_sp, 4* 43._sp / 3., 4* 31._sp / 3. , 4* 7.0_sp,&
                          16* 19.0_sp, 16* 43._sp / 3., 16* 31._sp / 3. , 16* 7.0_sp],&
                          [size(x3,1), size(x3,3)] )*2._sp/3.)&
                 < sptol))
        call check(error, all(abs(moment(x3, order, 3) -&
                 reshape([ 7._sp/3., 21._sp, 175._sp/3.,&
                           343._sp/3., 28._sp/3., 112._sp/3.,&
                           84._sp, 448._sp/3., 189._sp,&
                           700._sp/3., 847._sp/3., 336._sp],&
                           [size(x3,1), size(x3,2)] )*2./3.)&
                 < sptol))
    end subroutine
    
    subroutine test_sp_dim3_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(x3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(x3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(x3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(x3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_sp_dim3_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(x3, order, mask=(x3 < 11)) -&
                      7.7370242214532876_dp ) < sptol)
        call check(error, all(abs(moment(x3, order, 1, mask=(x3 < 45)) -&
                      reshape([5._sp, 5._sp, 1.25_sp,  20._sp, 20._sp, 5._sp,&
                               80._sp, 80._sp, 32._sp/3.],&
                               [size(x3, 2), size(x3, 3)])) < sptol ))
        call check(error, all(abs(moment(x3, order, 2, mask=(x3 < 45)) -&
                      reshape([ 38._sp/3., 86._sp/9., 62._sp/9., 14._sp/3., 152._sp/3.,&
                                344._sp/9., 248._sp/9., 168._sp/9., 1824._sp/9.,&
                                1376._sp/9., 992._sp/9., 4._sp&
                               ],&
                      [size(x3, 1), size(x3, 3)])) < sptol ))
        call check(error, all(abs(moment(x3, order, 3, mask=(x3 < 45)) -&
                     reshape([14._sp/9., 14._sp, 350._sp/9., 686._sp/9., 56._sp/9.,&
                              224._sp/9., 56._sp, 896._sp/9., 126._sp, 1400._sp/9.,&
                              1694._sp/9., 36._sp&
                               ], [size(x3,1), size(x3,2)] ))&
                     < sptol ))
    end subroutine

    subroutine test_dp_dim1_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(dx1, order)) < dptol)
        call check(error, abs(moment(dx1, order, dim=1)) < dptol)
    end subroutine

    subroutine test_dp_dim1_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(dx1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(dx1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_dp_dim1_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(dx1, order, mask=(dx1 < 5))) < dptol)
        call check(error, abs(moment(dx1, order, 1, mask=(dx1 < 5))) < dptol)
    end subroutine

    subroutine test_dp_dim2_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(dx2, order)) < dptol)
        call check(error, all(abs(moment(dx2, order, 1)) < dptol))
        call check(error, all(abs(moment(dx2, order, 2)) < dptol))
    end subroutine

    subroutine test_dp_dim2_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(dx2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(dx2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(dx2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_dp_dim2_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(dx2, order, mask=(dx2 < 11))) < dptol)
        call check(error, all(abs(moment(dx2, order, 1, mask=(dx2 < 11))) < dptol))
        call check(error, all(abs(moment(dx2, order, 2, mask=(dx2 < 11))) < dptol))
    end subroutine

    subroutine test_dp_dim3_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(dx3, order)) < dptol)
        call check(error, all(abs(moment(dx3, order, 1)) < dptol))
        call check(error, all(abs(moment(dx3, order, 2)) < dptol))
        call check(error, all(abs(moment(dx3, order, 3)) < dptol))
    end subroutine
    
    subroutine test_dp_dim3_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(dx3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(dx3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(dx3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(dx3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_dp_dim3_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(dx3, order, mask=(dx3 < 11)) ) < dptol)
        call check(error, all(abs(moment(dx3, order, 1, mask=(dx3 < 45))) < dptol ))
        call check(error, all(abs(moment(dx3, order, 2, mask=(dx3 < 45))) < dptol ))
        call check(error, all(abs(moment(dx3, order, 3, mask=(dx3 < 45))) < dptol ))
    end subroutine

    subroutine test_dp_dim1_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(dx1, order) - 2._dp) < dptol)
        call check(error, abs(moment(dx1, order, dim=1) - 2._dp) < dptol)
    end subroutine

    subroutine test_dp_dim1_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(dx1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(dx1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_dp_dim1_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(dx1, order, mask=(dx1 < 5)) - 1.25_dp) < dptol)
        call check(error, abs(moment(dx1, order, 1, mask=(dx1 < 5)) - 1.25_dp) < dptol)
    end subroutine

    subroutine test_dp_dim2_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(dx2, order) - 107.25_dp/9.) < dptol)
        call check(error, all(abs(moment(dx2, order, 1) - [5._dp, 5._dp, 1.25_dp]) < dptol))
        call check(error, all(abs(moment(dx2, order, 2) -&
                      [19._dp, 43._dp / 3., 31._dp / 3. , 7._dp]*2._dp/3.) < dptol))
    end subroutine

    subroutine test_dp_dim2_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(dx2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(dx2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(dx2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_dp_dim2_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(dx2, order, mask=(dx2 < 11))- 2.75_dp*3.) < dptol)
        call check(error, all(abs(moment(dx2, order, 1, mask=(dx2 < 11)) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
        call check(error, all(abs(moment(dx2, order, 2, mask=(dx2 < 11)) -&
                      [19._dp*2./3., 43._dp/9.*2., 0.25_dp , 0.25_dp]) < dptol))
    end subroutine

    subroutine test_dp_dim3_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(dx3, order) - 153.4_dp*35./36.) < dptol)
        call check(error, all(abs(moment(dx3, order, 1) -&
                 reshape([20._dp / 3., 20._dp / 3., 5._dp / 3.,&
                          4* 20._dp / 3., 4* 20._dp / 3., 4* 5._dp / 3.,&
                          16* 20._dp / 3., 16* 20._dp / 3., 16* 5._dp / 3.],&
                          [size(dx3,2), size(dx3,3)])*3._dp/4.)&
                 < dptol))
        call check(error, all(abs(moment(dx3, order, 2) -&
                 reshape([19._dp, 43._dp / 3., 31._dp / 3. , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3., 4* 31._dp / 3. , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3., 16* 31._dp / 3. , 16* 7.0_dp],&
                          [size(dx3,1), size(dx3,3)] )*2._dp/3.)&
                 < dptol))
        call check(error, all(abs(moment(dx3, order, 3) -&
                 reshape([ 7._dp/3., 21._dp, 175._dp/3.,&
                           343._dp/3., 28._dp/3., 112._dp/3.,&
                           84._dp, 448._dp/3., 189._dp,&
                           700._dp/3., 847._dp/3., 336._dp],&
                           [size(dx3,1), size(dx3,2)] )*2./3.)&
                 < dptol))
    end subroutine
    
    subroutine test_dp_dim3_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(dx3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(dx3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(dx3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(dx3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_dp_dim3_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(dx3, order, mask=(dx3 < 11)) -&
                          7.7370242214532876_dp ) < dptol)
        call check(error, all(abs(moment(dx3, order, 1, mask=(dx3 < 45)) -&
                      reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                               80._dp, 80._dp, 32._dp/3.],&
                               [size(dx3, 2), size(dx3, 3)])) < dptol ))
        call check(error, all(abs(moment(dx3, order, 2, mask=(dx3 < 45)) -&
                      reshape([ 38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                                344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                                1376._dp/9., 992._dp/9., 4._dp&
                               ],&
                      [size(dx3, 1), size(dx3, 3)])) < dptol ))
        call check(error, all(abs(moment(dx3, order, 3, mask=(dx3 < 45)) -&
                     reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                              224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                              1694._dp/9., 36._dp&
                               ], [size(dx3,1), size(dx3,2)] ))&
                     < dptol ))
    end subroutine

    subroutine test_int32_dim1_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(i1, order)) < dptol)
        call check(error, abs(moment(i1, order, dim=1)) < dptol)
    end subroutine

    subroutine test_int32_dim1_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(i1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(i1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_int32_dim1_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(i1, order, mask=(i1 < 5))) < dptol)
        call check(error, abs(moment(i1, order, 1, mask=(i1 < 5))) < dptol)
    end subroutine

    subroutine test_int32_dim2_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(i2, order)) < dptol)
        call check(error, all(abs(moment(i2, order, 1)) < dptol))
        call check(error, all(abs(moment(i2, order, 2)) < dptol))
    end subroutine

    subroutine test_int32_dim2_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(i2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(i2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(i2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_int32_dim2_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(i2, order, mask=(i2 < 11))) < dptol)
        call check(error, all(abs(moment(i2, order, 1, mask=(i2 < 11))) < dptol))
        call check(error, all(abs(moment(i2, order, 2, mask=(i2 < 11))) < dptol))
    end subroutine

    subroutine test_int32_dim3_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(i3, order)) < dptol)
        call check(error, all(abs(moment(i3, order, 1)) < dptol))
        call check(error, all(abs(moment(i3, order, 2)) < dptol))
        call check(error, all(abs(moment(i3, order, 3)) < dptol))
    end subroutine
    
    subroutine test_int32_dim3_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(i3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(i3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(i3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(i3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_int32_dim3_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(i3, order, mask=(i3 < 11)) ) < dptol)
        call check(error, all(abs(moment(i3, order, 1, mask=(i3 < 45))) < dptol ))
        call check(error, all(abs(moment(i3, order, 2, mask=(i3 < 45))) < dptol ))
        call check(error, all(abs(moment(i3, order, 3, mask=(i3 < 45))) < dptol ))
    end subroutine
 
    subroutine test_int32_dim1_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(i1, order) - 2._dp) < dptol)
        call check(error, abs(moment(i1, order, dim=1) - 2._dp) < dptol)
    end subroutine

    subroutine test_int32_dim1_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(i1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(i1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_int32_dim1_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(i1, order, mask=(i1 < 5)) - 1.25_dp) < dptol)
        call check(error, abs(moment(i1, order, 1, mask=(i1 < 5)) - 1.25_dp) < dptol)
    end subroutine

    subroutine test_int32_dim2_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(i2, order) - 107.25_dp/9.) < dptol)
        call check(error, all(abs(moment(i2, order, 1) - [5._dp, 5._dp, 1.25_dp]) < dptol))
        call check(error, all(abs(moment(i2, order, 2) -&
                      [19._dp, 43._dp / 3., 31._dp / 3. , 7._dp]*2._dp/3.) < dptol))
    end subroutine

    subroutine test_int32_dim2_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(i2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(i2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(i2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_int32_dim2_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(i2, order, mask=(i2 < 11))- 2.75_dp*3.) < dptol)
        call check(error, all(abs(moment(i2, order, 1, mask=(i2 < 11)) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
        call check(error, all(abs(moment(i2, order, 2, mask=(i2 < 11)) -&
                      [19._dp*2./3., 43._dp/9.*2., 0.25_dp , 0.25_dp]) < dptol))
    end subroutine

    subroutine test_int32_dim3_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(i3, order) - 153.4_dp*35./36.) < dptol)
        call check(error, all(abs(moment(i3, order, 1) -&
                 reshape([20._dp / 3., 20._dp / 3., 5._dp / 3.,&
                          4* 20._dp / 3., 4* 20._dp / 3., 4* 5._dp / 3.,&
                          16* 20._dp / 3., 16* 20._dp / 3., 16* 5._dp / 3.],&
                          [size(i3,2), size(i3,3)])*3._dp/4.)&
                 < dptol))
        call check(error, all(abs(moment(i3, order, 2) -&
                 reshape([19._dp, 43._dp / 3., 31._dp / 3. , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3., 4* 31._dp / 3. , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3., 16* 31._dp / 3. , 16* 7.0_dp],&
                          [size(i3,1), size(i3,3)] )*2._dp/3.)&
                 < dptol))
        call check(error, all(abs(moment(i3, order, 3) -&
                 reshape([ 7._dp/3., 21._dp, 175._dp/3.,&
                           343._dp/3., 28._dp/3., 112._dp/3.,&
                           84._dp, 448._dp/3., 189._dp,&
                           700._dp/3., 847._dp/3., 336._dp],&
                           [size(i3,1), size(i3,2)] )*2./3.)&
                 < dptol))
    end subroutine
    
    subroutine test_int32_dim3_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(i3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(i3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(i3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(i3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_int32_dim3_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(i3, order, mask=(i3 < 11)) -&
                          7.7370242214532876_dp ) < dptol)
        call check(error, all(abs(moment(i3, order, 1, mask=(i3 < 45)) -&
                      reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                               80._dp, 80._dp, 32._dp/3.],&
                               [size(i3, 2), size(i3, 3)])) < dptol ))
        call check(error, all(abs(moment(i3, order, 2, mask=(i3 < 45)) -&
                      reshape([ 38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                                344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                                1376._dp/9., 992._dp/9., 4._dp&
                               ],&
                      [size(i3, 1), size(i3, 3)])) < dptol ))
        call check(error, all(abs(moment(i3, order, 3, mask=(i3 < 45)) -&
                     reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                              224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                              1694._dp/9., 36._dp&
                               ], [size(i3,1), size(i3,2)] ))&
                     < dptol ))
    end subroutine

    subroutine test_int64_dim1_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(di1, order)) < dptol)
        call check(error, abs(moment(di1, order, dim=1)) < dptol)
    end subroutine

    subroutine test_int64_dim1_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(di1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(di1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_int64_dim1_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(di1, order, mask=(di1 < 5))) < dptol)
        call check(error, abs(moment(di1, order, 1, mask=(di1 < 5))) < dptol)
    end subroutine

    subroutine test_int64_dim2_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(di2, order)) < dptol)
        call check(error, all(abs(moment(di2, order, 1)) < dptol))
        call check(error, all(abs(moment(di2, order, 2)) < dptol))
    end subroutine

    subroutine test_int64_dim2_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(di2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(di2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(di2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_int64_dim2_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(di2, order, mask=(di2 < 11))) < dptol)
        call check(error, all(abs(moment(di2, order, 1, mask=(di2 < 11))) < dptol))
        call check(error, all(abs(moment(di2, order, 2, mask=(di2 < 11))) < dptol))
    end subroutine

    subroutine test_int64_dim3_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(di3, order)) < dptol)
        call check(error, all(abs(moment(di3, order, 1)) < dptol))
        call check(error, all(abs(moment(di3, order, 2)) < dptol))
        call check(error, all(abs(moment(di3, order, 3)) < dptol))
    end subroutine
    
    subroutine test_int64_dim3_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(moment(di3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(di3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(di3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(di3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_int64_dim3_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(di3, order, mask=(di3 < 11)) ) < dptol)
        call check(error, all(abs(moment(di3, order, 1, mask=(di3 < 45))) < dptol ))
        call check(error, all(abs(moment(di3, order, 2, mask=(di3 < 45))) < dptol ))
        call check(error, all(abs(moment(di3, order, 3, mask=(di3 < 45))) < dptol ))
    end subroutine
 
    subroutine test_int64_dim1_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(di1, order) - 2._dp) < dptol)
        call check(error, abs(moment(di1, order, dim=1) - 2._dp) < dptol)
    end subroutine

    subroutine test_int64_dim1_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(di1, order, mask=.false.)))
        call check(error, ieee_is_nan(moment(di1, order, 1, mask=.false.)))
    end subroutine

    subroutine test_int64_dim1_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(di1, order, mask=(di1 < 5)) - 1.25_dp) < dptol)
        call check(error, abs(moment(di1, order, 1, mask=(di1 < 5)) - 1.25_dp) < dptol)
    end subroutine

    subroutine test_int64_dim2_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(di2, order) - 107.25_dp/9.) < dptol)
        call check(error, all(abs(moment(di2, order, 1) - [5._dp, 5._dp, 1.25_dp]) < dptol))
        call check(error, all(abs(moment(di2, order, 2) -&
                      [19._dp, 43._dp / 3., 31._dp / 3. , 7._dp]*2._dp/3.) < dptol))
    end subroutine

    subroutine test_int64_dim2_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(i2, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(di2, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(di2, order, 2, mask=.false.))))
    end subroutine

    subroutine test_int64_dim2_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(di2, order, mask=(di2 < 11))- 2.75_dp*3.) < dptol)
        call check(error, all(abs(moment(di2, order, 1, mask=(di2 < 11)) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
        call check(error, all(abs(moment(di2, order, 2, mask=(di2 < 11)) -&
                      [19._dp*2./3., 43._dp/9.*2., 0.25_dp , 0.25_dp]) < dptol))
    end subroutine

    subroutine test_int64_dim3_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(di3, order) - 153.4_dp*35./36.) < dptol)
        call check(error, all(abs(moment(di3, order, 1) -&
                 reshape([20._dp / 3., 20._dp / 3., 5._dp / 3.,&
                          4* 20._dp / 3., 4* 20._dp / 3., 4* 5._dp / 3.,&
                          16* 20._dp / 3., 16* 20._dp / 3., 16* 5._dp / 3.],&
                          [size(di3,2), size(di3,3)])*3._dp/4.)&
                 < dptol))
        call check(error, all(abs(moment(di3, order, 2) -&
                 reshape([19._dp, 43._dp / 3., 31._dp / 3. , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3., 4* 31._dp / 3. , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3., 16* 31._dp / 3. , 16* 7.0_dp],&
                          [size(di3,1), size(di3,3)] )*2._dp/3.)&
                 < dptol))
        call check(error, all(abs(moment(di3, order, 3) -&
                 reshape([ 7._dp/3., 21._dp, 175._dp/3.,&
                           343._dp/3., 28._dp/3., 112._dp/3.,&
                           84._dp, 448._dp/3., 189._dp,&
                           700._dp/3., 847._dp/3., 336._dp],&
                           [size(di3,1), size(di3,2)] )*2./3.)&
                 < dptol))
    end subroutine
    
    subroutine test_int64_dim3_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(moment(di3, order, mask=.false.)))
        call check(error, any(ieee_is_nan(moment(di3, order, 1, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(di3, order, 2, mask=.false.))))
        call check(error, any(ieee_is_nan(moment(di3, order, 3, mask=.false.))))
    end subroutine
    
    subroutine test_int64_dim3_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(di3, order, mask=(di3 < 11)) -&
                          7.7370242214532876_dp ) < dptol)
        call check(error, all(abs(moment(di3, order, 1, mask=(di3 < 45)) -&
                      reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                               80._dp, 80._dp, 32._dp/3.],&
                               [size(di3, 2), size(di3, 3)])) < dptol ))
        call check(error, all(abs(moment(di3, order, 2, mask=(di3 < 45)) -&
                      reshape([ 38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                                344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                                1376._dp/9., 992._dp/9., 4._dp&
                               ],&
                      [size(i3, 1), size(i3, 3)])) < dptol ))
        call check(error, all(abs(moment(di3, order, 3, mask=(di3 < 45)) -&
                     reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                              224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                              1694._dp/9., 36._dp&
                               ], [size(di3,1), size(di3,2)] ))&
                     < dptol ))
    end subroutine

    subroutine test_csp_dim1_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(c1, order)) < sptol)
        call check(error, abs(moment(c1, order, dim=1)) < sptol)
    end subroutine

    subroutine test_csp_dim1_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(abs(moment(c1, order, mask=.false.))))
        call check(error, ieee_is_nan(abs(moment(c1, order, 1, mask=.false.))))
    end subroutine

    subroutine test_csp_dim1_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(c1, order, mask=(aimag(c1) == 0))) < sptol)
        call check(error, abs(moment(c1, order, 1, mask=(aimag(c1) == 0))) < sptol)
    end subroutine

    subroutine test_csp_dim2_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(c2, order)) < sptol)
        call check(error, all(abs(moment(c2, order, 1)) < sptol))
        call check(error, all(abs(moment(c2, order, 2)) < sptol))
    end subroutine

    subroutine test_csp_dim2_mask_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, ieee_is_nan(abs(moment(c2, order, mask=.false.))))
        call check(error, any(ieee_is_nan(abs(moment(c2, order, 1, mask=.false.)))))
        call check(error, any(ieee_is_nan(abs(moment(c2, order, 2, mask=.false.)))))
    end subroutine

    subroutine test_csp_dim2_mask_array_order1(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 1
        call check(error, abs(moment(c2, order, mask=(aimag(c2) == 0))) < sptol)
        call check(error, all(abs(moment(c2, order, 1, mask=(aimag(c2) == 0))) < sptol))
        call check(error, any(ieee_is_nan( abs(moment(c2, order, 2,&
                          mask=(aimag(c2) == 0))))))
    end subroutine

    subroutine test_csp_dim1_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(c1, order) - (-6.459422410E-02,-0.556084037)) < sptol)
        call check(error, abs(moment(c1, order, dim=1) -&
                        (-6.459422410E-02,-0.556084037)) < sptol)
    end subroutine

    subroutine test_csp_dim1_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(abs(moment(c1, order, mask=.false.))))
        call check(error, ieee_is_nan(abs(moment(c1, order, 1, mask=.false.))))
    end subroutine

    subroutine test_csp_dim1_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(c1, order, mask=(aimag(c1) == 0)) -&
                          (8.969944715E-02,0.00000000)) < sptol)
        call check(error, abs(moment(c1, order, 1, mask=(aimag(c1) == 0)) -&
                          (8.969944715E-02,0.00000000)) < sptol)
    end subroutine

    subroutine test_csp_dim2_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(c2, order) - (-0.163121477,-1.86906016)) < sptol)
        call check(error, all(abs(moment(c2, order, 1) -&
                     [(-6.459422410E-02,-0.556084037),&
                      (-0.581347823,-5.00475645),&
                      (-0.145336956,-1.25118911)]&
                     ) < sptol))
        call check(error, all(abs(moment(c2, order, 2) -&
                     [(0.240498722,0.00000000),&
                     (-1.49895227,0.00000000),&
                     (1.15390968,0.00000000),&
                     (-0.569927275,0.00000000),&
                     (0.944405317,0.00000000)]&
                     ) < sptol))
    end subroutine

    subroutine test_csp_dim2_mask_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, ieee_is_nan(abs(moment(c2, order, mask=.false.))))
        call check(error, any(ieee_is_nan(abs(moment(c2, order, 1, mask=.false.)))))
        call check(error, any(ieee_is_nan(abs(moment(c2, order, 2, mask=.false.)))))
    end subroutine

    subroutine test_csp_dim2_mask_array_order2(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: order = 2
        call check(error, abs(moment(c2, order, mask=(aimag(c2) == 0))-&
                     (1.08109438,0.00000000)) < sptol)
        call check(error, all(abs(moment(c2, order, 1, mask=(aimag(c2)==0)) -&
                      [(8.969944715E-02,0.00000000),&
                       (0.807295084,0.00000000),&
                       (0.201823771,0.00000000)]&
                       ) < sptol))
    end subroutine

end module test_moment


program tester

    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_moment, only: collect_moment, initialize_test_data
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("moment", collect_moment) &
        ]

    call initialize_test_data()

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
