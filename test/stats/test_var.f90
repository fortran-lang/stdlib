module test_var
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds, only: sp, dp, int32, int64
    use stdlib_stats, only: var
    use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none


    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    integer(int32), parameter :: i321(5) = [1, 2, 3, 4, 5]
    integer(int64), parameter :: i641(5) = [1, 2, 3, 4, 5]

    real(sp), parameter :: s1(5) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]
    real(dp), parameter :: d1(5) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]

    real(dp), parameter :: d(4, 3) = reshape([1._dp, 3._dp, 5._dp, 7._dp,&
                                   2._dp, 4._dp, 6._dp, 8._dp,&
                                   9._dp, 10._dp, 11._dp, 12._dp], [4, 3])
    real(dp), parameter :: d3(4, 3, 3) = reshape([d, d*2, d*4], shape(d3))
    real(sp), parameter :: s(4, 3) = d
    real(sp), parameter :: s3(4, 3, 3) = reshape([s, s*2, s*4], shape(s3))

    integer(int32), parameter :: i32(4, 3) = d
    integer(int32), parameter :: i323(4, 3, 3) = d3
    integer(int64), parameter :: i64(4, 3) = d
    integer(int64), parameter :: i643(4, 3, 3) = d3

    complex(sp), parameter :: cs1(5) = [ cmplx(0.57706_sp, 0.00000_sp, sp),&
                            cmplx(0.00000_sp, 1.44065_sp, sp),&
                            cmplx(1.26401_sp, 0.00000_sp, sp),&
                            cmplx(0.00000_sp, 0.88833_sp, sp),&
                            cmplx(1.14352_sp, 0.00000_sp, sp)]
    complex(dp), parameter :: cd1(5) = [ cmplx(0.57706_dp, 0.00000_dp,kind=dp),&
                            cmplx(0.00000_dp, 1.44065_dp,kind=dp),&
                            cmplx(1.26401_dp, 0.00000_dp,kind=dp),&
                            cmplx(0.00000_dp, 0.88833_dp,kind=dp),&
                            cmplx(1.14352_dp, 0.00000_dp,kind=dp)]
    complex(sp), parameter :: cs(5,3) = reshape([cs1, cs1*3.0_sp, cs1*1.5_sp], shape(cs))
    complex(dp), parameter :: cd(5,3) = reshape([cd1, cd1*3.0_dp, cd1*1.5_dp], shape(cd))

contains


    !> Collect all exported unit tests
    subroutine collect_var(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("sp_1dim", test_sp_1dim), &
            new_unittest("sp_1dim_mask", test_sp_1dim_mask), &
            new_unittest("sp_2dim", test_sp_2dim), &
            new_unittest("sp_2dim_mask", test_sp_2dim_mask), &
            new_unittest("sp_2dim_mask_array", test_sp_2dim_mask_array), &
            new_unittest("sp_3dim", test_sp_3dim), &
            new_unittest("sp_3dim_mask", test_sp_3dim_mask), &
            new_unittest("sp_3dim_mask_array", test_sp_3dim_mask_array), &
            new_unittest("dp_1dim", test_dp_1dim), &
            new_unittest("dp_1dim_mask", test_dp_1dim_mask), &
            new_unittest("dp_1dim_mask_array", test_dp_1dim_mask_array), &
            new_unittest("dp_2dim", test_dp_2dim), &
            new_unittest("dp_2dim_mask", test_dp_2dim_mask), &
            new_unittest("dp_2dim_mask_array", test_dp_2dim_mask_array), &
            new_unittest("dp_3dim", test_dp_3dim), &
            new_unittest("dp_3dim_mask", test_dp_3dim_mask), &
            new_unittest("dp_3dim_mask_array", test_dp_3dim_mask_array), &
            new_unittest("int32_1dim", test_int32_1dim), &
            new_unittest("int32_1dim_mask", test_int32_1dim_mask), &
            new_unittest("int32_1dim_mask_array", test_int32_1dim_mask_array), &
            new_unittest("int32_2dim", test_int32_2dim), &
            new_unittest("int32_2dim_mask", test_int32_2dim_mask), &
            new_unittest("int32_2dim_mask_array", test_int32_2dim_mask_array), &
            new_unittest("int32_3dim", test_int32_3dim), &
            new_unittest("int32_3dim_mask", test_int32_3dim_mask), &
            new_unittest("int32_3dim_mask_array", test_int32_3dim_mask_array), &
            new_unittest("int64_1dim", test_int64_1dim), &
            new_unittest("int64_1dim_mask", test_int64_1dim_mask), &
            new_unittest("int641_1dim_mask_array", test_int641_1dim_mask_array), &
            new_unittest("int64_2dim", test_int64_2dim), &
            new_unittest("int64_2dim_mask", test_int64_2dim_mask), &
            new_unittest("int64_2dim_mask_array", test_int64_2dim_mask_array), &
            new_unittest("int64_3dim", test_int64_3dim), &
            new_unittest("int64_3dim_mask", test_int64_3dim_mask), &
            new_unittest("int64_3dim_mask_array", test_int64_3dim_mask_array), &
            new_unittest("csp_1dim", test_csp_1dim), &
            new_unittest("csp_1dim_mask", test_csp_1dim_mask), &
            new_unittest("csp_1dim_mask_array", test_csp_1dim_mask_array), &
            new_unittest("csp_2dim", test_csp_2dim), &
            new_unittest("csp_2dim_mask", test_csp_2dim_mask), &
            new_unittest("csp_2dim_mask_array", test_csp_2dim_mask_array), &
            new_unittest("cdp_1dim", test_cdp_1dim), &
            new_unittest("cdp_1dim_mask", test_cdp_1dim_mask), &
            new_unittest("cdp_1dim_mask_array", test_cdp_1dim_mask_array), &
            new_unittest("cdp_2dim", test_cdp_2dim), &
            new_unittest("cdp_2dim_mask", test_cdp_2dim_mask), &
            new_unittest("cdp_2dim_mask_array", test_cdp_2dim_mask_array) &
            ]
    end subroutine collect_var

    subroutine test_sp_1dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(s1) - 2.5) < sptol)
        call check(error, abs(var(s1, dim=1) - 2.5) < sptol)

    end subroutine test_sp_1dim

    subroutine test_sp_1dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(s1, .false.)))
        call check(error, ieee_is_nan(var(s1, 1, .false.)))

    end subroutine test_sp_1dim_mask

    subroutine test_sp_1dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(s1, s1 < 5) - 5./3.) < sptol)
        call check(error, ieee_is_nan((var(s1, s1 < 0.))))
        call check(error, ieee_is_nan((var(s1, s1 == 1.))))
        call check(error, abs(var(s1, 1, s1 < 5) - 5./3.) < sptol)

    end subroutine test_sp_1dim_mask_array

    subroutine test_sp_2dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(s) - 13) < sptol)
        call check(error, all( abs( var(s, 1) - [20. / 3., 20. / 3., 5. / 3.]) < sptol))
        call check(error, all( abs( var(s, 2) - [19.0, 43. / 3., 31. / 3. , 7.0]) < sptol))

    end subroutine test_sp_2dim

    subroutine test_sp_2dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(s, .false.)))
        call check(error, any(ieee_is_nan(var(s, 1, .false.))))
        call check(error, any(ieee_is_nan(var(s, 2, .false.))))

    end subroutine test_sp_2dim_mask

    subroutine test_sp_2dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(s, s < 11) - 27.5 / 3.) < sptol)
        call check(error, all( abs( var(s, 1, s < 11) - [20. / 3., 20. / 3., 0.5]) < sptol))
        call check(error, all( abs( var(s, 2, s < 11) - [19.0, 43. / 3., 0.5 , 0.5]) < sptol))

    end subroutine test_sp_2dim_mask_array

    subroutine test_sp_3dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(s3) - 153.4) < sptol)
        call check(error, all( abs( var(s3, 1) -&
                 reshape([20. / 3., 20. / 3., 5. / 3.,&
                          4* 20. / 3., 4* 20. / 3., 4* 5. / 3.,&
                          16* 20. / 3., 16* 20. / 3., 16* 5. / 3.],&
                          [size(s3,2), size(s3,3)]))&
                 < sptol))
        call check(error, all( abs( var(s3, 2) -&
                 reshape([19.0, 43. / 3., 31. / 3. , 7.0,&
                          4* 19.0, 4* 43. / 3., 4* 31. / 3. , 4* 7.0,&
                          16* 19.0, 16* 43. / 3., 16* 31. / 3. , 16* 7.0],&
                          [size(s3,1), size(s3,3)] ))&
                 < sptol))
        call check(error, all(abs( var(s3, 3) -&
                 reshape([ 7./3., 21., 175./3.,&
                           343./3., 28./3., 112./3.,&
                           84., 448./3., 189.,&
                           700./3., 847./3., 336.], [size(s3,1), size(s3,2)] ))&
                 < sptol))

    end subroutine test_sp_3dim

    subroutine test_sp_3dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(s3, .false.)))
        call check(error, any(ieee_is_nan(var(s3, 1, .false.))))
        call check(error, any(ieee_is_nan(var(s3, 2, .false.))))
        call check(error, any(ieee_is_nan(var(s3, 3, .false.))))

    end subroutine test_sp_3dim_mask

    subroutine test_sp_3dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(s3, s3 < 11) - 8.2205877_sp) < sptol)
        call check(error, all( abs( var(s3, 1, s3 < 45) -&
                  reshape([20./3., 20./3., 5./3.,  80./3., 80./3., 20./3.,&
                           320./3., 320./3., 16.],&
                  [size(s3, 2), size(s3, 3)])) < sptol ))
        call check(error, any( ieee_is_nan( var(s3, 2, s3 < 25))))
        call check(error, all( abs( var(s3, 3, s3 < 25) -&
                 reshape([ 7./3., 21., 175./3.,&
                           24.5, 28./3., 112./3.,&
                           84., 32., 40.5,&
                           50., 60.5, 72.], [size(s3,1), size(s3,2)] ))&
                 < sptol ))

    end subroutine test_sp_3dim_mask_array

    subroutine test_dp_1dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(d1) - 2.5) < dptol)
        call check(error, abs(var(d1, 1) - 2.5) < dptol)

    end subroutine test_dp_1dim

    subroutine test_dp_1dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(d1, .false.)))
        call check(error, ieee_is_nan(var(d1, 1, .false.)))

    end subroutine test_dp_1dim_mask

    subroutine test_dp_1dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(d1, d1 < 5) - 5._dp/3._dp) < dptol)
        call check(error, ieee_is_nan((var(d1, d1 < 0.))))
        call check(error, ieee_is_nan((var(d1, d1 == 1.))))
        call check(error, abs(var(d1, 1, d1 < 5) - 5._dp/3._dp) < dptol)

    end subroutine test_dp_1dim_mask_array

    subroutine test_dp_2dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(d) - 13) < dptol)
        call check(error, all( abs( var(d,1) -&
            [20._dp/3._dp, 20._dp/3._dp, 5._dp/3._dp]) < dptol))
        call check(error, all( abs( var(d,2) -&
            [19.0_dp, 43._dp/3._dp, 31._dp/3._dp, 7.0_dp]) < dptol))

    end subroutine test_dp_2dim

    subroutine test_dp_2dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

    call check(error, ieee_is_nan(var(d, .false.)))
    call check(error, any(ieee_is_nan(var(d, 1, .false.))))
    call check(error, any(ieee_is_nan(var(d, 2, .false.))))

    end subroutine test_dp_2dim_mask

    subroutine test_dp_2dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

    call check(error, abs(var(d, d < 11) - 27.5_dp / 3._dp) < dptol)
    call check(error, all( abs( var(d, 1, d < 11) -&
                 [20._dp / 3._dp, 20._dp / 3._dp, 0.5_dp]) < dptol))
    call check(error, all( abs( var(d, 2, d < 11) -&
                 [19.0_dp, 43._dp / 3._dp, 0.5_dp, 0.5_dp]) < dptol))

    end subroutine test_dp_2dim_mask_array

    subroutine test_dp_3dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(d3) - 153.4_dp) < dptol)
        call check(error, all( abs( var(d3, 1) -&
                 reshape([20._dp / 3._dp, 20._dp / 3._dp, 5._dp / 3._dp,&
                          4* 20._dp / 3._dp, 4* 20._dp / 3._dp, 4* 5._dp / 3._dp,&
                          16* 20._dp / 3._dp, 16* 20._dp / 3._dp, 16* 5._dp / 3._dp],&
                          [size(d3,2), size(d3,3)]))&
                 < dptol))
        call check(error, all( abs( var(d3, 2) -&
                 reshape([19.0_dp, 43._dp / 3._dp, 31._dp / 3._dp , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3._dp, 4* 31._dp / 3._dp , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3._dp, 16* 31._dp / 3._dp ,&
                          16* 7.0_dp],&
                          [size(d3,1), size(d3,3)] ))&
                 < dptol))
        call check(error, all(abs( var(d3, 3) -&
                 reshape([ 7._dp/3._dp, 21._dp, 175._dp/3._dp,&
                           343._dp/3._dp, 28._dp/3._dp, 112._dp/3._dp,&
                           84._dp, 448._dp/3._dp, 189._dp,&
                           700._dp/3._dp, 847._dp/3._dp, 336._dp],&
                           [size(d3,1), size(d3,2)] ))&
                 < dptol))

    end subroutine test_dp_3dim

    subroutine test_dp_3dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(d3, .false.)))
        call check(error, any(ieee_is_nan(var(d3, 1, .false.))))
        call check(error, any(ieee_is_nan(var(d3, 2, .false.))))
        call check(error, any(ieee_is_nan(var(d3, 3, .false.))))

    end subroutine test_dp_3dim_mask

    subroutine test_dp_3dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(d3, d3 < 25) - 46.041379310344823_dp) < dptol)
        call check(error, all( abs( var(d3, 1, d3 < 45) -&
                  reshape([20._dp/3._dp, 20._dp/3._dp, 5._dp/3._dp,&
                           80._dp/3._dp, 80._dp/3._dp, 20._dp/3._dp,&
                           320._dp/3._dp, 320._dp/3._dp, 16._dp],&
                           [size(d3, 2), size(d3, 3)]))&
                  < dptol ))
        call check(error, any( ieee_is_nan( var(d3, 2, d3 < 25))))
        call check(error, all( abs( var(d3, 3, d3 < 25) -&
                 reshape([ 7._dp/3._dp, 21._dp, 175._dp/3._dp,&
                           24.5_dp, 28._dp/3._dp, 112._dp/3._dp,&
                           84._dp, 32._dp, 40.5_dp,&
                           50._dp, 60.5_dp, 72._dp],&
                           [size(d3,1), size(d3,2)] ))&
                 < dptol ))

    end subroutine test_dp_3dim_mask_array

    subroutine test_int32_1dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i321) - 2.5) < dptol)
        call check(error, abs(var(i321, 1) - 2.5) < dptol)

    end subroutine test_int32_1dim

    subroutine test_int32_1dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(i321, .false.)))
        call check(error, ieee_is_nan(var(i321, 1, .false.)))

    end subroutine test_int32_1dim_mask

    subroutine test_int32_1dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i321, i321 < 5) - 5._dp/3._dp) < dptol)
        call check(error, ieee_is_nan((var(i321, i321 < 0))))
        call check(error, ieee_is_nan((var(i321, i321 == 1))))
        call check(error, abs(var(i321, 1, i321 < 5) - 5._dp/3._dp) < dptol)

    end subroutine test_int32_1dim_mask_array

    subroutine test_int32_2dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i32) - 13) < dptol)
        call check(error, all( abs( var(i32,1) -&
            [20._dp/3._dp, 20._dp/3._dp, 5._dp/3._dp]) < dptol))
        call check(error, all( abs( var(i32,2) -&
            [19.0_dp, 43._dp/3._dp, 31._dp/3._dp, 7.0_dp]) < dptol))

    end subroutine test_int32_2dim

    subroutine test_int32_2dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(i32, .false.)))
        call check(error, any(ieee_is_nan(var(i32, 1, .false.))))
        call check(error, any(ieee_is_nan(var(i32, 2, .false.))))

    end subroutine test_int32_2dim_mask

    subroutine test_int32_2dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i32, i32 < 11) - 27.5_dp / 3._dp) < dptol)
        call check(error, all( abs( var(i32, 1, i32 < 11) -&
            [20._dp / 3._dp, 20._dp / 3._dp, 0.5_dp]) < dptol))
        call check(error, all( abs( var(i32, 2, i32 < 11) -&
            [19.0_dp, 43._dp / 3._dp, 0.5_dp, 0.5_dp]) < dptol))

    end subroutine test_int32_2dim_mask_array

    subroutine test_int32_3dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i323) - 153.4_dp) < dptol)
        call check(error, all( abs( var(i323, 1) -&
                 reshape([20._dp / 3._dp, 20._dp / 3._dp, 5._dp / 3._dp,&
                          4* 20._dp / 3._dp, 4* 20._dp / 3._dp, 4* 5._dp / 3._dp,&
                          16* 20._dp / 3._dp, 16* 20._dp / 3._dp, 16* 5._dp / 3._dp],&
                          [size(i323,2), size(i323,3)]))&
                 < dptol))
        call check(error, all( abs( var(i323, 2) -&
                 reshape([19.0_dp, 43._dp / 3._dp, 31._dp / 3._dp , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3._dp, 4* 31._dp / 3._dp , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3._dp, 16* 31._dp / 3._dp ,&
                          16* 7.0_dp],&
                          [size(i323,1), size(i323,3)] ))&
                 < dptol))
        call check(error, all(abs( var(i323, 3) -&
                 reshape([ 7._dp/3._dp, 21._dp, 175._dp/3._dp,&
                           343._dp/3._dp, 28._dp/3._dp, 112._dp/3._dp,&
                           84._dp, 448._dp/3._dp, 189._dp,&
                           700._dp/3._dp, 847._dp/3._dp, 336._dp],&
                           [size(i323,1), size(i323,2)] ))&
                 < dptol))

    end subroutine test_int32_3dim

    subroutine test_int32_3dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(i323, .false.)))
        call check(error, any(ieee_is_nan(var(i323, 1, .false.))))
        call check(error, any(ieee_is_nan(var(i323, 2, .false.))))
        call check(error, any(ieee_is_nan(var(i323, 3, .false.))))

    end subroutine test_int32_3dim_mask

    subroutine test_int32_3dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i323, i323 < 25) - 46.041379310344823_dp) < dptol)
        call check(error, all( abs( var(i323, 1, i323 < 45) -&
                  reshape([20._dp/3._dp, 20._dp/3._dp, 5._dp/3._dp,&
                           80._dp/3._dp, 80._dp/3._dp, 20._dp/3._dp,&
                           320._dp/3._dp, 320._dp/3._dp, 16._dp],&
                           [size(i323, 2), size(i323, 3)]))&
                  < dptol ))
        call check(error, any( ieee_is_nan( var(i323, 2, i323 < 25))))
        call check(error, all( abs( var(i323, 3, i323 < 25) -&
                 reshape([ 7._dp/3._dp, 21._dp, 175._dp/3._dp,&
                           24.5_dp, 28._dp/3._dp, 112._dp/3._dp,&
                           84._dp, 32._dp, 40.5_dp,&
                           50._dp, 60.5_dp, 72._dp],&
                           [size(i323,1), size(i323,2)] ))&
                 < dptol ))

    end subroutine test_int32_3dim_mask_array

    subroutine test_int64_1dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i641) - 2.5) < dptol)
        call check(error, abs(var(i641, 1) - 2.5) < dptol)

    end subroutine test_int64_1dim

    subroutine test_int64_1dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(i641, .false.)))
        call check(error, ieee_is_nan(var(i641, 1, .false.)))

    end subroutine test_int64_1dim_mask

    subroutine test_int641_1dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i641, i641 < 5) - 5._dp/3._dp) < dptol)
        call check(error, ieee_is_nan((var(i641, i641 < 0))))
        call check(error, ieee_is_nan((var(i641, i641 == 1))))
        call check(error, abs(var(i641, 1, i641 < 5) - 5._dp/3._dp) < dptol)

    end subroutine test_int641_1dim_mask_array

    subroutine test_int64_2dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i64) - 13) < dptol)
        call check(error, all( abs( var(i64,1) -&
            [20._dp/3._dp, 20._dp/3._dp, 5._dp/3._dp]) < dptol))
        call check(error, all( abs( var(i64,2) -&
            [19.0_dp, 43._dp/3._dp, 31._dp/3._dp, 7.0_dp]) < dptol))

    end subroutine test_int64_2dim

    subroutine test_int64_2dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(i64, .false.)))
        call check(error, any(ieee_is_nan(var(i64, 1, .false.))))
        call check(error, any(ieee_is_nan(var(i64, 2, .false.))))

    end subroutine test_int64_2dim_mask

    subroutine test_int64_2dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i64, i64 < 11) - 27.5_dp / 3._dp) < dptol)
        call check(error, all( abs( var(i64, 1, i64 < 11) -&
            [20._dp / 3._dp, 20._dp / 3._dp, 0.5_dp]) < dptol))
        call check(error, all( abs( var(i64, 2, i64 < 11) -&
            [19.0_dp, 43._dp / 3._dp, 0.5_dp, 0.5_dp]) < dptol))

    end subroutine test_int64_2dim_mask_array

    subroutine test_int64_3dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i643) - 153.4_dp) < dptol)
        call check(error, all( abs( var(i643, 1) -&
                 reshape([20._dp / 3._dp, 20._dp / 3._dp, 5._dp / 3._dp,&
                          4* 20._dp / 3._dp, 4* 20._dp / 3._dp, 4* 5._dp / 3._dp,&
                          16* 20._dp / 3._dp, 16* 20._dp / 3._dp, 16* 5._dp / 3._dp],&
                          [size(i643,2), size(i643,3)]))&
                 < dptol))
        call check(error, all( abs( var(i643, 2) -&
                 reshape([19.0_dp, 43._dp / 3._dp, 31._dp / 3._dp , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3._dp, 4* 31._dp / 3._dp , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3._dp, 16* 31._dp / 3._dp ,&
                          16* 7.0_dp],&
                          [size(i643,1), size(i643,3)] ))&
                 < dptol))
        call check(error, all(abs( var(i643, 3) -&
                 reshape([ 7._dp/3._dp, 21._dp, 175._dp/3._dp,&
                           343._dp/3._dp, 28._dp/3._dp, 112._dp/3._dp,&
                           84._dp, 448._dp/3._dp, 189._dp,&
                           700._dp/3._dp, 847._dp/3._dp, 336._dp],&
                           [size(i643,1), size(i643,2)] ))&
                 < dptol))

    end subroutine test_int64_3dim

    subroutine test_int64_3dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(i643, .false.)))
        call check(error, any(ieee_is_nan(var(i643, 1, .false.))))
        call check(error, any(ieee_is_nan(var(i643, 2, .false.))))
        call check(error, any(ieee_is_nan(var(i643, 3, .false.))))

    end subroutine test_int64_3dim_mask

    subroutine test_int64_3dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(i643, i643 < 25) - 46.041379310344823_dp) < dptol)
        call check(error, all( abs( var(i643, 1, i643 < 45) -&
                  reshape([20._dp/3._dp, 20._dp/3._dp, 5._dp/3._dp,&
                           80._dp/3._dp, 80._dp/3._dp, 20._dp/3._dp,&
                           320._dp/3._dp, 320._dp/3._dp, 16._dp],&
                           [size(i643, 2), size(i643, 3)]))&
                  < dptol ))
        call check(error, any( ieee_is_nan( var(i643, 2, i643 < 25))))
        call check(error, all( abs( var(i643, 3, i643 < 25) -&
                 reshape([ 7._dp/3._dp, 21._dp, 175._dp/3._dp,&
                           24.5_dp, 28._dp/3._dp, 112._dp/3._dp,&
                           84._dp, 32._dp, 40.5_dp,&
                           50._dp, 60.5_dp, 72._dp],&
                           [size(i643,1), size(i643,2)] ))&
                 < dptol ))

    end subroutine test_int64_3dim_mask_array

    subroutine test_csp_1dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cs1) - (var(real(cs1)) + var(aimag(cs1)))) < sptol)
        call check(error, abs(var(cs1, dim=1) - (var(real(cs1),1) + var(aimag(cs1), 1)) ) < sptol)

    end subroutine test_csp_1dim

    subroutine test_csp_1dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(cs1, .false.)))
        call check(error, ieee_is_nan(var(cs1, 1, .false.)))

    end subroutine test_csp_1dim_mask

    subroutine test_csp_1dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cs1, aimag(cs1) == 0) - var(real(cs1), aimag(cs1) == 0)) < sptol)
        call check(error, abs(var(cs1, 1, aimag(cs1) == 0) - var(real(cs1), 1, aimag(cs1) == 0)) < sptol)

    end subroutine test_csp_1dim_mask_array

    subroutine test_csp_2dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cs) - (var(real(cs)) + var(aimag(cs)))) < sptol)
        call check(error, all( abs( var(cs, 1) - (var(real(cs), 1) + var(aimag(cs), 1))) < sptol))
        call check(error, all( abs( var(cs, 2) - (var(real(cs), 2) + var(aimag(cs), 2))) < sptol))

    end subroutine test_csp_2dim

    subroutine test_csp_2dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(cs, .false.)))
        call check(error, any(ieee_is_nan(var(cs, 1, .false.))))
        call check(error, any(ieee_is_nan(var(cs, 2, .false.))))

    end subroutine test_csp_2dim_mask


    subroutine test_csp_2dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cs, aimag(cs) == 0) - var(real(cs), aimag(cs) == 0)) < sptol)
        call check(error, all( abs( var(cs, 1, aimag(cs) == 0) - var(real(cs), 1, aimag(cs) == 0)) < sptol))
        call check(error, any( ieee_is_nan( var(cs, 2, aimag(cs) == 0))))

    end subroutine test_csp_2dim_mask_array

    subroutine test_cdp_1dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cd1) - (var(real(cd1)) + var(aimag(cd1)))) < dptol)
        call check(error, abs(var(cd1, dim=1) - (var(real(cd1),1) + var(aimag(cd1), 1)) ) < dptol)

    end subroutine test_cdp_1dim

    subroutine test_cdp_1dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(cd1, .false.)))
        call check(error, ieee_is_nan(var(cd1, 1, .false.)))

    end subroutine test_cdp_1dim_mask

    subroutine test_cdp_1dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cd1, aimag(cd1) == 0) - var(real(cd1), aimag(cd1) == 0)) < dptol)
        call check(error, abs(var(cd1, 1, aimag(cd1) == 0) - var(real(cd1), 1, aimag(cd1) == 0)) < dptol)

    end subroutine test_cdp_1dim_mask_array

    subroutine test_cdp_2dim(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cd) - (var(real(cd)) + var(aimag(cd)))) < dptol)
        call check(error, all( abs( var(cd, 1) - (var(real(cd), 1) + var(aimag(cd), 1))) < dptol))
        call check(error, all( abs( var(cd, 2) - (var(real(cd), 2) + var(aimag(cd), 2))) < dptol))

    end subroutine test_cdp_2dim

    subroutine test_cdp_2dim_mask(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, ieee_is_nan(var(cd, .false.)))
        call check(error, any(ieee_is_nan(var(cd, 1, .false.))))
        call check(error, any(ieee_is_nan(var(cd, 2, .false.))))

    end subroutine test_cdp_2dim_mask

    subroutine test_cdp_2dim_mask_array(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, abs(var(cd, aimag(cd) == 0) - var(real(cd), aimag(cd) == 0)) < dptol)
        call check(error, all( abs( var(cd, 1, aimag(cd) == 0) - var(real(cd), 1, aimag(cd) == 0)) < dptol))
        call check(error, any( ieee_is_nan( var(cd, 2, aimag(cd) == 0))))

    end subroutine test_cdp_2dim_mask_array

end module


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_var, only : collect_var
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("var", collect_var) &
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
