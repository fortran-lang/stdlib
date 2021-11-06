module test_varn
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
  use stdlib_kinds, only: sp, dp, int32
  use stdlib_stats, only: var
  use testdrive, only: new_unittest, unittest_type, error_type, check
  implicit none

  private
  public :: collect_varn, initialize_test_data

  real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
  real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

  integer(int32) :: i321(5) = [1, 2, 3, 4, 5]

  integer(int32), allocatable :: i32(:,:), i323(:,:,:)

  real(sp) :: s1(5) = [1._sp, 2._sp, 3._sp, 4._sp, 5._sp]
  real(dp) :: d1(5) = [1._dp, 2._dp, 3._dp, 4._dp, 5._dp]

  real(sp), allocatable :: s(:,:), s3(:,:,:)
  real(dp), allocatable :: d3(:,:,:)
  real(dp) :: d(4,3) = reshape([1._dp, 3._dp, 5._dp, 7._dp, &
                                2._dp, 4._dp, 6._dp, 8._dp, &
                                9._dp, 10._dp, 11._dp, 12._dp], [4, 3])

  complex(dp) :: cd1(5) = [(0.57706_dp, 0.00000_dp), &
                           (0.00000_dp, 1.44065_dp), &
                           (1.26401_dp, 0.00000_dp), &
                           (0.00000_dp, 0.88833_dp), &
                           (1.14352_dp, 0.00000_dp)]
  complex(dp) :: cd(5,3)

contains

  subroutine initialize_test_data()

    s = d
    i32 = d

    allocate(s3(size(s, 1),size(s, 2),3))
    s3(:,:,1) = s
    s3(:,:,2) = s * 2
    s3(:,:,3) = s * 4

    allocate(d3(size(d, 1),size(d, 2),3))
    d3(:,:,1) = d
    d3(:,:,2) = d * 2
    d3(:,:,3) = d * 4

    allocate(i323(size(i32, 1),size(i32, 2),3))
    i323(:,:,1) = i32
    i323(:,:,2) = i32 * 2
    i323(:,:,3) = i32 * 4

    cd(:,1) = cd1
    cd(:,2) = cd1 * 3_sp
    cd(:,3) = cd1 * 1.5_sp

  end subroutine initialize_test_data

  !> Collect all exported unit tests
  subroutine collect_varn(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest('varn_sp_1dim', test_sp_1dim), &
      new_unittest('varn_sp_1dim_mask', test_sp_1dim_mask), &
      new_unittest('varn_sp_1dim_mask_array', test_sp_1dim_mask_array), &
      new_unittest('varn_sp_2dim', test_sp_2dim), &
      new_unittest('varn_sp_2dim_mask', test_sp_2dim_mask), &
      new_unittest('varn_sp_2dim_mask_array', test_sp_2dim_mask_array), &
      new_unittest('varn_sp_3dim', test_sp_3dim), &
      new_unittest('varn_sp_3dim_mask', test_sp_3dim_mask), &
      new_unittest('varn_sp_3dim_mask_array', test_sp_3dim_mask_array), &
      new_unittest('varn_dp_1dim', test_dp_1dim), &
      new_unittest('varn_dp_1dim_mask', test_dp_1dim_mask), &
      new_unittest('varn_dp_1dim_mask_array', test_dp_1dim_mask_array), &
      new_unittest('varn_dp_2dim', test_dp_2dim), &
      new_unittest('varn_dp_2dim_mask', test_dp_2dim_mask), &
      new_unittest('varn_dp_2dim_mask_array', test_dp_2dim_mask_array), &
      new_unittest('varn_dp_3dim', test_dp_3dim), &
      new_unittest('varn_dp_3dim_mask', test_dp_3dim_mask), &
      new_unittest('varn_dp_3dim_mask_array', test_dp_3dim_mask_array), &
      new_unittest('varn_int32_1dim', test_int32_1dim), &
      new_unittest('varn_int32_1dim_mask', test_int32_1dim_mask), &
      new_unittest('varn_int32_1dim_mask_array', test_int32_1dim_mask_array), &
      new_unittest('varn_int32_2dim', test_int32_2dim), &
      new_unittest('varn_int32_2dim_mask', test_int32_2dim_mask), &
      new_unittest('varn_int32_2dim_mask_array', test_int32_2dim_mask_array), &
      new_unittest('varn_int32_3dim', test_int32_3dim), &
      new_unittest('varn_int32_3dim_mask', test_int32_3dim_mask), &
      new_unittest('varn_int32_3dim_mask_array', test_int32_3dim_mask_array), &
      new_unittest('varn_cdp_1dim', test_cdp_1dim), &
      new_unittest('varn_cdp_1dim_mask', test_cdp_1dim_mask), &
      new_unittest('varn_cdp_1dim_mask_array', test_cdp_1dim_mask_array), &
      new_unittest('varn_cdp_2dim', test_cdp_2dim), &
      new_unittest('varn_cdp_2dim_mask', test_cdp_2dim_mask), &
      new_unittest('varn_cdp_2dim_mask_array', test_cdp_2dim_mask_array) &
    ]

  end

  subroutine test_sp_1dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(s1, corrected=.false.) - 2.5*(4./5.)) < sptol)
    call check(error, abs(var(s1, dim=1, corrected=.false.) - 2.5*(4./5.)) < sptol)
  end

  subroutine test_sp_1dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(s1, .false., corrected=.false.)))
    call check(error, ieee_is_nan(var(s1, 1, .false., corrected=.false.)))
  end

  subroutine test_sp_1dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(s1, s1 < 5, corrected=.false.) - 5./4.) < sptol)
    call check(error, ieee_is_nan((var(s1, s1 < 0., corrected=.false.))))
    call check(error, abs(var(s1, s1 == 1., corrected=.false.)) < sptol)
    call check(error, abs(var(s1, 1, s1 < 5, corrected=.false.) - 5./4.) < sptol)
  end

  subroutine test_sp_2dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(s, corrected=.false.) - 13.*11./12.) < sptol)
    call check(error, all(abs(var(s, 1, corrected=.false.) &
                              - [20., 20., 5.]/4.) < sptol))
    call check(error, all(abs(var(s, 2, corrected=.false.) &
                              - [19., 43./3., 31./ 3., 7.0]*2./3.) < sptol))
  end

  subroutine test_sp_2dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(s, .false., corrected=.false.)))
    call check(error, any(ieee_is_nan(var(s, 1, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(s, 2, .false., corrected=.false.))))
  end

  subroutine test_sp_2dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(s, s < 11, corrected=.false.) - 2.75*3.) < sptol)
    call check(error, all(abs(var(s, 1, s < 11, corrected=.false.) &
                              - [5., 5., 0.25]) < sptol))
    call check(error, all(abs(var(s, 2, s < 11, corrected=.false.) &
                              - [19.0*2./3., 43./9.*2., 0.25 , 0.25]) < sptol))
  end

  subroutine test_sp_3dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(s3, corrected=.false.) - 153.4*35./36.) < sptol)
    call check(error, all(abs(var(s3, 1, corrected=.false.) -&
                 reshape([20. / 3., 20. / 3., 5. / 3.,&
                          4* 20. / 3., 4* 20. / 3., 4* 5. / 3.,&
                          16* 20. / 3., 16* 20. / 3., 16* 5. / 3.],&
                          [size(s3, 2), size(s3, 3)])*3./4.)&
                 < sptol))
    call check(error, all(abs(var(s3, 2, corrected=.false.) -&
                 reshape([19.0, 43. / 3., 31. / 3. , 7.0,&
                          4* 19.0, 4* 43. / 3., 4* 31. / 3. , 4* 7.0,&
                          16* 19.0, 16* 43. / 3., 16* 31. / 3. , 16* 7.0],&
                          [size(s3, 1), size(s3, 3)])*2./3.)&
                 < sptol))
    call check(error, all(abs(var(s3, 3, corrected=.false.) -&
                 reshape([ 7./3., 21., 175./3.,&
                           343./3., 28./3., 112./3.,&
                           84., 448./3., 189.,&
                           700./3., 847./3., 336.], [size(s3,1), size(s3,2)] )*2./3.)&
                 < sptol))
  end

  subroutine test_sp_3dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(s3, .false., corrected=.false.)))
    call check(error, any(ieee_is_nan(var(s3, 1, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(s3, 2, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(s3, 3, .false., corrected=.false.))))
  end

  subroutine test_sp_3dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(s3, s3 < 11, corrected=.false.) - 7.73702383_sp) < sptol)
    call check(error, all( abs( var(s3, 1, s3 < 45, corrected=.false.) -&
                  reshape([5., 5., 1.25,  20., 20., 5., 80., 80., 32./3.],&
                  [size(s3, 2), size(s3, 3)])) < sptol ))
    call check(error, all( abs( var(s3, 2, s3 < 45, corrected=.false.) -&
                  reshape([ 38./3., 86./9., 6.88888931, 14./3., 152./3.,&
                            38.2222214, 27.5555573, 18.6666660, 202.666672,&
                            152.888885, 110.222229, 4.&
                           ],&
                  [size(s3, 1), size(s3, 3)])) < sptol ))
    call check(error, all( abs( var(s3, 3, s3 < 45, corrected=.false.) -&
                 reshape([1.555555, 14., 38.888888, 76.222222, 6.2222222,&
                          24.888888, 56., 99.5555, 126., 155.555555, 188.22222, 36.&
                           ], [size(s3,1), size(s3,2)] ))&
                 < sptol ))
  end

  subroutine test_dp_1dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(d1, corrected=.false.) - 2.5_dp*(4._dp/5.)) < dptol)
    call check(error, abs(var(d1, dim=1, corrected=.false.) - 2.5_dp*(4._dp/5.)) < dptol)
  end

  subroutine test_dp_1dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(d1, .false., corrected=.false.)))
    call check(error, ieee_is_nan(var(d1, 1, .false., corrected=.false.)))
  end

  subroutine test_dp_1dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(d1, d1 < 5, corrected=.false.) - 5._dp/4.) < dptol)
    call check(error, ieee_is_nan((var(d1, d1 < 0, corrected=.false.))))
    call check(error, abs(var(d1, d1 == 1, corrected=.false.)) < dptol)
    call check(error, abs(var(d1, 1, d1 < 5, corrected=.false.) - 5._dp/4.) < dptol)
  end

  subroutine test_dp_2dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(d, corrected=.false.) - 13._dp*11./12.) < dptol)
    call check(error, all( abs( var(d, 1, corrected=.false.) - [20., 20., 5.]/4._dp) < dptol))
    call check(error, all( abs( var(d, 2, corrected=.false.) -&
                      [38._dp, 86._dp / 3._dp, 62._dp / 3._dp , 14._dp]/3._dp) < dptol))
  end

  subroutine test_dp_2dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(d, .false., corrected=.false.)))
    call check(error, any(ieee_is_nan(var(d, 1, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(d, 2, .false., corrected=.false.))))
  end

  subroutine test_dp_2dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(d, d < 11, corrected=.false.) - 2.75_dp*3._dp) < dptol)
    call check(error, all( abs( var(d, 1, d < 11, corrected=.false.) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
    call check(error, all( abs( var(d, 2, d < 11, corrected=.false.) -&
                      [38._dp/3., 86._dp/9., 0.25_dp , 0.25_dp]) < dptol))
  end

  subroutine test_dp_3dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(d3, corrected=.false.) - 153.4_dp*35._dp/36._dp) < dptol)
    call check(error, all( abs( var(d3, 1, corrected=.false.) -&
                 reshape([20._dp , 20._dp , 5._dp ,&
                          4* 20._dp , 4* 20._dp , 4* 5._dp ,&
                          16* 20._dp , 16* 20._dp , 16* 5._dp ],&
                          [size(d3,2), size(d3,3)])/4._dp)&
                 < dptol))
    call check(error, all( abs( var(d3, 2, corrected=.false.) -&
                 reshape([38._dp, 86. / 3._dp, 62. / 3._dp , 14._dp,&
                          8* 19._dp, 8* 43. / 3._dp, 8* 31. / 3._dp, 8* 7._dp,&
                          32* 19._dp, 32* 43. / 3._dp, 32* 31. / 3._dp, 32* 7._dp],&
                          [size(d3,1), size(d3,3)] )/3._dp)&
                 < dptol))
    call check(error, all(abs( var(d3, 3, corrected=.false.) -&
                 reshape([7._dp/3., 21._dp, 175._dp/3.,&
                          343._dp/3., 28._dp/3., 112._dp/3.,&
                          84._dp, 448._dp/3., 189._dp,&
                          700._dp/3., 847._dp/3., 336._dp],&
                          [size(d3, 1), size(d3, 2)] )*2._dp/3._dp)&
                 < dptol))
  end

  subroutine test_dp_3dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(d3, .false., corrected=.false.)))
    call check(error, any(ieee_is_nan(var(d3, 1, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(d3, 2, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(d3, 3, .false., corrected=.false.))))
  end

  subroutine test_dp_3dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(d3, d3 < 11, corrected=.false.) -&
                     7.7370242214532876_dp) < dptol)
    call check(error, all(abs(var(d3, 1, d3 < 45, corrected=.false.) -&
                  reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                           80._dp, 80._dp, 32._dp/3.],&
                           [size(d3, 2), size(d3, 3)])) < dptol))
    call check(error, all(abs( var(d3, 2, d3 < 45, corrected=.false.) -&
                  reshape([38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                           344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                           1376._dp/9., 992._dp/9., 4._dp&
                           ],&
                  [size(d3, 1), size(d3, 3)])) < dptol))
    call check(error, all(abs(var(d3, 3, d3 < 45, corrected=.false.) -&
                 reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                          224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                          1694._dp/9., 36._dp&
                           ], [size(d3, 1), size(d3, 2)]))&
                 < dptol ))
  end

  subroutine test_int32_1dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(i321, corrected=.false.) - 2.5_dp*(4._dp/5.)) < dptol)
    call check(error, abs(var(i321, dim=1, corrected=.false.) - 2.5_dp*(4._dp/5.)) < dptol)
  end

  subroutine test_int32_1dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(i321, .false., corrected=.false.)))
    call check(error, ieee_is_nan(var(i321, 1, .false., corrected=.false.)))
  end

  subroutine test_int32_1dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(i321, i321 < 5, corrected=.false.) - 5._dp/4.) < dptol)
    call check(error, ieee_is_nan((var(i321, i321 < 0, corrected=.false.))))
    call check(error, abs(var(i321, i321 == 1, corrected=.false.)) < dptol)
    call check(error, abs(var(i321, 1, i321 < 5, corrected=.false.) - 5._dp/4.) < dptol)
  end

  subroutine test_int32_2dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(i32, corrected=.false.) - 13._dp*11./12.) < dptol)
    call check(error, all(abs(var(i32, 1, corrected=.false.) -&
                      [20., 20., 5.]/4._dp) < dptol))
    call check(error, all(abs(var(i32, 2, corrected=.false.) -&
                      [38._dp, 86._dp / 3._dp, 62._dp / 3._dp , 14._dp]/3._dp) < dptol))
  end

  subroutine test_int32_2dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(i32, .false., corrected=.false.)))
    call check(error, any(ieee_is_nan(var(i32, 1, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(i32, 2, .false., corrected=.false.))))
  end

  subroutine test_int32_2dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(i32, i32 < 11, corrected=.false.) - 2.75_dp*3._dp) < dptol)
    call check(error, all(abs(var(i32, 1, i32 < 11, corrected=.false.) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
    call check(error, all(abs(var(i32, 2, i32 < 11, corrected=.false.) -&
                      [38._dp/3., 86._dp/9., 0.25_dp , 0.25_dp]) < dptol))
  end

  subroutine test_int32_3dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(i323, corrected=.false.) - 153.4_dp*35._dp/36._dp) < dptol)
    call check(error, all(abs(var(i323, 1, corrected=.false.) -&
                 reshape([20._dp , 20._dp , 5._dp ,&
                          4* 20._dp , 4* 20._dp , 4* 5._dp ,&
                          16* 20._dp , 16* 20._dp , 16* 5._dp ],&
                          [size(i323,2), size(i323,3)])/4._dp)&
                 < dptol))
    call check(error, all(abs(var(i323, 2, corrected=.false.) -&
                 reshape([38._dp, 86. / 3._dp, 62. / 3._dp , 14._dp,&
                          8* 19._dp, 8* 43. / 3._dp, 8* 31. / 3._dp, 8* 7._dp,&
                          32* 19._dp, 32* 43. / 3._dp, 32* 31. / 3._dp, 32* 7._dp],&
                          [size(i323,1), size(i323,3)] )/3._dp)&
                 < dptol))
    call check(error, all(abs(var(i323, 3, corrected=.false.) -&
                 reshape([ 7._dp/3., 21._dp, 175._dp/3.,&
                           343._dp/3., 28._dp/3., 112._dp/3.,&
                           84._dp, 448._dp/3., 189._dp,&
                           700._dp/3., 847._dp/3., 336._dp],&
                           [size(i323,1), size(i323,2)] )*2._dp/3._dp)&
                 < dptol))
  end

  subroutine test_int32_3dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(i323, .false., corrected=.false.)))
    call check(error, any(ieee_is_nan(var(i323, 1, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(i323, 2, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(i323, 3, .false., corrected=.false.))))
  end

  subroutine test_int32_3dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(i323, i323 < 11, corrected=.false.) -&
                     7.7370242214532876_dp) < dptol)
    call check(error, all(abs(var(i323, 1, i323 < 45, corrected=.false.) -&
                  reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                           80._dp, 80._dp, 32._dp/3.],&
                           [size(i323, 2), size(i323, 3)])) < dptol ))
    call check(error, all(abs(var(i323, 2, i323 < 45, corrected=.false.) -&
                  reshape([ 38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                            344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                            1376._dp/9., 992._dp/9., 4._dp&
                           ],&
                  [size(i323, 1), size(i323, 3)])) < dptol ))
    call check(error, all(abs(var(i323, 3, i323 < 45, corrected=.false.) -&
                 reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                          224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                          1694._dp/9., 36._dp&
                           ], [size(i323,1), size(i323,2)] ))&
                 < dptol ))
  end

  subroutine test_cdp_1dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(cd1, corrected=.false.) -&
                 (var(real(cd1), corrected=.false.) +&
                  var(aimag(cd1), corrected=.false.))) < dptol)
    call check(error, abs(var(cd1, dim=1, corrected=.false.) -&
                 (var(real(cd1), dim=1, corrected=.false.) +&
                  var(aimag(cd1), dim=1, corrected=.false.))) < dptol)
  end

  subroutine test_cdp_1dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(cd1, .false., corrected=.false.)))
    call check(error, ieee_is_nan(var(cd1, 1, .false., corrected=.false.)))
  end

  subroutine test_cdp_1dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(cd1, cd1%im == 0, corrected=.false.) &
                          - var(cd1%re, cd1%im == 0, corrected=.false.)) < dptol)
    call check(error, abs(var(cd1, 1, cd1%im == 0, corrected=.false.) &
                          - var(cd1%re, 1, cd1%im == 0, corrected=.false.)) < dptol)
    call check(error, ieee_is_nan((var(cd1, all([cd1%re, cd1%im] == 0), &
                                       corrected=.false.))))
    call check(error, abs(var(cd1, (cd1%re > 1.2 .and. cd1%im == 0), &
                              corrected=.false.)) < dptol)
  end

  subroutine test_cdp_2dim(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(cd, corrected=.false.) &
                          - (var(cd%re, corrected=.false.) &
                          + var(cd%im, corrected=.false.))) < dptol)
    call check(error, all(abs(var(cd, 1, corrected=.false.) &
                          - (var(cd%re, 1, corrected=.false.) &
                          + var(cd%im, 1, corrected=.false.))) < dptol))
    call check(error, all(abs(var(cd, 2, corrected=.false.) &
                          - (var(cd%re, 2, corrected=.false.) &
                          + var(cd%im, 2, corrected=.false.))) < dptol))
  end

  subroutine test_cdp_2dim_mask(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, ieee_is_nan(var(cd, .false., corrected=.false.)))
    call check(error, any(ieee_is_nan(var(cd, 1, .false., corrected=.false.))))
    call check(error, any(ieee_is_nan(var(cd, 2, .false., corrected=.false.))))
  end

  subroutine test_cdp_2dim_mask_array(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error
    call check(error, abs(var(cd, cd%im == 0, corrected=.false.) &
                          - var(cd%re, cd%im == 0, corrected=.false.)) < dptol)
    call check(error, all(abs(var(cd, 1, cd%im == 0, corrected=.false.) &
                          - var(cd%re, 1, cd%im == 0, corrected=.false.)) < dptol))
    call check(error, any(ieee_is_nan(var(cd, 2, cd%im == 0, corrected=.false.))))
  end

end module test_varn


program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type
  use test_varn, only: collect_varn, initialize_test_data
  implicit none
  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0

  testsuites = [ &
    new_testsuite('varn', collect_varn) &
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
