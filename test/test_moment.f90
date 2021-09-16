program test_moment
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp, int32, int64
    use stdlib_stats, only: moment
    use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none


    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    real(dp) :: d1(5) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
    real(dp) :: d(4, 3) = reshape([1._dp, 3._dp, 5._dp, 7._dp,&
                                   2._dp, 4._dp, 6._dp, 8._dp,&
                                   9._dp, 10._dp, 11._dp, 12._dp], [4, 3])


    complex(sp) :: cs1(5) = [ cmplx(0.57706_sp, 0.00000_sp),&
                            cmplx(0.00000_sp, 1.44065_sp),&
                            cmplx(1.26401_sp, 0.00000_sp),&
                            cmplx(0.00000_sp, 0.88833_sp),&
                            cmplx(1.14352_sp, 0.00000_sp)]
    complex(sp) :: cs(5,3)


    call test_sp(real(d1,sp), real(d,sp))
    call test_dp(d1, d)
    call test_int32(int(d1, int32), int(d, int32))
    call test_int64(int(d1, int64), int(d, int64))

    cs(:,1) = cs1
    cs(:,2) = cs1*3_sp
    cs(:,3) = cs1*1.5_sp
    call test_csp(cs1, cs)


contains
    subroutine test_sp(x1, x2)
        real(sp), intent(in) :: x1(:), x2(:, :)

        integer :: order
        real(sp), allocatable :: x3(:, :, :)

        order = 1

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order)) < sptol)
        call check( abs(moment(x1, order, dim=1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5))) < sptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5))) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order)) < sptol)
        call check( all( abs( moment(x2, order, 1)) < sptol))
        call check( all( abs( moment(x2, order, 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))) < sptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11))) < sptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11))) < sptol))

        !3dim
        allocate(x3(size(x2,1),size(x2,2),3))
        x3(:,:,1)=x2;
        x3(:,:,2)=x2*2;
        x3(:,:,3)=x2*4;
    
        print*,' test_sp_3dim', order
        call check( abs(moment(x3, order)) < sptol)
        call check( all( abs( moment(x3, order, 1)) < sptol))
        call check( all( abs( moment(x3, order, 2)) < sptol))
        call check( all( abs( moment(x3, order, 3)) < sptol))
    
        print*,' test_sp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_sp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) ) < sptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45))) < sptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45))) < sptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45))) < sptol ))
 

        order = 2

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order) - 2._sp) < sptol)
        call check( abs(moment(x1, order, dim=1) - 2._sp) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5)) - 1.25_sp) < sptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5)) - 1.25_sp) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order) - 107.25_sp/9.) < sptol)
        call check( all( abs( moment(x2, order, 1) - [5._sp, 5._sp, 1.25_sp]) < sptol))
        call check( all( abs( moment(x2, order, 2) -&
                           [19.0, 43. / 3., 31. / 3. , 7.0]*2./3.) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))- 2.75_sp*3.) < sptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11)) -&
                      [5._sp, 5._sp, 0.25_sp]) < sptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11)) -&
                      [19._sp*2./3., 43._sp/9.*2., 0.25_sp , 0.25_sp]) < sptol))

        !3dim
        print*,' test_sp_3dim', order
        call check( abs(moment(x3, order) - 153.4_sp*35./36.) < sptol)
        call check( all( abs( moment(x3, order, 1) -&
                 reshape([20._sp / 3., 20._sp / 3., 5._sp / 3.,&
                          4* 20._sp / 3., 4* 20._sp / 3., 4* 5._sp / 3.,&
                          16* 20._sp / 3., 16* 20._sp / 3., 16* 5._sp / 3.],&
                          [size(x3,2), size(x3,3)])*3._sp/4.)&
                 < sptol))
        call check( all( abs( moment(x3, order, 2) -&
                 reshape([19._sp, 43._sp / 3., 31._sp / 3. , 7.0_sp,&
                          4* 19.0_sp, 4* 43._sp / 3., 4* 31._sp / 3. , 4* 7.0_sp,&
                          16* 19.0_sp, 16* 43._sp / 3., 16* 31._sp / 3. , 16* 7.0_sp],&
                          [size(x3,1), size(x3,3)] )*2._sp/3.)&
                 < sptol))
        call check( all( abs( moment(x3, order, 3) -&
                 reshape([ 7._sp/3., 21._sp, 175._sp/3.,&
                           343._sp/3., 28._sp/3., 112._sp/3.,&
                           84._sp, 448._sp/3., 189._sp,&
                           700._sp/3., 847._sp/3., 336._sp],&
                           [size(x3,1), size(x3,2)] )*2./3.)&
                 < sptol))
    
        print*,' test_sp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_sp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) -&
                      7.7370242214532876_dp ) < sptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45)) -&
                      reshape([5._sp, 5._sp, 1.25_sp,  20._sp, 20._sp, 5._sp,&
                               80._sp, 80._sp, 32._sp/3.],&
                               [size(x3, 2), size(x3, 3)])) < sptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45)) -&
                      reshape([ 38._sp/3., 86._sp/9., 62._sp/9., 14._sp/3., 152._sp/3.,&
                                344._sp/9., 248._sp/9., 168._sp/9., 1824._sp/9.,&
                                1376._sp/9., 992._sp/9., 4._sp&
                               ],&
                      [size(x3, 1), size(x3, 3)])) < sptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45)) -&
                     reshape([14._sp/9., 14._sp, 350._sp/9., 686._sp/9., 56._sp/9.,&
                              224._sp/9., 56._sp, 896._sp/9., 126._sp, 1400._sp/9.,&
                              1694._sp/9., 36._sp&
                               ], [size(x3,1), size(x3,2)] ))&
                     < sptol ))

    end subroutine

    subroutine test_dp(x1, x2)
        real(dp), intent(in) :: x1(:), x2(:, :)

        integer :: order
        real(dp), allocatable :: x3(:, :, :)

        order = 1

        !1dim
        print*,' test_dp_1dim', order
        call check( abs(moment(x1, order)) < dptol)
        call check( abs(moment(x1, order, dim=1)) < dptol)

        print*,' test_dp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_dp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5))) < dptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5))) < dptol)

        !2dim
        print*,' test_dp_2dim', order
        call check( abs(moment(x2, order)) < dptol)
        call check( all( abs( moment(x2, order, 1)) < dptol))
        call check( all( abs( moment(x2, order, 2)) < dptol))

        print*,' test_dp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_dp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))) < dptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11))) < dptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11))) < dptol))

        !3dim
        allocate(x3(size(x2,1),size(x2,2),3))
        x3(:,:,1)=x2;
        x3(:,:,2)=x2*2;
        x3(:,:,3)=x2*4;
    
        print*,' test_dp_3dim', order
        call check( abs(moment(x3, order)) < dptol)
        call check( all( abs( moment(x3, order, 1)) < dptol))
        call check( all( abs( moment(x3, order, 2)) < dptol))
        call check( all( abs( moment(x3, order, 3)) < dptol))
    
        print*,' test_dp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_dp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) ) < dptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45))) < dptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45))) < dptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45))) < dptol ))
 

        order = 2

        !1dim
        print*,' test_dp_1dim', order
        call check( abs(moment(x1, order) - 2._dp) < dptol)
        call check( abs(moment(x1, order, dim=1) - 2._dp) < dptol)

        print*,' test_dp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_dp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5)) - 1.25_dp) < dptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5)) - 1.25_dp) < dptol)

        !2dim
        print*,' test_dp_2dim', order
        call check( abs(moment(x2, order) - 107.25_dp/9.) < dptol)
        call check( all( abs( moment(x2, order, 1) - [5._dp, 5._dp, 1.25_dp]) < dptol))
        call check( all( abs( moment(x2, order, 2) -&
                      [19._dp, 43._dp / 3., 31._dp / 3. , 7._dp]*2._dp/3.) < dptol))

        print*,' test_dp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_dp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))- 2.75_dp*3.) < dptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11)) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11)) -&
                      [19._dp*2./3., 43._dp/9.*2., 0.25_dp , 0.25_dp]) < dptol))

        !3dim
        print*,' test_dp_3dim', order
        call check( abs(moment(x3, order) - 153.4_dp*35./36.) < dptol)
        call check( all( abs( moment(x3, order, 1) -&
                 reshape([20._dp / 3., 20._dp / 3., 5._dp / 3.,&
                          4* 20._dp / 3., 4* 20._dp / 3., 4* 5._dp / 3.,&
                          16* 20._dp / 3., 16* 20._dp / 3., 16* 5._dp / 3.],&
                          [size(x3,2), size(x3,3)])*3._dp/4.)&
                 < dptol))
        call check( all( abs( moment(x3, order, 2) -&
                 reshape([19._dp, 43._dp / 3., 31._dp / 3. , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3., 4* 31._dp / 3. , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3., 16* 31._dp / 3. , 16* 7.0_dp],&
                          [size(x3,1), size(x3,3)] )*2._dp/3.)&
                 < dptol))
        call check( all( abs( moment(x3, order, 3) -&
                 reshape([ 7._dp/3., 21._dp, 175._dp/3.,&
                           343._dp/3., 28._dp/3., 112._dp/3.,&
                           84._dp, 448._dp/3., 189._dp,&
                           700._dp/3., 847._dp/3., 336._dp],&
                           [size(x3,1), size(x3,2)] )*2./3.)&
                 < dptol))
    
        print*,' test_dp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_dp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) -&
                          7.7370242214532876_dp ) < dptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45)) -&
                      reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                               80._dp, 80._dp, 32._dp/3.],&
                               [size(x3, 2), size(x3, 3)])) < dptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45)) -&
                      reshape([ 38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                                344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                                1376._dp/9., 992._dp/9., 4._dp&
                               ],&
                      [size(x3, 1), size(x3, 3)])) < dptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45)) -&
                     reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                              224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                              1694._dp/9., 36._dp&
                               ], [size(x3,1), size(x3,2)] ))&
                     < dptol ))

    end subroutine

    subroutine test_int32(x1, x2)
        integer(int32), intent(in) :: x1(:), x2(:, :)

        integer :: order
        integer(int32), allocatable :: x3(:, :, :)

        order = 1

        !1dim
        print*,' test_dp_1dim', order
        call check( abs(moment(x1, order)) < dptol)
        call check( abs(moment(x1, order, dim=1)) < dptol)

        print*,' test_dp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_dp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5))) < dptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5))) < dptol)

        !2dim
        print*,' test_dp_2dim', order
        call check( abs(moment(x2, order)) < dptol)
        call check( all( abs( moment(x2, order, 1)) < dptol))
        call check( all( abs( moment(x2, order, 2)) < dptol))

        print*,' test_dp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_dp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))) < dptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11))) < dptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11))) < dptol))

        !3dim
        allocate(x3(size(x2,1),size(x2,2),3))
        x3(:,:,1)=x2;
        x3(:,:,2)=x2*2;
        x3(:,:,3)=x2*4;
    
        print*,' test_dp_3dim', order
        call check( abs(moment(x3, order)) < dptol)
        call check( all( abs( moment(x3, order, 1)) < dptol))
        call check( all( abs( moment(x3, order, 2)) < dptol))
        call check( all( abs( moment(x3, order, 3)) < dptol))
    
        print*,' test_dp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_dp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) ) < dptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45))) < dptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45))) < dptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45))) < dptol ))
 

        order = 2

        !1dim
        print*,' test_dp_1dim', order
        call check( abs(moment(x1, order) - 2._dp) < dptol)
        call check( abs(moment(x1, order, dim=1) - 2._dp) < dptol)

        print*,' test_dp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_dp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5)) - 1.25_dp) < dptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5)) - 1.25_dp) < dptol)

        !2dim
        print*,' test_dp_2dim', order
        call check( abs(moment(x2, order) - 107.25_dp/9.) < dptol)
        call check( all( abs( moment(x2, order, 1) - [5._dp, 5._dp, 1.25_dp]) < dptol))
        call check( all( abs( moment(x2, order, 2) -&
                      [19._dp, 43._dp / 3., 31._dp / 3. , 7._dp]*2._dp/3.) < dptol))

        print*,' test_dp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_dp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))- 2.75_dp*3.) < dptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11)) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11)) -&
                      [19._dp*2./3., 43._dp/9.*2., 0.25_dp , 0.25_dp]) < dptol))

        !3dim
        print*,' test_dp_3dim', order
        call check( abs(moment(x3, order) - 153.4_dp*35./36.) < dptol)
        call check( all( abs( moment(x3, order, 1) -&
                 reshape([20._dp / 3., 20._dp / 3., 5._dp / 3.,&
                          4* 20._dp / 3., 4* 20._dp / 3., 4* 5._dp / 3.,&
                          16* 20._dp / 3., 16* 20._dp / 3., 16* 5._dp / 3.],&
                          [size(x3,2), size(x3,3)])*3._dp/4.)&
                 < dptol))
        call check( all( abs( moment(x3, order, 2) -&
                 reshape([19._dp, 43._dp / 3., 31._dp / 3. , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3., 4* 31._dp / 3. , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3., 16* 31._dp / 3. , 16* 7.0_dp],&
                          [size(x3,1), size(x3,3)] )*2._dp/3.)&
                 < dptol))
        call check( all( abs( moment(x3, order, 3) -&
                 reshape([ 7._dp/3., 21._dp, 175._dp/3.,&
                           343._dp/3., 28._dp/3., 112._dp/3.,&
                           84._dp, 448._dp/3., 189._dp,&
                           700._dp/3., 847._dp/3., 336._dp],&
                           [size(x3,1), size(x3,2)] )*2./3.)&
                 < dptol))
    
        print*,' test_dp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_dp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) -&
                          7.7370242214532876_dp ) < dptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45)) -&
                      reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                               80._dp, 80._dp, 32._dp/3.],&
                               [size(x3, 2), size(x3, 3)])) < dptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45)) -&
                      reshape([ 38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                                344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                                1376._dp/9., 992._dp/9., 4._dp&
                               ],&
                      [size(x3, 1), size(x3, 3)])) < dptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45)) -&
                     reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                              224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                              1694._dp/9., 36._dp&
                               ], [size(x3,1), size(x3,2)] ))&
                     < dptol ))

    end subroutine

    subroutine test_int64(x1, x2)
        integer(int64), intent(in) :: x1(:), x2(:, :)

        integer :: order
        integer(int64), allocatable :: x3(:, :, :)

        order = 1

        !1dim
        print*,' test_dp_1dim', order
        call check( abs(moment(x1, order)) < dptol)
        call check( abs(moment(x1, order, dim=1)) < dptol)

        print*,' test_dp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_dp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5))) < dptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5))) < dptol)

        !2dim
        print*,' test_dp_2dim', order
        call check( abs(moment(x2, order)) < dptol)
        call check( all( abs( moment(x2, order, 1)) < dptol))
        call check( all( abs( moment(x2, order, 2)) < dptol))

        print*,' test_dp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_dp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))) < dptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11))) < dptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11))) < dptol))

        !3dim
        allocate(x3(size(x2,1),size(x2,2),3))
        x3(:,:,1)=x2;
        x3(:,:,2)=x2*2;
        x3(:,:,3)=x2*4;
    
        print*,' test_dp_3dim', order
        call check( abs(moment(x3, order)) < dptol)
        call check( all( abs( moment(x3, order, 1)) < dptol))
        call check( all( abs( moment(x3, order, 2)) < dptol))
        call check( all( abs( moment(x3, order, 3)) < dptol))
    
        print*,' test_dp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_dp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) ) < dptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45))) < dptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45))) < dptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45))) < dptol ))
 

        order = 2

        !1dim
        print*,' test_dp_1dim', order
        call check( abs(moment(x1, order) - 2._dp) < dptol)
        call check( abs(moment(x1, order, dim=1) - 2._dp) < dptol)

        print*,' test_dp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, 1, mask = .false.)))

        print*,' test_dp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (x1 < 5)) - 1.25_dp) < dptol)
        call check( abs(moment(x1, order, 1, mask = (x1 < 5)) - 1.25_dp) < dptol)

        !2dim
        print*,' test_dp_2dim', order
        call check( abs(moment(x2, order) - 107.25_dp/9.) < dptol)
        call check( all( abs( moment(x2, order, 1) - [5._dp, 5._dp, 1.25_dp]) < dptol))
        call check( all( abs( moment(x2, order, 2) -&
                      [19._dp, 43._dp / 3., 31._dp / 3. , 7._dp]*2._dp/3.) < dptol))

        print*,' test_dp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, 2, mask = .false.))))

        print*,' test_dp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (x2 < 11))- 2.75_dp*3.) < dptol)
        call check( all( abs( moment(x2, order, 1, mask = (x2 < 11)) -&
                      [5._dp, 5._dp, 0.25_dp]) < dptol))
        call check( all( abs( moment(x2, order, 2, mask = (x2 < 11)) -&
                      [19._dp*2./3., 43._dp/9.*2., 0.25_dp , 0.25_dp]) < dptol))

        !3dim
        print*,' test_dp_3dim', order
        call check( abs(moment(x3, order) - 153.4_dp*35./36.) < dptol)
        call check( all( abs( moment(x3, order, 1) -&
                 reshape([20._dp / 3., 20._dp / 3., 5._dp / 3.,&
                          4* 20._dp / 3., 4* 20._dp / 3., 4* 5._dp / 3.,&
                          16* 20._dp / 3., 16* 20._dp / 3., 16* 5._dp / 3.],&
                          [size(x3,2), size(x3,3)])*3._dp/4.)&
                 < dptol))
        call check( all( abs( moment(x3, order, 2) -&
                 reshape([19._dp, 43._dp / 3., 31._dp / 3. , 7.0_dp,&
                          4* 19.0_dp, 4* 43._dp / 3., 4* 31._dp / 3. , 4* 7.0_dp,&
                          16* 19.0_dp, 16* 43._dp / 3., 16* 31._dp / 3. , 16* 7.0_dp],&
                          [size(x3,1), size(x3,3)] )*2._dp/3.)&
                 < dptol))
        call check( all( abs( moment(x3, order, 3) -&
                 reshape([ 7._dp/3., 21._dp, 175._dp/3.,&
                           343._dp/3., 28._dp/3., 112._dp/3.,&
                           84._dp, 448._dp/3., 189._dp,&
                           700._dp/3., 847._dp/3., 336._dp],&
                           [size(x3,1), size(x3,2)] )*2./3.)&
                 < dptol))
    
        print*,' test_dp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, 1, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 2, mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, 3, mask = .false.))))
    
        print*,' test_dp_3dim_mask_array', order
        call check( abs(moment(x3, order, mask = (x3 < 11)) -&
                          7.7370242214532876_dp ) < dptol)
        call check( all( abs( moment(x3, order, 1, mask = (x3 < 45)) -&
                      reshape([5._dp, 5._dp, 1.25_dp,  20._dp, 20._dp, 5._dp,&
                               80._dp, 80._dp, 32._dp/3.],&
                               [size(x3, 2), size(x3, 3)])) < dptol ))
        call check( all( abs( moment(x3, order, 2, mask = (x3 < 45)) -&
                      reshape([ 38._dp/3., 86._dp/9., 62._dp/9., 14._dp/3., 152._dp/3.,&
                                344._dp/9., 248._dp/9., 168._dp/9., 1824._dp/9.,&
                                1376._dp/9., 992._dp/9., 4._dp&
                               ],&
                      [size(x3, 1), size(x3, 3)])) < dptol ))
        call check( all( abs( moment(x3, order, 3, mask = (x3 < 45)) -&
                     reshape([14._dp/9., 14._dp, 350._dp/9., 686._dp/9., 56._dp/9.,&
                              224._dp/9., 56._dp, 896._dp/9., 126._dp, 1400._dp/9.,&
                              1694._dp/9., 36._dp&
                               ], [size(x3,1), size(x3,2)] ))&
                     < dptol ))

    end subroutine

    subroutine test_csp(x1, x2)
        complex(sp), intent(in) :: x1(:), x2(:, :)

        integer :: order

        order = 1

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order)) < sptol)
        call check( abs(moment(x1, order, dim=1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(abs(moment(x1, order, mask = .false.))))
        call check( ieee_is_nan(abs(moment(x1, order, 1, mask = .false.))))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (aimag(x1) == 0))) < sptol)
        call check( abs(moment(x1, order, 1, mask = (aimag(x1) == 0))) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order)) < sptol)
        call check( all( abs( moment(x2, order, 1)) < sptol))
        call check( all( abs( moment(x2, order, 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(abs(moment(x2, order, mask = .false.))))
        call check( any(ieee_is_nan(abs(moment(x2, order, 1, mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, 2, mask = .false.)))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (aimag(x2) == 0))) < sptol)
        call check( all( abs( moment(x2, order, 1, mask = (aimag(x2) == 0))) < sptol))
        call check( any(ieee_is_nan( abs( moment(x2, order, 2,&
                          mask = (aimag(x2) == 0))))))

        order = 2

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order) - (-6.459422410E-02,-0.556084037)) < sptol)
        call check( abs(moment(x1, order, dim=1) -&
                        (-6.459422410E-02,-0.556084037)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(abs(moment(x1, order, mask = .false.))))
        call check( ieee_is_nan(abs(moment(x1, order, 1, mask = .false.))))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, mask = (aimag(x1) == 0)) -&
                          (8.969944715E-02,0.00000000)) < sptol)
        call check( abs(moment(x1, order, 1, mask = (aimag(x1) == 0)) -&
                          (8.969944715E-02,0.00000000)) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order) - (-0.163121477,-1.86906016)) < sptol)
        call check( all( abs( moment(x2, order, 1) -&
                     [(-6.459422410E-02,-0.556084037),&
                      (-0.581347823,-5.00475645),&
                      (-0.145336956,-1.25118911)]&
                     ) < sptol))
        call check( all( abs( moment(x2, order, 2) -&
                     [(0.240498722,0.00000000),&
                     (-1.49895227,0.00000000),&
                     (1.15390968,0.00000000),&
                     (-0.569927275,0.00000000),&
                     (0.944405317,0.00000000)]&
                     ) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(abs(moment(x2, order, mask = .false.))))
        call check( any(ieee_is_nan(abs(moment(x2, order, 1, mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, 2, mask = .false.)))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, mask = (aimag(x2) == 0))-&
                     (1.08109438,0.00000000)) < sptol)
        call check( all( abs( moment(x2, order, 1, mask = (aimag(x2)==0)) -&
                      [(8.969944715E-02,0.00000000),&
                       (0.807295084,0.00000000),&
                       (0.201823771,0.00000000)]&
                       ) < sptol))

    end subroutine
end program
