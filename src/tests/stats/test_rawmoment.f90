program test_rawmoment
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp, int32
    use stdlib_stats, only: mean, moment
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
    call test_int32(int(d1, int32), int(d, int32))

    cs(:,1) = cs1
    cs(:,2) = cs1*3_sp
    cs(:,3) = cs1*1.5_sp
    call test_csp(cs1, cs)


contains
    subroutine test_sp(x1, x2)
        real(sp), intent(in) :: x1(:), x2(:, :)

        integer :: order
        real(sp), allocatable :: x3(:, :, :)
        real(sp), allocatable :: zero2_1(:), zero2_2(:)
        real(sp), allocatable :: zero3_1(:,:), zero3_2(:,:), zero3_3(:,:)

        allocate(zero2_1(size(x2, 2)), zero2_2(size(x2, 1)))
        zero2_1 = 0
        zero2_2 = 0

        order = 1

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order, center = 0.) - mean(x1)) < sptol)
        call check( abs(moment(x1, order, 1, center = 0.) - mean(x1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, center = 0., mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, dim = 1, center = 0., mask = .false.)))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, center = 0., mask = (x1 < 5)) -&
                          mean(x1, mask = (x1 < 5)) ) < sptol)
        call check( abs(moment(x1, order, dim = 1, center = 0., mask = (x1 < 5)) -&
                          mean(x1, dim = 1, mask = (x1 < 5))) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order, center = 0.) - mean(x2)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0.) -&
                        mean(x2, dim = 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0.) -&
                        mean(x2, dim = 2)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1) -&
                        mean(x2, dim = 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2) -&
                        mean(x2, dim = 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, center = 0., mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, dim = 1, center = zero2_1,&
                        mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, dim = 2, center = zero2_2,&
                        mask = .false.))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, center = 0., mask = (x2 < 11)) -&
                                mean(x2, x2 < 11)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0.,&
                                mask = (x2 < 11)) -&
                                mean(x2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0.,&
                                mask = (x2 < 11)) -&
                                mean(x2, 2, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1,&
                                mask = (x2 < 11)) -&
                                mean(x2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2,&
                                mask = (x2 < 11)) -&
                                mean(x2, 2, x2 < 11)) < sptol))

        !3dim
        allocate(x3(size(x2,1),size(x2,2),3))
        x3(:,:,1)=x2;
        x3(:,:,2)=x2*2;
        x3(:,:,3)=x2*4;

        allocate(zero3_1(size(x3, 2), size(x3, 3))&
                 ,zero3_2(size(x3, 1), size(x3, 3))&
                 ,zero3_3(size(x3, 1), size(x3, 2)))
        zero3_1 = 0
        zero3_2 = 0
        zero3_3 = 0

        print*,' test_sp_3dim', order
        call check( abs(moment(x3, order, center = 0.) - mean(x3)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0._sp) -&
                               mean(x3, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = 0._sp) -&
                               mean(x3, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = 0._sp) -&
                               mean(x3, 3)) < sptol))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1) -&
                               mean(x3, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2) -&
                               mean(x3, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3) -&
                               mean(x3, 3)) < sptol))

        print*,' test_sp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, center = 0., mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, dim = 1, center = zero3_1,&
                         mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 2, center = zero3_2,&
                         mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 3, center = zero3_3,&
                         mask = .false.))))

        print*,' test_sp_3dim_mask_array', order
        call check( abs(moment(x3, order, center = 0., mask = (x3 < 11)) -&
                         mean(x3, x3 < 11)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0.,&
                                mask = (x3 < 45)) -&
                                mean(x3, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = 0.,&
                                mask = (x3 < 45)) -&
                                mean(x3, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = 0.,&
                                mask = (x3 < 45)) -&
                                mean(x3, 3, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1,&
                                mask = (x3 < 45)) -&
                                mean(x3, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2,&
                                mask = (x3 < 45)) -&
                                mean(x3, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3,&
                                mask = (x3 < 45)) -&
                                mean(x3, 3, x3 < 45)) < sptol ))


        order = 2

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order, center = 0.) - mean(x1**2)) < sptol)
        call check( abs(moment(x1, order, 1, center = 0.) - mean(x1**2, 1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, center = 0., mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, dim = 1, center = 0., mask = .false.)))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, center = 0., mask = (x1 < 5)) -&
                          mean(x1**2, x1 < 5)) < sptol)
        call check( abs(moment(x1, order, dim = 1, center = 0., mask = (x1 < 5)) -&
                          mean(x1**2, 1, x1 < 5)) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order, center = 0.) - mean(x2**2)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0.) -&
                               mean(x2**2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0.) - &
                               mean(x2**2, 2)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1) -&
                               mean(x2**2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2) - &
                               mean(x2**2, 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, center = 0., mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, dim = 1, center = zero2_1, &
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, dim = 2, center = zero2_2, &
                               mask = .false.))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, center = 0., mask = (x2 < 11)) -&
                          mean(x2**2, x2 < 11)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0.,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0.,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 2, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 2, x2 < 11)) < sptol))

        !3dim
        print*,' test_sp_3dim', order
        call check( abs(moment(x3, order, center = 0.) - mean(x3**2)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0.) -&
                                mean(x3**2, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = 0.) -&
                                mean(x3**2, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = 0.) -&
                                mean(x3**2, 3)) < sptol))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1) -&
                                mean(x3**2, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2) -&
                                mean(x3**2, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3) -&
                                mean(x3**2, 3)) < sptol))

        print*,' test_sp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, center = 0., mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, dim = 1, center = zero3_1,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 2, center = zero3_2,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 3, center = zero3_3,&
                               mask = .false.))))

        print*,' test_sp_3dim_mask_array', order
        call check( abs(moment(x3, order, center = 0., mask = (x3 < 11)) -&
                          mean(x3**2, x3 < 11)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0.,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = 0.,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = 0.,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 3, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 3, x3 < 45)) < sptol ))

    end subroutine

    subroutine test_int32(x1, x2)
        integer(int32), intent(in) :: x1(:), x2(:, :)

        integer :: order
        integer(int32), allocatable :: x3(:, :, :)
        real(dp), allocatable :: zero2_1(:), zero2_2(:)
        real(dp), allocatable :: zero3_1(:,:), zero3_2(:,:), zero3_3(:,:)

        allocate(zero2_1(size(x2, 2)), zero2_2(size(x2, 1)))
        zero2_1 = 0
        zero2_2 = 0

        order = 1

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order, center = 0._dp) - mean(x1)) < sptol)
        call check( abs(moment(x1, order, 1, center = 0._dp) - mean(x1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, center = 0._dp, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, dim = 1, center = 0._dp,&
                               mask = .false.)))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, center = 0._dp, mask = (x1 < 5)) -&
                          mean(x1, mask = (x1 < 5)) ) < sptol)
        call check( abs(moment(x1, order, dim = 1, center = 0._dp, mask = (x1 < 5)) -&
                          mean(x1, dim = 1, mask = (x1 < 5))) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order, center = 0._dp) - mean(x2)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0._dp) -&
                               mean(x2, dim = 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0._dp) -&
                               mean(x2, dim = 2)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1) -&
                               mean(x2, dim = 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2) -&
                               mean(x2, dim = 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, center = 0._dp, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, dim = 1, center = zero2_1,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, dim = 2, center = zero2_2,&
                               mask = .false.))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, center = 0._dp, mask = (x2 < 11)) -&
                               mean(x2, x2 < 11)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0._dp,&
                               mask = (x2 < 11)) -&
                               mean(x2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0._dp,&
                               mask = (x2 < 11)) -&
                               mean(x2, 2, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1,&
                               mask = (x2 < 11)) -&
                               mean(x2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2,&
                               mask = (x2 < 11)) -&
                               mean(x2, 2, x2 < 11)) < sptol))

        !3dim
        allocate(x3(size(x2,1),size(x2,2),3))
        x3(:,:,1)=x2;
        x3(:,:,2)=x2*2;
        x3(:,:,3)=x2*4;

        allocate(zero3_1(size(x3, 2), size(x3, 3))&
                 ,zero3_2(size(x3, 1), size(x3, 3))&
                 ,zero3_3(size(x3, 1), size(x3, 2)))
        zero3_1 = 0
        zero3_2 = 0
        zero3_3 = 0

        print*,' test_sp_3dim', order
        call check( abs(moment(x3, order, center = 0._dp) - mean(x3)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0._dp) -&
                               mean(x3, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = 0._dp) -&
                               mean(x3, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = 0._dp) -&
                               mean(x3, 3)) < sptol))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1) -&
                               mean(x3, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2) -&
                               mean(x3, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3) -&
                               mean(x3, 3)) < sptol))

        print*,' test_sp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, center = 0._dp, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, dim = 1, center = zero3_1,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 2, center = zero3_2,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 3, center = zero3_3,&
                               mask = .false.))))

        print*,' test_sp_3dim_mask_array', order
        call check( abs(moment(x3, order, center = 0._dp, mask = (x3 < 11)) -&
                               mean(x3, x3 < 11)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0._dp,&
                               mask = (x3 < 45)) -&
                               mean(x3, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = 0._dp,&
                               mask = (x3 < 45)) -&
                               mean(x3, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = 0._dp,&
                               mask = (x3 < 45)) -&
                               mean(x3, 3, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1,&
                               mask = (x3 < 45)) -&
                               mean(x3, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2,&
                               mask = (x3 < 45)) -&
                               mean(x3, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3,&
                               mask = (x3 < 45)) -&
                               mean(x3, 3, x3 < 45)) < sptol ))


        order = 2

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order, center = 0._dp) - mean(x1**2)) < sptol)
        call check( abs(moment(x1, order, 1, center = 0._dp) - mean(x1**2, 1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(moment(x1, order, center = 0._dp, mask = .false.)))
        call check( ieee_is_nan(moment(x1, order, dim = 1, center = 0._dp,&
                               mask = .false.)))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, center = 0._dp, mask = (x1 < 5)) -&
                          mean(x1**2, x1 < 5)) < sptol)
        call check( abs(moment(x1, order, dim = 1, center = 0._dp, mask = (x1 < 5)) -&
                          mean(x1**2, 1, x1 < 5)) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order, center = 0._dp) - mean(x2**2)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0._dp) -&
                               mean(x2**2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0._dp) -&
                               mean(x2**2, 2)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1) -&
                               mean(x2**2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2) -&
                               mean(x2**2, 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(moment(x2, order, center = 0._dp, mask = .false.)))
        call check( any(ieee_is_nan(moment(x2, order, dim = 1, center = 0._dp,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, dim = 2, center = 0._dp,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, dim = 1, center = zero2_1,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x2, order, dim = 2, center = zero2_2,&
                               mask = .false.))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, center = 0._dp, mask = (x2 < 11)) -&
                          mean(x2**2, x2 < 11)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = 0._dp,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = 0._dp,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 2, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 1, x2 < 11)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2,&
                               mask = (x2 < 11)) -&
                               mean(x2**2, 2, x2 < 11)) < sptol))

        !3dim
        print*,' test_sp_3dim', order
        call check( abs(moment(x3, order, center = 0._dp) - mean(x3**2)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0._dp) -&
                                mean(x3**2, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = 0._dp) -&
                                mean(x3**2, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = 0._dp) -&
                                mean(x3**2, 3)) < sptol))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1) -&
                                mean(x3**2, 1)) < sptol))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2) -&
                                mean(x3**2, 2)) < sptol))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3) -&
                                mean(x3**2, 3)) < sptol))

        print*,' test_sp_3dim_mask', order
        call check( ieee_is_nan(moment(x3, order, center = 0._dp, mask = .false.)))
        call check( any(ieee_is_nan(moment(x3, order, dim = 1, center = 0._dp,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 2, center = 0._dp,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 3, center = 0._dp,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 1, center = zero3_1,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 2, center = zero3_2,&
                               mask = .false.))))
        call check( any(ieee_is_nan(moment(x3, order, dim = 3, center = zero3_3,&
                               mask = .false.))))

        print*,' test_sp_3dim_mask_array', order
        call check( abs(moment(x3, order, center = 0._dp, mask = (x3 < 11)) -&
                          mean(x3**2, x3 < 11)) < sptol)
        call check( all( abs( moment(x3, order, dim = 1, center = 0._dp,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = 0._dp,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = 0._dp,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 3, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 1, center = zero3_1,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 1, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 2, center = zero3_2,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 2, x3 < 45)) < sptol ))
        call check( all( abs( moment(x3, order, dim = 3, center = zero3_3,&
                               mask = (x3 < 45)) -&
                               mean(x3**2, 3, x3 < 45)) < sptol ))

    end subroutine

    subroutine test_csp(x1, x2)
        complex(sp), intent(in) :: x1(:), x2(:, :)

        integer :: order
        complex(sp), allocatable :: zero2_1(:), zero2_2(:)

        allocate(zero2_1(size(x2, 2)), zero2_2(size(x2, 1)))
        zero2_1 = (0., 0.)
        zero2_2 = (0., 0.)

        order = 1

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order, center = (0., 0.)) - mean(x1)) < sptol)
        call check( abs(moment(x1, order, 1, center = (0., 0.)) - mean(x1, 1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(abs(moment(x1, order, center = (0., 0.),&
                               mask = .false.))))
        call check( ieee_is_nan(abs(moment(x1, order, dim = 1, center = (0., 0.),&
                               mask = .false.))))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, center = (0., 0.), mask = (aimag(x1) == 0)) -&
                          mean(x1, aimag(x1) == 0)) < sptol)
        call check( abs(moment(x1, order, dim = 1, center = (0., 0.),&
                          mask = (aimag(x1) == 0)) -&
                          mean(x1, 1, aimag(x1) == 0)) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order, center = (0., 0.)) - mean(x2)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = (0., 0.)) -&
                               mean(x2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = (0., 0.)) -&
                               mean(x2, 2)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1) -&
                               mean(x2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2) -&
                               mean(x2, 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(abs(moment(x2, order, center = (0., 0.),&
                               mask = .false.))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 1, center = (0., 0.),&
                               mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 2, center = (0., 0.),&
                               mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 1, center = zero2_1,&
                               mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 2, center = zero2_2,&
                               mask = .false.)))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, center = (0., 0.), mask = (aimag(x2) == 0)) -&
                          mean(x2, aimag(x2) == 0)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = (0., 0.),&
                               mask = (aimag(x2) == 0)) -&
                               mean(x2, 1, aimag(x2) == 0)) < sptol))
        call check( any(ieee_is_nan( abs( moment(x2, order,&
                         dim = 2, center = (0., 0.), mask = (aimag(x2) == 0)) -&
                         mean(x2, 2, aimag(x2) == 0)))))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1,&
                               mask = (aimag(x2) == 0)) -&
                               mean(x2, 1, aimag(x2) == 0)) < sptol))
        call check( any(ieee_is_nan( abs( moment(x2, order,&
                         dim = 2, center = zero2_2, mask = (aimag(x2) == 0)) -&
                         mean(x2, 2, aimag(x2) == 0)))))

        order = 2

        !1dim
        print*,' test_sp_1dim', order
        call check( abs(moment(x1, order, center = (0., 0.)) - mean(x1**2)) < sptol)
        call check( abs(moment(x1, order, 1, center = (0., 0.)) -&
                          mean(x1**2, 1)) < sptol)

        print*,' test_sp_1dim_mask', order
        call check( ieee_is_nan(abs(moment(x1, order, center = (0., 0.),&
                               mask = .false.))))
        call check( ieee_is_nan(abs(moment(x1, order, dim = 1, center = (0., 0.),&
                               mask = .false.))))

        print*,' test_sp_1dim_mask_array', order
        call check( abs(moment(x1, order, center = (0., 0.), mask = (aimag(x1) == 0)) -&
                          mean(x1**2, aimag(x1) == 0)) < sptol)
        call check( abs(moment(x1, order, dim = 1, center = (0., 0.),&
                          mask = (aimag(x1) == 0)) -&
                          mean(x1**2, 1, aimag(x1) == 0)) < sptol)

        !2dim
        print*,' test_sp_2dim', order
        call check( abs(moment(x2, order, center = (0., 0.)) - mean(x2**2)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = (0., 0.)) -&
                               mean(x2**2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = (0., 0.)) -&
                               mean(x2**2, 2)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1) -&
                               mean(x2**2, 1)) < sptol))
        call check( all( abs( moment(x2, order, dim = 2, center = zero2_2) -&
                               mean(x2**2, 2)) < sptol))

        print*,' test_sp_2dim_mask', order
        call check( ieee_is_nan(abs(moment(x2, order, center = (0., 0.),&
                               mask = .false.))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 1, center = (0., 0.),&
                               mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 2, center = (0., 0.),&
                               mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 1, center = zero2_1,&
                               mask = .false.)))))
        call check( any(ieee_is_nan(abs(moment(x2, order, dim = 2, center = zero2_2,&
                               mask = .false.)))))

        print*,' test_sp_2dim_mask_array', order
        call check( abs(moment(x2, order, center = (0., 0.), mask = (aimag(x2) == 0)) -&
                          mean(x2**2, aimag(x2) == 0)) < sptol)
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1,&
                          mask = (aimag(x2)==0)) -&
                          mean(x2**2, 1, aimag(x2)==0)) < sptol))
        call check( all( abs( moment(x2, order, dim = 1, center = zero2_1,&
                          mask = (aimag(x2)==0)) -&
                          mean(x2**2, 1, aimag(x2)==0)) < sptol))

    end subroutine
end program
