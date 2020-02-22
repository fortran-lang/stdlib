program test_varn
    use stdlib_experimental_error, only: assert
    use stdlib_experimental_kinds, only: sp, dp, int32, int64
    use stdlib_experimental_stats, only: var
    implicit none


    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    integer(int32) :: i321(5) = [1, 2, 3, 4, 5]
    integer(int64) :: i641(5) = [1, 2, 3, 4, 5]

    integer(int32), allocatable :: i32(:,:), i323(:, :, :)
    integer(int64), allocatable :: i64(:,:), i643(:, :, :)

    real(sp) :: s1(5) = [1.0_sp, 2.0_sp, 3.0_sp, 4.0_sp, 5.0_sp]
    real(dp) :: d1(5) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]

    real(sp), allocatable :: s(:, :), s3(:, :, :)
    real(dp), allocatable :: d3(:, :, :)
    real(dp) :: d(4, 3) = reshape([1._dp, 3._dp, 5._dp, 7._dp,&
                                   2._dp, 4._dp, 6._dp, 8._dp,&
                                   9._dp, 10._dp, 11._dp, 12._dp], [4, 3])


    complex(sp) :: cs1(5) = [ cmplx(0.57706_sp, 0.00000_sp),&
                            cmplx(0.00000_sp, 1.44065_sp),&
                            cmplx(1.26401_sp, 0.00000_sp),&
                            cmplx(0.00000_sp, 0.88833_sp),&
                            cmplx(1.14352_sp, 0.00000_sp)]
    complex(dp) :: cd1(5) = [ cmplx(0.57706_dp, 0.00000_dp),&
                            cmplx(0.00000_dp, 1.44065_dp),&
                            cmplx(1.26401_dp, 0.00000_dp),&
                            cmplx(0.00000_dp, 0.88833_dp),&
                            cmplx(1.14352_dp, 0.00000_dp)]
    complex(sp) :: cs(5,3)
    complex(dp) :: cd(5,3)


    !sp
    !1dim
    print*,' test_sp_1dim'
    call assert( abs(var(s1, corrected=.false.) - 2.5*(4./5.)) < sptol)
    call assert( abs(var(s1, dim=1, corrected=.false.) - 2.5*(4./5.)) < sptol)

    print*,' test_sp_1dim_mask'
    call assert( isnan(var(s1, .false., corrected=.false.)))
    call assert( isnan(var(s1, 1, .false., corrected=.false.)))

    print*,' test_sp_1dim_mask_array'
    call assert( abs(var(s1, s1 < 5, corrected=.false.) - 5./4.) < sptol)
    call assert( isnan((var(s1, s1 < 0., corrected=.false.))))
    call assert( abs(var(s1, s1 == 1., corrected=.false.)) < sptol)
    call assert( abs(var(s1, 1, s1 < 5, corrected=.false.) - 5./4.) < sptol)

    !2dim
    print*,' test_sp_2dim'
    s = d
    call assert( abs(var(s, corrected=.false.) - 13.*11./12.) < sptol)
    call assert( all( abs( var(s, 1, corrected=.false.) - [20., 20., 5.]/4.) < sptol))
    call assert( all( abs( var(s, 2, corrected=.false.) -&
                      [19.0, 43. / 3., 31. / 3. , 7.0]*2./3.) < sptol))

    print*,' test_sp_2dim_mask'
    call assert( isnan(var(s, .false., corrected=.false.)))
    call assert( any(isnan(var(s, 1, .false., corrected=.false.))))
    call assert( any(isnan(var(s, 2, .false., corrected=.false.))))

    print*,' test_sp_2dim_mask_array'
    call assert( abs(var(s, s < 11, corrected=.false.) - 2.75*3.) < sptol)
    call assert( all( abs( var(s, 1, s < 11, corrected=.false.) -&
                      [5., 5., 0.25]) < sptol))
    call assert( all( abs( var(s, 2, s < 11, corrected=.false.) -&
                      [19.0*2./3., 43./9.*2., 0.25 , 0.25]) < sptol))


    !3dim
    allocate(s3(size(s,1),size(s,2),3))
    s3(:,:,1)=s;
    s3(:,:,2)=s*2;
    s3(:,:,3)=s*4;

    print*,' test_sp_3dim'
    call assert( abs(var(s3, corrected=.false.) - 153.4*35./36.) < sptol)
    call assert( all( abs( var(s3, 1, corrected=.false.) -&
                 reshape([20. / 3., 20. / 3., 5. / 3.,&
                          4* 20. / 3., 4* 20. / 3., 4* 5. / 3.,&
                          16* 20. / 3., 16* 20. / 3., 16* 5. / 3.],&
                          [size(s3,2), size(s3,3)])*3./4.)&
                 < sptol))
    call assert( all( abs( var(s3, 2, corrected=.false.) -&
                 reshape([19.0, 43. / 3., 31. / 3. , 7.0,&
                          4* 19.0, 4* 43. / 3., 4* 31. / 3. , 4* 7.0,&
                          16* 19.0, 16* 43. / 3., 16* 31. / 3. , 16* 7.0],&
                          [size(s3,1), size(s3,3)] )*2./3.)&
                 < sptol))
    call assert( all(abs( var(s3, 3, corrected=.false.) -&
                 reshape([ 7./3., 21., 175./3.,&
                           343./3., 28./3., 112./3.,&
                           84., 448./3., 189.,&
                           700./3., 847./3., 336.], [size(s3,1), size(s3,2)] )*2./3.)&
                 < sptol))

    print*,' test_sp_3dim_mask'
    call assert( isnan(var(s3, .false., corrected=.false.)))
    call assert( any(isnan(var(s3, 1, .false., corrected=.false.))))
    call assert( any(isnan(var(s3, 2, .false., corrected=.false.))))
    call assert( any(isnan(var(s3, 3, .false., corrected=.false.))))

    print*,' test_sp_3dim_mask_array'
    call assert( abs(var(s3, s3 < 11, corrected=.false.) - 7.73702383_sp) < sptol)
    call assert( all( abs( var(s3, 1, s3 < 45, corrected=.false.) -&
                  reshape([5., 5., 1.25,  20., 20., 5., 80., 80., 32./3.],&
                  [size(s3, 2), size(s3, 3)])) < sptol ))
    call assert( all( abs( var(s3, 2, s3 < 45, corrected=.false.) -&
                  reshape([ 38./3., 86./9., 6.88888931, 14./3., 152./3.,&
                            38.2222214, 27.5555573, 18.6666660, 202.666672,&
                            152.888885, 110.222229, 4.&
                           ],&
                  [size(s3, 1), size(s3, 3)])) < sptol ))
    call assert( all( abs( var(s3, 3, s3 < 45, corrected=.false.) -&
                 reshape([1.555555, 14., 38.888888, 76.222222, 6.2222222,&
                          24.888888, 56., 99.5555, 126., 155.555555, 188.22222, 36.&
                           ], [size(s3,1), size(s3,2)] ))&
                 < sptol ))


end program
