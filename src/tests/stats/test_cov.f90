program test_cov
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp, int32, int64
    use stdlib_stats, only: cov, var
    use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none


    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    real(dp) :: d1(5) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
    real(dp) :: d(4, 3) = reshape([1._dp, 3._dp, 5._dp, 7._dp,&
                                   2._dp, 4._dp, 6._dp, 8._dp,&
                                   9._dp, 10._dp, 11._dp, 12._dp], [4, 3])

    complex(dp) :: cd1(5) = [ cmplx(0.57706_dp, 0.00000_dp,kind=dp),&
                            cmplx(0.00000_dp, 1.44065_dp,kind=dp),&
                            cmplx(1.26401_dp, 0.00000_dp,kind=dp),&
                            cmplx(0.00000_dp, 0.88833_dp,kind=dp),&
                            cmplx(1.14352_dp, 0.00000_dp,kind=dp)]
    complex(dp) :: ds(2,3) = reshape([ cmplx(1._dp, 0._dp,kind=dp),&
                            cmplx(0._dp, 2._dp,kind=dp),&
                            cmplx(3._dp, 0._dp,kind=dp),&
                            cmplx(0._dp, 4._dp,kind=dp),&
                            cmplx(5._dp, 0._dp,kind=dp),&
                            cmplx(0._dp, 6._dp,kind=dp)], [2, 3])


    call test_sp(real(d1, sp), real(d, sp))

    call test_dp(d1,d)

    call test_int32(int(d1, int32) ,int(d, int32))

    call test_int64(int(d1, int64) ,int(d, int64))

    call test_csp(cmplx(cd1, kind = sp), cmplx(ds, kind = sp))

    call test_cdp(cd1, ds)

contains

    subroutine test_sp(x, x2)
        real(sp), intent(in) :: x(:)
        real(sp), intent(in) :: x2(:, :)

        call check( abs(cov(x, 1) - 2.5_sp) < sptol&
            , 'sp check 1')
        call check( ieee_is_nan(cov(x, 1, .false.))&
            , 'sp check 2')
        call check( ieee_is_nan((cov(x, 1, x == 1.)))&
            , 'sp check 3')
        call check( abs(cov(x, 1, x < 5) - 5._sp/3) < sptol&
            , 'sp check 4')
        call check( abs(cov(x, 1, x < 5, corrected = .false.) -&
            5._sp/4) < sptol&
            , 'sp check 5')

        call check( any(ieee_is_nan(cov(x2, 1, mask = .false.)))&
            , 'sp check 6')
        call check( any(ieee_is_nan(cov(x2, 2, mask = .false.)))&
            , 'sp check 7')

        call check( all( abs( cov(x2, 1) - reshape([&
            60._sp/9, 60._sp/9, 30._sp/9&
            ,60._sp/9, 60._sp/9, 30._sp/9&
            ,30._sp/9, 30._sp/9, 15._sp/9]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'sp check 8')
        call check( all( abs( cov(x2, 2) - reshape([&
            19._sp, 16.5_sp, 14._sp, 11.5_sp, 16.5_sp, 129._sp/9&
            ,109.5_sp/9, 10._sp, 14._sp, 109.5_sp/9, 93._sp/9&
            , 8.5_sp, 11.5_sp, 10._sp, 8.5_sp, 7._sp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'sp check 9')

        call check( all( abs( cov(x2, 1, corrected=.false.) - reshape([&
            60._sp/9, 60._sp/9, 30._sp/9&
            ,60._sp/9, 60._sp/9, 30._sp/9&
            ,30._sp/9, 30._sp/9, 15._sp/9]&
            *(size(x2, 1)-1._sp)/size(x2, 1)&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'sp check 10')
        call check( all( abs( cov(x2, 2, corrected=.false.) - reshape([&
            19._sp, 16.5_sp, 14._sp, 11.5_sp, 16.5_sp, 129._sp/9&
            ,109.5_sp/9, 10._sp, 14._sp, 109.5_sp/9, 93._sp/9&
            , 8.5_sp, 11.5_sp, 10._sp, 8.5_sp, 7._sp]&
            *(size(x2, 2)-1._sp)/size(x2, 2)&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'sp check 11')

        call check( any(ieee_is_nan(cov(x2, 1, mask = x2 < 10)))&
            , 'sp check 12')
        call check( all( abs( cov(x2, 1, mask = x2 < 11) - reshape([&
             60._sp/9, 60._sp/9, 1._sp, 60._sp/9, 60._sp/9, 1._sp&
             , 1._sp, 1._sp, 0.5_sp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'sp check 13')
        call check( all( abs( cov(x2, 2, mask = x2 < 11) - reshape([&
            19._sp, 16.5_sp, 0.5_sp, 0.5_sp, 16.5_sp&
            ,129._sp/9, 0.5_sp, 0.5_sp, 0.5_sp, 0.5_sp&
            ,0.5_sp, 0.5_sp, 0.5_sp, 0.5_sp, 0.5_sp, 0.5_sp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'sp check 14')

        call check( all( abs( cov(x2, 1, mask = x2 < 11, corrected = .false.) -&
            reshape([&
             5._sp, 5._sp, 0.5_sp, 5._sp, 5._sp, 0.5_sp, 0.5_sp,&
             0.5_sp, 0.25_sp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'sp check 15')
        call check( all( abs( cov(x2, 2, mask = x2 < 11, corrected = .false.) -&
            reshape([&
            114._sp/9, 11._sp, 0.25_sp, 0.25_sp, 11._sp, 86._sp/9,&
            0.25_sp, 0.25_sp, 0.25_sp, 0.25_sp, 0.25_sp, 0.25_sp,&
            0.25_sp, 0.25_sp, 0.25_sp, 0.25_sp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'sp check 16')

        call check( all( abs( cov(x2, 1, mask = x2 < 1000) - cov(x2, 1))&
            < sptol)&
            , 'sp check 17')

        call check( all( abs( cov(x2, 2, mask = x2 < 1000) - cov(x2, 2))&
            < sptol)&
            , 'sp check 18')

    end subroutine test_sp

    subroutine test_dp(x, x2)
        real(dp), intent(in) :: x(:)
        real(dp), intent(in) :: x2(:, :)

        call check( abs(cov(x, 1) - 2.5_dp) < dptol&
            , 'dp check 1')
        call check( ieee_is_nan(cov(x, 1, .false.))&
            , 'dp check 2')
        call check( ieee_is_nan((cov(x, 1, x == 1.)))&
            , 'dp check 3')
        call check( abs(cov(x, 1, x < 5) - 5._dp/3) < dptol&
            , 'dp check 4')
        call check( abs(cov(x, 1, x < 5, corrected = .false.) -&
            5._dp/4) < dptol&
            , 'dp check 5')

        call check( any(ieee_is_nan(cov(x2, 1, mask = .false.)))&
            , 'dp check 6')
        call check( any(ieee_is_nan(cov(x2, 2, mask = .false.)))&
            , 'dp check 7')

        call check( all( abs( cov(x2, 1) - reshape([&
            60._dp/9, 60._dp/9, 30._dp/9&
            ,60._dp/9, 60._dp/9, 30._dp/9&
            ,30._dp/9, 30._dp/9, 15._dp/9]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'dp check 8')
        call check( all( abs( cov(x2, 2) - reshape([&
            19._dp, 16.5_dp, 14._dp, 11.5_dp, 16.5_dp, 129._dp/9&
            ,109.5_dp/9, 10._dp, 14._dp, 109.5_dp/9, 93._dp/9&
            , 8.5_dp, 11.5_dp, 10._dp, 8.5_dp, 7._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'dp check 9')

        call check( all( abs( cov(x2, 1, corrected=.false.) - reshape([&
            60._dp/9, 60._dp/9, 30._dp/9&
            ,60._dp/9, 60._dp/9, 30._dp/9&
            ,30._dp/9, 30._dp/9, 15._dp/9]&
            *(size(x2, 1)-1._dp)/size(x2, 1)&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'dp check 10')
        call check( all( abs( cov(x2, 2, corrected=.false.) - reshape([&
            19._dp, 16.5_dp, 14._dp, 11.5_dp, 16.5_dp, 129._dp/9&
            ,109.5_dp/9, 10._dp, 14._dp, 109.5_dp/9, 93._dp/9&
            , 8.5_dp, 11.5_dp, 10._dp, 8.5_dp, 7._dp]&
            *(size(x2, 2)-1._dp)/size(x2, 2)&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'dp check 11')

        call check( any(ieee_is_nan(cov(x2, 1, mask = x2 < 10)))&
            , 'dp check 12')
        call check( all( abs( cov(x2, 1, mask = x2 < 11) - reshape([&
             60._dp/9, 60._dp/9, 1._dp, 60._dp/9, 60._dp/9, 1._dp&
             , 1._dp, 1._dp, 0.5_dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'dp check 13')
        call check( all( abs( cov(x2, 2, mask = x2 < 11) - reshape([&
            19._dp, 16.5_dp, 0.5_dp, 0.5_dp, 16.5_dp&
            ,129._dp/9, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp&
            ,0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'dp check 14')

        call check( all( abs( cov(x2, 1, mask = x2 < 11, corrected = .false.) -&
            reshape([&
             5._dp, 5._dp, 0.5_dp, 5._dp, 5._dp, 0.5_dp, 0.5_dp,&
             0.5_dp, 0.25_dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'dp check 15')
        call check( all( abs( cov(x2, 2, mask = x2 < 11, corrected = .false.) -&
            reshape([&
            114._dp/9, 11._dp, 0.25_dp, 0.25_dp, 11._dp, 86._dp/9,&
            0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp,&
            0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'dp check 16')

        call check( all( abs( cov(x2, 1, mask = x2 < 1000) - cov(x2, 1))&
            < dptol)&
            , 'dp check 17')

        call check( all( abs( cov(x2, 2, mask = x2 < 1000) - cov(x2, 2))&
            < dptol)&
            , 'dp check 18')

    end subroutine test_dp

    subroutine test_int32(x, x2)
        integer(int32), intent(in) :: x(:)
        integer(int32), intent(in) :: x2(:, :)

        call check( abs(cov(x, 1) - 2.5_dp) < dptol&
            , 'int32 check 1')
        call check( ieee_is_nan(cov(x, 1, .false.))&
            , 'int32 check 2')
        call check( ieee_is_nan((cov(x, 1, x == 1.)))&
            , 'int32 check 3')
        call check( abs(cov(x, 1, x < 5) - 5._dp/3) < dptol&
            , 'int32 check 4')
        call check( abs(cov(x, 1, x < 5, corrected = .false.) -&
            5._dp/4) < dptol&
            , 'int32 check 5')

        call check( any(ieee_is_nan(cov(x2, 1, mask = .false.)))&
            , 'int32 check 6')
        call check( any(ieee_is_nan(cov(x2, 2, mask = .false.)))&
            , 'int32 check 7')

        call check( all( abs( cov(x2, 1) - reshape([&
            60._dp/9, 60._dp/9, 30._dp/9&
            ,60._dp/9, 60._dp/9, 30._dp/9&
            ,30._dp/9, 30._dp/9, 15._dp/9]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int32 check 8')
        call check( all( abs( cov(x2, 2) - reshape([&
            19._dp, 16.5_dp, 14._dp, 11.5_dp, 16.5_dp, 129._dp/9&
            ,109.5_dp/9, 10._dp, 14._dp, 109.5_dp/9, 93._dp/9&
            , 8.5_dp, 11.5_dp, 10._dp, 8.5_dp, 7._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int32 check 9')

        call check( all( abs( cov(x2, 1, corrected=.false.) - reshape([&
            60._dp/9, 60._dp/9, 30._dp/9&
            ,60._dp/9, 60._dp/9, 30._dp/9&
            ,30._dp/9, 30._dp/9, 15._dp/9]&
            *(size(x2, 1)-1._dp)/size(x2, 1)&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int32 check 10')
        call check( all( abs( cov(x2, 2, corrected=.false.) - reshape([&
            19._dp, 16.5_dp, 14._dp, 11.5_dp, 16.5_dp, 129._dp/9&
            ,109.5_dp/9, 10._dp, 14._dp, 109.5_dp/9, 93._dp/9&
            , 8.5_dp, 11.5_dp, 10._dp, 8.5_dp, 7._dp]&
            *(size(x2, 2)-1._dp)/size(x2, 2)&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int32 check 11')

        call check( any(ieee_is_nan(cov(x2, 1, mask = x2 < 10)))&
            , 'int32 check 12')
        call check( all( abs( cov(x2, 1, mask = x2 < 11) - reshape([&
             60._dp/9, 60._dp/9, 1._dp, 60._dp/9, 60._dp/9, 1._dp&
             , 1._dp, 1._dp, 0.5_dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int32 check 13')
        call check( all( abs( cov(x2, 2, mask = x2 < 11) - reshape([&
            19._dp, 16.5_dp, 0.5_dp, 0.5_dp, 16.5_dp&
            ,129._dp/9, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp&
            ,0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int32 check 14')

        call check( all( abs( cov(x2, 1, mask = x2 < 11, corrected = .false.) -&
            reshape([&
             5._dp, 5._dp, 0.5_dp, 5._dp, 5._dp, 0.5_dp, 0.5_dp,&
             0.5_dp, 0.25_dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int32 check 15')
        call check( all( abs( cov(x2, 2, mask = x2 < 11, corrected = .false.) -&
            reshape([&
            114._dp/9, 11._dp, 0.25_dp, 0.25_dp, 11._dp, 86._dp/9,&
            0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp,&
            0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int32 check 16')

        call check( all( abs( cov(x2, 1, mask = x2 < 1000) - cov(x2, 1))&
            < dptol)&
            , 'int32 check 17')

        call check( all( abs( cov(x2, 2, mask = x2 < 1000) - cov(x2, 2))&
            < dptol)&
            , 'int32 check 18')

    end subroutine test_int32

    subroutine test_int64(x, x2)
        integer(int64), intent(in) :: x(:)
        integer(int64), intent(in) :: x2(:, :)

        call check( abs(cov(x, 1) - 2.5_dp) < dptol&
            , 'int64 check 1')
        call check( ieee_is_nan(cov(x, 1, .false.))&
            , 'int64 check 2')
        call check( ieee_is_nan((cov(x, 1, x == 1)))&
            , 'int64 check 3')
        call check( abs(cov(x, 1, x < 5) - 5._dp/3) < dptol&
            , 'int64 check 4')
        call check( abs(cov(x, 1, x < 5, corrected = .false.) -&
            5._dp/4) < dptol&
            , 'int64 check 5')

        call check( any(ieee_is_nan(cov(x2, 1, mask = .false.)))&
            , 'int64 check 6')
        call check( any(ieee_is_nan(cov(x2, 2, mask = .false.)))&
            , 'int64 check 7')

        call check( all( abs( cov(x2, 1) - reshape([&
            60._dp/9, 60._dp/9, 30._dp/9&
            ,60._dp/9, 60._dp/9, 30._dp/9&
            ,30._dp/9, 30._dp/9, 15._dp/9]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int64 check 8')
        call check( all( abs( cov(x2, 2) - reshape([&
            19._dp, 16.5_dp, 14._dp, 11.5_dp, 16.5_dp, 129._dp/9&
            ,109.5_dp/9, 10._dp, 14._dp, 109.5_dp/9, 93._dp/9&
            , 8.5_dp, 11.5_dp, 10._dp, 8.5_dp, 7._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int64 check 9')

        call check( all( abs( cov(x2, 1, corrected=.false.) - reshape([&
            60._dp/9, 60._dp/9, 30._dp/9&
            ,60._dp/9, 60._dp/9, 30._dp/9&
            ,30._dp/9, 30._dp/9, 15._dp/9]&
            *(size(x2, 1)-1._dp)/size(x2, 1)&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int64 check 10')
        call check( all( abs( cov(x2, 2, corrected=.false.) - reshape([&
            19._dp, 16.5_dp, 14._dp, 11.5_dp, 16.5_dp, 129._dp/9&
            ,109.5_dp/9, 10._dp, 14._dp, 109.5_dp/9, 93._dp/9&
            , 8.5_dp, 11.5_dp, 10._dp, 8.5_dp, 7._dp]&
            *(size(x2, 2)-1._dp)/size(x2, 2)&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int64 check 11')

        call check( any(ieee_is_nan(cov(x2, 1, mask = x2 < 10)))&
            , 'int64 check 12')
        call check( all( abs( cov(x2, 1, mask = x2 < 11) - reshape([&
             60._dp/9, 60._dp/9, 1._dp, 60._dp/9, 60._dp/9, 1._dp&
             , 1._dp, 1._dp, 0.5_dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int64 check 13')
        call check( all( abs( cov(x2, 2, mask = x2 < 11) - reshape([&
            19._dp, 16.5_dp, 0.5_dp, 0.5_dp, 16.5_dp&
            ,129._dp/9, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp&
            ,0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp, 0.5_dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int64 check 14')

        call check( all( abs( cov(x2, 1, mask = x2 < 11, corrected = .false.) -&
            reshape([&
             5._dp, 5._dp, 0.5_dp, 5._dp, 5._dp, 0.5_dp, 0.5_dp,&
             0.5_dp, 0.25_dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int64 check 15')
        call check( all( abs( cov(x2, 2, mask = x2 < 11, corrected = .false.) -&
            reshape([&
            114._dp/9, 11._dp, 0.25_dp, 0.25_dp, 11._dp, 86._dp/9,&
            0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp,&
            0.25_dp, 0.25_dp, 0.25_dp, 0.25_dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int64 check 16')

        call check( all( abs( cov(x2, 1, mask = x2 < 1000) - cov(x2, 1))&
            < dptol)&
            , 'int64 check 17')

        call check( all( abs( cov(x2, 2, mask = x2 < 1000) - cov(x2, 2))&
            < dptol)&
            , 'int64 check 18')

    end subroutine test_int64

    subroutine test_csp(x, x2)
        complex(sp), intent(in) :: x(:)
        complex(sp), intent(in) :: x2(:, :)

!        complex(sp), allocatable :: cd(:,:)

        call check( abs(cov(x, dim=1) -&
                     (var(real(x),1) + var(aimag(x), 1)) ) < sptol&
            , 'csp check 1')
        call check( abs(cov(x, 1, aimag(x) == 0) -&
                     var(real(x), 1, aimag(x) == 0)) < sptol&
            , 'csp check 2')

        call check( abs(cov(x, dim=1, corrected=.false.) -&
                     (var(real(x), dim=1, corrected=.false.) +&
                      var(aimag(x), dim=1, corrected=.false.))) <&
                      sptol&
            , 'csp check 3')

        call check( ieee_is_nan(real(cov(x, 1, .false., corrected=.false.)))&
            , 'csp check 4')

        call check( abs(cov(x, 1, aimag(x) == 0, corrected=.false.) -&
                      var(real(x), 1, aimag(x) == 0,&
                      corrected=.false.)) < sptol&
            , 'csp check 5')


        call check( all( abs( cov(x2, 1) - reshape([&
            (2.5_sp,0._sp), (5.5_sp,-1._sp), (8.5_sp,-2._sp)&
            , (5.5_sp,1._sp),  (12.5_sp,0._sp), (19.5_sp,-1._sp)&
            , (8.5_sp,2._sp),  (19.5_sp,1._sp),  (30.5_sp,0._sp)]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'csp check 6')
        call check( all( abs( cov(x2, 2) - reshape([&
            (4._sp,0._sp), (0._sp,4._sp),&
            (0._sp,-4._sp), (4._sp,0._sp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'csp check 7')

        call check( all( abs( cov(x2, 1, corrected=.false.) - reshape([&
            (2.5_sp,0._sp), (5.5_sp,-1._sp), (8.5_sp,-2._sp)&
            , (5.5_sp,1._sp),  (12.5_sp,0._sp), (19.5_sp,-1._sp)&
            , (8.5_sp,2._sp),  (19.5_sp,1._sp),  (30.5_sp,0._sp)]&
            *(size(x2, 1)-1._sp)/size(x2, 1)&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'csp check 8')
        call check( all( abs( cov(x2, 2, corrected=.false.) - reshape([&
            (4._sp,0._sp), (0._sp,4._sp),&
            (0._sp,-4._sp), (4._sp,0._sp)]&
            *(size(x2, 2)-1._sp)/size(x2, 2)&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'csp check 9')

! Issue with gfortran 7 and 8: do not extract cd(1:2, 1:2) correctly
!        allocate(cd, source = cov(x2, 1, mask = aimag(x2) < 6))
!        call check( all( abs( cd(1:2, 1:2) - reshape([&
!             (2.5_sp,0._sp), (5.5_sp,-1._sp)&
!             ,(5.5_sp,1._sp), (12.5_sp,0._sp)]&
!            ,[2, 2])&
!            ) < sptol)&
!            , 'csp check 10')
!        call check( ieee_is_nan(real(cd(3,3)))&
!            , 'csp check 10 bis')


        call check( all( abs( cov(x2, 1, mask = aimag(x2) < 8) - cov(x2, 1))&
            < sptol)&
            , 'csp check 11')

        call check( all( abs( cov(x2, 2, mask = aimag(x2) < 8) - cov(x2, 2))&
            < sptol)&
            , 'csp check 12')

        call check( all( abs( cov(x2, 2, mask = aimag(x2) < 6) - reshape([&
             (4._sp,0._sp), (0._sp,2._sp)&
             ,(0._sp,-2._sp), (2._sp,0._sp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'csp check 13')

        call check( all( abs( cov(x2, 2, mask = aimag(x2) < 6, corrected = .false.) -&
            reshape([&
             (2.6666666666666666_sp,0._sp), (0._sp,1._sp)&
             ,(0._sp,-1._sp), (1._sp,0._sp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sp)&
            , 'csp check 14')

    end subroutine test_csp

    subroutine test_cdp(x, x2)
        complex(dp), intent(in) :: x(:)
        complex(dp), intent(in) :: x2(:, :)

!        complex(dp), allocatable :: cd(:,:)

        call check( abs(cov(x, dim=1) -&
                     (var(real(x),1) + var(aimag(x), 1)) ) < dptol&
            , 'cdp check 1')
        call check( abs(cov(x, 1, aimag(x) == 0) -&
                     var(real(x), 1, aimag(x) == 0)) < dptol&
            , 'cdp check 2')

        call check( abs(cov(x, dim=1, corrected=.false.) -&
                     (var(real(x), dim=1, corrected=.false.) +&
                      var(aimag(x), dim=1, corrected=.false.))) <&
                      dptol&
            , 'cdp check 3')

        call check( ieee_is_nan(real(cov(x, 1, .false., corrected=.false.)))&
            , 'cdp check 4')

        call check( abs(cov(x, 1, aimag(x) == 0, corrected=.false.) -&
                      var(real(x), 1, aimag(x) == 0,&
                      corrected=.false.)) < dptol&
            , 'cdp check 5')


        call check( all( abs( cov(x2, 1) - reshape([&
            (2.5_dp,0._dp), (5.5_dp,-1._dp), (8.5_dp,-2._dp)&
            , (5.5_dp,1._dp),  (12.5_dp,0._dp), (19.5_dp,-1._dp)&
            , (8.5_dp,2._dp),  (19.5_dp,1._dp),  (30.5_dp,0._dp)]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'cdp check 6')
        call check( all( abs( cov(x2, 2) - reshape([&
            (4._dp,0._dp), (0._dp,4._dp),&
            (0._dp,-4._dp), (4._dp,0._dp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'cdp check 7')

        call check( all( abs( cov(x2, 1, corrected=.false.) - reshape([&
            (2.5_dp,0._dp), (5.5_dp,-1._dp), (8.5_dp,-2._dp)&
            , (5.5_dp,1._dp),  (12.5_dp,0._dp), (19.5_dp,-1._dp)&
            , (8.5_dp,2._dp),  (19.5_dp,1._dp),  (30.5_dp,0._dp)]&
            *(size(x2, 1)-1._dp)/size(x2, 1)&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'cdp check 8')
        call check( all( abs( cov(x2, 2, corrected=.false.) - reshape([&
            (4._dp,0._dp), (0._dp,4._dp),&
            (0._dp,-4._dp), (4._dp,0._dp)]&
            *(size(x2, 2)-1._dp)/size(x2, 2)&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'cdp check 9')

! Issue with gfortran 7 and 8: do not extract cd(1:2, 1:2) correctly
!        allocate(cd, source = cov(x2, 1, mask = aimag(x2) < 6))
!
!        call check( all( abs( cd(1:2, 1:2) - reshape([&
!             (2.5_dp,0._dp), (5.5_dp,-1._dp)&
!             ,(5.5_dp,1._dp), (12.5_dp,0._dp)]&
!            ,[2, 2])&
!            ) < dptol)&
!            , 'cdp check 10')
!        call check( ieee_is_nan(real(cd(3,3)))&
!            , 'cdp check 10 bis')

        call check( all( abs( cov(x2, 1, mask = aimag(x2) < 8) - cov(x2, 1))&
            < dptol)&
            , 'cdp check 11')

        call check( all( abs( cov(x2, 2, mask = aimag(x2) < 8) - cov(x2, 2))&
            < dptol)&
            , 'cdp check 12')

        call check( all( abs( cov(x2, 2, mask = aimag(x2) < 6) - reshape([&
             (4._dp,0._dp), (0._dp,2._dp)&
             ,(0._dp,-2._dp), (2._dp,0._dp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'cdp check 13')

        call check( all( abs( cov(x2, 2, mask = aimag(x2) < 6, corrected = .false.) -&
            reshape([&
             (2.6666666666666666_dp,0._dp), (0._dp,1._dp)&
             ,(0._dp,-1._dp), (1._dp,0._dp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'cdp check 14')

    end subroutine test_cdp

end program test_cov
