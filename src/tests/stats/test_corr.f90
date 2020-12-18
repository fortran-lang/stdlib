program test_corr
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp, int32, int64
    use stdlib_stats, only: corr
    use,intrinsic :: ieee_arithmetic, only : ieee_is_nan
    implicit none


    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    real(dp) :: d1(5) = [1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]
    real(dp) :: d(4, 3) = reshape([1._dp, 3._dp, 5._dp, 22._dp,&
                                   3._dp, 4._dp, 6._dp, 20._dp,&
                                   15._dp, 14._dp, 13._dp, 12._dp], [4, 3])

    complex(dp) :: cd1(5) = [ cmplx(0.57706_dp, 0.00000_dp, kind = dp),&
                            cmplx(0.00000_dp, 1.44065_dp, kind = dp),&
                            cmplx(1.26401_dp, 0.00000_dp, kind = dp),&
                            cmplx(0.00000_dp, 0.88833_dp, kind = dp),&
                            cmplx(1.14352_dp, 0.00000_dp, kind = dp)]
    complex(dp) :: ds(2,3) = reshape([ cmplx(1._dp, 0._dp, kind = dp),&
                            cmplx(0._dp, 2._dp, kind = dp),&
                            cmplx(3._dp, 0._dp, kind = dp),&
                            cmplx(0._dp, 4._dp, kind = dp),&
                            cmplx(5._dp, 0._dp, kind = dp),&
                            cmplx(0._dp, 6._dp, kind = dp)], [2, 3])


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

        call check( abs(corr(x, 1) - 1._sp) < sptol&
            , 'sp check 1')
        call check( ieee_is_nan(corr(x, 1, .false.))&
            , 'sp check 2')
        call check( ieee_is_nan(corr(x, 1, x == 1.)), 'sp check 3')
        call check( abs(corr(x, 1, x < 5) - 1._sp) < sptol, 'sp check 4')
        call check( ieee_is_nan(corr(x, 1, x < -999)), 'sp check 5')

        call check( any(ieee_is_nan(corr(x2, 1, mask = .false.)))&
            , 'sp check 6')
        call check( any(ieee_is_nan(corr(x2, 2, mask = .false.)))&
            , 'sp check 7')

        call check( all( abs( corr(x2, 1) - reshape([&
            1._sp, 0.9994439103600_sp, -0.870544389237152_sp, 0.99944391036_sp,&
            1._sp, -0.86261576629742_sp, -0.87054438923715_sp,  -0.862615766297428_sp,&
            1._sp ]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'sp check 8')
        call check( all( abs( corr(x2, 2) - reshape([&
             1._sp, 0.998742137866914_sp,  0.999846989517886_sp, -0.998337488459582_sp,&
             0.998742137866914_sp, 1._sp, 0.999466429486246_sp, -0.99419162560192020_sp,&
             0.999846989517886_sp, 0.999466429486246_sp, 1._sp, -0.99717646495273815_sp,&
             -0.998337488459582_sp, -0.994191625601920_sp, -0.997176464952738_sp, 1._sp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'sp check 9')

        call check( any(ieee_is_nan(corr(x2, 1, mask = x2 < 10)))&
            , 'sp check 10')
        call check( all( abs( corr(x2, 1, mask = x2 < 22) - reshape([&
              1._sp, 0.981980506061965_sp, -1._sp&
              ,0.981980506061965_sp, 1._sp, -0.862615766297428_sp&
              ,-1._sp, -0.862615766297428_sp, 1._sp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'sp check 11')
        call check( all( abs( corr(x2, 2, mask = x2 < 22) - reshape([&
            1._sp, 0.998742137866914_sp, 0.999846989517886_sp, -1._sp&
            ,0.998742137866914_sp, 1._sp, 0.999466429486246_sp, -1._sp&
            ,0.999846989517886_sp, 0.999466429486246_sp, 1._sp, -1._sp&
            ,-1._sp, -1._sp, -1._sp, 1._sp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'sp check 12')


        call check( all(abs(corr(x2, 1, mask = x2 < 1000) - corr(x2, 1))&
            < sptol)&
            , 'sp check 13')

        call check( all(abs(corr(x2, 2, mask = x2 < 1000) - corr(x2, 2))&
            < sptol)&
            , 'sp check 14')

    end subroutine test_sp

    subroutine test_dp(x, x2)
        real(dp), intent(in) :: x(:)
        real(dp), intent(in) :: x2(:, :)

        call check( abs(corr(x, 1) - 1._dp) < dptol&
            , 'dp check 1')
        call check( ieee_is_nan(corr(x, 1, .false.))&
            , 'dp check 2')
        call check( ieee_is_nan(corr(x, 1, x == 1.)), 'dp check 3')
        call check( abs(corr(x, 1, x < 5) - 1._dp) < dptol, 'dp check 4')
        call check( ieee_is_nan(corr(x, 1, x < -999)), 'dp check 5')

        call check( any(ieee_is_nan(corr(x2, 1, mask = .false.)))&
            , 'dp check 6')
        call check( any(ieee_is_nan(corr(x2, 2, mask = .false.)))&
            , 'dp check 7')

        call check( all( abs( corr(x2, 1) - reshape([&
            1._dp, 0.9994439103600_dp, -0.870544389237152_dp, 0.99944391036_dp,&
            1._dp, -0.86261576629742_dp, -0.87054438923715_dp,  -0.862615766297428_dp,&
            1._dp ]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'dp check 8')
        call check( all( abs( corr(x2, 2) - reshape([&
             1._dp, 0.998742137866914_dp,  0.999846989517886_dp, -0.998337488459582_dp,&
             0.998742137866914_dp, 1._dp, 0.999466429486246_dp, -0.99419162560192020_dp,&
             0.999846989517886_dp, 0.999466429486246_dp, 1._dp, -0.99717646495273815_dp,&
             -0.998337488459582_dp, -0.994191625601920_dp, -0.997176464952738_dp, 1._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'dp check 9')

        call check( any(ieee_is_nan(corr(x2, 1, mask = x2 < 10)))&
            , 'dp check 10')
        call check( all( abs( corr(x2, 1, mask = x2 < 22) - reshape([&
              1._dp, 0.981980506061965_dp, -1._dp&
              ,0.981980506061965_dp, 1._dp, -0.862615766297428_dp&
              ,-1._dp, -0.862615766297428_dp, 1._dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'dp check 11')
        call check( all( abs( corr(x2, 2, mask = x2 < 22) - reshape([&
            1._dp, 0.998742137866914_dp, 0.999846989517886_dp, -1._dp&
            ,0.998742137866914_dp, 1._dp, 0.999466429486246_dp, -1._dp&
            ,0.999846989517886_dp, 0.999466429486246_dp, 1._dp, -1._dp&
            ,-1._dp, -1._dp, -1._dp, 1._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'dp check 12')

        call check( all(abs(corr(x2, 1, mask = x2 < 1000) - corr(x2, 1))&
            < dptol)&
            , 'dp check 13')

        call check( all(abs(corr(x2, 2, mask = x2 < 1000) - corr(x2, 2))&
            < dptol)&
            , 'dp check 14')

    end subroutine test_dp

    subroutine test_int32(x, x2)
        integer(int32), intent(in) :: x(:)
        integer(int32), intent(in) :: x2(:, :)

        call check( abs(corr(x, 1) - 1._dp) < dptol&
            , 'int32 check 1')
        call check( ieee_is_nan(corr(x, 1, .false.))&
            , 'int32 check 2')
        call check( ieee_is_nan(corr(x, 1, x == 1.)), 'int32 check 3')
        call check( abs(corr(x, 1, x < 5) - 1._dp) < dptol, 'int32 check 4')
        call check( ieee_is_nan(corr(x, 1, x < -999)), 'int32 check 5')

        call check( any(ieee_is_nan(corr(x2, 1, mask = .false.)))&
            , 'int32 check 6')
        call check( any(ieee_is_nan(corr(x2, 2, mask = .false.)))&
            , 'int32 check 7')

        call check( all( abs( corr(x2, 1) - reshape([&
            1._dp, 0.9994439103600_dp, -0.870544389237152_dp, 0.99944391036_dp,&
            1._dp, -0.86261576629742_dp, -0.87054438923715_dp,  -0.862615766297428_dp,&
            1._dp ]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int32 check 8')
        call check( all( abs( corr(x2, 2) - reshape([&
             1._dp, 0.998742137866914_dp,  0.999846989517886_dp, -0.998337488459582_dp,&
             0.998742137866914_dp, 1._dp, 0.999466429486246_dp, -0.99419162560192020_dp,&
             0.999846989517886_dp, 0.999466429486246_dp, 1._dp, -0.99717646495273815_dp,&
             -0.998337488459582_dp, -0.994191625601920_dp, -0.997176464952738_dp, 1._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int32 check 9')

        call check( any(ieee_is_nan(corr(x2, 1, mask = x2 < 10)))&
            , 'int32 check 10')
        call check( all( abs( corr(x2, 1, mask = x2 < 22) - reshape([&
              1._dp, 0.981980506061965_dp, -1._dp&
              ,0.981980506061965_dp, 1._dp, -0.862615766297428_dp&
              ,-1._dp, -0.862615766297428_dp, 1._dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int32 check 11')
        call check( all( abs( corr(x2, 2, mask = x2 < 22) - reshape([&
            1._dp, 0.998742137866914_dp, 0.999846989517886_dp, -1._dp&
            ,0.998742137866914_dp, 1._dp, 0.999466429486246_dp, -1._dp&
            ,0.999846989517886_dp, 0.999466429486246_dp, 1._dp, -1._dp&
            ,-1._dp, -1._dp, -1._dp, 1._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int32 check 12')

        call check( all(abs(corr(x2, 1, mask = x2 < 1000) - corr(x2, 1))&
            < dptol)&
            , 'int32 check 13')

        call check( all(abs(corr(x2, 2, mask = x2 < 1000) - corr(x2, 2))&
            < dptol)&
            , 'int32 check 14')

    end subroutine test_int32

    subroutine test_int64(x, x2)
        integer(int64), intent(in) :: x(:)
        integer(int64), intent(in) :: x2(:, :)

        call check( abs(corr(x, 1) - 1._dp) < dptol&
            , 'int64 check 1')
        call check( ieee_is_nan(corr(x, 1, .false.))&
            , 'int64 check 2')
        call check( ieee_is_nan(corr(x, 1, x == 1)), 'int64 check 3')
        call check( abs(corr(x, 1, x < 5) - 1._dp) < dptol, 'int64 check 4')
        call check( ieee_is_nan(corr(x, 1, x < -999)), 'int64 check 5')

        call check( any(ieee_is_nan(corr(x2, 1, mask = .false.)))&
            , 'int64 check 6')
        call check( any(ieee_is_nan(corr(x2, 2, mask = .false.)))&
            , 'int64 check 7')

        call check( all( abs( corr(x2, 1) - reshape([&
            1._dp, 0.9994439103600_dp, -0.870544389237152_dp, 0.99944391036_dp,&
            1._dp, -0.86261576629742_dp, -0.87054438923715_dp,  -0.862615766297428_dp,&
            1._dp ]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int64 check 8')
        call check( all( abs( corr(x2, 2) - reshape([&
             1._dp, 0.998742137866914_dp,  0.999846989517886_dp, -0.998337488459582_dp,&
             0.998742137866914_dp, 1._dp, 0.999466429486246_dp, -0.99419162560192020_dp,&
             0.999846989517886_dp, 0.999466429486246_dp, 1._dp, -0.99717646495273815_dp,&
             -0.998337488459582_dp, -0.994191625601920_dp, -0.997176464952738_dp, 1._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int64 check 9')

        call check( any(ieee_is_nan(corr(x2, 1, mask = x2 < 10)))&
            , 'int64 check 10')
        call check( all( abs( corr(x2, 1, mask = x2 < 22) - reshape([&
              1._dp, 0.981980506061965_dp, -1._dp&
              ,0.981980506061965_dp, 1._dp, -0.862615766297428_dp&
              ,-1._dp, -0.862615766297428_dp, 1._dp]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'int64 check 11')
        call check( all( abs( corr(x2, 2, mask = x2 < 22) - reshape([&
            1._dp, 0.998742137866914_dp, 0.999846989517886_dp, -1._dp&
            ,0.998742137866914_dp, 1._dp, 0.999466429486246_dp, -1._dp&
            ,0.999846989517886_dp, 0.999466429486246_dp, 1._dp, -1._dp&
            ,-1._dp, -1._dp, -1._dp, 1._dp]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'int64 check 12')

        call check( all(abs(corr(x2, 1, mask = x2 < 1000) - corr(x2, 1))&
            < dptol)&
            , 'int64 check 13')

        call check( all(abs(corr(x2, 2, mask = x2 < 1000) - corr(x2, 2))&
            < dptol)&
            , 'int64 check 14')

    end subroutine test_int64

    subroutine test_csp(x, x2)
        complex(sp), intent(in) :: x(:)
        complex(sp), intent(in) :: x2(:, :)

        call check( abs(corr(x, dim=1) - 1._sp)  < sptol&
            , 'csp check 1')
        call check( abs(corr(x, 1, aimag(x) == 0) - 1._sp ) < sptol&
            , 'csp check 2')

        call check( ieee_is_nan(corr(x, 1, aimag(x) == -99 )) &
            , 'csp check 3')

        call check( ieee_is_nan(real(corr(x, 1, .false.)))&
            , 'csp check 4')

        call check( all( abs( corr(x2, 1) - reshape([&
               (1._sp,0._sp), (0.983869910099907_sp,-0.178885438199983_sp),&
               (0.973417168333576_sp,-0.229039333725547_sp),&
               (0.983869910099907_sp,0.178885438199983_sp), (1._sp,0._sp),&
               (0.998687663476588_sp,-0.051214751973158_sp),&
               (0.973417168333575_sp,0.229039333725547_sp),&
               (0.998687663476588_sp,0.0512147519731583_sp), (1._sp,0._sp) ]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < sptol)&
            , 'csp check 6')
        call check( all( abs( corr(x2, 2) - reshape([&
            (1._sp,0._sp), (0._sp,1._sp),&
            (0._sp,-1._sp), (1._sp,0._sp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'csp check 7')

        call check( all( abs( corr(x2, 2, mask = aimag(x2) < 6) - reshape([&
             (1._sp,0._sp), (0._sp,1._sp)&
             ,(0._sp,-1._sp), (1._sp,0._sp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < sptol)&
            , 'csp check 8')

        call check( all(abs(corr(x2, 1, mask = aimag(x2) < 1000) - corr(x2, 1))&
            < sptol)&
            , 'csp check 9')

        call check( all(abs(corr(x2, 2, mask = aimag(x2) < 1000) - corr(x2, 2))&
            < sptol)&
            , 'csp check 10')

    end subroutine test_csp

    subroutine test_cdp(x, x2)
        complex(dp), intent(in) :: x(:)
        complex(dp), intent(in) :: x2(:, :)

        call check( abs(corr(x, dim=1) - 1._dp)  < dptol&
            , 'cdp check 1')
        call check( abs(corr(x, 1, aimag(x) == 0) - 1._dp ) < dptol&
            , 'cdp check 2')

        call check( ieee_is_nan(corr(x, 1, aimag(x) == -99 )) &
            , 'cdp check 3')

        call check( ieee_is_nan(real(corr(x, 1, .false.)))&
            , 'cdp check 4')

        call check( all( abs( corr(x2, 1) - reshape([&
               (1._dp,0._dp), (0.983869910099907_dp,-0.178885438199983_dp),&
               (0.973417168333576_dp,-0.229039333725547_dp),&
               (0.983869910099907_dp,0.178885438199983_dp), (1._dp,0._dp),&
               (0.998687663476588_dp,-0.051214751973158_dp),&
               (0.973417168333575_dp,0.229039333725547_dp),&
               (0.998687663476588_dp,0.0512147519731583_dp), (1._dp,0._dp) ]&
            ,[ size(x2, 2), size(x2, 2)])&
            ) < dptol)&
            , 'cdp check 6')
        call check( all( abs( corr(x2, 2) - reshape([&
            (1._dp,0._dp), (0._dp,1._dp),&
            (0._dp,-1._dp), (1._dp,0._dp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'cdp check 7')

        call check( all( abs( corr(x2, 2, mask = aimag(x2) < 6) - reshape([&
             (1._dp,0._dp), (0._dp,1._dp)&
             ,(0._dp,-1._dp), (1._dp,0._dp)]&
            ,[ size(x2, 1), size(x2, 1)])&
            ) < dptol)&
            , 'cdp check 8')

        call check( all(abs(corr(x2, 1, mask = aimag(x2) < 1000) - corr(x2, 1))&
            < sptol)&
            , 'csp check 9')

        call check( all(abs(corr(x2, 2, mask = aimag(x2) < 1000) - corr(x2, 2))&
            < sptol)&
            , 'csp check 10')

    end subroutine test_cdp

end program test_corr
