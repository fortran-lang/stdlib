Module stdlib_stats_distribution_gamma    
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
    use stdlib_kinds, only : sp, dp, xdp
    use stdlib_error, only : error_stop
    use stdlib_optval, only : optval
    use stdlib_stats_distribution_uniform, only : uni=>rvs_uniform
    use stdlib_stats_distribution_normal, only : rnor=>rvs_normal
    use stdlib_specialfunctions_gamma, only : regamma_p=>regularized_gamma_p

    implicit none
    intrinsic :: log_gamma
    private

    public :: rvs_gamma
    public :: pdf_gamma
    public :: cdf_gamma


    interface rvs_gamma
    !! Version experimental
    !!
    !! Gamma Distribution Random Variates
    !! ([Specification](../page/specs/stdlib_stats_distribution_gamma.html#
    !! rvs_gamma-gamma-distribution-random-variates))
    !!
        module procedure gamma_dist_rvs_1_rsp     ! 1 argument
        module procedure gamma_dist_rvs_1_rdp     ! 1 argument
        module procedure gamma_dist_rvs_1_csp     ! 1 argument
        module procedure gamma_dist_rvs_1_cdp     ! 1 argument

        module procedure gamma_dist_rvs_rsp       ! 2 arguments
        module procedure gamma_dist_rvs_rdp       ! 2 arguments
        module procedure gamma_dist_rvs_csp       ! 2 arguments
        module procedure gamma_dist_rvs_cdp       ! 2 arguments

        module procedure gamma_dist_rvs_array_rsp ! 3 arguments
        module procedure gamma_dist_rvs_array_rdp ! 3 arguments
        module procedure gamma_dist_rvs_array_csp ! 3 arguments
        module procedure gamma_dist_rvs_array_cdp ! 3 arguments
    end interface rvs_gamma


    interface pdf_gamma
    !! Version experimental
    !!
    !! Gamma Distribution Probability Density Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_gamma.html#
    !! pdf_gamma-gamma-distribution-probability-density-function))
    !!
        module procedure gamma_dist_pdf_rsp
        module procedure gamma_dist_pdf_rdp
        module procedure gamma_dist_pdf_csp
        module procedure gamma_dist_pdf_cdp

        module procedure gamma_dist_pdf_impure_rsp
        module procedure gamma_dist_pdf_impure_rdp
        module procedure gamma_dist_pdf_impure_csp
        module procedure gamma_dist_pdf_impure_cdp
    end interface pdf_gamma


    interface cdf_gamma
    !! Version experimental
    !!
    !! Gamma Distribution Cumulative Distribution Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_gamma.html#
    !! cdf_gamma-gamma-distribution-cumulative-distribution-function))
    !!
        module procedure gamma_dist_cdf_rsp
        module procedure gamma_dist_cdf_rdp
        module procedure gamma_dist_cdf_csp
        module procedure gamma_dist_cdf_cdp

        module procedure gamma_dist_cdf_impure_rsp
        module procedure gamma_dist_cdf_impure_rdp
        module procedure gamma_dist_cdf_impure_csp
        module procedure gamma_dist_cdf_impure_cdp
    end interface cdf_gamma




contains

    impure elemental function gamma_dist_rvs_1_rsp(shape) result(res)
    ! Gamma distribution random variate. "A Simple Method for Generating Gamma
    ! Variables", G. Marsaglia & W. W. Tsang, ACM Transactions on Mathematical
    ! Software, 26(3), 2000, p. 363
    !
        real(sp), intent(in) :: shape
        real(sp) :: res
        real(sp) :: x, v, u, zz, d, c
        real(sp), parameter :: sq = 0.0331_sp

        if(shape <= 0.0_sp) then
            res = ieee_value(1.0_sp, ieee_quiet_nan)
            return
        end if

        zz = shape

        if(zz < 1._sp) zz = 1._sp + zz
        !shift shape parameter > 1
        d = zz - 1._sp / 3._sp
        c = 1._sp / (3._sp * sqrt(d))

        do
            do
                x = rnor(0.0_sp, 1.0_sp)
                v = 1._sp + c * x
                v = v * v * v

                if(v > 0._sp) exit

            end do

            x = x * x
            u = uni(1.0_sp)

            if(u < (1._sp - sq * x * x)) exit

            if(log(u) < 0.5_sp * x + d * (1._sp - v + log(v))) exit

        end do

        res = d * v

        if(shape < 1._sp) then
        !restore shape parameter < 1
            u = uni(1.0_sp)
            res = res * u ** (1._sp / shape)

        endif
    end function gamma_dist_rvs_1_rsp

    impure elemental function gamma_dist_rvs_1_rdp(shape) result(res)
    ! Gamma distribution random variate. "A Simple Method for Generating Gamma
    ! Variables", G. Marsaglia & W. W. Tsang, ACM Transactions on Mathematical
    ! Software, 26(3), 2000, p. 363
    !
        real(dp), intent(in) :: shape
        real(dp) :: res
        real(dp) :: x, v, u, zz, d, c
        real(dp), parameter :: sq = 0.0331_dp

        if(shape <= 0.0_dp) then
            res = ieee_value(1.0_dp, ieee_quiet_nan)
            return
        end if

        zz = shape

        if(zz < 1._dp) zz = 1._dp + zz
        !shift shape parameter > 1
        d = zz - 1._dp / 3._dp
        c = 1._dp / (3._dp * sqrt(d))

        do
            do
                x = rnor(0.0_dp, 1.0_dp)
                v = 1._dp + c * x
                v = v * v * v

                if(v > 0._dp) exit

            end do

            x = x * x
            u = uni(1.0_dp)

            if(u < (1._dp - sq * x * x)) exit

            if(log(u) < 0.5_dp * x + d * (1._dp - v + log(v))) exit

        end do

        res = d * v

        if(shape < 1._dp) then
        !restore shape parameter < 1
            u = uni(1.0_dp)
            res = res * u ** (1._dp / shape)

        endif
    end function gamma_dist_rvs_1_rdp



    impure elemental function gamma_dist_rvs_1_csp(shape) result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(sp), intent(in) :: shape
        complex(sp) :: res

        res = cmplx(gamma_dist_rvs_1_rsp(shape%re),                        &
                    gamma_dist_rvs_1_rsp(shape%im), kind=sp)
    end function gamma_dist_rvs_1_csp

    impure elemental function gamma_dist_rvs_1_cdp(shape) result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(dp), intent(in) :: shape
        complex(dp) :: res

        res = cmplx(gamma_dist_rvs_1_rdp(shape%re),                        &
                    gamma_dist_rvs_1_rdp(shape%im), kind=dp)
    end function gamma_dist_rvs_1_cdp



    impure elemental function gamma_dist_rvs_rsp(shape, rate, loc)      &
        result(res)
    !
        real(sp), intent(in) :: shape, rate
        real(sp), intent(in), optional :: loc
        real(sp) :: res
        real(sp) :: loc_

        loc_ = optval(loc, 0.0_sp)

        if(rate <= 0.0_sp) call error_stop("Error(gamma_dist_rvs): Gamma" &
        //" distribution rate parameter must be greater than zero")

        res = gamma_dist_rvs_1_rsp(shape) / rate + loc_
    end function gamma_dist_rvs_rsp

    impure elemental function gamma_dist_rvs_rdp(shape, rate, loc)      &
        result(res)
    !
        real(dp), intent(in) :: shape, rate
        real(dp), intent(in), optional :: loc
        real(dp) :: res
        real(dp) :: loc_

        loc_ = optval(loc, 0.0_dp)

        if(rate <= 0.0_dp) call error_stop("Error(gamma_dist_rvs): Gamma" &
        //" distribution rate parameter must be greater than zero")

        res = gamma_dist_rvs_1_rdp(shape) / rate + loc_
    end function gamma_dist_rvs_rdp



    impure elemental function gamma_dist_rvs_csp(shape, rate, loc)      &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(sp), intent(in) :: shape, rate
        complex(sp), intent(in), optional :: loc
        complex(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = cmplx(gamma_dist_rvs_rsp(shape%re, rate%re) + loc_%re,                 &
                    gamma_dist_rvs_rsp(shape%im, rate%im) + loc_%im, kind=sp)
    end function gamma_dist_rvs_csp

    impure elemental function gamma_dist_rvs_cdp(shape, rate, loc)      &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(dp), intent(in) :: shape, rate
        complex(dp), intent(in), optional :: loc
        complex(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = cmplx(gamma_dist_rvs_rdp(shape%re, rate%re) + loc_%re,                 &
                    gamma_dist_rvs_rdp(shape%im, rate%im) + loc_%im, kind=dp)
    end function gamma_dist_rvs_cdp



    function gamma_dist_rvs_array_rsp(shape, rate, array_size, loc)     &
        result(res)
    !
        real(sp), intent(in) :: shape, rate
        integer, intent(in) :: array_size
        real(sp), intent(in), optional :: loc
        real(sp) :: res(array_size)
        integer :: i
        real(sp) :: loc_

        loc_ = optval(loc, 0.0_sp)

        do i = 1, array_size

            res(i) = gamma_dist_rvs_rsp(shape, rate, loc_)

        end do
    end function gamma_dist_rvs_array_rsp

    function gamma_dist_rvs_array_rdp(shape, rate, array_size, loc)     &
        result(res)
    !
        real(dp), intent(in) :: shape, rate
        integer, intent(in) :: array_size
        real(dp), intent(in), optional :: loc
        real(dp) :: res(array_size)
        integer :: i
        real(dp) :: loc_

        loc_ = optval(loc, 0.0_dp)

        do i = 1, array_size

            res(i) = gamma_dist_rvs_rdp(shape, rate, loc_)

        end do
    end function gamma_dist_rvs_array_rdp



    function gamma_dist_rvs_array_csp(shape, rate, array_size, loc)     &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(sp), intent(in) :: shape, rate
        integer, intent(in) :: array_size
        complex(sp), intent(in), optional :: loc
        complex(sp) :: res(array_size)
        integer :: i
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        do i = 1, array_size

            res(i) = gamma_dist_rvs_csp(shape, rate, loc_)

        end do
    end function gamma_dist_rvs_array_csp

    function gamma_dist_rvs_array_cdp(shape, rate, array_size, loc)     &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(dp), intent(in) :: shape, rate
        integer, intent(in) :: array_size
        complex(dp), intent(in), optional :: loc
        complex(dp) :: res(array_size)
        integer :: i
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        do i = 1, array_size

            res(i) = gamma_dist_rvs_cdp(shape, rate, loc_)

        end do
    end function gamma_dist_rvs_array_cdp



    elemental function gamma_dist_pdf_rsp(x, shape, rate, loc)     &
        result(res)
    ! Gamma distribution probability density function
    !
        real(sp), intent(in) :: x, shape, rate
        real(sp), intent(in), optional :: loc
        real(sp) :: res
        real(sp) :: xs
        real(sp) :: loc_

        loc_ = optval(loc, 0.0_sp)

        xs = x - loc_

        if(rate <= 0.0_sp .or. shape <= 0.0_sp) then
            res = ieee_value(1.0_sp, ieee_quiet_nan)
            return
        end if

        if(xs == 0.0_sp) then

            if(shape == 1.0_sp) then

                res = rate

            else if(shape > 1.0_sp) then

                res = 0.0_sp

            else

                res = huge(res)

            endif

        else if(xs < 0.0_sp) then

            res = 0.0_sp

        else

            res = exp((shape - 1._sp) * log(xs) - xs * rate + shape *      &
                log(rate) - log_gamma(shape))

        endif
    end function gamma_dist_pdf_rsp

    elemental function gamma_dist_pdf_rdp(x, shape, rate, loc)     &
        result(res)
    ! Gamma distribution probability density function
    !
        real(dp), intent(in) :: x, shape, rate
        real(dp), intent(in), optional :: loc
        real(dp) :: res
        real(dp) :: xs
        real(dp) :: loc_

        loc_ = optval(loc, 0.0_dp)

        xs = x - loc_

        if(rate <= 0.0_dp .or. shape <= 0.0_dp) then
            res = ieee_value(1.0_dp, ieee_quiet_nan)
            return
        end if

        if(xs == 0.0_dp) then

            if(shape == 1.0_dp) then

                res = rate

            else if(shape > 1.0_dp) then

                res = 0.0_dp

            else

                res = huge(res)

            endif

        else if(xs < 0.0_dp) then

            res = 0.0_dp

        else

            res = exp((shape - 1._dp) * log(xs) - xs * rate + shape *      &
                log(rate) - log_gamma(shape))

        endif
    end function gamma_dist_pdf_rdp



    impure elemental function gamma_dist_pdf_impure_rsp(x, shape, rate, err, loc) &
        result(res)
    ! Gamma distribution probability density function (impure wrapper)
    !
        real(sp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        real(sp), intent(in), optional :: loc
        real(sp) :: res

        res = gamma_dist_pdf_rsp(x, shape, rate, loc)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_pdf_impure_rsp

    impure elemental function gamma_dist_pdf_impure_rdp(x, shape, rate, err, loc) &
        result(res)
    ! Gamma distribution probability density function (impure wrapper)
    !
        real(dp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        real(dp), intent(in), optional :: loc
        real(dp) :: res

        res = gamma_dist_pdf_rdp(x, shape, rate, loc)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_pdf_impure_rdp



    elemental function gamma_dist_pdf_csp(x, shape, rate, loc)     &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are &
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, shape, rate
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = gamma_dist_pdf_rsp(x%re, shape%re, rate%re, loc_%re)
        res = res * gamma_dist_pdf_rsp(x%im, shape%im, rate%im, loc_%im)
    end function gamma_dist_pdf_csp

    elemental function gamma_dist_pdf_cdp(x, shape, rate, loc)     &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are &
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, shape, rate
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = gamma_dist_pdf_rdp(x%re, shape%re, rate%re, loc_%re)
        res = res * gamma_dist_pdf_rdp(x%im, shape%im, rate%im, loc_%im)
    end function gamma_dist_pdf_cdp



    impure elemental function gamma_dist_pdf_impure_csp(x, shape, rate, err, loc) &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = gamma_dist_pdf_rsp(x%re, shape%re, rate%re, loc_%re)
        res = res * gamma_dist_pdf_rsp(x%im, shape%im, rate%im, loc_%im)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_pdf_impure_csp

    impure elemental function gamma_dist_pdf_impure_cdp(x, shape, rate, err, loc) &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = gamma_dist_pdf_rdp(x%re, shape%re, rate%re, loc_%re)
        res = res * gamma_dist_pdf_rdp(x%im, shape%im, rate%im, loc_%im)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_pdf_impure_cdp



    impure elemental function gamma_dist_cdf_rsp(x, shape, rate, loc)   &
        result(res)
    ! Gamma distribution cumulative distribution function
    !
        real(sp), intent(in) :: x, shape, rate
        real(sp), intent(in), optional :: loc
        real(sp) :: res, xs
        real(sp) :: loc_

        loc_ = optval(loc, 0.0_sp)

        if(rate <= 0.0_sp .or. shape <= 0.0_sp) then
            res = ieee_value(1.0_sp, ieee_quiet_nan)
            return
        end if

        xs = x - loc_
        if(xs <= 0.0_sp) then
            res = 0.0_sp
            return
        end if

        res = real(regamma_p(shape, xs * rate), kind=sp)
    end function gamma_dist_cdf_rsp

    impure elemental function gamma_dist_cdf_rdp(x, shape, rate, loc)   &
        result(res)
    ! Gamma distribution cumulative distribution function
    !
        real(dp), intent(in) :: x, shape, rate
        real(dp), intent(in), optional :: loc
        real(dp) :: res, xs
        real(dp) :: loc_

        loc_ = optval(loc, 0.0_dp)

        if(rate <= 0.0_dp .or. shape <= 0.0_dp) then
            res = ieee_value(1.0_dp, ieee_quiet_nan)
            return
        end if

        xs = x - loc_
        if(xs <= 0.0_dp) then
            res = 0.0_dp
            return
        end if

        res = real(regamma_p(shape, xs * rate), kind=dp)
    end function gamma_dist_cdf_rdp



    impure elemental function gamma_dist_cdf_impure_rsp(x, shape, rate, err, loc) &
        result(res)
    ! Gamma distribution cumulative distribution function (impure wrapper)
    !
        real(sp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        real(sp), intent(in), optional :: loc
        real(sp) :: res

        res = gamma_dist_cdf_rsp(x, shape, rate, loc)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_cdf_impure_rsp

    impure elemental function gamma_dist_cdf_impure_rdp(x, shape, rate, err, loc) &
        result(res)
    ! Gamma distribution cumulative distribution function (impure wrapper)
    !
        real(dp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        real(dp), intent(in), optional :: loc
        real(dp) :: res

        res = gamma_dist_cdf_rdp(x, shape, rate, loc)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_cdf_impure_rdp



    impure elemental function gamma_dist_cdf_csp(x, shape, rate, loc)   &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, shape, rate
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = gamma_dist_cdf_rsp(x%re, shape%re, rate%re, loc_%re)

        res = res * gamma_dist_cdf_rsp(x%im, shape%im, rate%im, loc_%im)
    end function gamma_dist_cdf_csp

    impure elemental function gamma_dist_cdf_cdp(x, shape, rate, loc)   &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, shape, rate
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = gamma_dist_cdf_rdp(x%re, shape%re, rate%re, loc_%re)

        res = res * gamma_dist_cdf_rdp(x%im, shape%im, rate%im, loc_%im)
    end function gamma_dist_cdf_cdp



    impure elemental function gamma_dist_cdf_impure_csp(x, shape, rate, err, loc) &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = gamma_dist_cdf_csp(x, shape, rate, loc_)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_cdf_impure_csp

    impure elemental function gamma_dist_cdf_impure_cdp(x, shape, rate, err, loc) &
        result(res)
    ! Complex parameter gamma distributed. The real part and imaginary part are           &
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, shape, rate
        integer, intent(out) :: err
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = gamma_dist_cdf_cdp(x, shape, rate, loc_)
        err = 0
        if(ieee_is_nan(res)) err = 1
    end function gamma_dist_cdf_impure_cdp



end module stdlib_stats_distribution_gamma
