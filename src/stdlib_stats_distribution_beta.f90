module stdlib_stats_distribution_beta
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan, ieee_is_nan
    use stdlib_kinds, only : sp, dp, xdp
    use stdlib_error, only : error_stop
    use stdlib_optval, only : optval
    use stdlib_stats_distribution_uniform, only : uni=>rvs_uniform
    use stdlib_stats_distribution_gamma, only : rgamma=>rvs_gamma
    use stdlib_specialfunctions_gamma, only : beta, incomplete_beta, log_beta

    implicit none
    private

    public :: rvs_beta
    public :: pdf_beta
    public :: cdf_beta


    interface rvs_beta
    !! Version experimental
    !!
    !! Beta Distribution Random Variates
    !! ([Specification](../page/specs/stdlib_stats_distribution_beta.html#
    !! rvs_beta-beta-distribution-random-variates))
    !!
        module procedure beta_dist_rvs_rsp       ! 2 arguments
        module procedure beta_dist_rvs_rdp       ! 2 arguments
        module procedure beta_dist_rvs_csp       ! 2 arguments
        module procedure beta_dist_rvs_cdp       ! 2 arguments

        module procedure beta_dist_rvs_array_rsp ! 3 arguments
        module procedure beta_dist_rvs_array_rdp ! 3 arguments
        module procedure beta_dist_rvs_array_csp ! 3 arguments
        module procedure beta_dist_rvs_array_cdp ! 3 arguments
    end interface rvs_beta


    interface pdf_beta
    !! Version experimental
    !!
    !! Beta Distribution Probability Density Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_beta.html#
    !! pdf_beta-beta-distribution-probability-density-function))
    !!
        module procedure beta_dist_pdf_rsp
        module procedure beta_dist_pdf_rdp
        module procedure beta_dist_pdf_csp
        module procedure beta_dist_pdf_cdp

        module procedure beta_dist_pdf_impure_rsp
        module procedure beta_dist_pdf_impure_rdp
        module procedure beta_dist_pdf_impure_csp
        module procedure beta_dist_pdf_impure_cdp
    end interface pdf_beta


    interface cdf_beta
    !! Version experimental
    !!
    !! Beta Distribution Cumulative Distribution Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_beta.html#
    !! cdf_beta-beta-distribution-cumulative-distribution-function))
    !!
        module procedure beta_dist_cdf_rsp
        module procedure beta_dist_cdf_rdp
        module procedure beta_dist_cdf_csp
        module procedure beta_dist_cdf_cdp

        module procedure beta_dist_cdf_impure_rsp
        module procedure beta_dist_cdf_impure_rdp
        module procedure beta_dist_cdf_impure_csp
        module procedure beta_dist_cdf_impure_cdp
    end interface cdf_beta




contains

    impure elemental function beta_dist_rvs_rsp(a, b, loc)      &
        result(res)
    ! Beta random variate using gamma variates
    ! If a < 1 or b < 1, uses uniform method, otherwise uses gamma method
    !
        real(sp), intent(in) :: a, b
        real(sp), intent(in), optional :: loc
        real(sp) :: res, x, y, xx(2)
        real(sp) :: loc_
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        loc_ = optval(loc, 0.0_sp)

        if(a <= zero .or. b <= zero) then
            res = ieee_value(1.0_sp, ieee_quiet_nan)
            return
        end if

        if(a < one .or. b < one) then
            ! Use uniform method for small parameters
            do
                xx = uni(zero, one, 2)
                x = xx(1) ** (one / a)
                y = xx(2) ** (one / b)
                y = x + y
                if(y <= one .and. y /= zero) exit
            end do
        else
            ! Use gamma method for larger parameters
            do
                x = rgamma(a, one)
                y = rgamma(b, one)
                y = x + y
                if(y /= zero) exit
            end do
        end if
        
        res = x / y + loc_
    end function beta_dist_rvs_rsp

    impure elemental function beta_dist_rvs_rdp(a, b, loc)      &
        result(res)
    ! Beta random variate using gamma variates
    ! If a < 1 or b < 1, uses uniform method, otherwise uses gamma method
    !
        real(dp), intent(in) :: a, b
        real(dp), intent(in), optional :: loc
        real(dp) :: res, x, y, xx(2)
        real(dp) :: loc_
        real(dp), parameter :: zero = 0.0_dp, one = 1.0_dp

        loc_ = optval(loc, 0.0_dp)

        if(a <= zero .or. b <= zero) then
            res = ieee_value(1.0_dp, ieee_quiet_nan)
            return
        end if

        if(a < one .or. b < one) then
            ! Use uniform method for small parameters
            do
                xx = uni(zero, one, 2)
                x = xx(1) ** (one / a)
                y = xx(2) ** (one / b)
                y = x + y
                if(y <= one .and. y /= zero) exit
            end do
        else
            ! Use gamma method for larger parameters
            do
                x = rgamma(a, one)
                y = rgamma(b, one)
                y = x + y
                if(y /= zero) exit
            end do
        end if
        
        res = x / y + loc_
    end function beta_dist_rvs_rdp



    impure elemental function beta_dist_rvs_csp(a, b, loc)      &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(sp), intent(in) :: a, b
        complex(sp), intent(in), optional :: loc
        complex(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = cmplx(beta_dist_rvs_rsp(a%re, b%re) + loc_%re,                 &
                    beta_dist_rvs_rsp(a%im, b%im) + loc_%im, kind=sp)
    end function beta_dist_rvs_csp

    impure elemental function beta_dist_rvs_cdp(a, b, loc)      &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(dp), intent(in) :: a, b
        complex(dp), intent(in), optional :: loc
        complex(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = cmplx(beta_dist_rvs_rdp(a%re, b%re) + loc_%re,                 &
                    beta_dist_rvs_rdp(a%im, b%im) + loc_%im, kind=dp)
    end function beta_dist_rvs_cdp



    function beta_dist_rvs_array_rsp(a, b, array_size, loc)     &
        result(res)
    !
        real(sp), intent(in) :: a, b
        integer, intent(in) :: array_size
        real(sp), intent(in), optional :: loc
        real(sp) :: res(array_size)
        integer :: i
        real(sp) :: loc_

        loc_ = optval(loc, 0.0_sp)

        do i = 1, array_size
            res(i) = beta_dist_rvs_rsp(a, b, loc_)
        end do
    end function beta_dist_rvs_array_rsp

    function beta_dist_rvs_array_rdp(a, b, array_size, loc)     &
        result(res)
    !
        real(dp), intent(in) :: a, b
        integer, intent(in) :: array_size
        real(dp), intent(in), optional :: loc
        real(dp) :: res(array_size)
        integer :: i
        real(dp) :: loc_

        loc_ = optval(loc, 0.0_dp)

        do i = 1, array_size
            res(i) = beta_dist_rvs_rdp(a, b, loc_)
        end do
    end function beta_dist_rvs_array_rdp



    function beta_dist_rvs_array_csp(a, b, array_size, loc)     &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(sp), intent(in) :: a, b
        integer, intent(in) :: array_size
        complex(sp), intent(in), optional :: loc
        complex(sp) :: res(array_size)
        integer :: i
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        do i = 1, array_size
            res(i) = beta_dist_rvs_csp(a, b, loc_)
        end do
    end function beta_dist_rvs_array_csp

    function beta_dist_rvs_array_cdp(a, b, array_size, loc)     &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(dp), intent(in) :: a, b
        integer, intent(in) :: array_size
        complex(dp), intent(in), optional :: loc
        complex(dp) :: res(array_size)
        integer :: i
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        do i = 1, array_size
            res(i) = beta_dist_rvs_cdp(a, b, loc_)
        end do
    end function beta_dist_rvs_array_cdp



    elemental function beta_dist_pdf_rsp(x, a, b, loc)     &
        result(res)
    ! Beta distribution probability density function
    !
        real(sp), intent(in) :: x, a, b
        real(sp), intent(in), optional :: loc
        real(sp) :: res
        real(sp) :: xs
        real(sp) :: loc_
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(a <= zero .or. b <= zero) then
            res = ieee_value(1.0_sp, ieee_quiet_nan)
            return
        end if

        loc_ = optval(loc, 0.0_sp)
        xs = x - loc_

        if(xs <= zero .or. xs >= one) then
            res = zero
            return
        end if

        ! Use log formulation for numerical stability
        res = exp((a - one) * log(xs) + (b - one) * log(one - xs) - log_beta(a, b))
    end function beta_dist_pdf_rsp

    elemental function beta_dist_pdf_rdp(x, a, b, loc)     &
        result(res)
    ! Beta distribution probability density function
    !
        real(dp), intent(in) :: x, a, b
        real(dp), intent(in), optional :: loc
        real(dp) :: res
        real(dp) :: xs
        real(dp) :: loc_
        real(dp), parameter :: zero = 0.0_dp, one = 1.0_dp

        if(a <= zero .or. b <= zero) then
            res = ieee_value(1.0_dp, ieee_quiet_nan)
            return
        end if

        loc_ = optval(loc, 0.0_dp)
        xs = x - loc_

        if(xs <= zero .or. xs >= one) then
            res = zero
            return
        end if

        ! Use log formulation for numerical stability
        res = exp((a - one) * log(xs) + (b - one) * log(one - xs) - log_beta(a, b))
    end function beta_dist_pdf_rdp



    impure elemental function beta_dist_pdf_impure_rsp(x, a, b, err, loc) &
        result(res)
    ! Beta distribution probability density function (impure wrapper)
    !
        real(sp), intent(in) :: x, a, b
        integer, intent(out) :: err
        real(sp), intent(in), optional :: loc
        real(sp) :: res

        res = beta_dist_pdf_rsp(x, a, b, loc)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_pdf_impure_rsp

    impure elemental function beta_dist_pdf_impure_rdp(x, a, b, err, loc) &
        result(res)
    ! Beta distribution probability density function (impure wrapper)
    !
        real(dp), intent(in) :: x, a, b
        integer, intent(out) :: err
        real(dp), intent(in), optional :: loc
        real(dp) :: res

        res = beta_dist_pdf_rdp(x, a, b, loc)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_pdf_impure_rdp



    elemental function beta_dist_pdf_csp(x, a, b, loc)     &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, a, b
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = beta_dist_pdf_rsp(x%re, a%re, b%re, loc_%re)
        res = res * beta_dist_pdf_rsp(x%im, a%im, b%im, loc_%im)
    end function beta_dist_pdf_csp

    elemental function beta_dist_pdf_cdp(x, a, b, loc)     &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, a, b
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = beta_dist_pdf_rdp(x%re, a%re, b%re, loc_%re)
        res = res * beta_dist_pdf_rdp(x%im, a%im, b%im, loc_%im)
    end function beta_dist_pdf_cdp



    impure elemental function beta_dist_pdf_impure_csp(x, a, b, err, loc) &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, a, b
        integer, intent(out) :: err
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = beta_dist_pdf_rsp(x%re, a%re, b%re, loc_%re)
        res = res * beta_dist_pdf_rsp(x%im, a%im, b%im, loc_%im)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_pdf_impure_csp

    impure elemental function beta_dist_pdf_impure_cdp(x, a, b, err, loc) &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, a, b
        integer, intent(out) :: err
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = beta_dist_pdf_rdp(x%re, a%re, b%re, loc_%re)
        res = res * beta_dist_pdf_rdp(x%im, a%im, b%im, loc_%im)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_pdf_impure_cdp



    elemental function beta_dist_cdf_rsp(x, a, b, loc)   &
        result(res)
    ! Beta distribution cumulative distribution function
    !
        real(sp), intent(in) :: x, a, b
        real(sp), intent(in), optional :: loc
        real(sp) :: res
        real(sp) :: xs
        real(sp) :: loc_
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        loc_ = optval(loc, 0.0_sp)
        xs = x - loc_

        if(a <= zero .or. b <= zero) then
            res = ieee_value(1.0_sp, ieee_quiet_nan)
            return
        end if

        if(xs <= zero) then
            res = zero
            return
        end if

        if(xs >= one) then
            res = one
            return
        end if

        res = real(incomplete_beta(xs, a, b), kind=sp)
    end function beta_dist_cdf_rsp

    elemental function beta_dist_cdf_rdp(x, a, b, loc)   &
        result(res)
    ! Beta distribution cumulative distribution function
    !
        real(dp), intent(in) :: x, a, b
        real(dp), intent(in), optional :: loc
        real(dp) :: res
        real(dp) :: xs
        real(dp) :: loc_
        real(dp), parameter :: zero = 0.0_dp, one = 1.0_dp

        loc_ = optval(loc, 0.0_dp)
        xs = x - loc_

        if(a <= zero .or. b <= zero) then
            res = ieee_value(1.0_dp, ieee_quiet_nan)
            return
        end if

        if(xs <= zero) then
            res = zero
            return
        end if

        if(xs >= one) then
            res = one
            return
        end if

        res = real(incomplete_beta(xs, a, b), kind=dp)
    end function beta_dist_cdf_rdp



    impure elemental function beta_dist_cdf_impure_rsp(x, a, b, err, loc) &
        result(res)
    ! Beta distribution cumulative distribution function (impure wrapper)
    !
        real(sp), intent(in) :: x, a, b
        integer, intent(out) :: err
        real(sp), intent(in), optional :: loc
        real(sp) :: res

        res = beta_dist_cdf_rsp(x, a, b, loc)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_cdf_impure_rsp

    impure elemental function beta_dist_cdf_impure_rdp(x, a, b, err, loc) &
        result(res)
    ! Beta distribution cumulative distribution function (impure wrapper)
    !
        real(dp), intent(in) :: x, a, b
        integer, intent(out) :: err
        real(dp), intent(in), optional :: loc
        real(dp) :: res

        res = beta_dist_cdf_rdp(x, a, b, loc)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_cdf_impure_rdp



    elemental function beta_dist_cdf_csp(x, a, b, loc)   &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, a, b
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = beta_dist_cdf_rsp(x%re, a%re, b%re, loc_%re)
        res = res * beta_dist_cdf_rsp(x%im, a%im, b%im, loc_%im)
    end function beta_dist_cdf_csp

    elemental function beta_dist_cdf_cdp(x, a, b, loc)   &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, a, b
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = beta_dist_cdf_rdp(x%re, a%re, b%re, loc_%re)
        res = res * beta_dist_cdf_rdp(x%im, a%im, b%im, loc_%im)
    end function beta_dist_cdf_cdp



    impure elemental function beta_dist_cdf_impure_csp(x, a, b, err, loc) &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(sp), intent(in) :: x, a, b
        integer, intent(out) :: err
        complex(sp), intent(in), optional :: loc
        real(sp) :: res
        complex(sp) :: loc_

        loc_ = optval(loc, cmplx(0.0_sp, 0.0_sp, kind=sp))

        res = beta_dist_cdf_csp(x, a, b, loc_)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_cdf_impure_csp

    impure elemental function beta_dist_cdf_impure_cdp(x, a, b, err, loc) &
        result(res)
    ! Complex parameter beta distributed. The real part and imaginary part are
    ! independent of each other.
    !
        complex(dp), intent(in) :: x, a, b
        integer, intent(out) :: err
        complex(dp), intent(in), optional :: loc
        real(dp) :: res
        complex(dp) :: loc_

        loc_ = optval(loc, cmplx(0.0_dp, 0.0_dp, kind=dp))

        res = beta_dist_cdf_cdp(x, a, b, loc_)
        err = merge(1, 0, ieee_is_nan(res))
    end function beta_dist_cdf_impure_cdp



end module stdlib_stats_distribution_beta
