module stdlib_stats_distribution_normal
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use stdlib_kinds, only: sp, dp, xdp, qp, int32
    use stdlib_random, only: dist_rand
    use stdlib_stats_distribution_uniform, only: uni => rvs_uniform

    implicit none
    private

    real(dp), parameter  :: HALF = 0.5_dp, ONE = 1.0_dp, TWO = 2.0_dp
    integer :: kn(0:127)
    real(dp) :: wn(0:127), fn(0:127)
    logical  :: zig_norm_initialized = .false.

    public :: rvs_normal
    public :: pdf_normal
    public :: cdf_normal

    interface rvs_normal
    !! version: experimental
    !!
    !! Normal Distribution Random Variates
    !! ([Specification](../page/specs/stdlib_stats_distribution_normal.html#
    !! rvs_normal-normal-distribution-random-variates))
    !!
        module procedure rvs_norm_0_rsp                  !0 dummy variable

            module procedure rvs_norm_rsp        !2 dummy variables
            module procedure rvs_norm_rdp        !2 dummy variables
            module procedure rvs_norm_csp        !2 dummy variables
            module procedure rvs_norm_cdp        !2 dummy variables

            module procedure rvs_norm_array_rsp  !3 dummy variables
            module procedure rvs_norm_array_rdp  !3 dummy variables
            module procedure rvs_norm_array_csp  !3 dummy variables
            module procedure rvs_norm_array_cdp  !3 dummy variables
    end interface rvs_normal

    interface pdf_normal
    !! version: experimental
    !!
    !! Normal Distribution Probability Density Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_normal.html#
    !! pdf_normal-normal-distribution-probability-density-function))
    !!
            module procedure pdf_norm_rsp
            module procedure pdf_norm_rdp
            module procedure pdf_norm_csp
            module procedure pdf_norm_cdp
    end interface pdf_normal

    interface cdf_normal
    !! version: experimental
    !!
    !! Normal Distribution Cumulative Distribution Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_normal.html#
    !! cdf_normal-normal-distribution-cumulative-distribution-function))
    !!
            module procedure cdf_norm_rsp
            module procedure cdf_norm_rdp
            module procedure cdf_norm_csp
            module procedure cdf_norm_cdp
    end interface cdf_normal

contains

    impure subroutine zigset
        ! Marsaglia & Tsang generator for random normals & random exponentials.
        ! Translated from C by Alan Miller (amiller@bigpond.net.au), released as public
        ! domain (https://jblevins.org/mirror/amiller/)
        !
        ! Marsaglia, G. & Tsang, W.W. (2000) `The ziggurat method for generating
        ! random variables', J. Statist. Software, v5(8).
        !
        ! This is an electronic journal which can be downloaded from:
        ! http://www.jstatsoft.org/v05/i08
        !
        ! Latest version - 1 January 2001
        !
        real(dp), parameter :: M1 = 2147483648.0_dp, vn = 0.00991256303526217_dp
        real(dp)            :: dn, tn, q
        integer :: i

        dn = 3.442619855899_dp
        tn = dn
        !tables for random normals
        q = vn*exp(HALF*dn*dn)
        kn(0) = int((dn/q)*M1, kind=int32)
        kn(1) = 0
        wn(0) = q/M1
        wn(127) = dn/M1
        fn(0) = ONE
        fn(127) = exp(-HALF*dn*dn)
        do i = 126, 1, -1
            dn = sqrt(-TWO*log(vn/dn + exp(-HALF*dn*dn)))
            kn(i + 1) = int((dn/tn)*M1, kind=int32)
            tn = dn
            fn(i) = exp(-HALF*dn*dn)
            wn(i) = dn/M1
        end do
        zig_norm_initialized = .true.
    end subroutine zigset

        impure function rvs_norm_0_rsp () result(res)
            !
            ! Standard normal random variate (0,1)
            !
            real(sp) :: res
            real(sp), parameter  ::  r = 3.442619855899_sp, rr = 1.0_sp/r
            real(sp) ::  x, y
            integer :: hz, iz

            if (.not. zig_norm_initialized) call zigset
            iz = 0
            hz = dist_rand(1_int32)          !32bit random integer
            iz = iand(hz, 127)             !random integer in [0, 127]
            if (abs(hz) < kn(iz)) then
                res = hz*wn(iz)
            else
                L1: do
                    L2: if (iz == 0) then
                        do
                            x = -log(uni(1.0_sp))*rr
                            y = -log(uni(1.0_sp))
                            if (y + y >= x*x) exit
                        end do
                        res = r + x
                        if (hz <= 0) res = -res
                        exit L1
                    end if L2
                    x = hz*wn(iz)
                    if (fn(iz) + uni(1.0_sp)*(fn(iz - 1) - fn(iz)) < &
                        exp(-HALF*x*x)) then
                        res = x
                        exit L1
                    end if
                    hz = dist_rand(1_int32)
                    iz = iand(hz, 127)
                    if (abs(hz) < kn(iz)) then
                        res = hz*wn(iz)
                        exit L1
                    end if
                end do L1
            end if
        end function rvs_norm_0_rsp

        impure function rvs_norm_0_rdp () result(res)
            !
            ! Standard normal random variate (0,1)
            !
            real(dp) :: res
            real(dp), parameter  ::  r = 3.442619855899_dp, rr = 1.0_dp/r
            real(dp) ::  x, y
            integer :: hz, iz

            if (.not. zig_norm_initialized) call zigset
            iz = 0
            hz = dist_rand(1_int32)          !32bit random integer
            iz = iand(hz, 127)             !random integer in [0, 127]
            if (abs(hz) < kn(iz)) then
                res = hz*wn(iz)
            else
                L1: do
                    L2: if (iz == 0) then
                        do
                            x = -log(uni(1.0_dp))*rr
                            y = -log(uni(1.0_dp))
                            if (y + y >= x*x) exit
                        end do
                        res = r + x
                        if (hz <= 0) res = -res
                        exit L1
                    end if L2
                    x = hz*wn(iz)
                    if (fn(iz) + uni(1.0_dp)*(fn(iz - 1) - fn(iz)) < &
                        exp(-HALF*x*x)) then
                        res = x
                        exit L1
                    end if
                    hz = dist_rand(1_int32)
                    iz = iand(hz, 127)
                    if (abs(hz) < kn(iz)) then
                        res = hz*wn(iz)
                        exit L1
                    end if
                end do L1
            end if
        end function rvs_norm_0_rdp


        impure elemental &
            function rvs_norm_rsp (loc, scale) result(res)
            !
            ! Normal random variate (loc, scale)
            !
            real(sp), intent(in) :: loc, scale
            real(sp) :: res

            if (scale <= 0._sp) then
                res = ieee_value(1._sp, ieee_quiet_nan)
            else
                res = rvs_norm_0_rsp ()
                res = res*scale + loc
            end if

        end function rvs_norm_rsp

        impure elemental &
            function rvs_norm_rdp (loc, scale) result(res)
            !
            ! Normal random variate (loc, scale)
            !
            real(dp), intent(in) :: loc, scale
            real(dp) :: res

            if (scale <= 0._dp) then
                res = ieee_value(1._dp, ieee_quiet_nan)
            else
                res = rvs_norm_0_rdp ()
                res = res*scale + loc
            end if

        end function rvs_norm_rdp


        impure elemental function rvs_norm_csp (loc, scale) result(res)
            !
            ! Normally distributed complex. The real part and imaginary part are       &
            ! independent of each other.
            !
            complex(sp), intent(in) :: loc, scale
            complex(sp) :: res
            real(sp) :: tr, ti

            tr = rvs_norm_rsp (loc%re, scale%re)
            ti = rvs_norm_rsp (loc%im, scale%im)
            res = cmplx(tr, ti, kind=sp)

        end function rvs_norm_csp

        impure elemental function rvs_norm_cdp (loc, scale) result(res)
            !
            ! Normally distributed complex. The real part and imaginary part are       &
            ! independent of each other.
            !
            complex(dp), intent(in) :: loc, scale
            complex(dp) :: res
            real(dp) :: tr, ti

            tr = rvs_norm_rdp (loc%re, scale%re)
            ti = rvs_norm_rdp (loc%im, scale%im)
            res = cmplx(tr, ti, kind=dp)

        end function rvs_norm_cdp


        impure function rvs_norm_array_rsp (loc, scale, array_size) result(res)
            real(sp), intent(in) :: loc, scale
            integer, intent(in) :: array_size
            real(sp) :: res(array_size)
            real(sp), parameter  ::  r = 3.442619855899_sp, rr = 1.0_sp/r
            real(sp) ::  x, y, re
            integer :: hz, iz, i

            if (.not. zig_norm_initialized) call zigset

            if (scale <= 0._sp) then
                res = ieee_value(1._sp, ieee_quiet_nan)
                return
            end if

            do i = 1, array_size
                iz = 0
                hz = dist_rand(1_int32)
                iz = iand(hz, 127)
                if (abs(hz) < kn(iz)) then
                    re = hz*wn(iz)
                else
                    L1: do
                        L2: if (iz == 0) then
                            do
                                x = -log(uni(1.0_sp))*rr
                                y = -log(uni(1.0_sp))
                                if (y + y >= x*x) exit
                            end do
                            re = r + x
                            if (hz <= 0) re = -re
                            exit L1
                        end if L2
                        x = hz*wn(iz)
                        if (fn(iz) + uni(1.0_sp)*(fn(iz - 1) - fn(iz)) < &
                            exp(-HALF*x*x)) then
                            re = x
                            exit L1
                        end if

                        hz = dist_rand(1_int32)
                        iz = iand(hz, 127)
                        if (abs(hz) < kn(iz)) then
                            re = hz*wn(iz)
                            exit L1
                        end if
                    end do L1
                end if
                res(i) = re*scale + loc
            end do
        end function rvs_norm_array_rsp

        impure function rvs_norm_array_rdp (loc, scale, array_size) result(res)
            real(dp), intent(in) :: loc, scale
            integer, intent(in) :: array_size
            real(dp) :: res(array_size)
            real(dp), parameter  ::  r = 3.442619855899_dp, rr = 1.0_dp/r
            real(dp) ::  x, y, re
            integer :: hz, iz, i

            if (.not. zig_norm_initialized) call zigset

            if (scale <= 0._dp) then
                res = ieee_value(1._dp, ieee_quiet_nan)
                return
            end if

            do i = 1, array_size
                iz = 0
                hz = dist_rand(1_int32)
                iz = iand(hz, 127)
                if (abs(hz) < kn(iz)) then
                    re = hz*wn(iz)
                else
                    L1: do
                        L2: if (iz == 0) then
                            do
                                x = -log(uni(1.0_dp))*rr
                                y = -log(uni(1.0_dp))
                                if (y + y >= x*x) exit
                            end do
                            re = r + x
                            if (hz <= 0) re = -re
                            exit L1
                        end if L2
                        x = hz*wn(iz)
                        if (fn(iz) + uni(1.0_dp)*(fn(iz - 1) - fn(iz)) < &
                            exp(-HALF*x*x)) then
                            re = x
                            exit L1
                        end if

                        hz = dist_rand(1_int32)
                        iz = iand(hz, 127)
                        if (abs(hz) < kn(iz)) then
                            re = hz*wn(iz)
                            exit L1
                        end if
                    end do L1
                end if
                res(i) = re*scale + loc
            end do
        end function rvs_norm_array_rdp


        impure function rvs_norm_array_csp (loc, scale, array_size) result(res)
            complex(sp), intent(in) :: loc, scale
            integer, intent(in) :: array_size
            integer :: i
            complex(sp) :: res(array_size)
            real(sp) :: tr, ti

            do i = 1, array_size
                tr = rvs_norm_rsp (loc%re, scale%re)
                ti = rvs_norm_rsp (loc%im, scale%im)
                res(i) = cmplx(tr, ti, kind=sp)
            end do

        end function rvs_norm_array_csp

        impure function rvs_norm_array_cdp (loc, scale, array_size) result(res)
            complex(dp), intent(in) :: loc, scale
            integer, intent(in) :: array_size
            integer :: i
            complex(dp) :: res(array_size)
            real(dp) :: tr, ti

            do i = 1, array_size
                tr = rvs_norm_rdp (loc%re, scale%re)
                ti = rvs_norm_rdp (loc%im, scale%im)
                res(i) = cmplx(tr, ti, kind=dp)
            end do

        end function rvs_norm_array_cdp


        elemental function pdf_norm_rsp (x, loc, scale) result(res)
            !
            ! Normal distribution probability density function
            !
            real(sp), intent(in) :: x, loc, scale
            real(sp) :: res
            real(sp), parameter :: sqrt_2_pi = sqrt(2.0_sp*acos(-1.0_sp))

            if (scale <= 0._sp) then
                res = ieee_value(1._sp, ieee_quiet_nan)
            else
                res = exp(-0.5_sp*((x - loc)/scale)*(x - loc)/scale)/ &
                      (sqrt_2_Pi*scale)
            end if

        end function pdf_norm_rsp

        elemental function pdf_norm_rdp (x, loc, scale) result(res)
            !
            ! Normal distribution probability density function
            !
            real(dp), intent(in) :: x, loc, scale
            real(dp) :: res
            real(dp), parameter :: sqrt_2_pi = sqrt(2.0_dp*acos(-1.0_dp))

            if (scale <= 0._dp) then
                res = ieee_value(1._dp, ieee_quiet_nan)
            else
                res = exp(-0.5_dp*((x - loc)/scale)*(x - loc)/scale)/ &
                      (sqrt_2_Pi*scale)
            end if

        end function pdf_norm_rdp


        elemental function pdf_norm_csp (x, loc, scale) result(res)
            complex(sp), intent(in) :: x, loc, scale
            real(sp) :: res

            res = pdf_norm_rsp (x%re, loc%re, scale%re)
            res = res*pdf_norm_rsp (x%im, loc%im, scale%im)
        end function pdf_norm_csp

        elemental function pdf_norm_cdp (x, loc, scale) result(res)
            complex(dp), intent(in) :: x, loc, scale
            real(dp) :: res

            res = pdf_norm_rdp (x%re, loc%re, scale%re)
            res = res*pdf_norm_rdp (x%im, loc%im, scale%im)
        end function pdf_norm_cdp


        elemental function cdf_norm_rsp (x, loc, scale) result(res)
            !
            ! Normal distribution cumulative distribution function
            !
            real(sp), intent(in) :: x, loc, scale
            real(sp) :: res
            real(sp), parameter :: sqrt_2 = sqrt(2.0_sp)

            if (scale <= 0._sp) then
                res = ieee_value(1._sp, ieee_quiet_nan)
            else
                res = erfc(-(x - loc)/(scale*sqrt_2))/2.0_sp
            end if

        end function cdf_norm_rsp

        elemental function cdf_norm_rdp (x, loc, scale) result(res)
            !
            ! Normal distribution cumulative distribution function
            !
            real(dp), intent(in) :: x, loc, scale
            real(dp) :: res
            real(dp), parameter :: sqrt_2 = sqrt(2.0_dp)

            if (scale <= 0._dp) then
                res = ieee_value(1._dp, ieee_quiet_nan)
            else
                res = erfc(-(x - loc)/(scale*sqrt_2))/2.0_dp
            end if

        end function cdf_norm_rdp


        elemental function cdf_norm_csp (x, loc, scale) result(res)
            complex(sp), intent(in) :: x, loc, scale
            real(sp) :: res

            res = cdf_norm_rsp (x%re, loc%re, scale%re)
            res = res*cdf_norm_rsp (x%im, loc%im, scale%im)
        end function cdf_norm_csp

        elemental function cdf_norm_cdp (x, loc, scale) result(res)
            complex(dp), intent(in) :: x, loc, scale
            real(dp) :: res

            res = cdf_norm_rdp (x%re, loc%re, scale%re)
            res = res*cdf_norm_rdp (x%im, loc%im, scale%im)
        end function cdf_norm_cdp


end module stdlib_stats_distribution_normal
