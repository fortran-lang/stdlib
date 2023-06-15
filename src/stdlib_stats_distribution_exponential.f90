module stdlib_stats_distribution_exponential
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use stdlib_kinds, only : sp, dp, xdp, qp, int32
    use stdlib_random, only : dist_rand
    use stdlib_stats_distribution_uniform, only : uni=>rvs_uniform

    implicit none
    private

    integer :: ke(0:255)
    real(dp) :: we(0:255), fe(0:255)
    logical  :: zig_exp_initialized = .false.

    public :: rvs_exp
    public :: pdf_exp
    public :: cdf_exp



    interface rvs_exp
    !! Version experimental
    !!
    !! Exponential Distribution Random Variates
    !! ([Specification](../page/specs/stdlib_stats_distribution_exponential.html#
    !! rvs_exp-exponential-distribution-random-variates))
    !!
        module procedure rvs_exp_0_rsp                 !0 dummy variable

        module procedure rvs_exp_rsp       !1 dummy variable
        module procedure rvs_exp_rdp       !1 dummy variable
        module procedure rvs_exp_csp       !1 dummy variable
        module procedure rvs_exp_cdp       !1 dummy variable

        module procedure rvs_exp_array_rsp !2 dummy variables
        module procedure rvs_exp_array_rdp !2 dummy variables
        module procedure rvs_exp_array_csp !2 dummy variables
        module procedure rvs_exp_array_cdp !2 dummy variables
    end interface rvs_exp



    interface pdf_exp
    !! Version experimental
    !!
    !! Exponential Distribution Probability Density Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_exponential.html#
    !! pdf_exp-exponential-distribution-probability-density-function))
    !!
        module procedure pdf_exp_rsp
        module procedure pdf_exp_rdp
        module procedure pdf_exp_csp
        module procedure pdf_exp_cdp
    end interface pdf_exp



    interface cdf_exp
    !! Version experimental
    !!
    !! Exponential Cumulative Distribution Function
    !! ([Specification](../page/specs/stdlib_stats_distribution_exponential.html#
    !! cdf_exp-exponential-distribution-cumulative-distribution-function))
    !!
        module procedure cdf_exp_rsp
        module procedure cdf_exp_rdp
        module procedure cdf_exp_csp
        module procedure cdf_exp_cdp
    end interface cdf_exp





contains

    impure subroutine zigset
    ! Marsaglia & Tsang generator for random normals & random exponentials.
    ! Translated from C by Alan Miller (amiller@bigpond.net.au)
    !
    ! Marsaglia, G. & Tsang, W.W. (2000) 'The ziggurat method for generating
    ! random variables', J. Statist. Software, v5(8).
    !
    ! This is an electronic journal which can be downloaded from:
    ! http://www.jstatsoft.org/v05/i08
    !
    ! Latest version - 1 January 2001
    !
        real(dp), parameter :: M2 = 2147483648.0_dp, ve = 0.003949659822581572_dp
        real(dp), parameter :: ONE = 1.0_dp
        real(dp)            :: de, te, q
        integer :: i

        de = 7.697117470131487_dp
        te = de
    ! tables for random exponentials
        q = ve * exp(de)
        ke(0) = int((de / q) * M2, kind = int32)
        ke(1) = 0
        we(0) = q / M2
        we(255) = de / M2
        fe(0) = ONE
        fe(255) = exp(- de)
        do  i = 254, 1, -1
            de = -log(ve / de + exp(- de))
            ke(i+1) = int(M2 * (de / te), kind = int32)
            te = de
            fe(i) = exp(- de)
            we(i) = de / M2
        end do
        zig_exp_initialized = .true.
    end subroutine zigset




    impure function rvs_exp_0_rsp( ) result(res)
    !
    ! Standard exponential random variate (lambda=1)
    !
        real(sp) :: res, x
        real(sp), parameter :: r = 7.69711747013104972_sp
        integer :: jz, iz

        if(.not. zig_exp_initialized ) call zigset
        iz = 0
        jz = dist_rand(1_int32)                ! 32bit random integer
        iz = iand( jz, 255 )                   ! random integer in [0, 255]
        if( abs( jz ) < ke(iz) ) then
            res = abs(jz) * we(iz)
        else
            L1: do
                if( iz == 0 ) then
                    res = r - log( uni(1.0_sp) )
                    exit L1
                end if
                x = abs( jz ) * we(iz)
                if(fe(iz) + uni(1.0_sp) * (fe(iz-1) - fe(iz)) < exp(-x)) then
                    res = x
                    exit L1
                end if
                jz = dist_rand(1_int32)
                iz = iand( jz, 255 )
                if( abs( jz ) < ke(iz) ) then
                    res = abs( jz ) * we(iz)
                    exit L1
                end if
           end do L1
       endif
    end function rvs_exp_0_rsp

    impure function rvs_exp_0_rdp( ) result(res)
    !
    ! Standard exponential random variate (lambda=1)
    !
        real(dp) :: res, x
        real(dp), parameter :: r = 7.69711747013104972_dp
        integer :: jz, iz

        if(.not. zig_exp_initialized ) call zigset
        iz = 0
        jz = dist_rand(1_int32)                ! 32bit random integer
        iz = iand( jz, 255 )                   ! random integer in [0, 255]
        if( abs( jz ) < ke(iz) ) then
            res = abs(jz) * we(iz)
        else
            L1: do
                if( iz == 0 ) then
                    res = r - log( uni(1.0_dp) )
                    exit L1
                end if
                x = abs( jz ) * we(iz)
                if(fe(iz) + uni(1.0_dp) * (fe(iz-1) - fe(iz)) < exp(-x)) then
                    res = x
                    exit L1
                end if
                jz = dist_rand(1_int32)
                iz = iand( jz, 255 )
                if( abs( jz ) < ke(iz) ) then
                    res = abs( jz ) * we(iz)
                    exit L1
                end if
           end do L1
       endif
    end function rvs_exp_0_rdp





    impure elemental function rvs_exp_rsp(lambda) result(res)
    !
    ! Exponential distributed random variate
    !
        real(sp), intent(in) :: lambda
        real(sp) :: res

        if (lambda <= 0._sp) then
            res = ieee_value(1._sp, ieee_quiet_nan)
        else
            res = rvs_exp_0_rsp(  )
            res = res / lambda
        end if
    end function rvs_exp_rsp

    impure elemental function rvs_exp_rdp(lambda) result(res)
    !
    ! Exponential distributed random variate
    !
        real(dp), intent(in) :: lambda
        real(dp) :: res

        if (lambda <= 0._dp) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else
            res = rvs_exp_0_rdp(  )
            res = res / lambda
        end if
    end function rvs_exp_rdp





    impure elemental function rvs_exp_csp(lambda) result(res)
        complex(sp), intent(in) :: lambda
        complex(sp) :: res
        real(sp) :: tr, ti

        tr = rvs_exp_rsp(lambda % re)
        ti = rvs_exp_rsp(lambda % im)
        res = cmplx(tr, ti, kind=sp)
    end function rvs_exp_csp

    impure elemental function rvs_exp_cdp(lambda) result(res)
        complex(dp), intent(in) :: lambda
        complex(dp) :: res
        real(dp) :: tr, ti

        tr = rvs_exp_rdp(lambda % re)
        ti = rvs_exp_rdp(lambda % im)
        res = cmplx(tr, ti, kind=dp)
    end function rvs_exp_cdp





    impure function rvs_exp_array_rsp(lambda, array_size) result(res)
        real(sp), intent(in) :: lambda
        integer, intent(in) :: array_size
        real(sp) :: res(array_size), x, re
        real(sp), parameter :: r = 7.69711747013104972_sp
        integer :: jz, iz, i

        if (lambda <= 0._sp) then
            res = ieee_value(1._sp, ieee_quiet_nan)
            return
        end if

        if(.not. zig_exp_initialized) call zigset
        do i = 1, array_size
            iz = 0
            jz = dist_rand(1_int32)
            iz = iand( jz, 255 )
            if( abs( jz ) < ke(iz) ) then
                re = abs(jz) * we(iz)
            else
                L1: do
                    if( iz == 0 ) then
                        re = r - log( uni(1.0_sp) )
                        exit L1
                    end if
                    x = abs( jz ) * we(iz)
                    if(fe(iz) + uni(1.0_sp)*(fe(iz-1)-fe(iz)) < exp(-x)) then
                        re = x
                        exit L1
                    end if
                    jz = dist_rand(1_int32)
                    iz = iand( jz, 255 )
                    if( abs( jz ) < ke(iz) ) then
                        re = abs( jz ) * we(iz)
                        exit L1
                    end if
               end do L1
            endif
            res(i) = re / lambda
        end do
    end function rvs_exp_array_rsp

    impure function rvs_exp_array_rdp(lambda, array_size) result(res)
        real(dp), intent(in) :: lambda
        integer, intent(in) :: array_size
        real(dp) :: res(array_size), x, re
        real(dp), parameter :: r = 7.69711747013104972_dp
        integer :: jz, iz, i

        if (lambda <= 0._dp) then
            res = ieee_value(1._dp, ieee_quiet_nan)
            return
        end if

        if(.not. zig_exp_initialized) call zigset
        do i = 1, array_size
            iz = 0
            jz = dist_rand(1_int32)
            iz = iand( jz, 255 )
            if( abs( jz ) < ke(iz) ) then
                re = abs(jz) * we(iz)
            else
                L1: do
                    if( iz == 0 ) then
                        re = r - log( uni(1.0_dp) )
                        exit L1
                    end if
                    x = abs( jz ) * we(iz)
                    if(fe(iz) + uni(1.0_dp)*(fe(iz-1)-fe(iz)) < exp(-x)) then
                        re = x
                        exit L1
                    end if
                    jz = dist_rand(1_int32)
                    iz = iand( jz, 255 )
                    if( abs( jz ) < ke(iz) ) then
                        re = abs( jz ) * we(iz)
                        exit L1
                    end if
               end do L1
            endif
            res(i) = re / lambda
        end do
    end function rvs_exp_array_rdp





    impure function rvs_exp_array_csp(lambda, array_size) result(res)
        complex(sp), intent(in) :: lambda
        integer, intent(in) :: array_size
        complex(sp) :: res(array_size)
        integer :: i
        real(sp) :: tr, ti

        do i = 1, array_size
            tr = rvs_exp_rsp(lambda % re)
            ti = rvs_exp_rsp(lambda % im)
            res(i) = cmplx(tr, ti, kind=sp)
        end do
    end function rvs_exp_array_csp

    impure function rvs_exp_array_cdp(lambda, array_size) result(res)
        complex(dp), intent(in) :: lambda
        integer, intent(in) :: array_size
        complex(dp) :: res(array_size)
        integer :: i
        real(dp) :: tr, ti

        do i = 1, array_size
            tr = rvs_exp_rdp(lambda % re)
            ti = rvs_exp_rdp(lambda % im)
            res(i) = cmplx(tr, ti, kind=dp)
        end do
    end function rvs_exp_array_cdp





    elemental function pdf_exp_rsp(x, lambda) result(res)
    !
    ! Exponential Distribution Probability Density Function
    !
        real(sp), intent(in) :: x, lambda
        real(sp) :: res

        if (lambda <= 0._sp) then
            res = ieee_value(1._sp, ieee_quiet_nan)
        else if (x < 0._sp) then
            res = 0._sp
        else
            res = exp(- x * lambda) * lambda
        end if
    end function pdf_exp_rsp

    elemental function pdf_exp_rdp(x, lambda) result(res)
    !
    ! Exponential Distribution Probability Density Function
    !
        real(dp), intent(in) :: x, lambda
        real(dp) :: res

        if (lambda <= 0._dp) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (x < 0._dp) then
            res = 0._dp
        else
            res = exp(- x * lambda) * lambda
        end if
    end function pdf_exp_rdp





    elemental function pdf_exp_csp(x, lambda) result(res)
        complex(sp), intent(in) :: x, lambda
        real(sp) :: res

        res = pdf_exp_rsp(x % re, lambda % re)
        res = res * pdf_exp_rsp(x % im, lambda % im)
    end function pdf_exp_csp

    elemental function pdf_exp_cdp(x, lambda) result(res)
        complex(dp), intent(in) :: x, lambda
        real(dp) :: res

        res = pdf_exp_rdp(x % re, lambda % re)
        res = res * pdf_exp_rdp(x % im, lambda % im)
    end function pdf_exp_cdp





    elemental function cdf_exp_rsp(x, lambda) result(res)
    !
    ! Exponential Distribution Cumulative Distribution Function
    !
        real(sp), intent(in) :: x, lambda
        real(sp) :: res

        if (lambda <= 0._sp) then
            res = ieee_value(1._sp, ieee_quiet_nan)
        else if (x < 0._sp) then
            res = 0._sp
        else
            res = 1.0_sp - exp(- x * lambda)
        end if
    end function cdf_exp_rsp

    elemental function cdf_exp_rdp(x, lambda) result(res)
    !
    ! Exponential Distribution Cumulative Distribution Function
    !
        real(dp), intent(in) :: x, lambda
        real(dp) :: res

        if (lambda <= 0._dp) then
            res = ieee_value(1._dp, ieee_quiet_nan)
        else if (x < 0._dp) then
            res = 0._dp
        else
            res = 1.0_dp - exp(- x * lambda)
        end if
    end function cdf_exp_rdp





    elemental function cdf_exp_csp(x, lambda) result(res)
        complex(sp), intent(in) :: x, lambda
        real(sp) :: res

        res = cdf_exp_rsp(x % re, lambda % re)
        res = res * cdf_exp_rsp(x % im, lambda % im)
    end function cdf_exp_csp

    elemental function cdf_exp_cdp(x, lambda) result(res)
        complex(dp), intent(in) :: x, lambda
        real(dp) :: res

        res = cdf_exp_rdp(x % re, lambda % re)
        res = res * cdf_exp_rdp(x % im, lambda % im)
    end function cdf_exp_cdp


end module stdlib_stats_distribution_exponential
