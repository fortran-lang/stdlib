
submodule(stdlib_math) stdlib_math_is_close

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none
    
    real(sp), parameter :: sqrt_eps_sp = sqrt(epsilon(1.0_sp))
    real(dp), parameter :: sqrt_eps_dp = sqrt(epsilon(1.0_dp))
    
contains


    elemental module logical function is_close_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
        real(sp), intent(in) :: a, b
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan
        logical :: equal_nan_

        equal_nan_ = optval(equal_nan, .false.)
        
        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            close = equal_nan_ .and. ieee_is_nan(a) .and. ieee_is_nan(b)
        else
            close = abs(a - b) <= max( abs(optval(rel_tol, sqrt_eps_sp)*max(abs(a), abs(b))), &
                                       abs(optval(abs_tol, 0.0_sp)) )
        end if     

    end function is_close_rsp
    elemental module logical function is_close_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
        real(dp), intent(in) :: a, b
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan
        logical :: equal_nan_

        equal_nan_ = optval(equal_nan, .false.)
        
        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            close = equal_nan_ .and. ieee_is_nan(a) .and. ieee_is_nan(b)
        else
            close = abs(a - b) <= max( abs(optval(rel_tol, sqrt_eps_dp)*max(abs(a), abs(b))), &
                                       abs(optval(abs_tol, 0.0_dp)) )
        end if     

    end function is_close_rdp

    elemental module logical function is_close_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
        complex(sp), intent(in) :: a, b
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = is_close_rsp(a%re, b%re, rel_tol, abs_tol, equal_nan) .and. &
                is_close_rsp(a%im, b%im, rel_tol, abs_tol, equal_nan)

    end function is_close_csp
    elemental module logical function is_close_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
        complex(dp), intent(in) :: a, b
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = is_close_rdp(a%re, b%re, rel_tol, abs_tol, equal_nan) .and. &
                is_close_rdp(a%im, b%im, rel_tol, abs_tol, equal_nan)

    end function is_close_cdp

end submodule stdlib_math_is_close
