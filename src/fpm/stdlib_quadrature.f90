module stdlib_quadrature
    !! ([Specification](../page/specs/stdlib_quadrature.html#description))
    use stdlib_kinds, only: sp, dp, qp

    implicit none

    private

    ! array integration
    public :: trapz
    public :: trapz_weights
    public :: simps
    public :: simps_weights


    interface trapz
        !! version: experimental
        !!
        !! Integrates sampled values using trapezoidal rule
        !! ([Specification](../page/specs/stdlib_quadrature.html#description))
        pure module function trapz_dx_sp(y, dx) result(integral)
          real(sp), dimension(:), intent(in) :: y
          real(sp), intent(in) :: dx
          real(sp) :: integral
        end function trapz_dx_sp
        pure module function trapz_dx_dp(y, dx) result(integral)
          real(dp), dimension(:), intent(in) :: y
          real(dp), intent(in) :: dx
          real(dp) :: integral
        end function trapz_dx_dp
        pure module function trapz_dx_qp(y, dx) result(integral)
          real(qp), dimension(:), intent(in) :: y
          real(qp), intent(in) :: dx
          real(qp) :: integral
        end function trapz_dx_qp
        module function trapz_x_sp(y, x) result(integral)
            real(sp), dimension(:), intent(in) :: y
            real(sp), dimension(:), intent(in) :: x
            real(sp) :: integral
        end function trapz_x_sp
        module function trapz_x_dp(y, x) result(integral)
            real(dp), dimension(:), intent(in) :: y
            real(dp), dimension(:), intent(in) :: x
            real(dp) :: integral
        end function trapz_x_dp
        module function trapz_x_qp(y, x) result(integral)
            real(qp), dimension(:), intent(in) :: y
            real(qp), dimension(:), intent(in) :: x
            real(qp) :: integral
        end function trapz_x_qp
    end interface trapz


    interface trapz_weights
        !! version: experimental
        !!
        !! Integrates sampled values using trapezoidal rule weights for given abscissas
        !! ([Specification](../page/specs/stdlib_quadrature.html#description_1))
        pure module function trapz_weights_sp(x) result(w)
            real(sp), dimension(:), intent(in) :: x
            real(sp), dimension(size(x)) :: w
        end function trapz_weights_sp
        pure module function trapz_weights_dp(x) result(w)
            real(dp), dimension(:), intent(in) :: x
            real(dp), dimension(size(x)) :: w
        end function trapz_weights_dp
        pure module function trapz_weights_qp(x) result(w)
            real(qp), dimension(:), intent(in) :: x
            real(qp), dimension(size(x)) :: w
        end function trapz_weights_qp
    end interface trapz_weights


    interface simps
        !! version: experimental
        !!
        !! Integrates sampled values using Simpson's rule
        !! ([Specification](../page/specs/stdlib_quadrature.html#description_3))
        ! "recursive" is an implementation detail
        pure recursive module function simps_dx_sp(y, dx, even) result(integral)
            real(sp), dimension(:), intent(in) :: y
            real(sp), intent(in) :: dx
            integer, intent(in), optional :: even
            real(sp) :: integral
        end function simps_dx_sp
        pure recursive module function simps_dx_dp(y, dx, even) result(integral)
            real(dp), dimension(:), intent(in) :: y
            real(dp), intent(in) :: dx
            integer, intent(in), optional :: even
            real(dp) :: integral
        end function simps_dx_dp
        pure recursive module function simps_dx_qp(y, dx, even) result(integral)
            real(qp), dimension(:), intent(in) :: y
            real(qp), intent(in) :: dx
            integer, intent(in), optional :: even
            real(qp) :: integral
        end function simps_dx_qp
        recursive module function simps_x_sp(y, x, even) result(integral)
            real(sp), dimension(:), intent(in) :: y
            real(sp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: even
            real(sp) :: integral
        end function simps_x_sp
        recursive module function simps_x_dp(y, x, even) result(integral)
            real(dp), dimension(:), intent(in) :: y
            real(dp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: even
            real(dp) :: integral
        end function simps_x_dp
        recursive module function simps_x_qp(y, x, even) result(integral)
            real(qp), dimension(:), intent(in) :: y
            real(qp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: even
            real(qp) :: integral
        end function simps_x_qp
    end interface simps


    interface simps_weights
        !! version: experimental
        !!
        !! Integrates sampled values using trapezoidal rule weights for given abscissas
        !! ([Specification](../page/specs/stdlib_quadrature.html#description_3))
        pure recursive module function simps_weights_sp(x, even) result(w)
            real(sp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: even
            real(sp), dimension(size(x)) :: w
        end function simps_weights_sp
        pure recursive module function simps_weights_dp(x, even) result(w)
            real(dp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: even
            real(dp), dimension(size(x)) :: w
        end function simps_weights_dp
        pure recursive module function simps_weights_qp(x, even) result(w)
            real(qp), dimension(:), intent(in) :: x
            integer, intent(in), optional :: even
            real(qp), dimension(size(x)) :: w
        end function simps_weights_qp
    end interface simps_weights


    ! Interface for a simple f(x)-style integrand function.
    ! Could become fancier as we learn about the performance
    ! ramifications of different ways to do callbacks.
    abstract interface
        pure function integrand_sp(x) result(f)
            import :: sp
            real(sp), intent(in) :: x
            real(sp) :: f
        end function integrand_sp
        pure function integrand_dp(x) result(f)
            import :: dp
            real(dp), intent(in) :: x
            real(dp) :: f
        end function integrand_dp
        pure function integrand_qp(x) result(f)
            import :: qp
            real(qp), intent(in) :: x
            real(qp) :: f
        end function integrand_qp
    end interface

end module stdlib_quadrature
