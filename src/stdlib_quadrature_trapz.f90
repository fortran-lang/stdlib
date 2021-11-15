
submodule (stdlib_quadrature) stdlib_quadrature_trapz
    use stdlib_error, only: check
    implicit none

contains

    
    pure module function trapz_dx_sp(y, dx) result(integral)
        real(sp), dimension(:), intent(in) :: y
        real(sp), intent(in) :: dx
        real(sp) :: integral

        integer :: n

        n = size(y)
        
        select case (n)
        case (0:1)
            integral = 0.0_sp
        case (2)
            integral = 0.5_sp*dx*(y(1) + y(2))
        case default
            integral = dx*(sum(y(2:n-1)) + 0.5_sp*(y(1) + y(n)))
        end select
    end function trapz_dx_sp
    
    
    pure module function trapz_dx_dp(y, dx) result(integral)
        real(dp), dimension(:), intent(in) :: y
        real(dp), intent(in) :: dx
        real(dp) :: integral

        integer :: n

        n = size(y)
        
        select case (n)
        case (0:1)
            integral = 0.0_dp
        case (2)
            integral = 0.5_dp*dx*(y(1) + y(2))
        case default
            integral = dx*(sum(y(2:n-1)) + 0.5_dp*(y(1) + y(n)))
        end select
    end function trapz_dx_dp
    

    module function trapz_x_sp(y, x) result(integral)
        real(sp), dimension(:), intent(in) :: y
        real(sp), dimension(:), intent(in) :: x
        real(sp) :: integral

        integer :: i
        integer :: n

        n = size(y)
        call check(size(x) == n, "trapz: Arguments `x` and `y` must be the same size.")

        select case (n)
        case (0:1)
            integral = 0.0_sp
        case (2)
            integral = 0.5_sp*(x(2) - x(1))*(y(1) + y(2))
        case default
            integral = 0.0_sp
            do i = 2, n
                integral = integral + (x(i) - x(i-1))*(y(i) + y(i-1))
            end do
            integral = 0.5_sp*integral
        end select
    end function trapz_x_sp


    module function trapz_x_dp(y, x) result(integral)
        real(dp), dimension(:), intent(in) :: y
        real(dp), dimension(:), intent(in) :: x
        real(dp) :: integral

        integer :: i
        integer :: n

        n = size(y)
        call check(size(x) == n, "trapz: Arguments `x` and `y` must be the same size.")

        select case (n)
        case (0:1)
            integral = 0.0_dp
        case (2)
            integral = 0.5_dp*(x(2) - x(1))*(y(1) + y(2))
        case default
            integral = 0.0_dp
            do i = 2, n
                integral = integral + (x(i) - x(i-1))*(y(i) + y(i-1))
            end do
            integral = 0.5_dp*integral
        end select
    end function trapz_x_dp


    pure module function trapz_weights_sp(x) result(w)
        real(sp), dimension(:), intent(in) :: x
        real(sp), dimension(size(x)) :: w

        integer :: i
        integer :: n

        n = size(x)

        select case (n)
        case (0)
            ! no action needed
        case (1)
            w(1) = 0.0_sp
        case (2)
            w = 0.5_sp*(x(2) - x(1))
        case default
            w(1) = 0.5_sp*(x(2) - x(1))
            w(n) = 0.5_sp*(x(n) - x(n-1))
            do i = 2, size(x)-1
                w(i) = 0.5_sp*(x(i+1) - x(i-1))
            end do
        end select
    end function trapz_weights_sp


    pure module function trapz_weights_dp(x) result(w)
        real(dp), dimension(:), intent(in) :: x
        real(dp), dimension(size(x)) :: w

        integer :: i
        integer :: n

        n = size(x)

        select case (n)
        case (0)
            ! no action needed
        case (1)
            w(1) = 0.0_dp
        case (2)
            w = 0.5_dp*(x(2) - x(1))
        case default
            w(1) = 0.5_dp*(x(2) - x(1))
            w(n) = 0.5_dp*(x(n) - x(n-1))
            do i = 2, size(x)-1
                w(i) = 0.5_dp*(x(i+1) - x(i-1))
            end do
        end select
    end function trapz_weights_dp

end submodule stdlib_quadrature_trapz
