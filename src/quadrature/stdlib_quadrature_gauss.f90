submodule (stdlib_quadrature) stdlib_quadrature_gauss
    use stdlib_specialfunctions, only: legendre, dlegendre
    implicit none

    real(dp), parameter :: pi = acos(-1._dp)
    real(dp), parameter :: tolerance = 4._dp * epsilon(1._dp)
    integer, parameter :: newton_iters = 100

contains

    pure module subroutine gauss_legendre_fp64 (x, w, interval)
        real(dp), intent(out) :: x(:), w(:)
        real(dp), intent(in), optional :: interval(2)

        associate (n => size(x)-1 )
        select case (n)
            case (0)
                x = 0
                w = 2
            case (1)
                x(1) = -sqrt(1._dp/3._dp)
                x(2) = -x(1)
                w = 1
            case default
                block
                integer :: i,j
                real(dp) :: leg, dleg, delta

                do i = 0, (n+1)/2 - 1
                    ! use Gauss-Chebyshev points as an initial guess
                    x(i+1) = -cos((2*i+1)/(2._dp*n+2._dp) * pi)
                    do j = 1, newton_iters
                        leg  = legendre(n+1,x(i+1))
                        dleg = dlegendre(n+1,x(i+1))
                        delta = -leg/dleg
                        x(i+1) = x(i+1) + delta
                        if ( abs(delta) <= tolerance * abs(x(i+1)) )  exit
                    end do
                    x(n-i+1) = -x(i+1)

                    dleg = dlegendre(n+1,x(i+1))
                    w(i+1)   = 2._dp/((1-x(i+1)**2)*dleg**2) 
                    w(n-i+1) = w(i+1)
                end do

                if (mod(n,2) == 0) then
                    x(n/2+1) = 0

                    dleg = dlegendre(n+1, 0.0_dp)
                    w(n/2+1) = 2._dp/(dleg**2) 
                end if
                end block
        end select
        end associate

        if (present(interval)) then
            associate ( a => interval(1) , b => interval(2) )
            x = 0.5_dp*(b-a)*x+0.5_dp*(b+a)
            w = 0.5_dp*(b-a)*w
            end associate
        end if
    end subroutine

    pure module subroutine gauss_legendre_lobatto_fp64 (x, w, interval)
        real(dp), intent(out) :: x(:), w(:)
        real(dp), intent(in), optional :: interval(2)

        associate (n => size(x)-1)
        select case (n)
            case (1)
                x(1) = -1
                x(2) =  1
                w = 1
            case default
                block
                integer :: i,j
                real(dp) :: leg, dleg, delta

                x(1)   = -1._dp
                x(n+1) =  1._dp
                w(1)   =  2._dp/(n*(n+1._dp))
                w(n+1) =  2._dp/(n*(n+1._dp))

                do i = 1, (n+1)/2 - 1
                    ! initial guess from an approximate form given by SV Parter (1999)
                    x(i+1) = -cos( (i+0.25_dp)*pi/n  - 3/(8*n*pi*(i+0.25_dp)))
                    do j = 1, newton_iters
                        leg  = legendre(n+1,x(i+1)) - legendre(n-1,x(i+1))
                        dleg = dlegendre(n+1,x(i+1)) - dlegendre(n-1,x(i+1))
                        delta = -leg/dleg
                        x(i+1) = x(i+1) + delta
                        if ( abs(delta) <= tolerance * abs(x(i+1)) )  exit
                    end do
                    x(n-i+1) = -x(i+1)

                    leg = legendre(n, x(i+1))
                    w(i+1)   = 2._dp/(n*(n+1._dp)*leg**2) 
                    w(n-i+1) = w(i+1)
                end do

                if (mod(n,2) == 0) then
                    x(n/2+1) = 0

                    leg = legendre(n, 0.0_dp)
                    w(n/2+1)   = 2._dp/(n*(n+1._dp)*leg**2) 
                end if
                end block
        end select
        end associate
        
        if (present(interval)) then
            associate ( a => interval(1) , b => interval(2) )
            x = 0.5_dp*(b-a)*x+0.5_dp*(b+a)
            x(1)       = interval(1)
            x(size(x)) = interval(2)
            w = 0.5_dp*(b-a)*w
            end associate
        end if
    end subroutine
end submodule    
