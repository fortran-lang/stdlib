submodule (stdlib_quadrature) stdlib_quadrature_gauss
    use stdlib_kinds, only: sp, dp, qp
    use stdlib_functions, only: legendre, dlegendre
    implicit none

    real(dp), parameter :: PI = acos(-1._dp)
    real(dp), parameter :: tolerance = 4._dp * epsilon(1._dp)
    integer, parameter :: newton_iters = 100

contains

    pure module subroutine gauss_legendre_fp64 (x, w, interval)
        real(dp), intent(out) :: x(:), w(:)
        real(dp), intent(in), optional :: interval(2)

        associate (N => size(x)-1 )
        select case (N)
            case (0)
                x = 0._dp
                w = 2._dp
            case (1)
                x = [-sqrt(1._dp/3._dp), sqrt(1._dp/3._dp)]
                w = [1._dp, 1._dp]
            case default
                block
                integer :: i,j
                real(dp) :: leg, dleg, delta

                do i = 0, int(floor((N+1)/2._dp)-1)
                    x(i+1) = -cos((2*i+1)/(2._dp*N+2._dp) * PI)
                    do j = 0, newton_iters-1
                        leg  = legendre(N+1,x(i+1))
                        dleg = dlegendre(N+1,x(i+1))
                        delta = -leg/dleg
                        x(i+1) = x(i+1) + delta
                        if ( abs(delta) <= tolerance * abs(x(i+1)) )  exit
                    end do
                    x(N-i+1) = -x(i+1)

                    dleg = dlegendre(N+1,x(i+1))
                    w(i+1)   = 2._dp/((1-x(i+1)**2)*dleg**2) 
                    w(N-i+1) = w(i+1)
                end do

                if (mod(N,2) == 0) then
                    x(N/2+1) = 0.0

                    dleg = dlegendre(N+1, 0.0_dp)
                    w(N/2+1) = 2._dp/(dleg**2) 
                end if
                end block
        end select
        end associate

        if (present(interval)) then
            associate ( a => interval(1) , b => interval(2) )
            x = 0.5*(b-a)*x+0.5*(b+a)
            w = 0.5*(b-a)*w
            end associate
        end if
    end subroutine

    pure module subroutine gauss_legendre_lobatto_fp64 (x, w, interval)
        real(dp), intent(out) :: x(:), w(:)
        real(dp), intent(in), optional :: interval(2)

        associate (N => size(x)-1)
        select case (N)
            case (1)
                x = [-1._dp, 1._dp]
                w = [ 1._dp, 1._dp]
            case default
                block
                integer :: i,j
                real(dp) :: leg, dleg, delta

                x(1)   = -1._dp
                x(N+1) =  1._dp
                w(1)   =  2._dp/(N*(N+1._dp))
                w(N+1) =  2._dp/(N*(N+1._dp))

                do i = 1, int(floor((N+1)/2._dp)-1)
                    x(i+1) = -cos( (i+0.25_dp)*PI/N  - 3/(8*N*PI*(i+0.25_dp)))
                    do j = 0, newton_iters-1
                        leg  = legendre(N+1,x(i+1)) - legendre(N-1,x(i+1))
                        dleg = dlegendre(N+1,x(i+1)) - dlegendre(N-1,x(i+1))
                        delta = -leg/dleg
                        x(i+1) = x(i+1) + delta
                        if ( abs(delta) <= tolerance * abs(x(i+1)) )  exit
                    end do
                    x(N-i+1) = -x(i+1)

                    leg = legendre(N, x(i+1))
                    w(i+1)   = 2._dp/(N*(N+1._dp)*leg**2) 
                    w(N-i+1) = w(i+1)
                end do

                if (mod(N,2) == 0) then
                    x(N/2+1) = 0.0

                    leg = legendre(N, 0.0_dp)
                    w(N/2+1)   = 2._dp/(N*(N+1._dp)*leg**2) 
                end if
                end block
        end select
        end associate
        
        if (present(interval)) then
            associate ( a => interval(1) , b => interval(2) )
            x = 0.5*(b-a)*x+0.5*(b+a)
            w = 0.5*(b-a)*w
            end associate
        end if
    end subroutine
end submodule    
