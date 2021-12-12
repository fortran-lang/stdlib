module stdlib_specialfunctions
    use stdlib_kinds, only: sp, dp, xdp, qp

    implicit none

    private

    public :: legendre 
    public :: dlegendre 


    interface legendre
        !! version: experimental
        !! 
        !! Legendre polynomial
        pure elemental module function legendre_fp64(n,x) result(leg)
            integer, intent(in) :: n
            real(dp), intent(in) :: x
            real(dp) :: leg
        end function
    end interface

    interface dlegendre
        !! version: experimental
        !! 
        !! First derivative Legendre polynomial
        pure elemental module function dlegendre_fp64(n,x) result(dleg)
            integer, intent(in) :: n
            real(dp), intent(in) :: x
            real(dp) :: dleg
        end function
    end interface

end module stdlib_specialfunctions
