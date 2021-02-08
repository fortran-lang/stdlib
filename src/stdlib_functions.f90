module stdlib_functions
    use stdlib_kinds, only: sp, dp, qp

    implicit none

    private

    public :: legendre 
    public :: dlegendre 


    interface legendre
        !! version: experimental
        !! 
        !! Legendre polynomial
        pure elemental module function legendre_fp64(N,x) result(L)
            integer, intent(in) :: N
            real(dp), intent(in) :: x
            real(dp) :: L
        end function
    end interface

    interface dlegendre
        !! version: experimental
        !! 
        !! First derivative Legendre polynomial
        pure elemental module function dlegendre_fp64(N,x) result(DL)
            integer, intent(in) :: N
            real(dp), intent(in) :: x
            real(dp) :: DL
        end function
    end interface

end module stdlib_functions
