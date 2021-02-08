submodule (stdlib_functions) stdlib_functions_legendre
    use stdlib_kinds, only: sp, dp, qp
    implicit none

contains

    ! derivatives of Legendre polynomials
    ! unspecified behaviour if N is negative
    pure elemental module function dlegendre_fp64(N,x) result(DL)
        integer, intent(in) :: N
        real(dp), intent(in) :: x
        real(dp) :: DL

        select case(N)
            case(0)
                DL = 0
            case(1)
                DL = 1
            case default
                block
                    real(dp) :: L_down1, L_down2, L
                    real(dp) :: DL_down1, DL_down2
                    integer :: i 

                    L_down1  = x
                    DL_down1 = 1

                    L_down2  = 1
                    DL_down2 = 0

                    do i = 2, N
                        L = (2*i-1)*x*L_down1/i - (i-1)*L_down2/i
                        DL = DL_down2 + (2*i-1)*L_down1
                        L_down2 = L_down1
                        L_down1 = L
                        DL_down2 = DL_down1
                        DL_down1 = DL
                    end do
                end block
        end select
    end function

    ! Legendre polynomials
    ! unspecified behaviour if N is negative
    pure elemental module function legendre_fp64(N,x) result(L)
        integer, intent(in) :: N
        real(dp), intent(in) :: x
        real(dp) :: L
        select case(N)
            case(0)
                L  = 1
            case(1)
                L  = x
            case default
                block
                    real(dp) :: L_down1, L_down2
                    integer :: i 

                    L_down1  = x
                    L_down2  = 1

                    do i = 2, N
                        L = (2*i-1)*x*L_down1/i - (i-1)*L_down2/i
                        L_down2 = L_down1
                        L_down1 = L
                    end do
                end block
        end select
    end function

end submodule


