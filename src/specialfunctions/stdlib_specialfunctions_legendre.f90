submodule (stdlib_specialfunctions) stdlib_specialfunctions_legendre
    implicit none

contains

    ! derivatives of legegendre polynomials
    ! unspecified behaviour if n is negative
    pure elemental module function dlegendre_fp64(n,x) result(dleg)
        integer, intent(in) :: n
        real(dp), intent(in) :: x
        real(dp) :: dleg

        select case(n)
            case(0)
                dleg = 0
            case(1)
                dleg = 1
            case default
                block
                    real(dp) :: leg_down1, leg_down2, leg
                    real(dp) :: dleg_down1, dleg_down2
                    integer :: i 

                    leg_down1  = x
                    dleg_down1 = 1

                    leg_down2  = 1
                    dleg_down2 = 0

                    do i = 2, n
                        leg = (2*i-1)*x*leg_down1/i - (i-1)*leg_down2/i
                        dleg = dleg_down2 + (2*i-1)*leg_down1
                        leg_down2 = leg_down1
                        leg_down1 = leg
                        dleg_down2 = dleg_down1
                        dleg_down1 = dleg
                    end do
                end block
        end select
    end function

    ! legegendre polynomials
    ! unspecified behaviour if n is negative
    pure elemental module function legendre_fp64(n,x) result(leg)
        integer, intent(in) :: n
        real(dp), intent(in) :: x
        real(dp) :: leg
        select case(n)
            case(0)
                leg  = 1
            case(1)
                leg  = x
            case default
                block
                    real(dp) :: leg_down1, leg_down2
                    integer :: i 

                    leg_down1  = x
                    leg_down2  = 1

                    do i = 2, n
                        leg = (2*i-1)*x*leg_down1/i - (i-1)*leg_down2/i
                        leg_down2 = leg_down1
                        leg_down1 = leg
                    end do
                end block
        end select
    end function

end submodule


