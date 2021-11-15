
submodule (stdlib_quadrature) stdlib_quadrature_simps
    use stdlib_error, only: check
    implicit none

    ! internal use only
    interface simps38
        module procedure simps38_dx_sp
        module procedure simps38_x_sp
        module procedure simps38_dx_dp
        module procedure simps38_x_dp
    end interface simps38

    ! internal use only
    interface simps38_weights
        module procedure simps38_weights_sp
        module procedure simps38_weights_dp
    end interface simps38_weights

contains


    pure recursive module function simps_dx_sp(y, dx, even) result(integral)
        real(sp), dimension(:), intent(in) :: y
        real(sp), intent(in) :: dx
        integer, intent(in), optional :: even
        real(sp) :: integral

        integer :: n

        n = size(y)

        select case (n)
        case (0:1)
            integral = 0.0_sp
        case (2)
            integral = 0.5_sp*dx*(y(1) + y(2))
        case (3)
            integral = dx/3.0_sp*(y(1) + 4*y(2) + y(3))
        case (4)
            integral = simps38(y, dx)
        ! case (5) not needed; handled by default
        case (6) ! needs special handling because of averaged 3/8's rule case
            if (present(even)) then
                if (even < 0) then
                    ! 3/8 rule on left
                    integral =  simps38(y(1:4), dx) + simps(y(4:6), dx)
                    return
                else if (even > 0) then
                    ! 3/8 rule on right
                    integral = simps(y(1:3), dx) + simps38(y(3:6), dx)
                    return
                else
                    ! fall through
                end if
            end if
            ! either `even` not present or is zero
            ! equivalent to averaging left and right
            integral = dx/48.0_sp * (17*(y(1) + y(6)) + 59*(y(2) + y(5)) + 44*(y(3) + y(4)))
        case default
            if (mod(n, 2) == 1) then
                integral = dx/3.0_sp*(y(1) + 4*sum(y(2:n-1:2)) + 2*sum(y(3:n-2:2)) + y(n))
            else
                if (present(even)) then
                    if (even < 0) then
                        ! 3/8th rule on left
                        integral = simps38(y(1:4), dx) + simps(y(4:n), dx)
                        return
                    else if (even > 0) then
                        ! 3/8 rule on right
                        integral = simps(y(1:n-3), dx) + simps38(y(n-3:n), dx)
                        return
                    else
                        ! fall through
                    end if
                end if
                ! either `even` not present or is zero
                ! equivalent to averaging left and right
                integral = dx/48.0_sp * (17*(y(1) + y(n)) + 59*(y(2) + y(n-1)) &
                        + 43*(y(3) + y(n-2)) + 49*(y(4) + y(n-3)) + 48*sum(y(5:n-4)))
            end if
        end select
    end function simps_dx_sp


    pure recursive module function simps_dx_dp(y, dx, even) result(integral)
        real(dp), dimension(:), intent(in) :: y
        real(dp), intent(in) :: dx
        integer, intent(in), optional :: even
        real(dp) :: integral

        integer :: n

        n = size(y)

        select case (n)
        case (0:1)
            integral = 0.0_dp
        case (2)
            integral = 0.5_dp*dx*(y(1) + y(2))
        case (3)
            integral = dx/3.0_dp*(y(1) + 4*y(2) + y(3))
        case (4)
            integral = simps38(y, dx)
        ! case (5) not needed; handled by default
        case (6) ! needs special handling because of averaged 3/8's rule case
            if (present(even)) then
                if (even < 0) then
                    ! 3/8 rule on left
                    integral =  simps38(y(1:4), dx) + simps(y(4:6), dx)
                    return
                else if (even > 0) then
                    ! 3/8 rule on right
                    integral = simps(y(1:3), dx) + simps38(y(3:6), dx)
                    return
                else
                    ! fall through
                end if
            end if
            ! either `even` not present or is zero
            ! equivalent to averaging left and right
            integral = dx/48.0_dp * (17*(y(1) + y(6)) + 59*(y(2) + y(5)) + 44*(y(3) + y(4)))
        case default
            if (mod(n, 2) == 1) then
                integral = dx/3.0_dp*(y(1) + 4*sum(y(2:n-1:2)) + 2*sum(y(3:n-2:2)) + y(n))
            else
                if (present(even)) then
                    if (even < 0) then
                        ! 3/8th rule on left
                        integral = simps38(y(1:4), dx) + simps(y(4:n), dx)
                        return
                    else if (even > 0) then
                        ! 3/8 rule on right
                        integral = simps(y(1:n-3), dx) + simps38(y(n-3:n), dx)
                        return
                    else
                        ! fall through
                    end if
                end if
                ! either `even` not present or is zero
                ! equivalent to averaging left and right
                integral = dx/48.0_dp * (17*(y(1) + y(n)) + 59*(y(2) + y(n-1)) &
                        + 43*(y(3) + y(n-2)) + 49*(y(4) + y(n-3)) + 48*sum(y(5:n-4)))
            end if
        end select
    end function simps_dx_dp


    recursive module function simps_x_sp(y, x, even) result(integral)
        real(sp), dimension(:), intent(in) :: y
        real(sp), dimension(:), intent(in) :: x
        integer, intent(in), optional :: even
        real(sp) :: integral

        integer :: i
        integer :: n

        real(sp) :: h1, h2
        real(sp) :: a, b, c

        n = size(y)
        call check(size(x) == n, "simps: Arguments `x` and `y` must be the same size.")

        select case (n)
        case (0:1)
            integral = 0.0_sp
        case (2)
            integral = 0.5_sp*(x(2) - x(1))*(y(1) + y(2))
        case (3)
            h1 = x(2) - x(1)
            h2 = x(3) - x(2)
            a = (2*h1**2 + h1*h2 - h2**2)/(6*h1)
            b = (h1+h2)**3/(6*h1*h2)
            c = (2*h2**2 + h1*h2 - h1**2)/(6*h2)
            integral = a*y(1) + b*y(2) + c*y(3)
        case (4)
            integral = simps38(y, x)
        ! case (6) unneeded; handled by default
        case default
            if (mod(n, 2) == 1) then
                integral = 0.0_sp
                do i = 1, n-2, 2
                    h1 = x(i+1) - x(i)
                    h2 = x(i+2) - x(i+1)
                    a = (2*h1**2 + h1*h2 - h2**2)/(6*h1)
                    b = (h1+h2)**3/(6*h1*h2)
                    c = (2*h2**2 + h1*h2 - h1**2)/(6*h2)
                    integral = integral + a*y(i) + b*y(i+1) + c*y(i+2)
                end do
            else
                if (present(even)) then
                    if (even < 0) then
                        ! 3/8 rule on left
                        integral = simps38(y(1:4), x(1:4)) + simps(y(4:n), x(4:n))
                        return
                    else if (even > 0) then
                        ! 3/8 rule on right
                        integral = simps(y(1:n-3), x(1:n-3)) + simps38(y(n-3:n), x(n-3:n))
                        return
                    else
                        ! fall through
                    end if
                end if
                ! either `even` not present or is zero
                integral = 0.5_sp * ( simps38(y(1:4), x(1:4)) + simps(y(4:n), x(4:n)) &
                    + simps(y(1:n-3), x(1:n-3)) + simps38(y(n-3:n), x(n-3:n)) )
            end if
        end select
    end function simps_x_sp


    recursive module function simps_x_dp(y, x, even) result(integral)
        real(dp), dimension(:), intent(in) :: y
        real(dp), dimension(:), intent(in) :: x
        integer, intent(in), optional :: even
        real(dp) :: integral

        integer :: i
        integer :: n

        real(dp) :: h1, h2
        real(dp) :: a, b, c

        n = size(y)
        call check(size(x) == n, "simps: Arguments `x` and `y` must be the same size.")

        select case (n)
        case (0:1)
            integral = 0.0_dp
        case (2)
            integral = 0.5_dp*(x(2) - x(1))*(y(1) + y(2))
        case (3)
            h1 = x(2) - x(1)
            h2 = x(3) - x(2)
            a = (2*h1**2 + h1*h2 - h2**2)/(6*h1)
            b = (h1+h2)**3/(6*h1*h2)
            c = (2*h2**2 + h1*h2 - h1**2)/(6*h2)
            integral = a*y(1) + b*y(2) + c*y(3)
        case (4)
            integral = simps38(y, x)
        ! case (6) unneeded; handled by default
        case default
            if (mod(n, 2) == 1) then
                integral = 0.0_dp
                do i = 1, n-2, 2
                    h1 = x(i+1) - x(i)
                    h2 = x(i+2) - x(i+1)
                    a = (2*h1**2 + h1*h2 - h2**2)/(6*h1)
                    b = (h1+h2)**3/(6*h1*h2)
                    c = (2*h2**2 + h1*h2 - h1**2)/(6*h2)
                    integral = integral + a*y(i) + b*y(i+1) + c*y(i+2)
                end do
            else
                if (present(even)) then
                    if (even < 0) then
                        ! 3/8 rule on left
                        integral = simps38(y(1:4), x(1:4)) + simps(y(4:n), x(4:n))
                        return
                    else if (even > 0) then
                        ! 3/8 rule on right
                        integral = simps(y(1:n-3), x(1:n-3)) + simps38(y(n-3:n), x(n-3:n))
                        return
                    else
                        ! fall through
                    end if
                end if
                ! either `even` not present or is zero
                integral = 0.5_dp * ( simps38(y(1:4), x(1:4)) + simps(y(4:n), x(4:n)) &
                    + simps(y(1:n-3), x(1:n-3)) + simps38(y(n-3:n), x(n-3:n)) )
            end if
        end select
    end function simps_x_dp


    pure recursive module function simps_weights_sp(x, even) result(w)
        real(sp), dimension(:), intent(in) :: x
        integer, intent(in), optional :: even
        real(sp), dimension(size(x)) :: w

        integer :: i, n
        real(sp) :: h1, h2

        n = size(x)

        select case (n)
        case (0)
            ! no action needed
        case (1)
            w(1) = 0.0_sp
        case (2)
            w = 0.5_sp*(x(2) - x(1))
        case (3)
            h1 = x(2) - x(1)
            h2 = x(3) - x(2)
            w(1) = (2*h1**2 + h1*h2 - h2**2)/(6*h1)
            w(2) = (h1+h2)**3/(6*h1*h2)
            w(3) = (2*h2**2 + h1*h2 - h1**2)/(6*h2)
        case (4)
            w = simps38_weights(x)
        case default
            if (mod(n, 2) == 1) then
                w = 0.0_sp
                do i = 1, n-2, 2
                    h1 = x(i+1) - x(i)
                    h2 = x(i+2) - x(i+1)
                    w(i) = w(i) + (2*h1**2 + h1*h2 - h2**2)/(6*h1)
                    w(i+1) = w(i+1) + (h1+h2)**3/(6*h1*h2)
                    w(i+2) = w(i+2) + (2*h2**2 + h1*h2 - h1**2)/(6*h2)
                end do
            else
                if (present(even)) then
                    if (even < 0) then
                        ! 3/8 rule on left
                        w = 0.0_sp
                        w(1:4) = simps38_weights(x(1:4))
                        w(4:n) = w(4:n) + simps_weights(x(4:n)) ! position 4 needs both rules
                        return
                    else if (even > 0) then
                        ! 3/8 rule on right
                        w = 0.0_sp
                        w(1:n-3) = simps_weights(x(1:n-3))
                        w(n-3:n) = w(n-3:n) + simps38_weights(x(n-3:n)) ! position n-3 needs both rules
                        return
                    else
                        ! fall through
                    end if
                end if
                ! either `even` not present or is zero
                w = 0.0_sp
                ! 3/8 rule on left
                w(1:4) = simps38_weights(x(1:4))
                w(4:n) = w(4:n) + simps_weights(x(4:n))
                ! 3/8 rule on right
                w(1:n-3) = w(1:n-3) + simps_weights(x(1:n-3))
                w(n-3:n) = w(n-3:n) + simps38_weights(x(n-3:n))
                ! average
                w = 0.5_sp * w
            end if
        end select
    end function simps_weights_sp


    pure recursive module function simps_weights_dp(x, even) result(w)
        real(dp), dimension(:), intent(in) :: x
        integer, intent(in), optional :: even
        real(dp), dimension(size(x)) :: w

        integer :: i, n
        real(dp) :: h1, h2

        n = size(x)

        select case (n)
        case (0)
            ! no action needed
        case (1)
            w(1) = 0.0_dp
        case (2)
            w = 0.5_dp*(x(2) - x(1))
        case (3)
            h1 = x(2) - x(1)
            h2 = x(3) - x(2)
            w(1) = (2*h1**2 + h1*h2 - h2**2)/(6*h1)
            w(2) = (h1+h2)**3/(6*h1*h2)
            w(3) = (2*h2**2 + h1*h2 - h1**2)/(6*h2)
        case (4)
            w = simps38_weights(x)
        case default
            if (mod(n, 2) == 1) then
                w = 0.0_dp
                do i = 1, n-2, 2
                    h1 = x(i+1) - x(i)
                    h2 = x(i+2) - x(i+1)
                    w(i) = w(i) + (2*h1**2 + h1*h2 - h2**2)/(6*h1)
                    w(i+1) = w(i+1) + (h1+h2)**3/(6*h1*h2)
                    w(i+2) = w(i+2) + (2*h2**2 + h1*h2 - h1**2)/(6*h2)
                end do
            else
                if (present(even)) then
                    if (even < 0) then
                        ! 3/8 rule on left
                        w = 0.0_dp
                        w(1:4) = simps38_weights(x(1:4))
                        w(4:n) = w(4:n) + simps_weights(x(4:n)) ! position 4 needs both rules
                        return
                    else if (even > 0) then
                        ! 3/8 rule on right
                        w = 0.0_dp
                        w(1:n-3) = simps_weights(x(1:n-3))
                        w(n-3:n) = w(n-3:n) + simps38_weights(x(n-3:n)) ! position n-3 needs both rules
                        return
                    else
                        ! fall through
                    end if
                end if
                ! either `even` not present or is zero
                w = 0.0_dp
                ! 3/8 rule on left
                w(1:4) = simps38_weights(x(1:4))
                w(4:n) = w(4:n) + simps_weights(x(4:n))
                ! 3/8 rule on right
                w(1:n-3) = w(1:n-3) + simps_weights(x(1:n-3))
                w(n-3:n) = w(n-3:n) + simps38_weights(x(n-3:n))
                ! average
                w = 0.5_dp * w
            end if
        end select
    end function simps_weights_dp


    pure function simps38_dx_sp(y, dx) result (integral)
        real(sp), dimension(4), intent(in) :: y
        real(sp), intent(in) :: dx
        real(sp) :: integral

        integral = 3.0_sp*dx/8.0_sp * (y(1) + y(4) + 3*(y(2) + y(3)))
    end function simps38_dx_sp


    pure function simps38_dx_dp(y, dx) result (integral)
        real(dp), dimension(4), intent(in) :: y
        real(dp), intent(in) :: dx
        real(dp) :: integral

        integral = 3.0_dp*dx/8.0_dp * (y(1) + y(4) + 3*(y(2) + y(3)))
    end function simps38_dx_dp


    pure function simps38_x_sp(y, x) result(integral)
        real(sp), dimension(4), intent(in) :: y
        real(sp), dimension(4), intent(in) :: x
        real(sp) :: integral

        real(sp) :: h1, h2, h3
        real(sp) :: a, b, c, d

        h1 = x(2) - x(1)
        h2 = x(3) - x(2)
        h3 = x(4) - x(3)

        a = (h1+h2+h3)*(3*h1**2 + 2*h1*h2 - 2*h1*h3 - h2**2 + h3**2)/(12*h1*(h1+h2))
        b = (h1+h2-h3)*(h1+h2+h3)**3/(12*h1*h2*(h2+h3))
        c = (h2+h3-h1)*(h1+h2+h3)**3/(12*h2*h3*(h1+h2))
        d = (h1+h2+h3)*(3*h3**2 + 2*h2*h3 - 2*h1*h3 - h2**2 + h1**2)/(12*h3*(h2+h3))

        integral = a*y(1) + b*y(2) + c*y(3) + d*y(4)
    end function simps38_x_sp


    pure function simps38_x_dp(y, x) result(integral)
        real(dp), dimension(4), intent(in) :: y
        real(dp), dimension(4), intent(in) :: x
        real(dp) :: integral

        real(dp) :: h1, h2, h3
        real(dp) :: a, b, c, d

        h1 = x(2) - x(1)
        h2 = x(3) - x(2)
        h3 = x(4) - x(3)

        a = (h1+h2+h3)*(3*h1**2 + 2*h1*h2 - 2*h1*h3 - h2**2 + h3**2)/(12*h1*(h1+h2))
        b = (h1+h2-h3)*(h1+h2+h3)**3/(12*h1*h2*(h2+h3))
        c = (h2+h3-h1)*(h1+h2+h3)**3/(12*h2*h3*(h1+h2))
        d = (h1+h2+h3)*(3*h3**2 + 2*h2*h3 - 2*h1*h3 - h2**2 + h1**2)/(12*h3*(h2+h3))

        integral = a*y(1) + b*y(2) + c*y(3) + d*y(4)
    end function simps38_x_dp


    pure function simps38_weights_sp(x) result(w)
        real(sp), intent(in) :: x(4)
        real(sp) :: w(size(x))

        real(sp) :: h1, h2, h3

        h1 = x(2) - x(1)
        h2 = x(3) - x(2)
        h3 = x(4) - x(3)

        w(1) = (h1+h2+h3)*(3*h1**2 + 2*h1*h2 - 2*h1*h3 - h2**2 + h3**2)/(12*h1*(h1+h2))
        w(2) = (h1+h2-h3)*(h1+h2+h3)**3/(12*h1*h2*(h2+h3))
        w(3) = (h2+h3-h1)*(h1+h2+h3)**3/(12*h2*h3*(h1+h2))
        w(4) = (h1+h2+h3)*(3*h3**2 + 2*h2*h3 - 2*h1*h3 - h2**2 + h1**2)/(12*h3*(h2+h3))
    end function simps38_weights_sp


    pure function simps38_weights_dp(x) result(w)
        real(dp), intent(in) :: x(4)
        real(dp) :: w(size(x))

        real(dp) :: h1, h2, h3

        h1 = x(2) - x(1)
        h2 = x(3) - x(2)
        h3 = x(4) - x(3)

        w(1) = (h1+h2+h3)*(3*h1**2 + 2*h1*h2 - 2*h1*h3 - h2**2 + h3**2)/(12*h1*(h1+h2))
        w(2) = (h1+h2-h3)*(h1+h2+h3)**3/(12*h1*h2*(h2+h3))
        w(3) = (h2+h3-h1)*(h1+h2+h3)**3/(12*h2*h3*(h1+h2))
        w(4) = (h1+h2+h3)*(3*h3**2 + 2*h2*h3 - 2*h1*h3 - h2**2 + h1**2)/(12*h3*(h2+h3))
    end function simps38_weights_dp


end submodule stdlib_quadrature_simps
