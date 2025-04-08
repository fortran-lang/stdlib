module stdlib_specialfunctions_gamma
    use ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use stdlib_kinds, only :  sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_error, only : error_stop

    implicit none
    private

    integer(int8), parameter  :: max_fact_int8 = 6_int8
    integer(int16), parameter :: max_fact_int16 = 8_int16
    integer(int32), parameter :: max_fact_int32 = 13_int32
    integer(int64), parameter :: max_fact_int64 = 21_int64

    real(sp), parameter :: tol_sp = epsilon(1.0_sp)
    real(dp), parameter :: tol_dp = epsilon(1.0_dp)
    
    public :: gamma, log_gamma, log_factorial
    public :: lower_incomplete_gamma, log_lower_incomplete_gamma
    public :: upper_incomplete_gamma, log_upper_incomplete_gamma
    public :: regularized_gamma_p, regularized_gamma_q



    interface gamma
    !! Gamma function for integer and complex numbers
    !!
        module procedure gamma_iint8
        module procedure gamma_iint16
        module procedure gamma_iint32
        module procedure gamma_iint64
        module procedure gamma_csp
    end interface gamma



    interface log_gamma
    !! Logarithm of gamma function
    !!
        module procedure l_gamma_iint8
        module procedure l_gamma_iint16
        module procedure l_gamma_iint32
        module procedure l_gamma_iint64
        module procedure l_gamma_csp
    end interface log_gamma



    interface log_factorial
    !! Logarithm of factorial n!, integer variable
    !!
        module procedure l_factorial_iint8
        module procedure l_factorial_iint16
        module procedure l_factorial_iint32
        module procedure l_factorial_iint64
    end interface log_factorial



    interface lower_incomplete_gamma
    !! Lower incomplete gamma function
    !!
        module procedure ingamma_low_iint8sp
        module procedure ingamma_low_iint16sp
        module procedure ingamma_low_iint32sp
        module procedure ingamma_low_iint64sp

        module procedure ingamma_low_rsp
    end interface lower_incomplete_gamma



    interface log_lower_incomplete_gamma
    !! Logarithm of lower incomplete gamma function
    !!
        module procedure l_ingamma_low_iint8sp
        module procedure l_ingamma_low_iint16sp
        module procedure l_ingamma_low_iint32sp
        module procedure l_ingamma_low_iint64sp

        module procedure l_ingamma_low_rsp
    end interface log_lower_incomplete_gamma



    interface upper_incomplete_gamma
    !! Upper incomplete gamma function
    !!
        module procedure ingamma_up_iint8sp
        module procedure ingamma_up_iint16sp
        module procedure ingamma_up_iint32sp
        module procedure ingamma_up_iint64sp

        module procedure ingamma_up_rsp
    end interface upper_incomplete_gamma



    interface log_upper_incomplete_gamma
    !! Logarithm of upper incomplete gamma function
    !!
        module procedure l_ingamma_up_iint8sp
        module procedure l_ingamma_up_iint16sp
        module procedure l_ingamma_up_iint32sp
        module procedure l_ingamma_up_iint64sp

        module procedure l_ingamma_up_rsp
    end interface log_upper_incomplete_gamma



    interface regularized_gamma_p
    !! Regularized (normalized) lower incomplete gamma function, P
    !!
        module procedure regamma_p_iint8sp
        module procedure regamma_p_iint16sp
        module procedure regamma_p_iint32sp
        module procedure regamma_p_iint64sp

        module procedure regamma_p_rsp
    end interface regularized_gamma_p



    interface regularized_gamma_q
    !! Regularized (normalized) upper incomplete gamma function, Q
    !!
        module procedure regamma_q_iint8sp
        module procedure regamma_q_iint16sp
        module procedure regamma_q_iint32sp
        module procedure regamma_q_iint64sp

        module procedure regamma_q_rsp
    end interface regularized_gamma_q



    interface gpx
    ! Incomplete gamma G function.
    ! Internal use only
    !
        module procedure gpx_rsp         !for real p and x

        module procedure gpx_iint8sp   !for integer p and real x
        module procedure gpx_iint16sp   !for integer p and real x
        module procedure gpx_iint32sp   !for integer p and real x
        module procedure gpx_iint64sp   !for integer p and real x
    end interface gpx



    interface l_gamma
    ! Logarithm of gamma with integer argument for designated output kind.
    ! Internal use only
    !
          module procedure l_gamma_iint8sp
          module procedure l_gamma_iint16sp
          module procedure l_gamma_iint32sp
          module procedure l_gamma_iint64sp
    end interface l_gamma





contains

    impure elemental function gamma_iint8(z) result(res)
        integer(int8), intent(in) :: z
        integer(int8) :: res, i
        integer(int8), parameter :: zero = 0_int8, one = 1_int8

        if(z <= zero) call error_stop("Error(gamma): Gamma function argument"  &
            //" must be positive integer.")

        if(z > max_fact_int8) call error_stop("Error(gamma): Gamma function" &
            //" integer argument is greater than the upper limit from which an"&
            //" integer overflow will be generated. Suggest switch to high "   &
            //" precision or convert to real data type")

        res = one

        do i = one, z - one

            res = res * i

        end do

    end function gamma_iint8

    impure elemental function gamma_iint16(z) result(res)
        integer(int16), intent(in) :: z
        integer(int16) :: res, i
        integer(int16), parameter :: zero = 0_int16, one = 1_int16

        if(z <= zero) call error_stop("Error(gamma): Gamma function argument"  &
            //" must be positive integer.")

        if(z > max_fact_int16) call error_stop("Error(gamma): Gamma function" &
            //" integer argument is greater than the upper limit from which an"&
            //" integer overflow will be generated. Suggest switch to high "   &
            //" precision or convert to real data type")

        res = one

        do i = one, z - one

            res = res * i

        end do

    end function gamma_iint16

    impure elemental function gamma_iint32(z) result(res)
        integer(int32), intent(in) :: z
        integer(int32) :: res, i
        integer(int32), parameter :: zero = 0_int32, one = 1_int32

        if(z <= zero) call error_stop("Error(gamma): Gamma function argument"  &
            //" must be positive integer.")

        if(z > max_fact_int32) call error_stop("Error(gamma): Gamma function" &
            //" integer argument is greater than the upper limit from which an"&
            //" integer overflow will be generated. Suggest switch to high "   &
            //" precision or convert to real data type")

        res = one

        do i = one, z - one

            res = res * i

        end do

    end function gamma_iint32

    impure elemental function gamma_iint64(z) result(res)
        integer(int64), intent(in) :: z
        integer(int64) :: res, i
        integer(int64), parameter :: zero = 0_int64, one = 1_int64

        if(z <= zero) call error_stop("Error(gamma): Gamma function argument"  &
            //" must be positive integer.")

        if(z > max_fact_int64) call error_stop("Error(gamma): Gamma function" &
            //" integer argument is greater than the upper limit from which an"&
            //" integer overflow will be generated. Suggest switch to high "   &
            //" precision or convert to real data type")

        res = one

        do i = one, z - one

            res = res * i

        end do

    end function gamma_iint64





    impure elemental function gamma_csp(z) result(res)
        complex(sp), intent(in) :: z
        complex(sp) :: res
        integer :: i

        real(sp), parameter :: zero_k1 = 0.0_sp
        real(dp), parameter :: half = 0.5_dp,             &
                             one = 1.0_dp, pi = acos(- one), sqpi = sqrt(pi)
        complex(dp) :: y, x, sum


        integer, parameter :: n = 10
        real(dp), parameter :: r = 10.900511_dp
        real(dp), parameter :: d(0 : n) = [2.48574089138753566e-5_dp,        &
                                             1.05142378581721974_dp,       &
                                            -3.45687097222016235_dp,       &
                                             4.51227709466894824_dp,       &
                                            -2.98285225323576656_dp,       &
                                             1.05639711577126713_dp,       &
                                         -1.95428773191645870e-1_dp,       &
                                          1.70970543404441224e-2_dp,       &
                                         -5.71926117404305781e-4_dp,       &
                                          4.63399473359905637e-6_dp,       &
                                         -2.71994908488607704e-9_dp]
        ! parameters from above referenced source.


        if(abs(z % im) < tol_sp) then

            res = cmplx(gamma(z % re), kind = sp)
            return

        end if

        if(z % re < zero_k1) then

            x = cmplx(abs(z % re), - z % im, kind = sp)
            y = x - one

        else

            y = z - one

        end if

        sum = cmplx(d(0), kind = dp)

        do i = 1, n

            sum = sum + d(i) / (y + i)

        end do

        y = exp((y + half) * log(y + half + r) - y) * sum

        y = y * 2 / sqpi                         !Re(z) > 0 return

        if(z % re < zero_k1 ) then

            y = - pi / (sin(pi * x) * x * y)     !Re(z) < 0 return

        end if

        res = y
    end function gamma_csp


    impure elemental function l_gamma_iint8(z) result(res)
    !
    ! Logarithm of gamma function for integer input
    !
        integer(int8), intent(in) :: z
        real(dp) :: res
        integer(int8) :: i
        integer(int8), parameter :: zero = 0_int8, one = 1_int8, two = 2_int8

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = 0.0

        case (two :)

            res = 0.0

            do i = one, z - one

               res = res + log(real(i,dp))

            end do

        end select
    end function l_gamma_iint8

    impure elemental function l_gamma_iint16(z) result(res)
    !
    ! Logarithm of gamma function for integer input
    !
        integer(int16), intent(in) :: z
        real(dp) :: res
        integer(int16) :: i
        integer(int16), parameter :: zero = 0_int16, one = 1_int16, two = 2_int16

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = 0.0

        case (two :)

            res = 0.0

            do i = one, z - one

               res = res + log(real(i,dp))

            end do

        end select
    end function l_gamma_iint16

    impure elemental function l_gamma_iint32(z) result(res)
    !
    ! Logarithm of gamma function for integer input
    !
        integer(int32), intent(in) :: z
        real(dp) :: res
        integer(int32) :: i
        integer(int32), parameter :: zero = 0_int32, one = 1_int32, two = 2_int32

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = 0.0

        case (two :)

            res = 0.0

            do i = one, z - one

               res = res + log(real(i,dp))

            end do

        end select
    end function l_gamma_iint32

    impure elemental function l_gamma_iint64(z) result(res)
    !
    ! Logarithm of gamma function for integer input
    !
        integer(int64), intent(in) :: z
        real(dp) :: res
        integer(int64) :: i
        integer(int64), parameter :: zero = 0_int64, one = 1_int64, two = 2_int64

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = 0.0

        case (two :)

            res = 0.0

            do i = one, z - one

               res = res + log(real(i,dp))

            end do

        end select
    end function l_gamma_iint64






    impure elemental function l_gamma_iint8sp(z, x) result(res)
    !
    ! Logarithm of gamma function for integer input with defined precision output
    !
        integer(int8), intent(in) :: z
        real(sp), intent(in) :: x
        real(sp) :: res
        integer(int8) :: i
        integer(int8), parameter :: zero = 0_int8, one = 1_int8, two = 2_int8
        real(sp), parameter :: zero_k2 = 0.0_sp

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = zero_k2

        case (two :)

            res = zero_k2

            do i = one, z - one

               res = res + log(real(i, sp))

            end do

        end select
    end function l_gamma_iint8sp


    impure elemental function l_gamma_iint16sp(z, x) result(res)
    !
    ! Logarithm of gamma function for integer input with defined precision output
    !
        integer(int16), intent(in) :: z
        real(sp), intent(in) :: x
        real(sp) :: res
        integer(int16) :: i
        integer(int16), parameter :: zero = 0_int16, one = 1_int16, two = 2_int16
        real(sp), parameter :: zero_k2 = 0.0_sp

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = zero_k2

        case (two :)

            res = zero_k2

            do i = one, z - one

               res = res + log(real(i, sp))

            end do

        end select
    end function l_gamma_iint16sp


    impure elemental function l_gamma_iint32sp(z, x) result(res)
    !
    ! Logarithm of gamma function for integer input with defined precision output
    !
        integer(int32), intent(in) :: z
        real(sp), intent(in) :: x
        real(sp) :: res
        integer(int32) :: i
        integer(int32), parameter :: zero = 0_int32, one = 1_int32, two = 2_int32
        real(sp), parameter :: zero_k2 = 0.0_sp

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = zero_k2

        case (two :)

            res = zero_k2

            do i = one, z - one

               res = res + log(real(i, sp))

            end do

        end select
    end function l_gamma_iint32sp


    impure elemental function l_gamma_iint64sp(z, x) result(res)
    !
    ! Logarithm of gamma function for integer input with defined precision output
    !
        integer(int64), intent(in) :: z
        real(sp), intent(in) :: x
        real(sp) :: res
        integer(int64) :: i
        integer(int64), parameter :: zero = 0_int64, one = 1_int64, two = 2_int64
        real(sp), parameter :: zero_k2 = 0.0_sp

        if(z <= zero) call error_stop("Error(log_gamma): Gamma function"       &
            //" argument must be positive integer.")

        select case(z)

        case (one)

            res = zero_k2

        case (two :)

            res = zero_k2

            do i = one, z - one

               res = res + log(real(i, sp))

            end do

        end select
    end function l_gamma_iint64sp





    impure elemental function l_gamma_csp(z) result (res)
    !
    ! log_gamma function for any complex number, excluding negative whole number
    ! "Computation of special functions", Shanjie Zhang & Jianmin Jin, 1996, p.48
    ! "Computing the principal branch of log-gamma", D.E.G. Hare,
    ! J. of Algorithms, 25(2), 1997 p. 221–236
    !
    ! Fortran 90 program by Jim-215-Fisher
    !
        complex(sp), intent(in) :: z
        complex(sp) :: res, z1, z2
        real(sp) :: d
        integer :: m, i
        complex(dp) :: zr, zr2, sum, s
        real(sp), parameter :: z_limit = 10.0_sp, zero_k1 = 0.0_sp
        integer, parameter :: n = 20
        real(dp), parameter :: zero = 0.0_dp, one = 1.0_dp,              &
                             pi = acos(-one), ln2pi = log(2 * pi)
        real(dp), parameter :: a(n) = [                                          &
                           .8333333333333333333333333333333333333333E-1_dp,&
                          -.2777777777777777777777777777777777777778E-2_dp,&
                           .7936507936507936507936507936507936507937E-3_dp,&
                          -.5952380952380952380952380952380952380952E-3_dp,&
                           .8417508417508417508417508417508417508418E-3_dp,&
                          -.1917526917526917526917526917526917526918E-2_dp,&
                           .6410256410256410256410256410256410256410E-2_dp,&
                          -.2955065359477124183006535947712418300654E-1_dp,&
                           .1796443723688305731649384900158893966944E+0_dp,&
                          -.1392432216905901116427432216905901116427E+1_dp,&
                           .1340286404416839199447895100069013112491E+2_dp,&
                          -.1568482846260020173063651324520889738281E+3_dp,&
                           .2193103333333333333333333333333333333333E+4_dp,&
                          -.3610877125372498935717326521924223073648E+5_dp,&
                           .6914722688513130671083952507756734675533E+6_dp,&
                          -.1523822153940741619228336495888678051866E+8_dp,&
                           .3829007513914141414141414141414141414141E+9_dp,&
                         -.1088226603578439108901514916552510537473E+11_dp,&
                          .3473202837650022522522522522522522522523E+12_dp,&
                         -.1236960214226927445425171034927132488108E+14_dp]
        ! parameters from above reference

        z2 = z

        if(z % re < zero_k1) then

            z2 = cmplx(abs(z % re), - z % im, kind = sp) + 1

        end if

        d = hypot(z2 % re, z2 % im)
        z1 = z2
        m = 0

        if(d <= z_limit) then                       !for small |z|

            m = ceiling(z_limit - d)
            z1 = z2 + m

        end if

        zr = one / z1
        zr2 = zr * zr

        sum = (((a(20) * zr2 + a(19)) * zr2 + a(18)) * zr2 + a(17)) * zr2
        sum = (((sum + a(16)) * zr2 + a(15)) * zr2 + a(14)) * zr2
        sum = (((sum + a(13)) * zr2 + a(12)) * zr2 + a(11)) * zr2
        sum = (((sum + a(10)) * zr2 + a(9)) * zr2 + a(8)) * zr2
        sum = (((sum + a(7)) * zr2 + a(6)) * zr2 + a(5)) * zr2
        sum = (((sum + a(4)) * zr2 + a(3)) * zr2 + a(2)) * zr2
        sum = (sum + a(1)) * zr + ln2pi / 2 - z1 + (z1 - 0.5_dp) * log(z1)

        if(m /= 0) then

            s = cmplx(zero, zero, kind = dp)

            do i = 1, m

                s = s + log(cmplx(z1, kind = dp) - i)

            end do

            sum = sum - s

        end if

        if(z % re < zero_k1) then

            sum = log(pi) - log(sin(pi * z)) - sum
            m = ceiling((2 * z % re - 3) / 4)
            sum % im = sum % im + 2 * pi * m * sign(1.0_sp, z % im)

        end if

        res = cmplx(sum, kind = sp)
    end function l_gamma_csp





    impure elemental function l_factorial_iint8(n) result(res)
    !
    ! Log(n!)
    !
        integer(int8), intent(in) :: n
        real(sp) :: res
        integer(int8), parameter :: zero = 0_int8, one = 1_int8, two = 2_int8
        real(sp), parameter :: zero_sp = 0.0_sp, one_sp = 1.0_sp

        if(n < zero) call error_stop("Error(l_factorial): Logarithm of"        &
            //" factorial function argument must be non-negative")

        select case(n)

        case (zero)

            res = zero_sp

        case (one)

            res = zero_sp

        case (two : )

            res = l_gamma(n + 1, one_sp)

        end select
    end function l_factorial_iint8

    impure elemental function l_factorial_iint16(n) result(res)
    !
    ! Log(n!)
    !
        integer(int16), intent(in) :: n
        real(sp) :: res
        integer(int16), parameter :: zero = 0_int16, one = 1_int16, two = 2_int16
        real(sp), parameter :: zero_sp = 0.0_sp, one_sp = 1.0_sp

        if(n < zero) call error_stop("Error(l_factorial): Logarithm of"        &
            //" factorial function argument must be non-negative")

        select case(n)

        case (zero)

            res = zero_sp

        case (one)

            res = zero_sp

        case (two : )

            res = l_gamma(n + 1, one_sp)

        end select
    end function l_factorial_iint16

    impure elemental function l_factorial_iint32(n) result(res)
    !
    ! Log(n!)
    !
        integer(int32), intent(in) :: n
        real(sp) :: res
        integer(int32), parameter :: zero = 0_int32, one = 1_int32, two = 2_int32
        real(sp), parameter :: zero_sp = 0.0_sp, one_sp = 1.0_sp

        if(n < zero) call error_stop("Error(l_factorial): Logarithm of"        &
            //" factorial function argument must be non-negative")

        select case(n)

        case (zero)

            res = zero_sp

        case (one)

            res = zero_sp

        case (two : )

            res = l_gamma(n + 1, one_sp)

        end select
    end function l_factorial_iint32

    impure elemental function l_factorial_iint64(n) result(res)
    !
    ! Log(n!)
    !
        integer(int64), intent(in) :: n
        real(sp) :: res
        integer(int64), parameter :: zero = 0_int64, one = 1_int64, two = 2_int64
        real(sp), parameter :: zero_sp = 0.0_sp, one_sp = 1.0_sp

        if(n < zero) call error_stop("Error(l_factorial): Logarithm of"        &
            //" factorial function argument must be non-negative")

        select case(n)

        case (zero)

            res = zero_sp

        case (one)

            res = zero_sp

        case (two : )

            res = l_gamma(n + 1, one_sp)

        end select
    end function l_factorial_iint64




    impure elemental function gpx_rsp(p, x) result(res)
    !
    ! Approximation of incomplete gamma G function with real argument p.
    !
    ! Based on Rémy Abergel and Lionel Moisan "Algorithm 1006, Fast and
    ! Accurate Evaluation of a Generalized Incomplete Gamma Function", ACM
    ! Transactions on Mathematical Software, March 2020.
    !
    ! Fortran 90 program by Jim-215-Fisher
    !
        real(sp), intent(in) :: p, x
        integer :: n

        real(dp) :: res, p_lim, a, b, g, c, d, y
        real(dp), parameter :: zero = 0.0_dp, one = 1.0_dp
        real(dp), parameter :: dm = tiny(1.0_dp) * 10 ** 6
        real(sp), parameter :: zero_k1 = 0.0_sp

        if(p <= zero_k1) call error_stop("Error(gpx): Incomplete gamma"        &
            //" function must have a positive parameter p")

        if(x < -9.0_sp) then

            p_lim = 5.0_sp * (sqrt(abs(x)) - 1.0_sp)

        elseif(x >= -9.0_sp .and. x <= zero_k1) then

            p_lim = zero_k1

        else

            p_lim = x

        endif

        if(x < zero_k1 .and. p < p_lim .and. abs(anint(p) - p) > tol_sp)   &
            call error_stop("Error(gpx): Incomplete gamma function with "      &
            //"negative x must come with a whole number p not too small")

        if(x < zero_k1) call error_stop("Error(gpx): Incomplete gamma"         &
            // " function with negative x must have an integer parameter p")

        if(p >= p_lim) then     !use modified Lentz method of continued fraction
                                !for eq. (15) in the above reference.
            a = one
            b = p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                if(mod(n, 2) == 0) then
                    a = (one - p - n / 2) * x
                else
                    a = (n / 2) * x
                end if

                b = p - one + n
                d =  d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_dp) exit

            end do

        else if(x >= zero_k1) then       !use modified Lentz method of continued
                                         !fraction for eq. (16) in the reference.
            a = one
            b = x + one - p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                a = (n - 1) * (1 + p - n)
                b = b + 2
                d = d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_dp) exit

            end do

        else
            g = ieee_value(1._sp, ieee_quiet_nan)

        end if

        res = g
    end function gpx_rsp




    impure elemental function gpx_iint8sp(p, x) result(res)
    !
    ! Approximation of incomplete gamma G function with integer argument p.
    !
    ! Based on Rémy Abergel and Lionel Moisan "Algorithm 1006, Fast and
    ! Accurate Evaluation of a Generalized Incomplete Gamma Function", ACM
    ! Transactions on Mathematical Software, March 2020.
    !
        integer(int8), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, p_lim, a, b, g, c, d, y
        integer :: n
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp
        real(sp), parameter :: dm = tiny(1.0_sp) * 10 ** 6
        integer(int8), parameter :: zero_k1 = 0_int8, two = 2_int8

        if(p <= zero_k1) call error_stop("Error(gpx): Incomplete gamma "       &
            //"function must have a positive parameter p")

        if(x < -9.0_sp) then

            p_lim = 5.0_sp * (sqrt(abs(x)) - 1.0_sp)

        else if(x >= -9.0_sp .and. x <= zero) then

            p_lim = zero

        else

            p_lim = x

        end if

        if(real(p, sp) >= p_lim) then

            a = one
            b = p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                if(mod(n, 2) == 0) then

                    a = (one - p - n / 2) * x

                else

                    a = (n / 2) * x

                end if

                b = p - 1 + n
                d =  d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else if(x >= zero) then

            a = one
            b = x + 1 - p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                a = -(n - 1) * (n - 1 - p)
                b = b + 2
                d = d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else

            a = -x
            c = one / a
            d = p - 1
            b = c * (a - d)
            n = 1

            do

                c = d * (d - one) / (a * a)
                d = d - 2
                y = c * ( a - d)
                b = b + y
                n = n + 1

                if(int(n, int8) > (p - two) / two .or. y < b * tol_sp) exit

            end do

            if(y >= b * tol_sp .and. mod(p, two) /= zero_k1)               &
                b = b + d * c / a

            g = ((-1) ** p * exp(-a + l_gamma(p, one) - (p - 1) * log(a))      &
                + b ) / a

        end if

        res = g
    end function gpx_iint8sp

    impure elemental function gpx_iint16sp(p, x) result(res)
    !
    ! Approximation of incomplete gamma G function with integer argument p.
    !
    ! Based on Rémy Abergel and Lionel Moisan "Algorithm 1006, Fast and
    ! Accurate Evaluation of a Generalized Incomplete Gamma Function", ACM
    ! Transactions on Mathematical Software, March 2020.
    !
        integer(int16), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, p_lim, a, b, g, c, d, y
        integer :: n
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp
        real(sp), parameter :: dm = tiny(1.0_sp) * 10 ** 6
        integer(int16), parameter :: zero_k1 = 0_int16, two = 2_int16

        if(p <= zero_k1) call error_stop("Error(gpx): Incomplete gamma "       &
            //"function must have a positive parameter p")

        if(x < -9.0_sp) then

            p_lim = 5.0_sp * (sqrt(abs(x)) - 1.0_sp)

        else if(x >= -9.0_sp .and. x <= zero) then

            p_lim = zero

        else

            p_lim = x

        end if

        if(real(p, sp) >= p_lim) then

            a = one
            b = p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                if(mod(n, 2) == 0) then

                    a = (one - p - n / 2) * x

                else

                    a = (n / 2) * x

                end if

                b = p - 1 + n
                d =  d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else if(x >= zero) then

            a = one
            b = x + 1 - p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                a = -(n - 1) * (n - 1 - p)
                b = b + 2
                d = d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else

            a = -x
            c = one / a
            d = p - 1
            b = c * (a - d)
            n = 1

            do

                c = d * (d - one) / (a * a)
                d = d - 2
                y = c * ( a - d)
                b = b + y
                n = n + 1

                if(int(n, int16) > (p - two) / two .or. y < b * tol_sp) exit

            end do

            if(y >= b * tol_sp .and. mod(p, two) /= zero_k1)               &
                b = b + d * c / a

            g = ((-1) ** p * exp(-a + l_gamma(p, one) - (p - 1) * log(a))      &
                + b ) / a

        end if

        res = g
    end function gpx_iint16sp

    impure elemental function gpx_iint32sp(p, x) result(res)
    !
    ! Approximation of incomplete gamma G function with integer argument p.
    !
    ! Based on Rémy Abergel and Lionel Moisan "Algorithm 1006, Fast and
    ! Accurate Evaluation of a Generalized Incomplete Gamma Function", ACM
    ! Transactions on Mathematical Software, March 2020.
    !
        integer(int32), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, p_lim, a, b, g, c, d, y
        integer :: n
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp
        real(sp), parameter :: dm = tiny(1.0_sp) * 10 ** 6
        integer(int32), parameter :: zero_k1 = 0_int32, two = 2_int32

        if(p <= zero_k1) call error_stop("Error(gpx): Incomplete gamma "       &
            //"function must have a positive parameter p")

        if(x < -9.0_sp) then

            p_lim = 5.0_sp * (sqrt(abs(x)) - 1.0_sp)

        else if(x >= -9.0_sp .and. x <= zero) then

            p_lim = zero

        else

            p_lim = x

        end if

        if(real(p, sp) >= p_lim) then

            a = one
            b = p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                if(mod(n, 2) == 0) then

                    a = (one - p - n / 2) * x

                else

                    a = (n / 2) * x

                end if

                b = p - 1 + n
                d =  d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else if(x >= zero) then

            a = one
            b = x + 1 - p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                a = -(n - 1) * (n - 1 - p)
                b = b + 2
                d = d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else

            a = -x
            c = one / a
            d = p - 1
            b = c * (a - d)
            n = 1

            do

                c = d * (d - one) / (a * a)
                d = d - 2
                y = c * ( a - d)
                b = b + y
                n = n + 1

                if(int(n, int32) > (p - two) / two .or. y < b * tol_sp) exit

            end do

            if(y >= b * tol_sp .and. mod(p, two) /= zero_k1)               &
                b = b + d * c / a

            g = ((-1) ** p * exp(-a + l_gamma(p, one) - (p - 1) * log(a))      &
                + b ) / a

        end if

        res = g
    end function gpx_iint32sp

    impure elemental function gpx_iint64sp(p, x) result(res)
    !
    ! Approximation of incomplete gamma G function with integer argument p.
    !
    ! Based on Rémy Abergel and Lionel Moisan "Algorithm 1006, Fast and
    ! Accurate Evaluation of a Generalized Incomplete Gamma Function", ACM
    ! Transactions on Mathematical Software, March 2020.
    !
        integer(int64), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, p_lim, a, b, g, c, d, y
        integer :: n
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp
        real(sp), parameter :: dm = tiny(1.0_sp) * 10 ** 6
        integer(int64), parameter :: zero_k1 = 0_int64, two = 2_int64

        if(p <= zero_k1) call error_stop("Error(gpx): Incomplete gamma "       &
            //"function must have a positive parameter p")

        if(x < -9.0_sp) then

            p_lim = 5.0_sp * (sqrt(abs(x)) - 1.0_sp)

        else if(x >= -9.0_sp .and. x <= zero) then

            p_lim = zero

        else

            p_lim = x

        end if

        if(real(p, sp) >= p_lim) then

            a = one
            b = p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                if(mod(n, 2) == 0) then

                    a = (one - p - n / 2) * x

                else

                    a = (n / 2) * x

                end if

                b = p - 1 + n
                d =  d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else if(x >= zero) then

            a = one
            b = x + 1 - p
            g = a / b
            c = a / dm
            d = one / b
            n = 2

            do

                a = -(n - 1) * (n - 1 - p)
                b = b + 2
                d = d * a + b

                if(d == zero) d = dm

                c = b + a / c

                if(c == zero) c = dm

                d = one / d
                y = c * d
                g = g * y
                n = n + 1

                if(abs(y - one) < tol_sp) exit

            end do

        else

            a = -x
            c = one / a
            d = p - 1
            b = c * (a - d)
            n = 1

            do

                c = d * (d - one) / (a * a)
                d = d - 2
                y = c * ( a - d)
                b = b + y
                n = n + 1

                if(int(n, int64) > (p - two) / two .or. y < b * tol_sp) exit

            end do

            if(y >= b * tol_sp .and. mod(p, two) /= zero_k1)               &
                b = b + d * c / a

            g = ((-1) ** p * exp(-a + l_gamma(p, one) - (p - 1) * log(a))      &
                + b ) / a

        end if

        res = g
    end function gpx_iint64sp




    impure elemental function ingamma_low_rsp(p, x) result(res)
    !
    ! Approximation of lower incomplete gamma function with real p.
    !
        real(sp), intent(in) :: p, x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > p) then

            s1 = log_gamma(p)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = exp(s1 + log(y))

        else if(x <= p .and. x > zero) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else

            call error_stop("Error(Logarithm of upper incomplete gamma "       &
              //"function): negative x must be with integer p")

        end if
    end function ingamma_low_rsp




    impure elemental function ingamma_low_iint8sp(p, x)          &
        result(res)
    !
    ! Approximation of lower incomplete gamma function with integer p.
    !
        integer(int8), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = exp(s1 + log(y))

        else if(x <= real(p, sp) .and. x > zero) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else

            s1 = -x + p * log(abs(x))
            res = gpx(p, x) * exp(s1)
            res = (-1) ** p * res

        end if
    end function ingamma_low_iint8sp

    impure elemental function ingamma_low_iint16sp(p, x)          &
        result(res)
    !
    ! Approximation of lower incomplete gamma function with integer p.
    !
        integer(int16), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = exp(s1 + log(y))

        else if(x <= real(p, sp) .and. x > zero) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else

            s1 = -x + p * log(abs(x))
            res = gpx(p, x) * exp(s1)
            res = (-1) ** p * res

        end if
    end function ingamma_low_iint16sp

    impure elemental function ingamma_low_iint32sp(p, x)          &
        result(res)
    !
    ! Approximation of lower incomplete gamma function with integer p.
    !
        integer(int32), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = exp(s1 + log(y))

        else if(x <= real(p, sp) .and. x > zero) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else

            s1 = -x + p * log(abs(x))
            res = gpx(p, x) * exp(s1)
            res = (-1) ** p * res

        end if
    end function ingamma_low_iint32sp

    impure elemental function ingamma_low_iint64sp(p, x)          &
        result(res)
    !
    ! Approximation of lower incomplete gamma function with integer p.
    !
        integer(int64), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = exp(s1 + log(y))

        else if(x <= real(p, sp) .and. x > zero) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else

            s1 = -x + p * log(abs(x))
            res = gpx(p, x) * exp(s1)
            res = (-1) ** p * res

        end if
    end function ingamma_low_iint64sp




    impure elemental function l_ingamma_low_rsp(p, x) result(res)

        real(sp), intent(in) :: p, x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp


        if(x == zero) then

            res = zero

        else if(x > p) then

            s1 = log_gamma(p)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = s1 + log(y)

        else if(x <= p .and. x > zero) then

            s1 = -x + p * log(abs(x))
            res = log(abs(gpx(p, x))) + s1

        else

            call error_stop("Error(Logarithm of upper incomplete gamma "       &
              //"function): negative x must be with integer p")

        end if
    end function l_ingamma_low_rsp





    impure elemental function l_ingamma_low_iint8sp(p, x)        &
        result(res)

        integer(int8), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = s1 + log(y)

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x))
            res = log(abs(gpx(p, x))) + s1

        end if
    end function l_ingamma_low_iint8sp

    impure elemental function l_ingamma_low_iint16sp(p, x)        &
        result(res)

        integer(int16), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = s1 + log(y)

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x))
            res = log(abs(gpx(p, x))) + s1

        end if
    end function l_ingamma_low_iint16sp

    impure elemental function l_ingamma_low_iint32sp(p, x)        &
        result(res)

        integer(int32), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = s1 + log(y)

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x))
            res = log(abs(gpx(p, x))) + s1

        end if
    end function l_ingamma_low_iint32sp

    impure elemental function l_ingamma_low_iint64sp(p, x)        &
        result(res)

        integer(int64), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = l_gamma(p, one)
            y = one - exp(-x + p * log(x) - s1) * gpx(p, x)
            res = s1 + log(y)

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x))
            res = log(abs(gpx(p, x))) + s1

        end if
    end function l_ingamma_low_iint64sp




    impure elemental function ingamma_up_rsp(p, x) result(res)
    !
    ! Approximation of upper incomplete gamma function with real p.
    !
        real(sp), intent(in) :: p, x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = gamma(p)

        else if(x > p) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else if(x <= p .and. x > zero) then

            y = log_gamma(p)
            s1 = -x + p * log(x) - y
            res = (one - gpx(p, x) * exp(s1)) * exp(y)

        else


            call error_stop("Error(Logarithm of upper incomplete gamma "       &
              //"function): negative x must be with integer p")

        end if
    end function ingamma_up_rsp




    impure elemental function ingamma_up_iint8sp(p, x)           &
        result(res)
    !
    ! Approximation of upper incomplete gamma function with integer p.
    !
        integer(int8), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = gamma(real(p, sp))

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = (one - res) * exp(y)

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) - y
            res = gpx(p, x) * exp(s1)
            res = (one - (-1) ** p * res) * exp(y)

        end if
    end function ingamma_up_iint8sp

    impure elemental function ingamma_up_iint16sp(p, x)           &
        result(res)
    !
    ! Approximation of upper incomplete gamma function with integer p.
    !
        integer(int16), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = gamma(real(p, sp))

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = (one - res) * exp(y)

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) - y
            res = gpx(p, x) * exp(s1)
            res = (one - (-1) ** p * res) * exp(y)

        end if
    end function ingamma_up_iint16sp

    impure elemental function ingamma_up_iint32sp(p, x)           &
        result(res)
    !
    ! Approximation of upper incomplete gamma function with integer p.
    !
        integer(int32), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = gamma(real(p, sp))

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = (one - res) * exp(y)

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) - y
            res = gpx(p, x) * exp(s1)
            res = (one - (-1) ** p * res) * exp(y)

        end if
    end function ingamma_up_iint32sp

    impure elemental function ingamma_up_iint64sp(p, x)           &
        result(res)
    !
    ! Approximation of upper incomplete gamma function with integer p.
    !
        integer(int64), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = gamma(real(p, sp))

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = gpx(p, x) * exp(s1)

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = (one - res) * exp(y)

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) - y
            res = gpx(p, x) * exp(s1)
            res = (one - (-1) ** p * res) * exp(y)

        end if
    end function ingamma_up_iint64sp




    impure elemental function l_ingamma_up_rsp(p, x) result(res)

        real(sp), intent(in) :: p, x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp


        if(x == zero) then

            res = log_gamma(p)

        else if(x > p) then

            s1 = -x + p * log(x)
            res = log(gpx(p, x)) + s1

        else if(x <= p .and. x > zero) then

            y= log_gamma(p)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = log(one - res) + y

        else

            call error_stop("Error(Logarithm of upper incomplete gamma "       &
              //"function): negative x must be with integer p")

        end if
    end function l_ingamma_up_rsp





    impure elemental function l_ingamma_up_iint8sp(p, x)         &
        result(res)

        integer(int8), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = l_gamma(p, one)

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = log(gpx(p, x)) + s1

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = log(one - res) + y

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) + log(gpx(p, x))
            res = (-1) ** p * exp(s1)
            res = log(abs(exp(y) - res))

        end if
    end function l_ingamma_up_iint8sp

    impure elemental function l_ingamma_up_iint16sp(p, x)         &
        result(res)

        integer(int16), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = l_gamma(p, one)

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = log(gpx(p, x)) + s1

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = log(one - res) + y

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) + log(gpx(p, x))
            res = (-1) ** p * exp(s1)
            res = log(abs(exp(y) - res))

        end if
    end function l_ingamma_up_iint16sp

    impure elemental function l_ingamma_up_iint32sp(p, x)         &
        result(res)

        integer(int32), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = l_gamma(p, one)

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = log(gpx(p, x)) + s1

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = log(one - res) + y

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) + log(gpx(p, x))
            res = (-1) ** p * exp(s1)
            res = log(abs(exp(y) - res))

        end if
    end function l_ingamma_up_iint32sp

    impure elemental function l_ingamma_up_iint64sp(p, x)         &
        result(res)

        integer(int64), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1, y
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x == zero) then

            res = l_gamma(p, one)

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x)
            res = log(gpx(p, x)) + s1

        else if(x <= real(p, sp) .and. x > zero) then

            y = l_gamma(p, one)
            s1 = -x + p * log(x) - y
            res = gpx(p, x) * exp(s1)
            res = log(one - res) + y

        else

            y = l_gamma(p, one)
            s1 = -x + p * log(abs(x)) + log(gpx(p, x))
            res = (-1) ** p * exp(s1)
            res = log(abs(exp(y) - res))

        end if
    end function l_ingamma_up_iint64sp





    impure elemental function regamma_p_rsp(p, x) result(res)
    !
    ! Approximation of regularized incomplete gamma function P(p,x) for real p
    !
        real(sp), intent(in) :: p, x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_p): Regularized gamma_p"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = zero

        else if(x > p) then

            s1 = -x + p * log(x) - log_gamma(p)
            res = one - exp(s1 + log(gpx(p,x)))

        else if(x <= p) then

            s1 = -x + p * log(abs(x)) - log_gamma(p)
            res = exp(log(gpx(p, x)) + s1)

        end if
    end function regamma_p_rsp




    impure elemental function regamma_p_iint8sp(p, x) result(res)
    !
    ! Approximation of regularized incomplete gamma function P(p,x) for integer p
    !
        integer(int8), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_p): Regularized gamma_p"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = exp(log(gpx(p, x)) + s1)

        end if
    end function regamma_p_iint8sp

    impure elemental function regamma_p_iint16sp(p, x) result(res)
    !
    ! Approximation of regularized incomplete gamma function P(p,x) for integer p
    !
        integer(int16), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_p): Regularized gamma_p"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = exp(log(gpx(p, x)) + s1)

        end if
    end function regamma_p_iint16sp

    impure elemental function regamma_p_iint32sp(p, x) result(res)
    !
    ! Approximation of regularized incomplete gamma function P(p,x) for integer p
    !
        integer(int32), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_p): Regularized gamma_p"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = exp(log(gpx(p, x)) + s1)

        end if
    end function regamma_p_iint32sp

    impure elemental function regamma_p_iint64sp(p, x) result(res)
    !
    ! Approximation of regularized incomplete gamma function P(p,x) for integer p
    !
        integer(int64), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_p): Regularized gamma_p"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = zero

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        else if(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = exp(log(gpx(p, x)) + s1)

        end if
    end function regamma_p_iint64sp




    impure elemental function regamma_q_rsp(p, x) result(res)
    !
    ! Approximation of regularized incomplete gamma function Q(p,x) for real p
    !
        real(sp), intent(in) :: p, x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_p): Regularized gamma_q"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = one

        else if(x > p) then

            s1 = -x + p * log(x) - log_gamma(p)
            res = exp(s1 + log(gpx(p,x)))

        else if(x <= p) then

            s1 = -x + p * log(abs(x)) - log_gamma(p)
            res = one - exp(log(gpx(p, x)) + s1)

        end if
    end function regamma_q_rsp




    impure elemental function regamma_q_iint8sp(p, x) result(res)
    !
    ! Approximation of regularized incomplet gamma function Q(p,x) for integer p
    !
        integer(int8), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_q): Regularized gamma_q"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = one

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = exp(log(gpx(p,x)) + s1)

        elseif(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        end if
     end function regamma_q_iint8sp

    impure elemental function regamma_q_iint16sp(p, x) result(res)
    !
    ! Approximation of regularized incomplet gamma function Q(p,x) for integer p
    !
        integer(int16), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_q): Regularized gamma_q"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = one

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = exp(log(gpx(p,x)) + s1)

        elseif(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        end if
     end function regamma_q_iint16sp

    impure elemental function regamma_q_iint32sp(p, x) result(res)
    !
    ! Approximation of regularized incomplet gamma function Q(p,x) for integer p
    !
        integer(int32), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_q): Regularized gamma_q"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = one

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = exp(log(gpx(p,x)) + s1)

        elseif(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        end if
     end function regamma_q_iint32sp

    impure elemental function regamma_q_iint64sp(p, x) result(res)
    !
    ! Approximation of regularized incomplet gamma function Q(p,x) for integer p
    !
        integer(int64), intent(in) :: p
        real(sp), intent(in) :: x
        real(sp) :: res, s1
        real(sp), parameter :: zero = 0.0_sp, one = 1.0_sp

        if(x < zero) call error_stop("Error(regamma_q): Regularized gamma_q"   &
            //" function is not defined at x < 0")


        if(x == zero) then

            res = one

        else if(x > real(p, sp)) then

            s1 = -x + p * log(x) - l_gamma(p, one)
            res = exp(log(gpx(p,x)) + s1)

        elseif(x <= real(p, sp)) then

            s1 = -x + p * log(abs(x)) - l_gamma(p, one)
            res = one - exp(s1 + log(gpx(p,x)))

        end if
     end function regamma_q_iint64sp


end module stdlib_specialfunctions_gamma
