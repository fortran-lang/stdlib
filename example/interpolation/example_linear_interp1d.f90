program example_linear_interp1d
    use stdlib_kinds, only: dp
    use stdlib_interpolation, only: linear_interp1d_dp_type, linear_interp1d, &
        interp1d, interp_extrap_clamp
    implicit none

    real(dp) :: x(5), y(5), xq(4), yq(4), y_val
    type(linear_interp1d_dp_type) :: interp

    ! Define sample nodes and values.
    x = [0.0_dp, 1.0_dp, 2.5_dp, 4.0_dp, 5.0_dp]
    y = [0.0_dp, 2.0_dp, 1.0_dp, 4.0_dp, 3.5_dp]

    ! Initialize reusable interpolator and evaluate on an array.
    interp = linear_interp1d(x, y, extrap=interp_extrap_clamp)
    xq = [0.5_dp, 1.75_dp, 3.25_dp, 6.0_dp]
    yq = interp%evaluate(xq)
    print *, "Interpolated array :", yq
    call interp%destroy()

    ! Evaluate a single point with the one-shot driver.
    y_val = interp1d(0.5_dp, x, y)
    print *, "interp1d at x=0.5  :", y_val

end program example_linear_interp1d
