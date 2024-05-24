program example_constants
    use stdlib_constants, only: c, pi=>PI_dp
    use stdlib_codata, only: alpha=>ALPHA_PARTICLE_ELECTRON_MASS_RATIO
    use stdlib_codata_type, only : to_real
    use stdlib_kinds, only: dp, sp

    ! Use most common physical constants defined as double precision reals
    print *, "speed of light in vacuum= ", c

    ! Use of mathematical constants such as PI
    print *, "PI as double precision real= ", pi

    ! Use codata_constant type for evaluating the value to the desired precision
    print *, "Value of alpha... evaluated to double precision=", alpha%to_real(1.0_dp)
    print *, "Uncertainty of alpha... evaluated to double precision=", alpha%to_real(1.0_sp, .true.)
    print *, "Value of alpha... evaluated to single precision=", alpha%to_real(1.0_sp)

    ! Convert a codata constant to a real
    print *, "Value of the alpha... evaluated to double precision=", to_real(alpha, 1.0_dp)


    ! Print out codata constant attributes: name, value, uncertainty and unit
    call alpha%print()

end program example_constants
