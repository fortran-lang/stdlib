program example_constants
    use stdlib_constants, only: c, PI=>PI_dp
    use stdlib_codata, only: alpha=>ALPHA_PARTICLE_ELECTRON_MASS_RATIO
    use stdlib_codata, only: ATOMIC_UNIT_OF_1ST_HYPERPOLARIZABILITY

    call alpha%print()

    print *, ATOMIC_UNIT_OF_1ST_HYPERPOLARIZABILITY%eval_value(1.0)

    print *, c
    print *, PI

end program example_constants
