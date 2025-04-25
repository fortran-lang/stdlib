program example_math_rad2deg
    use stdlib_math, only: rad2deg
    use stdlib_constants, only: PI_sp
    implicit none
    print *, rad2deg(0.0)              ! 0.0
    print *, rad2deg(PI_sp / 2.0)      ! 90.0
    print *, rad2deg(-PI_sp)           ! -3.1416

end program example_math_rad2deg
