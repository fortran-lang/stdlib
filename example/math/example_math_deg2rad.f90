program example_math_deg2rad
    use stdlib_math, only: deg2rad
    implicit none
    print *, deg2rad(0.0)       ! 0.0
    print *, deg2rad(90.0)      ! 1.57508
    print *, deg2rad(-180.0)    ! -3.1416

end program example_math_deg2rad
