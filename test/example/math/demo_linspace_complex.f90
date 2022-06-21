program demo_linspace_complex
    use stdlib_math, only: linspace
    use stdlib_kinds, only: dp
    implicit none

    complex(dp) :: start = complex(10.0_dp, 5.0_dp)
    complex(dp) :: end = complex(-10.0_dp, 15.0_dp)

    complex(dp) :: z(11)

    z = linspace(start, end, 11)
end program demo_linspace_complex
