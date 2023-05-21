program example_radix_sort
    use iso_fortran_env, only: int8, int16, dp => real64
    use stdlib_sorting, only: radix_sort
    implicit none
    integer(int8), allocatable :: arri8(:)
    integer(int16), allocatable :: arri16(:)
    real(dp) ::  x
    real(dp), allocatable ::  arrf64(:)

    arri8 = [-128, 127, 0, -1, 1]
    call radix_sort(arri8)
    print *, arri8

    arri16 = [-32767, 32767, 0, 0, -3, 2, -3]
    call radix_sort(arri16, reverse=.true.)
    print *, arri16

    allocate (arrf64(10))
    x = 0.0_dp ! divide zero will arise compile error
    arrf64 = [1.0_dp/x, 0.0_dp, 0.0_dp/x, -1.0_dp/x, -0.0_dp, 1.0_dp, -1.0_dp, 3.45_dp, -3.14_dp, 3.44_dp]
    call radix_sort(arrf64)
    print *, arrf64
    ! Expected output:
    ! nan, -inf, -3.14, -1.0, -0.0, 0.0, 1.0, 3.44, 3.45, inf
    ! Note: the position of nan is undefined
end program example_radix_sort
