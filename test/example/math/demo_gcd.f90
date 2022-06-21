program demo_gcd
    use stdlib_math, only: gcd
    implicit none
    integer :: a, b, c

    a = 48
    b = 18
    c = gcd(a, b) ! returns 6
end program demo_gcd
