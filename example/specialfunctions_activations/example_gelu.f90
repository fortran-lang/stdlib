program example_gelu
    use stdlib_kinds, only: sp
    use stdlib_math, only: linspace
    use stdlib_specialfunctions, only: gelu
    implicit none
    integer, parameter :: n = 10
    real(sp) :: x(n), y(n)
    
    x = linspace(-2._sp, 2._sp, n)
    y = gelu( x )
    print *, y
end program example_gelu
  