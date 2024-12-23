program example_silu
    use stdlib_kinds, only: sp
    use stdlib_math, only: linspace
    use stdlib_specialfunctions, only: silu
    
    integer, parameter :: n = 10
    real(sp) :: x(n), y(n)
    implicit none
  
    x = linspace(-2._sp, 2._sp, n)
    y = silu( x )
end program example_silu
  