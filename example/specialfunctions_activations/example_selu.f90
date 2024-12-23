program example_selu
    use stdlib_kinds, only: sp
    use stdlib_math, only: linspace
    use stdlib_specialfunctions, only: selu
    
    integer, parameter :: n = 10
    real(sp) :: x(n), y(n)
    implicit none
  
    x = linspace(-2._sp, 2._sp, n)
    y = selu( x )
end program example_selu
  