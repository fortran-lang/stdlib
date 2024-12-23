program example_elu
    use stdlib_kinds, only: sp
    use stdlib_math, only: linspace
    use stdlib_specialfunctions, only: elu
    
    integer, parameter :: n = 10
    real(sp) :: x(n), y(n)
    implicit none
  
    x = linspace(-2._sp, 2._sp, n)
    y = elu( x , 1.0 )
end program example_elu
  