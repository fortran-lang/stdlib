program example_gelu
    use stdlib_kinds, only: sp
    use stdlib_math, only: linspace
    use stdlib_specialfunctions, only: leaky_relu
    implicit none
    integer, parameter :: n = 10
    real(sp) :: x(n), y(n)
    
    x = linspace(-2._sp, 2._sp, n)
    y = leaky_relu( x , 0.1_sp )
    print *, y
end program example_gelu
  