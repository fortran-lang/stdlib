program example_elu
    use stdlib_kinds, only: sp
    use stdlib_math, only: linspace
    use stdlib_specialfunctions, only: elu
    implicit none
    integer, parameter :: n = 10
    real(sp) :: x(n), y(n)
  
    x = linspace(-2._sp, 2._sp, n)
    y = elu( x , 1.0 )
    print *, y
end program example_elu
  