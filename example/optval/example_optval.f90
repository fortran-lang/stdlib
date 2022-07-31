program example_optval
  use stdlib_optval, only: optval
  implicit none
  print *, root(64.0)
! 8.0
  print *, root(64.0, 3)
! 4.0
contains
  real function root(x, n)
    real, intent(in) :: x
    integer, intent(in), optional :: n
    root = x**(1.0/optval(n, 2))
  end function root
end program example_optval
