program example_log_gamma
  use stdlib_kinds, only: sp, dp
  use stdlib_specialfunctions_gamma, only: log_gamma
  implicit none

  integer :: i
  real :: x
  real(dp) :: y
  complex(sp) :: z

  i = 10
  x = 8.76
  y = x
  z = (5.345, -3.467)

  print *, log_gamma(i)     !default single precision output
!12.8018274

  print *, log_gamma(x)     !intrinsic function call

!10.0942659

  print *, log_gamma(y)     !intrinsic function call

!10.094265528673880

  print *, log_gamma(z)     !same kind as input

!(2.56165648, -5.73382425)

end program example_log_gamma
