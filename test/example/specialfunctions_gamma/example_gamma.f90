program example_gamma
  use stdlib_kinds, only: dp, int64
  use stdlib_specialfunctions_gamma, only: gamma
  implicit none

  integer :: i
  integer(int64) :: n
  real :: x
  real(dp) :: y
  complex :: z
  complex(dp) :: z1

  i = 10
  n = 15_int64
  x = 2.5
  y = 4.3_dp
  z = (2.3, 0.6)
  z1 = (-4.2_dp, 3.1_dp)

  print *, gamma(i)              !integer gives exact result
! 362880

  print *, gamma(n)
! 87178291200

  print *, gamma(x)              ! intrinsic function call
! 1.32934034

  print *, gamma(y)              ! intrinsic function call
! 8.8553433604540341

  print *, gamma(z)
! (0.988054395, 0.383354813)

  print *, gamma(z1)
! (-2.78916032990983999E-005, 9.83164600163221218E-006)
end program example_gamma
