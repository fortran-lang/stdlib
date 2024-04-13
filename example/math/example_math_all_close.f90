program example_math_all_close

  use stdlib_math, only: all_close
  implicit none
  real    :: y, NAN
  complex :: z(4, 4)

  y = -3
  NAN = sqrt(y)
  z = (1.0, 1.0)

  print *, all_close(z + cmplx(1.0e-11, 1.0e-11), z)     ! T
  print *, NAN, all_close([NAN], [NAN]), all_close([NAN], [NAN], equal_nan=.true.)
! NAN, F, T

end program example_math_all_close
