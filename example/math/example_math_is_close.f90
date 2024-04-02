program example_math_is_close

  use stdlib_math, only: is_close
  implicit none
  real :: x(2) = [1, 2], y, NAN

  y = -3
  NAN = sqrt(y)

  print *, is_close(x, [real :: 1, 2.1])       ! [T, F]
  print *, is_close(2.0, 2.1, abs_tol=0.1)    ! T
  print *, NAN, is_close(2.0, NAN), is_close(2.0, NAN, equal_nan=.true.)   ! NAN, F, F
  print *, is_close(NAN, NAN), is_close(NAN, NAN, equal_nan=.true.)        ! F, T

end program example_math_is_close
