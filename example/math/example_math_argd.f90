program example_math_argd
  use stdlib_math, only: argd
  implicit none
  print *, argd((0.0, 0.0))                  ! 0.0°
  print *, argd((3.0, 4.0))                  ! 53.1°
  print *, argd(2.0*exp((0.0, 0.5)))         ! 28.64°
  print *, argd([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])  ! [90°, 0°, -90°, 180°]
end program example_math_argd
