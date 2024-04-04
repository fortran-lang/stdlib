program example_math_argpi
  use stdlib_math, only: argpi
  implicit none
  print *, argpi((0.0, 0.0))                  ! 0.0
  print *, argpi((3.0, 4.0))                  ! 0.295
  print *, argpi(2.0*exp((0.0, 0.5)))         ! 0.159
  print *, argpi([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])  ! [0.5, 0.0, -0.5, 1.0]
end program example_math_argpi
