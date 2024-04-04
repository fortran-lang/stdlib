program example_math_arg
  use stdlib_math, only: arg
  implicit none
  print *, arg((0.0, 0.0))                  ! 0.0
  print *, arg((3.0, 4.0))                  ! 0.927
  print *, arg(2.0*exp((0.0, 0.5)))         ! 0.5
  print *, arg([(0.0, 1.0), (1.0, 0.0), (0.0, -1.0), (-1.0, 0.0)])  ! [π/2, 0.0, -π/2, π]
end program example_math_arg
